// -----------------------------------------------------------------------------
// -- Includes -----------------------------------------------------------------
// -----------------------------------------------------------------------------

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <teamspeak/public_errors.h>
#include <teamspeak/public_errors_rare.h>
#include <teamspeak/public_rare_definitions.h>
#include <teamspeak/clientlib_publicdefinitions.h>

#include <boost/thread.hpp>

#include <json/json.h>

#include "plugin.h"
#include "rpc.hpp"

// -----------------------------------------------------------------------------
// -- Preprocessor macros ------------------------------------------------------
// -----------------------------------------------------------------------------

#define _strcpy(dest, destSize, src)            \
    {                                           \
        strncpy(dest, src, destSize - 1);       \
        (dest)[destSize - 1] = '\0';            \
    }

#define PLUGIN_API_VERSION 20

#define PATH_BUFSIZE 512
#define COMMAND_BUFSIZE 128
#define INFODATA_BUFSIZE 128
#define SERVERINFO_BUFSIZE 256
#define CHANNELINFO_BUFSIZE 512
#define RETURNCODE_BUFSIZE 128

// -----------------------------------------------------------------------------
// -- Type definitions ---------------------------------------------------------
// -----------------------------------------------------------------------------

typedef char*                  mcstring_t;
typedef const char*            ccstring_t;
typedef enum PluginMenuType    menu_type_t;
typedef struct PluginMenuItem* menu_item_t;
typedef anyID                  ident_t;

// -----------------------------------------------------------------------------
// -- Global variables ---------------------------------------------------------
// -----------------------------------------------------------------------------

static struct TS3Functions ts3Functions;
static char *pluginID = NULL;
rpc::server_handle_t *rpc_server = NULL;

// -----------------------------------------------------------------------------
// -- Helper functions ---------------------------------------------------------
// -----------------------------------------------------------------------------

//! Helper function for logging.
void pluginLog(std::string level, std::string message) {
    std::string pluginLogPrefix = "ZeroMQ";
    std::cerr << pluginLogPrefix << ":"
              << " "
              << "[" << level << "]"
              << " " << message << std::endl;
}

//! Call pluginLog with level DEBUG.
void pluginLog_DEBUG(std::string message) { pluginLog("DEBUG", message); }

//! Call pluginLog with level INFO.
void pluginLog_INFO(std::string message) { pluginLog("INFO", message); }

//! Call pluginLog with level WARNING.
void pluginLog_WARNING(std::string message) { pluginLog("WARNING", message); }

//! Call pluginLog with level ERROR.
void pluginLog_ERROR(std::string message) { pluginLog("ERROR", message); }

// -----------------------------------------------------------------------------
// -- Required functions -------------------------------------------------------
// -----------------------------------------------------------------------------

//! Unique name identifying this plugin.
ccstring_t ts3plugin_name() { return "ZeroMQ Plugin"; }

//! Plugin version.
ccstring_t ts3plugin_version() { return "0.0.1"; }

//! Plugin API version.
//! Must be the same as the client's API major version, or else the plugin will
//! fail to load.
int32_t ts3plugin_apiVersion() { return PLUGIN_API_VERSION; }

//! Plugin author.
ccstring_t ts3plugin_author() { return "Remy Goldschmidt / Michael Bishop"; }

//! Plugin description.
ccstring_t ts3plugin_description() {
    return "This plugin exposes the TeamSpeak plugin API over a ZeroMQ server.";
}

//! Set TeamSpeak 3 callback functions.
void ts3plugin_setFunctionPointers(const struct TS3Functions funcs) {
    ts3Functions = funcs;
}

//! Custom code called right after loading the plugin.
//!
//! Possible return values:
//!   *  0 --- Plugin successfully loaded.
//!   *  1 --- Plugin failed to load.
//!   * -2 --- Plugin failed to load, but don't notify the user.
//!
//! The -2 return value is used for plugins that want to display their own error
//! messages when failing, but still want to be unloaded automatically.
int32_t ts3plugin_init() {
    pluginLog_INFO("Initializing server");
    rpc_server = new rpc::server_handle_t;
    pluginLog_INFO("Starting server");
    rpc_server->start_server();
    return 0;
}

//! Custom code called right before the plugin is unloaded.
void ts3plugin_shutdown() {
    pluginLog_INFO("Shutting down");
    pluginLog_DEBUG("Starting shutdown process");

    pluginLog_DEBUG("Checking if RPC server exists");
    if(rpc_server) {
        pluginLog_DEBUG("RPC server found");
        pluginLog_DEBUG("Sending shutdown message");
        Json::Value root;
        root["event"] = "shutdown";
        rpc_server->send_event(root);
        pluginLog_DEBUG("Telling RPC server to shut down");
        rpc_server->shutdown_server();
        delete rpc_server;
        rpc_server = NULL;
    }

    if(pluginID) {
        free(pluginID);
        pluginID = NULL;
    }

    pluginLog_DEBUG("Plugin shutdown completed");
}

// -----------------------------------------------------------------------------
// -- Optional functions -------------------------------------------------------
// -----------------------------------------------------------------------------

//! Tell client if plugin offers a configuration window.
//! If this function is not implemented, it's an assumed "does not offer".
//!
//! Possible return values:
//!   * PLUGIN_OFFERS_NO_CONFIGURE
//!     --- Plugin does not implement ts3plugin_configure.
//!   * PLUGIN_OFFERS_CONFIGURE_NEW_THREAD
//!     --- Plugin does implement ts3plugin_configure and requests
//!         to run this function in an own thread.
//!   * PLUGIN_OFFERS_CONFIGURE_QT_THREAD
//!     --- Plugin does implement ts3plugin_configure and requests
//!         to run this function in the Qt GUI thread.
int32_t ts3plugin_offersConfigure() {
    pluginLog_DEBUG("offersConfigure");
    return PLUGIN_OFFERS_NO_CONFIGURE;
}

//! Plugin might offer a configuration window.
//! If ts3plugin_offersConfigure returns 0, this function will not be called.
void ts3plugin_configure(void *handle, void *qParentWidget) {
    pluginLog_DEBUG("configure");
}

//! If the plugin wants to use error return codes, plugin commands, hotkeys or
//! menu items, it needs to register a command ID. This function will be
//! automatically called after the plugin was initialized.
//! If you don't use these features, this function can be omitted.
//! Note the passed pluginID parameter is no longer valid after calling this
//! function, so you must copy it and store it in the plugin.
void ts3plugin_registerPluginID(ccstring_t id) {
    // FIXME: clean up
    const size_t sz = strlen(id) + 1;
    pluginID = (char *) malloc(sz * sizeof(char));
    // The id buffer will invalidate after exiting this function, so we copy it
    _strcpy(pluginID, sz, id);
    printf("PLUGIN: registerPluginID: %s\n", pluginID);
}

//! Plugin command keyword. Return NULL or "" if not used.
ccstring_t ts3plugin_commandKeyword() { return "zeromq"; }

//! Plugin processes console command.
//!
//! Possible return values:
//!   * 0 --- Plugin handled the command.
//!   * 1 --- Plugin did not handle the command.
int32_t ts3plugin_processCommand(uint64_t schandlerID, ccstring_t command) {
    return 0;
}

// -----------------------------------------------------------------------------
// Implement the following three functions when the plugin should display a line
// in the server/channel/client info.
//
// If any of ts3plugin_infoTitle, ts3plugin_infoData or ts3plugin_freeMemory is
// missing, the info text will not be displayed
// -----------------------------------------------------------------------------

//! Static title shown in the left column in the info frame.
ccstring_t ts3plugin_infoTitle() {
    return "The ZeroMQ plugin for TeamSpeak 3.";
}

//! Dynamic content shown in the right column in the info frame.
//! Memory for the data string needs to be allocated in this function.
//!
//! The client will call ts3plugin_freeMemory once done with the string to
//! release the allocated memory again.
//!
//! Check the parameter "type" if you want to implement this feature only for
//! specific item types.
//!
//! Set the parameter "data" to NULL to have the client ignore the info data.
void ts3plugin_infoData(uint64_t schandlerID,
                        uint64_t id,
                        enum PluginItemType type,
                        char **data) {
    data = NULL;
}

//! Required to release the memory for parameter "data" allocated
//! in ts3plugin_infoData and ts3plugin_initMenus.
void ts3plugin_freeMemory(void *data) { free(data); }

//! If autoload is enabled, the plugin will request to always be automatically
//! loaded by the client unless the user manually disabled it in the plugin
//! dialog.
//!
//! This function is optional. If missing, no autoload is assumed.
//!
//! Possible return values:
//!   * 0 --- The plugin should not be autoloaded.
//!   * 1 --- The plugin should be autoloaded.
int32_t ts3plugin_requestAutoload() { return 1; }

//! Helper function to create a menu item
static menu_item_t createMenuItem(menu_type_t type,
                                  int32_t id,
                                  ccstring_t text,
                                  ccstring_t icon) {
    auto menuItem = (menu_item_t) malloc(sizeof(menu_item_t));
    menuItem->type = type;
    menuItem->id = id;
    _strcpy(menuItem->text, PLUGIN_MENU_BUFSZ, text);
    _strcpy(menuItem->icon, PLUGIN_MENU_BUFSZ, icon);
    return menuItem;
}

// Some macros to make the code to create menu items a bit more readable.

#define BEGIN_CREATE_MENUS(x)                                       \
    const size_t sz = x + 1;                                        \
    size_t n = 0;                                                   \
    *menuItems = (menu_item_t *) malloc(sizeof(menu_item_t) * sz);

#define CREATE_MENU_ITEM(a, b, c, d)                \
    (*menuItems)[n++] = createMenuItem(a, b, c, d);

#define END_CREATE_MENUS                        \
    (*menuItems)[n++] = NULL;                   \
    assert(n == sz);

//! This is an enum describing the menu IDs for this plugin.
//! Pass these IDs when creating a menuitem to the TS3 client.
//! When the menu item is triggered, ts3plugin_onMenuItemEvent will be called
//! passing the menu ID of the triggered menu item.
//! These IDs are freely choosable by the plugin author.
//! It's not really needed to use an enum, it just looks prettier.
enum {
    MENU_ID_CLIENT_1 = 1,
    MENU_ID_CLIENT_2,
    MENU_ID_CHANNEL_1,
    MENU_ID_CHANNEL_2,
    MENU_ID_CHANNEL_3,
    MENU_ID_GLOBAL_1,
    MENU_ID_GLOBAL_2
};

/*
 * Initialize plugin menus.
 * This function is called after ts3plugin_init and ts3plugin_registerPluginID.
 * A pluginID is required for plugin menus to work.
 * Both ts3plugin_registerPluginID and ts3plugin_freeMemory must be implemented
 * to use menus.
 * If plugin menus are not used by a plugin, do not implement this function or
 * return NULL.
 */
void ts3plugin_initMenus(struct PluginMenuItem ***menuItems, char **menuIcon) {
    /*
     * Create the menus
     * There are three types of menu items:
     * - PLUGIN_MENU_TYPE_CLIENT:  Client context menu
     * - PLUGIN_MENU_TYPE_CHANNEL: Channel context menu
     * - PLUGIN_MENU_TYPE_GLOBAL:  "Plugins" menu in menu bar of main window
     *
     * Menu IDs are used to identify the menu item when
     * ts3plugin_onMenuItemEvent is called
     *
     * The menu text is required, max length is 128 characters
     *
     * The icon is optional, max length is 128 characters. When not using icons,
     * just pass an empty string.
     * Icons are loaded from a subdirectory in the TeamSpeak client plugins
     * folder. The subdirectory must be named like the
     * plugin filename, without dll/so/dylib suffix
     * e.g. for "test_plugin.dll", icon "1.png" is loaded from <TeamSpeak 3
     * Client install dir>\plugins\test_plugin\1.png
     */

    BEGIN_CREATE_MENUS(
        7); /* IMPORTANT: Number of menu items must be correct! */
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_CLIENT, MENU_ID_CLIENT_1, "Client item 1", "1.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_CLIENT, MENU_ID_CLIENT_2, "Client item 2", "2.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_1, "Channel item 1", "1.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_2, "Channel item 2", "2.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_3, "Channel item 3", "3.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_GLOBAL, MENU_ID_GLOBAL_1, "Global item 1", "1.png");
    CREATE_MENU_ITEM(
        PLUGIN_MENU_TYPE_GLOBAL, MENU_ID_GLOBAL_2, "Global item 2", "2.png");
    END_CREATE_MENUS; /* Includes an assert checking if the number of menu items
                         matched */

    /*
     * Specify an optional icon for the plugin. This icon is used for the
     * plugins submenu within context and main menus
     * If unused, set menuIcon to NULL
     */
    *menuIcon = (char *) malloc(PLUGIN_MENU_BUFSZ * sizeof(char));
    _strcpy(*menuIcon, PLUGIN_MENU_BUFSZ, "t.png");

    /*
     * Menus can be enabled or disabled with:
     * ts3Functions.setPluginMenuEnabled(pluginID, menuID, 0|1);
     * Test it with plugin command: /test enablemenu <menuID> <0|1>
     * Menus are enabled by default. Please note that shown menus will not
     * automatically enable or disable when calling this function to
     * ensure Qt menus are not modified by any thread other the UI thread. The
     * enabled or disable state will change the next time a
     * menu is displayed.
     */
    /* For example, this would disable MENU_ID_GLOBAL_2: */
    /* ts3Functions.setPluginMenuEnabled(pluginID, MENU_ID_GLOBAL_2, 0); */

    /* All memory allocated in this function will be automatically released by
     * the TeamSpeak client later by calling ts3plugin_freeMemory */
}

/* Helper function to create a hotkey */
static struct PluginHotkey *createHotkey(ccstring_t keyword,
                                         ccstring_t description) {
    struct PluginHotkey *hotkey =
        (struct PluginHotkey *) malloc(sizeof(struct PluginHotkey));
    _strcpy(hotkey->keyword, PLUGIN_HOTKEY_BUFSZ, keyword);
    _strcpy(hotkey->description, PLUGIN_HOTKEY_BUFSZ, description);
    return hotkey;
}

/* Some makros to make the code to create hotkeys a bit more readable */
#define BEGIN_CREATE_HOTKEYS(x)                                         \
    const size_t sz = x + 1;                                            \
    size_t n = 0;                                                       \
    *hotkeys =                                                          \
                 (struct PluginHotkey **) malloc(sizeof(struct PluginHotkey *) * sz);
#define CREATE_HOTKEY(a, b) (*hotkeys)[n++] = createHotkey(a, b);
#define END_CREATE_HOTKEYS                      \
    (*hotkeys)[n++] = NULL;                     \
    assert(n == sz);

/*
 * Initialize plugin hotkeys. If your plugin does not use this feature, this
 * function can be omitted.
 * Hotkeys require ts3plugin_registerPluginID and ts3plugin_freeMemory to be
 * implemented.
 * This function is automatically called by the client after ts3plugin_init.
 */
void ts3plugin_initHotkeys(struct PluginHotkey ***hotkeys) {
    /* Register hotkeys giving a keyword and a description.
     * The keyword will be later passed to ts3plugin_onHotkeyEvent to identify
     * which hotkey was triggered.
     * The description is shown in the clients hotkey dialog. */
    BEGIN_CREATE_HOTKEYS(
        3); /* Create 3 hotkeys. Size must be correct for allocating memory. */
    CREATE_HOTKEY("keyword_1", "Test hotkey 1");
    CREATE_HOTKEY("keyword_2", "Test hotkey 2");
    CREATE_HOTKEY("keyword_3", "Test hotkey 3");
    END_CREATE_HOTKEYS;

    /* The client will call ts3plugin_freeMemory to release all allocated memory
     */
}

/************************** TeamSpeak callbacks ***************************/
/*
 * Following functions are optional, feel free to remove unused callbacks.
 * See the clientlib documentation for details on each function.
 */

/* Clientlib */

void ts3plugin_onConnectStatusChangeEvent(uint64_t schandlerID,
                                          int32_t newStatus,
                                          uint32_t errorNumber) {}

void ts3plugin_onNewChannelEvent(uint64_t schandlerID,
                                 uint64_t channelID,
                                 uint64_t channelParentID) {
    Json::Value root;
    root["tag"] = "NewChannel";
    root["_NewChannel_schandlerID"] = (int) schandlerID;
    root["_NewChannel_channelParentID"] = (int) channelParentID;
    root["_NewChannel_channelID"] = (int) channelID;
    rpc_server->send_event(root);
}

void ts3plugin_onNewChannelCreatedEvent(uint64_t schandlerID,
                                        uint64_t channelID,
                                        uint64_t channelParentID,
                                        ident_t invokerID,
                                        ccstring_t invokerName,
                                        ccstring_t invokerUID) {
    Json::Value root;
    root["tag"] = "NewChannelCreated";
    root["_NewChannelCreated_schandlerID"] = (int) schandlerID;
    root["_NewChannelCreated_channelID"] = (int) channelID;
    root["_NewChannelCreated_cparentID"] = (int) channelParentID;
    root["_NewChannelCreated_invokerID"] = invokerID;
    root["_NewChannelCreated_invokerName"] = invokerName;
    root["_NewChannelCreated_invokerUID"] = invokerUID;
    rpc_server->send_event(root);
}

void ts3plugin_onDelChannelEvent(uint64_t schandlerID,
                                 uint64_t channelID,
                                 ident_t invokerID,
                                 ccstring_t invokerName,
                                 ccstring_t invokerUID) {}

void ts3plugin_onChannelMoveEvent(uint64_t schandlerID,
                                  uint64_t channelID,
                                  uint64_t newChannelParentID,
                                  ident_t invokerID,
                                  ccstring_t invokerName,
                                  ccstring_t invokerUID) {}

void ts3plugin_onUpdateChannelEvent(uint64_t schandlerID, uint64_t channelID) {}

void ts3plugin_onUpdateChannelEditedEvent(uint64_t schandlerID,
                                          uint64_t channelID,
                                          ident_t invokerID,
                                          ccstring_t invokerName,
                                          ccstring_t invokerUID) {}

void ts3plugin_onUpdateClientEvent(uint64_t schandlerID,
                                   ident_t clientID,
                                   ident_t invokerID,
                                   ccstring_t invokerName,
                                   ccstring_t invokerUID) {}

void ts3plugin_onClientMoveEvent(uint64_t schandlerID,
                                 ident_t clientID,
                                 uint64_t oldChannelID,
                                 uint64_t newChannelID,
                                 int32_t visibility,
                                 ccstring_t moveMessage) {}

void ts3plugin_onClientMoveSubscriptionEvent(uint64_t schandlerID,
                                             ident_t clientID,
                                             uint64_t oldChannelID,
                                             uint64_t newChannelID,
                                             int32_t visibility) {}

void ts3plugin_onClientMoveTimeoutEvent(uint64_t schandlerID,
                                        ident_t clientID,
                                        uint64_t oldChannelID,
                                        uint64_t newChannelID,
                                        int32_t visibility,
                                        ccstring_t timeoutMessage) {}

void ts3plugin_onClientMoveMovedEvent(uint64_t schandlerID,
                                      ident_t clientID,
                                      uint64_t oldChannelID,
                                      uint64_t newChannelID,
                                      int32_t visibility,
                                      ident_t moverID,
                                      ccstring_t moverName,
                                      ccstring_t moverUID,
                                      ccstring_t moveMessage) {}

void ts3plugin_onClientKickFromChannelEvent(uint64_t schandlerID,
                                            ident_t clientID,
                                            uint64_t oldChannelID,
                                            uint64_t newChannelID,
                                            int32_t visibility,
                                            ident_t kickerID,
                                            ccstring_t kickerName,
                                            ccstring_t kickerUID,
                                            ccstring_t kickMessage) {}

void ts3plugin_onClientKickFromServerEvent(uint64_t schandlerID,
                                           ident_t clientID,
                                           uint64_t oldChannelID,
                                           uint64_t newChannelID,
                                           int32_t visibility,
                                           ident_t kickerID,
                                           ccstring_t kickerName,
                                           ccstring_t kickerUID,
                                           ccstring_t kickMessage) {}

void ts3plugin_onClientIDsEvent(uint64_t schandlerID,
                                ccstring_t clientUID,
                                ident_t clientID,
                                ccstring_t clientName) {}

void ts3plugin_onClientIDsFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onServerEditedEvent(uint64_t schandlerID,
                                   ident_t editorID,
                                   ccstring_t editorName,
                                   ccstring_t editorUID) {}

void ts3plugin_onServerUpdatedEvent(uint64_t schandlerID) {}

int32_t ts3plugin_onServerErrorEvent(uint64_t schandlerID,
                                     ccstring_t errorMessage,
                                     uint32_t error,
                                     ccstring_t returnCode,
                                     ccstring_t extraMessage) {
    printf("PLUGIN: onServerErrorEvent %llu %s %d %s\n",
           (long long unsigned int) schandlerID,
           errorMessage,
           error,
           (returnCode ? returnCode : ""));
    if(returnCode) {
        /* A plugin could now check the returnCode with previously (when calling
         * a function) remembered returnCodes and react accordingly */
        /* In case of using a a plugin return code, the plugin can return:
         * 0: Client will continue handling this error (print32_t to chat tab)
         * 1: Client will ignore this error, the plugin announces it has handled
         * it */
        return 1;
    }
    return 0; /* If no plugin return code was used, the return value of this
                 function is ignored */
}

void ts3plugin_onServerStopEvent(uint64_t schandlerID,
                                 ccstring_t shutdownMessage) {}

//! Triggered when a text message event is received
//!
//! A return value of 0 means that the message should be handled normally, while
//! a return value of 1 means that the client should ignore the message.
int32_t ts3plugin_onTextMessageEvent(uint64_t schandlerID,
                                     ident_t targetMode,
                                     ident_t toID,
                                     ident_t fromID,
                                     ccstring_t fromName,
                                     ccstring_t fromUID,
                                     ccstring_t message,
                                     int32_t ffIgnored) {
    Json::Value root;
    root["tag"] = "TextMessage";
    root["schandlerID"] = (int) schandlerID;
    root["targetMode"] = targetMode;
    root["toID"] = toID;
    root["fromID"] = fromID;
    root["fromName"] = fromName;
    root["fromUID"] = fromUID;
    root["message"] = message;
    root["ffIgnored"] = ffIgnored;
    rpc_server->send_event(root);
    return 0;
}

void ts3plugin_onTalkStatusChangeEvent(uint64_t schandlerID,
                                       int32_t status,
                                       int32_t isReceivedWhisper,
                                       ident_t clientID) {
    /* Demonstrate usage of getClientDisplayName */
    char name[512];
    if(ts3Functions.getClientDisplayName(schandlerID, clientID, name, 512) ==
       ERROR_ok) {
        char buffer[100];
        Json::Value root;
        root["name"] = name;
        root["event"] = "TalkStatusChange";
        root["status"] = status;
        if(status == STATUS_TALKING) {
            snprintf(buffer, 100, "--> %s starts talking", name);
        } else {
            snprintf(buffer, 100, "--> %s stops talking", name);
        }
        rpc_server->send_event(root);
    }
}

void ts3plugin_onConnectionInfoEvent(uint64_t schandlerID, ident_t clientID) {}

void ts3plugin_onServerConnectionInfoEvent(uint64_t schandlerID) {}

void ts3plugin_onChannelSubscribeEvent(uint64_t schandlerID,
                                       uint64_t channelID) {}

void ts3plugin_onChannelSubscribeFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onChannelUnsubscribeEvent(uint64_t schandlerID,
                                         uint64_t channelID) {}

void ts3plugin_onChannelUnsubscribeFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onChannelDescriptionUpdateEvent(uint64_t schandlerID,
                                               uint64_t channelID) {}

void ts3plugin_onChannelPasswordChangedEvent(uint64_t schandlerID,
                                             uint64_t channelID) {}

void ts3plugin_onPlaybackShutdownCompleteEvent(uint64_t schandlerID) {}

void ts3plugin_onSoundDeviceListChangedEvent(ccstring_t modeID,
                                             int32_t playOrCap) {}

void ts3plugin_onEditPlaybackVoiceDataEvent(uint64_t schandlerID,
                                            ident_t clientID,
                                            short *samples,
                                            int32_t sampleCount,
                                            int32_t channels) {}

void ts3plugin_onEditPostProcessVoiceDataEvent(uint64_t schandlerID,
                                               ident_t clientID,
                                               short *samples,
                                               int32_t sampleCount,
                                               int32_t channels,
                                               const uint32_t* speakers,
                                               uint32_t* fillMask) {}

void ts3plugin_onEditMixedPlaybackVoiceDataEvent(
    uint64_t schandlerID,
    short *samples,
    int32_t sampleCount,
    int32_t channels,
    const uint32_t *channelSpeakerArray,
    uint32_t *channelFillMask) {}

void ts3plugin_onEditCapturedVoiceDataEvent(uint64_t schandlerID,
                                            short *samples,
                                            int32_t sampleCount,
                                            int32_t channels,
                                            int32_t *edited) {}

void ts3plugin_onCustom3dRolloffCalculationClientEvent(uint64_t schandlerID,
                                                       ident_t clientID,
                                                       float distance,
                                                       float *volume) {}

void ts3plugin_onCustom3dRolloffCalculationWaveEvent(uint64_t schandlerID,
                                                     uint64_t waveHandle,
                                                     float distance,
                                                     float *volume) {}

void ts3plugin_onUserLoggingMessageEvent(ccstring_t logMessage,
                                         int32_t logLevel,
                                         ccstring_t logChannel,
                                         uint64_t logID,
                                         ccstring_t logTime,
                                         ccstring_t completeLogString) {}

/* Clientlib rare */

void ts3plugin_onClientBanFromServerEvent(uint64_t schandlerID,
                                          ident_t clientID,
                                          uint64_t oldChannelID,
                                          uint64_t newChannelID,
                                          int32_t visibility,
                                          ident_t kickerID,
                                          ccstring_t kickerName,
                                          ccstring_t kickerUID,
                                          uint64_t time,
                                          ccstring_t kickMessage) {}

int32_t ts3plugin_onClientPokeEvent(uint64_t schandlerID,
                                    ident_t fromClientID,
                                    ccstring_t pokerName,
                                    ccstring_t pokerUID,
                                    ccstring_t message,
                                    int32_t ffIgnored) {
    Json::Value root;
    root["event"] = "ClientPoke";
    root["schandlerID"] = (int) schandlerID;
    root["fromClientID"] = fromClientID;
    root["pokerName"] = pokerName;
    root["message"] = message;
    rpc_server->send_event(root);
    return 0; /* 0 = handle normally, 1 = client will ignore the poke */
}

void ts3plugin_onClientSelfVariableUpdateEvent(uint64_t schandlerID,
                                               int32_t flag,
                                               ccstring_t oldValue,
                                               ccstring_t newValue) {}

void ts3plugin_onFileListEvent(uint64_t schandlerID,
                               uint64_t channelID,
                               ccstring_t path,
                               ccstring_t name,
                               uint64_t size,
                               uint64_t datetime,
                               int32_t type,
                               uint64_t incompletesize,
                               ccstring_t returnCode) {}

void ts3plugin_onFileListFinishedEvent(uint64_t schandlerID,
                                       uint64_t channelID,
                                       ccstring_t path) {}

void ts3plugin_onFileInfoEvent(uint64_t schandlerID,
                               uint64_t channelID,
                               ccstring_t name,
                               uint64_t size,
                               uint64_t datetime) {}

void ts3plugin_onServerGroupListEvent(uint64_t schandlerID,
                                      uint64_t serverGroupID,
                                      ccstring_t name,
                                      int32_t type,
                                      int32_t iconID,
                                      int32_t saveDB) {}

void ts3plugin_onServerGroupListFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onServerGroupByClientIDEvent(uint64_t schandlerID,
                                            ccstring_t name,
                                            uint64_t serverGroupList,
                                            uint64_t clientDBID) {}

void ts3plugin_onServerGroupPermListEvent(uint64_t schandlerID,
                                          uint64_t serverGroupID,
                                          uint32_t permissionID,
                                          int32_t permissionValue,
                                          int32_t permissionNegated,
                                          int32_t permissionSkip) {}

void ts3plugin_onServerGroupPermListFinishedEvent(uint64_t schandlerID,
                                                  uint64_t serverGroupID) {}

void ts3plugin_onServerGroupClientListEvent(uint64_t schandlerID,
                                            uint64_t serverGroupID,
                                            uint64_t clientDBID,
                                            ccstring_t clientNameIdentifier,
                                            ccstring_t clientUniqueID) {}

void ts3plugin_onChannelGroupListEvent(uint64_t schandlerID,
                                       uint64_t channelGroupID,
                                       ccstring_t name,
                                       int32_t type,
                                       int32_t iconID,
                                       int32_t saveDB) {}

void ts3plugin_onChannelGroupListFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onChannelGroupPermListEvent(uint64_t schandlerID,
                                           uint64_t channelGroupID,
                                           uint32_t permissionID,
                                           int32_t permissionValue,
                                           int32_t permissionNegated,
                                           int32_t permissionSkip) {}

void ts3plugin_onChannelGroupPermListFinishedEvent(uint64_t schandlerID,
                                                   uint64_t channelGroupID) {}

void ts3plugin_onChannelPermListEvent(uint64_t schandlerID,
                                      uint64_t channelID,
                                      uint32_t permissionID,
                                      int32_t permissionValue,
                                      int32_t permissionNegated,
                                      int32_t permissionSkip) {}

void ts3plugin_onChannelPermListFinishedEvent(uint64_t schandlerID,
                                              uint64_t channelID) {}

void ts3plugin_onClientPermListEvent(uint64_t schandlerID,
                                     uint64_t clientDBID,
                                     uint32_t permissionID,
                                     int32_t permissionValue,
                                     int32_t permissionNegated,
                                     int32_t permissionSkip) {}

void ts3plugin_onClientPermListFinishedEvent(uint64_t schandlerID,
                                             uint64_t clientDBID) {}

void ts3plugin_onChannelClientPermListEvent(uint64_t schandlerID,
                                            uint64_t channelID,
                                            uint64_t clientDBID,
                                            uint32_t permissionID,
                                            int32_t permissionValue,
                                            int32_t permissionNegated,
                                            int32_t permissionSkip) {}

void ts3plugin_onChannelClientPermListFinishedEvent(uint64_t schandlerID,
                                                    uint64_t channelID,
                                                    uint64_t clientDBID) {}

void ts3plugin_onClientChannelGroupChangedEvent(uint64_t schandlerID,
                                                uint64_t channelGroupID,
                                                uint64_t channelID,
                                                ident_t clientID,
                                                ident_t invokerClientID,
                                                ccstring_t invokerName,
                                                ccstring_t invokerUID) {}

int32_t ts3plugin_onServerPermissionErrorEvent(uint64_t schandlerID,
                                               ccstring_t errorMessage,
                                               uint32_t error,
                                               ccstring_t returnCode,
                                               uint32_t failedPermissionID) {
    return 0; /* See onServerErrorEvent for return code description */
}

void ts3plugin_onPermissionListGroupEndIDEvent(uint64_t schandlerID,
                                               uint32_t groupEndID) {}

void ts3plugin_onPermissionListEvent(uint64_t schandlerID,
                                     uint32_t permissionID,
                                     ccstring_t permissionName,
                                     ccstring_t permissionDescription) {}

void ts3plugin_onPermissionListFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onPermissionOverviewEvent(uint64_t schandlerID,
                                         uint64_t clientDBID,
                                         uint64_t channelID,
                                         int32_t overviewType,
                                         uint64_t overviewID1,
                                         uint64_t overviewID2,
                                         uint32_t permissionID,
                                         int32_t permissionValue,
                                         int32_t permissionNegated,
                                         int32_t permissionSkip) {}

void ts3plugin_onPermissionOverviewFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onServerGroupClientAddedEvent(uint64_t schandlerID,
                                             ident_t clientID,
                                             ccstring_t clientName,
                                             ccstring_t clientUID,
                                             uint64_t serverGroupID,
                                             ident_t invokerClientID,
                                             ccstring_t invokerName,
                                             ccstring_t invokerUID) {}

void ts3plugin_onServerGroupClientDeletedEvent(uint64_t schandlerID,
                                               ident_t clientID,
                                               ccstring_t clientName,
                                               ccstring_t clientUID,
                                               uint64_t serverGroupID,
                                               ident_t invokerClientID,
                                               ccstring_t invokerName,
                                               ccstring_t invokerUID) {}

void ts3plugin_onClientNeededPermissionsEvent(uint64_t schandlerID,
                                              uint32_t permissionID,
                                              int32_t permissionValue) {}

void ts3plugin_onClientNeededPermissionsFinishedEvent(uint64_t schandlerID) {}

void ts3plugin_onFileTransferStatusEvent(ident_t transferID,
                                         uint32_t status,
                                         ccstring_t statusMessage,
                                         uint64_t remotefileSize,
                                         uint64_t schandlerID) {}

void ts3plugin_onClientChatClosedEvent(uint64_t schandlerID,
                                       ident_t clientID,
                                       ccstring_t clientUID) {}

void ts3plugin_onClientChatComposingEvent(uint64_t schandlerID,
                                          ident_t clientID,
                                          ccstring_t clientUID) {}

void ts3plugin_onServerLogEvent(uint64_t schandlerID, ccstring_t logMsg) {}

void ts3plugin_onServerLogFinishedEvent(uint64_t schandlerID,
                                        uint64_t lastPos,
                                        uint64_t fileSize) {}

void ts3plugin_onMessageListEvent(uint64_t schandlerID,
                                  uint64_t messageID,
                                  ccstring_t fromClientUID,
                                  ccstring_t subject,
                                  uint64_t timestamp,
                                  int32_t flagRead) {}

void ts3plugin_onMessageGetEvent(uint64_t schandlerID,
                                 uint64_t messageID,
                                 ccstring_t fromClientUID,
                                 ccstring_t subject,
                                 ccstring_t message,
                                 uint64_t timestamp) {}

void ts3plugin_onClientDBIDfromUIDEvent(uint64_t schandlerID,
                                        ccstring_t clientUID,
                                        uint64_t clientDBID) {}

void ts3plugin_onClientNamefromUIDEvent(uint64_t schandlerID,
                                        ccstring_t clientUID,
                                        uint64_t clientDBID,
                                        ccstring_t clientNickName) {}

void ts3plugin_onClientNamefromDBIDEvent(uint64_t schandlerID,
                                         ccstring_t clientUID,
                                         uint64_t clientDBID,
                                         ccstring_t clientNickName) {}

void ts3plugin_onComplainListEvent(uint64_t schandlerID,
                                   uint64_t targetClientDBID,
                                   ccstring_t targetClientNickName,
                                   uint64_t fromClientDBID,
                                   ccstring_t fromClientNickName,
                                   ccstring_t complainReason,
                                   uint64_t timestamp) {}

void ts3plugin_onBanListEvent(uint64_t schandlerID,
                              uint64_t banID,
                              ccstring_t ip,
                              ccstring_t name,
                              ccstring_t uid,
                              uint64_t creationTime,
                              uint64_t durationTime,
                              ccstring_t invokerName,
                              uint64_t invokercldbid,
                              ccstring_t invokeruid,
                              ccstring_t reason,
                              int32_t numberOfEnforcements,
                              ccstring_t lastNickName) {}

void ts3plugin_onClientServerQueryLoginPasswordEvent(
    uint64_t schandlerID, ccstring_t loginPassword) {}

void ts3plugin_onPluginCommandEvent(uint64_t schandlerID,
                                    ccstring_t pluginName,
                                    ccstring_t pluginCommand) {
    printf("ON PLUGIN COMMAND: %s %s\n", pluginName, pluginCommand);
}

void ts3plugin_onIncomingClientQueryEvent(uint64_t schandlerID,
                                          ccstring_t commandText) {}

void ts3plugin_onServerTemporaryPasswordListEvent(uint64_t schandlerID,
                                                  ccstring_t clientNickname,
                                                  ccstring_t clientUID,
                                                  ccstring_t description,
                                                  ccstring_t password,
                                                  uint64_t timestampStart,
                                                  uint64_t timestampEnd,
                                                  uint64_t targetChannelID,
                                                  ccstring_t targetChannelPW) {
}

/* Client UI callbacks */

/*
 * Called from client when an avatar image has been downloaded to or deleted
 * from cache.
 * This callback can be called spontaneously or in response to
 * ts3Functions.getAvatar()
 */
void ts3plugin_onAvatarUpdated(uint64_t schandlerID,
                               ident_t clientID,
                               ccstring_t avatarPath) {
    /* If avatarPath is NULL, the avatar got deleted */
    /* If not NULL, avatarPath contains the path to the avatar file in the
     * TS3Client cache */
}

/*
 * Called when a plugin menu item (see ts3plugin_initMenus) is triggered.
 * Optional function, when not using plugin menus, do not implement this.
 *
 * Parameters:
 * - schandlerID: ID of the current server tab
 * - type: Type of the menu (PLUGIN_MENU_TYPE_CHANNEL, PLUGIN_MENU_TYPE_CLIENT
 * or PLUGIN_MENU_TYPE_GLOBAL)
 * - menuItemID: Id used when creating the menu item
 * - selectedItemID: Channel or Client ID in the case of
 * PLUGIN_MENU_TYPE_CHANNEL and PLUGIN_MENU_TYPE_CLIENT. 0 for
 * PLUGIN_MENU_TYPE_GLOBAL.
 */
void ts3plugin_onMenuItemEvent(uint64_t schandlerID,
                               menu_type_t type,
                               int32_t menuItemID,
                               uint64_t selectedItemID) {}

/* This function is called if a plugin hotkey was pressed. Omit if hotkeys are
 * unused. */
void ts3plugin_onHotkeyEvent(ccstring_t keyword) {
    printf("PLUGIN: Hotkey event: %s\n", keyword);
    /* Identify the hotkey by keyword ("keyword_1", "keyword_2" or "keyword_3"
     * in this example) and handle here... */
}

/* Called when recording a hotkey has finished after calling
 * ts3Functions.requestHotkeyInputDialog */
void ts3plugin_onHotkeyRecordedEvent(ccstring_t keyword, ccstring_t key) {}

/* Called when client custom nickname changed */
void ts3plugin_onClientDisplayNameChanged(uint64_t schandlerID,
                                          ident_t clientID,
                                          ccstring_t displayName,
                                          ccstring_t clientUID) {}
