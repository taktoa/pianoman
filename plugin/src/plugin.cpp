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


#define _strcpy(dest, destSize, src) \
    { strncpy(dest, src, destSize-1); (dest)[destSize-1] = '\0'; }

#define PLUGIN_API_VERSION 20

#define PATH_BUFSIZE 512
#define COMMAND_BUFSIZE 128
#define INFODATA_BUFSIZE 128
#define SERVERINFO_BUFSIZE 256
#define CHANNELINFO_BUFSIZE 512
#define RETURNCODE_BUFSIZE 128


// -----------------------------------------------------------------------------
// -- Global variables ---------------------------------------------------------
// -----------------------------------------------------------------------------


static struct TS3Functions ts3Functions;
static char* pluginID = NULL;
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
              << " "
              << message
              << std::endl;
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
const char* ts3plugin_name() { return "ZeroMQ Plugin"; }

//! Plugin version.
const char* ts3plugin_version() { return "0.0.1"; }

//! Plugin API version.
//! Must be the same as the client's API major version, or else the plugin will
//! fail to load.
int ts3plugin_apiVersion() { return PLUGIN_API_VERSION; }

//! Plugin author.
const char* ts3plugin_author() { return "Remy Goldschmidt / Michael Bishop"; }

//! Plugin description.
const char* ts3plugin_description() {
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
int ts3plugin_init() {
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

    if(pluginID) { free(pluginID); pluginID = NULL; }

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
int ts3plugin_offersConfigure() {
    pluginLog_DEBUG("offersConfigure");
    return PLUGIN_OFFERS_NO_CONFIGURE;
}

//! Plugin might offer a configuration window.
//! If ts3plugin_offersConfigure returns 0, this function will not be called.
void ts3plugin_configure(void* handle, void* qParentWidget) {
    pluginLog_DEBUG("configure");
}

//! If the plugin wants to use error return codes, plugin commands, hotkeys or
//! menu items, it needs to register a command ID. This function will be
//! automatically called after the plugin was initialized.
//! If you don't use these features, this function can be omitted.
//! Note the passed pluginID parameter is no longer valid after calling this
//! function, so you must copy it and store it in the plugin.
void ts3plugin_registerPluginID(const char* id) {
    // FIXME: clean up
    const size_t sz = strlen(id) + 1;
    pluginID = (char*)malloc(sz * sizeof(char));
    // The id buffer will invalidate after exiting this function, so we copy it
    _strcpy(pluginID, sz, id);
    printf("PLUGIN: registerPluginID: %s\n", pluginID);
}

//! Plugin command keyword. Return NULL or "" if not used.
const char* ts3plugin_commandKeyword() { return "zeromq"; }

//! Plugin processes console command.
//!
//! Possible return values:
//!   * 0 --- Plugin handled the command.
//!   * 1 --- Plugin did not handle the command.
int ts3plugin_processCommand(uint64 schandlerID, const char* command) {
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
const char* ts3plugin_infoTitle() {
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
void ts3plugin_infoData(uint64              schandlerID,
                        uint64              id,
                        enum PluginItemType type,
                        char**              data) {
    data = NULL;
}

//! Required to release the memory for parameter "data" allocated
//! in ts3plugin_infoData and ts3plugin_initMenus.
void ts3plugin_freeMemory(void* data) {
    free(data);
}

//! If autoload is enabled, the plugin will request to always be automatically
//! loaded by the client unless the user manually disabled it in the plugin
//! dialog.
//!
//! This function is optional. If missing, no autoload is assumed.
//!
//! Possible return values:
//!   * 0 --- The plugin should not be autoloaded.
//!   * 1 --- The plugin should be autoloaded.
int ts3plugin_requestAutoload() { return 1; }

typedef enum PluginMenuType    menu_type_t;
typedef struct PluginMenuItem* menu_item_t;

//! Helper function to create a menu item
static menu_item_t createMenuItem(menu_type_t type,
                                  int32_t id,
                                  const char* text,
                                  const char* icon) {
    auto menuItem = (menu_item_t) malloc(sizeof(menu_item_t));
    menuItem->type = type;
    menuItem->id = id;
    _strcpy(menuItem->text, PLUGIN_MENU_BUFSZ, text);
    _strcpy(menuItem->icon, PLUGIN_MENU_BUFSZ, icon);
    return menuItem;
}

// Some macros to make the code to create menu items a bit more readable.

#define BEGIN_CREATE_MENUS(x)                                     \
    const size_t sz = x + 1;                                      \
    size_t n = 0;                                                 \
    *menuItems = (menu_item_t*) malloc(sizeof(menu_item_t) * sz);

#define CREATE_MENU_ITEM(a, b, c, d)                \
    (*menuItems)[n++] = createMenuItem(a, b, c, d);

#define END_CREATE_MENUS                       \
    (*menuItems)[n++] = NULL; assert(n == sz);

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
 * This function is called after ts3plugin_init and ts3plugin_registerPluginID. A pluginID is required for plugin menus to work.
 * Both ts3plugin_registerPluginID and ts3plugin_freeMemory must be implemented to use menus.
 * If plugin menus are not used by a plugin, do not implement this function or return NULL.
 */
void ts3plugin_initMenus(struct PluginMenuItem*** menuItems, char** menuIcon) {
    /*
     * Create the menus
     * There are three types of menu items:
     * - PLUGIN_MENU_TYPE_CLIENT:  Client context menu
     * - PLUGIN_MENU_TYPE_CHANNEL: Channel context menu
     * - PLUGIN_MENU_TYPE_GLOBAL:  "Plugins" menu in menu bar of main window
     *
     * Menu IDs are used to identify the menu item when ts3plugin_onMenuItemEvent is called
     *
     * The menu text is required, max length is 128 characters
     *
     * The icon is optional, max length is 128 characters. When not using icons, just pass an empty string.
     * Icons are loaded from a subdirectory in the TeamSpeak client plugins folder. The subdirectory must be named like the
     * plugin filename, without dll/so/dylib suffix
     * e.g. for "test_plugin.dll", icon "1.png" is loaded from <TeamSpeak 3 Client install dir>\plugins\test_plugin\1.png
     */

    BEGIN_CREATE_MENUS(7);  /* IMPORTANT: Number of menu items must be correct! */
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_CLIENT,  MENU_ID_CLIENT_1,  "Client item 1",  "1.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_CLIENT,  MENU_ID_CLIENT_2,  "Client item 2",  "2.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_1, "Channel item 1", "1.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_2, "Channel item 2", "2.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_CHANNEL, MENU_ID_CHANNEL_3, "Channel item 3", "3.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_GLOBAL,  MENU_ID_GLOBAL_1,  "Global item 1",  "1.png");
    CREATE_MENU_ITEM(PLUGIN_MENU_TYPE_GLOBAL,  MENU_ID_GLOBAL_2,  "Global item 2",  "2.png");
    END_CREATE_MENUS;  /* Includes an assert checking if the number of menu items matched */

    /*
     * Specify an optional icon for the plugin. This icon is used for the plugins submenu within context and main menus
     * If unused, set menuIcon to NULL
     */
    *menuIcon = (char*)malloc(PLUGIN_MENU_BUFSZ * sizeof(char));
    _strcpy(*menuIcon, PLUGIN_MENU_BUFSZ, "t.png");

    /*
     * Menus can be enabled or disabled with: ts3Functions.setPluginMenuEnabled(pluginID, menuID, 0|1);
     * Test it with plugin command: /test enablemenu <menuID> <0|1>
     * Menus are enabled by default. Please note that shown menus will not automatically enable or disable when calling this function to
     * ensure Qt menus are not modified by any thread other the UI thread. The enabled or disable state will change the next time a
     * menu is displayed.
     */
    /* For example, this would disable MENU_ID_GLOBAL_2: */
    /* ts3Functions.setPluginMenuEnabled(pluginID, MENU_ID_GLOBAL_2, 0); */

    /* All memory allocated in this function will be automatically released by the TeamSpeak client later by calling ts3plugin_freeMemory */
}

/* Helper function to create a hotkey */
static struct PluginHotkey* createHotkey(const char* keyword, const char* description) {
    struct PluginHotkey* hotkey = (struct PluginHotkey*)malloc(sizeof(struct PluginHotkey));
    _strcpy(hotkey->keyword, PLUGIN_HOTKEY_BUFSZ, keyword);
    _strcpy(hotkey->description, PLUGIN_HOTKEY_BUFSZ, description);
    return hotkey;
}

/* Some makros to make the code to create hotkeys a bit more readable */
#define BEGIN_CREATE_HOTKEYS(x) const size_t sz = x + 1; size_t n = 0; *hotkeys = (struct PluginHotkey**)malloc(sizeof(struct PluginHotkey*) * sz);
#define CREATE_HOTKEY(a, b) (*hotkeys)[n++] = createHotkey(a, b);
#define END_CREATE_HOTKEYS (*hotkeys)[n++] = NULL; assert(n == sz);

/*
 * Initialize plugin hotkeys. If your plugin does not use this feature, this function can be omitted.
 * Hotkeys require ts3plugin_registerPluginID and ts3plugin_freeMemory to be implemented.
 * This function is automatically called by the client after ts3plugin_init.
 */
void ts3plugin_initHotkeys(struct PluginHotkey*** hotkeys) {
    /* Register hotkeys giving a keyword and a description.
     * The keyword will be later passed to ts3plugin_onHotkeyEvent to identify which hotkey was triggered.
     * The description is shown in the clients hotkey dialog. */
    BEGIN_CREATE_HOTKEYS(3);  /* Create 3 hotkeys. Size must be correct for allocating memory. */
    CREATE_HOTKEY("keyword_1", "Test hotkey 1");
    CREATE_HOTKEY("keyword_2", "Test hotkey 2");
    CREATE_HOTKEY("keyword_3", "Test hotkey 3");
    END_CREATE_HOTKEYS;

    /* The client will call ts3plugin_freeMemory to release all allocated memory */
}

/************************** TeamSpeak callbacks ***************************/
/*
 * Following functions are optional, feel free to remove unused callbacks.
 * See the clientlib documentation for details on each function.
 */

/* Clientlib */

void ts3plugin_onConnectStatusChangeEvent(uint64 schandlerID, int newStatus, unsigned int errorNumber) {
    /* Some example code following to show how to use the information query functions. */

    if(newStatus == STATUS_CONNECTION_ESTABLISHED) {  /* connection established and we have client and channels available */
        char* s;
        char msg[1024];
        anyID myID;
        uint64* ids;
        size_t i;
        unsigned int error;

        /* Print clientlib version */
        if(ts3Functions.getClientLibVersion(&s) == ERROR_ok) {
            printf("PLUGIN: Client lib version: %s\n", s);
            ts3Functions.freeMemory(s);  /* Release string */
        } else {
            ts3Functions.logMessage("Error querying client lib version", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }

        /* Write plugin name and version to log */
        snprintf(msg, sizeof(msg), "Plugin %s, Version %s, Author: %s", ts3plugin_name(), ts3plugin_version(), ts3plugin_author());
        ts3Functions.logMessage(msg, LogLevel_INFO, "Plugin", schandlerID);

        /* Print virtual server name */
        if((error = ts3Functions.getServerVariableAsString(schandlerID, VIRTUALSERVER_NAME, &s)) != ERROR_ok) {
            if(error != ERROR_not_connected) {  /* Don't spam error in this case (failed to connect) */
                ts3Functions.logMessage("Error querying server name", LogLevel_ERROR, "Plugin", schandlerID);
            }
            return;
        }
        printf("PLUGIN: Server name: %s\n", s);
        ts3Functions.freeMemory(s);

        /* Print virtual server welcome message */
        if(ts3Functions.getServerVariableAsString(schandlerID, VIRTUALSERVER_WELCOMEMESSAGE, &s) != ERROR_ok) {
            ts3Functions.logMessage("Error querying server welcome message", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }
        printf("PLUGIN: Server welcome message: %s\n", s);
        ts3Functions.freeMemory(s);  /* Release string */

        /* Print own client ID and nickname on this server */
        if(ts3Functions.getClientID(schandlerID, &myID) != ERROR_ok) {
            ts3Functions.logMessage("Error querying client ID", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }
        if(ts3Functions.getClientSelfVariableAsString(schandlerID, CLIENT_NICKNAME, &s) != ERROR_ok) {
            ts3Functions.logMessage("Error querying client nickname", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }
        printf("PLUGIN: My client ID = %d, nickname = %s\n", myID, s);
        ts3Functions.freeMemory(s);

        /* Print list of all channels on this server */
        if(ts3Functions.getChannelList(schandlerID, &ids) != ERROR_ok) {
            ts3Functions.logMessage("Error getting channel list", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }
        printf("PLUGIN: Available channels:\n");
        for(i=0; ids[i]; i++) {
            /* Query channel name */
            if(ts3Functions.getChannelVariableAsString(schandlerID, ids[i], CHANNEL_NAME, &s) != ERROR_ok) {
                ts3Functions.logMessage("Error querying channel name", LogLevel_ERROR, "Plugin", schandlerID);
                return;
            }
            printf("PLUGIN: Channel ID = %llu, name = %s\n", (long long unsigned int)ids[i], s);
            ts3Functions.freeMemory(s);
        }
        ts3Functions.freeMemory(ids);  /* Release array */

        /* Print list of existing server connection handlers */
        printf("PLUGIN: Existing server connection handlers:\n");
        if(ts3Functions.getServerConnectionHandlerList(&ids) != ERROR_ok) {
            ts3Functions.logMessage("Error getting server list", LogLevel_ERROR, "Plugin", schandlerID);
            return;
        }
        for(i=0; ids[i]; i++) {
            if((error = ts3Functions.getServerVariableAsString(ids[i], VIRTUALSERVER_NAME, &s)) != ERROR_ok) {
                if(error != ERROR_not_connected) {  /* Don't spam error in this case (failed to connect) */
                    ts3Functions.logMessage("Error querying server name", LogLevel_ERROR, "Plugin", schandlerID);
                }
                continue;
            }
            printf("- %llu - %s\n", (long long unsigned int)ids[i], s);
            ts3Functions.freeMemory(s);
        }
        ts3Functions.freeMemory(ids);
    }
}

void ts3plugin_onNewChannelEvent(uint64 schandlerID, uint64 channelID, uint64 channelParentID) {
    Json::Value root;
    root["tag"] = "NewChannel";
    root["_NewChannel_schandlerID"] = (int)schandlerID;
    root["_NewChannel_channelParentID"] = (int)channelParentID;
    root["_NewChannel_channelID"] = (int)channelID;
    rpc_server->send_event(root);
}

void ts3plugin_onNewChannelCreatedEvent(uint64 schandlerID, uint64 channelID, uint64 channelParentID, anyID invokerID, const char* invokerName, const char* invokerUniqueIdentifier) {
    Json::Value root;
    root["tag"] = "NewChannelCreated";
    root["_NewChannelCreated_schandlerID"] = (int)schandlerID;
    root["_NewChannelCreated_channelID"] = (int)channelID;
    root["_NewChannelCreated_cparentID"] = (int)channelParentID;
    root["_NewChannelCreated_invokerID"] = invokerID;
    root["_NewChannelCreated_invokerName"] = invokerName;
    root["_NewChannelCreated_invokerUID"] = invokerUniqueIdentifier;
    rpc_server->send_event(root);
}

void ts3plugin_onDelChannelEvent(uint64 schandlerID, uint64 channelID, anyID invokerID, const char* invokerName, const char* invokerUniqueIdentifier) {
}

void ts3plugin_onChannelMoveEvent(uint64 schandlerID, uint64 channelID, uint64 newChannelParentID, anyID invokerID, const char* invokerName, const char* invokerUniqueIdentifier) {
}

void ts3plugin_onUpdateChannelEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onUpdateChannelEditedEvent(uint64 schandlerID, uint64 channelID, anyID invokerID, const char* invokerName, const char* invokerUniqueIdentifier) {
}

void ts3plugin_onUpdateClientEvent(uint64 schandlerID, anyID clientID, anyID invokerID, const char* invokerName, const char* invokerUniqueIdentifier) {
}

void ts3plugin_onClientMoveEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, const char* moveMessage) {
}

void ts3plugin_onClientMoveSubscriptionEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility) {
}

void ts3plugin_onClientMoveTimeoutEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, const char* timeoutMessage) {
}

void ts3plugin_onClientMoveMovedEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, anyID moverID, const char* moverName, const char* moverUniqueIdentifier, const char* moveMessage) {
}

void ts3plugin_onClientKickFromChannelEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, anyID kickerID, const char* kickerName, const char* kickerUniqueIdentifier, const char* kickMessage) {
}

void ts3plugin_onClientKickFromServerEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, anyID kickerID, const char* kickerName, const char* kickerUniqueIdentifier, const char* kickMessage) {
}

void ts3plugin_onClientIDsEvent(uint64 schandlerID, const char* clientUID, anyID clientID, const char* clientName) {
}

void ts3plugin_onClientIDsFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onServerEditedEvent(uint64 schandlerID, anyID editerID, const char* editerName, const char* editerUniqueIdentifier) {
}

void ts3plugin_onServerUpdatedEvent(uint64 schandlerID) {
}

int ts3plugin_onServerErrorEvent(uint64 schandlerID, const char* errorMessage, unsigned int error, const char* returnCode, const char* extraMessage) {
    printf("PLUGIN: onServerErrorEvent %llu %s %d %s\n", (long long unsigned int)schandlerID, errorMessage, error, (returnCode ? returnCode : ""));
    if(returnCode) {
        /* A plugin could now check the returnCode with previously (when calling a function) remembered returnCodes and react accordingly */
        /* In case of using a a plugin return code, the plugin can return:
         * 0: Client will continue handling this error (print to chat tab)
         * 1: Client will ignore this error, the plugin announces it has handled it */
        return 1;
    }
    return 0;  /* If no plugin return code was used, the return value of this function is ignored */
}

void ts3plugin_onServerStopEvent(uint64 schandlerID, const char* shutdownMessage) {
}

//! Triggered when a text message event is received
//!
//! A return value of 0 means that the message should be handled normally, while
//! a return value of 1 means that the client should ignore the message.
int ts3plugin_onTextMessageEvent(uint64      schandlerID,
                                 anyID       targetMode,
                                 anyID       toID,
                                 anyID       fromID,
                                 const char* fromName,
                                 const char* fromUniqueIdentifier,
                                 const char* message,
                                 int         ffIgnored) {
    Json::Value root;
    root["tag"]         = "TextMessage";
    root["schandlerID"] = (int) schandlerID;
    root["targetMode"]  = targetMode;
    root["toID"]        = toID;
    root["fromID"]      = fromID;
    root["fromName"]    = fromName;
    root["fromUID"]     = fromUniqueIdentifier;
    root["message"]     = message;
    root["ffIgnored"]   = ffIgnored;
    rpc_server->send_event(root);
    return 0;
}

void ts3plugin_onTalkStatusChangeEvent(uint64 schandlerID, int status, int isReceivedWhisper, anyID clientID) {
    /* Demonstrate usage of getClientDisplayName */
    char name[512];
    if(ts3Functions.getClientDisplayName(schandlerID, clientID, name, 512) == ERROR_ok) {
                char buffer[100];
        Json::Value root;
        root["name"] = name;
        root["event"] = "TalkStatusChange";
        root["status"] = status;
        if(status == STATUS_TALKING) {
            snprintf(buffer,100,"--> %s starts talking", name);
        } else {
            snprintf(buffer,100,"--> %s stops talking", name);
        }
        rpc_server->send_event(root);
    }
}

void ts3plugin_onConnectionInfoEvent(uint64 schandlerID, anyID clientID) {
}

void ts3plugin_onServerConnectionInfoEvent(uint64 schandlerID) {
}

void ts3plugin_onChannelSubscribeEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onChannelSubscribeFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onChannelUnsubscribeEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onChannelUnsubscribeFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onChannelDescriptionUpdateEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onChannelPasswordChangedEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onPlaybackShutdownCompleteEvent(uint64 schandlerID) {
}

void ts3plugin_onSoundDeviceListChangedEvent(const char* modeID, int playOrCap) {
}

void ts3plugin_onEditPlaybackVoiceDataEvent(uint64 schandlerID, anyID clientID, short* samples, int sampleCount, int channels) {
}

void ts3plugin_onEditPostProcessVoiceDataEvent(uint64 schandlerID, anyID clientID, short* samples, int sampleCount, int channels, const unsigned int* channelSpeakerArray, unsigned int* channelFillMask) {
}

void ts3plugin_onEditMixedPlaybackVoiceDataEvent(uint64 schandlerID, short* samples, int sampleCount, int channels, const unsigned int* channelSpeakerArray, unsigned int* channelFillMask) {
}

void ts3plugin_onEditCapturedVoiceDataEvent(uint64 schandlerID, short* samples, int sampleCount, int channels, int* edited) {
}

void ts3plugin_onCustom3dRolloffCalculationClientEvent(uint64 schandlerID, anyID clientID, float distance, float* volume) {
}

void ts3plugin_onCustom3dRolloffCalculationWaveEvent(uint64 schandlerID, uint64 waveHandle, float distance, float* volume) {
}

void ts3plugin_onUserLoggingMessageEvent(const char* logMessage, int logLevel, const char* logChannel, uint64 logID, const char* logTime, const char* completeLogString) {
}

/* Clientlib rare */

void ts3plugin_onClientBanFromServerEvent(uint64 schandlerID, anyID clientID, uint64 oldChannelID, uint64 newChannelID, int visibility, anyID kickerID, const char* kickerName, const char* kickerUniqueIdentifier, uint64 time, const char* kickMessage) {
}

int ts3plugin_onClientPokeEvent(uint64 schandlerID, anyID fromClientID, const char* pokerName, const char* pokerUniqueIdentity, const char* message, int ffIgnored) {
    anyID myID;
    char buffer[512];

    snprintf(buffer,512,"PLUGIN onClientPokeEvent: %llu %d %s %s %d", (long long unsigned int)schandlerID, fromClientID, pokerName, message, ffIgnored);
    Json::Value root;
    root["event"] = "ClientPoke";
    root["schandlerID"] = (int)schandlerID;
    root["fromClientID"] = fromClientID;
    root["pokerName"] = pokerName;
    root["message"] = message;
    //~rpc_server->send_event(root);

    /* Check if the Friend/Foe manager has already blocked this poke */
    if(ffIgnored) {
        return 0;  /* Client will block anyways, doesn't matter what we return */
    }

    /* Example code: Send text message back to poking client */
    if(ts3Functions.getClientID(schandlerID, &myID) != ERROR_ok) {  /* Get own client ID */
        ts3Functions.logMessage("Error querying own client id", LogLevel_ERROR, "Plugin", schandlerID);
        return 0;
    }
    if(fromClientID != myID) {  /* Don't reply when source is own client */
        if(ts3Functions.requestSendPrivateTextMsg(schandlerID, "Received your poke!", fromClientID, NULL) != ERROR_ok) {
            ts3Functions.logMessage("Error requesting send text message", LogLevel_ERROR, "Plugin", schandlerID);
        }
    }

    return 0;  /* 0 = handle normally, 1 = client will ignore the poke */
}

void ts3plugin_onClientSelfVariableUpdateEvent(uint64 schandlerID, int flag, const char* oldValue, const char* newValue) {
}

void ts3plugin_onFileListEvent(uint64 schandlerID, uint64 channelID, const char* path, const char* name, uint64 size, uint64 datetime, int type, uint64 incompletesize, const char* returnCode) {
}

void ts3plugin_onFileListFinishedEvent(uint64 schandlerID, uint64 channelID, const char* path) {
}

void ts3plugin_onFileInfoEvent(uint64 schandlerID, uint64 channelID, const char* name, uint64 size, uint64 datetime) {
}

void ts3plugin_onServerGroupListEvent(uint64 schandlerID, uint64 serverGroupID, const char* name, int type, int iconID, int saveDB) {
}

void ts3plugin_onServerGroupListFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onServerGroupByClientIDEvent(uint64 schandlerID, const char* name, uint64 serverGroupList, uint64 clientDatabaseID) {
}

void ts3plugin_onServerGroupPermListEvent(uint64 schandlerID, uint64 serverGroupID, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onServerGroupPermListFinishedEvent(uint64 schandlerID, uint64 serverGroupID) {
}

void ts3plugin_onServerGroupClientListEvent(uint64 schandlerID, uint64 serverGroupID, uint64 clientDatabaseID, const char* clientNameIdentifier, const char* clientUniqueID) {
}

void ts3plugin_onChannelGroupListEvent(uint64 schandlerID, uint64 channelGroupID, const char* name, int type, int iconID, int saveDB) {
}

void ts3plugin_onChannelGroupListFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onChannelGroupPermListEvent(uint64 schandlerID, uint64 channelGroupID, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onChannelGroupPermListFinishedEvent(uint64 schandlerID, uint64 channelGroupID) {
}

void ts3plugin_onChannelPermListEvent(uint64 schandlerID, uint64 channelID, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onChannelPermListFinishedEvent(uint64 schandlerID, uint64 channelID) {
}

void ts3plugin_onClientPermListEvent(uint64 schandlerID, uint64 clientDatabaseID, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onClientPermListFinishedEvent(uint64 schandlerID, uint64 clientDatabaseID) {
}

void ts3plugin_onChannelClientPermListEvent(uint64 schandlerID, uint64 channelID, uint64 clientDatabaseID, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onChannelClientPermListFinishedEvent(uint64 schandlerID, uint64 channelID, uint64 clientDatabaseID) {
}

void ts3plugin_onClientChannelGroupChangedEvent(uint64 schandlerID, uint64 channelGroupID, uint64 channelID, anyID clientID, anyID invokerClientID, const char* invokerName, const char* invokerUniqueIdentity) {
}

int ts3plugin_onServerPermissionErrorEvent(uint64 schandlerID, const char* errorMessage, unsigned int error, const char* returnCode, unsigned int failedPermissionID) {
    return 0;  /* See onServerErrorEvent for return code description */
}

void ts3plugin_onPermissionListGroupEndIDEvent(uint64 schandlerID, unsigned int groupEndID) {
}

void ts3plugin_onPermissionListEvent(uint64 schandlerID, unsigned int permissionID, const char* permissionName, const char* permissionDescription) {
}

void ts3plugin_onPermissionListFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onPermissionOverviewEvent(uint64 schandlerID, uint64 clientDatabaseID, uint64 channelID, int overviewType, uint64 overviewID1, uint64 overviewID2, unsigned int permissionID, int permissionValue, int permissionNegated, int permissionSkip) {
}

void ts3plugin_onPermissionOverviewFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onServerGroupClientAddedEvent(uint64 schandlerID, anyID clientID, const char* clientName, const char* clientUniqueIdentity, uint64 serverGroupID, anyID invokerClientID, const char* invokerName, const char* invokerUniqueIdentity) {
}

void ts3plugin_onServerGroupClientDeletedEvent(uint64 schandlerID, anyID clientID, const char* clientName, const char* clientUniqueIdentity, uint64 serverGroupID, anyID invokerClientID, const char* invokerName, const char* invokerUniqueIdentity) {
}

void ts3plugin_onClientNeededPermissionsEvent(uint64 schandlerID, unsigned int permissionID, int permissionValue) {
}

void ts3plugin_onClientNeededPermissionsFinishedEvent(uint64 schandlerID) {
}

void ts3plugin_onFileTransferStatusEvent(anyID transferID, unsigned int status, const char* statusMessage, uint64 remotefileSize, uint64 schandlerID) {
}

void ts3plugin_onClientChatClosedEvent(uint64 schandlerID, anyID clientID, const char* clientUniqueIdentity) {
}

void ts3plugin_onClientChatComposingEvent(uint64 schandlerID, anyID clientID, const char* clientUniqueIdentity) {
}

void ts3plugin_onServerLogEvent(uint64 schandlerID, const char* logMsg) {
}

void ts3plugin_onServerLogFinishedEvent(uint64 schandlerID, uint64 lastPos, uint64 fileSize) {
}

void ts3plugin_onMessageListEvent(uint64 schandlerID, uint64 messageID, const char* fromClientUniqueIdentity, const char* subject, uint64 timestamp, int flagRead) {
}

void ts3plugin_onMessageGetEvent(uint64 schandlerID, uint64 messageID, const char* fromClientUniqueIdentity, const char* subject, const char* message, uint64 timestamp) {
}

void ts3plugin_onClientDBIDfromUIDEvent(uint64 schandlerID, const char* clientUID, uint64 clientDatabaseID) {
}

void ts3plugin_onClientNamefromUIDEvent(uint64 schandlerID, const char* clientUID, uint64 clientDatabaseID, const char* clientNickName) {
}

void ts3plugin_onClientNamefromDBIDEvent(uint64 schandlerID, const char* clientUID, uint64 clientDatabaseID, const char* clientNickName) {
}

void ts3plugin_onComplainListEvent(uint64 schandlerID, uint64 targetClientDatabaseID, const char* targetClientNickName, uint64 fromClientDatabaseID, const char* fromClientNickName, const char* complainReason, uint64 timestamp) {
}

void ts3plugin_onBanListEvent(uint64 schandlerID, uint64 banid, const char* ip, const char* name, const char* uid, uint64 creationTime, uint64 durationTime, const char* invokerName,
                              uint64 invokercldbid, const char* invokeruid, const char* reason, int numberOfEnforcements, const char* lastNickName) {
}

void ts3plugin_onClientServerQueryLoginPasswordEvent(uint64 schandlerID, const char* loginPassword) {
}

void ts3plugin_onPluginCommandEvent(uint64 schandlerID, const char* pluginName, const char* pluginCommand) {
    printf("ON PLUGIN COMMAND: %s %s\n", pluginName, pluginCommand);
}

void ts3plugin_onIncomingClientQueryEvent(uint64 schandlerID, const char* commandText) {
}

void ts3plugin_onServerTemporaryPasswordListEvent(uint64 schandlerID, const char* clientNickname, const char* clientUID, const char* description, const char* password, uint64 timestampStart, uint64 timestampEnd, uint64 targetChannelID, const char* targetChannelPW) {
}

/* Client UI callbacks */

/*
 * Called from client when an avatar image has been downloaded to or deleted from cache.
 * This callback can be called spontaneously or in response to ts3Functions.getAvatar()
 */
void ts3plugin_onAvatarUpdated(uint64 schandlerID, anyID clientID, const char* avatarPath) {
    /* If avatarPath is NULL, the avatar got deleted */
    /* If not NULL, avatarPath contains the path to the avatar file in the TS3Client cache */
    if(avatarPath != NULL) {
        printf("onAvatarUpdated: %llu %d %s\n", (long long unsigned int)schandlerID, clientID, avatarPath);
    } else {
        printf("onAvatarUpdated: %llu %d - deleted\n", (long long unsigned int)schandlerID, clientID);
    }
}

/*
 * Called when a plugin menu item (see ts3plugin_initMenus) is triggered. Optional function, when not using plugin menus, do not implement this.
 *
 * Parameters:
 * - schandlerID: ID of the current server tab
 * - type: Type of the menu (PLUGIN_MENU_TYPE_CHANNEL, PLUGIN_MENU_TYPE_CLIENT or PLUGIN_MENU_TYPE_GLOBAL)
 * - menuItemID: Id used when creating the menu item
 * - selectedItemID: Channel or Client ID in the case of PLUGIN_MENU_TYPE_CHANNEL and PLUGIN_MENU_TYPE_CLIENT. 0 for PLUGIN_MENU_TYPE_GLOBAL.
 */
void ts3plugin_onMenuItemEvent(uint64 schandlerID, enum PluginMenuType type, int menuItemID, uint64 selectedItemID) {
    printf("PLUGIN: onMenuItemEvent: schandlerID=%llu, type=%d, menuItemID=%d, selectedItemID=%llu\n", (long long unsigned int)schandlerID, type, menuItemID, (long long unsigned int)selectedItemID);
    switch(type) {
        case PLUGIN_MENU_TYPE_GLOBAL:
            /* Global menu item was triggered. selectedItemID is unused and set to zero. */
            switch(menuItemID) {
                case MENU_ID_GLOBAL_1:
                    /* Menu global 1 was triggered */
                    break;
                case MENU_ID_GLOBAL_2:
                    /* Menu global 2 was triggered */
                    break;
                default:
                    break;
            }
            break;
        case PLUGIN_MENU_TYPE_CHANNEL:
            /* Channel contextmenu item was triggered. selectedItemID is the channelID of the selected channel */
            switch(menuItemID) {
                case MENU_ID_CHANNEL_1:
                    /* Menu channel 1 was triggered */
                    break;
                case MENU_ID_CHANNEL_2:
                    /* Menu channel 2 was triggered */
                    break;
                case MENU_ID_CHANNEL_3:
                    /* Menu channel 3 was triggered */
                    break;
                default:
                    break;
            }
            break;
        case PLUGIN_MENU_TYPE_CLIENT:
            /* Client contextmenu item was triggered. selectedItemID is the clientID of the selected client */
            switch(menuItemID) {
                case MENU_ID_CLIENT_1:
                    /* Menu client 1 was triggered */
                    break;
                case MENU_ID_CLIENT_2:
                    /* Menu client 2 was triggered */
                    break;
                default:
                    break;
            }
            break;
        default:
            break;
    }
}

/* This function is called if a plugin hotkey was pressed. Omit if hotkeys are unused. */
void ts3plugin_onHotkeyEvent(const char* keyword) {
    printf("PLUGIN: Hotkey event: %s\n", keyword);
    /* Identify the hotkey by keyword ("keyword_1", "keyword_2" or "keyword_3" in this example) and handle here... */
}

/* Called when recording a hotkey has finished after calling ts3Functions.requestHotkeyInputDialog */
void ts3plugin_onHotkeyRecordedEvent(const char* keyword, const char* key) {
}

/* Called when client custom nickname changed */
void ts3plugin_onClientDisplayNameChanged(uint64 schandlerID, anyID clientID, const char* displayName, const char* clientUID) {
}
