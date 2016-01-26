#include <iostream>
#include "rpc.hpp"
#include "plugin.h"
#include <stdint.h>
#include <limits.h>

using namespace std;

typedef anyID                ident_t;
typedef const char*          string_t;
typedef float                float_t;
typedef const uint32_t*      speakers_t;
typedef uint32_t*            fill_mask_t;
typedef int16_t*             samples_t;
typedef enum PluginMenuType  menu_it_t_t;

// FIXME: sort these sensibly
// FIXME: use separate random generators for different output types
// FIXME: make type aliases for more types

// Generate a random int8_t between 0 and max.
int8_t rint8(int8_t max) { return (rand() % max); }

fill_mask_t example_fillMask     = new uint32_t[1] { 5 };
speakers_t  example_speakerArray = new uint32_t[1] { 5 };
samples_t   example_voiceSamples = new int16_t[1]  { 5 };
int32_t*    example_voiceEdited  = new int32_t[1]  { 5 };
float_t*    example_volume       = new float[1]    { 5 };

ident_t     gen_schandlerID()             { return rint8(200); }
ident_t     gen_channelID()               { return rint8(200); }
string_t    gen_modeID()                  { return "modeID"; }
ident_t     gen_clientID()                { return rint8(200); }
string_t    gen_clientUID()               { return "clientUID"; }
string_t    gen_clientName()              { return "clientName"; }
uint64_t    gen_time()                    { return rint8(200); }
int32_t     gen_connectionStatus()        { return rint8(200); }
uint64_t    gen_connectionError()         { return rint8(200); }
int32_t     gen_visibility()              { return rint8(200); }
uint64_t    gen_serverError()             { return rint8(200); }
string_t    gen_permissionErrorMessage()  { return "permissionErrorMessage"; }
string_t    gen_serverErrorMessage()      { return "serverErrorMessage"; }
string_t    gen_serverErrorExtraMessage() { return "serverErrorExtraMessage"; }
string_t    gen_serverErrorReturnCode()   { return "serverErrorReturnCode"; }
string_t    gen_kickMessage()             { return "kickMessage"; }
string_t    gen_moveMessage()             { return "moveMessage"; }
string_t    gen_shutdownMessage()         { return "shutdownMessage"; }
string_t    gen_timeoutMessage()          { return "timeoutMessage"; }
string_t    gen_textMessage()             { return "textMessage"; }
int32_t     gen_ffIgnored()               { return rint8(200); }
int32_t     gen_isReceivedWhisper()       { return rint8(200); }
int32_t     gen_playOrCap()               { return rint8(200); }
int32_t     gen_talkStatus()              { return rint8(200); }
ident_t     gen_targetMode()              { return rint8(200); }
int32_t     gen_channels()                { return rint8(200); }
fill_mask_t gen_fillMask()                { return example_fillMask; }
speakers_t  gen_speakerArray()            { return example_speakerArray; }
samples_t   gen_voiceSamples()            { return example_voiceSamples; }
int32_t     gen_voiceSamplesCount()       { return rint8(200); }
float_t     gen_distance()                { return rint8(200); }
int32_t*    gen_voiceEdited()             { return example_voiceEdited; }
float_t*    gen_volume()                  { return example_volume; }
uint64_t    gen_waveHandle()              { return rint8(200); }
string_t    gen_userLogMessage()          { return "userLogMessage"; }
string_t    gen_userLogChannel()          { return "userLogChannel"; }
int64_t     gen_userLogLevel()            { return rint8(200); }
uint64_t    gen_userLogID()               { return rint8(200); }
string_t    gen_userLogTime()             { return "userLogTime"; }
string_t    gen_userLogString()           { return "userLogString"; }
uint64_t    gen_fileDate()                { return rint8(200); }
string_t    gen_fileName()                { return "fileName"; }
string_t    gen_filePath()                { return "filePath"; }
uint64_t    gen_fileProgress()            { return rint8(200); }
uint64_t    gen_fileSize()                { return rint8(200); }
int64_t     gen_fileType()                { return rint8(200); }
string_t    gen_pokeMessage()             { return "pokeMessage"; }
string_t    gen_returnCode()              { return "returnCode"; }
uint64_t    gen_serverGroupID()           { return rint8(200); }
int32_t     gen_variableFlag()            { return rint8(200); }
string_t    gen_variableNewValue()        { return "variableNewValue"; }
string_t    gen_variableOldValue()        { return "variableOldValue"; }
string_t    gen_channelGroupName()        { return "channelGroupName"; }
int32_t     gen_channelGroupType()        { return rint8(200); }
uint64_t    gen_clientDBID()              { return rint8(200); }
ident_t     gen_iconID()                  { return rint8(200); }
ident_t     gen_permissionID()            { return rint8(200); }
int32_t     gen_permissionNegated()       { return rint8(200); }
int32_t     gen_permissionSkip()          { return rint8(200); }
int32_t     gen_permissionValue()         { return rint8(200); }
int32_t     gen_saveDB()                  { return rint8(200); }
uint64_t    gen_serverGroupList()         { return rint8(200); }
string_t    gen_serverGroupName()         { return "serverGroupName"; }
int32_t     gen_serverGroupType()         { return rint8(200); }
uint64_t    gen_channelGroupID()          { return rint8(200); }
string_t    gen_serverLogMessage()        { return "serverLogMessage"; }
uint64_t    gen_serverLogLastPos()        { return rint8(200); }
uint32_t    gen_groupEndID()              { return rint8(200); }
uint64_t    gen_messageID()               { return rint8(200); }
uint64_t    gen_overviewID()              { return rint8(200); }
int32_t     gen_overviewType()            { return rint8(200); }
string_t    gen_permissionDescription()   { return "permissionDescription"; }
uint32_t    gen_permissionError()         { return rint8(200); }
string_t    gen_permissionName()          { return "permissionName"; }
string_t    gen_permissionReturnCode()    { return "permissionReturnCode"; }
uint64_t    gen_serverLogSize()           { return rint8(200); }
ident_t     gen_transferID()              { return rint8(200); }
uint64_t    gen_transferSize()            { return rint8(200); }
uint32_t    gen_transferStatus()          { return rint8(200); }
string_t    gen_transferStatusMessage()   { return "transferStatusMessage"; }
string_t    gen_clientNickName()          { return "clientNickName"; }
int32_t     gen_flagRead()                { return rint8(200); }
string_t    gen_messageContents()         { return "messageContents"; }
string_t    gen_messageSubject()          { return "messageSubject"; }
uint64_t    gen_timestamp()               { return rint8(200); }
uint64_t    gen_banID()                   { return rint8(200); }
string_t    gen_banReason()               { return "banReason"; }
string_t    gen_complainReason()          { return "complainReason"; }
string_t    gen_ipAddress()               { return "ipAddress"; }
int32_t     gen_numberOfEnforcements()    { return rint8(200); }
string_t    gen_password()                { return "password"; }
string_t    gen_pluginName()              { return "pluginName"; }
string_t    gen_avatarPath()              { return "avatarPath"; }
string_t    gen_commandText()             { return "commandText"; }
string_t    gen_displayName()             { return "displayName"; }
string_t    gen_key()                     { return "key"; }
string_t    gen_keyword()                 { return "keyword"; }
int32_t     gen_menuItemID()              { return rint8(200); }
menu_it_t_t gen_menuItemType()            { return (menu_it_t_t) rint8(2); }
string_t    gen_pluginCommand()           { return "pluginCommand"; }
uint64_t    gen_selectedItemID()          { return rint8(200); }

void integration_test() {
    // #### Required functions ####


    cout << "Plugin name:        " << ts3plugin_name()        << endl;
    cout << "Plugin version:     " << ts3plugin_version()     << endl;
    cout << "Plugin API version: " << ts3plugin_apiVersion()  << endl;
    cout << "Plugin author:      " << ts3plugin_author()      << endl;
    cout << "Plugin description: " << ts3plugin_description() << endl;


    // #### ClientLib ####


    ts3plugin_onConnectStatusChangeEvent(gen_schandlerID(),
                                         gen_connectionStatus(),
                                         gen_connectionError());
    ts3plugin_onNewChannelEvent(gen_schandlerID(),
                                gen_channelID(),
                                gen_channelID());
    ts3plugin_onNewChannelCreatedEvent(gen_schandlerID(),
                                       gen_channelID(),
                                       gen_channelID(),
                                       gen_clientID(),
                                       gen_clientName(),
                                       gen_clientUID());
    ts3plugin_onDelChannelEvent(gen_schandlerID(),
                                gen_channelID(),
                                gen_clientID(),
                                gen_clientName(),
                                gen_clientUID());
    ts3plugin_onChannelMoveEvent(gen_schandlerID(),
                                 gen_channelID(),
                                 gen_channelID(),
                                 gen_clientID(),
                                 gen_clientName(),
                                 gen_clientUID());
    ts3plugin_onUpdateChannelEvent(gen_schandlerID(),
                                   gen_channelID());
    ts3plugin_onUpdateChannelEditedEvent(gen_schandlerID(),
                                         gen_channelID(),
                                         gen_clientID(),
                                         gen_clientName(),
                                         gen_clientUID());
    ts3plugin_onUpdateClientEvent(gen_schandlerID(),
                                  gen_clientID(),
                                  gen_clientID(),
                                  gen_clientName(),
                                  gen_clientUID());
    ts3plugin_onClientMoveEvent(gen_schandlerID(),
                                gen_clientID(),
                                gen_channelID(),
                                gen_channelID(),
                                gen_visibility(),
                                gen_moveMessage());
    ts3plugin_onClientMoveSubscriptionEvent(gen_schandlerID(),
                                            gen_clientID(),
                                            gen_channelID(),
                                            gen_channelID(),
                                            gen_visibility());
    ts3plugin_onClientMoveTimeoutEvent(gen_schandlerID(),
                                       gen_clientID(),
                                       gen_channelID(),
                                       gen_channelID(),
                                       gen_visibility(),
                                       gen_timeoutMessage());
    ts3plugin_onClientMoveMovedEvent(gen_schandlerID(),
                                     gen_clientID(),
                                     gen_channelID(),
                                     gen_channelID(),
                                     gen_visibility(),
                                     gen_clientID(),
                                     gen_clientName(),
                                     gen_clientUID(),
                                     gen_moveMessage());
    ts3plugin_onClientKickFromChannelEvent(gen_schandlerID(),
                                           gen_clientID(),
                                           gen_channelID(),
                                           gen_channelID(),
                                           gen_visibility(),
                                           gen_clientID(),
                                           gen_clientName(),
                                           gen_clientUID(),
                                           gen_kickMessage());
    ts3plugin_onClientKickFromServerEvent(gen_schandlerID(),
                                          gen_clientID(),
                                          gen_channelID(),
                                          gen_channelID(),
                                          gen_visibility(),
                                          gen_clientID(),
                                          gen_clientName(),
                                          gen_clientUID(),
                                          gen_kickMessage());
    ts3plugin_onClientIDsEvent(gen_schandlerID(),
                               gen_clientUID(),
                               gen_clientID(),
                               gen_clientName());
    ts3plugin_onClientIDsFinishedEvent(gen_schandlerID());
    ts3plugin_onServerEditedEvent(gen_schandlerID(),
                                  gen_clientID(),
                                  gen_clientName(),
                                  gen_clientUID());
    ts3plugin_onServerUpdatedEvent(gen_schandlerID());
    // FIXME: generates int
    ts3plugin_onServerErrorEvent(gen_schandlerID(),
                                 gen_serverErrorMessage(),
                                 gen_serverError(),
                                 gen_serverErrorReturnCode(),
                                 gen_serverErrorExtraMessage());
    ts3plugin_onServerStopEvent(gen_schandlerID(),
                                gen_shutdownMessage());
    // FIXME: generates int
    ts3plugin_onTextMessageEvent(gen_schandlerID(),
                                 gen_targetMode(),
                                 gen_clientID(),
                                 gen_clientID(),
                                 gen_clientName(),
                                 gen_clientUID(),
                                 gen_textMessage(),
                                 gen_ffIgnored());
    ts3plugin_onTalkStatusChangeEvent(gen_schandlerID(),
                                      gen_talkStatus(),
                                      gen_isReceivedWhisper(),
                                      gen_clientID());
    ts3plugin_onConnectionInfoEvent(gen_schandlerID(),
                                    gen_clientID());
    ts3plugin_onServerConnectionInfoEvent(gen_schandlerID());
    ts3plugin_onChannelSubscribeEvent(gen_schandlerID(),
                                      gen_channelID());
    ts3plugin_onChannelSubscribeFinishedEvent(gen_schandlerID());
    ts3plugin_onChannelUnsubscribeEvent(gen_schandlerID(),
                                        gen_channelID());
    ts3plugin_onChannelUnsubscribeFinishedEvent(gen_schandlerID());
    ts3plugin_onChannelDescriptionUpdateEvent(gen_schandlerID(),
                                              gen_channelID());
    ts3plugin_onChannelPasswordChangedEvent(gen_schandlerID(),
                                            gen_channelID());
    ts3plugin_onPlaybackShutdownCompleteEvent(gen_schandlerID());
    ts3plugin_onSoundDeviceListChangedEvent(gen_modeID(),
                                            gen_playOrCap());
    ts3plugin_onEditPlaybackVoiceDataEvent(gen_schandlerID(),
                                           gen_clientID(),
                                           gen_voiceSamples(),
                                           gen_voiceSamplesCount(),
                                           gen_channels());
    ts3plugin_onEditPostProcessVoiceDataEvent(gen_schandlerID(),
                                              gen_clientID(),
                                              gen_voiceSamples(),
                                              gen_voiceSamplesCount(),
                                              gen_channels(),
                                              gen_speakerArray(),
                                              gen_fillMask());
    ts3plugin_onEditMixedPlaybackVoiceDataEvent(gen_schandlerID(),
                                                gen_voiceSamples(),
                                                gen_voiceSamplesCount(),
                                                gen_channels(),
                                                gen_speakerArray(),
                                                gen_fillMask());
    ts3plugin_onEditCapturedVoiceDataEvent(gen_schandlerID(),
                                           gen_voiceSamples(),
                                           gen_voiceSamplesCount(),
                                           gen_channels(),
                                           gen_voiceEdited());
    ts3plugin_onCustom3dRolloffCalculationClientEvent(gen_schandlerID(),
                                                      gen_clientID(),
                                                      gen_distance(),
                                                      gen_volume());
    ts3plugin_onCustom3dRolloffCalculationWaveEvent(gen_schandlerID(),
                                                    gen_waveHandle(),
                                                    gen_distance(),
                                                    gen_volume());
    ts3plugin_onUserLoggingMessageEvent(gen_userLogMessage(),
                                        gen_userLogLevel(),
                                        gen_userLogChannel(),
                                        gen_userLogID(),
                                        gen_userLogTime(),
                                        gen_userLogString());


    // #### ClientLib Rare ####


    ts3plugin_onClientBanFromServerEvent(gen_schandlerID(),
                                         gen_clientID(),
                                         gen_channelID(),
                                         gen_channelID(),
                                         gen_visibility(),
                                         gen_clientID(),
                                         gen_clientName(),
                                         gen_clientUID(),
                                         gen_time(),
                                         gen_kickMessage());
    // FIXME: generates int
    ts3plugin_onClientPokeEvent(gen_schandlerID(),
                                gen_clientID(),
                                gen_clientName(),
                                gen_clientUID(),
                                gen_pokeMessage(),
                                gen_ffIgnored());
    ts3plugin_onClientSelfVariableUpdateEvent(gen_schandlerID(),
                                              gen_variableFlag(),
                                              gen_variableOldValue(),
                                              gen_variableNewValue());
    ts3plugin_onFileListEvent(gen_schandlerID(),
                              gen_channelID(),
                              gen_filePath(),
                              gen_fileName(),
                              gen_fileSize(),
                              gen_fileDate(),
                              gen_fileType(),
                              gen_fileProgress(),
                              gen_returnCode());
    ts3plugin_onFileListFinishedEvent(gen_schandlerID(),
                                      gen_channelID(),
                                      gen_filePath());
    ts3plugin_onFileInfoEvent(gen_schandlerID(),
                              gen_channelID(),
                              gen_fileName(),
                              gen_fileSize(),
                              gen_fileDate());
    ts3plugin_onServerGroupListEvent(gen_schandlerID(),
                                     gen_serverGroupID(),
                                     gen_serverGroupName(),
                                     gen_serverGroupType(),
                                     gen_iconID(),
                                     gen_saveDB());
    ts3plugin_onServerGroupListFinishedEvent(gen_schandlerID());
    ts3plugin_onServerGroupByClientIDEvent(gen_schandlerID(),
                                           gen_clientName(),
                                           gen_serverGroupList(),
                                           gen_clientDBID());
    ts3plugin_onServerGroupPermListEvent(gen_schandlerID(),
                                         gen_serverGroupID(),
                                         gen_permissionID(),
                                         gen_permissionValue(),
                                         gen_permissionNegated(),
                                         gen_permissionSkip());
    ts3plugin_onServerGroupPermListFinishedEvent(gen_schandlerID(),
                                                 gen_serverGroupID());
    ts3plugin_onServerGroupClientListEvent(gen_schandlerID(),
                                           gen_serverGroupID(),
                                           gen_clientDBID(),
                                           gen_clientName(),
                                           gen_clientUID());
    ts3plugin_onChannelGroupListEvent(gen_schandlerID(),
                                      gen_channelGroupID(),
                                      gen_channelGroupName(),
                                      gen_channelGroupType(),
                                      gen_iconID(),
                                      gen_saveDB());
    ts3plugin_onChannelGroupListFinishedEvent(gen_schandlerID());
    ts3plugin_onChannelGroupPermListEvent(gen_schandlerID(),
                                          gen_channelGroupID(),
                                          gen_permissionID(),
                                          gen_permissionValue(),
                                          gen_permissionNegated(),
                                          gen_permissionSkip());
    ts3plugin_onChannelGroupPermListFinishedEvent(gen_schandlerID(),
                                                  gen_channelGroupID());
    ts3plugin_onChannelPermListEvent(gen_schandlerID(),
                                     gen_channelID(),
                                     gen_permissionID(),
                                     gen_permissionValue(),
                                     gen_permissionNegated(),
                                     gen_permissionSkip());
    ts3plugin_onChannelPermListFinishedEvent(gen_schandlerID(),
                                             gen_channelID());
    ts3plugin_onClientPermListEvent(gen_schandlerID(),
                                    gen_clientDBID(),
                                    gen_permissionID(),
                                    gen_permissionValue(),
                                    gen_permissionNegated(),
                                    gen_permissionSkip());
    ts3plugin_onClientPermListFinishedEvent(gen_schandlerID(),
                                            gen_clientDBID());
    ts3plugin_onChannelClientPermListEvent(gen_schandlerID(),
                                           gen_channelID(),
                                           gen_clientDBID(),
                                           gen_permissionID(),
                                           gen_permissionValue(),
                                           gen_permissionNegated(),
                                           gen_permissionSkip());
    ts3plugin_onChannelClientPermListFinishedEvent(gen_schandlerID(),
                                                   gen_channelID(),
                                                   gen_clientDBID());
    ts3plugin_onClientChannelGroupChangedEvent(gen_schandlerID(),
                                               gen_channelGroupID(),
                                               gen_channelID(),
                                               gen_clientID(),
                                               gen_clientID(),
                                               gen_clientName(),
                                               gen_clientUID());
    // FIXME: generates int
    ts3plugin_onServerPermissionErrorEvent(gen_schandlerID(),
                                           gen_permissionErrorMessage(),
                                           gen_permissionError(),
                                           gen_permissionReturnCode(),
                                           gen_permissionID());
    ts3plugin_onPermissionListGroupEndIDEvent(gen_schandlerID(),
                                              gen_groupEndID());
    ts3plugin_onPermissionListEvent(gen_schandlerID(),
                                    gen_permissionID(),
                                    gen_permissionName(),
                                    gen_permissionDescription());
    ts3plugin_onPermissionListFinishedEvent(gen_schandlerID());
    ts3plugin_onPermissionOverviewEvent(gen_schandlerID(),
                                        gen_clientDBID(),
                                        gen_channelID(),
                                        gen_overviewType(),
                                        gen_overviewID(),
                                        gen_overviewID(),
                                        gen_permissionID(),
                                        gen_permissionValue(),
                                        gen_permissionNegated(),
                                        gen_permissionSkip());
    ts3plugin_onPermissionOverviewFinishedEvent(gen_schandlerID());
    ts3plugin_onServerGroupClientAddedEvent(gen_schandlerID(),
                                            gen_clientID(),
                                            gen_clientName(),
                                            gen_clientUID(),
                                            gen_serverGroupID(),
                                            gen_clientID(),
                                            gen_clientName(),
                                            gen_clientUID());
    ts3plugin_onServerGroupClientDeletedEvent(gen_schandlerID(),
                                              gen_clientID(),
                                              gen_clientName(),
                                              gen_clientUID(),
                                              gen_serverGroupID(),
                                              gen_clientID(),
                                              gen_clientName(),
                                              gen_clientUID());
    ts3plugin_onClientNeededPermissionsEvent(gen_schandlerID(),
                                             gen_permissionID(),
                                             gen_permissionValue());
    ts3plugin_onClientNeededPermissionsFinishedEvent(gen_schandlerID());
    ts3plugin_onFileTransferStatusEvent(gen_transferID(),
                                        gen_transferStatus(),
                                        gen_transferStatusMessage(),
                                        gen_transferSize(),
                                        gen_schandlerID());
    ts3plugin_onClientChatClosedEvent(gen_schandlerID(),
                                      gen_clientID(),
                                      gen_clientUID());
    ts3plugin_onClientChatComposingEvent(gen_schandlerID(),
                                         gen_clientID(),
                                         gen_clientUID());
    ts3plugin_onServerLogEvent(gen_schandlerID(),
                               gen_serverLogMessage());
    ts3plugin_onServerLogFinishedEvent(gen_schandlerID(),
                                       gen_serverLogLastPos(),
                                       gen_serverLogSize());
    ts3plugin_onMessageListEvent(gen_schandlerID(),
                                 gen_messageID(),
                                 gen_clientUID(),
                                 gen_messageSubject(),
                                 gen_timestamp(),
                                 gen_flagRead());
    ts3plugin_onMessageGetEvent(gen_schandlerID(),
                                gen_messageID(),
                                gen_clientUID(),
                                gen_messageSubject(),
                                gen_messageContents(),
                                gen_timestamp());
    ts3plugin_onClientDBIDfromUIDEvent(gen_schandlerID(),
                                       gen_clientUID(),
                                       gen_clientDBID());
    ts3plugin_onClientNamefromUIDEvent(gen_schandlerID(),
                                       gen_clientUID(),
                                       gen_clientDBID(),
                                       gen_clientNickName());
    ts3plugin_onClientNamefromDBIDEvent(gen_schandlerID(),
                                        gen_clientUID(),
                                        gen_clientDBID(),
                                        gen_clientNickName());
    ts3plugin_onComplainListEvent(gen_schandlerID(),
                                  gen_clientDBID(),
                                  gen_clientNickName(),
                                  gen_clientDBID(),
                                  gen_clientNickName(),
                                  gen_complainReason(),
                                  gen_timestamp());
    ts3plugin_onBanListEvent(gen_schandlerID(),
                             gen_banID(),
                             gen_ipAddress(),
                             gen_clientName(),
                             gen_clientUID(),
                             gen_time(),
                             gen_time(),
                             gen_clientName(),
                             gen_clientDBID(),
                             gen_clientUID(),
                             gen_banReason(),
                             gen_numberOfEnforcements(),
                             gen_clientNickName());
    ts3plugin_onClientServerQueryLoginPasswordEvent(gen_schandlerID(),
                                                    gen_password());
    ts3plugin_onPluginCommandEvent(gen_schandlerID(),
                                   gen_pluginName(),
                                   gen_pluginCommand());
    ts3plugin_onIncomingClientQueryEvent(gen_schandlerID(),
                                         gen_commandText());
    ts3plugin_onServerTemporaryPasswordListEvent(gen_schandlerID(),
                                                 gen_clientNickName(),
                                                 gen_clientUID(),
                                                 "temporary password list",
                                                 gen_password(),
                                                 gen_timestamp(),
                                                 gen_timestamp(),
                                                 gen_channelID(),
                                                 gen_password());


    // #### Client UI callbacks ####


    ts3plugin_onAvatarUpdated(gen_schandlerID(),
                              gen_clientID(),
                              gen_avatarPath());
    ts3plugin_onMenuItemEvent(gen_schandlerID(),
                              gen_menuItemType(),
                              gen_menuItemID(),
                              gen_selectedItemID());
    ts3plugin_onHotkeyEvent(gen_keyword());
    ts3plugin_onHotkeyRecordedEvent(gen_keyword(),
                                    gen_key());
    ts3plugin_onClientDisplayNameChanged(gen_schandlerID(),
                                         gen_clientID(),
                                         gen_displayName(),
                                         gen_clientUID());
}

template < typename... Ts >
uint32_t ui32_noop(Ts...) { return 1; }

template < typename... Ts >
uint64_t ui64_noop(Ts...) { return 1; }

template < typename... Ts >
void void_noop(Ts...) {}

const struct TS3Functions noopFunctions = {
    // # General
    ui32_noop, // unsigned int (*getClientLibVersion)(char** result);
    ui32_noop, // unsigned int (*getClientLibVersionNumber)(uint64* result);
    ui32_noop, // unsigned int (*spawnNewServerConnectionHandler)(int port, uint64* result);
    ui32_noop, // unsigned int (*destroyServerConnectionHandler)(uint64 schandlerID);

    // # Error handling
    ui32_noop, // unsigned int (*getErrorMessage)(unsigned int errorCode, char** error);

    // # Memory management
    ui32_noop, // unsigned int (*freeMemory)(void* pointer);

    // # Logging
    ui32_noop, // unsigned int (*logMessage)(string_t logMessage, enum LogLevel severity, string_t channel, uint64 logID);

    // # Sound
    ui32_noop, // unsigned int (*getPlaybackDeviceList)(string_t modeID, char**** result);
    ui32_noop, // unsigned int (*getPlaybackModeList)(char*** result);
    ui32_noop, // unsigned int (*getCaptureDeviceList)(string_t modeID, char**** result);
    ui32_noop, // unsigned int (*getCaptureModeList)(char*** result);
    ui32_noop, // unsigned int (*getDefaultPlaybackDevice)(string_t modeID, char*** result);
    ui32_noop, // unsigned int (*getDefaultPlayBackMode)(char** result);
    ui32_noop, // unsigned int (*getDefaultCaptureDevice)(string_t modeID, char*** result);
    ui32_noop, // unsigned int (*getDefaultCaptureMode)(char** result);
    ui32_noop, // unsigned int (*openPlaybackDevice)(uint64 schandlerID, string_t modeID, string_t playbackDevice);
    ui32_noop, // unsigned int (*openCaptureDevice)(uint64 schandlerID, string_t modeID, string_t captureDevice);
    ui32_noop, // unsigned int (*getCurrentPlaybackDeviceName)(uint64 schandlerID, char** result, int* isDefault);
    ui32_noop, // unsigned int (*getCurrentPlayBackMode)(uint64 schandlerID, char** result);
    ui32_noop, // unsigned int (*getCurrentCaptureDeviceName)(uint64 schandlerID, char** result, int* isDefault);
    ui32_noop, // unsigned int (*getCurrentCaptureMode)(uint64 schandlerID, char** result);
    ui32_noop, // unsigned int (*initiateGracefulPlaybackShutdown)(uint64 schandlerID);
    ui32_noop, // unsigned int (*closePlaybackDevice)(uint64 schandlerID);
    ui32_noop, // unsigned int (*closeCaptureDevice)(uint64 schandlerID);
    ui32_noop, // unsigned int (*activateCaptureDevice)(uint64 schandlerID);
    ui32_noop, // unsigned int (*playWaveFileHandle)(uint64 schandlerID, string_t path, int loop, uint64* waveHandle);
    ui32_noop, // unsigned int (*pauseWaveFileHandle)(uint64 schandlerID, uint64 waveHandle, int pause);
    ui32_noop, // unsigned int (*closeWaveFileHandle)(uint64 schandlerID, uint64 waveHandle);
    ui32_noop, // unsigned int (*playWaveFile)(uint64 schandlerID, string_t path);
    ui32_noop, // unsigned int (*registerCustomDevice)(string_t deviceID, string_t deviceDisplayName, int capFrequency, int capChannels, int playFrequency, int playChannels);
    ui32_noop, // unsigned int (*unregisterCustomDevice)(string_t deviceID);
    ui32_noop, // unsigned int (*processCustomCaptureData)(string_t deviceName, const short* buffer, int samples);
    ui32_noop, // unsigned int (*acquireCustomPlaybackData)(string_t deviceName, short* buffer, int samples);

    // # Preprocessor
    ui32_noop, // unsigned int (*getPreProcessorInfoValueFloat)(uint64 schandlerID, string_t ident, float* result);
    ui32_noop, // unsigned int (*getPreProcessorConfigValue)(uint64 schandlerID, string_t ident, char** result);
    ui32_noop, // unsigned int (*setPreProcessorConfigValue)(uint64 schandlerID, string_t ident, string_t value);

    // # Encoder
    ui32_noop, // unsigned int (*getEncodeConfigValue)(uint64 schandlerID, string_t ident, char** result);

    // # Playback
    ui32_noop, // unsigned int (*getPlaybackConfigValueAsFloat)(uint64 schandlerID, string_t ident, float* result);
    ui32_noop, // unsigned int (*setPlaybackConfigValue)(uint64 schandlerID, string_t ident, string_t value);
    ui32_noop, // unsigned int (*setClientVolumeModifier)(uint64 schandlerID, anyID clientID, float value);

    // # Recording
    ui32_noop, // unsigned int (*startVoiceRecording)(uint64 schandlerID);
    ui32_noop, // unsigned int (*stopVoiceRecording)(uint64 schandlerID);

    // # 3D sound positioning
    ui32_noop, // unsigned int (*systemset3DListenerAttributes)(uint64 schandlerID, const TS3_VECTOR* position, const TS3_VECTOR* forward, const TS3_VECTOR* up);
    ui32_noop, // unsigned int (*set3DWaveAttributes)(uint64 schandlerID, uint64 waveHandle, const TS3_VECTOR* position);
    ui32_noop, // unsigned int (*systemset3DSettings)(uint64 schandlerID, float distanceFactor, float rolloffScale);
    ui32_noop, // unsigned int (*channelset3DAttributes) (uint64 schandlerID, anyID clientID, const TS3_VECTOR* position);

    // # Server interaction
    ui32_noop, // unsigned int (*startConnection)(uint64 schandlerID, string_t identity, string_t ip, unsigned int port, string_t nickname, string_t * defaultChannelArray, string_t defaultChannelPassword, string_t serverPassword);
    ui32_noop, // unsigned int (*stopConnection)(uint64 schandlerID, string_t quitMessage);
    ui32_noop, // unsigned int (*requestClientMove)(uint64 schandlerID, anyID clientID, uint64 newChannelID, string_t password, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientVariables)(uint64 schandlerID, anyID clientID, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientKickFromChannel)(uint64 schandlerID, anyID clientID, string_t kickReason, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientKickFromServer)(uint64 schandlerID, anyID clientID, string_t kickReason, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelDelete)(uint64 schandlerID, uint64 channelID, int force, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelMove)(uint64 schandlerID, uint64 channelID, uint64 newChannelParentID, uint64 newChannelOrder, string_t returnCode);
    ui32_noop, // unsigned int (*requestSendPrivateTextMsg)(uint64 schandlerID, string_t message, anyID targetClientID, string_t returnCode);
    ui32_noop, // unsigned int (*requestSendChannelTextMsg)(uint64 schandlerID, string_t message, uint64 targetChannelID, string_t returnCode);
    ui32_noop, // unsigned int (*requestSendServerTextMsg)(uint64 schandlerID, string_t message, string_t returnCode);
    ui32_noop, // unsigned int (*requestConnectionInfo)(uint64 schandlerID, anyID clientID, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientSetWhisperList)(uint64 schandlerID, anyID clientID, const uint64* targetChannelIDArray, const anyID* targetClientIDArray, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelSubscribe)(uint64 schandlerID, const uint64* channelIDArray, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelSubscribeAll)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelUnsubscribe)(uint64 schandlerID, const uint64* channelIDArray, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelUnsubscribeAll)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelDescription)(uint64 schandlerID, uint64 channelID, string_t returnCode);
    ui32_noop, // unsigned int (*requestMuteClients)(uint64 schandlerID, const anyID* clientIDArray, string_t returnCode);
    ui32_noop, // unsigned int (*requestUnmuteClients)(uint64 schandlerID, const anyID* clientIDArray, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientPoke)(uint64 schandlerID, anyID clientID, string_t message, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientIDs)(uint64 schandlerID, string_t clientUniqueIdentifier, string_t returnCode);
    ui32_noop, // unsigned int (*clientChatClosed)(uint64 schandlerID, string_t clientUniqueIdentifier, anyID clientID, string_t returnCode);
    ui32_noop, // unsigned int (*clientChatComposing)(uint64 schandlerID, anyID clientID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerTemporaryPasswordAdd)(uint64 schandlerID, string_t password, string_t description, uint64 duration, uint64 targetChannelID, string_t targetChannelPW, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerTemporaryPasswordDel)(uint64 schandlerID, string_t password, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerTemporaryPasswordList)(uint64 schandlerID, string_t returnCode);

    // # Query own client ID
    ui32_noop, // unsigned int (*getClientID)(uint64 schandlerID, anyID* result);

    // # Client information
    ui32_noop, // unsigned int (*getClientSelfVariableAsInt)(uint64 schandlerID, size_t flag, int* result);
    ui32_noop, // unsigned int (*getClientSelfVariableAsString)(uint64 schandlerID, size_t flag, char** result);
    ui32_noop, // unsigned int (*setClientSelfVariableAsInt)(uint64 schandlerID, size_t flag, int value);
    ui32_noop, // unsigned int (*setClientSelfVariableAsString)(uint64 schandlerID, size_t flag, string_t value);
    ui32_noop, // unsigned int (*flushClientSelfUpdates)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*getClientVariableAsInt)(uint64 schandlerID, anyID clientID, size_t flag, int* result);
    ui32_noop, // unsigned int (*getClientVariableAsUInt64)(uint64 schandlerID, anyID clientID, size_t flag, uint64* result);
    ui32_noop, // unsigned int (*getClientVariableAsString)(uint64 schandlerID, anyID clientID, size_t flag, char** result);
    ui32_noop, // unsigned int (*getClientList)(uint64 schandlerID, anyID** result);
    ui32_noop, // unsigned int (*getChannelOfClient)(uint64 schandlerID, anyID clientID, uint64* result);

    // # Channel information
    ui32_noop, // unsigned int (*getChannelVariableAsInt)(uint64 schandlerID, uint64 channelID, size_t flag, int* result);
    ui32_noop, // unsigned int (*getChannelVariableAsUInt64)(uint64 schandlerID, uint64 channelID, size_t flag, uint64* result);
    ui32_noop, // unsigned int (*getChannelVariableAsString)(uint64 schandlerID, uint64 channelID, size_t flag, char** result);
    ui32_noop, // unsigned int (*getChannelIDFromChannelNames)(uint64 schandlerID, char** channelNameArray, uint64* result);
    ui32_noop, // unsigned int (*setChannelVariableAsInt)(uint64 schandlerID, uint64 channelID, size_t flag, int value);
    ui32_noop, // unsigned int (*setChannelVariableAsUInt64)(uint64 schandlerID, uint64 channelID, size_t flag, uint64 value);
    ui32_noop, // unsigned int (*setChannelVariableAsString)(uint64 schandlerID, uint64 channelID, size_t flag, string_t value);
    ui32_noop, // unsigned int (*flushChannelUpdates)(uint64 schandlerID, uint64 channelID, string_t returnCode);
    ui32_noop, // unsigned int (*flushChannelCreation)(uint64 schandlerID, uint64 channelParentID, string_t returnCode);
    ui32_noop, // unsigned int (*getChannelList)(uint64 schandlerID, uint64** result);
    ui32_noop, // unsigned int (*getChannelClientList)(uint64 schandlerID, uint64 channelID, anyID** result);
    ui32_noop, // unsigned int (*getParentChannelOfChannel)(uint64 schandlerID, uint64 channelID, uint64* result);

    // # Server information
    ui32_noop, // unsigned int (*getServerConnectionHandlerList)(uint64** result);
    ui32_noop, // unsigned int (*getServerVariableAsInt)(uint64 schandlerID, size_t flag, int* result);
    ui32_noop, // unsigned int (*getServerVariableAsUInt64)(uint64 schandlerID, size_t flag, uint64* result);
    ui32_noop, // unsigned int (*getServerVariableAsString)(uint64 schandlerID, size_t flag, char** result);
    ui32_noop, // unsigned int (*requestServerVariables)(uint64 schandlerID);

    // # Connection information
    ui32_noop, // unsigned int (*getConnectionStatus)(uint64 schandlerID, int* result);
    ui32_noop, // unsigned int (*getConnectionVariableAsUInt64)(uint64 schandlerID, anyID clientID, size_t flag, uint64* result);
    ui32_noop, // unsigned int (*getConnectionVariableAsDouble)(uint64 schandlerID, anyID clientID, size_t flag, double* result);
    ui32_noop, // unsigned int (*getConnectionVariableAsString)(uint64 schandlerID, anyID clientID, size_t flag, char** result);
    ui32_noop, // unsigned int (*cleanUpConnectionInfo)(uint64 schandlerID, anyID clientID);

    // # Client-related
    ui32_noop, // unsigned int (*requestClientDBIDfromUID)(uint64 schandlerID, string_t clientUniqueIdentifier, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientNamefromUID)(uint64 schandlerID, string_t clientUniqueIdentifier, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientNamefromDBID)(uint64 schandlerID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientEditDescription)(uint64 schandlerID, anyID clientID, string_t clientDescription, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientSetIsTalker)(uint64 schandlerID, anyID clientID, int isTalker, string_t returnCode);
    ui32_noop, // unsigned int (*requestIsTalker)(uint64 schandlerID, int isTalkerRequest, string_t isTalkerRequestMessage, string_t returnCode);

    // # Plugin-related
    ui32_noop, // unsigned int (*requestSendClientQueryCommand)(uint64 schandlerID, string_t command, string_t returnCode);

    // # File transfer
    ui32_noop, // unsigned int (*getTransferFileName)(anyID transferID, char** result);
    ui32_noop, // unsigned int (*getTransferFilePath)(anyID transferID, char** result);
    ui32_noop, // unsigned int (*getTransferFileSize)(anyID transferID, uint64* result);
    ui32_noop, // unsigned int (*getTransferFileSizeDone)(anyID transferID, uint64* result);
    ui32_noop, // unsigned int (*isTransferSender)(anyID transferID, int* result); // 1 == upload, 0 == download
    ui32_noop, // unsigned int (*getTransferStatus)(anyID transferID, int* result);
    ui32_noop, // unsigned int (*getCurrentTransferSpeed)(anyID transferID, float* result);
    ui32_noop, // unsigned int (*getAverageTransferSpeed)(anyID transferID, float* result);
    ui32_noop, // unsigned int (*getTransferRunTime)(anyID transferID, uint64* result);
    ui32_noop, // unsigned int (*sendFile)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t file, int overwrite, int resume, string_t sourceDirectory, anyID* result, string_t returnCode);
    ui32_noop, // unsigned int (*requestFile)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t file, int overwrite, int resume, string_t destinationDirectory, anyID* result, string_t returnCode);
    ui32_noop, // unsigned int (*haltTransfer)(uint64 schandlerID, anyID transferID, int deleteUnfinishedFile, string_t returnCode);
    ui32_noop, // unsigned int (*requestFileList)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t path, string_t returnCode);
    ui32_noop, // unsigned int (*requestFileInfo)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t file, string_t returnCode);
    ui32_noop, // unsigned int (*requestDeleteFile)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t * file, string_t returnCode);
    ui32_noop, // unsigned int (*requestCreateDirectory)(uint64 schandlerID, uint64 channelID, string_t channelPW, string_t directoryPath, string_t returnCode);
    ui32_noop, // unsigned int (*requestRenameFile)(uint64 schandlerID, uint64 fromChannelID, string_t channelPW, uint64 toChannelID, string_t toChannelPW, string_t oldFile, string_t newFile, string_t returnCode);

    // # Offline messages
    ui32_noop, // unsigned int (*requestMessageAdd)(uint64 schandlerID, string_t toClientUID, string_t subject, string_t message, string_t returnCode);
    ui32_noop, // unsigned int (*requestMessageDel)(uint64 schandlerID, uint64 messageID, string_t returnCode);
    ui32_noop, // unsigned int (*requestMessageGet)(uint64 schandlerID, uint64 messageID, string_t returnCode);
    ui32_noop, // unsigned int (*requestMessageList)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestMessageUpdateFlag)(uint64 schandlerID, uint64 messageID, int flag, string_t returnCode);

    // # Passwords
    ui32_noop, // unsigned int (*verifyServerPassword)(uint64 schandlerID, string_t serverPassword, string_t returnCode);
    ui32_noop, // unsigned int (*verifyChannelPassword)(uint64 schandlerID, uint64 channelID, string_t channelPassword, string_t returnCode);

    // # Banning
    ui32_noop, // unsigned int (*banclient)(uint64 schandlerID, anyID clientID, uint64 timeInSeconds, string_t banReason, string_t returnCode);
    ui32_noop, // unsigned int (*banadd)(uint64 schandlerID, string_t ipRegExp, string_t nameRegexp, string_t uniqueIdentity, uint64 timeInSeconds, string_t banReason, string_t returnCode);
    ui32_noop, // unsigned int (*banclientdbid)(uint64 schandlerID, uint64 clientDBID, uint64 timeInSeconds, string_t banReason, string_t returnCode);
    ui32_noop, // unsigned int (*bandel)(uint64 schandlerID, uint64 banID, string_t returnCode);
    ui32_noop, // unsigned int (*bandelall)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestBanList)(uint64 schandlerID, string_t returnCode);

    // # Complaining
    ui32_noop, // unsigned int (*requestComplainAdd)(uint64 schandlerID, uint64 targetClientDatabaseID, string_t complainReason, string_t returnCode);
    ui32_noop, // unsigned int (*requestComplainDel)(uint64 schandlerID, uint64 targetClientDatabaseID, uint64 fromClientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestComplainDelAll)(uint64 schandlerID, uint64 targetClientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestComplainList)(uint64 schandlerID, uint64 targetClientDatabaseID, string_t returnCode);

    // # Permissions
    ui32_noop, // unsigned int (*requestServerGroupList)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupAdd)(uint64 schandlerID, string_t groupName, int groupType, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupDel)(uint64 schandlerID, uint64 serverGroupID, int force, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupAddClient)(uint64 schandlerID, uint64 serverGroupID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupDelClient)(uint64 schandlerID, uint64 serverGroupID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupsByClientID)(uint64 schandlerID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupAddPerm)(uint64 schandlerID, uint64 serverGroupID, int continueonerror, const unsigned int* permissionIDArray, const int* permissionValueArray, const int* permissionNegatedArray, const int* permissionSkipArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupDelPerm)(uint64 schandlerID, uint64 serverGroupID, int continueOnError, const unsigned int* permissionIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupPermList)(uint64 schandlerID, uint64 serverGroupID, string_t returnCode);
    ui32_noop, // unsigned int (*requestServerGroupClientList)(uint64 schandlerID, uint64 serverGroupID, int withNames, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupList)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupAdd)(uint64 schandlerID, string_t groupName, int groupType, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupDel)(uint64 schandlerID, uint64 channelGroupID, int force, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupAddPerm)(uint64 schandlerID, uint64 channelGroupID, int continueonerror, const unsigned int* permissionIDArray, const int* permissionValueArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupDelPerm)(uint64 schandlerID, uint64 channelGroupID, int continueOnError, const unsigned int* permissionIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelGroupPermList)(uint64 schandlerID, uint64 channelGroupID, string_t returnCode);
    ui32_noop, // unsigned int (*requestSetClientChannelGroup)(uint64 schandlerID, const uint64* channelGroupIDArray, const uint64* channelIDArray, const uint64* clientDatabaseIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelAddPerm)(uint64 schandlerID, uint64 channelID, const unsigned int* permissionIDArray, const int* permissionValueArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelDelPerm)(uint64 schandlerID, uint64 channelID, const unsigned int* permissionIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelPermList)(uint64 schandlerID, uint64 channelID, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientAddPerm)(uint64 schandlerID, uint64 clientDatabaseID, const unsigned int* permissionIDArray, const int* permissionValueArray, const int* permissionSkipArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientDelPerm)(uint64 schandlerID, uint64 clientDatabaseID, const unsigned int* permissionIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestClientPermList)(uint64 schandlerID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelClientAddPerm)(uint64 schandlerID, uint64 channelID, uint64 clientDatabaseID, const unsigned int* permissionIDArray, const int* permissionValueArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelClientDelPerm)(uint64 schandlerID, uint64 channelID, uint64 clientDatabaseID, const unsigned int* permissionIDArray, int arraySize, string_t returnCode);
    ui32_noop, // unsigned int (*requestChannelClientPermList)(uint64 schandlerID, uint64 channelID, uint64 clientDatabaseID, string_t returnCode);
    ui32_noop, // unsigned int (*privilegeKeyUse)(uint64 schandlerID, string_t tokenKey, string_t returnCode);
    ui32_noop, // unsigned int (*requestPermissionList)(uint64 schandlerID, string_t returnCode);
    ui32_noop, // unsigned int (*requestPermissionOverview)(uint64 serverConnectionHandler, uint64 clientDBID, uint64 channelID, string_t returnCode);

    // # Helper functions
    ui32_noop, // unsigned int (*clientPropertyStringToFlag)(string_t clientPropertyString, size_t* resultFlag);
    ui32_noop, // unsigned int (*channelPropertyStringToFlag)(string_t channelPropertyString, size_t* resultFlag);
    ui32_noop, // unsigned int (*serverPropertyStringToFlag)(string_t serverPropertyString, size_t* resultFlag);

    // # Client functions
    void_noop, // void         (*getAppPath)(char* path, size_t maxLen);
    void_noop, // void         (*getResourcesPath)(char* path, size_t maxLen);
    void_noop, // void         (*getConfigPath)(char* path, size_t maxLen);
    void_noop, // void         (*getPluginPath)(char* path, size_t maxLen);
    ui64_noop, // uint64       (*getCurrentServerConnectionHandlerID)();
    void_noop, // void         (*printMessage)(uint64 schandlerID, string_t message, enum PluginMessageTarget messageTarget);
    void_noop, // void         (*printMessageToCurrentTab)(string_t message);
    void_noop, // void         (*urlsToBB)(string_t text, char* result, size_t maxLen);
    void_noop, // void         (*sendPluginCommand)(uint64 schandlerID, string_t pluginID, string_t command, int targetMode, const anyID* targetIDs, string_t returnCode);
    void_noop, // void         (*getDirectories)(string_t path, char* result, size_t maxLen);
    ui32_noop, // unsigned int (*getServerConnectInfo)(uint64 schandlerID, char* host, unsigned short* port, char* password, size_t maxLen);
    ui32_noop, // unsigned int (*getChannelConnectInfo)(uint64 schandlerID, uint64 channelID, char* path, char* password, size_t maxLen);
    void_noop, // void         (*createReturnCode)(string_t pluginID, char* returnCode, size_t maxLen);
    ui32_noop, // unsigned int (*requestInfoUpdate)(uint64 schandlerID, enum PluginItemType itemType, uint64 itemID);
    ui64_noop, // uint64       (*getServerVersion)(uint64 schandlerID);
    ui32_noop, // unsigned int (*isWhispering)(uint64 schandlerID, anyID clientID, int* result);
    ui32_noop, // unsigned int (*isReceivingWhisper)(uint64 schandlerID, anyID clientID, int* result);
    ui32_noop, // unsigned int (*getAvatar)(uint64 schandlerID, anyID clientID, char* result, size_t maxLen);
    void_noop, // void         (*setPluginMenuEnabled)(string_t pluginID, int menuID, int enabled);
    void_noop, // void         (*showHotkeySetup)();
    void_noop, // void         (*requestHotkeyInputDialog)(string_t pluginID, string_t keyword, int isDown, void* qParentWindow);
    ui32_noop, // unsigned int (*getHotkeyFromKeyword)(string_t pluginID, string_t * keywords, char** hotkeys, size_t arrayLen, size_t hotkeyBufSize);
    ui32_noop, // unsigned int (*getClientDisplayName)(uint64 schandlerID, anyID clientID, char* result, size_t maxLen);
    ui32_noop, // unsigned int (*getBookmarkList)(struct PluginBookmarkList** list);
    ui32_noop, // unsigned int (*getProfileList)(enum PluginGuiProfile profile, int* defaultProfileIdx, char*** result);
    ui32_noop, // unsigned int (*guiConnect)(enum PluginConnectTab connectTab, string_t serverLabel, string_t serverAddress, string_t serverPassword, string_t nickname, string_t channel, string_t channelPassword, string_t captureProfile, string_t playbackProfile, string_t hotkeyProfile, string_t soundProfile, string_t userIdentity, string_t oneTimeKey, string_t phoneticName, uint64* schandlerID);
    ui32_noop, // unsigned int (*guiConnectBookmark)(enum PluginConnectTab connectTab, string_t bookmarkuuid, uint64* schandlerID);
    ui32_noop, // unsigned int (*createBookmark)(string_t bookmarkuuid, string_t serverLabel, string_t serverAddress, string_t serverPassword, string_t nickname, string_t channel, string_t channelPassword, string_t captureProfile, string_t playbackProfile, string_t hotkeyProfile, string_t soundProfile, string_t uniqueUserId, string_t oneTimeKey, string_t phoneticName);
    ui32_noop, // unsigned int (*getPermissionIDByName)(uint64 schandlerID, string_t permissionName, unsigned int* result);
    ui32_noop  // unsigned int (*getClientNeededPermission)(uint64 schandlerID, string_t permissionName, int* result);
};

int process_args(int argc, char **argv) {
    cout << "Executable: " << argv[0] << endl;
    cout << "Arguments:" << endl;
    for(int i = 1; i < argc; ++i) {
        cout << "  " << i << ": " << argv[i] << endl;
    }
    return 0;
}

int plugin_initialize() {
    cout << "Setting plugin function pointers." << endl;
    try {
        ts3plugin_setFunctionPointers(noopFunctions);
    } catch(const std::exception& e) {
        cerr << "Error while setting plugin function pointers: "
             << e.what() << endl;
    } catch(...) {
        cerr << "Error while setting plugin function pointers: UNKNOWN" << endl;
        return 1;
    }

    cout << "Initializing plugin." << endl;
    try {
        int result = ts3plugin_init();

        if(result != 0) {
            cerr << "Error: plugin failed to initialize." << endl
                 << "Initialization return code: " << result << endl;
            return 1;
        }
    } catch(const std::exception& e) {
        cerr << "Error during plugin initialization: " << e.what() << endl;
    } catch(...) {
        cerr << "Error during plugin initialization: UNKNOWN" << endl;
        return 1;
    }
    return 0;
}

int run_test() {
    const int max_iterations = 100000;
    cout << "Starting integration test." << endl;
    cout << "Running " << max_iterations << " times." << endl;
    try {
        for(int i = 0; i < max_iterations; ++i) {
            cout << "Iteration #" << i << endl;
            integration_test();
            usleep(100000); // sleep for 0.1 seconds
        }
    } catch(const std::exception& e) {
        cerr << "Error during integration test: " << e.what() << endl;
    } catch(...) {
        cerr << "Error during integration test: UNKNOWN" << endl;
    }
    return 0;
}

int plugin_shutdown() {
    cout << "Shutting down plugin" << endl;
    try {
        ts3plugin_shutdown();
    } catch(const std::exception& e) {
        cerr << "Error during plugin shutdown: " << e.what() << endl;
        return 3;
    } catch(...) {
        cerr << "Error during plugin shutdown: UNKNOWN" << endl;
        return 3;
    }
    return 0;
}

int main(int argc, char **argv) {
    int retval = 0;

    if((retval = process_args(argc, argv))) { return retval; }
    if((retval = plugin_initialize()))      { return retval; }

    run_test();

    if((retval = plugin_shutdown()))        { return retval; }

    return 0;
}
