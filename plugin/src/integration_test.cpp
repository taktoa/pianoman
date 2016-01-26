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
typedef enum PluginMenuType  menu_item_type_t;

ident_t          gen_schandlerID()             { return 1; }
ident_t          gen_channelID()               { return 1; }
string_t         gen_modeID()                  { return "modeID"; }
ident_t          gen_clientID()                { return 1; }
string_t         gen_clientUID()               { return "clientUID"; }
string_t         gen_clientName()              { return "clientName"; }
uint64_t         gen_time()                    { return 1; }
int32_t          gen_connectionStatus()        { return 1; }
uint64_t         gen_connectionError()         { return 1; }
int32_t          gen_visibility()              { return 1; }
uint64_t         gen_serverError()             { return 1; }
string_t         gen_permissionErrorMessage()  { return "permissionErrorMessage"; }
string_t         gen_serverErrorMessage()      { return "serverErrorMessage"; }
string_t         gen_serverErrorExtraMessage() { return "serverErrorExtraMessage"; }
string_t         gen_serverErrorReturnCode()   { return "serverErrorReturnCode"; }
string_t         gen_kickMessage()             { return "kickMessage"; }
string_t         gen_moveMessage()             { return "moveMessage"; }
string_t         gen_shutdownMessage()         { return "shutdownMessage"; }
string_t         gen_timeoutMessage()          { return "timeoutMessage"; }
string_t         gen_textMessage()             { return "textMessage"; }
int32_t          gen_ffIgnored()               { return 1; }
int32_t          gen_isReceivedWhisper()       { return 1; }
int32_t          gen_playOrCap()               { return 1; }
int32_t          gen_talkStatus()              { return 1; }
ident_t          gen_targetMode()              { return 1; }
int32_t          gen_channels()                { return 1; }
fill_mask_t      gen_fillMask()                { return new uint32_t[1] { 5 }; }
speakers_t       gen_speakerArray()            { return new uint32_t[1] { 5 }; }
samples_t        gen_voiceSamples()            { return new int16_t[1] { 5 }; }
int32_t          gen_voiceSamplesCount()       { return 1; }
float_t          gen_distance()                { return 1; }
int32_t*         gen_voiceEdited()             { return new int32_t[1] { 5 }; }
float_t*         gen_volume()                  { return new float[1] { 5 }; }
uint64_t         gen_waveHandle()              { return 1; }
string_t         gen_userLogMessage()          { return "userLogMessage"; }
string_t         gen_userLogChannel()          { return "userLogChannel"; }
int64_t          gen_userLogLevel()            { return 1; }
uint64_t         gen_userLogID()               { return 1; }
string_t         gen_userLogTime()             { return "userLogTime"; }
string_t         gen_userLogString()           { return "userLogString"; }
uint64_t         gen_fileDate()                { return 1; }
string_t         gen_fileName()                { return "fileName"; }
string_t         gen_filePath()                { return "filePath"; }
uint64_t         gen_fileProgress()            { return 1; }
uint64_t         gen_fileSize()                { return 1; }
int64_t          gen_fileType()                { return 1; }
string_t         gen_pokeMessage()             { return "pokeMessage"; }
string_t         gen_returnCode()              { return "returnCode"; }
uint64_t         gen_serverGroupID()           { return 1; }
int32_t          gen_variableFlag()            { return 1; }
string_t         gen_variableNewValue()        { return "variableNewValue"; }
string_t         gen_variableOldValue()        { return "variableOldValue"; }
string_t         gen_channelGroupName()        { return "channelGroupName"; }
int32_t          gen_channelGroupType()        { return 1; }
uint64_t         gen_clientDBID()              { return 1; }
ident_t          gen_iconID()                  { return 1; }
ident_t          gen_permissionID()            { return 1; }
int32_t          gen_permissionNegated()       { return 1; }
int32_t          gen_permissionSkip()          { return 1; }
int32_t          gen_permissionValue()         { return 1; }
int32_t          gen_saveDB()                  { return 1; }
uint64_t         gen_serverGroupList()         { return 1; }
string_t         gen_serverGroupName()         { return "serverGroupName"; }
int32_t          gen_serverGroupType()         { return 1; }
uint64_t         gen_channelGroupID()          { return 1; }
string_t         gen_serverLogMessage()        { return "serverLogMessage"; }
uint64_t         gen_serverLogLastPos()        { return 1; }
uint32_t         gen_groupEndID()              { return 1; }
uint64_t         gen_messageID()               { return 1; }
uint64_t         gen_overviewID()              { return 1; }
int32_t          gen_overviewType()            { return 1; }
string_t         gen_permissionDescription()   { return "permissionDescription"; }
uint32_t         gen_permissionError()         { return 1; }
string_t         gen_permissionName()          { return "permissionName"; }
string_t         gen_permissionReturnCode()    { return "permissionReturnCode"; }
uint64_t         gen_serverLogSize()           { return 1; }
ident_t          gen_transferID()              { return 1; }
uint64_t         gen_transferSize()            { return 1; }
uint32_t         gen_transferStatus()          { return 1; }
string_t         gen_transferStatusMessage()   { return "transferStatusMessage"; }
string_t         gen_clientNickName()          { return "clientNickName"; }
int32_t          gen_flagRead()                { return 1; }
string_t         gen_messageContents()         { return "messageContents"; }
string_t         gen_messageSubject()          { return "messageSubject"; }
uint64_t         gen_timestamp()               { return 1; }
uint64_t         gen_banID()                   { return 1; }
string_t         gen_banReason()               { return "banReason"; }
string_t         gen_complainReason()          { return "complainReason"; }
string_t         gen_ipAddress()               { return "ipAddress"; }
int32_t          gen_numberOfEnforcements()    { return 1; }
string_t         gen_password()                { return "password"; }
string_t         gen_pluginName()              { return "pluginName"; }
string_t         gen_avatarPath()              { return "avatarPath"; }
string_t         gen_commandText()             { return "commandText"; }
string_t         gen_displayName()             { return "displayName"; }
string_t         gen_key()                     { return "key"; }
string_t         gen_keyword()                 { return "keyword"; }
int32_t          gen_menuItemID()              { return 1; }
menu_item_type_t gen_menuItemType()            { return (menu_item_type_t) 0; }
string_t         gen_pluginCommand()           { return "pluginCommand"; }
uint64_t         gen_selectedItemID()          { return 1; }

void integration_test() {
    // Required functions
    printf("Plugin name:        %s", ts3plugin_name());
    printf("Plugin version:     %s", ts3plugin_version());
    printf("Plugin API version: %d", ts3plugin_apiVersion());
    printf("Plugin author:      %s", ts3plugin_author());
    printf("Plugin description: %s", ts3plugin_description());

    // Clientlib
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
    // GENERATES int
    ts3plugin_onServerErrorEvent(gen_schandlerID(),
                                 gen_serverErrorMessage(),
                                 gen_serverError(),
                                 gen_serverErrorReturnCode(),
                                 gen_serverErrorExtraMessage());
    ts3plugin_onServerStopEvent(gen_schandlerID(),
                                gen_shutdownMessage());
    // GENERATES int
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

    // Clientlib rare
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
    // GENERATES int
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
    // GENERATES int
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

    // Client UI callbacks
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


int main(int argc, char **argv) {
    cout << "Executable: " << argv[0] << endl;
    cout << "Arguments:" << endl;
    for(int i = 1; i < argc; ++i) {
        cout << "  " << i << ": " << argv[i] << endl;
    }

    cout << "Initializing plugin." << endl;
    try {
        ts3plugin_init();
    } catch(const std::exception& e) {
        cerr << "Error during plugin initialization: " << e.what() << endl;
    } catch(...) {
        cerr << "Error during plugin initialization: UNKNOWN" << endl;
        return 1;
    }

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
