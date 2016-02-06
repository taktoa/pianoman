#include <iostream>
#include <unistd.h>
#include "rpc.hpp"
#include "plugin.h"

using namespace std;

int main(int argc, char **argv) {
    cout << "starting" << endl;
    rpc_server = new rpc::server_handle_t;
    rpc_server->start_server();
    while (1) {
        sleep(2);
        anyID randomid;
        ts3plugin_onTextMessageEvent(0,randomid,randomid,randomid,"fromname","guid","spamage",0);
    }
    cout << "stopping" << endl;
    rpc_server->shutdown_server();
    delete rpc_server;
    return 0;
}
