#include <iostream>
#include "rpc.hpp"

using namespace std;

int main(int argc, char **argv) {
    cout << "starting" << endl;
    rpc::server_handle_t *foo = new rpc::server_handle_t;
    foo->start_server();
    sleep(10);
    cout << "stopping" << endl;
    foo->shutdown_server();
    delete foo;
    return 0;
}
