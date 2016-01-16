#include "rpc.hpp"

namespace rpc {
    std::string event_t::serialize() {
        return "";
    }

    void server_handle_t::send_event(event_t event) {
    }

    server_handle_t start_server() {
        return NULL;
    }

    void shutdown_server(server_handle_t handle) {
    }
}
