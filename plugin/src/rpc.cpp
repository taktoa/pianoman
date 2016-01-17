#include <iostream>
#include <boost/chrono/duration.hpp>
#include "rpc.hpp"

using namespace std;
using namespace boost;

namespace rpc {
    server_handle_t::server_handle_t(): context(1), publisher(context,ZMQ_PUB) {
        shutdown = true;
        cout << "made " << this << endl;
    }

    vector<uint8_t> event_t::serialize() const {
        cout << "base called" << endl;
        return vector<uint8_t>();
    }

    simple_event::simple_event(std::string msg) : message(msg.begin(), msg.end()) {
    }
    vector<uint8_t> simple_event::serialize() const {
        return message;
    }

    void server_handle_t::send_event(const event_t &event) {
        vector<uint8_t> binary = event.serialize();
        cout << "sending '" << string(binary.begin(), binary.end()) << "'" << binary.size() << endl;
        zmq::message_t message(binary.size());
        memcpy(message.data(),binary.data(),binary.size());
        publisher.send(message);
    }

    void server_handle_t::start_server() {
        shutdown = false;
        server_thread = thread(boost::ref(*this));
        publisher.bind("ipc://teamspeak.ipc");
    }

    void server_handle_t::shutdown_server() {
        server_thread.interrupt();
        server_thread.join();
    }
    void server_handle_t::operator()() {
        cout << "in thread " << this << endl;
        boost::chrono::duration<int> sleep_interval(1);
        while (true) {
            this_thread::sleep_for(sleep_interval);
            //send_event(rpc::simple_event("tick")); // example, remove later
            if (shutdown) break;
        }
    }
}
