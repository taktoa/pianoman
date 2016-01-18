#include <iostream>

#include <boost/chrono/duration.hpp>
#include "rpc.hpp"

#include <QGuiApplication>
#include <QWindow>

using namespace std;
using namespace boost;

static void shutdown_teamspeak() {
    QWindowList windows = QGuiApplication::topLevelWindows();
    QWindow *win;
    foreach(win, windows) {
        std::cout << win << win->title().toStdString() << std::endl;
        win->close();
    }
}

namespace rpc {
    server_handle_t::server_handle_t(): context(1), publisher(context,ZMQ_PUB), request_server(context,ZMQ_REP) {
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

    void server_handle_t::send_event(const Json::Value root) {
        Json::FastWriter fw;
        string text = fw.write(root);
        vector<uint8_t> binary(text.begin(), text.end());
        cout << "sending '" << text << "'" << endl;
        zmq::message_t message(binary.size());
        memcpy(message.data(),binary.data(),binary.size());
        publisher.send(message);
    }

    void server_handle_t::start_server() {
        shutdown = false;
        server_thread = thread(boost::ref(*this));
        publisher.bind("ipc://teamspeak.pub");
        request_server.bind("ipc://teamspeak.rep");
    }

    void server_handle_t::shutdown_server() {
        server_thread.interrupt();
        server_thread.join();
    }
    void server_handle_t::operator()() {
        cout << "in thread " << this << endl;
        //boost::chrono::duration<int> sleep_interval(1);
        while (true) {
            try {
                zmq::message_t msg;
                request_server.recv(&msg);
                string x = string((const char *)msg.data(),msg.size());
                if (x == "die") {
                    shutdown_teamspeak();
                }
                cout << "got packet" << x << endl;
                zmq::message_t reply(0);
                request_server.send(reply);
            } catch(zmq::error_t& e) {
                cout << "got error:" << e.what() << endl;
            }
            //this_thread::sleep_for(sleep_interval);
            //send_event(rpc::simple_event("tick")); // example, remove later
            if (shutdown) break;
        }
        cout << "quiting thread" << endl;
    }
}
