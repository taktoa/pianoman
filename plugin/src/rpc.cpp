#include <iostream>

#include "rpc.hpp"

#include <QGuiApplication>
#include <QWindow>
#include <QSocketNotifier>

using namespace std;

static void shutdown_teamspeak() {
    QWindowList windows = QGuiApplication::topLevelWindows();
    QWindow *win;
    foreach(win, windows) {
        std::cout << win << win->title().toStdString() << std::endl;
        win->close();
    }
}

namespace rpc {
    server_handle_t::server_handle_t(): context(1), publisher(context,ZMQ_PUB),
        request_server(context,ZMQ_REP) {
        cout << "made " << this << endl;
        int zmq_fd = 0;
        size_t zmq_fd_size = sizeof(zmq_fd);
        request_server.getsockopt(ZMQ_FD,&zmq_fd,&zmq_fd_size);
        socket_notifier = new QSocketNotifier(zmq_fd,QSocketNotifier::Read,this);
        connect(socket_notifier,SIGNAL(activated(int)),this,SLOT(socket_ready(int)));
        cout << "hooked zmq fd" << zmq_fd << endl;
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
        publisher.bind("ipc://teamspeak.pub");
        request_server.bind("ipc://teamspeak.rep");
    }

    void server_handle_t::shutdown_server() {
        cout << "stopping" << this << endl;
    }
    void server_handle_t::socket_ready(int socket) {
        uint32_t poll_state;
        size_t poll_state_size = sizeof(poll_state);
        while (true) {
            request_server.getsockopt(ZMQ_EVENTS,&poll_state,&poll_state_size);
            cout << "state" << poll_state << endl;
            if (!(poll_state && ZMQ_POLLIN)) break;
            zmq::message_t msg;
            if (request_server.recv(&msg,ZMQ_NOBLOCK)) {
                string x = string((const char *)msg.data(),msg.size());
                if (x == "die") {
                    shutdown_teamspeak();
                    return;
                }
                cout << "got packet" << x << endl;
                zmq::message_t reply(0);
                request_server.send(reply);
            }
        }
    }
}
