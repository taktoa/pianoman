#include <iostream>

#include "rpc.hpp"

#include <QGuiApplication>
#include <QWindow>
#include <QSocketNotifier>

static void shutdown_teamspeak() {
    QWindowList windows = QGuiApplication::topLevelWindows();
    QWindow *win;
    foreach(win, windows) {
        std::cout << win << win->title().toStdString() << "\n";
        win->close();
    }
}

namespace rpc {
    server_handle_t::server_handle_t()
        : context(1),
          publisher(context, ZMQ_PUB),
          request_server(context, ZMQ_REP)
    {
        std::cout << "made " << this << "\n";
        int fd = 0;
        size_t fd_size = sizeof(fd);
        request_server.getsockopt(ZMQ_FD, &fd, &fd_size);
        socket_notifier = new QSocketNotifier(fd, QSocketNotifier::Read, this);
        connect(socket_notifier, SIGNAL(activated(int)),
                this, SLOT(socket_ready(int)));
        std::cout << "hooked zmq fd: " << fd << "\n";
    }

    std::vector<uint8_t> event_t::serialize() const {
        std::cout << "base called\n";
        return std::vector<uint8_t>();
    }

    simple_event::simple_event(std::string msg)
        : message(msg.begin(), msg.end()) {
    }

    std::vector<uint8_t> simple_event::serialize() const {
        return message;
    }

    void server_handle_t::send_event(const nlohmann::json& root) {
        std::string text = ({
                std::ostringstream ss;
                ss << root;
                ss.str();
            });
        std::vector<uint8_t> binary(text.begin(), text.end());
        std::cout << "sending '" << text << "'\n";
        zmq::message_t message(binary.size());
        memcpy(message.data(), binary.data(), binary.size());
        publisher.send(message);
    }

    void server_handle_t::start_server() {
        publisher.bind("ipc://teamspeak.pub");
        request_server.bind("ipc://teamspeak.rep");
    }

    void server_handle_t::shutdown_server() {
        std::cout << "stopping" << this << "\n";
    }
    void server_handle_t::socket_ready(int socket) {
        uint32_t poll_state;
        size_t poll_state_size = sizeof(poll_state);
        while(true) {
            request_server.getsockopt(ZMQ_EVENTS, &poll_state, &poll_state_size);
            std::cout << "state" << poll_state << "\n";
            if(!(poll_state && ZMQ_POLLIN)) { break; }
            zmq::message_t msg;
            if(request_server.recv(&msg, ZMQ_NOBLOCK)) {
                std::string packet = {
                    static_cast<const char*>(msg.data()),
                    msg.size()
                };
                if(packet == "die") {
                    shutdown_teamspeak();
                    return;
                }
                std::cout << "got packet: " << packet << "\n";
                zmq::message_t reply { 0 };
                request_server.send(reply);
            }
        }
    }
}
