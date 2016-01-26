#include <iostream>

#include <boost/chrono/duration.hpp>
#include "rpc.hpp"

#include <QGuiApplication>
#include <QWindow>

#include <array>
#include <boost/asio.hpp>
//#include <zmq.hpp>

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
    server_handle_t::server_handle_t(): publisher(ios), request_server(ios) {
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
        publisher.send(asio::buffer(binary));
    }

    void server_handle_t::start_server() {
        shutdown = false;
        publisher.bind("ipc://teamspeak.pub");
        request_server.bind("ipc://teamspeak.rep");

        //server_thread = thread(boost::bind(&boost::asio::io_service::run, &ios));

        server_thread = thread(boost::ref(*this));
    }

    void server_handle_t::shutdown_server() {
        cout << "stopping" << this << endl;
        shutdown = true;
        ios.stop();
        cout << "a" << endl;
        server_thread.interrupt();
        cout << "a" << endl;
        server_thread.join();
        cout << "a" << endl;
    }
    void server_handle_t::operator()() {
        cout << "in thread " << this << endl;
        //boost::chrono::duration<int> sleep_interval(1);
        
        while (true) {
            //cout << "tick" << endl;
            ios.run();
        std::array<char, 256> buf;
        std::array<boost::asio::mutable_buffer,1> rcv_bufs = {{ boost::asio::buffer(buf) }};
        request_server.async_receive(rcv_bufs,
          [&](boost::system::error_code const& ec, size_t bytes_transfered) {
            string x = string(buf.data(),bytes_transfered);
            if (x == "die") {
                ios.stop();
                shutdown_teamspeak();
                return;
            }
            cout << "got packet" << x << endl;
            std::array<char,0> reply;
            request_server.send(asio::buffer(reply));
        });
            //cout << "tock" << endl;
            if (shutdown) break;
        }
        cout << "quiting thread" << endl;
    }
}
