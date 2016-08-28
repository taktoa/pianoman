#pragma once

#include <queue>
#include <string>
#include <iostream>
#include <streambuf>

#include <nlohmann/json.hpp>
#include <zmq.hpp>

#include <QObject>

class QSocketNotifier;

//! FIXME: doc
namespace rpc {
    //! FIXME: doc
    class event_t {
    public:
        //! FIXME: doc
        virtual std::vector<uint8_t> serialize() const;
    };

    //! FIXME: doc
    class simple_event : public event_t {
    private:
        // FIXME: doc
        std::vector<uint8_t> message;

    public:
        //! FIXME: doc
        simple_event(std::string message);

        //! FIXME: doc
        virtual std::vector<uint8_t> serialize() const;
    };

    //! FIXME: doc
    class server_handle_t : QObject {
    Q_OBJECT
    private:
        // FIXME: Replace std::queue with something more thread-safe
        // FIXME: doc
        std::queue<event_t> server_mailbox;

        // FIXME: doc
        zmq::context_t context;

        // FIXME: doc
        zmq::socket_t publisher;

        // FIXME: doc
        zmq::socket_t request_server;

        // FIXME: doc
        QSocketNotifier* socket_notifier;

    public:
        //! FIXME: doc
        server_handle_t();

        //! FIXME: doc
        void send_event(const nlohmann::json& msg);

        //! FIXME: doc
        void start_server();

        //! FIXME: doc
        void shutdown_server();

    public slots:
        //! FIXME: doc
        void socket_ready(int socket);
    };
} /* namespace rpc */
