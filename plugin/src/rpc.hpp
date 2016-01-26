#pragma once

#include <queue>
#include <string>
#include <iostream>
#include <streambuf>

#include <boost/thread/thread.hpp>

#include <json/json.h>
#include <azmq/socket.hpp>

// //! FIXME: doc
// namespace msgpack {
//     MSGPACK_API_VERSION_NAMESPACE(MSGPACK_DEFAULT_API_NS) {
//         namespace adaptor {
//             template <>
//             struct pack<T> {
//                 template <typename Stream>
//                 msgpack::packer<Stream>& operator()(msgpack::packer<Stream>& o,
//                                                     T const& v) const {
//                     // packing implementation.
//                     // o.pack_???(v.???);
//                     return o;
//                 }
//             };
//         } // namespace adaptor
//     } // MSGPACK_API_VERSION_NAMESPACE(MSGPACK_DEFAULT_API_NS)
// } // namespace msgpack

//! FIXME: doc
namespace rpc {
    //! FIXME: doc
    class event_t {
    private:
    public:
        //! FIXME: doc
        virtual std::vector<uint8_t> serialize() const;
    };
    class simple_event : public event_t {
    public:
        simple_event(std::string message);
        virtual std::vector<uint8_t> serialize() const;
    private:
        std::vector<uint8_t> message;
    };

    //! FIXME: doc
    class server_handle_t {
    private:
        //! FIXME: doc
        boost::thread server_thread;
        // FIXME: Replace std::queue with something more thread-safe
        //! FIXME: doc
        std::queue<event_t> server_mailbox;
        bool shutdown;
        boost::asio::io_service ios;
        azmq::pub_socket publisher;
        azmq::req_socket request_server;
    public:
        //! FIXME: doc
        server_handle_t();
        //! FIXME: doc
        void send_event(const Json::Value msg);
        void operator()();
        //! FIXME: doc
        void start_server();
        //! FIXME: doc
        void shutdown_server();
    };


} /* namespace rpc */
