#pragma once

#include <queue>
#include <string>
#include <iostream>
#include <streambuf>

#include <boost/thread/thread.hpp>

#include <msgpack.hpp>
#include <zmq.hpp>

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
        zmq::context_t context;
        zmq::socket_t publisher;
        zmq::socket_t request_server;
    public:
        //! FIXME: doc
        server_handle_t();
        //! FIXME: doc
        void send_event(const event_t &event);
        void operator()();
        //! FIXME: doc
        void start_server();
        //! FIXME: doc
        void shutdown_server();
    };


} /* namespace rpc */
