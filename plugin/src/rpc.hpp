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
    typedef struct event_t {
    private:
    public:
        //! FIXME: doc
        std::string serialize();
    } event_t;

    //! FIXME: doc
    typedef class server_handle_t {
    private:
        //! FIXME: doc
        boost::thread server_thread;
        // FIXME: Replace std::queue with something more thread-safe
        //! FIXME: doc
        std::queue<event_t> server_mailbox;
    public:
        //! FIXME: doc
        server_handle_t();
        //! FIXME: doc
        void send_event(event_t event);
    } server_handle_t;

    //! FIXME: doc
    server_handle_t start_server();

    //! FIXME: doc
    void shutdown_server(server_handle_t handle);
} /* namespace rpc */
