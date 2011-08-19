Hurricane
=========

Hurricane is a scalable messaging system implemented in Erlang. It is
designed from the ground-up for fast transient messaging,
fault-tolerance, and a high degree of extensibility.

PHP, Python, and Java drivers are available (re-implementing the Erlang
binary communication protocol fully). Example code is also provided.
Messaging can be done over Standard I/O (processes managed by
Hurricane), TCP, or via native Erlang modules.

For example usage, see http_handler.py, time_server.php, and
hurricane.config, which, when run, expose an endpoint called
/current_time on port 8000.  This endpoint uses messaging not only
between Erlang and the HTTP handler, but between the HTTP handler
and the example time server as well.  There's also examples for
communication over TCP (which also reside in the examples directory).

Hurricane supports the scatter-gather pattern of
messaging/data-retrieval completely transparently over both Standard
I/O and TCP.  Also, the ability to register over TCP with an arbitrary
message group is supported.

For Standard I/O-based processes, Hurricane supports specifying how
long processes should take to start up and respond. When processes are
slacking, they get killed off and restarted. This configurable
management allows good fault-tolerance when dealing with misbehaving
processes.

Additionally, Hurricane provides log levels that can be configured to
help debug the message life cycle and see exactly what is going on in
the system.

Next Steps:
-----------

- Documentation (code, example config, etc)
- Add drivers for more languages (Go, C++, etc)
- Build out multi-node distribution (connecting Hurricane instances
  together)
