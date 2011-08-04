Hurricane
=========

This project is still very young and changing quite rapidly.

What it does provide right now is a messaging system that works, PHP,
Python, and Java drivers to connect to Hurricane, and some examples.
Also, asynchronous messaging support over TCP is provided as well.

For example usage, see http_handler.py, time_server.php, and
hurricane.config, which, when run, expose an endpoint called
/current_time on port 8000.  This endpoint uses messaging not only
between Erlang and the HTTP handler, but between the HTTP handler
and the example time server as well.  There's also examples for
communication over TCP (which also resides in the examples directory).

As of now, synchronous and asynchronous messaging modes are supported.
Even right now, Hurricane supports the scatter-gather algorithm of
messaging/data-retrieval completely transparently.  Also, asynchronous
TCP messaging is supported, as well as the ability to register over
TCP with an arbitrary message group.

Next Steps:
-----------

- Allow configurable timeouts per-spawned-process
- Add drivers for more languages (Go, C++, etc)
- Create an official logging facility (instead of print statements)
- Create a synchronous TCP server (for scatter-gather over TCP)
- Build out multi-node distribution (connecting Hurricane instances
  together)
- Build administrative interface for external processes
