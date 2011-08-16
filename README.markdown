Hurricane
=========

This project is still very young and changing quite rapidly.

What it does provide right now is a messaging system that works, PHP,
Python, and Java drivers to connect to Hurricane, and some examples.
Also, messaging support over TCP is provided as well.

For example usage, see http_handler.py, time_server.php, and
hurricane.config, which, when run, expose an endpoint called
/current_time on port 8000.  This endpoint uses messaging not only
between Erlang and the HTTP handler, but between the HTTP handler
and the example time server as well.  There's also examples for
communication over TCP (which also resides in the examples directory).

Even right now, Hurricane supports the scatter-gather algorithm of
messaging/data-retrieval completely transparently.  Also, the ability
to register over TCP with an arbitrary message group is supported.

Next Steps:
-----------

- Support two-phase timeouts for stdio-based processes
- Add drivers for more languages (Go, C++, etc)
- Build out multi-node distribution (connecting Hurricane instances
  together)
- Build administrative interface for external processes
