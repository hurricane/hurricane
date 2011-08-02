Hurricane
=========

This project is still very young and changing quite rapidly.

What it does provide right now is a messaging system that works, PHP and
Python drivers to connect to Hurricane, and some examples.  Also,
asynchronous messaging support over TCP is provided as well.

For example usage, see http_handler.py, time_server.php, and
hurricane.config, which, when run, expose an endpoint called
/current_time on port 8000.  This endpoint uses messaging not only
between Erlang and the HTTP handler, but between the HTTP handler
and the example time server as well.  There's also examples for
communication over TCP (which also resides in the examples directory).

As of now, synchronous and asynchronous messaging modes are supported.
Even right now, Hurricane supports the scatter-gather algorithm of
messaging/data-retrieval completely transparently.

Next Steps:
-----------

- Allow configurable timeouts per-spawned-process
- Add drivers for more languages (Go and Java support is next)
- Create an official logging facility (instead of print statements)
- Create a sync tcp server (for scatter-gather)
- Build out multi-node distribution (connecting Hurricane instances
  together)
- Build administrative interface for external processes
