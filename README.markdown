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

License (New BSD License)
-------------------------

Copyright (c) 2011, Ilia Cheishvili
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
+ Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
+ Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
+ Neither the name of the <organization> nor the
  names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
