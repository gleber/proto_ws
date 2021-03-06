Websocket protocols parsers
===========================

This repository contains functional-style parsers for websocket
protocols. It is based on code from Misultin and since Misultin got
discontinued we factored out Websocket-specific code into separate
project along with some changes which allows for easier integration
into other projects.

The code has been functionalized and all parsing functions are
pure. They do not operate on sockets, instead they are working on
stream of bytes only. They provide foldl-like API, allowing to operate
on received messages right away or accumulated depending on the needs
of users.

License
=======

Copyright (c) 2012 Livepress Inc. Based on work by:

* Roberto Ostinelli <roberto@ostinelli.net>
* Joe Armstrong
* Sean Hinde
* Bob Ippolito <bob@mochimedia.com> for Mochi Media, Inc.
* and other contributors of Misultin

This library is distributed under BSD license, as is Misultin.

Roadmap
=======

0.9.0
* Current version

1.0.0
* Add examples
* Add code documentation
