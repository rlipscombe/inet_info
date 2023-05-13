# inet_info

Programmatic access to 'inet' information.

## Motivation

`inet:i()` is really useful for figuring out what your Erlang node is doing, but sometimes you want to deal with that
information programmatically. For example you might want to get only the listening sockets, or maybe you want a list of
sockets connected to your RabbitMQ broker.

By returning the same information as `inet:i()`, but in a way that can be handled programmatically, you can do those
things.

## Limitations

- The new socket API is unsupported. Sockets created with the new API will not be returned in the list.

## Caveats

This is for introspection and debugging _only_. This library relies, to a certain extent, on internal implementation
details of Erlang/OTP which may change without notice.
