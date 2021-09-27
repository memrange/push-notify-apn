0.3.0.2
=======

- Specify http2 library constraints, need 3.0 or later

0.3.0.1
=======

- Tighten compile flags and fix some errors

0.3.0.0
=======

- Add support for JWT authentication
- Allow arbitrary user specific content to be passed to the push API
- Stability improvements

0.2.0.2
=======

- Fix build for newer packages

0.2.0.1
=======

- Use Data.Pool

0.2.0.0
=======

- Threading bugfix
- Migrate to http2-client-0.9.0.0 and lts-14

0.1.1.1
=======

- Improve error handling

0.1.1.0
=======

- Let the client send alerts with no title

0.1.0.9
=======

- Add "semigroups" as a dependency for older ghc versions

0.1.0.8
=======

- Remove version constraints for http2-client

0.1.0.7
=======

- Make compatible with latest http2-client-0.7.0.0

0.1.0.6
=======

- Use http2-client-0.5.0.0 or greater
- Detect http2 goaway frames and remove connections from the
  connection pool accordingly
- Detect connection errors when sending messages and remove
  connections from the pool when they happen
- Fix in the README: The parameter is not timeout, but
  parallelConnections

0.1.0.5
=======

- Bugfix: Close the cleanup thread when closing a session
- Check if the certificates and key exist early, when the session is created
- Catch IO errors and return a temporary failure instead
- Depend explicitly on http2-client-0.3.0.2 for now

0.1.0.4
=======

- Add an interactive/scriptable mode where messages are read from stdin
- Re-structure exports to improve readability of the documentation
- Close connections in addition to sending http2 gtfo when idle time exceeded (needs http2-client-0.3.0.2)
- Add a closeSession method
- Close sessions when they are garbage collected

0.1.0.3
=======

- Filter out invalid token characters when hex encoded tokens are supplied
- Clarify the documentation
- Close the flow control thread when closing connections

0.1.0.1
=======

- Initial release
