# Direct declaration effects integration

Compiler and runtime dependencies are pinned to the verified direct-declaration
implementation. Route projection declares its known car/cdr cells explicitly;
it no longer calls the compiler's removed whole-network diff helpers.

The maintained route projection, web clients, demo, semantic REPL, environment
IO, file-loader, and runtime organization suites passed: 32 tests and 163
assertions. Each individual test finished within three seconds.

The visualization-combinators demo remains a loopback-only server at
http://127.0.0.1:45668/relationships by default. Restart the server after changing
Clojure dependencies; its file watcher reloads Lain source files only.
