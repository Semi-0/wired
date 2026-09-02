# propagators-tui

Charm entrypoints, editor and layout behavior, rendering, servers, dashboards,
plots, web clients, XR adapters, and UI examples for the propagator system.

## Entrypoints

```clojure
(propagators.tui.main/-main ...)
(propagators.tui.server/-main ...)
```

Application assembly in `propagators.tui.assembly` explicitly registers TUI
operator groups with the presentation-independent runtime.

## Verify

```bash
clojure -M:test
node --test test/propagators/tui/graph/xr_static_update_test.mjs
```

Committed sibling dependencies use pinned Git SHAs. The Repo workspace may
provide local-root overrides without changing this file.
