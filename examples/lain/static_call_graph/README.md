# Static Clojure call graph in XR

This example adds a boundary-owned `clj-kondo` analysis effect to a Lain
environment, projects its receipt into the existing semantic graph format, and
passes that graph to `xr:io`. The browser owns the force-directed layout; the
propagator side emits only declarative graph data.

From the repository root, run:

```sh
clojure -M:example/static-call-graph
```

Then open <http://127.0.0.1:45666/relationships>.

The included `program.lain` analyzes the transitive internal callees of
`propagators.tui.graph.xr-runtime/graph->json` under `src/`. Editing the Lain file
rebuilds the complete session. If the analyzed Clojure files change, increment
the revision argument to request a fresh effect identity.

The public Lain operations are:

```clojure
(clojure-call-graph "source-root" "qualified.namespace/var" revision)
(call-graph-result analysis-receipt)
```

Static calls through resolved vars are included. Runtime-generated calls,
higher-order targets, multimethod dispatch, and reflective invocation may not
be visible to static analysis.
