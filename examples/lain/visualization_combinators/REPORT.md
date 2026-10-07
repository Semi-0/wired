# Higher-order, source-linked declarative views

Implementation and verification: 2026-09-26. Experimental and opt-in.

## Result

The headless composition and XR example work without compiler, runner, cell
evaluator, HOP, TMS, Net-shape, compound-sync, or layered-runtime changes.
Network definitions execute as callbacks; no host callback substitutes for HOP.

`chain.lain` expresses:

```clojure
(def boundary? (:: [item]
  (tracked-or (member? item (inputs-of selected))
              (member? item (outputs-of selected)))))
(def boundary (filter boundary? chain-graph))
(def boundary-list (transpose boundary :list))
(def values (map (:: [item] (strongest-of item)) boundary-list))
(def focused (focus values selection))
(def sources (sources-of focused))
(def zoom (map (:: [ref] (strongest-of ref))
               sources))
;; composition-graph is produced by the relationship operators in chain.lain.
(xr:io (juxtapose chain-graph boundary boundary-list
                  (selectable values selection) focused sources zoom
                  composition-graph))
```

The chain produces input 5 and output 7. Selecting either value follows its
source and updates the zoom collection. `composition-graph` traces this
composition using the same relationship tooling. The tests also inspect the
Lain-defined chain's actual structural children, including child propagators
and parent-to-child edges.

### Reading the eight panels

1. Top-level traced computation (not an expanded backward dependency chain).
2. Declared input/output cells selected by the composed predicate.
3. The same boundary references organized as a list.
4. Strongest values, selectable through the card or Views menu.
5. Selected value; empty until a selection is made.
6. Its retained source references; not a reconstruction of every upstream edge.
7. Strongest values read through those references.
8. Top-level topology of the visualization computation itself.

Raw trace graphs compose directly inside nested `juxtapose` declarations.
Their source cells are resolved afresh at the presentation boundary. Graph
metadata retains cell/propagator kinds through union, tracing and projection;
the browser draws white cell circles, cyan propagator triangles, and arrowheads.
Shapes, force-directed layout, measured collision-free label placement and
per-card −/Fit/+ controls belong to XR, not to propagators. Labels that cannot
fit are omitted; zoom recalculates placement. A dense graph is not guaranteed
to show every label at once. Cards remain coplanar to avoid billboard overlap.

## Run

From the repository root:

```sh
clojure -M -m examples.lain.visualization-combinators.demo
```

Open [the example](http://127.0.0.1:45668/relationships).
The server binds loopback by default. Use the collection buttons in the sidebar,
or the mobile **Views** menu. The server watches `chain.lain`; a successful
external edit recompiles and replaces the entire environment. Stop with Ctrl-C.

In 2D, collection cards use a screen-shaped grid, and camera framing uses the
same card positions. The previous horizontal row reduced the initial cards to
a nearly invisible strip on phones. Verification now includes a fresh page
without installing a trace, portrait and landscape fitting, and switching modes.
The separate trace graph remains empty until a trace is installed; changing the
view mode does not create a trace.

An alternative file and port may be passed as positional arguments:

```sh
clojure -M -m examples.lain.visualization-combinators.demo examples/lain/visualization_combinators/chain.lain 45669
```

For an explicitly enabled trusted-LAN demo, supply the bind address:

```sh
clojure -M -m examples.lain.visualization-combinators.demo examples/lain/visualization_combinators/chain.lain 45668 0.0.0.0
```

Visit `http://<host-LAN-IP>:45668/relationships` from the same network. This
experimental server exposes interactive runtime controls without authentication;
do not expose it to untrusted networks. Reload the browser after renderer edits;
restart the server after Clojure edits. Only `.lain` edits trigger full-env reload.

Headless use installs the two bundles through the existing session extension API:

```clojure
(require '[propagators.compiler-2.runtime.session.file-loader :as loader]
         '[propagators.experimental.visualization.extension :as views]
         '[propagators.experimental.visualization.layered-primitives :as primitives])
(def session
  (loader/load-session-from-file
   "examples/lain/visualization_combinators/chain.lain"
   {:extensions [primitives/session-extension views/extension]}))
```

These are opt-in bindings; existing base bindings are not redefined globally.
Use this bundle instead of stacking it with the older explicit-output
relationship/XR bundle: this experiment's `juxtapose` uses nested return syntax.

## Ownership and contracts

- **Collections:** cell-backed accessor declarations hold stable element
  identities, value-cell references, gates, source references and ordering.
  Generic map/filter declare ordinary applications through flat GUR. Both
  implicit-return and explicit one-input/one-output network callbacks work.
  Lists are discovered one link per propagator activation, not eagerly
  materialized as a whole list by a private executor.
- **Observation:** `inputs-of` and `outputs-of` inspect the selected application's
  declared interface, not node degree. `occurrence-of` selects by network
  definition and explicit application ports; ambiguity fails explicitly.
  `member?` compares identity. `strongest-of` separates strongest projection
  from retained support content.
- **Shared evidence transport:** dependency layers and existing distributed TMS
  carry argument, captured-value and predicate supports. Equal-valued elements
  keep separate identities. There is no mutable provenance dictionary.
- **Layered primitives:** `tracked+` and `tracked-or` reuse the existing layered
  procedure/materialization path. The ordinary compiler `+` remains unchanged;
  its earlier dependency-inside-TMS loss remains a characterized baseline.
- **Data resolution:** a headless resolver reads live value/gate cells. The same
  collection can feed another map/filter, transpose, focus or source traversal.
  `selectable` annotates collection data rather than wrapping it in a terminal
  display object. XR does not execute predicates or define transformation rules.
- **Presentation:** narrow `:collection` interpretation was added to the
  existing visualizer and XR bridge. Lists and graph layouts are drawn by the
  browser. Labels come from the source graph. Transposed lists retain a link
  to the original graph, including edges not drawn in list form.
- **Interaction:** published view identity, element identity, revision, epoch,
  and environment generation are validated before an ordinary runtime input
  writes a dedicated selection reducer. Client input does not supply a writable
  source-cell ID. Ordered control facts use the existing reducer machinery.
- **Reload:** the existing file-loader owns a public
  `:environment/generation` marker outside Net. This was necessary because
  identical source produces identical topology IDs after replacement.
  Old selections are rejected even when reloading identical text.

The bridge directly imports experimental collection resolution/interaction.
This is a narrow experimental integration, not a claim that the collection
representation has been promoted to a stable shared-kernel API.

## Concrete propagation semantics

Readiness remains owned by existing concrete propagators:

- true includes; false excludes;
- nothing and contradiction block activation and emit no new result;
- a blocked callback leaves its result pending if it has never produced one;
- a later contradiction does **not** erase a previously produced result.

No special contradiction transport, automatic invalidation, latest-wins source
policy, behavior value, or callback-local TMS policy was added. Candidate
descriptors and gates remain present even when a candidate is excluded/pending.
Malformed non-Boolean predicate results fail instead of being coerced to false.

## Verification

Focused suite:

```sh
clojure -M:test propagators.experimental.visualization-composition-test propagators.experimental.view-collections-test propagators.experimental.view-xr-test
node --experimental-default-type=module examples/lain/visualization_combinators/browser_contract_test.mjs
```

The initial experimental run passed 35 tests and 113 assertions. The final
commit verification additionally covers composed raw graphs, typed nodes,
selection after publication, and observer/server regressions; see the commands
and results below. Historical broader-suite and benchmark results are retained
separately rather than presented as reruns.

Final commit verification (2026-09-26): **58 Clojure tests, 207 assertions,
0 failures/errors**; **14 JavaScript tests** and **24 browser contract
assertions**, all passing.

```sh
clojure -M:test propagators.experimental.visualization-composition-test propagators.experimental.view-collections-test propagators.experimental.view-xr-test propagators.visualizer-test propagators.relationship-dataflow-test propagators.relationship-observer-test propagators.compiler-2.runtime.relationship-observer-test graph.xr-relationship-server-test
node --experimental-default-type=module --test test/graph/card_labels_test.mjs test/graph/card_symbols_test.mjs test/graph/xr_mobile_controls_test.mjs test/graph/xr_static_update_test.mjs
node --experimental-default-type=module examples/lain/visualization_combinators/browser_contract_test.mjs
git diff --cached --check
```

Covered: actual HOP callbacks; lexical capture; late inputs/definitions; implicit
and explicit outputs; nonvisual/empty/partial lists; equal-value distinct
origins; predicate and callback supports; cyclic declared interfaces; stable
graph growth; dangling-edge removal; deterministic transposition; path-distinct
nested references; headless zoom; post-publication composition; composable
selectable data; no source writes; stale/unpublished selection rejection;
full reload including a separate OS process writing the source file; composition
tracing and the compound network's children.

Earlier regression run: **164 tests, 569 assertions, all passing**, comprising
visualizer, layered procedures, Compiler-2 application, compile-2, TMS, compound
network slots, reducer cells, and relationship-server tests.

The older `graph.xr-runtime-test` is **not green**: 55 tests, 208 assertions,
41 failures and 1 error. A temporary classpath overlay restoring the four clean
XR/visualizer bridge files from HEAD reproduced all 41 assertion failures.
That baseline run had 3 errors: the TUI error plus two socket-permission errors.
Two sampled failing tests were also independently reproduced with the overlay.
No behavior/TUI/shared-runtime repair was attempted. The complete propagator
suite was not run, and this report does not claim the repository is fully green.

Browser verification used the real loopback server: desktop selection, 390x844
mobile menu/selection, source zoom changing from 7 to 5, and named graph items
were observed. Temporary viewport sizing was reset. Physical phone and headset
testing were not performed; immersive XR plane picking remains hardware-unverified.

## Small benchmark

```sh
clojure -M -m examples.lain.visualization-combinators.benchmark 2 8 16
```

One warm-up per alternative, then one measured sample per size in the same JVM.
The measurement starts after common environment/list setup and includes
projection declaration, topology construction and activation. The direct
alternative spells out one strongest-of/tracked+ projection per source, with
the same path-qualified references. Both use the same shared primitives.

| Elements | Direct ms | Generic ms | Direct activations | Generic activations | Direct added nodes | Generic added nodes |
|---:|---:|---:|---:|---:|---:|---:|
| 2 | 182.96 | 100.26 | 658 | 414 | 445 | 286 |
| 8 | 635.14 | 205.28 | 2,159 | 1,146 | 1,417 | 712 |
| 16 | 1,419.25 | 395.23 | 4,164 | 2,206 | 2,713 | 1,280 |

Values **and provenance** matched at every size. Re-running all retained
propagators added **zero outer-network nodes** for both alternatives.
The generic declaration remained 46 characters; direct declarations measured
109/403/813 characters, excluding common setup and host reference seeding.
Activation counts include nested runner work observed through the existing
advance-transform instrumentation hook.

These are small setup experiments, not steady-state throughput/allocation
benchmarks. Other verification processes ran during this session; timings are
noisy. There are no confidence intervals or large-scale performance claims.
Repeated collection scans and path-prefix traversal can still be quadratic.

## Assessment and limits

- **Expressiveness:** the same operations work on nonvisual lists and graph
  data, accept lexical network callbacks, and compose after publication.
- **Readability/conciseness:** the application pipeline stays declarative and
  constant-size; its reusable implementation is larger than a one-off projection.
- **Robustness:** source/control separation, dependency tests, stable identities,
  graph-growth checks and generation validation are explicit. Callback definitions
  are observational by convention, not sandboxed from arbitrary effects.
- **Performance:** this small comparison favors generic composition, but it does
  not establish scalability or reduced temporary layered-materialization work.
- **Limits:** finite acyclic collection lists; graph-to-list transpose only;
  an accessor slot with unresolved differing parents is rejected; strongest-of
  navigates valid cell references, not arbitrary provenance labels.
  General layer nesting, premise retraction, retained-only updates, temporal
  views, consolidation and superposition remain deferred.
- **UI limits:** canvas lists show the first ten items; the DOM controls expose
  the full list. Complex/large graph layout and immersive picking need further
  visual/hardware testing.

## What this composition buys us

The comparison here is with a one-way application pipeline that transforms data
into chart-specific records and ends at rendered marks. Reactive visualization
systems can also implement these capabilities; they are not exclusive to this
experiment. Our advantage is sharing the same cell, application and dependency
contracts across transformations and observations.

| Problem | One-way chart pipeline | This experiment |
|---|---|---|
| Explain a filtered/mapped item | Maintain a separate mark-to-source lookup through each transform | Element identity and source references travel with collection data |
| Continue from a displayed result | Reconstruct data from UI state or add a chart-specific drill-down API | Feed the same published collection to another map/filter/focus operation |
| Graph → list → focused values | Coordinate several chart-specific datasets and selection adapters | Change organization with transpose, retain identity, use a shared selection cell |
| Late inputs and reusable predicates | Coordinate readiness and bespoke callback/update wiring | Compose ordinary network definitions through the existing HOP and concrete-propagator rules |
| Explain the visualization logic | Build separate instrumentation for transformation code | Observe the transformation network with the same graph tooling |
| Render the same result elsewhere | Move logic out of renderer callbacks | Headless composition already yields declarative data; XR only interprets it |

For example, a result can be filtered into a graph, transposed into a list,
selected, followed back to its sources, and transformed again without treating
the first publication as terminal. The challenging invariant is not drawing a
graph: it is preserving identity, dependencies, readiness and meaning across
those steps. That invariant is shared rather than rebuilt in each visualizer.

This does not automatically solve readable layout, arbitrary upstream causal
reconstruction, source write-back, sandboxing, retraction or large-scale
performance. A static chart is simpler when those composition requirements are
absent. The UI label/layout iterations are evidence of that distinction.

## Grounding and evidence review

The grounding and architecture-evolution skills kept semantic composition
outside fixed runtime dependencies and retained the layered materialization
boundary. The reviewed runner-semantic-composition, explicit-projection,
retained-evidence, pull-only-observation and runtime-owned-effect constraints
guided the implementation. Existing unrelated dirty work was preserved.

No KIROSHI model mutation or approval occurred. Evidence for separate review:
the new experimental collection/observation modules, three focused test
namespaces, `chain.lain`, and the executable benchmark demonstrate composition
through existing HOP/layered/TMS contracts. The loader generation change and
narrow visualizer/XR bridge are explicit boundary deltas; no component
reparenting or new domain policy in the runner is proposed.
