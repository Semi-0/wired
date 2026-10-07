# Functional-network web corrections

Web routing declares slot access through the existing compound-object API.
Authorization reads those projected cells; it does not manually merge accessor
parents or install topology. Flat GUR availability gates declare late rows and
tails in the coordinator network. A usable prefix can authorize a matching
route while an unavailable tail waits. Contradictory, malformed, and cyclic
branches cannot supply an authorization witness. Traversal is bounded to 256
steps; exhausting the bound without a witness denies delivery.

Projection cells, propagators, and availability declarations use semantic IDs.
Repeated installation is a no-op. Equivalent redeclaration preserves graph
identities. Client-list input remains an accessor-backed external input.

The runtime replacement/watch regression came from rebuilding a network with
an environment ID obtained from a separate, discarded network. The runtime now
uses both the network and environment returned by the same root declaration.
No GUR, compound-object, TMS, cell-merge, or scheduler changes were required.

The Lain tracer no longer routes the result of recursive statement bodies back
into their explicit output. Its bodies already write that output, and retain
their returned linked-list interfaces. A production test executes the loaded
tracer through the existing one-time flat-GUR runner and verifies disposal.

Graph labels and call expansions use canonical `network` syntax and declared
names. Widget names and channel labels preserve widget identities, event/view
cell IDs, and channel IDs. Terminal TUI repair, styling, layout changes, and
generic performance work are excluded.

## Verification rules

Each test Var must resolve and finish within three seconds. Namespace/JVM setup
is reported separately. Output printing is bounded only inside the test report
function: binding print limits around execution changes `pr-str` and therefore
the existing stable-ID encoding. Runs with those global bindings are invalid.

Compiler checks after behavior deprecation: 147 tests / 464 assertions passed. Shared runtime checks:
158 tests / 572 assertions passed. Focused routing and graph
label checks: 6 tests / 72 assertions passed. Replacement updates to 6 and edited
watch updates to 16 both pass below one second. The loaded Lain tracer execution
test passes below two seconds. Five direct route-projection tests also passed
14 assertions, including late input, contradictions, stable identities, cycles,
and malformed lists. The three widget label tests passed independently.
After correcting the owner-to-workspace resource and semantic-renderer mappings,
the mirrored projection, tracer, semantic-application, and semantic-REPL suites
passed 17 tests / 79 assertions. Every Var finished below three seconds; the
longest was tracer execution at 1.58 seconds. Namespace startup was 3.72 seconds
and was timed separately.

The earlier broader selected web run reported one failure and two errors. The demo
print failure is a real contract mismatch: `lain-compiler`'s
`operators/behavior.clj:325` accepts legacy `closure-info`, then constructs a
`compiler-reducer/closure-merge-net` / `reducer/reducer-subnet`. Canonical
functional-network callable cells no longer contain that representation.
Repair requires a separately reviewed behavior/reducer migration; it is outside
this correction's compiler ownership. No special demo printer or compatibility
conversion was added. The user subsequently authorized behavior deprecation:
active session roots now load TMS-only bindings, and the demo joins its two cells
directly. The demo and slider integration tests now pass (2 tests / 8 assertions,
each below one second). The two server errors were sandbox socket-binding errors.

The first browser attempt was rejected with `net::ERR_BLOCKED_BY_CLIENT`.
Subsequent verification through localhost succeeded: the web client compiled
canonical `define`/`network` source, reported 7 nodes / 6 edges, displayed named
calls, cells, and results, and acknowledged slider `gain/value: 1`. No browser
errors were reported. Physical mobile/XR hardware was not tested.

The workspace now preserves wired's operator-enabled assembly as
`graph.compiler-2-assembly`. Its deprecated graph facade delegates session
creation to that assembly. Mirrored slider, loading, replacement, and file-watch
checks pass: 13 tests / 53 assertions. Wired's maintained web integration passes
32 tests / 163 assertions; browser-side tests pass 14/14. The broader mirrored
compiler run passes 187 tests / 622 assertions. Every Clojure test Var finishes
below three seconds, with startup timed separately.

Verification was repeated using published Git pins: compiler
`b9806338dfeea418210733b748ebce38f23c561b` and runtime
`473876b1433d312a3f58b3d9ab13c1afe6d91edf`. The four shared-session regressions
from the earlier port report now pass 4 tests / 14 assertions, each below one
second. Infrastructure remains pinned to
`6b031cf71df7a27a71e52c14cf1227904f637a13`.

Wired's remote main additions were retained, including the file-watch and
replacement checks. Source examples use canonical functional-network syntax.
Delivery is ordered compiler, runtime, wired, then research, with downstream
dependencies pinned to the published owner revisions. Terminal TUI repair and
deprecated temporal behavior programs remain excluded from release verification.

KIROSHI grounding used revisions 398–399. No model fact was persisted, approved,
or superseded. Generic dependencies and the session-extension contract remain
fixed. Each correction is delivered in its owning repository before downstream
dependency promotion.
