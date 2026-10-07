# Runner and visualization port: review status

Historical port snapshot. The functional-network migration subsequently fixes
the listed shared-session and routing regressions: maintained web tests pass
32/163 assertions and the four targeted shared-session regressions pass 4/14
assertions on published compiler/runtime pins. See
[current corrections](web-functional-network-corrections.md). This report retains
its original baseline evidence; repository default-branch settings are unchanged.

Source: `Semi-0/datalog-research` at
`5a1c0be62e44c48510f69316c766ccfcaa5251af`.

## Ownership

Infrastructure owns the continuation runner, structural relationships, and pure
graph/list data. Compiler owns lowering and higher-order collection application.
Runtime owns sessions, effects, extensions, and pure inspection. Wired owns XR
assembly, HTTP endpoints, rendering, mobile controls, and runnable UI examples.
No model mutation or component reparenting was performed.

This port preserves the split compiler's CPS operand contract and UI adapter
injection. Full source reload retains injected session options. The XR endpoint
reads the existing UI-owned effect projection directly. The static analysis
example targets `propagators.tui.graph.xr-runtime/graph->json` in this repository.

## Verification

- Infrastructure: 186 tests, 779 assertions, zero failures/errors.
- Compiler: 55 tests, 183 assertions, zero failures/errors.
- Runtime: 147 tests, 527 assertions, zero failures/errors.
- Visualization collection/composition/XR tests: 35 tests, 116 assertions,
  zero failures/errors.
- Static call-graph example, including HTTP endpoint: 8 tests, 30 assertions,
  zero failures/errors.
- Browser Node tests: 14 tests passed.
- Visualization browser contract: 24 assertions passed.
- Production namespace dependency scan: 227 namespaces, zero missing or upward
  dependencies among the four repository owners.
- `git diff --check` passed.

Library tests used published dependency revisions. Initial UI verification used
the corresponding staged local checkouts. The combined example/visualization
suite was then repeated using only the published Git SHA pins: 43 tests,
146 assertions, zero failures/errors. Tests do not constitute visual QA on
physical desktop/mobile devices, nor LAN reachability verification.

## Blocking existing behavior regressions

Baseline wired `040a46930e5a9ad31fed64c3747c5d90ddcfa2fe`, with its original
dependency pins, passes these three test vars (3 tests, 9 assertions) in
`propagators.tui.graph.vijual.compiler-2-runtime-server-test`:

| Test | Staged migration result |
| --- | --- |
| `block-target-expression-writes-future-block` | Expected 7, got nothing |
| `blocks-compile-into-one-growing-env` | Expected 6 after editing inc1, got 5 |
| `be-block-watch-installs-and-updates-without-full-rebuild` | Expected 56, got nothing |

The staged comparison has 6 passing and 3 failing assertions. This comparison
was repeated with print limits scoped only to reporting, not test execution.
The precise cause of each regression is not yet proven. Removal of retained
application rescheduling and the move to explicit flat-topology dependencies
are relevant boundaries to investigate, not a justification to weaken tests.

The broader suite also reported web-client routing failures and timed out in
`edited-be-block-watch-uses-new-display-epoch`. The standard harness has a
2-second per-test limit; an investigative run with 30 seconds also timed out.
The harness was not changed. No completed full-suite pass is claimed.

This branch is a review snapshot, not a compatible default release. Keep the
default branch, `codex/xr-regression-followup`, on its working dependency pins
until maintained behavior is restored. Do not
solve the regressions by changing compound bidirectional semantics, inserting
domain branches in the runner, or restoring the retired compatibility runner.

## Focused reproduction

From wired, use the dependency pins in this branch and add test to the classpath:

```sh
clojure -Sdeps '{:paths ["src" "test"]}' -M -e "(require 'clojure.test 'propagators.tui.graph.vijual.compiler-2-runtime-server-test) (clojure.test/test-vars [#'propagators.tui.graph.vijual.compiler-2-runtime-server-test/block-target-expression-writes-future-block #'propagators.tui.graph.vijual.compiler-2-runtime-server-test/blocks-compile-into-one-growing-env #'propagators.tui.graph.vijual.compiler-2-runtime-server-test/be-block-watch-installs-and-updates-without-full-rebuild]) (shutdown-agents)"
```

`test-vars` reports failures but does not set the process exit code. Use the
test counters when running this as a CI gate. Do not bind print limits around
the tests: some tests construct source or view data using `pr-str`.
