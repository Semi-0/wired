# Completed availability-control pruning measurement

This is an optimization experiment on a separate immutable copy of the current
`live-cell-traces.lain` TTMS visualization workload. The active LAN session was
left untouched. No external effect handlers were executed.

| Measurement | Before | After |
|---|---:|---:|
| Cells | 405 | 405 |
| Propagators | 363 | 350 |

Thirteen completed flat-GUR `when` controls were retired in **211 ms** (one sample;
JVM startup excluded). Surviving propagators retain identical activation references
and cell IDs. Cells and dictionary state are unchanged. Ownership through retired
controls is subsumed to its existing surviving ancestors.

Six independent reactivation/evidence comparisons passed in 180–193 ms each:
reactivation, repeated input, conflicting input, and unavailable, false, or
contradictory condition evidence. No topology growth occurred. A waiting guard
remained installed and returned 9 after late input (338 ms). Both semantic graphs
retained 6 nodes/16 edges; the runtime chain retained 601 nodes/1161 edges. Their
graph payloads matched; entire evidence frames changed to describe the rewritten
network.

The development-only implementation and focused tests are published in
`Semi-0/lain-compiler`, branch `codex/completed-when-pruning`, under
`dev/propagators/compiler/experimental/completed_when_pruning.clj` and
`completed_when_pruning_test.clj`. Compiler documentation includes an independent
test invocation with the published infrastructure dependency.

## Limits

Persistent completion markers must be retained. Clearing markers or rollback to
pre-declaration state is unsupported; TTMS premise replacement/retraction safety
is unverified. This is a trusted-constructor experiment, not a generic opaque-ID
capture audit. It retires completed work and does not reduce compilation already
performed. No latency improvement is claimed.

The full visualizer measurement was obtained from a combined working-tree
workspace, including separately developing tracer code. This evidence does not
claim reproducibility from a clean released wired checkout. Unrelated tracer,
editor, source-loader, compiler, and optimizer work was not included in this commit.

Raw measurements: `evidence/live-cell-trace-completed-pruning.json`.
