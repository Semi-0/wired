# Vijual Graph Visualization Notes

Recorded on 2026-06-09 while experimenting with `graph.vijual` and
`compiler_2` network drawings.

## Goal

Vijual is an ASCII graph renderer for inspecting network-shaped data. The
current target is not just to draw boxes, but to preserve readable connection
semantics:

- directed edges should visibly connect the right source and destination;
- arrows should not imply the opposite direction;
- dense compiler wiring should be inspectable;
- a compiler graph should also have a collapsed semantic view that hides
  mechanical compiler cells and propagators.

## Experiments Tried

### Original grid placement

The first implementation kept the original Vijual-style grid placement and
orthogonal connector routing. It is small and deterministic, but directed
graphs were often hard to read because layout quality depended heavily on the
input order.

Status: kept as the compatibility/current strategy.

### Force-directed placement

We added a light continuous force-directed layout, then snapped nodes onto the
ASCII grid. This is useful for general undirected or circular-looking graphs,
but it does not consistently communicate hierarchy in directed compiler graphs.

Status: kept as a comparison strategy.

### Sugiyama-style layered drawing

We added a Sugiyama-style layered layout for directed graphs. It is good when
the graph has a DAG-like flow and when source/destination layering matters.
It was especially useful for debugging direction mistakes because it makes
`x -> + -> output` read top-to-bottom.

Status: kept as a comparison strategy and for directed hierarchy checks.

### Stress majorization with balancing penalties

We added stress placement over continuous coordinates, snapped to the ASCII
grid, then refined by local grid moves/swaps. This produced the best general
quality for mixed graphs and compiler graphs.

The current stress energy is:

```text
energy =
  graph_distance_error
+ node_overlap_penalty
+ edge_length_variance_penalty
+ aspect_ratio_penalty
+ center_balance_penalty
+ crossing_penalty
+ wiring_overlap_balance_penalty
```

The important options are:

- `:stress-node-spacing` controls ideal spacing between connected nodes.
- `:stress-aspect-ratio` keeps the drawing from becoming too tall or too wide.
- `:stress-iterations` controls continuous stress majorization.
- `:stress-refine-iterations` controls snapped-grid refinement.
- `:stress-energy-weights` can tune individual penalty weights.

Status: chosen as the primary layout for the compiler demo and semantic views.

### Routing strategies

We kept two route strategies:

- current orthogonal routing;
- routing grid plus shortest path.

Shortest-path routing generally gives better results around boxes and occupied
lanes, so the demos use:

```clojure
{:routing :shortest-path}
```

### Arrow placement

Middle arrows helped earlier dense directed graphs because the arrow remains
visible even when the edge endpoint is close to a box. For semantic graphs,
middle arrows can be misleading because they do not mark the destination.

Current choice:

- raw directed graph demos may use middle arrows by default;
- semantic compiler graphs use endpoint arrows:

```clojure
{:arrow-position :end}
```

### Directed route orientation bug

We found that `route-edges` canonicalized edges by grid position before routing.
That works for undirected graphs, but for directed graphs it could physically
route `dest -> source` and compensate with `:arrow :start`.

That preserved direction mathematically, but visually it made inbound and
outbound wires look mixed. The fix is:

- directed routes preserve original `source -> dest`;
- only undirected routes are canonicalized.

## Chosen Defaults

For the compiler demo, the current direction is stable:

- stress placement for compact balanced drawings;
- shortest-path routing for box-aware routes;
- endpoint arrows for semantic graphs.

The exact tuning lives in:

```text
graph/vijual_compiler_2_demo.clj
```

## Layout Strategy Files

The public layout API remains in:

```text
graph/vijual/layout.clj
```

That namespace owns the shared graph normalization, route expansion,
shortest-path routing, geometry, metrics, and public `layout-*` entry points.
The placement strategies are split into strategy-specific namespaces:

```text
graph/vijual/layout/anneal.clj
graph/vijual/layout/force.clj
graph/vijual/layout/stress.clj
graph/vijual/layout/sugiyama.clj
```

This keeps each layout algorithm readable while preserving the existing
external API.

## Wiring Overlap Balance Target

The latest stress target is `:wiring-overlap-balance-penalty`.

The purpose is to prefer evenly distributed overlaps when overlap is
unavoidable. If several routes share the same logical lane, the penalty uses a
squared extra-load score:

```text
lane load 1 -> cost 0
lane load 2 -> cost 1
lane load 3 -> cost 4
```

So two lanes with load `2, 2` are preferred over one lane with load `3` and one
empty lane. This does not make every overlap disappear; it gives the stress
refinement a target for spreading unavoidable overlap more evenly.

Default weight:

```clojure
:wiring-overlap-balance-penalty 3.0
```

Override example:

```clojure
{:stress-energy-weights
 {:wiring-overlap-balance-penalty 8.0}}
```

## Compiler 2 Visualization

The compiler demo lives in:

```text
graph/vijual_compiler_2_demo.clj
```

Run it with:

```bash
clojure -M -m graph.vijual-compiler-2-demo
```

The demo draws two kinds of graph.

### Raw compiler graph

The raw graph renders the actual compiled propagator network. It includes:

- cells;
- application IR cells;
- argument/context/operator cells;
- propagators;
- closure slot wiring.

Labels are inferred without changing `compiler_2`:

- environment bindings label named cells such as `inc-local` and closure input
  `x`;
- application IR labels `app:<->`, `app:+`, `app:inc-local`;
- operator cells label as `op:<->`, `op:+`;
- slot declarations label things like `prop:closure/env`;
- a small Datalog join over graph facts links app IR to its app propagator,
  giving `prop:<->`, `prop:+`, and `prop:inc-local`.

### Semantic graph

The semantic graph is inferred from the compiled graph and intentionally hides
compiler wiring.

For the current demo source:

```clojure
(let-cell [inc-local]
  (<-> inc-local
       (:: [x]
         (+ x 1)))
  (inc-local 5))
```

The main semantic graph is:

```text
inc-local -> <->
:: [x]    -> <->
<->       -> inc-local
<->       -> :: [x]
inc-local -> call inc-local
5         -> call inc-local
call inc-local -> result
```

The closure-body semantic graph is:

```text
x -> +
1 -> +
+ -> output
```

This is not a compiler change. It is a visualization pass that reads compiled
network data:

- compiler environment values;
- closure values;
- literal cells;
- retained application IR;
- graph input/output facts.

## Commands

Graph tests:

```bash
clojure -M:test graph
```

Clojure class hierarchy comparison demo:

```bash
clojure -M -m graph.vijual-clojure-lang-demo
```

Compiler 2 semantic/raw graph demo:

```bash
clojure -M -m graph.vijual-compiler-2-demo
```

## Current Recommendation

Use stress placement with shortest-path routing as the default for exploratory
graph visualization. Use Sugiyama as a comparison/debug view when direction or
layering is in doubt.

For compiler graphs, inspect both:

1. the semantic graph, to understand source-level meaning;
2. the raw compiler graph, to debug compiler wiring.
