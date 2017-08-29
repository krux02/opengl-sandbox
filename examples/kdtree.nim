import glm

type
  KdNode = object
    x: Vec3f
    left,right: ptr KdNode

proc dist(a,b: ptr KdNode; dim: int): float32 {.inline.} =
  length(a.x - b.x)

proc swap(x, y: ptr KdNode): void =
  swap(x[], y[])

#[ see quickselect method ]#

proc `+`[T](a: ptr T, i: int): ptr T =
  cast[ptr T](cast[uint](a) + uint(sizeof(T) * i))

proc `-`[T](a: ptr T, i: int): ptr T =
  cast[ptr T](cast[uint](a) - uint(sizeof(T) * i))

proc `-`[T](a,b: ptr T): int =
  cast[int](cast[uint](a) - cast[uint](b)) div sizeof(T)

proc `++`[T](arg: var ptr T): void =
  arg = cast[ptr T](cast[uint](arg) + uint(sizeof(T)))

proc find_median(a,b: ptr KdNode; idx: int): ptr KdNode =
  var start = a
  var endd = b
  if endd <= start:
    return nil;

  if endd == start + 1:
    return start;

  var p,store: ptr KdNode
  let md: ptr KdNode = start + (endd - start) div 2;
  var pivot: float32


  while true:
    pivot = md.x.arr[idx]

    swap(md, endd - 1)

    p = start
    store = p

    while p < endd:
      if p.x.arr[idx] < pivot:
        if p != store:
          swap(p, store);

        ++store

      ++p

    swap(store, endd - 1);

    # median has duplicate values
    if store.x.arr[idx] == md.x.arr[idx]:
      return md;

    if store > md:
      endd = store;
    else:
      start = store;

proc make_tree(t: ptr KdNode, len, i, dim: int): ptr KdNode =
  if len == 0:
    return nil

  result = find_median(t, t + len, i)

  if result != nil:
    let j = (i + 1) mod dim
    result.left = make_tree(t, result - t, j, dim)
    result.right = make_tree(result + 1, t + len - (result + 1), j, dim)


# global variable, so sue me

var visited: int

proc nearest(root, nd: ptr KdNode, i, dim: int; best: var ptr KdNode, best_dist: var float32): void =
  if root == nil:
    return

  let d = dist(root, nd, dim)
  let dx = root.x.arr[i] - nd.x[i]
  let dx2 = dx * dx

  visited += 1

  if best == nil or d < best_dist:
    best_dist = d
    best = root


  if best_dist == 0:
     return #(perfect match)

  let j = (i + 1) mod dim

  nearest(if 0 < dx: root.left else: root.right, nd, j, dim, best, best_dist)
  if (dx2 >= best_dist):
    return
  nearest(if 0 < dx: root.right else: root.left, nd, j, dim, best, best_dist)


const N = 1000000

import random
proc rand1(): float32 =
  float32(random.random(1.0f))
proc rand_pt(v: var KdNode): void =
  v.x.arr = [rand1(), rand1(), rand1()]


#define rand1() (rand() / (double)RAND_MAX)
#define rand_pt(v) { v.x[0] = rand1(); v.x[1] = rand1(); v.x[2] = rand1(); }

proc main(): void =
  var i = 0
  var wp : seq[KdNode] = @[
    KdNode(x: vec3f(2,3,0)),
    KdNode(x: vec3f(5,4,0)),
    KdNode(x: vec3f(9,6,0)),
    KdNode(x: vec3f(4,7,0)),
    KdNode(x: vec3f(8,1,0)),
    KdNode(x: vec3f(7,2,0))
  ]

  var testNode = KdNode(x: vec3f(2,3,0))
  var root, found: ptr KdNode
  var best_dist: float32

  root = make_tree(wp[0].addr, wp.len, 0, 2)

  visited = 0;
  found = nil;

  nearest(root, testNode.addr, 0, 2, found, best_dist);

  echo ">> WP tree\nsearching for (", testNode.x[0] ,", ", testNode.x[1],")"
  echo "found (", found.x[0], ", ", found.x[1], ") dist ", sqrt(best_dist)
  echo "seen ", visited, " nodes\n"


  var million = newSeq[KdNode](N)
  for i in 0 ..< N:
    rand_pt(million[i])

  root = make_tree(million[0].addr, N, 0, 3);
  rand_pt(testNode);

  visited = 0;
  found = nil;
  nearest(root, testNode.addr, 0, 3, found, best_dist);

  rand_pt(testNode);

  visited = 0;
  found = nil;
  nearest(root, testNode.addr, 0, 3, found, best_dist);

  echo ">> Million tree"
  echo "searching for ", testNode.x
  echo "found ", found.x, " dist ", sqrt(best_dist)
  echo "seen ", visited

  #[ search many random points in million tree to see average behavior
     tree size vs avg nodes visited:
     10    ~  7
     100   ~ 16.5
     1000    ~ 25.5
     10000     ~ 32.8
     100000    ~ 38.3
     1000000   ~ 42.6
     10000000  ~ 46.7        ]#
  var sum: int = 0
  var test_runs: int = 100000;
  for i in 0 ..< test_runs:
    found = nil
    visited = 0
    rand_pt(testNode)
    nearest(root, testNode.addr, 0, 3, found, best_dist)
    sum += visited


  echo "Million tree"
  echo "visited ", sum ," nodes for ", test_runs,
       " random findings (", sum / test_runs, " per lookup)\n"

when isMainModule:
  main()

#[
{
  int i;
  struct kd_node_t wp[] = {
    {{2, 3}}, {{5, 4}}, {{9, 6}}, {{4, 7}}, {{8, 1}}, {{7, 2}}
  };
  struct kd_node_t testNode = {{9, 2}};
  struct kd_node_t *root, *found, *million;
  double best_dist;

  root = make_tree(wp, sizeof(wp) / sizeof(wp[1]), 0, 2);

  visited = 0;
  found = 0;
  nearest(root, &testNode, 0, 2, &found, &best_dist);

  printf(">> WP tree\nsearching for (%g, %g)\n"
      "found (%g, %g) dist %g\nseen %d nodes\n\n",
      testNode.x[0], testNode.x[1],
      found->x[0], found->x[1], sqrt(best_dist), visited);

  million =(struct kd_node_t*) calloc(N, sizeof(struct kd_node_t));
  srand(time(0));
  for (i = 0; i < N; i++) rand_pt(million[i]);

  root = make_tree(million, N, 0, 3);
  rand_pt(testNode);

  visited = 0;
  found = 0;
  nearest(root, &testNode, 0, 3, &found, &best_dist);

  printf(">> Million tree\nsearching for (%g, %g, %g)\n"
      "found (%g, %g, %g) dist %g\nseen %d nodes\n",
      testNode.x[0], testNode.x[1], testNode.x[2],
      found->x[0], found->x[1], found->x[2],
      sqrt(best_dist), visited);

  # search many random points in million tree to see average behavior
     tree size vs avg nodes visited:
     10    ~  7
     100   ~ 16.5
     1000    ~ 25.5
     10000     ~ 32.8
     100000    ~ 38.3
     1000000   ~ 42.6
     10000000  ~ 46.7        */
  int sum = 0, test_runs = 100000;
  for (i = 0; i < test_runs; i++) {
    found = 0;
    visited = 0;
    rand_pt(testNode);
    nearest(root, &testNode, 0, 3, &found, &best_dist);
    sum += visited;
  }
  printf("\n>> Million tree\n"
      "visited %d nodes for %d random findings (%f per lookup)\n",
      sum, test_runs, sum/(double)test_runs);

  // free(million);

  return 0;
}
]#
