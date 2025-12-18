(** Mutation operations for evolving expressions *)

open Complex_expr

(** Complexity evolution mode *)
type complexity_mode =
  | Decrease      (* Favor simplification *)
  | Maintain      (* Keep roughly the same complexity *)
  | SlowIncrease  (* Gradually increase complexity *)
  | FastIncrease  (* Rapidly increase complexity *)

let complexity_mode_to_string = function
  | Decrease -> "Decrease"
  | Maintain -> "Maintain"
  | SlowIncrease -> "Slow+"
  | FastIncrease -> "Fast+"

let all_complexity_modes = [| Decrease; Maintain; SlowIncrease; FastIncrease |]

(** Random number generator state *)
let rng = Random.State.make_self_init ()

let random_float lo hi =
  lo +. Random.State.float rng (hi -. lo)

let random_int n =
  Random.State.int rng n

let random_bool () =
  Random.State.bool rng

(** Generate a random constant *)
let random_const () =
  let re = random_float (-2.0) 2.0 in
  let im = random_float (-2.0) 2.0 in
  Const (complex re im)

(** Generate a small perturbation to a constant *)
let perturb_const c =
  let delta = 0.3 in
  complex (c.re +. random_float (-.delta) delta)
          (c.im +. random_float (-.delta) delta)

(** List of unary operations *)
let unary_ops = [|
  (fun e -> Exp e);
  (fun e -> Log e);
  (fun e -> Sin e);
  (fun e -> Cos e);
  (fun e -> Sinh e);
  (fun e -> Cosh e);
  (fun e -> Tanh e);
  (fun e -> Sqrt e);
  (fun e -> Conj e);
  (fun e -> Abs e);
  (fun e -> Arg e);
  (fun e -> Re e);
  (fun e -> Im e);
  (fun e -> Spiral e);
  (fun e -> Wave e);
|]

(** List of binary operations *)
let binary_ops = [|
  (fun a b -> Add (a, b));
  (fun a b -> Sub (a, b));
  (fun a b -> Mul (a, b));
  (fun a b -> Div (a, b));
  (fun a b -> Pow (a, b));
  (fun a b -> Loop (a, b));
|]

(** Generate a random simple expression *)
let rec random_expr max_depth =
  if max_depth <= 1 then
    if random_bool () then Var else random_const ()
  else
    match random_int 10 with
    | 0 -> Var
    | 1 -> random_const ()
    | 2 | 3 | 4 | 5 ->
      (* Unary operation *)
      let op = unary_ops.(random_int (Array.length unary_ops)) in
      op (random_expr (max_depth - 1))
    | _ ->
      (* Binary operation *)
      let op = binary_ops.(random_int (Array.length binary_ops)) in
      let d1 = 1 + random_int (max_depth - 1) in
      let d2 = 1 + random_int (max_depth - 1) in
      op (random_expr d1) (random_expr d2)

(** Count all nodes and assign indices *)
let rec count_nodes expr =
  match expr with
  | Var | Const _ -> 1
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
  | Loop (a, b) ->
    1 + count_nodes a + count_nodes b
  | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
  | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
    1 + count_nodes e

(** Get the nth node (0-indexed, pre-order) *)
let rec get_node expr n =
  if n = 0 then Some expr
  else
    match expr with
    | Var | Const _ -> None
    | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
    | Loop (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then get_node a (n - 1)
      else get_node b (n - 1 - left_size)
    | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
    | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
      get_node e (n - 1)

(** Replace the nth node *)
let rec replace_node expr n replacement =
  if n = 0 then replacement
  else
    match expr with
    | Var | Const _ -> expr
    | Add (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Add (replace_node a (n - 1) replacement, b)
      else Add (a, replace_node b (n - 1 - left_size) replacement)
    | Sub (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Sub (replace_node a (n - 1) replacement, b)
      else Sub (a, replace_node b (n - 1 - left_size) replacement)
    | Mul (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Mul (replace_node a (n - 1) replacement, b)
      else Mul (a, replace_node b (n - 1 - left_size) replacement)
    | Div (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Div (replace_node a (n - 1) replacement, b)
      else Div (a, replace_node b (n - 1 - left_size) replacement)
    | Pow (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Pow (replace_node a (n - 1) replacement, b)
      else Pow (a, replace_node b (n - 1 - left_size) replacement)
    | Exp e -> Exp (replace_node e (n - 1) replacement)
    | Log e -> Log (replace_node e (n - 1) replacement)
    | Sin e -> Sin (replace_node e (n - 1) replacement)
    | Cos e -> Cos (replace_node e (n - 1) replacement)
    | Sinh e -> Sinh (replace_node e (n - 1) replacement)
    | Cosh e -> Cosh (replace_node e (n - 1) replacement)
    | Tanh e -> Tanh (replace_node e (n - 1) replacement)
    | Sqrt e -> Sqrt (replace_node e (n - 1) replacement)
    | Conj e -> Conj (replace_node e (n - 1) replacement)
    | Abs e -> Abs (replace_node e (n - 1) replacement)
    | Arg e -> Arg (replace_node e (n - 1) replacement)
    | Re e -> Re (replace_node e (n - 1) replacement)
    | Im e -> Im (replace_node e (n - 1) replacement)
    | Spiral e -> Spiral (replace_node e (n - 1) replacement)
    | Wave e -> Wave (replace_node e (n - 1) replacement)
    | Loop (a, b) ->
      let left_size = count_nodes a in
      if n - 1 < left_size then Loop (replace_node a (n - 1) replacement, b)
      else Loop (a, replace_node b (n - 1 - left_size) replacement)

(** Mutation: replace a random subtree *)
let mutate_replace expr =
  let n = count_nodes expr in
  let idx = random_int n in
  let new_subtree = random_expr (2 + random_int 2) in
  replace_node expr idx new_subtree

(** Mutation: wrap a random node with a unary operation *)
let mutate_wrap expr =
  let n = count_nodes expr in
  let idx = random_int n in
  match get_node expr idx with
  | None -> expr
  | Some node ->
    let op = unary_ops.(random_int (Array.length unary_ops)) in
    replace_node expr idx (op node)

(** Mutation: insert a binary operation *)
let mutate_insert_binary expr =
  let n = count_nodes expr in
  let idx = random_int n in
  match get_node expr idx with
  | None -> expr
  | Some node ->
    let op = binary_ops.(random_int (Array.length binary_ops)) in
    let other = if random_bool () then Var else random_const () in
    let new_node = if random_bool () then op node other else op other node in
    replace_node expr idx new_node

(** Mutation: simplify by removing a layer *)
let mutate_simplify expr =
  let n = count_nodes expr in
  if n <= 1 then expr
  else
    let idx = random_int n in
    match get_node expr idx with
    | None -> expr
    | Some node ->
      (* Try to extract a child *)
      let child = match node with
        | Add (a, _) | Sub (a, _) | Mul (a, _) | Div (a, _) | Pow (a, _)
        | Loop (a, _) ->
          if random_bool () then Some a else
            (match node with
             | Add (_, b) | Sub (_, b) | Mul (_, b) | Div (_, b) | Pow (_, b)
             | Loop (_, b) -> Some b
             | _ -> None)
        | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
        | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
          Some e
        | Var | Const _ -> None
      in
      match child with
      | Some c -> replace_node expr idx c
      | None -> expr

(** Mutation: perturb constants *)
let rec mutate_constants expr prob =
  match expr with
  | Var -> Var
  | Const c ->
    if random_float 0.0 1.0 < prob then Const (perturb_const c) else Const c
  | Add (a, b) -> Add (mutate_constants a prob, mutate_constants b prob)
  | Sub (a, b) -> Sub (mutate_constants a prob, mutate_constants b prob)
  | Mul (a, b) -> Mul (mutate_constants a prob, mutate_constants b prob)
  | Div (a, b) -> Div (mutate_constants a prob, mutate_constants b prob)
  | Pow (a, b) -> Pow (mutate_constants a prob, mutate_constants b prob)
  | Exp e -> Exp (mutate_constants e prob)
  | Log e -> Log (mutate_constants e prob)
  | Sin e -> Sin (mutate_constants e prob)
  | Cos e -> Cos (mutate_constants e prob)
  | Sinh e -> Sinh (mutate_constants e prob)
  | Cosh e -> Cosh (mutate_constants e prob)
  | Tanh e -> Tanh (mutate_constants e prob)
  | Sqrt e -> Sqrt (mutate_constants e prob)
  | Conj e -> Conj (mutate_constants e prob)
  | Abs e -> Abs (mutate_constants e prob)
  | Arg e -> Arg (mutate_constants e prob)
  | Re e -> Re (mutate_constants e prob)
  | Im e -> Im (mutate_constants e prob)
  | Spiral e -> Spiral (mutate_constants e prob)
  | Wave e -> Wave (mutate_constants e prob)
  | Loop (a, b) -> Loop (mutate_constants a prob, mutate_constants b prob)

(** Mutation: change a unary operation to another *)
let mutate_change_unary expr =
  let rec find_unary_indices expr idx acc =
    match expr with
    | Var | Const _ -> acc
    | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
    | Loop (a, b) ->
      let acc = find_unary_indices a (idx + 1) acc in
      find_unary_indices b (idx + 1 + count_nodes a) acc
    | Exp _ | Log _ | Sin _ | Cos _ | Sinh _ | Cosh _ | Tanh _
    | Sqrt _ | Conj _ | Abs _ | Arg _ | Re _ | Im _ | Spiral _ | Wave _ ->
      idx :: acc
  in
  let indices = find_unary_indices expr 0 [] in
  if indices = [] then expr
  else
    let idx = List.nth indices (random_int (List.length indices)) in
    match get_node expr idx with
    | None -> expr
    | Some node ->
      let child = match node with
        | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
        | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e -> e
        | _ -> Var
      in
      let op = unary_ops.(random_int (Array.length unary_ops)) in
      replace_node expr idx (op child)

(** Mutation: change a binary operation to another *)
let mutate_change_binary expr =
  let rec find_binary_indices expr idx acc =
    match expr with
    | Var | Const _ -> acc
    | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
    | Loop (a, b) ->
      let acc = idx :: acc in
      let acc = find_binary_indices a (idx + 1) acc in
      find_binary_indices b (idx + 1 + count_nodes a) acc
    | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
    | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
      find_binary_indices e (idx + 1) acc
  in
  let indices = find_binary_indices expr 0 [] in
  if indices = [] then expr
  else
    let idx = List.nth indices (random_int (List.length indices)) in
    match get_node expr idx with
    | None -> expr
    | Some node ->
      let (left, right) = match node with
        | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
        | Loop (a, b) -> (a, b)
        | _ -> (Var, Var)
      in
      let op = binary_ops.(random_int (Array.length binary_ops)) in
      replace_node expr idx (op left right)

(** Get growth bias from complexity mode *)
let growth_bias_of_mode = function
  | Decrease -> 0.2       (* 20% growth, 80% simplify/neutral *)
  | Maintain -> 0.5       (* 50/50 balance *)
  | SlowIncrease -> 0.65  (* 65% growth *)
  | FastIncrease -> 0.85  (* 85% growth *)

(** Apply a random mutation based on complexity mode *)
let mutate ?(mode=Maintain) expr =
  let growth_factor = growth_bias_of_mode mode in
  let r = random_float 0.0 1.0 in
  if r < growth_factor then
    (* Growth-promoting mutations *)
    match random_int 10 with
    | 0 | 1 | 2 -> mutate_wrap expr           (* 30% of growth: add unary wrapper *)
    | 3 | 4 | 5 -> mutate_insert_binary expr  (* 30% of growth: add binary op *)
    | 6 | 7 ->                                 (* 20% of growth: replace with larger subtree *)
      let n = count_nodes expr in
      let idx = random_int n in
      let new_subtree = random_expr (3 + random_int 3) in  (* Larger subtrees *)
      replace_node expr idx new_subtree
    | _ -> mutate_constants expr 0.3          (* 20% of growth: tweak constants *)
  else
    (* Neutral or simplifying mutations *)
    match random_int 10 with
    | 0 | 1 -> mutate_simplify expr           (* 20%: simplify *)
    | 2 | 3 -> mutate_replace expr            (* 20%: replace subtree *)
    | 4 | 5 | 6 -> mutate_change_unary expr   (* 30%: swap unary ops *)
    | 7 | 8 -> mutate_change_binary expr      (* 20%: swap binary ops *)
    | _ -> mutate_constants expr 0.5          (* 10%: tweak constants *)

(** Apply multiple mutations to create variation *)
let mutate_multi ?(mode=Maintain) expr num_mutations =
  let rec loop e n =
    if n <= 0 then e
    else loop (mutate ~mode e) (n - 1)
  in
  loop expr num_mutations

(** Generate n distinct mutations of an expression *)
let generate_variants ?(mode=Maintain) base_expr n =
  let variants = Array.make n base_expr in
  for i = 0 to n - 1 do
    (* Vary the number of mutations to create diversity *)
    let num_mutations = 1 + random_int 3 in  (* 1-3 mutations *)
    variants.(i) <- mutate_multi ~mode base_expr num_mutations
  done;
  (* Ensure all are distinct by checking string representation *)
  let seen = Hashtbl.create n in
  for i = 0 to n - 1 do
    let s = to_string variants.(i) in
    if Hashtbl.mem seen s then
      (* Generate a new one *)
      let rec try_new attempts =
        if attempts > 10 then variants.(i)
        else
          let v = mutate_multi ~mode base_expr (1 + random_int 4) in
          let sv = to_string v in
          if Hashtbl.mem seen sv then try_new (attempts + 1)
          else (Hashtbl.add seen sv true; v)
      in
      variants.(i) <- try_new 0
    else
      Hashtbl.add seen s true
  done;
  variants

(** Generate initial population of simple expressions *)
let generate_initial_population n =
  Array.init n (fun _ -> random_expr (3 + random_int 3))  (* depth 3-5 *)
