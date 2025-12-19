(** Complex number expression AST and evaluation *)

open Complex
type complex = t

let complex re im = { re; im }

let c_add = add
let c_sub = sub
let c_mul = mul
let c_div = div

let c_abs = norm
let c_arg = arg

let c_exp = exp

let c_log = log

let c_sin z =
  { re = sin z.re *. cosh z.im;
    im = cos z.re *. sinh z.im }

let c_cos z =
  { re = cos z.re *. cosh z.im;
    im = -. sin z.re *. sinh z.im }

let c_sinh z =
  { re = sinh z.re *. cos z.im;
    im = cosh z.re *. sin z.im }

let c_cosh z =
  { re = cosh z.re *. cos z.im;
    im = sinh z.re *. sin z.im }

let c_tanh z =
  let s = c_sinh z in
  let c = c_cosh z in
  c_div s c

let c_pow = pow

let c_sqrt = sqrt
let c_conj = conj

let c_abs_c z = { re = c_abs z; im = 0.0 }
let c_arg_c z = { re = c_arg z; im = 0.0 }
let c_re z = { re = z.re; im = 0.0 }
let c_im z = { re = z.im; im = 0.0 }

(* Spiral function - creates interesting patterns *)
let c_spiral z =
  let r = c_abs z in
  let theta = c_arg z in
  { re = r *. cos (theta +. r);
    im = r *. sin (theta +. r) }

(* Wave function - creates wave-like patterns *)
let c_wave z =
  { re = sin (z.re *. 3.0) +. cos (z.im *. 2.0);
    im = cos (z.re *. 2.0) +. sin (z.im *. 3.0) }

(* Clamp to avoid infinities *)
let c_clamp z =
  let clamp_val v =
    if Float.is_nan v then 0.0
    else if v > 1e6 then 1e6
    else if v < -1e6 then -1e6
    else v
  in
  { re = clamp_val z.re; im = clamp_val z.im }

(* Max and Min by magnitude *)
let c_max a b = if c_abs a >= c_abs b then a else b
let c_min a b = if c_abs a <= c_abs b then a else b

(** Expression AST *)
type expr =
  | Var                         (* The input variable z *)
  | Const of complex            (* A constant complex number *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Exp of expr
  | Log of expr
  | Sin of expr
  | Cos of expr
  | Sinh of expr
  | Cosh of expr
  | Tanh of expr
  | Sqrt of expr
  | Conj of expr                (* Complex conjugate *)
  | Abs of expr                 (* Returns |z| as real *)
  | Arg of expr                 (* Returns arg(z) as real *)
  | Re of expr                  (* Real part *)
  | Im of expr                  (* Imaginary part *)
  | Spiral of expr              (* Custom spiral transform *)
  | Wave of expr                (* Custom wave transform *)
  | Loop of expr * expr         (* Loop(n, body): iterate body floor(|n|) times *)
  | Max of expr * expr          (* Value with larger magnitude *)
  | Min of expr * expr          (* Value with smaller magnitude *)

(** Evaluate an expression given z *)
let rec eval z = function
  | Var -> z
  | Const c -> c
  | Add (a, b) -> c_clamp (c_add (eval z a) (eval z b))
  | Sub (a, b) -> c_clamp (c_sub (eval z a) (eval z b))
  | Mul (a, b) -> c_clamp (c_mul (eval z a) (eval z b))
  | Div (a, b) -> c_clamp (c_div (eval z a) (eval z b))
  | Pow (a, b) -> c_clamp (c_pow (eval z a) (eval z b))
  | Exp e -> c_clamp (c_exp (eval z e))
  | Log e -> c_clamp (c_log (eval z e))
  | Sin e -> c_clamp (c_sin (eval z e))
  | Cos e -> c_clamp (c_cos (eval z e))
  | Sinh e -> c_clamp (c_sinh (eval z e))
  | Cosh e -> c_clamp (c_cosh (eval z e))
  | Tanh e -> c_clamp (c_tanh (eval z e))
  | Sqrt e -> c_clamp (c_sqrt (eval z e))
  | Conj e -> c_conj (eval z e)
  | Abs e -> c_abs_c (eval z e)
  | Arg e -> c_arg_c (eval z e)
  | Re e -> c_re (eval z e)
  | Im e -> c_im (eval z e)
  | Spiral e -> c_clamp (c_spiral (eval z e))
  | Wave e -> c_clamp (c_wave (eval z e))
  | Loop (n_expr, body) ->
    let n_val = eval z n_expr in
    let n_abs = min 8.0 (max 0.0 (c_abs n_val)) in
    let n_floor = int_of_float (floor n_abs) in
    let n_ceil = int_of_float (ceil n_abs) in
    let frac = n_abs -. floor n_abs in
    let rec loop_iter i acc =
      if i <= 0 then acc
      else loop_iter (i - 1) (c_clamp (eval acc body))
    in
    let result_floor = loop_iter n_floor z in
    if frac < 1e-9 then result_floor
    else
      let result_ceil = loop_iter n_ceil z in
      (* Interpolate between floor and ceil results *)
      c_add (c_mul { re = 1.0 -. frac; im = 0.0 } result_floor)
            (c_mul { re = frac; im = 0.0 } result_ceil)
  | Max (a, b) -> c_max (eval z a) (eval z b)
  | Min (a, b) -> c_min (eval z a) (eval z b)

(** Pretty print expression *)
let rec to_string = function
  | Var -> "z"
  | Const c ->
    if c.im >= 0.0 then Printf.sprintf "(%.3f+%.3fi)" c.re c.im
    else Printf.sprintf "(%.3f%.3fi)" c.re c.im
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (to_string a) (to_string b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (to_string a) (to_string b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (to_string a) (to_string b)
  | Div (a, b) -> Printf.sprintf "(%s / %s)" (to_string a) (to_string b)
  | Pow (a, b) -> Printf.sprintf "(%s ^ %s)" (to_string a) (to_string b)
  | Exp e -> Printf.sprintf "exp(%s)" (to_string e)
  | Log e -> Printf.sprintf "log(%s)" (to_string e)
  | Sin e -> Printf.sprintf "sin(%s)" (to_string e)
  | Cos e -> Printf.sprintf "cos(%s)" (to_string e)
  | Sinh e -> Printf.sprintf "sinh(%s)" (to_string e)
  | Cosh e -> Printf.sprintf "cosh(%s)" (to_string e)
  | Tanh e -> Printf.sprintf "tanh(%s)" (to_string e)
  | Sqrt e -> Printf.sprintf "sqrt(%s)" (to_string e)
  | Conj e -> Printf.sprintf "conj(%s)" (to_string e)
  | Abs e -> Printf.sprintf "|%s|" (to_string e)
  | Arg e -> Printf.sprintf "arg(%s)" (to_string e)
  | Re e -> Printf.sprintf "Re(%s)" (to_string e)
  | Im e -> Printf.sprintf "Im(%s)" (to_string e)
  | Spiral e -> Printf.sprintf "spiral(%s)" (to_string e)
  | Wave e -> Printf.sprintf "wave(%s)" (to_string e)
  | Loop (n, body) -> Printf.sprintf "loop(%s, %s)" (to_string n) (to_string body)
  | Max (a, b) -> Printf.sprintf "max(%s, %s)" (to_string a) (to_string b)
  | Min (a, b) -> Printf.sprintf "min(%s, %s)" (to_string a) (to_string b)

(** Count nodes in expression (for complexity control) *)
let rec size = function
  | Var | Const _ -> 1
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
  | Loop (a, b) | Max (a, b) | Min (a, b) ->
    1 + size a + size b
  | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
  | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
    1 + size e

(** Maximum depth of expression tree *)
let rec depth = function
  | Var | Const _ -> 1
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
  | Loop (a, b) | Max (a, b) | Min (a, b) ->
    1 + max (depth a) (depth b)
  | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
  | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
    1 + depth e

(** Extract all constants from an expression with their indices *)
let collect_constants expr =
  let rec aux idx expr acc =
    match expr with
    | Var -> (acc, idx)
    | Const c -> ((idx, c) :: acc, idx + 1)
    | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Pow (a, b)
    | Loop (a, b) | Max (a, b) | Min (a, b) ->
      let (acc, idx) = aux idx a acc in
      aux idx b acc
    | Exp e | Log e | Sin e | Cos e | Sinh e | Cosh e | Tanh e
    | Sqrt e | Conj e | Abs e | Arg e | Re e | Im e | Spiral e | Wave e ->
      aux idx e acc
  in
  let (consts, _) = aux 0 expr [] in
  List.rev consts

(** Replace the constant at a given index with a new value *)
let replace_constant idx new_val expr =
  let counter = ref 0 in
  let rec aux expr =
    match expr with
    | Var -> Var
    | Const _ ->
      let current = !counter in
      incr counter;
      if current = idx then Const new_val else expr
    | Add (a, b) -> let a = aux a in let b = aux b in Add (a, b)
    | Sub (a, b) -> let a = aux a in let b = aux b in Sub (a, b)
    | Mul (a, b) -> let a = aux a in let b = aux b in Mul (a, b)
    | Div (a, b) -> let a = aux a in let b = aux b in Div (a, b)
    | Pow (a, b) -> let a = aux a in let b = aux b in Pow (a, b)
    | Loop (a, b) -> let a = aux a in let b = aux b in Loop (a, b)
    | Max (a, b) -> let a = aux a in let b = aux b in Max (a, b)
    | Min (a, b) -> let a = aux a in let b = aux b in Min (a, b)
    | Exp e -> Exp (aux e)
    | Log e -> Log (aux e)
    | Sin e -> Sin (aux e)
    | Cos e -> Cos (aux e)
    | Sinh e -> Sinh (aux e)
    | Cosh e -> Cosh (aux e)
    | Tanh e -> Tanh (aux e)
    | Sqrt e -> Sqrt (aux e)
    | Conj e -> Conj (aux e)
    | Abs e -> Abs (aux e)
    | Arg e -> Arg (aux e)
    | Re e -> Re (aux e)
    | Im e -> Im (aux e)
    | Spiral e -> Spiral (aux e)
    | Wave e -> Wave (aux e)
  in
  aux expr
