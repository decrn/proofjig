type t = Eq of (Imp.aexp * Imp.aexp)
       | Bexp of Imp.bexp
       | Impl of (t * t)
       | Conj of t list
       | Disj of t list
       | Neg of t
       | Forall of (string * t)
       | Exists of (string * t)

let mk_true = Conj []
let mk_false = Disj []

let mk_eq a1 a2 = Eq (a1, a2)
let mk_bexp b = Bexp b
let mk_impl a c = Impl (a, c)
let mk_conj l = Conj l
let mk_disj l = Disj l
let mk_neg a = Neg a
let mk_forall x a = Forall (x, a)
let mk_exists x a = Exists (x, a)

let rec free_vars (a : t) =
  match a with
  | Eq (a1, a2) -> Imp.vars_aexp a1 @ Imp.vars_aexp a2
  | Bexp b -> Imp.vars_bexp b
  | Impl (a, c) -> free_vars a @ free_vars c
  | Conj l | Disj l -> List.concat_map free_vars l
  | Neg a -> free_vars a
  | Forall (y, a) | Exists (y, a) -> List.filter (fun z -> z <> y) (free_vars a)

(* all variable names occurring anywhere, bound or free *)
let rec all_vars (a : t) =
  match a with
  | Forall (y, a) | Exists (y, a) -> y :: all_vars a
  | Impl (a, c) -> all_vars a @ all_vars c
  | Conj l | Disj l -> List.concat_map all_vars l
  | Neg a -> all_vars a
  | Eq _ | Bexp _ -> free_vars a

let fresh_var avoid x =
  let rec go i =
    let y = x ^ string_of_int i in
    if List.mem y avoid then go (i + 1) else y
  in
  go 0

(* capture-avoiding substitution a[e/x] *)
let rec subst x e (a : t) =
  match a with
  | Eq (a1, a2) -> Eq (Imp.subst_aexp x e a1, Imp.subst_aexp x e a2)
  | Bexp b -> Bexp (Imp.subst_bexp x e b)
  | Impl (a, c) -> Impl (subst x e a, subst x e c)
  | Conj l -> Conj (List.map (subst x e) l)
  | Disj l -> Disj (List.map (subst x e) l)
  | Neg a -> Neg (subst x e a)
  | Forall (y, body) -> subst_binder (fun y b -> Forall (y, b)) x e y body
  | Exists (y, body) -> subst_binder (fun y b -> Exists (y, b)) x e y body

and subst_binder mk x e y body =
  if y = x || not (List.mem x (free_vars body)) then
    if y = x then mk y body else mk y (subst x e body)
  else if List.mem y (Imp.vars_aexp e) then
    let y' = fresh_var (x :: Imp.vars_aexp e @ all_vars body) y in
    mk y' (subst x e (subst y (Imp.Var y') body))
  else mk y (subst x e body)

let rec pp_t fmt (a : t) =
  match a with
  | Eq (a1, a2) -> Format.fprintf fmt "%a = %a" Imp.pp_aexp a1 Imp.pp_aexp a2
  | Bexp b -> Imp.pp_bexp fmt b
  | Impl (a, c) -> Format.fprintf fmt "%a -> %a" pp_t a pp_t c
  | Conj [] -> Format.fprintf fmt "True"
  | Conj (a :: []) -> Format.fprintf fmt "%a" pp_t a 
  | Conj (a :: asss) -> Format.fprintf fmt "(%a)" (pp_ts true) (a :: asss) 
  | Disj [] -> Format.fprintf fmt "False" 
  | Disj (a :: []) -> Format.fprintf fmt "%a" pp_t a 
  | Disj (a :: asss) -> Format.fprintf fmt "(%a)" (pp_ts false) (a :: asss) 
  | Neg a -> Format.fprintf fmt "~%a" pp_t a
  | Forall (s, a) -> Format.fprintf fmt "∀ %s . %a" s pp_t a
  | Exists (s, a) -> Format.fprintf fmt "∃ %s . %a" s pp_t a
  and pp_ts (is_conj : bool) fmt (asss : t list) = 
  match asss with
  | [] -> if is_conj then Format.fprintf fmt "True" else Format.fprintf fmt "False"
  | a :: [] -> Format.fprintf fmt "%a" pp_t a
  | a :: asss -> Format.fprintf fmt (if is_conj then "%a /\\ %a" else "%a \\/ %a") pp_t a (pp_ts is_conj) asss
               
type entailment = Entails
let true_impl_true = Entails
let rule_intro_true (a : t) = Entails
let rule_axiom (a : t) = Entails
let rule_elim_false (a : t) = Entails
let rule_trans e1 e2 = Entails