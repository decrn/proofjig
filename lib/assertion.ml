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

let rec pp_t fmt (a : t) =
  match a with
  | Eq (a1, a2) -> Format.fprintf fmt "%a = %a" Imp.pp_aexp a1 Imp.pp_aexp a2
  | Bexp b -> Imp.pp_bexp fmt b
  | Impl (a, c) -> Format.fprintf fmt "%a -> %a" pp_t a pp_t c
  | Conj [] -> Format.fprintf fmt "True"
  | Conj (a :: []) -> Format.fprintf fmt "%a" pp_t a 
  | Conj (a :: asss) -> Format.fprintf fmt "(%a)" (pp_ts true) asss 
  | Disj [] -> Format.fprintf fmt "False" 
  | Disj (a :: []) -> Format.fprintf fmt "%a" pp_t a 
  | Disj (a :: asss) -> Format.fprintf fmt "(%a)" (pp_ts false) asss 
  | Neg a -> Format.fprintf fmt "~%a" pp_t a
  | Forall (s, a) -> Format.fprintf fmt "∀ %s . %a" s pp_t a
  | Exists (s, a) -> Format.fprintf fmt "∃ %s . %a" s pp_t a
  and pp_ts (is_conj : bool) fmt (asss : t list) = 
  match asss with
  | [] -> if is_conj then Format.fprintf fmt "True" else Format.fprintf fmt "False"
  | a :: [] -> Format.fprintf fmt "%a" pp_t a
  | a :: asss -> Format.fprintf fmt "%a /\\ %a" pp_t a (pp_ts is_conj) asss
               
type entailment = Entails
let true_impl_true = Entails
let rule_intro_true (a : t) = Entails
let rule_axiom (a : t) = Entails
let rule_elim_false (a : t) = Entails
let rule_trans e1 e2 = Entails