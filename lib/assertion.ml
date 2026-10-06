type t = Impl of (t * t)
       | Conj of t list
       | Disj of t list
       | Neg of t
       | Forall of (string * t)
       | Exists of (string * t)

let mk_true = Conj []
let mk_false = Disj []

let rec pp_t fmt (a : t) =
  match a with
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
let true_impl_true = assert false
let rule_intro_true = assert false
let rule_axiom = assert false
let rule_elim_false = assert false
let rule_trans = assert false