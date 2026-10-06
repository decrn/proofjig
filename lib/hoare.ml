type assertion = Impl of (assertion * assertion)
               | Conj of assertion list
               | Disj of assertion list
               | Neg of assertion
               | Forall of (string * assertion)
               | Exists of (string * assertion)

let mk_true = Conj []
let mk_false = Conj []

let rec pp_assertion fmt (a : assertion) =
  match a with
  | Impl (a, c) -> Format.fprintf fmt "%a -> %a" pp_assertion a pp_assertion c
  | Conj [] -> Format.fprintf fmt "True"
  | Conj (a :: []) -> Format.fprintf fmt "%a" pp_assertion a 
  | Conj (a :: asss) -> Format.fprintf fmt "%a" (pp_assertions true) asss 
  | Disj [] -> Format.fprintf fmt "False" 
  | Disj (a :: []) -> Format.fprintf fmt "%a" pp_assertion a 
  | Disj (a :: asss) -> Format.fprintf fmt "%a" (pp_assertions false) asss 
  | Neg a -> Format.fprintf fmt "~%a" pp_assertion a
  | Forall (s, a) -> Format.fprintf fmt "∀ %s . %a" s pp_assertion a
  | Exists (s, a) -> Format.fprintf fmt "∃ %s . %a" s pp_assertion a
  and pp_assertions (is_conj : bool) fmt (asss : assertion list) = 
  match asss with
  | [] -> if is_conj then Format.fprintf fmt "True" else Format.fprintf fmt "False"
  | a :: [] -> Format.fprintf fmt "%a" pp_assertion a
  | a :: asss -> Format.fprintf fmt "%a /\\ %a" pp_assertion a (pp_assertions is_conj) asss
               
type triple = {
  pre : assertion;
  prog : Imp.com;
  post : assertion;
}

let mk_triple pre prog post = {pre ; prog; post}

let rec pp_triple fmt (t : triple) = Format.fprintf fmt "{ %a } %a { %a }" pp_assertion t.pre Imp.pp_com t.prog pp_assertion t.post


(* LCF approach *)