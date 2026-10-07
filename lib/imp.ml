type aexp = Num of int | Var of string | Add of (aexp * aexp) | Mul of (aexp * aexp)

let rec pp_aexp fmt (a : aexp) =
  match a with
  | Num n -> Format.fprintf fmt "%n" n
  | Var x -> Format.fprintf fmt "%s" x
  | Add (a1, a2) -> Format.fprintf fmt "%a + %a" pp_aexp a1 pp_aexp a2
  | Mul (a1, a2) -> Format.fprintf fmt "%a * %a" pp_aexp a1 pp_aexp a2

let rec subst_aexp x e (a : aexp) =
  match a with
  | Num _ -> a
  | Var y -> if y = x then e else a
  | Add (a1, a2) -> Add (subst_aexp x e a1, subst_aexp x e a2)
  | Mul (a1, a2) -> Mul (subst_aexp x e a1, subst_aexp x e a2)

let rec vars_aexp (a : aexp) =
  match a with
  | Num _ -> []
  | Var y -> [y]
  | Add (a1, a2) | Mul (a1, a2) -> vars_aexp a1 @ vars_aexp a2

type bexp = True | False | And of (bexp * bexp) | Or of (bexp * bexp) | Le of (aexp * aexp)

let rec pp_bexp fmt (b : bexp) =
  match b with
  | True -> Format.fprintf fmt "True"
  | False -> Format.fprintf fmt "False" 
  | And (b1, b2) -> Format.fprintf fmt "%a /\\ %a" pp_bexp b1 pp_bexp b2
  | Or (b1, b2) -> Format.fprintf fmt "%a \\/ %a" pp_bexp b1 pp_bexp b2
  | Le (a1, a2) -> Format.fprintf fmt "%a < %a" pp_aexp a1 pp_aexp a2

let rec subst_bexp x e (b : bexp) =
  match b with
  | True | False -> b
  | And (b1, b2) -> And (subst_bexp x e b1, subst_bexp x e b2)
  | Or (b1, b2) -> Or (subst_bexp x e b1, subst_bexp x e b2)
  | Le (a1, a2) -> Le (subst_aexp x e a1, subst_aexp x e a2)

let rec vars_bexp (b : bexp) =
  match b with
  | True | False -> []
  | And (b1, b2) | Or (b1, b2) -> vars_bexp b1 @ vars_bexp b2
  | Le (a1, a2) -> vars_aexp a1 @ vars_aexp a2

type com = CSkip
         | CAsgn of (string * aexp)
         | CSeq of (com * com)
         | CIf of (bexp * com * com)
         | CWhile of (bexp * com)

let rec pp_com fmt (c : com) =
  match c with
  | CSkip -> Format.fprintf fmt "skip"
  | CAsgn (n, v) -> Format.fprintf fmt "%s := %a" n pp_aexp v
  | CSeq (c1, c2) -> Format.fprintf fmt "%a; %a" pp_com c1 pp_com c2
  | CIf (b, c, a) -> Format.fprintf fmt "if (%a) then { %a } else { %a }" pp_bexp b pp_com c pp_com a
  | CWhile (b, c) -> Format.fprintf fmt "while (%a) { %a }" pp_bexp b pp_com c
