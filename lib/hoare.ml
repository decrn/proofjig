open Assertion

type triple = {
  pre : Assertion.t;
  prog : Imp.com;
  post : Assertion.t;
}

let mk_triple pre prog post = {pre ; prog; post}

let rec pp_triple fmt (t : triple) = Format.fprintf fmt
  "{ %a } %a { %a }"
  Assertion.pp_t t.pre
  Imp.pp_com t.prog
  Assertion.pp_t t.post

let rule_skip a = mk_triple a CSkip a
let rule_asgn a x v = assert false
let rule_if a b con alt = assert false
let rule_while a b c = assert false
let rule_mono e1 t1 e2 = assert false