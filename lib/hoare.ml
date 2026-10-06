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