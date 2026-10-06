open Hoare
open Imp

let rule_skip a = mk_triple a CSkip a

(* TODO all these... *)
let rule_asgn a x v = mk_triple a (CAsgn (x, v)) a
let rule_if a b con alt = mk_triple a (CIf (b, con, alt)) a
let rule_while a b c = mk_triple a (CWhile (b, c)) a
let rule_mono e1 t1 e2 = t1