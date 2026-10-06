open Proofjig.Imp
open Proofjig.Hoare

let prog = CSeq (CAsgn ("x", Num 45), CSkip)
let _ = Format.printf "%a" pp_triple (mk_triple mk_true prog mk_true)
