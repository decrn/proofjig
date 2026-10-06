open Proofjig.Imp

let _ = Format.printf "%a" pp_com (CSeq (CAsgn ("x", Num 45), CSkip))
