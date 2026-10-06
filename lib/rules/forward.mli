(* Strongest postcondition calculus (aka forward reasoning rules) *)

val rule_skip   : Assertion.t -> Hoare.triple
val rule_asgn   : Assertion.t ->
                  string -> Imp.aexp -> Hoare.triple
val rule_if     : Assertion.t ->
                  Imp.bexp -> Imp.com -> Imp.com -> Hoare.triple
val rule_while  : Assertion.t ->
                  Imp.bexp -> Imp.com -> Hoare.triple
val rule_mono   : Assertion.entailment -> Hoare.triple ->
                  Assertion.entailment -> Hoare.triple
