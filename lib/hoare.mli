type triple

val mk_triple : Assertion.t -> Imp.com -> Assertion.t -> triple
val pp_triple : Format.formatter -> triple -> unit

(* Strongest postcondition calculus (aka forward reasoning rules) *)
val rule_skip   : Assertion.t -> triple
val rule_asgn   : Assertion.t ->
                  string -> Imp.aexp -> triple
val rule_if     : Assertion.t ->
                  Imp.bexp -> Imp.com -> Imp.com -> triple
val rule_while  : Assertion.t ->
                  Imp.bexp -> Imp.com -> triple
val rule_mono   : Assertion.entailment -> triple ->
                  Assertion.entailment -> triple
