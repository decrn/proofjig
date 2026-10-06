type t (* the type of assertions *)

val pp_t : Format.formatter -> t -> unit
val mk_true : t
val mk_false : t

type entailment (* the type of proofs that one assertion implies another *)

val true_impl_true : entailment (* True *)
val rule_intro_true : t -> entailment (* P -> True *)
val rule_axiom : t -> entailment (* P -> P *)
val rule_elim_false : t -> entailment (* False -> P *)
val rule_trans : entailment -> entailment -> entailment (* (P -> Q) -> (Q -> R) -> (P -> R) *)