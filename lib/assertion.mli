type t (* the type of assertions *)

val pp_t : Format.formatter -> t -> unit
val mk_true : t
val mk_false : t
val mk_eq : Imp.aexp -> Imp.aexp -> t
val mk_bexp : Imp.bexp -> t
val mk_impl : t -> t -> t
val mk_conj : t list -> t
val mk_disj : t list -> t
val mk_neg : t -> t
val mk_forall : string -> t -> t
val mk_exists : string -> t -> t

type entailment (* the type of proofs that one assertion implies another *)

val true_impl_true : entailment (* True *)
val rule_intro_true : t -> entailment (* P -> True *)
val rule_axiom : t -> entailment (* P -> P *)
val rule_elim_false : t -> entailment (* False -> P *)
val rule_trans : entailment -> entailment -> entailment (* (P -> Q) -> (Q -> R) -> (P -> R) *)