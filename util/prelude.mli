val concat_with_sep : Latex.t list -> Latex.t -> Latex.t

val nabla : Latex.t
val implies : Latex.t

val itshape : Latex.t -> Latex.t
val upalpha : Latex.t
val upbeta : Latex.t
val upgamma : Latex.t
val updelta : Latex.t
val upepsilon : Latex.t
val upeta : Latex.t
val upiota : Latex.t
val upmu : Latex.t
val uppi : Latex.t
val uprho : Latex.t
val upsigma : Latex.t
val upomega : Latex.t

val log_ : Latex.t


type pattern = Latex.t*Latex.t
val default_interp_pattern : pattern -> Latex.t

val forall : pattern list -> Latex.t -> Latex.t
val exists : pattern list -> Latex.t -> Latex.t
val union  : pattern list -> Latex.t -> Latex.t
val intersection : pattern list -> Latex.t -> Latex.t
val family : pattern list -> Latex.t -> Latex.t
val comprehension : pattern list -> Latex.t -> Latex.t

val powerset : Latex.t -> Latex.t

(**** Bibliography ****)

val cite : ?extra:Latex.t -> Latex.t -> Latex.t

(*** Holes ***)

val citation_needed : Latex.t

(*** Include graphics ***)

val includegraphics : ?height:Latex.size -> ?width:Latex.size -> ?keepaspectratio:bool
  -> Latex.t -> Latex.t

(*** Definition blocks ***)

type defline

val defline : ?side:Latex.t -> Latex.t -> Latex.t -> defline

val definition : ?symb:Latex.t -> defline list -> Latex.t

(** A short module for proof.sty *)
module Infer : sig

  (** A single deduction step *)
  val rule : ?label:Latex.t -> Latex.t list -> Latex.t -> Latex.t
  (** A single deduction step, with premises on several rules *)
  val rule_multi : ?label:Latex.t -> Latex.t list list -> Latex.t -> Latex.t
  (** Multiple deduction steps *)
  val derived : ?label:Latex.t -> Latex.t list -> Latex.t -> Latex.t

end
