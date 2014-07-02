open Latex

let rec concat_with_sep a l sep =
  match l with
  | [] -> a
  | b::l -> concat_with_sep (concat [a;sep;b]) l sep

let concat_with_sep l sep =
  match l with
  | [] -> empty
  | a::l -> concat_with_sep a l sep


let nabla = command "nabla" [] M
let implies = command "implies" ~packages:["amsmath",""] [] M

let itshape x = block(unusual_command "itshape" [T,nobr,x] T)
let upalpha = command "upalpha" ~packages:["upgreek",""] [] M
let upbeta = command "upbeta" ~packages:["upgreek",""] [] M
let upgamma = command "upgamma" ~packages:["upgreek",""] [] M
let updelta = command "updelta" ~packages:["upgreek",""] [] M
let upepsilon =  command "upepsilon" ~packages:["upgreek",""] [] M
let upeta = command "upeta" ~packages:["upgreek",""] [] M
let upiota = command "upiota" ~packages:["upgreek",""] [] M
let upmu = command "upmu" ~packages:["upgreek",""] [] M
let uppi = command "uppi" ~packages:["upgreek",""] [] M
let uprho = command "uprho" ~packages:["upgreek",""] [] M
let upsigma = command "upsigma" ~packages:["upgreek",""] [] M
let upomega = command "upomega" ~packages:["upgreek",""] [] M

let log_ = command "log" [] M



type pattern = Latex.t*Latex.t

let interp_pattern_autospacing (x,a) =
  concat [ x ; in_ ; a ]
let interp_pattern_tightspacing (x,a) =
  let space = text"\\," in
  concat [ x ; space ; block in_ ; space ; a ]
let interp_pattern_sup (x,a) =
  exponent x (in_^^a)
let interp_pattern_small (x,a) =
  let space = text"\\," in
  x ^^ scriptsize (concat [ block in_ ; space ; a ])

let default_interp_pattern = interp_pattern_autospacing
  

let quantifier c q t =
  mode M (concat [ c ; text"_" ; scriptsize (interp_pattern_autospacing q) ; t ])
let quantifier c = List.fold_right (quantifier c)
let union = quantifier bigcup
let intersection = quantifier bigcap

(* let hquantifier c q t = *)
(*   mode M (concat [ c ; (interp_pattern_sup q) ; text ".\\," ; t ]) *)
(* let hquantifier c = List.fold_right (hquantifier c) *)
let hquantifier c q t =
  let q = concat_with_sep (List.map interp_pattern_sup q) (text",") in
  mode M (concat [ c ; q ; text ".\\,\\," ; t ])
let forall = hquantifier forall
let exists = hquantifier exists

let family q t =
  index
    (between `Paren t)
    (interp_pattern_autospacing q)
let family = List.fold_right family

let comprehension p b = between `Brace (concat [ interp_pattern_tightspacing p ; mid ; b])
let comprehension p b =
  match p with
  | [p] -> comprehension p b
  | _ -> failwith "comprehension cannot be used with several patterns."

let powerset u =
  concat [
    mathcal (text "P");
    block (between `Paren u)
  ]


(**** Bibliography ****)
let cite ?extra t =
  let opt =
    match extra with
    | Some x -> Some (T,x)
    | None -> None
  in
  command "cite" ?opt [T,t] T


(*** Holes ***)

let citation_needed = small (text"[citation]")

(*** Include graphics ***)

let includegraphics ?height ?width ?keepaspectratio t =
  let param k n x = 
    Option.map (fun x -> concat [text n;text"=";k x]) x
  in
  let sparam = param latex_of_size in
  let bparam = param begin function true -> text"true" | false -> text"false" end in
  let width = sparam "width" width in
  let height = sparam "height" height in
  let keepaspectratio = bparam "keepaspectratio" keepaspectratio in
  let opt = A,concat_with_sep (Option.flatten [height;width;keepaspectratio])  (text",") in
  command "includegraphics" ~packages:["graphicx",""] ~opt [A,t] A


(** A short module for proof.sty *)

let latex_of_halignment = function
  | `C -> text "c" 
  | `L -> text "l"
  | `R -> text "r"
  | `S -> text "s"

let eqmakeframebox name ~tag ?halign t =
  let tag = (A,bracket,text tag) in
  let halign = (* arnaud: Opt.map *)
    match halign with
    | Some h -> Some (A,bracket,latex_of_halignment h)
    | None -> None
  in
  let t = (T,brace,t) in
  (* arnaud: opt_cons -> Opt.cons *)
  unusual_command name ~packages:["eqparbox",""] (tag::(Option.cons halign [t])) A
let eqmakebox = eqmakeframebox "eqmakebox"

let same_sized_box =
  let gen = variable 0 in
  fun k ->
    setf gen ((+)1) ^^
    get gen begin fun i ->
      let tag = Format.sprintf "tag%i" i in
      k (eqmakebox ~tag)
    end

let flex_lines l =
  let min_sep = qquad in
  let sep = min_sep^^hspace(`Stretch 1) in
  same_sized_box begin fun equalize ->
    let linize l = array_line [equalize (concat_with_sep l sep)] in
    array [`C] (List.map linize l)
  end

module Infer = struct

  let infer_gen cmd ?label premisses concl =
    let premisses = concat_with_sep premisses (text" & ") in
    let label = match label with None -> None | Some l -> Some (M,l) in
    command cmd ~packages:["proof",""] ?opt:label [ M,concl ; M,premisses ] M

  let rule = infer_gen "infer"
  let derived = infer_gen "infer*"

  let rule_multi ?label p c =
    match p with
    | [p] -> rule ?label p c
    | p -> rule ?label [flex_lines p] c
end
