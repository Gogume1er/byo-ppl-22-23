module Enumeration = struct
  (* Used to compute the probability of the current path *)
  type prob = { score: float }

  exception Reject
  exception UndefinedSupport

  (* In the below functions, the continuation k returns a list containing values and their associated scores *)
  (* The probability is computed along the path *)

  (* Execute a single branch of the execution *)
  let run_branch v logpdf k prob =
    let prob = { score= prob.score +. logpdf } in
    k v prob

  let sample d k prob =
    let support =
      try
        Distribution.get_support ~shrink:true d
      with
      | Invalid_argument _ -> raise UndefinedSupport
    in
    let _, scores =
      (* Here, we compute the result value and its probability for each possible issue in the distribution *)
      (* At this stage, we could do some optimization by merging issues which have the same result value *)
      (* but it would require to compute the exponential of the probabilities, which could probably lead to a loss of precision *)
      (* Moreover, the library already handles duplicated values in the support pretty well, so we can leave things as they are *)
      Array.fold_left
        (fun (i, l1) v ->
           try
             let logpdf = support.logits.(i) in
             let l2 = run_branch v logpdf k prob in
             (i + 1, List.rev_append l1 l2)
           with Reject -> (i + 1, l1)
        )
        (0, [])
        support.values
    in
    scores

  (* We continue the execution only if the predicate is valid *)
  let assume p k prob = if not p then raise Reject else k () prob

  (* The implementation is a simplification of applying sample then assume *)
  let observe d v k prob =
    let score = prob.score +. (Distribution.logpdf d v) in
    k () { score }

  let infer model data =
    let init_prob = { score= 0.0 } in
    (* Default continuation, which for a value result the value and the computed probability *)
    let k = (fun v prob -> [v, prob.score]) in
    let scores = model data k init_prob in
    let scores = Array.of_list scores in
    let values, logits = Array.split scores in
    Distribution.support ~values ~logits

end

module Rejection_sampling = struct

  type prob = Prob

  exception Reject
  let sample _prob d = Distribution.draw d
  let factor _prob _ = assert false

  let assume _prob p = if not p then raise Reject

  let observe prob d v =
    let y = sample prob d in
    assume prob (y = v)

  let infer ?(n=1000) model data =
    let rec exec i = try model Prob data with Reject -> exec i in
    let values = Array.init n exec in
    Distribution.uniform_support ~values
end

module Importance_sampling = struct
  type prob = {id: int; scores: float Array.t}

  let sample _prob d = Distribution.draw d
  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s
  let assume prob p = factor prob (if p then 0. else -. infinity)
  let observe prob d v = factor prob (Distribution.logpdf d v)
  let infer ?(n=1000) model data = 
    let scores = Array.make n 0. in
    let values = Array.mapi (fun i _ -> model { id=i; scores } data) scores in
    Distribution.support ~values ~logits:scores
end
