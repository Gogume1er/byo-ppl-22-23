type 'a support = {
  values : 'a array;
  logits : float array;
  probs : float array;
}

type 'a t = {
  sample : unit -> 'a;
  logpdf : 'a -> float;
  mean : (unit -> float) option;
  var : (unit -> float) option;
  samples : 'a array Lazy.t;
  support : 'a support option;
}

let make ?(n = 10000) ~sample ~logpdf ?mean ?var ?support () =
  let samples = lazy (Array.init n (fun _ -> sample ())) in
  { sample; logpdf; mean; var; samples; support }

let draw dist = dist.sample ()
let get_samples dist = Lazy.force dist.samples

let get_support ?(shrink = false) dist =
  match shrink with
  | false -> Option.get dist.support
  | true ->
    let { values; probs; _ } = Option.get dist.support in
    let values, probs = Utils.shrink ~values ~probs in
    { values; logits = Array.map log probs; probs }

let logpdf dist x = dist.logpdf x

let mean_generic ~transform dist : float =
  match (dist.mean, dist.support) with
  | Some mean, _ -> mean ()
  | _, Some { values; logits; _ } ->
    let values = values |> Array.map transform |> Utils.to_owl_arr in
    let logits = Utils.to_owl_arr logits in
    Utils.average ~values ~logits
  | _ -> Owl_stats.mean (dist |> get_samples |> Array.map transform)

let var_generic ~transform dist =
  match (dist.var, dist.support) with
  | Some var, _ -> var ()
  | _, Some { values; logits; _ } ->
    let m = mean_generic ~transform dist in
    let values =
      values |> Array.map transform |> Utils.to_owl_arr
      |> Owl.Arr.(fun a -> (a -$ m) **$ 2.)
    in
    let logits = Utils.to_owl_arr logits in
    Utils.average ~values ~logits
  | _ -> Owl_stats.var (dist |> get_samples |> Array.map transform)

let mean = mean_generic ~transform:Fun.id
let mean_int = mean_generic ~transform:Float.of_int
let var = var_generic ~transform:Fun.id
let var_int = var_generic ~transform:Float.of_int
let std dist = sqrt (var dist)
let std_int dist = sqrt (var_int dist)
let stats dist = (mean dist, std dist)
let stats_int dist = (mean_int dist, std_int dist)

let bernoulli ~p =
  assert (0. <= p && p <= 1.);
  let sample () = Owl_stats.binomial_rvs ~p ~n:1 in
  let logpdf x = Owl_stats.binomial_logpdf x ~p ~n:1 in
  let mean () = p in
  let var () = p *. (1. -. p) in
  let support =
    {
      values = [| 0; 1 |];
      logits = [| log (1. -. p); log p |];
      probs = [| 1. -. p; p |];
    }
  in
  make ~sample ~logpdf ~support ~mean ~var ()

let binomial ~p ~n =
  assert (n >= 0 && 0. <= p && p <= 1.);
  let sample () = Owl_stats.binomial_rvs ~p ~n in
  let logpdf x = Owl_stats.binomial_logpdf x ~p ~n in
  let prob x = Owl_stats.binomial_pdf x ~p ~n in
  let mean () = Float.of_int n *. p in
  let var () = Float.of_int n *. p *. (1. -. p) in
  let support =
    {
      values = Array.init (n + 1) (fun i -> i);
      logits = Array.init (n + 1) logpdf;
      probs = Array.init (n + 1) prob
    }
  in
  make ~sample ~logpdf ~support ~mean ~var ()

let dirac ~v =
  let sample () : 'a = v in
  let logpdf x = if x = v then 0. else -.infinity in
  let support =
    {
      values = [| v |];
      logits = [| 0. |];
      probs = [| 1. |];
    }
  in
  make ~sample ~logpdf ~support ()

let hypergeometric ~good ~bad ~sample =
  assert (sample >= 0 && good >= 0 && bad >= 0 && sample <= good + bad);
  let sum = good + bad in
  let p = Float.of_int good /. Float.of_int sum in
  let sample' = sample in
  let sample () = Owl_stats.hypergeometric_rvs ~good ~bad ~sample in
  let logpdf x = Owl_stats.hypergeometric_logpdf x ~good ~bad ~sample:sample' in
  let prob x = Owl_stats.hypergeometric_pdf x ~good ~bad ~sample:sample' in
  let mean () = Float.of_int sample' *. p in
  let var () = Float.of_int sample' *. p *. (1. -. p) *. (Float.of_int sum -. Float.of_int sample') /. (Float.of_int sum -. 1.) in
  let support =
    {
      values = Array.init (sample' + 1) (fun i -> i);
      logits = Array.init (sample' + 1) logpdf;
      probs = Array.init (sample' + 1) prob;
    }
  in
  make ~sample ~logpdf ~support ~mean ~var ()

(* Shamefully stolen from {{: https://github.com/IBM/probzelus}ProbZelus} *)
let pi = 4. *. atan 1.
let two_pi = 2.0 *. pi
let sqrt_two_pi = sqrt two_pi

let gamma =
  let g = 7. in
  let c =
    [|0.99999999999980993; 676.5203681218851; -1259.1392167224028;
      771.32342877765313; -176.61502916214059; 12.507343278686905;
      -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7|]
  in
  let rec ag z d =
    if d = 0 then c.(0) +. ag z 1
    else if d < 8 then c.(d) /. (z +. float d) +. ag z (succ d)
    else c.(d) /. (z +. float d)
  in
  fun z ->
    let z = z -. 1. in
    let p = z +. g +. 0.5 in
    sqrt_two_pi *. p ** (z +. 0.5) *. exp (-. p) *. ag z 0

let poisson ~lambda =
  assert (lambda >= 0.);
  let sample () =
    let rec aux t k =
      let t = t +. Owl_stats.exponential_rvs ~lambda in
      if t > 1. then k
      else aux t (k + 1)
    in
    aux 0. 0
  in
  let logpdf x =
    if x < 0 then -. infinity
    else
      Float.of_int x *. log lambda -. lambda -. log (gamma (Float.of_int (x + 1)))
  in
  let mean () = lambda in
  let var () = lambda in
  make ~sample ~logpdf ~mean ~var ()

let support ~values ~logits =
  assert (Array.length values = Array.length logits);
  let probs = Utils.normalize logits in
  let support = { values; logits; probs } in
  let sample () =
    let i = Owl_stats.categorical_rvs probs in
    values.(i)
  in
  let logpdf x =
    let idx = Option.get (Utils.findi values x) in
    logits.(idx)
  in
  make ~sample ~logpdf ~support ()

let uniform_support ~values =
  support ~values ~logits:(Array.make (Array.length values) 0.)

let split dist =
  let { values; logits; _ } = get_support dist in
  let v1, v2 = values |> Array.to_list |> List.split in
  (* let v1, v2 = Array.split values in *)
  ( support ~values:(Array.of_list v1) ~logits,
    support ~values:(Array.of_list v2) ~logits )

let split_list dist =
  let { values; logits; _ } = get_support dist in
  assert (Array.length values > 0);
  assert (Array.for_all (fun v -> List.length v = List.length values.(0)) values);
  let rec split res sup =
    if Array.for_all (( = ) []) sup then res
    else
      let res = split res (Array.map (fun x -> List.tl x) sup) in
      let values = Array.map (fun x -> List.hd x) sup in
      support ~values ~logits :: res
  in
  split [] values

let split_array dist =
  let { values; logits; _ } = get_support dist in
  let values = Array.map Array.to_list values in
  let d = split_list (support ~values ~logits) in
  Array.of_list d

let beta ~a ~b =
  assert (a > 0. && b > 0.);
  let sample () = Owl_stats.beta_rvs ~a ~b in
  let logpdf x = Owl_stats.beta_logpdf x ~a ~b in
  let mean () = a /. (a +. b) in
  let var () = a *. b /. (((a +. b) ** 2.) *. (a +. b +. 1.)) in
  make ~sample ~logpdf ~mean ~var ()

let gaussian ~mu ~sigma =
  assert (sigma > 0.);
  let sample () = Owl_stats.gaussian_rvs ~mu ~sigma in
  let logpdf x = Owl_stats.gaussian_logpdf x ~mu ~sigma in
  let mean () = mu in
  let var () = sigma ** 2. in
  make ~sample ~logpdf ~mean ~var ()

let uniform ~a ~b =
  let sample () = Owl_stats.uniform_rvs ~a ~b in
  let logpdf x = Owl_stats.uniform_logpdf x ~a ~b in
  let mean () = a +. (b /. 2.) in
  let var () = 1. /. 12. *. ((b -. a) ** 2.) in
  make ~sample ~logpdf ~mean ~var ()

let exponential ~lambda =
  assert (lambda >= 0.);
  let sample () = Owl_stats.exponential_rvs ~lambda in
  let logpdf x = Owl_stats.exponential_logpdf x ~lambda in
  let mean () = 1. /. lambda in
  let var () = 1. /. (lambda *. lambda) in
  make ~sample ~logpdf ~mean ~var ()

let gamma ~shape ~scale =
  assert (shape > 0. && scale > 0.);
  let sample () = Owl_stats.gamma_rvs ~shape ~scale in
  let logpdf x = Owl_stats.gamma_logpdf x ~shape ~scale in
  let mean () = shape *. scale in
  let var () = shape *. scale *. scale in
  make ~sample ~logpdf ~mean ~var ()

let chi2 ~df =
  assert (df > 0.);
  let sample () = Owl_stats.chi2_rvs ~df in
  let logpdf x = Owl_stats.chi2_logpdf x ~df in
  let mean () = df in
  let var () = 2. *. df in
  make ~sample ~logpdf ~mean ~var ()
