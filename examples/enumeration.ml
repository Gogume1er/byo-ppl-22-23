open Byoppl
open Distribution
open Cps_operators
open Basic.Enumeration
open Owl_plplot
open Owl

let test_bias str model data =
  Format.printf "@.-- %s, CPS Enumeration --@." str;
  let dist = infer model data in
  let m, s = Distribution.stats dist in
  Format.printf "%s bias, mean: %f std:%f@." str m s

let test_distrib str model data =
  Format.printf "@.-- %s, CPS Enumeration --@." str;
  let dist = infer model data in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  let length = Array.length values in
  if length <= 1 then ()
  else
    let h = Plot.create ("plots/enumeration_" ^ str ^ ".png") in
    let x = Mat.of_array (Array.map Float.of_int values) 1 length in
    let y = Mat.of_array probs 1 length in
    Plot.set_yrange h 0. 1.;
    Plot.set_ylabel h "Probabilities";
    Plot.set_xlabel h "Values";
    Plot.set_title h ("Probability distribution for " ^ str);
    Plot.stem ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 5.; LineStyle 1 ] x y;
    Plot.output h

let unique_exec () =
  let* x = sample (dirac ~v:5) in
  let* y = sample (dirac ~v:1) in
  if (x + y) = 6 then
    let* z = sample (dirac ~v:3) in
    return (x + y + z)
  else
    return (x - y)

let _ =
  test_distrib "Unique execution" unique_exec ()

let coin_distrib () =
  sample (bernoulli ~p:0.5)

let coin2_distrib_bern () =
  let* n1 = sample (bernoulli ~p:0.5) in
  let* n2 = sample (bernoulli ~p:0.5) in
  return (n1 + n2)

let coin2_distrib_binom () =
  sample (binomial ~p:0.5 ~n:2)

let coin_bias data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let* () = Cps_list.iter (observe (bernoulli ~p:z)) data in
  return z

let coin_bias_rec data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let rec aux data =
    match data with
    | [] -> return ()
    | h :: t ->
      let* () = observe (bernoulli ~p:z) h in aux t
  in
  let* () = aux data in
  return z

let coin_bias_funny data =
  let* z = sample (uniform_support ~values:(Array.init 101 (fun x -> x))) in
  let z = Float.of_int z in
  let z = z /. 100. in
  let* direction = sample (bernoulli ~p:0.5) in
  let rec aux data =
    match data with
    | [] -> return ()
    | h :: t ->
      let* () = observe (bernoulli ~p:z) h in aux t
  in
  let data = if direction = 1 then data else List.rev data in
  let* () = aux data in
  return z

let _ =
  test_distrib "One coin" coin_distrib ();
  test_distrib "Two coins with bernoulli" coin2_distrib_bern ();
  test_distrib "Two coins with binomial" coin2_distrib_binom ();
  test_bias "Coin" coin_bias [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ];
  test_bias "Coin rec" coin_bias_rec [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ];
  test_bias "Coin funny" coin_bias_funny [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ]

let simple_branching () =
  let* x = sample (dirac ~v:5) in
  let* y = sample (bernoulli ~p:0.8) in
  if (x + y) = 6 then
    let* z = sample (dirac ~v:3) in
    return (x + y + z)
  else
    return (x - y)

let _ =
  test_distrib "Simple branching" simple_branching ()

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  test_distrib "Funny Bernoulli" funny_bernoulli ()

(* https://fr.wikipedia.org/wiki/Loi_hypergéométrique#Exemple_simple *)
let fish_example () =
  sample (hypergeometric ~good:25 ~bad:75 ~sample:10)

let _ =
  test_distrib "Hypergeometric - Fish" fish_example ()

let rec plinko n =
  match n with
  | 0 -> return 0
  | _ ->
    let* x = sample (bernoulli ~p:0.5) in
    let* s = plinko (n - 1) in
    return ((1 - 2 * x) + s)

let _ =
  test_distrib "Plinko 10" plinko 10

let sum_dice_even () =
  let* d1 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* d2 = sample (uniform_support ~values:(Array.init 6 (fun i -> i))) in
  let* () = assume (d1 mod 2 = 0 || d2 mod 2 = 0) in
  return (d1 + d2)

let _ =
  test_distrib "Sum dice even" sum_dice_even ()
