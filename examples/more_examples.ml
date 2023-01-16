open Byoppl
open Distribution
open Cps_operators
open Owl_plplot
open Owl

open Infer.Particle_filter

let test_bias str model data =
  Format.printf "@.-- %s, CPS Particle filter --@." str;
  let dist = infer model data in
  let m, s = Distribution.stats dist in
  Format.printf "%s bias, mean: %f std:%f@." str m s;
  let h = Plot.create ("plots/more_examples_" ^ str ^ ".png") in
  let y = Distribution.get_samples dist in
  let y = Mat.of_array y (Array.length y) 1 in
  Plot.set_ylabel h "Probabilities";
  Plot.set_xlabel h "Values";
  Plot.set_title h ("Probability distribution for " ^ str);
  Plot.histogram ~h ~bin:50 ~spec:[ RGB (0,0,255) ] y;
  Plot.output h

let rec weird () =
  let* b = sample (bernoulli ~p:0.5) in
  let* mu = if (b = 1) then return 0.5 else return 1.0 in
  let* theta = sample (gaussian ~mu ~sigma:1.0) in
  if theta > 0. then
    let* () = observe (gaussian ~mu ~sigma:0.5) theta in
    return theta
  else
    weird ()

let _ =
  test_bias "Weird" weird ()

let uni_disk () =
  let* x = sample (uniform ~a:0. ~b:1.) in
  let* y = sample (uniform ~a:0. ~b:1.) in
  let d = (x *. x) +. (y *. y) in
  if d < 1. then return 4. else return 0.

let _ =
  test_bias "Disk" uni_disk ()
