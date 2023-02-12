let mean_time f k =
  let s = ref 0. in
  let t = Array.make k 0. in
  for i = 0 to k-1 do
    let t0 = Sys.time () in
    f ();
    t.(i) <- Sys.time() -. t0;
    s := !s +. t.(i)
  done;
  t, !s /. (float_of_int k)

let std t m = 
  let s = ref 0. in
  let n = Array.length t in
  for i = 0 to n - 1 do
    let e = m -. t.(i) in
    s := e *. e
  done;
  Float.sqrt (!s /. (float_of_int n))


let write_csv t path =
  let f = open_out path in
  let n = Array.length t in
  for i = 0 to n - 1 do
    let size,time,ut = t.(i) in
    let s = Int.to_string size ^ "," ^ Float.to_string time ^  "," ^ Float.to_string ut ^ "\n" in 
    Out_channel.output_string f s
  done;
  Out_channel.close f

let run_test_naif a b = 
  let n = b - a + 1 and k = 20 in
  let t_naif = Array.make n (0,0.,0.) in
  for i = a to b do
    let t,m = mean_time (fun () -> Prime_naif.generate_prime_naif i) k in
    let s = std t m in
    t_naif.(i-a) <- (i, m, s)
  done;
  write_csv t_naif "./Test_results/naif.csv"

let run_test_miller a b =
  let n = b - a and k = 100 in
  let step = 4 in
  let t_miller = Array.make ((n/step)+1) (0,0.,0.) in
  let i = ref a in
  while !i <= b do
    let t,m = mean_time (fun () -> Prime.generate_prime !i) k in
    let s = std t m in
    t_miller.((!i-a)/step) <- (!i,m,s);
    i := !i + step
  done;
  write_csv t_miller "./Test_results/miller.csv"



let _ =
  run_test_naif 32 50
