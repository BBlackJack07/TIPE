let mean_time f k =
  let s = ref 0. in
  for i = 0 to k-1 do
    let t0 = Sys.time () in
    f ();
    s := !s +. Sys.time() -. t0
  done;
  !s /. (float_of_int k)

let write_csv t path =
  let f = open_out path in
  let n = Array.length t in
  for i = 0 to n - 1 do
    let size,time = t.(i) in
    let s = Int.to_string size ^ "," ^ Float.to_string time ^ "\n" in
    Out_channel.output_string f s
  done;
  Out_channel.close f

let run_test a b = 
  let n = b - a + 1 and k = 20 in
  let t_naif = Array.make n (0,0.) in
  let t_miller = Array.make n (0,0.) in
  for i = a to b do
    let t1 = mean_time (fun () -> Prime.generate_prime i) k and
    t2 = mean_time (fun () -> Prime_naif.generate_prime_naif i) k in
    t_miller.(i-a) <- (i,t1);
    t_naif.(i-a) <- (i, t2)
  done;
  write_csv t_naif "./naif.csv";
  write_csv t_miller "./miller.csv"

let run_test_miller a b =
  let n = b - a and k = 100 in
  let step = 4 in
  let t_miller = Array.make ((n/step)+1) (0,0.) in
  let i = ref a in
  while !i <= b do
    let t1 = mean_time (fun () -> Prime.generate_prime !i) k in
    t_miller.((!i-a)/step) <- (!i,t1);
    i := !i + step
  done;
  write_csv t_miller "./miller_alone.csv"



let _ =
  run_test_miller 1024 2048
