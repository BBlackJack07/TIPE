(* Large prime numbers generation *)
(* #require "zarith" *) (*when using in toplevel*)
(* compile using 'ocamlfind ocamlc prime.ml -package zarith -linkpkg' *)

let show n = Z.print n; print_newline() (* for debugging purpose *)

let zmod a b = let _,r = Z.div_rem a b in r

let sieve n = (* Generating prime numbers lower than int n using sieve of Eratosthenes *)
  let tab = Array.make (n-1) true in (* array representing numbers from 2 to n *)
  for i = 0 to n-2 do
    if tab.(i) then
      for j = i+3 to n do
        tab.(j-2) <- tab.(j-2) && j mod (i+2) <> 0
      done
  done;
  let l = ref [] in
  for i = n-2 downto 0 do
    if tab.(i) then
      l := (Z.of_int (i+2)) :: !l
  done;
  !l

let max_bit n =
    let p = ref Z.one in
    let m = ref 0 in
    while Z.(!p<n) do
        p := Z.(!p * of_int 2);
        incr m
    done;
    !m

let random_number n = (* random number below n *)
    let m = max_bit n in
    let x = ref Z.one in
    while Z.(!x >= n) || !x=Z.one do
        let pow2 = ref Z.one in
        x := Z.zero;
        for i = 0 to m-1 do
            if Random.bool () then
                x := Z.add !x !pow2;
            pow2 := Z.(!pow2 * of_int 2)
        done
    done;
    !x

let rd_potential_prime n = (* random number generation, integer n is the number of bits *)
  let two = Z.succ Z.one in
  let pow2 = Z.pow two in
  let x = ref (Z.add (pow2 (n-1)) Z.one) in (* The number is at least 2^(n-1)+1, in order to be large enough and odd *)
  for i = 1 to n-2 do
    if Random.bool () then (* if random bool is true, i-th bit is one *)
      x := Z.add !x (pow2 i)
  done;
  !x

let rec maybe_prime n l = (* n: Z.t, l: Z.t list, a list of prime numbers *)
  match l with
  | [] -> true
  | p::t -> (zmod n p <> Z.zero || n = p) && maybe_prime n t

let rec decompose2 n = (* n: Z.t, find s: Z.t and d: Z.t such that n=2^s*d with d and odd number *)
    let two = Z.(succ one) in
    match n with
    | _ when zmod n two <> Z.zero -> Z.zero,n
    | _ -> let s,d = decompose2 (Z.div n two) in Z.succ s,d


let rec modpow n e m = (* use fast exponentiation to compute n^(e) mod m*)
  let two = Z.(succ one) in
  match e with
  | _ when e = Z.zero -> Z.one
  | _ when Z.rem e two = Z.zero -> modpow Z.(rem (n*n) m) Z.(e/two) m
  | _ -> Z.(rem (n * (modpow Z.(rem (n*n) m) Z.(e/two) m)) m)


let miller_witness n s d a = (* find if a is Miller witness of n not being prime, with s,d such that n-1 = 2^s*d, d odd *)
  let m = ref (modpow a d n) in
  if !m = Z.one || !m = Z.(n-one) then false
  else begin
    let r = ref Z.one in
    while !m <> Z.(n-one) && Z.(!r < s) do
      r:=Z.succ !r;
      m:= zmod Z.(!m * !m) n
    done;
    !m <> Z.(n-one)
  end

let rabin_miller n k = (* perform Rabin Miller test of integer n: Z.t with k: int iterations *)
  let s,d = decompose2 Z.(n-one) in
  let i = ref 0 in
  let not_prime = ref false in
  while (not !not_prime) && !i<k do
    let a = ref (random_number Z.(n-one)) in
    not_prime := miller_witness n s d !a;
    incr i
  done;
  !not_prime

let generate_prime nb_bits = (* Compute a random number of nb_bits bits which is almost certainly prime *)
  Random.self_init ();
  let l = sieve 10000 in
  let p = ref (rd_potential_prime nb_bits) in
  while (not (maybe_prime !p l)) || (rabin_miller !p 25) do
    p := rd_potential_prime nb_bits
  done;
  !p

