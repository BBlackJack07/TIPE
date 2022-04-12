(* Large prime numbers generation *)
#require "zarith"

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

let random_number n = (* random number generation, integer n is the number of bits *)
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

let rec fast_pow n p = 
    let two = Z.(succ one) in
    match p with
    | _ when p = Z.zero -> Z.one
    | _ when zmod p two = Z.zero -> fast_pow Z.(n*n) Z.(p/two)
    | _ -> Z.(n * (fast_pow Z.(n*n) Z.(p/two)))


let millerWitness n s d a = (* find if a is Miller witness of n not being prime, with s,d such that n-1 = 2^s*d, d odd *)
    let i = ref Z.zero in
    let m = ref Z.one in
    while Z.(!i<=d) do
        m := Z.(zmod (a * !m) n);
        i := Z.succ !i
    done;
    if !m = Z.one then false
    else begin
        let r = ref Z.zero in
        while !m <> Z.(n-one) && Z.(!r < s) do
            r:=Z.succ !r;
            m:= zmod Z.(!m * !m) n
        done;
        !m <> Z.(n-one)
    end

