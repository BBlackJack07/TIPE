(* Large prime numbers generation *)
(*#require "zarith"*)

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
