let show n = Printf.printf "%s\n" Z.(to_string n)

let sqrt n =
  let two = Z.(succ one) in
  let rec aux a b =
    if Z.(abs (b-a) <= one) then a
    else begin
      let m = Z.((a+b)/two) in
      let p = Z.(m * m) in
      if p = n then m
      else if p < n then aux m b
      else aux a m
    end
  in aux Z.zero n

let is_prime n =
  let two = Z.(succ one) in
  let sqrt_n = sqrt n in
  let rec test = function
    | i when i > sqrt_n -> false
    | i -> Z.(n mod i = zero) || test Z.(i+two)
  in not ((Z.(n mod two = zero) && n <> two) || test (Z.succ two))

let generate_prime_naif nb_bits =
  Random.self_init ();
  let p = ref (Prime.rd_potential_prime nb_bits) in
  while not (is_prime !p) do
    p := Prime.rd_potential_prime nb_bits
  done;
  !p
