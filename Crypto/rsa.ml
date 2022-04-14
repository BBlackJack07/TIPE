type private_key = {d: Z.t; n: Z.t}
type public_key = {e: Z.t; n: Z.t}

let rec pgcd a b = match Z.(a = zero || b = zero) with
  | true -> if a = Z.zero then b else a
  | _    -> pgcd b (Z.rem a b)

let euclide a b = (* Algorithme d'Euclide étendu, pour calculer les coefficients de Bézout d'un couple d'entier *) 
  let u0,u1,v0,v1 = Z.(ref one, ref zero, ref zero, ref one) in
  let r0 = ref a and r1 = ref b in
  while !r1 <> Z.zero do
    let t1,t2,t3 = !u1,!v1,!r1 in
    let q = Z.(!r0 / !r1) in
    u1 := Z.(!u0 - q * !u1);
    v1 := Z.(!v0 - q * !v1);
    r1 := Z.(!r0 - q * !r1);
    u0 := t1;
    v0 := t2;
    r0 := t3
  done;
  (!u0,!v0)

let rec modpow n e m = (* use fast exponentiation to compute n^(e) mod m*)
  let two = Z.(succ one) in
  match e with
  | _ when e = Z.zero -> Z.one
  | _ when Z.rem e two = Z.zero -> modpow Z.(rem (n*n) m) Z.(e/two) m
  | _ -> Z.(rem (n * (modpow Z.(rem (n*n) m) Z.(e/two) m)) m)

let build_keys p q = (* p : Z.t, q : Z.t, p and q are supposed to be prime integers *)
(* returned values are public and private rsa keys associated to p and q *)
  let phi = Z.((p-one)*(q-one)) in
  let n = Z.(p*q) in
  let e = ref Z.(phi-one) in
  while pgcd phi !e <> Z.one do
    e := Z.(!e-one)
  done;
  let d = ref (fst (euclide !e phi)) in
  while !d<Z.zero do
    d:= Z.(!d + phi)
  done;
  while !d>phi do
    d:= Z.(!d - phi)
  done;
  ({e= !e; n=n},{d= !d; n=n})

let encrypt m pubkey = (* encrypt Z.t int m using public key 'pubkey' *)
  modpow m pubkey.e pubkey.n

let decrypt c privkey = (* decrypt (or sign) Z.t int c using private key 'privkey' *)
  modpow c privkey.d privkey.n
