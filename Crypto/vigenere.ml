let encrypt_char c k =
  let a = int_of_char 'a' and 
  ci = (int_of_char c) - a and
  ki = (int_of_char k) - a in
  char_of_int (((ci + ki) mod 26) + a)
;;

let decrypt_char e k =
  let a = int_of_char 'a' and 
  ci = (int_of_char c) - a and
  ki = (int_of_char k) - a in
  char_of_int (((ci - ki) mod 26) + a)
;;

let encrypt msg key =
  let n = String.length msg and
  l = String.length key in
  let emsg = String.make n ' ' in
  for i = O to n-1 do
    emsg.[i] <- encrypt_char msg.[i] key.[i mod l]
  done
;;
