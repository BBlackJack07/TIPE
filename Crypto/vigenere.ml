let encrypt_char c k =
  let a = int_of_char 'a' in
  let ci = (int_of_char c) - a in
  let ki = (int_of_char k) - a in
  char_of_int (((ci + ki) mod 26) + a)
;;

let decrypt_char e k =
  let a = int_of_char 'a' in 
  let ci = (int_of_char e) - a in
  let ki = (int_of_char k) - a in
  char_of_int (((ci - ki) mod 26) + a)
;;

let encrypt msg key =
  let n = String.length msg and
  l = String.length key in
  let emsg = Bytes.make n ' ' in
  for i = 0 to n-1 do
    Bytes.set emsg i (encrypt_char msg.[i] key.[i mod l])
  done;
  String.of_bytes emsg
;;
