let swap t i j = let tmp = t.(i) in t.(i) <- t.(j); t.(j) <- tmp

let clear () = Sys.command "clear";;

let rotd r t =
  let n = Array.length t in
  let copy = Array.copy t in
  for i = 0 to n-1 do
    let j = (i+r) mod (n) in
    t.(j) <- copy.(i)
  done

let shift_rows t =
  let n = Array.length t in
  for i = 1 to n-1 do 
    rotd i t.(i)
  done

