
let read_file filename =
  let chan = open_in filename in
  let rec go acc =
    match input_line chan with
    | exception End_of_file -> acc
    | line -> go (acc ^ "\n" ^ line)
  in go ""

let _ =
  let ( let* ) = Option.bind in
  let* input =
    if Array.length Sys.argv > 1
    then
      let filename = Sys.argv.(1) in
      match read_file filename with
      | input -> Some input
      | exception _ ->
         let _ = print_endline ("error: could not read \'" ^ filename ^ "\'") in
         None
    else
      let _ = print_endline "error: no file given" in
      None
  in
  let _ = Demo.(compile (Array.length Sys.argv > 2) (parse input)) in
  None
