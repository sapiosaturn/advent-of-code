(* This function will go from filename to list of lines. *)
let read_lines filename =
  let fc_in = open_in filename in
  let try_read () =
    try Some (input_line fc_in) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> close_in fc_in; List.rev acc
  in
  loop []

(* This function will go from line to pair of integers. *)
let split_line line =
  String.split_on_char ' ' line
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

(* This function will go from lines to pairs of integers. *)
let rec split_lines lines =
  match lines with
  | [] -> []
  | x :: v -> split_line x :: split_lines v

(* This function will transform the list of pairs into two column-lists. *)
let rec split_columns lists col1 col2 =
  match lists with
  | [] -> (List.rev col1, List.rev col2)
  | [x; y] :: v -> split_columns v (x :: col1) (y :: col2)
  | _ -> failwith "Invalid format."

let split_columns_init lists =
  split_columns lists [] []

(* Sort the columns. *)
let sort_columns col_tuple =
  match col_tuple with
  | (col1, col2) -> (List.sort compare col1, List.sort compare col2)

(* Single term *)
let compute_single_term a b =
  abs (a - b)

(* Computing the sum of differences. *)
let compute_answer col_tuple =
  match col_tuple with
  | (col1, col2) -> (List.map2 compute_single_term col1 col2)
    |> List.fold_left ( + ) 0

(* For debugging, printing all lines. *)
let rec print_all_lines u =
  match u with
  | [] -> ()
  | x :: v -> print_endline x; print_all_lines v

(* Main *)
let () =
  read_lines "input.txt"
  |> split_lines
  |> split_columns_init
  |> sort_columns
  |> compute_answer
  |> print_int
