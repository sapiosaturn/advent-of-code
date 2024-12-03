(* This function will go from filename to list of lines. *)
(* Reused from day01. *)
let read_lines filename =
  let fc_in = open_in filename in
  let try_read () =
    try Some (input_line fc_in) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> close_in fc_in; List.rev acc
  in
  loop []

(* From a string to list of integers. *)
(* Reused from day01. *)
let split_line line =
  String.split_on_char ' ' line
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

(* This function will go from lines to lists of integers. *)
(* Reused from day01. *)
let rec split_lines lines =
  match lines with
  | [] -> []
  | x :: v -> split_line x :: split_lines v

(* Returns 1 if safe and 0 if unsafe. *)
let evaluate_asc line =
  let rec aux line =
    match line with
      | [] -> 1
      | x1 :: x2 :: v -> let diff = x2-x1 in (
        match diff with
          | x when (abs diff) > 3 -> 0
          | x when diff>0 -> aux (x2 :: v)
          | x -> 0
      )
      | x1 :: v -> 1
  in
  aux line

(* Returns 1 if safe and 0 if unsafe. *)
let evaluate_desc line =
  let rec aux line =
    match line with
      | [] -> 1
      | x1 :: x2 :: v -> let diff = x2-x1 in (
        match diff with
          | x when (abs diff) > 3 -> 0
          | x when diff<0 -> aux (x2 :: v)
          | x -> 0
      )
      | x1 :: v -> 1
  in
  aux line

(* Returns 1 if safe and 0 if unsafe. *)
let evaluate_line line =
  match line with
  | x1 :: x2 :: v -> let diff = x2-x1 in (
    match diff with
    | x when (abs diff) > 3 -> 0
    | x when diff>0 -> evaluate_asc line
    | x when diff<0 -> evaluate_desc line
    | x -> 0
  )
  | _ -> 0

(* Naive solution, check with each dropped if fail. *)
let rec drop line idx cur =
  match line with
  | u :: v -> if cur=idx then v else u :: drop v idx (cur+1)
  | [] -> []

let problem_dampener line =
  if evaluate_line line = 1 then 1 else
  let range = List.init (List.length line) (fun x -> x) in
  let droptest idx =
    drop line idx 0 |> evaluate_line
  in
  let successes = List.map droptest range |> List.fold_left ( + ) 0 in
  if successes>0 then 1 else 0

(* Returns number of safe records. *)
let evaluate_lines lines =
  List.map problem_dampener lines
  |> List.fold_left ( + ) 0

(* Main *)
let () =
  read_lines "input.txt"
  |> split_lines
  |> evaluate_lines
  |> print_int
