let even_digits n =
  let num_str = string_of_int n in
  let num_length = String.length num_str in
  num_length mod 2 = 0

let split_number n =
  let string = string_of_int n in
  let middle = (String.length string) / 2 in
  let first_half = int_of_string (String.sub string 0 middle) in
  let second_half = int_of_string (String.sub string middle middle) in
  [first_half; second_half]

let rec apply_blinks (cache : (int * int, int) Hashtbl.t) (num_blinks : int) (stone : int) =
  if num_blinks = 0 then
    1
  else
    match Hashtbl.find_opt cache (stone, num_blinks) with
    | Some value -> value
    | None ->
       let (result : int) =
         if stone = 0 then
           apply_blinks cache (num_blinks - 1) 1
         else if even_digits stone then
           split_number stone
           |> List.map (apply_blinks cache (num_blinks - 1))
           |> List.fold_left (+) 0
         else
           apply_blinks cache (num_blinks - 1) (stone * 2024);
       in
          Hashtbl.add cache (stone, num_blinks) result;
          result

let read_int_list filename =
  let in_channel = open_in filename in
  try
    let first_line = input_line in_channel in
    close_in in_channel;
    first_line
    |> String.split_on_char ' '
    |> List.map int_of_string
  with e ->
    close_in in_channel;
    raise e

let () =
  let stones = read_int_list "2024/input/day-11.txt" in
  let table : ((int * int), int) Hashtbl.t = Hashtbl.create 10 in
  stones
  |> List.map (apply_blinks table 25)
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n"
