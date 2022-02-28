module Puzzle =
struct 
  let make_board () = 
    [|0; 1; 2; 3; 
      4; 5; 6; 7; 
      8; 9;10;11;
     12;13;14;15|]

  let print b = 
    let out = Array.make 16 "   " in 
    for i = 1 to 15 do 
      out.(b.(i)) <- Printf.sprintf " %2d" i
    done;
    for i = 0 to 15 do
      if i mod 4 = 0 then print_endline "";
      print_string out.(i);
    done;
    print_endline ""

  let move board n = 
    let hole, spot = board.(0), board.(n) in 
    board.(0) <- spot;
    board.(n) <- hole

  let shuffle board = 
    Random.init (Unix.time () |> int_of_float);
    for _ = 1 to 15 do
      move board (1 + Random.int 15)
    done

  let welcome_message () = 
    print_endline "Welcome to 15. Try get all the pieces in order from 0 (empty space) to 15.";
    print_endline "Enter the number of a space you'd like to swap with the empty space."

    
  let is_adjacent board n = 
    let z = board.(0) in
    let x = board.(n) in 
    x = z + 1 || x = z - 1 || x = z + 4 || x = z - 4

  
  let is_valid board input = 
    Str.string_match (Str.regexp "[0-9]+") input 0 && 
    let n = int_of_string input in 
    n >= 1 && n <= 15 &&
    is_adjacent board n

  let rec game_loop board =
    print board;
    print_string "> ";
    let input = read_line () in 
    if is_valid board input then begin
      int_of_string input |> move board;
      game_loop board end 
    else begin 
      Printf.printf "Invalid input: %s. You must enter a number between 1 and 15, and the number must be directly left, right, above, or below the empty space.\n" input;
      game_loop board end

  let play () = 
    welcome_message ();
    let board = make_board () in
    shuffle board;
    game_loop board

end

let () = Puzzle.play ()