(** Pic Breeder - Interactive evolutionary art with complex expressions *)

open Pic_breeder
open Complex_expr
open Mutation
open Color
open Render

(** Configuration *)
let grid_cols = 4
let grid_rows = 3
let num_variants = grid_cols * grid_rows
let cell_width = 200
let cell_height = 200
let output_dir = "output"

(** History entry for undo *)
type history_entry = {
  h_expr : expr;
  h_variants : expr array;
  h_generation : int;
}

(** Current state *)
type state = {
  mutable current_expr : expr;
  mutable variants : expr array;
  mutable generation : int;
  mutable color_mode : color_mode;
  mutable history : history_entry list;  (* Stack of previous states *)
}

let create_output_dir () =
  if not (Sys.file_exists output_dir) then
    Unix.mkdir output_dir 0o755

(** Save the current grid *)
let save_grid state =
  let filename = Printf.sprintf "%s/gen_%04d_grid.ppm" output_dir state.generation in
  let grid = render_grid_labeled ~color_mode:state.color_mode
               ~cell_width ~cell_height grid_cols grid_rows state.variants in
  save_ppm filename grid;
  Printf.printf "Saved grid to %s\n%!" filename

(** Save a high-quality version of a specific expression *)
let save_hq state idx =
  if idx >= 0 && idx < Array.length state.variants then begin
    let expr = state.variants.(idx) in
    let filename = Printf.sprintf "%s/gen_%04d_pic_%02d.ppm" output_dir state.generation (idx + 1) in
    let img = render_hq ~color_mode:state.color_mode expr in
    save_ppm filename img;
    Printf.printf "Saved high-quality image to %s\n%!" filename;
    Printf.printf "Expression: %s\n%!" (to_string expr)
  end else
    Printf.printf "Invalid selection: %d (must be 1-%d)\n%!" (idx + 1) num_variants

(** Initialize with random expressions *)
let init_random () =
  let variants = generate_initial_population num_variants in
  {
    current_expr = variants.(0);
    variants;
    generation = 0;
    color_mode = ColorWheel;
    history = [];
  }

(** Save current state to history before making changes *)
let push_history state =
  let entry = {
    h_expr = state.current_expr;
    h_variants = Array.copy state.variants;
    h_generation = state.generation;
  } in
  state.history <- entry :: state.history

(** Undo to previous state *)
let undo state =
  match state.history with
  | [] ->
    Printf.printf "Nothing to undo.\n%!"
  | entry :: rest ->
    state.current_expr <- entry.h_expr;
    state.variants <- entry.h_variants;
    state.generation <- entry.h_generation;
    state.history <- rest;
    Printf.printf "\n=== Undo to Generation %d ===\n%!" state.generation;
    Printf.printf "Expression: %s\n%!" (to_string state.current_expr);
    Printf.printf "History depth: %d\n%!" (List.length state.history);
    save_grid state

(** Evolve from a selected picture *)
let evolve state selection =
  if selection >= 0 && selection < Array.length state.variants then begin
    push_history state;  (* Save state before evolving *)
    state.current_expr <- state.variants.(selection);
    state.variants <- generate_variants state.current_expr num_variants;
    (* Keep the parent as the first variant for reference *)
    state.variants.(0) <- state.current_expr;
    state.generation <- state.generation + 1;
    Printf.printf "\n=== Generation %d ===\n%!" state.generation;
    Printf.printf "Evolved from: %s\n%!" (to_string state.current_expr);
    save_grid state
  end else
    Printf.printf "Invalid selection: %d (must be 1-%d)\n%!" (selection + 1) num_variants

(** Print help *)
let print_help () =
  Printf.printf "\n";
  Printf.printf "=== Pic Breeder Commands ===\n";
  Printf.printf "  1-12     : Select picture and evolve\n";
  Printf.printf "  u        : Undo (go back to previous generation)\n";
  Printf.printf "  s 1-12   : Save high-quality version of picture\n";
  Printf.printf "  c <mode> : Change color mode (wheel, coords, vibrant, rainbow, fire, ice, twilight)\n";
  Printf.printf "  r        : Reset with new random expressions\n";
  Printf.printf "  g        : Regenerate grid (same parent, new mutations)\n";
  Printf.printf "  p        : Print current expressions\n";
  Printf.printf "  h        : Show this help\n";
  Printf.printf "  q        : Quit\n";
  Printf.printf "\n%!"

(** Print all current expressions *)
let print_expressions state =
  Printf.printf "\n=== Current Expressions ===\n";
  for i = 0 to Array.length state.variants - 1 do
    Printf.printf "%2d: %s\n" (i + 1) (to_string state.variants.(i))
  done;
  Printf.printf "\n%!"

(** Parse color mode *)
let parse_color_mode s =
  match String.lowercase_ascii (String.trim s) with
  | "wheel" -> Some ColorWheel
  | "coords" -> Some Coords
  | "vibrant" -> Some Vibrant
  | "rainbow" -> Some Rainbow
  | "fire" -> Some Fire
  | "ice" -> Some Ice
  | "twilight" -> Some Twilight
  | _ -> None

(** Main REPL loop *)
let rec main_loop state =
  Printf.printf "> %!";
  match In_channel.input_line In_channel.stdin with
  | None -> Printf.printf "\nGoodbye!\n"
  | Some line ->
    let line = String.trim line in
    if line = "" then main_loop state
    else begin
      begin match line with
      | "q" | "quit" | "exit" ->
        Printf.printf "Goodbye!\n";
        exit 0
      | "h" | "help" | "?" ->
        print_help ()
      | "r" | "reset" ->
        let new_state = init_random () in
        state.current_expr <- new_state.current_expr;
        state.variants <- new_state.variants;
        state.generation <- 0;
        Printf.printf "Reset to new random expressions\n%!";
        save_grid state
      | "g" | "regen" ->
        push_history state;  (* Save state before regenerating *)
        state.variants <- generate_variants state.current_expr num_variants;
        state.variants.(0) <- state.current_expr;
        Printf.printf "Regenerated mutations from current parent\n%!";
        save_grid state
      | "p" | "print" ->
        print_expressions state
      | "u" | "undo" ->
        undo state
      | _ ->
        (* Try to parse as number or command *)
        if String.length line >= 2 && line.[0] = 's' && line.[1] = ' ' then begin
          (* Save command *)
          try
            let num = int_of_string (String.trim (String.sub line 2 (String.length line - 2))) in
            save_hq state (num - 1)
          with _ ->
            Printf.printf "Invalid save command. Use: s <number>\n%!"
        end
        else if String.length line >= 2 && line.[0] = 'c' && line.[1] = ' ' then begin
          (* Color mode command *)
          let mode_str = String.sub line 2 (String.length line - 2) in
          match parse_color_mode mode_str with
          | Some mode ->
            state.color_mode <- mode;
            Printf.printf "Color mode set to: %s\n%!" (color_mode_to_string mode);
            save_grid state
          | None ->
            Printf.printf "Unknown color mode: %s\n%!" mode_str;
            Printf.printf "Available: wheel, coords, vibrant, rainbow, fire, ice, twilight\n%!"
        end
        else begin
          try
            let num = int_of_string line in
            if num >= 1 && num <= num_variants then
              evolve state (num - 1)
            else
              Printf.printf "Please enter a number between 1 and %d\n%!" num_variants
          with Failure _ ->
            Printf.printf "Unknown command: %s (type 'h' for help)\n%!" line
        end
      end;
      main_loop state
    end

(** Entry point *)
let () =
  Printf.printf "╔═══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║           PIC BREEDER - Evolutionary Complex Art          ║\n";
  Printf.printf "╠═══════════════════════════════════════════════════════════╣\n";
  Printf.printf "║  Evolve beautiful images from complex number expressions  ║\n";
  Printf.printf "║  Each image maps z (pixel coordinate) through a formula   ║\n";
  Printf.printf "║  and displays the result as a color.                      ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";

  create_output_dir ();

  let state = init_random () in
  Printf.printf "=== Generation 0 ===\n";
  Printf.printf "Starting with %d random expressions\n%!" num_variants;
  save_grid state;
  Printf.printf "\nImages saved to '%s/' directory.\n" output_dir;
  Printf.printf "Open the grid image to see all options, then select a number.\n";
  print_help ();
  main_loop state
