(** Pic Breeder - Interactive evolutionary art with complex expressions
    GUI version using Bogue *)

open Pic_breeder
open Complex_expr
open Mutation
open Color
open Render

module W = Bogue.Widget
module L = Bogue.Layout
module T = Bogue.Trigger
module Main = Bogue.Main

(** Configuration *)
let grid_cols = 4
let grid_rows = 3
let num_variants = grid_cols * grid_rows
let cell_width = 200
let cell_height = 200
let margin = 10
let output_dir = "output"

(** History entry for undo *)
type history_entry = {
  h_expr : expr;
  h_variants : expr array;
  h_generation : int;
}

(** Viewport settings *)
type viewport = {
  center_x : float;
  center_y : float;
  size : float;
}

let default_viewport = { center_x = 0.0; center_y = 0.0; size = 2.0 }

(** Application mode *)
type app_mode =
  | GridMode       (* Main grid selection mode *)
  | EditMode of int  (* Editing a specific picture (index) *)

(** Current state *)
type state = {
  mutable current_expr : expr;
  mutable variants : expr array;
  mutable generation : int;
  mutable color_mode : color_mode;
  mutable complexity_mode : complexity_mode;
  mutable history : history_entry list;
  mutable viewport : viewport;
  mutable mode : app_mode;
  mutable edit_expr : expr option;  (* Expression being edited *)
  mutable slider_range : float;     (* Current slider range for edit mode *)
}

let create_output_dir () =
  if not (Sys.file_exists output_dir) then
    Unix.mkdir output_dir 0o755

(** Compute x and y ranges from viewport *)
let viewport_ranges vp =
  let x_range = (vp.center_x -. vp.size, vp.center_x +. vp.size) in
  let y_range = (vp.center_y -. vp.size, vp.center_y +. vp.size) in
  (x_range, y_range)

(** Convert our RGB image to SDL surface and then texture *)
let render_to_texture renderer expr color_mode viewport width height =
  let open Tsdl in
  let (x_range, y_range) = viewport_ranges viewport in
  let img = render_expr ~color_mode ~x_range ~y_range width height expr in
  let img_height = Array.length img in
  let img_width = if img_height > 0 then Array.length img.(0) else 0 in

  (* Create RGB surface *)
  match Sdl.create_rgb_surface ~w:img_width ~h:img_height ~depth:24
          (Int32.of_int 0x0000FF) (Int32.of_int 0x00FF00)
          (Int32.of_int 0xFF0000) Int32.zero with
  | Error (`Msg e) -> failwith ("Failed to create surface: " ^ e)
  | Ok surface ->
    (* Lock surface for pixel access *)
    (match Sdl.lock_surface surface with
     | Error (`Msg e) -> failwith ("Failed to lock surface: " ^ e)
     | Ok () ->
       let pixels = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
       let pitch = Sdl.get_surface_pitch surface in
       for y = 0 to img_height - 1 do
         for x = 0 to img_width - 1 do
           let c = img.(y).(x) in
           let offset = y * pitch + x * 3 in
           Bigarray.Array1.set pixels offset c.b;
           Bigarray.Array1.set pixels (offset + 1) c.g;
           Bigarray.Array1.set pixels (offset + 2) c.r;
         done
       done;
       Sdl.unlock_surface surface);
    match Sdl.create_texture_from_surface renderer surface with
    | Error (`Msg e) ->
      Sdl.free_surface surface;
      failwith ("Failed to create texture: " ^ e)
    | Ok texture ->
      Sdl.free_surface surface;
      texture

(** Initialize with random expressions *)
let init_random () =
  let variants = generate_initial_population num_variants in
  {
    current_expr = variants.(0);
    variants;
    generation = 0;
    color_mode = ColorWheel;
    complexity_mode = Mutation.Maintain;
    history = [];
    viewport = default_viewport;
    mode = GridMode;
    edit_expr = None;
    slider_range = 0.1;
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
  | [] -> false
  | entry :: rest ->
    state.current_expr <- entry.h_expr;
    state.variants <- entry.h_variants;
    state.generation <- entry.h_generation;
    state.history <- rest;
    true

(** Evolve from a selected picture *)
let evolve state selection =
  if selection >= 0 && selection < Array.length state.variants then begin
    push_history state;
    state.current_expr <- state.variants.(selection);
    state.variants <- generate_variants ~mode:state.complexity_mode state.current_expr num_variants;
    state.variants.(0) <- state.current_expr;
    state.generation <- state.generation + 1;
    true
  end else
    false

(** Save a high-quality version of a specific expression *)
let save_hq_expr expr color_mode viewport filename =
  let (x_range, y_range) = viewport_ranges viewport in
  let img = render_hq ~color_mode ~x_range ~y_range expr in
  save_ppm filename img;
  Printf.printf "Saved: %s\n%!" filename

(** GUI Application *)
module Gui = struct
  open Bogue

  (* Reference for switching layouts *)
  let top_layout_ref : L.t option ref = ref None
  let board_ref : Main.board option ref = ref None

  (* Get grid cell index from position *)
  let cell_from_pos x y =
    let col = (x - margin) / (cell_width + margin) in
    let row = (y - margin) / (cell_height + margin) in
    if col < 0 || col >= grid_cols || row < 0 || row >= grid_rows then None
    else begin
      let cell_x = margin + col * (cell_width + margin) in
      let cell_y = margin + row * (cell_height + margin) in
      if x >= cell_x && x < cell_x + cell_width &&
         y >= cell_y && y < cell_y + cell_height then
        Some (row * grid_cols + col)
      else
        None
    end

  (* Forward declarations for mutual recursion *)
  let create_grid_layout_fn : (state -> L.t) ref = ref (fun _ -> failwith "not initialized")
  let switch_to_grid_fn : (state -> unit) ref = ref (fun _ -> ())

  (* Forward declaration for recreating edit view *)
  let recreate_edit_view_fn : (state -> int -> unit) ref = ref (fun _ _ -> ())

  (* Create edit mode view with sliders *)
  let create_edit_view state idx =
    (* Use current edit_expr if available (preserves edits), otherwise start fresh *)
    let expr = match state.edit_expr with
      | Some e -> e
      | None -> state.variants.(idx)
    in
    state.edit_expr <- Some expr;

    (* Main image area - larger *)
    let img_size = 400 in

    (* Create the widget first, then get its internal sdl_area *)
    let img_widget = W.sdl_area ~w:img_size ~h:img_size () in
    let widget_area = W.get_sdl_area img_widget in

    (* Draw function that renders the current edit expression *)
    let draw_image renderer =
      let open Tsdl in
      match state.edit_expr with
      | None -> ()
      | Some e ->
        (* Get the physical drawing size *)
        let (phys_w, phys_h) = Sdl_area.drawing_size widget_area in
        let texture = render_to_texture renderer e state.color_mode state.viewport phys_w phys_h in
        let dst = Sdl.Rect.create ~x:0 ~y:0 ~w:phys_w ~h:phys_h in
        ignore (Sdl.render_copy ~dst renderer texture);
        Sdl.destroy_texture texture
    in
    (* Add the draw function to the widget's internal area *)
    Sdl_area.add widget_area ~name:"edit_image" draw_image;

    let img_layout = L.resident ~w:img_size ~h:img_size img_widget in

    (* Collect constants from current expression for sliders *)
    let constants = collect_constants expr in
    let num_constants = List.length constants in

    (* Use slider_range from state *)
    let slider_range = state.slider_range in
    let slider_steps = 1000 in  (* 1000 steps for fine control *)

    (* Create sliders for each constant (real and imaginary parts) *)
    let slider_layouts =
      if num_constants = 0 then
        [L.resident (W.label "No constants to edit")]
      else
        List.mapi (fun list_idx (const_idx, c) ->
          (* Current values become the center of the slider range *)
          let re_center = c.re in
          let im_center = c.im in

          (* Slider starts at middle (500 out of 1000) representing current value *)
          let re_label = W.label (Printf.sprintf "C%d Re: %.4f" (list_idx + 1) re_center) in
          let re_slider = W.slider ~value:(slider_steps / 2) ~length:200 ~thickness:20 slider_steps in

          let im_label = W.label (Printf.sprintf "C%d Im: %.4f" (list_idx + 1) im_center) in
          let im_slider = W.slider ~value:(slider_steps / 2) ~length:200 ~thickness:20 slider_steps in

          (* Update function for sliders *)
          let update_constant () =
            match state.edit_expr with
            | None -> ()
            | Some e ->
              (* Map slider [0, slider_steps] to [center - range, center + range] *)
              let re_slider_val = Slider.value (W.get_slider re_slider) in
              let im_slider_val = Slider.value (W.get_slider im_slider) in
              let re_val = re_center +. slider_range *. (float_of_int (re_slider_val - slider_steps / 2) /. float_of_int (slider_steps / 2)) in
              let im_val = im_center +. slider_range *. (float_of_int (im_slider_val - slider_steps / 2) /. float_of_int (slider_steps / 2)) in
              W.set_text re_label (Printf.sprintf "C%d Re: %.4f" (list_idx + 1) re_val);
              W.set_text im_label (Printf.sprintf "C%d Im: %.4f" (list_idx + 1) im_val);
              let new_const = complex re_val im_val in
              state.edit_expr <- Some (replace_constant const_idx new_const e);
              (* Force redraw by clearing and re-adding the draw function *)
              Sdl_area.clear widget_area;
              Sdl_area.add widget_area ~name:"edit_image" draw_image;
              Sdl_area.update widget_area
          in

          (* Connect sliders to update function *)
          let action _ _ _ = update_constant () in
          let conn_re = W.connect re_slider re_slider action Slider.triggers in
          let conn_im = W.connect im_slider im_slider action Slider.triggers in
          W.add_connection re_slider conn_re;
          W.add_connection im_slider conn_im;

          L.tower [
            L.resident re_label;
            L.resident re_slider;
            L.resident im_label;
            L.resident im_slider;
            L.empty ~w:10 ~h:10 ()
          ]
        ) constants
    in

    (* Back button *)
    let back_btn = W.button "Back" in
    let back_action _ _ _ =
      state.mode <- GridMode;
      state.edit_expr <- None;
      state.slider_range <- 0.1;  (* Reset to default *)
      !switch_to_grid_fn state
    in
    W.add_connection back_btn (W.connect back_btn back_btn back_action T.buttons_up);

    (* Save button *)
    let save_btn = W.button "Save HQ" in
    let save_action _ _ _ =
      match state.edit_expr with
      | None -> ()
      | Some e ->
        let filename = Printf.sprintf "%s/edited_gen_%04d_pic_%02d.ppm"
                        output_dir state.generation (idx + 1) in
        save_hq_expr e state.color_mode state.viewport filename
    in
    W.add_connection save_btn (W.connect save_btn save_btn save_action T.buttons_up);

    (* Zoom In button (finer control - halve the range) *)
    let zoom_in_btn = W.button "Zoom In" in
    let zoom_in_action _ _ _ =
      state.slider_range <- state.slider_range /. 2.0;
      !recreate_edit_view_fn state idx
    in
    W.add_connection zoom_in_btn (W.connect zoom_in_btn zoom_in_btn zoom_in_action T.buttons_up);

    (* Zoom Out button (coarser control - double the range) *)
    let zoom_out_btn = W.button "Zoom Out" in
    let zoom_out_action _ _ _ =
      state.slider_range <- state.slider_range *. 2.0;
      !recreate_edit_view_fn state idx
    in
    W.add_connection zoom_out_btn (W.connect zoom_out_btn zoom_out_btn zoom_out_action T.buttons_up);

    (* Range label showing current slider range *)
    let range_label = W.label (Printf.sprintf "Range: +/-%.4f" slider_range) in

    (* Expression label *)
    let expr_str = to_string expr in
    let truncated = if String.length expr_str > 50 then String.sub expr_str 0 50 ^ "..." else expr_str in
    let expr_label = W.label (Printf.sprintf "Expr: %s" truncated) in

    (* Scrollable slider area *)
    let sliders_col = L.tower ~sep:5 slider_layouts in
    let sliders_scroll = L.make_clip ~h:250 sliders_col in

    (* Right panel with sliders and buttons *)
    let right_panel = L.tower ~sep:10 [
      L.resident expr_label;
      L.flat ~sep:10 [L.resident range_label; L.resident zoom_in_btn; L.resident zoom_out_btn];
      sliders_scroll;
      L.flat ~sep:10 [L.resident back_btn; L.resident save_btn]
    ] in

    L.flat ~sep:20 [img_layout; right_panel]

  (* Function to recreate edit view (used by zoom buttons) *)
  let recreate_edit_view state idx =
    match !top_layout_ref with
    | None -> ()
    | Some top ->
      let edit_layout = create_edit_view state idx in
      L.set_rooms top [edit_layout]

  let () = recreate_edit_view_fn := recreate_edit_view

  (* Create the main grid layout *)
  let create_grid_layout state =
    let total_width = grid_cols * cell_width + (grid_cols + 1) * margin in
    let total_height = grid_rows * cell_height + (grid_rows + 1) * margin in

    let grid_widget = W.sdl_area ~w:total_width ~h:total_height () in
    (* We need to set up drawing on the widget's internal sdl_area *)
    let widget_area = W.get_sdl_area grid_widget in

    (* Copy the draw function to the widget's area *)
    let draw renderer =
      let open Tsdl in
      (* Get the physical drawing size to compute scale factor *)
      let (phys_w, phys_h) = Sdl_area.drawing_size widget_area in
      let scale_x = float_of_int phys_w /. float_of_int total_width in
      let scale_y = float_of_int phys_h /. float_of_int total_height in

      ignore (Sdl.set_render_draw_color renderer 40 40 40 255);
      ignore (Sdl.render_clear renderer);
      for i = 0 to num_variants - 1 do
        let row = i / grid_cols in
        let col = i mod grid_cols in
        (* Scale logical coordinates to physical pixels *)
        let x = int_of_float (float_of_int (margin + col * (cell_width + margin)) *. scale_x) in
        let y = int_of_float (float_of_int (margin + row * (cell_height + margin)) *. scale_y) in
        let w = int_of_float (float_of_int cell_width *. scale_x) in
        let h = int_of_float (float_of_int cell_height *. scale_y) in
        (* Render at physical size *)
        let texture = render_to_texture renderer state.variants.(i)
                        state.color_mode state.viewport w h in
        let dst = Sdl.Rect.create ~x ~y ~w ~h in
        ignore (Sdl.render_copy ~dst renderer texture);
        Sdl.destroy_texture texture;
        let num = i + 1 in
        let hue = float_of_int (num * 30 mod 360) in
        let indicator = hsv_to_rgb hue 1.0 1.0 in
        ignore (Sdl.set_render_draw_color renderer indicator.r indicator.g indicator.b 255);
        let box_x = int_of_float (float_of_int (margin + col * (cell_width + margin) + 2) *. scale_x) in
        let box_y = int_of_float (float_of_int (margin + row * (cell_height + margin) + 2) *. scale_y) in
        let box_size = int_of_float (14.0 *. scale_x) in
        let box = Sdl.Rect.create ~x:box_x ~y:box_y ~w:box_size ~h:box_size in
        ignore (Sdl.render_fill_rect renderer (Some box))
      done
    in
    Sdl_area.add widget_area ~name:"grid" draw;

    let grid_layout = L.resident ~w:total_width ~h:total_height grid_widget in

    (* Status bar *)
    let status = W.label (Printf.sprintf "Gen %d | %s | %s"
                           state.generation (color_mode_to_string state.color_mode)
                           (complexity_mode_to_string state.complexity_mode)) in
    let status_layout = L.resident status in

    (* Control buttons *)
    let undo_btn = W.button "Undo" in
    let reset_btn = W.button "Reset" in
    let regen_btn = W.button "Regen" in
    let color_btn = W.button "Color" in
    let complexity_btn = W.button "Complexity" in

    (* Helper to refresh the grid *)
    let refresh_grid () =
      W.set_text status (Printf.sprintf "Gen %d | %s | %s"
                          state.generation (color_mode_to_string state.color_mode)
                          (complexity_mode_to_string state.complexity_mode));
      Sdl_area.clear widget_area;
      Sdl_area.add widget_area ~name:"grid" draw;
      Sdl_area.update widget_area
    in

    (* Button actions *)
    let undo_action _ _ _ =
      if undo state then refresh_grid ()
    in
    W.add_connection undo_btn (W.connect undo_btn undo_btn undo_action T.buttons_up);

    let reset_action _ _ _ =
      let new_state = init_random () in
      state.current_expr <- new_state.current_expr;
      state.variants <- new_state.variants;
      state.generation <- 0;
      state.history <- [];
      refresh_grid ()
    in
    W.add_connection reset_btn (W.connect reset_btn reset_btn reset_action T.buttons_up);

    let regen_action _ _ _ =
      push_history state;
      state.variants <- generate_variants ~mode:state.complexity_mode state.current_expr num_variants;
      state.variants.(0) <- state.current_expr;
      refresh_grid ()
    in
    W.add_connection regen_btn (W.connect regen_btn regen_btn regen_action T.buttons_up);

    let color_action _ _ _ =
      let modes = all_color_modes in
      let current_idx = ref 0 in
      Array.iteri (fun i m -> if m = state.color_mode then current_idx := i) modes;
      let next_idx = (!current_idx + 1) mod Array.length modes in
      state.color_mode <- modes.(next_idx);
      refresh_grid ()
    in
    W.add_connection color_btn (W.connect color_btn color_btn color_action T.buttons_up);

    let complexity_action _ _ _ =
      let modes = all_complexity_modes in
      let current_idx = ref 0 in
      Array.iteri (fun i m -> if m = state.complexity_mode then current_idx := i) modes;
      let next_idx = (!current_idx + 1) mod Array.length modes in
      state.complexity_mode <- modes.(next_idx);
      refresh_grid ()
    in
    W.add_connection complexity_btn (W.connect complexity_btn complexity_btn complexity_action T.buttons_up);

    (* Grid click handler *)
    let grid_click_action _ _ ev =
      let open Tsdl in
      (* Get mouse position *)
      let (mx, my) = Mouse.button_pos ev in
      (* Get layout position *)
      let lx = L.xpos grid_layout in
      let ly = L.ypos grid_layout in
      let rel_x = mx - lx in
      let rel_y = my - ly in

      match cell_from_pos rel_x rel_y with
      | None -> ()
      | Some idx ->
        (* Check for Ctrl/Cmd modifier *)
        let kmods = Sdl.get_mod_state () in
        let ctrl_pressed = Sdl.Kmod.(kmods land (lctrl + rctrl + lgui + rgui)) <> Sdl.Kmod.none in

        if ctrl_pressed then begin
          (* Edit mode *)
          Printf.printf "Edit mode: picture %d\n%!" (idx + 1);
          state.mode <- EditMode idx;

          (* Create edit view and switch to it *)
          let edit_layout = create_edit_view state idx in
          match !top_layout_ref with
          | None -> ()
          | Some top ->
            L.set_rooms top [edit_layout]
        end else begin
          (* Evolve mode *)
          if evolve state idx then begin
            Printf.printf "Gen %d: Evolved from picture %d\n%!" state.generation (idx + 1);
            refresh_grid ()
          end
        end
    in
    W.add_connection grid_widget (W.connect grid_widget grid_widget grid_click_action T.buttons_up);

    let buttons_layout = L.flat ~sep:10 [
      L.resident undo_btn;
      L.resident reset_btn;
      L.resident regen_btn;
      L.resident color_btn;
      L.resident complexity_btn;
    ] in

    L.tower ~sep:10 [
      status_layout;
      grid_layout;
      buttons_layout;
    ]

  (* Set up the forward reference *)
  let () = create_grid_layout_fn := create_grid_layout

  (* Switch back to grid mode *)
  let switch_to_grid state =
    match !top_layout_ref with
    | None -> ()
    | Some top ->
      let grid_layout = create_grid_layout state in
      L.set_rooms top [grid_layout]

  let () = switch_to_grid_fn := switch_to_grid

  (* Main GUI entry point *)
  let run () =
    create_output_dir ();

    let state = init_random () in

    Printf.printf "=== Pic Breeder GUI ===\n";
    Printf.printf "Click on a picture to evolve from it\n";
    Printf.printf "Ctrl+Click (Cmd+Click on Mac) to enter edit mode\n%!";

    (* Create initial layout *)
    let inner_layout = create_grid_layout state in

    (* Wrap in a container so we can swap contents *)
    let top_layout = L.tower [inner_layout] in
    top_layout_ref := Some top_layout;

    (* Create shortcuts *)
    let shortcuts = Main.shortcuts_empty () in
    let shortcuts = Main.shortcuts_add shortcuts Tsdl.Sdl.K.escape (fun _ -> raise Main.Exit) in
    let shortcuts = Main.shortcuts_add_ctrl shortcuts Tsdl.Sdl.K.z (fun _ ->
      if undo state then begin
        switch_to_grid state
      end
    ) in

    (* Create and run the board *)
    let board = Main.of_layout ~shortcuts top_layout in
    board_ref := Some board;

    Main.run board;
    Main.quit ()

end

(** Entry point - run GUI by default *)
let () =
  (* Check for --cli flag for command line mode *)
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--cli" then begin
    Printf.printf "CLI mode not available in GUI version.\n";
    Printf.printf "Run without --cli for GUI mode.\n"
  end else
    Gui.run ()
