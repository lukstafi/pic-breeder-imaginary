(** Image rendering from expressions *)

open Complex_expr
open Color

(** Image represented as a 2D array of RGB pixels *)
type image = rgb array array

(** Create an empty image *)
let create_image width height =
  Array.init height (fun _ -> Array.init width (fun _ -> rgb 0 0 0))

(** Render an expression to an image
    Maps pixel coordinates to complex plane:
    x in [0, width-1] -> real in [-2, 2]
    y in [0, height-1] -> imag in [-2, 2] (inverted so +i is up)
*)
let render_expr ?(color_mode=ColorWheel) ?(x_range=(-2.0, 2.0)) ?(y_range=(-2.0, 2.0))
                width height expr =
  let img = create_image width height in
  let (x_min, x_max) = x_range in
  let (y_min, y_max) = y_range in
  let color_fn = get_color_fn color_mode in
  for py = 0 to height - 1 do
    for px = 0 to width - 1 do
      (* Map pixel to complex coordinate *)
      let x = x_min +. (float_of_int px /. float_of_int (width - 1)) *. (x_max -. x_min) in
      (* Invert y so positive imaginary is up *)
      let y = y_max -. (float_of_int py /. float_of_int (height - 1)) *. (y_max -. y_min) in
      let z = complex x y in
      let result = eval z expr in
      img.(py).(px) <- color_fn result
    done
  done;
  img

(** Save image as PPM (P6 binary format) *)
let save_ppm filename img =
  let height = Array.length img in
  let width = if height > 0 then Array.length img.(0) else 0 in
  let oc = open_out_bin filename in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let c = img.(y).(x) in
      output_char oc (char_of_int c.r);
      output_char oc (char_of_int c.g);
      output_char oc (char_of_int c.b);
    done
  done;
  close_out oc

(** Save image as PPM (P3 ASCII format - more compatible) *)
let save_ppm_ascii filename img =
  let height = Array.length img in
  let width = if height > 0 then Array.length img.(0) else 0 in
  let oc = open_out filename in
  Printf.fprintf oc "P3\n%d %d\n255\n" width height;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let c = img.(y).(x) in
      Printf.fprintf oc "%d %d %d " c.r c.g c.b;
    done;
    output_char oc '\n';
  done;
  close_out oc

(** Render a grid of expressions (for selection panel) *)
let render_grid ?(color_mode=ColorWheel) ?(cell_width=200) ?(cell_height=200)
                ?(margin=5) cols rows exprs =
  let n = Array.length exprs in
  let total_width = cols * cell_width + (cols + 1) * margin in
  let total_height = rows * cell_height + (rows + 1) * margin in
  let grid = create_image total_width total_height in
  (* Fill with dark gray background *)
  for y = 0 to total_height - 1 do
    for x = 0 to total_width - 1 do
      grid.(y).(x) <- rgb 40 40 40
    done
  done;
  (* Render each cell *)
  for i = 0 to min n (cols * rows) - 1 do
    let row = i / cols in
    let col = i mod cols in
    let cell_img = render_expr ~color_mode cell_width cell_height exprs.(i) in
    let x_offset = margin + col * (cell_width + margin) in
    let y_offset = margin + row * (cell_height + margin) in
    (* Copy cell into grid *)
    for cy = 0 to cell_height - 1 do
      for cx = 0 to cell_width - 1 do
        grid.(y_offset + cy).(x_offset + cx) <- cell_img.(cy).(cx)
      done
    done
  done;
  grid

(** Render a grid with labels (numbers in corners) *)
let render_grid_labeled ?(color_mode=ColorWheel) ?(cell_width=200) ?(cell_height=200)
                        ?(margin=10) cols rows exprs =
  let grid = render_grid ~color_mode ~cell_width ~cell_height ~margin cols rows exprs in
  (* Simple number rendering - just mark corners with colored pixels *)
  let n = min (Array.length exprs) (cols * rows) in
  for i = 0 to n - 1 do
    let row = i / cols in
    let col = i mod cols in
    let x_offset = margin + col * (cell_width + margin) in
    let y_offset = margin + row * (cell_height + margin) in
    (* Draw number indicator in top-left corner *)
    (* Use a simple pattern: colored square indicating the number *)
    let num = i + 1 in
    let hue = float_of_int (num * 30 mod 360) in
    let indicator_color = hsv_to_rgb hue 1.0 1.0 in
    (* Draw a small box *)
    for dy = 2 to 12 do
      for dx = 2 to 12 do
        if dy < 12 && dx < 12 then
          grid.(y_offset + dy).(x_offset + dx) <- indicator_color
      done
    done;
    (* Draw the number using simple pixel patterns *)
    let draw_digit d x_start y_start =
      let white = rgb 255 255 255 in
      let patterns = [|
        (* 0 *) [|0b111; 0b101; 0b101; 0b101; 0b111|];
        (* 1 *) [|0b010; 0b110; 0b010; 0b010; 0b111|];
        (* 2 *) [|0b111; 0b001; 0b111; 0b100; 0b111|];
        (* 3 *) [|0b111; 0b001; 0b111; 0b001; 0b111|];
        (* 4 *) [|0b101; 0b101; 0b111; 0b001; 0b001|];
        (* 5 *) [|0b111; 0b100; 0b111; 0b001; 0b111|];
        (* 6 *) [|0b111; 0b100; 0b111; 0b101; 0b111|];
        (* 7 *) [|0b111; 0b001; 0b001; 0b001; 0b001|];
        (* 8 *) [|0b111; 0b101; 0b111; 0b101; 0b111|];
        (* 9 *) [|0b111; 0b101; 0b111; 0b001; 0b111|];
      |] in
      let pattern = patterns.(d mod 10) in
      for row = 0 to 4 do
        for col = 0 to 2 do
          if (pattern.(row) lsr (2 - col)) land 1 = 1 then begin
            let px = x_start + col in
            let py = y_start + row in
            if py >= 0 && py < Array.length grid &&
               px >= 0 && px < Array.length grid.(0) then
              grid.(py).(px) <- white
          end
        done
      done
    in
    (* Draw number (up to 2 digits) *)
    if num < 10 then
      draw_digit num (x_offset + 5) (y_offset + 4)
    else begin
      draw_digit (num / 10) (x_offset + 3) (y_offset + 4);
      draw_digit (num mod 10) (x_offset + 7) (y_offset + 4)
    end
  done;
  grid

(** Quick preview - render at low resolution *)
let render_preview ?(color_mode=ColorWheel) expr =
  render_expr ~color_mode 100 100 expr

(** High quality render *)
let render_hq ?(color_mode=ColorWheel) expr =
  render_expr ~color_mode 800 800 expr
