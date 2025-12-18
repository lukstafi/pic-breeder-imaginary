(** Color mapping from complex numbers to RGB *)

open Complex_expr

type rgb = { r : int; g : int; b : int }

let rgb r g b = { r; g; b }

(** Convert HSL to RGB
    h in [0, 360), s in [0, 1], l in [0, 1] *)
let hsl_to_rgb h s l =
  let h = mod_float h 360.0 in
  let h = if h < 0.0 then h +. 360.0 else h in
  let c = (1.0 -. abs_float (2.0 *. l -. 1.0)) *. s in
  let x = c *. (1.0 -. abs_float (mod_float (h /. 60.0) 2.0 -. 1.0)) in
  let m = l -. c /. 2.0 in
  let (r', g', b') =
    if h < 60.0 then (c, x, 0.0)
    else if h < 120.0 then (x, c, 0.0)
    else if h < 180.0 then (0.0, c, x)
    else if h < 240.0 then (0.0, x, c)
    else if h < 300.0 then (x, 0.0, c)
    else (c, 0.0, x)
  in
  let to_int v = int_of_float (min 255.0 (max 0.0 ((v +. m) *. 255.0))) in
  rgb (to_int r') (to_int g') (to_int b')

(** Convert HSV to RGB
    h in [0, 360), s in [0, 1], v in [0, 1] *)
let hsv_to_rgb h s v =
  let h = mod_float h 360.0 in
  let h = if h < 0.0 then h +. 360.0 else h in
  let c = v *. s in
  let x = c *. (1.0 -. abs_float (mod_float (h /. 60.0) 2.0 -. 1.0)) in
  let m = v -. c in
  let (r', g', b') =
    if h < 60.0 then (c, x, 0.0)
    else if h < 120.0 then (x, c, 0.0)
    else if h < 180.0 then (0.0, c, x)
    else if h < 240.0 then (0.0, x, c)
    else if h < 300.0 then (x, 0.0, c)
    else (c, 0.0, x)
  in
  let to_int v = int_of_float (min 255.0 (max 0.0 ((v +. m) *. 255.0))) in
  rgb (to_int r') (to_int g') (to_int b')

(** Smooth function to map any real to [0, 1] *)
let smooth x =
  0.5 +. 0.5 *. tanh (x *. 0.5)

(** Map complex number to color using color wheel approach
    - Argument (angle) -> Hue
    - Magnitude -> Saturation/Brightness
    This creates a classic domain coloring effect *)
let complex_to_color_wheel z =
  let arg = c_arg z in
  let mag = c_abs z in
  (* Map angle to hue (0-360) *)
  let hue = (arg +. Float.pi) *. 180.0 /. Float.pi in
  (* Map magnitude to saturation and lightness *)
  let sat = 1.0 -. (1.0 /. (1.0 +. mag)) in
  let light = 1.0 -. (1.0 /. (1.0 +. mag *. 0.5)) in
  let light = 0.1 +. 0.8 *. light in  (* Keep in visible range *)
  hsl_to_rgb hue sat light

(** Alternative: Map using both real and imaginary parts
    - Real part -> Hue
    - Imaginary part -> Saturation
    - Magnitude -> Lightness *)
let complex_to_color_coords z =
  let hue = smooth z.re *. 360.0 in
  let sat = smooth z.im in
  let mag = c_abs z in
  let light = 0.1 +. 0.8 *. (1.0 -. 1.0 /. (1.0 +. mag *. 0.3)) in
  hsl_to_rgb hue sat light

(** Vibrant color mapping - uses HSV for more saturated colors
    Great for creating psychedelic effects *)
let complex_to_color_vibrant z =
  let arg = c_arg z in
  let mag = c_abs z in
  let hue = (arg +. Float.pi) *. 180.0 /. Float.pi in
  (* Use a cyclic function of magnitude for value to create rings *)
  let v = 0.3 +. 0.7 *. (0.5 +. 0.5 *. sin (mag *. 2.0)) in
  let s = 0.7 +. 0.3 *. (0.5 +. 0.5 *. cos (mag *. 1.5)) in
  hsv_to_rgb hue s v

(** Rainbow spiral - creates rainbow patterns based on both components *)
let complex_to_color_rainbow z =
  let x = z.re in
  let y = z.im in
  (* Create independent color channels from the complex plane *)
  let r = int_of_float (127.5 +. 127.5 *. sin (x *. 3.0 +. y)) in
  let g = int_of_float (127.5 +. 127.5 *. sin (y *. 3.0 +. x +. 2.094)) in
  let b = int_of_float (127.5 +. 127.5 *. sin ((x +. y) *. 2.0 +. 4.188)) in
  rgb (max 0 (min 255 r)) (max 0 (min 255 g)) (max 0 (min 255 b))

(** Fire colormap - warm colors based on magnitude and argument *)
let complex_to_color_fire z =
  let mag = c_abs z in
  let arg = c_arg z in
  let t = smooth mag in
  let phase = (arg +. Float.pi) /. (2.0 *. Float.pi) in
  (* Fire: black -> red -> orange -> yellow -> white *)
  let r = min 1.0 (t *. 3.0) in
  let g = min 1.0 (max 0.0 ((t -. 0.33) *. 3.0)) in
  let b = min 1.0 (max 0.0 ((t -. 0.67) *. 3.0)) in
  (* Add some variation with argument *)
  let r = r *. (0.8 +. 0.2 *. cos (phase *. Float.pi *. 4.0)) in
  rgb (int_of_float (r *. 255.0))
      (int_of_float (g *. 255.0))
      (int_of_float (b *. 255.0))

(** Cool/Ice colormap *)
let complex_to_color_ice z =
  let mag = c_abs z in
  let arg = c_arg z in
  let t = smooth mag in
  let phase = (arg +. Float.pi) /. (2.0 *. Float.pi) in
  (* Ice: dark blue -> cyan -> white *)
  let b = 0.3 +. 0.7 *. t in
  let g = t *. 0.8 in
  let r = max 0.0 ((t -. 0.5) *. 2.0) in
  (* Variation *)
  let b = b *. (0.9 +. 0.1 *. sin (phase *. Float.pi *. 6.0)) in
  rgb (int_of_float (r *. 255.0))
      (int_of_float (g *. 255.0))
      (int_of_float (b *. 255.0))

(** Twilight - purple/pink/orange sunset colors *)
let complex_to_color_twilight z =
  let mag = c_abs z in
  let arg = c_arg z in
  let t = smooth mag in
  let hue_base = 270.0 +. 90.0 *. t in  (* Purple to orange *)
  let hue = hue_base +. 30.0 *. sin (arg *. 2.0) in
  let sat = 0.6 +. 0.4 *. (1.0 -. t) in
  let val_ = 0.4 +. 0.6 *. t in
  hsv_to_rgb hue sat val_

type color_mode =
  | ColorWheel
  | Coords
  | Vibrant
  | Rainbow
  | Fire
  | Ice
  | Twilight

let color_mode_to_string = function
  | ColorWheel -> "wheel"
  | Coords -> "coords"
  | Vibrant -> "vibrant"
  | Rainbow -> "rainbow"
  | Fire -> "fire"
  | Ice -> "ice"
  | Twilight -> "twilight"

let all_color_modes = [| ColorWheel; Coords; Vibrant; Rainbow; Fire; Ice; Twilight |]

(** Get the color mapping function for a mode *)
let get_color_fn = function
  | ColorWheel -> complex_to_color_wheel
  | Coords -> complex_to_color_coords
  | Vibrant -> complex_to_color_vibrant
  | Rainbow -> complex_to_color_rainbow
  | Fire -> complex_to_color_fire
  | Ice -> complex_to_color_ice
  | Twilight -> complex_to_color_twilight
