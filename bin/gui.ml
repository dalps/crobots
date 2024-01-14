open Crobots
open Raylib

module V = Vector2
module R = Rectangle

let font_path = "bin/fonts/monogram.ttf"
let stat_fontsize = 28
let stat_fontsize_f = stat_fontsize |> float
let winner_fontsize = 80
let font_spacing = 1.
let stat_font = ref (Raylib.get_font_default ())
let winner_font = ref (Raylib.get_font_default ())

let dummy_texture =
  Texture2D.create Unsigned.UInt.zero 0 0 0 PixelFormat.Compressed_astc_4x4_rgba

let skull_texture = ref dummy_texture
let tank_texture = ref dummy_texture
let turret_texture = ref dummy_texture
let tank_shadow_texture = ref dummy_texture
let turret_shadow_texture = ref dummy_texture
let trail_texture = ref dummy_texture
let box_texture = ref dummy_texture
let arena_shadow_texture = ref dummy_texture

let text_color = Color.raywhite
let background_color = Color.create 58 68 102 255
let skull_color = fade Color.red 0.75

let arena_padding = 15
let stats_width = 400
let arena_width = 1000

let arena_texture_width = 96
let arena_border_thickness = arena_texture_width / 3

let dstrec_arena =
  R.create (arena_padding |> float) (arena_padding |> float)
    (arena_width + (arena_border_thickness * 2) |> float)
    (arena_width + (arena_border_thickness * 2) |> float)

let npatch_arena =
  let width = arena_texture_width in
  let border = arena_border_thickness in
  NPatchInfo.create
    (R.create 0. 0. (width |> float) (width |> float))
    border border border border NPatchLayout.Nine_patch

let npatch_stat =
  let width = arena_texture_width in
  let border = 16 in
  NPatchInfo.create
    (R.create 0. 96. (width |> float) (width |> float))
    border border border border NPatchLayout.Nine_patch

let padding = arena_padding + arena_border_thickness
let window_height = arena_width + (2 * padding)
let window_width = window_height + padding + stats_width
let window_width_f = window_width |> float
let window_height_f = window_height |> float

let statbox_n = 5
let statbox_width = stats_width
let statbox_height = (window_height - ((statbox_n + 1) * padding)) / statbox_n

let stat_height = stat_fontsize
let name_sep = 20

let get_srcrec texture =
  (* Printf.printf "w: %d, h: %d\n" (Texture.width texture)(Texture.height texture); *)
  R.create 0. 0.
    (Texture.width texture |> float)
    (Texture.height texture |> float)

let load_fonts () =
  stat_font := load_font_ex font_path stat_fontsize None;
  winner_font := load_font_ex font_path winner_fontsize None;
  gen_texture_mipmaps (addr (Font.texture !stat_font));
  set_texture_filter (Font.texture !stat_font) TextureFilter.Point

let unload_fonts () =
  unload_font !stat_font;
  unload_font !winner_font

let load_textures () =
  skull_texture := load_texture "bin/textures/skull.png";
  tank_texture := load_texture "bin/textures/tank.png";
  turret_texture := load_texture "bin/textures/turret.png";
  tank_shadow_texture := load_texture "bin/textures/tank_shadow.png";
  turret_shadow_texture := load_texture "bin/textures/turret_shadow.png";
  trail_texture := load_texture "bin/textures/trail.png";
  box_texture := load_texture "bin/textures/gui.png"

let draw_stat_text text pos_x pos_y color =
  draw_text_ex !stat_font text
    (V.create (pos_x |> float) (pos_y |> float))
    stat_fontsize_f font_spacing color

let draw_stat_text_s text pos_x pos_y size color =
  draw_text_ex !stat_font text
    (V.create (pos_x |> float) (pos_y |> float))
    size font_spacing color

let measure_stat_text text =
  measure_text_ex !stat_font text stat_fontsize_f font_spacing

let draw_arena () =
  draw_line padding
    (padding + (arena_width / 2))
    (padding + arena_width)
    (padding + (arena_width / 2))
    Color.lightgray;
  draw_line
    (padding + (arena_width / 2))
    padding
    (padding + (arena_width / 2))
    (padding + arena_width) Color.lightgray;
  draw_texture_npatch !box_texture npatch_arena dstrec_arena (V.zero ()) 0.
    Color.white

let draw_stats i (r : Robot.t) color =
  let spr = Printf.sprintf in
  let pos_x = window_height in
  let pos_y = arena_padding + ((statbox_height + arena_padding) * i) in
  let col_sep = 150 in
  let row_padding = 20 in
  let col_padding = row_padding + 10 in

  let dstrec =
    R.create (pos_x |> float) (pos_y |> float) (statbox_width |> float)
      (statbox_height |> float)
  in
  draw_texture_npatch !box_texture npatch_stat dstrec (V.zero ()) 0.
    (color_contrast color (-0.));
  draw_stat_text (spr "%d. %s" i r.name) (pos_x + col_padding)
    (pos_y + row_padding) text_color;

  (if r.status = Robot.DEAD then
     let skull_width = stat_fontsize_f in
     let srcrec = get_srcrec !skull_texture in
     let dstrec =
       R.(
         create
           ((pos_x |> float)
           +. (statbox_width - row_padding |> float)
           -. skull_width)
           (pos_y + row_padding |> float)
           skull_width skull_width)
     in
     draw_texture_pro !skull_texture srcrec dstrec (V.zero ()) 0. skull_color);

  draw_stat_text (spr "d%%: %d" r.damage) (pos_x + col_padding)
    (pos_y + name_sep + (stat_height * 1))
    text_color;
  draw_stat_text
    (spr "sc: %d" r.scan_degrees)
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + (stat_height * 1))
    text_color;

  draw_stat_text (spr "sp: %d" r.speed) (pos_x + col_padding)
    (pos_y + name_sep + (stat_height * 2))
    text_color;
  draw_stat_text (spr "hd: %d" r.heading)
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + (stat_height * 2))
    text_color;

  draw_stat_text
    (spr " x: %d" (r.x / Robot.click))
    (pos_x + col_padding)
    (pos_y + name_sep + (stat_height * 3))
    text_color;
  draw_stat_text
    (spr " y: %d" (r.y / Robot.click))
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + (stat_height * 3))
    text_color

let draw_endgame result =
  let h1 = winner_fontsize |> float in
  let h2 = 48. in
  let t1 = "Quit [Esc] - New match [R]" in
  let w = measure_text_ex !stat_font result h1 font_spacing in
  let t1w = measure_text_ex !stat_font t1 h2 font_spacing in
  draw_rectangle 0 0 window_width window_height (fade Color.white 0.35);
  draw_text_ex !winner_font result
    (V.create
       ((window_width_f /. 2.) -. (V.x w /. 2.))
       ((window_height_f /. 2.) -. (V.y w /. 2.)))
    h1 font_spacing text_color;
  draw_text_ex !winner_font t1
    (V.create
       ((window_width_f /. 2.) -. (V.x t1w /. 2.))
       ((window_height_f /. 2.) -. (V.y t1w /. 2.) +. V.y w))
    h2 font_spacing text_color

let draw_cycles c =
  let text = Printf.sprintf "CPU cycle: %6d" c in
  let v = measure_stat_text text in
  draw_stat_text text
    (window_width - (V.x v |> int_of_float) - padding)
    (window_height - (V.y v |> int_of_float) - padding)
    text_color

let draw_fps n =
  let text = Printf.sprintf "FPS: %6d" n in
  let v = measure_stat_text text in
  draw_stat_text text
    (window_width - (V.x v |> int_of_float) - padding)
    (window_height - ((V.y v |> int_of_float) * 2) - padding)
    text_color
