open Crobots
open Raylib

module V = Vector2
module R = Rectangle

let window_width = 1200
let window_height = 900

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
let arena_texture_width = 96
let arena_w = arena_texture_width |> float
let bottom_bar_width = window_width - (arena_padding * 2)
let bottom_bar_height = 50
let arena_border_thickness = arena_texture_width / 3
let padding = arena_padding + arena_border_thickness
let arena_width =
  window_height - (2 * padding) - (bottom_bar_height + arena_padding)

let statbox_n = 4
let stats_width =
  window_width - arena_width - (arena_border_thickness * 2) - (arena_padding * 3)
let stats_height = window_height - (bottom_bar_height + arena_padding)
let stat_height = stat_fontsize
let statbox_width = stats_width
let statbox_height =
  (stats_height - ((statbox_n + 1) * arena_padding)) / statbox_n
let name_sep = 20

let dstrec_arena =
  R.create (arena_padding |> float) (arena_padding |> float)
    (arena_width + (arena_border_thickness * 2) |> float)
    (arena_width + (arena_border_thickness * 2) |> float)

let npatch_arena =
  let border = arena_border_thickness in
  NPatchInfo.create
    (R.create 0. 0. arena_w arena_w)
    border border border border NPatchLayout.Nine_patch

let npatch_stat =
  NPatchInfo.create
    (R.create 0. 96. arena_w arena_w)
    32 48 32 32 NPatchLayout.Nine_patch

let npatch_avatar =
  NPatchInfo.create
    (R.create 0. (96. *. 2.) arena_w arena_w)
    32 32 32 32 NPatchLayout.Nine_patch

let npatch_slab =
  NPatchInfo.create
    (R.create 0. (96. *. 3.) arena_w 38.)
    12 12 12 12 NPatchLayout.Nine_patch

let get_srcrec texture =
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
  draw_texture_npatch !box_texture npatch_arena dstrec_arena (V.zero ()) 0.
    Color.white;
  draw_rectangle_lines padding padding arena_width arena_width Color.lightgray;
  draw_line padding
    (padding + (arena_width / 2))
    (padding + arena_width)
    (padding + (arena_width / 2))
    Color.lightgray;
  draw_line
    (padding + (arena_width / 2))
    padding
    (padding + (arena_width / 2))
    (padding + arena_width) Color.lightgray

let draw_stats i (r : Robot.t) _ =
  let spr = Printf.sprintf in
  let pos_x = arena_width + (padding * 2) in
  let pos_y = arena_padding + ((statbox_height + arena_padding) * i) in
  let padding_x = 32 in

  let title_text = spr "%d.%s" i r.name in
  let wh = measure_stat_text title_text in
  let wp = measure_stat_text "AA: 000" in
  let dstrec =
    R.create (pos_x |> float) (pos_y |> float) (statbox_width |> float)
      (statbox_height |> float)
  in
  draw_texture_npatch !box_texture npatch_stat dstrec (V.zero ()) 0. Color.white;

  let avatar_size = 72 in

  let dstrec =
    R.create (pos_x |> float) (pos_y |> float) (statbox_width |> float)
      (avatar_size |> float)
  in
  draw_texture_npatch !box_texture npatch_slab dstrec (V.zero ()) 0. Color.white;

  let dstrec =
    R.create (pos_x |> float) (pos_y |> float) (avatar_size |> float)
      (avatar_size |> float)
  in
  draw_texture_npatch !box_texture npatch_avatar dstrec (V.zero ()) 0.
    Color.white;

  let dstrec =
    R.create (arena_padding |> float)
      ((padding * 2) + arena_width |> float)
      (bottom_bar_width |> float)
      (bottom_bar_height |> float)
  in
  draw_texture_npatch !box_texture npatch_slab dstrec (V.zero ()) 0. Color.white;

  draw_stat_text title_text
    (pos_x + avatar_size + 16)
    (pos_y + (avatar_size / 2) - (V.y wh *. 0.5 |> int_of_float))
    text_color;

  (if r.status = Robot.DEAD then
     let skull_width = stat_fontsize_f *. 1.5 in
     let srcrec = get_srcrec !skull_texture in
     let dstrec =
       R.(
         create
           ((pos_x |> float)
           +. ((avatar_size |> float) *. 0.5)
           -. (skull_width *. 0.5))
           ((pos_y |> float)
           +. ((avatar_size |> float) *. 0.5)
           -. (skull_width *. 0.5))
           skull_width skull_width)
     in
     draw_texture_pro !skull_texture srcrec dstrec (V.zero ()) 0. skull_color);

  let x, y = (pos_x + padding_x, pos_y + (avatar_size / 2) + (padding_x / 2)) in
  draw_stat_text (spr "D%%: %3.0f" r.damage) x (y + stat_height) text_color;
  draw_stat_text (spr "SP: %3.0f" r.speed) x (y + (stat_height * 2)) text_color;
  draw_stat_text (spr "PX: %3.0f" r.p.x) x (y + (stat_height * 3)) text_color;

  let x = pos_x + statbox_width - (V.x wp |> int_of_float) - padding_x in
  draw_stat_text (spr "SC: %3.0f" r.scan_degrees) x (y + stat_height) text_color;
  draw_stat_text
    (spr "HD: %3.0f" r.heading)
    x
    (y + (stat_height * 2))
    text_color;
  draw_stat_text (spr "PY: %3.0f" r.p.y) x (y + (stat_height * 3)) text_color

let draw_endgame result =
  let h1 = winner_fontsize |> float in
  let h2 = 36. in
  let t1 = "Quit [Esc] - New match [R]" in
  let w = measure_text_ex !stat_font result h1 font_spacing in
  let t1w = measure_text_ex !stat_font t1 h2 font_spacing in
  let window_width_f, window_height_f =
    (window_width |> float, window_height |> float)
  in
  begin_blend_mode BlendMode.Multiplied;
  draw_rectangle 0 0 window_width window_height (fade Color.black 0.35);
  end_blend_mode ();

  let textbox_width, textbox_height = (V.x w +. 50., V.y w +. 50.) in
  let dstrec =
    R.create
      (((window_width |> float) *. 0.5) -. (textbox_width *. 0.5))
      (((window_height |> float) *. 0.5) -. (textbox_height *. 0.5))
      textbox_width textbox_height
  in
  draw_texture_npatch !box_texture npatch_slab dstrec (V.zero ()) 0. Color.white;
  let t1_width, t1_height = (V.x t1w +. 25., V.y t1w +. 25.) in
  let dstrec =
    R.create
      (((window_width |> float) *. 0.5) -. (t1_width *. 0.5))
      (((window_height |> float) *. 0.5) -. (t1_height *. 0.5) +. textbox_height *. 0.75)
      t1_width t1_height
  in
  draw_texture_npatch !box_texture npatch_slab dstrec (V.zero ()) 0. Color.gray;

  draw_text_ex !winner_font result
    (V.create
       ((window_width_f /. 2.) -. (V.x w /. 2.))
       ((window_height_f /. 2.) -. (V.y w /. 2.)))
    h1 font_spacing text_color;
  draw_text_ex !winner_font t1
    (V.create
       ((window_width_f /. 2.) -. (V.x t1w /. 2.) +. 4.)
       ((window_height_f /. 2.) -. (V.y t1w /. 2.) +. textbox_height *. 0.75))
    h2 font_spacing text_color

let draw_cycles c =
  let text = Printf.sprintf "CPU cycle: %6d" c in
  let v = measure_stat_text text in
  draw_stat_text text
    (window_width - (V.x v |> int_of_float) - (arena_padding * 2))
    (window_height
    - (V.y v *. 0.5 |> int_of_float)
    - (bottom_bar_height / 2) - arena_padding)
    text_color

let draw_fps n =
  let text = Printf.sprintf "FPS: %3d" n in
  let v = measure_stat_text text in
  draw_stat_text text (arena_padding * 2)
    (window_height
    - (V.y v *. 0.5 |> int_of_float)
    - (bottom_bar_height / 2) - arena_padding)
    text_color
