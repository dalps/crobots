open Crobots

let background_color = Raylib.Color.raywhite

let font_path = "bin/fonts/monogram.ttf"
let stat_fontsize = 32
let stat_fontsize_f = stat_fontsize |> float_of_int
let winner_fontsize = 80
let font_spacing = 1.

let stat_font = ref (Raylib.get_font_default ())
let winner_font = ref (Raylib.get_font_default ())

let load_fonts () =
  let open Raylib in
  stat_font := load_font_ex font_path stat_fontsize None;
  winner_font := load_font_ex font_path winner_fontsize None;
  gen_texture_mipmaps (addr (Font.texture !stat_font));
  set_texture_filter (Font.texture !stat_font) TextureFilter.Point

let unload_fonts () =
  let open Raylib in
  unload_font !stat_font;
  unload_font !winner_font

let draw_stat_text text pos_x pos_y color =
  let open Raylib in
  draw_text_ex !stat_font text
    (Vector2.create (pos_x |> float_of_int) (pos_y |> float_of_int))
    stat_fontsize_f font_spacing color

let measure_stat_text text =
  let open Raylib in
  measure_text_ex !stat_font text stat_fontsize_f font_spacing

let padding = 15
let stats_width = 400
let arena_width = 1000

let window_height = arena_width + (2 * padding)
let window_width = window_height + padding + stats_width
let window_width_f = window_width |> float_of_int
let window_height_f = window_height |> float_of_int

let statbox_n = 5
let statbox_width = stats_width
let statbox_height = (window_height - ((statbox_n + 1) * padding)) / statbox_n

let stat_height = stat_fontsize
let name_sep = 20

let spr = Printf.sprintf

let draw_arena () =
  let open Raylib in
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
  draw_rectangle_lines padding padding arena_width arena_width Color.gray

let draw_stats i (r : Robot.t) color =
  let pos_x = window_height in
  let pos_y = padding + ((statbox_height + padding) * i) in
  let col_sep = 150 in
  let col_padding = 10 in
  let open Raylib in
  draw_rectangle pos_x pos_y statbox_width statbox_height (fade color 0.25);
  draw_rectangle_lines pos_x pos_y statbox_width statbox_height color;
  draw_stat_text (spr "%d. %s" i r.name) (pos_x + col_padding)
    (pos_y + col_padding) Color.black;
  if r.status = Robot.DEAD then
    draw_stat_text "(dead)"
      (pos_x + statbox_width - 100)
      (pos_y + col_padding) Color.red;
  draw_stat_text
    (spr " x: %d" (r.x / Robot.click))
    (pos_x + col_padding)
    (pos_y + name_sep + stat_height)
    Color.black;
  draw_stat_text
    (spr " y: %d" (r.y / Robot.click))
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + stat_height)
    Color.black;

  draw_stat_text (spr "d%%: %d" r.damage) (pos_x + col_padding)
    (pos_y + name_sep + (stat_height * 2))
    Color.black;
  draw_stat_text
    (spr "sc: %d" r.scan_degrees)
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + (stat_height * 2))
    Color.black;

  draw_stat_text (spr "sp: %d" r.speed) (pos_x + col_padding)
    (pos_y + name_sep + (stat_height * 3))
    Color.black;
  draw_stat_text (spr "hd: %d" r.heading)
    (pos_x + col_padding + col_sep)
    (pos_y + name_sep + (stat_height * 3))
    Color.black

let draw_endgame result =
  let open Raylib in
  let fs = winner_fontsize |> float_of_int in
  let w = measure_text_ex !stat_font result fs font_spacing in
  let p = 20. in
  draw_rectangle 0 0 window_width window_height (fade Color.black 0.35);
  draw_rectangle_v
    (Vector2.create
       ((window_width_f /. 2.) -. (Vector2.x w /. 2.) -. (p /. 2.))
       ((window_height_f /. 2.) -. (Vector2.y w /. 2.) -. (p /. 2.)))
    (Vector2.create (Vector2.x w +. 20.) (Vector2.y w +. 20.))
    (fade Color.white 0.75);
  draw_text_ex !winner_font result
    (Vector2.create
       ((window_width_f /. 2.) -. (Vector2.x w /. 2.))
       ((window_height_f /. 2.) -. (Vector2.y w /. 2.)))
    fs font_spacing Color.black

let draw_cycles c =
  let open Raylib in
  let text = Printf.sprintf "CPU cycle %6d" c in
  let v = measure_stat_text text in
  draw_stat_text text
    (window_width - (Vector2.x v |> int_of_float) - padding)
    (window_height - (Vector2.y v |> int_of_float) - padding)
    Color.black

let draw_fps n =
  let open Raylib in
  let text = Printf.sprintf "FPS %6d" n in
  let v = measure_stat_text text in
  draw_stat_text text
    (window_width - (Vector2.x v |> int_of_float) - padding)
    (window_height - ((Vector2.y v |> int_of_float) * 2) - padding)
    Color.black
