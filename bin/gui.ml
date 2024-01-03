open Crobots

let font_path = "bin/fonts/monogram.ttf"
let stat_fontsize = 32
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
    (stat_fontsize |> float_of_int)
    font_spacing color

let padding = 10
let stats_width = 400
let arena_width = 1000

let window_height = arena_width + (2 * padding)
let window_width = window_height + padding + stats_width

let statbox_n = 4
let statbox_width = stats_width
let statbox_height = (window_height - ((statbox_n + 1) * padding)) / statbox_n

let stat_height = 30
let name_sep = 20

let spr = Printf.sprintf

let draw_stats i (r : Robot.t) color =
  let pos_x = window_height in
  let pos_y = padding + ((statbox_height + padding) * i) in
  let open Raylib in
  draw_rectangle pos_x pos_y statbox_width statbox_height (fade color 0.25);
  draw_rectangle_lines pos_x pos_y statbox_width statbox_height color;
  draw_stat_text (spr "%d. %s" i r.name) (pos_x + 5) (pos_y + 5) Color.black;
  if r.status = Robot.DEAD then
    draw_stat_text "(dead)" (pos_x + 200) (pos_y + 5) Color.red;
  draw_stat_text (spr "d%%: %d" r.damage) (pos_x + 5)
    (pos_y + name_sep + stat_height)
    Color.black;
  draw_stat_text (spr "sp: %d" r.speed) (pos_x + 5)
    (pos_y + name_sep + (stat_height * 2))
    Color.black;
  draw_stat_text (spr "hd: %d" r.heading) (pos_x + 5)
    (pos_y + name_sep + (stat_height * 3))
    Color.black;
  draw_stat_text
    (spr "sc: %d" r.scan_degrees)
    (pos_x + 5)
    (pos_y + name_sep + (stat_height * 4))
    Color.black;
  draw_stat_text
    (spr "x: %d" (r.x / Robot.click))
    (pos_x + 5 + 100)
    (pos_y + name_sep + stat_height)
    Color.black;
  draw_stat_text
    (spr "y: %d" (r.y / Robot.click))
    (pos_x + 5 + 100)
    (pos_y + name_sep + (stat_height * 2))
    Color.black;
  draw_stat_text (spr "rl: %d" r.reload)
    (pos_x + 5 + 100)
    (pos_y + name_sep + (stat_height * 3))
    Color.black;
  Array.iteri
    (fun i (m : Missile.t) ->
      draw_stat_text
        (spr "m%d: %s %d, %d r:%d/%d" i
           (Missile.string_of_status m.status)
           (m.cur_x / Robot.click) (m.cur_y / Robot.click) m.travelled m.range)
        (pos_x + 5 + 100)
        (pos_y + name_sep + (stat_height * (4 + i)))
        Color.black)
    r.missiles

let draw_endgame result =
  let open Raylib in
  let fs = winner_fontsize |> float_of_int in
  let w = measure_text_ex !stat_font result fs font_spacing in
  draw_text_ex !winner_font result
    (Vector2.create
       (((window_width |> float_of_int) /. 2.) -. (Vector2.x w /. 2.))
       (((window_height |> float_of_int) /. 2.) -. (fs /. 2.)))
    fs font_spacing Color.gray

let draw_cycles c =
  let open Raylib in
  draw_stat_text
    (Printf.sprintf "CYCLES %d" c)
    (padding + 5 + 100)
    (padding + 5) Color.black

let draw_fps n =
  let open Raylib in
  draw_stat_text
    (Printf.sprintf "FPS %d" n)
    (padding + 5) (padding + 5) Color.black
