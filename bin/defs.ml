open Crobots
open Raylib

module V = Vector2
module R = Rectangle
module PS = Particles.ParticleSystem

(* *** Dimensions *** *)

let window_width = 1200
let window_height = window_width - 300

let arena_padding = 15
let arena_texture_width = 96
let arena_w = arena_texture_width |> float
let bottom_bar_width = window_width - (arena_padding * 2)
let bottom_bar_height = 50
let arena_border_thickness = arena_texture_width / 3
let padding = arena_padding + arena_border_thickness
let arena_width =
  window_height - (2 * padding) - (bottom_bar_height + arena_padding)

let ratio = (arena_width |> float) /. Robot._max_x

let robot_width = Robot._robot_size *. ratio
let tank_width = robot_width
let turret_width = tank_width *. 0.7
let missile_width = robot_width *. 0.15
let missile_height = missile_width
let scan_height = 70.

let max_frame_speed = 60.
let max_trail_speed = 15.

let trail_height = 6.

let colors = Raylib.[| Color.blue; Color.brown; Color.darkgreen; Color.pink |]

let get_screen_x x =
  let robot_x = x in
  (ratio *. robot_x) +. (padding |> float)

let get_screen_y y =
  let robot_y = y in
  (-1. *. ratio *. robot_y) +. (arena_width |> float) +. (padding |> float)

let get_screen_degrees d = -.d +. 270.

(* *** Textures and fonts *** *)

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

let get_srcrec texture =
  R.create 0. 0.
    (Texture.width texture |> float)
    (Texture.height texture |> float)

(* *** Text utilities *** *)

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
