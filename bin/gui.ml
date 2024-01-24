open Crobots
open Raylib
open Defs

let text_color = Color.raywhite
let background_color = Color.create 58 68 102 255
let arena_bg_color = Color.create 10 10 10 255
let lines_color = Color.create 30 30 30 255
let bullet_color = Color.white
let skull_color = fade Color.red 0.75

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

let draw_arena () =
  draw_rectangle (padding - 10) (padding - 10) (arena_width + 20)
    (arena_width + 20) arena_bg_color;
  draw_texture_npatch !box_texture npatch_arena dstrec_arena (V.zero ()) 0.
    Color.white;
  draw_rectangle_lines padding padding arena_width arena_width lines_color;
  draw_line padding
    (padding + (arena_width / 2))
    (padding + arena_width)
    (padding + (arena_width / 2))
    lines_color;
  draw_line
    (padding + (arena_width / 2))
    padding
    (padding + (arena_width / 2))
    (padding + arena_width) lines_color

let draw_stats i (r : Robot.t) _ =
  let spr = Printf.sprintf in
  let pos_x = arena_width + (padding * 2) in
  let pos_y = arena_padding + ((statbox_height + arena_padding) * i) in
  let pos_x_f, pos_y_f = (pos_x |> float, pos_y |> float) in
  let padding_x = 32 in

  let title_text = spr "%d.%s" i r.name in
  let wh = measure_stat_text title_text in
  let wp = measure_stat_text "AA: 000" in
  let dstrec =
    R.create pos_x_f pos_y_f (statbox_width |> float) (statbox_height |> float)
  in
  draw_texture_npatch !box_texture npatch_stat dstrec (V.zero ()) 0. Color.white;

  let avatar_size = 72 in

  let dstrec =
    R.create pos_x_f pos_y_f (statbox_width |> float) (avatar_size |> float)
  in
  draw_texture_npatch !box_texture npatch_slab dstrec (V.zero ()) 0. Color.white;

  (* let dstrec =
    R.create pos_x_f pos_y_f (avatar_size |> float) (avatar_size |> float)
  in
  draw_texture_npatch !box_texture npatch_avatar dstrec (V.zero ()) 0.
    Color.white; *)

  Sprite.(
    let c = sprites.(i).color in
    draw
      (create
         (pos_x_f +. ((avatar_size |> float) *. 0.5))
         (pos_y_f +. ((avatar_size |> float) *. 0.5))
         c)
      0. 0. c c);

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
           (pos_x_f +. ((avatar_size |> float) *. 0.5) -. (skull_width *. 0.5))
           (pos_y_f +. ((avatar_size |> float) *. 0.5) -. (skull_width *. 0.5))
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
      (((window_height |> float) *. 0.5)
      -. (t1_height *. 0.5) +. (textbox_height *. 0.75))
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
       ((window_height_f /. 2.) -. (V.y t1w /. 2.) +. (textbox_height *. 0.75)))
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
