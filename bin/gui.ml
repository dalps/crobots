open Crobots

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
  draw_text (spr "%d. %s" i r.name) (pos_x + 5) (pos_y + 5) 20 Color.black;
  if r.status = Robot.DEAD then
    draw_text "(dead)" (pos_x + 200) (pos_y + 5) 20 Color.red;
  draw_text (spr "d%%: %d" r.damage) (pos_x + 5)
    (pos_y + name_sep + stat_height)
    20 Color.black;
  draw_text (spr "sp: %d" r.speed) (pos_x + 5)
    (pos_y + name_sep + (stat_height * 2))
    20 Color.black;
  draw_text (spr "hd: %d" r.heading) (pos_x + 5)
    (pos_y + name_sep + (stat_height * 3))
    20 Color.black;
  draw_text
    (spr "sc: %d" r.scan_degrees)
    (pos_x + 5)
    (pos_y + name_sep + (stat_height * 4))
    20 Color.black;
  draw_text
    (spr "x: %d" (r.x / Robot.click))
    (pos_x + 5 + 100)
    (pos_y + name_sep + stat_height)
    20 Color.black;
  draw_text
    (spr "y: %d" (r.y / Robot.click))
    (pos_x + 5 + 100)
    (pos_y + name_sep + (stat_height * 2))
    20 Color.black;
  draw_text (spr "rl: %d" r.reload)
    (pos_x + 5 + 100)
    (pos_y + name_sep + (stat_height * 3))
    20 Color.black;
  Array.iteri
    (fun i (m : Missile.t) ->
      draw_text
        (spr "m%d: %s %d, %d r:%d/%d" i
           (Missile.string_of_status m.status)
           (m.cur_x / Robot.click) (m.cur_y / Robot.click) m.travelled m.range)
        (pos_x + 5 + 100)
        (pos_y + name_sep + (stat_height * (4 + i)))
        20 Color.black)
    r.missiles
