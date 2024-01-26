/* rook.r  -  scans the battlefield like a rook, i.e., only 0,90,180,270 */
/* move horizontally only, but looks horz and vertically */

int course;
int boundary;
int d;

turn(angle) {
  drive(angle, 0);

  while (heading() != angle)
    ;
}

accel(angle, sp) {
  drive(angle, sp);

  while (speed() < 5)
    ;
}

main()
{
  int y;

  /* move to center of board */
  if (loc_y() < 500) {
    accel(90,70);                              /* start moving */
    while (loc_y() - 500 < 20 && speed() > 0)  /* stop near center */
      ;
  } else {
    accel(270,70);                             /* start moving */
    while (loc_y() - 500 > 20 && speed() > 0)  /* stop near center */
      ;
  }
  drive(y,0);

  /* initialize starting parameters */
  d = damage();
  course = 0;
  boundary = 960;
  drive(course,30);

  /* main loop */

  while(1) {

    /* look all directions */
    look(0);
    look(90);
    look(180);
    look(270);

    /* if near end of battlefield, change directions */
    if (course == 0) {
      if (loc_x() > boundary || speed() == 0)
        change();
    }
    else {
      if (loc_x() < boundary || speed() == 0)
        change();
    }
  }

}

/* look somewhere, and fire cannon repeatedly at in-range target */
look(deg)
{
  int range;

  while ((range=scan(deg,2)) > 0 && range <= 700)  {
    drive(course,0);
    cannon(deg,range);
    if (d+20 != damage()) {
      d = damage();
      change();
    }
  }
}


change() {
  if (course == 0) {
    boundary = 60;
    course = 180;
  } else {
    boundary = 960;
    course = 0;
  }
  turn(course);
  accel(course,100);
}


/* end of rook.r */
