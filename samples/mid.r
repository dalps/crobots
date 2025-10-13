int dir;

main()
{
  while (1)
  {
    if (loc_x() < 350 && loc_x() > 250)
    {
      drive(0, 0);
    }
    else
    {
      vai(500, 500, 100);
    }
  }
}

vai(xx, yy, vel)
{
  drive(dir = 180 + 180 * (xx > loc_x()) + atan(100000 * (loc_y() - yy) / (loc_x() - xx)), vel);
}