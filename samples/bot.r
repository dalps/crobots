distance(x1, y1, x2, y2)
{
  int x, y, d;

  x = x1 - x2;
  y = y1 - y2;
  d = sqrt((x * x) + (y * y));

  return (d);
}

main()
{
  int x = loc_x();
  int y = loc_y();
  int d = 0;
  while (1)
  {
    drive(180,0);
  };
}