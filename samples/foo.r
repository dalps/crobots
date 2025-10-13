int i;

main()
{
  i = 3;
  foo(270);
  return i;
}

foo(deg)
{
  int i = 0;
  while (i < deg)
    i = i + 1;
}