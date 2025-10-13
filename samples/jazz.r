
/**********************************************************************
 *  == Jazz ==  CRobot by Giuseppe Menozzi (Wheeling Pitch) 10/9/91.  *
 *              Questo semplice CRobot corre sulla diagonale princi-  *
 *              pale, sparando con poca precisione. Aiuta in maniera  *
 *              sorprendente il tiro usando un Range leggerissima-    *
 *              mente piu' corto di quello fornito da scan().         *
 **********************************************************************/


int	Dir, Range, DirR;

main()
{
	DirR = xy2dir (100, 100);

	while (1) {
		drive (DirR, 100);
		while (loc_x() > 90 && speed()) {
			if (Range = scan (Dir, 3))
				cannon (Dir, 7 * Range / 8);
			else
			 {
				Dir -= 23;
				while (!(Range = scan (Dir, 10))) 
					Dir += 20;
				if (Range < 60) 
					Range = 60;
				cannon (Dir, 7 * Range / 8);
			};
		};
		DirR = xy2dir (899, 899);
		while (speed() > 50) 
			drive (DirR, 50);

		drive (DirR, 100);
		while (loc_x() < 910 && speed()) {
			if (Range = scan (Dir, 3))
				cannon (Dir, 7 * Range / 8);
			else
			 {
				Dir -= 23;
				while (!(Range = scan (Dir, 10))) 
					Dir += 20;
				if (Range < 60) 
					Range = 60;
				cannon (Dir, 7 * Range / 8);
			};
		};
		DirR = xy2dir (100, 100);
		while (speed() > 50) 
			drive (DirR, 50);
	};
}


xy2dir (x, y)
{
	int	d, locx, locy;

	locx = loc_x();
	locy = loc_y();

	if (locx == x) {
		if (y > locy)
			d = 90;
		else
			d = 270;
	} else
	 {
		if (y < locy) {
			if (x > locx)
				d = 360 + atan ((100000 * (locy - y)) / (locx - x) );
			else
				d = 180 + atan ((100000 * (locy - y)) / (locx - x) );
		} else if (x > locx)
			d = atan ((100000 * (locy - y)) / (locx - x) );
		else
			d = 180 + atan ((100000 * (locy - y)) / (locx - x) );
	};
	return (d);
}