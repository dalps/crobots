/* godel.r

   Sandro Biraghi, via Caio Lucrezio 5, 41012 Carpi (MO), 059-690758


   E' un CRobots basato su due routine di fuoco: fermo() e shoot().
   fermo() e' molto precisa perche' fa fuoco da fermo (appunto...)
   mentre shoot() e' una leggera modifica della routine di fuoco
   presente in Cube nel precedente torneo. Secondo me Cube e' stato il
   vincitore morale del II Torneo di MC quindi ho modificato la sua
   routine migliorandola (speriamo...).
   Va' comunque detto che la routine di cube ha un peso molto modesto
   all'interno del totale perche' nella stragrande maggioranza del tempo
   si rimane fermi.
   Per il resto il CRobottino sta fermo finche' puo' e se proprio deve
   si muove di angolo in angolo in senso antiorario.
   Se non si sente molto dentro allo scontro si va a posizionare in
   centro a cercare compagnia.


						Sandro Biraghi
				     internet:birags@ghost.dsi.unimi.it

*/



int K,k,quadrante,DeltaR,i,orange,range,D,dir,danni,DeltaD,x,y;

main() {

 quadrante=plot(500,500)/90;        /*   32    quadranti, 4 e' il centro  */
				    /*   01                               */
 k=30;x=100;y=100;                  /* k testa i range consecutivi > 800  */
				    /* cioe' se si e' in zona sicura.     */

 while(sicuro()) fermo();           /* finche' dura non ti muovere        */

 if (k) {if (quadrante==0) {x=900;y=900;}    /* genera le coordinate del  */
	  else if (quadrante==1) y=900;      /* primo angolo              */
	    else if (quadrante==3) x=900;
	 drive(D=plot(x,y),80);              /* e ci va sparando          */
	 while(dist(x,y)) shoot();
	 drive(D,0);
	 quadrante=(quadrante+2)%4;
	}
   else quadrante=4;               /* va in centro a cercar fortuna */


 while(1) {         /* "Fino alla fine del mondo" */


   /* i cinque if (quadrante...) sono per i quattro angoli + il centro.    */
   /* Sono molto semplici, se i danni salgono si cambia angolo oppure se   */
   /* tutti gli avversari sono lontani ci si porta in centro (quadrante=4) */

   if (quadrante==0) {sit_danni();    /* se sotto fuoco sceglie l'angolo */
		      while(sicuro()) fermo();  /* in senso antiorario   */
		      if (k) {drive(D=0,80);
			      while(loc_x()<850) shoot();
			      drive(D,0);
			      quadrante=1;
			     }
			else quadrante=4; /* in centro a combattere ! */
		     }
   else if (quadrante==1) {sit_danni();
			   while(sicuro()) fermo();
			   if (k) {drive(D=90,80);
				   while(loc_y()<850) shoot();
				   drive(D,0);
				   quadrante=2;
				  }
			   else quadrante=4;
			  }
     else if (quadrante==2) {sit_danni();
			     while(sicuro()) fermo();
			     if (k) {drive(D=180,80);
				     while(loc_x()>150) shoot();
				     drive(D,0);
				     quadrante=3;
				    }
			       else quadrante=4;
			    }
       else if (quadrante==3) {sit_danni();
			       while(sicuro()) fermo();
			       if (k) {drive(D=270,80);
				       while(loc_y()>150) shoot();
				       drive(D,0);
				       quadrante=0;
				      }
				 else quadrante=4;
			      }
	/* in centro, se i danni salgono troppo tornatene al vecchio  */
	/* angolo e li' rimani (con vergogna...)                      */

	else if (quadrante==4) { sit_danni();
				 while(danni>80) fermo();
				 x=loc_x();y=loc_y();
				 D=plot(500,500);
				 drive(D,80);
				 while(dist(500,500)) shoot();
				 drive(D,0);
				 while(damage()<90
				       && damage()-danni<40) fermo();
				 drive(D+180,80); /* scappa dal centro !!! */
				 while(dist(x,y)) shoot();
				 drive(D+180,0);
				 quadrante=D/90;
			       }
 }

}



fermo ()    /* la routine di fuoco da fermo                         */
	    /* pochi commenti, dopo prova e riprova e' venuta cosi' */
{

    if(orange=scan(dir,10)) {

      if (orange>700) { dir+=19;--k;K=1;return;}

      if (scan(dir,2)) {
       if(scan(dir+=1,1));
	else dir+=359;
       }
      else if(scan(dir+6,4)) dir+=6; else dir+=354;

      if (K) {i=5;while(--i);}

      DeltaR=4;

      if (range=scan(dir,1)) DeltaD=0;

      else if(range=scan(dir+=4,2))
	      DeltaD=7;
      else if(range=scan(dir+=352,2))
	      DeltaD=353;
      else if(range=scan(dir+=12,2)) {
	      DeltaD=8;
	      DeltaR=3;
	      }
      else if(range=scan(dir+=344,2)) {
	      DeltaD=352;
	      DeltaR=3;
	      }
      else if(range=scan(dir+=20,2)) {
	      DeltaD=10;
	      DeltaR=3;
	      }
      else if(range=scan(dir+=336,2)) {
	      DeltaD=350;
	      DeltaR=3;
	      }
      else {dir+=330;K=1;return;}

      K=fuoco();

      k=30;

    }

    else {
     while (!(scan(dir+=19,10)));
     dir%=360;
     }
}


shoot ()  /* la routine di fuoco tratta dallo splendido Cube */
{
	if (!(range = scan (dir, 5))) {
		if (range = scan(dir += 350, 5))
			DeltaD = 354;
		else if (range = scan(dir += 345, 10))
			DeltaD = 350;
		else if (range = scan(dir += 35, 5))
			DeltaD = 6;
		else if (range = scan(dir += 15, 10))
			DeltaD = 10;
		else {
			i = 6;
			while (!(range = scan(dir += 20, 10)) && (--i));
			DeltaD = 0;
			orange = range;
			return;
		}
	}

	orange-=30*cos(D-dir)/100000;

	fuoco();

	orange=range;
}


plot (x, y)     /* data una coordinata cartesiana ritorna la direzione per */
                /* raggingerla dalla posizione attuale */
{
	int     locx, locy, r;

	locx = loc_x();
	locy = loc_y();

	if (locx==x) {
		if (y>locy) return 90;
		   else return 270;
	 } else {

		 r=atan(100000*(locy-y)/(locx-x));

		 if( y < locy) {
		  if (x > locx) return 360 + r;
			   else return 180 + r;
		  }
		  else if (x > locx) return r;
				else return 180 + r;
	       }

}



/* Queste sono routine che raccolgono le parti ripetute in varie zone. */
/* Tutto per risparmiare un po' di spazio.                             */

sicuro()
{
  return (damage()-danni<20 && k);
}

fuoco()
{
 return !(cannon(dir+DeltaD,(815+(range-orange)*DeltaR)*range/800));
}

dist(x,y)
{ int a,b;

   a=x-loc_x();
   b=y-loc_y();
   return ((a*a+b*b)>1000);

}

sit_danni()
{
 danni=damage();
}
