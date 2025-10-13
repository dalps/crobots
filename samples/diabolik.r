/****************************************************************************/
/*                                                                          */
/*  VII Torneo di CRobots di MCmicrocomputer (1997)                         */
/*                                                                          */
/*  CROBOT: DIABOLIK.R                                                      */
/*                                                                          */
/*  AUTORE: Daniele Nuzzo (7/4/1973)                                        */
/*          via Alfonso Borelli nř7                                         */
/*          00161 ROMA                                                      */
/*          tel. 06/4957637                                                 */
/*                                                                          */
/****************************************************************************/

/* Nel caso si rendesse necessario limitare i combattimenti ad un solo robot
   per concorrente preferisco veder combattere l'altro crobot (ABYSS.R).    */

/*               
                          SCHEDA TECNICA:

  Inizialmente il robot si reca nell'angolo in alto a sinistra dell'arena;
  quindi guarda in direzione dei due angoli adiacenti e si comporta nel
  seguente modo:
    - se in basso non c'Š nessuno, allora si dirige verso sud lungo il
      bordo dell'arena fino a raggiungere circa la met… del lato e quindi
      torna indietro;
    - altrimenti se a destra non c'Š nessuno, allora si dirige verso est
      lungo il bordo dell'arena fino a raggiungere circa la met… del lato
      e quindi torna indietro;
    - altrimenti sceglie come direzione quella in cui avvista il nemico
      pi— vicino.
  Circa a met… del match e poco prima della fine controlla se Š rimasto un
  solo avversario; in tal caso, dopo aver controllato i propri danni decide
  se attaccarlo (se non attacca continua il suo movimento oscillatorio lungo
  il perimetro di gioco).
  La routine di attacco prevede un movimento a forma di rombo i cui vertici
  sono le met… dei quattro lati dell'arena; in questo modo riesce a coprire
  tutto il campo di battaglia in poco tempo e rende la vita difficile ai
  nemici che si nascondono negli angoli.
  Vengono utilizzate due routine di fuoco, entrambe in movimento (il robot
  non st… mai fermo!!!): una Š derivata da quella di tox.r; mentre l'altra
  Š molto pi— semplice, ma efficace contro i robot che utilizzano
  oscillazioni brevi. La scelta di quale utilizzare Š legata al controllo
  del proprio movimento ed alla distanza dell'avversario.

  NOTE:  - il sorgente Š straottimizzato (e quindi anche abbastanza difficile
           da comprendere); Š un vero peccato che la limitazione della
           lunghezza del codice (1000 istruzioni macchina???) sia cosŤ
           restrittiva e vincolante. In tal modo la creativit… degli autori
           dei robot e la stragegia di gioco si riducono veramente all'osso!
           Credo che se non si far… nulla per ovviare a questo handicap, il
           gioco purtroppo sar… destinato a tramontare entro breve tempo.
           La sua longevit… Š stata una delle caratteristiche pi— gradite,
           ma analizzando la qualit… dei crobot dei vari tornei, risulta
           evidente che anno dopo anno si sono creati robot sempre molto
           pi— competitivi, ma anche con sorgenti pi— lunghi!!!
           Un'altra cosa da correggere sarebbe il bug sulla scansione in
           direzione 0: falsa il risultato di molti combattimenti e rende
           inadeguate diverse strategie di gioco!

*/

int deg,rng,orng,dir,x,y,t,rr,aa,odeg,f;

main()
{
    drive(dir=180,100);
    while(loc_x()>100) fuoco2();
    drive(0,0);
    while(speed()>49);
    drive(dir=90,100);
    while(loc_y()<900) fuoco2();
    drive(270,0);

    t=100;
    while (--t)
    {
        orng=scan(270,10);
        rng=scan(0,10);
        drive(135,0);

        if (orng<=rng)
        {
            dir=270;
            while(loc_y()>600) { drive(270,100);
                                 if (orng<500) fuoco2(); else fuoco(); }
            drive(dir=90,0);
            while(speed()>49);
            while(loc_y()<800) { drive(90,100);
                                 if (orng<500) fuoco2(); else fuoco(); }
            while(loc_y()<880) { drive(90,100); fuoco2(); }
        }
        else
        {
            dir=0;
            while(loc_x()<400) { drive(0,100);
                                 if (orng<500) fuoco2(); else fuoco(); }
            drive(dir=180,0);
            while(speed()>49);
            while(loc_x()>200) { drive(180,100);
                                 if (orng<500) fuoco2(); else fuoco(); }
            while(loc_x()>120) { drive(180,100); fuoco2(); }
        }
        if (t==50) { NAvv(); if (f<2) if (damage()<60) Attacca(); }
    }
    NAvv();
    if (f<2) Attacca();
}

NAvv()
{
    drive(f=0,0);
    if (scan(275,10)) f+=1;
    if (scan(295,10)) f+=1;
    if (scan(315,10)) f+=1;
    if (scan(335,10)) f+=1;
    if (scan(355,10)) f+=1;
}

Attacca()
{
    while(damage()<90)
    {
        Go(500,900);
        Go(100,500);
        Go(500,100);
        Go(900,500);
    }
}

Go(x1,y1)
{
    int curx,nx,ny,dist;

    /* Calcola la direzione per recarsi al punto stabilito: */    
    nx=(curx=loc_x())-x1;
    ny=(loc_y()-y1)*100000;
    if (x1>curx) dir=360+atan(ny/nx);
           else dir=180+atan(ny/nx);

    /* Si reca nel punto stabilito: */    
    
    dist=35001;
    drive(dir,0);
    while(dist>35000)
    {    
        drive(dir,100);
        f=!f;
        if (f) fuoco(); else fuoco2();
        nx=x1-loc_x();
        ny=y1-loc_y();
        dist=(nx*nx)+(ny*ny);
    }
}

fuoco()
{
     if (!scan(deg,5))
        if (!scan(deg-=10,5))
           if (!scan(deg+=20,5))
              if (!scan(deg-=30,5))
                 if (!scan(deg+=40,5)) { deg+=40; return; }

     if (scan(deg,5)>700) { deg+=40; return; }

       if(scan(deg-5,1)) deg-=5;
       if(scan(deg+5,1)) deg+=5;
       if(scan(deg-3,1)) deg-=3;
       if(scan(deg+3,1)) deg+=3;
       if(scan(deg-1,1)) deg-=1;
       if(scan(deg+1,1)) deg+=1;
       if (rng=scan(deg,5))
         {
           orng=rng;
           odeg=deg;
           if(scan(deg-5,1)) deg-=5;
           if(scan(deg+5,1)) deg+=5;
           if(scan(deg-3,1)) deg-=3;
           if(scan(deg+3,1)) deg+=3;
           if(scan(deg-1,1)) deg-=1;
           if(scan(deg+1,1)) deg+=1;
             if (rng=scan(deg,10))
               {
                 x=(deg+(deg-odeg)*((1200+rng)>>9)-(sin(deg-dir)>>14));
                 y=(rng*160/(160+orng-rng-(cos(deg-dir)>>12)));
                 cannon(x,y);
               }
         }
}

fuoco2()
{
    if (orng=scan(deg,10)) ;
    else if (orng=scan(deg-=20,10)) ;
    else if (orng=scan(deg+=40,10)) ;
    else { deg+=40; return; }

    if (!scan(deg+=5,5)) deg-=10;
    if (!scan(deg+=3,3)) deg-=6;
    if (!scan(deg+=2,1)) deg-=4;

    if (rng=scan(deg,10)) { cannon(deg,2*rng-orng); }
    if (orng>700) deg+=40;
}
