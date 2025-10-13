int angle;
int range; /* variabili esterne */

main() /* Un piccolo robot che segue il bersaglio cannoneggiandolo */
{
  angle = 0;

  while (1)
  {
    drive(angle, 49);           /* velocita' massima per cambiare direzione */
    shoot();                    /* funzione esterna	*/
    angle = (angle + 85) % 360; /* scanning nei quattro quadranti */
  }
} /* end of main */

shoot() /* funzione esterna */
{
  while (range = scan(angle, 10)) /* esegui una scansione finche' il */
  {                               /* bersaglio non e' individuato... */
    cannon(angle, range);         /* e poi spara */
  }
}