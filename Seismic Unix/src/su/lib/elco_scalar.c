/* segy coordinate and elevation scalar utilities */
#include <math.h>
#include "su.h"

/* what to multiple tr.sx, etc. by when retrieving from segy struct */
double from_segy_elco_multiplier(short scalar)
{
  if(scalar > 0) return (double) scalar;
  if(scalar < 0) return 1.0/scalar;
  return 1.0;
}

/* what to multiple tr.sx, etc. by when storing into segy struct */
double to_segy_elco_multiplier(short scalar)
{
  return 1.0/from_segy_elco_multiplier(scalar);
}

/* returns best power of 10 coordinate/elevation scalar for use
 * in SEG-Y/SU trace headers */
short elco_scalar(int ncoords, double c[])
{
   int i;
   double dtemp, etemp;

     dtemp = 0.0;
     for(i=0; i<ncoords; ++i) {
         etemp = fabs(c[i]);
         dtemp = (etemp > dtemp) ? etemp : dtemp;
     }
     if(dtemp == 0.0) return 1;
     if(dtemp >= 2147483647500.0) return 10000;
     if(dtemp >= 214748364750.00) return 1000;
     if(dtemp >= 21474836475.000) return 100;
     if(dtemp >= 2147483647.5000) return 10;
     if(dtemp >= 214748364.75000) return 1;
     if(dtemp >= 21474836.475000) return -10;
     if(dtemp >= 2147483.6475000) return -100;
     if(dtemp >= 214748.36475000) return -1000;
     return -10000;
}

