/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*                                                              */
/*  Function RTu()                                              */
/*                                                              */
/*  Computation of the reflection and transmission coefficients */
/*  of upgoing incident waves                                   */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */ 
/*                         global variable                      */
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffU.................Reflection and transmition           */
/*                         coefficients                         */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
#include "posteriori.h"
void RTu(int i1, int i2)
{
   /* declaration of variables */
   float rho1rho2;                 /* rho1 * rho2 */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux2, aux1aux3;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1u, d2u;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */
   
   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i1][0].r - uuC.r;
   auxm2 = PSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1][0].r - uuC.r;
   auxm2 = SSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2][0].r - uuC.r;
   auxm2 = PSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2][0].r - uuC.r;
   auxm2 = SSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1][0].r - rho[i2] * S2Velocity[i2][0].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1][0].i - rho[i2] * S2Velocity[i2][0].i);

   cuu.r = c.r * uuC.r - c.i * uuC.i;
   cuu.i = c.r * uuC.i + c.i * uuC.r;

   aux1.r = cuu.r - (rho[i1] - rho[i2]);
   aux1.i = cuu.i;
   
   aux2.r = cuu.r + rho[i2];
   aux2.i = cuu.i;

   aux3.r = cuu.r - rho[i1];
   aux3.i = cuu.i;

   caux2.r = c.r * aux2.r - c.i * aux2.i;
   caux2.i = c.r * aux2.i + c.i * aux2.r;

   aux1aux3.r = aux1.r * aux3.r - aux1.i * aux3.i;
   aux1aux3.i = aux1.r * aux3.i + aux1.i * aux3.r;

   a1b1.r = a1.r * b1.r - a1.i * b1.i;
   a1b1.i = a1.r * b1.i + a1.i * b1.r;
   a1b2.r = a1.r * b2.r - a1.i * b2.i;
   a1b2.i = a1.r * b2.i + a1.i * b2.r;
   a2b1.r = a2.r * b1.r - a2.i * b1.i;
   a2b1.i = a2.r * b1.i + a2.i * b1.r;
   a2b2.r = a2.r * b2.r - a2.i * b2.i;
   a2b2.i = a2.r * b2.i + a2.i * b2.r;
   a1a2b1b2.r = a1b1.r * a2b2.r - a1b1.i * a2b2.i;
   a1a2b1b2.i = a1b1.r * a2b2.i + a1b1.i * a2b2.r;

   /* computing D factors */
   auxm1 = aux2.r * aux2.r - aux2.i * aux2.i;
   auxm2 = 2 * aux2.r * aux2.i;
   auxm3 = a1b1.r * auxm1 - a1b1.i * auxm2;
   auxm4 = a1b1.r * auxm2 + a1b1.i * auxm1;
   d1u.r = auxm3 + a1b2.r * rho1rho2;
   d1u.i = auxm4 + a1b2.i * rho1rho2;

   auxm1 = aux1.r * aux1.r - aux1.i * aux1.i;
   auxm2 = 2 * aux1.r * aux1.i;
   auxm3 = auxm1 * uuC.r - auxm2 * uuC.i;
   auxm4 = auxm1 * uuC.i + auxm2 * uuC.r;
   d1u.r += auxm3;
   d1u.i += auxm4;

   auxm1 = aux3.r * aux3.r - aux3.i * aux3.i;
   auxm2 = 2 * aux3.r * aux3.i;
   auxm3 = a2b2.r * auxm1 - a2b2.i * auxm2;
   auxm4 = a2b2.r * auxm2 + a2b2.i * auxm1;
   d2u.r = auxm3 + a2b1.r * rho1rho2;
   d2u.i = auxm4 + a2b1.i * rho1rho2;

   auxm1 = c.r * cuu.r - c.i * cuu.i;
   auxm2 = c.r * cuu.i + c.i * cuu.r;
   auxm3 = a1a2b1b2.r * auxm1 - a1a2b1b2.i * auxm2;
   auxm4 = a1a2b1b2.r * auxm2 + a1a2b1b2.i * auxm1;
   d2u.r += auxm3;
   d2u.i += auxm4;

   /* more auxiliar quantities */
   /* d1u + d2u */
   dd.r = d1u.r + d2u.r;
   dd.i = d1u.i + d2u.i;

   /* 1 / dd */
   aux = dd.r * dd.r + dd.i * dd.i;
   dd.r = dd.r / aux;
   dd.i = -dd.i / aux;

   dpda.r = a2.r * dd.r - a2.i * dd.i;
   dpda.i = a2.r * dd.i + a2.i * dd.r;
   dpdb.r = b2.r * dd.r - b2.i * dd.i;
   dpdb.i = b2.r * dd.i + b2.i * dd.r;

   /* computing the coefficients - first reflection */
   auxm1 = d2u.r - d1u.r;
   auxm2 = d2u.i - d1u.i;
   /* (d2u - d1u) / (d1u + d2u) */
   coeffU[0].r = auxm1 * dd.r - auxm2 * dd.i;
   coeffU[0].i = auxm1 * dd.i + auxm2 * dd.r;                 /* Rpp */

   auxm1 = a1b1.r * caux2.r - a1b1.i * caux2.i;
   auxm2 = a1b1.r * caux2.i + a1b1.i * caux2.r;
   coeffU[1].r = aux1aux3.r + auxm1;
   coeffU[1].i = aux1aux3.i + auxm2;                          /* Rsp */  
								 
   coeffU[2].r = coeffU[1].r;
   coeffU[2].i = coeffU[1].i;                                 /* Rps*/  

   auxm3 = -dpdb.r * uC2.r + dpdb.i * uC2.i;
   auxm4 = -dpdb.r * uC2.i - dpdb.i * uC2.r;
   
   aux = auxm3 * coeffU[1].r - auxm4 * coeffU[1].i;
   coeffU[1].i = auxm3 * coeffU[1].i + auxm4 * coeffU[1].r;
   coeffU[1].r = aux;                                         /* Rsp */

   auxm3 = dpda.r * uC2.r - dpda.i * uC2.i;
   auxm4 = dpda.r * uC2.i + dpda.i * uC2.r;
   
   aux = auxm3 * coeffU[2].r - auxm4 * coeffU[2].i;
   coeffU[2].i = auxm3 * coeffU[2].i + auxm4 * coeffU[2].r;
   coeffU[2].r = aux;                                         /* Rps */

   auxm1 = d2u.r - d1u.r;
   auxm2 = d2u.i - d1u.i;
   auxm3 = 2 * rho1rho2 * (a2b1.r - a1b2.r);
   auxm4 = 2 * rho1rho2 * (a2b1.i - a1b2.i);

   coeffU[3].r = auxm1 - auxm3;
   coeffU[3].i = auxm2 - auxm4;
   aux = coeffU[3].r * dd.r - coeffU[3].i * dd.i;
   coeffU[3].i = coeffU[3].r * dd.i + coeffU[3].i * dd.r;
   coeffU[3].r = aux;                                         /* Rss */

   /* now transmition */
   auxm1 = b1.r * aux2.r - b1.i * aux2.i;
   auxm2 = b1.r * aux2.i + b1.i * aux2.r;
   auxm3 = b2.r * aux3.r - b2.i * aux3.i;
   auxm4 = b2.r * aux3.i + b2.i * aux3.r;
   coeffU[4].r = auxm1 - auxm3;     
   coeffU[4].i = auxm2 - auxm4;
   aux = 2 * rho[i2] * (dpda.r * coeffU[4].r - dpda.i * coeffU[4].i);
   coeffU[4].i = 2 * rho[i2] * (dpda.r * coeffU[4].i + dpda.i * coeffU[4].r);
   coeffU[4].r = aux;                                         /* Tpp */

   coeffU[5].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
   coeffU[5].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
   auxm1 = rho[i2] * (dpdb.r * uC2.r - dpdb.i * uC2.i);
   auxm2 = rho[i2] * (dpdb.r * uC2.i + dpdb.i * uC2.r);
   aux = coeffU[5].r * auxm1 - coeffU[5].i * auxm2;
   coeffU[5].i = coeffU[5].r * auxm2 + coeffU[5].i * auxm1;   /* Tsp */
   coeffU[5].r = aux; 

   coeffU[6].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
   coeffU[6].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
   auxm1 = -rho[i2] * (dpda.r * uC2.r - dpda.i * uC2.i);
   auxm2 = -rho[i2] * (dpda.r * uC2.i + dpda.i * uC2.r);
   aux = coeffU[6].r * auxm1 - coeffU[6].i * auxm2;
   coeffU[6].i = coeffU[6].r * auxm2 + coeffU[6].i * auxm1;   /* Tsp */
   coeffU[6].r = aux;

   auxm1 = a1.r * aux2.r - a1.i * aux2.i;
   auxm2 = a1.r * aux2.i + a1.i * aux2.r;
   coeffU[7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
   coeffU[7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
   auxm3 = 2 * rho[i2] * dpdb.r;
   auxm4 = 2 * rho[i2] * dpdb.i;
   aux = coeffU[7].r * auxm3 - coeffU[7].i * auxm4;
   coeffU[7].i = coeffU[7].r * auxm4 + coeffU[7].i * auxm3;   /* Tss */
   coeffU[7].r = aux;
}
/*                                                              */
/*  Function RTd()                                              */
/*                                                              */
/*  Computation of the reflection and transmission coefficients */
/*  of downgoing incident waves                                 */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */ 
/*                         global variable                      */
/*  i1.....................points to the first layer            */
/*  i2.....................points to the second layer           */
/*                                                              */
/*  Output parameters:                                          */
/*  coeffD.................Reflection and transmition           */
/*                         coefficients                         */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void RTd(int i1, int i2)
{ 
  /* declaration of variables */

   float rho1rho2;                 /* rho1 * rho2 */
   complex aux1, aux2, aux3, c;    
   complex cuu, caux3, aux1aux2;   /* auxiliar quantities */
   complex a1, a2, b1, b2;         /* all vertical slownesses */
   complex a1b1, a1b2, a2b1, a2b2; /* auxiliar quantities */
   complex a1a2b1b2;               /* auxiliar quantities */
   complex d1d, d2d;               /* auxiliar quantities */
   complex dpda, dpdb;             /* auxiliar quantities */

   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[i1][0].r - uuC.r;
   auxm2 = PSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a1.r = auxm3 * cos(angle);
   a1.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i1][0].r - uuC.r;
   auxm2 = SSlowness[i1][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b1.r = auxm3 * cos(angle);
   b1.i = auxm3 * sin(angle);

   /* square-root of PSlowness^2 - uuC */    
   auxm1 = PSlowness[i2][0].r - uuC.r;
   auxm2 = PSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   a2.r = auxm3 * cos(angle);
   a2.i = auxm3 * sin(angle);

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[i2][0].r - uuC.r;
   auxm2 = SSlowness[i2][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   b2.r = auxm3 * cos(angle);
   b2.i = auxm3 * sin(angle);

   /* computing auxiliary quantities */
   rho1rho2 = rho[i1] * rho[i2];
   c.r = 2 * (rho[i1] * S2Velocity[i1][0].r - rho[i2] * S2Velocity[i2][0].r);
   c.i = 2 * (rho[i1] * S2Velocity[i1][0].i - rho[i2] * S2Velocity[i2][0].i);

   cuu.r = c.r * uuC.r - c.i * uuC.i;
   cuu.i = c.r * uuC.i + c.i * uuC.r;
   
   aux1.r = cuu.r - (rho[i1] - rho[i2]);
   aux1.i = cuu.i;

   aux2.r = cuu.r + rho[i2];
   aux2.i = cuu.i;
   
   aux1aux2.r = aux1.r * aux2.r - aux1.i * aux2.i;
   aux1aux2.i = aux1.r * aux2.i + aux1.i * aux2.r;

   aux3.r = cuu.r - rho[i1];
   aux3.i = cuu.i;

   caux3.r = c.r * aux3.r - c.i * aux3.i;
   caux3.i = c.r * aux3.i + c.i * aux3.r;
   
   a1b1.r = a1.r * b1.r - a1.i * b1.i;
   a1b1.i = a1.r * b1.i + a1.i * b1.r;
   a1b2.r = a1.r * b2.r - a1.i * b2.i;
   a1b2.i = a1.r * b2.i + a1.i * b2.r;
   a2b1.r = a2.r * b1.r - a2.i * b1.i;
   a2b1.i = a2.r * b1.i + a2.i * b1.r;
   a2b2.r = a2.r * b2.r - a2.i * b2.i;
   a2b2.i = a2.r * b2.i + a2.i * b2.r;
   a1a2b1b2.r = a1b1.r * a2b2.r - a1b1.i * a2b2.i;
   a1a2b1b2.i = a1b1.r * a2b2.i + a1b1.i * a2b2.r;
   
   /* computing D factors */   
   auxm1 = aux3.r * aux3.r - aux3.i * aux3.i;
   auxm2 = 2 * aux3.r * aux3.i;
   auxm3 = a2b2.r * auxm1 - a2b2.i * auxm2;
   auxm4 = a2b2.r * auxm2 + a2b2.i * auxm1;
   d1d.r = auxm3 + a2b1.r * rho1rho2;
   d1d.i = auxm4 + a2b1.i * rho1rho2;
   
   auxm1 = aux1.r * aux1.r - aux1.i * aux1.i;
   auxm2 = 2 * aux1.r * aux1.i;
   auxm3 = auxm1 * uuC.r - auxm2 * uuC.i;
   auxm4 = auxm1 * uuC.i + auxm2 * uuC.r;
   d1d.r += auxm3;
   d1d.i += auxm4;
   
   auxm1 = aux2.r * aux2.r - aux2.i * aux2.i;
   auxm2 = 2 * aux2.r * aux2.i;
   auxm3 = a1b1.r * auxm1 - a1b1.i * auxm2;
   auxm4 = a1b1.r * auxm2 + a1b1.i * auxm1;
   d2d.r = auxm3 + a1b2.r * rho1rho2;
   d2d.i = auxm4 + a1b2.i * rho1rho2;

   auxm1 = c.r * cuu.r - c.i * cuu.i;
   auxm2 = c.r * cuu.i + c.i * cuu.r;
   auxm3 = a1a2b1b2.r * auxm1 - a1a2b1b2.i * auxm2;
   auxm4 = a1a2b1b2.r * auxm2 + a1a2b1b2.i * auxm1;
   d2d.r += auxm3;
   d2d.i += auxm4;
   
   /* more auxiliar quantities */
   /* d1d + d2d */
   dd.r = d1d.r + d2d.r;
   dd.i = d1d.i + d2d.i;

   /* 1 / dd */
   aux = dd.r * dd.r + dd.i * dd.i;
   dd.r = dd.r / aux;
   dd.i = -dd.i / aux;

   dpda.r = a1.r * dd.r - a1.i * dd.i;
   dpda.i = a1.r * dd.i + a1.i * dd.r;
   dpdb.r = b1.r * dd.r - b1.i * dd.i;
   dpdb.i = b1.r * dd.i + b1.i * dd.r;

   /* computing the coefficients - first reflection */
   auxm1 = d2d.r - d1d.r;
   auxm2 = d2d.i - d1d.i;
   /* (d2d - d1d) / (d1d + d2d) */
   coeffD[0].r = auxm1 * dd.r - auxm2 * dd.i;
   coeffD[0].i = auxm1 * dd.i + auxm2 * dd.r;                 /* Rpp */
   
   auxm1 = a2b2.r * caux3.r - a2b2.i * caux3.i;
   auxm2 = a2b2.r * caux3.i + a2b2.i * caux3.r;
   coeffD[1].r = aux1aux2.r + auxm1;
   coeffD[1].i = aux1aux2.i + auxm2;                          /* Rsp */  
								 
   coeffD[2].r = coeffD[1].r;
   coeffD[2].i = coeffD[1].i;                                 /* Rps*/  

   auxm3 = dpdb.r * uC2.r - dpdb.i * uC2.i;
   auxm4 = dpdb.r * uC2.i + dpdb.i * uC2.r;
   
   aux = auxm3 * coeffD[1].r - auxm4 * coeffD[1].i;
   coeffD[1].i = auxm3 * coeffD[1].i + auxm4 * coeffD[1].r;
   coeffD[1].r = aux;                                         /* Rsp */
  
   auxm3 = -dpda.r * uC2.r + dpda.i * uC2.i;
   auxm4 = -dpda.r * uC2.i - dpda.i * uC2.r;
   
   aux = auxm3 * coeffD[2].r - auxm4 * coeffD[2].i;
   coeffD[2].i = auxm3 * coeffD[2].i + auxm4 * coeffD[2].r;
   coeffD[2].r = aux;                                         /* Rps */

   auxm1 = d2d.r - d1d.r;
   auxm2 = d2d.i - d1d.i;
   auxm3 = 2 * rho1rho2 * (a1b2.r - a2b1.r);
   auxm4 = 2 * rho1rho2 * (a1b2.i - a2b1.i);

   coeffD[3].r = auxm1 - auxm3;
   coeffD[3].i = auxm2 - auxm4;
   aux = coeffD[3].r * dd.r - coeffD[3].i * dd.i;
   coeffD[3].i = coeffD[3].r * dd.i + coeffD[3].i * dd.r;
   coeffD[3].r = aux;                                         /* Rss */
  
   /* now transmition */
   auxm1 = b1.r * aux2.r - b1.i * aux2.i;
   auxm2 = b1.r * aux2.i + b1.i * aux2.r;
   auxm3 = b2.r * aux3.r - b2.i * aux3.i;
   auxm4 = b2.r * aux3.i + b2.i * aux3.r;
   coeffD[4].r = auxm1 - auxm3;     
   coeffD[4].i = auxm2 - auxm4;
   aux = 2 * rho[i1] * (dpda.r * coeffD[4].r - dpda.i * coeffD[4].i);
   coeffD[4].i = 2 * rho[i1] * (dpda.r * coeffD[4].i + dpda.i * coeffD[4].r);
   coeffD[4].r = aux;                                         /* Tpp */

   coeffD[5].r = aux1.r + a1b2.r * c.r - a1b2.i * c.i;
   coeffD[5].i = aux1.i + a1b2.r * c.i + a1b2.i * c.r;
   auxm1 = rho[i1] * (dpdb.r * uC2.r - dpdb.i * uC2.i);
   auxm2 = rho[i1] * (dpdb.r * uC2.i + dpdb.i * uC2.r);
   aux = coeffD[5].r * auxm1 - coeffD[5].i * auxm2;
   coeffD[5].i = coeffD[5].r * auxm2 + coeffD[5].i * auxm1;   /* Tsp */
   coeffD[5].r = aux; 

   coeffD[6].r = aux1.r + a2b1.r * c.r - a2b1.i * c.i;
   coeffD[6].i = aux1.i + a2b1.r * c.i + a2b1.i * c.r;
   auxm1 = -rho[i1] * (dpda.r * uC2.r - dpda.i * uC2.i);
   auxm2 = -rho[i1] * (dpda.r * uC2.i + dpda.i * uC2.r);
   aux = coeffD[6].r * auxm1 - coeffD[6].i * auxm2;
   coeffD[6].i = coeffD[6].r * auxm2 + coeffD[6].i * auxm1;   /* Tsp */
   coeffD[6].r = aux;

   auxm1 = a1.r * aux2.r - a1.i * aux2.i;
   auxm2 = a1.r * aux2.i + a1.i * aux2.r;
   coeffD[7].r = auxm1 - (a2.r * aux3.r - a2.i * aux3.i);
   coeffD[7].i = auxm2 - (a2.r * aux3.i + a2.i * aux3.r);     
   auxm3 = 2 * rho[i1] * dpdb.r;
   auxm4 = 2 * rho[i1] * dpdb.i;
   aux = coeffD[7].r * auxm3 - coeffD[7].i * auxm4;
   coeffD[7].i = coeffD[7].r * auxm4 + coeffD[7].i * auxm3;   /* Tss */
   coeffD[7].r = aux;
}
/*                                                              */
/*  Function Rm()                                               */
/*                                                              */
/*  Computing reflectivity matrices for a given model at a      */
/*  specific slowness and temporal frequency                    */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */
/*                         global variable                      */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  rm.....................the reflectivity matrix              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void Rm()
{
   /* declaration of variables */
   int iL;                      /* counter */
   complex aux1;                /* auxiliar quantities */
   complex am, amI, bm, bmI;    /* vertical slownesses for P and S waves */
                                /* amI = am * I, bmI = bm * I */
   complex wThick;              /* wC * thickness */
   complex E[2][2];             /* phase-shift matrix */
   complex mT[2][2];            /* reflectivity matrix at top of layer */
   complex mTtD[2][2];          /* mt * tD */
   complex tUinv[2][2];         /* tU * inv */
   complex mB[2][2];            /* reflectivity matrix at bottom of layer */
   complex rD[2][2], tD[2][2];  /* reflec. and transm. coefficients for */
                                /* downgoing waves */
   complex rU[2][2], tU[2][2];  /* reflec. and transm. coefficients for */
                                /* upgoing waves */
   complex mAux[2][2];          /* auxiliar matrix */
   complex inv[2][2];           /* inv = (I - mT * rU)^-1 */
   
   /* initializing the reflectivity matrix at the bottom of half space */
   mT[0][0] = zeroC; mT[0][1] = zeroC;
   mT[1][0] = zeroC; mT[1][1] = zeroC;
   
   /* initializing the reflectivity matrix at the bottom */
   /* of layer nL - 1 */
   RTd(nL - 1, nL);

   mB[0][0] = coeffD[0]; mB[0][1] = coeffD[1]; 
   mB[1][0] = coeffD[2]; mB[1][1] = coeffD[3];

   /* main loop over the nL layers */
   for (iL = nL - 1; iL >= 1; iL--)
   {
      /* square-root of PSlowness^2 - uuC */    
      auxm1 = PSlowness[iL][0].r - uuC.r;
      auxm2 = PSlowness[iL][0].i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      am.r = auxm3 * cos(angle);
      am.i = auxm3 * sin(angle);

      /* am * I */
      amI.r = -am.i;
      amI.i = am.r;

      /* square-root of SSlowness^2 - uuC */    
      auxm1 = SSlowness[iL][0].r - uuC.r;
      auxm2 = SSlowness[iL][0].i - uuC.i;
      auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
      auxm3 = sqrt(auxm3);
      angle = atan2(auxm2, auxm1) / 2;
      bm.r = auxm3 * cos(angle);
      bm.i = auxm3 * sin(angle);

      /* bm * I */
      bmI.r = -bm.i;
      bmI.i = bm.r;

      /* computing phase-shift matrix */
      wThick.r = wC.r * (-2 * thick[iL]);
      wThick.i = wC.i * (-2 * thick[iL]);

      /* cexp (amI * wThick) */
      auxm1 = amI.r * wThick.r - amI.i * wThick.i;
      auxm2 = amI.r * wThick.i + amI.i * wThick.r;
      E[0][0].r = exp(auxm1) * cos(auxm2);
      E[0][0].i = exp(auxm1) * sin(auxm2);

      /* cexp((amI + bmI) * (wThick * .5)) */
      auxm1 = amI.r + bmI.r;
      auxm2 = amI.i + bmI.i;
      auxm3 = .5 * (auxm1 * wThick.r - auxm2 * wThick.i);
      auxm4 = .5 * (auxm1 * wThick.i + auxm2 * wThick.r);
      E[0][1].r = exp(auxm3) * cos(auxm4);
      E[0][1].i = exp(auxm3) * sin(auxm4);

      E[1][0] = E[0][1];

      /* cexp (bmI * wThick) */
      auxm1 = bmI.r * wThick.r - bmI.i * wThick.i;
      auxm2 = bmI.r * wThick.i + bmI.i * wThick.r;
      E[1][1].r = exp(auxm1) * cos(auxm2);
      E[1][1].i = exp(auxm1) * sin(auxm2);
	
      /* applying phase-shift */
      mT[0][0].r = mB[0][0].r * E[0][0].r - mB[0][0].i * E[0][0].i;
      mT[0][0].i = mB[0][0].r * E[0][0].i + mB[0][0].i * E[0][0].r;
      mT[0][1].r = mB[0][1].r * E[0][1].r - mB[0][1].i * E[0][1].i;
      mT[0][1].i = mB[0][1].r * E[0][1].i + mB[0][1].i * E[0][1].r;
      mT[1][0].r = mB[1][0].r * E[1][0].r - mB[1][0].i * E[1][0].i;
      mT[1][0].i = mB[1][0].r * E[1][0].i + mB[1][0].i * E[1][0].r;
      mT[1][1].r = mB[1][1].r * E[1][1].r - mB[1][1].i * E[1][1].i;
      mT[1][1].i = mB[1][1].r * E[1][1].i + mB[1][1].i * E[1][1].r;

      /* bottom-layer matrix - need a sequence of Ref and TRANS coeff. */
      RTd(iL - 1, iL);

      rD[0][0] = coeffD[0]; rD[0][1] = coeffD[1]; 
      rD[1][0] = coeffD[2]; rD[1][1] = coeffD[3];
      
      tD[0][0] = coeffD[4]; tD[0][1] = coeffD[5]; 
      tD[1][0] = coeffD[6]; tD[1][1] = coeffD[7];

      /* computing mT * tD */
      mTtD[0][0].r = mT[0][0].r * tD[0][0].r - mT[0][0].i * tD[0][0].i
	           + mT[0][1].r * tD[1][0].r - mT[0][1].i * tD[1][0].i;
      mTtD[0][0].i = mT[0][0].r * tD[0][0].i + mT[0][0].i * tD[0][0].r
	           + mT[0][1].r * tD[1][0].i + mT[0][1].i * tD[1][0].r;
      mTtD[0][1].r = mT[0][0].r * tD[0][1].r - mT[0][0].i * tD[0][1].i
	           + mT[0][1].r * tD[1][1].r - mT[0][1].i * tD[1][1].i;
      mTtD[0][1].i = mT[0][0].r * tD[0][1].i + mT[0][0].i * tD[0][1].r
	           + mT[0][1].r * tD[1][1].i + mT[0][1].i * tD[1][1].r;
      mTtD[1][0].r = mT[1][0].r * tD[0][0].r - mT[1][0].i * tD[0][0].i
                   + mT[1][1].r * tD[1][0].r - mT[1][1].i * tD[1][0].i;
      mTtD[1][0].i = mT[1][0].r * tD[0][0].i + mT[1][0].i * tD[0][0].r
      	           + mT[1][1].r * tD[1][0].i + mT[1][1].i * tD[1][0].r;
      mTtD[1][1].r = mT[1][0].r * tD[0][1].r - mT[1][0].i * tD[0][1].i
      	           + mT[1][1].r * tD[1][1].r - mT[1][1].i * tD[1][1].i;
      mTtD[1][1].i = mT[1][0].r * tD[0][1].i + mT[1][0].i * tD[0][1].r
      	           + mT[1][1].r * tD[1][1].i + mT[1][1].i * tD[1][1].r;

      RTu(iL - 1, iL);

      rU[0][0] = coeffU[0]; rU[0][1] = coeffU[1]; 
      rU[1][0] = coeffU[2]; rU[1][1] = coeffU[3];
      
      tU[0][0] = coeffU[4]; tU[0][1] = coeffU[5]; 
      tU[1][0] = coeffU[6]; tU[1][1] = coeffU[7];

      /* inv = (I - mT * rU)^-1 */
      auxm1 = mT[0][0].r * rU[0][0].r - mT[0][0].i * rU[0][0].i;
      auxm2 = mT[0][0].r * rU[0][0].i + mT[0][0].i * rU[0][0].r;
      auxm3 = mT[0][1].r * rU[1][0].r - mT[0][1].i * rU[1][0].i;
      auxm4 = mT[0][1].r * rU[1][0].i + mT[0][1].i * rU[1][0].r;
      mAux[0][0].r = 1 - (auxm1 + auxm3);
      mAux[0][0].i = - (auxm2 + auxm4);

      auxm1 = mT[0][0].r * rU[0][1].r - mT[0][0].i * rU[0][1].i;
      auxm2 = mT[0][0].r * rU[0][1].i + mT[0][0].i * rU[0][1].r;
      auxm3 = mT[0][1].r * rU[1][1].r - mT[0][1].i * rU[1][1].i;
      auxm4 = mT[0][1].r * rU[1][1].i + mT[0][1].i * rU[1][1].r;
      mAux[0][1].r = - (auxm1 + auxm3);
      mAux[0][1].i = - (auxm2 + auxm4);

      auxm1 = mT[1][0].r * rU[0][0].r - mT[1][0].i * rU[0][0].i;
      auxm2 = mT[1][0].r * rU[0][0].i + mT[1][0].i * rU[0][0].r;
      auxm3 = mT[1][1].r * rU[1][0].r - mT[1][1].i * rU[1][0].i;
      auxm4 = mT[1][1].r * rU[1][0].i + mT[1][1].i * rU[1][0].r;
      mAux[1][0].r = - (auxm1 + auxm3);
      mAux[1][0].i = - (auxm2 + auxm4);

      auxm1 = mT[1][0].r * rU[0][1].r - mT[1][0].i * rU[0][1].i;
      auxm2 = mT[1][0].r * rU[0][1].i + mT[1][0].i * rU[0][1].r;
      auxm3 = mT[1][1].r * rU[1][1].r - mT[1][1].i * rU[1][1].i;
      auxm4 = mT[1][1].r * rU[1][1].i + mT[1][1].i * rU[1][1].r;
      mAux[1][1].r = 1 - (auxm1 + auxm3);
      mAux[1][1].i = - (auxm2 + auxm4);

      /* inverting */
      auxm1 = mAux[0][0].r * mAux[1][1].r - mAux[0][0].i * mAux[1][1].i;
      auxm2 = mAux[0][0].r * mAux[1][1].i + mAux[0][0].i * mAux[1][1].r;
      auxm3 = mAux[0][1].r * mAux[1][0].r - mAux[0][1].i * mAux[1][0].i;
      auxm4 = mAux[0][1].r * mAux[1][0].i + mAux[0][1].i * mAux[1][0].r;
      aux1.r = auxm1 - auxm3;
      aux1.i = auxm2 - auxm4;

      /* 1 / aux1 */
      aux = aux1.r * aux1.r + aux1.i * aux1.i;
      aux1.r = aux1.r / aux;
      aux1.i = -aux1.i / aux;

      inv[0][0].r = mAux[1][1].r * aux1.r - mAux[1][1].i * aux1.i;
      inv[0][0].i = mAux[1][1].r * aux1.i + mAux[1][1].i * aux1.r;
      inv[0][1].r = -1 * (mAux[0][1].r * aux1.r - mAux[0][1].i * aux1.i);
      inv[0][1].i = -1 * (mAux[0][1].r * aux1.i + mAux[0][1].i * aux1.r);
      inv[1][0].r = -1 * (mAux[1][0].r * aux1.r - mAux[1][0].i * aux1.i);
      inv[1][0].i = -1 * (mAux[1][0].r * aux1.i + mAux[1][0].i * aux1.r);
      inv[1][1].r = mAux[0][0].r * aux1.r - mAux[0][0].i * aux1.i;
      inv[1][1].i = mAux[0][0].r * aux1.i + mAux[0][0].i * aux1.r;
      

      /* computing tU * inv */
      tUinv[0][0].r = tU[0][0].r * inv[0][0].r - tU[0][0].i * inv[0][0].i
  	            + tU[0][1].r * inv[1][0].r - tU[0][1].i * inv[1][0].i;
      tUinv[0][0].i = tU[0][0].r * inv[0][0].i + tU[0][0].i * inv[0][0].r
	            + tU[0][1].r * inv[1][0].i + tU[0][1].i * inv[1][0].r;
      tUinv[0][1].r = tU[0][0].r * inv[0][1].r - tU[0][0].i * inv[0][1].i
	            + tU[0][1].r * inv[1][1].r - tU[0][1].i * inv[1][1].i;
      tUinv[0][1].i = tU[0][0].r * inv[0][1].i + tU[0][0].i * inv[0][1].r
	            + tU[0][1].r * inv[1][1].i + tU[0][1].i * inv[1][1].r;
      tUinv[1][0].r = tU[1][0].r * inv[0][0].r - tU[1][0].i * inv[0][0].i
                    + tU[1][1].r * inv[1][0].r - tU[1][1].i * inv[1][0].i;
      tUinv[1][0].i = tU[1][0].r * inv[0][0].i + tU[1][0].i * inv[0][0].r
      	            + tU[1][1].r * inv[1][0].i + tU[1][1].i * inv[1][0].r;
      tUinv[1][1].r = tU[1][0].r * inv[0][1].r - tU[1][0].i * inv[0][1].i
      	            + tU[1][1].r * inv[1][1].r - tU[1][1].i * inv[1][1].i;
      tUinv[1][1].i = tU[1][0].r * inv[0][1].i + tU[1][0].i * inv[0][1].r
      	            + tU[1][1].r * inv[1][1].i + tU[1][1].i * inv[1][1].r;
      
      /* finally the matrix */
      auxm1 = mTtD[0][0].r * tUinv[0][0].r - mTtD[0][0].i * tUinv[0][0].i;
      auxm2 = mTtD[0][0].r * tUinv[0][0].i + mTtD[0][0].i * tUinv[0][0].r;
      auxm3 = mTtD[1][0].r * tUinv[0][1].r - mTtD[1][0].i * tUinv[0][1].i;
      auxm4 = mTtD[1][0].r * tUinv[0][1].i + mTtD[1][0].i * tUinv[0][1].r;
      mB[0][0].r = rD[0][0].r + (auxm1 + auxm3);
      mB[0][0].i = rD[0][0].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][1].r * tUinv[0][0].r - mTtD[0][1].i * tUinv[0][0].i;
      auxm2 = mTtD[0][1].r * tUinv[0][0].i + mTtD[0][1].i * tUinv[0][0].r;
      auxm3 = mTtD[1][1].r * tUinv[0][1].r - mTtD[1][1].i * tUinv[0][1].i;
      auxm4 = mTtD[1][1].r * tUinv[0][1].i + mTtD[1][1].i * tUinv[0][1].r;
      mB[0][1].r = rD[0][1].r + (auxm1 + auxm3);
      mB[0][1].i = rD[0][1].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][0].r * tUinv[1][0].r - mTtD[0][0].i * tUinv[1][0].i;
      auxm2 = mTtD[0][0].r * tUinv[1][0].i + mTtD[0][0].i * tUinv[1][0].r;
      auxm3 = mTtD[1][0].r * tUinv[1][1].r - mTtD[1][0].i * tUinv[1][1].i;
      auxm4 = mTtD[1][0].r * tUinv[1][1].i + mTtD[1][0].i * tUinv[1][1].r;
      mB[1][0].r = rD[1][0].r + (auxm1 + auxm3);
      mB[1][0].i = rD[1][0].i + (auxm2 + auxm4);

      auxm1 = mTtD[0][1].r * tUinv[1][0].r - mTtD[0][1].i * tUinv[1][0].i;
      auxm2 = mTtD[0][1].r * tUinv[1][0].i + mTtD[0][1].i * tUinv[1][0].r;
      auxm3 = mTtD[1][1].r * tUinv[1][1].r - mTtD[1][1].i * tUinv[1][1].i;
      auxm4 = mTtD[1][1].r * tUinv[1][1].i + mTtD[1][1].i * tUinv[1][1].r;
      mB[1][1].r = rD[1][1].r + (auxm1 + auxm3);
      mB[1][1].i = rD[1][1].i + (auxm2 + auxm4);
   }
   /* computing final phase-shift matrix */
   /* square-root of Pslowness^2 - uuC */    
   auxm1 = PSlowness[0][0].r - uuC.r;
   auxm2 = PSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   am.r = auxm3 * cos(angle);
   am.i = auxm3 * sin(angle);

   /* am * I */
   amI.r = -am.i;
   amI.i = am.r;

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[0][0].r - uuC.r;
   auxm2 = SSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   bm.r = auxm3 * cos(angle);
   bm.i = auxm3 * sin(angle);

   /* bm * I */
   bmI.r = -bm.i;
   bmI.i = bm.r;
   
   /* computing phase-shift matrix */
   wThick.r = wC.r * (-2 * thick[0]);
   wThick.i = wC.i * (-2 * thick[0]);

   /* cexp (amI * wThick) */
   auxm1 = amI.r * wThick.r - amI.i * wThick.i;
   auxm2 = amI.r * wThick.i + amI.i * wThick.r;
   E[0][0].r = exp(auxm1) * cos(auxm2);
   E[0][0].i = exp(auxm1) * sin(auxm2);

   /* cexp((amI + bmI) * (wThick * .5)) */
   auxm1 = amI.r + bmI.r;
   auxm2 = amI.i + bmI.i;
   auxm3 = .5 * (auxm1 * wThick.r - auxm2 * wThick.i);
   auxm4 = .5 * (auxm1 * wThick.i + auxm2 * wThick.r);
   E[0][1].r = exp(auxm3) * cos(auxm4);
   E[0][1].i = exp(auxm3) * sin(auxm4);
   
   E[1][0] = E[0][1];

   /* cexp (bmI * wThick) */
   auxm1 = bmI.r * wThick.r - bmI.i * wThick.i;
   auxm2 = bmI.r * wThick.i + bmI.i * wThick.r;
   E[1][1].r = exp(auxm1) * cos(auxm2);
   E[1][1].i = exp(auxm1) * sin(auxm2);

   /* applying phase-shift */
   mT[0][0].r = mB[0][0].r * E[0][0].r - mB[0][0].i * E[0][0].i;
   mT[0][0].i = mB[0][0].r * E[0][0].i + mB[0][0].i * E[0][0].r;
   mT[0][1].r = mB[0][1].r * E[0][1].r - mB[0][1].i * E[0][1].i;
   mT[0][1].i = mB[0][1].r * E[0][1].i + mB[0][1].i * E[0][1].r;
   mT[1][0].r = mB[1][0].r * E[1][0].r - mB[1][0].i * E[1][0].i;
   mT[1][0].i = mB[1][0].r * E[1][0].i + mB[1][0].i * E[1][0].r;
   mT[1][1].r = mB[1][1].r * E[1][1].r - mB[1][1].i * E[1][1].i;
   mT[1][1].i = mB[1][1].r * E[1][1].i + mB[1][1].i * E[1][1].r;

   /* copying to matrix rm */
   rm[0][0] = mT[0][0]; rm[0][1] = mT[0][1];
   rm[1][0] = mT[1][0]; rm[1][1] = mT[1][1];
}
/*                                                              */
/*  Function Rp()                                               */
/*                                                              */
/*  Computing the response from the free surface                */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */
/*                         global variable                      */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  rp.....................response from the free surface       */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void Rp()
{
   /* declaration of variables */
   complex aux1, aux12;         /* auxiliar variables */
   complex wThick;              /* auxiliar variable */
   complex E[2][2];             /* phase shift matrix */
   complex am, bm;              /* vertical slownesses for P and S waves */
   complex amI, bmI;            /* amI = I * am, bmI = I * bm */
   complex ambm;                /* = am * bm */
   complex ambm4uu;             /* = 4 * am * bm * uC * uC */
   complex den;                 /* denominator of coefficients */

   /* square-root of PSlowness - uuC */    
   auxm1 = PSlowness[0][0].r - uuC.r;
   auxm2 = PSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   am.r = auxm3 * cos(angle);
   am.i = auxm3 * sin(angle);

   /* am * I */
   amI.r = -am.i;
   amI.i = am.r;

   /* square-root of SSlowness^2 - uuC */    
   auxm1 = SSlowness[0][0].r - uuC.r;
   auxm2 = SSlowness[0][0].i - uuC.i;
   auxm3 = sqrt(auxm1 * auxm1 + auxm2 * auxm2);
   auxm3 = sqrt(auxm3);
   angle = atan2(auxm2, auxm1) / 2;
   bm.r = auxm3 * cos(angle);
   bm.i = auxm3 * sin(angle);

   /* bm * I */
   bmI.r = -bm.i;
   bmI.i = bm.r;
   
   aux1.r = SSlowness[0][0].r - uuC2.r;
   aux1.i = SSlowness[0][0].i - uuC2.i;
   aux12.r = aux1.r * aux1.r - aux1.i * aux1.i;
   aux12.i = 2 * aux1.r * aux1.i;

   /* let ambm = am * bm */
   ambm.r = am.r * bm.r - am.i * bm.i;
   ambm.i = am.r * bm.i + am.i * bm.r;

   /* and ambm4uu = am * bm * 4 * uu */
   ambm4uu.r = 4 * (ambm.r * uuC.r - ambm.i * uuC.i);
   ambm4uu.i = 4 * (ambm.r * uuC.i + ambm.i * uuC.r);

   den.r = aux12.r + ambm4uu.r;
   den.i = aux12.i + ambm4uu.i;
   
   /* 1/ den */
   aux = den.r * den.r + den.i * den.i;
   den.r = den.r / aux;
   den.i = -den.i / aux;

   auxm1 = ambm4uu.r - aux12.r;
   auxm2 = ambm4uu.i - aux12.i;
   rp[0][0].r = auxm1 * den.r - auxm2 * den.i;
   rp[0][0].i = auxm1 * den.i + auxm2 * den.r;               /* Rpp */

   auxm1 = bm.r * aux1.r - bm.i * aux1.i;
   auxm2 = bm.r * aux1.i + bm.i * aux1.r;
   auxm3 = 4 * (auxm1 * uC.r - auxm2 * uC.i);
   auxm4 = 4 * (auxm1 * uC.i + auxm2 * uC.r);
   rp[0][1].r = den.r * auxm3 - den.i * auxm4;
   rp[0][1].i = den.r * auxm4 + den.i * auxm3;               /* Rsp */

   auxm1 = am.r * aux1.r - am.i * aux1.i;
   auxm2 = am.r * aux1.i + am.i * aux1.r;
   auxm3 = -4 * (auxm1 * uC.r - auxm2 * uC.i);
   auxm4 = -4 * (auxm1 * uC.i + auxm2 * uC.r);
   rp[1][0].r = den.r * auxm3 - den.i * auxm4;
   rp[1][0].i = den.r * auxm4 + den.i * auxm3;               /* Rsp */

   rp[1][1].r = -rp[0][0].r;
   rp[1][1].i = -rp[0][0].i;                                 /* Rss */

      /* computing phase-shift matrix */
   wThick.r = wC.r * (-2 * zs);
   wThick.i = wC.i * (-2 * zs);

   /* cexp (amI * wThick) */
   auxm1 = amI.r * wThick.r - amI.i * wThick.i;
   auxm2 = amI.r * wThick.i + amI.i * wThick.r;
   E[0][0].r = exp(auxm1) * cos(auxm2);
   E[0][0].i = exp(auxm1) * sin(auxm2);

   /* cexp((amI + bmI) * (wThick * .5)) */
   auxm1 = amI.r + bmI.r;
   auxm2 = amI.i + bmI.i;
   auxm3 = .5 * (auxm1 * wThick.r - auxm2 * wThick.i);
   auxm4 = .5 * (auxm1 * wThick.i + auxm2 * wThick.r);
   E[0][1].r = exp(auxm3) * cos(auxm4);
   E[0][1].i = exp(auxm3) * sin(auxm4);

   E[1][0] = E[0][1];

   /* cexp (bmI * wThick) */
   auxm1 = bmI.r * wThick.r - bmI.i * wThick.i;
   auxm2 = bmI.r * wThick.i + bmI.i * wThick.r;
   E[1][1].r = exp(auxm1) * cos(auxm2);
   E[1][1].i = exp(auxm1) * sin(auxm2);
	
   /* applying phase-shift */
   aux = rp[0][0].r * E[0][0].r - rp[0][0].i * E[0][0].i;
   rp[0][0].i = rp[0][0].r * E[0][0].i + rp[0][0].i * E[0][0].r;
   rp[0][0].r = aux;
   aux = rp[0][1].r * E[0][1].r - rp[0][1].i * E[0][1].i;
   rp[0][1].i = rp[0][1].r * E[0][1].i + rp[0][1].i * E[0][1].r;
   rp[0][1].r = aux;
   aux = rp[1][0].r * E[1][0].r - rp[1][0].i * E[1][0].i;
   rp[1][0].i = rp[1][0].r * E[1][0].i + rp[1][0].i * E[1][0].r;
   rp[1][0].r = aux;
   aux = rp[1][1].r * E[1][1].r - rp[1][1].i * E[1][1].i;
   rp[1][1].i = rp[1][1].r * E[1][1].i + rp[1][1].i * E[1][1].r;
   rp[1][1].r = aux;
}
/*                                                              */
/*  Function horSlowness()                                      */
/*                                                              */
/*  Computing the following parameters used in the              */
/*  reflectivity computation:                                   */
/*                                                              */
/*  1) P-wave slowness squared                                  */
/*  2) S-wave slowness squared                                  */
/*  3) S-wave velocity squared                                  */
/*                                  ...for all layers           */
/*                                                              */
/*  Input parameters:                                           */
/*  alpha..................p-wave velocities of the model       */
/*                         global variable                      */
/*  beta...................s-wave velocities of the model       */
/*                         global variable                      */
/*  rho....................densities of the elastic model       */
/*                         global variable                      */
/*  thick..................thicknesses of the model             */
/*                         global variable                      */
/*  wC.....................complex frequency                    */
/*                         global variable                      */
/*  uC.....................complex slowness                     */
/*                         global variable                      */
/*                                                              */
/*  Output parameters:                                          */
/*  PSlowness[0][nL].......p-wave slowness squared              */
/*  SSlowness[0][nL].......s-wave slowness squared              */
/*  S2Velocity[0][nL]......s-wave velocity squared              */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void horSlowness()
{
   /* declaration of variables */
   int iL;                       /* counters */
   float cteQp1, cteQp2;
   float cteQs1, cteQs2;         /* constants used in absorption */
   float a, b;                   /* p and s-wave velocities */

   for(iL = 0; iL <= nL; iL++)
   {
      a = alpha[iL];
      b = beta[iL];

      /* constants used in absorption */
      cteQp1 = a / (PI * qP[iL]);
      cteQp2 = a / (2 * qP[iL]);
      cteQs1 = b / (PI * qS[iL]);
      cteQs2 = b / (2 * qS[iL]);
      
      /* p-wave slowness squared */
      PSlowness[iL][0].r = a + cteQp1 * log(wCRwR);
      PSlowness[iL][0].i = cteQp2 - cteQp1 * wCP;

      /* 1. / (PSlowness[iL] * PSlowness[iL]) */
      auxm1 = PSlowness[iL][0].r * PSlowness[iL][0].r;
      auxm2 = PSlowness[iL][0].i * PSlowness[iL][0].i;
      aux = (auxm1 + auxm2) * (auxm1 + auxm2); aux = 1 / aux;
      auxm3 = (auxm1 - auxm2) * aux;
      PSlowness[iL][0].i = -2 * PSlowness[iL][0].r * 
                           PSlowness[iL][0].i * aux;
      PSlowness[iL][0].r = auxm3;
      
      /* s-wave velocity */
      auxm1 = b + cteQs1 * log(wCRwR);
      auxm2 = cteQs2 - cteQs1 * wCP;

      /* S2Velocity[iL] * S2Velocity[iL] */
      S2Velocity[iL][0].r = auxm1 * auxm1 - auxm2 * auxm2;
      S2Velocity[iL][0].i = 2 * auxm1 * auxm2;

      /* 1. / S2Velocity^2 */
      aux = S2Velocity[iL][0].r * S2Velocity[iL][0].r + 
	    S2Velocity[iL][0].i * S2Velocity[iL][0].i;

      SSlowness[iL][0].r = S2Velocity[iL][0].r / aux;
      SSlowness[iL][0].i = -S2Velocity[iL][0].i / aux;
   }
}
/*                                                              */
/*  Function Bessels()                                          */
/*                                                              */
/*  Computing Bessel functions order 1 and 0 for a given        */
/*  real argument                                               */
/*                                                              */
/*  Input parameters:                                           */
/*  arg....................argument for bessel functions        */
/*                                                              */
/*  Output parameters:                                          */
/*  J00....................bessel function order 0 evaluated    */
/*                         at arg  (global variable)            */
/*  J11....................bessel function order 1 evaluated    */
/*                         at arg  (global variable)            */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void Bessels(float x)
{
   register float xd3, x2, x4, x6, x8, x10, x12, sqrx;
   if (x <= 2.75) 
   {
      xd3 = x / 3;
      x2 = xd3 * xd3;
      x4 = x2 * x2;
      x6 = x2 * x4;
      x8 = x4 * x4;
      x10 = x4 * x6;
      x12 = x6 * x6;
      
      J00 = 1.-2.2499997*x2+1.2656208*x4
              -0.3163866*x6+0.0444479*x8
  	      -0.0039444*x10+0.00021*x12;
      
      J11 = (0.5-0.56249985*x2+0.21093573*x4
	        -0.03954289*x6+0.00443319*x8
	        -0.00031761*x10+0.00001109*x12)*x;
   }
   else
   {
      sqrx = sqrt(2. / (PI * x));
      J00 = sqrx * cos(x - 0.25 * PI);
      J11 = sqrx * cos(x - 0.75 * PI);
   }
}
/*                                                              */
/*  Function filter()                                           */
/*                                                              */
/*  Computes a windowing operator for the frequency domain      */
/*  tp avoid shar edges                                         */
/*                                                              */
/*  Input parameters:                                           */
/*  arg....................argument for bessel functions        */
/*  nSamples...............number of samples per trace          */
/*                         global                               */
/*  percW..................percentage of filtering              */
/*                                                              */
/*  Output parameters:                                          */
/*  window.................array with window coefficients       */
/*                         global                               */
/*                                                              */
/*                                              Wences Gouveia  */
/*                                              September 1995  */
void filter(float percW)
{
   /* declaration of variables */
   int i, iF, indexF;            /* counters */
   int wL;                       /* window length */

   /* memory allocation */
   window = alloc1float(nSamples);
   /* reseting array */
   for (i = 0; i < nSamples / 2 + 1; i++)
      window[i] = 0;

   wL = percW * nF;   wL = 2 * wL - 1;
   /* building hanning window */
   for (i = 0, indexF = NINT(f1 / dF), iF = 0; iF < nF; iF++, indexF++)
   {
      if (hanningFlag)
	 window[indexF] =
	    .42 - .5 * cos(2 * PI * (float) iF / ((float) (nF - 1))) +
            .08 * cos(4 * PI * (float) iF / ((float) (nF - 1)));
      else
      {
	 window[indexF] = 1;
	 if (iF < (wL - 1) / 2)
	 {
	    window[indexF] = 
	       .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
  	       .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
	    i++;
	 }
	 else if (iF > nF - (wL - 1) / 2)
	 {
	    i++;
	    window[indexF] =
	       .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
  	       .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
	 }
      }
   }
}
