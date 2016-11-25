/****
!<CPS_v1 type=AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : color
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CGM library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char color_ident[100] = 
"$Id: colorcgm.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

/* Fill color */
void cgmGsfaci(int color_index_in)
{
  int color_index;

  if (cgmdebug) fprintf(err_unit,"cgm_gsfaci: %d \n",color_index_in);
  color_index = color_index_in;
  if (color_index == 0) color_index = 512;
  if (fillcolor != color_index) {
     fillcolor = color_index;
     cgmPutInt(fillcolor);
     cgmPutData(5,23);
  }
}

/* Background Fill color */
void cgmGsfbci(int color_index_in)
{
  int color_index;
  if (cgmdebug) fprintf(err_unit,"cgm_gsfbci: %d \n",color_index_in);
}

/* Line color */
void cgmGsplci(int color_index_in)
{
  int color_index;

  if (cgmdebug) fprintf(err_unit,"cgm_gsplci: %d \n",color_index_in);
  color_index = color_index_in;
  if (color_index == 0) color_index = 512;
  if (linecolor != color_index) {
     linecolor = color_index;
     cgmPutInt(linecolor);
     cgmPutData(5,4);
  }
}

/* Marker color */
void cgmGspmci(int color_index_in)
{
  int color_index;

  if (cgmdebug) fprintf(err_unit,"cgm_gsmci: %d \n",color_index_in);
  color_index = color_index_in;
  if (color_index == 0) color_index = 512;
  if (markercolor != color_index) {
     markercolor = color_index;
     cgmPutInt(markercolor);
     cgmPutData(5,8);
  }
}

/* Text color */
void cgmGstxci(int color_index_in)
{
  int color_index;

  if (cgmdebug) fprintf(err_unit,"cgm_gstxci: %d \n",color_index_in);
  color_index = color_index_in;
  if (color_index == 0) color_index = 512;
  if (textcolor != color_index) {
     textcolor = color_index;
     cgmPutInt(textcolor);
     cgmPutData(5,14);
  }
}

/* Define color */
void cgmGscr(int iwk,int icol1, float r1, float g1, float b1)
{
  int icol;
  float r,g,b;

  if (cgmdebug) fprintf(err_unit,
                       "cgm_gscr: %d %d %f %f %f \n",
                        iwk,icol1,r1,g1,b1);
  icol = icol1;
  r = r1;
  g = g1;
  b = b1;
  if (icol == 0) {
     if (r==0.0 && g==0.0 && b==0.0) {
        r = 1.0;
        g = 1.0;
        b = 1.0;
     }
     if (r!=back_red || g!=back_green || b!=back_blue) {
        back_red = r;
        back_green = g;
        back_blue = b;
        cgmPutColor(back_red,back_green,back_blue);
        cgmPutData(2,7);
     }
     icol = 512;
  }
  else if (icol == 1) {
     if (r==1.0 && g==1.0 && b==1.0) {
        r = 0.0;
        g = 0.0;
        b = 0.0;
     }
  }

  cgmPutInt(icol);
  cgmPutColor(r,g,b);
  cgmPutData(5,34);
}

void cgmGqplci(int *ierr, int *icol)
{
  *icol = linecolor;
  if (*icol == 512) *icol = 0;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqpci: %d %d \n",*ierr,*icol);
}

void cgmGqfaci(int *ierr, int *icol)
{
  *icol = fillcolor;
  if (*icol == 512) *icol = 0;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqfaci: %d %d \n",*ierr,*icol);
}

void cgmGqfais(int *ierr, int *icol)
{
  *icol = filltype;
  if (*icol == 512) *icol = 0;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqfais: %d %d \n",*ierr,*icol);
}

void cgmGqtxci(int *ierr, int *icol)
{
  *icol = textcolor;
  if (*icol == 512) *icol = 0;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqtxci: %d %d \n",*ierr,*icol);
}

void cgmGqmkci(int *ierr, int *icol)
{
  *icol = markercolor;
  if (*icol == 512) *icol = 0;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqmkci: %d %d \n",*ierr,*icol);
}
