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
! Name       : fill
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2002-08-15   by: Donna K. Vunderink
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
!  2. 2002-08-15  Vunderink    Fixed array bounds bug in cgmGfa.
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char fill_ident[100] = 
"$Id: fill.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmGsfais( int itype)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsfais: %d \n",itype);
  filltype = itype;
  cgmPutInt(filltype);
  cgmPutData(5,22);
}

void cgmGfa(int n,float x[], float y[])
{
  int i;

  if (cgmdebug) fprintf(err_unit,"cgm_gsfa: %d \n",n);
  if (filltype == 0)
     cgmGpl(n,x,y);
  else {
     for (i=0;i<n;i++) {
         cgmPutPoint(x[i],y[i]);
     }
     if (x[n-1] != x[0] || y[n-1] != x[0]) cgmPutPoint(x[0],y[0]);
     cgmPutData(4,7);
  }
}

void cgmGsfasi(int ihatch)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsfasi: %d \n",ihatch);
  fillhatch = ihatch;
  if (filltype == 2) {
     cgmPutInt(fillhatch-256);
     cgmPutData(5,25);
  }
  else if (filltype == 3) {
     if (fillhatch >= 1 && fillhatch <= 6)
        cgmPutInt(fillhatch);
     else
        cgmPutInt(6);                    /* combined left and right slant */
     cgmPutData(5,24);
  }
}

void cgmGsparf(float x0, float y0)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsparf: %f %f \n",x0,y0);
  cgmPutPoint(x0,y0);
  cgmPutData(5,31);
}

void cgmGspa(float dx, float dy)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gspa: %f %f \n",dx,dy);
  cgmPutPoint(0.0,dy);
  cgmPutPoint(dx,0.0);
  cgmPutData(5,33);
}

void cgmGspar(int i_wk,int i_pat,int i_dx,int i_dy,int i_dim,int ipat_array[])
{
  int i;

  if (cgmdebug) fprintf(err_unit,"cgm_gspar: %d %d \n",i_wk,i_pat);
  cgmPutIndex(i_pat-256);
  cgmPutInt(i_dx);
  cgmPutInt(i_dy);
  cgmPutInt(16);
  for (i=0;i<i_dx*i_dy;i++) {
      if (ipat_array[i] == -1) ipat_array[i] = 512;
      cgmPutInt(ipat_array[i]);
  }
  cgmPutData(5,32);
}

void cgmGpfill(int icol,int ifasi,int ifais)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gpfill: %d %d %d \n",icol,ifasi,ifais);
  filltype = ifais;
  fillcolor = icol;
  if (fillcolor == 0) fillcolor = 512;
  fillhatch = ifasi;
}

void cgmGqfill(int *icol,int *ifasi,int *ifais)
{
  *ifais = filltype;
  *icol = fillcolor;
  if (*icol == 512) *icol = 0;
  *ifasi = fillhatch;
  if (cgmdebug) fprintf(err_unit,"cgm_gqfill: %d %d %d \n",*icol,*ifasi,*ifais);
}
