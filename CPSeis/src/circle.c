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
! Name       : circle
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2002-05-16   by: Donna K. Vunderink
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
!  2. 2002-05-16  Vunderink    Make r_tmp1 positive in cgmGgdpCircle
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char circle_ident[100] = 
"$Id: circle.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmGgdpCircle(float xc, float yc, float r)
{
  float tmp1,tmp2,r_tmp1,r_tmp2,r_tmp;

  if (cgmdebug) fprintf(err_unit,"cgm_ggdp_circle: %f %f %f \n",xc,yc,r);
  cgmPutPoint(xc,yc);
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(tmp1+r,tmp2+r,&r_tmp1,&r_tmp2);
  r_tmp = ABS(r_tmp1)*scalefactor;
  if (vdc_type == 0 && r_tmp < 1.0) r_tmp = 1.0;
  cgmPutVdc(r_tmp);
  cgmPutData(4,12);
}

void cgmGgdpArc(float xc, float yc, float dx1, float dy1, float dx2, float dy2,
                float r, int itype)
{
  float tmp,tmp1,tmp2,tmp3,tmp4,r_tmp;

  if (cgmdebug) fprintf(err_unit,
                       "cgm_ggdp_arc: %f %f %f %f %f %f %f %d \n",
                        xc,yc,dx1,dy1,dx2,dy2,r,itype);
  cgmPutPoint(xc,yc);
  tmp = sqrt(dx1*dx1+dy1*dy1);
  dx1 = 10000.0*dx1/tmp;
  dy1 = 10000.0*dy1/tmp;
  cgmPutVdc(dx1);
  cgmPutVdc(dy1);
  tmp = sqrt(dx2*dx2+dy2*dy2);
  dx2 = 10000.0*dx2/tmp;
  dy2 = 10000.0*dy2/tmp;
  cgmPutVdc(dx2);
  cgmPutVdc(dy2);
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(tmp1+r,tmp2+r,&tmp3,&tmp4);
  r_tmp = tmp3*scalefactor;
  if (vdc_type == 0 && r_tmp < 1.0) r_tmp = 1.0;
  cgmPutVdc(r_tmp);
  if (itype == 2)
     cgmPutData(4,15);
  else {
     cgmPutEnum(itype);
     cgmPutData(4,16);
  }
}
