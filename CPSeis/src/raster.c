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
! Name       : raster
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-17   by: Donna K. Vunderink
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
!  2. 2001-04-17  Vunderink    Removed debug printout.
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char raster_ident[100] = 
"$Id: raster.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

void cgmGsras(float x, float y, char a[], int nr,int nc)
{
  int i,i1;

  if (cgmdebug) fprintf(err_unit,"cgm_gsras: %f %f a %d %d \n",x,y,nr,nc);
  cgmPutPoint(x,y);
  cgmPutPoint(x+nr,y+nc);
  cgmPutPoint(x+nr,y);
  cgmPutInt(nr);
  cgmPutInt(nc);
  cgmPutInt(8);            /* Local Color Precision */
  cgmPutEnum(1);           /* Packed List Data Mode */
  for (i=0;i<nr;i++) {
    for (i1=0;i1<nc;i1++) {
      cgmPutByte(a[i*nc+i1]);
    }
    if (nc%2 != 0) {
      cgmPutByte(0);       /* Each Row must start on a word boundary */
    }
  }
  cgmPutData(4,9);
}

void cgmGpras()
{
  fprintf(err_unit,"Raster data reading not supported -- GPRAS \n");
}

void cgmPixplt(float x_1,float y_1,float x_2,float y_2,int nx,int ny,
               char a[], int isw)
{
  cgmGscell(x_1,y_1,x_2,y_2,nx,ny,a,isw);
}

void cgmGscell(float x_1,float y_1,float x_2,float y_2,int nx,int ny,
               char a[], int isw)
{
  float x1,y1,x2,y2;
  int i,i1,length,w_length;
  char btemp;

  if (cgmdebug) fprintf(err_unit,"cgm_gscell: %f %f %f %f %d %d a %d \n",
                        x_1,y_1,x_2,y_2,nx,ny,isw);
  x1 = x_1;
  x2 = x_2;
  y1 = y_1;
  y2 = y_2;
  if (nx < ny/2) {
    cgmPutPoint(x1,y1);
    cgmPutPoint(x2,y2);
    cgmPutPoint(x1,y2);
    cgmPutInt(ny);
    cgmPutInt(nx);
    cgmPutInt(8);        /* Local Color Precision */
    cgmPutEnum(1);       /* Packed List Data Mode */

    cgmPutHeader(4,9,31);           /* (class,id,length) */
    length = (pointer+1) & 0x0000ffff;
    w_length = length | 0x00008000;
    cgmPutIntNow(w_length);
    for (i=0;i<pointer+1;i++) {
       btemp = buffer[i];
       cgmPutByteNow(btemp);
    }
    if ((pointer+1)%2 != 0)  cgmPutByteNow(0);

    if (nx > 1) {
      for (i=0;i<nx-1;i++) {
        if (ny%2 == 0)
          length = ny & 0x00007fff;
        else
          length = (ny+1) & 0x00007fff;
        w_length = length | 0x00008000;
        cgmPutIntNow(w_length);
        for (i1=0;i1<ny;i1++) {
           cgmPutByteNow(a[i1*nx+i]);
        }
        if (nx%2 != 0)  cgmPutByteNow(0);
      }    
    }
    if (ny%2 == 0)
      length = ny & 0x00007fff;
    else
      length = (ny+1) & 0x00007fff;
    w_length = length | 0x00000000;
    cgmPutIntNow(w_length);
    for (i1=0;i1<ny;i1++) {
      cgmPutByteNow(a[i1*nx]);
    }
    if (ny%2 != 0)  cgmPutByteNow(0);
    pointer = -1;
  }
  else {
    cgmPutPoint(x1,y1);
    cgmPutPoint(x2,y2);
    cgmPutPoint(x2,y1);
    cgmPutInt(nx);
    cgmPutInt(ny);
    cgmPutInt(8);        /* Local Color Precision */
    cgmPutEnum(1);       /* Packed List Data Mode */

    cgmPutHeader(4,9,31);           /* (class,id,length) */
    length = (pointer+1) & 0x0000ffff;
    w_length = length | 0x00008000;
    cgmPutIntNow(w_length);
    for (i=0;i<pointer+1;i++) {
      btemp = buffer[i];
      cgmPutByteNow(btemp);
    }
    if ((pointer+1)%2 != 0)  cgmPutByteNow(0);

    for (i=0;i<ny-1;i++) {
      if (nx%2 == 0)
        length = nx & 0x0000ffff;
      else
        length = (nx+1) & 0x0000ffff;
      w_length = length | 0x00008000;
      cgmPutIntNow(w_length);
      for (i1=0;i1<nx;i1++) {
        cgmPutByteNow(a[i*nx+i1]);
      }
      if (nx%2 != 0)  cgmPutByteNow(0);
    }
    if (nx%2 == 0)
      length = nx & 0x00007fff;
    else
      length = (nx+1) & 0x00007fff;
    w_length = length | 0x00000000;
    cgmPutIntNow(w_length);
    for (i1=0;i1<nx;i1++) {
      cgmPutByteNow(a[i*nx+i1]);
    }
    if (nx%2 != 0)  cgmPutByteNow(0);
    pointer = -1;
  }
}
