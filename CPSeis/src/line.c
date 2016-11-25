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
! Name       : line
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

char line_ident[100] = 
"$Id: line.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include "cgm_common.h"

static int ndashs;
static float dashs[16];
static int id, isw = 0;
static float dsh, gap;
static float tic_len = 1.0;
static float htic = 1.0;

/* Query line type */
void cgmGqln(int *ierr, int *ln)
{
  *ln = linetype;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqln: %d %d \n",*ierr,*ln);
}

/* Query line width scale factor */
void cgmGqlwsc(int *ierr, float *sf)
{
  *sf = linewidth;
  *ierr = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqlwsc: %d %f \n",*ierr,*sf);
}

/* Query line scale factor, line type, and line color */
void cgmGqline(int *ln, float *sf, int *icol)
{
  *ln = linetype;
  *sf = linewidth;
  *icol = linecolor;
  if (*icol == 512) *icol = 0;
  if (cgmdebug) fprintf(err_unit,"cgm_gqline: %d %f %d \n",*ln,*sf,*icol);
}

/* Set line type */
void cgmGsln(int lt)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gsln: %d \n",lt);
  if (linetype != lt) {
     if (lt >= 1 && lt <= 4) {
        cgmPutInt(lt);
        cgmPutData(5,2);
     }
     else {
        if (linetype != 1) {
           cgmPutInt(1);
           cgmPutData(5,2);
        }
     }
     linetype = lt;
  }
}

/* Set line width scale factor */
void cgmGslwsc(float sftmp)
{
  float sf;

  if (cgmdebug) fprintf(err_unit,"cgm_gslwsc: %f \n",sftmp);
  sf = sftmp;
  if (sf < 1.0) sf = 1.0;
  if (linewidth != sf) {
     linewidth = sf;
     cgmPutReal(linewidth);
     cgmPutData(5,3);
  }
}

/* Routine to draw a dashed line through n vertices
**             n - Number of vertices
**             x - x array of vertices
**             y - y array of vertices
*/
void cgmDashLine(int n, float x[], float y[])
{
  int i, j;
  float next_x,next_y,dx,dy,d,d1;
  float xh = 0.0;
  float yh = 0.0;

  if (linecolor == -1) cgmGsplci(1);
  if (n == 1) {
     i = 1;
     if (isw == 1) {
        next_x = x[i];
        next_y = y[i];
        dx = next_x - current_x;
        dy = next_y - current_y;
        d = sqrt(dx*dx+dy*dy);
        if (d > 0.) {
           xh = dx / d;
           yh = dy / d;
        }
     }
     else if (isw == 2) {
        next_x = x[i];
        next_y = y[i];
        dx = next_x - current_x;
        dy = next_y - current_y;
        d = sqrt(dx*dx+dy*dy);
        if (d > 0.) {
           xh = dx / d;
           yh = dy / d;
        }
     else
        return;
     }
  }
  else if (n == 0) {
     current_x = x[0];
     current_y = y[0];
     id = 0;
     dsh = dashs[0];
     isw = 1;
     return;
  }
  else {
    i = 2;
    id = 0;
    current_x = x[0];
    current_y = y[0];
    next_x = x[1];
    next_y = y[1];
    dx = next_x - current_x;
    dy = next_y - current_y;
    d = sqrt(dx*dx+dy*dy);
    if (d > 0.) {
       xh = dx / d;
       yh = dy / d;
    }
    cgmPutPoint(current_x,current_y);
    dsh = 0.0;
    gap = 0.0;
    if (dashs[id] < 0.0) {
       dsh = ABS(dashs[id]);
       gap = 0.0;
    }
    else if (dashs[id] > 0.0) {
       dsh = 0.0;
       gap = dashs[id];
    }
  }

  while (i < n) {

/* ... Draw part of dash */
    while (dsh > 0.) {
      d1 = MIN(dsh,d);
      current_x = current_x + d1 * xh;
      current_y = current_y + yh * d1;
      cgmPutPoint(current_x,current_y);
      dsh = dsh - d1;
      d = d - d1;

      if (d > 0.) break;
         i = i + 1;
      if (i > n) {
         isw = 1;
         cgmPutData(4,2);
         return;
      }

/* ... Prepare for new point for dash */
      next_x = x[i];
      next_y = y[i];
      dx = next_x - current_x;
      dy = next_y - current_y;
      d = sqrt(dx*dx+dy*dy);
      if (d > 0.) {
         xh = dx / d;
         yh = dy / d;
      }
    }

/* ... Draw new gap */
    while (gap > 0.) {
      d1 = MIN(gap,d);
      current_x = current_x + d1 * xh;
      current_y = current_y + yh * d1;
      gap = gap - d1;
      d =d - d1;
      cgmPutPoint(current_x,current_y);

      if (d > 0.) break;
      i = i + 1;
      if (i > n) {
         isw = 2;
         cgmPutData(4,2);
         return;
      }

/* ... Prepare for new gap point */
      next_x = x[i];
      next_y = y[i];
      dx = next_x - current_x;
      dy = next_y - current_y;
      d = sqrt(dx*dx+dy*dy);
      if (d > 0.) {
         xh = dx / d;
         yh = dy / d;
      }
    }

    j = 0;
    while (j == 0) {
      id = id + 1;
      if (id > ndashs-1) id = 0;
      if (dashs[id] < 0.0) {
         dsh = ABS(dashs[id]);
         gap = 0.0;
         j = 1;
      }
      else if (dashs[id] > 0.0) {
         dsh = 0.0;
         gap = dashs[id];
         j = 1;
      }
    }
  }
}


void cgmSetDashline(float dash_s[], int ndash_s)
{
  int i;

  if (cgmdebug) fprintf(err_unit,"cgm_set_dashline: \n");
  dshflg = TRUE;
  ndashs = ndash_s;
  if (ndashs <=  0) return;
  for (i=0;i<ndashs;i++) {
      dashs[i] = dash_s[i];
  }

  id=1;
  while (id <= ndashs) {
    if (dashs[id] < 0.0) {
       dsh = ABS(dashs[id]);
       isw = 1;
    }
    else if (dashs[id] > 0.0) {
       gap = dashs[id];
       isw = 2;
    }
    else {
       id = id + 1;
    }
  }
}

/* -- Draw a line and put a tickmark every TICLEN distance HTIC is the text
** -- size of tic mark.
*/
void cgmTicLine(int n, float x[], float y[])
{
  int i;
  float tic,ticlen,d1,next_x,next_y,dx,dy,d;
  float xh = 0.0;
  float yh = 0.0;

  if (linecolor == -1) cgmGsplci(1);
  ticlen = tic_len;
  d = 0.0;
  if (n == 1) {
     i = 0;
     cgmPutPoint(current_x,current_y);
  }
  else if (n == 0) {
     tic = ticlen;
     return;
  }
  else {
     current_x = x[0];
     current_y = y[0];
     i = 1;
     tic = ticlen;
  }

  while (i < n) {
    while (tic > 0.0) {
      d1 = MIN(tic,d);
      current_x = current_x + d1 * xh;
      current_y = current_y + yh * d1;
      cgmPutPoint(current_x,current_y);
      tic = tic - d1;
      d = d - d1;
      if (d > 0.0) break;
      i = i + 1;
      if (i > n) {
         cgmPutPoint(current_x,current_y);
         return;
       }
       next_x = x[i];
       next_y = y[i];
       dx = next_x - current_x;
       dy = next_y - current_y;
       d = sqrt(dx*dx+dy*dy);
       if (d > 0.0) {
         xh = dx / d;
         yh = dy / d;
       }
    }
    cgmPutPoint(current_x-yh*htic,current_y+xh*htic);
    cgmPutPoint(current_x,current_y);
    tic = ticlen;
  }
}

void cgmSetTicLine(float tic1, float htic1)
{
  if (cgmdebug) fprintf(err_unit,"cgm_set_tic_line_parms: \n");
  tic_len = tic1;
  htic = htic1;
}

/* Routine to draw a polygon line through n vertices
**             n - Number of vertices
**             x - x array of vertices
**             y - y array of vertices
*/
void cgmGpl(int n, float x[], float y[])
{
  int i;
  int ntmp;
  float tmpx[8192],tmpy[8192];

  if (cgmdebug) {
    fprintf(err_unit,"cgm_gpl: %d \n",n);
     for (i=0;i<n;i++) {
       fprintf(err_unit,"         %f %f \n",x[i],y[i]);
     }
  }
  if (linecolor == -1) cgmGsplci(1);
  if (n == 0) {
     current_x = x[0];
     current_y = y[0];
  }
  else if (n == 1) {
     cgmPutPoint(current_x,current_y);
     cgmPutPoint(x[0],y[0]);
     cgmPutData(4,1);
     current_x = x[0];
     current_y = y[0];
  }
  else {
    ntmp = n;
    for (i=0;i<n;i++) {
        tmpx[i] = x[i];
        tmpy[i] = y[i];
    }
    if (dshflg && linetype == 5) {
       cgmDashLine (ntmp,tmpx,tmpy);
    }
    else if (linetype == 6) {
       cgmTicLine(ntmp,tmpx,tmpy);
       cgmPutData(4,1);
    }
    else {
       for (i=0;i<ntmp;i++) {
           cgmPutPoint(tmpx[i],tmpy[i]);
       }
       cgmPutData(4,1);
       current_x = x[n-1];
       current_y = y[n-1];
    }
  }
}
