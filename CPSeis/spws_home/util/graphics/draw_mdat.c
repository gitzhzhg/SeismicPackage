/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include <string.h>
#include "gplot.h"
#include "model.h"

void draw_mdat(long *iwk,ErsMaterial *mat, long *idraw, char *msg );

/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_mat
C          Written:  93/02/14  by:  R.S.Day
C     Last revised:            by:  
C
C  Purpose:     Plot the material control point data in ErsMaterial.
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/graphics         graphlib.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  draw_mdat.c      none                               
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:
C  Header files: model.h
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/01/01  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C  void draw_mdat(long *iwk,ErsMaterial *mat, long *idraw, char *msg );
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  long *  iwk     IN      > 1	     Integer flag of active plot window
C  long *  idraw   IN      0,1       Undraw, Draw the the data
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. see model.c and model_io.c for methods to manipulate an ErsCell
C     strucutre
C-----------------------------------------------------------------------
C\END DOC
****/
void draw_mdat(long *iwk,ErsMaterial *mat, long *idraw, char *msg )
{
  long npts,mid;
  float xi,zi;
  float XN,YN,XI,XJ,FAT;
  float xbuf[200],ybuf[200];
  float f1,f2,f3,xs,ys,xe,ye,dx;
  float r[4],g[4],b[4];
  int   i,num,style;
  char  *hname,*cname;
  unsigned int   lwidth;

  long  NC, IOP,l1,l2,l3,count;
  long  INDEXS=0, WHITE = 1, BLACK = 0, RED = 2, GRN=3;
  long  BACKG=MAXCOL;
  int   n;
  char  line[80];

  cname = material_getcolor(mat);
  hname = material_getname(mat);
  lwidth = 1;
  style = 1;
 
  if(mat == NULL )
   { printf("draw_mdat: no data to plot\n");
     return; }
/*--SET DEFAULT COLORS */
  NC = 4;
  r[0] = 0.0; g[0]=0.0; b[0]=0.0;
  r[1] = 1.0; g[1]=1.0; b[1]=1.0;
  r[2] = 1.0; g[2]=0.0; b[2]=0.0;
  r[3] = 0.0; g[3]=1.0; b[3]=0.0;
  gscr_Nrgb(iwk,&INDEXS,&NC,r,g,b);

  if(*idraw == 0) /* undraw old material? */
   { gsplci(&BACKG);
     gspmci(&BACKG);
     gstxci(&BACKG);
   }
  else            /* draw the material */
   { gsplci(&GRN);
     gspmci(&GRN);
     gstxci(&BLACK);
   }
/*--draw the material coordinates */
  if(mat != NULL)
   { int *stype,ndof,mid;
     float *x,*y,*pv;
     material_get(mat,&ndof,&mid,&npts,&stype,&x,&y,&pv);
     if(npts > 200)
      { printf("draw_mdat: error, more than 200 boundary pts.\n");
        return;
      }
     if(npts < 1) goto L10;
     l1 = 1; gsclip(&l1);
     count=0;
     xbuf[count] = x[0]; ybuf[count] = y[0];
     xs = xbuf[count]; xe = xbuf[count];
     ys = ybuf[count]; ye = ybuf[count];
     count++;
     for(n=1;n<npts;n++)
      { if(stype[n] == stype[n-1])
         {xbuf[count] = x[n]; ybuf[count] = y[n];
         }
        else
         {Xgpm(&count,xbuf,ybuf); /*PLOT CONTROL POINTS */
          Xvecgpl(&count,xbuf,ybuf,hname,cname,lwidth,style);
      /*    Xgpl(&count,xbuf,ybuf);*/
          count = 0;
          xbuf[count] = x[n]; ybuf[count] = y[n];
         }
        if(xbuf[count] < xs) xs = xbuf[count];
        if(xbuf[count] > xe) xe = xbuf[count];
        if(ybuf[count] < ys) ys = ybuf[count];
        if(ybuf[count] > ye) ye = ybuf[count];
        count++;
      }
     if(count != 0)
      {Xgpm(&count,xbuf,ybuf); /*PLOT CONTROL POINTS */
       Xvecgpl(&count,xbuf,ybuf,hname,cname,lwidth,style);
   /*    Xgpl(&count,xbuf,ybuf); */
      }
     dx = 0.01*(xe-xs);
     xs = xs-dx;
     xe = xe+dx;
     dx = 0.01*(ye-ys);
     ys = ys-dx;
     ye = ye+dx;
    /* refresh_box(iwk,&xs,&xe,&ys,&ye);*/      /* force CopyArea */
     l1 = 0; gsclip(&l1);
   }

L10:
 gsplci(&BLACK);
 gspmci(&BLACK);
 gstxci(&BLACK);
 return;
}
