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
#include <math.h>
#include "pick.h"
#include "gplot.h"

void draw_pdat(PR_ *pr, int *lorm, int *labels,
             long *iwk, float *ul, float *ur,float *ut, float *ub);
void draw_hdat(PR_ *pr, ErsHorizon *horizon, int *lorm);
void draw_sdat(ErsHorizon *horizon,int *lorm);

/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_pdat, draw_hdat, draw_sdat
C          Written:  93/01/01  by:  R.S.Day
C     Last revised:            by:  
C
C  Purpose:       Plot Picking Record, Horizon , or Segment data
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
C  draw_pdat.c      none                               
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:    picklib.a
C  Header files: Pick.h
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
C  void draw_pdat(PR_ *pr, int *lorm, int *labels,
C               long *iwk, float *ul, float *ur,float *ut, float *ub);
C  void draw_hdat(PR_ *pr, ErsHorizon *horizon, int *lorm);
C  void draw_sdat(ErsHorizon *horizon,int *lorm);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  int *   labels  IN      0,1       Data and axes, or data only
C  int *   lorm    IN      1,2,3     plot data as marks,lines or both 
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. draw_hdat will plot the segment "horizon" plus all segments with
C     the same name as "horizon". draw_sdat plots only the single
C     segment 'horizon'
C  2. See Pick.h for definition of PR_
C  3. See modelio.c & model_io.h for methods to input data
C  4. call xopws, gsvp, and gswn prior to these calls
C-----------------------------------------------------------------------
C\END DOC
****/
void draw_pdat(PR_ *pr, int *lorm, int *labels,
             long *iwk, float *ul, float *ur, float *ut, float *ub)
{ ErsHorizon       *uhlist[99];
  ErsTransform     *T;
  long n1,n2,iop,istat;
  long icol0,index,nclass,clid;
  float f1,f2,f3,r[2],g[2],b[2];
  float FAT = 1.0;
  long WHITE=1, LT=1;
  long BLACK=0;
  int   one = 1;
  int  zero = 0;
  int i;

/********************************************
 *          ul = x of left corner          **
 *          ur = x of right corner         **
 *          ut = z of upper corner         **
 *          ub = z of lower corner         **
 *******************************************/
  if(pr == NULL) return;

/*--SET UP A COLOR TABLE AND DEFAULT COLORS */
  iop=5;
  r[0] = 0.0; g[0] = 0.0; b[0] = 0.0;
  gscr_Nrgb(iwk,&BLACK,&one,r,g,b);
  r[0] = 1.0; g[0] = 1.0; b[0] = 1.0;
  gscr_Nrgb(iwk,&WHITE,&one,r,g,b);

  gsplci(&BLACK);
  gspmci(&BLACK);
  gstxci(&BLACK);

/*
 --BOX AND LABEL THE PICK PLOT?
 --LORM....1 FOR MARKS, 2 FOR LINES
 --LABELS..0 FOR BOX AND LABELS, 1 FOR PICKS ONLY
 */
 if(*labels == 0)
  { long linew;
    char  *xlabel,*ylabel,strx[16],stry[16];
    linew = 2;
    T = PR_x_transf(pr);
    xlabel = transform_getname(T);
    T = PR_z_transf(pr);
    ylabel = transform_getname(T);
    strcpy(strx,"X - AXIS");
    strcpy(stry,"Y - AXIS");
    if(xlabel != NULL && ylabel != NULL)
     draw_frame_(  ul, ur, ut, ub, &linew, xlabel, ylabel,"black" );
    else
     draw_frame_(  ul, ur, ut, ub, &linew, strx, stry,"black" );
  }

/*--PLOT ALL THE PICKS IN ALL THE HORIZONS */
 gsclip(&one);
 ErsHorizonGetHList(pr,&nclass, uhlist);
 for(i=0;i<nclass;i++)
  { clid = uhlist[i]->hid;
    draw_hdat(pr, uhlist[i], lorm);
  }
 gsclip(&zero);
/*
 refresh_all(iwk);
*/
 return;
}

void draw_hdat(PR_ *pr, ErsHorizon *horizon, int *lorm)
{ErsHorizon *hlist[99];
 ErsPoint *point;
 float x,z;
 int   one = 1;
 int   i,count,N;
 if(horizon == NULL || pr == NULL) return;
 
 ErsHorizonMemberList(pr, horizon, &count, &N, hlist);
 for(i=0;i<count;i++)
  { draw_sdat(hlist[i],lorm); }
 return;
}

void draw_sdat(ErsHorizon *horizon,int *lorm)
{ErsPoint *point;
 static long nbuf;
 float x[80],z[80];
 int   zero =0, one = 1;
 int   i,num,style;
 char  *hname,*cname;
 unsigned int   lwidth;
 if(horizon == NULL) return;
/* point = ErsHorizonGetFirstPoint(horizon); */
 hname = horizon->horizon_name;
 cname = horizon->color_name;
 lwidth= (unsigned int) horizon->line_width;
 style = 0;
 point = horizon->first_point;
 num = 0;
 while(point != NULL)
  {for(i=0;i<=point->npicks;i++)
    {if(i == point->npicks)
      { x[num]= point->pkey; z[num] = point->time; }
     else
      { x[num] = point->pick_list[i].pkey;
        z[num] = point->pick_list[i].time;
      }
     num++;
     if(num >= 80)
      {if(*lorm == 1) { Xgpm(&num,x,z); }
       else if(*lorm == 2)
        { Xvecgpl(&num,x,z,hname,cname,lwidth,style); }
       else
        { Xgpm(&num,x,z);
          Xvecgpl(&num,x,z,hname,cname,lwidth,style);
        }
       num = 0;
      }
    }
   point = point->next;
  /* point = ErsPointNext(point); */
  }
 if(num > 0)
 {if(*lorm == 1) { Xgpm(&num,x,z); }
  else if(*lorm == 2)
   { Xvecgpl(&num,x,z,hname,cname,lwidth,style); }
  else
   { Xgpm(&num,x,z);
     Xvecgpl(&num,x,z,hname,cname,lwidth,style);
   }
  num = 0;
 }
 return;
}
