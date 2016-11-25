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
#include "model.h"
#include "gplot.h"

void draw_cell(long *iwk,ErsCell *cell, long *idraw, char *msg );
void draw_cell_label(long *iwk,ErsCell *cell, long *idraw, char *msg );
void draw_cell_labels(long *iwk,ErsCells *cells,long *idraw,char *msg);

/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_cell
C          Written:  93/01/01  by:  R.S.Day
C     Last revised:            by:  
C
C  Purpose:     Plot the boundary data in ErsCell and fill the interior.
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
C  draw_cell.c      none                               
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
C  2. 93/03/12  R.S.Day    Changed the way color is obtained
C  1. 93/01/01  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C  void draw_cell(long *iwk,ErsCell *cell, long *idraw, char *msg );
C  void draw_cell_label(long *iwk,ErsCell *cell, long *idraw, char *msg );
C  void draw_cell_labels(long *iwk,ErsCells *cells,long *idraw,char *msg);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  long *  iwk     IN      > 1	     Integer flag of active plot window
C  long *  idraw   IN      0,1       0 = Undraw cell and label
C                                    1 = Draw the cell and label
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. see model.c and model_io.c for methods to manipulate an ErsCell
C     strucutre
C-----------------------------------------------------------------------
C\END DOC
****/
void draw_cell(long *iwk,ErsCell *cell, long *idraw, char *msg )
{ char  str[16];
  float xb[500],zb[500],xi,zi;
  float XN,YN,XI,XJ,FAT;
  float r[3],g[3],b[3];
  float f1,f2,f3,xs,ys,xe,ye;
  long  npts,mid;
  long  IPATH,NCH,NC,ISTAT,IOP,l1,l2,l3,NPIXY,NPIX;
  long  N,INDEXS=0, WHITE = 1, BLACK = 0, RED = 2, BACKG=MAXCOL;

  msg[0]='\0';
  if(cell == NULL )
   { sprintf(msg,"draw_cell: no cell to plot\n");
     return; }
/*--SET DEFAULT COLORS */
  N=3;
  r[0] = 0.0; g[0] = 0.0; b[0]=0.0;
  r[1] = 1.0; g[1] = 1.0; b[1]=1.0;
  r[2] = 1.0; g[2] = 0.0; b[2]=0.0;
  gscr_Nrgb(iwk,&INDEXS,&N,r,g,b);

  gsplci(&BLACK);
  if(*idraw == 0) /* undraw old cell? */
   { gsfaci(&BACKG);
     gstxci(&BACKG);
   }
  else            /* hilight the cell */
   { gsfaci(&RED);
     gstxci(&BLACK);
   }
/*--draw the cell and labels */
  if(cell != NULL)
   { long npix,dir;
     cell_get_bnd(cell,&npts,xb,zb);
     cell_get(cell,&xi,&zi,&mid);
     if(npts > 500)
      { sprintf(msg,"draw_cell: more than 500 boundary pts.\n");
        return;
      }
     l1 = 1; gsclip(&l1);
     if(npts > 0);
      { Xgfa(&npts,xb,zb);
        sprintf(str,"%d",mid); npix = 10; dir=0;
        draw_str_(str,&xi,&zi,&npix,&dir);
      }
     gsfaci(&BLACK);
     gstxci(&BLACK);
     Xgpl(&npts,xb,zb); /*PLOT CELL BOUNDARY */
     l1 = 0; gsclip(&l1);
    }
 gsfaci(&BLACK);
 gstxci(&BLACK);
 refresh_all(iwk);
 return;
}

void draw_cell_labels(long *iwk,ErsCells *cells, long *idraw, char *msg )
{int i;
 ErsCell *cell;
 if(cells == NULL) return;

 for(i=0;i<cells_count(cells);i++)
   { cell  = cells_get_nth(cells, i);
    draw_cell_label(iwk,cell,idraw, msg);
   }

}

void draw_cell_label(long *iwk,ErsCell *cell, long *idraw, char *msg )
{ char  str[16];
  float xi,zi;
  float XN,YN,XI,XJ,FAT;
  float r[3],g[3],b[3];
  float f1,f2,f3,xs,ys,xe,ye;
  long  npts,mid;
  long  IPATH,NCH,NC,ISTAT,IOP,l1,l2,l3,NPIXY,NPIX;
  long  N,INDEXS=0, WHITE = 1, BLACK = 0, RED = 2, BACKG=MAXCOL;

  msg[0]='\0';
  if(cell == NULL )
   { sprintf(msg,"draw_cell: no cell to plot\n");
     return; }
/*--SET DEFAULT COLORS */
  N=3;
  r[0] = 0.0; g[0] = 0.0; b[0]=0.0;
  r[1] = 1.0; g[1] = 1.0; b[1]=1.0;
  r[2] = 1.0; g[2] = 0.0; b[2]=0.0;
  gscr_Nrgb(iwk,&INDEXS,&N,r,g,b);

  gsplci(&BLACK);
  if(*idraw == 0) /* undraw old cell? */
   { gstxci(&BACKG);
   }
  else            /* hilight the cell */
   { gstxci(&BLACK);
   }
/*--draw the cell labels*/
  if(cell != NULL)
   { long npix,dir;
     cell_get(cell,&xi,&zi,&mid);
     l1 = 1; gsclip(&l1);
      sprintf(str,"%d",mid); npix = 10; dir=0;
      draw_str_(str,&xi,&zi,&npix,&dir);
     gstxci(&BLACK);
     l1 = 0; gsclip(&l1);
   }
 gstxci(&BLACK);
 return;
}
