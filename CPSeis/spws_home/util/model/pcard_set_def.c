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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ers_seis.h"
#include "pick.h"
#include "model.h"
 
/* 
/***********************************************************************
c        1         2         3         4         5         6         7
|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                      designed to be called from c
C
C   Primitive names: pcardSetDefaults
C                    
C  Source directory: ~spws/util/model/
C           Library: picklib.a
C           Written: 92/11/12   by:  R.S.Day
C      Last revised:            by:  
C
C  Purpose:      Scan header data and set some elements of the 
C                ErsModel structure
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C   Bool pcardSetDefaults(Widget W, ErsModel *)
C
C  Type    Name    I/O     Valid     Description
C  ----    ----    ---     -----     -----------
C  Widget  W        IN     non NULL  Widget ID of the CBYT D.A. widget
C  ErsModel*        IN               see model.h  C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. Looks at the range of header values in one panel of a CBYT display
C     and tries to set up some coordinate transformations. These
C     transformations are used in the the transform file of a  VBLD model
C     file. A VBLD 'file' may have seperate DATA, HEADER and TRANSFORM
C     files
C  2. Sets  - default model 
C             transformations for key CPS header words(1,7,17,37)
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 92/11/12  R. Day     Initial version
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:          XXXX
C  Header files:       Pcard.h ,Pick.h, ErsSeis.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC
C***********************************************************************/
Bool pcardSetDefaults(Widget W, ErsModel *model)
{ PR_ *pikrec;
  ModLimits        *mlimits;
  GridLimits       *glimits;
  ErsTransforms    *tdata;
  ErsTransform     *tx, *ty, *tz;
  int  Phdr, Shdr, Thdr, khdr, frame,i, header;
  int  numv, nhdrs, itrans;
  float *hd;
  char  string[24];
  float xmin,xmax, ymin,ymax, zmin,zmax;
  char  card[132],name[32];
  float p1,p2,s1,s2,t1,t2;
  float temp, srval, xval1, xval2, x1,x2;

 if(W     == NULL) return False;
 if(model == NULL) return False;
 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec == NULL) return False;

 /* Check if the transforms are already set. */
 /* Do not override if they  are set!        */
 tdata = model_gettdata(model);
 if(tdata == NULL) return False;
 if(ErsTrans_count(tdata) > 0) return False;

/* Get the header Key values  & header data        */
/* Set up some transformations based upon the data */
 Phdr = PR_GetPhdr(pikrec);
 Shdr = PR_GetShdr(pikrec);
 Thdr = PR_GetThdr(pikrec);
 ErsSeismicGethdDat(W,&hd,&nhdrs,&numv);
 pcardsettran( tdata,
               numv, hd,  nhdrs,
               Phdr, Shdr, Thdr);
 khdr = 1;
 if(Phdr != UNDEFINED_KEY) khdr = Phdr;
 ErsTransName(khdr, name);
 tx = ErsTransGetTran(tdata,name);
 tz = ErsTransGetTran(tdata,"KILOMETER");
 PR_setx_transf(pikrec,tx);
 PR_setz_transf(pikrec,tz);

/* Determine what the Pick input coordinates are */
/* Get the sample rate */
 ErsSeismicGetSampleRate(W,&srval);

/* Model limits description */
 ErsSeismicGetLimits(W, &frame,&xmin,&xmax,
     &p1, &p2, &s1, &s2,&t1, &t2,
     &zmin,&zmax);
 xval1 = xmin;
 xval2 = xmax;
 if(khdr == Phdr)
  { xval1 = p1; xval2 = p2; }
 if(xval1 > xval2)
  { temp = xval1;
    xval1= xval2;
    xval2= temp;
  }
 if(zmin > zmax)
  { temp = zmin;
    zmin = zmax;
    zmax = temp;
  }

 mlimits = model_getmlim(model);
 if(mlimits->xmin == mlimits->xmax)
  {ymin = 1.0; ymax = frame+1.0; ty = NULL;
   mlimits_set(mlimits, &xval1, &xval2, &zmin, &zmax, &ymin, &ymax,
     PR_x_transf(pikrec), ty, PR_z_transf(pikrec) );
  }
/* Default grid parameters */
 if(numv < 2) numv = 101;
 glimits = model_getglim(model);

 return True;
}

