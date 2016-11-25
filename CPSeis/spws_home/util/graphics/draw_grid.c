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
#include <math.h>
#include "model.h"
#include "rmodc.h"
#include "dbutil.h"
#include "wrdcnvrt.h"

#ifdef CRAY
#define bswap_      BSWAP
#endif

#ifdef VMS
#define bswap_      bswap
#endif

void draw_grid(char *file);
/********
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from fortran or c
C
C     Utility Name:  draw_grid()
C          Written:  93/02/15  by:  R.S.Day
C     Last revised:  94/02/14
C
C  Purpose:       Read in and plot a gridded depth model.
C
C  Related Documentation:  other similar routines...
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C    --> Modify the information here as necessary.
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/graphics         graphlib.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  draw_grid.c      none
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:    graphlib.a  picklib.a
C  Header files: model.h     Transform.h
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 94/02/14  R.S.Day    Assuming IEEE on input instead of local word
C  1. 93/01/01  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C       void draw_grid(char *file)
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  char *  file    IN                Name of grid header file
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. The grid header and grid data files should be in CVM format. The 
C     CVM application can build the CVM grid files.
C  2. Call xopws, gsvp and gswn prior to draw_grid.
C-----------------------------------------------------------------------
C\END DOC
****/
void draw_grid(char *file)
{ErsModel   *temp_model;
 GridLimits *glimits;
 ErsTransform *tx,*ty,*tz,*t1,*t2,*t3;
 char  *hfile,*dfile,*tfile,*type;
 long  zero=0, one=1, four=4, istat,ix,iz;
 char  cxi[16], cyi[16], czi[16];
 long  key,rec,i,j,opstat,lut,lutold,nclr,iwk;
 long  n1,n2,n3,ntran,ierr,nc;
 long  local_word,word_in;
 char  hostname[32],os[32],msg[96];
 float o1,o2,o3,d1,d2,d3,scfac,eps;
 float gmin,gmax,*wbuf,xpos,zpos;
 float xx[4],zz[4],us[4],vp[4];

 if(file == NULL) return;
/*****************************************************
 * create temporary model to hold the grid model    **
 * Set the header file name before calling read     **
 ****************************************************/
 temp_model = new_model();
 if(temp_model == NULL)
  {destroy_model(temp_model);
   return;}
 model_sethfile(temp_model,file);
 
 cxi[0]='\0'; cyi[0]='\0'; czi[0]='\0';
 pcardrdhd(temp_model, cxi,cyi,czi, &istat);
 if(istat != 0)
  {printf("error reading header file\n");
   destroy_model(temp_model);
   return;
  }
/*****************************************************
 * Check model type and the grid limits for sanity  **
 ****************************************************/
 type = model_gettype(temp_model);
 if(strncmp(type,"GRID",4) != 0)
  {printf("Not a GRID file\n");
   destroy_model(temp_model);
   return;
  }
 glimits = model_getglim(temp_model);
 if(glimits == NULL)
  {printf("no grid limits \n");
   destroy_model(temp_model);
   return;
  }
 glimits_get(glimits, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             &t1, &t2, &t3 );
 if(n1 < 1 || n2 < 1)
  {printf("nx or nz < 1, empty file?\n");
   destroy_model(temp_model);
   return;
  }
 dfile = model_getdfile(temp_model);
 if(dfile == NULL)
  {printf("No data file component to the GRID file\n");
   destroy_model(temp_model);
   return;
  }
 /***************************************
  * Open an old grid file             ***
  **************************************/
  word_in    = model_getwtype(temp_model);
  if(word_in <= 0)  word_in    = WVMS;
  local_word = netw_lnode(hostname, os);
  key = 0;
  opstat = 1; /* try as an old file*/
  griddaop_(&key,dfile,&opstat,&n1,&ierr,&word_in);
  if(ierr != 0 || key < 1)
  {printf("Couldnt open grid file\n");
   destroy_model(temp_model);
   return;
  }

 /****************************************
  * SCAN THE DATA FOR MIN AND MAX VALUES *
  ***************************************/
  wbuf= (float *) calloc(1,n1*sizeof(float));
  if(key > 0)
   {long  dof,ierr;
    gmin= 1.0E+25;
    gmax= -1.0E+25;
    dof = 1; /* can generalize to multicomponent models */
    rec = 0;
    for(i=0;i<n2;i++)
     {rec ++;
      rmodrdc_(&key,&rec, &one, &n1, (char *)wbuf,
      &word_in,&ierr, msg);
      if(ierr != 0) break;
      if(local_word == WIEEE)
        nc = bswap_(&n1,(unsigned char *) wbuf);
      for(j=0;j<n1;j++)
       {if(wbuf[j] < gmin) gmin = wbuf[j];
        if(wbuf[j] > gmax) gmax = wbuf[j];
       }
     }
   }

 /****************************************
  *  PLOT THE MODEL USING GFA            *
  ***************************************/
  nclr = 16;
  iwk  = gqactive();
  scfac=(gmax-gmin);
  eps  = 0.001*(gmax-gmin);
  if (scfac != 0) scfac=(nclr-1)/scfac;
  gscr_gray(&iwk,&nclr);
  xpos=o2-0.5*d2;
  for(ix=0;ix<n2;ix++)
   {xx[0]=xpos;
    xx[1]=xx[0];
    xx[2]=xpos+d2;
    xx[3]=xx[2];
    rec  = ix+1;
    rmodrdc_(&key,&rec, &one, &n1, (char *)wbuf,
      &word_in,&ierr, msg);
    if(ierr != 0) break;
    if(local_word == WIEEE)
     nc = bswap_(&n1,(unsigned char *) wbuf);
    zpos=o1-0.5*d1;
    zz[0]=zpos;
    zz[1]=zz[0]+d1;
    zz[2]=zz[1];
    zz[3]=zz[0];
    for(iz=0;iz<n1;iz++)
     {zpos = o1 - 0.5*d1 + iz*d1;
      wbuf[iz]= scfac*(wbuf[iz]-gmin)  + 0.5;
      j = wbuf[iz];
      lut = nclr - j;
      if(iz == 0) lutold = lut;
      if(lut == lutold)
       {zz[1] += d1;
        zz[2] += d1;
       }
      else
       {gsfaci(&lutold);
        Xgfa(&four,xx,zz);
        zz[0]=zpos;
        zz[1]=zz[0]+d1;
        zz[2]=zz[1];
        zz[3]=zz[0];
        lutold = lut;
       }
     }
    gsfaci(&lut);
    Xgfa(&four,xx,zz);
    xpos=xpos+d2;
   }

/* Place a color bar at the left edge of the plot */
 lut = 1;
 gqcntn(&ierr,&ntran);
 gqnt(&ntran,&ierr,us,vp);
 vp[0] = vp[1] + 0.004;
 vp[1] += 0.035;
 draw_cbar( &nclr,&lut, &gmax, &gmin,
            &vp[0], &vp[1], &vp[3], &vp[2]);
 refresh_all(&iwk);

 free(wbuf);
 rmodclos_w_(&key);
 destroy_model(temp_model);
 return;
}

