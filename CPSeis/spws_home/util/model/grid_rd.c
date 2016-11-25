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
#include "model_io.h"
#include "rmodc.h"
#include "dbutil.h"
#include "wrdcnvrt.h"

float *grid_rd(char *file, int  *start, int  *slices,
               int  *N1, float *O1, float *D1, char *lab1,
               int  *N2, float *O2, float *D2, char *lab2,
               int  *N3, float *O3, float *D3, char *lab3);

#ifdef CRAY
#define bswap_       BSWAP
#endif

#if( VMS || _AIX || hpux)
#define bswap_       bswap
#endif

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
C     Utility Name:  grid_rd()
C          Written:  93/02/15  by:  R.S.Day
C     Last revised:  94/02/14
C
C  Purpose:       Read in a gridded depth model.
C
C  Related Documentation:  other similar routines...
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C    --> Modify the information here as necessary.
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/model            libpick.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  grid_rd.c       none
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:    libpick.a
C  Header files: model.h     transform.h
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  1. 94/07/21  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C float *grid_rd(char *file, int  *start, int  *slices, 
C              int  *N1, float *O1, float *D1, char *lab1,
C              int  *N2, float *O2, float *D2, char *lab2,
C              int  *N3, float *O3, float *D3, char *lab2);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  char *  file    IN                Name of grid header file
C  int     start   in      >0        Starting slice number.
C  int     slices  in      >0        Maximum number of slices to return.
C  int     N1...   out               no. of grid pts in dimension 1,2,3.
C  float   D1...   out               grid increment in 1st,2nd,3rd dimen.
C  float   O1...   out               grid origins in 1st,2nd,3rd dimen.
C  char    lab1... out               coordinate description.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. The grid header and grid data files should be in CVM format. The 
C     CVM application can build the CVM grid files.
C  2. Grid limits will be set even if zero slices are read. The returned
C     grid limits indicate what is in the file not what is returned in
C     garr. 
C-----------------------------------------------------------------------
C\END DOC
****/
float *grid_rd(char *file, int  *start, int  *slices,
               int  *N1, float *O1, float *D1, char *lab1,
               int  *N2, float *O2, float *D2, char *lab2,
               int  *N3, float *O3, float *D3, char *lab3)
{ErsModel   *temp_model;
 GridLimits *glimits;
 ErsTransform *t1,*t2,*t3;
 char  *hfile,*dfile,*type;
 int  istat;
 char  cxi[16], cyi[16], czi[16];
 int   key=-1,rec,i,j,opstat,ierr;
 int   n1,n2,n3,nc,form,access;
 long  hdr1,hdr2,hdr3;
 int   local_word,word_in;
 char  hostname[32],os[32],msg[96];
 char *str;
 float o1,o2,o3,d1,d2,d3;
 float *wbuf = NULL;

 if(file == NULL) return NULL;
/*****************************************************
 * create temporary model to hold the grid model    **
 * Set the header file name before calling read     **
 ****************************************************/
 temp_model = new_model();
 if(temp_model == NULL)
  {destroy_model(temp_model);
   return NULL;}
 model_sethfile(temp_model,file);
 
 cxi[0]='\0'; cyi[0]='\0'; czi[0]='\0';
 pcardrdhd(temp_model, cxi,cyi,czi, &istat);
 if(istat != 0)
  {sprintf(msg, "grid_rd: error reading header file=%s\n",file);
   goto error;
  }
/*****************************************************
 * Check model type and the grid limits for sanity  **
 ****************************************************/
 type = model_gettype(temp_model);
 if(strncmp(type,"GRID",4) != 0)
  {sprintf(msg,"grid_rd: Not a GRID file");
   goto error;
  }
 glimits = model_getglim(temp_model);
 if(glimits == NULL)
  {sprintf(msg,"grid_rd: no grid limits");
   goto error;
  }
 glimits_get(glimits, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             &t1, &t2, &t3 );
 *N1 = n1; *O1 = o1; *D1 = d1;
 *N2 = n2; *O2 = o2; *D2 = d2;
 *N3 = n3; *O3 = o3; *D3 = d3;
 lab1[0]='\0'; lab2[0]='\0'; lab3[0]='\0';
 str = transform_getname(t1);
 if(str) strcpy(lab1,str);
 str = transform_getname(t2);
 if(str) strcpy(lab2,str);
 str = transform_getname(t3);
 if(str) strcpy(lab3,str);
 hdr1 = (long) transform_gethdr(t1);
 hdr2 = (long) transform_gethdr(t2);
 hdr3 = (long) transform_gethdr(t3);
 if(*start < 1) *start = 1;
 if(*start > n3) *start= n3;
 if(*slices< 1) *slices= 1;
 if(n3 < *start + *slices -1) *slices = n3 - *start + 1;
 if(n1 < 1 || n2 < 1 || n3 < 1)
  {sprintf(msg,"grid_rd: n1 or n2 or n3 < 1, i.e. empty file?");
   goto error;
  }
 dfile = model_getdfile(temp_model);
 if(dfile == NULL)
  {sprintf(msg,"grid_rd: No data file component to the GRID file");
   goto error;
  }
 /***************************************
  * Open an old grid file             ***
  **************************************/
  word_in    = model_getwtype(temp_model);
  if(word_in <= 0)  word_in    = WVMS;
  local_word = netw_lnode(hostname, os);
  key = -1;
  opstat = 1; /* try as an old file*/
  form   = 1; /* unformatted */
  access = 1; /* direct */
  rmodopen_w_(&key,dfile,&form,&opstat,&access,&n1,&word_in,&ierr);
  if(ierr != 0 || key < 1)
  {sprintf(msg,"grid_rd: Couldnt open grid file");
   goto error;
  }

 /****************************************
  * Read in the grid data                *
  ***************************************/
  wbuf= (float *) calloc(1,n1*n2*(*slices)*sizeof(float));
  if(wbuf == NULL) return NULL;
  if(key > 0)
   {int   num,cnt=0;
    float *oarr;
    num = n1*n2;
    for(i= *start -1;i< *start + *slices - 1 ;i++)
     {rec = i*n2 + 1;
      oarr = wbuf + cnt*num;
      rmodrdc_(&key,&rec, &n2, &n1, (char *) oarr,
        &word_in,&ierr, msg);
      if(local_word == WIEEE)
         nc = bswap_(&num,(unsigned char *) oarr);

      cnt++;
      if(ierr) goto error;
     }
   }


 rmodclos_w_(&key);
 destroy_model(temp_model);
 return wbuf;
error:
 if(wbuf) free(wbuf);
 if(key > 0) rmodclos_w_(&key);
 destroy_model(temp_model);
 printf("%s\n",msg);
 return NULL;
}

