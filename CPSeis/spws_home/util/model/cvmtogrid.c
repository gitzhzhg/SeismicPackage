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
#include "tfdefs.h"
#include "model_io.h"

#ifdef CRAY
#define cvmtogrid_ CVMTOGRID
#endif
#ifdef VMS
#define cvmtogrid_ cvmtogrid
#endif


/*********
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: cvmtogrid_
C        Author: R.S.DAY
C       Written: 93/05/26
C  Last revised: 
C
C  Purpose:      Reads in a CVM file, computes a gridded model, and
C                saves the gridded model to disk.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C    int cvmtogrid(_char *infile, char *outfile )
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C infile     char *     input    Name of the CVM header file.
C outfile    char *     in&out   Name of the grid header file.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. The input file must be a complete CVM file.
C 2. outfile will be accepted verbatim but, the data file
C    will have the extension gdata added.
C 3. Returns 0 if there were no problems
C 4. This routine is independent of X-windows and a graphics display
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 1.  93/02/24 R.S.Day    Corrected cell summation logic bug
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
******/
int cvmtogrid_(char *infile, char *outfile)
{
 ErsModel         *model_temp;
 PR_              *pr_new;
 ErsCells         *cells;
 ModLimits        *mlimits;
 GridLimits       *glim;
 UserInfo         *us;
 ErsTransform     *tx,*ty,*tz;
 ErsTransform     *t1,*t2,*t3;
 float xmin,xmax,zmin,zmax,ymin,ymax;
 int  nx,nz,ny;
 float xginc,zginc,yginc,xorg,zorg,yorg;
 char str[80],msg[120];
 long nseg,npicks;
 int  ierr,tru_ftype;
 float x_small,x_big,z_small,z_big;
 float *garr;
 char  gfil[96],hfil[96];

 ierr = 1;
 if(infile == NULL || outfile == NULL) return ierr;
 if(strcmp(infile,"NONE") == 0) return ierr;
 if(strcmp(infile,"none") == 0) return ierr;
 if(strlen(infile)==0) return ierr;
 if(strncmp(infile," ",1) == 0) return ierr;
 if(strcmp(outfile,"NONE") == 0) return ierr;
 if(strcmp(outfile,"none") == 0) return ierr;
 if(strlen(outfile)==0) return ierr;

 /**********************************
  * Allocate memory for a model   **
  *********************************/
 model_temp = new_model();
 if(model_temp == NULL) { ierr=4; return ierr; }

 /**********************************
  * Read in the data              **
  *********************************/
 if(!pcardrd(model_temp, infile, &tru_ftype))
  {strcpy(msg,"cvmtogrid: Error, Model file not read in");
   printf("%s\n",msg);
   destroy_model(model_temp);
   ierr = 2;
   return ierr;
  }

 /**********************************
  * Check the file type           **
  *********************************/
 if(tru_ftype != LAYER_TYPE)
  { strcpy( msg,"cvmtogrid: Error, file not of type CVM");
    printf("%s\n",msg);
    destroy_model(model_temp);
    ierr = 3;
    return ierr;
  }

 /*********************************************
  * Scan picks for the min & max limits.     **
  * Set model limits=data limits?            **
  ********************************************/
 pr_new = (PR_ *) model_getpdata(model_temp);
 ErsPRMinMax(pr_new,&x_small,&x_big,
           &z_small, &z_big);
 mlimits = model_getmlim(model_temp);
 mlimits_get(mlimits, &xmin, &xmax,
     &zmin, &zmax, &ymin, &ymax,&tx,&ty,&tz );
 if(xmin == xmax)
  { xmin = x_small; xmax = x_big;}
 if(zmin == zmax)
  { zmin = z_small; zmax = z_big;}
 mlimits_set(mlimits, &xmin, &xmax,
     &zmin, &zmax, &ymin, &ymax, tx, ty, tz );

/***************************************
 * Compute the cell boundarys and     **
 * connect to the id values.          **
 **************************************/
 ierr  = (int) cell_bndid(model_temp,&ierr);
 if(ierr != 0) goto jump1;
 /*cells = (ErsCells *) cell_comp(model_temp);
 model_rep_cdata(model_temp,cells);
*/

/********************************************
 * Get grid limits and compute grid        **
 *******************************************/
 glim = (GridLimits *) model_getglim(model_temp);
 glimits_get(glim,
     &nz, &zorg, &zginc,
     &nx, &xorg, &xginc,
     &ny, &yorg, &yginc,
     &t1, &t2, &t3 );
 
 strcpy(hfil,outfile);
 strcpy(gfil,outfile);
 addext_rep_(gfil,"gdata", &ierr);
 /* Compute grid and dump to disk */
 /* Set file names before going to grid_comp */
 model_sethfile(model_temp,hfil);
 model_settfile(model_temp,hfil);
 model_setdfile(model_temp,gfil);

 garr = NULL; /* suppress return of grid data */
 ierr = grid_comp(model_temp,
                  &nx,&xorg,&xginc,
                  &nz,&zorg,&zginc, garr);
 strcpy(outfile,hfil);
 glimits_get(glim,
     &nz, &zorg, &zginc,
     &nx, &xorg, &xginc,
     &ny, &yorg, &yginc,
     &t1, &t2, &t3 );

 jump1:
 free_model(model_temp);
 if(ierr != 0)
  {char msg[88];
   strcpy(msg,"cvmtogrid: FAILED TO GENERATE A GRIDDED MODEL\n");
   printf("%s",msg);
  }
 else
  {char msg[88];
   strcpy(msg,"cvmtogrid:GENERATED A GRIDDED MODEL\n");
   ierr = 0;
   printf("%s",msg);
  }

 
 return ierr;
}
