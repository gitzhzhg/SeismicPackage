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
#include <string.h>
#include <stdlib.h>
#include "vect/ll_vector.hh"
#include "oprim/modbase_data.hh"
#include "vec_list_util.hh"
#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define gtol     GTOL
#endif
#ifdef NEED_UNDERSCORE
#define gtol     gtol_
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

int  grid_to_layr(VectorLinkedList *m_vlist,
     VectorLinkedList *c_vlist,
     int *nx, float *xorg, float *dx,
     int *nz, float *zorg, float *dz, float *garr);
void gtol(int *mone,int *nmats,int *mat_ids,int *hflag,
     int *ixv,int *nxv,float *xv,float *zv,float *pv,
     int *ncell,int *cell_ids,int *ixc,int *nxc,float *xc,float *zc,
     int *nx,float *xorg,float *dx,int *nz,float *zorg,float *dz,
     float *garr,int *mwrk,float *wrk,int *ierr);
#ifdef __cplusplus
}                   // for C++
#endif


/*********
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: grid_to_layr
C        Author: R.S.DAY
C       Written: 94/08/10
C  Last revised: 95/11/21
C
C  Purpose:      Takes gridded model information and sets the values
C                for velocity horizons in a layered velocity model.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C  int  grid_to_layr(VectorLinkedList *m_vlist,
C       VectorLinkedList *c_vlist,
C       int *nx, float *xorg, float *dx,
C       int *nz, float *zorg, float *dz, float *garr);
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C m_vlist    *         out      material information
C c_vlist    *         in       cell boundary information
C nx,nz      integer   in       number of x,z grid values
C dx,dz      float     in       x and z grid increment size
C xorg,zorg  float     in       x and z grid origins
C garr       float     in       nz by nx array of grid values
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. Calls D. Hansons gtol function
C 2. Uses  nx * nz values in the array garr[,]
C    nz is the fastest storage dimension!
C 3. Returns 1 if there were no problems
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 2.  95/11/21 R.S.Day    Bumped up the minimum scratch size
C 1.  94/08/09 R.S.Day    Initial version 
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
******/
int  grid_to_layr(VectorLinkedList *m_vlist,
     VectorLinkedList *c_vlist, 
     int *nx, float *xorg, float *dx,
     int *nz, float *zorg, float *dz, float *garr)
{
 long  nzf;
 long  nc;
 float dzf;

 int   mwrk,ierr;
 int   ncell;
 int   *ixc=NULL,*nxc=NULL,*cell_ids=NULL;
 float *xc=NULL,*yc=NULL,*zc=NULL;

 int   nmats;
 int   rdof,ndof=1,nvcrd;
 int   *ixv=NULL,*nxv=NULL,*mat_ids=NULL,*hflag=NULL;
 float *xv=NULL,*yv=NULL,*zv=NULL,*pv=NULL;


 float *wrk=NULL;
 int   mone = -1;
 int   m,dof;
 char  msg[96];
 int   istat;

 istat = 0;
 /****************************************
  * Check for necessary input data      **
  * Check for sane grid limits          **
  ***************************************/
 if(!c_vlist)
  { strcpy(msg,"grid_to_layr: NULL vector list for cells, bad input\n");
    goto error; }
 if(!m_vlist)
  { strcpy(msg,"grid_to_layr: NULL vector list for materials, bad input\n");
    goto error; }
 if(!garr)
  { strcpy(msg,"grid_to_layr: NULL grid, invalid input\n");
    goto error; }
 nc = VectorListCount((void *) c_vlist);
 if(nc <= 0)
  { strcpy(msg,"grid_to_layr: no cells defined, input inadequate\n");
    goto error; }
 nc = VectorListCount((void *) m_vlist);
 if(nc <= 0)
  { strcpy(msg,"grid_to_layr: no materials defined, input inadequate\n");
    goto error; }
 if(*nx < 1 || *nz < 1 )
  { strcpy(msg,"grid_to_layr: nx and/or nz < 1 for the grid\n");
    goto error; }
 if(*dx == 0. || *dz == 0. )
  { strcpy(msg,"grid_to_layr: dx and/or dz = 0 for the grid\n");
    goto error; }

 if(ndof == 0) // need to generalize this.
  { strcpy(msg,"grid_to_layr: degrees of freedom = 0\n");
    goto error; }

 /***************************************
  * Put cell info into arrays.         **
  * This call allocates memory.        **
  **************************************/
   ncell = VectorListCaccess((void *) c_vlist,&ncell,&cell_ids,&ixc,
           &nxc,&xc,&yc,&zc);
 /***************************************
  * Put material info into arrays.     **
  * This call allocates memory.        **
  **************************************/
  nvcrd = VectorListMaccess((void *) m_vlist, &rdof, &nvcrd,
           &nmats, &mat_ids, &ixv, &nxv, &hflag, &xv, &yv, &zv, &pv);
 if(nmats <= 0)
  { strcpy(msg,"grid_to_layr: no materials defined, input is inadequate\n");
    goto error; }

 /***************************************
  * Allocate work arrays for grid comp **
  **************************************/
 nzf = *nz;
 dzf = *dz;
 mwrk = 14*(*nx)*(*nz) + 100;
 if(mwrk < 750000) mwrk = 750000;
 wrk  = (float *) calloc(1,mwrk*sizeof(float));
 if(!wrk)
  {strcpy(msg,"grid_to_layr: calloc failed\n");
   istat=0; goto error;
  }
 /**********************************************************
  * Call function to compute the velocity values at the   **
  * velocity horizon positions given the grid data input. **
  *********************************************************/
 m = 0;
 for(dof=0;dof<ndof;dof++) /* loop over attributes */
  { /*    idir   */
   gtol(&mone, &nmats,mat_ids,hflag,ixv,nxv,xv,zv,pv,
    &ncell,cell_ids,ixc,nxc,xc,zc,
    nx,xorg,dx,nz,zorg,dz,garr+m,&mwrk,wrk,&ierr);
   m += (*nz)*(*nx);
   strcpy(msg,"grid_to_layr: gtol failure\n");
   if(ierr != 0) goto error;
  }

/********************************************************
  * Place the new velocity data back into m_vlist.     **
  ******************************************************/
  if(! VectorListMreset((void *) m_vlist, rdof, nvcrd,
       nmats, mat_ids, ixv, nxv, hflag, xv, yv, zv, pv))
   { strcpy(msg,"grid_to_layr: VectorListMreset failure\n");
     goto error; }

 istat = 1;
 error:
 if(wrk) free(wrk);
 if(cell_ids) free(cell_ids);
 if(ixc) free(ixc);
 if(nxc) free(nxc);
 if(xc) free(xc);
 if(yc) free(yc);
 if(zc) free(zc);
 if(mat_ids) free(mat_ids);
 if(ixv) free(ixv);
 if(nxv) free(nxv);
 if(hflag) free(hflag);
 if(xv) free(xv);
 if(yv) free(yv);
 if(zv) free(zv);
 if(pv) free(pv);
 if(!istat) printf("%s",msg);
 return istat;
}
