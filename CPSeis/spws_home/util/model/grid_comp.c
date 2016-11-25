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
#include "model_io.h"
#include "mod_access.h"
#include "wrdcnvrt.h"
#include "dbutil.h"
#include "tfdefs.h"
#include "rmodc.h"
#include "c2f_interface.h"

#define MC   3500

#ifdef NEED_CAPITALS
#define gtolgtoc    GTOLGTOC
#define gtol        GTOL
#endif
#ifdef NEED_UNDERSCORE
#define gtolgtoc    gtolgtoc_
#define gtol        gtol_
#endif

int  grid_comp(ErsModel *model,
     int  *nx, float *xorg, float *dx,
     int  *nz, float *zorg, float *dz, float *garr);
void grid_scale_cells(ErsCells *cells,ErsTransform *txo,ErsTransform
*tzo  );

/*********
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: grid_comp
C        Author: R.S.DAY
C       Written: 93/01/22
C  Last revised: 96/03/27
C
C  Purpose:      Takes model geometry , and the velocity information
C                and constructs a gridded model. Will save data to a
C                file if the file names are set in model. Will return
C                grid array to caller if caller wants.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C    int grid_comp(ErsModel *model, int  *nx, float *xorg, float *dx,
C                   int  *nz, float *zorg, float *dz, float *garr);
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C model      ErsModel  INPUT    A model with the geometry defined
C nx         integer   in       number of x grid values to output
C nz         integer   in       number of z grid values to output
C dx,dz      float     in       x and z grid increment size
C xorg,zorg  float     in       x and z grid origins
C garr       float     out      Optional array for returned grid values
C                               Pass in NULL to ignore.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. Calls D. Hansons gtolgtoc function
C 2. Places nx * nz output values into the array garr[,]
C    nz is the fastest storage dimension! If and only if garr !=NULL.
C    No grid values are returned if garr = NULL 
C 3. If the model describes more than 1 attribute per grid point, then
C    the number of grid points generated will be ndof*nx*nz. ndof is
C    the number of degrees of freedom in the description of material 0.
C    All nx vectors for an attribute are grouped sequentially on disk. 
C 4. Returns 0 if there were no problems
C 5. Output is suppressed if the model header file name is set to NULL.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 13. 96/03/27 R.S.DAY    Compute withe 1/V (slowness)  and output V.
C 12  95/11/21 R.S.DAY    Increased min work buffer
C 11. 94/09/28 R.S.DAY    Added dbutil header file
C 10. 94/06/21 R.S.DAY    Replaced griddaop by rmodopen_w
C 9.  94/03/08 R.S.DAY    Honors models word_type flag for output files
C 8   94/03/02 R.S.DAY    Converted gridwrec call to rmodwrc
C 7.  94/02/14 R.S.Day    Output file as ansi ieee floats.
C 6.  93/08/15 R.S.Day    Further correction of normed sum
C 5.  93/08/12 R.S.Day    Corrected normalization of grid sum
C 4.  93/07/27 R.S.Day    Use fine grid and average to output grid.
C 3.  93/07/23 R.S.Day    Allowed NULL file name to supress writes.
C                         Data extension changed to "grid".
C                         Initializing garr to zero internally.
C 2.  93/03/11 R.S.Day    Corrected problem with grid limits
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
int grid_comp(ErsModel *model,
     int  *nx, float *xorg, float *dx,
     int  *nz, float *zorg, float *dz, float *garr)
{ModLimits    *mlimits;
 GridLimits   *glimits;
 ErsMaterials *mats;
 ErsMaterial  *mat;
 ErsCells     *cdata;
 ErsCell      *cell;

 ErsTransform *tx,*ty,*tz,*t1,*t2,*t3;
 int   mfac;
 int   n1,n2,n3,nrecl,nc;
 int   local_word,word_type;
 int   key;
 char  hostname[32],os[32];
 int   iform  = 1; /* unformatted */
 int   iaccess= 1; /* direct access or c stream */

 float o1,o2,o3,d1,d2,d3,adx,adz;
 float xmin,xmax,ymin,ymax,zmin,zmax;

 int   mwrk,ierr;
 int   ierr1;
 int   ncell;

 int   *ixc,*nxc,*cell_ids;
 float *xc,*zc;

 int   nmats;
 int   rdof,ndof,nvcrd;
 int   *ixv,*nxv,*mat_ids,*hflag;
 float *xv,*zv,*pv;


 float xgpt, *vcol,*wrk = NULL;

 int   il,ix,iz;
 int   npt,ngp,rec;
 int   one = 1;
 int   i,m,dof;
 char  msg[96],gfil[96];
 char  *hf,*gf;
 int   istat;
 int   opstat = 0; /* open as new */

 istat = 1;
 /****************************************
  * Check for necessary model components**
  * Check for sane grid limits          **
  ***************************************/
 if(model == NULL )
  { strcpy(msg,"grid_comp: NULL model, no input for calculation\n");
    printf("%s",msg);
    return istat;

   }
 cdata = model_getcdata(model);
 ncell = cells_count(cdata);
 for(i=0;i<ncell;i++)
   {cell = cells_get_nth(cdata,i);
    npt = 0;
    if(cell) npt = cell_getnum(cell);
    if(npt<3)
     {strcpy(msg,"grid_comp: bad cell boundary\n");
      printf("%s",msg);
      return istat;
     }
   }
 if(ncell <= 0 || !cdata)
  { strcpy(msg,"grid_comp: no cells defined, input is inadequate\n");
    printf("%s",msg);
    return istat;
  }
 mats = model_getmdata(model);
 nmats= materials_count(mats);
 mat  = mats_get_nth(mats,0);
 ndof = material_getdof(mat);
 if(nmats <= 0)
  { strcpy(msg,"grid_comp: no materials defined, input is inadequate\n");
    printf("%s",msg);
    return istat; }
 if(*nx < 1 || *nz < 1 )
  { strcpy(msg,"grid_comp: nx and/or nz < 1 for the grid\n");
    printf("%s",msg);
    return istat; }
 if(*dx == 0. || *dz == 0. )
  { strcpy(msg,"grid_comp: dx and/or dz = 0 for the grid\n");
    printf("%s",msg);
    return istat; }
 if(ndof == 0)
    { strcpy(msg,"grid_comp: degrees of freedom = 0\n");
    printf("%s",msg);
    return istat; }

 /*************************************
  * Retrive old Grid limits          **
  * n1 for x and n3 for z.           **
  ************************************/
 glimits = model_getglim(model);
 glimits_get(glimits,&n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             &t1, &t2, &t3 );
 mlimits = model_getmlim(model);
 mlimits_get(mlimits, &xmin, &xmax,
     &zmin, &zmax, &ymin, &ymax, &tx, &ty, &tz );

 /***************************************
  * Open a grid file.                  **
  * force file extension to be hgrid.  **
  **************************************/
  word_type = model_getwtype(model);
  if(word_type < 1) word_type = WIEEE;
  key = 0;
  hf = model_gethfile(model);
  gf = NULL;
  if(hf == NULL)
   { strcpy(msg,"grid_comp: NULL header file, no output file written\n");
    printf("%s",msg);
   }
  else
   {strcpy(gfil,hf);
    addext_rep_(gfil,"grid",&ierr1);
    gf = gfil;
    opstat = 0; /* try as a new file*/
    nrecl = *nz;
    rmodopen_w_(&key,gfil,&iform,&opstat,&iaccess,&nrecl,&word_type,&ierr);
    if(ierr != 0)
     {opstat=1; /* try as an old file(will be readonly)*/
      rmodopen_w_(&key,gfil,&iform,&opstat,&iaccess,&nrecl,&word_type,&ierr);
     }
    if(ierr != 0) { gf = NULL; key = 0; }
   }
  istat = dbutil_putwordtype_(&key,&word_type);
  model_setdfile(model,gf);
  model_settfile(model,hf);
 /***************************************
  * Put cell info into arrays.         **
  * This call allocates memory.        **
  **************************************/
   ncell = access_cells(cdata,&ncell,&cell_ids,&ixc,&nxc,&xc,&zc);
 /***************************************
  * Put material info into arrays.     **
  * This call allocates memory.        **
  * Invert velocity to slowness        **
  **************************************/
  nvcrd = access_mats(model, &rdof, &nvcrd,
          &nmats, &mat_ids, &ixv, &nxv, &hflag, &xv, &zv, &pv);
  for(i=0;i<rdof*nvcrd;i++) if(pv[i] != 0.0) pv[i]= 1.0/pv[i];
 /***************************************
  * Allocate work arrays for grid comp **
  **************************************/
 mfac= 1; /* let gtol do all smoothing */
 ngp = mfac * (*nx) * (*nz);
 vcol = (float *) calloc(1,ngp*sizeof(float));
 if(!vcol) {istat=1; goto error; }
 mwrk = 10*(*nz) + 10*(*nx) + 200;
 if(mwrk < 100000) mwrk = 100000;
 wrk  = (float *) calloc(1,mwrk*sizeof(float));
 if(!wrk)
  {printf(msg,"grid_comp: calloc failed for scratch\n");
   istat=1;
   goto error;
  }
 local_word = netw_lnode(hostname,os);
 /**********************************************************
  * Call function to compute the velocity values at the   **
  * grid points. Dump data to file if key is set.         **
  *********************************************************/
 if(garr != NULL)
  { for(il=0;il<ndof*ngp;il++) garr[il]=0.; }
 m = 0;
 
 adx = (*dx < 0) ? -*dx : *dx;
 adz = (*dz < 0) ? -*dz : *dz;
 for(dof=0;dof<ndof;dof++) /* loop over attributes */
  { 
   for(i=0;i<ngp;i++) { vcol[i]=0.0; }
    /* for(ix=0;ix<*nx;ix++)  loop over grid basement points */
   {xgpt = *xorg;
    if(xgpt <= xmin) xgpt += 0.0005*(xmax-xmin);
    if(xgpt >= xmax) xgpt -= 0.0005*(xmax-xmin);

/*            idir   */
   gtol(&one,(int  *) &nmats,mat_ids,hflag,ixv,nxv,xv,zv,pv,
     &ncell,cell_ids,ixc,nxc,xc,zc,
     nx,&xgpt,dx,nz,zorg,dz,vcol,&mwrk,wrk,&ierr);
   for(i=0;i<ngp;i++)
     {if(vcol[i] != 0.) vcol[i] = 1.0/vcol[i]; }
   if(garr != NULL)
     {for(i=0;i<ngp;i++) garr[m + i] = vcol[i]; 
     }
    rec = *nx*dof + 1;
    if(key > 0)
     {/* convert local_word to ansi ieee floats */
      if(local_word == WIEEE)
       nc = bswap_(&ngp,(unsigned char *)vcol);
       rmodwrc_(&key,&rec, nx, &ngp, (char *) vcol,
                &word_type,&local_word,&ierr, msg);
       if(ierr != 0) {istat = 2; goto error; }
     }
   }
    m += ngp;
  }

 /* reset the grid limits before we return */
 n3 = ndof;
 glimits_set(glimits,nz,zorg,dz, nx, xorg, dx, &n3,&o3,&d3, 
             t1,t2,t3);
 /* close the grid file and write the header file */
 model_settype(model,"GRID");
 model_setwtype(model,&word_type);
 if(key > 0)
  {rmodclos_w_(&key);
   opstat = 0; /* try as a new file*/
   pcardwrhd(model, &opstat, &istat);
   if(istat != 0)
    {opstat=1; /* try aS an old file */
     pcardwrhd(model, &opstat, &istat);
    }
  }

 istat = 0;
 error:
 free(vcol);
 if(wrk) free(wrk);
 free(cell_ids);
 free(ixc);
 free(nxc);
 free(xc);
 free(zc);
 free(mat_ids);
 free(ixv);
 free(nxv);
 free(hflag);
 free(xv);
 free(zv);
 free(pv);
 if(istat != 0) printf("%s",msg);
 return istat;
}

void grid_scale_cells(ErsCells *cells,ErsTransform *txo,ErsTransform
*tzo  )
{
 ErsCell      *cell;
 ErsTransform *tx,*ty,*tz;
 int i,j;

 if(cells == NULL) return;
 tx = cells_get_transx(cells);
 tz = cells_get_transz(cells);
 if(tx == NULL || tz == NULL) return;
 for(i=0;i<cells_count(cells);i++)
  { cell = cells_get_nth(cells,i);
    cell_scalex(cell,tx,txo);
    cell_scalez(cell,tz,tzo);
  }
 
 return;
}
