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
#include "mod_access.h"

#ifdef CRAY
#define cellcalc_ CELL
#define cellpnt_w_ CELLPNT_W
#define cellwher_w_ CELLWHER_W
#endif
#if (VMS || _AIX || hpux)
#define cellcalc_ cell
#define cellpnt_w_ cellpnt_w
#define cellwher_w_ cellwher_w
#endif

#if ( ultrix || sun || __sgi)
#define cellcalc_ cell_
#endif

#define MAXA 200
#define MC   5500
#define MWRK 45000

ErsCells *cell_comp(ErsModel *model);
int cell_bndid(ErsModel *model, int *ierr);

/*********
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: cell_comp
C        Author: R.S.DAY
C       Written: 93/01/08
C  Last revised: 
C
C  Purpose:      Compute the cells for a model given the model geometry.
C                Takes digitized horizon information and computes
C                the boundarys of closed cellular regions in a model.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       ErsCells *cell_comp(ErsModel *model)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C model       ErsModel  INPUT    A model with the geometry defined
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. ErsCells * is returned ( see model.h for a definition )
C 2. coordinate system for the output cells is the same as for the
C    input model geometry.
C 3. Does not connect computed boudarys to material id values.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 1.  
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. Makes use of doug hansons cell subroutine
C-----------------------------------------------------------------------
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
******/
ErsCells *cell_comp(ErsModel *model)
{PR_ *pikrec;
 ModLimits *mlimits;
 ErsTransform *tx,*ty,*tz;
 ErsCells  *cnew;
 ErsCell   *cell1;
 int   maxa,mc,mwrk;
 int   ierr;
 int   *wrk=NULL;
 int   ncell,cell_id,ixcell[MAXA],nxcell[MAXA];
 float xcell[MC],zcell[MC];
 float *xc,*zc;

 int   i,j,m,istat;
 float *x1=NULL,*z1=NULL,*a1=NULL;
 int   *segpntr=NULL,*segptcnt=NULL;
 int   pdof;
 int   nseg,npik;

 float xmin,xmax,ymin,ymax,zmin,zmax;
 float rtol,xv;
 char  msg[96];
 extern void cellcalc_();

 istat = 1;
 cnew  = NULL;
 if(model == NULL) { return cnew; }
 /*************************************
  * Retrive & check the Model limits **
  ************************************/
 mlimits = model_getmlim(model);
 mlimits_get(mlimits,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax,
             &tx, &ty, &tz );
 rtol = .0001*( xmax - xmin );
 if(rtol < 0 )
  { float xv;
    rtol = -rtol;
    xv   = xmin;
    xmin = xmax;
    xmax = xv;
  }
    
 if(rtol == 0)
   { strcpy(msg,"cell_comp: Zero width model");
     goto L1001;
   }
 
 /*************************************
  * Retrive & check for picks        **
  ************************************/
 pikrec = (PR_ *) model_getpdata(model);
 PRCounts(pikrec,(long *) &nseg,(long *) &npik);
 if(nseg <= 0 || npik <= 0)
 {strcpy(msg,"cell_comp: No segments to process");
  goto L1001;
 }

 /***************************************
  * Rearrange the pick storage for cell**
  * preallocate memory                 **
  * Get unique horizon names & class ids*
  **************************************/
 pdof = 3;  /* safe limit ?*/
 x1 = (float *) calloc(1,npik*sizeof(float));
 z1 = (float *) calloc(1,npik*sizeof(float));
 a1 = (float *) calloc(1,npik*pdof*sizeof(float));
 segpntr = (int  *) calloc(1,(nseg+5)*sizeof(float));
 segptcnt= (int  *) calloc(1,(nseg+5)*sizeof(float));
 access_pdat_(model, &npik,&pdof,x1,z1,a1,&nseg,segpntr,segptcnt);

 wrk= (int  *) calloc(1,MWRK*sizeof(int ));
 if(!wrk) goto jump1;
 maxa  = MAXA;
 mc    = MC;
 mwrk  = MWRK;
 cellcalc_(&xmin,&xmax,&zmin,&zmax,
       &nseg,segpntr,segptcnt,x1,z1,
       &maxa,&mc,&ncell,ixcell,nxcell,xcell,zcell,
       &mwrk,wrk,&istat);
 if(istat != 0)
  { 
    strcpy(msg,"cell_comp: error status returned by cell\n");
    goto L1001;
  }

 cnew = new_cells();
 for(i=0;i<ncell;i++)
  {/* copy boundary data to structures */
   xc = &xcell[ixcell[i]];
   zc = &zcell[ixcell[i]];
   cell1 = new_cell();
   cell_set_bnd(cell1,nxcell[i],xc,zc);
   cells_add(cnew,cell1);
  }
 cells_set_transx(cnew,tx);
 cells_set_transz(cnew,tz);
 cells_set_transy(cnew,ty);

 jump1:
 istat = 0;
 
 L1001:
 if(x1) free(x1);
 if(z1) free(z1);
 if(a1) free(a1);
 if(segpntr) free(segpntr);
 if(segptcnt) free(segptcnt);
 if(wrk) free(wrk);
 if(istat != 0) printf("%s\n",msg);
 return cnew;
}

/******************************************************************
 * Compute the cell boundarys and link to id values.
 *****************************************************************/
int cell_bndid(ErsModel *model, int *ierr)
{ PR_ *pikrec;
  ErsCells         *new_cells,*old_cells;
  ErsCell          *cell1,*cello;
  ModLimits        *mlimits;
  ErsTransform     *tx,*ty,*tz;
  float  x0,y0,s0,xs0,zs0,rs0;
  float  xcell[3000],zcell[3000];
  int    ixcell[300],nxcell[300],is0,ins;
  int    new_cno[100],old_cno[100],newnum,oldnum;
  float  xc[1500],zc[1500],xs,ys;
  float  xmin,xmax,ymin,ymax,zmin,zmax;
  float  *x1,*z1,*a1;
  int    *segpntr,*segptcnt;
  int    num,npick,nseg,pdof;
  int    istat,cell_id,id,mid;
  int    i,j,n,k,match;
  char   str[16],line[200];

  *ierr = 1;
  istat = 1;
/*
C
C 1. DETERMINE THE X,Z POINTS THAT MAKE UP CELL BOUNDARYS
C 2. COMPUTE A POINT WITHIN EACH CELL AND SAVE .
C    THE ROUTINES CELL,CELLWHER, AND CELLPNT WERE WRITTEN BY
C    DOUG HANSON.
C 4. ATTEMPT TO USE OLD MID INFORMATION BY MATCHING THE OLD
C    CELL COORDINATES WITH THE CURRENT COMPUTED VALUES OF XC AND ZC.
*/
 /********************************************
 *  Check that we have model and pick data ***
 ********************************************/
 if(model == NULL)
  {strcpy(line,"cell_bndid: NULL model");
   istat = 1;
   goto error;}
 pikrec = (PR_ *) model_getpdata(model);
 PRCounts(pikrec,(long *) &nseg,(long *) &npick);
 if(npick <= 0)
  {strcpy(line,"cell_bndid: No picks in memory");
   istat = 2;
   goto error;
  }

 /********************************************
 * Compute the cell boundarys and set the  ***
 * tranform pointers consistent with picks ***
 * cell_comp allocates a new cells struct  ***
 * and the internal cell structures needed ***
 ********************************************/
 new_cells = (ErsCells *) cell_comp(model);
 if(new_cells == NULL)
  {strcpy(line,"cell_bndid: Cell computations failed in cell_comp");
   istat = 3;
   goto error;
  }
 /********************************************
 * Find an inside point (x0,y0) for each cell*
 * Find the cell number for point (x0,y0)    *
 ********************************************/
 access_cdat_(new_cells, &newnum, ixcell, nxcell, xcell, zcell);
 for(n=0;n<newnum;n++)
  {new_cno[n] = -1;
   x0 = 0; y0 = 0; mid = -1;
   i = ixcell[n];
   cellpnt_w_(&x0,&y0,&nxcell[n],&xcell[i],&zcell[i],&ins,&istat);
   if(istat == 0)
    { cellwher_w_(&x0,&y0,&is0,&new_cno[n],&xs0,&zs0,&rs0,
                &newnum,ixcell,nxcell,xcell,zcell,&istat);
    }
   else
    {strcpy(line,"cell_bndid: Scanning cell-points of the current model\n");
     strcat(line,"failed for cell number: ");
     sprintf(str,"%d",n);
     strcat(line,str);
     printf("%s\n",line);
    }
   cell1 = cells_get_nth(new_cells,n);
   cell_set(cell1, &x0, &y0, &mid); /* save the pt (x0,y0) */
  }

 /********************************************
 * Find the cell number for point (x0,y0)    *
 * for the old cells.
 ********************************************/
 old_cells = model_getcdata(model);
 oldnum = cells_count(old_cells);
 for(n=0;n<oldnum;n++)
  {old_cno[n] = -1;
   cell1 = cells_get_nth(old_cells,n);
   cell_get3(cell1, &x0, &y0,&s0, &mid);
   cellwher_w_(&x0,&y0,&is0,&old_cno[n],&xs0,&zs0,&rs0,
               &newnum,ixcell,nxcell,xcell,zcell,&istat);
   if(istat != 0)
    {strcpy(line,"cell_bndid: Scanning cell-points of the old model\n");
     strcat(line,"failed for cell number: ");
     sprintf(str,"%d",n);
     strcat(line,str);
     printf("%s\n",line);
    }
  }

 /********************************************
 * Compare cell numbers of old and new cells.
 * Update new_cells where there is a match.
 ********************************************/
  for(n=0;n<newnum;n++)
   {cell1 = cells_get_nth(new_cells,n);
    cell_get3(cell1, &x0, &y0, &s0, &mid); /* save the pt (x0,y0) */
    match=0;
    for(j=0;j<oldnum;j++)
     {if(new_cno[n] == old_cno[j]) { k=j; match += 1;}
     }
    if(match > 1 || match ==0)
     { mid= 0;
       cell_set3(cell1, &x0, &y0,&s0, &mid);
     }
    else
     { cello= cells_get_nth(old_cells,k);
       cell_get3(cello, &x0, &y0,&s0, &mid);
       cell_set3(cell1, &x0, &y0,&s0, &mid);
     }
   }

 /********************************************
 * Destroy the old Cell information in model *
 * and replace with the new Cell information *
 * that is tied to the material ids.         *
 ********************************************/
 model_rep_cdata(model,new_cells);
 istat = 0;
 *ierr = istat;
 return istat;

 error:
 if(istat != 0) printf("%s\n",line);
 *ierr = istat;
 return istat;
}

