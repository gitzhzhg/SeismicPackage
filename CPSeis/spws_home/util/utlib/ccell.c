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
#include <Pcard.h>

struct {
    char cllab[2970];
} gmf2_;

#define gmf2_1 gmf2_

/* Table of constant values */
static long maxc = 99;
static long mc   = 3500;

/****************************************************************************
C**                 CONFIDENTIAL AND PROPRIETARY INFORMATION                  
C**                              OF CONOCO INC.                               
C**                      PROTECTED BY THE COPYRIGHT LAW                       
C**                          AS AN UNPUBLISHED WORK                           
C****************************************************************************
C\USER DOC 
C  ----------------------------------------------------------------------- 
C                    INTERACTIVE CONOCO PROCESSING SYSTEM 
C                  EXPLORATION RESEARCH & SERVICES DIVISION 
C                               CONOCO, INC. 
C
C   Process name: ccell()
C         Author: R.S.DAY 
C        Written: 92/10/15
C   Last revised:          Who:  Day 
C
C   Purpose:      USED TO HELP CONTRUCT DEPTH MODELS. 
C                 TAKES DIGITIZED HORIZON INFORMATION AND IDENTIFIES 
C                 THE BOUNDARYS OF CLOSED CELLULAR REGIONS IN A MODEL. 
C  ----------------------------------------------------------------------- 
C                            CALLING SEQUENCE 
C        ccell(long *istat,PCard *pcard);
C
C                            INPUT PARAMETERS 
C
C   Name       Type      InOut    Description 
C   ----       -------   -----    ----------- 
C  istat.......INTEGER   OUTPUT   RETURN ERROR STATUS 
C  pcard.......PCard *   In & Out Structure with model and boundary info.
C  ----------------------------------------------------------------------- 
C                                  NOTES 
C
C  1. DETERMINE THE X,Z POINTS THAT MAKE UP CELL BOUNDARYS. 
C     WILL PLACE INFORMATION ABOUT THE CELLS INTO CDEF1. 
C     INFORMATION FROM /GMF1/ IS USED TO COMPUTE DATA FOR /CDEF1/ 
C  2. FOLLOW VXZCELL BY A CALL TO VXZGRID 
C  ----------------------------------------------------------------------- 
C\END DOC 
C\PROG DOC 
C  ----------------------------------------------------------------------- 
C                          REVISION HISTORY 
C      Date     Author     Description 
C      ----     ------     ----------- 
C  1.  90/07/11 Day        Original Version 
C  ----------------------------------------------------------------------- 
C                                  NOTES 
C
C  1. MAKES USE OF DOUG HANSONS CELL SUBROUTINE 
C     UPDATE INFORMATION IN ALL ARRAYS AFTER WE RETURN FROM CELL 
C  ----------------------------------------------------------------------- 
C       LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES 
C  ----------------------------------------------------------------------- 
C  NAMED COMMONS :VDEF1           CDEF1           GMF1        GMF2 
C
C   EXTERNAL CALLS:CELL 
C
C  SUBROUTINES   :VXZCELL 
C
C  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C\END DOC 
C*************************************************************************/
void ccell( int *istat, Pcard *pcard)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy();

    /* Local variables */
    static char line[80];
    static integer ierr;
    static real rtol;
    static integer i, j, m;
    extern void cell_w_();  /* Subroutine */
    static integer iprint, lprint;
    ErsPickingRecord *pikrec;
    ErsHorizon       *hlist[199],*slist[32],*horizon;
    ErsPoint         *point;
    Ersmodel         *model;
    ModLimits        *mlimits;
    long             nhor,nseg,cnt,count;
    long             ibtyp[299];
    char             string[81];
    Bool             success;
/* Model boundary related variables */
    long             nclass, nsubcl[99];
    long             nseg, hids[99], nptsubcl[99], npntr[99], 
    long             num, nattr;
    float            x1[2000], y1[2000], a1[4000];
/* Cell related variables */
    long             ncell, ixc[99], nxc[99];
    float            xc[3500], yc[3500];
    long             idc[3500];
    static long      *wrk,mwrk;
    long             

 *istat = 1;
 if(pcard == NULL) return;
 pikrec = pcard->pikrec;
 if(pikrec == NULL) return;
 model = pcard->model;
 if(model == NULL) return;

 mlimits = &model->modlim;
 rtol = 0.0001*(mlimits->xmax - mlimits->xmin);
 if(rtol < 0) rtol = -rtol;

/* get horizon and segment counts and set hids */
 ErsHorizonGetHList(pikrec, &nhor, hlist);
 cnt = pikrec->horizon_count; /* total number of segments */
 cnt = 0;
 npntr[cnt]=0;
 for (i = 0; i < nhor; i++) /*loop over unique horizon names */
   { ErsHorizonMemberList(pikrec, hlist[i], &nseg, &N, slist);
     for (j = 0; j < nseg; ++j) /* loop over segments */
       { horizon = slist[j];
         point     = horizon->first_point;
         count = 0;
         while (point != NULL)
         {
          count++;
          if(point->npicks > 0)
            {
            }
          else
            { x1[count] = point->tn;
              y1[count] = point->time;
            }
          point = point->next;
         }
         hids[cnt] = horizon->hid;
         nptsubcl[cnt] = count; /* ErsHorizonGetPointCount(horizon); */
         npntr[cnt]   += nptsubcl[cnt];
         cnt++;
       }
   }

 if (cnt == 0)
   { ierr = -1;
     strcpy(string,"ccell: No segments to process?");
     goto error;
   }
 if(mlimits->xmin == mlimits->xmax)
   { ierr = -2;
     strcpy(string,"ccell: No model window defined");
     goto error;
   }

 iprint = 1;
 lprint = 6;
 mwrk   = 15*cnt + 4*(3000);
 if(wrk != NULL ) free(wrk);
 wrk = (long *) calloc(1,mwrk*sizeof(float));
 cell_w_(&rtol,&mlimits->xmin,&mlmits->xmax,&mlmits->zmin,&mlmits->zmax,
         &cnt, hids, npntr, nptsubcl, x1, y1,
         &maxc, &mc, &ncell, ixc, nxc, xc, yc, idc,
         &mwrk, &wrk[1],
         &iprint, &lprint, &ierr, istat);
 free(wrk);
 wrk = NULL;
 if (*istat != 0)
   { strcpy(string,"ccell: Error occured in cell call");
     goto error; }

 nclass = 1;
 clid[nclass - 1]   = ibtyp[0];
 nsubcl[nclass - 1] = 1;
 for (m = 1; m < cnt; m++)
   {
     if (ibtyp[m] != ibtyp[m - 1])
       { ++nclass;
         clid[nclass - 1] = ibtyp[m];
         nsubcl[nclass - 1] = 1;
       }
     else
        { ++nsubcl[nclass - 1]; }

   }

 return 0;

error:
 *istat = ierr;
 printf("%s\n",string);
 return 0;
}

