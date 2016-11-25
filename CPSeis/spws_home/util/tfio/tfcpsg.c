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
#include <stdio.h>
#include "tfdefs.h"
/*******************
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Primitive name: tf_cpsg_()     Language:  C-language
C          Author: R.S. Day
C         Written: 93/07/07
C    Last revised: 97/06/03
C
C  Purpose: Convert CPS global data into a TF_Global structure. A  
C           routine to facilitate opening a new 'TFILE' file that is
C           consistent with the CPS global common block.
C
C-----------------------------------------------------------------------
C           CALLING SEQUENCE & INPUT PARAMETERS
C
C     int  tf_cpsg_( void *cpsin, void *gout)
C
C  Name   in/out   Valid    Description
C  cpsin  input             Address of the first word in a CPS globals
C                           common block. 
C  gout   output            Pointer to a GLBL structure(see tfio.h) that
C                           can be passed to the tf_open call.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.tf_cpsg takes CPS globals information as input and constructs a
C   GLBL structure that is consistent with the globals data. tf_cpsg
C   is callable from fortran. The header file tfdefs.h aliases the
C   name tf_cpsg_ to compensate for fortran compiler conventions on
C   cray, vms and ultrix machines.
C
C 2.Returns zero if all is OK. Returns non-zero if there is a problem.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date      Author    Description
C     ----      ------    -----------
C 13  93/07/07  Day       Creation date
C-----------------------------------------------------------------------
C_______________________________________________________________________
C                                 NOTES
C 1.
C
C-----------------------------------------------------------------------
C\END DOC
****/
int  tf_cpsg_( void *cpsin, void *gout)
{ CPSG *cpsg;
  TF_Global *global;
/*
 * Convert CPS global data to a GLBL structure
 */
 cpsg = (CPSG *) cpsin;
 global = (TF_Global *) gout;
 if(cpsg == NULL) return 1;
 if(global == NULL) return 1;
 global->grecsiz= GRECSIZ;
 global->ntb   = 0;
 global->numhc = 0;
 global->ntrcll= 1;
 global->nbyhd = sizeof(float);
 global->nbydp = sizeof(float);
 global->nhdwd = cpsg->nwih;
 global->ndptr = cpsg->nt;
 global->nbycll= global->ndptr *global->nbydp + global->nhdwd*global->nbyhd; 
 global->hdtyp = 0;
 global->wdtyp = 0;
 global->srval = cpsg->dt;
 global->tstrt = cpsg->t0;
 global->xorg  = cpsg->xorg;
 global->yorg  = cpsg->yorg;
 strcpy(global->ftyp,"TFILE");
 global->h = 0;
 memcpy(global->dx0,cpsg->dx0,4*sizeof(float));
 memcpy(global->dn0,cpsg->dn0,4*sizeof(float));

 return 0;
}
