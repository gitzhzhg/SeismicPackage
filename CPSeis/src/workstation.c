/****
!<CPS_v1 type="PRIMITIVE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>
*/

/*----------------------   workstation.c       -------------------------------*/
/*----------------------   workstation.c       -------------------------------*/
/*----------------------   workstation.c       -------------------------------*/
 
                 /* other files are:  trciof77.h         */
 


/****
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : workstation.c  (provide trcio globals for workstation)
! Category   : IO
! Written    : 2002-05-02   by: Michael L. Sherrill
! Revised    : 2002-07-25   by: Michael L. Sherrill
! Maturity   : production   2002-08-19
! Purpose    : Provide trcio globals for workstation
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! Provides the workstation software with trcio trace file globals  
! via call to trcio wrappers. This source also contains C++ style
! access to the the various global members.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS 
!
! None
! 
! 
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS  
!  None
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! Trace headers are not passed in and therefore are not altered.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!      o                              i      o
!  globals = workstation_globals(filename, istat)
!
! TF_Global *globals  = Workstation globals structure returned. NULL on errors.
! char      *filename = Trcio file to get the globals from.
! int       *istat    = Status code for error handling.
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! 
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author               Description
!     ----        ------               -----------
!  3. 2002-08-19  Michael L. Sherrill  Added 16 bit segy support
!  2. 2002-05-20  Michael L. Sherrill  Added segy support
!  1. 2002-05-02  Michael L. Sherrill  Initial Version
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS 
!
! None
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES 
! Returns NULL on failure
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/


char WORKSTATION_GLOBALS_IDENT[100] =
"$Id: workstation.c,v 1.3 2002/08/14 16:15:04 Sherrill prod sps $";


#include "trciof77.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

TF_Global *workstation_globals_get_globals(char *filename, int *istat)
{
  TF_Global *g;
  float dt, tmin;
  int ndpts, nwih, nbits, nbits_hd, lun;
  int data_start_position, num_traces;
  char wtype[10];
  double trmaxg;
  double xorigin, yorigin, dx11, dx12, dx21, dx22;



  g = (TF_Global *) malloc(sizeof(TF_Global));

  if(!g)
    {
      *istat = 1;
      return NULL;
    }

  /*Initialize variables*/
  g->lun     = -1;
  g->grecsiz = 0;
  g->ntrfil  = 0;
  g->nbycll  = 1024;
  g->ntrcll  = 1;
  g->wdtyp   = 1;
  g->nbydp   = 1;
  g->nbyhd   = 1;
  g->hdtyp   = 0;
  g->nhdwd   = 0;
  g->ndptr   = 1024;
  g->ntb     = 0;
  g->numhc   = 0;
  g->h       = 0;
  strcpy(g->path,filename);    
  strcpy(wtype,"");
  dt = 0.0F; ndpts = nwih = nbits = nbits_hd = 0;
  tmin = 0.0F;
  data_start_position = 0;
  xorigin = yorigin = dx11 = dx12 = dx21 = dx22 = 0.0;
  num_traces = 0;
  trmaxg = 0.0F;
  lun = 0;
  *istat = 0;

  trciof77wrapper_get_globals_(filename, wtype, &dt, &ndpts, &nwih, &nbits, 
                               &nbits_hd, &tmin, &data_start_position, 
                               &xorigin, &yorigin, &dx11, &dx12, &dx21,
                               &dx22, &num_traces, &trmaxg, &lun, istat);

  if(*istat)
    {
      free(g);
      return NULL;
    }

  /*The following allows workstation software to read partial files
    that are in the process of being written out */
  if(!num_traces)
    {
      num_traces = 999999999;
      *istat = MAYBE_INCOMPLETE_FILE;
    }

  /*If less than 64 header words like segy's 60,  CPS will give us
    64 anyway.  */
  if(nwih < 64)
    g->nhdwd = 64;
  else
    g->nhdwd  = nwih;

  /* The following file type tests may not be fullproof but the cps i/o
     does not give us much more info on the type read */
  if(strstr(wtype, "IBM") || (strstr(wtype, "INT") && nbits_hd < 64) )/*segy*/
    {
      strcpy(g->ftyp, "DSEGY");
    }
  else /* maybe internal trot type */
    {
      if(strstr(wtype, "IEEE") || (strstr(wtype, "INT") && nbits_hd >= 64))
        strcpy(g->ftyp, "TROT"); 
      else
        assert(0); /* who knows what it is */
    }

  g->nbyhd  = nbits_hd / 8;
  g->nbydp  = nbits / 8;
  g->ntrcll = 1;
  g->ntb    = 0;
  g->numhc  = 0;
  g->tstrt  = tmin;
  g->hdtyp  = 0;
  g->grecsiz= data_start_position;
  g->xorg   = (float)xorigin;
  g->yorg   = (float)yorigin;
  g->dx0[0] = (float)dx11; g->dx0[1] = (float)dx12; 
  g->dx0[2] = (float)dx21; g->dx0[3] = (float)dx22;
  g->dn0[0] = (float)dx11; g->dn0[1] = (float)dx12; 
  g->dn0[2] = (float)dx21; g->dn0[3] = (float)dx22;
  g->dunits = 0;
  g->ndptr  = ndpts;
  g->srval  = dt;
  g->ntrfil = num_traces;
  g->trmaxg = (float)trmaxg;
  g->nbycll = nwih * g->nbyhd + g->nbydp * g->ndptr;
  g->lun    = lun;


  return g;

}

/***** Psuedo C++ style access to the global structure ******/
char *workstation_globals_ftype(TF_Global *g)
{ 
  return g->ftyp;
}

int workstation_globals_get_nhdwd ( TF_Global *g )
{
  return ( g->nhdwd );
}

char *workstation_globals_data_file(TF_Global *g)
{
  return g->path;
}

int workstation_globals_get_wdtyp ( TF_Global *g )
{ 
  return g->wdtyp;
}

int workstation_globals_grid_sizes(TF_Global *g, int *n1, int *n2, int *n3)
{
  Grid3DDesc *h=NULL;
  int X=1,Y=2,Z=0;
  *n1 = g->ndptr;
  *n2 = g->ntrfil;
  *n3 =  1;

  h = (Grid3DDesc *) g->h;

  if(!h)
    return 1;

  switch(h->ftype) {

    case TF3D_TYPE:
    case TF3DF_TYPE:
    case TROT_TYPE:
      *n1 = h->N.v[0]; *n2 = h->N.v[1]; *n3 = h->N.v[2];
      break;

    case HGRID_TYPE: 
    case LAYER_TYPE: 
      *n1 = h->N.v[Z]; *n2 = h->N.v[X]; *n3 = h->N.v[Y];
      break;

    case HG3DL_TYPE:
      X=0; Y=1; Z=2;
      *n1 = h->N.v[X]; *n2 = h->N.v[Y]; *n3 = h->N.v[Z];
      break;

    case VOXET_TYPE:
    case BRICK_TYPE:
      *n1 = h->N.v[0]; *n2 = h->N.v[1]; *n3 = h->N.v[2];
      break;
  }

  return 1;
}

float workstation_globals_get_srval ( TF_Global *g )
{
  return  g->srval ;
}


float workstation_globals_get_tstrt ( TF_Global *g )
{
  return  g->tstrt ;
}

int workstation_globals_get_nbydp ( TF_Global *g )
{
  return ( g->nbydp );
}



