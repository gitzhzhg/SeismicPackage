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
/****
!<CPS_v1 type="PRIMITIVE"/>
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
! Revised    : 2002-05-09   by: Michael L. Sherrill
! Maturity   : production   2002-05-20   
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
#include "workstation.h"

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

char WORKSTATION_GLOBALS_IDENT[100] =
"$Id: workstation.c,v 1.1 2002/07/24 18:54:41 wjdone Exp $";

TF_Global *workstation_globals_get_globals(char *filename, int *istat)
{
  TF_Global *g;
  float dt, tmin;
  int ndpts, nwih, nbits, nbits_hd, lun;
  int data_start_position, num_traces;
  char wtype[10];
  double trmaxg;
  double xorigin, yorigin, dx11, dx12, dx21, dx22;
  int isa_jseis_file;



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
  /* wmm added next line */
  isa_jseis_file = 0;
  /* lun = jsfiles_getlun (filename, "r");
  fprintf(stderr,"%s:%d: jsfiles_getlun %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  jsfiles_open (lun);
  isa_jseis_file = jsfiles_isa (lun);
  fprintf(stderr,"%s:%d: isa_jseis_file=%d(%s) lun=%d\n",__FILE__,__LINE__,isa_jseis_file,filename,lun);
  if (!isa_jseis_file) {
    fprintf (stderr,"%s:%d: workstation_globals_get_globals: calling jsfiles_close\n",__FILE__,__LINE__);
    jsfiles_close (lun);
    lun = 0;
  }
  */
  if (!isa_jseis_file) {
    trciof77wrapper_get_globals_(filename, wtype, &dt, &ndpts, &nwih, &nbits, 
                               &nbits_hd, &tmin, &data_start_position, 
                               &xorigin, &yorigin, &dx11, &dx12, &dx21,
                               &dx22, &num_traces, &trmaxg, &lun, istat);
  }
  else {
    /*
    jsfiles_wtype (lun, wtype, 10);
    num_traces = jsfiles_gettracecount (lun);
    ndpts = jsfiles_getsamplecount (lun);
    dt = jsfiles_getsamplerate (lun);
    tmin = jsfiles_getstarttimeinsecs (lun);
    nwih = jsfiles_getheadercount (lun);
    */
    /* trmaxg = jsfiles_getlav (lun); */
    /* *istat = jsfiles_status (lun);
    nbits = 32;
    nbits_hd = 64;
    fprintf (stderr,"%s:%d: workstation_globals_get_globals: calling jsfiles_close\n",__FILE__,__LINE__);
    jsfiles_close (lun);
    */
  }

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
  else if (strstr(wtype, "JSEIS"))
    {
      strcpy (g->ftyp, wtype);
    }
  else /* maybe internal trot type */
    {
      if(strstr(wtype, "IEEE") || strstr(wtype, "LBO") ||
        (strstr(wtype, "INT") && nbits_hd >= 64))
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

char *workstation_get_jseis_fprop (char *filename)
{
  int lun, isa_jseis_file, len;
  char *ext;
  char *retval = NULL;
  /* wmm added next line */
  return filename;

  lun = jsfiles_getlun (filename, "r");
  fprintf(stderr,"%s:%d: jsfiles_getlun %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  jsfiles_open (lun);
  fprintf(stderr,"%s:%d: jsfiles_open   %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  isa_jseis_file = jsfiles_isa (lun);
  fprintf(stderr,"%s:%d: isa_jseis_file=%d(%s) lun=%d\n",__FILE__,__LINE__,isa_jseis_file,filename,lun);

  if (isa_jseis_file != 0) {
    /* e.g. filename = name.jsf */
    ext = strrchr (filename, '.');
    len = ext - filename;
    /* e.g. *out = name.js/FileProperties.xml */
    retval = (char *)malloc (sizeof(char)*(len+23));
    strcpy (retval, "");
    strncpy (retval, filename, len);
    retval[len] = '\0';
    strcat (retval, ".js/FileProperties.xml");
  }
  else {
    retval = filename;
  }
  return retval;
}

void workstation_force_jseis_close (char *filename)
{
  int lun, isa_jseis_file;

  lun = jsfiles_getlun (filename, "r");
  fprintf(stderr,"%s:%d: jsfiles_getlun %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  jsfiles_open (lun);
  fprintf(stderr,"%s:%d: jsfiles_open   %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  isa_jseis_file = jsfiles_isa (lun);
  fprintf(stderr,"%s:%d: isa_jseis_file=%d(%s) lun=%d\n",__FILE__,__LINE__,isa_jseis_file,filename,lun);
  if (isa_jseis_file) {
    jsfiles_close (lun); 
    fprintf(stderr,"%s:%d: jsfiles_close   %s lun=%d\n",__FILE__,__LINE__,filename,lun);
  }
}

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

