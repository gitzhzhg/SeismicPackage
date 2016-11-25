/*!<CPS_v1 type="HEADER_FILE"/>*/
/*------------------------   trciof77.h       --------------------------------*/
/*------------------------   trciof77.h       --------------------------------*/
/*------------------------   trciof77.h       --------------------------------*/

                  /* other files are: trciof77_wrapper.f90 
                                      workstation.c
                  */

/****
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

!<brief_doc>
!-------------------------------------------------------------------------------
!<center>                C P S   P R I M I T I V E               </center>
!
! Name       : trciof77.h
! Category   : io
! Written    : 2002-05-02   by: Michael L. Sherrill
! Revised    : 2002-05-09   by: Michael L. Sherrill
! Maturity   : production   2002-05-20  
! Purpose    : Provide prototypes for workstation software interface
!              to CPS io routines.
! Portability: None known
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author                 Description
!     ----        ------                 -----------
!  2. 2002-05-20  Michael L. Sherrill    Added cube headers.
!  1. 2002-05-02  Michael L. Sherrill    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


#ifndef _TRCIOF77_H_
#define _TRCIOF77_H_

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define trciof77wrapper_get_globals_ TRCIOF77WRAPPER_GET_GLOBALS
#define trciof77wrapper_get_keyword_ TRCIOF77WRAPPER_GET_KEYWORD
#define trciof77wrapper_get_trace_   TRCIOF77WRAPPER_GET_TRACE
#define trciof77wrapper_close_file_  TRCIOF77WRAPPER_CLOSE_FILE
#define trciof77wrapper_create_file_ TRCIOF77WRAPPER_CREATE_FILE
#define trciof77wrapper_write_trace_ TRCIOF77WRAPPER_WRITE_TRACE
#endif

#define MAYBE_INCOMPLETE_FILE 777

/* Flags for supported file types */
#define BADFN_TYPE -1
#define UNKWN_TYPE 0
#define GOCAD_TYPE 1
#define RMOD_TYPE  2
#define BRICK_TYPE 3
#define VOXET_TYPE 4
#define HGRID_TYPE 5
#define LAYER_TYPE 6
#define HG3DL_TYPE 7
#define TFILE_TYPE 8
#define SEGY_TYPE  9
#define STROT_TYPE 10
#define DTROT_TYPE 11
#define CBYTE_TYPE 12
#define AGRID_TYPE 13
#define GWS_TYPE   14
#define TF3D_TYPE  15
#define TF3DF_TYPE 16
#define QTROT_TYPE 17
#define TROT_TYPE  18


#ifdef __cplusplus
extern "C" {
#endif


                            /* Typedefs */

/*Structure that contains trace file global variables*/
  typedef struct GLBL {
    int  ntrfil;   /* Number of traces in the file       */
    int  nbycll;   /* Number of bytes per disk cell      */
    int  ntrcll;   /* Number of traces per disk cell     */
    int  grecsiz;  /* Size of the global record in bytes */
    int  ntb;      /* No. of trailer bytes between traces and history*/
    int  numhc;    /* Number of history cards in the file*/
    int  nbydp;    /* Number of bytes/data point >0      */
    int  nbyhd;    /* Number of bytes/header word  >0    */
    int  hdtyp;    /* Header type.0-CPS,1-ISP,2-SEGY...  */
    int  wdtyp;    /* Float type, see wrdcnvrt.h         */
    int  nhdwd;    /* Number of header words/trace       */
    int  ndptr;    /* Number of data points/trace        */
    float srval;   /* Sample rate value of the data points*/
    float tstrt;   /* Trace start time(or depth)         */
    float xorg;    /* CPS global                         */
    float yorg;    /* CPS global                         */
    float dx0[4];  /* CPS global                         */
    float dn0[4];  /* CPS global                         */
    float trmaxg;  /* Largest amplitude in all traces    */
    int  dunits;   /* 1 for meters, 2 for feet           */
    int  lun;      /* File descriptor - Channel number   */
    char ftyp[32]; /* File type. IEEE for trot IBM for segy*/
    char path[1024];/* Name of the file for these globals */
    char srun[8];  /* Sample rate units(ME,FE,SE,MS)     */
    int  opstat;   /* store opstat for tf_close_         */
    int  crossline_header; /*Header to use for cube creation */
    int  inline_header;    /*Header to use for cube creation */
    void *h;       /* for 3d data cube descriptions      */
  } TF_Global;


 /*
 * Define structures that help support IO for 3-D data cubes
 */
  typedef struct _Pt3Df { float v[3]; } Pt3Df;

  typedef struct _Pt3Di { int   v[3]; } Pt3Di;

  typedef struct _Pt3Ds { char  v[3][16]; } Pt3Ds;

  typedef struct _PropDesc {
    int  id;
    int  nkey;
    char name[32];
    char file[96];
    char etype[8];
    int  esize;
    char keys[4][16];
  } PropDesc;

  /*3D description structure*/
  typedef struct _Grid3DDesc {
    char  header_file[120];
    int   ftype;
    char  name[64];
    Pt3Df O;
    Pt3Df U;
    Pt3Df V;
    Pt3Df W;
    Pt3Di N;
    Pt3Df MI;
    Pt3Df MA;
    Pt3Ds axis;
    Pt3Ds type;
    Pt3Df D;
    Pt3Di blocks;
    Pt3Di brick;
    Pt3Di hd;
    int   nkey;
    char  keys[24][16];
    char  fmt[24];
    int   np;
    PropDesc P;
    float zdatum;
    char *tran_file;
  } Grid3DDesc;


/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

  /*Cps trcio trot file f77 wrappers that call f90 subroutines. Used
  for interfacing C with CPS io*/
  void trciof77wrapper_get_globals_(char *ifil,char *wtype, float *dt, 
                                    int *ndpt, int *nwih, int *nbits, 
                                    int *nbits_hd, float *tmin, 
                                    int *data_start_position, double *xorigin, 
                                    double *yorigin, double *dx11,
                                    double *dx12, double *dx21, double *dx22,
                                    int *num_traces, double *trmaxg, 
                                    int *lun, int *stat);

  int trciof77wrapper_get_keyword_( char *filename, 
                                    char *search_for_keyword,
                                    char *returned_keyword_value,
                                    int  *stat);

  void trciof77wrapper_get_trace_(  char *ifil, int *open_file, double *hd,
                                    float *tr, 
                                    int *tnum, int *lun, int *stat, 
                                    int *hdsize, int *trsize);

  void trciof77wrapper_close_file_( int *lun, int *istat);

  void trciof77wrapper_create_file_(char *filename, char *mode, int *scratch, 
                                    int *nwih, int *nsamp, int *nbits_trace, 
                                    int *nbits_header, float *dt, 
                                    float *tmin, float *tmax, float *trmaxg,
                                    int *lun, int *stat);

  void trciof77wrapper_write_trace_(int *lun, double *hd, float *tr, 
                                    int *hdsize, int *trsize, int *stat);


  /*Globals prototypes*/
  TF_Global *workstation_globals_get_globals(char *filename, int *istat);

  char *workstation_globals_ftype(TF_Global *g);

  int workstation_globals_get_nhdwd (TF_Global *g );

  char *workstation_globals_data_file(TF_Global *g);

  int workstation_globals_get_wdtyp (TF_Global *g );

  int workstation_globals_grid_sizes(TF_Global *g, int *n1, int *n2, int *n3);

  float workstation_globals_get_srval(TF_Global *g );

  float workstation_globals_get_tstrt(TF_Global *g );

  int workstation_globals_get_nbydp(TF_Global *g );



  /*------------------------- end of prototypes ---------------------------*/
  /*------------------------- end of prototypes ---------------------------*/
  /*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif



/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/


















#endif 
