/*!<CPS_v1 type="HEADER_FILE"/>*/
/*-----------------------------   cube.h    -------------------------------*/
/*-----------------------------   cube.h    -------------------------------*/
/*-----------------------------   cube.h    -------------------------------*/

                  /* other files are: cube.c, trot.f90  */ 

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
! Name       : cube.h
! Category   : io
! Written    : 2002-05-02   by: Michael L. Sherrill
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Creates a CSV compatible file for the workstation.
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
!  4. 2007-03-27  Kruger Corn            Updated to 64 bit architecture.
!                                        Basically changed long to int32_t
!  3. 2005-05-31  Michael L. Sherrill    Added structure member to insure
!                                        that any existing csv file will be
!                                        overwritten when called by CPS or
!                                        CSVTROT.
!  2. 2002-05-20  Michael L. Sherrill    Added ability to specify cube headers
!  1. 2002-05-02  Michael L. Sherrill    Initial version, from Kruger Corn
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/



#ifndef _CUBE_
#define _CUBE_

#include "c2f_interface.h"
#include "trciof77.h"
#include <stdio.h>

#include "upgrade264.h"

#ifdef NEED_CAPITALS
#define cube_trcio_create_file_batch CUBE_TRCIO_CREATE_FILE_BATCH
#define cube_trcio_create_file       CUBE_TRCIO_CREATE_FILE
#endif

#ifdef NEED_UNDERSCORE
#define cube_trcio_create_file_batch   cube_trcio_create_file_batch_
#define cube_trcio_create_file         cube_trcio_create_file_
#endif


#define UNDEFHDR -99


#ifdef __cplusplus
extern "C" {                                    /* for C++*/
#endif

enum {
  NOT_VALID_FILE     = -9,                      /* name is not a valid file */
  VALID_FILE_EMPTY   = -8,                      /* valid file is empty      */
  FILE_SORT_ERROR    = -7,                      /* file is sorted wrong     */
  FILE_PRED_ERROR    = -6,                      /* file size not predictable*/
  ZSLICE_WRITE_ERROR = -5,                      /* Error writing Z-slice fil*/
  ZSLICE_READ_ERROR  = -4                       /* Error reading Z-slice fil*/
};

typedef int (*StatusFunction)                   /* status and or abort func */
  (void *_status_object,                        /* instance of status class */
   float percent_done);                         /* given pct done of task   */

typedef struct _CubeTrcio {                     /* CubeTrcio member data    */
  char *    _name;                              /* name of Trot file        */
  char *_csv_name;                              /* name of aux Z-slice file */
  int _csv_unit;                                /* unit# of aux Z-slice file*/
  int _byte_offset_to_data;                     /* byte offset to Z-slice da*/
  int32_t _block_byte_size;                        /* size in bytes of a block */
  double _trmaxg;                               /* global trce max abs value*/
  float _scale_int_to_float;                    /* fac to scale int to float*/
  int _crossline_hdr;                           /* header word for X-axis   */
  int    _inline_hdr;                           /* header word for Y-axis   */
  int    _sample_count;                         /* # of samples             */
  int _crossline_count;                         /* # of crosslines          */
  int    _inline_count;                         /* # of inlines             */
  double _first_sample;                         /* value of first sample    */
  double _first_crossline;                      /* value of first crossline */
  double _first_inline;                         /* value of first inline    */
  double    _sample_incr;                       /* increment along Z-axis   */
  double _crossline_incr;                       /* increment along X-axis   */
  double    _inline_incr;                       /* increment along Y-axis   */
  int _word_type;                               /* data word type           */
  int _bytes_per_sample;                        /* # of bytes per sample    */
  int _sorted_by_x_then_y;                      /* flag referring to Trot f */
  int _overwrite_existing_file;                 /* flag to overwrite file   */
  StatusFunction _status_function;              /* status and/or abort func */
  void *_status_object;                         /* instance of status class */
} CubeTrcio;


void cube_trcio_create_file_batch
  (char *filename,                              /* CPS Fortran code entry  */
   int  *crossline_header,                      /* Fastest varying header  */
   int  *inline_header,                         /* Slowest varying header  */
   int  *error);                                /* Error flag              */


int cube_trcio_create_file
  (char           *filename,                    /* C/C++ code entry        */
   StatusFunction function,                    
   void           *obj,
   int            crossline_header,
   int            inline_header,
   int            overwrite_existing_file);
              

CubeTrcio *cube_trcio_is3D                      /* check for valid input   */
  (TF_Global      *g,
   int            crossline_header,
   int            inline_header);

CubeTrcio *cube_trcio_create                    /* constructor              */
  (char *name,                                  /* name of Trot file        */
   int *crossline_hdr,                          /* header word for X-axis   */
   int *   inline_hdr,                          /* header word for Y-axis   */
   TF_Global *g);


/* CubeTrcio member functions follow */
int cube_trcio_cubeTrcioIsOk 
  (CubeTrcio *cubetrcio);

void cube_trcio_delete                          /* destructor               */
  (CubeTrcio **This);                           /* instance of CubeTrcio obj*/

int cube_trcio_checkHeaders                     /* make hdrs so sorted_by_x */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   TF_Global *g);

int cube_trcio_initialize                       /* populates object         */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_initializeCsvFile                /* inits the aux Z file     */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_writeCsvHeader                   /* writes out the aux Z fhdr*/
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_initializeCsvData                /* inits the aux Z data     */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_writeCsvData                     /* writes out the aux Z data*/
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_getOffsetToData                  /* rtns offset to aux Z data*/
  (char *csv_name);                             /* given name of aux Z file */

void cube_trcio_getSectionOffset                /* compute offsets to sectn */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   int index,                                   /* given index of section   */
   INTEGER *which_block,                        /* rtnd offset to extent    */
   INTEGER *which_byte);                        /* rtnd offset to byte n ext*/

void cube_trcio_updateOffset                    /* update offsets to data   */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   INTEGER num_bytes,                           /* #bytes to shift offset by*/
   INTEGER *which_block,                        /* gvn/rtnd offset to extent*/
   INTEGER *which_byte);                        /* gvn/rtnd offset to byte  */

void cube_trcio_updateTrmaxg                    /* updates _trmaxg          */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   double lav);                                 /* gvn sngl trc lav 4 update*/

int cube_trcio_writeTrmaxg                      /* write trmaxg to aux Z fil*/
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   INTEGER istream);                            /* given file logical unit #*/

double cube_trcio_getTrmaxg                     /* rtn trmaxg stored in obj */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

double cube_trcio_trmaxg                        /* rtn trmaxg stored in file*/
  (char *csv_name);                             /* given name of aux Z file */

int cube_trcio_getTrmaxgFromFile                /* get trmaxg stored in file*/
  (char *name,                                  /* given name of trace file */
   int   crossline_header,
   int   inline_header,
   double *trmaxg);                             /* returned trmaxg          */

CubeTrcio *cube_trcio_createFromCsvFile         /* rtns new CubeTrcio       */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_readCsvSection                   /* reads in the aux Z data  */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   int index,                                   /* zero-relative index read */
   int transpose,                               /* flag = 1 => transpose    */
   float *asection);                            /* rtnd section read        */

void cube_trcio_initializeScaleFactor           /* rtn scl fac frm ints to f*/
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   int use_unity_factor);                       /* 0 if not to use unity fac*/

int cube_trcio_scaleFloatToInt                  /* scales a float to an int */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   float in);                                   /* input float to scale to i*/

float cube_trcio_scaleIntToFloat                /* scales a float to an int */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   int in);                                     /* input int to scale to flt*/

int cube_trcio_csvFileExists                    /* rtns 0 if no aux Z file  */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

const char *cube_trcio_csvName                  /* writes out the aux Z file*/
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

const char *cube_trcio_setCsvName               /* sets given name and rtns */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   const char *name);                           /* new name of aux Z file   */

const char *cube_trcio_setCsvNamePath           /* sets given path and rtns */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   const char *path);                           /* new path for aux Z file  */

const char *cube_trcio_fileNoPath               /* rtns filename w/o path   */
  (const char *name,                            /* file name to process     */
   size_t *length);                             /* length of filename       */

const char *cube_trcio_filePrefix               /* returns prefix of file   */
  (const char *name,                            /* file name to process     */
   size_t *length);                             /* length of prefix         */

const char *cube_trcio_filePath                 /* returns path of file     */
  (const char *name,                            /* file name to process     */
   size_t *length);                             /* length of path           */

const char *cube_trcio_fileExtension            /* returns extension of file*/
  (const char *name,                            /* file name to process     */
   size_t *length);                             /* length of extension      */

Grid3DDesc *cube_trcio_grid3DDesc               /* rtns 3D structure        */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/


Grid3DDesc *cube_trcio_create_desc              /* Create the 3d structure  */
  (char *hfile, char *dfile,
   char *pname, char *obj_name,
   float o1, float o2, float o3,
   int   n1, int   n2, int   n3,
   float d1, float d2, float d3, 
   char *lab1,char *lab2,char *lab3,
   int X, int Y, int Z,
   int ftype, int  wdtype);

void cube_trcio_desc_init                       /* Initialize the 3d struct*/
  (Grid3DDesc *h);

void cube_trcio_setuvw                          /* Set the 3d axis         */
  (Grid3DDesc *h, int X, int Y, int Z);

int cube_trcio_matches                          /* rtns 0 if new name       */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   char *name);                                 /* name to test for match   */

int cube_trcio_matchesCsvFile                   /* rtns 0 if new name       */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

void cube_trcio_setStatusFunction               /* registers status function*/
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   StatusFunction func,                         /* given function           */
   void *obj);                                  /* instance of status object*/

int cube_trcio_descriptionsMatch                /* rtns 0 if don't match    */
  (CubeTrcio *This,                             /* instance of CubeTrcio obj*/
   CubeTrcio *cube);                            /* instance of CubeTrcio obj*/

int cube_trcio_getline                          /* return length of a line  */
  (char *string);                               /* given string of chars    */

void cube_trcio_printError                      /* print error msg to stdout*/
  (int error);                                  /* given error code         */

int cube_trcio_X                                /* return crossline hdr wrd */
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_Y                                /* return inline header wrd */ 
  (CubeTrcio *This);                            /* instance of CubeTrcio obj*/

int cube_trcio_supported                        /* rtn 0 if file not valid  */
  (char *name);                                 /* name of file candidate   */

int cube_trcio_doublesClose
  (double first,                                /* first value to compare   */
   double second);                              /* second value to compare  */

static void cube_trcio_swap                     /* Swap bytes as needed     */
  (void *ptr, int size, int nobj);


#ifdef __cplusplus
}                                               /* for C++*/
#endif

#endif
