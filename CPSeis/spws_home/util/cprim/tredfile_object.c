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
/*
                          tredfile_object.c

************************* COPYRIGHT NOTICE ****************************
*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
************************* COPYRIGHT NOTICE ****************************
C\USER DOC
-----------------------------------------------------------------------
                    SEISMIC PROCESSING WORKSTATION
                          C-LANGUAGE UTILITY 
                  designed to be called from C or C++

  Utility name:  tredfile_object    (trace edit file)

  Subdirectory:  cprim               (and [primitives.math] in CPS)
  Library:       cprim.a             (and CONLIB in CPS)
  Header file:   cprim.h and tredfile_object.h
  Source file:   tredfile_object.c

  Written:       94/12/20  by:  Kruger Corn
  Last revised:  94/12/28  by:  Tom Stoeckley

  Purpose:       To delete, kill, reverse-polarity, and/or flag
                 seismic traces based on information residing in
                 a file picked in CBYT.  This object is used by
                 CBYT and by the CPS process TREDIT.

  Related Documentation:
-----------------------------------------------------------------------
                       GENERAL INFORMATION

  This is a C-style object analogous to a C++ class.

-----------------------------------------------------------------------
                   INPUT AND OUTPUT ARGUMENTS

  For each function documented here, each argument is flagged as
  follows:
      i = value required upon INPUT to the function.
      o = value set by the function upon OUTPUT.
      b = value BOTH required upon input and changed upon output.

  Values required for input may not necessarily be flagged in functions
  where all arguments are input arguments.

  For pointers, the flag (i,o,b) refers to the contents pointed to
  by the pointer, not to the value of the pointer itself.  The pointer
  value is required upon INPUT in all cases.
-----------------------------------------------------------------------
                 SUMMARY OF FUNCTIONS IN THIS UTILITY

          tredfile_create      to create the object
          tredfile_destroy     to delete the object
          tredfile_get         to read a trace-edit file
          tredfile_put         to write a trace-edit file
          tredfile_clear       to clear the object

-----------------------------------------------------------------------
                   DOCUMENTATION OF EACH FUNCTION

  See the header files and the code in this implementation file for
  details.
-----------------------------------------------------------------------
                        REVISION HISTORY

     Date     Author       Description
     ----     ------       -----------
  2. 94/12/28 Stoeckley    Name changes made, and initial documentation
                            provided, to comply with CPS requirements.
  1. 94/12/20 Kruger Corn  Initial version.
-----------------------------------------------------------------------
C\END DOC
*/


/*-------------------- TRED file header files ---------------------*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "cprim.h"
/*#include "debug.h"*/
#include "tredfile_object.h"

static const char * const TRED_FILE_TITLE = "TRED_FILE\0";

static const char * const TRED_FILE_LABEL =
".DO.CODE.HW1..STRTVLU1..ENDVLU1.HW2..STRTVLU2..ENDVLU2.HW3..STRTVLU3..ENDVLU3\0";
/*
 XXXXXXXX  XX -m.dd+exx -m.dd+exx XX -m.dd+exx -m.dd+exx XX -m.dd+exx -m.dd+exx
*/

/*-------------------- get TRED file data ------------------------*/

#undef ERROR
#define ERROR(n, msg)                                          \
  {                                                            \
    if (info)                                                  \
      sprintf (info, "Error %d %s", (n), (msg));               \
    if (stream) fclose (stream);                               \
/*  DBG_EXIT("tred_file_*"); */                                \
    return (n);                                                \
  }

TredFile *tredfile_create (void)
{
  TredFile *tf = (TredFile *)malloc (sizeof (TredFile));
  if (tf) tredfile_clear (tf);
  return tf;
}


TredFile *tredfile_destroy (TredFile *tf)
{
  if (tf) free (tf);
  return 0;
}

int tredfile_get (const char *filename, TredFile *tf, char *info)
{
  FILE *stream;
  int nstrngs_read;
  char buffer [136];
  char do1[5], code[5];
  int k2;

/*DBG_ENTER("tred_file_get"); */
/* open the file */
  stream = fopen (filename, "r");
  if (!stream) ERROR (errno, "opening TRED file for input");

/* read the header records */
  rewind (stream);
  nstrngs_read = fscanf (stream, "%d %s", &tf->_nrecs, buffer);
  if (nstrngs_read != 2) ERROR(errno, "TRED file header corrupt");
  if (tf->_nrecs <= 0) ERROR(-3, "TRED file empty");
  if (strcmp (buffer, TRED_FILE_TITLE)) ERROR(-4, "TRED file title invalid");
  nstrngs_read = fscanf (stream, "%s", buffer);
  if (nstrngs_read != 1) ERROR (errno,"TRED file record label corrupt");
  if (strcmp (buffer, TRED_FILE_LABEL))
    ERROR (errno, "TRED file record label invalid");

/* read the data records */
  for (k2 = 0; k2 < tf->_nrecs; k2++)
    {
      nstrngs_read = fscanf (stream, "%s%s%d%g%g%d%g%g%d%g%g",
        do1, code,
        &tf->_hdr_wrd_1[k2], &tf->_strt_vlu_1[k2], &tf->_end_vlu_1[k2],
        &tf->_hdr_wrd_2[k2], &tf->_strt_vlu_2[k2], &tf->_end_vlu_2[k2],
        &tf->_hdr_wrd_3[k2], &tf->_strt_vlu_3[k2], &tf->_end_vlu_3[k2]);
      if (nstrngs_read == EOF) ERROR (errno, "TRED file record(s) missing");
      if (nstrngs_read != 11) ERROR (errno, "TRED file record(s) corrupt");
      memcpy (tf->_dos[k2],   do1,  (size_t)4);
      memcpy (tf->_codes[k2], code, (size_t)4);
    }

/* close the file */
  if (info) sprintf (info, " ");
  fclose (stream);
/*DBG_EXIT("tred_rec_get"); */
  return 0;
}

/*-------------------- put TRED file data ------------------------*/

int tredfile_put (const char *filename, TredFile *tf, char *info)
{
  FILE *stream;
  int nchars_written;
  int k2;
  char do1[5], code[5];

/*DBG_ENTER("tred_file_put");*/
/* open the file */
  stream = fopen (filename, "w");
  if (!stream) ERROR (errno, "opening TRED file for output");

/* write the header records */
  rewind (stream);
  nchars_written = fprintf(stream, " %d %s\n", tf->_nrecs, TRED_FILE_TITLE);
  if (nchars_written < 0)
    ERROR (errno, "writing TRED file title");
  nchars_written = fprintf (stream, "%s\n", TRED_FILE_LABEL);
  if (nchars_written < 0)
    ERROR (errno, "writing TRED file record label");

/* write the data records */
  for (k2 = 0; k2 < tf->_nrecs; k2++)
    {
      memcpy (do1,  tf->_dos[k2],   4);
      do1[4] = 0;
      memcpy (code, tf->_codes[k2], 4);
      code[4] = 0;
      nchars_written = fprintf (stream,
        "%4s %4s %2d %9g %9g %2d %9g %9g %2d %9g %9g\n",
        do1, code,
        tf->_hdr_wrd_1[k2], tf->_strt_vlu_1[k2], tf->_end_vlu_1[k2],
        tf->_hdr_wrd_2[k2], tf->_strt_vlu_2[k2], tf->_end_vlu_2[k2],
        tf->_hdr_wrd_3[k2], tf->_strt_vlu_3[k2], tf->_end_vlu_3[k2]);
      if (nchars_written < 0)
        ERROR (errno, "TRED file record(s) missing");
    }

/* close the file */
  if (info) sprintf (info, " ");
  fclose (stream);
/*DBG_EXIT("tred_rec_put"); */
  return 0;
}

int tredfile_clear (TredFile *tf)
{
  int k2;

  tf->_nrecs = 0;
  for (k2 = 0; k2 < TRED_TABLE_NMAX; k2++)
    {
       memcpy(tf->_dos[k2],   "    ", (size_t)TRED_CODES_SIZE);
       memcpy(tf->_codes[k2], "    ", (size_t)TRED_CODES_SIZE);
       tf->_hdr_wrd_1 [k2] = 0 ;
       tf->_strt_vlu_1[k2] = 0.;
       tf->_end_vlu_1 [k2] = 0.;
       tf->_hdr_wrd_2 [k2] = 0 ;
       tf->_strt_vlu_2[k2] = 0.;
       tf->_end_vlu_2 [k2] = 0.;
       tf->_hdr_wrd_3 [k2] = 0 ;
       tf->_strt_vlu_3[k2] = 0.;
       tf->_end_vlu_3 [k2] = 0.;
    }
  return 0;
}
