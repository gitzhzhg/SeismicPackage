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

/*-------------------------     cube.c    ------------------------------------*/
/*-------------------------     cube.c    ------------------------------------*/
/*-------------------------     cube.c    ------------------------------------*/
 
                   /* other files are:  cube.h        */
 


/****
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : cube.c  (Create CSV file for workstation executable)
! Category   : IO
! Written    : 2002-05-02   by: Michael L. Sherrill
! Revised    : 2007-03-27   by: Kruger Corn
! Maturity   : beta
! Purpose    : Create CSV file for workstation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! Creates a cube volume from a reguralized grid of seismic lines. Code can
! be called from a stand alone program or as a CPS batch process.
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
! Trace headers are not altered.
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
!                                    i                 i
!  create_csv_file_batch(char *input_filename, int *crossline_header
!                                    i                 o
!                        int *inline_header,   int *status)
!
!  char *input_filename       = Input file to create csv file from.
!  int  *crossline_header     = Header containing fastest varying values.
!  int  *inline_header        = Header containing slowest varying values.
!  int  *error                = Status code for error handling. 
!                               Value of 0 indicates success.
!
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
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2007-03-27  Corn           Updated to 64 bit architecture. Basically
!                                changed long to int32_t.
!  6. 2005-05-31  Stoeckley      Changed variable DELETE to XDELETE for C++.
!  5. 2004-11-11  Stoeckley      Fixed problem to insure that any existing
!                                csv file will be overwritten when called by
!                                CPS or CSVTROT.
!  4. 2003-11-18  Selzler        Resolved SGI compiler warning (unused var).
!  3. 2002-09-27  M.L. Sherrill  Added structure member to insure
!                                that any existing csv file will be
!                                overwritten when called by CPS or
!                                CSVTROT.
!  2. 2002-05-20  M.L. Sherrill  Added ability to specify cube headers.
!  1. 2002-05-02  M.L. Sherrill  Initial Version from Kruger Corn's code.
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
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/


char CUBETRCIO_IDENT[100] ="$Id: cube.c,v 1.7 2007/03/28 15:09:42 Corn beta sps $";


#include "cube.h"
#include "trciof77.h"
#include "cio_crou.h"
#include "exptilde_crou.h"
#include "swap.h"
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <float.h>
#include <math.h>


/*Currently the largest file size possible for files on Linux disk is 2gig*/
#define DEFAULT_BLOCK_SIZE 2147482624 


/*===================== Main batch fortran entry   =====================*/
void cube_trcio_create_file_batch(char *filename, 
                                  int  *crossline_header,
                                  int  *inline_header,
                                  int  *error)
{
  *error = cube_trcio_create_file(filename, NULL, NULL, *crossline_header,
                                  *inline_header, 1);
}



/*===================== Main workstation code entry ====================*/
int cube_trcio_create_file(char *filename, StatusFunction status_function, 
                           void *obj, int crossline_header, int inline_header,
                           int overwrite_existing_file)
{
  int ok = 0, stat = 0;
  int error = 1;
  CubeTrcio *cubetrcio;
  TF_Global *globals;


  globals = workstation_globals_get_globals(filename, &stat);
  if(!globals)
      return error;

  cubetrcio = cube_trcio_is3D(globals, crossline_header, inline_header);
  if(!cubetrcio)
    {
      free (globals);
      return error;
    }

  globals->h = cube_trcio_grid3DDesc (cubetrcio);
  if(!globals->h)
    {
      free (globals);
      return error;
    }

  if(status_function)
    cube_trcio_setStatusFunction (cubetrcio, status_function, obj);

  cubetrcio->_overwrite_existing_file = overwrite_existing_file;

  ok = cube_trcio_cubeTrcioIsOk(cubetrcio);

  if(!ok)
    {
      free (globals);
      return error;
    }

  free (globals);

  return 0;
}





CubeTrcio *cube_trcio_is3D(TF_Global *g, int crossline_header,
                           int inline_header)
{
  CubeTrcio *cubetrcio = 0;



  if (g->ftyp == '\0' || (!strstr(g->ftyp,"TROT" ) && !strstr(g->ftyp,"DSEGY")))
     return cubetrcio;

  
  cubetrcio = cube_trcio_create(g->path, &crossline_header, &inline_header, g);

  return cubetrcio;
}



/*============================================================================
  ==== The work of writing the file is done here. It call's Kruger's      ====
  ==== CubeTrcio C code to write the data.                                ====
  ==========================================================================*/
int cube_trcio_cubeTrcioIsOk (CubeTrcio *cubetrcio)
{
  int retval = 0;
  char *fn;
  size_t fnl;

  if (cubetrcio->_name) 
    {
      if (!cube_trcio_supported (cubetrcio->_name)) 
        {
          return 1; /*don't penalize the obsolete file types*/
        }
    }

  
  if(cubetrcio->_overwrite_existing_file    ) 
    {
      /* this is for CPS batch processing. */
      /* get a copy of the auxiliary file name*/
      fnl = strlen (cube_trcio_csvName(cubetrcio)) + 1;
      fn  = (char *)malloc (fnl);
      strcpy (fn, cube_trcio_csvName(cubetrcio));
      /* create the file where the data is, overwriting file if exists*/
      cube_trcio_setCsvName (cubetrcio, (const char *)fn);
      free (fn);
      printf("csv file name = %s\n", cubetrcio->_name);
      if(cube_trcio_csvFileExists(cubetrcio)   ) 
        {
          printf("csv file already exists and will be overwritten\n");
        }
      else
        {
          printf("new csv file will be created\n");
        }
      if (!cube_trcio_initializeCsvFile(cubetrcio)) 
        {
          /* privileges and space are available for the auxiliary file*/
          /* where the data is so write the data*/
          if (!cube_trcio_writeCsvData(cubetrcio))
            {
             /*  the data was successfully written */
              printf("csv file was successfully written\n");
              retval = 1;
            }
          else
            {
             /*  error writing the data */
              printf("error writing the csv file\n");
              retval = 0;
            }
        }
      else
        {
          /* either privileges or space were not available for the 
             auxiliary file where the data is */
          printf("privileges or space not available to write the csv file\n");
          retval = 0;
        }
      return retval;
    }

  
/*
  if(!cube_trcio_csvFileExists(cubetrcio) || 
     cubetrcio->_overwrite_existing_file    ) 
*/
  if(!cube_trcio_csvFileExists(cubetrcio)   ) 
    {
      /* the auxiliary file does not exist where the data is so*/
      /* get a copy of the auxiliary file name*/
      fnl = strlen (cube_trcio_csvName(cubetrcio)) + 1;
      fn  = (char *)malloc (fnl);
      strcpy (fn, cube_trcio_csvName(cubetrcio));
      /* reset the csv_name to be located in CPSDATA/ and continue...*/
      cube_trcio_setCsvNamePath (cubetrcio, "~/cpsdata/");
      if (!cube_trcio_csvFileExists(cubetrcio)) 
        {
          /* the auxiliary file does not exist there either so try to create*/
          /* file where the data is*/
          cube_trcio_setCsvName (cubetrcio, (const char *)fn);
          free (fn);
          if (!cube_trcio_initializeCsvFile(cubetrcio)) 
            {
              /* priveledges and space are available for the auxiliary file*/
              /* where the data is so write the data*/
              if (!cube_trcio_writeCsvData(cubetrcio))
                {
                 /*  the data was successfully written */
                  retval = 1;
                }
            }
          else
            {
              /* either priveledges or space were not available for the 
                 auxiliary file where the data is so try to create the 
                 file in CPSDATA/ */
              cube_trcio_setCsvNamePath(cubetrcio, "~/cpsdata/");
              if (!cube_trcio_initializeCsvFile(cubetrcio))
                {
                  /* priveledges and space were available for the auxiliary*/
                  /* file in CPSDATA/  so write the data*/
                  if (!cube_trcio_writeCsvData(cubetrcio))
                    {
                      /* the data was successfully written*/
                      retval = 1;
                    }
                }
            }
        }
      else 
        {
          /* the auxiliary file exists in CPSDATA/ */
          if (cube_trcio_matchesCsvFile(cubetrcio)) 
            {
              /* the auxiliary file matches the TRCIO data*/
              retval = 1;
            }
        }
    }
  else 
    {
      /* the auxiliary file exists where the data is*/
      if (cube_trcio_matchesCsvFile(cubetrcio)) 
        {
          /* the auxiliary file matches the TRCIO data*/
          retval = 1;
        }
    }

  return retval;
}






CubeTrcio *cube_trcio_create (char *name, int *crossline_hdr,
                             int *inline_hdr, TF_Global *globals)
{

/*
// Originally, the design was intended to be as follows:

   Trcio file must be in sorted order with either the crosslines or the
   inlines varying most rapidly CONSISTENTLY
   crosslines will be associated with the X dimension
   inlines will be associated with the Y dimension
   samples will be associated with the Z dimension
   dead traces must be padded out so data is a perfect cube
   Trcio file must have consistent spacing between crossline values and
     inline values (i.e. a fixed retangular grid)

   But due to a bug reported 03/14/01 (kcc), the following modifications
     were made because of a lack of time to fix the real problem(s):

   Trcio file must be in sorted order with the crosslines varying most
     rapidly CONSISTENTLY, regardless of the given header word designation.
   The routine cube_trcio_checkHeaders was written to check and reassign
     the given header words if they are reversed. The user is responsible to
     assign the header words correctly on the "add cube" dialog box.
     Essentially, the object instantiator only has to know which two header
     words are used when it calls this constructor. Bottom line is that
     cbtr->_sorted_by_x_then_y must always equal 1 as the class stands now.
    
*/
  int error;
  CubeTrcio *cbtr;


  error = 0;
  if (!name || *name == '\0') error = 1;
  if (*crossline_hdr == *inline_hdr) error = 2;

  cbtr = 0;
  cbtr = (CubeTrcio *)malloc (sizeof(CubeTrcio));
  if (!cbtr) {
    error = 3;
  }
  else {
    cbtr->_name                =  0;
    cbtr->_csv_name            =  0;
    cbtr->_status_object       =  0;
    cbtr->_trmaxg              = -1;
    cbtr->_byte_offset_to_data =  0;
    cbtr->_block_byte_size     =  DEFAULT_BLOCK_SIZE;
    cbtr->_csv_unit            =  0;
  }

  if (!error) {
    cbtr->_name = (char *)malloc (strlen(name)+(size_t)1);
    strcpy (cbtr->_name, name);
    cbtr->_crossline_hdr = *crossline_hdr;
    cbtr->   _inline_hdr = *inline_hdr;
    error = cube_trcio_checkHeaders (cbtr, globals);
    if (!error)
      { 
        error = cube_trcio_initialize (cbtr);
      }
  }

  if (error) {
    cube_trcio_printError (error);
    cube_trcio_delete (&cbtr);
  }
  return cbtr;
}

void cube_trcio_delete(CubeTrcio **cbtr)
{
  if ((*cbtr)) {
    if ((*cbtr)->_csv_name) free ((*cbtr)->_csv_name);
    if ((*cbtr)->    _name) free ((*cbtr)->    _name);
    free (*cbtr);
    *cbtr = 0;
  }
}

/*
   reassign the crossline_hdr and inline_hdr if it turns out that the
     file is not sorted 1st by crossline and 2nd by inline. That is make
     it so that cbtr->_sorted_by_x_then_y must be 1
 */
int cube_trcio_checkHeaders (CubeTrcio *cbtr, TF_Global *globals)
{
  int retval, logical_unit, open_file;
  int num_header_words, trace_num, num_samples;
  float trace[1];
  double *headers;
  double first_inline, first_crossline, next_crossline, next_inline, tmp;
  char *file_type, *data_file;


  assert (cbtr);

/* normal return is zero */
  retval = 0;


/* read the first trace */
  open_file = 1;
  num_header_words = workstation_globals_get_nhdwd (globals);

  headers = (double *)malloc ((size_t)num_header_words*sizeof(double));
  trace_num = 1;
  num_samples = 0;
  data_file = workstation_globals_data_file (globals);
  trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
    &trace_num, &logical_unit, &retval, &num_header_words, &num_samples); 
  if (retval) {
    free (headers);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return retval;
  }

 file_type = workstation_globals_ftype (globals); 

  if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_inline_hdr for SEGY as temp fix
 */
    if (headers[cbtr->_inline_hdr-1] == 0) {
      headers[cbtr->_inline_hdr-1] = headers[2];
    }
  }
  open_file = 0;
  first_crossline = headers[cbtr->_crossline_hdr-1];
  first_inline    = headers[cbtr->   _inline_hdr-1];

/* read the second trace to identify the sort orientation
 */
  trace_num = 2;
  trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
    &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
  if (retval) {
    free (headers);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return retval;
  }
 if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_inline_hdr for SEGY as temp fix
 */
    if (headers[cbtr->_inline_hdr-1] == 0) {
      headers[cbtr->_inline_hdr-1] = headers[2];
    }
  }
  next_crossline = headers[cbtr->_crossline_hdr-1];
  next_inline    = headers[cbtr->   _inline_hdr-1];

/* force the headers to be defined so that _sorted_by_x_then_y must
 *   always be 1
 */
  if (!cube_trcio_doublesClose(first_inline   ,next_inline   ) &&
       cube_trcio_doublesClose(first_crossline,next_crossline)   ) {
    tmp                  = cbtr->_inline_hdr   ;
    cbtr->   _inline_hdr = cbtr->_crossline_hdr;
    cbtr->_crossline_hdr = tmp                 ;
  }
  free (headers);
/* close the valid CPS file */
  trciof77wrapper_close_file_ (&logical_unit, &retval);
  return retval;
}

int cube_trcio_initialize (CubeTrcio *cbtr)
{
  int retval, logical_unit, trace_count, dummy, k2, open_file,
    num_header_words, trace_num, num_samples, finished,
    istat;
  float trace[1];
  double *headers, next_crossline, next_inline, cur_section, pred_crossline,
    pred_inline, hdr_inline, hdr_crossline;
  char *file_type, *data_file;
  TF_Global *globals;
  int segy;

  assert (cbtr);

/* normal return is zero */
  retval = 0;

/* get the globals from the valid CPS file */
  globals = workstation_globals_get_globals(cbtr->_name, &istat);
  if (!globals) {
    return istat;
  }

  file_type = workstation_globals_ftype (globals);

  if (globals->ftyp == '\0' || (strcmp(globals->ftyp,"TROT" ) && 
                                      strcmp(globals->ftyp,"DSEGY")))
    {
/* close the file, its not a valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return NOT_VALID_FILE;
  }
  cbtr->_word_type = workstation_globals_get_wdtyp (globals);
  cbtr->_trmaxg    = (double)globals->trmaxg;

/* find dimensions of cube */
  workstation_globals_grid_sizes (globals, &(cbtr->_sample_count), &trace_count,
    &dummy);
  if (cbtr->_sample_count <= 0 || trace_count <= 1) {
    free (globals);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return VALID_FILE_EMPTY;
  }

/* find the _sample_incr */
  cbtr->_sample_incr = workstation_globals_get_srval (globals);

/* find the _first_sample */
  cbtr->_first_sample = workstation_globals_get_tstrt (globals);

/* find the number of bytes per sample */
  cbtr->_bytes_per_sample = workstation_globals_get_nbydp (globals);

/* initialize the scale factor to take ints to floats */
  if(!strcmp(file_type,"DSEGY"))
     segy = 1;
  else
     segy = 0;
  cube_trcio_initializeScaleFactor (cbtr, segy);

/* read the first trace to establish the _first_crossline, _first_inline */
  open_file = 1;
  num_header_words = workstation_globals_get_nhdwd (globals);
  headers = (double *)malloc ((size_t)num_header_words*sizeof(double));
  trace_num = 1;
  num_samples = 0;
  data_file = workstation_globals_data_file (globals);
  trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
    &trace_num, &logical_unit, &retval, &num_header_words, &num_samples); 
  if (retval) {
    free (globals);
    free (headers);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return retval;
  }
  if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
    if (headers[cbtr->_inline_hdr-1] == 0) {
      headers[cbtr->_inline_hdr-1] = headers[2];
    }
  }
  open_file = 0;
  cbtr->_first_crossline = headers[cbtr->_crossline_hdr-1];
  cbtr->_first_inline    = headers[cbtr->   _inline_hdr-1];

/* read the second trace to identify the sort orientation and either
     _crossline_incr or _inline_incr */
  trace_num = 2;
  trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
    &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
  if (retval) {
    free (globals);
    free (headers);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return retval;
  }
  if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
    if (headers[cbtr->_inline_hdr-1] == 0) {
      headers[cbtr->_inline_hdr-1] = headers[2];
    }
  }
  next_crossline = headers[cbtr->_crossline_hdr-1];
  next_inline    = headers[cbtr->   _inline_hdr-1];

  if ( cube_trcio_doublesClose(cbtr->_first_inline,next_inline      ) &&
      !cube_trcio_doublesClose(cbtr->_first_crossline,next_crossline)   ) {
    cbtr->_sorted_by_x_then_y = 1;
    cbtr->_crossline_incr = next_crossline - cbtr->_first_crossline;

/* read each header sequentially until the end of an inline section is
     identified and thus _crossline_count */
    finished = 0;
    for (k2 = 2; k2 < trace_count && !finished; k2++) {
      trace_num = k2 + 1;
      cur_section = next_crossline;
      trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
        &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
      if (!retval) {
        if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
          if (headers[cbtr->_inline_hdr-1] == 0) {
            headers[cbtr->_inline_hdr-1] = headers[2];
          }
        }
        next_crossline = headers[cbtr->_crossline_hdr-1];
        next_inline    = headers[cbtr->   _inline_hdr-1];
        if (cube_trcio_doublesClose(cbtr->_first_inline,next_inline)) {
          if (!cube_trcio_doublesClose(cbtr->_crossline_incr,
            next_crossline-cur_section)) {
/* _crossline_incr is not consistent! */
            retval = FILE_SORT_ERROR;
            finished = 1;
          }
        }
        else {
/* next inline found */
          cbtr->_inline_incr = next_inline - cbtr->_first_inline;
          finished = 1;
        }
      }
      else {
        finished = 1;
      }
    }
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    else {
/* store the crossline count */
      cbtr->_crossline_count = trace_num - 1;
    }

/* read the third inline starting trace as a quick check on the consistency
     of _inline_incr */
    cur_section = next_inline;
    trace_num = cbtr->_crossline_count * 2 + 1;
    trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
      &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
      if (headers[cbtr->_inline_hdr-1] == 0) {
        headers[cbtr->_inline_hdr-1] = headers[2];
      }
    }
    next_inline = headers[cbtr->_inline_hdr-1];

    if (cube_trcio_doublesClose(cbtr->_inline_incr,
      next_inline-cur_section)) {
/* _crossline_incr is not consistent! */
      retval = FILE_SORT_ERROR;
      finished = 1;
    }
/* compute the _inline_count using what is currently known */
    cbtr->_inline_count = trace_count / cbtr->_crossline_count;
/* read the last trace of the last inline to confirm computation */
    trace_num = trace_count;
    trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
      &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
      if (headers[cbtr->_inline_hdr-1] == 0) {
        headers[cbtr->_inline_hdr-1] = headers[2];
      }
    }
    pred_crossline = cbtr->_first_crossline + (cbtr->_crossline_count - 1)
      * cbtr->_crossline_incr;
    pred_inline    = cbtr->_first_inline    + (cbtr->   _inline_count - 1)
      * cbtr->_inline_incr;
    hdr_crossline = headers[cbtr->_crossline_hdr-1];
    hdr_inline    = headers[cbtr->   _inline_hdr-1];
    if (!cube_trcio_doublesClose(hdr_crossline,pred_crossline) ||
        !cube_trcio_doublesClose(hdr_inline   ,pred_inline   )   ) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      printf ("Expected trace number %d to show hdw %d equal to %d\n",
        trace_count, cbtr->_crossline_hdr, (int)pred_crossline);
      printf ("  and hdw %d equal to %d\n", cbtr->_inline_hdr,
        (int)pred_inline);
      return FILE_PRED_ERROR;
    }
  }

  else if (!cube_trcio_doublesClose(cbtr->_first_inline,next_inline) &&
            cube_trcio_doublesClose(cbtr->_first_crossline,
              next_crossline)) {
    cbtr->_sorted_by_x_then_y = 0;
    cbtr->_inline_incr = next_inline - cbtr->_first_inline;

/* read each header sequentially until the end of a crossline section is
     identified and thus _inline_count */
    finished = 0;
    for (k2 = 2; k2 < trace_count && !finished; k2++) {
      trace_num = k2 + 1;
      cur_section = next_inline;
      trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
        &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
      if (!retval) {
        if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
          if (headers[cbtr->_inline_hdr-1] == 0) {
            headers[cbtr->_inline_hdr-1] = headers[2];
          }
        }
        next_inline    = headers[cbtr->   _inline_hdr-1];
        next_crossline = headers[cbtr->_crossline_hdr-1];
        if (cube_trcio_doublesClose(cbtr->_first_crossline,
          next_crossline)) {
          if (!cube_trcio_doublesClose(cbtr->_inline_incr,
            next_inline-cur_section)) {
/* _inline_incr is not consistent! */
            retval = FILE_SORT_ERROR;
            finished = 1;
          }
        }
        else {
/* next crossline found */
          cbtr->_crossline_incr = next_crossline - cbtr->_first_crossline;
          finished = 1;
        }
      }
      else {
        finished = 1;
      }
    }
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    else {
/* store the inline count */
      cbtr->_inline_count = trace_num - 1;
    }

/* read the third crossline starting trace as a quick check on the consistency
     of _crossline_incr */
    cur_section = next_crossline;
    trace_num = cbtr->_inline_count * 2 + 1;
    trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
      &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
      if (headers[cbtr->_inline_hdr-1] == 0) {
        headers[cbtr->_inline_hdr-1] = headers[2];
      }
    }
    next_crossline = headers[cbtr->_crossline_hdr-1];

    if (!cube_trcio_doublesClose(cbtr->_crossline_incr,
      next_crossline-cur_section)) {
/* _crossline_incr is not consistent! */
      retval = FILE_SORT_ERROR;
      finished = 1;
    }
/* compute the _crossline_count using what is currently known */
    cbtr->_crossline_count = trace_count / cbtr->_inline_count;
/* read the last trace of the last crossline to confirm computation */
    trace_num = trace_count;
    trciof77wrapper_get_trace_ (data_file, &open_file, headers, trace,
      &trace_num, &logical_unit, &retval, &num_header_words, &num_samples);
    if (retval) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      return retval;
    }
    if (!strcmp(file_type,"DSEGY")) {
/* this is a cluge but force the 3rd HWD (current group number) into
 * the cbtr->_crossline_hdr for SEGY as temp fix
 */
      if (headers[cbtr->_inline_hdr-1] == 0) {
        headers[cbtr->_inline_hdr-1] = headers[2];
      }
    }
    pred_inline    = cbtr->_first_inline    + (cbtr->   _inline_count - 1)
      * cbtr->_inline_incr;
    pred_crossline = cbtr->_first_crossline + (cbtr->_crossline_count - 1)
      * cbtr->_crossline_incr;
    hdr_inline    = headers[cbtr->   _inline_hdr-1];
    hdr_crossline = headers[cbtr->_crossline_hdr-1];
    if (!cube_trcio_doublesClose(hdr_inline   ,pred_inline   ) ||
        !cube_trcio_doublesClose(hdr_crossline,pred_crossline)   ) {
      free (globals);
      free (headers);
/* close valid CPS file */
      trciof77wrapper_close_file_ (&logical_unit, &retval);
      printf ("Expected trace number %d to show hdw %d equal to %d\n",
        trace_count, cbtr->_crossline_hdr, (int)pred_crossline);
      printf ("  and hdw %d equal to %d\n", cbtr->_inline_hdr,
        (int)pred_inline);
      return FILE_PRED_ERROR;
    }
  }

  else {
    free (globals);
    free (headers);
/* close the valid CPS file */
    trciof77wrapper_close_file_ (&logical_unit, &retval);
    return FILE_SORT_ERROR;
  }

  free (globals);
  free (headers);
/* close the valid CPS file */
  trciof77wrapper_close_file_ (&logical_unit, &retval);
  return retval;
}

int cube_trcio_initializeCsvFile(CubeTrcio *cbtr)
{
  int          retval = cube_trcio_writeCsvHeader    (cbtr);
  if (!retval) retval = cube_trcio_initializeCsvData (cbtr);
  return retval;
}

#define MAXNL 256
int cube_trcio_writeCsvHeader (CubeTrcio *cbtr)
{
  int retval = 0;
  size_t nco, ncw;
  INTEGER start_at_zero = SEEK_SET;
  INTEGER loc_of_offset_block, loc_of_offset_byte, block_size;
  INTEGER byte_offset, block_offset, original_file_size = 0;
  char string[MAXNL];
  INTEGER istream, keep = 0, xdelete = 1;
  struct stat info;

  if (!cbtr || !(cbtr->_csv_name)) return ZSLICE_WRITE_ERROR;

/* establish the size of an extent for the CSV file to write */
  /*First get the original file size*/
  if(stat(cbtr->_name, &info)!=0) 
    return ZSLICE_WRITE_ERROR;

  original_file_size = info.st_size;

  block_size = MinimumValue((INTEGER)DEFAULT_BLOCK_SIZE, original_file_size);
  if (cio_set_file_ext_size_c(&block_size)) {
    return ZSLICE_WRITE_ERROR;
  }
/* open CSV file to write */
  assert (!(cbtr->_csv_unit));
  istream = cio_fopen_c (cbtr->_csv_name, "w", &keep);

/* read back the size of an extent for the CSV file opened */
  cbtr->_block_byte_size = (int32_t)cio_get_file_ext_size_c(&istream);
  if (cbtr->_block_byte_size < 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out file identifier (ascii) assumed to be exactly 66 chars */
  nco = (size_t)sprintf (string,
    "cbtr file contains sample-slices used by CSV for a valid CPS file\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the offset to the sample-slice data (ascii)
 *   the string is assumed to be 57 chars
 *   first write out a dummy number that will be big enough to handle any
 *   size
 */
  nco = (size_t)sprintf (string, "Byte offset to sample-slices:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }
  block_size = (INTEGER)cbtr->_block_byte_size;
  if (cio_ftell_block_and_byte_c(&istream,&block_size,&loc_of_offset_block,
    &loc_of_offset_byte)) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* number is assumed to be 10 chars
 *   the previous three records plus these 10 chars are assumed to be
 *   exactly 106 chars
 */
  nco= (size_t)sprintf(string, "%10.10"PRId32"\n",
    (int32_t)cbtr->_byte_offset_to_data);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the global trace maximum absolute value (needed for SEGY)
 *   the string is assumed to be 36 chars
 *   first write out a dummy number that will be big enough to handle any
 *   size
 */
  nco = (size_t)sprintf (string, "File global maximum absolute value:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* store a double which is forced to be 20 chars
 *   the last five records which includes these 20 chars are assumed to
 *   be exactly 163 chars (including the \n char)
 */
  nco = (size_t)sprintf (string, "%20.12g\n", cbtr->_trmaxg);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the valid CPS file name where the sample-slices are from (ascii)
 */
  nco = (size_t)sprintf (string, "CPS file name:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  assert (strlen(cbtr->_name) <= MAXNL);
  nco = (size_t)sprintf (string, "%s\n", cbtr->_name);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }
    
/* write out the cube size values (ascii) */
  nco = (size_t)sprintf (string,
    "Number of samples, crosslines, inlines:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%d %d %d\n",
    cbtr->_sample_count, cbtr->_crossline_count, cbtr->_inline_count);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the cube origin values (ascii) */
  nco = (size_t)sprintf (string,
    "First value for samples, crosslines, inlines:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%g %g %g\n",
    cbtr->_first_sample, cbtr->_first_crossline, cbtr->_first_inline);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }
  
/* write out the cube increment values (ascii) */
  nco = (size_t)sprintf (string,
    "Increments for samples, crosslines, inlines:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%g %g %g\n",
    cbtr->_sample_incr, cbtr->_crossline_incr, cbtr->_inline_incr);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the word type flag */
  nco = (size_t)sprintf (string, "Word type:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%s\n", "IEEE");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the sort orientation (ascii) */
  nco = (size_t)sprintf (string, "Trace sort orientation:\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  if (!cbtr->_sorted_by_x_then_y) {
    nco = (size_t)sprintf (string, "Sorted by Y then X\n");
  }
  else {
    nco = (size_t)sprintf (string, "Sorted by X then Y\n");
  }
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the header number used to identify crossline values (ascii) */
  nco = (size_t)sprintf (string, "Crossline header word (X):\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%d\n", cbtr->_crossline_hdr);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* write out the header number used to identify inline values (ascii) */
  nco = (size_t)sprintf (string, "Inline header word (Y):\n");
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%d\n", cbtr->_inline_hdr);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* rewrite out the offsets to the sample-slice data (ascii)
 *   after using ftell to find out what to write in place of the dummy
 *   numbers
 */
  if (cio_ftell_block_and_byte_c(&istream,&block_size,&block_offset,
    &byte_offset)) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  if (block_offset != 0) {
/* assuming here that the following is less than 2^(sizeof(int)*4-1) */
    cbtr->_byte_offset_to_data = (int)(block_offset * cbtr->_block_byte_size
      + byte_offset);
  }
  else {
    cbtr->_byte_offset_to_data = (int)byte_offset;
  }

  if (cio_fseek_block_and_byte_c(&istream,&block_size,&loc_of_offset_block,
    &loc_of_offset_byte,&start_at_zero)) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  nco= (size_t)sprintf (string, "%10.10"PRId32"\n",
    (int32_t)cbtr->_byte_offset_to_data);
  if (nco <= 0) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  retval = ncw != nco;
  if (retval) {
    cio_fclose_c (&istream, &keep);
    return ZSLICE_WRITE_ERROR;
  }
  
  cbtr->_csv_unit = (int)istream; /* leave open if no error occurs !!! */
  return retval;
}

int cube_trcio_initializeCsvData (CubeTrcio *cbtr)
{
/* initialize with NULL the data space starting at the end of the header
 * this is a quick way to test for file space availability
 */
  int retval = 0;
  int k2, bytes_per_value, istat, do_status;
  size_t nwo, slice_size, slice_bytes;
  INTEGER istream, keep = 0, xdelete = 1;
  INTEGER start_at_zero = SEEK_SET, loc_of_offset_block, loc_of_offset_byte;
  INTEGER block_size, prev_block, which_block, which_byte;
  char *slice;
  TF_Global *globals;
  int32_t lretval;

  if (!cbtr || !(cbtr->_csv_name) || !(cbtr->_byte_offset_to_data)) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }


/* simply assign the istream */
  assert (cbtr->_csv_unit); /* assume the file is already open and ready!*/
  istream = (INTEGER)cbtr->_csv_unit;
  cbtr->_csv_unit = 0; /* csv unit has done its job */

/* get the globals from the valid CPS file */
  globals = workstation_globals_get_globals(cbtr->_name, &istat);
  if (!globals) {
    cio_fclose_c (&istream, &xdelete);
    return istat;
  }

/* allocate and initialize memory to write out a Z-slice of zero data */
  slice_size = (size_t)(cbtr->_inline_count * cbtr->_crossline_count);
  bytes_per_value = cbtr->_bytes_per_sample > 4 ? 4 : cbtr->_bytes_per_sample;
  assert (bytes_per_value >= 1 && bytes_per_value < 8);
  slice_bytes = slice_size * (size_t)bytes_per_value;
  slice = (char *)calloc (slice_size, (size_t)bytes_per_value);
  if (!slice) {
    free (globals);
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* position file to end of header */
  loc_of_offset_block = (INTEGER)0;
  loc_of_offset_byte  = (INTEGER)0;
  cube_trcio_updateOffset (cbtr, (INTEGER)cbtr->_byte_offset_to_data,
    &loc_of_offset_block, &loc_of_offset_byte);
    
  block_size = (INTEGER)cbtr->_block_byte_size;
  lretval = cio_fseek_block_and_byte_c (&istream, &block_size,
    &loc_of_offset_block, &loc_of_offset_byte, &start_at_zero);

  if (lretval) {
    if      (lretval < INT_MIN) retval = INT_MIN;
    else if (lretval > INT_MAX) retval = INT_MAX;
    else                        retval = (int)lretval;
    free (slice);
    free (globals);
    cio_fclose_c (&istream, &xdelete);
    return retval;
  }

/* attempt to quickly secure file space by writing zero-filled slices out
 * for samples located at the extent boundaries!!!
 */
  prev_block  = loc_of_offset_block;
  which_block = loc_of_offset_block;
  which_byte  = loc_of_offset_byte;
  for (k2 = 0; k2 < cbtr->_sample_count; k2++) {
    if (which_block != prev_block) {
      prev_block = which_block;
      cube_trcio_updateOffset (cbtr, (INTEGER)(-slice_bytes), &which_block,
        &which_byte);
      lretval = cio_fseek_block_and_byte_c (&istream, &block_size,
        &which_block, &which_byte, &start_at_zero);
      if (lretval) {
        free (slice);
        free (globals);
        cio_fclose_c (&istream, &xdelete);
        return ZSLICE_WRITE_ERROR;
      }
      nwo = cio_pfio_write_c (slice, slice_bytes, istream);
      if (nwo != slice_bytes) {
        free (slice);
        free (globals);
        cio_fclose_c (&istream, &xdelete);
        return ZSLICE_WRITE_ERROR;
      }
      if (cbtr->_status_object) {
        do_status = (int)cbtr->_status_function (cbtr->_status_object,
          (float)k2/(float)cbtr->_sample_count);
        if (do_status) {
          free (slice);
          free (globals);
          cio_fclose_c (&istream, &xdelete);
          return ZSLICE_WRITE_ERROR;
        }
      }
    }
    cube_trcio_updateOffset (cbtr, (INTEGER)slice_bytes, &which_block,
      &which_byte);
  }
/* write out the last sample to expand to the end of the last block */
  cube_trcio_updateOffset (cbtr, (INTEGER)(-slice_bytes), &which_block,
    &which_byte);
  lretval = cio_fseek_block_and_byte_c (&istream, &block_size,
    &which_block, &which_byte, &start_at_zero);
  if (lretval) {
    free (slice);
    free (globals);
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }
  nwo = cio_pfio_write_c (slice, slice_bytes, istream);
  if (nwo != slice_bytes) {
    free (slice);
    free (globals);
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }
  if (cbtr->_status_object) {
    do_status = (int)cbtr->_status_function (cbtr->_status_object,
      (float)k2/(float)cbtr->_sample_count);
    if (do_status) {
      free (slice);
      free (globals);
      cio_fclose_c (&istream, &xdelete);
      return ZSLICE_WRITE_ERROR;
    }
  }

  free (slice);
  free (globals);
  cio_fclose_c (&istream, &keep);
  return retval;
}

int cube_trcio_writeCsvData(CubeTrcio *cbtr)
{
/* write out the sample-slice data starting at the end of the header */

  int retval = 0, k2, open_file, logical_unit, num_header_words,
    sample_count, trace_num, slab_width, trace_jump, bytes_per_value,
    slab_size, slab_strip_size, slab_strip_bytes, ierr = 0, row,
    first_trace, first_offset, first_trace_jump, slab_strip_part,
    offset, k3, k4, offset_jump, trace_sum, trace_count,
    istat, do_status;
  INTEGER csv_offset_block, csv_offset_byte, first_csv_offset_block,
    first_csv_offset_byte, block_size;
  INTEGER csv_offset_jump, first_csv_offset_jump, start_at_zero = SEEK_SET;
  short ashort, *pshort;
  size_t nwo, slab_strip_part_bytes;
  INTEGER istream, keep = 0, xdelete = 1;
  float *atrace, *pfloat;
  double *aheader;
  char *slab, *pchar, *file_type;
  TF_Global *globals;
  char *data_file, achar;
  int32_t lretval;

  if (!cbtr || !(cbtr->_csv_name) || !(cbtr->_byte_offset_to_data)) {
    return ZSLICE_WRITE_ERROR;
  }

/* open the CSV file to write */
  istream = cio_fopen_c (cbtr->_csv_name, "r+", &keep);
  if (!istream) {
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* get the globals from the valid CPS file */
  globals = workstation_globals_get_globals(cbtr->_name, &istat);
  if (!globals) {
    cio_fclose_c (&istream, &xdelete);
    return istat;
  }

/* allocate memory to retrieve a trace into */
  sample_count = cbtr->_sample_count;
  num_header_words = workstation_globals_get_nhdwd (globals);
  file_type = workstation_globals_ftype (globals);
  if (!strcmp(file_type,"DSEGY")) {
    num_header_words = globals->nhdwd; /* use globals->nhdwd later */
    cbtr->_trmaxg = -1.0; /* reset the global trace max abs value */
  }
  aheader = (double *)malloc ((size_t)num_header_words*sizeof(double));
  atrace = (float *)malloc ((size_t)sample_count*sizeof(float));

/* allocate memory to store a slab of sample-slices into */
  slab_width = sample_count / cbtr->_inline_count + 1;
/* currently only floats are returned from trcio */
  bytes_per_value = cbtr->_bytes_per_sample > 4 ? 4 : cbtr->_bytes_per_sample;
  assert (bytes_per_value >= 1 && bytes_per_value < 8);
  slab_strip_size = slab_width * cbtr->_inline_count;
  slab_strip_bytes = slab_strip_size * bytes_per_value;
  slab_size = slab_strip_size * cbtr->_sample_count;
  slab = (char *)malloc ((size_t)(slab_size*bytes_per_value));
  if (!slab) {
    free (atrace);
    free (globals);
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

/* initialize for the read */
  open_file     = 1;
  data_file     = workstation_globals_data_file (globals);

  if (cbtr->_sorted_by_x_then_y) {
    trace_jump = cbtr->_crossline_count;
    first_trace_jump = 1;
  }
  else {
    trace_jump = 1;
    first_trace_jump = cbtr->_crossline_count;
  }

  first_trace = 1;
  row = 0;
  first_offset = 0;
  offset_jump = slab_strip_size;

  block_size = (INTEGER)cbtr->_block_byte_size;
  first_csv_offset_block = (INTEGER)0;
  first_csv_offset_byte  = (INTEGER)0;
  cube_trcio_updateOffset (cbtr, (INTEGER)cbtr->_byte_offset_to_data,
    &first_csv_offset_block, &first_csv_offset_byte);
  first_csv_offset_jump = (INTEGER)slab_strip_bytes;
  csv_offset_jump = (INTEGER)(cbtr->_inline_count * cbtr->_crossline_count * 
    bytes_per_value);
  trace_sum = 0;
  trace_count = cbtr->_inline_count * cbtr->_crossline_count;

/* go through all the crosslines sequentially */
  for (k2 = 0; k2 < cbtr->_crossline_count; k2++) {
    trace_num = first_trace;

/* read each trace in a crossline */
    for (k3 = 0; k3 < cbtr->_inline_count; k3++) {
      trciof77wrapper_get_trace_ (data_file, &open_file, aheader, atrace,
        &trace_num, &logical_unit, &retval, &num_header_words,
        &sample_count);

      if (retval) {
        free (globals);
        free (atrace);
        free (slab);
/* close the CSV file */
        cio_fclose_c (&istream, &xdelete);
/* close the valid CPS file */
        trciof77wrapper_close_file_ (&logical_unit, &ierr);
        return retval;
      }
      open_file = 0;

     if (!strcmp(file_type,"DSEGY")) { 
/* update the TRMAXG global */
        cube_trcio_updateTrmaxg (cbtr, aheader[24]);
      }
  
/* scatter move each trace value in the crossline to a timeslice slab */
      offset = first_offset;
      switch (bytes_per_value) {
/* convert the trace value if it will minimize storage requirements */
        case 1:
          for (k4 = 0; k4 < cbtr->_sample_count; k4++) {
            achar = (char)cube_trcio_scaleFloatToInt (cbtr, atrace[k4]);
            pchar = slab + offset;
            memcpy ((void *)pchar, (void *)&achar, (size_t)bytes_per_value);
            offset += offset_jump;
          }
          break;
        case 2:
          for (k4 = 0; k4 < cbtr->_sample_count; k4++) {
            ashort = (short)cube_trcio_scaleFloatToInt (cbtr, atrace[k4]);
            pshort = (short *)(slab + offset * bytes_per_value);
            memcpy ((void *)pshort, (void *)&ashort, (size_t)bytes_per_value);
            offset += offset_jump;
          }
          break;
        case 4:
          for (k4 = 0; k4 < cbtr->_sample_count; k4++) {
            pfloat = (float *)(slab + offset * bytes_per_value);
            memcpy ((void *)pfloat, (void *)(&atrace[k4]),
              (size_t)bytes_per_value);
            offset += offset_jump;
          }
          break;
        default:
          assert (0);
      };
      trace_num += trace_jump;
      first_offset++;
      trace_sum++;

      if (cbtr->_status_object) {
        do_status = (int)cbtr->_status_function (cbtr->_status_object,
          (float)trace_sum/(float)trace_count);
        if (do_status) {
          free (globals);
          free (atrace);
          free (slab);
/* close the valid CPS file */
          trciof77wrapper_close_file_ (&logical_unit, &retval);
/* close the CSV file */
          cio_fclose_c (&istream, &xdelete);
          return ZSLICE_WRITE_ERROR;
        }
      }
    }
    row++;
    if (row == slab_width) {
/* do a series of fwrites to disk at the appropriate locations for
 * each strip of the sample-slices
 */
      offset = 0;
      csv_offset_block = first_csv_offset_block;
      csv_offset_byte  = first_csv_offset_byte ;
      for (k3 = 0; k3 < cbtr->_sample_count; k3++) {
        lretval = cio_fseek_block_and_byte_c (&istream, &block_size,
          &csv_offset_block, &csv_offset_byte, &start_at_zero);
        if (lretval) {
          free (globals);
          free (aheader);
          free (slab);
/* close the valid CPS file */
          trciof77wrapper_close_file_ (&logical_unit, &ierr);
/* close the CSV file */
          cio_fclose_c (&istream, &xdelete);
          if      (lretval < INT_MIN) retval = INT_MIN;
          else if (lretval > INT_MAX) retval = INT_MAX;
          else                        retval = (int)lretval;
          return retval;
        }
        cube_trcio_swap((void *)(&slab[offset]), bytes_per_value,
          slab_strip_size);
        nwo = cio_pfio_write_c (&slab[offset],
          (size_t)slab_strip_bytes, istream);
        if (nwo != (size_t)slab_strip_bytes) {
          free (globals);
          free (atrace);
          free (slab);
/* close the valid CPS file */
          trciof77wrapper_close_file_ (&logical_unit, &retval);
/* close the CSV file */
          cio_fclose_c (&istream, &xdelete);
          return ZSLICE_WRITE_ERROR;
        }
        offset += slab_strip_bytes;
        cube_trcio_updateOffset (cbtr, csv_offset_jump, &csv_offset_block,
          &csv_offset_byte);

        if (cbtr->_status_object) {
          do_status = (int)cbtr->_status_function (cbtr->_status_object,
            (float)trace_sum/(float)trace_count+(float)k3
            /(float)cbtr->_sample_count/100.);
          if (do_status) {
            free (globals);
            free (atrace);
            free (slab);
/* close the valid CPS file */
            trciof77wrapper_close_file_ (&logical_unit, &retval);
/* close the CSV file */
            cio_fclose_c (&istream, &xdelete);
            return ZSLICE_WRITE_ERROR;
          }
        }
      }
      row = 0;
      first_offset = 0;
      cube_trcio_updateOffset (cbtr, first_csv_offset_jump,
        &first_csv_offset_block, &first_csv_offset_byte);
    }
    first_trace += first_trace_jump;
  }

  if (row) {
/* do a series of fwrites to disk at the appropriate locations for
 * final partial strip of the sample-slices
 */
    offset = 0;
    csv_offset_block = first_csv_offset_block;
    csv_offset_byte  = first_csv_offset_byte ;
    slab_strip_part = row * cbtr->_inline_count;
    slab_strip_part_bytes = (size_t)(slab_strip_part * bytes_per_value);
    for (k2 = 0; k2 < cbtr->_sample_count; k2++) {
      lretval = cio_fseek_block_and_byte_c (&istream, &block_size,
        &csv_offset_block, &csv_offset_byte, &start_at_zero);
      if (lretval) {
        free (globals);
        free (aheader);
        free (slab);
/* close the valid CPS file */
        trciof77wrapper_close_file_ (&logical_unit, &ierr);
/* close the CSV file */
        cio_fclose_c (&istream, &xdelete);
        if      (lretval < INT_MIN) retval = INT_MIN;
        else if (lretval > INT_MAX) retval = INT_MAX;
        else                        retval = (int)lretval;
        return retval;
      }
      cube_trcio_swap ((void *)(&slab[offset]), bytes_per_value,
        slab_strip_part);
      nwo = cio_pfio_write_c (&slab[offset],
        slab_strip_part_bytes, istream);
      if (nwo != slab_strip_part_bytes) {
        free (globals);
        free (atrace);
        free (slab);
/* close the valid CPS file */
        trciof77wrapper_close_file_ (&logical_unit, &retval);
/* close the CSV file */
        cio_fclose_c (&istream, &xdelete);
        return ZSLICE_WRITE_ERROR;
      }
      offset += slab_strip_bytes;
      cube_trcio_updateOffset (cbtr, csv_offset_jump, &csv_offset_block,
        &csv_offset_byte);
    }
  }

/* close the CSV file and return */
  free (globals);
  free (atrace);
  free (slab);
/* close the valid CPS file */
  trciof77wrapper_close_file_ (&logical_unit, &retval);


/* in case of SEGY AND not float data (i.e. <= 2 bytes), set the global
 *   trace maximum absolute value to the maximum absolute value of the integer
 */
  if(!strcmp(file_type,"DSEGY") && cbtr->_bytes_per_sample < 4) {
    assert ((cbtr->_bytes_per_sample == 1) || (cbtr->_bytes_per_sample == 2));
    if (cbtr->_bytes_per_sample == 1) {
      assert (cbtr->_trmaxg <= (double)127);
      cbtr->_trmaxg = (double)127; /* will insure a scale factor of 1.0 */
    }
    else {
      assert (cbtr->_trmaxg <= (double)32767);
      cbtr->_trmaxg = (double)32767; /* will insure a scale factor of 1.0 */
    }
  }
  if (cube_trcio_writeTrmaxg (cbtr,istream)) {
/* close the CSV file */
    cio_fclose_c (&istream, &xdelete);
    return ZSLICE_WRITE_ERROR;
  }

  return cio_fclose_c (&istream, &keep);
}

int cube_trcio_getOffsetToData (char *csv_name)
{
  int retval;
  int nor, k2, length;
  INTEGER nci, ncr;
  char string0[MAXNL+1];
  char *string1;
  INTEGER istream, keep = 0;

/* open the file */
  istream = cio_fopen_c (csv_name, "r", &keep);
  if (istream) {

/* read part of the header data */
    nci = (INTEGER)MAXNL;
    ncr = cio_pfio_read_c (string0, nci, istream);
    if (ncr == nci) {
      string1 = string0;
/* skip over the first 2 lines */
      for (k2 = 0; k2 < 2; k2++) {
        length = cube_trcio_getline (string1);
        string1 = &string1[length];
      }
/* read in the offset to the data */
      nor = sscanf (string1, "%d\n", &retval);
      if (nor <= 0) {
        retval = 0;
      }
    }
    else {
      retval = 0;
    }

/* close file */
    cio_fclose_c (&istream, &keep);
  }
  else {
    retval = 0;
  }
  return retval;
}

void cube_trcio_getSectionOffset (CubeTrcio *cbtr, int index,
  INTEGER *which_block, INTEGER *which_byte)
{
  int bytes_per_value, k2;
  INTEGER values_per_section, bytes_per_section;

  bytes_per_value = cbtr->_bytes_per_sample > 4
    ? 4 : cbtr->_bytes_per_sample;
  values_per_section = (INTEGER)(cbtr->_inline_count
    * cbtr->_crossline_count);
  bytes_per_section = (INTEGER)(values_per_section * bytes_per_value);

  *which_block = 0;
  *which_byte  = 0;
  cube_trcio_updateOffset (cbtr, cbtr->_byte_offset_to_data, which_block,
    which_byte);
  for (k2 = 0; k2 < index; k2++) {
    cube_trcio_updateOffset (cbtr, bytes_per_section, which_block,
      which_byte);
  }
}

void cube_trcio_updateOffset (CubeTrcio *cbtr, INTEGER num_bytes,
  INTEGER *which_block, INTEGER *which_byte)
{
  if (num_bytes > 0) {
    for ( ; num_bytes > 0; ) {
      if ((INTEGER)cbtr->_block_byte_size - *which_byte > num_bytes) {
        *which_byte += num_bytes;
        num_bytes = 0;
      }
      else /*if((INTEGER)cbtr->_block_byte_size - *which_byte <= num_bytes)*/ {
        (*which_block)++;
        num_bytes -= (INTEGER)cbtr->_block_byte_size - *which_byte;
        *which_byte = 0;
      }
    }
  }
  else if (num_bytes < 0) {
    for ( ; num_bytes < 0; ) {
      if (*which_byte >= -num_bytes) {
        *which_byte += num_bytes;
        num_bytes = 0;
      }
      else /* if (*which_byte < -num_bytes) */ {
        (*which_block)--;
        num_bytes += *which_byte + 1;
        *which_byte = (INTEGER)(cbtr->_block_byte_size - 1);
      }
    }
  }
}

void cube_trcio_updateTrmaxg (CubeTrcio *cbtr, double lav)
{
  if (cbtr->_trmaxg < DBL_MIN) {
    cbtr->_trmaxg = lav;
  }
  else {
    if (fabs(lav) > cbtr->_trmaxg) cbtr->_trmaxg = lav;
  }
}

int cube_trcio_writeTrmaxg (CubeTrcio *cbtr, INTEGER istream)
{
  int32_t lretval;
  char string[MAXNL];
  INTEGER offset, offset_block, offset_byte, block_size,
    start_at_zero = SEEK_SET;
  size_t nco, ncw;

/* position to the 143rd character from the beginning of the file
 *   write a double which is forced to be 20 chars
 */
  offset = (INTEGER)143;
  offset_block = (INTEGER)0;
  offset_byte  = (INTEGER)0;
  cube_trcio_updateOffset (cbtr, offset, &offset_block, &offset_byte);

  block_size = (INTEGER)cbtr->_block_byte_size;
  lretval = cio_fseek_block_and_byte_c (&istream, &block_size, &offset_block,
    &offset_byte, &start_at_zero);

  if (lretval) {
    return ZSLICE_WRITE_ERROR;
  }

  nco = (size_t)sprintf (string, "%20.12g\n", cbtr->_trmaxg);
  if (nco <= 0) {
    return ZSLICE_WRITE_ERROR;
  }

  ncw = cio_pfio_write_c (string, nco, istream);
  if (ncw != nco) {
    return ZSLICE_WRITE_ERROR;
  }
  return 0; /* normal return */
}

double cube_trcio_getTrmaxg (CubeTrcio *cbtr)
{
  return cbtr->_trmaxg;
}

double cube_trcio_trmaxg (char *csv_name)
{
  int nor;
  double retval;
  INTEGER nci, ncr;
  char string0[MAXNL];
  char string1[21];
  INTEGER istream, keep = 0;

/* open the file */
  istream = cio_fopen_c (csv_name, "r", &keep);
  if (istream) {

/* read the first 163 characters from the CSV file which should include
 * the global trace maximum absolute value
 */
    nci = (INTEGER)163;
    ncr = cio_pfio_read_c (string0, nci, istream);
    if (ncr != nci) {
      retval = 0;
    }
    else {
      strncpy (string1, (const char *)(&string0[143]), 20);
      nor = sscanf (string1, "%lg", &retval);
      if (nor <= 0) {
        retval = -1.;
      }
    }
/* close file */
    cio_fclose_c (&istream, &keep);
  }
  else {
    retval = 0;
  }
  return retval;
}

int cube_trcio_getTrmaxgFromFile (char *name,
                                  int crossline_header, int inline_header,
                                  double *trmaxg )
{
  int retval;
  CubeTrcio *cube;
  int istat;
  TF_Global *globals;

  globals = workstation_globals_get_globals(name, &istat);
  if(!globals)
    {
      return 0;
    }

  cube  = cube_trcio_create(name, &crossline_header, &inline_header,globals);
  if (cube) {
    if (cube_trcio_csvFileExists(cube)) {
      *trmaxg = cube->_trmaxg;
      retval = 1;
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }

  free (globals);

  return retval;
}

/* from the given CubeTrcio pointer, the auxillary CSV file name is found
 * using the CSV file, a new CubeTrcio structure is allocated and a pointer
 * to it is return
 */
CubeTrcio *cube_trcio_createFromCsvFile (CubeTrcio *cbtr)
{
  CubeTrcio *retval;
  int nor, length, k2;
  char *string0 = 0, word_type[8], sort_type[20];
  char *string1 = 0;
  INTEGER offset_to_data, ncr;
  INTEGER istream, keep = 0;

  offset_to_data = (INTEGER)cube_trcio_getOffsetToData (cbtr->_csv_name);
  if (offset_to_data) {

/* open the file */
    istream = cio_fopen_c (cbtr->_csv_name, "r", &keep);
    if (istream) {
/* read the header data */
      string0 = (char *)malloc (sizeof(char)*(size_t)offset_to_data);
      if (string0) {
        ncr = cio_pfio_read_c (string0, offset_to_data,
          istream);
        if (ncr == offset_to_data) {
          retval = (CubeTrcio *)malloc (sizeof(CubeTrcio));
          retval->_csv_name = (char *)malloc (strlen(cbtr->_csv_name)+1);
          strcpy (retval->_csv_name, (const char *)cbtr->_csv_name);
          assert (retval);
          string1 = string0;
/* skip over the first 4 lines */
          for (k2 = 0; k2 < 4; k2++) {
            length = cube_trcio_getline (string1);
            string1 = &string1[length];
          }
/* read in the global trace maximum absolute value */
          nor = sscanf (string1, "%lg\n", &retval->_trmaxg);
          if (nor > 0) {
            for (k2 = 0; k2 < 4; k2++) {
              length = cube_trcio_getline (string1);
              string1 = &string1[length];
            }
/* read in the # of samples, crosslines, and inlines */
            nor = sscanf (string1, "%d %d %d\n", 
              &retval->_sample_count, &retval->_crossline_count,
              &retval->_inline_count);
            if (nor > 0) {
              length = cube_trcio_getline (string1);
              string1 = &string1[length];
              length = cube_trcio_getline (string1);
              string1 = &string1[length];
/* read in the first sample, crossline, and inline values */
              nor = sscanf (string1, "%lg %lg %lg\n", 
                &retval->_first_sample, &retval->_first_crossline,
                &retval->_first_inline);
              if (nor > 0) {
                length = cube_trcio_getline (string1);
                string1 = &string1[length];
                length = cube_trcio_getline (string1);
                string1 = &string1[length];
/* read in the sample, crossline, and inline increment rates */
                nor = sscanf (string1, "%lg %lg %lg\n", 
                  &retval->_sample_incr , &retval->_crossline_incr ,
                  &retval->_inline_incr);
                if (nor > 0) {
                  length = cube_trcio_getline (string1);
                  string1 = &string1[length];
                  length = cube_trcio_getline (string1);
                  string1 = &string1[length];
/* read in the word type */
                  nor = sscanf (string1, "%s\n", word_type);
                  if (nor > 0) {
                    retval->_word_type = 1;
                    length = cube_trcio_getline (string1);
                    string1 = &string1[length];
                    length = cube_trcio_getline (string1);
                    string1 = &string1[length];
/* read in the whether the data is sorted X then Y or vice versa */
                    nor = sscanf (string1, "%s\n", sort_type);
                    if (nor > 0) {
                      if (!strcmp((const char *)sort_type,
                        "Sorted by Y then X")) {
                        retval->_sorted_by_x_then_y = 0;
                      }
                      else {
                        retval->_sorted_by_x_then_y = 1;
                      }
                      length = cube_trcio_getline (string1);
                      string1 = &string1[length];
                      length = cube_trcio_getline (string1);
                      string1 = &string1[length];
/* read in the X header word (crossline values) */
                      nor = sscanf (string1, "%d\n",
                        &retval->_crossline_hdr);
                      if (nor > 0) {
                        length = cube_trcio_getline (string1);
                        string1 = &string1[length];
                        length = cube_trcio_getline (string1);
                        string1 = &string1[length];
/* read in the Y header word (inline values) */
                        nor = sscanf (string1, "%d\n",
                          &retval->_inline_hdr);
                        if (nor > 0) {
                          retval->_name = (char *)malloc
                            (strlen(cbtr->_name)+1);
                          strcpy (retval->_name,
                            (const char *)cbtr->_name);
                          retval->_byte_offset_to_data
                            = cbtr->_byte_offset_to_data;
                          retval->_bytes_per_sample
                            = cbtr->_bytes_per_sample;
                          cube_trcio_initializeScaleFactor (retval, 0);
                          retval->_block_byte_size
                            = (int32_t)cio_get_file_ext_size_c (&istream);
                        }
                        else {
                          retval = 0;
                        }
                      }
                      else {
                        retval = 0;
                      }
                    }
                    else {
                      retval = 0;
                    }
                  }
                  else {
                    retval = 0;
                  }
                }
                else {
                  retval = 0;
                }
              }
              else {
                retval = 0;
              }
            }
            else {
              retval = 0;
            }
          }
          else {
            retval = 0;
          }
        }
        else {
          retval = 0;
        }
      }
      else {
        retval = 0;
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  if (string0) free (string0);
  return retval;
}

/* index is zero-relative
 * the float array asection is assumed to be allocated previously
 * for the nontransposed result, the section is returned sorted by X then Y
 */
int cube_trcio_readCsvSection(CubeTrcio *cbtr, int index, int transpose,
  float *asection)
{
  int retval = 0, bytes_per_value, k2, k3, in, out, first_out, in_jump,
    out_jump, first_out_jump;
  float afloat, *pfloat;
  INTEGER start_at_zero = SEEK_SET, which_block, which_byte, block_size;
  short ashort, *pshort;
  char *ptr;
  INTEGER bytes_per_section, nri, values_per_section;
  INTEGER istream, keep = 0;
  int32_t lretval;

  istream = cio_fopen_c (cbtr->_csv_name, "r", &keep);
  if (istream) {

    cube_trcio_getSectionOffset (cbtr, index, &which_block, &which_byte);

    block_size = (INTEGER)cbtr->_block_byte_size;
    lretval = cio_fseek_block_and_byte_c (&istream, &block_size, &which_block,
      &which_byte, &start_at_zero);

    bytes_per_value = cbtr->_bytes_per_sample > 4
      ? 4 : cbtr->_bytes_per_sample;
    values_per_section = (INTEGER)(cbtr->_inline_count
      * cbtr->_crossline_count);
    bytes_per_section = (INTEGER)(values_per_section * bytes_per_value);

    if (!lretval) {
      ptr = (char *)malloc ((size_t)bytes_per_section);
      nri = cio_pfio_read_c (ptr, bytes_per_section, istream);
      if (nri == bytes_per_section) {
        cube_trcio_swap ((void *)ptr, bytes_per_value,
          (int)values_per_section);
        if (transpose) {
          in             = 0;
          in_jump        = bytes_per_value;
          first_out      = 0;
          first_out_jump = 1;
                out_jump = cbtr->_crossline_count;
          switch (bytes_per_value) {
            case 1:
              for (k2 = 0; k2 < cbtr->_inline_count; k2++) {
                out = first_out;
                for (k3 = 0; k3 < cbtr->_crossline_count; k3++) {
                  asection[out] = cube_trcio_scaleIntToFloat (cbtr,
                    (int)ptr[in]);
                  in  +=  in_jump;
                  out += out_jump;
                }
                first_out += first_out_jump;
              }
              break;
            case 2:
              for (k2 = 0; k2 < cbtr->_inline_count; k2++) {
                out = first_out;
                for (k3 = 0; k3 < cbtr->_crossline_count; k3++) {
                  pshort = (short *)(&ptr[in]);
                  memcpy ((void *)&ashort, (void *)pshort,
                    (size_t)bytes_per_value);
                  asection[out] = cube_trcio_scaleIntToFloat (cbtr,
                    (int)ashort);
                  in  +=  in_jump;
                  out += out_jump;
                }
                first_out += first_out_jump;
              }
              break;
            case 4:
              for (k2 = 0; k2 < cbtr->_inline_count; k2++) {
                out = first_out;
                for (k3 = 0; k3 < cbtr->_crossline_count; k3++) {
                  pfloat = (float *)(&ptr[in]);
                  memcpy ((void *)&afloat, (void *)pfloat,
                    (size_t)bytes_per_value);
                  asection[out] = afloat;
                  out += out_jump;
                  in  +=  in_jump;
                }
                first_out += first_out_jump;
              }
              break;
            default:
              assert (0);
          };
        }
        else {
          switch (bytes_per_value) {
            case 1:
              for (k2 = 0; k2 < (int)values_per_section; k2++) {
                asection[k2] = cube_trcio_scaleIntToFloat (cbtr,
                  (int)ptr[k2]);
              }
              break;
            case 2:
              in = 0;
              in_jump = bytes_per_value;
              for (k2 = 0; k2 < (int)values_per_section; k2++) {
                pshort = (short *)(&ptr[in]);
                memcpy ((void *)&ashort, (void *)pshort,
                  (size_t)bytes_per_value);
                asection[k2] = cube_trcio_scaleIntToFloat (cbtr,
                  (int)ashort);
                in += in_jump;
              }
              break;
            case 4:
              in = 0;
              in_jump = bytes_per_value;
              for (k2 = 0; k2 < (int)values_per_section; k2++) {
                pfloat = (float *)(&ptr[in]);
                memcpy ((void *)&afloat, (void *)pfloat,
                  (size_t)bytes_per_value);
                asection[k2] = afloat;
                in += in_jump;
              }
              break;
            default:
              assert (0);
          };
        }
      }
      else {
        free (ptr);
        cio_fclose_c (&istream, &keep);
        retval = ZSLICE_READ_ERROR;
      }
    }
    else {
      if      (lretval < INT_MIN) retval = INT_MIN;
      else if (lretval > INT_MAX) retval = INT_MAX;
      else                        retval = (int)lretval;
      cio_fclose_c (&istream, &keep);
    }
  }
  else {
    cio_fclose_c (&istream, &keep);
    retval = ZSLICE_READ_ERROR;
  }
  return retval;
}

/* given the global trace maximum abs value a'priori, compute a scale factor
 *   to take ints to floats. Note that the Z-slice values for 1 and 2 byte
 *   seismic data will never match the inline or crossline amplitude values
 *   because for cross-sections hdr wrd #25 is used for processing the 1
 *   and 2 byte trace data. The only way to make Z-slices comparable
 *   would be to store or create (by reading every header!) a "scale factor
 *   slice" with the Z-slice data and then adjust each stored Z-slice
 *   value accordingly. cbtr class is not doing that yet!
 *   If it is decided to store the "scale factor slice" then put it at the
 *   bottom of the cube.  For backward compatibility, try to read the
 *   scale slice first and if an error occurs, then initially read every
 *   header from the *.trc* file and later modify the *.csv file (if
 *   possible) for future accesses. cbtr is preferable to creating it
 *   everytime.
 */
void cube_trcio_initializeScaleFactor (CubeTrcio *cbtr, int use_unity_factor)
{
  if (use_unity_factor) {
/* e.g. SEGY data does not have the global trace maximum abs value a'priori */
    cbtr->_scale_int_to_float = (float)1;
  }
  else {
    if (cbtr->_bytes_per_sample == 1) {
      cbtr->_scale_int_to_float = (float)(cbtr->_trmaxg / (double)127);
    }
    else if (cbtr->_bytes_per_sample == 2) {
      cbtr->_scale_int_to_float = (float)(cbtr->_trmaxg / (double)32767);
    }
    else {
      cbtr->_scale_int_to_float = (float)(cbtr->_trmaxg / (double)2147483647);
    }
  }
}

/* compute a int from a float */
int cube_trcio_scaleFloatToInt (CubeTrcio *cbtr, float in)
{
  int out;
  out = (int)(in / cbtr->_scale_int_to_float); /* scale fact may be given*/
  return out;
}

/* compute a float from an int */
float cube_trcio_scaleIntToFloat (CubeTrcio *cbtr, int in)
{
  float out;
  out = (float)in * cbtr->_scale_int_to_float; /* scale fact may be given*/
  return out;
}

int cube_trcio_csvFileExists(CubeTrcio *cbtr)
{
  int retval;

  if (cube_trcio_csvName(cbtr)) {
/* find out offset to data */
    cbtr->_byte_offset_to_data = cube_trcio_getOffsetToData (cbtr->_csv_name);
    if (cbtr->_byte_offset_to_data) {
/* find out the global trace maximum absolute value (need for SEGY type) */
      cbtr->_trmaxg = cube_trcio_trmaxg (cbtr->_csv_name);
      retval = 1;
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

const char *cube_trcio_csvName(CubeTrcio *cbtr)
{
  const char *path, *pref, *name;
  size_t path_length, pref_length;
  if (cbtr) {
    if (!(cbtr->_csv_name)) {
      name = (const char *)cbtr->_name;
/* create default CSV file name */
      path = cube_trcio_filePath(name, &path_length);
      pref = cube_trcio_filePrefix(name, &pref_length);
      cbtr->_csv_name = (char *)malloc (path_length+pref_length+(size_t)5);
      strncpy (cbtr->_csv_name, path, path_length);
      cbtr->_csv_name[path_length] = '\0';
      strncat (cbtr->_csv_name, pref, pref_length);
      strcat  (cbtr->_csv_name, ".csv");
    }
    return (const char *)cbtr->_csv_name;
  }
  else {
    return 0;
  }
}

/* this is provided in order to allow a user defined name for the 
 * sample-slice file
 */
const char *cube_trcio_setCsvName(CubeTrcio *cbtr, const char *name)
{
  const char *retval;
  if (cbtr) {
    if (cbtr->_csv_name) free (cbtr->_csv_name);
    cbtr->_csv_name = (char *)malloc (strlen(name)+1);
    if (cbtr->_csv_name) {
      strcpy (cbtr->_csv_name, name);
      retval = (const char *)cbtr->_csv_name;
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

const char *cube_trcio_setCsvNamePath(CubeTrcio *cbtr, const char *path)
{
  const char *retval;
  const char *name;
  char *fn, exp_path[MAXNL];
  size_t nl, fnl;

  if (cbtr) {
    if (cbtr->_csv_name) {
      name = cube_trcio_fileNoPath(cbtr->_csv_name, &nl);
    }
    else {
      name = cube_trcio_fileNoPath(cbtr->_name, &nl);
    } 
    exptilde_crou2 (exp_path, (char *)path);
    fnl  = strlen ((const char *)exp_path) + nl + 1; /* add one for '\0' */
    fn   = (char *)malloc (fnl);
    strcpy (fn, (const char *)exp_path);
    strcat (fn, name);
    free (cbtr->_csv_name);
    cbtr->_csv_name = fn;
    retval = (const char *)cbtr->_csv_name;
  }
  else {
    retval = 0;
  }
  return retval;
}

const char *cube_trcio_fileNoPath(const char *name, size_t *length)
{
/* get filename with out the path from name */
  size_t pl;
  const char *path;
  const char *retval;

  if (name) {
    path = cube_trcio_filePath(name, &pl);
    if (pl) {
      retval = (const char *)&(path[pl]);
    }
    else {
      retval = name;
    }
    *length = strlen (retval);
  }
  else {
    *length = 0;
    retval = 0;
  }
  return retval;
}

const char *cube_trcio_filePrefix(const char *name, size_t *length)
{
/* get name prefix from filename */
  size_t pl, el;
  const char *path;
  const char *extension;
  const char *retval;

  if (name) {
    path = cube_trcio_filePath(name, &pl);
    if (pl) {
      retval = (const char *)&(path[pl]);
    }
    else {
      retval = name;
    }
    extension = cube_trcio_fileExtension (name, &el);
    if (extension) {
      *length = (size_t)(extension - retval - 1); /* don't include '.' */
    }
    else {
      *length = strlen (retval);
    }
  }
  else {
    *length = 0;
    retval = 0;
  }
  return retval;
}

const char *cube_trcio_filePath(const char *name, size_t *length)
{
/* get filename path from name*/
  const char *retval;
  char *fname;

  if (name) {
    fname = strrchr (name, '/');
    if (fname) {
      *length = (size_t)(fname - name + 1);
      retval = (const char *)name;
    }
    else {
      *length = 0;
      retval = 0;
    }
  }
  else {
    *length = 0;
    retval = 0;
  }
  return retval;
}

/* get filename's extension
 * extension points to the first character after the first '.'
 * the length is set to the number of characters following the first '.'
 * if no '.' exists then NULL is returned.
 * if a '.' exists but is followed immediately by a NULL, then length is
 * set to zero but a pointer to the NULL is returned
 */
const char *cube_trcio_fileExtension (const char *name, size_t *length)
{
  const char *retval;

  if (name) {
    retval = (const char *)strchr (name, '.');
    if (retval) {
      retval++; /* first character after the first '.' */
      *length = strlen (retval);
    }
    else {
      *length = 0;
      retval = 0;
    }
  }
  else {
    *length = 0;
    retval = 0;
  }
  return retval;
}

Grid3DDesc *cube_trcio_grid3DDesc (CubeTrcio *cbtr)
{
  int n1, n2, n3, X, Y, Z, cntnu, ftype, cmpx, cmpy;
  float o1, o2, o3, d1, d2, d3;
  char lab1[5], lab2[12], lab3[12], pname[8], oname[1];
  Grid3DDesc *retval;
  
  retval = 0;

  assert (cbtr);
  if (cbtr) {
    strcpy (pname, "Seismic");
    strcpy (oname, "");
    cntnu = 1;
    ftype = TROT_TYPE;

    cmpx = cbtr->_crossline_hdr;
    cmpy = cbtr->   _inline_hdr;

    if (cbtr->_sorted_by_x_then_y) {
      o1 = (float)cbtr->_first_sample;
      o2 = (float)cbtr->_first_crossline;
      o3 = (float)cbtr->_first_inline;
      n1 = cbtr->   _sample_count;
      n2 = cbtr->_crossline_count;
      n3 = cbtr->   _inline_count;
      d1 = (float)cbtr->   _sample_incr;
      d2 = (float)cbtr->_crossline_incr;
      d3 = (float)cbtr->   _inline_incr;
      X  = 2;
      Y  = 3;
      Z  = 1;
      if ((cmpx == 7 && cmpy == 8) ||
          (cmpx == 8 && cmpy == 7)   ) {
        strcpy (lab2, "XGRID");
        strcpy (lab3, "YGRID");
      }
      else if ((cmpx == 17 && cmpy == 18) ||
               (cmpx == 18 && cmpy == 17)   ) {
        strcpy (lab2, "XBSMT");
        strcpy (lab3, "YBSMT");
      }
      else if ((cmpx == 37 && cmpy == 38) ||
               (cmpx == 38 && cmpy == 37)   ) {
        strcpy (lab2, "XANNOTATION");
        strcpy (lab3, "YANNOTATION");
      }
      else {
        strcpy (lab2, "XAXIS");
        strcpy (lab3, "YAXIS");
      }
    }
    else {
      o1 = (float)cbtr->_first_sample;
      o2 = (float)cbtr->_first_inline;
      o3 = (float)cbtr->_first_crossline;
      n1 = cbtr->   _sample_count;
      n2 = cbtr->   _inline_count;
      n3 = cbtr->_crossline_count;
      d1 = (float)cbtr->   _sample_incr;
      d2 = (float)cbtr->   _inline_incr;
      d3 = (float)cbtr->_crossline_incr;
      X  = 3;
      Y  = 2;
      Z  = 1;
      if ((cmpx == 7 && cmpy == 8) ||
          (cmpx == 8 && cmpy == 7)   ) {
        strcpy (lab3, "XGRID");
        strcpy (lab2, "YGRID");
      }
      else if ((cmpx == 17 && cmpy == 18) ||
               (cmpx == 18 && cmpy == 17)   ) {
        strcpy (lab3, "XBSMT");
        strcpy (lab2, "YBSMT");
      }
      else if ((cmpx == 37 && cmpy == 38) ||
               (cmpx == 38 && cmpy == 37)   ) {
        strcpy (lab3, "XANNOTATION");
        strcpy (lab2, "YANNOTATION");
      }
      else {
        strcpy (lab3, "XAXIS");
        strcpy (lab2, "YAXIS");
      }
    }

    strcpy (lab1, "TIME");

    if (cntnu) {
      retval = cube_trcio_create_desc (
        cbtr->_name,        /* name of header file                        */
        cbtr->_name,        /* name of data file                          */
        pname,              /* optional property name for the binary data */
        oname,              /* optional object name for cbtr data set...  */
        o1,                 /* Origin along axis-1, the fastest axis      */
        o2,                 /* Origin along axis-2, the medium  axis      */
        o3,                 /* Origin along axis-3, the slowest axis      */
        n1,                 /* grid size along axis-1, the fastest axis   */
        n2,                 /* grid size along axis-2, the medium  axis   */
        n3,                 /* grid size along axis-3, the fastest axis   */
        d1,                 /* grid incr along axis-1, the fastest axis   */
        d2,                 /* grid incr along axis-2, the medium  axis   */
        d3,                 /* grid incr along axis-3, the slowest axis   */
        lab1,               /* name of axis-1, the fastest axis           */
        lab2,               /* name of axis-2, the fastest axis           */
        lab3,               /* name of axis-3, the medium axis            */
        X,                  /* which axis is X                            */
        Y,                  /* which axis is Y                            */
        Z,                  /* which axis is Z                            */
        ftype,              /* defined file type (see tfio.h)             */
        cbtr->_word_type);  /* type of word used for the binary data      */
    }
  }
  else {
    retval = 0;
  }
  return retval;
}



Grid3DDesc *cube_trcio_create_desc(char *hfile, char *dfile,
                                   char *pname, char *obj_name,
                                   float o1, float o2, float o3,
                                   int   n1, int   n2, int   n3,
                                   float d1, float d2, float d3, 
                                   char *lab1,char *lab2,char *lab3,
                                   int X, int Y, int Z,
                                   int ftype, int  wdtype) 
{
  Grid3DDesc *h=0;
  int dsize=1;

  if(n1<1) n1=1;
  if(n2<1) n2=1;
  if(n3<1) n3=1;
  if(d1==0.) d1=1.0;
  if(d2==0.) d2=1.0;
  if(d3==0.) d3=1.0;
  dsize=4;
  h = (Grid3DDesc *) calloc(1,sizeof(Grid3DDesc));

  switch(ftype)
    { 
      case TROT_TYPE:
        cube_trcio_desc_init(h);
        break;

      default:
        if(h) free(h);
        return 0;

    }

  strcpy(h->header_file,hfile);
  strcpy(h->name,obj_name);
  strcpy(h->P.name,pname);
  strcpy(h->P.file,dfile);
  strcpy(h->P.etype,"IEEE");
  h->P.esize = dsize;
  h->ftype  = ftype;
  h->N.v[0] = n1;
  h->N.v[1] = n2;
  h->N.v[2] = n3;
  h->O.v[0] = o1;
  h->O.v[1] = o2;
  h->O.v[2] = o3;
  h->D.v[0] = d1;
  h->D.v[1] = d2;
  h->D.v[2] = d3;
  h->MI.v[0] =h->O.v[0]; 
  h->MI.v[1] =h->O.v[1]; 
  h->MI.v[2] =h->O.v[2]; 
  h->MA.v[0] =h->O.v[0] + (h->N.v[0]-1)*h->D.v[0]; 
  h->MA.v[1] =h->O.v[1] + (h->N.v[1]-1)*h->D.v[1]; 
  h->MA.v[2] =h->O.v[2] + (h->N.v[2]-1)*h->D.v[2]; 
  if(lab1) strcpy(h->axis.v[0],lab1);
  if(lab2) strcpy(h->axis.v[1],lab2);
  if(lab3) strcpy(h->axis.v[2],lab3);
  h->hd.v[0]=UNDEFHDR;
  h->hd.v[1]=UNDEFHDR;
  h->hd.v[2]=UNDEFHDR;

  cube_trcio_setuvw( h, X, Y, Z);

  return h;
}


void cube_trcio_desc_init(Grid3DDesc *h)
{
  int i=0;

  if(!h) return;
 
  h->ftype=UNKWN_TYPE;
  strcpy(h->keys[i],"N1"); h->fmt[i]='i'; i++;
  strcpy(h->keys[i],"N2"); h->fmt[i]='i'; i++;
  strcpy(h->keys[i],"N3"); h->fmt[i]='i'; i++;
  strcpy(h->keys[i],"O1"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"O2"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"O3"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"D1"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"D2"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"D3"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"AXIS1"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"AXIS2"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"AXIS3"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"FILE"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"PROPERTY"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"WORDTYPE"); h->fmt[i]='c'; i++;
  strcpy(h->keys[i],"AXIS_U"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"AXIS_V"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"AXIS_W"); h->fmt[i]='f'; i++;
  strcpy(h->keys[i],"HD1"); h->fmt[i]='i'; i++;
  strcpy(h->keys[i],"HD2"); h->fmt[i]='i'; i++;
  strcpy(h->keys[i],"HD3"); h->fmt[i]='i'; i++;
  h->nkey= i;
  h->hd.v[0]=UNDEFHDR;
  h->hd.v[1]=UNDEFHDR;
  h->hd.v[2]=UNDEFHDR;
  strcpy(h->type.v[0],"even");
  strcpy(h->type.v[1],"even");
  strcpy(h->type.v[2],"even");
  strcpy(h->axis.v[0],"axis-1");
  strcpy(h->axis.v[1],"axis-2");
  strcpy(h->axis.v[2],"axis-3");
  h->U.v[0]=0.0;
  h->U.v[1]=0.0;
  h->U.v[2]=0.0;
  h->V.v[0]=0.0;
  h->V.v[1]=0.0;
  h->V.v[2]=0.0;
  h->W.v[0]=0.0;
  h->W.v[1]=0.0;
  h->W.v[2]=0.0;

  h->np = 0;
  h->P.esize = 4;
  strcpy(h->P.name,"UNKNOWN");
  strcpy(h->P.file,"NONE");
  strcpy(h->P.etype,"WIEEE");
  strcpy(h->P.keys[0],"PROPERTY");
  strcpy(h->P.keys[1],"FILE");
  strcpy(h->P.keys[2],"PROP_ESIZE");
  strcpy(h->P.keys[3],"WORDTYPE"); /* redundant */
  h->P.nkey =4;
}


void cube_trcio_setuvw(Grid3DDesc *h, int X, int Y, int Z)
{
  int        nn;
  float      al[3]; /* for axis lengths */

  if(!h) return;

  /* Assumes h->N.v and h->D.v have been set beforehand */
  nn = (h->N.v[0]>1) ? h->N.v[0]-1 : 1;
  al[0] = nn*h->D.v[0];
  nn = (h->N.v[1]>1) ? h->N.v[1]-1 : 1;
  al[1] = nn*h->D.v[1];
  nn = (h->N.v[2]>1) ? h->N.v[2]-1 : 1;
  al[2] = nn*h->D.v[2];
  h->U.v[0]=0.; h->U.v[1]=0.; h->U.v[2]=0.;
  h->V.v[0]=0.; h->V.v[1]=0.; h->V.v[2]=0.;
  h->W.v[0]=0.; h->W.v[1]=0.; h->W.v[2]=0.;
  switch(X) {/* set the nonzero components */
    case 1:
      h->U.v[0]  = al[X-1];
      if(Y==2) {
        h->V.v[1]  = al[Y-1];
        h->W.v[2]  = al[Z-1];
      }
      else {
        h->V.v[2]  = al[Z-1];
        h->W.v[1]  = al[Y-1];
      }
      break;
    case 2:
      h->V.v[0]  = al[X-1];
      if(Z==1) {
        h->W.v[1]  = al[Y-1];
        h->U.v[2]  = al[Z-1];
      }
      else {
        h->W.v[2]  = al[Z-1];
        h->U.v[1]  = al[Y-1];
      }
      break;
    case 3:
      h->W.v[0]  = al[X-1];
      if(Z==1) {
        h->V.v[1]  = al[Y-1];
        h->U.v[2]  = al[Z-1];
      }
      else {
        h->V.v[2]  = al[Z-1];
        h->U.v[1]  = al[Y-1];
      }
  }

}









int cube_trcio_matches(CubeTrcio *cbtr, char *name)
{
  int retval;
  if (cbtr) {
    retval = !strcmp (cbtr->_name, name);
  }
  else {
    retval = 0;
  }
  return retval;
}

int cube_trcio_matchesCsvFile(CubeTrcio *cbtr)
{
  int retval;
  CubeTrcio *cube;

  retval = 0;

/* does the csv file description match this initialized cube */
  if (cbtr) {
    if (cube_trcio_csvFileExists(cbtr)) {
      cube = cube_trcio_createFromCsvFile (cbtr);
      if (cube) {
        if (cube_trcio_descriptionsMatch(cbtr,cube)) {
          retval = 1;
        }
        cube_trcio_delete(&cube);
      }
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

void cube_trcio_setStatusFunction (CubeTrcio *cbtr,
                                  StatusFunction func, void *obj)
{
  cbtr->_status_function = func;
  cbtr->_status_object   = obj ;
}

int cube_trcio_descriptionsMatch (CubeTrcio *cbtr, CubeTrcio *cube)
{
/* all except the path of the TRCIO file, the CSV file name, block byte size,
 * the offset to the data, the bytes per sample, the int to float scale
 * factor, and abort subff have to be identical before a match is established
 */
  int retval;
  size_t tlen, clen;
  const char *tname, *cname;
  tname = (const char *)cbtr->_name;
  cname = (const char *)cube->_name;
  retval = (!strcmp (cube_trcio_filePrefix  (tname,&tlen),
                     cube_trcio_filePrefix  (cname,&clen) )       ) &&
           (!strcmp (cube_trcio_fileExtension(tname,&tlen),
                     cube_trcio_fileExtension(cname,&clen) )       ) &&
           (cbtr->_trmaxg             == cube->_trmaxg            ) &&
           (cbtr->_crossline_hdr      == cube->_crossline_hdr     ) &&
           (cbtr->   _inline_hdr      == cube->   _inline_hdr     ) &&
           (cbtr->   _sample_count    == cube->   _sample_count   ) &&
           (cbtr->_crossline_count    == cube->_crossline_count   ) &&
           (cbtr->   _inline_count    == cube->   _inline_count   ) &&
           (cbtr->_first_sample       == cube->_first_sample      ) &&
           (cbtr->_first_crossline    == cube->_first_crossline   ) &&
           (cbtr->_first_inline       == cube->_first_inline      ) &&
           (cbtr->_crossline_incr     == cube->_crossline_incr    ) &&
           (cbtr->   _inline_incr     == cube->   _inline_incr    ) &&
           (cbtr->_word_type          == cube->_word_type         ) &&
           (cbtr->_sorted_by_x_then_y == cube->_sorted_by_x_then_y)   ;
  return retval;
}

int cube_trcio_getline (char *string)
{
  int retval, k2;
  size_t len;
  char c;

  if (string) {
    len = strlen ((const char *)string);
    c = '\0';
    for (k2 = 0; k2 < (int)len && c != '\n'; k2++) {
      c = string[k2];
    }
    retval = k2;
  }
  else {
    retval = 0;
  }
  return retval;
}

void cube_trcio_printError (int error)
{
  switch (error) {
    case NOT_VALID_FILE:
      printf ("CubeTrcio - Unsupported file type error\n");
      break;
    case VALID_FILE_EMPTY:
      printf ("CubeTrcio - File was empty\n");
      break;
    case FILE_SORT_ERROR:
      printf ("CubeTrcio - Error in sort of Hdrs 7 & 8\n");
      break;
    case FILE_PRED_ERROR:
      printf ("CubeTrcio - File is not a rectangular volume\n");
      break;
    case ZSLICE_WRITE_ERROR:
      printf ("CubeTrcio - Error writing the CSV file\n");
      break;
    case ZSLICE_READ_ERROR:
      printf ("CubeTrcio - Error reading the CSV file\n");
      break;
    default:
      printf ("CubeTrcio - I/O error %d\n", error);
      break;
  };
}

int cube_trcio_X (CubeTrcio *cbtr)
{
  assert (cbtr);
  return cbtr->_crossline_hdr;
}

int cube_trcio_Y (CubeTrcio *cbtr)
{
  assert (cbtr);
  return cbtr->_inline_hdr;
}

int cube_trcio_supported (char *name)
{
  int retval, istat;
  TF_Global *globals;

/* get the globals from the valid CPS file */
  globals = workstation_globals_get_globals(name, &istat);
  if (globals) {

    if (globals->ftyp == '\0' || (strcmp(globals->ftyp,"TROT" ) 
                                  && strcmp(globals->ftyp,"DSEGY"))){
      retval = 0;
    }
    else {
      retval = 1;
    }
  }
  else {
    retval = 0;
  }
  free (globals);
  return retval;
}

int cube_trcio_doublesClose (double first, double second)
{
  /* this assumes that the doubles should have been ints!!!! */
  int retval;
  if (floor(first+0.5) == floor(second+0.5)) {
    retval = 1;
  }
  else {
    retval = 0;
  }
  return retval;
}


/*===========================================================================
  =============== Handle byte swapping for various platforms ================
  ===========================================================================*/
static void cube_trcio_swap(void *ptr, int size, int nobj)
{
  if(swap_endian() || nobj == 0)
    return;

  switch(size)
    {
      case 2: swap_short_2_cvec((short *)ptr, &nobj);
        break;
      case 4: swap_float_4_cvec((float *)ptr, &nobj);
        break;
      case 8: swap_double_8_cvec((double *)ptr, &nobj);
        break;
    }
}
