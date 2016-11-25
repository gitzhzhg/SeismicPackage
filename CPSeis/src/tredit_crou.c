/* !<CPS_v1 type="AUXILIARY_FILE"/> */

/*------------------------------ tredit_crou.c -------------------------------*/
/*------------------------------ tredit_crou.c -------------------------------*/
/*------------------------------ tredit_crou.c -------------------------------*/

/****
For documentation, see

    tredit.f90

****/

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
!                   C P S   P R O C E S S   F I L E
!
! Name       : tredit_crou
! Category   : headers
! Written    : 2000-06-20   by: SMCook
! Revised    : 2007-12-04   by: Bill Menger  
! Maturity   : beta
! Purpose    : Edit traces according to information in a TREDIT file or BYFIL
!              spreadsheet.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                              REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2007-12-04  Bill Menger  Fix calling args for tredit_crou_any_hdr_wrd_is_valid
!  6. 2005-05-31  Stoeckley    Fix to compile with C++.
!  5. 2004-02-02  Goodger      Changed auxiliary in brief doc section to 
!                              process.
!  4. 2003-04-29  SMCook       Added #include <string.h> to eliminate compile
!                               warning.
!                              Eliminated tabs.
!                              Added brief_doc section to comply with current
!                               code check-in standards.
!  3. 2000-08-15  Cook         Made printout more readable by removing nils.
!  2. 2000-07-26  Cook         Added user-readable printout 'sprintf_record'.
!                              Added 'flagging_requested' argument.
!  1. 2000-06-20  Cook         Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

/*---------------------------- start of coding -------------------------------*/
/*---------------------------- start of coding -------------------------------*/
/*---------------------------- start of coding -------------------------------*/

char *TREDIT_CROU_IDENT=
 "$Id: tredit_crou.c,v 1.7 2007/12/05 15:05:54 Menger beta sps $";

#include <stdio.h> /* for debug print statements */
#include <stdlib.h>
#include <string.h>

#include "c2f_interface.h"
#include "tredfile.h"


#ifdef NEED_UNDERSCORE
#define  tredit_crou_read_file        tredit_crou_read_file_
#define  tredit_crou_mimic_read_file  tredit_crou_mimic_read_file_
#define  tredit_crou_sprintf_record   tredit_crou_sprintf_record_
#define  tredit_crou_get_choice       tredit_crou_get_choice_
#define  tredit_crou_kill_data        tredit_crou_kill_data_

#elif defined NEED_CAPITALS
#define  tredit_crou_read_file        TREDIT_CROU_READ_FILE
#define  tredit_crou_mimic_read_file  TREDIT_CROU_MIMIC_READ_FILE
#define  tredit_crou_sprintf_record   TREDIT_CROU_SPRINTF_RECORD
#define  tredit_crou_get_choice       TREDIT_CROU_GET_CHOICE
#define  tredit_crou_kill_data        TREDIT_CROU_KILL_DATA

#else
#define  TREDIT_CROU_READ_FILE        tredit_crou_read_file
#define  TREDIT_CROU_MIMIC_READ_FILE  tredit_crou_mimic_read_file
#define  TREDIT_CROU_SPRINTF_RECORD   tredit_crou_sprintf_record
#define  TREDIT_CROU_GET_CHOICE       tredit_crou_get_choice
#define  TREDIT_CROU_KILL_DATA        tredit_crou_kill_data

#endif

#ifdef __cplusplus
extern "C" {
#endif


/*-------------------------- start of functions ------------------------------*/
/*-------------------------- start of functions ------------------------------*/
/*-------------------------- start of functions ------------------------------*/

/* TredFile typedef is from old cprim.h */

typedef struct _TredFile TredFile;

TredFile *tredfile_create  (void);
int       tredfile_get     (const char *filename, TredFile *tf, char *info);
int       tredfile_put     (const char *filename, TredFile *tf, char *info);
int       tredfile_clear   (TredFile *tf);
TredFile *tredfile_destroy (TredFile *tf);

typedef struct _HiddenStruct
{
  TredFile *tf;
  char *dos  [TRED_TABLE_NMAX];
  char *codes[TRED_TABLE_NMAX];
} HiddenStruct;


static int tredit_crou_any_hdr_wrd_is_valid (const INTEGER num_header_words,
  const INTEGER hdr_wrd1, const INTEGER hdr_wrd2, INTEGER hdr_wrd3)
{
  return
         (hdr_wrd1 > 0 && hdr_wrd1 <= num_header_words) ||
         (hdr_wrd2 > 0 && hdr_wrd2 <= num_header_words) ||
         (hdr_wrd3 > 0 && hdr_wrd3 <= num_header_words);
}


static int tredit_crou_hrng (
  const DOUBLE *a_header, INTEGER num_header_words,
  const INTEGER hdr_wrd, const REAL strt_vlu, const REAL end_vlu)
{
  int iret;
  float ftmp;

/* because use with ANDing assumed and valid hdr_wrd not req'd */
  if (hdr_wrd < 1 || hdr_wrd > num_header_words)
    {
    iret=1;
    }
  else
    {
    ftmp = a_header[(int)hdr_wrd -1];
    iret = ftmp >= strt_vlu && ftmp <= end_vlu;
    }

  return iret;
}

/* given a header and a header word selection table, find out whether */
/* or not to delete, kill, reverse, and flag a trace by ANDing the */
/* three sets of header word value ranges (if no valid header word is used */
/* then ignore the set). */

/*------------------------------- read file ----------------------------------*/
/*------------------------------- read file ----------------------------------*/
/*------------------------------- read file ----------------------------------*/

int tredit_crou_asgn_code_to_hdr (
  const DOUBLE *a_header,
  const INTEGER *num_header_words,
  const INTEGER *table_size,
  const char **dos,
  const char **codes,
  const INTEGER *hdr_wrd_1, const REAL *strt_vlu_1, const REAL *end_vlu_1,
  const INTEGER *hdr_wrd_2, const REAL *strt_vlu_2, const REAL *end_vlu_2,
  const INTEGER *hdr_wrd_3, const REAL *strt_vlu_3, const REAL *end_vlu_3,
  INTEGER *del_tr, INTEGER *kill_tr, INTEGER *rev_tr, INTEGER *flag_tr) 
{
  int k2;

/* initialize return */
  *del_tr  = 0, *kill_tr = 0, *rev_tr  = 0, *flag_tr = 0;

  for (k2 = 0; k2 < *table_size; k2++)
    if (tredit_crou_any_hdr_wrd_is_valid (*num_header_words, hdr_wrd_1[k2],
      hdr_wrd_2[k2], hdr_wrd_3[k2]))
      {
/* printf("valid code is %c\n",codes[k2][0]); debug print, Cook, 6/2000 */

        if (codes[k2][0] == 'D')
          {
            if (dos[k2][0] == 'D' && !*del_tr)
              *del_tr =
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *del_tr)
              *del_tr = 
                !(tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
          }

        else if (codes[k2][0] == 'K')
          {
            if (dos[k2][0] == 'D' && !*kill_tr)
              *kill_tr =
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *kill_tr)
              *kill_tr = 
                !(tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
          }

        else if (codes[k2][0] == 'R')
          {
            if (dos[k2][0] == 'D' && !*rev_tr)
              *rev_tr =
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *rev_tr)
              *rev_tr = 
                !(tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
          }

        else if (codes[k2][0] == 'F')
          {
            if (dos[k2][0] == 'D' && !*flag_tr)
              *flag_tr =
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *flag_tr)
              *flag_tr = 
                !(tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                tredit_crou_hrng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
          }
      }
  return 0;   /* added by Tom Stoeckley 12/22/94 */
}

/*------------------------------- read file ----------------------------------*/
/*------------------------------- read file ----------------------------------*/
/*------------------------------- read file ----------------------------------*/

/* added by Steve Cook for better printout w/new CPS */
void tredit_crou_sprintf_record(HiddenStruct **hidden2, char *mesg, INTEGER *i)
{
  HiddenStruct *hidden = *hidden2;
  char s1[5],s2[7];

  if     (hidden->tf->_dos[*i][0] == 'D')
    strcpy(s1,"  DO");
  else if(hidden->tf->_dos[*i][0] == 'U')
    strcpy(s1,"UNDO");
  else
    strcpy(s1," ERR");

  if     (hidden->tf->_codes[*i][0] == 'D')
    strcpy(s2,"DELETE");
  else if(hidden->tf->_codes[*i][0] == 'F')
    strcpy(s2,"  FLAG");
  else if(hidden->tf->_codes[*i][0] == 'R')
    strcpy(s2,"    RP");
  else if(hidden->tf->_codes[*i][0] == 'K')
    strcpy(s2,"  KILL");
  else
    strcpy(s2,"   ERR");

  strcpy(mesg,"");

  if(hidden->tf->_hdr_wrd_2[*i] < 1)
    {
    sprintf(mesg,
      "%5i %s %s, %3i %10.3e %10.3e",

      *i + 1,

      s1, s2,

      hidden->tf-> _hdr_wrd_1[*i],
      hidden->tf->_strt_vlu_1[*i],
      hidden->tf-> _end_vlu_1[*i]);
    }
  else if(hidden->tf->_hdr_wrd_3[*i] < 1)
    {
    sprintf(mesg,
      "%5i %s %s, %3i %10.3e %10.3e, %3i %10.3e %10.3e",

      *i + 1,

      s1, s2,

      hidden->tf-> _hdr_wrd_1[*i],
      hidden->tf->_strt_vlu_1[*i],
      hidden->tf-> _end_vlu_1[*i],

      hidden->tf-> _hdr_wrd_2[*i],
      hidden->tf->_strt_vlu_2[*i],
      hidden->tf-> _end_vlu_2[*i]);
    }
  else
    {
    sprintf(mesg,
      "%5i %s %s, %3i %10.3e %10.3e, %3i %10.3e %10.3e, %3i %10.3e %10.3e",

      *i + 1,

      s1, s2,

      hidden->tf-> _hdr_wrd_1[*i],
      hidden->tf->_strt_vlu_1[*i],
      hidden->tf-> _end_vlu_1[*i],

      hidden->tf-> _hdr_wrd_2[*i],
      hidden->tf->_strt_vlu_2[*i],
      hidden->tf-> _end_vlu_2[*i],

      hidden->tf-> _hdr_wrd_3[*i],
      hidden->tf->_strt_vlu_3[*i],
      hidden->tf-> _end_vlu_3[*i]);
    }
}

void tredit_crou_read_file
  (HiddenStruct **hidden2, char *info,
   const char *filename,
   INTEGER *nrecords,
   INTEGER *flagging_requested)
{
  int i,e;

  HiddenStruct *hidden = (HiddenStruct*)malloc(sizeof(HiddenStruct));
  *hidden2=hidden;

  hidden->tf = tredfile_create();
  if(hidden->tf)
      {
      e = tredfile_get(filename, hidden->tf, info);
      if(e)
          {
          hidden->tf = tredfile_destroy(hidden->tf);
          free(hidden);
          return;
          }
      }
  else
      {
      free(hidden);
      return;
      }

  *flagging_requested=0;

  *nrecords=hidden->tf->_nrecs;

  for(i = 0; i < *nrecords; i++)
      {
      hidden->dos  [i] = hidden->tf->_dos  [i];
      hidden->codes[i] = hidden->tf->_codes[i];
     
      if((hidden->codes[i][0] == 'f') || (hidden->codes[i][0] == 'F'))
        {
        *flagging_requested=1;
        }
      }

}

/*------------------------- mimic read file ----------------------------------*/
/*------------------------- mimic read file ----------------------------------*/
/*------------------------- mimic read file ----------------------------------*/
/*
 Function tredit_crou_mimic_read_file()
   creates HiddenStruct ** from caller-supplied arrays instead of reading in
   the information from a '.tred' format disk file.

 The input arrays do not supply DO/UNDO flags, however.  They can only DO.

 This allows the rest of the logic to be shared without further modification.

 Added by Steve Cook, 6/2000.
*/

void tredit_crou_mimic_read_file
  (HiddenStruct **hidden2, char *info,
   INTEGER *nrecords, const char *input_codes,
   INTEGER *hwrd1, REAL *hstrt1, REAL *hend1,
   INTEGER *hwrd2, REAL *hstrt2, REAL *hend2,
   INTEGER *hwrd3, REAL *hstrt3, REAL *hend3,
   INTEGER *flagging_requested)
{
  int i;

  HiddenStruct *hidden = (HiddenStruct*)malloc(sizeof(HiddenStruct));
  *hidden2=hidden;

  hidden->tf = tredfile_create();

  if(hidden->tf)
      {
      for (i=0; i < *nrecords; i++)
        {
        strcpy(hidden->tf->_dos[i], "D"); /* force all to DO mode */

        strcpy  (hidden->tf->_codes[i] ,""); /* added insurance */
        sprintf (hidden->tf->_codes[i] ,"%c%c", input_codes[i], '\0');
        }
      }
  else
      {
      free(hidden);
      return;
      }

  hidden->tf->_nrecs = *nrecords;

  *flagging_requested=0;

  for(i = 0; i < *nrecords; i++)
      {
      hidden->dos  [i] = hidden->tf->_dos  [i];
      hidden->codes[i] = hidden->tf->_codes[i];

      if((hidden->codes[i][0] == 'f') || (hidden->codes[i][0] == 'F'))
        {
        *flagging_requested=1;
        }

      hidden->tf-> _hdr_wrd_1[i] =  hwrd1[i];
      hidden->tf->_strt_vlu_1[i] = hstrt1[i];
      hidden->tf-> _end_vlu_1[i] =  hend1[i];

      hidden->tf-> _hdr_wrd_2[i] =  hwrd2[i];
      hidden->tf->_strt_vlu_2[i] = hstrt2[i];
      hidden->tf-> _end_vlu_2[i] =  hend2[i];

      hidden->tf-> _hdr_wrd_3[i] =  hwrd3[i];
      hidden->tf->_strt_vlu_3[i] = hstrt3[i];
      hidden->tf-> _end_vlu_3[i] =  hend3[i];
      }
}

/*------------------------------- kill data ----------------------------------*/
/*------------------------------- kill data ----------------------------------*/
/*------------------------------- kill data ----------------------------------*/

void tredit_crou_kill_data(HiddenStruct **hidden2)
{
  HiddenStruct *hidden=*hidden2;

  if(hidden->tf) hidden->tf = tredfile_destroy(hidden->tf);
  free(hidden);
}

/*------------------------------- get choice ---------------------------------*/
/*------------------------------- get choice ---------------------------------*/
/*------------------------------- get choice ---------------------------------*/

/*
modified from old CBYT-compatible function to accomodate FORTRAN
  calling and increase portability (Steve Cook, 6/2000)

old signature was:

void tredit_get_choice (
   long *tredfile_pointer, const float *hd, long *nwih,
   long *del_trace, long *kill_trace,
   long *rev_trace, long *flag_trace)

*/
void tredit_crou_get_choice
  (HiddenStruct **hidden2, const DOUBLE *hd, const INTEGER *nwih,
   INTEGER *del_trace, INTEGER *kill_trace,
   INTEGER *rev_trace, INTEGER *flag_trace)
{
  HiddenStruct *hidden=*hidden2;

  tredit_crou_asgn_code_to_hdr(
    hd,
    nwih,
    &hidden->tf->_nrecs,
    (const char **)(hidden->dos),
    (const char **)(hidden->codes),
    hidden->tf->_hdr_wrd_1, hidden->tf->_strt_vlu_1, hidden->tf->_end_vlu_1,
    hidden->tf->_hdr_wrd_2, hidden->tf->_strt_vlu_2, hidden->tf->_end_vlu_2,
    hidden->tf->_hdr_wrd_3, hidden->tf->_strt_vlu_3, hidden->tf->_end_vlu_3,
    del_trace, kill_trace, rev_trace, flag_trace);

/*
  printf("%i %f %f   %i %i %i %i\n",
    hidden->tf->_hdr_wrd_1[0],
    hidden->tf->_strt_vlu_1[0],
    hidden->tf->_end_vlu_1[0],
    *del_trace, *kill_trace, *rev_trace, *flag_trace);
*/

  if (*del_trace)
    { *kill_trace = 0, *rev_trace = 0, *flag_trace = 0; return; }
  if (*kill_trace)
    { *rev_trace = 0; return; }
}

#ifdef __cplusplus
}
#endif

/*----------------------------------- end ------------------------------------*/
/*----------------------------------- end ------------------------------------*/
/*----------------------------------- end ------------------------------------*/
