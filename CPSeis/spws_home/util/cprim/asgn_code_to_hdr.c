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
/*                        asgn_code_to_hdr_spws.c

************************* COPYRIGHT NOTICE ****************************
*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
************************* COPYRIGHT NOTICE ****************************
C\USER DOC
-----------------------------------------------------------------------
                    SEISMIC PROCESSING WORKSTATION
                          C-LANGUAGE UTILITY
                  designed to be called from C or C++

  Utility name:  asgn_code_to_hdr_spws    (assign code to trace header)

  Subdirectory:  cprim               (and [primitives.math] in CPS)
  Library:       cprim.a             (and CONLIB in CPS)
  Header file:   cprim.h
  Source file:   asgn_code_to_hdr_spws.c

  Written:       94/12/20  by:  Kruger Corn
  Last revised:  94/12/28  by:  Tom Stoeckley

  Purpose:       To return codes specifying whether the seismic trace
                 should be deleted, killed, polarity-reversed, and/or
                 flagged.  The tables in the argument list come
                 from a file maintained by tredfile_object.  This
                 utility is used by CBYT and by the CPS process TREDIT.

  Related Documentation:   tredfile_object.h and tredfile_object.c
-----------------------------------------------------------------------
                       GENERAL INFORMATION

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

                         asgn_code_to_hdr_spws

-----------------------------------------------------------------------
                   DOCUMENTATION OF EACH FUNCTION

  See the header files and the code in this implementation file for
  details.
-----------------------------------------------------------------------
                        REVISION HISTORY

     Date     Author       Description
     ----     ------       -----------
  3. 94/12/28 Stoeckley    Initial documentation provided to comply
                            with CPS requirements.
  2. 94/12/27 Stoeckley    Removed the routine asgn_codes_hierarchically,
                            and put this functionality into the CPS
                            process TREDIT.
  1. 94/12/20 Kruger Corn  Initial version.
-----------------------------------------------------------------------
C\END DOC
*/



#include "cprim.h"

static int any_hdr_wrd_is_valid (const long num_header_words,
  const long hdr_wrd1, const long hdr_wrd2, const long hdr_wrd3)
{
  return (hdr_wrd1 > 0 && hdr_wrd1 <= num_header_words) ||
         (hdr_wrd2 > 0 && hdr_wrd2 <= num_header_words) ||
         (hdr_wrd3 > 0 && hdr_wrd3 <= num_header_words);
}

static int hdr_val_in_rng (const float *a_header, long num_header_words,
  long hdr_wrd, float strt_vlu, float end_vlu)
{
  float *ptr;

/* because use with ANDing assumed and valid hdr_wrd not req'd */
  if (hdr_wrd < 1 || hdr_wrd > num_header_words) return 1;
  ptr = (float *) a_header + (int)hdr_wrd - 1;
  return *ptr >= strt_vlu && *ptr <= end_vlu;
}

/* given four flags in combinations of true or false to delete, kill, */
/* reverse, and flag a trace return the flags for a valid result. */

/***************
void asgn_flags_hierarchically
           (long *del_tr, long *kill_tr, long *rev_tr, long *flag_tr)
{
  if (*del_tr)
    { *kill_tr = 0, *rev_tr = 0, *flag_tr = 0; return; }
  if (*kill_tr)
    { *rev_tr = 0; return; }
}
***************/

/* given a header and a header word selection table, find out whether */
/* or not to delete, kill, reverse, and flag a trace by ANDing the */
/* three sets of header word value ranges (if no valid header word is used */
/* then ignore the set). */

int asgn_code_to_hdr_spws (const float *a_header, long *num_header_words,
  long *table_size, char **dos,
  char **codes, long *hdr_wrd_1, float *strt_vlu_1, float *end_vlu_1,
  long *hdr_wrd_2, float *strt_vlu_2, float *end_vlu_2, long *hdr_wrd_3,
  float *strt_vlu_3, float *end_vlu_3, long *del_tr, long *kill_tr,
  long *rev_tr, long *flag_tr) 
{
  int k2;

/* initialize return */
  *del_tr  = 0, *kill_tr = 0, *rev_tr  = 0, *flag_tr = 0;

/* loop through table */
  for (k2 = 0; k2 < *table_size; k2++)
    if (any_hdr_wrd_is_valid (*num_header_words, hdr_wrd_1[k2],
      hdr_wrd_2[k2], hdr_wrd_3[k2]))
      {
        if (codes[k2][0] == 'D')
	  {
            if (dos[k2][0] == 'D' && !*del_tr)
              *del_tr =
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *del_tr)
              *del_tr = 
                !(hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
	  }

        if (codes[k2][0] == 'K')
	  {
            if (dos[k2][0] == 'D' && !*kill_tr)
              *kill_tr =
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *kill_tr)
              *kill_tr = 
                !(hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
	  }

        if (codes[k2][0] == 'R')
	  {
            if (dos[k2][0] == 'D' && !*rev_tr)
              *rev_tr =
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *rev_tr)
              *rev_tr = 
                !(hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
	  }

        if (codes[k2][0] == 'F')
	  {
            if (dos[k2][0] == 'D' && !*flag_tr)
              *flag_tr =
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]);

            else if (dos[k2][0] == 'U' && *flag_tr)
              *flag_tr = 
                !(hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_1[k2],
                  strt_vlu_1[k2], end_vlu_1[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_2[k2],
                  strt_vlu_2[k2], end_vlu_2[k2]) &&
                hdr_val_in_rng (a_header, *num_header_words, hdr_wrd_3[k2],
                  strt_vlu_3[k2], end_vlu_3[k2]));
	  }
      }
  return 0;   /* added by Tom Stoeckley 12/22/94 */
}
