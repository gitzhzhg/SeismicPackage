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

/***** name of this file was changed
             from tred_file.h
             to tredfile_object.h
      by Tom Stoeckley on 12/27/94
      to comply with CPS naming conventions ******************/
 
#ifndef TREDFILE_OBJECT_H
#define TREDFILE_OBJECT_H

#define TRED_VECT_COUNT 8
#define TRED_CODES_NVAR 4
#define TRED_CODES_SIZE 4
#define TRED_TABLE_NMAX 4000

struct _TredFile
{
/* total number of data records (i.e. excludes header) in file */
  long _nrecs;
/* do/undo command determining disposition of trace */
  char   _dos[TRED_TABLE_NMAX][TRED_CODES_SIZE];
/* code identifying disposition of trace */
  char   _codes[TRED_TABLE_NMAX][TRED_CODES_SIZE];
/* header word number for first column */
  long   _hdr_wrd_1[TRED_TABLE_NMAX];
/* starting value of first header word */
  float  _strt_vlu_1[TRED_TABLE_NMAX];
/* ending value of first header word */
  float  _end_vlu_1[TRED_TABLE_NMAX];
/* header word number for second column */
  long   _hdr_wrd_2[TRED_TABLE_NMAX];
/* starting value of second header word */
  float  _strt_vlu_2[TRED_TABLE_NMAX];
/* ending value of second header word */
  float  _end_vlu_2[TRED_TABLE_NMAX];
/* header word number for third column */
  long   _hdr_wrd_3[TRED_TABLE_NMAX];
/* starting value of third header word */
  float  _strt_vlu_3[TRED_TABLE_NMAX];
/* ending value of third header word */
  float  _end_vlu_3[TRED_TABLE_NMAX];
};

#endif
