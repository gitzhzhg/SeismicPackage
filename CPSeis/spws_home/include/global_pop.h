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
/*-------------------------------------------------------------------------
 *USER DOC
 * File        : global_pop.h
 * Author      : Michael L. Sherrill
 * Date        : 07/92
 *
 * Purpose     Header file for global information widget.
 *
 *NOTES:
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *
 *END DOC
 *
-------------------------------------------------------------------------*/

#include "wproc.h"


#define GLOBALB       0
#define GLOBALIN      1
#define GLOBALFORM    2 
#define GLOBALVIEW    3
#define DESTROYG      4

#define NUM_WIG       5 


struct global_info
       {
        wunion              globalwig[NUM_WIG];  /*widget union*/
        struct CB           globalcb[NUM_WIG];   /*widget array*/
        long                good_infile;         /*boolean*/
        char                tmpfile[100];        /*temporary file holder*/
        struct FILE_INFO    infile_info;         /*file input info*/
        struct CURRFLD_INFO fld_info;            /*file input call back info*/
        Widget              viewtxt;            
        Widget              self;                
       };
