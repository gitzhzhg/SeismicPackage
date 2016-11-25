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
#ifndef _CPScust_
#define _CPScust_

#include <stdio.h>

typedef struct CPS_CUST
         { char *file[24];
           int   diagnos;
           int   mail;
         } CpsCust;
 
/***********************************************
 * Prototype of CpsCust Methods.             **/
long cust_getdiag(CpsCust *cust);
void cust_setdiag(CpsCust *cust, int diag);
long cust_getmail(CpsCust *cust);
void cust_setmail(CpsCust *cust, int mail);
char *cust_getfile(CpsCust *cust);
long cust_filecnt(CpsCust *cust);
void cust_freeall(CpsCust *cust);
void cust_freeend(CpsCust *cust,int start);
void cust_freenth(CpsCust *cust,int nth);
char *cust_getnth_file(CpsCust *cust, int nth);
void cust_setnth_file(CpsCust *cust, char *file,int nth);

#endif
