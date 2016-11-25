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
#include "c2f_interface.h"

#include <string.h>
#include <stdarg.h>

#ifdef NEED_CAPITALS
#define zro2blk_  ZRO2BLK
#define ljus_     LJUS
#define rjus_     RJUS
#define fnum2hol_ FNUM2HOL
#define count_    COUNT
#define mvbytes_  MVBYTES
#endif
#if(VMS || _AIX || __hpux)
#define zro2blk_  zro2blk
#define ljus_     ljus
#define rjus_     rjus
#define fnum2hol_ fnum2hol
#define count_    count
#define mvbytes_  mvbytes
#endif


/*
 * replace nulls with blanks
 */
void zro2blk_(char *arr, int *maxc) {
 int i;
 if(!arr || *maxc<0) return;
 for(i=0;i<*maxc;i++) { if(arr[i]=='\0') arr[i]=' '; }
}

/* 
 * For 8 bytes
 * 1) replace nulls with blanks
 * 2) squeeze out leading white space to left justify
 */
void ljus_(char *b) {
 int i,k=0;
 if(!b) return;
 for(i=0;i<8;i++) { if(b[i]=='\0') b[i]=' '; }
 while(b[k]<=32 && k<8) k++;
 if(k>0 && k< 8) {
  for(i=k;i<8;i++) { 
   b[i-k]=b[i];
   b[i]=' ';
  }
 }
}

/* 
 * For 8 bytes
 * 1) replace nulls with blanks
 * 2) squeeze out trailing white space to right justify
 */
void rjus_(char *b) {
 int i,k=7;
 if(!b) return;
 for(i=0;i<8;i++) { if(b[i]=='\0') b[i]=' '; }
 while(b[k]<=32 && k>0) k--;
 if(k<7 && k>0) {
  for(i=k;i>-1;i--) { 
   b[7-(k-i)]=b[i];
   b[i]=' ';
  }
 }
}

/*
 * Convert a float to a character string. The null terminator
 * is converted to a blank
void fnum2hol_(float *fnum,char *b,int *idec, int *jusflg,int *ipldec) {
 char fmt[8];
 int  i,dec=*idec,maxc=8;
 if(dec > 7) {
  printf(" FNUM2HOL - IDEC MUST BE 7 OR LESS\n");
  dec=7;
 }
 if(dec < 0) dec=0;
  
 if(*jusflg==1) {
  sprintf(fmt,"%%-8.%df",dec);
 } else {
  sprintf(fmt,"%%8.%df",dec);
 }

 for(i=0;i<8;i++) { b[i]='\0'; }
 sprintf(b,fmt,*fnum);
 for(i=0;i<8;i++) { if(b[i]=='\0') b[i]=' '; }
}
 */

void count_(char *b, int *mc, int *nc) {
 int i;
 *nc=0;
 for(i=0;i<*mc;i++) {
  if(b[i] !=32) { *nc=i+1; }
 }
}

void mvbytes_(char *in, int *o1, char *out, int *o2, int *n) {
 int i,m1,m2;
 m1 = *o1-1;
 m2 = *o2-1;
 if(!in || !out) return;
 for(i=0;i<*n;i++) {
  out[i+m2] = in[i+m1];
 }
}


