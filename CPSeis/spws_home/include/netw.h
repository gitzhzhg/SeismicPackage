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
#ifndef _NETW_H
#define _NETW_H

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define netw_getfil_ NETW_GETFIL
#define netw_putfil_ NETW_PUTFIL
#define netw_netinfo_ NETW_NETINFO
#endif
#if (VMS || _AIX || __hpux)
#define netw_getfil_ netw_getfil
#define netw_putfil_ netw_putfil
#define netw_netinfo_ netw_netinfo
#endif

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

/*          FUNCTION PROTOTYPES          */

int  netw_getfil_(char *lfile, char *rnode, char *ruser, char *rfile,
     int *wdtyp, char *msg);
int  netw_putfil_(char *lfile, char *rnode, char *ruser, char *rfile,
     int *wdtyp, char *msg);
int  netw_lnode(char *hostname, char *os);
int  netw_nodes_wrd(char *rnode);
int  netw_netinfo_(char *node, char *user, char *path);
int  netw_netname(char *c,char *n,char *u,char *f,int *bld);
int  netw_netname_cps(char *c,char *n,char *u,char *f,int *bld);
void netw_file_cppath(char *model, char *file);




#ifdef __cplusplus  
}                   // for C++
#endif


#endif
