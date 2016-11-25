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
#ifndef _RCPXFR_H
#define _RCPXFR_H

#include "c2f_interface.h"

/*
#ifdef CRAY
*/
#ifdef NEED_CAPITALS
#define rcpxfr_         RCPXFR
#define rcpxfr_net_     RCPXFR_NET
#define rcpxfr_net_cps_ RCPXFR_NET_CPS
#define rcpxfr_nb_      RCPXFR_NB
#endif


/*
#if (VMS || _AIX || __hpux)
*/
#ifdef REMOVE_UNDERSCORE
#define rcpxfr_         rcpxfr
#define rcpxfr_net_     rcpxfr_net
#define rcpxfr_net_cps_ rcpxfr_net_cps
#define rcpxfr_nb_      rcpxfr_nb
#endif

#ifdef __sgi
#define vfork fork
#endif

#ifdef __cplusplus  
extern "C" {                 // for C++ 
#endif

typedef struct _NetIO_request {
    char scrip[L_tmpnam];
    char errfil[L_tmpnam];
    char rnode[32];
    char rfile[72];
    char ruser[16];
    char lfile[120];
    char mode[8];
    int  dir;
    int  status;
    int  nap_time;
} NetIO_request;

/*          FUNCTION PROTOTYPES          */

int  rcpxfr_net_cps_(char *lfile,char *netname, int *local, int *dir);
int  rcpxfr_net_(char lfile[],char *netname, int *local, int *dir);
void rcpxfr_(char *lfile,char *rfile, char *rnode, char *ruser,
            int *dir, int *istat);
void rcpxfr_nb_(char *lfile,char *rfile, char *rnode, char *ruser, 
            int *dir, int *istat);
NetIO_request *rcpxfr_con(char *lfile,char *rfile, char *rnode, char *ruser, 
                int dir);
int  rcpxfr_check_req(NetIO_request *NIO);
void rcpxfr_del_req(NetIO_request *NIO);

#ifdef __cplusplus  
}                   // for C++
#endif


#endif
