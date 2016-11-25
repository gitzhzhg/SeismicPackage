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
#ifndef  _rmodc_
#define  _rmodc_

#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define rmod_lochost_   RMOD_LOCHOST
#define rmod_rmfil_     RMOD_RMFIL
#define rmod_shell_w_   RMOD_SHELL_W
#define rmod_cray_data_ RMOD_CRAY_DATA
#define rmod_name_bld_  RMOD_NAME_BLD
#define rmodopc_        RMODOPC
#define rmodclc_        RMODCLC
#define rmodrdc_        RMODRDC
#define rmodwrc_        RMODWRC
#define rmod_canrm_     RMOD_CANRM
#define rmodglf_        RMODGLF
#endif
#if (VMS || _AIX || __hpux)
#define rmod_lochost_   rmod_lochost
#define rmod_rmfil_     rmod_rmfil
#define rmod_shell_w_   rmod_shell_w
#define rmod_cray_data_ rmod_cray_data
#define rmod_name_bld_  rmod_name_bld
#define rmodopc_        rmodopc
#define rmodclc_        rmodclc
#define rmodrdc_        rmodrdc
#define rmodwrc_        rmodwrc
#define rmod_canrm_     rmod_canrm
#define rmodglf_        rmodglf
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
  
int  rmod_lochost_(char *lname, char *los);
void rmod_rmfil_(char *file);
int  rmod_shell_w_(char *cmd);
int  rmod_cray_data_(char *n, char *u, char *p);
int rmod_name_bld_(char *namein, char *nameout);

int  rmodopc_ (int  *fortranUnit, char *localFile, char *remoteFile,
        char *accessType, char *status, char *formatType,
        int  *recordLength, int  *word_type);
int  rmodclc_ (int  *indexNumber);
void rmodrdc_(int  *key,int  *irec, int  *nrec, int  *n, char *data,
     int  *wdtyp,int  *stat, char *msg);
void rmodwrc_(int  *key,int  *irec, int  *nrec, int  *n, char *data,
     int  *wdtypo,int  *wdtypi,int  *stat, char *msg);
int  rmod_canrm_(int  *key);

#ifdef __cplusplus
}                   // for C++
#endif
 
#endif




