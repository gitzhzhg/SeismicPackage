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
#ifndef SLP_FILEPAIR_HH
#define SLP_FILEPAIR_HH

#include <Xm/Xm.h>
#include "wproc.h"


extern "C" {                 // necessary to avoid C++ name mangling

  typedef void MakeSLpFilepairTrap
    (void *data,
     long *valid1,
     long *valid2,
     char *info1,
     char *info2,
     long *same_datasets);

  typedef void MakeSLpFilepairUpfun
    (void *updata);

  Widget make_slp_filepair
    (Widget parent,
     char *name,
     char *filetype,
     char *ext,
     struct HELPCTX *hctx,
     MakeSLpFilepairTrap *trap,
     void *data,
     char *filename1,
     char *filename2,
     long required1,
     long required2);

  Widget make_nonmatching_slp_filepair
    (Widget parent,
     char *name,
     char *filetype1,
     char *filetype2,
     char *ext1,
     char *ext2,
     char *suffix2,
     struct HELPCTX *hctx,
     MakeSLpFilepairTrap *trap,
     void *data,
     char *filename1,
     char *filename2,
     long required1,
     long required2);

  Widget make_alternate_slp_filepair
    (Widget parent,
     char *name,
     char *filetype,
     char *ext,
     struct HELPCTX *hctx,
     MakeSLpFilepairTrap *trap,
     void *data,
     char *filename1,
     char *filename2,
     long required1,
     long required2);

  void slp_filepair_register_update_function
    (Widget wpair,
     MakeSLpFilepairUpfun *upfun,
     void *updata);

  long update_slp_filepair
    (Widget wpair,
     char *msg4);

  long get_slp_filepair_status
    (Widget wpair,
     int which);

  const char *get_slp_filepair_message
    (Widget wpair,
     int which);

  void change_slp_filepair_message
    (Widget wpair,
     int which,
     char *msg);

}  
#endif
