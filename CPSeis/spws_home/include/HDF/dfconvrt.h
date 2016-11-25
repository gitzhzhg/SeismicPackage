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

/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id: dfconvrt.h,v 1.8 1994/10/19 20:55:12 koziol Exp $ */

/* In order to speed the conversion process, eliminate a layer of function
 * calls by making DFconvert into a macro.
 * Peter Webb, Oct 11, 1989
 */
#ifndef DFCONVRT_H
#define DFCONVRT_H

#ifndef FUNC_CONV
/* This is the default */
/* using the DFconvert macro instead of function */
#define DFconvert(src,dest,ntype,stype,dtype,size,status)\
{ char *s=(src), *d=(dest);\
  int nt=(ntype), st=(stype), dt=(dtype);\
  int sz=((int)size);\
  if (nt==DFNT_FLOAT) {\
    if ((st==DFNTF_IEEE && dt==DFNTF_PC) ||\
        (st==DFNTF_PC && dt==DFNTF_IEEE)) {\
      int32 i;\
      for (i=0;i<sz*4;i+=4) {\
        d[i] = s[i+3];\
        d[i+1] = s[i+2];\
        d[i+2] = s[i+1];\
        d[i+3] = s[i];\
      }\
      status=0;\
    } else {\
      if (st==DFNTF_PC) {\
        int i;\
        char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = s[i];\
          s[i] = s[i+3];\
          s[i+3] = t;\
          t = s[i+1];\
          s[i+1] = s[i+2];\
          s[i+2] = t;\
        }\
        st=DFNTF_IEEE;\
      }\
      if (st==DFNTF_IEEE && dt==DFNTF_CRAY) {\
    int i=1;\
        SCUP32(s,d,&sz,&i);\
      } else if (st==DFNTF_CRAY && (dt==DFNTF_IEEE || dt==DFNTF_PC)) {\
    int i=1;\
        CSPK32(s,d,&sz,&i);\
      } else if (st==DFNTF_IEEE && dt==DFNTF_VAX) {\
        status = DFCVieeeF2vaxF((union float_uint_uchar *)s,(union float_uint_uchar *)d,sz);\
      } else if (st==DFNTF_VAX && (dt==DFNTF_IEEE || dt==DFNTF_PC)) {\
        status = DFCVvaxF2ieeeF((union float_uint_uchar *)s,(union float_uint_uchar *)d,sz);\
      } else {\
        status = -1;\
      }\
      if (dt==DFNTF_PC) {\
        int i;\
        char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = d[i];\
          d[i] = d[i+3];\
          d[i+3] = t;\
          t = d[i+1];\
          d[i+1] = d[i+2];\
          d[i+2] = t;\
        }\
      }\
      if ((stype)==DFNTF_PC) {\
        int i;\
        char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = s[i];\
          s[i] = s[i+3];\
          s[i+3] = t;\
          t = s[i+1];\
          s[i+1] = s[i+2];\
          s[i+2] = t;\
        }\
        st=DFNTF_IEEE;\
      }\
    }\
  } else {\
  status = -1;\
  }\
  if (status == -1) HERROR(DFE_BADCONV);\
}
#endif /* !FUNC_CONV */

#endif /* !DFCONVRT_H */
