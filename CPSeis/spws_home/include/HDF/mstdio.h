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

/* $Id: mstdio.h,v 1.12 1997/10/24 21:01:07 koziol Exp $ */

/*-----------------------------------------------------------------------------
 * File:    mstdio.h
 * Purpose: Header file for stdio-like modeling information.
 * Dependencies: should be included after hdf.h
 * Invokes:
 * Contents: Structures & definitions for stdio modeling.  This header
 *              should only be included in hcomp.c and mstdio.c.
 * Structure definitions:
 * Constant definitions:
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef __MSTDIO_H
#define __MSTDIO_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/*
   ** from mstdio.c
 */

    extern int32 HCPmstdio_stread
                (accrec_t * rec);

    extern int32 HCPmstdio_stwrite
                (accrec_t * rec);

    extern int32 HCPmstdio_seek
                (accrec_t * access_rec, int32 offset, int origin);

    extern int32 HCPmstdio_inquire
                (accrec_t * access_rec, int32 *pfile_id, uint16 *ptag, uint16 *pref,
               int32 *plength, int32 *poffset, int32 *pposn, int16 *paccess,
                 int16 *pspecial);

    extern int32 HCPmstdio_read
                (accrec_t * access_rec, int32 length, void * data);

    extern int32 HCPmstdio_write
                (accrec_t * access_rec, int32 length, const void * data);

    extern intn HCPmstdio_endaccess
                (accrec_t * access_rec);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

/* model information about stdio model */
typedef struct
{
    int32      pos;            /* postion ? */
}
comp_model_stdio_info_t;

#ifndef MSTDIO_MASTER
extern funclist_t mstdio_funcs; /* functions to perform run-length encoding */
#else
funclist_t  mstdio_funcs =
{                               /* functions to perform run-length encoding */
    HCPmstdio_stread,
    HCPmstdio_stwrite,
    HCPmstdio_seek,
    HCPmstdio_inquire,
    HCPmstdio_read,
    HCPmstdio_write,
    HCPmstdio_endaccess
};
#endif

#endif /* __MSTDIO_H */
