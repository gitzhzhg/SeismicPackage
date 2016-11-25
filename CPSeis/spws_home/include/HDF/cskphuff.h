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

 /* $Id: cskphuff.h,v 1.6 1997/10/24 21:00:27 koziol Exp $ */

 /*-----------------------------------------------------------------------------
 * File:    cskphuff.h
 * Purpose: Header file for skipping huffman encoding information.
 * Dependencies: should only be included from hcompi.h
 * Invokes: none
 * Contents: Structures & definitions for skipping huffman encoding.
 * Structure definitions:
 * Constant definitions:
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef __CSKPHUFF_H
#define __CSKPHUFF_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/*
   ** from cskphuff.c
 */

    extern int32 HCPcskphuff_stread
                (accrec_t * rec);

    extern int32 HCPcskphuff_stwrite
                (accrec_t * rec);

    extern int32 HCPcskphuff_seek
                (accrec_t * access_rec, int32 offset, int origin);

    extern int32 HCPcskphuff_inquire
                (accrec_t * access_rec, int32 *pfile_id, uint16 *ptag, uint16 *pref,
               int32 *plength, int32 *poffset, int32 *pposn, int16 *paccess,
                 int16 *pspecial);

    extern int32 HCPcskphuff_read
                (accrec_t * access_rec, int32 length, void * data);

    extern int32 HCPcskphuff_write
                (accrec_t * access_rec, int32 length, const void * data);

    extern intn HCPcskphuff_endaccess
                (accrec_t * access_rec);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

/* The maximum source character code: */
#define SKPHUFF_MAX_CHAR     255

/* One greater than the maximum source character code: */
#define SUCCMAX     (SKPHUFF_MAX_CHAR+1)

/* Twice the maximum source character code: */
#define TWICEMAX    (2*SUCCMAX+1)

/* The root node in the tree */
#define ROOT        0

/* Skipping huffman [en|de]coding information */
typedef struct
{
    intn        skip_size;      /* number of bytes in each element */
    uintn     **left,           /* define the left and right pointer arrays */
              **right;
    uint8     **up;             /* define the up pointer array */
    intn        skip_pos;       /* current byte to read or write */
    int32       offset;         /* offset in the de-compressed array */
}
comp_coder_skphuff_info_t;

#ifndef CSKPHUFF_MASTER
extern funclist_t cskphuff_funcs;   /* functions to perform skipping huffman encoding */
#else
funclist_t  cskphuff_funcs =
{                               /* functions to perform skipping huffman encoding */
    HCPcskphuff_stread,
    HCPcskphuff_stwrite,
    HCPcskphuff_seek,
    HCPcskphuff_inquire,
    HCPcskphuff_read,
    HCPcskphuff_write,
    HCPcskphuff_endaccess
};
#endif

#endif /* __CSKPHUFF_H */
