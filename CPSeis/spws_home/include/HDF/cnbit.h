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

 /* $Id: cnbit.h,v 1.16 1997/10/24 21:00:20 koziol Exp $ */

 /*-----------------------------------------------------------------------------
 * File:    cnbit.h
 * Purpose: Header file for N-bit encoding information.
 * Dependencies: should only be included from hcompi.h
 * Invokes: none
 * Contents: Structures & definitions for N-bit encoding.
 * Structure definitions:
 * Constant definitions:
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef __CNBIT_H
#define __CNBIT_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/*
   ** from cnbit.c
 */

    extern int32 HCPcnbit_stread
                (accrec_t * rec);

    extern int32 HCPcnbit_stwrite
                (accrec_t * rec);

    extern int32 HCPcnbit_seek
                (accrec_t * access_rec, int32 offset, int origin);

    extern int32 HCPcnbit_inquire
                (accrec_t * access_rec, int32 *pfile_id, uint16 *ptag, uint16 *pref,
               int32 *plength, int32 *poffset, int32 *pposn, int16 *paccess,
                 int16 *pspecial);

    extern int32 HCPcnbit_read
                (accrec_t * access_rec, int32 length, void * data);

    extern int32 HCPcnbit_write
                (accrec_t * access_rec, int32 length, const void * data);

    extern intn HCPcnbit_endaccess
                (accrec_t * access_rec);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

/* size of the N-bit buffer */
#define NBIT_BUF_SIZE   (MAX_NT_SIZE*64)
/* size of the N-bit mask buffer (same as buffer size for now) */
#define NBIT_MASK_SIZE  (MAX_NT_SIZE)

typedef struct
{                               /* structure to hold bit vector info */
    intn        offset,         /* offset of the bit information */
                length;         /* number of bits in the information */
    uint8       mask;           /* mask for this bit vector */
}
nbit_mask_info_t;

/* N-bit [en|de]coding information */
typedef struct
  {
      int32       nt;           /* number type of data we are encoding */
      intn        nt_size;      /* size of the number-type in the file */
      intn        fill_one;     /* whether to fill with 1's or not (0's) */
      intn        sign_ext;     /* whether to sign extend or not */
      uint8       buffer[NBIT_BUF_SIZE];    /* buffer for expanding n-bit data in */
      intn        buf_pos;      /* current offset in the expansion buffer */
      intn        mask_off,     /* offset of the bit to start masking with */
                  mask_len;     /* number of bits to mask */
      int32       offset;       /* offset in the file in terms of bytes */
      uint8       mask_buf[NBIT_MASK_SIZE];     /* buffer to hold the bitmask */
      nbit_mask_info_t mask_info[NBIT_MASK_SIZE];   /* information about the mask */
      intn        nt_pos;       /* current byte to read or write */
  }
comp_coder_nbit_info_t;

#ifndef CNBIT_MASTER
extern funclist_t cnbit_funcs;  /* functions to perform N-bit encoding */
#else
funclist_t  cnbit_funcs =
{                               /* functions to perform N-bit encoding */
    HCPcnbit_stread,
    HCPcnbit_stwrite,
    HCPcnbit_seek,
    HCPcnbit_inquire,
    HCPcnbit_read,
    HCPcnbit_write,
    HCPcnbit_endaccess
};
#endif

#endif /* __CNBIT_H */
