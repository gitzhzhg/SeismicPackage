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

/* $Id: dfan.h,v 1.9 1994/06/29 13:03:51 koziol Exp $ */

/*------------------------------------------------------------------------------
 * File:    dfan.h
 * Purpose: header file for the Annotations set
 * Invokes: df.h
 * Contents:
 *  Structure definitions: DFANdirentry, DFANdirhead
 *  Constant definitions: DFAN_LABEL, DFAN_DESC
 * Remarks: none
 *----------------------------------------------------------------------------*/

#ifndef DFAN_H  /* avoid re-inclusion */
#define DFAN_H

#include "hdf.h"

#define DFAN_LABEL  0
#define DFAN_DESC   1

#define DFAN_LAB_BLKSIZE   64   /* default blksize to use for labels */
#define DFAN_DESC_BLKSIZE 512   /* default blksize to use for descriptions */

#define DFAN_DEFENTRIES 16  /* no of dir entries to add at a time */

/*
 * This structure stores an entry in the label/desc directory
 * for a label/desc in the file, it gives the ref of the label/desc,
 * and the tag/ref of the data item to which the label/desc relates
 */
typedef struct
  {
      uint16      annref;       /* ref of annotation */
      uint16      datatag;      /* tag of data */
      uint16      dataref;      /* ref of data */
  }
DFANdirentry;

/*
 * This structure is a head node for the directory, which is organized as
 * as a linked list of arrays.  DFANdirentry is the structure of an
 * array element, while DFANdirhead is the list element
 */
typedef struct DFANdirhead
  {
      struct DFANdirhead *next; /* list element */
      int32       nentries;     /* Numer of entries */
      DFANdirentry entries[1];  /* actually an arbitrary size array */
  }
DFANdirhead;

#endif /* DFAN_H */
