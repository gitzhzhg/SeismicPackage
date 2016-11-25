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

/* $Id: maldebug.h,v 1.7 1995/04/18 15:33:50 koziol Exp $ */

/*----------------------------------------------------------------------
 *
 *  maldebug.h -- Dynamic memory handler interface
 *  Description: memdebug.h provides the interface definitions for the dynamic
 *  memory handler.
 *  See memdebug.c for complete documentation.
 *
 */

#ifndef _MALDEBUG_H
#define _MALDEBUG_H

/* Compilation options */
#define MEM_LIST    /* Build internal list */
#define MEM_WHERE   /* Keep track of memory block source */
#define MEM_HEADER  /* Keep headers and footers around for each block */
#define MEM_COMP_FREE   /* Complement the space free'd */

/* Interface functions */
unsigned long Mem_Used(void);
void        Mem_Display(FILE * fp);

/* Interface functions to access only through macros */
#if defined(MEM_WHERE)
void       *mem_HDmalloc(size_t size, char *fil, int lin);
void       *mem_HDrealloc(void *old_ptr, size_t size, char *fil, int lin);
void       *mem_HDfree(void *ptr, char *fil, int lin);
#else
void       *mem_HDmalloc(size_t size);
void       *mem_HDrealloc(void *old_ptr, size_t size);
void       *mem_HDfree(void *ptr);
#endif

/* Interface macros */
#if !defined(__MALDEBUG__)
#if defined(MEM_WHERE)
#define HDmalloc(a)         mem_HDmalloc((a),__FILE__,__LINE__)
#define HDrealloc(a,b)      mem_HDrealloc((a),(b),__FILE__,__LINE__)
#define HDfree(a)           mem_HDfree((a),__FILE__,__LINE__)
#else
#define HDmalloc(a)         mem_HDmalloc(a)
#define HDrealloc(a,b)      mem_HDrealloc((a),(b))
#define HDfree(a)           mem_HDfree(a)
#endif
#endif

#endif /* _MALDEBUG_H */
