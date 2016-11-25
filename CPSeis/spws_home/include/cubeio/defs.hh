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
/*[
 * GOCAD Software
 * Association Scientifique pour la Geologie et ses Applications (ASGA)
 * Copyright (c) 1993-1996 ASGA. All Rights Reserved.
 *
 * This program is a Trade Secret of the ASGA and it is not to be:
 * - reproduced, published, or disclosed to other,
 * - distributed or displayed,
 * - used for purposes or on Sites other than described
 *   in the GOCAD Advancement Agreement,
 * without the prior written authorization of the ASGA. Licencee
 * agrees to attach or embed this Notice on all copies of the program,
 * including partial copies or modified versions thereof.
]*/

#ifndef gocad_utils_misc_defs_h
#define gocad_utils_misc_defs_h


#undef BACKWARD_COMPATIBILITY

/****************************************************************************/
// Boolean values




/* The following will not compile on Solaris 6.2 08/2001
#if !defined(__GNUC__) && !(sgi && _BOOL && __EDG_ABI_COMPATIBILITY_VERSION>227)
typedef unsigned bool;
static const bool false = 0;
static const bool true = 1;
#endif
*/

#ifdef sun 
typedef	unsigned char bool;
static const bool false = 0;
static const bool true = 1;
#endif

typedef bool boolean;

#ifdef BACKWARD_COMPATIBILITY
typedef bool boolean;
#endif


/****************************************************************************/
// Null pointer definition and checking macros

#ifndef nil
#define nil 0
#endif

/* This causes problems on some platforms - 
   I have not taken the time to figure out details.
inline bool is_nil(const void* p) { return p == (void*)0; }
inline bool is_not_nil(const void* p) { return p != (void*)0; }
*/


/****************************************************************************/
// Miscellanous macros for statically casting a pointer to a given type

#ifndef static_cast
#define static_cast(T,p) ((T*)p)
#endif

#ifndef static_const_cast
#define static_const_cast(T,p) ((const T*)p)
#endif

#ifdef BACKWARD_COMPATIBILITY

#ifndef static_cast
#define static_cast(p,T)  static_cast(T,p)
#endif
#ifndef static_const_cast
#define static_const_cast(p,T)  static_const_cast(T,p)
#endif

#endif


/****************************************************************************/
// Macros for deleting pointers and arrays and set the pointer back to nil

#ifndef DELETE
#define DELETE(p) (delete p, p = nil)
#define DELETE_ARRAY(p) (delete[] p, p = nil)
#endif


/****************************************************************************/
// Miscellanous macros for concatenating identifiers

#ifndef cpp_concat
#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __cpp_concat2(A,B)      A##B
#define __cpp_concat3(A,B,C)    A##B##C
#define __cpp_concat4(A,B,C,D)  A##B##C##D
#define cpp_concat(A,B)         __cpp_concat2(A,B)
#define cpp_concat3(A,B,C)      __cpp_concat3(A,B,C)
#define cpp_concat4(A,B,C,D)    __cpp_concat4(A,B,C,D)
#else
#define __cpp_concat2(A,B)      A/**/B
#define __cpp_concat3(A,B,C)    A/**/B/**/C
#define __cpp_concat4(A,B,C,D)  A/**/B/**/C/**/D
#define cpp_concat(A,B)         __cpp_concat2(A,B)
#define cpp_concat3(A,B,C)      __cpp_concat3(A,B,C)
#define cpp_concat4(A,B,C,D)    __cpp_concat4(A,B,C,D)
#endif
#endif

#endif
