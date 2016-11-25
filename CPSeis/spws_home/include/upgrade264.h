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
/*<CPS_v1 type="HEADER_FILE"/>
!------------------------------- bfio.h ----------------------------------
!------------------------------- bfio.h ----------------------------------
!------------------------------- bfio.h ----------------------------------
!other files are:                bfio.c


!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : upgrade264.h
! Category   : io
! Written    : 2007-03-20   by: Kruger Corn
! Revised    : 2007-03-20   by: Kruger Corn
! Maturity   : beta
! Purpose    : Used to upgrade from 32 bit to 64 bit architecture.
! References : See /usr/include
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!     Date        Author          Description
!     ----        ------          -----------
!  1. 2007-03-20  Kruger Corn     Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
*/

/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/

#ifndef UPGRADE264_H
#define UPGRADE264_H

#ifdef SUN
#include <sys/int_types.h> /* for int32_t & int64_t */
#include <sys/int_fmtio.h> /* for PRIi*             */
#elif SGI64
#include <inttypes.h>      /* for int32_t & int64_t */
#include <sgidefs.h>       /* for _MIPS_SZLONG      */

/* since SGI64 does not have PRIi* stuff create it */
#define __WORDSIZE _MIPS_SZLONG

# if __WORDSIZE == 64
#  define __PRI64_PREFIX        "l"
#  define __PRIPTR_PREFIX       "l"
# else
#  define __PRI64_PREFIX        "ll"
#  define __PRIPTR_PREFIX
# endif

/* Macros for printing format specifiers.  */

/* Decimal notation.  */
# define PRId8          "d"
# define PRId16         "d"
# define PRId32         "d"
# define PRId64         __PRI64_PREFIX "d"

# define PRIdLEAST8     "d"
# define PRIdLEAST16    "d"
# define PRIdLEAST32    "d"
# define PRIdLEAST64    __PRI64_PREFIX "d"

# define PRIdFAST8      "d"
# define PRIdFAST16     __PRIPTR_PREFIX "d"
# define PRIdFAST32     __PRIPTR_PREFIX "d"
# define PRIdFAST64     __PRI64_PREFIX "d"


# define PRIi8          "i"
# define PRIi16         "i"
# define PRIi32         "i"
# define PRIi64         __PRI64_PREFIX "i"

# define PRIiLEAST8     "i"
# define PRIiLEAST16    "i"
# define PRIiLEAST32    "i"
# define PRIiLEAST64    __PRI64_PREFIX "i"

# define PRIiFAST8      "i"
# define PRIiFAST16     __PRIPTR_PREFIX "i"
# define PRIiFAST32     __PRIPTR_PREFIX "i"
# define PRIiFAST64     __PRI64_PREFIX "i"

/* Octal notation.  */
# define PRIo8          "o"
# define PRIo16         "o"
# define PRIo32         "o"
# define PRIo64         __PRI64_PREFIX "o"

# define PRIoLEAST8     "o"
# define PRIoLEAST16    "o"
# define PRIoLEAST32    "o"
# define PRIoLEAST64    __PRI64_PREFIX "o"
# define PRIoFAST8      "o"
# define PRIoFAST16     __PRIPTR_PREFIX "o"
# define PRIoFAST32     __PRIPTR_PREFIX "o"
# define PRIoFAST64     __PRI64_PREFIX "o"

/* Unsigned integers.  */
# define PRIu8          "u"
# define PRIu16         "u"
# define PRIu32         "u"
# define PRIu64         __PRI64_PREFIX "u"

# define PRIuLEAST8     "u"
# define PRIuLEAST16    "u"
# define PRIuLEAST32    "u"
# define PRIuLEAST64    __PRI64_PREFIX "u"

# define PRIuFAST8      "u"
# define PRIuFAST16     __PRIPTR_PREFIX "u"
# define PRIuFAST32     __PRIPTR_PREFIX "u"
# define PRIuFAST64     __PRI64_PREFIX "u"

/* lowercase hexadecimal notation.  */
# define PRIx8          "x"
# define PRIx16         "x"
# define PRIx32         "x"
# define PRIx64         __PRI64_PREFIX "x"

# define PRIxLEAST8     "x"
# define PRIxLEAST16    "x"
# define PRIxLEAST32    "x"
# define PRIxLEAST64    __PRI64_PREFIX "x"

# define PRIxFAST8      "x"
# define PRIxFAST16     __PRIPTR_PREFIX "x"
# define PRIxFAST32     __PRIPTR_PREFIX "x"
# define PRIxFAST64     __PRI64_PREFIX "x"

/* UPPERCASE hexadecimal notation.  */
# define PRIX8          "X"
# define PRIX16         "X"
# define PRIX32         "X"
# define PRIX64         __PRI64_PREFIX "X"

# define PRIXLEAST8     "X"
# define PRIXLEAST16    "X"
# define PRIXLEAST32    "X"
# define PRIXLEAST64    __PRI64_PREFIX "X"

# define PRIXFAST8      "X"
# define PRIXFAST16     __PRIPTR_PREFIX "X"
# define PRIXFAST32     __PRIPTR_PREFIX "X"
# define PRIXFAST64     __PRI64_PREFIX "X"


/* Macros for printing `intmax_t' and `uintmax_t'.  */
# define PRIdMAX        __PRI64_PREFIX "d"
# define PRIiMAX        __PRI64_PREFIX "i"
# define PRIoMAX        __PRI64_PREFIX "o"
# define PRIuMAX        __PRI64_PREFIX "u"
# define PRIxMAX        __PRI64_PREFIX "x"
# define PRIXMAX        __PRI64_PREFIX "X"

#else
#include <stdint.h>        /* for int32_t & int64_t */
#include <inttypes.h>      /* for PRI*              */
#endif

#endif
