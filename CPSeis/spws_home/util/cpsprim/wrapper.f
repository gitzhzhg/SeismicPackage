C***************************** COPYRIGHT NOTICE ********************************
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C    Primitive name:  WRAPPER      (wrapper for CPS processes)
C  Source directory:  primitives/main_prog
C           Library:  conlib
C           Written:  92/05/11  by:  Tom Stoeckley
C      Last revised:  92/10/01  by:  Tom Stoeckley
C
C  Purpose: This is a wrapper to be used to call a CPS process
C           without using alternate returns.  It can be called
C           from C or from Fortran.
C
C  Related Documentation:  
C-----------------------------------------------------------------------
C        THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C  Parameters are real or integer based on Fortram conventions.
C  Arrays, hollerith, and character parameters are identified as such.
C-----------------------------------------------------------------------
C  To call a CPS process without using alternate returns:
C                       i     i    i    o
C       CALL WRAPPERS (NAMES,IPN,ISAVE,IERR)        ! setup entry
C       CALL WRAPPER  (NAME ,IPN,N,HD,TR,LTR,IERR)  ! main entry
C                       i     i  b b  b   b   o
C  NAMES           = name of CPS process setup entry (external).
C  NAME            = name of CPS process main entry (external).
C  IPN,ISAVE       = arguments that are in the CPS process setup call.
C  IPN,N,HD,TR,LTR = arguments that are in the CPS process main call.
C  IERR            = error return (0 means no error).
C-----------------------------------------------------------------------
C                            NOTES
C
C  1. This wrapper works for single trace processes only.  Other similar
C     wrappers will be needed for processes with other argument lists.
C     It is recommended that these additional wrappers, when needed, be
C     added to this primitive in this file.
C
C  2. If you call these routines from C, you should include the C header
C     file wrapper.h to guarantee correct usage via function prototypes
C     and to deal with Fortran external spelling conventions.  See the
C     help file mixing_c_and_fortran for more details.
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 2.  92/10/01 Stoeckley    Add documentation and C header file.
C 1.  92/05/11 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     WRAPPER
C  Functions:       none
C  Entry points:    WRAPPERS
C  Include files:   wrapper.h
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C                       (two formal parameters)
C-----------------------------------------------------------------------
C\END DOC

      SUBROUTINE WRAPPER (NAME,IPN,N,HD,TR,LTR,IERR)
      external name,names
      CALL       NAME         (IPN,N,HD,TR,LTR,*999)
      ierr=0
      return

      ENTRY WRAPPERS (NAMES,IPN,ISAVE,IERR)
      CALL  NAMES          (IPN,ISAVE,*999)
      ierr=0
      return

999   ierr=1
      return
      END

