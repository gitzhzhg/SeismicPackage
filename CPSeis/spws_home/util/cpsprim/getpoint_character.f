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
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C                             fortran code
C                  designed to be called from fortran
C
C    Primitive name:  GETPOINT_CHARACTER
C  Source directory:  primitives/memory
C           Library:  conlib          
C           Written:  92/07/29   by:  Tom Stoeckley
C      Last revised:  92/07/29   by:  Tom Stoeckley
C
C  Purpose:       brief reason for its existance
C
C  Related Documentation:  See the primitive GETPOINT_INTEGER for the
C                          documentation of this primitive.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 92/07/29  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:        getpoint_character
C  Functions:          none
C  Subroutine entries: putpoint_character
C  Function entries:   none
C  Common blocks:      none
C  Include files:      none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C              getpoint_string            convert_hh2cc  
C              putpoint_string            convert_cc2hh  
C-----------------------------------------------------------------------
C\END DOC


      subroutine getpoint_character (ipoint,index,cvar)
c     copy pointee to character value.
c     copy character value to pointee.

      implicit none
      character*(*) cvar
      integer ipoint,index,buffer(60),itype

      itype=len(cvar)
      call getpoint_string (ipoint,index,itype, buffer    ,itype)
      cvar=' '
      call convert_hh2cc (buffer,60,   cvar,itype)
      return

      entry putpoint_character (cvar,ipoint,index)
      itype=len(cvar)
      call convert_cc2hh (cvar,itype,   buffer,60)
      call putpoint_string (buffer    ,itype, ipoint,index,itype)
      return
      end


