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
C              designed to be called from fortran or C
C
C    Primitive name:  GET_INIL         (get nil)                             
C  Source directory:  [cps.primitives.math] and ~spws/cps/PRIMITIVES.DIR
C           Library:  conlib                and ~spws/lib/cpsprim.a
C           Written:  92/07/27   by:  Tom Stoeckley
C      Last revised:  92/10/01   by:  Tom Stoeckley
C
C  Purpose:       Get or set a value which will be interpreted as
C                 a nil, or an error flag, for integer, real, and 
C                 double precision numbers.
C
C  Related Documentation:  Used by the routines in primitive
C                          CONVERT_II2CC.
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS PRIMITIVE ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C  To get a nil flag or an error flag:
C                              o 
C              call get_inil (inil)      for integer variables
C              call get_fnil (fnil)      for real variables
C              call get_dnil (dnil)      for double precision variables
C                              o 
C              call get_ierr (ierr)      for integer variables
C              call get_ferr (ferr)      for real variables
C              call get_derr (derr)      for double precision variables
C
C  To reset a nil flag or an error flag:
C                              i 
C              call set_inil (inil)      for integer variables
C              call set_fnil (fnil)      for real variables
C              call set_dnil (dnil)      for double precision variables
C                              i 
C              call set_ierr (ierr)      for integer variables
C              call set_ferr (ferr)      for real variables
C              call set_derr (derr)      for double precision variables
C
C  integer inil = nil value for integers     (default -888728)
C  real    fnil = nil value for reals        (default -1.E-30)
C  double  dnil = nil value for doubles      (default -1.D-30)
C
C  integer ierr = error flag for integers    (default -999729)
C  real    ferr = error flag for reals       (default -2.E-30)
C  double  derr = error flag for doubles     (default -2.D-30)
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.  The default nil value for real variables is the value used in
C      CPS static files.
C
C  2.  If you wish to call these subroutines from C, you should include
C      the C header file get_inil.h to guarantee correct usage via
C      function prototypes and to deal with Fortran external naming
C      conventions.  See the help file mixing_c_and_fortran for more
C      specifics.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 92/10/01  Stoeckley  Add documentation and C header file.
C  1. 92/07/27  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:        GET-INIL
C                      GET_FNIL
C                      GET_DNIL
C  Functions:          none
C  Subroutine entries:             SET_INIL   GET_IERR   SET_IERR
C                                  SET_FNIL   GET_FERR   SET_FERR
C                                  SET_DNIL   GET_DERR   SET_DERR
C  Function entries:   none
C  Common blocks:      none
C  Include files:      get_inil.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC



      subroutine get_inil (ivar)
      implicit none
      integer ivar,inil,ierr
      save inil,ierr
      data inil,ierr/-888728,-999729/
c------------------------------------------get nil flag or error flag.
c     entry get_inil (ivar)
      ivar=inil
      return
      entry get_ierr (ivar)
      ivar=ierr
      return
c------------------------------------------reset nil flag or error flag.
      entry set_inil (ivar)
      inil=ivar
      return
      entry set_ierr (ivar)
      ierr=ivar
      return
      end




      subroutine get_fnil (fvar)
      implicit none
      real fvar,fnil,ferr
      save fnil,ferr
      data fnil,ferr/-1.E-30,-2.E-30/
c------------------------------------------get nil flag or error flag.
c     entry get_fnil (fvar)
      fvar=fnil
      return
      entry get_ferr (fvar)
      fvar=ferr
      return
c------------------------------------------reset nil flag or error flag.
      entry set_fnil (fvar)
      fnil=fvar
      return
      entry set_ferr (fvar)
      ferr=fvar
      return
      end




      subroutine get_dnil (dvar)
      implicit none
      double precision dvar,dnil,derr
      save dnil,derr
      data dnil,derr/-1.D-30,-2.D-30/
c------------------------------------------get nil flag or error flag.
c     entry get_dnil (dvar)
      dvar=dnil
      return
      entry get_derr (dvar)
      dvar=derr
      return
c------------------------------------------reset nil flag or error flag.
      entry set_dnil (dvar)
      dnil=dvar
      return
      entry set_derr (dvar)
      derr=dvar
      return
      end


