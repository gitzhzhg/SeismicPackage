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
C         (previously designed to be called from fortran or C)
C
C    Primitive name:  CONVERT_II2CC   (convert integer to character)         
C  Source directory:  [cps.primitives.math] and ~spws/cps/PRIMITIVES.DIR
C           Library:  conlib                and ~spws/lib/cpsprim.a
C           Written:  92/07/28   by:  Tom Stoeckley
C      Last revised:  93/06/09   by:  Tom Stoeckley
C
C  Purpose:       Convert a number to a character string, or vice
C                 versa.  Can be used for integers, real values, and
C                 double precision values.  Nil values and special
C                 error flag values are supported.
C
C  Related Documentation: 
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS PRIMITIVE ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C  To convert a number to a character string:
C                         i    o     i    i
C    call convert_ii2cc (ivar,cvar,nchar)        ! for integer variables
C    call convert_ff2cc (fvar,cvar,nchar,ndec)   ! for real variables
C    call convert_dd2cc (dvar,cvar,nchar,ndec)   ! for double variables
C    call convert_ii2hh (ivar,hvar,nchar)        ! for integers (from C)
C    call convert_ff2hh (fvar,hvar,nchar,ndec)   ! for reals    (from C)
C    call convert_dd2hh (dvar,hvar,nchar,ndec)   ! for doubleS  (from C)
C
C  To convert a character string to a number:
C                         i    o     o
C    call convert_cc2ii (cvar,ivar,istat)   ! for integer variables
C    call convert_cc2ff (cvar,fvar,istat)   ! for real variables
C    call convert_cc2dd (cvar,dvar,istat)   ! for double variables
C    call convert_hh2ii (hvar,ivar,istat)   ! for integers (from C)   
C    call convert_hh2ff (hvar,fvar,istat)   ! for reals    (from C)   
C    call convert_hh2dd (hvar,dvar,istat)   ! for doubles  (from C)   
C
C  integer          ivar = number to convert to or from a text string.
C  real             fvar = number to convert to or from a text string.
C  double precision dvar = number to convert to or from a text string.
C  character*(*)    cvar = text string containing converted number.
C  integer       hvar(*) = null-terminated text string to use with C.
C  integer         nchar = number of characters to convert.
C  integer          ndec = maximum number of decimals to convert.
C  integer         istat = status of converted number.
C
C       NOTE: The routines with "hh" in the names are now obsolete,
C       and should be replaced by similar routines with "ss" in
C       their names (a C-language primitive).
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. If nchar<=0, it is assumed to match the length of the character 
C     variable cvar.  In calls using variable hvar (called from C),
C     nchar must be greater than zero.
C
C  2. The routines documented above support two special values for
C     each type of number: a nil value and an error flag.  These
C     values can be retrieved or set by using routines in the
C     primitive GET_INIL. 
C
C  3. A nil value is displayed as a blank, and a blank display is
C     interpreted as a nil.
C
C  4. An error flag is displayed as an asterisk, and any display that
C     cannot be converted to a number is converted to the error flag.
C     A displayed asterisk can also mean that an otherwise valid
C     number cannot be displayed in the designated field.
C
C  5. Upon return from the second set of routines:
C     (a) istat=1 means the conversion was successful;
C     (b) istat=0 means the number is set to nil (cvar was a blank);
C     (c) istat=-1 means the number is set to an error flag (the
C            conversion was not successful or cvar was an asterisk).
C
C  6. When calling any of these routines from C, you should include
C     the C header file convert_ii2cc.h which contains function
C     prototypes and spelling adjustments.  See the help file called
C     mixing_c_and_fortran for rationale.  (The only routines you
C     should call from C are those with "hh" as part of the name.)
C
C       NOTE: The routines with "hh" in the names are now obsolete,
C       and should be replaced by similar routines with "ss" in
C       their names (a C-language primitive).
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  3. 93/06/09  Stoeckley  Change to call convert_ii2ss.c routines.
C                            Old stuff is commented out.
C  3. 92/10/01  Stoeckley  Fix bug in convert_xx2hh and hh2xx, and
C                          add header file and documentation.
C  2. 92/09/15  Stoeckley  Add conversions to and from hollerith for C.
C  1. 92/07/28  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:  convert_ii2cc   convert_cc2ii
C                convert_ff2cc   convert_cc2ff
C                convert_dd2cc   convert_cc2dd
C                convert_ii2hh 
C                convert_ii2cc_internal (commented out)
C                convert_dd2cc_internal (commented out)
C  Functions:          none
C  Subroutine entries: convert_ff2hh   convert_hh2ff   convert_hh2ii
C                      convert_dd2hh   convert_hh2dd
C  Function entries:   none
C  Common blocks:      none
C  Include files:      convert_ii2cc.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C     get_inil     get_fnil    get_dnil
C     get_ierr     get_ferr    get_derr
C-----------------------------------------------------------------------
C\END DOC


cccccccccccccccccccc bridging routines from C-language:
c (should now call C routines with "ss" in them instead of "hh")

      subroutine convert_ii2hh (ivar,hvar,nchar)
      implicit none
      integer ivar,hvar(*),nchar,ndec,istat
c     integer nbuf,sizeof_integer
      real fvar
      double precision dvar
c     character*200 cvar

ccc   entry convert_ii2hh (ivar,hvar,nchar)
      call  convert_ii2ss (ivar,hvar,nchar)
c     call  convert_ii2cc (ivar,cvar,nchar)
c     nbuf=1+nchar/sizeof_integer()
c     call  convert_cc2hh (cvar,nchar,   hvar,-nbuf)
      return

      entry convert_ff2hh (fvar,hvar,nchar,ndec)
      call  convert_ff2ss (fvar,hvar,nchar,ndec)
c     call  convert_ff2cc (fvar,cvar,nchar,ndec)
c     nbuf=1+nchar/sizeof_integer()
c     call  convert_cc2hh (cvar,nchar,   hvar,-nbuf)
      return

      entry convert_dd2hh (dvar,hvar,nchar,ndec)
      call  convert_dd2ss (dvar,hvar,nchar,ndec)
c     call  convert_dd2cc (dvar,cvar,nchar,ndec)
c     nbuf=1+nchar/sizeof_integer()
c     call  convert_cc2hh (cvar,nchar,   hvar,-nbuf)
      return

      entry convert_hh2ii (hvar,ivar,istat)
      call  convert_ss2ii (hvar,ivar,istat)
c     call  convert_hh2cc (hvar,0,   cvar,0)
c     call  convert_cc2ii (cvar,ivar,istat)
      return

      entry convert_hh2ff (hvar,fvar,istat)
      call  convert_ss2ff (hvar,fvar,istat)
c     call  convert_hh2cc (hvar,0,   cvar,0)
c     call  convert_cc2ff (cvar,fvar,istat)
      return

      entry convert_hh2dd (hvar,dvar,istat)
      call  convert_ss2dd (hvar,dvar,istat)
c     call  convert_hh2cc (hvar,0,   cvar,0)
c     call  convert_cc2dd (cvar,dvar,istat)
      return
      end




ccccccccccccccccccccc convert number to character string:


      SUBROUTINE convert_II2CC (IVAR,CVAR,NCHAR)
C     convert integer to character string.
      implicit none
      CHARACTER*(*) CVAR
      integer ivar,nchar
c     integer inil,ierr
      integer hvar(100)

      call convert_ii2ss (ivar,hvar,nchar)
      call convert_hh2cc (hvar,100,  cvar,nchar)
c     call get_inil (inil)
c     call get_ierr (ierr)
c     IF (IVAR.EQ.0) then
c          cvar='0'                        ! quick interception of zero.
c     else IF (IVAR.EQ.INIL) then
c          cvar=' '                        ! nil flag encountered.
c     else if (ivar.eq.ierr) then
c          cvar='*'                        ! error flag encountered.
c     else
c          call convert_ii2cc_internal (ivar,cvar,nchar)
c     end if
      return
      end



      SUBROUTINE convert_FF2CC (FVAR,CVAR,NCHAR,ndec)
C     convert float to character string.
      implicit none
      CHARACTER*(*) CVAR
      real fvar
c     real fnil,ferr
      integer nchar,ndec
      double precision dvar
      integer hvar(100)

      call convert_ff2ss (fvar,hvar,nchar,ndec)
      call convert_hh2cc (hvar,100,  cvar,nchar)
c     call get_fnil (fnil)
c     call get_ferr (ferr)
c     IF (FVAR.EQ.0) then
c          cvar='0'                        ! quick interception of zero.
c     else IF (FVAR.EQ.FNIL) then
c          cvar=' '                        ! nil flag encountered.
c     else if (Fvar.eq.Ferr) then
c          cvar='*'                        ! error flag encountered.
c     else
c          dvar=fvar
c          call convert_dd2cc_internal (dvar,cvar,nchar,ndec)
c     end if
      return
      end



      SUBROUTINE convert_DD2CC (DVAR,CVAR,NCHAR,ndec)
C     convert double precision to character string.
      implicit none
      CHARACTER*(*) CVAR
      double precision Dvar
c     double precision Dnil,Derr
      integer nchar,ndec
      integer hvar(100)

      call convert_dd2ss (dvar,hvar,nchar,ndec)
      call convert_hh2cc (hvar,100,  cvar,nchar)
c     call get_Dnil (Dnil)
c     call get_Derr (Derr)
c     IF (DVAR.EQ.0) then
c          cvar='0'                        ! quick interception of zero.
c     else IF (DVAR.EQ.DNIL) then
c          cvar=' '                        ! nil flag encountered.
c     else if (Dvar.eq.Derr) then
c          cvar='*'                        ! error flag encountered.
c     else
c          call convert_DD2cc_internal (Dvar,cvar,nchar,ndec)
c     end if
      return
      end



ccccccccccccccccccccc convert character string to number:



      subroutine convert_cc2ii (cvar,ivar,istat)
c     convert character string to integer.
      implicit none
      CHARACTER*(*) CVAR
      integer ivar,istat
c     CHARACTER*80 BUFFER
      integer hvar(100)

      call convert_cc2hh (cvar,0,   hvar,100)
      call convert_ss2ii (hvar,ivar,istat)
c     IF (CVAR.EQ.'0') then
c          istat=1
c          ivar=0                          ! quick interception of zero.
c     else IF (CVAR.EQ.' ') then
c          istat=0
c          call get_inil (ivar)            ! nil flag returned.
c     else
c          BUFFER=CVAR
c          READ (BUFFER,'(BN,I80)',ERR=999) IVAR
c          istat=1
c     end if
      return

c999   istat=-1
c      call get_ierr (ivar)                 ! error flag returned.
c      return
      end



      subroutine convert_cc2ff (cvar,fvar,istat)
c     convert character string to float.
      implicit none
      CHARACTER*(*) CVAR
      real fvar
      integer istat
c     CHARACTER*80 BUFFER
      integer hvar(100)

      call convert_cc2hh (cvar,0,   hvar,100)
      call convert_ss2ff (hvar,fvar,istat)
c     IF (CVAR.EQ.'0') then
c          istat=1
c          fvar=0                          ! quick interception of zero.
c     else IF (CVAR.EQ.' ') then
c          istat=0
c          call get_fnil (fvar)            ! nil flag returned.
c     else
c          BUFFER=CVAR
c          READ (BUFFER,'(BN,F80.0)',ERR=999) FVAR
c          istat=1
c     end if
      return

c999   istat=-1
c      call get_ferr (fvar)                 ! error flag returned.
c      return
      end



      subroutine convert_cc2dd (cvar,dvar,istat)
c     convert character string to double precision.
      implicit none
      CHARACTER*(*) CVAR
      double precision dvar
      integer istat
c     CHARACTER*80 BUFFER
      integer hvar(100)

      call convert_cc2hh (cvar,0,   hvar,100)
      call convert_ss2dd (hvar,dvar,istat)
c     IF (CVAR.EQ.'0') then
c          istat=1
c          dvar=0                          ! quick interception of zero.
c     else IF (CVAR.EQ.' ') then
c          istat=0
c          call get_dnil (dvar)            ! nil flag returned.
c     else
c          BUFFER=CVAR
c          READ (BUFFER,'(BN,F80.0)',ERR=999) DVAR
c          istat=1
c     end if
      return

c999   istat=-1
c      call get_derr (dvar)                 ! error flag returned.
c      return
      end



ccccccccccccccccccccc helping routines:




c      subroutine convert_ii2cc_internal (ivar,cvar,nchar)
c      implicit none
c      integer ivar,nchar,nchar2,i
c      CHARACTER*(*) CVAR
c      CHARACTER*80 BUFFER
cC----------DO THE WORK.
c      WRITE (BUFFER,'(I80)',ERR=999) IVAR
c      DO 20 I=79,1,-1
c      IF (BUFFER(I:I).EQ.' ') GO TO 30
c20    CONTINUE
c      I=0
c30    nchar2=len(cvar)
c      if (nchar.gt.0) nchar2=min(nchar2,nchar)
c      IF (80-I.GT.NCHAR2) GO TO 999
c      CVAR=BUFFER(I+1:80)
c      RETURN
c999   cvar='*'
c      RETURN
c      end



c      SUBROUTINE convert_DD2CC_internal (DVAR,CVAR,NCHAR,NDEC)
cC     CONVERTS DOUBLE PRECISION NUMBER TO CHARACTER STRING (LEFT JUSTIFIED).
cC     RETURNS ASTERISKS IF ERROR ENCOUNTERED.
cC     NCHAR>0 SAYS HOW MANY CHARACTERS TO WRITE.
cC     NCHAR<=0 MEANS TO WRITE ALL OF CHARACTER STRING CVAR.
cC     NDEC IS USED TO RESTRICT THE NUMBER OF DECIMAL DIGITS.
cC----------DIMENSION STATEMENTS.
c      implicit none
c      double precision dvar
c      CHARACTER*(*) CVAR
c      integer nchar,ndec,nchar2,ndec2,ia,ib
c      CHARACTER*80 BUFFER
c      CHARACTER*8 fmt
cC----------GET LENGTH OF CHARACTER STRING, and maximum number of decimals.
c      NCHAR2=min(LEN(CVAR),80)
c      IF (NCHAR.GT.0) NCHAR2=MIN(NCHAR2,NCHAR)
c      NDEC2=MAX(0,MIN(NDEC,30,nchar2-1))
cC----------DO THE WORK.
c10    write (fmt,1000) ndec2
c1000  format ('(F80.',I2,')')
c      write (buffer,fmt,err=995) dvar
cc--------------------find first non-blank character.
c      do ia=1,80
c           if (buffer(ia:ia).ne.' ') go to 15
c      end do
cccccc should not get to this line.
cc--------------------find last significant character.
c15    do ib=80,ia+1,-1
c           if (buffer(ib:ib).ne.'0') go to 16
c      end do
c      ib=ia
cc-----------------------decide what to do with the results.
c16    if (ib.gt.ia.and.buffer(ib:ib).eq.'.') ib=ib-1
c      if (ib-ia.lt.nchar2) then
c           cvar=buffer(ia:ib)
c           return
c      else
c           ndec2=min(ndec2-1,ndec2+nchar2+ia-81)
c           if (ndec2.ge.0) go to 10
c      end if
cc------------------------try using an exponential format.
c995   ndec2=min(ndec,30,nchar2-7)
c      if (ndec2.lt.0) go to 999
c      write (fmt,2000) nchar2,ndec2
c2000  format ('(E',I2,'.',I2,')')
c      write (cvar,fmt,err=999) dvar
c      return
c999   cvar='*'
c      RETURN
c      END


