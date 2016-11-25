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
C    Primitive name:  CONVERT_CC2HH     (CONVERT VARIABLES) 
C  Source directory:  primitives/character
C           Library:  CONLIB
C           Written:  92/07/22  by:  Tom Stoeckley
C      Last revised:  98/01/16  by:  R. Day
C
C  Purpose:   Convert variables to different forms.  This includes
C             conversions between characters variables and the following 
C             types of variables: hollerith, real, integer, double.
C             Also modifications of character variables.
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
C  To convert character variable to hollerith:
C                                i     i        o     i
C           call convert_cc2hh (cvar,nchar,   buffer,nbuf)
C
C  character*(*) cvar = character variable to convert to hollerith.
C  integer nchar      = number of characters to convert.
C  integer buffer(*)  = hollerith array containing character variable.
C  integer nbuf       = number of words in hollerith array.
C
C  It is OK for nchar to be <,=,> length of cvar, or zero or negative.
C    If nchar<=0, it is taken to be the length of cvar.
C  It is OK for nbuf to be <,=,> zero.
c    iabs(nbuf) = full length of buffer in integers, or zero if unknown.  
c    If nbuf>=0, buffer is terminated with null after nchar characters.
c    If nbuf<0, buffer is terminated with null after last non-blank character.
C    If nbuf=0, and the length of buffer is not sufficient to store the 
C      required number of characters plus a null, you will write past 
C      the end of the buffer.
C    If nbuf is not zero, and the length of buffer is not sufficient
C      to store the required number of characters (plus a null if
C      requested), some characters will be lost (but the null if
C      requested will still be there).
C  If cvar is entirely blank, buffer will still have at least one blank 
C      before the null.
c  Only if nchar<0 and nbuf>0, buffer is blank filled (no null).
C-----------------------------------------------------------------------
C  To convert hollerith variable to character:
C                                 i     i       o     i
C           call convert_hh2cc (buffer,nbuf,   cvar,nchar)
C
C  integer buffer(*)  = hollerith array to convert to character.
C  integer nbuf       = number of words in hollerith array.
C  character*(*) cvar = character variable returned.
C  integer nchar      = number of characters to convert.
C
c  buffer might or might not be terminated with null.
C  It is OK for nbuf to be <,=,> zero.
c    iabs(nbuf) = full length of buffer in integers, or zero if unknown.  
C    If nbuf=0, and buffer is not null terminated, and the length of
C      buffer is not sufficient to store nchar characters, you will read
C      past the end of the buffer.
c  cvar will always be blank filled (no null).
C  It is OK for nchar to be <,=,> length of cvar, or zero or negative.
C    If nchar<=0, it is taken to be the length of cvar.
C-----------------------------------------------------------------------
C  To convert a character variable to all upper case:
c  To convert all nulls to blanks in a character variable:
c  To squeeze out blanks in a character variable:
C  To right-justify the characters in a character variable:
C  To enforce character data to be alpha-numeric(32:126)
C                                          b     i 
c           call convert_to_upper        (cvar)
c           call convert_nulls_to_blanks (cvar)
c           call convert_squeeze_blanks  (cvar)
c           call convert_right_justify   (cvar,nchar)
c           call convert_enforce_alphanum(cvar,ic)
C
C  character*(*) cvar = character variable to convert.
C  integer nchar      = number of characters to convert.
C  integer ic         = decimal value of character replacement
C
C  In the first three routines, the entire character variable is
C    always converted.  In the fourth routine, the entire character
C    variable is converted if nchar<=0.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  3. 98/01/16  Day        Added convert_enforce_alphanum
C  2. 94/02/10  Stoeckley  Add some documentation for convert_cc2hh
C                            indicating that a blank character variable
C                            will be converted to a hollerith string
C                            with at least one blank before the null.
C  1. 92/07/22  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Subroutines:        convert_cc2hh           convert_to_upper
C                      convert_hh2cc           convert_nulls_to_blanks
C                      convert_squeeze_blanks  convert_right_justify 
C                      convert_enforce_alphanum
C  Functions:          none
C  Subroutine entries: none
C  Function entries:   none
C  Common blocks:      none
C  Include files:      none
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C   char_into_buffer    char_from_buffer     sizeof_integer
C-----------------------------------------------------------------------
C\END DOC



      subroutine convert_cc2hh (cvar,nchar,   buffer,nbuf)
c     copy character variable to hollerith buffer.
c     if nbuf>=0, buffer is terminated with null after nchar characters.
c     if nbuf<0, buffer is terminated with null after last non-blank character.
c     nchar can be <,=,> length of cvar, or zero.
c     iabs(nbuf) can be the length of the buffer in integers, or zero.
c     if nchar<0 and nbuf>0, buffer is blank filled (no null).

      implicit none
      character*(*) cvar
      integer nchar,buffer(*),nbuf,length,nchar2,i,j,k,m,sizeof_integer

      length=len(cvar)
      nchar2=length
      if (nchar.gt.0) nchar2=nchar
      length=min(length,nchar2)
      if (nbuf.ne.0) nchar2=min(iabs(nbuf)*sizeof_integer()-1,nchar2)

      m=ichar(' ')
      if (nchar.lt.0.and.nbuf.gt.0) then
           do i=1,nbuf*sizeof_integer()
                call char_into_buffer (m,i,buffer)
           end do
      end if

      k=nchar2
      if (nbuf.lt.0) k=1
      do i=1,nchar2
           if (i.le.length) then
                j=ichar(cvar(i:i))
                if (nbuf.lt.0.and.j.ne.m) k=i
           else
                j=m
           end if
           call char_into_buffer (j,i,buffer)
      end do
      if (nchar.lt.0.and.nbuf.gt.0) return
      call char_into_buffer (0,k+1,buffer)
      return
      end




      subroutine convert_hh2cc (buffer,nbuf,   cvar,nchar)
c     copy hollerith buffer to character variable.
c     character variable will be blank filled (no null).
c     buffer might be terminated with null.
c     nchar can be <,=,> length of cvar, or zero.
c     iabs(nbuf) can be the length of the buffer in integers, or zero.

      implicit none
      character*(*) cvar
      integer nchar,buffer(*),nbuf,nchar2,i,j,sizeof_integer

      nchar2=len(cvar)
      if (nchar.gt.0) nchar2=min(nchar,nchar2)
      if (nbuf.ne.0) nchar2=min(iabs(nbuf)*sizeof_integer(),nchar2)

      cvar=' '
      do i=1,nchar2
           call char_from_buffer (buffer,i,j)
           if (j.eq.0) return
           cvar(i:i)=char(j)
      end do
      return
      end




      subroutine convert_to_upper (cvar)
c     convert character variable of any length to upper case.

      implicit none
      character*(*) cvar
      integer length,i,i1,i2,i3,i4

      i1=ichar('a')
      i2=ichar('z')
      i3=ichar('A')
      length=len(cvar)
      do i=1,length
           i4=ichar(cvar(i:i))
           if (i4.ge.i1.and.i4.le.i2) cvar(i:i)=char(i4+i3-i1)
      end do
      return
      end




      subroutine convert_nulls_to_blanks (cvar)
c     convert nulls to blanks in any character variable of any length.

      implicit none
      character*(*) cvar
      integer length,i

      length=len(cvar)
      do i=1,length
           if (ichar(cvar(i:i)).eq.0) cvar(i:i)=' '
      end do
      return
      end




      subroutine convert_squeeze_blanks (cvar)
c     squeezes out blanks in any character variable of any length.

      implicit none
      character*(*) cvar
      character*1 single
      integer length,i,j

      length=len(cvar)
      j=0
      do i=1,length
           single=cvar(i:i)
           if (single.ne.' ') then
                j=j+1
                cvar(j:j)=single
           end if
      end do
      if (j.lt.length) cvar(j+1:length)=' '
      return
      end




      subroutine convert_right_justify (cvar,nchar)
c     right_justifies the first nchar characters in the character variable.
C     nchar can be <,=,> length of cvar.

      implicit none
      integer nchar,nchar2,i
      character*(*) cvar
      CHARACTER*200 BUFFER
C----------DO THE WORK.
      if (cvar.eq.' ') return
      nchar2=len(cvar)
      if (nchar.gt.0) nchar2=min(nchar2,nchar)
      do i=nchar2,1,-1
           if (cvar(i:i).ne.' ') go to 20
      end do
      return
20    if (i.eq.nchar2) return
      buffer=cvar
      cvar=' '
      cvar(1+nchar2-i:nchar2)=buffer
      return
      end

      subroutine convert_enforce_alphanum(cvar,ic)
c     replace any non alpha numeric characters by user specified replacement.
C     nchar can be <,=,> length of cvar.
      implicit none
      integer nchar,nchar2,i,ic,k
      character*(*) cvar
C----------DO THE WORK.
      if (cvar.eq.' ') return
      nchar2=len(cvar)
      do i=1,nchar2
       k = ichar(cvar(i:i))
       if(k.lt.32 .or. k.gt.126) cvar(i:i) = char(ic)
      end do
      return
      end

