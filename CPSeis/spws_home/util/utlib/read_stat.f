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
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                      CPS, ICP, and GWS PRIMITIVE
C
C  Primitive name:  READ_STAT     (read/write a CPS static file from C)
C          Author:  TOM STOECKLEY , 92/01/21
C    Last revised:  92/01/21
C
C  Purpose:         Read, write, add, replace, and delete a CPS static
C                   file.  Also inquire about the existence of a file.
C                   These routines are to be called from C.
C
C  Related documentation:    See READ_CPS_STATFILE.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C ---> To READ an existing CPS static file:
C ---> To WRITE a CPS static file (add or replace):
C ---> To ADD a new CPS static file (replace not allowed):
C ---> To REPLACE an existing CPS static file (add not allowed):
C ---> To DELETE an existing CPS static file:
C ---> To INQUIRE about the existance of a CPS static file:
C
C        read_stat_ (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C       write_stat_ (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C         add_stat_ (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C     replace_stat_ (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,***)
C      delete_stat_ (MSG,FILENAME,EXT,FULL)
C     inquire_stat_ (MSG,FILENAME,EXT,FULL)
C
C     *** = TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD
C
C  Name     IN/OUT  Type  Valid    Description
C  ----     ------  ----  -----    -----------
C  MSG      output  char*          Returns 'OK' if there is no error.
C                                  Otherwise returns an error message.
C                                  Recommend char*80.
C
C  FILENAME input   char*          Name of a CPS static file.
C
C  EXT      input   char*          If not blank, this is an extension
C                                  to be added to the file name if it
C                                  does not already have one.  The
C                                  period should not be included.
C
C  FULL     output  char*          Full expanded file name, including
C                                  disk, directories, extension, and
C                                  version number.  Recommend char*80.
C                                  It is OK for FULL to be the same
C                                  variable as FILENAME.
C
C  NMAX     input   long* >=NX*NY  Dimension of array S.
C  NCMAX    input   long* >=NCARD  Dimension of array CARD.
C
C  TYPE     either  char*  any     Type of static file.
c  NHX      either  long*  1-NWIH  Header word of static X ground pos.
c  NHY      either  long*  0-NWIH  Header word of static Y ground pos.
c  NHX2     either  long*  0-NWIH  Second X header word for S=R file.
c  NHY2     either  long*  0-NWIH  Second Y header word for S=R file.
c  X1       either  float* any     X ground position of first static.
c  Y1       either  float* any     Y ground position of first static.
c  XINC     either  float* not 0   X ground position increment.
c  YINC     either  float* not 0   Y ground position increment.
c  NX       either  long*  >=1     Number of X ground positions.
c  NY       either  long*  >=1     Number of Y ground posns (=1 for 2D).
c  NCARD    either  long*  >=0     Number of comment cards.
c  S        either  long*          Array of NX*NY static values.
c  CARD     either  char*          Array of NCARD 80-byte comment cards.
c
c  NOTE: The character variables will be null-terminated upon input
c        or output, except for the CARD variables.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. See documentation in STATRII for details of the CPS static file
C     format and the following static file parameters:
C     TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD.
C
C  2. See documentation in READ_CPS_STATFILE for other details.  This
C     set of C-callable routines is an interface to the routines in
C     READ_CPS_STATFILE.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 92/01/21  Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     READ_STAT
C  Functions:       none
C  Entry points:    WRITE_STAT   ADD_STAT   REPLACE_STAT   DELETE_STAT
C                   INQUIRE_STAT
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C  READ_CPS_STATFILE      WRITE_CPS_STATFILE      ADD_CPS_STATFILE
C  REPLACE_CPS_STATFILE  DELETE_CPS_STATFILE  INQUIRE_CPS_STATFILE
C-----------------------------------------------------------------------
C\END DOC

      subroutine READ_STAT (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)
      integer msg(*),filename(*),ext(*),full(*),type(*),card(*)
      integer ncardmax,sizeof_integer
      parameter (ncardmax=500)
      character*80 msg1,filename1,ext1,full1,card1(ncardmax)
      character*8 type1

      call hh2cc (filename,0,  filename1,0)
      call hh2cc (ext     ,0,  ext1     ,0)
      call hh2cc (full    ,0,  full1    ,0)
      k=80/sizeof_integer()
      ncmax2=min(ncmax,ncardmax/k)

      call READ_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1,NMAX,NCMAX2,
     $   TYPE1,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD1)

      call convert_cc2hh (type1   ,0,  type     ,0)
      call convert_cc2hh (msg1    ,0,  msg      ,0)
      if (ncard.gt.0) then
        do i=1,ncard
          j=1+(i-1)*k
          call convert_cc2hh (card1(i),0,  card(j),0)
        end do
      end if
      return

      entry write_STAT (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)
      iwhich=1
      go to 10

      entry add_STAT (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)
      iwhich=2
      go to 10

      entry replace_STAT (MSG,FILENAME,EXT,FULL,NMAX,NCMAX,
     $   TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD,S,CARD)
      iwhich=3

10    call hh2cc (filename,0,  filename1,0)
      call hh2cc (ext     ,0,  ext1     ,0)
      call hh2cc (full    ,0,  full1    ,0)
      k=80/sizeof_integer()
      ncmax2=min(ncmax,ncardmax/k)
      ncard2=min(ncard,ncardmax/k)
      if (ncard2.gt.0) then
        do i=1,ncard2
          j=1+(i-1)*k
          call hh2cc (card(j),0,  card1(i),0)
        end do
      end if

      if (iwhich.eq.1) then
       call write_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1,NMAX,NCMAX2,
     $   TYPE1,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD2,S,CARD1)
      else if (iwhich.eq.2) then
       call add_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1,NMAX,NCMAX2,
     $   TYPE1,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD2,S,CARD1)
      else    !  if (iwhich.eq.3) then
       call replace_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1,NMAX,NCMAX2,
     $   TYPE1,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,NCARD2,S,CARD1)
      end if

      call convert_cc2hh (type1   ,0,  type     ,0)
      call convert_cc2hh (msg1    ,0,  msg      ,0)
      if (ncard.gt.0) then
        k=80/sizeof_integer()
        do i=1,ncard
          j=1+(i-1)*k
          call convert_cc2hh (card1(i),0,  card(j)  ,0)
        end do
      end if
      return

      entry delete_STAT (MSG,FILENAME,EXT,FULL)
      call hh2cc (filename,0,  filename1,0)
      call hh2cc (ext     ,0,  ext1     ,0)
      call hh2cc (full    ,0,  full1    ,0)
      call delete_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1)
      call convert_cc2hh (msg1    ,0,  msg      ,0)
      return

      entry inquire_STAT (MSG,FILENAME,EXT,FULL)
      call hh2cc (filename,0,  filename1,0)
      call hh2cc (ext     ,0,  ext1     ,0)
      call hh2cc (full    ,0,  full1    ,0)
      call inquire_CPS_STATFILE (MSG1,FILENAME1,EXT1,FULL1)
      call convert_cc2hh (msg1    ,0,  msg      ,0)
      return
      end

