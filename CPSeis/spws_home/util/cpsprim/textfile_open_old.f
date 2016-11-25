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
C    Primitive name:  TEXTFILE_OPEN_OLD  (text file open/close utilities)
C  Source directory:  primitives/prim_io
C           Library:  conlib
C            Author:  Tom Stoeckley 
C           Written:  92/07/27  by:  Tom Stoeckley
C      Last revised:  93/07/14  by:  Bob Baumel
C
C  Purpose: This is a set of routines to open and close text files,
C           and to inquire whether a file exists.  The purpose is
C           to provide an ANSI-standard interface to Fortran I/O
C           routines, and optionally to Cray routines which fetch
C           and dispose files across the network.  The purpose is to
C           facilitate the use of the same CPS source code both in
C           Cray batch CPS jobs, and in interactive applications (on
C           any computer, including the Cray).
C
C  Related Documentation:  
C-----------------------------------------------------------------------
C    WARNING:  This primitive currently requires different code
C    in Cray batch CPS jobs than in interactive programs.
C               THIS IS THE INTERACTIVE VERSION
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
C  To open an old text file for read:
C  To open a new text file for write:
C                                          o     i      o
C                 CALL TEXTFILE_OPEN_OLD (LUN,FILENAME,IERR)
C                 CALL TEXTFILE_OPEN_NEW (LUN,FILENAME,IERR)
C
C  To close an old text file for read:
C  To close a new text file for write:
C                                           i     i      o
C                 CALL TEXTFILE_CLOSE_OLD (LUN,FILENAME,IERR)
C                 CALL TEXTFILE_CLOSE_NEW (LUN,FILENAME,IERR)
C
C  LUN      = logical unit number.
C  FILENAME = file name (character).
C  IERR     = error return (0 means no error).
C
C  These routines call GETLUN before opening a file.
C  In Cray batch CPS jobs, these routines currently call OPNFIL and
C    CLOSFIL.
C  In interactive applications, these routines currently do Fortran 
C    OPEN and CLOSE calls.  A pre-existing file can be written upon.
C  FILENAME is included in the close argument list because it is
C    needed there when disposing a file to another node.
C  See the notes below for more details and future plans.
C-----------------------------------------------------------------------
C  To inquire whether a file exists:
C                                           i       o     o
C                 CALL TEXTFILE_INQUIRE (FILENAME,IEXIST,IERR)
C
C  FILENAME = file name (character).
C  IEXIST   = status of file (1 means it exists, 0 means it does not).
C  IERR     = error return (0 means no error).
C
C  In Cray batch CPS jobs, this routine currently returns IEXIST=1
C    without checking for existence.
C  In interactive applications, this routine currently does a Fortran 
C    inquire.
C  Eventually, this routine will also check files on other nodes.
C  See the notes below for more details and future plans.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. Currently, there are two variations of these routines, one for
C     Cray batch CPS jobs, and the other for interactive applications
C     (on any computer, including the Cray).  Both variations are in 
C     this file, but one or the other variation is commented out.  Each
C     line of conditional code has one of these appended comments:
C                          ! batch
C                          ! interactive
C
C
C  2. These routines should be used for accessing all text files which 
C     may reside on other nodes.  Similar routines for binary files 
C     may also be provided later.
C
C
C  3. To make the same source code of CPS processes work in both a 
C     batch and interactive mode, the following changes should be made:
C
C      (a) To fetch an existing text file to read, the call to OPNFIL, 
C          and the subsequent CLOSE statement when finished with the 
C          file, should be replaced by TEXTFILE_OPEN_OLD and 
C          TEXTFILE_CLOSE_OLD.
C
C      (b) To write and dispose a new text file, the call to OPEN the 
C          file, and the subsequent call to CLOSFIL, should be replaced 
C          by TEXTFILE_OPEN_NEW and TEXTFILE_CLOSE_NEW.
C
C
C  5. Currently, in the Cray batch CPS system, these routines behave as
C     follows:
C
C     TEXTFILE_OPEN_OLD  calls OPNFIL to fetch a file.
C     TEXTFILE_CLOSE_OLD does a Fortran CLOSE with status DELETE.
C     TEXTFILE_OPEN_NEW  does a Fortran OPEN to create a new local file.
C     TEXTFILE_CLOSE_NEW calls CLOSFIL to dispose a file.
C     TEXTFILE_INQUIRE   returns IEXIST=1 indicating existence,
C                             without actually checking for existence.
C
C     A file name that does not contain a node name is assumed to reside
C     on the node from which the CPS job was submitted.  And if the file
C     name does not contain a directory name, the directory is assumed
C     to be the local directory from which the CPS job was submitted.
C
C
C  6. Currently, in an interactive application (on any computer, including
C     the Cray) (e.g. in CBYT), these routines behave as follows:
C
C     TEXTFILE_OPEN_OLD  does a Fortran OPEN with status OLD.
C     TEXTFILE_CLOSE_OLD does a Fortran CLOSE with status KEEP.
C     TEXTFILE_OPEN_NEW  does a Fortran OPEN with status UNKNOWN.
C     TEXTFILE_CLOSE_NEW does a Fortran CLOSE with status KEEP.
C     TEXTFILE_INQUIRE   does a Fortran INQUIRE and checks on existence.
C
C     A file name that does not contain a node name is assumed to reside
C     on the node where the application is running.  And if the file name
C     does not contain a directory name, the directory is assumed to be
C     the local directory where the application is running.
C
C
C  7. Eventually, in any batch or interactive job, plans are for these 
C     routines to behave as follows:
C
C     If the file name does not contain a node name, the assumptions
C     and behavior will match item 6 above.
C
C     If the file name contains a node name, these routines will use
C     the file transfer utility ftp or rcp, or some other common network 
C     facility.  This will have to wait until CFE puts the node name of 
C     the submitting node into file names before submitting the CPS job.
C     When this is done, different versions of these routines will no 
C     longer have to exist, and more universal flexibility for file 
C     transfers will be available.
C
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 3.  93/07/14 Baumel       Bug fixes to TEXTFILE_OPEN_NEW: Remove
C                             incorrect reference to ierr;  Fix
C                             local file name for Cray batch mode.
C 2.  93/01/12 Stoeckley    Comment out some print statements.
C 1.  92/07/27 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     TEXTFILE_OPEN_OLD      TEXTFILE_CLOSE_OLD
C                   TEXTFILE_OPEN_NEW      TEXTFILE_CLOSE_NEW
C                   TEXTFILE_INQUIRE
C  Functions:       none
C  Entry points:    none
C  Common blocks:   none
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C    In Cray batch jobs:            GETLUN      OPNFIL     CLOSFIL 
C
C    In interactive applications:   GETLUN
C-----------------------------------------------------------------------
C\END DOC


      subroutine textfile_inquire (filename,iexist,ierr)
      implicit none
      character*(*) filename
      integer iexist,ierr
      logical quest

c     PRINT *, 'inquiring about text file ',filename(1:40)
      iexist=0
c     quest=.true.                                  ! batch
      inquire (file=filename,exist=quest,err=999)   ! interactive
      if (quest) iexist=1
      ierr=0
      return
999   print *, 'error inquiring about text file'
      ierr=1
      return
      end




      subroutine textfile_open_old (lun,filename,ierr)
      implicit none
      character*(*) filename
      integer lun,ierr

c     PRINT *, 'opening old text file ',filename(1:40)
      call getlun (LUN,*999)
c     CALL OPNFIL (FILENAME,LUN,*999)                   ! batch
      open (lun,file=filename,status='OLD',err=999)     ! interactive
      ierr=0
      return
999   print *, 'error opening old text file'
      ierr=1
      return
      end




      subroutine textfile_open_new (lun,filename,ierr)
      implicit none
      character*(*) filename
      integer lun,ierr
      character*8 fort_fname

c     PRINT *, 'opening new text file ',filename(1:40)
      call getlun (LUN,*999)
c     write (fort_fname,'(''fort.'',I2,1X)') LUN           ! batch
c     open (lun,file=fort_fname,status='NEW',err=999)      ! batch
      open (lun,file=filename,status='UNKNOWN',err=999)    ! interactive
      ierr=0
      return
999   print *, 'error opening new text file'
      ierr=1
      return
      end





      subroutine textfile_close_old (lun,filename,ierr)
      implicit none
      character*(*) filename
      integer lun,ierr

c     PRINT *, 'closing old text file ',filename(1:40)
c     close (lun,status='DELETE')       ! batch
      close (lun,status='KEEP')         ! interactive
      ierr=0
      return
      end




      subroutine textfile_close_new (lun,filename,ierr)
      implicit none
      character*(*) filename
      integer lun,ierr

c     PRINT *, 'closing new text file ',filename(1:40)
c     CALL CLOSFIL (FILENAME,LUN,*999)    ! batch
      close (lun,status='KEEP')           ! interactive
      ierr=0
      return
999   print *, 'error closing new text file'
      ierr=1
      return
      end

