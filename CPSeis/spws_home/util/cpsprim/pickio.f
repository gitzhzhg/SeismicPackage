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
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                  CONOCO PROCESSING SYSTEM
C                EXPLORATION RESEARCH AND SERVICES
C                         CONOCO INC.
C
C   Primitive name: PICKIO            (SCRS pickfile I/O)
C           Author: Tom Stoeckley
C          Written: 98/07/22
C     Last revised: 98/07/22
C
C   Purpose:  Subroutines to read and write SCRS pickfiles.
C             Made from code in SCRS.CFT.
C             Can be used in batch CPS or on a unix workstation or a VAX.
C
C-----------------------------------------------------------------------
C                        REVISION HISTORY
C      Date    Author    Description
C      ----    ------    ----------
C 2.
C 1.  98/07/22 Stoeckley Initial version.
C
C-----------------------------------------------------------------------
C             CALLING SEQUENCE TO READ A PICKFILE
C
C  integer LUN = Fortran logical unit number to use to read the file.
C
C  Call this only if necessary to learn array sizes to allow allocating
C  before calling PICKIO_READ_HEADER:
C  Like PICKIO_READ_HEADER but does not return arrays:
C
C     CALL PICKIO_READ_SIZES
C    $         (LUN,                                                ! input
C    $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,  ! returned
C    $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY)        ! returned
C
C  Call this to read header information:
C
C     CALL PICKIO_READ_HEADER
C    $         (LUN,                                                ! input
C    $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,  ! returned
C    $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY,        ! returned
C    $          XPW,YPW,PKWN)                                       ! returned
C
C  Call this for each shot profile (1 thru NGRP):
C
C     CALL PICKIO_READ_SHOT
C    $         (LUN,NCH,                              ! input
C    $          SHOT,IARRIV,IXGP,IYGP,IOFF,ELEV)      ! returned
C
C-----------------------------------------------------------------------
C             CALLING SEQUENCE TO WRITE A PICKFILE
C
C  integer LUN = Fortran logical unit number to use to save the file.
C
C  Call this to save header information:
C
C     CALL PICKIO_WRITE_HEADER
C    $         (LUN,                                                 ! input
C    $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,   ! input
C    $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY,         ! input
C    $          XPW,YPW,PKWN)                                        ! input
C
C  Call this for each shot profile (1 thru NGRP):
C
C     CALL PICKIO_WRITE_SHOT
C    $         (LUN,NCH,                             ! input
C    $          SHOT,IARRIV,IXGP,IYGP,IOFF,ELEV)     ! input
C
C-----------------------------------------------------------------------
C               ARGUMENT DEFINITIONS FOR HEADER RECORD
C
C   integer       ngrp     number of groups (shot profiles).
C   integer       itmax    maximum pick time on shot profile (millisec).
C   integer       mxxgp    maximum x ground position.
C   integer       mxygp    maximum y ground position.
C   integer       mnxgp    minimum x ground position.
C   integer       mnygp    minimum y ground position.
C   integer       mxo      maximum offset (ground position units).
C   integer       incgp    ground position increment.
C   real          rinc     receiver interval (feet or meters).
C   integer       nch      number of channels in a shot profile.
C   real          trsh     threshhold amplitude for picking.
C   character*(*) izc      flag for zero-crossing search ('YES' or 'NO').
C   integer       nxpw     number of xpw values (cannot exceed 100).
C   integer       nypw     number of ypw values (cannot exceed 100).
C   integer       npwoff   number of pick window offsets (2 thru 6).
C   integer       ipkwhx   hwd # for pick window x locs (default 9).
C   integer       ipkwhy   hwd # for pick window y locs (default 0).
C
C   real   xpw(nxpw)   pick window x locations
C   real   ypw(nypw)   pick window y locations
C
C   real   pkwn(npwoff * 3 * nxpw * nypw)
C                      pick window functions for each location (XPW,YPW).
C                      Each function consists of (offset,twin,bwin)
C                      for NPWOFF offsets at each location.
C
C-----------------------------------------------------------------------
C               ARGUMENT DEFINITIONS FOR SHOT RECORDS
C
C   real    shot(10)      values relating to shot profile:              
C
C           shot( 1) = source X ground position (hwd 33 or 46).  
C           shot( 2) = hwd 20 (source depth).            
C           shot( 3) = hwd 44 (source uphole time).      
C           shot( 4) = hwd 13 (source elev) - hwd 20.    
C           shot( 5) = not used - should be set to zero.
C           shot( 6) = not used - should be set to zero.
C           shot( 7) = not used - should be set to zero.
C           shot( 8) = source Y ground position (hwd 34 or 0).   
C           shot( 9) = hwd 9 (shot profile number).      
C           shot(10) = not used - should be set to zero.
C
C   integer iarriv(NCH)    pick time on trace (millisec).             
C   integer ixgp  (NCH)    receiver X ground position (hwd 35 or 47).         
C   integer iygp  (NCH)    receiver Y ground position (hwd 36 or  0).         
C   integer ioff  (NCH)    offset of trace (hwd 6) (feet or meters).  
C   integer elev  (NCH)    receiver elevation (hwd 16).               
C
C-----------------------------------------------------------------------
C            SUBROUTINES AND FUNCTIONS IN THIS MODULE
C
C         PICKIO_READ_SIZES
C         PICKIO_READ_HEADER       PICKIO_WRITE_HEADER
C         PICKIO_READ_SHOT         PICKIO_WRITE_SHOT
C
C-----------------------------------------------------------------------
C                        COMMON BLOCKS
C                             none
C
C-----------------------------------------------------------------------
C                       EXTERNALS CALLED
C                             none
C
C-----------------------------------------------------------------------
C             MEMORY REQUIREMENTS WITHIN THIS MODULE
C                             none
C
C-----------------------------------------------------------------------
C\END DOC




ccccccccccccccccccccccc read sizes ccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccc read sizes ccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccc read sizes ccccccccccccccccccccccccccccccccccccc


ccc like PICKIO_READ_HEADER but does not return arrays.
ccc allows learning array sizes to allow allocating before calling
ccc   PICKIO_READ_HEADER.


      SUBROUTINE PICKIO_READ_SIZES
     $         (LUN,                                                ! input
     $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,  ! returned
     $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY)        ! returned

      implicit none
      integer LUN                                           ! input
      integer ngrp,itmax,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP  ! returned
      real    rinc                                          ! returned
      character*(*) IZC                                     ! returned
      integer NCH,TRSH,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY       ! returned

      REWIND LUN
      READ(LUN,5001)NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,
     $   NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY
 5001 FORMAT(8I6,/,F6.1,I6,F6.3,3X,A3,5I4)
      return
      end



ccccccccccccccccccccccc read or write header ccccccccccccccccccccccccccc
ccccccccccccccccccccccc read or write header ccccccccccccccccccccccccccc
ccccccccccccccccccccccc read or write header ccccccccccccccccccccccccccc


ccc call this routine first.


      SUBROUTINE PICKIO_READ_HEADER
     $         (LUN,                                                ! input
     $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,  ! returned
     $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY,        ! returned
     $          XPW,YPW,PKWN)                                       ! returned

      implicit none
      integer       LUN                                           ! input
      integer       ngrp,itmax,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP  ! returned
      real          rinc                                          ! returned
      character*(*) IZC                                           ! returned
      integer       NCH,TRSH,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY       ! returned
      real          XPW(*),YPW(*),PKWN(*)                         ! returned
      integer       i                                             ! local

      REWIND LUN
      READ(LUN,5001)NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,
     $   NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY
      READ(LUN,5005) (XPW(I),I=1,NXPW)
      READ(LUN,5005) (YPW(I),I=1,NYPW)
      READ(LUN,5006) (PKWN(I),I=1,NPWOFF*3*NXPW*NYPW)
 5001 FORMAT(8I6,/,F6.1,I6,F6.3,3X,A3,5I4)
 5005 FORMAT(5(1X,F9.0))
 5006 FORMAT(3(1X,F9.3))
      return
      end



      SUBROUTINE PICKIO_WRITE_HEADER
     $         (LUN,                                                 ! input
     $          NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,   ! input
     $          NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY,         ! input
     $          XPW,YPW,PKWN)                                        ! input

      implicit none
      integer       LUN                                           ! input
      integer       ngrp,itmax,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP  ! input
      real          rinc                                          ! input
      character*(*) IZC                                           ! input
      integer       NCH,TRSH,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY       ! input
      real          XPW(*),YPW(*),PKWN(*)                         ! input
      integer       i                                             ! local

      REWIND LUN
      WRITE(LUN,5001)NGRP,ITMAX,MXXGP,MXYGP,MNXGP,MNYGP,MXO,INCGP,RINC,
     $   NCH,TRSH,IZC,NXPW,NYPW,NPWOFF,IPKWHX,IPKWHY
      WRITE(LUN,5005) (XPW(I),I=1,NXPW)
      WRITE(LUN,5005) (YPW(I),I=1,NYPW)
      WRITE(LUN,5006) (PKWN(I),I=1,NPWOFF*3*NXPW*NYPW)
 5001 FORMAT(8I6,/,F6.1,I6,F6.3,3X,A3,5I4)
 5005 FORMAT(5(1X,F9.0))
 5006 FORMAT(3(1X,F9.3))
      return
      end



ccccccccccccccccccccccc read or write one shot ccccccccccccccccccccccc
ccccccccccccccccccccccc read or write one shot ccccccccccccccccccccccc
ccccccccccccccccccccccc read or write one shot ccccccccccccccccccccccc


ccc call this routine for each shot from 1 thru NGRP.


      SUBROUTINE PICKIO_READ_SHOT
     $         (LUN,NCH,                              ! input
     $          SHOT,IARRIV,IXGP,IYGP,IOFF,ELEV)      ! returned

      implicit none
      integer LUN,nch                                              ! input
      real    shot(10)                                             ! returned
      integer IARRIV(nch),IXGP(nch),IYGP(nch),IOFF(nch),ELEV(nch)  ! returned
      integer i                                                    ! local

      READ(LUN,5002) SHOT
      READ(LUN,5003) (IARRIV(I),IXGP(I),IYGP(I),IOFF(I),ELEV(I),I=1,NCH)
 5002 FORMAT(5F10.3/5F10.3)
 5003 FORMAT(5I6)
      return
      end




      SUBROUTINE PICKIO_WRITE_SHOT
     $         (LUN,NCH,                             ! input
     $          SHOT,IARRIV,IXGP,IYGP,IOFF,ELEV)     ! input

      implicit none
      integer LUN,nch                                              ! input
      real    shot(10)                                             ! input
      integer IARRIV(nch),IXGP(nch),IYGP(nch),IOFF(nch),ELEV(nch)  ! input
      integer i                                                    ! local

      WRITE(LUN,5002) SHOT
      WRITE(LUN,5003)
     $               (IARRIV(I),IXGP(I),IYGP(I),IOFF(I),ELEV(I),I=1,NCH)
 5002 FORMAT(5F10.3/5F10.3)
 5003 FORMAT(5I6)
      return
      end




cccccccccccccccccccccccccccccc end ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccc end ccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccc end ccccccccccccccccccccccccccccccccccc


