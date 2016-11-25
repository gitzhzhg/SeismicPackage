C     SUBROUTINE WUHIST (IPN,IBUFU)
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
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION                    
C                              CONOCO, INC.
C
C  Process name: WUHIST
C        Author: John B Sinton
C  Last revised: 98/05/17  R.S.Day
C
C  Purpose: This routine has three entry points/purposes.
C  1. To write user history cards to a disk file. 
C  2. To read  user history cards from a disk file.
C  3. To rewind a user history disk file.
C                                               
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS
C  **************** NONE **************** 
C-----------------------------------------------------------------------
C  This process is re-enterable, but not stand alone.  It must be used
C  in conjunction with INIT and BHIST. It pulls information from the
C  INITHIS COMMON block which is in the include file init_par.inc
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. See documentation for the CPS process BHIST for information    
C     concerning the CPS/CONSEIS history. 
C  2. The card image passed to WUHIST must be blank filled.  This will
C     conform to the CONSEIS standard history file format.  Also do not
C     use the ':' character or lower case letters.  Check with the 
C     CYBER display code character data set if you are unsure about
C     compatibility.
C  3. On multiprocessor jobs running under MPI, only the root proccessor
C     will perform IO on the files. Currently the root processor=0.
C
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date      Author    Description
C  7. 98/05/17 RS Day     all ipn < 0 sent to %DCODE0. Not just -1
C  6. 98/04/xx RS Day   , Eliminated Cray IO routines. Rewritten so that
C                         it works in MPP jobs. Embedded fortran rewind
C                         in REUHIST for %DCODE0.
C  5. 90/06/07 MS Howard, Change MAXIPN to 199.
C  4. 88/09/26 JB Sinton, NWIH conversion.
C  3. 88/05/04 JB Sinton, Added call to CCSTUC, converts to uppercase.
C  2. 86/09/25 JB Sinton, Added comment on blank filling (see below).
C  1. 86/09/15 JB Sinton, First version. 
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL WUHIST (IPN,IBUFU)    Write a history card.
C        CALL RUHIST (IPN,IBUFU)    Read  a history card.
C        CALL REUHIST(IPN,NCARDS)   Rewind a history file.
C        CALL WUHIST_SETNOHIST(FLAG)
C        CALL WUHIST_NAME(IPN,FNAME)
C                                 
C  IPN     - Process number, integer.
C  IBUFU   - 80 character card to input or output, may be any type,
C            eg. integer, real,..., that can hold character data.
C  NCARDS  - # of 80 character cards in a disk file. If = 0, then 
C            file does not exist or never written to.  This parameter
C            is returned to the calling routine.  Integer.
C  FLAG    - A logical to turn history processing on or off.
C
C-----------------------------------------------------------------------
C                                 NOTES
C  1. These routines assume that the names of all processes used in the
C     job were input to the CPS process INIT.  The names of the pro-
C     cesses must be placed in the HISTORY common block.  If NPROCN is
C     equal to zero (0) then no current history can be generated and
C     WUHIST, RUHIST, and REUHIST will return with no action.
C  2. Card images are written to disk files with names generated from 
C     names of the proceedures.  The IPN parameter identifies the 
C     name to use for the disk file name.  If the name of the procee-
C     dure is TTRIN and its the 10th process then the disk file name 
C     will be '%TTRI10'. If the name is XP and it is the 2nd process 
C     then the disk file name will be '%XP%%%2'.
C  3. The card images must be in ASCII coded form.  Examples follow.
C      Example: CARD = 'THIS IS AN example.'
C       WRITE (CARD,'(10A8)') (IBUF(I),I=1,10)
C       CALL WUHIST (IPN,IBUF)
C      or,
C       CALL WUHIST (IPN,CARD)
C  4. NPROCN = the # of process names in the array PROCN.
C     PROCN  = the names of all processes in this job.
C     NPROCC = a count of the cards written by WUHIST for each process
C              in this job.
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  WUHIST
C  RUHIST
C  REUHIST
C  WUHIST_NAME
C  WUHIST_SETNOHIST
C  WHUHIST_REW_DCODE0
C Functions    : none
C Entrys       :
C Common blocks: /WUHIST_PAR/ 
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C	FLNNC	CCSTUC CHISTWR CHISTRD CHISTREW CMPI_LINE
C  Note- all subroutines are CPS primitives.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C                                       
C  Storage - none
C  Scratch - none 
C  Parms   - none 
C                                           
C-----------------------------------------------------------------------
C\END DOC
CCC
      SUBROUTINE WUHIST(IPN,IBUFU)
      INTEGER IPN,IBUFU(*),IP
      IP=0
      CALL WUHIST_PE(IPN,IBUFU,IP)
      RETURN
      END
CCC
      SUBROUTINE WUHIST_PE(IPN,IBUFU,IP)
      IMPLICIT NONE
      CHARACTER PNAME*8,MSG*80
      INTEGER IPN,IP,IBUFU(*),LUNSO,IERR,NPROCN,PNO
      INTEGER NHRBAD
      INTEGER CMPI_I_PEL,CHISTWR
      LOGICAL NOHIST
      COMMON/WUHIST_PAR/ NOHIST,LUNSO,PNO
      DATA NOHIST/.FALSE./,NHRBAD/0/,LUNSO/6/,PNO/0/
      CALL INIT_PROC_NUM(NPROCN) 
      IF (NPROCN.EQ.0 .AND.IPN.GT.0) THEN
       IF (NOHIST) RETURN 
       MSG= '=>WUHIST; WARNING-no current history can be generated.'
       CALL CMPI_LINE(LUNSO,PNO,MSG)
       MSG= '=>WUHIST; No proceedure name were input to INIT.'
       CALL CMPI_LINE(LUNSO,PNO,MSG)
       NOHIST = .TRUE.
       RETURN
      ENDIF

      IF(CMPI_I_PEL().EQ. PNO) THEN
       IF(IPN.LE.-1) THEN  !all ipn<0 for %DCODE0
        IERR = CHISTWR(IPN,IBUFU)
        RETURN
       ENDIF
       CALL INIT_PROC_CNAME(IPN,PNAME)
       IF (PNAME(1:2).EQ.'  ') THEN         
        NHRBAD = NHRBAD+1
       ENDIF
       IF(IPN.LE.NPROCN) THEN  !ipn > nprocn is an internal process call
        IERR = CHISTWR(IPN,IBUFU)
        IF(IERR.NE.1) THEN
         WRITE(MSG,'("WUHIST: HISTORY WRITE FAILED, IPN=",
     +   I3," ERR#=",I3)') IPN,IERR
         CALL CMPI_LINE(LUNSO,IP,MSG)
         RETURN
        ENDIF
        CALL INIT_ADD_CARD(IPN)
       ENDIF
      ENDIF   !IF(CMPI_I_PEL().EQ. IP) THEN
      RETURN
      END
CCC
      SUBROUTINE WUHIST_COLLECT(IPN,IBUFU)
      IMPLICIT NONE
      include 'cmpif.h'
      INTEGER IPN,IBUFU(*),NPROCN
      INTEGER RANK,NPES,NUM,DEST,I_ERR,N
      INTEGER SIZEOF_REAL,CMPI_I_PEL,CMPI_N_PEL
      INTEGER SOURCE,TAG,RSOURCE,RTAG
      CALL INIT_PROC_NUM(NPROCN) 
      IF(IPN.LT.0 .OR. IPN.GT.NPROCN) RETURN
      RANK = CMPI_I_PEL()
      NPES = CMPI_N_PEL()
      NUM = 80/SIZEOF_REAL()
      TAG=TAG_WUHIST
      IF(RANK.NE.0) THEN
       DEST=0
       CALL CMPI_SEND_I(IBUFU,NUM,DEST,TAG,I_ERR)
      ELSE
       DO N=1,NPES-1
        SOURCE = -1 ! translated to MPI_ANY_SOURCE
        CALL CMPI_RECV_I(IBUFU,NUM,SOURCE,
     1  TAG,RSOURCE,RTAG,I_ERR)
        CALL WUHIST(IPN,IBUFU)
       ENDDO
      ENDIF
      RETURN
      END
CCC
      SUBROUTINE RUHIST (IPN,IBUFU)
      IMPLICIT NONE
      INTEGER IPN,IBUFU(*)
      INTEGER   IERR,IPNAME(4),LUNSO,PNO
      INTEGER    CMPI_I_PEL,CHISTRD
      CHARACTER PNAME*8,MSG*80
      LOGICAL   NOHIST
      COMMON/WUHIST_PAR/ NOHIST,LUNSO,PNO
      IF (NOHIST) RETURN
      IERR=1
      IF(CMPI_I_PEL().EQ.PNO) THEN
       IF(IPN.EQ.-1) THEN  ! for %DCODE0
        IERR =  CHISTRD(IPN,IPNAME,IBUFU)
       ELSE IF (IPN.GE.0 .AND. IPN.LE.199) THEN
        CALL INIT_PROC_CNAME(IPN,PNAME)
        CALL CONVERT_CC2HH(PNAME,0,IPNAME,-2)
        IF (PNAME(1:2).EQ.'  ') THEN         
         CALL INIT_BAD_NEWS()
        ENDIF
        IERR =  CHISTRD(IPN,IPNAME,IBUFU)
       ENDIF
      ENDIF    !IF(CMPI_I_PEL().EQ.PNO) THEN
      IF(IERR.LT.1) THEN
       WRITE(MSG,'("RUHIST: HISTORY READ FAILED, IPN=",
     +  I3," ERR#=",I3)') IPN,IERR
       CALL CMPI_LINE(LUNSO,PNO,MSG)
      ENDIF
      RETURN
      END
CCC
C Rewind stream file associated with ipn, and return
C the current card count. REUHIST is called by DCODES.
      SUBROUTINE REUHIST (IPN,NCARDS)
      IMPLICIT NONE
      CHARACTER PNAME*8
      INTEGER   IPN,LUNSO,NCARDS,I,NPROCN,INIT_CARD_COUNT,PNO
      INTEGER   CMPI_I_PEL
      LOGICAL   NOHIST
      COMMON/WUHIST_PAR/ NOHIST,LUNSO,PNO
      NCARDS=0
      IF(CMPI_I_PEL().EQ.PNO) THEN
       CALL INIT_PROC_NUM(NPROCN)
       IF(IPN.EQ.-1) THEN
        CALL WHUHIST_REW_DCODE0()
        CALL CHISTREW(IPN,NCARDS)
        RETURN
       ELSE IF (IPN.GE.0 .AND. IPN.LE.199) THEN
        CALL INIT_PROC_CNAME(IPN,PNAME)
        IF (NOHIST .OR. INIT_CARD_COUNT(IPN).EQ.0) RETURN
        CALL FLNNC(PNAME,I,32)
        IF (PNAME(1:2).EQ.'  ') THEN         
         CALL INIT_BAD_NEWS()
        ENDIF
        CALL CHISTREW(IPN,NCARDS)
       ENDIF
      ENDIF     !IF(CMPI_I_PEL().EQ.PNO) THEN
      RETURN
      END
CCC
C The file DCODE0 may have been written directly from FORTRAN
C so we will open and rewind it via Fortran to force the buffers
C to flush before c access.
      SUBROUTINE WHUHIST_REW_DCODE0()
      IMPLICIT NONE
      INTEGER LUN(2)
      CHARACTER CIOU*8
      CIOU='%DCODE0'
      CALL CONVERT_CC2HH(CIOU,0,LUN,-2)
      REWIND(LUN(1))
      RETURN
      END
CCC
      SUBROUTINE WUHIST_NAME(IPN,FNAME)
      IMPLICIT NONE
      CHARACTER*(*) FNAME
      CHARACTER PNAME*8
      INTEGER I,IPN,NPROCN
      IF(IPN.LT.0) THEN
       FNAME='%DCODE0'//CHAR(0)
       RETURN
      ENDIF
      FNAME  = '%%%%%%%'//CHAR(0)
      CALL INIT_PROC_NUM(NPROCN)
      IF(NPROCN.LE.0) RETURN
      CALL INIT_PROC_CNAME(IPN,PNAME)
      IF(PNAME(1:2).EQ.'  ') RETURN
      FNAME  = '%'//PNAME(1:7)
      CALL FLNBC(FNAME,I,37)
      IF(IPN.LT.10) THEN
       WRITE(FNAME(7:7),'(I1)') IPN
      ELSE IF(IPN.LE.99) THEN
       WRITE(FNAME(6:7),'(I2)') IPN
      ELSE
       WRITE(FNAME(5:7),'(I3)') IPN
      ENDIF
      FNAME(8:8)=CHAR(0)
      RETURN
      END
CCC
      SUBROUTINE WUHIST_SETNOHIST(FLAG)
      IMPLICIT NONE
      INTEGER   LUNSO,PNO
      LOGICAL   NOHIST,FLAG
      COMMON/WUHIST_PAR/ NOHIST,LUNSO,PNO
      NOHIST = FLAG
      RETURN
      END
