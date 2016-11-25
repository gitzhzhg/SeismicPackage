C\USER DOC
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
C-----------------------------------------------------------------------
C                       C P S   P R I M I T I V E
C                             fortran code
C                  designed to be called from fortran
C
C    Primitive name:  DSKIOF_* 
C  Source directory:  tfio
C           Written:  97/11/26  by:  R.S. Day
C      Last revised:  99/10/07       Day
C           Purpose:  Fortran wrappers to routines that open IO channels
C                     to a variety of IO types on a variety of computer
C                     architectures.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C
C     SUBROUTINE DSKIOF_XOP(IOTYPE, UFI, NAME, OFLAG, I_ERR)
C       - Open a file with a particular iotype
C     SUBROUTINE DSKIOF_XOPB(IOTYPE, UFI, NAME, OFLAG, 
C    x   PAGE_SIZE, NUM_PAGES,I_ERR)
C       - Open a file for a particular iotype.
C       - page_size controls buffer or record lengths when relevant.
C       - A superset of dskiof_xop
C     SUBROUTINE DSKIOF_XOPC(IOTYPE, UFI, NAME, OFLAG, 
C    x   PAGE_SIZE, NUM_PAGES,RECL,IWRD,I_ERR)
C       - Open a file for a particular iotype.
C       - superset of xopb but lets user control record lengths
C         when iotype=5.
C     INTEGER FUNCTION DSKIOF_RM(FILE)
C       - remove FILE (a wrapper for dskiorm_)
C     SUBROUTINE DSKIOF_GET_FNAME(UFI,FNAME)
C       - return file name for file index UFI
C     INTEGER FUNCTION DSKIO_CHAIN_IOTYPE(UFI) (see dskio.c)
C       - return the IOTYPE for file index UFI
C     INTEGER FUNCTION DSKIO_CHAIN_RECL(UFI) (see dskio.c)
C       - return the RECORD LENGTH for file index UFI
C     SUBROUTINE DSKIOF_GET_LUN(LUN,I_ERR)
C       - return 1st available fortran unit number
C     INTEGER FUNCTION DSKIOF_IS_LOCAL(FNAME,MUST_EXIST)
C       - check if a file is a local(return value=1) file
C         or not(return value=0)
C     SUBROUTINE DSKIOF_NETINFO(RNODE,RUSER,RPATH)
C       - checke _netinfo file for node, userid, and path defaults
C     SUBROUTINE DSKIOF_PARSE_FULLX1(FULL,U,N,F,BF,MODNAME)
C       - parses full file name into its components and uses
C         _netinfo file to replace missing node,user, path data
C     INTEGER FUNCTION DSKIOF_NETNAME(FNAME,FULLNAME)
C       - builds a qualified network name
C     INTEGER FUNCTION DSKIOF_HOST(HOST)
C       - gets host name and returns the number of characters.
C         No characters indicates failure.
C     INTEGER FUNCTION DSKIOF_CWD(CWD)
C       - gets the current working directory name and returns
C         the number of characters
C     INTEGER FUNCTION DSKIOF_ENVUSER(ENVUSER)
C       - gets the value of the environment variable USER and 
C         returns the number of characters
C     INTEGER FUNCTION  DSKIOF_HOST_TO_DISK(HOST, DISK)
C       - given the hostname HOST return a preferred path
C         prefix in the variable DISK. The return value is the
C         number of non blank character returned in DISK
C     INTEGER FUNCTION  DSKIOF_DISK_TO_HOST(M,HOST, DISK)
C       - given a DISK prefix name, return a preferred list
C         of hosts in the character array HOST. M is the
C         maximum no. of entries allowed for HOST. The return
C         value is the number of entries set in HOST
C     SUBROUTINE DSKIOF_OPTIMAL_PATH(PATH,TMPORPTMP,HOST,
C    x  NBLK,BYPERBLK)
C       - return the disk path that is "optimal" for host
C         TMPORPTMP = "PERMANENT","TEMPORARY","BOTH"
C         nblk*byerblk = desired file size in blocks
C
C IOTYPE    Integer to choose io style
C           0 - low level c(unbuffered)
C           1 - ffio (Crays)
C           2 - sio  (Cray t3e)
C           3 - aqio (Crays)
C           4 - stream io in c(buffered)
C           5 - Fortran Direct Access
C  UFI      Is a an integer that is a Unique File Index.
C NAME      The input file name
C OFLAG     open mode flag (see dskio_nrw() for instance)
C           ( irrelevant for AQIO )
C I_ERR     The return error status(-1= failure, 0=OK)
C PAGE_SIZE When IOTYPE=1 or 4, PAGE_SIZE is a buffer size.
C           This will be rounded to a multiple of 4096 for FFIO type IO.
C           The default for XOP is 1048576 on the Crays and 16384 on
C           workstations.
C NUM_PAGES Number of pages per PE. FFIO is the only IO type to use
C           this. The default value used by XOP on the Crays is 12.
C RECL      Used as a record length When IOTYPE=5.
C IWRD      RECL unit flag. WRD=0 RECL is bytes, WRD=1 RECL is words.
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. The preferred call to open a file is DSKIOF_XOPC, as it has the
C     most control over setting up parameters like buffer sizes and
C     record lengths.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C11. 99/10/07   R.Day      Added function DSKIOF_OPTIMAL_PATH
C10. 99/09/23   R.Day      Added the function DSKIOF_HOST_TO_DISK
C                          and DSKIOF_DISK_TO_HOST
C 9. 99/05/24   R.Day      Updating Conlib
C 8. 99/05/04   R.Day      Uncommented a call to AQOPEN that should not
C                          have been.
C 7. 99/05/03   R. Day     Changed name dskio_rinw to dskiof_rinw
C 6. 99/04/29   R.Day      Added dskiof_is_local,dskiof_host,
C                          dskiof_netinfo, dskiof_parse_fullx1
C                          dskiof_envuser, dskiof_cwd, dskiof_exist
C 5. 99/04/01   R.Day      Added new methods -
C                          dskiof_get_lun, dskiof_fdop, dskiof_fdacl,
C                          dskiof_fdawr, dskiof_fdard, dskio_chain_recl,
C                          dskio_chain_iotype, dskio_get_fname.
C                          dskiof_rm, dskiof_xopc
C                          Mostly to support fortran direct access IO.
C 4. 99/03/08   R.Day      Added DSKIO_AQOP for opening AQIO files.
C                          and eliminated some unecessary code.
C 3. 99/02/26   R.Day      IOTYPE=4 documented. Added new call DSKIO_XOPB
C                          that lets user set buffer sizes.
C 2. 98/09/15   R.Day      Added fortran wrappers to some parsing routines
C                          that were in C.
C 1. 97/12/12   R.Day      Fixed aqopen problem and added get_machine call
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C           LIST OF SUBROUTINE, FUNCTION, ENTRY, COMMON, INCLUDE  NAMES
C
C  Subroutines: 14
C   DSKIOF_AQOP              DSKIOF_BLD_FULL          DSKIOF_FDACL            
C   DSKIOF_FDAOP             DSKIOF_FDARD             DSKIOF_FDAWR            
C   DSKIOF_GET_FNAME         DSKIOF_GET_LUN           DSKIOF_NETINFO          
C   DSKIOF_PARSE_FULL        DSKIOF_PARSE_FULLX1      DSKIOF_XOP              
C   DSKIOF_XOPB              DSKIOF_XOPC             
C  Functions: 8
C   INTEGER DSKIOF_RM        INTEGER DSKIOF_NODE      INTEGER DSKIOF_USERID   
C   INTEGER DSKIOF_FILE      INTEGER DSKIOF_BARE_FILE INTEGER DSKIOF_PATH     
C   INTEGER DSKIOF_PTMP      INTEGER DSKIOF_IS_LOCAL 
C-----------------------------------------------------------------------
C           LIST OF ALL CALLS MADE BY THIS MODULE
C
C  Calls:11
C   AQOPEN                   CONVERT_CC2HH            CONVERT_HH2CC           
C   DSKIOF_GET_FNAME         DSKIOF_GET_LUN           DSKIO_BLD_FULL          
C   DSKIO_CHAIN_FNAME        DSKIO_PARSE_FULL         DSKIO_XOP               
C   DSKIO_XOPB               DSKIO_XOPC              
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C The routine convert_cc2hh makes use of 
C   char_into_buffer  char_from_buffer   sizeof_integer get_machine
C which are in:
C  1) char_into_buffer.c
C  2) sizeof_real.f
C  3) sizeof_string.c
C  4) convert_cc2hh.f
C-----------------------------------------------------------------------
C\END DOC
C<VERIFICATION_CODE>
C     CHARACTER CWD*200,HOST*80,DISK*80,PATH*80
C     CHARACTER TMPORPTMP*8
C     INTEGER   I,NBLK,BYPERBLK
C     INTEGER DSKIOF_HOST,DSKIOF_CWD, DSKIOF_HOST_TO_DISK
C     I = DSKIOF_HOST(HOST)
C     WRITE(6,'("HOST=",A)') HOST(1:32)
C     I = DSKIOF_CWD(CWD)
C     WRITE(6,'("CWD=",A)') CWD(1:64)
C     I =  DSKIOF_HOST_TO_DISK(HOST, DISK)
C     WRITE(6,'("DISK=",A)') DISK(1:64)
C
C     TMPORPTMP='TMP'
C     BYPERBLK=256
C     DO I=1,5
C      NBLK=100000
C      CALL DSKIOF_OPTIMAL_PATH(DISK,TMPORPTMP,HOST,
C    x  NBLK,BYPERBLK)
C      BYPERBLK=BYPERBLK*4
C      WRITE(6,'("OPT PATH=",A,1X,I6)') DISK(1:64),BYPERBLK
C     ENDDO
C
C     STOP
C     END
C</VERIFICATION_CODE>
CCC
      SUBROUTINE DSKIOF_XOP(IOTYPE, UFI, NAME, OFLAG, I_ERR)
      IMPLICIT NONE
      INTEGER  IOTYPE, UFI, OFLAG, I_ERR, INAME(40)
      CHARACTER*(*) NAME
      CALL CONVERT_CC2HH(NAME,0,INAME,-40)
      CALL DSKIO_XOP(IOTYPE,UFI,INAME,OFLAG,I_ERR)
      RETURN
      END
CCC
C When IOTYPE=1 or 4 PAGE_SIZE is a buffer size
      SUBROUTINE DSKIOF_XOPB(IOTYPE, UFI, NAME, OFLAG, 
     x   PAGE_SIZE, NUM_PAGES,I_ERR)
      IMPLICIT NONE
      INTEGER  IOTYPE, UFI, OFLAG, I_ERR, INAME(40)
      INTEGER  PAGE_SIZE,NUM_PAGES
      CHARACTER*(*) NAME
      CALL CONVERT_CC2HH(NAME,0,INAME,-40)
      CALL DSKIO_XOPB(IOTYPE,UFI,INAME,OFLAG,I_ERR,
     x  PAGE_SIZE,NUM_PAGES)
      RETURN
      END
CCC
      SUBROUTINE DSKIOF_XOPC(IOTYPE, UFI, NAME, OFLAG, 
     x   PAGE_SIZE, NUM_PAGES,RECL,IWRD,I_ERR)
      IMPLICIT NONE
      INTEGER  IOTYPE, UFI, OFLAG, I_ERR, INAME(40)
      INTEGER  PAGE_SIZE,NUM_PAGES,RECL,IWRD
      CHARACTER*(*) NAME
      CALL CONVERT_CC2HH(NAME,0,INAME,-40)
      CALL DSKIO_XOPC(IOTYPE,UFI,INAME,OFLAG,I_ERR,
     x  PAGE_SIZE,NUM_PAGES,RECL,IWRD)
      RETURN
      END
C
CCC DSKIO_XOP calls this since AQOPEN treats calls from
C   C and Fortran differently.
      SUBROUTINE DSKIOF_AQOP(FD,INAME,I_ERR)
      IMPLICIT NONE
      INTEGER  FD(*),INAME(*),I_ERR,N,MACH
      INTEGER  GET_MACHINE
      CHARACTER NAME*120
      MACH = GET_MACHINE()
      CALL CONVERT_HH2CC(INAME,0,NAME,0)
      IF(MACH.EQ.1 .OR. MACH.EQ.8) THEN
        N=56
C       CALL AQOPEN(FD,N,NAME,I_ERR)
      ELSE
        I_ERR=1
      ENDIF
      RETURN
      END
CCC
      SUBROUTINE DSKIOF_FDAOP(FD,INAME,ISTATUS,
     x   REC_SIZ_BYTES,I_ERR)
      IMPLICIT  NONE
      INTEGER   FD,INAME(*),ISTATUS(*),REC_SIZ_BYTES
      INTEGER   I_ERR,MACH,L_REC
      INTEGER   GET_MACHINE,DSKIO_SIZ_REAL
      REAL      A(2)
      CHARACTER NAME*120,STATUS*24

      I_ERR=-1
      FD=-1
      IF(REC_SIZ_BYTES.LE.0) THEN
       RETURN
      ENDIF
      CALL CONVERT_HH2CC(INAME,0,NAME,0)
      CALL CONVERT_HH2CC(ISTATUS,0,STATUS,0)

      CALL DSKIOF_GET_LUN(FD,I_ERR)
      IF (I_ERR .NE. 0) GOTO 998

      MACH = GET_MACHINE()
      IF (MACH .NE. 2) THEN
            L_REC = REC_SIZ_BYTES
      ELSE    ! IF (GET_MACHINE() .NE. 2) THEN
            L_REC = REC_SIZ_BYTES/DSKIO_SIZ_REAL(A(1),A(2))
      ENDIF    ! IF (GET_MACHINE() .NE. 2) THEN

      OPEN(FD,FILE=NAME,STATUS=STATUS,FORM='UNFORMATTED'
     1,ACCESS='DIRECT',RECL=L_REC,ERR=997)

      RETURN
  997 CONTINUE
      WRITE(6
     1,'(/,'' ERROR IN DSKIO_FDAOP'')')
      GOTO 999
 
  998 CONTINUE
      WRITE(6
     1,'(/,'' ERROR IN DSKIO_FDAOP GETTING UNIT NUMBER'')')
      GOTO 999
  999 CONTINUE
      WRITE(6
     1,'(/,'' descriptor    ='',i5
     1,/,'' file          ='',a
     1,/,'' status        ='',a
     1,/,'' rec_siz_bytes ='',i8
     1,/,'' get_machine   ='',i8
     1,/,'' sizeof_real   ='',i8
     1)')
     1 FD
     1,NAME
     1,STATUS
     1,rec_siz_bytes
     1,get_machine()
     1,dskio_siz_real(A(1),A(2))
      i_err = -1

      RETURN
      END
CCC
C  close a fortran file
      SUBROUTINE DSKIOF_FDACL(UFI)
      IMPLICIT  NONE
      INTEGER   FD,UFI,DSKIO_CHAIN_HANDLE
      IF(UFI.LT.0) RETURN
      FD=DSKIO_CHAIN_HANDLE(UFI)
      IF(FD.LT.1) RETURN
      CLOSE(FD)
      RETURN
      END
CCC
C FD   = Channel number
C RECL = record length in bytes
C OFF  = File offset in 8bit bytes. Starts from 0 and must
C        be a multiple of RECL-1.
C NBY  = Number of bytes to transfer(must be a multiple of a real)
C I_ERR= Error flag=0 if there are no errors
      SUBROUTINE DSKIOF_FDARD(UFI,OFF,NBY,DATA,I_ERR)
      IMPLICIT  NONE
      REAL    DATA(*)
      INTEGER UFI,FD,RECL,OFF,NBY,I_DATA,N_DATA,I_ERR
      INTEGER I_REC_1,I_REC_2,J_DATA_1,J_DATA_2,W_RECL
      INTEGER M_DATA,I_REC,J_DATA,N_REC,SR
      INTEGER DSKIOF_RINW,DSKIO_SIZ_REAL
      INTEGER DSKIO_CHAIN_RECL,DSKIO_CHAIN_HANDLE
      CHARACTER FNAME*96
      REAL    A(2)
      SR=DSKIO_SIZ_REAL(A(1),A(2))

      I_ERR=1
      IF(UFI.LE.0) RETURN
      FD=DSKIO_CHAIN_HANDLE(UFI)
      RECL=DSKIO_CHAIN_RECL(UFI)
      IF(FD.LE.0) RETURN
      IF(RECL.LE.0) RETURN
      IF(OFF.LT.0) RETURN
      IF(RECL.LT.NBY) RETURN
      
      I_DATA = OFF/RECL + 1 ! 1st record
      IF((I_DATA-1)*RECL .NE.OFF) THEN
       I_ERR=2
       WRITE(6,*) 'DSKIO_FDARD: OFF=',OFF,' NOT A RECORD MULTIPLE'
       WRITE(6,*) 'DSKIO_FDARD: RECL=',RECL
       RETURN
      ENDIF
      N_DATA = NBY/SR       !No. of words to transfer
      IF(N_DATA*SR.NE.NBY) THEN
       I_ERR=2
       WRITE(6,*) 'DSKIO_FDARD: NBY=',NBY,' NOT A WORD MULTIPLE'
       RETURN
      ENDIF
      W_RECL = RECL/SR
      M_DATA = MIN(W_RECL,N_DATA)
      N_REC = DSKIOF_RINW(N_DATA,M_DATA)

      I_REC_1 = (I_DATA - 1) * N_REC + 1
      I_REC_2 = I_REC_1 + N_REC - 1
      DO I_REC = I_REC_1 , I_REC_2

         J_DATA_1 = (I_REC - I_REC_1) * M_DATA + 1
         J_DATA_2 = MIN(N_DATA,J_DATA_1+M_DATA-1)

C        READ(FD,REC=I_REC)
         READ(FD,REC=I_REC,ERR=997)
     x   (DATA(J_DATA),J_DATA=J_DATA_1,J_DATA_2)
      ENDDO

      I_ERR=0
      RETURN
 997  CONTINUE
      I_ERR=-1
      CALL DSKIOF_GET_FNAME(UFI,FNAME)
      WRITE(6
     1,'(/,'' ERROR in DSKIO_FDARD '',A
     1,/,'' I_REC='',I8,'' N_REC='',I8
     1,/,'' I_REC_1='',I8,'' I_REC_2='',I8
     1,/,'' J_DATA_1='',I8,'' J_DATA_2='',I8
     1,/,'' RECORD LENGTH='',I8
     1,/,'' DESCRIPTOR='',I8
     1)')
     1 FNAME,I_REC,N_REC
     1,I_REC_1,I_REC_2
     1,J_DATA_1,J_DATA_2,RECL,FD

      RETURN
      END
CCC
C FD   = Channel number
C RECL = record length in bytes
C OFF  = File offset in 8bit bytes. Starts from 0 and must
C        be a multiple of RECL-1.
C NBY  = Number of bytes to transfer(must be a multiple of a real)
C I_ERR= Error flag=0 if there are no errors
      SUBROUTINE DSKIOF_FDAWR(UFI,OFF,NBY,DATA,I_ERR)
      IMPLICIT  NONE
      REAL    DATA(*)
      INTEGER UFI,FD,RECL,OFF,NBY,I_DATA,N_DATA,I_ERR
      INTEGER I_REC_1,I_REC_2,J_DATA_1,J_DATA_2,W_RECL
      INTEGER M_DATA,I_REC,J_DATA,N_REC,SR
      INTEGER DSKIOF_RINW,DSKIO_SIZ_REAL
      INTEGER DSKIO_CHAIN_RECL,DSKIO_CHAIN_HANDLE
      CHARACTER FNAME*96
      REAL    A(2)
      SR=DSKIO_SIZ_REAL(A(1),A(2))

      I_ERR=1
      IF(UFI.LE.0) RETURN
      FD=DSKIO_CHAIN_HANDLE(UFI)
      RECL=DSKIO_CHAIN_RECL(UFI)
      IF(FD.LE.0) RETURN
      IF(RECL.LE.0) RETURN
      IF(OFF.LT.0) RETURN

      I_DATA = OFF/RECL + 1
      IF((I_DATA-1)*RECL .NE.OFF) THEN
       I_ERR=2
       WRITE(6,*) 'DSKIO_FDAWR: OFF=',OFF,' NOT A RECORD MULTIPLE'
       WRITE(6,*) 'DSKIO_FDAWR: RECL=',RECL
       RETURN
      ENDIF
      N_DATA = NBY/SR       !No. of words to transfer
      IF(N_DATA*SR.NE.NBY) THEN
       I_ERR=2
       WRITE(6,*) 'DSKIO_FDAWR: NBY=',NBY,' NOT A WORD MULTIPLE'
       RETURN
      ENDIF
 
      W_RECL = RECL/SR
      M_DATA = MIN(W_RECL,N_DATA)
      N_REC = DSKIOF_RINW(N_DATA,M_DATA)

      I_REC_1 = (I_DATA - 1) * N_REC + 1
      I_REC_2 = I_REC_1 + N_REC - 1
      DO I_REC = I_REC_1 , I_REC_2

         J_DATA_1 = (I_REC - I_REC_1) * M_DATA + 1
         J_DATA_2 = MIN(N_DATA,J_DATA_1+M_DATA-1)

C        WRITE(FD,REC=I_REC)
         WRITE(FD,REC=I_REC,ERR=997)
     1 (DATA(J_DATA),J_DATA=J_DATA_1,J_DATA_2)

      ENDDO

      RETURN
 997  CONTINUE
      I_ERR=-1
      CALL DSKIOF_GET_FNAME(UFI,FNAME)
      WRITE(6
     1,'(/,'' ERROR in DSKIO_FDAWR '',A
     1,/,'' I_REC='',I8,'' N_REC='',I8
     1,/,'' I_REC_1='',I8,'' I_REC_2='',I8
     1,/,'' J_DATA_1='',I8,'' J_DATA_2='',I8
     1,/,'' RECORD LENGTH='',I8
     1,/,'' DESCRIPTOR='',I8
     1)')
     1 FNAME,I_REC,N_REC
     1,I_REC_1,I_REC_2
     1,J_DATA_1,J_DATA_2,RECL,FD

      RETURN
      END
CCC
      INTEGER   FUNCTION DSKIOF_RINW(N,M)
C  number of records length m in n words
      IMPLICIT  NONE
      INTEGER   N,M
      IF (M .EQ. 0) THEN
        DSKIOF_RINW = 1
      ELSE    ! IF (M .EQ. 0) THEN
        DSKIOF_RINW = (N - 1) / M + 1
      ENDIF    ! IF (M .EQ. 0) THEN
      IF (N .EQ. 0) DSKIOF_RINW = 0
      RETURN
      END
CCC
      SUBROUTINE DSKIOF_GET_LUN(LUN,I_ERR)
      IMPLICIT  NONE
      INTEGER   LUN,I,I_ERR
      LOGICAL QUEST
      INTEGER GET_MACHINE                           ! added for HP 94/11/29
      INTEGER LSTART,LSTOP,FIRST,LOCAL_IERR
      PARAMETER (LSTART=20,LSTOP=99)
      INTEGER LAST,LIST(LSTOP)
      SAVE    LAST,LIST
      DATA    LAST,LIST/LSTOP,LSTOP*0/

      I_ERR=0
      LUN=-1
      DO I=LSTART,LSTOP
           LAST=LAST+1
           IF (LAST.GT.LSTOP) LAST=LSTART
           IF (I.EQ.LSTART) FIRST=LAST
           IF (GET_MACHINE().EQ.6) THEN             ! added for HP 94/11/29
                INQUIRE (LAST,OPENED=QUEST,ERR=999) ! added for HP 95/01/10
           ELSE                                     ! added for HP 94/11/29
                INQUIRE (LAST,OPENED=QUEST,IOSTAT=LOCAL_IERR)
                IF(LOCAL_IERR.NE.0) GOTO 999
           END IF                                   ! added for HP 94/11/29
           IF (.NOT.QUEST) THEN
                LIST(LAST)=1
                LUN=LAST
                RETURN
           END IF
      END DO

999   CONTINUE
      I_ERR= -1
      WRITE(6,*) 'DSKIOF_GET_LUN FAILED, lun=',FIRST,' thru ',last
      RETURN
      END
CCC
      SUBROUTINE DSKIOF_GET_FNAME(UFI,FNAME)
      IMPLICIT NONE
      INTEGER UFI,INAME(40)
      CHARACTER*(*) FNAME   
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      CALL DSKIO_CHAIN_FNAME(UFI,INAME)
      CALL CONVERT_HH2CC(INAME,0,FNAME,0)
      RETURN
      END
CCC
C - A wrapper for the c routine dskiorm which removes a file
      INTEGER FUNCTION DSKIOF_RM(FNAME)
      IMPLICIT NONE
      INTEGER DSKIORM,INAME(30)
      CHARACTER*(*) FNAME
      CALL CONVERT_CC2HH(FNAME,0,INAME,-30)
      DSKIOF_RM=DSKIORM(INAME)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_node
      INTEGER FUNCTION  DSKIOF_NODE(FNAME,NODE)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,NODE
      INTEGER       INAME(40),INODE(20),DSKIO_NODE
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_NODE=DSKIO_NODE(INAME,INODE)
      CALL CONVERT_HH2CC(INODE,0,NODE,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_userid
      INTEGER FUNCTION  DSKIOF_USERID(FNAME,USERID)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,USERID
      INTEGER       INAME(40),IUSERID(4),DSKIO_USERID
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_USERID=DSKIO_USERID(INAME,IUSERID)
      CALL CONVERT_HH2CC(IUSERID,0,USERID,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_file
      INTEGER FUNCTION  DSKIOF_FILE(FNAME, FILE)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,FILE
      INTEGER       INAME(40),IFILE(40),DSKIO_FILE
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_FILE=DSKIO_FILE(INAME,IFILE)
      CALL CONVERT_HH2CC(IFILE,0,FILE,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_bare_file
      INTEGER FUNCTION  DSKIOF_BARE_FILE(FNAME, BFILE)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,BFILE
      INTEGER       INAME(40),IBFILE(40),DSKIO_BARE_FILE
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_BARE_FILE=DSKIO_BARE_FILE(INAME,IBFILE)
      CALL CONVERT_HH2CC(IBFILE,0,BFILE,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_path
      INTEGER FUNCTION  DSKIOF_PATH(FNAME, PATH)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,PATH
      INTEGER       INAME(40),IPATH(40),DSKIO_PATH
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_PATH=DSKIO_PATH(INAME,IPATH)
      CALL CONVERT_HH2CC(IPATH,0,PATH,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_ptmp
      INTEGER FUNCTION  DSKIOF_PTMP(PTMP)
      IMPLICIT NONE
      CHARACTER*(*) PTMP
      INTEGER       IPTMP(20),DSKIO_PTMP
      DSKIOF_PTMP=DSKIO_PTMP(IPTMP)
      CALL CONVERT_HH2CC(IPTMP,0,PTMP,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_bld_full
C                                 O   I I I I
      SUBROUTINE DSKIOF_BLD_FULL(FULL,U,N,P,BF)
      IMPLICIT NONE
      CHARACTER*(*) FULL,U,N,P,BF
      INTEGER       IFULL(40),IN(10),IU(10),IP(20),IBF(8)
      CALL CONVERT_CC2HH(U,0,IU,-10)
      CALL CONVERT_CC2HH(N,0,IN,-10)
      CALL CONVERT_CC2HH(P,0,IP,-20)
      CALL CONVERT_CC2HH(BF,0,IBF,-8)
      CALL DSKIO_BLD_FULL(IFULL,IU,IN,IP,IBF)
      CALL CONVERT_HH2CC(IFULL,0,FULL,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_parse_full
C                                  I    O O O O
      SUBROUTINE DSKIOF_PARSE_FULL(FULL,U,N,P,BF)
C A wrapper for the c routine dskio_parse_full
      IMPLICIT NONE
      CHARACTER*(*) FULL,U,N,P,BF
      INTEGER       IFULL(40),IN(10),IU(10),IP(20),IBF(8)
      CALL CONVERT_CC2HH(FULL,0,IFULL,-40)
      CALL DSKIO_PARSE_FULL(IFULL,IU,IN,IP,IBF)
      CALL CONVERT_HH2CC(IU,0,U,0)
      CALL CONVERT_HH2CC(IN,0,N,0)
      CALL CONVERT_HH2CC(IP,0,P,0)
      CALL CONVERT_HH2CC(IBF,0,BF,0)
      RETURN
      END
C A wrapper for the c routine dskio_parse_fullx1
C                                  I    O O O O O   O
      SUBROUTINE DSKIOF_PARSE_FULLX1(FULL,U,N,F,BF,MODNAME)
C A wrapper for the c routine dskio_parse_fullx1
      IMPLICIT NONE
      CHARACTER*(*) FULL,U,N,F,BF,MODNAME
      CHARACTER      NINFO*16
      INTEGER       IINFO(4),IMODNAME(40)
      INTEGER       I,DSKIO_PARSE_FULLX1
      INTEGER       IFULL(40),IN(10),IU(10),IF(20),IBF(8)
      NINFO='_netinfo'
      CALL CONVERT_CC2HH(NINFO,0,IINFO,-4)
      CALL CONVERT_CC2HH(FULL,0,IFULL,-40)
      I =  DSKIO_PARSE_FULLX1(IFULL,IINFO,IN,IU,IF,IBF,IMODNAME)
      CALL CONVERT_HH2CC(IU,0,U,0)
      CALL CONVERT_HH2CC(IN,0,N,0)
      CALL CONVERT_HH2CC(IF,0,F,0)
      CALL CONVERT_HH2CC(IBF,0,BF,0)
      CALL CONVERT_HH2CC(IMODNAME,0,MODNAME,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_netname
C                                     I     O
      INTEGER FUNCTION DSKIOF_NETNAME(FNAME,FULLNAME)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,FULLNAME
      INTEGER       DSKIO_NETNAME,IFNAME(40),IFULL(40)
      CALL CONVERT_CC2HH(FNAME,0,IFNAME,-40)
      DSKIOF_NETNAME=DSKIO_NETNAME(IFNAME,IFULL)
      CALL CONVERT_HH2CC(IFULL,0,FULLNAME,0)
      RETURN
      END
C A wrapper for the c routine dskio_is_local
      INTEGER FUNCTION DSKIOF_IS_LOCAL(FNAME,MUST_EXIST)
      IMPLICIT NONE
      CHARACTER*(*) FNAME
      INTEGER       MUST_EXIST,INAME(40),DSKIO_IS_LOCAL
      DSKIOF_IS_LOCAL=0
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_IS_LOCAL = DSKIO_IS_LOCAL(INAME,MUST_EXIST)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_netinfo
      SUBROUTINE DSKIOF_NETINFO(RNODE,RUSER,RPATH)
      IMPLICIT NONE
      CHARACTER *(*) RNODE,RUSER,RPATH
      CHARACTER      NINFO*16
      INTEGER        IINFO(4),INODE(20),IUSER(20),IPATH(40)
      INTEGER        DSKIO_NETINFO,I
      INODE(1)=0
      IUSER(1)=0
      IPATH(1)=0
      NINFO='_netinfo'
      CALL CONVERT_CC2HH(NINFO,0,IINFO,-4)
      I = DSKIO_NETINFO(IINFO,INODE,IUSER,IPATH)
      CALL CONVERT_HH2CC(INODE,0,RNODE,0)
      CALL CONVERT_HH2CC(IUSER,0,RUSER,0)
      CALL CONVERT_HH2CC(IPATH,0,RPATH,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_exist
      INTEGER FUNCTION DSKIOF_EXIST(FNAME)
      IMPLICIT NONE
      CHARACTER*(*) FNAME
      INTEGER DSKIO_EXIST,INAME(40)
      CALL CONVERT_CC2HH(FNAME,0,INAME,-40)
      DSKIOF_EXIST = DSKIO_EXIST(INAME)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_host
      INTEGER FUNCTION DSKIOF_HOST(HOST)
      IMPLICIT NONE
      CHARACTER*(*) HOST
      INTEGER DSKIO_HOST,IHOST(40)
      CALL CONVERT_CC2HH(HOST,0,IHOST,-40)
      DSKIOF_HOST = DSKIO_HOST(IHOST)
      CALL CONVERT_HH2CC(IHOST,0,HOST,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_host
      INTEGER FUNCTION DSKIOF_CWD(CWD)
      IMPLICIT NONE
      CHARACTER*(*) CWD
      INTEGER DSKIO_CWD,ICWD(40)
      CALL CONVERT_CC2HH(CWD,0,ICWD,-40)
      DSKIOF_CWD = DSKIO_CWD(ICWD)
      CALL CONVERT_HH2CC(ICWD,0,CWD,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_host
      INTEGER FUNCTION DSKIOF_ENVUSER(ENVUSER)
      IMPLICIT NONE
      CHARACTER*(*) ENVUSER
      INTEGER DSKIO_ENVUSER,IENVUSER(40)
      CALL CONVERT_CC2HH(ENVUSER,0,IENVUSER,-40)
      DSKIOF_ENVUSER = DSKIO_ENVUSER(IENVUSER)
      CALL CONVERT_HH2CC(IENVUSER,0,ENVUSER,0)
      RETURN
      END
CCC
C A wrapper for the c routine dskio_host_to
      INTEGER FUNCTION  DSKIOF_HOST_TO_DISK(HOST, DISK)
      IMPLICIT NONE
      CHARACTER*(*) HOST,DISK
      INTEGER       IHOST(40),IDISK(40),DSKIO_HOST_TO,I
      CALL CONVERT_CC2HH(HOST,0,IHOST,-40)
      DSKIOF_HOST_TO_DISK=DSKIO_HOST_TO(IHOST,IDISK)
      CALL CONVERT_HH2CC(IDISK,0,DISK,0)
      RETURN
      END
c

C A wrapper for the c routine dskio_to_host
      INTEGER FUNCTION  DSKIOF_DISK_TO_HOST(MEL,HOST, DISK)
      IMPLICIT NONE
      CHARACTER*(*) HOST(*),DISK
      CHARACTER     HOSTS*1024
      INTEGER       IHOST(256),IDISK(40),DSKIO_TO_HOST
      INTEGER       I,J,L,CNT,MEL
      CALL CONVERT_CC2HH(DISK,0,IDISK,-40)
      CNT = DSKIO_TO_HOST(IHOST,IDISK)
      CNT = MIN(CNT,MEL)
      DSKIOF_DISK_TO_HOST= CNT
      CALL CONVERT_HH2CC(IHOST,0,HOSTS,0)
      J=1
      I=0
      DO L = 1,LEN(HOSTS)
        IF(HOSTS(L:L).EQ.' ') THEN
          HOST(I+1) = HOSTS(J:L-1)//' '
          J = L+1
          I=I+1
          IF(I.GE.CNT) RETURN
        ENDIF 
      ENDDO
      RETURN
      END
CCC
      SUBROUTINE DSKIOF_OPTIMAL_PATH(PATH,TMPORPTMP,HOST,
     x  NBLK,BYPERBLK)
      IMPLICIT NONE
      CHARACTER*(*) PATH,TMPORPTMP,HOST
      CHARACTER     STR*8
      INTEGER       NBLK,BYPERBLK
      INTEGER       I,MAXSIZE
      INTEGER       IHOST(20),IPATH(20),TORP(2)
      INTEGER    DSKIOF_HOST

      IF(HOST.EQ.' ') THEN
        PATH=' '
        RETURN
      ENDIF
      
      STR=TMPORPTMP
      IF(STR(1:1).EQ.'p') STR='P'
      IF(STR(1:1).EQ.'t') STR='T'
      IF(STR(1:1).EQ.'b') STR='B'
      CALL CONVERT_CC2HH(HOST,0,IHOST,-20)
      CALL CONVERT_CC2HH(STR,0,TORP,-2)
      CALL DSKIO_HOST_TO_OPTPATH(IHOST,IPATH,TORP,NBLK,BYPERBLK)
      CALL CONVERT_HH2CC(IPATH,0,PATH,0)
      RETURN
      END

