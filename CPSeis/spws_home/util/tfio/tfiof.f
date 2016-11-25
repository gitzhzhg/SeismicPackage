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
C    Primitive name:  TFIOF_* 
C  Source directory:  tfio
C           Written:  99/04/25  by:  R.S. Day
C      Last revised:  99/09/30       Day
C           Purpose:  Fortran wrappers to c-routines that open,read,
C                     write,close,flush simple trace files.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C
C     INTEGER FUNCTION TFIOF_OP(UFI, IOTYPE, FNAME, OFLAG,
C    x FTYPE, WTYPE, NWIH,
C    x N1,O1,D1,UNITS1)
C       - Open a file with a particular iotype
C     INTEGER FUNCTION TFIOF_OP_NET(UFI, IOTYPE, FNAME, OFLAG,
C    x FTYPE, WTYPE, NWIH,
C    x N1,O1,D1,UNITS1,NETNAME)
C      TFIOF_OP_NET1 has same arguments as TFIOF_OP but FNAME
C      is interpreted like netname. The local file name is derived
C      from the network name, netname.
C       - Open file FNAME
C       - NETNAME is location of source/dest. file on the network
C       - (i.e. netname = 'userid@none_name:pathnname)
C
C     INTEGER FUNCTION TFIOF_FILE_PARMS(FNAME,NTRFIL,NDPT,SRVAL)
C       - Given a file name, return some key file parameter values.
C       - ntrfil = number of traces in the file
C       - ndpt   = number of trace samples
C       - srval  = trace sample rate
C
C     INTEGER FUNCTION TFIOF_RD(UFI,NUM,TPOS,NWIH,HD,NDPT,TR,
C    x        WRKSIZE,WRKBUFF)
C       - read NUM trace and header records starting at TPOS
C       - returns the number of records read or -1 on error 
C
C     INTEGER FUNCTION TFIOF_WR(UFI,NUM,TPOS,NWIH,HD,NDPT,TR,
C    x        CLIP,WRKSIZE,WRKBUFF)
C       - write NUM trace and header records starting at TPOS
C       - returns the number of records written(0 or NUM)
C       - or -1 on error 
C
C     INTEGER FUNCTION TFIOF_CL(UFI,REMOVE_IT)
C       - Closes file with Unique File Index UFI.
C       - set REMOVE_IT = 0 to keep file upon close
C
C     INTEGER FUNCTION TFIOF_FLUSH(UFI)
C       - Forces a file flush when IOTYPE is set for buffered IO
C
C UFI       A Unique File Index(A file handle returned by the open).
C IOTYPE    Integer to choose io style
C           0 - low level c(unbuffered)
C           1 - ffio (Crays)
C           2 - for future use
C           3 - aqio (Crays)
C           4 - stream io in c(buffered)
C           5 - Fortran Direct Access
C FNAME     The file name to read or write
C OFLAG     open mode flag ( irrelevant for AQIO )
C            NEW ... Open new(overwrite existing files)
C            NEWE... Open new-exclusive(only if file does not exist)
C            OLD ... Open old file for read and write
C FTYPE     File TYPE
C            TFILE - a self descriptive Conoco file format
C            SEGY  - an industry standard(more or less)
C           Is a returned for old files
C WTYPE     Word type for the trace data.
C            IEEE,IBM,IBM2,BYTE,SBYT,VMS are legitimate.
C            IEEE or BYTE are most portable
C            FTYPE=SEGY overrides this parameter
C CLIP      CLIP amplitudes to this value when converting to 8 or 16 bit
C            CLIP < 0 does no clipping
C NUM       Number of hd-tr vectors to write/read
C TPOS      Initial Trace Position to write/read(starts from 1)
C NWIH      1st dimension of HD array
C            On the open call this is only relevant for TFILE format
C            NWIH can be different on the Open, read, and write calls.
C HD        REAL Header data
C NDPT      1st dimension of TR array
C TR        REAL Trace data
C WRKSIZE   Size in REAL words of the user supplied work buffer
C           NUM*(NWIH + NDPT)/2 is a safe size
C WRKBUFF   The work buffer used for reads and writes
C N1        Number of trace samples(e.g. the NDPT global)
C O1        Coordinate origin of the 1st sample(e.g. TSTRT global)
C D1        The trace sample rate(e.g. The DT global)
C UNITS1    A label for the trace axis(e.g. seconds, TIME, DEPTH)
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. The open call specifies the header and trace format for the
C     subsequent read and write calls. 
C    -Header words(if any) are written as IEEE 32 bit floats unless
C     the file type is SEGY.
C    -Header and Trace data passed to the read & write routine are 
C     assumed to be of type REAL
C    -Header and Trace data returned from the read routine are 
C     of type REAL (RAW routines?)
C    -Headers will be passed in CPS format
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C 4. 99/09/30   R.Day      TFIOF_WR buffer size check corrected
C 3. 99/06/25   R.Day      Fixed bugs with Segy file types on R/W
C 2. 99/06/23   R.Day      Fixed problem in tfiof_cl for segy files
C 1. 99/04/26   R.Day      Original
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C           LIST OF SUBROUTINE, FUNCTION, ENTRY, COMMON, INCLUDE  NAMES
C
C  Functions: 5
C   INTEGER TFIOF_OP         INTEGER TFIOF_RD         INTEGER TFIOF_WR        
C   INTEGER TFIOF_CL         INTEGER TFIOF_FLUSH     
C-----------------------------------------------------------------------
C           LIST OF ALL CALLS MADE BY THIS MODULE
C
C  Calls:2
C   CONVERT_CC2HH            DSKIO_XFLUSH            
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
C Test Suite
C     IMPLICIT NONE
C     REAL      TR(1501,10),HD(96,10)
C     REAL      WRKBUFF(15000),D1,O1
C     INTEGER   NWIH,NDPT,NHDWD,hdsiz,trsiz
C     INTEGER   WRKSIZE,IOTYPE
C     CHARACTER FNAME*80,FTYPE*8,UNITS1*8,OFLAG*8,WTYPE*8
C     INTEGER   I,J,NUM,UFI,TPOS,RECL,REMOVE_IT,I_ERR
C     REAL      CLIP
C     INTEGER   TFIOF_OP,TFIOF_WR,TFIOF_RD,TFIOF_CL,TFIOF_FLUSH
C     INTEGER   DSKIO_CHAIN_RECL,DSKIO_CHAIN_IOTYPE
C
C     TRSIZ=1501
C     HDSIZ=96
C     WRKSIZE=15000
C     NUM=10
C     NWIH=64
C     NDPT=1001
C Create some dummy traces
C     DO I = 1,NUM
C       DO J = 1,HDSIZ
C        HD(J,I) = (I-1)*NWIH + J
C       ENDDO
C       DO J = 1,NDPT
C        TR(J,I)=I
C       ENDDO
C     ENDDO
C     FNAME  ='./tfiof.tf'
C     FTYPE ='TFILE'
C     UNITS1='sec'
C     WTYPE ='IEEE'  !output as IEEE 32 bit floats
C     OFLAG ='NEW'  ! new file, overwrite any old file
C     IOTYPE=4  !buffered C IO
C     D1    =0.004
C     O1    =0.0
C
C Open a file.
C     I_ERR = TFIOF_OP(UFI, IOTYPE, FNAME, OFLAG,
C    x FTYPE, WTYPE, NWIH,
C    x NDPT,O1,D1,UNITS1)
C     IF(UFI.GT.0) THEN
C       RECL = DSKIO_CHAIN_RECL(UFI)
C       CALL DSKIOF_GET_FNAME(UFI,FNAME)
C       WRITE(6,*) 'TEST_SUITE: UFI=',UFI,' I_ERR=',I_ERR
C       WRITE(6,*) 'TEST_SUITE: Trace record length=',RECL
C       WRITE(6,*) 'TEST_SUITE: file name=',FNAME(1:64)
C       WRITE(6,*) 'TEST_SUITE: iotype=',DSKIO_CHAIN_IOTYPE(UFI)
C     ELSE
C       WRITE(6,*) 'TEST_SUITE: OPEN FAILURE',I_ERR
C      STOP
C     ENDIF
C
C     TPOS=1
C     CLIP=-1.0
C     I_ERR = TFIOF_WR(UFI,NUM,TPOS,HDSIZ,HD,TRSIZ,TR,
C    x        CLIP,WRKSIZE,WRKBUFF)
C     RECL = DSKIO_CHAIN_RECL(UFI)
C     WRITE(6,*) 'TFIOF_WR: UFI=',UFI,' I_ERR=',I_ERR,' RECL=',RECL
C
C     I_ERR = TFIOF_FLUSH(UFI)
C     WRITE(6,*) 'TFIOF_FLUSH: UFI=',UFI,' I_ERR=',I_ERR
C
C     NUM=2
C     TPOS=7
C     I_ERR = TFIOF_RD(UFI,NUM,TPOS,HDSIZ,HD,TRSIZ,TR,
C    x        WRKSIZE,WRKBUFF)
C     WRITE(6,*) 'TFIOF_RD: UFI=',UFI,' I_ERR=',I_ERR
C     DO I=1,HDSIZ
C      WRITE(6,*) 'I=',I,' HD=',HD(I,1)
C     ENDDO
C     WRITE(6,*) 'TR(5,1)=',TR(5,1)
C
C     REMOVE_IT=0
C     I_ERR = TFIOF_CL(UFI,REMOVE_IT)
C     STOP
C     END
CCC
      INTEGER FUNCTION TFIOF_OP_NET1(UFI, IOTYPE, NETNAME, OFLAG,
     x FTYPE, WTYPE, NWIH,
     x N1,O1,D1,UNITS1)
      IMPLICIT NONE
      CHARACTER*(*) FTYPE,OFLAG,WTYPE,UNITS1,NETNAME
      INTEGER  UFI,IOTYPE,NWIH
      INTEGER  N1
      REAL     O1,D1
      CHARACTER NODE*32,FILE*88,BFILE*64
      INTEGER  I
      INTEGER  DSKIOF_NODE,DSKIOF_FILE,DSKIOF_BARE_FILE
      INTEGER  TFIOF_OP_NET
                           
      I = DSKIOF_NODE(NETNAME,NODE)
      I = DSKIOF_FILE(NETNAME,FILE)
      I = DSKIOF_BARE_FILE(NETNAME,BFILE)
      if(NODE(1:4).EQ.'none') NODE=' '
      if(NODE(1:4).EQ.'NONE') NODE=' '
      IF(NODE.NE.' ') THEN
        FILE=BFILE
      ENDIF
      TFIOF_OP_NET1= TFIOF_OP_NET(UFI, IOTYPE, FILE, OFLAG,
     x FTYPE, WTYPE, NWIH,
     x N1,O1,D1,UNITS1,NETNAME)
      RETURN
      END
CCC
      INTEGER FUNCTION TFIOF_OP_NET(UFI, IOTYPE, FNAME, OFLAG,
     x FTYPE, WTYPE, NWIH,
     x N1,O1,D1,UNITS1,NETNAME)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,FTYPE,OFLAG,WTYPE,UNITS1,NETNAME
      INTEGER  UFI,IOTYPE,NWIH
      INTEGER  N1
      REAL     O1,D1
      CHARACTER NODE*32,FILE*88,BFILE*64
      INTEGER  DIR,LOCAL,I
      INTEGER  DSKIOF_NODE,RCPXFRF_NET
      INTEGER  TFIOF_OP

      CHARACTER  NETNAMES(100)*96
      COMMON /TFIOFDAT/ NETNAMES
      

      TFIOF_OP_NET=-1
C
C  rcp old input file if node info is contained in netname
      NODE = ' '
      IF(NETNAME.NE. ' ') THEN
        I = DSKIOF_NODE(NETNAME,NODE)
      ENDIF
      if(NODE(1:4).EQ.'none') NODE=' '
      if(NODE(1:4).EQ.'NONE') NODE=' '
      IF(NODE.NE.' ') THEN
        IF(OFLAG(1:1).EQ.'O' .OR. OFLAG(1:1).EQ.'o') THEN 
          DIR=0
          I = RCPXFRF_NET(FNAME,NETNAME,LOCAL,DIR)
          IF(I.NE.0) THEN
           WRITE(6,*) 'TFIOF_OP: Failed rcpxfrf_net call'
           RETURN
          ENDIF
        ELSE
        ENDIF
      ENDIF
      TFIOF_OP_NET = TFIOF_OP(UFI, IOTYPE, FNAME, OFLAG,
     x FTYPE, WTYPE, NWIH,
     x N1,O1,D1,UNITS1)

      IF(UFI.GT.0 .AND. UFI.LT.100) THEN
        NETNAMES(UFI) = NETNAME
      ENDIF
      RETURN
      END
CCC
      INTEGER FUNCTION TFIOF_OP(UFI, IOTYPE, FNAME, OFLAG,
     x FTYPE, WTYPE, NWIH,
     x N1,O1,D1,UNITS1)
      IMPLICIT NONE
      CHARACTER*(*) FNAME,FTYPE,OFLAG,WTYPE,UNITS1
      INTEGER  UFI,IOTYPE,IOFLAG,NWIH
      INTEGER  N1,N2,N3
      REAL     O1,O2,O3,D1,D2,D3
      INTEGER  IFNAME(40),IFTYPE(4),IWTYPE(4),IUNITS1(4)
      INTEGER  I,GRECSIZ
      CHARACTER CTMP*8
      CHARACTER NODE*32,FILE*88,BFILE*64
      INTEGER  TF_XOPENF

      CHARACTER  NETNAMES(100)*96
      COMMON /TFIOFDAT/ NETNAMES
      INTEGER  FIRST_TIME
      DATA FIRST_TIME/0/
      SAVE FIRST_TIME

      IF(FIRST_TIME.EQ.0) THEN
        DO I=1,100
         NETNAMES(I)=' '
        ENDDO
        FIRST_TIME=1
      ENDIF

      TFIOF_OP=-1
      IF(FNAME(1:4).EQ.'NONE' .OR. FNAME.EQ.' ') RETURN
      CTMP = FTYPE
      GRECSIZ=1024
      IF(FTYPE.EQ.'SEGY' .OR. FTYPE.EQ.'DSEGY') THEN
       CTMP='DSEGY'
       GRECSIZ=3600
      ENDIF
C
C Convert fortran strings to null terminated c-type strings
      CALL CONVERT_CC2HH(FNAME,0, IFNAME,-40)
      CALL CONVERT_CC2HH(CTMP,0, IFTYPE,-4)
      CALL CONVERT_CC2HH(WTYPE,0, IWTYPE,-4)
      CALL CONVERT_CC2HH(UNITS1,0,IUNITS1,-4)

      IOFLAG=0 !default to readonly
      IF(OFLAG(1:1).EQ.'N' .OR. OFLAG(1:1).EQ.'n') IOFLAG=-4
      IF(OFLAG(4:4).EQ.'E') IOFLAG=4
      IF(OFLAG(4:4).EQ.'e') IOFLAG=4
      IF(OFLAG(1:1).EQ.'O' .OR. OFLAG(1:1).EQ.'o') IOFLAG=2
      UFI = TF_XOPENF(UFI, IOTYPE, IFNAME, IOFLAG, IFTYPE,
     x      GRECSIZ,IWTYPE, NWIH,
     x      N1,O1,D1,IUNITS1)
      CALL CONVERT_HH2CC(IUNITS1,0,UNITS1,0)
      CALL CONVERT_HH2CC(IWTYPE,0,WTYPE,0)
      CALL CONVERT_HH2CC(IFTYPE,0,FTYPE,0)

      TFIOF_OP=UFI
      RETURN
      END
CCC
      INTEGER FUNCTION TFIOF_FILE_PARMS(FNAME,NTRFIL,NDPT,SRVAL)
      IMPLICIT NONE
      CHARACTER*(*) FNAME
      INTEGER UFI,NTRFIL,NDPT,IFNAME(40),I
      INTEGER GET_GLOBAL_PARMS
      REAL    SRVAL
      TFIOF_FILE_PARMS=-1
      IF(FNAME(1:4).EQ.'NONE' .OR.FNAME.EQ.' ') RETURN
      CALL CONVERT_CC2HH(FNAME,0, IFNAME,-40)
      TFIOF_FILE_PARMS=GET_GLOBAL_PARMS(IFNAME,NTRFIL,NDPT,SRVAL)
      RETURN
      END
CCC
C Purpose:  Read NUM hd-tr records starting at TPOS
      INTEGER FUNCTION TFIOF_RD(UFI,NUM,TPOS,NWIH,HD,NDPT,TR,
     x        WRKSIZE,WRKBUFF)
      IMPLICIT NONE
      INTEGER UFI,NUM,TPOS,NWIH,NDPT,WRKSIZE
      REAL    HD(NWIH,*),TR(NDPT,*),WRKBUFF(*)
      INTEGER READ_DATAF,DSKIO_SIZ_REAL
      INTEGER TRUNUM,BWRKSIZE
      TFIOF_RD=0
      IF(UFI.LT.0 .OR. NUM.LE.0) RETURN
C
C Convert buffer size to bytes
      BWRKSIZE=WRKSIZE*DSKIO_SIZ_REAL(WRKBUFF(1),WRKBUFF(2))
C
C Will return data as true reals even if float is not same
      TRUNUM = READ_DATAF(UFI, NUM, TPOS,
     x        NWIH, HD, NDPT, TR,
     x        WRKBUFF,BWRKSIZE)
      TFIOF_RD=TRUNUM
      IF(TRUNUM.LE.0) THEN
        RETURN
      ENDIF

      RETURN
      END
CCC
C Purpose: Write NUM trace and header records starting at TPOS 
C       - returns the number of records written(0 or NUM)
C       - or -1 on error 
C 1. Call write_dataf to output data to disk
C    write_dataf will do conversion to output type
      INTEGER FUNCTION TFIOF_WR(UFI,NUM,TPOS,
     x        NWIH,HD,NDPT,TR,
     x        CLIP,WRKSIZE,WRKBUFF)
      IMPLICIT NONE
      INTEGER UFI,NUM,TPOS,NWIH,NDPT,WRKSIZE
      REAL    HD(NWIH,*),TR(NDPT,*),WRKBUFF(*),CLIP
      INTEGER WRITE_DATAF,DSKIO_SIZ_FLOAT,DSKIO_SIZ_REAL
      INTEGER SIZR,I_ERR,BWRKSIZE
      TFIOF_WR=0
      IF(UFI.LT.0 .OR. NUM.LE.0) RETURN
      SIZR=DSKIO_SIZ_REAL(WRKBUFF(1),WRKBUFF(2))
      BWRKSIZE=WRKSIZE*SIZR
      IF(SIZR .NE. DSKIO_SIZ_FLOAT()) THEN
        IF(4*NUM*(NWIH+NDPT) .GT. BWRKSIZE) THEN
          TFIOF_WR=-1
          WRITE(6,*) 'TFIOF_WR: Buffer is too small!!!'
          RETURN
        ENDIF
      ENDIF
      I_ERR = WRITE_DATAF(UFI, NUM, TPOS,
     x          HD,NWIH,TR,NDPT,
     x          CLIP,WRKBUFF,BWRKSIZE)
      TFIOF_WR=NUM
      IF(I_ERR.EQ.0) TFIOF_WR=-1
      RETURN
      END
CCC
C       - Closes file associated with Unique File Index UFI.
      INTEGER FUNCTION TFIOF_CL(UFI,REMOVE_IT)
      IMPLICIT NONE
      INTEGER UFI,REMOVE_IT
      INTEGER  TF_CLOSE,TF_GLBL_GET,PUT_GLOBAL_TO_FILE,IMSG(20)
      INTEGER  DSKIOF_NODE,DSKIOF_BARE_FILE,DSKIO_CHAIN_OFLAG
      INTEGER  DSKIO_ORW,TFIOF_FLUSH,RCPXFRF_NET
      INTEGER  GTMP(256),I,I_ERR,open_flag
      INTEGER  LOCAL,DIR
      CHARACTER NODE*32,FNAME*96,NETNAME*96

      CHARACTER  NETNAMES(100)*96
      COMMON /TFIOFDAT/ NETNAMES

      TFIOF_CL=0
      IF(UFI.LT.0) RETURN
      I=TF_GLBL_GET(I_ERR,UFI,GTMP)
      IF(I_ERR.NE.0) THEN
        WRITE(6,*) 'TFIOF_CL: Failed to get TF_GLOBAL'
        RETURN
      ENDIF
      I= PUT_GLOBAL_TO_FILE(GTMP)
      IF(I.EQ.0) THEN
        WRITE(6,*) 'TFIOF_CL: Failed to update file header'
      ENDIF

      I = TFIOF_FLUSH(UFI)
      OPEN_FLAG = DSKIO_CHAIN_OFLAG(UFI)

c get info needed for the network transfer before we close the file
      CALL DSKIOF_GET_FNAME(UFI,FNAME)
      NETNAME = ' '
      IF(UFI.LT.100) NETNAME = NETNAMES(UFI)
      I = DSKIOF_NODE(NETNAME,NODE)
      IF(NODE(1:4).EQ.'none') NODE=' '
      IF(NODE(1:4).EQ.'NONE') NODE=' '

c  close the file before the net transfer
      TFIOF_CL = TF_CLOSE(UFI,REMOVE_IT,IMSG)

      IF(OPEN_FLAG.NE.DSKIO_ORW()) THEN !possible transfer if new file

        IF(NODE.NE.' ') THEN
          DIR=1
          I = RCPXFRF_NET(FNAME,NETNAME,LOCAL,DIR)
          IF(I.NE.0) THEN
           WRITE(6,*) 'TFIOF_CL: Failed rcpxfrf_net call'
          ENDIF
        ENDIF

      ENDIF

      NETNAMES(UFI) = ' '
C      TFIOF_CL = TF_CLOSE(UFI,REMOVE_IT,IMSG)

      RETURN
      END
CCC
C       - Forces a file flush when IOTYPE is set for buffered IO
      INTEGER FUNCTION TFIOF_FLUSH(UFI)
      IMPLICIT NONE
      INTEGER UFI,I_ERR
      TFIOF_FLUSH=0
      IF(UFI.LT.0) RETURN
      CALL DSKIO_XFLUSH(UFI,I_ERR)
      TFIOF_FLUSH=I_ERR
      RETURN
      END
