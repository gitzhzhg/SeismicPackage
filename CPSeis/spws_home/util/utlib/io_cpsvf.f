      SUBROUTINE OPEN_CPS_VELFILE (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*)
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
C  Process name:  OPEN_CPS_VELFILE   : Main  : Open  input-output file
C                 OPEN_CPS_VELFILE2  : Entry : Open  i-o with NHX,NHY
C                 OPEN_CPS_VELFILE3  : Entry : Open  i-o w/ SIGN & POWER
C                 CLOSE_CPS_VELFILE  : Entry : Close input-output file
C                 READ_CPS_VELNAME   : Entry : Get V-function header
C                 READ_CPS_VELFILE   : Entry : Get Header & V-function
C                 WRITE_CPS_VELFILE  : Entry : Write header & V-function
C                 CPSVF_SETUP        : Entry : Setup these routines.
C        Author:  R.S. DAY & J. B. SINTON
C  Last revised:  00/10/10  Schmauch
C       Purpose:  To manage CPS velocity function files for input and 
C                 output.
C-----------------------------------------------------------------------
C             DIFFERENCES BETWEEN THE VMS AND UNIX VERSIONS
C
C  The VMS and UNIX versions of this routine are different.
C  Here are the differences:
C    (1) The VMS version has some non-ansi code in the OPEN statements,
C          and in some WRITE statements.
C    (2) Since VMS creates new versions of existing files, and UNIX
C          overwrites existing files, the STATUS parameter must be
C          different in one of the OPEN statements.
C    (3) Some code executes incorrectly in the UNIX version, and seems
C          not to be needed at all.  That code is omitted in UNIX.
C    (4) The CRT_MESS call is omitted in UNIX.
C
C  The code for both versions is present here.
C  Code which should be used for only one of the two versions is
C  preceded and followed by special comment cards specifying which
C  version the code is to be used for.  The code for the incorrect
C  operating system should be commented out, but not removed.
C
C  When modifications are made to this code, the following instructions
C  should be followed:
C    (1) First, the modifications should be made and tested on either
C          the VMS or UNIX system, whichever is more convenient.
C    (2) Next, the modified code should be copied to the other system.
C    (3) Finally, on the other system, the code specific to the wrong
C          system should be commented out, and the code specific to
C          the correct system should be un-commented.
C
C  Search for the word FIX to find the code which needs to be commented
C  or un-commented.
C
C-----------------------------------------------------------------------
C                             REVISION HISTORY
C 15) 00/10/10: Schmauch    , Added CLOSE to 702 error since file was OPENed
C                             in OPEN_CPS_VELFILE3.
C 14) 99/09/21: Stoeckley   , Fixed bug by increasing two local arrays
C                             from 100 to 200 words.
C 13) 98/05/07: Stoeckley   , Preset X and Y to zero before reading them.
C 12) 97/05/15: Vunderink   , Put new consolidated code on VMS
C 11) 97/05/07: Stoeckley   , Consolidate the VMS and UNIX versions
C                             of this code, with instructions on how
C                             to keep both versions consistent and
C                             up-to-date.  Also add more explicit
C                             error messages (from UNIX version), and
C                             added error returns to INQUIRE statements
C                             and an OPEN statement.
C 10) 97/04/23: Vunderink   , Add entry OPEN_CPS_VELFILE3, and add NMC
C                             SIGN and POWER to *GLOBALS card.
C  9) 94/01/19: K. Goodger  , Add common block SPLTVELX to communicate
c                             with SPLT.
C  8) 92/01/22: TR Stoeckley, Change dimensioning of SV since Unix
C                             compiler doesn't like it.
C  7) 91/02/15: TR Stoeckley, Add entry OPEN_CPS_VELFILE2, and add NHX
C                             and NHY to *GLOBALS card.
C  6) 90/07/19: WR Troutt, Add error logic for OPEN when IOSTAT=WRITE.
C  5) 90/03/07: WR Troutt, Add CARRIAGECONTROL='LIST' to OPEN statements
C                             to allow use of TYPE and PRINT commands.
C  4) 89/05/15: JB Sinton, Changed WRITE_CPS_VELFILE to be more robust.
C  3) 89/04/10: JB Sinton, Argument FILE_NAME returns with the full
C                             file name.
C  2) 89/03/02: JB Sinton, Corrections to OPEN_CPS_VELFILE when reopen-
C                             ing an already opened file.
C  1) 88/10/19: JB Sinton, Added LFN and NVPP to argument lists.
C-----------------------------------------------------------------------
C                             CALLING SEQUENCE
C      CALL OPEN_CPS_VELFILE (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*)
C      CALL OPEN_CPS_VELFILE2(LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*,NHX,NHY)
C      CALL OPEN_CPS_VELFILE3(LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*,NHX,NHY,
C     +        NMC_SIGN,NMC_POWER)
C      CALL CLOSE_CPS_VELFILE(LFN,IOSTAT)
C      CALL READ_CPS_VELNAME (LFN,NVPP,VN,NVP,X,Y,CTVF,*,*,
C     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT)
C      CALL READ_CPS_VELFILE (LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,*,*,
C     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
C     +        SV)
C      CALL WRITE_CPS_VELFILE (LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
C     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
C     +        SV)
C      CALL CPSVF_SETUP (EZED,PRTON)
C
C LFN        - Fortran logical file number, 1-999; INTEGER.
C FILE_NAME  - Name of the velocity file for input or output.
C              On return FILE_NAME will contain the complete name
C              including all references to disks and directories
C              if the actual argument is of type CHARACTER.
C IOSTAT     - 'READ' OR 'WRITE'; CHARACTER.
C NVPP       - Number of points in each velocity pair; INTEGER. 
C              This parameter is returned by OPEN_CPS_VELFILE.
C              This parameter must be passed to all other calls.
C NVF        - Number of velocity functions in the file; INTEGER.
C              This parameter is returned by OPEN_CPS_VELFILE.
C VN         - Velocity function name; CHARACTER*8.
C NVP        - Number of pairs in the velocity function; INTEGER.
C X          - X coordinate of the velocity function; REAL.
C Y          - Y coordinate of the velocity function; REAL.
C NHX        - CPS trace header word containing X coord; INTEGER.
C NHY        - CPS trace header word containing Y coord; INTEGER.
C NMC_SIGN   - The sign used for the moveout computation
C NMC_POWER  - The exponent used for the moveout computation
C--------------- VELOCITY FUNCTION ARRAYS ------------------
C T(i)       - Array that holds 'TIME' values; REAL.
C V(i)       - Array that holds the 'VELOCITY' values; REAL.
C SV()       - Array that holds optional values when NVPP>2; REAL.
C              If NVPP>3, then SV will look like a multidimensional
C              array, ie. SV(NVPP,NVP).
C---------- OPTIONAL FUNCTION CHARACTERISTICS --------------
C CTVF       - Type of velocity function; CHARACTER*4.
C              If CTVF=' ', then default is 'VTRM' for time,RMS.
C              See VELF for other possibilities.
C PROJECT    - Project name; CHARACTER*10. Starts in column 40.
C LINE_NAME  - Line name; CHARACTER*10. Starts IN COLUMN 51
C REC_DATE   - Recording date; CHARACTER*5. 
C PROJ_DATE  - Project data; CHARACTER*5.
C COMMENT    - Comment; CHARACTER*15.
C------------------ ALTERNATE RETURNS ----------------------
C 1st alternate return is for open or read errors.
C 2nd alternate return is for EOF conditions on file reads.
C no alternate return available for write errors.
C-----------------------------------------------------------------------
C                                 NOTES
C 1. Entry CPSVF_SETUP should be called to tell these routines how to
C    communicate error messages.  The defaults, if CPSVF_SETUP is not 
C    called, is to set EZED=.TRUE. and PRTON=.TRUE..  EZED=.TRUE. will
C    cause messages to be displayed using EZED program CRT_MESS. 
C    EZED=.FALSE. will cause messages to be displayed using PRINT.
C    PRTON=.TRUE. allows all messages to be communicated.  PRTON=.FALSE.
C    turns off all messages.  In UNIX, the EZED argument is irrelevant,
C    and messages will be displayed using PRINT.
C
C 2. OPEN_CPS_VELFILE, OPEN_CPS_VELFILE2, or OPEN_CPS_VELFILE3 will open
C    the file, read/write the GLOBAL record which contains the NVPP,NVF 
C    parameters, return these values if IOSTAT='READ'.  The second
C    entry will also read/write/return NHX and NHY.  The third entry
C    will also read/write/return NMC_SIGN and NMC_EXP.  If NHX, NHY,
C    NMC_SIGN, or NMC_EXP are not in the file, their values are
C    returned as zero.
C    The following actions will happen if IOSTAT='READ':
C     If the file is already open and attached to LFN then it will be
C      rewound with not futher action, ie. NVF and NVPP are not reset.
C     If the file is already open, but not attached to LFN then an 
C       error return will be executed.
C     If the file does not exist then an error return will be executed.
C     If the GLOBAL record does not exist the file will be closed and
C      an error return will be executed.
C    The following actions will happen if IOSTAT='WRITE':
C     If the file already exists then an error return will be executed.
C
C 3. READ_CPS_VELNAME and READ_CPS_VELFILE return information for
C    a single function. They leave the file positioned to read 
C    the next function. The input file is rewound when an EOF is 
C    encountered.  READ_CPS_VELNAME does not return the velocity
C    function.  READ_CPS_VELFILE returns all information.
C
C 4. WRITE_CPS_VELFILE writes all the information for a single 
C    velocity function to a file.
C
C 5. Output files will always have a *GLOBAL header card.  The first
C    numeric field of this card is NVPP, the second is NVF, the
C    third is a version number used by these routines, the fourth is
C    NHX, the fifth is NHY, the sixth is NMC_SIGN, and the seventh is
C    NMC_POWER.  If NHX and NHY are omitted from the argument list (by
C    calling OPEN_CPS_VELFILE rather than OPEN_CPS_VELFILE2), the
C    fields will contain 0.  If NMC_SIGN and NMC_POWER are omitted from
C    the argument list (by calling OPEN_CPS_VELFILE or OPEN_CPS_VELFILE2
C    rather than OPEN_CPS_VELFILE3), the fields will contain 0.
C
C 6. Link to POGUN::USER1:[CFE.LIBS]UTLIB/L,EZEDLIB/L
C    On UNIX, link to ~spws/lib/xxxx/libutlib.a (where xxxx is the
C    relevant machine, chosen from dec, hp, hpoc, ibm, sgi, sgi64, sol,
C    sun, intelsol, gnusol, etc.)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC
CCC     SUBROUTINE OPEN_CPS_VELFILE (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*)
        PARAMETER ( IVERSION = 1 )
        CHARACTER*(*) FILE_NAME ,VN ,CTVF ,IOSTAT
        CHARACTER PROJECT*10,LINE_NAME*10,REC_DATE*5,PROJ_DATE*5,
     +                USERID*3,COMMENT*15
        CHARACTER CTVFO*4
        CHARACTER FILE_NAME_NEW*120 ,LINE*120 ,MESSAGE*80 ,RFN*120
        CHARACTER*13 CT(20000),CV(20000)
        DIMENSION T(*) ,V(*) 
CCC     DIMENSION SV(MAX(1,NVPP-2),*) ! The Ultrix compiler does not like this.
        DIMENSION SV(*) 
        COMMON/SPLTVELX/ISPLTNHX
        INTEGER VERSION
        REAL NMC_SIGN,NMC_POWER
        LOGICAL EZED,PRTON
        LOGICAL EXIST ,ANSWER ,EZED_ENTRY ,PRINT_ON 
        LOGICAL READ_COMPLETE_FUNCTION, USE_NHX_AND_NHY
        LOGICAL USE_NMC_SIGN_AND_POWER
        character*16 fmt     ! Added to remove non-ansi parts from formats.
        DATA EZED_ENTRY/.TRUE./ ,PRINT_ON/.TRUE./
C
C&&&********************************************************************
C
CCC     ENTRY OPEN_CPS_VELFILE (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*)
        USE_NHX_AND_NHY=.FALSE.
        USE_NMC_SIGN_AND_POWER=.FALSE.
        GO TO 1
C
C&&&********************************************************************
C
        ENTRY OPEN_CPS_VELFILE2 (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*,
     +                           NHX,NHY)
        USE_NHX_AND_NHY=.TRUE.
        USE_NMC_SIGN_AND_POWER=.FALSE.
        GO TO 1
C
C&&&********************************************************************
C
        ENTRY OPEN_CPS_VELFILE3 (LFN,FILE_NAME,IOSTAT,NVPP,NVF,*,*,
     +                           NHX,NHY,NMC_SIGN,NMC_POWER)
        USE_NHX_AND_NHY=.TRUE.
        USE_NMC_SIGN_AND_POWER=.TRUE.

1       CONTINUE
        IERRSTAT = 0                     ! use normal return unless reset.
        VERSION = IVERSION
        FILE_NAME_NEW=FILE_NAME
        CALL ADDEXT(FILE_NAME_NEW,'VEL')         
        CALL FLNBC (FILE_NAME_NEW,NCIFN,ICHAR(' '))
        INQUIRE (FILE=FILE_NAME_NEW ,OPENED=ANSWER ,EXIST=EXIST,
     +           NUMBER=LFNINQ, ERR=904)               ! ERR added 5/7/97
        LUSC = INDEX(FILE_NAME_NEW,';')
        IF (ANSWER) THEN
         IF      (IOSTAT(:1).EQ.'R' .AND. LFNINQ.EQ.LFN) THEN
          REWIND LFN
          READ (LFN,'(A80)',ERR=701,END=900) LINE
          MESSAGE = 'Previouly opened file rewound.'
          GOTO 990                          ! use normal return.
         ELSE IF (IOSTAT(:1).EQ.'R') THEN
          IERRSTAT = 2                      ! use alternate return for EOF.
          MESSAGE = 'File already opened to another LFN, try again.'
          GOTO 990
         ELSE 
          IERRSTAT = 2                      ! use alternate return for EOF.
          MESSAGE = 'You cannot write to that file, try again.'
          GOTO 990
         ENDIF
        ENDIF 
        IF      (IOSTAT(:1).EQ.'R' .AND. .NOT.EXIST) THEN
         IERRSTAT = 2                      ! use alternate return for EOF.
         MESSAGE = 'File does not exist, try again.'
         GOTO 990
        ELSE IF (IOSTAT(:1).NE.'R' .AND.      EXIST .AND. LUSC.NE.0) 
     +  THEN
         IERRSTAT = 2                      ! use alternate return for EOF.
         MESSAGE = 'File already exists, try again.'
         GOTO 990
        ENDIF
        IF (IOSTAT(:1).EQ.'R') THEN
c-FIX--------------- the following code is VMS only ------------------------
c        OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='OLD',
c    +     ERR=906,CARRIAGECONTROL='LIST',READONLY,SHARED) ! ERR added 5/7/97
c-FIX--------------- VMS code is above and UNIX code is below --------------
         OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='OLD',
     +     ERR=906)                                        ! ERR added 5/7/97
c-FIX--------------- the above code is UNIX only ---------------------------
         READ (LFN,'(A80)',ERR=702,END=900) LINE
         IF (LINE(:9).EQ.'*GLOBALS:') THEN
           IF (USE_NHX_AND_NHY .AND. USE_NMC_SIGN_AND_POWER) THEN
            NHX=0
            NHY=0
            NMC_SIGN=0.0
            NMC_POWER=0.0
            READ (LINE(10:),*,END=10) NVPP,NVF,VERSION,NHX,NHY,NMC_SIGN,
     +                                NMC_POWER
            ISPLTNHX=NHX 
           ELSE IF (USE_NHX_AND_NHY) THEN
            NHX=0
            NHY=0
            READ (LINE(10:),*,END=10) NVPP,NVF,VERSION,NHX,NHY
            ISPLTNHX=NHX 
           ELSE
            READ (LINE(10:),*,END=10) NVPP,NVF,VERSION
           ENDIF 
 10       CONTINUE
         ELSE
          CLOSE (LFN,STATUS='KEEP')
          IERRSTAT = 2                     ! use alternate return for EOF.
          MESSAGE = 'First card does not contain *GLOBALS:'
          GOTO 990
         ENDIF
        ELSE 
c-FIX--------------- the following code is VMS only ------------------------
c        OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='NEW',
c    +         CARRIAGECONTROL='LIST',ERR=903)
c-FIX--------------- VMS code is above and UNIX code is below --------------
         OPEN (LFN,FILE=FILE_NAME_NEW,FORM='FORMATTED',STATUS='UNKNOWN',
     +         ERR=903)
c-FIX--------------- the above code is UNIX only ---------------------------
         VERSION = IVERSION
         IF (USE_NHX_AND_NHY .AND. USE_NMC_SIGN_AND_POWER) THEN
            WRITE (LFN,'(''*GLOBALS: '',5I10,2(1X,G9.3))') NVPP,NVF,
     +                                VERSION,NHX,NHY,NMC_SIGN,NMC_POWER
         ELSE IF (USE_NHX_AND_NHY) THEN
            WRITE (LFN,'(''*GLOBALS: '',5I10,2(1X,G9.3))') NVPP,NVF,
     +                                VERSION,NHX,NHY,0.0,0.0
         ELSE
            WRITE (LFN,'(''*GLOBALS: '',5I10,2(1X,G9.3))') NVPP,NVF,
     +                                VERSION,0,0,0.0,0.0
         ENDIF 
        ENDIF 
        LENFN = LEN(FILE_NAME)
        IF (LENFN.NE.0) THEN
         INQUIRE (FILE=FILE_NAME_NEW, NAME=RFN, ERR=905)  ! ERR added 5/7/97
c-FIX--------------- the following code is VMS only ------------------------
c        CALL FLNBC (RFN,IFN_END,ICHAR(' '))
c        LSC = INDEX(RFN,';')
c        IF (LUSC.NE.0 .AND. LENFN.GE.(LSC+NCIFN-LUSC) ) THEN 
c         FILE_NAME = RFN(:LSC-1) // FILE_NAME_NEW(LUSC:NCIFN) 
c        ELSE IF (LENFN.GE.LSC) THEN
c         FILE_NAME = RFN(:LSC-1)
c        ENDIF
c        FILE_NAME_NEW = FILE_NAME
c-FIX--------------- the above code is VMS only ----------------------------
ccc         In UNIX, the above statements somehow cause nvpp to be
ccc                        reset to a huge number.
        ENDIF
        RETURN
C
C&&&********************************************************************
C
        ENTRY CLOSE_CPS_VELFILE (LFN,IOSTAT)
        CLOSE (UNIT=LFN ,STATUS='KEEP' ,IOSTAT=IOS)
        RETURN 
C
C&&&********************************************************************
C
        ENTRY READ_CPS_VELNAME (LFN,NVPP,VN,NVP,X,Y,CTVF,*,*,
     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT)
        READ_COMPLETE_FUNCTION = .FALSE.
        GOTO 50
C                 
C&&&********************************************************************
C
        ENTRY READ_CPS_VELFILE (LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,*,*,
     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
     +        SV)
        READ_COMPLETE_FUNCTION = .TRUE.

 50     CONTINUE
        READ (LFN,'(A120)',ERR=703,END=900) LINE 
        CALL FFOCH (LINE(9:),NCPV,'V')                   
        IF (NCPV.NE.0) THEN
         CTVF = LINE(9+NCPV-1:9+NCPV+2)
        ELSE
         CTVF = 'VTRM'
        ENDIF
        VN        = LINE(:8)
        PROJECT   = LINE(40:49)
        LINE_NAME = LINE(51:60)
        REC_DATE  = LINE(62:66)
        PROJ_DATE = LINE(68:72)
        USERID    = LINE(74:76)
        COMMENT   = LINE(78:92)
        x = 0.0   ! added 5/7/98
        y = 0.0   ! added 5/7/98 since no error occurs if Y is missing!
        READ (LINE(9:),*,END=110) NVP,X,Y
 110    CONTINUE
        IF (READ_COMPLETE_FUNCTION) THEN
         IF (NVPP.EQ.2) THEN
          READ (LFN,*,ERR=704,END=900) (T(I),V(I),I=1,NVP)
         ELSE
          J = NVPP-2
          READ (LFN,*,ERR=705,END=900) 
     +    (T(I),V(I),(SV(K+(I-1)*J),K=1,J),I=1,NVP)
CCC  +    (T(I),V(I),(SV(K,I),K=1,J),I=1,NVP)    ! old code (removed)
         ENDIF
        ELSE
         IF (NVPP.EQ.2) THEN
          READ (LFN,*,ERR=706,END=900) (TDUM,VDUM,I=1,NVP)
         ELSE
          J = NVPP-2
          READ (LFN,*,ERR=707,END=900) 
     +    (TDUM,VDUM,(SVIDUM,K=1,J),I=1,NVP)
         ENDIF
        ENDIF
        RETURN
C
C&&&********************************************************************
C
        ENTRY WRITE_CPS_VELFILE (LFN,NVPP,VN,NVP,X,Y,CTVF,T,V,
     +        PROJECT,LINE_NAME,REC_DATE,PROJ_DATE,USERID,COMMENT,
     +        SV)
        IF (CTVF(:1).EQ.' ') THEN
         CTVFO = 'VTRM'
        ELSE
         CTVFO = CTVF
        ENDIF
        WRITE (LINE(9:),'(1X,I4,1X,F9.1,1X,F9.1,1X,A4)') NVP,X,Y,CTVFO
        LINE(:8)    = VN 
        LINE(39:39) = ' '
        LINE(40:)   = PROJECT
        LINE(51:)   = LINE_NAME
        LINE(62:)   = REC_DATE
        LINE(68:)   = PROJ_DATE
        LINE(74:)   = USERID
        LINE(78:)   = COMMENT
        WRITE (LFN,'(A120)',ERR=902) LINE
        DO I=1,NVP
         VMAX = MAX( ABS(V(I)) ,VMAX )
        ENDDO 
        MAXCT = 0
        MAXCV = 0
        DO I=1,NVP
         CT(I) = '             '
         CV(I) = '             '
         CALL NUM2ALFR (T(I),CT(I),*902,NCT)
 !! integerization removed 3/25/02:
 !!      IF (VMAX.GT.0) THEN
 !!       CALL NUM2ALFI (NINT(V(I)),CV(I),*902,NCV)
 !!      ELSE
          CALL NUM2ALFR (     V(I) ,CV(I),*902,NCV)
 !!      ENDIF
         MAXCT = MAX( MAXCT ,NCT )
         MAXCV = MAX( MAXCV ,NCV )
        ENDDO
        IF (NVPP.EQ.2) THEN
         NVPC  = 80/(MAXCT+MAXCV+3)
c-FIX--------------- the following code is VMS only ------------------------
c        WRITE (LFN,'(<NVPC>(A))',ERR=902) 
c    +   (' '//CT(I  )(:MAXCT)//' '//CV(I  )(:MAXCV)//',',I=1,NVP-1),
c    +    ' '//CT(NVP)(:MAXCT)//' '//CV(NVP)(:MAXCV)
c-FIX--------------- VMS code is above and UNIX code is below --------------
         write (fmt,'(A1,I3,A4)') '(',nvpc,'(A))'
         WRITE (LFN,fmt,ERR=902)
     +   (' '//CT(I  )(:MAXCT)//' '//CV(I  )(:MAXCV)//',',I=1,NVP-1),
     +    ' '//CT(NVP)(:MAXCT)//' '//CV(NVP)(:MAXCV)
c-FIX--------------- the above code is UNIX only ---------------------------
        ELSE
         J = NVPP-2
c-FIX--------------- the following code is VMS only ------------------------
c        WRITE (LFN,'(A,<J>E13.5)',ERR=902) 
c    +   (' '//CT(I)(:MAXCT)//' '//CV(I)(:MAXCV)//', ',
c    +   (SV(K+(I-1)*J),K=1,J),I=1,NVP)
c-FIX--------------- VMS code is above and UNIX code is below --------------
         write (fmt,'(A1,I3,A6)') '(',j,'E13.5)'
         WRITE (LFN,fmt,ERR=902)
     +   (' '//CT(I)(:MAXCT)//' '//CV(I)(:MAXCV)//', ',
     +   (SV(K+(I-1)*J),K=1,J),I=1,NVP)
c-FIX--------------- the above code is UNIX only ---------------------------
CCC  +   (SV(K,I),K=1,J),I=1,NVP)   ! old code (removed)
        ENDIF
        RETURN
C
C&&&********************************************************************
C
 900    CONTINUE
        REWIND LFN     !EOF on input.
        IERRSTAT = 2              ! use alternate return for EOF.
                                  ! (for read, or open for read)
        MESSAGE = ' '             ! no message.
        GOTO 990
 701    MESSAGE = 'Error 701 in reading velocity functions.'
        GOTO 799
 702    MESSAGE = 'Error 702 in reading velocity functions.'
        CLOSE (LFN,STATUS='KEEP')  ! Added 10oct00, ehs
        GOTO 799
 703    MESSAGE = 'Error 703 in reading velocity functions.'
        GOTO 799
 704    MESSAGE = 'Error 704 in reading velocity functions.'
        GOTO 799
 705    MESSAGE = 'Error 705 in reading velocity functions.'
        GOTO 799
 706    MESSAGE = 'Error 706 in reading velocity functions.'
        GOTO 799
 707    MESSAGE = 'Error 707 in reading velocity functions.'
        GOTO 799
 799    IERRSTAT = 1           ! use alternate return for read error.
        GOTO 990
c 901    CONTINUE
c        IERRSTAT = 1           ! use alternate return for read error.
c        MESSAGE = 'Error in reading velocity functions.'
c        GOTO 990
 902    CONTINUE
        IERRSTAT = 3        ! no alternate return available for write error.
        MESSAGE = 'Error in writing velocity functions.'
        CLOSE (LFN,STATUS='KEEP')
        GOTO 990
 903    CONTINUE
        IERRSTAT = 1      ! use alternate return for open (for write) error.
        MESSAGE = 'Error opening output velocity file. Bad file name?'
        GOTO 990
 904    CONTINUE
        IERRSTAT = 1      ! use alternate return for open (for r/w) error.
        MESSAGE = 'Error inquiring about velocity file.'
        GOTO 990
 905    CONTINUE
        IERRSTAT = 1      ! use alternate return for open (for r/w) error.
        MESSAGE = 'Error with second inquire about velocity file.'
        GOTO 990
 906    CONTINUE
        IERRSTAT = 1      ! use alternate return for open (for read) error.
        MESSAGE = 'Error opening input velocity file.'
        GOTO 990
 990    CONTINUE 
        IF (MESSAGE(:1).NE.' '.AND.PRINT_ON) THEN  !No message if ' '.
c-FIX--------------- the following code is VMS only ------------------------
c        IF (EZED_ENTRY) THEN
c         CALL FLNBC (MESSAGE,LNBC,ICHAR(' '))
c         MESSAGE(LNBC+1:LNBC+1) = CHAR(0)
c         CALL CRT_MESS (23,%REF(MESSAGE))
c        ELSE
c         PRINT *,MESSAGE
c        ENDIF
c-FIX--------------- VMS code is above and UNIX code is below --------------
         PRINT *,MESSAGE
c-FIX--------------- the above code is UNIX only ---------------------------
        ENDIF
        IF (IERRSTAT.EQ.0) RETURN        !Normal return
        IF (IERRSTAT.EQ.1) RETURN 1      !Alternate return 1, error.
        IF (IERRSTAT.EQ.2) RETURN 2      !Alternate return 2, EOF.
        STOP 'FATAL ERROR!'              !Fatal error.
C
C&&&********************************************************************
C
        ENTRY CPSVF_SETUP (EZED,PRTON)
        EZED_ENTRY = EZED
        PRINT_ON   = PRTON
        RETURN
        END
