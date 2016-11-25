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
C  Process name:  ISEP    (INTERACTIVE STATIC EDITING PROGRAM)
C        Author:  Tom Stoeckley  
C       Written:  1989-01-31
C  Last revised:  2008-02-22 Tom Stoeckley
C
C  Purpose:       Interactive program (on the VAX and UNIX workstations)
C                 to be used for building and editing static files.
C-----------------------------------------------------------------------
C                           NOTES FOR USERS
C
C  1.  Here are some of the options available in this program:
C            Build a static file from scratch.
C            Read and save static files in either CPS or generic format.
C            List static file attributes.
C            Make a line-printer-type plot of the file on the screen.
C            Make a hard-copy printer-plot of the static file.
C            Edit the static file.
C            Grade the static file between two ground positions.
C            Grade the static file over a rectangular area.
C            Integrate the static file.
C            Remove a running average.
C            Smooth the static file.
C            Replace nils with interpolated values.
C            Multiply the static values by a constant.
C            Add a constant to the static values.
C            Add a constant to the ground positions.
C            Truncate, extend, or resample the static file.
C            Compare two static files.
C            Add, subtract, or average two static files.
C            Splice two static files together.
C
C  2.  The program will prompt the user for all required information.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC        
C-----------------------------------------------------------------------
C                         REVISION HISTORY                              
C     Date       Author   Description
C     ----       ------   -----------
C 29. 07-22-08 Stoeckley  Increase static file size from 9000000 to 40000000.
C 28. 07-14-03 Stoeckley  Use STATIO to read and write static files; increase
C                          static file size from 3000000 to 9000000.
C 27. 09-12-00 Stoeckley  Change TYPE to STATUS and NAME to FILE in OPEN
C                          statements; change (1H1) to ("1") in format
C                          statement.
C 26. 11/03/97 Stoeckley  Add new options NX, NY, NN, V1, and V2, and
C                          remove option SISC.  Also change references to
C                          foreign, cyber, or conseis files to generic.
C 25. 10/24/97 Stoeckley  Add new rectangular grading option (GG).
C 24. 3/10/97  Stoeckley  Add new choice in the SPLICE option.
C 23. 8/28/96  Stoeckley  Add capability to read and save 3D foreign
C                          static files.  Previously could read and save
C                          only 2D foreign static files.  Changed CFILE
C                          and FFILE, and added CFILE2 and CFILE3.
C 22. 4/12/95  Stoeckley  Replace RAN with RANDOM for ANSI-standardness.
C                          Also remove 4 unused variables from OPTIONS.
C                          Also replace DATE and TIME with DATE_AND_TIME
C                          for ANSI-standardness.
C 21. 6/09/94  Stoeckley  Increase file size from 200000 to 2,000,000,
C                          and number of cards from 500 to 5000.
C                          (POGUN has 2,000,000 and 5000 sizes;
C                          the former required format changes.)
C 20. 3/22/94  Stoeckley  Increase file size from 50000 to 200000.
C 19. 8/25/93  Stoeckley  Add option Z in two places.
C 18. 7/28/93  Troutt,    Change file integration, preserving the nils,
C              Stoeckley   not to reset the value in the last ground
C                          position back to a nil.  Stoeckley made this
C                          fix after Troutt made it in VMS.
C 17. 6/25/93  McDaniel,  In OPTIONS, change TYPE=MUTE to TYPE=RESID when
C              Michell     SISC option is selected.  This will allow 
C                          CPS header word #43 to be properly updated
C                          when file is applied using SHFT.
C 16. 6/22/93  Michell,   Increase the number of allowed comment cards,
C              Stoeckley   and test for number of comment cards.
C 15. 5/24/93  McDaniel   Add OPTIONS, to allow the user to more easily
C                          convert the .INC files to CBYT .MUTE files,
C                          and also to more easily convert from .MUTE files
C                          to Integrated, Averaged, .SISC files. 
C 14. 11/30/92 Stoeckley  Fix LFN bug in OPTIONC, and modify expansion
C                          of file names in INQUIRE statements.
C 13. 05/22/92 Stoeckley  Allow both lower and upper case input.
C                          Also change OPEN calls.
C 12. 05/30/91 Stoeckley  Add option for blanking out unwanted columns
c                         while reading foreign static file.
C 11. 03/08/91 Stoeckley  Allow interpolations to work also in the
C                         crossline direction.
C 10. 01/07/91 Stoeckley  Modify method of integrating static files.
C  9. 07/27/90 Stoeckley  Add non-uniform GP option to "foreign" files.
C  8. 06/01/90 Stoeckley  Add output of a "foreign" static file.
C  7. 05/14/90 Stoeckley  Add smoothing in the Y direction.
C  6. 02/07/90 Stoeckley  Add ability to set Y1 and YINC when building
C                         a static file when the Y header is zero.  Also
C                         add CARRIAGECONTROL='LIST' to OPEN statements
C                         to allow use of TYPE and PRINT commands.
C  5. 10/09/89 Stoeckley  Switch the file backward in option C if the
C                         transformation reverses the ground position
C                         increment.  Also suppress excessive number of
C                         comment cards generated by edits.  Also add
C                         clarification to options AV,ADD,SUB,SPLICE,
C                         and allow new option N at that point.
C  4. 08/14/89 Stoeckley  Generalize the conversion of CONSEIS static
C                         files to CPS so that it can be easily done
C                         for other types of "foreign" static files.
C                         Also remove an incorrect print statement.
C                         Also make minor improvement in edit option.
C                         Also generalize the option for changing ground
C                         positions.
C  3. 06/28/89 Stoeckley  Fix bug in end option for running average;
C                         change running average from points to ground
C                         position range; add resetting of TYPE when
C                         integrating SISC file; spruce up the editing.
C  2. 06/26/89 Stoeckley  Add clarification of hard copy printer-plots,
C                         and prohibit use of same name for plot file
C                         and static file.
C  1. 05/09/89 Stoeckley  Add conversion of CONSEIS static file to CPS.
C-----------------------------------------------------------------------
C                          PROGRAMMING NOTES
C
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                             IN THIS MODULE
C
C  Subroutines, functions, and entries:  ISEP (program name),
C      OPTIONL, OPTIONP, OPTIONH, OPTIONE, OPTIONG, OPTIONI, OPTIONR,
C      OPTIONN, OPTIONNX, OPTIONNY, OPTIONNN,
C      OPTIONM, OPTIONA, OPTIONC, OPTIONT, OPTIONJ, OPTIONS, 
C      OPTIONZ, OPTIONV1, OPTIONV2, CFILE2, CFILE3, OPTIONGG,
C      SFILE, FFILE, RFILE, BFILE, HFILE, CFILE, LISTPREV1, LISTPREV2,
C      OPTHELP, OPTAV, OPTADD, OPTSUB, SPLICE, MATCH, LISTPREV3,
C      MOVE, OPENFILE, HARDSTAT, HARDHEAD, HARDLINE, OPTIONC1,
C      STATHEAD, STATLINE, IPLOT, IPICK, SHOW5, SHOW10,
C      HIT,  HITQ,  HITI,  HITX,  HITC,  HITXX,  HITII,  HITIIII,
C      HIT2, HITQ2, HITI2, HITX2, HITC2, HITXX2, HITII2, HITIIII2,
C      LIMITS, QSORT22, RUNAV, QTRIM, TRUNK, GPQUEST, HOW,
C      PLOTL, PLOTXY, PLOTY, PLOTX2, PLOTLINE, REVERSE,
C      PICTURE, PRETTY, PRETTY2, PICLINE, PLOT1, PLOT2. 
C
C  Common blocks:  PLOTZ1, PLOTZ2.
C-----------------------------------------------------------------------
C            LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C
C      STATRII, STATRCC, STATREAD, DATE_AND_TIME, RANDOM,
C      STATWII, STATWCC, STATWRIT, ADDEXT_REPLACE 
C-----------------------------------------------------------------------
C\END DOC
C     INTERACTIVE STATIC EDITING PROGRAM.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (NNNN=40000000,KKKK=5000)
      DIMENSION S(NNNN),S2(NNNN),S3(NNNN),SAVE(NNNN)
CCC   DIMENSION S(NX,NY)   !   S(IX,IY)=S(I)  WHERE  I=IX+(IY-1)*NX.
      CHARACTER*8 OPTION,TYPE
      CHARACTER*80 CARD(KKKK),VAXFILE
      save s,s2,s3,save,card
C----------INITIALIZATION.
1     PRINT *, 'TYPE  READ   TO READ A CPS STATIC FILE      (DEFAULT)'
      PRINT *, 'TYPE  BUILD  TO BUILD A NEW STATIC FILE'
      PRINT *, 'TYPE  CYBER  TO READ A GENERIC STATIC FILE',
     $                                ' (simple columns)'
CCC   PRINT *, 'TYPE  SISC   TO WORK WITH SISC STATIC FILES'
      PRINT *, 'TYPE  X      TO EXIT THE PROGRAM'
      OPTION='READ'
      CALL HITC (OPTION)
      IF (OPTION.EQ.'READ'.OR.OPTION.EQ.'BUILD'.OR.OPTION.EQ.'X'
     $    .OR.OPTION.EQ.'CYBER') GO TO 5
CCC   IF (OPTION.EQ.'SISC') CALL SISC (S,S2,S3,SAVE,NNNN,CARD,KKKK)
      GO TO 1
C----------LIST MENU.
2     PRINT *, 'TYPE  L   TO LIST STATIC FILE ATTRIBUTES'
      PRINT *, 'TYPE  P   TO PLOT THE FILE'
      PRINT *, 'TYPE  H   TO MAKE A HORIZONTAL PLOT'
      PRINT *, 'TYPE  E   TO EDIT THE FILE'
      PRINT *, 'TYPE  G   TO GRADE STATIC VALUES BETWEEN TWO GROUND',
     $                               ' POSITIONS'
      PRINT *, 'TYPE  GG  TO GRADE STATIC VALUES OVER A RECTANGULAR',
     $                               ' AREA'
      PRINT *, 'TYPE  I   TO INTEGRATE THE FILE'
      PRINT *, 'TYPE  R   TO REMOVE A RUNNING AVERAGE'
      PRINT *, 'TYPE  S   TO SMOOTH THE FILE'
      PRINT *, 'TYPE  N   TO REPLACE NILS WITH INTERPOLATED VALUES'
      PRINT *, 'TYPE  NX  TO INTERPOLATE NILS IN X DIRECTION',
     $                                            ' (no extrapolation)'
      PRINT *, 'TYPE  NY  TO INTERPOLATE NILS IN Y DIRECTION',
     $                                            ' (no extrapolation)'
      PRINT *, 'TYPE  NN  TO INTERPOLATE NILS ONLY FROM NEARBY VALUES'
      PRINT *, 'TYPE  Z   TO REPLACE NILS WITH ZEROES'
      PRINT *, 'TYPE  V1  TO REPLACE NILS WITH SPECIFIED VALUE'
      PRINT *, 'TYPE  V2  TO REPLACE SPECIFIED VALUES WITH NILS'
      PRINT *, 'TYPE  M   TO MULTIPLY THE STATIC VALUES BY A CONSTANT'
      PRINT *, 'TYPE  A   TO ADD A CONSTANT TO THE STATIC VALUES'
      PRINT *, 'TYPE  C   TO CHANGE THE GROUND POSITIONS'
      PRINT *, 'TYPE  T   TO TRUNCATE (OR EXTEND OR RESAMPLE) THE FILE'
      PRINT *, 'TYPE  J   TO JOIN OR COMPARE FILE WITH ANOTHER FILE'
      PRINT *, 'TYPE  SAVE    TO SAVE A CPS STATIC FILE'
      PRINT *, 'TYPE  FOREIGN TO SAVE A GENERIC STATIC FILE',
     $                                            ' (simple columns)'
      PRINT *, 'TYPE  READ    TO READ ANOTHER CPS STATIC FILE'
      PRINT *, 'TYPE  BUILD   TO BUILD A NEW STATIC FILE'
      PRINT *, 'TYPE  CYBER   TO READ A GENERIC STATIC FILE',
     $                                            ' (simple columns)'
CCC   PRINT *, 'TYPE  SISC    TO WORK WITH SISC STATIC FILES'
      PRINT *, 'TYPE  DELETE  TO DELETE ALL E,G,GG,I,R,S,N,NX,NY,NN,Z,',
     $                                          'V1,V2,M,A'
      PRINT *, '                     DONE SINCE LAST C,T,J,READ,BUILD'
      PRINT *, 'TYPE  HARD    TO MAKE A HARD COPY PRINTER-PLOT'
      PRINT *, 'TYPE  X  TO EXIT THE PROGRAM'
4     OPTION=' '
      CALL HITC (OPTION)
C----------ACT ON THE CHOSEN OPTION.
5     IF (OPTION.EQ.'L') THEN
           CALL OPTIONL (6,VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,
     $        XINC,YINC,NX,NY,CARD,NCARD,S)
      ELSE IF (OPTION.EQ.'P') THEN
           CALL OPTIONP (1,X1,Y1,XINC,YINC,NX,NY,S,SAVE,S3)
      ELSE IF (OPTION.EQ.'H') THEN
           CALL OPTIONH (1,X1,Y1,XINC,YINC,NX,NY,S,SAVE,S3)
      ELSE IF (OPTION.EQ.'E') THEN
           CALL OPTIONE (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
      ELSE IF (OPTION.EQ.'G') THEN
           CALL OPTIONG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
      ELSE IF (OPTION.EQ.'GG') THEN
           CALL OPTIONGG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
      ELSE IF (OPTION.EQ.'I') THEN
           CALL OPTIONI (TYPE,S,S2,NX,NY,CARD,NCARD)
           CALL OPTIONN (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'R'.OR.OPTION.EQ.'S') THEN
           CALL OPTIONR (OPTION,S,S2,XINC,YINC,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'N') THEN
           CALL OPTIONN (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'NX') THEN
           CALL OPTIONNX (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'NY') THEN
           CALL OPTIONNY (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'NN') THEN
           CALL OPTIONNN (S,S3,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'Z') THEN
           CALL OPTIONZ (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'V1') THEN
           CALL OPTIONV1 (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'V2') THEN
           CALL OPTIONV2 (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'M') THEN
           CALL OPTIONM (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'A') THEN
           CALL OPTIONA (S,NX,NY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'C') THEN
           CALL OPTIONC (NHX,NHY,NHX3,NHY3,
     $        X1,Y1,XINC,YINC,NX,NY,S,CARD,NCARD)
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
      ELSE IF (OPTION.EQ.'T') THEN
           CALL OPTIONT (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD,S2)
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
      ELSE IF (OPTION.EQ.'J') THEN
           CALL OPTIONJ (TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,S,NCARD,CARD,SAVE,S2,S3,NNNN,KKKK)
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
      ELSE IF (OPTION.EQ.'SAVE') THEN
           CALL SFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S)
      ELSE IF (OPTION.EQ.'FOREIGN') THEN
           CALL FFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S)
      ELSE IF (OPTION.EQ.'READ') THEN
           CALL RFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S,NNNN,KKKK)
           IF (VAXFILE.EQ.' ') GO TO 1
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
      ELSE IF (OPTION.EQ.'BUILD') THEN
           CALL BFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S,NNNN)
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
CCC   ELSE IF (OPTION.EQ.'SISC') THEN
CCC        CALL OPTIONS (OPTION,VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,S2,
CCC  $        X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,A,NNNN)
      ELSE IF (OPTION.EQ.'CYBER') THEN
           CALL CFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S,S2,S3,NNNN)
           IF (VAXFILE.EQ.' ') GO TO 1
           CALL MOVE (S,SAVE,NX*NY)
           NCAVE=NCARD
      ELSE IF (OPTION.EQ.'DELETE') THEN
           CALL MOVE (SAVE,S,NX*NY)
           NCARD=NCAVE
      ELSE IF (OPTION.EQ.'HARD') THEN
           CALL HFILE (VAXFILE,TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $        NX,NY,CARD,NCARD,S)
C----------EXIT PROGRAM.
      ELSE IF (OPTION.EQ.'X') THEN
           STOP
C----------GO BACK FOR ANOTHER OPTION.
      ELSE
           GO TO 2
      END IF
      PRINT *, 'CHOOSE DESIRED OPTION (OR HIT RETURN FOR MENU)'
      GO TO 4
C----------ERROR.
ccc999   PRINT *, 'ERROR HAS OCCURRED'
ccc      GO TO 1
      END
                                


                               
         

      SUBROUTINE BFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,NNNN)
C     BUILD STATIC FILE FILLED WITH NILS.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NNNN)
      CHARACTER*80 VAXFILE,CARD(*)
      CHARACTER*8 TYPE
C----------GET TYPE OF STATIC FILE.
      VAXFILE='**NO STATIC FILE NAME**'
      PRINT *, 'WHAT TYPE OF STATIC FILE WILL THIS BE?'
      PRINT *, 'TYPE  1  FOR TYPE=DATUM'
      PRINT *, 'TYPE  2  FOR TYPE=REFR'
      PRINT *, 'TYPE  3  FOR TYPE=RESID'
      PRINT *, 'OR TYPE ANY NAME UP TO 8 CHARACTERS LONG'
      TYPE='MISC'
      CALL HITC2 (TYPE)
      IF (TYPE.EQ.'1') TYPE='DATUM'                          
      IF (TYPE.EQ.'2') TYPE='REFR'
      IF (TYPE.EQ.'3') TYPE='RESID'
C----------GET HEADER WORDS.
10    PRINT *, 'TYPE FOUR HEADER WORD NUMBERS.  THE MOST COMMON ARE:'
      PRINT *, '   GROUP FILE:   9  0  0  0'
      PRINT *, '  SOURCE FILE:  33 34  0  0   or   33  0  0  0  ',
     $   ' or   46 0  0  0'
      PRINT *, 'RECEIVER FILE:  35 36  0  0   or   35  0  0  0  ',
     $   ' or   47 0  0  0'
      PRINT *, '     S=R FILE:  35 36 33 34   or   35  0 33  0  ',
     $   ' or   47 0 46  0'
      NHX=47
      NHY=0
      NHX2=0
      NHY2=0
      CALL HITIIII2 (NHX,NHY,NHX2,NHY2)
      IF (NHX.EQ.0) THEN
           PRINT *, 'FIRST HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 10
      END IF
C----------GET INFORMATION IN X DIRECTION.
      X1=1.
      XINC=1.
      PRINT *, 'TYPE INITIAL X GROUND POSITION  (DEFAULT ',X1,')'
      CALL HITX (X1)
21    PRINT *, 'TYPE X GROUND POSITION INCREMENT  (DEFAULT ',XINC,')'
      CALL HITX (XINC)
      IF (XINC.EQ.0.) THEN
           PRINT *, 'VALUE CANNOT BE ZERO -- TRY AGAIN'
           GO TO 21
      END IF
      XEND=X1-XINC
22    PRINT *, 'TYPE LAST X GROUND POSITION  (NO DEFAULT)'
      CALL HITX (XEND)
      NX=NINT((XEND-X1)/XINC)+1
      IF (NX.LE.0) THEN
           PRINT *, 'VALUE TOO SMALL -- TRY AGAIN'
           GO TO 22
      ELSE IF (NX.GT.NNNN) THEN
           PRINT *, 'TOO MANY GROUND POSITIONS TO FIT IN MEMORY',
     $        ' -- TRY AGAIN'
           GO TO 22
      END IF
C----------GET INFORMATION IN Y DIRECTION.
      Y1=1.
      YINC=1.
      NY=1
      IF (NHY.EQ.0) THEN
           Y1=0.
           PRINT *, 'ANSWER THE FOLLOWING TWO QUESTIONS',
     $        ' EVEN THOUGH THE Y HEADER WORD IS ZERO:'
           PRINT *, ' '
      END IF
      PRINT *, 'TYPE INITIAL Y GROUND POSITION  (DEFAULT ',Y1,')'
      CALL HITX (Y1)
31    PRINT *, 'TYPE Y GROUND POSITION INCREMENT  (DEFAULT ',YINC,')'
      CALL HITX (YINC)
      IF (YINC.EQ.0.) THEN
           PRINT *, 'VALUE CANNOT BE ZERO -- TRY AGAIN'
           GO TO 31
      END IF
      IF (NHY.EQ.0) GO TO 40
      YEND=Y1-YINC
32    PRINT *, 'TYPE LAST Y GROUND POSITION  (NO DEFAULT)'
      CALL HITX (YEND)
      NY=NINT((YEND-Y1)/YINC)+1
      IF (NY.LE.0) THEN
           PRINT *, 'VALUE TOO SMALL -- TRY AGAIN'
           GO TO 32
      ELSE IF (NX*NY.GT.NNNN) THEN
           PRINT *, 'TOO MANY GROUND POSITIONS TO FIT IN MEMORY',
     $        ' -- TRY AGAIN'
           GO TO 32
      END IF
C----------SET STATIC FILE TO NILS.
40    N=NX*NY    
      DO 50 I=1,N
50    S(I)=FNIL
C----------FINISH UP.
      NCARD=1
      CARD(NCARD)='ISEP- BUILD NEW STATIC FILE AND FILL WITH NILS'
      PRINT *, 'NEW STATIC FILE BUILT AND FILLED WITH NILS'
      PRINT *, 'USE OPTION E TO FILL IN YOUR DESIRED STATIC VALUES'
      RETURN
      END

                                                 
                   
      SUBROUTINE CFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,SS,KK,NNNN)
C     READ A GENERIC STATIC FILE.
C     RETURNS VAXFILE=' ' IF UNSUCCESSFUL.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NNNN),SS(NNNN),KK(NNNN)
      CHARACTER*80 VAXFILE,CARD(*)
      CHARACTER*8 TYPE
      CHARACTER*4 Q
C----------FIND OUT WHETHER THIS IS A 2D OR 3D FILE.
222   PRINT *, 'IS THIS A 2D OR 3D GENERIC STATIC FILE? (DEFAULT 2D)'
      Q='2D'
      CALL HITC (Q)
      if (q(1:1).eq.'2') then
           call CFILE2 (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,SS,KK,NNNN)
      else if (q(1:1).eq.'3') then
           call CFILE3 (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,SS,KK,NNNN)
      else
           print *, 'INVALID OPTION - TRY AGAIN'
           go to 222
      end if 
      return
      end
                   




      SUBROUTINE CFILE3 (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,SS,KK,NNNN)
C     READ A 3D GENERIC STATIC FILE.
C     RETURNS VAXFILE=' ' IF UNSUCCESSFUL.
C----------DIMENSION AND DATA STATEMENTS.
      use string_module
      use named_constants_module
      PARAMETER (LFN=77)
      DIMENSION S(NNNN),SS(NNNN),KK(NNNN)
      CHARACTER*80 VAXFILE,CARD(*),MASK,MASK2
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
      CHARACTER*4 Q3
      logical orderxyv,orderxvy,orderyxv,orderyvx,ordervxy,ordervyx
C----------GET NAME OF 3D GENERIC STATIC FILE.
      CALL LISTPREV1 (DDDDTTTT)
777   CLOSE (LFN)
      PRINT *, 'TYPE NAME OF 3D GENERIC STATIC FILE'
      PRINT *, '(OR HIT RETURN TO QUIT)'
      VAXFILE=' '
      READ (*,1000,END=111) VAXFILE
1000  FORMAT (A80)
      IF (VAXFILE.EQ.' ') RETURN
C----------OPEN AND CHECK STATIC FILE.
      CALL OPENFILE (LFN,VAXFILE,'READ',*777)
      REWIND LFN
5     PRINT *, 'FIRST FEW CARDS OF 3D GENERIC STATIC FILE ARE:'
      DO I=1,5
           READ (LFN,1000,ERR=6,END=6) CARD(1)      
           write(*,1000) CARD(1)
      end do
6     PRINT *, 'THIS FILE SHOULD CONTAIN THREE VALUES (XGP,YGP,STATIC)'
      PRINT *, 'ON EACH CARD IN FREE-FORM FORMAT.'
      PRINT *, ' '
C----------ASK ONE QUESTION.
      orderxyv=.true.
      orderxvy=.false.
      orderyxv=.false.
      orderyvx=.false.
      ordervxy=.false.
      ordervyx=.false.
      PRINT *, 'DO YOU WANT TO MASK OUT SOME OF THE COLUMNS, WHICH'
      PRINT *, 'CONTAIN UNWANTED INFORMATION, OR SPECIFY A DIFFERENT'
      PRINT *, 'ORDER FOR (XGP,YGP,STATIC)?  (DEFAULT N)'
      Q3='N'
      CALL HITQ (Q3)
      IF (Q3.EQ.'Y') THEN
           REWIND LFN
           PRINT *, 'FIRST FEW CARDS OF 3D GENERIC STATIC FILE ARE:'
           DO I=1,5
                READ (LFN,'(A80)',ERR=538,END=538) CARD(1)      
                write (*,1000) CARD(1)      
           end do
538     PRINT *,'TYPE X''s IN THE COLUMNS CONTAINING X GROUND POSITIONS'
        PRINT *,'TYPE Y''s IN THE COLUMNS CONTAINING Y GROUND POSITIONS'
        PRINT *,'TYPE V''s IN THE COLUMNS CONTAINING STATIC VALUES'
           READ 1000, MASK
!!!!       call convert_to_upper (mask)
           call string_to_upper (mask)
           iiixfirst=0
           iiixlast =0
           iiiyfirst=0
           DO I=1,80
                IF (MASK(I:I).EQ.'X'.and.iiixfirst.eq.0) IIIxFIRST=i
                IF (MASK(I:I).EQ.'Y'.and.iiiyfirst.eq.0) IIIyFIRST=i
                IF (MASK(I:I).EQ.'V'.and.iiivfirst.eq.0) IIIvFIRST=i
           end do
           if (iiixfirst.eq.0) then
                PRINT *, 'ERROR -- NO X''s TYPED -- TRY AGAIN'
                GO TO 538
           else if (iiiyfirst.eq.0) then
                PRINT *, 'ERROR -- NO Y''s TYPED -- TRY AGAIN'
                GO TO 538
           else if (iiivfirst.eq.0) then
                PRINT *, 'ERROR -- NO V''s TYPED -- TRY AGAIN'
                GO TO 538
           end if
           orderxyv=(iiixfirst.lt.iiiyfirst.and.iiiyfirst.lt.iiivfirst)
           orderxvy=(iiixfirst.lt.iiivfirst.and.iiivfirst.lt.iiiyfirst)
           orderyxv=(iiiyfirst.lt.iiixfirst.and.iiixfirst.lt.iiivfirst)
           orderyvx=(iiiyfirst.lt.iiivfirst.and.iiivfirst.lt.iiixfirst)
           ordervxy=(iiivfirst.lt.iiixfirst.and.iiixfirst.lt.iiiyfirst)
           ordervyx=(iiivfirst.lt.iiiyfirst.and.iiiyfirst.lt.iiixfirst)
      END IF
C----------READ STATIC FILE FIRST TIME.
      REWIND LFN
      xmin= 9.9e30
      xmax=-9.9e30
      ymin= 9.9e30
      ymax=-9.9e30
      kount=0
      numnils=0
10    READ (LFN,1000,ERR=778,END=11) MASK2
      if (q3.eq.'Y') then
          DO II=1,80
               IF (MASK(II:II).EQ.' ') MASK2(II:II)=' '
          END DO
      end if
      if (orderxyv) READ (MASK2,*,ERR=779,END=779) xx,yy,vv
      if (orderxvy) READ (MASK2,*,ERR=779,END=779) xx,vv,yy
      if (orderyxv) READ (MASK2,*,ERR=779,END=779) yy,xx,vv
      if (orderyvx) READ (MASK2,*,ERR=779,END=779) yy,vv,xx
      if (ordervxy) READ (MASK2,*,ERR=779,END=779) vv,xx,yy
      if (ordervyx) READ (MASK2,*,ERR=779,END=779) vv,yy,xx
      xmin=amin1(xx,xmin)
      xmax=amax1(xx,xmax)
      ymin=amin1(yy,ymin)
      ymax=amax1(yy,ymax)
      kount=kount+1
      if (vv.eq.FNIL) numnils=numnils+1
      go to 10
11    print *, 'static file contains ',kount,' card images'
      print *, numnils,' of these static values are nil'
      PRINT *, 'X GROUND POSITIONS RANGE FROM ',XMIN,' TO ',XMAX
      PRINT *, 'Y GROUND POSITIONS RANGE FROM ',YMIN,' TO ',YMAX
C----------GET INFORMATION IN X DIRECTION.
      X1=XMIN
      XINC=1.
1234  print *, '----------------------------------------------------'
      PRINT *, 'TYPE FIRST DESIRED X GROUND POSITION  (DEFAULT ',
     $        X1,')  (min ',xmin,')'
      CALL HITX (X1)
1235  PRINT *, 'TYPE DESIRED X GROUND POSITION INCREMENT  (DEFAULT ',
     $        XINC,')'
      CALL HITX (XINC)
      IF (XINC.LE.0.) THEN
           PRINT *, 'THIS CANNOT BE ZERO OR NEGATIVE -- TRY AGAIN'
           GO TO 1235
      END IF
      NX=1+NINT((XMAX-X1)/XINC)
      if (nx.lt.1) nx=1
      XEND=X1+(NX-1)*XINC
      PRINT *, 'TYPE LAST DESIRED X GROUND POSITION  (DEFAULT ',
     $        XEND,')  (max ',xmax,')'
      CALL HITX (XEND)
      IF (XEND.LT.X1) THEN
           PRINT *, 'THIS CANNOT BE LESS THAN THE FIRST GROUND',
     $                               ' POSITION -- TRY AGAIN'
           GO TO 1235
      END IF
      NX=1+NINT((XEND-X1)/XINC)
      print *, 'THERE WILL BE ',NX,' X GROUND POSITIONS IN THIS FILE'
      if (nx.gt.nnnn) then
           print *, 'STATIC FILE WILL NOT FIT INTO MEMORY.'
           print *, 'YOU MUST REDUCE THE X GROUND POSITION RANGE'
           print *, 'OR INCREASE THE X GROUND POSITION INCREMENT.'
           print *, 'TRY AGAIN.'
           go to 1234
      end if
C----------GET INFORMATION IN Y DIRECTION.
      Y1=YMIN
      YINC=1.
5234  print *, '----------------------------------------------------'
      PRINT *, 'TYPE FIRST DESIRED Y GROUND POSITION  (DEFAULT ',
     $        Y1,')  (min ',ymin,')'
      CALL HITX (Y1)
5235  PRINT *, 'TYPE DESIRED Y GROUND POSITION INCREMENT  (DEFAULT ',
     $        YINC,')'
      CALL HITX (YINC)
      IF (YINC.LE.0.) THEN
           PRINT *, 'THIS CANNOT BE ZERO OR NEGATIVE -- TRY AGAIN'
           GO TO 5235
      END IF
      NY=1+NINT((YMAX-Y1)/YINC)
      if (nY.lt.1) nY=1
      YEND=Y1+(NY-1)*YINC
      PRINT *, 'TYPE LAST DESIRED Y GROUND POSITION  (DEFAULT ',
     $        YEND,')  (max ',ymax,')'
      CALL HITX (YEND)
      IF (YEND.LT.Y1) THEN
           PRINT *, 'THIS CANNOT BE LESS THAN THE FIRST GROUND',
     $                               ' POSITION -- TRY AGAIN'
           GO TO 5235
      END IF
      NY=1+NINT((YEND-Y1)/YINC)
      print *, 'THERE WILL BE ',NX,' X GROUND POSITIONS IN THIS FILE'
      print *, 'THERE WILL BE ',NY,' Y GROUND POSITIONS IN THIS FILE'
      print *, 'THERE WILL BE ',NX*NY,' GROUND POSITIONS IN THIS FILE'
      if (nx*nY.gt.nnnn) then
           print *, 'STATIC FILE WILL NOT FIT INTO MEMORY.'
           print *, 'YOU MUST REDUCE THE X OR Y GROUND POSITION RANGE'
           print *, 'OR INCREASE THE X OR Y GROUND POSITION INCREMENT.'
           print *, 'TRY AGAIN.'
           go to 1234
      end if
c----------READ STATIC VALUES.
      n=nx*ny
      do j=1,n
           ss(j)=0.0
           kk(j)=0
      end do
      iuse=0
      numnils=0
      numnils_out=0
      rewind lfn
      do i=1,kount
           READ (LFN,1000,ERR=778,END=778) MASK2
           if (q3.eq.'Y') then
               DO II=1,80
                    IF (MASK(II:II).EQ.' ') MASK2(II:II)=' '
               END DO
           end if
           if (orderxyv) READ (MASK2,*,ERR=779,END=779) xx,yy,vv
           if (orderxvy) READ (MASK2,*,ERR=779,END=779) xx,vv,yy
           if (orderyxv) READ (MASK2,*,ERR=779,END=779) yy,xx,vv
           if (orderyvx) READ (MASK2,*,ERR=779,END=779) yy,vv,xx
           if (ordervxy) READ (MASK2,*,ERR=779,END=779) vv,xx,yy
           if (ordervyx) READ (MASK2,*,ERR=779,END=779) vv,yy,xx
           ix=1+nint((xx-x1)/xinc)
           iy=1+nint((yy-y1)/yinc)
           j=ix+(iy-1)*nx
           if (ix.ge.1.and.ix.le.nx.and.iy.ge.1.and.iy.le.ny) then
                if (vv.eq.FNIL) then
                     numnils=numnils+1
                else
                     ss(j)=ss(j)+vv
                     kk(j)=kk(j)+1
                end if
                iuse=iuse+1
           else
                if(vv.eq.FNIL) numnils_out=numnils_out+1
           end if
      end do
      print *, '----------------------------------------------------'
      print *, iuse,' static values were used from disk file'
      print *, numnils,' static values used have nil values'
      print *, kount-iuse,' static values were skipped (out of range)'
      print *, numnils_out,' static values skipped have nil values'
      imult=0
      numnils=0
      do j=1,n
           if (kk(j).eq.0) then
                s(j)=FNIL
           else if (kk(j).eq.1) then
                s(j)=ss(j)
           else
                s(j)=ss(j)/kk(j)
                imult=imult+1
           end if
           if (s(j).eq.FNIL) numnils=numnils+1
      end do
      print *, '----------------------------------------------------'
      print *, 'THERE ARE ',NX*NY,' GROUND POSITIONS IN THIS FILE'
      print *, imult,' static values are averages of multiple entries',
     $                                  ' in disk file'
      print *, numnils,' static values are nil'
      CLOSE (LFN)
      CALL LISTPREV3 ('FOUND   ','GENERIC ',VAXFILE)
C----------GET TYPE OF STATIC FILE.
      print *, '----------------------------------------------------'
      PRINT *, 'WHAT TYPE OF STATIC FILE WILL THIS BE?'
      PRINT *, 'TYPE  1  FOR TYPE=DATUM'
      PRINT *, 'TYPE  2  FOR TYPE=REFR'
      PRINT *, 'TYPE  3  FOR TYPE=RESID'
      PRINT *, 'OR TYPE ANY NAME UP TO 8 CHARACTERS LONG'
      TYPE='MISC'
      CALL HITC2 (TYPE)
      IF (TYPE.EQ.'1') TYPE='DATUM'                          
      IF (TYPE.EQ.'2') TYPE='REFR'
      IF (TYPE.EQ.'3') TYPE='RESID'
C----------GET HEADER WORDS.
50    print *, '----------------------------------------------------'
      PRINT *, 'TYPE FOUR HEADER WORD NUMBERS.  RELEVANT ONES ARE',
     $   ' PROBABLY:'
      PRINT *, '  SOURCE FILE:  33  34   0   0'
      PRINT *, 'RECEIVER FILE:  35  36   0   0'
      PRINT *, '     S=R FILE:  33  34  35  36'
      NHX=33
      NHY=34
      NHX2=0
      NHY2=0
      CALL HITIIII2 (NHX,NHY,NHX2,NHY2)
      IF (NHX.EQ.0) THEN
           PRINT *, 'FIRST HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 50
      END IF
      IF (NHY.EQ.0) THEN
           PRINT *, 'SECOND HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 50
      END IF
C----------FINISH UP.
      print *, '----------------------------------------------------'
      NCARD=1
      CARD(NCARD)='ISEP- '//DDDDTTTT//'READ 3D GENERIC FILE '//VAXFILE
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SUCCESSFULLY READ 3D GENERIC STATIC FILE'
      PRINT *, 'SUCCESSFULLY READ 3D GENERIC STATIC FILE'
111   RETURN
C----------ERRORS.
778   print *, 'READ ERROR'
      go to 777
779   print *, 'ERROR IN SPECIFYING COLUMNS TO READ -- TRY AGAIN'
      go to 5
      END

                                                 
                   


      SUBROUTINE CFILE2 (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,SS,KK,NNNN)
C     READ A 2D GENERIC STATIC FILE.
C     RETURNS VAXFILE=' ' IF UNSUCCESSFUL.
C----------DIMENSION AND DATA STATEMENTS.
      use string_module
      use named_constants_module
      PARAMETER (LFN=77)
      DIMENSION S(NNNN),SS(NNNN),KK(NNNN)
      CHARACTER*80 VAXFILE,CARD(*),MASK,MASK2
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
      CHARACTER*4 Q,Q2,Q3
C----------GET NAME OF 2D GENERIC STATIC FILE.
      CALL LISTPREV1 (DDDDTTTT)
777   CLOSE (LFN)
      PRINT *, 'TYPE NAME OF 2D GENERIC STATIC FILE',
     $                                    ' (simple column format)'
      PRINT *, '(OR HIT RETURN TO QUIT)'
      VAXFILE=' '
      READ (*,1000,END=111) VAXFILE
1000  FORMAT (A80)
      IF (VAXFILE.EQ.' ') RETURN
C----------OPEN AND CHECK STATIC FILE.
      CALL OPENFILE (LFN,VAXFILE,'READ',*777)
      REWIND LFN
      PRINT *, 'FIRST FEW CARDS OF 2D GENERIC STATIC FILE ARE:'
      DO 5 I=1,5
      READ (LFN,1000,ERR=6,END=6) CARD(1)      
5     write(*,1000) CARD(1)
6     PRINT *, 'THIS FILE SHOULD CONTAIN PAIRS OF VALUES (GP1,STATIC1,'
      PRINT *, 'GP2,STATIC2,GP3,STATIC3,...), WITH ANY NUMBER OF VALUES'
      PRINT *, 'ON A CARD IN FREE-FORM FORMAT.  THE NUMBER OF VALUES IN'
      PRINT *, 'THE FILE MUST BE TWICE THE NUMBER OF GROUND POSITIONS.'
      PRINT *, 'THIS MUST BE A 2-D (NOT 3-D) FILE (X ONLY - NOT X,Y).'
      PRINT *, ' '
C----------ASK THREE QUESTIONS.
      PRINT *, 'DOES THE FIRST CARD CONTAIN AN INTEGER REPRESENTING'
      PRINT *, 'THE NUMBER OF VALUES IN THE FILE?  (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
      PRINT *, 'DO THE GROUND POSITIONS INCREMENT (OR DECREMENT)',
     $   ' UNIFORMLY?  (DEFAULT Y)'
      Q2='Y'
      CALL HITQ (Q2)
      PRINT *, 'DO YOU WANT TO MASK OUT SOME OF THE COLUMNS, WHICH'
      PRINT *, 'CONTAIN UNWANTED NUMBERS?  (DEFAULT N)'
      Q3='N'
      CALL HITQ (Q3)
      IF (Q3.EQ.'Y') THEN
           REWIND LFN
           PRINT *, 'FIRST FEW CARDS OF 2D GENERIC STATIC FILE ARE:'
           DO 537 I=1,5
           READ (LFN,'(A80)',ERR=538,END=538) CARD(1)      
537        write (*,1000) CARD(1)      
538        PRINT *,'TYPE G''s IN THE COLUMN CONTAINING GROUND POSITIONS'
           PRINT *,'TYPE V''s IN THE COLUMN CONTAINING STATIC VALUES'
           READ 1000, MASK
!!!!!      call convert_to_upper (mask)
           call string_to_upper (mask)
           DO 539 I=1,80
           IF (MASK(I:I).NE.' ') THEN
                IF (MASK(I:I).EQ.'G') IIIFIRST=1
                IF (MASK(I:I).NE.'G') IIIFIRST=2
                GO TO 540
           END IF
539        CONTINUE
           PRINT *, 'ERROR -- TRY AGAIN'
           GO TO 538
540        CONTINUE
      END IF
C----------READ STATIC FILE.
      REWIND LFN
      IF (Q.EQ.'Y') THEN
           READ (LFN,*,ERR=777) N
           IF (N.GT.NNNN) THEN
                PRINT *, 'STATIC FILE TOO LARGE TO FIT IN MEMORY'
                GO TO 777
           END IF
           IF (Q3.EQ.'N') THEN
                READ (LFN,*,ERR=777) (S(I),I=1,N)
           ELSE
                DO I=1,N,2
                     READ (LFN,1000,ERR=777) MASK2
                     DO II=1,80
                          IF (MASK(II:II).EQ.' ') MASK2(II:II)=' '
                     END DO
                     READ (MASK2,*,ERR=777,END=777) AAAA,BBBB
                     IF (IIIFIRST.EQ.1) THEN
                          S(I  )=AAAA
                          S(I+1)=BBBB
                     ELSE
                          S(I  )=BBBB
                          S(I+1)=AAAA
                     END IF
                END DO
           END IF
      ELSE
           DO 10 I=1,NNNN
10         S(I)=FNIL
           IF (Q3.EQ.'N') THEN
                READ (LFN,*,ERR=777,END=11) (S(I),I=1,NNNN)
           ELSE
                DO I=1,NNNN,2
                     READ (LFN,1000,ERR=777,END=11) MASK2
                     DO II=1,80
                          IF (MASK(II:II).EQ.' ') MASK2(II:II)=' '
                     END DO
                     READ (MASK2,*,ERR=777,END=777) AAAA,BBBB
                     IF (IIIFIRST.EQ.1) THEN
                          S(I  )=AAAA
                          S(I+1)=BBBB
                     ELSE
                          S(I  )=BBBB
                          S(I+1)=AAAA
                     END IF
                END DO
           END IF
11         DO 12 I=NNNN,1,-1
           IF (S(I).NE.FNIL) GO TO 13                                
12         CONTINUE
           PRINT *, 'NO STATIC VALUES IN FILE'
           GO TO 777
13         N=I
           IF (N.EQ.NNNN) PRINT *, 'STATIC FILE MIGHT HAVE BEEN',
     $                 ' TRUNCATED'
      END IF
      CLOSE (LFN)
      CALL LISTPREV3 ('FOUND   ','GENERIC ',VAXFILE)
C----------GET TYPE OF STATIC FILE.
      PRINT *, 'WHAT TYPE OF STATIC FILE WILL THIS BE?'
      PRINT *, 'TYPE  1  FOR TYPE=DATUM'
      PRINT *, 'TYPE  2  FOR TYPE=REFR'
      PRINT *, 'TYPE  3  FOR TYPE=RESID'
      PRINT *, 'OR TYPE ANY NAME UP TO 8 CHARACTERS LONG'
      TYPE='MISC'
      CALL HITC2 (TYPE)
      IF (TYPE.EQ.'1') TYPE='DATUM'                          
      IF (TYPE.EQ.'2') TYPE='REFR'
      IF (TYPE.EQ.'3') TYPE='RESID'
C----------GET HEADER WORDS.
50    PRINT *, 'TYPE FOUR HEADER WORD NUMBERS.  RELEVANT ONES ARE',
     $   ' PROBABLY:'
      PRINT *, '   GROUP FILE:   9   0   0   0'
      PRINT *, '  SOURCE FILE:  46   0   0   0'
      PRINT *, 'RECEIVER FILE:  47   0   0   0'
      PRINT *, '     S=R FILE:  47   0  46   0'
      NHX=47
      NHY=0
      NHX2=0
      NHY2=0
      CALL HITIIII2 (NHX,NHY,NHX2,NHY2)
      IF (NHX.EQ.0) THEN
           PRINT *, 'FIRST HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 50
      END IF
C----------GET INFORMATION IN X DIRECTION.
      IF (Q2(1:1).EQ.'Y') THEN
           X1=S(1)
           XINC=S(3)-S(1)
           NX=N/2
      ELSE
           SSMIN=1.E30
           SSMAX=-1.E30
           DO 1234 I=1,N,2
           SSMIN=AMIN1(S(I),SSMIN)
1234       SSMAX=AMAX1(S(I),SSMAX)
           PRINT *, 'GROUND POSITIONS RANGE FROM ',SSMIN,' TO ',SSMAX
           X1=SSMIN
           PRINT *, 'TYPE FIRST DESIRED GROUND POSITION  (DEFAULT ',
     $        X1,')'
           CALL HITX (X1)
           XINC=1.
1235       PRINT *, 'TYPE DESIRED GROUND POSITION INCREMENT  (DEFAULT ',
     $        XINC,')'
           CALL HITX (XINC)
           IF (XINC.EQ.0.) THEN
                PRINT *, 'THIS CANNOT BE ZERO -- TRY AGAIN'
                GO TO 1235
           END IF
           NX=1+NINT((SSMAX-X1)/XINC)
           IF (NX.LE.0) THEN
                PRINT *, 'THE NUMBER OF GROUND POSITIONS IS ZERO',
     $             ' OR LESS -- YOU MAY NEED A NEGATIVE INCREMENT'
                GO TO 1235
           END IF
      END IF
C----------GET INFORMATION IN Y DIRECTION.
      Y1=0.
      YINC=1.
      NY=1
C----------SET STATIC FILE VALUES.
      IF (Q2(1:1).EQ.'Y') THEN
           DO 60 I=1,NX
60         S(I)=S(2*I)
      ELSE
           DO 1236 I=1,NX
           KK(I)=0
1236       SS(I)=0.
           DO 1237 I=1,N,2
           INDEX=1+NINT((S(I)-X1)/XINC)
           INDEX=MIN0(NX,MAX0(INDEX,1))
           KK(INDEX)=KK(INDEX)+1
1237       SS(INDEX)=SS(INDEX)+S(I+1)
           DO 1238 I=1,NX
           S(I)=FNIL
           IF (KK(I).GT.0) S(I)=SS(I)/KK(I)
1238       CONTINUE
      END IF
C----------FINISH UP.
      CALL REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
      NCARD=1
      CARD(NCARD)='ISEP- '//DDDDTTTT//'READ GENERIC FILE '//VAXFILE
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SUCCESSFULLY READ GENERIC STATIC FILE'
      PRINT *, 'SUCCESSFULLY READ GENERIC STATIC FILE'
111   RETURN
      END

                                                 
                   

      SUBROUTINE RFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,NNNN,KKKK)
C     ISEP SUBROUTINE FOR READING STATIC FILES.
C     USES UNIT NUMBER 77 FOR ALL READS.
C     RETURNS VAXFILE=' ' IF NO READ WAS DONE.
C----------DIMENSION AND DATA STATEMENTS.
      use statio_module
      character(len=80),pointer :: phist(:)
      real             ,pointer :: pstatics(:)
      integer err
      character(len=80) msg
      DIMENSION S(*)
      CHARACTER*(*) VAXFILE
      CHARACTER*80 CARD(*)
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
c     DATA IIII/23719/   ! removed 4/12/95
C----------GET STATIC FILE NAME TO READ.  
      CALL LISTPREV1 (DDDDTTTT)
777   continue
      PRINT *, 'TYPE NAME OF CPS STATIC FILE TO READ',
     $   '  (OR HIT RETURN TO QUIT)'
      VAXFILE=' '
      READ (*,1000,END=111) VAXFILE
1000  FORMAT (A80)
      IF (VAXFILE.EQ.' ') RETURN
      IF (VAXFILE.EQ.'NONE'.or.vaxfile.eq.'none') GO TO 333
C----------READ STATIC FILE.
      nullify (pstatics)
      nullify (phist)
      call statio_read_file (vaxfile,type,nhx,nhy,nhx2,nhy2,
     $                       x1,y1,xinc,yinc,nx,ny,pstatics,
     $                       err,msg,phist,ncard,'ISEP',0)
      print *, trim(msg)
      if (err /= STATIO_OK) then
           if (associated(pstatics)) deallocate (pstatics)
           if (associated(phist   )) deallocate (phist)
           go to 777
      end if
      IF (NX*NY.GT.NNNN) THEN
           if (associated(pstatics)) deallocate (pstatics)
           if (associated(phist   )) deallocate (phist)
           PRINT *, 'STATIC FILE TOO LARGE TO FIT IN MEMORY'
           GO TO 777
      END IF
      IF (NCARD.GT.KKKK-50) THEN
           PRINT*,'NCARD= ',NCARD
           PRINT*,'TOO MANY COMMENT CARDS - last ones lost'
           NCARD=KKKK-50
      ENDIF
      s(1:nx*ny)    = pstatics(1:nx*ny)
      card(1:ncard) = phist(1:ncard)
      if (associated(pstatics)) deallocate (pstatics)
      if (associated(phist   )) deallocate (phist)
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- '//DDDDTTTT//'READ '//VAXFILE
      CALL LISTPREV3 ('FOUND   ','STATIC  ',VAXFILE)
111   RETURN
C----------GENERATE TEST STATIC FILE.
333   TYPE='RESID'
      NHX=35
      NHY=36
      NHX2=0
      NHY2=0
      X1=20.          
      Y1=30.
      XINC=2.
      YINC=3.
      NX=26
      NY=37
      CARD(1)='ISEP- TEST FILE GENERATED WITH RANDOM NUMBERS'
      NCARD=1
      DO 20 J=1,NY
      DO 20 I=1,NX                                           
      INDEX=I+(J-1)*NX
c20   S(INDEX)=40.*(RAN(IIII)-0.5)  ! removed 4/12/95
c20   S(INDEX)=40.*(RANDOM() -0.5)  ! added   4/12/95 to make ansi-standard
      call random_number (ran)      ! added 7/14/2003 to make f90 ansi-standard
      S(INDEX)=40.*(RAN-0.5)        ! added 7/14/2003 to make f90 ansi-standard
20    continue                      ! added 7/14/2003 to make f90 ansi-standard
      RETURN
      END


   
      SUBROUTINE FFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S)
C     ISEP SUBROUTINE FOR SAVING STATIC FILES IN GENERIC FORMAT.
C     USES UNIT NUMBER 77 FOR ALL WRITES.
C     RETURNS VAXFILE UNCHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (LFN=77)
      DIMENSION S(NX,NY)
      CHARACTER*(*) VAXFILE
      CHARACTER*80 CARD(*),VAXFILE2
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
      CHARACTER*20 MSG
C----------GET STATIC FILE NAME TO SAVE.
      CALL LISTPREV1 (DDDDTTTT)
666   CLOSE (LFN)
      PRINT *, 'TYPE GENERIC STATIC FILE NAME',
     $   '  (OR HIT RETURN IF NO SAVE DESIRED)'
      VAXFILE2=' '
      READ (*,1000,END=111) VAXFILE2
1000  FORMAT (A80)
      IF (VAXFILE2.EQ.' ') RETURN
      CALL LISTPREV2 ('GENERIC ',VAXFILE2,*666)
C----------GET LINE NAME OR OTHER CHARACTER STRING.
      PRINT *, 'TYPE LINE NAME (OR ANY MESSAGE) TO PUT IN THE FIRST',
     $   ' 20 COLUMNS ON EACH CARD:'
      MSG=' '
ccc   READ (*,1000) MSG   ! does not work.
      call hitc_keep (msg)
      print *, 'MESSAGE CHOSEN FOR FIRST 20 COLUMNS: ',MSG
C----------WRITE STATIC FILE.
      CALL OPENFILE (LFN,VAXFILE2,'WRITE',*666)
      CARD(NCARD+1)='ISEP- '//DDDDTTTT//'WRITE GENERIC FILE '//
     $   VAXFILE2
      DO 100 IY=1,NY
      DO 100 IX=1,NX
      SP=X1+(IX-1)*XINC
      if (ny.eq.1) then
           WRITE (LFN,3000,ERR=999) MSG,SP,S(IX,IY)
3000       FORMAT (A20,2F12.2)
      else
           SP2=Y1+(IY-1)*YINC
           WRITE (LFN,3001,ERR=999) MSG,SP,SP2,S(IX,IY)
3001       FORMAT (A20,3F12.2)
      end if
100   CONTINUE
      NCARD=NCARD+1
      CLOSE (LFN)
      CALL LISTPREV3 ('SAVED   ','GENERIC ',VAXFILE2)
111   RETURN
C----------ERROR ENCOUNTERED.
999   PRINT *, 'FILE WRITE ERROR'
      CLOSE (LFN)
      RETURN
      END


      SUBROUTINE SFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S)
C     ISEP SUBROUTINE FOR SAVING STATIC FILES.
C     USES UNIT NUMBER 77 FOR ALL WRITES.
C     RETURNS VAXFILE UNCHANGED IF SAVE WAS NOT DONE.
C     CHANGES VAXFILE TO NEW NAME IF SAVE WAS DONE.
C----------DIMENSION AND DATA STATEMENTS.
      use statio_module
      DIMENSION S(NX*NY)
      CHARACTER*(*) VAXFILE
      CHARACTER*80 CARD(NCARD),VAXFILE2
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
      integer err
      character(len=80) msg
C----------GET STATIC FILE NAME TO SAVE.
      CALL LISTPREV1 (DDDDTTTT)
666   continue
      PRINT *, 'TYPE OUTPUT STATIC FILE NAME',
     $   '  (OR HIT RETURN IF NO SAVE DESIRED)'
      VAXFILE2=' '
      READ (*,1000,END=111) VAXFILE2
1000  FORMAT (A80)
      IF (VAXFILE2.EQ.' ') RETURN
      CALL LISTPREV2 ('STATIC  ',VAXFILE2,*666)
C----------WRITE STATIC FILE.
      call statio_write_file (vaxfile2,type,nhx,nhy,nhx2,nhy2,
     $                        x1,y1,xinc,yinc,nx,ny,s,
     $                        err,msg,card,ncard,'ISEP',0)
      print *, trim(msg)
      if (err /= STATIO_OK) go to 666
      VAXFILE=VAXFILE2
      CALL LISTPREV3 ('SAVED   ','STATIC  ',VAXFILE)
111   RETURN
      END


   
      SUBROUTINE HFILE (VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S)
C     ISEP SUBROUTINE FOR SAVING HARD COPY PRINTER-PLOT.
C     USES UNIT NUMBER 77 FOR ALL WRITES.
C     RETURNS VAXFILE UNCHANGED, WHETHER OR NOT PLOT FILE WAS SAVED.
C----------DIMENSION AND DATA STATEMENTS.
      PARAMETER (LFN=77)
      DIMENSION S(*)
      CHARACTER*(*) VAXFILE
      CHARACTER*80 CARD(*),VAXFILE2
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE
C----------GET HARD COPY PRINTER-PLOT FILE NAME.
      CALL LISTPREV1 (DDDDTTTT)
667   CLOSE (LFN)
      PRINT *, 'TYPE FILE NAME FOR HARD COPY PRINTER-PLOT'
      PRINT *, '  (WHICH YOU CAN DISPOSE TO A PRINTER)'
      PRINT *, '  (OR HIT RETURN IF NO PLOT DESIRED)'
      VAXFILE2=' '
      READ (*,1000,END=111) VAXFILE2
1000  FORMAT (A80)
      IF (VAXFILE2.EQ.' ') RETURN
      CALL LISTPREV2 (' PLOT   ',VAXFILE2,*667)
C----------WRITE HARD COPY PLOT OF STATIC FILE.
      CALL OPENFILE (LFN,VAXFILE2,'WRITE',*667)
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- '//DDDDTTTT//'SAVE PLOT FILE '//VAXFILE2
      CALL HARDSTAT (LFN,VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $               X1,Y1,XINC,YINC,NX,NY,NCARD,CARD,S)
      CLOSE (LFN)
      CALL LISTPREV3 ('SAVED   ',' PLOT   ',VAXFILE2)
111   RETURN
      END


   

      SUBROUTINE LISTPREV1 (DDDDTTTT)
C     CALL LISTPREV1 TO LIST PREVIOUS STATIC FILES FOUND OR SAVED,
C                          AND RETURN DATE AND TIME.
C     CALL LISTPREV2 TO CHECK FOR RE-USE OF FILE NAME (FOR WRITE ONLY).
C     CALL LISTPREV3 TO UPDATE LIST OF STATIC FILES FOUND OR SAVED.
C----------DIMENSION AND DATA STATEMENTS.
      CHARACTER*(*) VAXFILE
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 RW1(10),RW2(10),MSG1,MSG2
      CHARACTER*80 NAME(10),FULLNAME
      CHARACTER*4 Q
      LOGICAL QUEST
      SAVE NAME,RW1,RW2
      DATA NAME,RW1,RW2/30*' '/
C----------LIST PREVIOUS STATIC FILES FOUND OR SAVED.
      DO 40 I=1,10
      IF (NAME(I).EQ.' ') GO TO 40
      PRINT 2000, RW2(I),RW1(I),NAME(I)
40    CONTINUE
c     CALL DATE (DDDD)
c     CALL TIME (TTTT)
      CALL DATE_AND_TIME (DDDDTTTT)
      RETURN
C----------TEST FOR RE-USE OF FILE NAME (NOT TO BE CALLED FOR READ).
      ENTRY LISTPREV2 (MSG2,VAXFILE,*)
      INQUIRE (FILE=VAXFILE,NAME=FULLNAME,ERR=999,EXIST=QUEST)
      if (vaxfile(1:1).eq.'/') fullname=VAXFILE
      IF (.NOT.QUEST) RETURN
      LASTF=80
      DO 10 J=1,80
      IF (FULLNAME(J:J).EQ.';') LASTF=J
10    CONTINUE
      DO 50 I=1,10
      IF (NAME(I).EQ.' ') GO TO 50
      LASTN=80
      DO 20 J=1,80
      IF (NAME(I)(J:J).EQ.';') LASTN=J
20    CONTINUE
      IF (FULLNAME(1:LASTF).EQ.NAME(I)(1:LASTN)) GO TO 51
50    CONTINUE
C----------THE FILE ALREADY EXISTS.
      PRINT *, 'THIS FILE ALREADY EXISTS.'
      GO TO 52
51    PRINT *, 'THIS ',MSG2,' FILE NAME MATCHES A PREVIOUS ',
     $   RW2(I),' FILE NAME.'
      IF (MSG2.NE.RW2(I)) GO TO 53
52    PRINT *, 'DO YOU STILL WISH TO USE THIS FILE NAME?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'Y') RETURN
53    PRINT *, 'PLEASE CHOOSE ANOTHER NAME.'
      RETURN 1
999   PRINT *, 'FILE NAME INQUIRY ERROR -- PLEASE CHOOSE ANOTHER NAME.'
      RETURN 1
C----------SUCCESSFUL READ OR WRITE HAS OCCURRED.
      ENTRY LISTPREV3 (MSG1,MSG2,VAXFILE)
      PRINT 5000, MSG1,MSG2,VAXFILE
      DO 60 I=2,10
      RW1(I-1)=RW1(I)
      RW2(I-1)=RW2(I)
60    NAME(I-1)=NAME(I)
      RW1(10)=MSG1
      RW2(10)=MSG2
      NAME(10)=VAXFILE
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (A80)
2000  FORMAT (' LAST ',A7,' FILE  ',A5,'  WAS  ',A48)
5000  FORMAT (' SUCCESSFULLY  ',A5,'  ',A7,' FILE  ',A44)
      END







      SUBROUTINE OPENFILE (LFN,VAXFILE,RW,*)
C     OPEN A VAX FILE FOR READ OR WRITE.
C     LFN = LOCAL FILE NAME (UNIT NUMBER).
C     VAXFILE = VAX FILE NAME.
C     RW = 'READ' TO OPEN AN EXISTING VAX FILE FOR READ.
C     RW = 'WRITE' (OR ANYTHING ELSE) TO OPEN A NEW VAX FILE FOR WRITE.
C     VAX FILE NAME WILL BE EXPANDED TO FULL DESCRIPTION.
C     PRINTS MESSAGE AND USES ALTERNATE RETURN UPON ERROR.
C----------DIMENSION STATEMENTS.
      CHARACTER*(*) VAXFILE,RW
      CHARACTER*80 FULLNAME
      LOGICAL QUEST
C----------OPEN FOR WRITE.
      IF (RW.NE.'READ') OPEN (UNIT=LFN,ERR=999,FILE=VAXFILE,
     $   STATUS='UNKNOWN',FORM='FORMATTED',ACCESS='SEQUENTIAL')
C----------EXPAND NAME AND CHECK ON EXISTANCE.
      INQUIRE (FILE=VAXFILE,NAME=FULLNAME,ERR=999,EXIST=QUEST)
      if (vaxfile(1:1).ne.'/') VAXFILE=FULLNAME
      IF (.NOT.QUEST) GO TO 999
C----------OPEN FOR READ.
      IF (RW.EQ.'READ') OPEN (UNIT=LFN,ERR=999,FILE=VAXFILE,
     $   STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
C----------WE ARE SUCCESSFUL.
      RETURN
C----------WE ARE NOT SUCCESSFUL.
999   PRINT *, VAXFILE
      PRINT *, 'ERROR ON ATTEMPT TO OPEN ABOVE FILE FOR ',RW
      RETURN 1
      END







      SUBROUTINE OPTIONP (IFLAG,X1,Y1,XINC,YINC,NX,NY,S1,S2,S3)
C     PLOT STATIC FILE VERTICALLY.
C     IFLAG=1: 1ST PLOT IS NEW VALUES; 2ND PLOT IS OLD VALUES.
C     IFLAG=2: 1ST PLOT IS FIRST FILE; 2ND PLOT IS SECOND FILE.
C     IFLAG=3: 1ST PLOT IS FIRST 2 FILES; 2ND PLOT IS THIRD FILE.
C----------DIMENSION STATEMENTS.
      DIMENSION S1(NX,NY),S2(NX,NY),S3(NX,NY)
      CHARACTER*8 Q
C----------GET STARTED.
      IF (IABS(IFLAG).NE.3) CALL LIMITS (NX*NY,S1,S2,S2,   SLO,SUP)
      IF (IABS(IFLAG).EQ.3) CALL LIMITS (NX*NY,S1,S2,S3,   SLO,SUP)
      IF (NX.EQ.1) GO TO 222
C----------WE WILL PLOT IN X DIRECTION.
111   IY=1
5     IB=0
3     CALL IPLOT (NX,IA,IB,   X1,XINC,'XGP     ')
      IF (IA.EQ.0) GO TO 9
C----------PRINT ONE PAGE OF PLOT.
      CALL STATHEAD (SLO,SUP,IFLAG)
      Y=Y1+(IY-1)*YINC                                      
      DO 20 IX=IA,IB
      X=X1+(IX-1)*XINC
20    CALL STATLINE (IX,IA,IB,X,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY),
     $   SLO,SUP,IFLAG)
      GO TO 3
C----------GO TO NEXT Y GROUND POSITION.
9     IF (NY.EQ.1) RETURN
      CALL IPICK (NY,IY,   Y1,YINC,'YGP     ')
      IF (IY.GT.0) GO TO 5
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE Y DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
C----------WE WILL PLOT IN Y DIRECTION.
222   IX=1
51    IB=0
31    CALL IPLOT (NY,IA,IB,   Y1,YINC,'YGP     ')
      IF (IA.EQ.0) GO TO 91
C----------PRINT ONE PAGE OF PLOT.
      CALL STATHEAD (SLO,SUP,-IFLAG)
      X=X1+(IX-1)*XINC                          
      DO 21 IY=IA,IB
      Y=Y1+(IY-1)*YINC                                      
21    CALL STATLINE (IY,IA,IB,X,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY),
     $   SLO,SUP,-IFLAG)
      GO TO 31
C----------GO TO NEXT X GROUND POSITION.
91    IF (NX.EQ.1) RETURN
      CALL IPICK (NX,IX,   X1,XINC,'XGP     ')
      IF (IX.GT.0) GO TO 51
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE X DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
      GO TO 111
      END

         




      SUBROUTINE OPTIONL (LFN,VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S)
C     LIST STATIC FILE ATTRIBUTES.
C     LFN = LOCAL FILE UNIT NUMBER.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 VAXFILE,CARD(NCARD)
      CHARACTER*16 WHICH
      CHARACTER*8 TYPE
C----------PRINT ATTRIBUTES.
      IF (LFN.EQ.6) PRINT '(/)'
      IF (LFN.NE.6) WRITE (LFN,'("1")')
      WHICH=' '
      IF (NHX.EQ.9) WHICH='(GROUP FILE)'
      IF (NHX.EQ.33.OR.NHX.EQ.46) WHICH='(SOURCE FILE)'
      IF (NHX.EQ.35.OR.NHX.EQ.47) WHICH='(RECEIVER FILE)'
      IF (NHX2.EQ.33.OR.NHX2.EQ.46.OR.NHX2.EQ.35.OR.NHX2.EQ.47)
     $   WHICH='(S=R FILE)'
      WRITE (LFN,*) VAXFILE(1:45),'  TYPE=',TYPE,'  ',WHICH
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      WRITE (LFN,4001) NHX,NHY,NHX2,NHY2,X1,Y1,XEND,YEND,XINC,YINC,NX,NY
C----------PRINT ADDITIONAL INFORMATION.
      CALL LIMITS (NX*NY,S,S,S,   SLO,SUP)
      NIL=0
      SUM=0.
      DO 20 IX=1,NX
      DO 20 IY=1,NY
      IF (S(IX,IY).EQ.FNIL) NIL=NIL+1
      IF (S(IX,IY).NE.FNIL) SUM=SUM+ABS(S(IX,IY))
20    CONTINUE
      LIVE=NX*NY-NIL
      IF (LIVE.GT.0) SUM=SUM/LIVE
      WRITE (LFN,6000) SLO,SUP,SUM,LIVE,NIL,NX*NY
C----------READ AND PRINT HISTORY CARDS.
      WRITE (LFN,5001)
      DO 40 I=1,NCARD
      IF (LFN.EQ.6.AND.MOD(I,22).EQ.14) CALL HIT
40    WRITE (LFN,5001) CARD(I)
      IF (LFN.EQ.6) PRINT '(/)'
      RETURN
C----------FORMAT STATEMENTS.
4001  FORMAT ('           X AND Y HEADER WORDS =              ',I8,I14,
     $                              3X,2I4/
     $        '           STARTING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '             ENDING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '           X AND Y GROUND POSITION INCREMENTS =',2G14.6/
     $        '           NUMBER OF X AND Y GROUND POSITIONS =',I8,I14)
5001  FORMAT (1X,A79)
6000  FORMAT (' RANGE OF STATIC VALUES: ',F10.2,'  TO ',F10.2,
     $   '      AV ABS VALUE: ',F10.2/
     $   ' NUMBER OF LIVE VALUES: ',I7,
     $   '      NIL VALUES: ',I7,'      TOTAL: ',I7)
      END

           
           

           



      SUBROUTINE HARDSTAT (LFN,VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,NCARD,CARD,S)
C     MAKE HARD COPY PLOT OF STATIC FILE.
C----------DIMENSION STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 VAXFILE,CARD(NCARD)
      CHARACTER*8 TYPE
      CHARACTER*8 Q 
C----------GET STARTED.
      CALL OPTIONL (LFN,VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,
     $          X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S)
      CALL LIMITS (NX*NY,S,S,S,   SLO,SUP)
      IF (NX.EQ.1) GO TO 222
C----------WE WILL PLOT IN X DIRECTION.
111   IY=1
5     Y=Y1+(IY-1)*YINC                                      
      WRITE (LFN,5000) 'X','YGP=',NINT(Y)
      PRINT 5001, 'X','YGP=',NINT(Y)
      CALL HARDHEAD (LFN,SLO,SUP)
      DO 20 IX=1,NX
      X=X1+(IX-1)*XINC
20    CALL HARDLINE (LFN,IX-1,X,Y,S(IX,IY),SLO,SUP,'X')
      CALL HARDHEAD (LFN,SLO,SUP)
C----------GO TO NEXT Y GROUND POSITION.
      IF (NY.EQ.1) RETURN
      CALL IPICK (NY,IY,   Y1,YINC,'YGP     ')
      IF (IY.GT.0) GO TO 5
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE Y DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
C----------WE WILL PLOT IN Y DIRECTION.
222   IX=1
51    X=X1+(IX-1)*XINC                          
      WRITE (LFN,5000) 'Y','XGP=',NINT(X)
      PRINT 5001, 'Y','XGP=',NINT(X)
      CALL HARDHEAD (LFN,SLO,SUP)
      DO 21 IY=1,NY
      Y=Y1+(IY-1)*YINC                                      
21    CALL HARDLINE (LFN,IY-1,X,Y,S(IX,IY),SLO,SUP,'Y')
      CALL HARDHEAD (LFN,SLO,SUP)
C----------GO TO NEXT X GROUND POSITION.
      IF (NX.EQ.1) RETURN
      CALL IPICK (NX,IX,   X1,XINC,'XGP     ')
      IF (IX.GT.0) GO TO 51
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE X DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
      GO TO 111
C----------FORMAT STATEMENTS.
5000  FORMAT (///'1PLOTTING IN ',A1,' DIRECTION AT ',A4,I7)
5001  FORMAT (' PLOTTING IN ',A1,' DIRECTION AT ',A4,I7)
      END


                



      SUBROUTINE HARDHEAD (LFN,SLO,SUP)
C     CALL HARDHEAD TO PRINT HEADER OF HARD COPY PLOT.
C     CALL HARDLINE TO PRINT ONE LINE OF HARD COPY PLOT.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      PARAMETER (K=8,L=6)
      CHARACTER*1 CHAR
c     CHARACTER*8 A(K*L+1),B(K*L+1),LETTER
      CHARACTER*8 A(K*L+1),LETTER
C----------PRINT PLOT HEADER.
      CALL PLOTLINE (K,L,DUMMY,SLO,SUP,'(F8.0)',A)
      DO 7 I=2,K*L
      IF (A(I)(1:1).EQ.'.') A(I)=' '
7     CONTINUE
      A(K*L+1)=' '
      A(1)(8:8)=' '
      WRITE (LFN,3000) A
      RETURN
C----------PRINT ONE LINE OF PLOT.
      ENTRY HARDLINE (LFN,II,X,Y,S,SLO,SUP,CHAR)
      LETTER=CHAR
      IF (S.EQ.FNIL) LETTER='N'
      IF (MOD(II,4).EQ.0) LETTER(2:2)='.'
      CALL PLOTLINE (K,L,S,SLO,SUP,LETTER,A)
      WRITE (LFN,4000) NINT(X),NINT(Y),S,A
      RETURN
C----------FORMAT STATEMENTS.
3000  FORMAT ('    XGP   YGP  ',A14,48A1)
cc4000  FORMAT (1X,I6,I6,G13.5,1X,49A1)
4000  FORMAT (1X,I7,I7,G13.5,1X,49A1)
      END




                 

      SUBROUTINE STATHEAD (SLO,SUP,IFLAG)
C     CALL STATHEAD TO PRINT HEADER OF STATIC FILE PLOT.
C     CALL STATLINE TO PRINT ONE LINE OF STATIC FILE PLOT.
C     IFLAG=+-1: 1ST PLOT IS OLD VALUES, 2ND PLOT IS NEW VALUES.
C     IFLAG=+-2: 1ST PLOT IS FIRST FILE, 2ND PLOT IS SECOND FILE.
C     IFLAG=+-3: 1ST PLOT IS BOTH FILES, 2ND PLOT IS COMBINED FILE.
C     POSITIVE IFLAG USES CHARACTER X; NEGATIVE IFLAG USES CHARACTER Y.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      PARAMETER (K=4,L=6)
      CHARACTER*8 A(K*L+1),B(K*L+1),LETTER
      CHARACTER*1 SECOND
      CHARACTER*5 SHOW5,cshow5,cshow52,cshow53
      CHARACTER*10 SHOW10,cshow10
C----------PRINT PLOT HEADER.
      CALL PLOTLINE (K,L,DUMMY,SLO,SUP,'(F8.0)',A)
      DO 7 I=2,K*L
      IF (A(I)(1:1).EQ.'.') A(I)=' '
7     CONTINUE
      A(K*L+1)=' '
      A(1)(8:8)=' '
      IF (IABS(IFLAG).EQ.1) PRINT 3001, A
      IF (IABS(IFLAG).EQ.2) PRINT 3002, A
      IF (IABS(IFLAG).EQ.3) PRINT 3003, A
      RETURN
C----------PRINT ONE LINE OF PLOT.
      ENTRY STATLINE (II,IA,IB,X,Y,S1,S2,S3,SLO,SUP,IFLAG)
      SECOND=' '
      IF (MOD(II-IA,4).EQ.0.OR.II.EQ.IB) SECOND='.'
C----------PREPARE FIRST PLOT.
      IF (IABS(IFLAG).EQ.3) THEN
           LETTER='1'
           IF (S1.EQ.S2) LETTER='B'
           IF (S1.EQ.FNIL) LETTER='N'
           LETTER(2:2)=SECOND
           CALL PLOTLINE (K,L,S1,SLO,SUP,LETTER,A)
           LETTER='2'
           IF (S1.EQ.S2) LETTER='B'
           IF (S2.EQ.FNIL) LETTER='N'
           LETTER(2:2)='B'
           IF (S1.EQ.FNIL.OR.S2.EQ.FNIL) LETTER(2:2)='N'
           CALL PLOTLINE (K,L,S2,SLO,SUP,LETTER,A)
      ELSE
           LETTER='X'
           IF (IFLAG.LT.0) LETTER='Y'
           IF (S1.NE.S2) LETTER='E'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) LETTER='1'
           IF (S1.EQ.FNIL) LETTER='N'
           LETTER(2:2)=SECOND
           CALL PLOTLINE (K,L,S1,SLO,SUP,LETTER,A)
      END IF
C----------PREPARE SECOND PLOT.
      LETTER='X'
      IF (IFLAG.LT.0) LETTER='Y'
      IF (IABS(IFLAG).EQ.3) THEN
           SSS=S3
           IF (S3.EQ.S1) LETTER='1'
           IF (S3.EQ.S2) LETTER='2'
           IF (S3.EQ.S1.AND.S3.EQ.S2) LETTER='B'
      ELSE
           IF (S1.NE.S2) LETTER='O'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) LETTER='2'
           SSS=S2
      END IF
      IF (SSS.EQ.FNIL) LETTER='N'
      LETTER(2:2)=SECOND
      CALL PLOTLINE (K,L,SSS,SLO,SUP,LETTER,B)
C----------PRINT THE LINE OF PLOT.
      IF (IABS(IFLAG).LE.2) THEN
           cshow10=show10(s1)
           cshow5=show5(s2)
           PRINT 4000, NINT(X),NINT(Y),cSHOW10   ,A,cSHOW5   ,B
c          PRINT 4000, NINT(X),NINT(Y),SHOW10(S1),A,SHOW5(S2),B
      ELSE
           cshow5=show5(s1)
           cshow52=show5(s2)
           cshow53=show5(s3)
           PRINT 4001, NINT(X),NINT(Y),cSHOW5   ,cSHOW52  ,A,
     $        cSHOW53  ,B
c          PRINT 4001, NINT(X),NINT(Y),SHOW5(S1),SHOW5(S2),A,
c    $        SHOW5(S3),B
      END IF
      RETURN
C----------FORMAT STATEMENTS.
3001  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'        OLD VALUES       ')
3002  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'   SECOND STATIC FILE    ')
3003  FORMAT ('    XGP  YGP',6X,A8,24A1,5X,'  COMBINED STATIC FILE   ')
cc4000  FORMAT (1X,I6,I5,1X,A10,1X,25A1,A5,1X,25A1)
cc4001  FORMAT (1X,I6,I5,1X,A5,A5,1X,25A1,A5,1X,25A1)
4000  FORMAT (1X,I7,I4,1X,A10,1X,25A1,A5,1X,25A1)
4001  FORMAT (1X,I7,I4,1X,A5,A5,1X,25A1,A5,1X,25A1)
      END                                
                   



      SUBROUTINE OPTIONR (OPTION,S,A,XINC,YINC,NX,NY,CARD,NCARD)
C     REMOVE RUNNING AVERAGE (OR SMOOTH THE FILE). 
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY),A(*)
      CHARACTER*80 CARD(*)
      CHARACTER*8 OPTION,IEND
      DATA NXRUN,NYRUN,IEND,TP/21,1,'NN',0./
C----------ASK QUESTIONS.
      PRINT *, 'TYPE X GROUND POSITION RANGE TO AVERAGE -- DEFAULT ',
     $   NXRUN
      CALL HITI (NXRUN)
      NPOINT=NINT(NXRUN/XINC)
      PRINT *, 'THIS GROUND POSITION RANGE CORRESPONDS TO ',NPOINT,
     $   ' POINTS'
      PRINT *, ' '
      PRINT *, 'TYPE  TT  FOR TRUNCATED END RANGE' 
      PRINT *, 'TYPE  SS  FOR SHIFTED END RANGE' 
      PRINT *, 'TYPE  EE  FOR EXTRAPOLATED END RANGE'
      PRINT *, 'TYPE  NN  FOR NARROWED END RANGE (GRADED ENDS)'
      CALL HITC2 (IEND)
      PRINT *, 'CHOOSE TRIM PERCENTAGE FOR CALCULATING RUNNING AVERAGE'
      PRINT *, 'EXAMPLE:    0 = CONVENTIONAL AVERAGE OR MEAN' 
      PRINT *, 'EXAMPLE:  100 = MEDIAN' 
      CALL HITX2 (TP) 
C----------DO THE WORK IN THE X DIRECTION.
      DO 80 IY=1,NY
      CALL MOVE (S(1,IY),A,NX)
      DO 40 IX=1,NX 
      AV=RUNAV(IX,A,NX,NPOINT,IEND,TP)
      IF (OPTION.EQ.'R') S(IX,IY)=A(IX)-AV 
      IF (OPTION.EQ.'S') S(IX,IY)=AV
40    CONTINUE
80    CONTINUE
C----------ASK QUESTIONS.
      IF (NY.EQ.1) GO TO 95
      PRINT *, 'TYPE Y GROUND POSITION RANGE TO AVERAGE -- DEFAULT ',
     $   NYRUN
      CALL HITI (NYRUN)
      NPOINT=NINT(NYRUN/YINC)
      PRINT *, 'THIS GROUND POSITION RANGE CORRESPONDS TO ',NPOINT,
     $   ' POINTS'
C----------DO THE WORK IN THE Y DIRECTION.
      DO 90 IX=1,NX
      DO 48 IY=1,NY
48    A(IY)=S(IX,IY)
      DO 50 IY=1,NY 
      AV=RUNAV(IY,A,NY,NPOINT,IEND,TP)
      IF (OPTION.EQ.'R') S(IX,IY)=A(IY)-AV 
      IF (OPTION.EQ.'S') S(IX,IY)=AV
50    CONTINUE
90    CONTINUE
C----------GENERATE COMMENT CARD.
95    NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) NXRUN,NYRUN,IEND,TP
1000  FORMAT ('ISEP- ',17X,'#XRUN=',I4,'    #YRUN=',I4,
     $   '    END OPTION=',A2,'    TRIM%=',F5.0)
      IF (OPTION.EQ.'R') CARD(NCARD)(7:21)='REMOVE RUN AV'
      IF (OPTION.EQ.'S') CARD(NCARD)(7:21)='SMOOTH THE FILE'
      IF (OPTION.EQ.'R') PRINT *, 'RUNNING AVERAGE HAS BEEN REMOVED'
      IF (OPTION.EQ.'S') PRINT *, 'FILE HAS BEEN SMOOTHED'
      RETURN
      END






      SUBROUTINE OPTIONI (TYPE,S,A,NX,NY,CARD,NCARD)
C     INTEGRATE THE FILE, PRESERVING THE NILS.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY),A(*)
      CHARACTER*8 TYPE
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      IF (TYPE.EQ.'INC') TYPE='RESID'
      IF (NX.EQ.1) RETURN
      DO 80 IY=1,NY
      CALL MOVE (S(1,IY),A,NX)
      SKEEP=S(1,IY)
      S(1,IY)=0. 
      DO 40 IX=2,NX 
      SSS=S(IX,IY) 
      S(IX,IY)=S(IX-1,IY)+SKEEP 
40    SKEEP=SSS
c     DO 50 IX=1,NX
      DO 50 IX=1,NX-1  ! last nil on each x-line should not be preserved.
      IF (A(IX).EQ.FNIL) S(IX,IY)=FNIL
50    CONTINUE
80    CONTINUE
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- INTEGRATE THE FILE, PRESERVING THE NILS'
      PRINT *, 'FILE HAS BEEN INTEGRATED, PRESERVING THE NILS'
      RETURN
      END






      SUBROUTINE OPTIONN (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH INTERPOLATED VALUES.
C     EACH INLINE (CONSTANT Y) IS INTERPOLATED FIRST.
C     THEN INTERPOLATION IS PERFORMED IN THE CROSSLINE DIRECTION.
C     IF ALL NILS, NILS ARE REPLACED BY ZEROES.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------COUNT THE NILS.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.FNIL) KOUNT=KOUNT+1
5     CONTINUE
      IF (KOUNT.EQ.0) RETURN
C----------FIRST INTERPOLATE EACH LINE SEPARATELY.
      DO 80 IY=1,NY
C----------CHECK FOR ALL NILS (AND RESET TO ZERO IF ONLY ONE LINE).
      DO 7 IX=1,NX 
      IF (S(IX,IY).NE.FNIL) GO TO 11
7     CONTINUE
      IF (NY.GT.1) GO TO 80
      DO 9 IX=1,NX
9     S(IX,IY)=0.
      GO TO 80
C----------WE HAVE AT LEAST ONE NON-NIL VALUE.
11    S(1,IY)=S(IX,IY) 
      DO 12 IX=NX,1,-1
      IF (S(IX,IY).NE.FNIL) GO TO 13
12    CONTINUE
CCC   WE SHOULD NOT GET TO HERE.
13    S(NX,IY)=S(IX,IY) 
      DO 20 IX=2,NX 
      IF (S(IX-1,IY).NE.FNIL.AND.S(IX,IY).EQ.FNIL) THEN 
           IA=IX-1
      ELSE IF (S(IX-1,IY).EQ.FNIL.AND.S(IX,IY).NE.FNIL) THEN
           IB=IX
           DO 15 J=IA+1,IB-1 
15         S(J,IY)=S(IA,IY)+(J-IA)*(S(IB,IY)-S(IA,IY))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------NOW INTERPOLATE IN THE CROSSLINE DIRECTION.
      IF (NY.EQ.1) GO TO 99
      DO 1180 IX=1,NX
C----------CHECK FOR ALL NILS (AND RESET TO ZERO IF ONLY ONE LINE).
      DO 117 IY=1,NY 
      IF (S(IX,IY).NE.FNIL) GO TO 111
117   CONTINUE
      DO 119 IY=1,NY
119   S(IX,IY)=0.
      GO TO 1180
C----------WE HAVE AT LEAST ONE NON-NIL VALUE.
111   S(IX,1)=S(IX,IY) 
      DO 112 IY=NY,1,-1
      IF (S(IX,IY).NE.FNIL) GO TO 113
112   CONTINUE
CCC   WE SHOULD NOT GET TO HERE.
113   S(IX,NY)=S(IX,IY) 
      DO 120 IY=2,NY 
      IF (S(IX,IY-1).NE.FNIL.AND.S(IX,IY).EQ.FNIL) THEN 
           IA=IY-1
      ELSE IF (S(IX,IY-1).EQ.FNIL.AND.S(IX,IY).NE.FNIL) THEN
           IB=IY
           DO 115 J=IA+1,IB-1 
115        S(IX,J)=S(IX,IA)+(J-IA)*(S(IX,IB)-S(IX,IA))/(IB-IA) 
      END IF
120   CONTINUE
1180  CONTINUE
C----------GENERATE COMMENT CARD.
99    NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' NILS WITH INTERPOLATED VALUES')
      PRINT *, KOUNT,' NILS HAVE BEEN REPLACED WITH INTERPOLATED VALUES'
      RETURN
      END





      SUBROUTINE OPTIONNX (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH INTERPOLATED VALUES IN X DIRECTION.
C     EACH INLINE (CONSTANT Y) IS INTERPOLATED BUT NOT EXTRAPOLATED.
C     ANY INLINE CONTAINING ALL NILS IS NOT CHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------INTERPOLATE EACH INLINE SEPARATELY.
      KOUNT=0
      DO 80 IY=1,NY
C----------FIND FIRST AND LAST NON-NIL VALUE.
      kx1=0
      kx2=0
      DO 7 IX=1,NX 
      IF (S(IX,IY).NE.FNIL.and.kx1.eq.0) kx1=ix
      IF (S(IX,IY).NE.FNIL             ) kx2=ix
7     CONTINUE
      IF (kx1.eq.0.or.kx2.eq.0.or.kx1.eq.kx2.or.kx2.eq.kx1+1) GO TO 80
C----------WE HAVE SOMETHING TO INTERPOLATE.
      DO 20 IX=kx1+1,kx2
      IF (S(IX-1,IY).NE.FNIL.AND.S(IX,IY).EQ.FNIL) THEN 
           IA=IX-1
      ELSE IF (S(IX-1,IY).EQ.FNIL.AND.S(IX,IY).NE.FNIL) THEN
           IB=IX
           DO 15 J=IA+1,IB-1 
           kount=kount+1
15         S(J,IY)=S(IA,IY)+(J-IA)*(S(IB,IY)-S(IA,IY))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' INTERPOLATE NILS IN X DIRECTION')
      PRINT *, KOUNT,' NILS HAVE BEEN INTERPOLATED IN X DIRECTION'
      RETURN
      END





      SUBROUTINE OPTIONNY (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH INTERPOLATED VALUES IN Y DIRECTION.
C     EACH CROSSLINE (CONSTANT X) IS INTERPOLATED BUT NOT EXTRAPOLATED.
C     ANY CROSSLINE CONTAINING ALL NILS IS NOT CHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------INTERPOLATE EACH CROSSLINE SEPARATELY.
      KOUNT=0
      DO 80 IX=1,NX
C----------FIND FIRST AND LAST NON-NIL VALUE.
      ky1=0
      ky2=0
      DO 7 IY=1,NY 
      IF (S(IX,IY).NE.FNIL.and.ky1.eq.0) ky1=iy
      IF (S(IX,IY).NE.FNIL             ) ky2=iy
7     CONTINUE
      IF (ky1.eq.0.or.ky2.eq.0.or.ky1.eq.ky2.or.ky2.eq.ky1+1) GO TO 80
C----------WE HAVE SOMETHING TO INTERPOLATE.
      DO 20 Iy=ky1+1,ky2
      IF (S(IX,IY-1).NE.FNIL.AND.S(IX,IY).EQ.FNIL) THEN 
           IA=Iy-1
      ELSE IF (S(IX,IY-1).EQ.FNIL.AND.S(IX,IY).NE.FNIL) THEN
           IB=Iy
           DO 15 J=IA+1,IB-1 
           kount=kount+1
15         S(IX,j)=S(Ix,Ia)+(J-IA)*(S(Ix,Ib)-S(Ix,Ia))/(IB-IA) 
      END IF
20    CONTINUE
80    CONTINUE
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' INTERPOLATE NILS IN Y DIRECTION')
      PRINT *, KOUNT,' NILS HAVE BEEN INTERPOLATED IN Y DIRECTION'
      RETURN
      END





      SUBROUTINE OPTIONNN (S,S3,NX,NY,CARD,NCARD)
C     REPLACE NILS ONLY FROM NEARBY INTERPOLATED VALUES.
C     S3 is scratch space.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY),S3(NX,NY)
      CHARACTER*80 CARD(*)
      CHARACTER*4 Q
      logical iyminus,iyplus,iyok
      logical ixminus,ixplus,ixok
C----------ASK QUESTIONS.
      print *, 'Nil values will be replaced by interpolated values'
      print *, 'only if non-nil values exist within a specified'
      print *, 'search distance in each direction from the nil value.'
      print *, 'A search distance of 0 means not to search in that',
     $                                               ' direction.'
      print *, 'A search distance of 1 means to search only one point',
     $                                               ' away,'
      print *, '   in both the negative and positive directions.'
      print *, 'A search distance of 2 means to search 2 points away',
     $                                                 ' (etc).'
      print *, 'Search distances should be kept small, since all'
      print *, 'non-nil values within the search range will be used.'
      PRINT *, 'TYPE SEARCH DISTANCE IN X DIRECTION  (DEFAULT 1)'
      IXDIST=1
      CALL HITI (IXDIST)
      PRINT *, 'TYPE SEARCH DISTANCE IN Y DIRECTION  (DEFAULT 1)'
      IYDIST=1
      CALL HITI (IYDIST)
      PRINT *, 'DO YOU WISH TO REQUIRE NON-NIL VALUES ON EACH SIDE?'
      print *, 'If you choose N, the nil value will be replaced'
      print *, 'regardless of where the non-nil values are within the',
     $                                               ' search range.'
      print *, 'If you choose Y, the nil value will be replaced'
      print *, 'only if there are non-nil values in both the positive'
      print *, 'and negative X directions, or if there are non-nil'
      print *, 'values in both the positive and negative Y directions.'
      PRINT *, 'DO YOU WISH TO REQUIRE NON-NIL VALUES ON EACH SIDE?',
     $                                            ' (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
C----------DO THE WORK.
      CALL MOVE (S,S3,NX*NY)
      KOUNT=0
      DO IY=1,NY 
      DO IX=1,NX
      if (s(ix,iy).ne.FNIL) go to 200
      iymin=max(iy-iydist,1)
      iymax=min(iy+iydist,ny)
      ixmin=max(ix-ixdist,1)
      ixmax=min(ix+ixdist,nx)
      k=0
      DO IY2=iymin,iymax
      DO IX2=ixmin,ixmax
      if (s(ix2,iy2).ne.FNIL) k=k+1
      end do
      end do
      if (k.eq.0) go to 200
      if (q.eq.'Y') then
           iyminus=.false.
           iyplus=.false.
           ixminus=.false.
           ixplus=.false.
           DO IY2=iymin,iy
           if (s(ix,iy2).ne.FNIL) iyminus=.true.
           end do
           DO IY2=iy,iymax
           if (s(ix,iy2).ne.FNIL) iyplus=.true.
           end do
           DO Ix2=ixmin,ix
           if (s(ix2,iy).ne.FNIL) ixminus=.true.
           end do
           DO Ix2=ix,ixmax
           if (s(ix2,iy).ne.FNIL) ixplus=.true.
           end do
           iyok=(iyminus.and.iyplus)
           ixok=(ixminus.and.ixplus)
           if(.not.iyok.and..not.ixok) go to 200
      end if
      sum=0.0
      weight=0.0
      DO IY2=iymin,iymax
      DO IX2=ixmin,ixmax
      if (s(ix2,iy2).ne.FNIL) then
           xdist=ix2-ix
           ydist=iy2-iy
           w=1.0/sqrt(xdist*xdist+ydist*ydist)
           weight=weight+w
           sum=sum+w*s(ix2,iy2)
      end if
      end do
      end do
      if (weight.gt.0.0) then
           s3(ix,iy)=sum/weight
           kount=kount+1
      end if
200   continue
      END DO
      END DO
      CALL MOVE (S3,S,NX*NY)
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' INTERPOLATE NILS FROM NEARBY VALUES')
      PRINT *, KOUNT,' NILS HAVE BEEN INTERPOLATED FROM NEARBY VALUES'
      RETURN
      END





      SUBROUTINE OPTIONZ (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH ZEROES.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.FNIL) THEN
           S(IX,IY)=0.
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' NILS HAVE BEEN REPLACED WITH ZEROES'
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' NILS WITH ZEROES')
      RETURN
      END




      SUBROUTINE OPTIONV1 (S,NX,NY,CARD,NCARD)
C     REPLACE NILS WITH ANY SPECIFIED VALUE.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE VALUE TO REPLACE NILS WITH  (DEFAULT 0)'
      VALUE=0.
      CALL HITX (VALUE)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).EQ.FNIL) THEN
           S(IX,IY)=VALUE
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' NILS HAVE BEEN REPLACED WITH ', VALUE
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT,VALUE
1000  FORMAT ('ISEP- REPLACE',I7,' NILS WITH VALUE',G13.5)
      RETURN
      END




      SUBROUTINE OPTIONV2 (S,NX,NY,CARD,NCARD)
C     REPLACE SPECIFIED RANGE OF VALUES WITH NILS.
C----------DIMENSION AND DATA STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE MINIMUM VALUE TO REPLACE WITH NILS (DEFAULT 0)'
      VMIN=0.
      CALL HITX (VMIN)
      PRINT *, 'TYPE MAXIMUM VALUE TO REPLACE WITH NILS (DEFAULT 0)'
      VMAX=0.
      CALL HITX (VMAX)
C----------DO THE WORK.
      KOUNT=0
      DO 5 IY=1,NY
      DO 5 IX=1,NX
      IF (S(IX,IY).GE.VMIN.and.S(IX,IY).LE.VMAX) THEN
           S(IX,IY)=FNIL
           KOUNT=KOUNT+1
      END IF
5     CONTINUE
C----------GENERATE COMMENT CARD.
      PRINT *, KOUNT,' VALUES HAVE BEEN REPLACED WITH NIL'
      IF (KOUNT.EQ.0) RETURN
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) KOUNT
1000  FORMAT ('ISEP- REPLACE',I7,' VALUES WITH NIL')
      RETURN
      END







      SUBROUTINE OPTIONM (S,NX,NY,CARD,NCARD)
C     MULTIPLY THE STATIC VALUES BY A CONSTANT.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE CONSTANT TO MULTIPLY STATIC VALUES BY  (DEFAULT 1)'
      CONST=1.
      CALL HITX (CONST)
      IF (CONST.EQ.1.) RETURN
C----------DO THE WORK.
      DO 80 IY=1,NY
      DO 80 IX=1,NX
80    S(IX,IY)=S(IX,IY)*CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) CONST
1000  FORMAT ('ISEP- MULTIPLY STATIC VALUES BY CONSTANT',G13.5)
      PRINT *, 'STATIC VALUES HAVE BEEN MULTIPLIED BY ',CONST
      RETURN
      END






      SUBROUTINE OPTIONA (S,NX,NY,CARD,NCARD)
C     ADD A CONSTANT TO THE STATIC VALUES.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS.
      PRINT *, 'TYPE CONSTANT TO ADD TO STATIC VALUES  (DEFAULT 0)'
      CONST=0.
      CALL HITX (CONST)
      IF (CONST.EQ.0.) RETURN
C----------DO THE WORK.
      DO 80 IY=1,NY
      DO 80 IX=1,NX
80    S(IX,IY)=S(IX,IY)+CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) CONST
1000  FORMAT ('ISEP- ADD CONSTANT',G13.5,' TO STATIC VALUES')
      PRINT *, 'CONSTANT ',CONST,' HAS BEEN ADDED TO STATIC VALUES'
      RETURN
      END






      SUBROUTINE OPTIONC (NHX,NHY,NHX2,NHY2,
     $   X1,Y1,XINC,YINC,NX,NY,S,CARD,NCARD)
C     CHANGE THE GROUND POSITIONS.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------GET STARTED.
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      PRINT 4001, NHX,NHY,NHX2,NHY2,X1,Y1,XEND,YEND,XINC,YINC,NX,NY
4001  FORMAT ('           X AND Y HEADER WORDS =              ',I8,I14,
     $                              3X,2I4/
     $        '           STARTING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '             ENDING X AND Y GROUND POSITIONS = ',2G14.6/
     $        '           X AND Y GROUND POSITION INCREMENTS =',2G14.6/
     $        '           NUMBER OF X AND Y GROUND POSITIONS =',I8,I14/)
C----------CHANGE THE X GROUND POSITIONS.
2     CALL OPTIONC1 ('present X',   X1OLD,X1NEW)
      CALL OPTIONC1 ('other  X ',   X2OLD,X2NEW)
      IF (X2OLD.EQ.X1OLD) THEN
           PRINT *, 'THE TWO OLD GROUND POSITIONS MUST BE DIFFERENT --',
     $        ' TRY AGAIN'
           GO TO 2
      END IF
      SLOPE=(X2NEW-X1NEW)/(X2OLD-X1OLD)
      X1=X1NEW+(X1-X1OLD)*SLOPE
      XINC=XINC*SLOPE
      CALL REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) 'X',X1OLD,X2OLD,X1NEW,X2NEW
1000  FORMAT ('ISEP- ',A1,' GPs ',2F12.2,'  CHANGED TO ',2F12.2)
C----------CHANGE THE Y GROUND POSITIONS.
      IF (NY.GT.1) THEN
4     CALL OPTIONC1 ('present Y',   Y1OLD,Y1NEW)
      CALL OPTIONC1 ('other  Y ',   Y2OLD,Y2NEW)
      IF (Y2OLD.EQ.Y1OLD) THEN
           PRINT *, 'THE TWO OLD GROUND POSITIONS MUST BE DIFFERENT --',
     $        ' TRY AGAIN'
           GO TO 4
      END IF
      SLOPE=(Y2NEW-Y1NEW)/(Y2OLD-Y1OLD)
      Y1=Y1NEW+(Y1-Y1OLD)*SLOPE
      YINC=YINC*SLOPE
      CALL REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) 'Y',Y1OLD,Y2OLD,Y1NEW,Y2NEW
      END IF
C----------GET HEADER WORDS.
      NZX=NHX
      NZY=NHY
      NZX2=NHX2
      NZY2=NHY2
30    PRINT *, 'TYPE FOUR HEADER WORD NUMBERS IF YOU WISH TO CHANGE',
     $   ' THEM.'
      PRINT *, 'OTHERWISE HIT RETURN.'
      PRINT *, 'The most common are:'
      PRINT *, '   Group file:   9  0  0  0'
      PRINT *, '  Source file:  33 34  0  0   or   33  0  0  0  ',
     $   ' or   46 0  0  0'
      PRINT *, 'Receiver file:  35 36  0  0   or   35  0  0  0  ',
     $   ' or   47 0  0  0'
      PRINT *, '     S=R file:  35 36 33 34   or   35  0 33  0  ',
     $   ' or   47 0 46  0'
      CALL HITIIII2 (NHX,NHY,NHX2,NHY2)
      IF (NHX.EQ.0) THEN
           PRINT *, 'FIRST HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 30
      END IF
      IF (NHY.EQ.0.AND.NY.GT.1) THEN
           PRINT *, 'SECOND HEADER WORD CANNOT BE ZERO -- TRY AGAIN'
           GO TO 30
      END IF
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),2000) NZX,NZY,NZX2,NZY2,NHX,NHY,NHX2,NHY2
2000  FORMAT ('ISEP- HEADERS CHANGED FROM ',4I4,'   TO ',4I4)
      RETURN
      END





      SUBROUTINE REVERSE (X1,Y1,XINC,YINC,NX,NY,S)
C     REVERSE THE STATIC FILE IF XINC OR YINC IS NEGATIVE.
      DIMENSION S(NX,NY)
C----------REVERSE THE FILE (IN X DIRECTION) IF NECESSARY.
      IF (NX.GT.1.AND.XINC.LT.0.) THEN
           NX2=(NX+1)/2
           DO 10 IY=1,NY
           DO 10 IX=1,NX2
           IX2=NX-IX+1
           SSS=S(IX,IY)
           S(IX,IY)=S(IX2,IY)
10         S(IX2,IY)=SSS
           X1=X1+(NX-1)*XINC
           XINC=-XINC
           PRINT *, 'STATIC FILE REVERSED BECAUSE THE',
     $        ' X GROUND POSITIONS ARE REVERSED'
      END IF
C----------REVERSE THE FILE (IN Y DIRECTION) IF NECESSARY.
      IF (NY.GT.1.AND.YINC.LT.0.) THEN
           NY2=(NY+1)/2
           DO 11 IX=1,NX
           DO 11 IY=1,NY2
           IY2=NY-IY+1
           SSS=S(IX,IY)
           S(IX,IY)=S(IX,IY2)
11         S(IX,IY2)=SSS
           Y1=Y1+(NY-1)*YINC
           YINC=-YINC
           PRINT *, 'STATIC FILE REVERSED BECAUSE THE',
     $        ' Y GROUND POSITIONS ARE REVERSED'
      END IF
      RETURN
      END





      SUBROUTINE OPTIONC1 (MSG,   XOLD,XNEW)
C     CALLED FROM OPTIONC.
      PARAMETER (ZIP=-123.456E-27)
      CHARACTER*9 MSG
3     PRINT 4002, MSG
4002  FORMAT (' Type any ',A9,' ground position, and what you want to',
     $   ' change it to:')
      XOLD=ZIP
      XNEW=ZIP
      CALL HITXX (XOLD,XNEW)
      IF (XOLD.EQ.ZIP.OR.XNEW.EQ.ZIP) THEN
           PRINT *, 'YOU MUST SUPPLY TWO NUMBERS -- TRY AGAIN'
           GO TO 3
      END IF
      RETURN
      END






      FUNCTION RUNAV (I,Z,N,NPOINT,IEND,TP) 
C     RETURNS RUNNING AVERAGE OF POINT Z(I) OUT OF ARRAY Z(N).
C     NPOINT = NUMBER OF POINTS TO INCLUDE IN RUNNING AVERAGE.
C     IEND = TT FOR TRUNCATED END RANGE 
C            SS FOR SHIFTED END RANGE 
C            EE FOR EXTENDED END RANGE
C            NN FOR NARROWED END RANGE (GRADED ENDS)
      use named_constants_module
      PARAMETER (NXY=901)
      DIMENSION Z(N),X(NXY),Y(NXY)          
      CHARACTER*8 IEND
C----------GET PRELIMINARY VALUE OF RUNNING AVERAGE.
      RUNAV=Z(I)
C----------GET NUMBER OF POINTS IN ONE SIDE OF RUNNING AVERAGE. 
      NPOINT2=MIN0(NPOINT,NXY)
      MPOINT=NPOINT2/2
      IF (MPOINT.LE.0) RETURN 
      IF (IEND.EQ.'NN') MPOINT=MIN0(MPOINT,N-I,I-1) 
C----------GET LOWER AND UPPER POINT LIMITS FOR THE AVERAGE.
      JA=MAX0(I-MPOINT,1) 
      JB=MIN0(I+MPOINT,N) 
      IF (IEND.EQ.'SS') JA=MAX0(1,MIN0(I-MPOINT,N-2*MPOINT))
      IF (IEND.EQ.'SS') JB=MIN0(N,MAX0(I+MPOINT,1+2*MPOINT))
      IF (IEND.EQ.'EE') JA=I-MPOINT 
      IF (IEND.EQ.'EE') JB=I+MPOINT 
C----------LOAD UP THE ARRAY TO AVERAGE.
      KSUM=0
      DO 72 J=JA,JB 
      JJ=J
      IF (IEND.EQ.'EE') JJ=MIN0(N,MAX0(1,J))
      IF (Z(JJ).EQ.FNIL) GO TO 72 
      KSUM=KSUM+1 
      X(KSUM)=Z(JJ) 
72    CONTINUE
      IF (KSUM.EQ.0) RETURN 
C----------USE ONLY HALF OF EACH END POINT FOR EVEN NPOINT. 
      IF (NPOINT2.EQ.2*MPOINT.AND.KSUM.GE.2) THEN 
           X(1)=(X(1)+X(KSUM))/2. 
           KSUM=KSUM-1
      END IF
C----------GET THE AVERAGE OR TRIMMED MEAN VALUE. 
      RUNAV=QTRIM(TP,KSUM,X,Y)
      RETURN
      END 








      FUNCTION QTRIM (TP,N,X,Y) 
C     RETURNS TRIMMED MEAN OF ARRAY X(N). 
C     TP = TRIM PERCENT (0=MEAN, 100=MEDIAN). 
C     ARRAY Y(N) IS NEEDED FOR SCRATCH SPACE. 
C     UPON RETURN, ARRAY X(N) IS REARRANGED INTO ASCENDING ORDER. 
C     UPON RETURN, ARRAY Y(N) = ARRAY X(N). 
      DIMENSION X(N),Y(N) 
      NTP=(N-1)*TP/200.+0.5 
      NTP=MIN0((N-1)/2,MAX0(0,NTP)) 
      IA=1+NTP
      IB=N-NTP
      IF (NTP.GT.0) CALL QSORT22 (N,X,Y)
      SUM=0.
      DO 20 I=IA,IB 
20    SUM=SUM+X(I)
      QTRIM=SUM/(IB-IA+1) 
      RETURN
      END 








      SUBROUTINE OPTIONE (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
C     EDIT THE FILE.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      PARAMETER (NEDIT=7)
      DIMENSION S(NX,NY),KEDIT(NEDIT)
      CHARACTER*80 CARD(*)
      CHARACTER*4 BUF,Q
C----------GET STARTED.
      IEDIT=0
      JEDIT=0
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      YGP=Y1
      IY=1
      IF (NY.EQ.1) GO TO 8
C----------GET DESIRED Y GROUND POSITION. 
5     PRINT 1000, 'Y',Y1,YEND,'Y',YGP
1000  FORMAT (' STARTING AND ENDING ',A1,' GROUND POSITIONS ARE ',
     $   F8.1,1X,F8.1/
     $   ' TYPE ',A1,' GROUND POSITION WHERE EDIT WILL OCCUR',
     $   ' (or X to quit)  (DEFAULT ',F8.1,')')
      CALL HITXC3 (YGP,BUF,'X','X','X')
      IF (BUF.EQ.'X') GO TO 80
      IY=NINT((YGP-Y1)/YINC)+1
      IF (IY.LT.1.OR.IY.GT.NY) GO TO 80
      YGP=Y1+(IY-1)*YINC
C----------GET DESIRED X GROUND POSITION.
8     XGP=X1-XINC
9     IDEFAULT=0
10    XGP=XGP+XINC
      IF (XGP.GT.XEND+0.5*XINC) GO TO 8
      PRINT 1000, 'X',X1,XEND,'X',XGP
      CALL HITXC3 (XGP,BUF,'X','X','X')
      IF (BUF.EQ.'X') GO TO 70
            IF (IDEFAULT.EQ.0) THEN
                 PRINT *, 'DO YOU WISH TO EDIT THE GROUND POSITIONS',
     $                   ' CONSECUTIVELY?  (DEFAULT N)'
                 Q='N'
                 CALL HITQ (Q)
                 IDEFAULT=1
                 IF (Q.EQ.'Y') IDEFAULT=999
            END IF
CCCCCC      IF (BUF.EQ.' ') IDEFAULT=IDEFAULT+1
      IX=NINT((XGP-X1)/XINC)
11    IX=IX+1
      IF (IX.LT.1.OR.IX.GT.NX) GO TO 8
      XGP=X1+(IX-1)*XINC
C----------EDIT THE FILE. 
      PRINT 2000, YGP,XGP,S(IX,IY)
2000  FORMAT (' YGP=',F8.1,7X,'XGP=',F8.1,10X,'STATIC=',G13.5)
3000  FORMAT (38X,'NEW STATIC=',G13.5/)
      PRINT *, 'TYPE NEW STATIC VALUE (OR RETURN IF OK)', 
     $   '  (or N for nil)  (or X if finished)'
      CALL HITXC3 (S(IX,IY),BUF,'X','N','N')
      IF (BUF.EQ.'X') GO TO 9
      IF (BUF.EQ.' ') GO TO 68
C----------WE HAVE A NEW STATIC VALUE.
      IF (BUF.EQ.'N') S(IX,IY)=FNIL
      PRINT 3000, S(IX,IY)
      JEDIT=JEDIT+1
      IEDIT=IEDIT+1
      KEDIT(IEDIT)=NINT(XGP)
C----------GENERATE COMMENT CARD.
      IF (IEDIT.EQ.NEDIT) THEN
           IF (JEDIT.LE.10) THEN   !!! TURNS OFF EXCESSIVE NUMBER OF CARDS.
                NCARD=NCARD+1
                WRITE (CARD(NCARD),4000) NINT(YGP),(KEDIT(I),I=1,IEDIT)
           END IF
           IEDIT=0
      END IF
68    IF (IDEFAULT.GT.4) GO TO 11
      GO TO 10
C----------GENERATE COMMENT CARD.
70    IF (IEDIT.GT.0) THEN
           NCARD=NCARD+1
           WRITE (CARD(NCARD),4000) NINT(YGP),(KEDIT(I),I=1,IEDIT)
4000       FORMAT ('ISEP- EDIT  YGP=',I7,' XGP=',7I7)
           IEDIT=0
      END IF
      IF (NY.GT.1) GO TO 5
C----------FINISH UP AND RETURN.
80    IF (JEDIT.EQ.0) PRINT *, 'NO EDITING DONE'
      IF (JEDIT.GT.0) PRINT *, 'EDITING FINISHED'
      RETURN
      END





      SUBROUTINE OPTIONG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
C     GRADE THE FILE.
C----------DIMENSION STATEMENTS. 
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------GET STARTED.
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      IY=1
      IXA=1
      IXB=NX
      YGP=Y1
      XGPA=X1
      XGPB=XEND
C----------GET DESIRED Y GROUND POSITION. 
      PRINT *, 'STARTING AND ENDING Y GROUND POSITIONS ARE ',Y1,YEND
      PRINT *, 'TYPE Y GROUND POSITION WHERE GRADE WILL OCCUR',
     $   '  (DEFAULT ',YGP,')'
      CALL HITX (YGP)
      IY=NINT((YGP-Y1)/YINC)+1
      IF (IY.LT.1.OR.IY.GT.NY) GO TO 90
      YGP=Y1+(IY-1)*YINC
C----------GET STARTING X GROUND POSITION.
      PRINT *, 'STARTING AND ENDING X GROUND POSITIONS ARE ',X1,XEND
      PRINT *, 'TYPE X GROUND POSITION TO START GRADE',
     $   '  (DEFAULT ',XGPA,')'
      CALL HITX (XGPA)
      IXA=NINT((XGPA-X1)/XINC)+1
      IF (IXA.LT.1.OR.IXA.GT.NX) GO TO 90
      XGPA=X1+(IXA-1)*XINC
C----------GET ENDING X GROUND POSITION.
      PRINT *, 'TYPE X GROUND POSITION TO END GRADE',
     $   '  (DEFAULT ',XGPB,')'
      CALL HITX (XGPB)
      IXB=NINT((XGPB-X1)/XINC)+1
      IF (IXB.LE.IXA.OR.IXB.GT.NX) GO TO 90
      XGPB=X1+(IXB-1)*XINC
C----------PRINT GROUND POSITIONS AND STATIC VALUES..
      PRINT *, 'YGP=',YGP,'   XGP=',XGPA,'   STARTING STATIC=',S(IXA,IY)
      PRINT *, 'YGP=',YGP,'   XGP=',XGPB,'     ENDING STATIC=',S(IXB,IY)
C----------GET STARTING STATIC VALUE.
      PRINT *, 'TYPE NEW STARTING STATIC VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SA=S(IXA,IY)
      CALL HITX (SA) 
      IF (SA.EQ.888.) GO TO 90
      IF (SA.EQ.999.) SA=FNIL 
C----------GET ENDING STATIC VALUE.
      PRINT *, 'TYPE NEW ENDING STATIC VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SB=S(IXB,IY)
      CALL HITX (SB) 
      IF (SB.EQ.888.) GO TO 90
      IF (SB.EQ.999.) SB=FNIL 
C----------GRADE THE FILE BETWEEN TWO GROUND POSITIONS. 
      DS=(SB-SA)/(IXB-IXA)
      DO 56 IX=IXA,IXB
56    S(IX,IY)=SA+(IX-IXA)*DS
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) NINT(YGP),NINT(XGPA),NINT(XGPB)
1000  FORMAT ('ISEP- GRADE YGP=',I6,' XGP=',I6,' TO ',I6)
      PRINT *, 'GRADING FINISHED'
      RETURN
90    PRINT *, 'GRADING CANCELLED'
      RETURN
      END





            

      SUBROUTINE OPTIONGG (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD)
C     GRADE THE FILE.
C----------DIMENSION STATEMENTS. 
      use named_constants_module
      DIMENSION S(NX,NY)
      CHARACTER*80 CARD(*)
C----------GET STARTED.
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      IYA=1
      IYB=NY
      IXA=1
      IXB=NX
      YGPA=Y1
      YGPB=YEND
      XGPA=X1
      XGPB=XEND
C----------GET STARTING Y GROUND POSITION. 
      PRINT *, 'STARTING AND ENDING Y GROUND POSITIONS ARE ',Y1,YEND
      PRINT *, 'TYPE Y GROUND POSITION TO START GRADE',
     $   '  (DEFAULT ',YGPA,')'
      CALL HITX (YGPA)
      IYA=NINT((YGPA-Y1)/YINC)+1
      IF (IYA.LT.1.OR.IYA.GT.NY) GO TO 90
      YGPA=Y1+(IYA-1)*YINC
C----------GET ENDING Y GROUND POSITION. 
      PRINT *, 'TYPE Y GROUND POSITION TO END GRADE',
     $   '  (DEFAULT ',YGPB,')'
      CALL HITX (YGPB)
      IYB=NINT((YGPB-Y1)/YINC)+1
      IF (IYB.LT.1.OR.IYB.GT.NY) GO TO 90
      YGPB=Y1+(IYB-1)*YINC
C----------GET STARTING X GROUND POSITION.
      PRINT *, 'STARTING AND ENDING X GROUND POSITIONS ARE ',X1,XEND
      PRINT *, 'TYPE X GROUND POSITION TO START GRADE',
     $   '  (DEFAULT ',XGPA,')'
      CALL HITX (XGPA)
      IXA=NINT((XGPA-X1)/XINC)+1
      IF (IXA.LT.1.OR.IXA.GT.NX) GO TO 90
      XGPA=X1+(IXA-1)*XINC
C----------GET ENDING X GROUND POSITION.
      PRINT *, 'TYPE X GROUND POSITION TO END GRADE',
     $   '  (DEFAULT ',XGPB,')'
      CALL HITX (XGPB)
      IXB=NINT((XGPB-X1)/XINC)+1
      IF (IXB.LE.IXA.OR.IXB.GT.NX) GO TO 90
      XGPB=X1+(IXB-1)*XINC
C----------PRINT GROUND POSITIONS AND STATIC VALUES..
      PRINT *, 'XA=',XGPA,'   YA=',YGPA,'   STATIC(XA,YA)=',S(IXA,IYA)
      PRINT *, 'XB=',XGPB,'   YA=',YGPA,'   STATIC(XB,YA)=',S(IXB,IYA)
      PRINT *, 'XA=',XGPA,'   YB=',YGPB,'   STATIC(XA,YB)=',S(IXA,IYB)
      PRINT *, 'XB=',XGPB,'   YB=',YGPB,'   STATIC(XB,YB)=',S(IXB,IYB)
C----------GET NEW CORNER STATIC VALUES.
      PRINT *, 'TYPE NEW STATIC(XA,YA) VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SAA=S(IXA,IYA)
      CALL HITX (SAA) 
      IF (SAA.EQ.888.) GO TO 90
      IF (SAA.EQ.999.) SAA=FNIL 
      PRINT *, 'TYPE NEW STATIC(XB,YA) VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SBA=S(IXB,IYA)
      CALL HITX (SBA) 
      IF (SBA.EQ.888.) GO TO 90
      IF (SBA.EQ.999.) SBA=FNIL 
      PRINT *, 'TYPE NEW STATIC(XA,YB) VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SAB=S(IXA,IYB)
      CALL HITX (SAB) 
      IF (SAB.EQ.888.) GO TO 90
      IF (SAB.EQ.999.) SAB=FNIL 
      PRINT *, 'TYPE NEW STATIC(XB,YB) VALUE (OR RETURN IF OK)',
     $   ' (OR 999 FOR NIL) (OR 888 TO CANCEL)'
      SBB=S(IXB,IYB)
      CALL HITX (SBB) 
      IF (SBB.EQ.888.) GO TO 90
      IF (SBB.EQ.999.) SBB=FNIL 
C----------GRADE THE FILE BETWEEN FOUR CORNER GROUND POSITIONS.
      SLOPEYA=(SAB-SAA)/max(IYB-IYA,1)
      SLOPEYB=(SBB-SBA)/max(IYB-IYA,1)
      DO IY=IYA,IYB
      sa=saa+(iy-iya)*slopeya
      sb=sba+(iy-iya)*slopeyb
      SLOPEX=(SB-SA)/max(IXB-IXA,1)
      DO IX=IXA,IXB
      S(IX,IY)=SA+(IX-IXA)*SLOPEX
      END DO
      END DO
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000)
     $                     NINT(YGPA),NINT(YGPB),NINT(XGPA),NINT(XGPB)
1000  FORMAT ('ISEP- GRADE YGP=',I6,' TO ',I6,' XGP=',I6,' TO ',I6)
      PRINT *, 'GRADING FINISHED'
      RETURN
90    PRINT *, 'GRADING CANCELLED'
      RETURN
      END





            



      SUBROUTINE IPLOT (N,IA,IB,   X1,XINC,MSG)
C     RETURNS IA AND IB FOR INTERACTIVE PLOT DO-LOOP RANGE. 
C     CALL JUST BEFORE PRINTING HEADER FOLLOWED BY DO LOOP. 
C     SET IB=0 BEFORE CALLING THIS ROUTINE FIRST TIME.
C     PLOT IS FINISHED WHEN RETURNED IA=0.
C     EXAMPLE OF USE -- 
C             IB=0
C           3 CALL IPLOT (N,IA,IB,   X1,XINC,MSG)
C             IF (IA.EQ.0) GO TO 9
C             PRINT *, 'HEADER FOR PLOT'
C             DO 5 I=IA,IB
C           5 PRINT *, I,X(I),Y(I)
C             GO TO 3 
C           9 CONTINUE
C----------DIMENSION STATEMENTS.
      PARAMETER (NPAGE=19)
      CHARACTER*8 MSG
C----------DO THE WORK. 
      IA=IB+1 
      IF (IA.GT.N) IA=0 
      IF (IB.EQ.0) GO TO 60 
      IF (N.LE.NPAGE) GO TO 80
      XEND=X1+(N-1)*XINC
      XGP=X1+(IA-1)*XINC
      IX1=NINT(X1)
      IXEND=NINT(XEND)
      IZERO=MIN0(IX1-1,0)
      IXGP=NINT(XGP)
      IF (IXGP.LT.IX1) IXGP=IZERO
      PRINT 1000, MSG,IXGP,IX1,IXEND,IZERO
      KEEP=IXGP
      CALL HITI (IXGP)
      IF (IXGP.LE.IZERO) GO TO 80
      IA=NINT((IXGP-X1)/XINC)+1
      IA=MIN0(N,MAX0(1,IA))
      IF (IXGP.EQ.KEEP) GO TO 60 
      IA=MAX0(1,MIN0(IA,N-NPAGE+1)) 
60    IB=MIN0(IA+NPAGE-1,N) 
      RETURN
80    IA=0
      IB=0 
      RETURN
C----------ALTERNATE ENTRY FOR SINGLE VALUE.
      ENTRY IPICK (N,IA,   X1,XINC,MSG)
      IA=IA+1
      IF (IA.GT.N) IA=0
      XEND=X1+(N-1)*XINC
      XGP=X1+(IA-1)*XINC
      IX1=NINT(X1)
      IXEND=NINT(XEND)
      IZERO=MIN0(IX1-1,0)
      IXGP=NINT(XGP)
      IF (IXGP.LT.IX1) IXGP=IZERO
      PRINT 2000, MSG,IXGP,IX1,IXEND,IZERO
      CALL HITI (IXGP)
      IF (IXGP.LE.IZERO) GO TO 90
      IA=NINT((IXGP-X1)/XINC)+1
      IA=MIN0(N,MAX0(1,IA))
      RETURN
90    IA=0
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (' TYPE NEXT ',A8,6X,'(DEFAULT',I6,')',
     $   6X,'(',I4,' TO',I6,',  OR',I4,' TO QUIT)')
2000  FORMAT (' TYPE ANOTHER ',A8,3X,'(DEFAULT',I6,')',
     $   6X,'(',I4,' TO',I6,',  OR',I4,' TO QUIT)')
      END 





      SUBROUTINE HIT
      use string_module
      CHARACTER*(*) C,C1,C2,C3
      CHARACTER*80 BUF
C----------READ CARRIAGE RETURN.
      PRINT *, '(HIT RETURN TO CONTINUE)' 
      ENTRY HIT2
      READ (*,*) 
      RETURN
C----------READ Y(YES) OR N(NO).
      ENTRY HITQ2 (C)
      PRINT *, 'DEFAULT=', C
      ENTRY HITQ (C)
      ASSIGN 20 TO JUMP
      GO TO 888
20    READ (BUF,'(A1)',ERR=777) C
!!!!  call convert_to_upper (c)
      call string_to_upper (c)
      IF (C.EQ.'Y'.OR.C.EQ.'N') RETURN
      PRINT *, 'TRY AGAIN  (Y OR N ALLOWED)'
      GO TO 888
C----------READ INTEGER.
      ENTRY HITI2 (I)
      PRINT *, 'DEFAULT=',I
      ENTRY HITI (I)
      ASSIGN 30 TO JUMP
      GO TO 888
30    READ (BUF,*,ERR=777) I
      RETURN 
C----------READ FLOATING POINT NUMBER.
      ENTRY HITX2 (X)
      PRINT 1000, X
1000  FORMAT (' DEFAULT=',G13.5)
      ENTRY HITX (X)
      ASSIGN 40 TO JUMP
      GO TO 888
40    READ (BUF,*,ERR=777) X
      RETURN 
C----------READ CHARACTER WORD. 
      ENTRY HITC2 (C)
      PRINT *, 'DEFAULT=',C
      ENTRY HITC (C)
      ASSIGN 50 TO JUMP
      GO TO 888
50    READ (BUF,'(A)',ERR=777) C
!!!!  call convert_to_upper (c)
      call string_to_upper (c)
      RETURN 
C----------READ CHARACTER WORD without converting to upper case. 
      ENTRY HITC2_keep (C)
      PRINT *, 'DEFAULT=',C
      ENTRY HITC_keep (C)
      ASSIGN 51 TO JUMP
      GO TO 888
51    READ (BUF,'(A)',ERR=777) C
      RETURN 
C----------READ TWO FLOATING POINT NUMBERS. 
      ENTRY HITXX2 (X,Y)
      PRINT *, 'DEFAULT=',X,Y
      ENTRY HITXX (X,Y)           
      ASSIGN 60 TO JUMP
      GO TO 888
60    READ (BUF,*,ERR=777) X,Y
      RETURN 
C----------READ TWO INTEGERS.
      ENTRY HITII2 (I,J)
      PRINT *, 'DEFAULT=',I,J
      ENTRY HITII (I,J) 
      ASSIGN 70 TO JUMP
      GO TO 888
70    READ (BUF,*,ERR=777) I,J
      RETURN 
C----------READ FOUR INTEGERS.
      ENTRY HITIIII2 (I,J,K,L)
      PRINT *, 'DEFAULT=',I,J,K,L
      ENTRY HITIIII (I,J,K,L) 
      ASSIGN 80 TO JUMP
      GO TO 888
80    READ (BUF,*,ERR=777) I,J,K,L
      RETURN 
C----------READ FLOATING POINT NUMBER OR SELECTED CHARACTER.
C          RETURNS C=' ' IF DEFAULT IS TAKEN.
C          RETURNS C=C1 OR C2 OR C3 IF ONE OF THESE IS TYPED.
C          RETURNS C='.' (AND CHANGED X) IF NUMBER IS TYPED.
      ENTRY HITXC3 (X,C,C1,C2,C3)
      C=' '
      ASSIGN 90 TO JUMP
      GO TO 888
90    READ (BUF,*,ERR=91) XX
      X=XX
      C='.'
      RETURN 
91    READ (BUF,'(A)',ERR=777) C
!!!!  call convert_to_upper (c)
      call string_to_upper (c)
      IF (C.NE.C1.AND.C.NE.C2.AND.C.NE.C3) GO TO 777
      RETURN 
C----------READ A LINE FROM THE SCREEN.
777   PRINT *, 'ERROR - TRY AGAIN'
888   READ (*,'(A80)') BUF
      IF (BUF.EQ.' ') RETURN
      GO TO JUMP
      END 
                        


                   
      SUBROUTINE LIMITS (N,X1,X2,X3,   XLO,XUP)
C     GET LIMITING VALUES. 
      DIMENSION X1(N),X2(N),X3(N)
      XLO=X1(1)
      XUP=X1(1)
      DO 10 I=1,N
      XLO=AMIN1(X1(I),X2(I),X3(I),XLO)
10    XUP=AMAX1(X1(I),X2(I),X3(I),XUP)
      IF (XUP.EQ.XLO) XUP=XLO+0.0001
      RETURN
      END










      SUBROUTINE MOVE (A,B,N)
      DIMENSION A(N),B(N)
      DO 10 I=1,N
10    B(I)=A(I)
      RETURN
      END






      SUBROUTINE PLOTLINE (K,L,X,XLO,XUP,LETTER,A) 
C----------DIMENSION STATEMENTS.
      CHARACTER*8 LETTER,A(*)
      CHARACTER*8 BUF
C----------GET STARTED.
      N=K*L+1 
      IF (K.EQ.0) N=L+1 
      IF (LETTER(1:1).NE.'(') GO TO 40 
C----------BUILD HEADER ARRAY.
      DO 5 I=1,N
5     A(I)=' ' 
      IF (K.EQ.0) RETURN
      DO 10 I=K,1,-1
      Y=XLO+I*(XUP-XLO)/K 
      WRITE  (BUF,LETTER) Y
      DO 8 J=1,8 
      JJ=MAX0(I*L-7+J,1)
8     A(JJ)=BUF(J:J)
10    CONTINUE
      WRITE (BUF,LETTER) XLO 
      READ (BUF,'(A8)') A(1) 
      RETURN
C----------BUILD PLOT LINE ARRAY. 
40    INDEX=1.5+(X-XLO)/(XUP-XLO)*(N-1) 
      INDEX=MIN0(N,MAX0(INDEX,1)) 
C----------INITIALIZE ARRAY AND CHOOSE WHICH CHARACTER TO PLOT.
      IF (LETTER(2:2).EQ.' '.OR.LETTER(2:2).EQ.'.') THEN
           DO 20 I=1,N 
20         A(I)=LETTER(2:2)
           IF (K.NE.0) THEN
                DO 50 I=1,N,L 
50              A(I)=':'
           END IF
           KK=1
      ELSE IF (A(INDEX).EQ.' '.OR.A(INDEX).EQ.'.'
     $                        .OR.A(INDEX).EQ.':') THEN
           KK=1
      ELSE
           KK=2
      END IF
C----------ADD THE POINT TO THE GRAPH.
      A(INDEX)=LETTER(KK:KK)
      RETURN
      END









      SUBROUTINE QSORT22 (N,A,B)
      DIMENSION A(N),B(N) 
C     SORTS REAL ARRAY A(N) INTO ASCENDING ORDER AND PUTS RESULTS 
C        INTO BOTH A AND B. 
C     THE SECOND ARRAY IS NEEDED FOR SCRATCH SPACE. 
C----------GET STARTED. 
      KK=0
      NN=1
1     NN=2*NN 
      KK=KK+1 
      IF (N.GT.NN) GO TO 1
      L1=1
C----------DO THE SORT (TIME PROPORTIONAL TO N LOG N).
      DO 50 II=1,KK 
      L11=L1
      L1=2*L1 
      DO 10 I=1,N,L1
      I1=I
      I2=I+L11
      I11=MIN0(I1+L11-1,N)
      I22=MIN0(I2+L11-1,N)
      J2=MIN0(I+L1-1,N) 
      DO 25 J=I,J2
      IF (I1.GT.I11) GO TO 19 
      IF (I2.GT.I22) GO TO 20 
      IF (A(I1).LT.A(I2)) GO TO 20
19    B(J)=A(I2)
      I2=I2+1 
      GO TO 25
20    B(J)=A(I1)
      I1=I1+1 
25    CONTINUE
10    CONTINUE
50    CALL MOVE (B,A,N) 
      RETURN
      END 







      SUBROUTINE PLOTL (XLO,XUP,YLO,YUP,LY) 
C     INITIALIZES ALL OF COMMON BLOCKS PLOTZ1 AND PLOTZ2.
C     CALL THIS ROUTINE BEFORE CALLING PLOTXY, PLOTY, PLOTX.
C     XLO,XUP = LIMITS ON X-AXIS.
C     YLO,YUP = LIMITS ON Y-AXIS.
C     LY = NUMBER OF ROWS IN THE PLOT.
C     COMMON BLOCK  PLOTZ1  EXISTS SO THAT AXIS ARRAYS  XAXIS,YAXIS  CAN
C        BE ALTERED.
C     COMMON BLOCK  PLOTZ2  EXISTS SO THAT  PLOTXY  CAN BE SUPPLEMENTED
C        OR BYPASSED BY ADDING POINTS DIRECTLY TO CHARACTER ARRAY  P.
C     LY  NEGATIVE ASSUMES THE RESULTS WILL BE PLOTTED ON A TERMINAL 
C        RATHER THAN LINE PRINTER.
C     LY  ZERO CHANGES THE LIMITS BUT DOES NOT ZERO OUT THE GRAPH OR 
C        CHANGE THE NUMBER OF ROWS OR COLUMNS.
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
C----------CHOOSE THE NUMBER OF ROWS AND COLUMNS.
      IF (LY.NE.0) NROWS=MIN0(60,MAX0(IABS(LY),2)) 
      IF (LY.GT.0) NCOL=121
      IF (LY.LT.0) NCOL=65                     
C----------SET UP THE STARTING POINTS AND INCREMENTS.
      DX=(XUP-XLO)/(NCOL-1) 
      DY=(YUP-YLO)/(NROWS-1)
      IF (DX.EQ.0.) DX=1./(NCOL-1) 
      IF (DY.EQ.0.) DY=1./(NROWS-1) 
      CCC=1.0-XLO/DX
      RRR=1.0+YUP/DY
C----------GENERATE THE AXIS VALUES.
      DO 5 IC=1,121 
5     XAXIS(IC)=XLO+(IC-1)*DX 
      DO 7 IR=1,60
7     YAXIS(IR)=YUP-(IR-1)*DY 
C----------INITIALIZE THE PLOT ARRAY.
      IF (LY.EQ.0) RETURN
      DO 8 IR=1,60
8     P(IR)=' '
      DO 10 IR=1,60,3
      DO 10 IC=1,121
10    P(IR)(IC:IC)='.'
      DO 11 IC=1,121
11    P(NROWS)(IC:IC)='.'
      DO 9 IR=2,60
      DO 9 IC=1,121,8
9     P(IR)(IC:IC)=':'
      RETURN
      END 


             





      SUBROUTINE PLOTXY (CHAR,X,Y)
C     PLOT POINT  (XA,YA)  USING CHARACTER  CHAR(1:1).
C     USES CHAR(2:2) IF PLOTTING OVER ANOTHER ALREADY-PLOTTED CHARACTER
C        THAT IS DIFFERENT FROM CHAR(1:1), BUT ONLY IF CHAR(2:2) IS NOT
C        BLANK.
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
      CHARACTER*2 CHAR
      CHARACTER*1 TEST
C----------DO THE WORK.
      IC=NINT(CCC+X/DX) 
      IR=NINT(RRR-Y/DY)
      IC=MIN0(NCOL,MAX0(IC,1))                             
      IR=MIN0(NROWS,MAX0(IR,1)) 
      TEST=P(IR)(IC:IC)
      IF (TEST.EQ.' '.OR.TEST.EQ.'.'.OR.TEST.EQ.':'
     $   .OR.TEST.EQ.CHAR(1:1).OR.CHAR(2:2).EQ.' ') THEN
           P(IR)(IC:IC)=CHAR(1:1)
      ELSE
           P(IR)(IC:IC)=CHAR(2:2)
      END IF
      RETURN
      END 






                 

      SUBROUTINE PLOTY (FMT) 
C     PRINTS GRAPH AND ORDINATE VALUES. 
C     CALL ROUTINES IN THIS ORDER -- PLOTL,PLOTXY,PLOTY,PLOTX.
C     FMT = FORMAT FOR PRINTING ORDINATES -- EXAMPLE:  '(F10.3)' .
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      COMMON/PLOTZ2/P
      CHARACTER*121 P(60)
      CHARACTER*(*) FMT
      CHARACTER*10 T
C----------PRINT GRAPH. 
      DO 40 IR=1,NROWS
      WRITE (T,FMT) YAXIS(IR)
      IF (T(10:10).EQ.'.') T(10:10)=' '
40    PRINT 200, T,(P(IR)(IC:IC),IC=1,NCOL) 
200   FORMAT (1X,A10,2X,121A1)
      RETURN
      END 

                 






      SUBROUTINE PLOTX2 (MSG,FMT)
C     PRINTS ABSCISSA VALUES HORIZONTALLY.
C     FMT = FORMAT FOR PRINTING ORDINATES -- EXAMPLE:  '(F8.3)' .
C----------DIMENSION STATEMENTS.
      COMMON/PLOTZ1/XAXIS(121),YAXIS(60),DX,DY,CCC,RRR,NROWS,NCOL 
      CHARACTER*(*) MSG,FMT
      CHARACTER*8 BUFFER,LLL(121)
C----------DO THE WORK.
      DO 20 IC=1,NCOL,8 
      WRITE (BUFFER,FMT) XAXIS(IC)
      LLL(IC)=BUFFER
      IF (BUFFER(8:8).EQ.'.') LLL(IC)=' '//BUFFER(1:7)
20    CONTINUE
      PRINT 600, MSG,(LLL(IC),IC=1,NCOL,8) 
600   FORMAT (1X,A5,16A8) 
      RETURN
      END 
                  





      SUBROUTINE TRUNK (ICHOICE,knils,X1,Y1,XINC,YINC,NX,NY,S,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   CARD,NCARD)
C     TRUNCATE OR EXTEND STATIC FILE, OR CHANGE INCREMENT.
C     ICHOICE=1 USES NEAREST VALUES (NILS PRESERVED).
C     ICHOICE=2 INTERPOLATES (NILS USED AS NEARLY ZERO).
C     knils=0 EXTRAPOLATES WITH EDGE VALUE.
C     knils=1 EXTRAPOLATES WITH NILS.
C     X1,Y1,XINC,YINC,NX,NY = ATTRIBUTES FOR INPUT FILE.
C     XX1,YY1,XXINC,YYINC,NXX,NYY = ATTRIBUTES FOR OUTPUT FILE.
C     ARRAY S = INPUT FILE.
C     ARRAY SS = OUTPUT FILE.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      DIMENSION S(NX,NY),SS(NXX,NYY)
      CHARACTER*80 CARD(*)
      CHARACTER*9 CHOICE(2)
      CHARACTER*4 NIL(2)
      DATA CHOICE/'NEAR VALS','INTERP'/
      DATA NIL/'EDGE','NILS'/
C----------PRESET OUTPUT ARRAY TO NILS.
      IF (knils.GE.1) THEN
           DO 5 IXX=1,NXX
           DO 5 IYY=1,NYY
5          SS(IXX,IYY)=FNIL
      END IF
C----------USE NEAREST VALUES.
      IF (ICHOICE.LE.1) THEN
      DO 31 IXX=1,NXX
         HD=XX1+(IXX-1)*XXINC
         IX=NINT((HD-X1)/XINC)+1
         IF (knils.GE.1) THEN
              IF (IX.LT.1.OR.IX.GT.NX) GO TO 31
         END IF
         IX=MIN0(NX,MAX0(IX,1))
      DO 30 IYY=1,NYY
         HD=YY1+(IYY-1)*YYINC
         IY=NINT((HD-Y1)/YINC)+1
         IF (knils.GE.1) THEN
              IF (IY.LT.1.OR.IY.GT.NY) GO TO 30
         END IF
         IY=MIN0(NY,MAX0(IY,1))
      SS(IXX,IYY)=S(IX,IY)
30    CONTINUE
31    CONTINUE
C----------INTERPOLATE.
      ELSE
      DO 41 IXX=1,NXX
         HD=XX1+(IXX-1)*XXINC
         X=(HD-X1)/XINC+1.
         IX=X
         JX=IX+1
         IF (knils.GE.1) THEN
              IF (JX.LT.1.OR.IX.GT.NX) GO TO 41
         END IF
         IX=MIN0(NX,MAX0(IX,1))
         JX=MIN0(NX,MAX0(JX,1))
      DO 40 IYY=1,NYY
         HD=YY1+(IYY-1)*YYINC
         Y=(HD-Y1)/YINC+1.
         IY=Y
         JY=IY+1
         IF (knils.GE.1) THEN
              IF (JY.LT.1.OR.IY.GT.NY) GO TO 40
         END IF
         IY=MIN0(NY,MAX0(IY,1))
         JY=MIN0(NY,MAX0(JY,1))
      SIY=S(IX,IY)+(X-IX)*(S(JX,IY)-S(IX,IY))
      SJY=S(IX,JY)+(X-IX)*(S(JX,JY)-S(IX,JY))
      SS(IXX,IYY)=SIY+(Y-IY)*(SJY-SIY)
40    CONTINUE
41    CONTINUE   
      END IF
C----------ADD COMMENT CARDS.
      NCARD=NCARD+1
      XEND=X1+(NX-1)*XINC
      YEND=Y1+(NY-1)*YINC
      WRITE (CARD(NCARD),1000) 'OLD',NINT(X1),NINT(XEND),
     $   NINT(XINC),NINT(Y1),NINT(YEND),NINT(YINC),CHOICE(ICHOICE),
     $   NIL(knils+1)
      NCARD=NCARD+1
      XEND=XX1+(NXX-1)*XXINC
      YEND=YY1+(NYY-1)*YYINC
      WRITE (CARD(NCARD),1000) 'NEW',NINT(XX1),NINT(XEND),
     $   NINT(XXINC),NINT(YY1),NINT(YEND),NINT(YYINC),CHOICE(ICHOICE),
     $   NIL(knils+1)
1000  FORMAT ('ISEP- TRUNC ',A3,'  XGP=',3I6,' YGP=',3I6,2X,A9,1X,A4)
      RETURN
      END







      SUBROUTINE OPTIONT (X1,Y1,XINC,YINC,NX,NY,S,NCARD,CARD,SS)
C     TRUNCATE OR EXTEND OR RESAMPLE STATIC FILE BY ASKING QUESTIONS.
C----------DIMENSION STATEMENTS.
      DIMENSION S(*),SS(*)
      CHARACTER*80 CARD(*)
C----------ASK QUESTIONS REGARDING X AND Y GROUND POSITIONS.
      CALL GPQUEST ('X',X1,XINC,NX,   XX1,XXINC,NXX)
      CALL GPQUEST ('Y',Y1,YINC,NY,   YY1,YYINC,NYY)
      IF (XX1.EQ.X1.AND.YY1.EQ.Y1.AND.XXINC.EQ.XINC.AND.
     $   YYINC.EQ.YINC.AND.NXX.EQ.NX.AND.NYY.EQ.NY) RETURN
      CALL HOW (ICHOICE)
C----------DO THE TRUNCATION AND/OR EXTENSION AND/OR RESAMPLING.
      CALL TRUNK (ICHOICE,0,X1,Y1,XINC,YINC,NX,NY,S,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   CARD,NCARD)
C----------COPY NEW STUFF TO OLD LOCATION.
      X1=XX1
      Y1=YY1
      XINC=XXINC
      YINC=YYINC
      NX=NXX
      NY=NYY
      CALL MOVE (SS,S,NX*NY)
      RETURN                 
      END





      SUBROUTINE GPQUEST (CHAR,X1,XINC,NX,   XX1,XXINC,NXX)
C     GET NEW GROUND POSITION ATTRIBUTES BY ASKING QUESTIONS.
      CHARACTER*1 CHAR
      CHARACTER*8 Q
C----------ASK QUESTION REGARDING GROUND POSITION INCREMENT.
      PRINT *, CHAR,' GROUND POSITION INCREMENT IS ',XINC
      PRINT *, 'TYPE NEW INCREMENT   (OR RETURN IF OK)'
20    XXINC=XINC
      CALL HITX (XXINC)
      IF (XXINC.LE.0.) THEN
           PRINT *, 'VALUE CANNOT BE ZERO OR LESS -- TRY AGAIN'
           GO TO 20
      END IF
C----------ASK QUESTION REGARDING GROUND POSITION RANGE.
30    XEND=X1+(NX-1)*XINC
      PRINT *, CHAR,' GROUND POSITION RANGE IS ',X1,' TO ',XEND
      PRINT *, 'TYPE NEW STARTING GROUND POSITION   (OR RETURN IF OK)'
      XX1=X1
      CALL HITX (XX1)
      PRINT *, 'TYPE NEW ENDING GROUND POSITION   (OR RETURN IF OK)'
      XXEND=XEND
      CALL HITX (XXEND)
C----------CHECK FOR VALID INFORMATION.
      NXX=NINT((XXEND-XX1)/XXINC)+1
      IF (NXX.LE.0) THEN
           PRINT *, 'ZERO OR NEGATIVE NUMBER OF GROUND POSITIONS',
     $   ' -- TRY AGAIN'
           GO TO 30
      END IF
C----------FINISH UP.
      IF (AMOD(XX1-X1,XINC).EQ.0..AND.AMOD(XX1-X1,XXINC).EQ.0.) RETURN
      PRINT *, 'THE DIFFERENCE BETWEEN THE OLD AND NEW STARTING ',
     $   CHAR,' GROUND POSITIONS'
      PRINT *, 'IS NOT AN INTEGRAL MULTIPLE OF THE GROUND POSITION',
     $   ' INCREMENT.'
      PRINT *, 'DO YOU WANT TO CHANGE THE NEW GROUND ',
     $   'POSITION RANGE?  (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'Y') GO TO 30
      RETURN
      END



                              
           
                      

      SUBROUTINE OPTIONJ (TYPE,NHX,NHY,NHX3,NHY3,X1,Y1,XINC,YINC,
     $   NX,NY,S,NCARD,CARD,S1,S2,S3,NNNN,KKKK)
C     JOIN OR COMPARE WITH ANOTHER FILE.
C     S = INPUT FILE (INPUT AND OUTPUT).
C     S1 = RESAMPLED INPUT FILE (EXTENDED IF NECESSARY).
C     S2 = RESAMPLED SECOND FILE (EXTENDED AND RESAMPLED IF NECESSARY).
C     S3 = COMBINED FILE (COMBINATION OF S1 AND S2).
C     CARD = COMMENT CARDS.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION S(NNNN),S1(NNNN),S2(NNNN),S3(NNNN)
      CHARACTER*8 OPTION,TYPE,TYPE2,Q
      CHARACTER*80 CARD(*)
      CHARACTER*80 VAXFILE
C----------READ A SECOND STATIC FILE.
      PRINT *, 'NOW LET''S READ A SECOND CPS STATIC FILE'
      CALL RFILE (VAXFILE,TYPE2,NHX2,NHY2,NHX4,NHY4,XX1,YY1,XXINC,YYINC,
     $   NXX,NYY,CARD(NCARD+2),NDUMMY,S3,NNNN,KKKK)
      IF (VAXFILE.EQ.' ') RETURN
      CARD(NCARD+1)='ISEP- ****** BEGIN CARDS FROM SECOND FILE ******'
      NCARD1=NCARD+NDUMMY+2
      CARD(NCARD1)='ISEP- ****** END OF CARDS FROM SECOND FILE ******'
      IFLAG=2
C----------MAKE THE TWO FILES MATCH.
      PRINT 1000, 'FIRST ',NHX,NHY,NHX3,NHY3,TYPE
      PRINT 1000, 'SECOND',NHX2,NHY2,NHX4,NHY4,TYPE2
1000  FORMAT (' HEADER WORDS FOR ',A6,' FILE ARE:  ',2I3,3X,2I3,
     $   '   TYPE=',A8)
      CALL MATCH (X1,Y1,XINC,YINC,NX,NY,S,   S1,
     $     XX1,YY1,XXINC,YYINC,NXX,NYY,S3,   S2,
     $     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,   CARD,NCARD1)
C----------CHOOSE OPERATION.
2     PRINT *, ' '
      PRINT *, '     P = PLOT THE TWO FILES'
      PRINT *, '     H = MAKE HORIZONTAL PLOTS OF THE TWO FILES'
      PRINT *, '  HELP = see note regarding options N,AV,ADD,SUB,SPLICE'
      PRINT *, '     N = REPLACE NIL''S (BOTH FILES) WITH INTERPOLATED',
     $                       ' VALUES'
      PRINT *, '     Z = REPLACE NIL''S (BOTH FILES) WITH ZEROES'
      PRINT *, '    AV = AVERAGE THE FILES TOGETHER'
      PRINT *, '   ADD = ADD THE FILES TOGETHER'
      PRINT *, '   SUB = SUBTRACT SECOND FILE FROM FIRST FILE'
      PRINT *, 'SPLICE = SPLICE THE FILES TOGETHER' 
      PRINT *, '  DONE = FINISHED BUILDING COMBINED FILE' 
      PRINT *, 'CHOOSE ONE OF THE ABOVE FILE-COMBINING OPTIONS' 
      OPTION=' '
      CALL HITC (OPTION)
C----------ACT ON DESIRED OPERATION.
      IF (OPTION.EQ.'P') THEN                
           CALL OPTIONP (IFLAG,XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,
     $        S1,S2,S3)
           CALL HIT
      ELSE IF (OPTION.EQ.'H') THEN
           CALL OPTIONH (IFLAG,XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,
     $        S1,S2,S3)
           IF (NXXX.EQ.1.OR.NYYY.EQ.1) CALL HIT
      ELSE IF (OPTION.EQ.'HELP') THEN
           CALL OPTHELP
      ELSE IF (OPTION.EQ.'N') THEN
           CALL OPTIONN (S1,NXXX,NYYY,CARD,NCARD)
           CALL OPTIONN (S2,NXXX,NYYY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'Z') THEN
           CALL OPTIONZ (S1,NXXX,NYYY,CARD,NCARD)
           CALL OPTIONZ (S2,NXXX,NYYY,CARD,NCARD)
      ELSE IF (OPTION.EQ.'AV') THEN
           CALL OPTAV (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3
      ELSE IF (OPTION.EQ.'ADD') THEN
           CALL OPTADD (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3       
      ELSE IF (OPTION.EQ.'SUB') THEN
           CALL OPTSUB (S1,S2,S3,NXXX,NYYY,CARD,NCARD1)
           IFLAG=3
      ELSE IF (OPTION.EQ.'SPLICE') THEN
           CALL SPLICE (X1,Y1,XINC,YINC,NX,NY,S1,
     $        XX1,YY1,XXINC,YYINC,NXX,NYY,S2,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,S3,
     $        CARD,NCARD1)
           IFLAG=3
C----------GO BACK TO MENU. 
      END IF
      IF (OPTION.NE.'DONE') GO TO 2
C----------FINISHED.
      IF (IFLAG.EQ.2) RETURN
      PRINT *, 'TYPE  Y  TO CONTINUE WITH THE NEW COMBINED FILE',
     $                      '  (DEFAULT)'
      PRINT *, 'TYPE  N  TO CONTINUE WITH THE UNCHANGED SINGLE FILE'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'N') THEN
           PRINT *, 'WE ARE CONTINUING WITH THE UNCHANGED SINGLE FILE'
           RETURN
      END IF
C----------REPLACE ORIGINAL FILE WITH NEW COMBINED FILE.
      NCARD=NCARD1
      X1=XXX1
      Y1=YYY1
      XINC=XXXINC
      YINC=YYYINC
      NX=NXXX
      NY=NYYY
      CALL MOVE (S3,S,NX*NY)
      PRINT *, 'WE ARE CONTINUING WITH THE NEW COMBINED FILE'
      RETURN
      END



      SUBROUTINE OPTIONS (OPTION,VAXFILE,TYPE,NHX,NHY,NHX2,NHY2,S2, 
     $                 X1,Y1,XINC,YINC,NX,NY,CARD,NCARD,S,A,NNNN)
C     ISEP SUBROUTINE TO ALLOW THE USER TO MORE EASILY WORK WITH SISC
C     INCREMENT FILES.  ROUTINE ALLOWS THE USER TO AUTOMATICALLY CONVERT 
C     THE INCREMENT FILE TO A CBYT MUTE FILE, OR FROM A CBYT MUTE FILE TO 
C     AN INTEGRATED, AVERAGED, SISC FILE.
C----------DIMENSION AND DATA STATEMENTS.
      use statio_module
      use addext_module
!      PARAMETER (LFN=77)
      DIMENSION S(NX,NY),A(*),S2(NX,NY)
      CHARACTER*8 extn
      CHARACTER*80 CARD(NCARD),VAXFILE2 
      CHARACTER*(*) VAXFILE  
      CHARACTER*25 DDDDTTTT
      CHARACTER*8 TYPE,IOPTN 
      CHARACTER*8 OPTION,IEND
      integer err
      character(len=80) msg
      DATA NXRUN,NYRUN,IEND,TP/21,1,'NN',0./
C-----PROMPT USER TO DETERMINE WHAT TYPE OF ACTION TO TAKE
5     PRINT *, '                                                 '
      PRINT *, 'TYPE  MUTE  TO CONVERT FILE TO .mute FILE FOR CBYT'
      PRINT *, 'TYPE  SISC  TO CONVERT FILE to .sisc FILE FOR CPS'
      PRINT *, 'TYPE  MENU  TO RETURN TO MAIN PROGRAM'
      PRINT *, 'TYPE  X  TO EXIT THE PROGRAM'
      PRINT *, '                                                 '
      PRINT *, 'CHOOSE DESIRED OPTION -- (DEFAULT = QUIT)'
      PRINT *, '                     '
      IOPTN='X'
      CALL HITC (IOPTN)
      PRINT *, '                                                 '
      PRINT *, 'OPTION=' ,IOPTN
      PRINT *, '                                                 '
C----------ACT ON THE CHOSEN OPTION.
      IF ('MUTE'.EQ.IOPTN.OR.'SISC'.EQ.IOPTN.OR.'MENU'.EQ.IOPTN
     $    .OR.'X'.EQ.IOPTN) GO TO 10
      GO TO 5
10    IF ('MUTE'.EQ.IOPTN)GO TO 20 
      IF ('SISC'.EQ.IOPTN)GO TO 85
      IF ('MENU'.EQ.IOPTN)GO TO 111   
      IF ('X'.EQ.IOPTN)GO TO 125
      IF ('MUTE'.EQ.IOPTN.OR.'SISC'.EQ.IOPTN.OR.'MENU'.EQ.IOPTN
     $    .OR.'X'.EQ.IOPTN) GO TO 10 
C----------SETUP DEFAULTS
      extn='mute'
C----------DO THE WORK (MULTIPLY BY -1.0).
20    CONST=-1. 
      DO 80 IY=1,NY
      DO 80 IX=1,NX
80    S(IX,IY)=S(IX,IY)*CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1000) CONST
1000  FORMAT ('ISEP- MULTIPLY STATIC VALUES BY CONSTANT',G13.5)
      PRINT *, 'STATIC VALUES HAVE BEEN MULTIPLIED BY ',CONST
      IF ('SISC'.EQ.IOPTN)GO TO 100
C----------DO THE WORK (ADD A CONSTANT TIME TO EACH VALUE).
85    PRINT *, 'TYPE A CONSTANT TO ADD TO EACH VALUE  (DEFAULT 400).'
      PRINT *, '                                                 '
      PRINT *, '                   NOTE:                           ' 
      PRINT *, '(YOUR VALUE MUST BE IN MILLISECONDS AND SHOULD BE',
     $               ' A POSITIVE NUMBER.)'
      PRINT *, '                                                 '
      PRINT *, '(THIS ROUTINE AUTOMATICALLY CONVERTS TO A NEGATIVE', 
     $            ' NUMBER IF YOU CHOOSE OPTION=SISC.)'
      CONST=400.
      CALL HITX (CONST)
      IF ('MUTE'.EQ.IOPTN)GO TO 92 
      IF ('SISC'.EQ.IOPTN)GO TO 90 
90    CONST=CONST*(-1.0)  
92    DO 95 IY=1,NY
      DO 95 IX=1,NX
95    S(IX,IY)=S(IX,IY)+CONST
C----------GENERATE COMMENT CARD.
      NCARD=NCARD+1
      WRITE (CARD(NCARD),1010) CONST
1010  FORMAT ('ISEP- ADD CONSTANT',G13.5,' TO STATIC VALUES')
      PRINT *, 'CONSTANT ',CONST,' HAS BEEN ADDED TO STATIC VALUES'
      IF ('SISC'.EQ.IOPTN)GO TO 20
      IF ('MUTE'.EQ.IOPTN)GO TO 110
C
C----------CONVERT FILE TYPE FROM 'MUTE' TO 'RESID'.
C
100   IF (TYPE .EQ. '_MUTE') TYPE= 'RESID'
C
C----------INTEGRATE THE FILE, PRESERVING THE NILS.
C
      CALL OPTIONI (TYPE,S,S2,NX,NY,CARD,NCARD)    
C
C----------REPLACE NILS WITH INTERPOLATED VALUES.
C
      CALL OPTIONN (S,NX,NY,CARD,NCARD)
C
C----------REMOVE RUNNING AVERAGE (OR SMOOTH THE FILE).
C
      OPTION='R'
      CALL OPTIONR (OPTION,S,S2,XINC,YINC,NX,NY,CARD,NCARD)
C
C----------CHANGE FILE NAME EXTENTION AND SAVE FILE.
C
      IF ('SISC'.EQ.IOPTN)GO TO 112
110   extn='mute'
      PRINT *, '                                   '
      PRINT *, 'SINCE YOU HAVE SELECTED OPTION=MUTE, YOUR FILE WILL',
     $           ' AUTOMATICALLY BE GIVEN A .mute EXTENTION!'
      PRINT *, '                                   '
      IF ('MUTE'.EQ.IOPTN)GO TO 115
112   PRINT *, '                                   '
      PRINT *, 'SINCE YOU HAVE SELECTED OPTION=SISC, YOUR FILE WILL',
     $           ' AUTOMATICALLY BE GIVEN A .sisc EXTENTION!'
      PRINT *, 'FILE TYPE HAS ALSO BEEN CHANGED TO RESID.'
      PRINT *, '                                   '
      extn='sisc'
115   CALL LISTPREV1 (DDDDTTTT)
      VAXFILE2=VAXFILE
      GO TO 120
666   continue
      PRINT *, 'TYPE OUTPUT STATIC FILE NAME',
     $   '  (OR HIT RETURN IF NO SAVE DESIRED)'
      VAXFILE2=' '
      READ (*,1020,END=111) VAXFILE2
1020  FORMAT (A80)
      IF (VAXFILE2.EQ.' ')GO TO 111  
120   CALL ADDEXT (VAXFILE2,extn, .true.) 
      PRINT *, 'CHECKING TO SEE IF ',IOPTN,'FILE EXISTS.'
      CALL LISTPREV2 ('STATIC  ',VAXFILE2,*666)
C----------WRITE STATIC FILE.
      call statio_write_file (vaxfile2,type,nhx,nhy,nhx2,nhy2,
     $                        x1,y1,xinc,yinc,nx,ny,s,
     $                        err,msg,card,ncard,'ISEP',0)
      print *, trim(msg)
      if (err /= STATIO_OK) go to 666
      CALL LISTPREV3 ('SAVED   ','STATIC  ',VAXFILE)
      PRINT *, '                                        '
111   RETURN
125   STOP
      END


      SUBROUTINE OPTHELP
C----------TYPE INFORMATION THE USER NEEDS TO KNOW.
      PRINT *, ' '
      PRINT *, 'When performing operations AV, ADD, SUB, and SPLICE,',
     $   ' NIL''s are treated as shown'
      PRINT *, 'in the following table (where A and B represent any',
     $   ' non-NIL values at a given'
      PRINT *, 'ground position):'
      PRINT *, ' '
      PRINT *, 'FILE1  FILE2  AV result  ADD result  SUB result  ',
     $   'SPLICE result (graded regions)'
      PRINT *, '-----  -----  ---------  ----------  ----------  ',
     $   '------------------------------'
      PRINT *, '  A      B     (A+B)/2      A+B         A-B      ',
     $   'WA*A+WB*B    (WA and WB are'
      PRINT *, '  A     NIL       A         2*A         NIL      ',
     $   '    A        variable weights)'    
      PRINT *, ' NIL     B        B         2*B         NIL      ',
     $   '    B'
      PRINT *, ' NIL    NIL      NIL        NIL         NIL      ',
     $   '   NIL'
      PRINT *, ' '
      PRINT *, 'If you do not want NIL''s to be treated as above:'
      PRINT *, ' (1) use option N to replace NIL''s by interpolated'//
     $                          ' values, or'
      PRINT *, ' (2) use option Z to replace NIL''s by zeroes.'
      CALL HIT
      RETURN
      END





      SUBROUTINE OPTAV (S1,S2,S3,NX,NY,CARD,NCARD)
C     AVERAGE S1 AND S2 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.FNIL.AND.S2(I).NE.FNIL) THEN
           S3(I)=0.5*(S1(I)+S2(I))
      ELSE IF (S1(I).EQ.FNIL) THEN
           S3(I)=S2(I)
      ELSE
           S3(I)=S1(I)
      END IF
10    CONTINUE
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE AVERAGED WITH THIS FILE'
      PRINT *, 'SECOND FILE AVERAGED WITH THIS FILE'
      RETURN
      END







      SUBROUTINE OPTADD (S1,S2,S3,NX,NY,CARD,NCARD)
C     ADD S1 AND S2 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.FNIL.AND.S2(I).NE.FNIL) THEN
           S3(I)=S1(I)+S2(I)
      ELSE IF (S1(I).EQ.FNIL.AND.S2(I).NE.FNIL) THEN
           S3(I)=2.*S2(I)
      ELSE IF (S2(I).EQ.FNIL.AND.S1(I).NE.FNIL) THEN
           S3(I)=2.*S1(I)
      ELSE
           S3(I)=FNIL
      END IF
10    CONTINUE
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE ADDED TO THIS FILE'
      PRINT *, 'SECOND FILE ADDED TO THIS FILE'
      RETURN           
      END







      SUBROUTINE OPTSUB (S1,S2,S3,NX,NY,CARD,NCARD)
C     SUBTRACT S2 FROM S1 AND PUT RESULT INTO S3.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      DIMENSION S1(*),S2(*),S3(*)
      CHARACTER*80 CARD(*)
C----------DO THE WORK.
      N=NX*NY
      DO 10 I=1,N
      IF (S1(I).NE.FNIL.AND.S2(I).NE.FNIL) THEN
           S3(I)=S1(I)-S2(I)
      ELSE
           S3(I)=FNIL
      END IF
10    CONTINUE   
C----------ADD A COMMENT CARD.
      NCARD=NCARD+1
      CARD(NCARD)='ISEP- SECOND FILE SUBTRACTED FROM THIS FILE'
      PRINT *, 'SECOND FILE SUBTRACTED FROM THIS FILE'
      RETURN
      END
          




      SUBROUTINE MATCH (X1,Y1,XINC,YINC,NX,NY,S,   T,
     $     XX1,YY1,XXINC,YYINC,NXX,NYY,SS,   TT,
     $     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,   CARD,NCARD)
C     MATCH THE RANGES OF TWO STATIC FILES BY ASKING QUESTIONS.
C     X1,Y1,XINC,YINC,NX,NY,S = FIRST STATIC FILE (T OUTPUT).
C     XX1,YY1,XXINC,YYINC,NXX,NYY,SS = SECOND STATIC FILE (TT OUTPUT).
C     XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY = MATCHED ATTRIBUTES (OUTPUT).
C----------DIMENSION STATEMENTS.
      DIMENSION S(*),T(*),SS(*),TT(*)
      CHARACTER*80 CARD(*)
C----------DRAW THE PICTURES.
      CALL PICTURE ('X',  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
      CALL PICTURE ('Y',  Y1,YINC,NY,  YY1,YYINC,NYY,
     $    YYY1,YYYINC,NYYY)
C----------NO CHANGES NEEDED.
      IF (XX1.EQ.X1.AND.YY1.EQ.Y1.AND.XXINC.EQ.XINC.AND.
     $   YYINC.EQ.YINC.AND.NXX.EQ.NX.AND.NYY.EQ.NY) THEN
           CALL MOVE (S,T,NX*NY)
           CALL MOVE (SS,TT,NXX*NYY)
C----------CHANGE BOTH FILES.
      ELSE
           PRINT *, 'If the ground position RANGES of these two',
     $        ' files do not match,'
           PRINT *, '  the files will be extended with NILs (as',
     $        ' necessary).'
           PRINT *, 'If the ground position INCREMENTS of these two',
     $        ' files do not match,'
           PRINT *, '  the second file will be resampled.'
           CALL HOW (ICHOICE)
           CALL TRUNK (ICHOICE,1,XX1,YY1,XXINC,YYINC,NXX,NYY,SS,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,TT,   CARD,NCARD)
           CALL TRUNK (ICHOICE,1,X1,Y1,XINC,YINC,NX,NY,S,
     $        XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,T,   CARD,NCARD)
      END IF
      RETURN                     
      END






      SUBROUTINE HOW (ICHOICE)
C     FIND OUT HOW TO RESAMPLE.
10    PRINT *, 'WHILE ADJUSTING GROUND POSITION RANGE AND/OR',
     $   ' INCREMENT:'
      PRINT *, 'TYPE  1  TO USE NEAREST VALUES  (NILS PRESERVED)',
     $                           '     (DEFAULT)'
      PRINT *, 'TYPE  2  TO INTERPOLATE  (NILS TREATED AS NEARLY ZERO)'
      ICHOICE=1
      CALL HITI (ICHOICE)
      IF (ICHOICE.NE.1.AND.ICHOICE.NE.2) GO TO 10
      RETURN
      END








      SUBROUTINE PICTURE (WHICH,  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
C     PRINT PICTURE OF X OR Y GROUND POSITION RANGES OF TWO FILES AND
C        THEIR COMBINATION.
C     RETURNS THE LIMITS OF THE COMBINED FILE.
C        XXX1 IS SET TO X1 +- MULTIPLE OF XINC.
C        XXXINC IS SET TO XINC.
C----------DIMENSION STATEMENTS.
      CHARACTER*(*) WHICH 
C----------DO THE WORK.
      XEND=X1+(NX-1)*XINC
      XXEND=XX1+(NXX-1)*XXINC
      XXX1=AMIN1(X1,XX1)
      XXXEND=AMAX1(XEND,XXEND)
      XXXINC=XINC
      IX=1+(XXX1-X1)/XINC
      XXX1=X1+(IX-1)*XINC
      NXXX=1+NINT((XXXEND-XXX1)/XXXINC)
      XXXEND=XXX1+(NXXX-1)*XXXINC
      IF (NX.EQ.1.AND.NXX.EQ.1.AND.NXXX.EQ.1.AND.X1.EQ.XX1) RETURN
C----------PRINT PICTURE.
      PRINT 1000, WHICH
      CALL PICLINE (XXX1,XXXEND,'FIRST','1',X1,XINC,NX)
      CALL PICLINE (XXX1,XXXEND,'SECOND','2',XX1,XXINC,NXX)
      CALL PICLINE (XXX1,XXXEND,'COMBINED','3',XXX1,XXXINC,NXXX)
C----------FINISH UP. 
      GPA=AMAX1(X1,XX1)
      GPB=AMIN1(XEND,XXEND)
      GPC=(GPA+GPB)/2.
      IF (GPA.LE.GPB) THEN
         PRINT *, 'FIRST AND SECOND FILES OVERLAP FROM ',GPA,' TO ',GPB
         PRINT *, 'CENTER OF OVERLAP IS AT GROUND POSITION ',GPC 
      ELSE
         PRINT *, 'FIRST AND SECOND FILES DO NOT OVERLAP' 
      END IF
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (/7X,'FILE  START',14X,A1,' GROUND POSITIONS',13X,
     $   'STOP  INCR  #VALS')
      END





                         
      SUBROUTINE PRETTY (X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX,S3)
C     PRINT PICTURE OF GROUND POSITION RANGES OF TWO FILES AND
C        THEIR SPLICING INFORMATION.
C----------DIMENSION STATEMENTS.
      DIMENSION S3(NXXX)
C----------DO THE WORK.
      PRINT 1000
      XXXEND=XXX1+(NXXX-1)*XXXINC
      CALL PRETTY2 (XXX1,XXXEND,'FIRST','1',X1,XINC,NX)
      CALL PRETTY2 (XXX1,XXXEND,'SECOND','2',XX1,XXINC,NXX)
      CALL PRETTY3 (XXX1,XXXEND,'SPLICED',XXX1,XXXINC,NXXX,S3)
      RETURN
C----------FORMAT STATEMENTS.
1000  FORMAT (/7X,'FILE  START',16X,'GROUND POSITIONS',13X,
     $   'STOP')
      END





                         
      SUBROUTINE PRETTY2 (XLO,XUP,WORD,CHAR,X1,XINC,NX)
C     PRINT ONE LINE OF PICTURE OF STATIC FILE GROUND POSITION RANGE.
C----------DIMENSION STATEMENTS.
      PARAMETER (N=40)
      CHARACTER*(*) WORD,CHAR
c     CHARACTER*1 H(N),CHAR2
      CHARACTER*1 H(N)
      DIMENSION S(NX)
C----------ARITHMETIC STATEMENT FUNCTION.
      INDEX(X)=1+NINT((N-1)*(X-XLO)/AMAX1(XUP-XLO,1.))
C----------DO THE WORK. 
      XEND=X1+(NX-1)*XINC
      I1=INDEX(X1)
      I2=INDEX(XEND)
      DO 10 I=1,N
10    H(I)=' '
      DO 11 I=I1,I2
11    H(I)=CHAR
      PRINT 2000, WORD,NINT(X1),H,NINT(XEND)
      RETURN
C----------MORE WORK.
      ENTRY PRETTY3 (XLO,XUP,WORD,X1,XINC,NX,S)
      XEND=X1+(NX-1)*XINC
      DO 20 I=1,N
20    H(I)=' '
      DO 30 IX=1,NX
      X=X1+(IX-1)*XINC
      I=INDEX(X)
      IF (S(IX).GE.1.) THEN
           H(I)='1'
      ELSE IF (S(IX).LE.0.) THEN
           H(I)='2'
      ELSE
           H(I)='-'
      END IF
30    CONTINUE 
      PRINT 2000, WORD,NINT(XLO),H,NINT(XUP)
      RETURN
C----------FORMAT STATEMENT.
2000  FORMAT (1X,A10,I7,1X,40A1,1X,I7)
      END

                      


                         
      SUBROUTINE PICLINE (XLO,XUP,WORD,CHAR,X1,XINC,NX)
C     PRINT ONE LINE OF PICTURE OF STATIC FILE GROUND POSITION RANGE.
C----------DIMENSION STATEMENTS.
      PARAMETER (N=40)
      CHARACTER*(*) WORD,CHAR
      CHARACTER*1 H(N)
C----------ARITHMETIC STATEMENT FUNCTION.
      INDEX(X1)=1+NINT((N-1)*(X1-XLO)/AMAX1(XUP-XLO,1.))
C----------DO THE WORK.
      XEND=X1+(NX-1)*XINC
      I1=INDEX(X1)
      I2=INDEX(XEND)
      DO 10 I=1,N
10    H(I)=' '
      DO 11 I=I1,I2
11    H(I)=CHAR
      PRINT 2000, WORD,NINT(X1),H,NINT(XEND),NINT(XINC),NX
      RETURN
C----------FORMAT STATEMENT.
2000  FORMAT (1X,A10,I7,1X,40A1,1X,I7,I6,I7)
      END

                      



      CHARACTER*5 FUNCTION SHOW5 (STAT) 
C     RETURNS FORMATTED STATIC VALUE (NEAREST INTEGER OR NIL). 
C     VALUE SHOULD BE PRINTED WITH FORMAT A5.
      use named_constants_module
      SHOW5='  NIL' 
      IF (STAT.NE.FNIL) WRITE (SHOW5,'(I5)') NINT(STAT) 
      RETURN
      END              




      CHARACTER*10 FUNCTION SHOW10 (STAT) 
C     RETURNS FORMATTED STATIC VALUE (OR NIL). 
C     VALUE SHOULD BE PRINTED WITH FORMAT A10.
      use named_constants_module
      SHOW10='       NIL' 
      IF (STAT.NE.FNIL) WRITE (SHOW10,'(F10.3)') STAT
      RETURN
      END 





      SUBROUTINE SPLICE (X1,Y1,XINC,YINC,NX,NY,S1,
     $   XX1,YY1,XXINC,YYINC,NXX,NYY,S2,
     $   XXX1,YYY1,XXXINC,YYYINC,NXXX,NYYY,S3,
     $   CARD,NCARD)
C     SPLICE TWO FILES TOGETHER.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      DIMENSION S1(NXXX,NYYY),S2(NXXX,NYYY),S3(NXXX,NYYY)
      DIMENSION XY(2)
      CHARACTER*80 CARD(*)
      CHARACTER*8 Q
C----------WARN USER ABOUT RESTRICTION.
      PRINT *, 'AT PRESENT, SPLICING CAN BE DONE ONLY FOR 2-D FILES'
      IF (NYYY.GT.1) THEN
           PRINT *, 'THEREFORE WE CANNOT SPLICE THESE FILES'
           RETURN
      ELSE
           PRINT *, 'THEREFORE WE ARE OK IN THIS CASE'
      END IF
C----------DRAW THE PICTURES.
      CALL PICTURE ('X',  X1,XINC,NX,  XX1,XXINC,NXX,
     $    XXX1,XXXINC,NXXX)
      XEND=X1+(NX-1)*XINC
      XXEND=XX1+(NXX-1)*XXINC
C----------SPLICE THE FILES TOGETHER. 
      GPA=AMAX1(X1,XX1)
      GPB=AMIN1(XEND,XXEND)
      IF (GPA.GT.GPB) GO TO 20
41    PRINT *, 'TYPE NUMBER OF SPLICE LOCATIONS DESIRED -- 1 OR 2'
      PRINT *, '      (OR TYPE 0 TO MERGE FILES TOGETHER'
      PRINT *, '  WITH AUTOMATIC GRADING IN OVERLAPPING AREAS)'
      PRINT *, '                (no default)'
      NSP=-999
      CALL HITI (NSP) 
      IF (NSP.EQ.1) GO TO 50
      IF (NSP.EQ.2) GO TO 60
      IF (NSP.EQ.0) GO TO 70
      GO TO 41
C----------THE FILES DO NOT OVERLAP.
20    PRINT *, 'SINCE THESE FILES DO NOT OVERLAP, AVERAGE THEM INSTEAD.'
      RETURN
C----------ONLY ONE SPLICE LOCATION DESIRED.
50    GPC=NINT((2.*GPA+GPB)/3.) 
      GPD=NINT((GPA+2.*GPB)/3.)
      GPE=9999999. 
      GPF=9999999. 
      PRINT *, 'TYPE GROUND POSITION OF START OF GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPC),')' 
      CALL HITX (GPC)
      PRINT *, 'TYPE GROUND POSITION OF END OF GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPD),')' 
      CALL HITX (GPD)
      IF (GPA.LE.GPC.AND.GPC.LT.GPD.AND.GPD.LE.GPB) GO TO 61
      PRINT *, 'INVALID COMBINATION -- TRY AGAIN' 
      GO TO 50
C----------TWO SPLICE LOCATIONS DESIRED.
60    GPC=NINT((7.*GPA+GPB)/8.)
      GPD=NINT((3.*GPA+GPB)/4.)
      GPE=NINT((GPA+3.*GPB)/4.)
      GPF=NINT((GPA+7.*GPB)/8.)
      PRINT *, 'TYPE GROUND POSITION OF START OF FIRST GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPC),')' 
      CALL HITX (GPC)
      PRINT *, 'TYPE GROUND POSITION OF END OF FIRST GRADED SPLICE', 
     $   '  (DEFAULT ',NINT(GPD),')' 
      CALL HITX (GPD)
      PRINT *, 'TYPE GROUND POSITION OF START OF SECOND GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPE),')' 
      CALL HITX (GPE)
      PRINT *, 'TYPE GROUND POSITION OF END OF SECOND GRADED SPLICE',
     $   '  (DEFAULT ',NINT(GPF),')' 
      CALL HITX (GPF)
      IF (GPA.LE.GPC.AND.GPC.LE.GPD.AND.GPD.LT.GPE
     $   .AND.GPE.LE.GPF.AND.GPF.LE.GPB) GO TO 61 
      PRINT *, 'INVALID COMBINATION -- TRY AGAIN' 
      GO TO 60
C----------MERGE WITH GRADING.
70    ntaper=10
      nshear=3
      print *, 'type maximum number of points to taper over (>=0)',
     $   '  (default ',ntaper,')'
      call hiti (ntaper)
      print *, 'type number of points to ignore next to a nil (>=0)',
     $   '  (default ',nshear,')'
      call hiti (nshear)
      ntaper=max(ntaper,0)
      nshear=max(nshear,0)
      do ixxx=1,nxxx
           if (s1(ixxx,1).eq.FNIL.and.s2(ixxx,1).eq.FNIL) then
                s3(ixxx,1)=FNIL
           else if (s1(ixxx,1).eq.FNIL) then
                s3(ixxx,1)=s2(ixxx,1)
           else if (s2(ixxx,1).eq.FNIL) then
                s3(ixxx,1)=s1(ixxx,1)
           else
                jmin=min(ixxx-ntaper,1)
                jmax=min(ixxx+ntaper,nxxx)
                do j=ixxx-1,jmin,-1
                     if (s1(j,1).eq.FNIL) go to 741
                end do
                j=jmin-1
741             ifirst1=j+1
                do j=ixxx+1,jmax
                     if (s1(j,1).eq.FNIL) go to 751
                end do
                j=jmax+1
751             ilast1=j-1
                do j=ixxx-1,jmin,-1
                     if (s2(j,1).eq.FNIL) go to 761
                end do
                j=jmin-1
761             ifirst2=j+1
                do j=ixxx+1,jmax
                     if (s2(j,1).eq.FNIL) go to 771
                end do
                j=jmax+1
771             ilast2=j-1
                ndist1=min(ixxx-ifirst1,ilast1-ixxx)
                ndist2=min(ixxx-ifirst2,ilast2-ixxx)
                nshearmod=min(nshear,ndist1,ndist2)
                w1=ndist1-nshearmod+1
                w2=ndist2-nshearmod+1
                s3(ixxx,1)=(w1*s1(ixxx,1)+w2*s2(ixxx,1))/(w1+w2)
           end if
      end do
      RETURN
C----------FINISH MAKING DECISIONS.
61    PRINT *, 'WHICH FILE SHOULD SUPPLY THE LOWEST GROUND POSITIONS',
     $   ' --  1 OR 2  ?' 
      KLO=0 
      CALL HITI (KLO)       
      IF (KLO.NE.1.AND.KLO.NE.2) GO TO 61 
      KUP=3-KLO 
C----------SET THE WEIGHTS INTO THE OUTPUT FILE.
      IC=NINT((GPC-XXX1)/XXXINC)+1
      ID=NINT((GPD-XXX1)/XXXINC)+1
      IE=NINT((GPE-XXX1)/XXXINC)+1
      IF=NINT((GPF-XXX1)/XXXINC)+1
      XY(KLO)=1.
      IF (KLO.EQ.2) XY(KLO)=0.
      XY(KUP)=1.-XY(KLO)
      DO 65 IXXX=1,NXXX
      IF (IXXX.LE.IC.OR.IXXX.GE.IF) GO TO 63
      IF (IXXX.GE.ID.AND.IXXX.LE.IE) GO TO 64 
      IF (IXXX.LE.ID) WEIGHT=(IXXX-IC)/AMAX0(ID-IC,1)
      IF (IXXX.GT.ID) WEIGHT=(IF-IXXX)/AMAX0(IF-IE,1)
      S3(IXXX,1)=(1.-WEIGHT)*XY(KLO)+WEIGHT*XY(KUP) 
      GO TO 65
63    S3(IXXX,1)=XY(KLO)
      GO TO 65
64    S3(IXXX,1)=XY(KUP)
65    CONTINUE
C----------VERIFY THE DECISION.
      CALL PRETTY (X1,XINC,NX,  XX1,XXINC,NXX,  XXX1,XXXINC,NXXX,S3)
      PRINT *, 'ARE THE SPLICING PARAMETERS OK?  (DEFAULT Y)'
      Q='Y'
      CALL HITQ (Q)
      IF (Q.EQ.'N') GO TO 41
C----------DO THE GRADED SPLICING.
      DO 75 IXXX=1,NXXX
      WEIGHT=S3(IXXX,1)
      IF (WEIGHT.GE.1.) THEN
           VALUE=S1(IXXX,1)
      ELSE IF (WEIGHT.LE.0.) THEN
           VALUE=S2(IXXX,1)
      ELSE IF (S1(IXXX,1).EQ.FNIL) THEN
           VALUE=S2(IXXX,1)
      ELSE IF (S2(IXXX,1).EQ.FNIL) THEN
           VALUE=S1(IXXX,1)
      ELSE           
           VALUE=(1.-WEIGHT)*S2(IXXX,1)+WEIGHT*S1(IXXX,1)
      END IF
75    S3(IXXX,1)=VALUE
      RETURN
      END






      SUBROUTINE OPTIONH (IFLAG,X1,Y1,XINC,YINC,NX,NY,S1,S2,S3)
C     PLOT STATIC FILES HORIZONTALLY.
C     IFLAG=1: 1ST PLOT IS NEW VALUES; 2ND PLOT IS OLD VALUES.
C     IFLAG=2: 1ST PLOT IS FIRST FILE; 2ND PLOT IS SECOND FILE.
C     IFLAG=3: 1ST PLOT IS FIRST 2 FILES; 2ND PLOT IS THIRD FILE.
C----------DIMENSION STATEMENTS.
      PARAMETER (LY=-10)
      DIMENSION S1(NX,NY),S2(NX,NY),S3(NX,NY)
c     CHARACTER*1 CHAR,CHAR1,CHAR2
      CHARACTER*8 Q    
C----------GET STARTED.
      IF (IFLAG.NE.3) CALL LIMITS (NX*NY,S1,S2,S2,   SLO,SUP)
      IF (IFLAG.EQ.3) CALL LIMITS (NX*NY,S1,S2,S3,   SLO,SUP)
      IF (NX.EQ.1) GO TO 222
C----------WE WILL PLOT IN X DIRECTION.
111   XLO=X1
      XUP=X1+(NX-1)*XINC
      PRINT 2000, 'X',NINT(XLO),NINT(XUP)
      CALL HITXX (XLO,XUP)
      IA=NINT((XLO-X1)/XINC)+1
      IB=NINT((XUP-X1)/XINC)+1
      IA=MIN0(NX,MAX0(IA,1))      
      IB=MIN0(NX,MAX0(IB,1))      
      XLO=X1+(IA-1)*XINC
      XUP=X1+(IB-1)*XINC
      IY=1
5     Y=Y1+(IY-1)*YINC                                      
      IF (IFLAG.EQ.1) PRINT 1001, 'YGP=',NINT(Y)
      IF (IFLAG.EQ.2) PRINT 1002, 'YGP=',NINT(Y)
      IF (IFLAG.EQ.3) PRINT 1003, 'YGP=',NINT(Y)
C----------PRINT FIRST PLOT.
      CALL PLOTL (XLO,XUP,SLO,SUP,LY) 
      DO 20 IX=IA,IB
      X=X1+(IX-1)*XINC
20    CALL PLOT1 (IFLAG,X,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
      CALL PLOTX2 ('XGP=','(F8.0)')
C----------PRINT SECOND PLOT.
      CALL PLOTL (XLO,XUP,SLO,SUP,LY) 
      DO 21 IX=IA,IB
      X=X1+(IX-1)*XINC
21    CALL PLOT2 (IFLAG,X,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
C----------GO TO NEXT Y GROUND POSITION.
      IF (NY.EQ.1) RETURN
      CALL IPICK (NY,IY,   Y1,YINC,'YGP     ')
      IF (IY.GT.0) GO TO 5
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE Y DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
C----------WE WILL PLOT IN Y DIRECTION.
222   YLO=Y1
      YUP=Y1+(NY-1)*YINC
      PRINT 2000, 'Y',NINT(YLO),NINT(YUP)
      CALL HITXX (YLO,YUP)
      IA=NINT((YLO-Y1)/YINC)+1
      IB=NINT((YUP-Y1)/YINC)+1
      IA=MIN0(NY,MAX0(IA,1))      
      IB=MIN0(NY,MAX0(IB,1))      
      YLO=Y1+(IA-1)*YINC
      YUP=Y1+(IB-1)*YINC
      IX=1
51    X=X1+(IX-1)*XINC                                      
      IF (IFLAG.EQ.1) PRINT 1001, 'XGP=',NINT(X)
      IF (IFLAG.EQ.2) PRINT 1002, 'XGP=',NINT(X)
      IF (IFLAG.EQ.3) PRINT 1003, 'XGP=',NINT(X)
C----------PRINT FIRST PLOT.
      CALL PLOTL (YLO,YUP,SLO,SUP,LY) 
      DO 30 IY=IA,IB
      Y=Y1+(IY-1)*YINC
30    CALL PLOT1 (-IFLAG,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
      CALL PLOTX2 ('YGP=','(F8.0)')
C----------PRINT SECOND PLOT.
      CALL PLOTL (YLO,YUP,SLO,SUP,LY) 
      DO 31 IY=IA,IB
      Y=Y1+(IY-1)*YINC
31    CALL PLOT2 (-IFLAG,Y,S1(IX,IY),S2(IX,IY),S3(IX,IY))
      CALL PLOTY ('(F10.0)')
C----------GO TO NEXT X GROUND POSITION.
      IF (NX.EQ.1) RETURN
      CALL IPICK (NX,IX,   X1,XINC,'XGP     ')
      IF (IX.GT.0) GO TO 51
C----------DECIDE ON NEXT ACTION.
      PRINT *, 'DO YOU WISH TO PLOT IN THE X DIRECTION?  (DEFAULT N)'
      Q='N'
      CALL HITQ (Q)
      IF (Q.EQ.'N') RETURN
      GO TO 111
C----------FORMAT STATEMENTS.
1001  FORMAT (15X,A4,I6,20X,'(OLD VALUES ON BOTTOM GRAPH)')
1002  FORMAT (15X,A4,I6,20X,'(SECOND FILE ON BOTTOM GRAPH)')
1003  FORMAT (15X,A4,I6,20X,'(COMBINED FILE ON BOTTOM GRAPH)')
2000  FORMAT (' TYPE MINIMUM AND MAXIMUM ',A1,' GP''S TO PLOT ',
     $   '(2 NUMBERS)  (DEFAULT',2I6,')')
      END





      SUBROUTINE PLOT1 (IFLAG,X,S1,S2,S3)
C     CALL PLOT1 TO ADD POINT TO FIRST PLOT.
C     CALL PLOT2 TO ADD POINT TO SECOND PLOT.
C     IFLAG=+-1: 1ST PLOT IS OLD VALUES, 2ND PLOT IS NEW VALUES.
C     IFLAG=+-2: 1ST PLOT IS FIRST FILE, 2ND PLOT IS SECOND FILE.
C     IFLAG=+-3: 1ST PLOT IS BOTH FILES, 2ND PLOT IS COMBINED FILE.
C     POSITIVE IFLAG USES CHARACTER X; NEGATIVE IFLAG USES CHARACTER Y.
C----------DIMENSION STATEMENTS.
      use named_constants_module
      CHARACTER*2 CHAR,CHAR1,CHAR2
C----------FIRST PLOT.                       
      IF (IABS(IFLAG).LE.2) THEN
           CHAR='X'
           IF (IFLAG.LT.0) CHAR='Y'
           IF (S1.NE.S2) CHAR='E'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) CHAR='1'
           IF (S1.EQ.FNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S1)
      ELSE
           CHAR1='1B'
           CHAR2='2B'
           IF (S1.EQ.FNIL) CHAR1='NN'
           IF (S2.EQ.FNIL) CHAR2='NN'
           CALL PLOTXY (CHAR1,X,S1)
           CALL PLOTXY (CHAR2,X,S2)
      END IF
      RETURN
C----------SECOND PLOT.
      ENTRY PLOT2 (IFLAG,X,S1,S2,S3)
      CHAR='X'
      IF (IFLAG.LT.0) CHAR='Y'
      IF (IABS(IFLAG).LE.2) THEN
           IF (S1.NE.S2) CHAR='O'
           IF (S1.NE.S2.AND.IABS(IFLAG).EQ.2) CHAR='2'
           IF (S2.EQ.FNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S2)
      ELSE
           IF (S3.EQ.S1) CHAR='1'
           IF (S3.EQ.S2) CHAR='2'
           IF (S3.EQ.S1.AND.S3.EQ.S2) CHAR='B'
           IF (S3.EQ.FNIL) CHAR='N'
           CALL PLOTXY (CHAR,X,S3)
      END IF
      RETURN
      END


