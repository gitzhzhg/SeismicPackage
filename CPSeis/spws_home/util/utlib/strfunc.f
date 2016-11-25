CCC
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
        SUBROUTINE STRUPC(OUTPUT,INPUT)      
        CHARACTER*(*) OUTPUT,INPUT
        DO 10 J = 1,LEN(INPUT)
         IC = ICHAR(INPUT(J:J))
         IF ( (IC.GT.96) .AND. (IC.LT.123) ) THEN
          OUTPUT(J:J)= CHAR(IC-32)            
         ELSE
          OUTPUT(J:J) = INPUT(J:J)   
         ENDIF
10      CONTINUE
        RETURN
        END
CCC
        SUBROUTINE STRTRI(DST,SRC,LAST)
        CHARACTER*(*)     DST,SRC
        INTEGER           NULL,BLANK,CURCHAR,LAST
C       SCAN FOR TRAILING BLANKS,NULLS.
        NULL    = 00
        BLANK   = 32
        DO 10 I = LEN(SRC),1,-1
         CURCHAR= ICHAR(SRC(I:I))
         IF (CURCHAR.NE.NULL .AND. CURCHAR.NE.BLANK)  GOTO 20
 10     CONTINUE
        I = 0
 20     LAST = I          ! LAST NON-BLANK/NULL CHARACTER POSITION.
        DST = SRC
        RETURN
        END
CCC
        SUBROUTINE STRRMB(INPUT,OUTPUT,LENGTH)
C       STR$REMOVE BLANKS I        O     O
        CHARACTER*(*) INPUT,OUTPUT
        LENGTH = 0
        M = LEN(OUTPUT)      
        DO 20 I = 1,LEN(INPUT)               
         IF(LENGTH.EQ.M) THEN
          PRINT*,' STRRMB: WARNING- TRUNCATED STRING TO ',M,
     &                 ' CHARACTERS.'
C R Day 94/06/13 CALL ABORT(' STRRMB: TRUNCATED OUTPUT STRING')
         ENDIF
         IF(INPUT(I:I).NE.CHAR(32))THEN
           LENGTH = LENGTH + 1
           OUTPUT(LENGTH:LENGTH) = INPUT(I:I)
          ENDIF
20      CONTINUE              
C       PAD OUTPUT WITH BLANKS.
        DO 30 I = LENGTH+1,M
30      OUTPUT(I:I) = CHAR(32)
        LENGTH = MAX(1,LENGTH)
        RETURN
        END
CCC
        SUBROUTINE TRISTR(A,B,N)
C       TRIM_STRING : REMOVE BLANKS, CONVERT TO UPPER CASE.
        CHARACTER*(*) A,B 
        CALL STRRMB(A,B,N)
        CALL STRUPC(B,B)
        RETURN
        END
CCC
        INTEGER FUNCTION STRFNIS(INPUT,SET)
C                                   I  I   > STRFNIS= OUTPUT INTEGER.
C        STRING FUNCTION FIND FIRST CHARACTER IN INPUT NOT IN SET.
        CHARACTER*(*) INPUT,SET
        STRFNIS = 0
        DO 10 I = 1,LEN(INPUT)
         IC = ICHAR(INPUT(I:I))
         DO 20 J = 1,LEN(SET)
          IS = ICHAR(SET(J:J))
          IF(IS.EQ.IC)GO TO 30
20       CONTINUE
         STRFNIS = I                                              
         RETURN
30       CONTINUE
10      CONTINUE
        RETURN
        END
CCC
        SUBROUTINE FMTFFO(CARD)
        CHARACTER*(*) CARD
        CHARACTER*80 T,U
        CHARACTER*1 FC                !FIRST CHARACTER NON BLANK
        CHARACTER*7 TFC                !TYPE OF FIRST CHARACTER
         CHARACTER*5 LABEL
        CHARACTER*26 LTRS, NUMS*10,BLANKS*20       
        CHARACTER*1   BK,TB,US,NL
        INTEGER    N,LFNB,RLFNN                !LOC FIRST NON BLANK
        INTEGER   STRFNIS
        DATA LTRS/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/,NUMS/'0123456789'/,
     &                BK/' '/,US/'_'/,
     &                BLANKS/'                    '/
        TB = CHAR(09)
        NL = CHAR(00)
        T = BLANKS//BLANKS//BLANKS//BLANKS
        U = BLANKS//BLANKS//BLANKS//BLANKS
        CALL STRUPC(T,CARD)
        CARD = BLANKS//BLANKS//BLANKS//BLANKS
C        REPLACE ALL '_' WITH ' ', ALL TABS WITH ' '
        DO 10 I = 1,80
         IF(T(I:I).EQ.US  .OR. T(I:I).EQ.NL .OR. T(I:I).EQ.TB)THEN
          T(I:I) = BK 
         ENDIF
10      CONTINUE
C       T = T//BK
C        WEED OUT COMMMENTS. A COMMENT HAS 'C ' IN FIRST 2 COLUMNS OR
C        IT HAS A ! IN IT.                        
        IF(T(1:2).EQ.'C ')THEN
         !POSSIBLY A COMMENT IN- TREAT AS SUCH
         WRITE(U,'(A80)')T(1:80)
        ELSE
         LFNB = STRFNIS(T,BK)        
         FC = T(LFNB:LFNB)
C         FIND OUT WHAT KIND OF CHARACTER THE FIRST NON BLANK IS..
         I = INDEX(NUMS,FC)
         J = INDEX(LTRS,FC)
C         PRINT*,' I,J = ',I,J
         IF(LFNB.LE.0)THEN
          RETURN ! ERROR CONDITION
         ELSEIF(I.GT.0)THEN
          TFC = 'NUMBER'
         ELSEIF(J.GT.0)THEN
          TFC = 'ALPHA'
         ELSE
          TFC = 'SPECIAL'
         ENDIF
C         WRITE(6,'(1X,A7)')TFC
C         SEE IF WE HAVE A CONTINUATION LINE, ALREADY WITH 5 SPACES.
         IF(LFNB.EQ.6)THEN                                                
          !POSSIBLE CONTINUATION LINE
C          PRINT*,' CONTINUATION LINE.'
          WRITE(U,'(A)')'     &'//T(7:80)
         ELSE
C          LOOK FOR LABELED STATEMENTS                             
          IF(TFC.EQ.'NUMBER')THEN                
           !POSSIBLE LABEL FOR A STATEMENT.   FIND REL LOC FIRST NON NUMERICAL
           RLFNN = STRFNIS(T(LFNB:),NUMS)
           IF(RLFNN.GT.5)THEN
            !LABEL IS TOO LONG.  THIS I CANNOT HANDLE
            PRINT*,' =>FMTFFO: LABEL IS TOO LONG'
C R Day 94/06/13 CALL ABORT('FMTFFO')
           ENDIF
           LABEL = T(LFNB:LFNB+RLFNN-1)
           CALL TRISTR(LABEL,LABEL,I)
           WRITE(U,'(1X,A,1X)')LABEL(:I) ! PUT LABEL IN RIGHT PLACE.
           I = STRFNIS(T,BK//NUMS)
           WRITE(U,'(A6,A66,8X)')U(1:6),T(I:I+66)
          ELSEIF(TFC.EQ.'ALPHA')THEN
           !POSSIBLE FORTRAN STATEMENT
           WRITE(U,'(6X,A66,8X)')T(LFNB:LFNB+66)
          ELSEIF(TFC.EQ.'SPECIAL')THEN         
           IF(FC.EQ.'!')THEN ! WE HAVE A COMMENT. 
            T(LFNB:LFNB) = 'C' ! REPLACE ! WITH C.
            WRITE(U,'(A80)')'C'//T(2:80)
           ENDIF
          ELSEIF(TFC.EQ.'OTHER')THEN
          ENDIF
          LCCHR = STRFNIS(T(73:),BK)
          IF(LCCHR.GT.0)THEN              
           WRITE(6,'('' FFF> CONTINUE LINE NEEDED: '',I2,1X,A1)')LCCHR,
     &                 T(LCCHR+72:LCCHR+72)
           ! WE NEED A CONTINUATION LINE
          ENDIF          
         ENDIF
        ENDIF
        WRITE(CARD,'(A72,8X)')U(1:72)
        RETURN
        END
C       SUBROUTINE STRCCT(STR,SUB,LSUB,NBR)
CC                         I   I    I   O
CC      STR$COUNT_NUMBER OF OCCURRANCES OF SUB_STRING
CC      STR = INPUT STRING
CC      SUB = SUB STRING (INPUT)
CC      LSUB= LENGTH OF SUB STRING
CC      NBR = OUTPUT NUMBER OF OCCURANCES OF SUB IN STR.
C       CHARACTER*(*) STR,SUB
C       INTEGER   LSUB,NBR
C       NBR = 0            
C       IF(LSUB.LE.0)RETURN
C       DO 20 I = 1,LEN(STR)
C        IF(STR(I:I+LSUB-1).EQ.SUB(:LSUB))NBR = NBR + 1
C20     CONTINUE
C       RETURN
C       END                          
CCC
        SUBROUTINE NUL2BNK(INPUT,OUTPUT)
C                            I     O     < CONVERT NULLS TO BLANKS.
C                                          PAD REST OF OUTPUT WITH BLANKS.
        CHARACTER*(*) INPUT,OUTPUT
        M = LEN(INPUT)
        N = LEN(OUTPUT)
C R.Day 94/06/13 IF(N.LT.M)CALL ABORT(' NUL2BNK: LEN(OUTPUT)<LEN(INPUT)')
        DO 30 I = 1,M
         IF(INPUT(I:I).EQ.CHAR(0)) THEN
          OUTPUT(I:I) = CHAR(32)
         ELSE
          OUTPUT(I:I) = INPUT(I:I)
         ENDIF
30      CONTINUE     
        DO 40 I = M+1,N
40       OUTPUT(I:I)  = CHAR(32)
        RETURN
        END
