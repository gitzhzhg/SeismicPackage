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
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name: CTUC  ,FFOCH ,FLOCH ,CFILL ,FLNBC ,FPNTC ,FLNNC
C              : FAOCH ,RBLCH ,CBLCH ,FSSBC ,FFOCS ,CCMTDM,CCSTUC
C              : FLNSC ,CFILLH
C        Author: John B. Sinton
C  Date Written: 87/03/30
C  Last revised: 92/10/27   Sinton
C
C  Purpose: These subroutines are for character manipulation.
C  1. CTUC;  converts a single character to uppercase.
C  2. FFOCH; finds the first occurence of a user specified character.
C     A. If the length of the search string is zero nothing happens.
C  3. FLOCH; finds the last  occurence of a user specified character.
C     A. If the length of the search string is zero nothing happens.
C  4. CFILL; fills a character string with a user specified character.
C     CFILLH; same as CFILL but operates on a non-character array.
C     A. If the length of the string is zero nothing happens.
C  5. FLNBC; finds the position of the last non-blank character.
C     A. Leading blanks (to a maximum of LENC-1) are removed by   
C        shifting the string leftward, and filling in blanks on 
C        the right.
C     B. All trailing blanks now present in the string are replaced 
C        by CHAR(IC), and the value of I returned is the number of 
C        initial characters NOT replaced by CHAR(IC).
C     C. If the initial string was completely blank, it is completely 
C        replaced by CHAR(IC), and I=0 is returned.
C  5a.FLNSC; finds the position of the last non-space character.  A
C     space character is either a ' ' or a CHAR(0).  This routine 
C     operates the same as FLNBC in every other aspect.
C  6. FPNTC; finds the position of a period, '.', character.
C     A. This entry returns the position of the first decimal point 
C        within the string, or returns LEN(C)+1 if the string has no 
C        decimal points.
C  7. FLNNC; finds the position of the last non-null character.
C     A. This entry replaces all trailing NULL characters by CHAR(IC).
C     B. The returned value of I is the number of initial characters 
C        not replaced. 
C     C. If the length of C is zero then the length is assumed to be 
C        8 characters.
C  8. RBLCH; Remove all blank characters for a substring.        
C     A. Characters are removed by squeezing them out. All imbedded 
C        blanks will be removed.  
C  9. FAOCH; Finds all positions of a user specified character.
C 10. CBLCH; Compress multiple adjacent blank characters into a 
C     single blank. Leading blanks are removed.  Output string is
C     Left justified and blanked filled.
C 11. FSSBC; Finds a sub-string, delimited by a single character
C     on both ends, within a character string.
C 12. FFOCS; Finds first occurrence of a character string, see FFOCH.
C 13. CCMTDM; Converts a character month to integer month.
C 14. CCSTUC; Same as CTUC but converts an character string.
C     
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS                         
C  NONE.
C-----------------------------------------------------------------------
C  This program is re-enterable.
C-----------------------------------------------------------------------
C                                 NOTES
C  See programmers doc.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date      Author     Description
C     ----      ------     -----------
C  5. 92/10/27: JB Sinton, Added the CFILLH routine.
C  4. 90/01/17: JB Sinton, Added the FLNSC routine.
C  3. 88/09/26: JB Sinton, NWIH conversion, added FSSBC.
C  2. 88/09/13: JB Sinton, Added FAOCH, RBLCH, and CBLCH routines.
C  1. 87/03/30; JB Sinton, Documentation added.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C             
C        RESULT(1:1) = CTUC ( CHAR(1:1) )  !This is a function.
C        CALL FFOCH (F ,N ,T)              !Find first occurence.
C        CALL FLOCH (F ,N ,T)              !Find last  occurence.
C        CALL CFILL (F ,T)                 !Character fill.
C        CALL CFILLH(A ,N ,T)              !Character fill.
C        CALL FLNBC (C ,I ,IC)             !Find last non-blank.
C        CALL FLNSC (C ,I ,IC)             !Find last non-space.
C        CALL FLNNC (C ,I ,IC)             !Find last non-null.
C        CALL FPNTC (C ,I)                 !Find first period.
C        CALL FAOCH (F,I1,I2,N,T)          !Find all occurences.
C        CALL RBLCH (F,FR,N)               !Remove blanks.
C        CALL CBLCH (F,N)                  !Compress blanks.
C        CALL FSSBC (F,FSS,NC,MC,C)        !Find sub-string.
C        CALL FFOCS (F,N,T)                !Find first sub-string.
C        CALL CCMTDM(F,N)                  !Convert month.
C        CALL CCSTUC(F,N)                  !Convert to uppercase.
C
C Arguments for CTUC
C CHAR     - A single character substring. 
C Arguments for FFOCH, FLOCH, and CFILL.
C F        - The character substring to search.
C N        - The position of the character relative to the start of the
C            character string F.
C T        - A single character to search for (or fill for CFILL).
C Arguments for CFILLH.
C A        - A non-character array, e.g. type integer, real, etc.
C N        - Number of words in array A.  CFILLH will fill 8*N characters
C            in array A.
C T        - A single character to fill array A with.
C Arguments for FLNBC, FLNSC, FPNTC, and FLNNC.
C C        - The character substring to search.
C I        - For FLNBC; number of non-blanks, =0, if all blanks.
C            For FLNSC; number of non-blanks, =0, if all blanks.
C            For FPNTC; position of period,   =LEN(C)+1, if no periods.
C            For FLNNC; number of non-nulls,  =0, if all nulls.  
C IC       - The number of the character to replace null or blank 
C            characters.  That is CHAR(IC), is the replacement charac-
C            ter.
C Arguments for FAOCH.
C F        - The character string to search.
C I1       - Position after  character T, returned integer array.
C I2       - Position before character T, returned integer array.
C N        - # of time T was found, returned integer.
C T        - single character to search for, character*1.
C Arguments for RBLCH.
C F        - Character string to remove blanks from, not altered.
C FR       - Character string returned without blanks.
C N        - # of characters in FR, returned integer.
C Arguments for CBLCH.
C F        - Character string to compress blanks from.
C            Note that F is altered in this routine.
C N        - Character position of the last non-blank character in F on
C            return to the calling routine.  If the input string is all
C            blanks then N=0 on return.  Integer argument.
C Arguments for FSSBC.
C F        - is the character string to search.                      
C FSS      - is the sub string if one is found.                      
C NC       - is the number of characters in FSS.  This will be zero  
C            if no sub string is found or F has length zero.         
C MC       - is the last character tested for the character C.       
C C        - is the character that delimites the sub string.         
C Arguments for FFOCS.
C F        - is the character string to search.
C N        - is the character position of the sub-string.
C T        - is the character sub-string to search for.
C Arguments for CCMTDM.
C F        - is a character variable containing the month spelled 
C            out in uppercase letters.  F need only contain enough
C            letters to uniquely define the month.
C N        - is the integer month, 1=January, 12=December.
C Arguments for CCSTUC.
C F        - is the character string to convert to uppercase.
C N        - is the length of the character string.  If N=0, then
C            then F is passed as type character.
C-----------------------------------------------------------------------
C                                 NOTES
C  1. See documentation within code for FLNBC, FPNTC, and FLNNC about
C     how the character string is modified.
C  2. To use CTUC you must add the following statements to your code.
C     CHARACTER*1 CTUC
C     EXTERNAL    CTUC
C  3. The routines FFOCH, FLOCH, CFILLH, AND CFILL are entry points to 
C     the samesubroutine.
C  4. The routines FLNBC, FPNTC, AND FLNNC are entry points to the same
C     subroutine. 
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  FAOCH ,RBLCH ,CBLCH ,FFOCH ,FLNBC
C Functions    : 
C  CTUC
C Entrys       :
C  In FFOCH: FLOCH ,CFILL ,CFILLH
C  In FLNBC: FPNTC ,FLNNC
C Common blocks: none
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  FFOCH    
C Functions:   none
C  Note- All routines called are CPS primitives.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C\END DOC
C
      CHARACTER*(*) FUNCTION CTUC(C)
      CHARACTER*1 C
      IC = ICHAR(C)
      CTUC = C
      IF ( (IC.GT.96) .AND. (IC.LT.123) ) CTUC = CHAR(IC-32)
      RETURN
      END
C&&&
      SUBROUTINE FAOCH (F,I1,I2,N,T)
      DIMENSION I1(*),I2(*)
      CHARACTER*(*) F,T
      LF = LEN(F)  
      N = 0
      I = 1
 100  CONTINUE
       CALL FFOCH (F(I:),ICP,T)
       IF (ICP.EQ.0.OR.ICP.EQ.LF) GOTO 110
       N     = N+1
       I1(N) = I
       I2(N) = (I-1)+(ICP-1)
       I     = (I-1)+(ICP+1)
      GOTO 100
 110  CONTINUE
      N = N+1          
      I1(N) = I
      I2(N) = LF
      RETURN
      END
C&&&
      SUBROUTINE RBLCH (F,FR,N)
      CHARACTER*(*) F,FR
      N = 0
      LF = LEN(F)
      DO 200 J=1,LF
       IF (F(J:J).NE.' ') THEN
        N = N+1
        IF (F(J:J).EQ.'|') THEN
         FR(N:N) = ' '
        ELSE
         FR(N:N) = F(J:J)
        ENDIF
       ENDIF
 200  CONTINUE
      RETURN            
      END
C&&&
        SUBROUTINE CBLCH (F,N)
        CHARACTER*(*) F
        N = LEN(F)
        I = 0 
 6      I = I+1
         IF (I.GT.N-1) GOTO 10
         IF (F(I:I).EQ.' ' .AND. F(I+1:I+1).EQ.' ') THEN
          DO 7 J=I+1,N-1
 7        F(J:J) = F(J+1:J+1)
          F(N:N) = ' '
          N = N-1
          I = I-1
          IF (N.EQ.0) GOTO 20
         ENDIF
         GOTO 6
 10     CONTINUE
        IF (F(N:N).EQ.' ') N = N-1
        RETURN
 20     RETURN
        END
C&&&
        SUBROUTINE FSSBC (F,FSS,NC,MC,C)
        CHARACTER*(*) F ,FSS ,C
        LF = LEN(F) 
        J = 0 
        DO 20 I=1,LF
         IF (F(I:I).NE.C) THEN       
          DO 10 J=I,LF
           IF (F(J:J).EQ.C) GOTO 30
 10       CONTINUE
          J = LF
          GOTO 30
         ENDIF
 20     CONTINUE
 30     CONTINUE
        NC = 0
        MC = J
        DO 40 L=I,J-1
         NC = NC+1
         FSS(NC:NC) = F(L:L)
 40     CONTINUE
        RETURN
        END
C&&&
      SUBROUTINE FFOCH (F,N,T)
      CHARACTER*(*) F,T
      CHARACTER*8 A(N)
      CHARACTER*9 MONTH(12)
      DATA MONTH/ 'JANUARY' ,'FEBRUARY' ,'MARCH' ,'APRIL' ,'MAY',
     +            'JUNE' ,'JULY' ,'AUGUST' ,'SEPTEMBER' ,'OCTOBER',
     +            'NOVEMBER' ,'DECEMBER' /
      LF = LEN(F)
      N = 0
      DO 100 J=1,LF
       IF (F(J:J).EQ.T) THEN
        N = J
        GOTO 110
       ENDIF
 100  CONTINUE
 110  CONTINUE
      RETURN
                                                                   
      ENTRY FLOCH (F,N,T)
      LF = LEN(F)
      N = 0
      if (lf.gt.0) then
       DO 200 J=LF,1,-1
        IF (F(J:J).EQ.T) THEN
         N = J
         GOTO 210
        ENDIF
 200   CONTINUE
 210   CONTINUE
      endif
      RETURN

      ENTRY CFILL(F,T)
      LF = LEN(F)
      DO 300 I=1,LF
       F(I:I) = T
 300  CONTINUE
      RETURN

      ENTRY CFILLH (A,N,T)
      DO 350 I=1,N
 350  A(I) = T
      RETURN

      ENTRY FFOCS (F,N,T)
      LF = LEN(F)
      LT = LEN(T)
      N = 0
      DO 400 I=1,LF-LT
       IF (F(I:I+LT-1).EQ.T(:LT)) THEN
        N = I
        GOTO 410
       ENDIF
 400  CONTINUE
 410  CONTINUE
      RETURN

      ENTRY CCMTDM (F,N)
      LF = LEN(F)
      DO 500 I=1,12
       IF (F(:LF).EQ.MONTH(I)(:LF)) GOTO 510
 500  CONTINUE                        
 510  N = I
      RETURN

      ENTRY CCSTUC (F,N)
      IF (N.EQ.0) THEN !Actual argument for F is a character string.
       LF = LEN(F)
      ELSE             !Actual argument for F isn't a character string.
       LF = N
      ENDIF
      DO 550 I=1,LF
       IC = ICHAR(F(I:I))
       IF (IC.GE.97 .AND. IC.LE.122) IC = IC-32
       F(I:I) = CHAR(IC) 
 550  CONTINUE
      RETURN
      END
C&&&
      SUBROUTINE FLNBC (C,I,IC)
C
C
      CHARACTER*(*) C
      CHARACTER*1 ZERO
C**************************************************
C *** The length is at least 8 characters long. ***
C *** If a hollerath variable is passed to FLNBC***
C *** on the Cray, its length, LEN(C), is zero. ***
      LENC = LEN(C)
      IF (LEN(C).LE.0) LENC = 8
C**************************************************
      IF (C(1:1).EQ.' ') THEN
        DO 10 I=2,LENC
          IF (C(I:I).NE.' ') GO TO 20
   10   CONTINUE
   20   I = I-1
        DO 25 J=1,LENC-I
   25   C(J:J) = C(J+I:J+I)
        DO 27 J=LENC-I+1,LENC
   27   C(J:J) = ' '
      ENDIF
      DO 30 I=LENC,1,-1
        IF (C(I:I).NE.' ') GO TO 40
   30 CONTINUE
   40 DO 45 J=I+1,LENC
        C(J:J) = CHAR(IC)      
   45 CONTINUE
      RETURN
C
C
      ENTRY FPNTC (C,I)
      DO 50 I=1,LEN(C)
        IF (C(I:I).EQ.'.') GO TO 60
   50 CONTINUE
   60 RETURN
C
C
      ENTRY FLNNC (C,I,IC)
C**************************************************
C *** The length is at least 8 characters long. ***
C *** If a hollerath variable is passed to FLNBC***
C *** on the Cray, its length, LEN(C), is zero. ***
      LENC = LEN(C)
      IF (LEN(C).LE.0) LENC = 8
C**************************************************
      ZERO = CHAR(0)
      DO 70 I=LENC,1,-1
        IF (C(I:I).NE.ZERO) GO TO 80
   70 CONTINUE
   80 DO 85 J=I+1,LENC
        C(J:J) = CHAR(IC)
   85 CONTINUE
      RETURN
C
C
      ENTRY FLNSC (C,I,IC)
C**************************************************
C *** The length is at least 8 characters long. ***
C *** If a hollerath variable is passed to FLNBC***
C *** on the Cray, its length, LEN(C), is zero. ***
      LENC = LEN(C)
      IF (LEN(C).LE.0) LENC = 8
C**************************************************
      ZERO = CHAR(0)
      IF (C(1:1).EQ.' ') THEN
        DO 110 I=2,LENC
          IF (C(I:I).NE.' ') GO TO 120
  110   CONTINUE
  120   I = I-1
        DO 125 J=1,LENC-I
  125   C(J:J) = C(J+I:J+I)
        DO 127 J=LENC-I+1,LENC
  127   C(J:J) = ' '
      ENDIF
      DO 130 I=LENC,1,-1
        IF (C(I:I).NE.' '.AND.C(I:I).NE.ZERO) GO TO 140
  130 CONTINUE
  140 DO 145 J=I+1,LENC
        C(J:J) = CHAR(IC)      
  145 CONTINUE
      RETURN
      END
CCC
C**************************************************
C Same as FLNSC, but does not strip leading blanks
C and assumes that C is a true character.
C**************************************************
      SUBROUTINE FLNSC2(C,I,IC)
      CHARACTER*(*) C
      CHARACTER*1 ZERO,BLANK
      INTEGER       I, IC
      LENC = LEN(C)
      IF(LENC.LE.0) THEN
        I=0
        RETURN
      END IF
      ZERO = CHAR(0)
      BLANK= CHAR(32)
      DO 130 I=LENC,1,-1
        IF (C(I:I).NE.' '.AND.C(I:I).NE.ZERO) GO TO 140
  130 CONTINUE
  140 DO 145 J=I+1,LENC
        C(J:J) = CHAR(IC)
  145 CONTINUE
      RETURN
      END
 

