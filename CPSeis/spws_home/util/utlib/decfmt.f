      SUBROUTINE DECFMT(F,N,T,M,LS,*)
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
C  Process name: DECFMT ,DECFMTA ,DECFMTB ,DECSETU ,DECVAL 
C        Author: John B. Sinton
C  Last revised: 89/04/12     
C                              
C  Purpose: These subroutines are for DCODE and NCODE.
C  1. DECFMT; Cracks the format statement in DCODE and NCODE (see their
C     documentation).
C  1b.DECFMTA & B; Cracks table format fields, [...].
C  2. DECVAL; Cracks a value for a parameter in DCODE.
C                                                   
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS                           
C  NONE                     
C-----------------------------------------------------------------------
C  This program is re-enterable, but is not stand alone.  It is to be
C  used with NCODE and DCODE.
C-----------------------------------------------------------------------
C                                 NOTES
C  See programmers documentation below.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date      Author     Description
C  6. 89/04/12: H.Ball     Documentation change only.
C  5. 88/09/26: JB Sinton, NWIH conversion.
C  4. 88/09/21: JB Sinton, Added "L" type to DECVAL.
C  3. 88/09/13: JB Sinton, Moved routines RBLCH and FAOCH to VAX_
C               CHARACTER_ROUTINES.
C  2. 88/07/21: JB Sinton, Tables format fields add (DECFMTA&B) and
C               unified CRAY and VAX versions.
C  1. 87/03/30; JB Sinton, Documentation added.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL DECFMT(F,N,T,M,LS,*)     !Cracks format statement.
C        CALL DECFMTA(F,N,NF,*)        !Sets up table cracking.
C        CALL DECFMTB(F,FR,NC)         !Add size to table sub-field.
C        CALL DECVAL(F,T,LOC,*)        !Convert to internal form.
C        CALL DECSETU(LENWORD,CPUTYPE) !Setup of these routines.
C
C Arguments for DECFMT.
C F        - DCODE(NCODE) format field, character.
C N        - # of parameters using this field, returned integer.
C T        - Field type, returned character*1.
C M        - # of values in this field (for each parameter), 
C            returned integer.
C LS       - Length of each value, will be multiple of LEN4R8,
C            returned integer.
C *        - Alternate error return.                             
C Arguments for DECFMTA.
C F        - DCODE(NCODE) table format field, character.
C            This format field should be enclosed in "[...]n"
C            Where n is the number of elements in sub-field in the
C            table.
C N        - Returned value of "n".
C NF       - Number of sub-fields in the table, in other words this is
C            the number of arrays in the table.
C *        - Alternate error return.
C =========> DECFMTA must be called before DECFMTB each time a new
C            table field is to be cracked.
C Arguments for DECFMTB.
C F        - DCODE(NCODE) format field, character.  This field may 
C            include a "[" as the first character or "]".
C FR       - Returned DCODE(NCDOE) format field, character.  DECFMTB
C            adds the length of the array to the format field so that
C            it may be cracked by DECFMT. 
C NC       - Number of characters in FR.
C Arguments for DECVAL.
C F        - The character string containing value of parameter.
C T        - Parameter type, see DCODE(NCODE).
C LOC      - Location where cracked value will be stored.
C *        - Alternate error return.
C Arguments for DECSETU.
C LENWORD  - Number of bytes/characters per word.              
C CPUTYPE  - If =.TRUE., then using CRAY cpu.          
C            If =.FALSE.,then using VAX  cpu.
C
C_______________________________________________________________________
C                                 NOTES
C  1. See documentation for DCODE(NCODE) for use of DECFMT, and DECVAL.
C  2. To crack a table sub-field call DECFMTA, DECFMTB with a sub-field, 
C     and finally call DECFMT to complete the cracking.
C  3, Use the DECSETU entry to set the CPU dependent values of number of
C     byte/word and CPU type.  DECSETU must be called before any other
C     call is made to the routines in this subroutine.
C  4. These routines call CTUC ,FFOCH ,FLOCH ,CFILL ,FLNBC ,FPNTC ,FLNNC
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  DECFMT
C Functions    : none
C Entrys       :
C  DECFMTA DECFMTB DECVAL  DECSETU
C Common blocks: none
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  FFOCH    
C Functions:
C  CTUC
C  Note- All routines called are CPS primitives.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C\END DOC
C
      CHARACTER*(*) F,T,FR
      CHARACTER FORMT*9,FL*4,FW*8,NSAVE*8,TEMP*1
      CHARACTER*1 CTUC
      LOGICAL CRAY ,CPUTYPE
      EXTERNAL CTUC
      EQUIVALENCE (IVAL,RVAL)
      SAVE NSAVE,KSAVE ,CRAY,LEN4R8
      DATA LEN4R8/8/ ,CRAY/.TRUE./
      T = ' '
      LF = LEN(F)
      if (lf.eq.0) goto 905
      DO 10 I=1,LF
       IF (ICHAR(F(I:I)).GE.65) THEN
        T = F(I:I)
        GOTO 20
       ENDIF   
 10   CONTINUE
      GOTO 900
 20   CONTINUE
      LS = LEN4R8
      IF      (I.EQ.LF) THEN
       M = 1
      ELSE IF (ICHAR(F(I+1:I+1)).GE.46 .AND. 
     +           ICHAR(F(I+1:I+1)).LE.57       ) THEN
       CALL FFOCH (F(I+1:),ICP,'.')
       IF (ICP.EQ.0) THEN
        WRITE (FL,'(I2)') LF-(I+1)+1
        FORMT = '(I' // FL // ')  '
        READ (F(I+1:LF),FORMT) M
       ELSE
        IF (ICP.EQ.1) THEN
         M = 1
        ELSE
         WRITE (FL,'(I2)') ICP-1
         FORMT = '(I' // FL // ')  '
         READ (F(I+1:I+ICP-1),FORMT) M
        ENDIF
        WRITE (FL,'(I2)') LF-(I+ICP+1)+1
        FORMT = '(I' // FL // ')  '
        READ (F(I+ICP+1:),FORMT) LS
        IF (MOD(LS,LEN4R8).NE.0) GOTO 903
       ENDIF
      ELSE  
       GOTO 904
      ENDIF
      IF (I.EQ.1) THEN
       N = 1
      ELSE
       WRITE (FL,'(I2)') I-1
       FORMT = '(I' // FL // ')  '
       READ (F(1:I-1),FORMT) N
      ENDIF
      RETURN

      ENTRY DECFMTA (F,N,NF,*)
      LF = LEN(F)
      CALL FFOCH (F(:LF),NLB,']')
      IF (NLB.EQ.0) GOTO 907
      NF = 1
      CALL FFOCH (F(:NLB),NCO,',')
      IF (NCO.NE.0) NF = 2
      NLB = NLB+1     !Position of character after "]".
      K = 0
      DO 115 IC=NLB,LF
       IF (ICHAR(F(IC:IC)).GE.48 .AND. ICHAR(F(IC:IC)).LE.57) THEN
        K = K+1
        FW(K:K) = F(IC:IC)
       ELSE IF (IC.EQ.1) THEN
        K = 1
        FW(:1) = '1'
        GOTO 120
       ELSE                      
        GOTO 120
       ENDIF
 115  CONTINUE
 120  CONTINUE
      WRITE (FL,'(I4)') K
      FORMT = '(I' // FL // ')'
      READ (FW,FORMT) N
      NSAVE = FW(:K)
      KSAVE = K
      RETURN

      ENTRY DECFMTB (F,FR,NC)
      LF = LEN(F)
      IF (F(:1).EQ.'[') THEN
       NS = 2
      ELSE
       NS = 1
      ENDIF
      CALL FFOCH (F(:LF),NLB,']')
      IF (NLB.NE.0) THEN
       NE = NLB-1   
      ELSE
       NE = LF
      ENDIF
      IF (FL(NE:NE).EQ.'/') NE = NE-1
      DO 125 I=NS,NE
       IF (ICHAR(F(I:I)).GE.65) GOTO 130
 125  CONTINUE
 130  NT = MIN(I,NE)
      IF (LF.EQ.1 .OR. NT.EQ.NE) THEN
       FR = F(NS:NE) // NSAVE(:KSAVE)
      ELSE
       FR = F(NS:NT) // NSAVE(:KSAVE) // F(NT+1:NE)
      ENDIF
      NC = (NE-NS+1) + KSAVE
      RETURN

      ENTRY DECVAL (F,T,LOC,*)
      NJJ = LEN(F)   !was NC R DAY 3/25
      IF (NJJ.LE.0) GOTO 906
      WRITE (FL,'(I2)') NJJ
      IF (CRAY) THEN
       TEMP = CHAR(0)
      ELSE
       TEMP = ' '
      ENDIF
      IF      (CTUC(T).EQ.'F') THEN
       FORMT = '(' // T // FL //  '.0)'
       READ (F,FORMT,ERR=901) RVAL
      ELSE IF (CTUC(T).EQ.'I') THEN
       FORMT = '(' // T // FL //  ')  '
       READ (F,FORMT,ERR=901) IVAL
      ELSE IF (CTUC(T).EQ.'H') THEN
       IF (LEN4R8.EQ.8) THEN
        FORMT = '(A8)   '
       ELSE
        FORMT = '(A4)   '
       ENDIF
       FW(:NJJ) = F(:NJJ)
       DO 150 I=NJJ+1,LEN4R8
 150   FW(I:I) = TEMP
       READ (FW,FORMT,ERR=901) IVAL
      ELSE IF (CTUC(T).EQ.'A') THEN
       IF (LEN4R8.EQ.8) THEN
        FORMT = '(A8)   '
       ELSE
        FORMT = '(A4)   '
       ENDIF
       FW(:NJJ) = F(:NJJ)
       DO 155 I=NJJ+1,LEN4R8
 155   FW(I:I) = TEMP
       READ (FW,FORMT,ERR=901) IVAL
      ELSE IF (CTUC(T).EQ.'L') THEN
       IF (LEN4R8.EQ.8) THEN
        FORMT = '(A8)   '
       ELSE
        FORMT = '(A4)   '
       ENDIF
       FW(:NJJ) = F(:NJJ)
       DO 160 I=NJJ+1,LEN4R8
 160   FW(I:I) = ' '
       READ (FW,FORMT,ERR=901) IVAL
      ELSE IF (CTUC(T).EQ.'R') THEN
       IF (CRAY) THEN
        FORMT = '(' // T // FL //  ')  '
        READ (F,FORMT,ERR=901) IVAL
       ELSE
        IF (LEN4R8.EQ.8) THEN
         FORMT = '(A8)   '
        ELSE
         FORMT = '(A4)   '
        ENDIF
       ENDIF
       READ (F,FORMT,ERR=901) IVAL
      ENDIF
      LOC = IVAL
      RETURN

      ENTRY DECSETU (LENWORD,CPUTYPE)
      IF (LENWORD.GT.0) LEN4R8 = LENWORD
      CRAY = CPUTYPE
      RETURN

 900  CONTINUE
      PRINT*,' =>DECFMT: format field err (no type), F=',F
      RETURN 1
 901  CONTINUE
      PRINT*,' =>DECVAL: data conversion err, T=',T,' F=',F
      RETURN 1
 902  CONTINUE
      PRINT*,' =>DECVAL: format field err (array length), F=',F
      RETURN 1
 903  CONTINUE
      PRINT*,' =>DECFMT: format field err (character length must',
     +' be multiple of ',LEN4R8,'), F=',F
      RETURN 1
 904  CONTINUE
      PRINT*,' =>DECFMT: format field err (invalid data type) F=',F
      RETURN 1
 905  PRINT*,' =>DECFMT: format has zero length!'
      RETURN 1
 906  PRINT*,' =>DECVAL: data to decode is length zero!'
      RETURN 1
 907  PRINT*,'=>DECFMTA&B: format has "[" but no "]"!'
      RETURN 1
      END
C&&&
