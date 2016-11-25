!<CPS_v1 type="PROGRAM"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!*******************************************************************************
!***********                    COPYRIGHT NOTICE                     ***********
!     Copyright CERN, Geneva 1991 - Copyright and any other
!     appropriate legal protection of these computer programs
!     and associated documentation reserved in all countries
!     of the world.
!     Author: Michael Metcalf  (metcalf@cern.ch)
!
!*******************************************************************************


!\USER DOC
!-------------------------------------------------------------------------------
!                              P R O G R A M 
!
!     Name: fx2fr 
! Category: --> stand-alone 
!  Written: 94/05/30   by: Michael Metcalf
! Maturity: --> raw, alpha, beta, production
!  Revised: --> 99/05/31   by: Karen Goodger
!  Purpose: --> Convert fixed format fortran to free format fortran90
!
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!    A program to convert FORTRAN 77 source form to Fortran 90 source    
!  form. It also formats the code by indenting the bodies of DO-loops  
!  and IF-blocks by ISHIFT columns. Statement keywords are             
!  followed if necessary by a blank, and blanks within tokens are      
!  are suppressed; this handling of blanks is optional.                
!    If a CONTINUE statement terminates a single DO loop, it is        
!  replaced by END DO.                                                 
!    Procedure END statements have the procedure name added, if        
!  blanks are handled.                                                
!    Statements like INTEGER*2 are converted to INTEGER(2), if blanks  
!  are handled. Depending on the target processor, a further global   
!  edit might be required (e.g. where 2 bytes correspond to KIND=1).   
!  Typed functions and assumed-length character specifications are     
!  treated similarly. The length specification *4 is removed for all   
!  data types except CHARACTER, as is *8 for COMPLEX. This             
!  treatment of non-standard type declarations includes any            
!  non-standard IMPLICIT statements.                                   
!    Optionally, interface blocks only may be produced; this requires  
!  blanks processing to be requested. The interface blocks are         
!  compatible with both the old and new source forms.                  
!
!-------------------------------------------------------------------------------
!                                USAGE

!    Usage: the program reads one data record in free format from the  
!          default input unit. This contains:                          
!                                                                      
!                        name of file                                  
!                        indentation depth                             
!                        maximum indentation level                     
!                        whether significant blanks should be handled  
!                        whether interface blocks only are required    
!                                                                      
!   The default values in the absence of this record are:              
!                               name 3 10 T F                          
!     - to do nothing but change the source form, type e.g.:           
!                               name 0  0 F F                          
!       or simply                                                      
!                               name /                                 
!   The input is read from name.f, the output is written to name.f90   
!                            n can be optionally included if it simplifies
!     --> the documentation which comes later.
!
!-------------------------------------------------------------------------------
!                             RESTRICTIONS
!
!   Restrictions:  The program does not indent FORMAT statements or    
!                any statement containing a character string with an  
!                embedded multiple blank.                              
!                  The order of comment lines and Fortran statements   
!                is slightly modified if there are sequences of        
!                more than KKLIM (=200) comment lines.                 
!                  If there are syntax errors, continued lines do not  
!                have a trailing &.                                    
!                  When producing interface blocks, a check is required
!                that any dummy argument that is a procedure has a     
!                corresponding EXTERNAL statement. Also, since no      
!                COMMON blocks or PARAMETER statements are copied,     
!                part of an assumed-size array declaration may be      
!                missing. Similarly, parts of an assumed-length        
!                character symbolic constant might be copied and have  
!                to be deleted. BLOCK DATA statements are copied and   
!                must be deleted. These problems would normally be     
!                detected by a compiler and are trivially corrected.   
!                  Within a given keyword, the case must be all upper  
!                or all lower, and lower case programs require         
!                blank handling for correct indenting.                 
!                                                                      
!
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
!  No known limitations.
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!\END DOC



!\HISTORY DOC
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date      Author       Description
!     ----      ------       -----------
!  3.
!  2. 99/05/31  Karen Goodger         Added Conoco doc structure.  Changed
!                                     subroutine names to begin with fx2fr.
!  1. 94/05/30  Michael Metcalf       Initial version.
!
!-------------------------------------------------------------------------------
!\END DOC



!\PROG DOC
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
!                                 none
!
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!\END DOC

   MODULE FX2FR_STRUCTURE
!
!***********************************************************************
!   Define maximum level of DO-loop nesting, and maximum length of     *
!   a Fortran statement. LEN may be reduced for                        *
!   compilers accepting a maximum character                            *
!   length below 2640 and this will cause any excess                   *
!   continuation lines and all following lines to be copied unchanged. *
!   KKLIM defines the length of the comment line buffer. If this       *
!   length is exceeded, the statement preceding the comments will      *
!   appear after them.                                                 *
!***********************************************************************
      INTEGER, PARAMETER :: NEST = 32 , LEN = 2640 , KKLIM = 200,      &
      KLEN = 72*KKLIM
!
      INTEGER KNTDO , KNTIF , KNTCOM , LABEL , LENST , LABLNO, NOARG
      INTEGER, DIMENSION(NEST) :: LABLDO
!
      LOGICAL SYNERR, BLNKFL, INTFL
!
      CHARACTER(LEN=LEN) STAMNT
      CHARACTER(LEN=KLEN) CBUF
      CHARACTER(LEN=42) NAME
!
   END MODULE FX2FR_STRUCTURE
   MODULE FX2FR_DATA
!
      INTEGER, SAVE :: ISHIFT , MXDPTH , NIN , NOUT, TIME0
      LOGICAL, SAVE :: BLANKS, INTBFL
!
   END MODULE FX2FR_DATA
   MODULE FX2FR_STATISTICS
!
      INTEGER, SAVE :: MXDO , MXIF , KARD , KNTPU
!
      LOGICAL, SAVE :: SYNTAX, OVFLW, NONSTD
!
   END MODULE FX2FR_STATISTICS
!***********************************************************************
   PROGRAM FX2FR
!
!   Initialize
      CALL FX2FR_START
!
!   Process the lines of program units
      CALL FX2FR_PROGRAM_UNITS
!
!   Print some statistics
      CALL FX2FR_TERMINATE
   END PROGRAM FX2FR
   SUBROUTINE FX2FR_ARGUMENT(ARGNAM, LENARG, STAMNT, LENST, NOARG)
!
!   To store the argument names and function name, if any, for later
!   use in checking whether a specification statement is relevant to an
!   interface block.
      CHARACTER(LEN=*), INTENT(INOUT) :: ARGNAM(:)
      INTEGER, INTENT(OUT)            :: LENARG(:)
      CHARACTER(LEN=*), INTENT(IN)    :: STAMNT
      INTEGER, INTENT(IN)             :: LENST
      INTEGER, INTENT(OUT)            :: NOARG
!
!   Correct length of function name
      IF (NOARG.EQ.1) LENARG(1) = LEN_TRIM(ARGNAM(1))
!
!   Get any other arguments
      IND1 = index(STAMNT(:LENST), '(') + 1
      IF (IND1 .NE. 1 .AND. STAMNT(IND1:IND1) .NE. ')') THEN
         NEWIND = index(STAMNT(IND1+1:LENST), '(')
         IF (NEWIND.NE.0) IND1 = NEWIND + 1 + IND1
    3    IND2 = index(STAMNT(IND1:LENST), ',') - 1
         IF (IND2 .EQ. -1) IND2 = index(STAMNT(IND1:LENST), ')') - 1
         IND2 = IND2 + IND1 - 1
         IF (STAMNT(IND1+1:IND1+1) .NE. '*' ) THEN
            NOARG = NOARG +1
            ARGNAM(NOARG) = STAMNT(IND1:IND2)
            LENARG(NOARG) = IND2 - IND1 +1
         ENDIF
            IF (STAMNT(IND2+1:IND2+1) .EQ. ')') GO TO 4
         IND1 = IND2 + 3
         GO TO 3
      ENDIF
    4 LENARG(:NOARG) = MIN(LENARG(:NOARG), 6)
!
   END SUBROUTINE FX2FR_ARGUMENT
   SUBROUTINE FX2FR_BLANK
!
!   To suppress all blanks in the statement, and then to place
!   a blank on each side of =,  +, -, * and / (but not ** or //), a
!   blank after each ) and , and a blank before each (.
!   No changes are made within character strings or FORMAT statememts.
!
      USE FX2FR_DATA
!
      USE FX2FR_STATISTICS
!
      USE FX2FR_STRUCTURE
!
      CHARACTER(LEN=LEN) BUFFER
!
!   Reduce length to that of significant characters
      BLNKFL = .FALSE.
      LENST = LEN_TRIM(STAMNT(1:LENST))
      IF (.NOT.BLANKS) THEN
         IF (LEN-LENST.GE.2) STAMNT(LENST+1:LENST+2) = '  '
         LENST = MIN(LENST+2, LEN)
         GO TO 99
      ENDIF
      BLNKFL = .TRUE.
!
!   Suppress blanks (add 2 to catch
!   odd number of apostrophes on a line in REFORM).
      LCHAR = 0
      NAPOST = 0
      DO L1 = 1, LENST
         IF (STAMNT(L1:L1) .EQ. "'") NAPOST = 1-NAPOST
         IF (NAPOST.EQ.0 .AND. STAMNT(L1:L1) .EQ. ' ') CYCLE
         LCHAR = LCHAR+1
         BUFFER(LCHAR:LCHAR) = STAMNT(L1:L1)
      END DO
      IF (LEN-LCHAR.GE.2) BUFFER(LCHAR+1:LCHAR+2) = '  '
      LCHAR = MIN(LCHAR+2, LEN)
!
!   Eliminate FORMATS
       IF( LABEL .NE. 0 .AND.                                          &
     & LCHAR .GE. 11 .AND.(BUFFER(:7) .EQ. 'FORMAT(' .OR.              &
     &                     BUFFER(:7) .EQ. 'format(') .AND.            &
     & BUFFER(LCHAR-2:LCHAR-2) .EQ. ')') THEN
         IF (LEN-LENST.GE.2) STAMNT(LENST+1:LENST+2) = '  '
         LENST = MIN(LENST+2, LEN)
         GO TO 99
       ENDIF
!
!   Insert blanks
      LENOLD = LENST
      LENST = 0
      NAPOST = 0
      DO L2 = 1, LCHAR
!
!   Check size of statement
         IF(LENST+3.GT.LEN) THEN
            LENST = LCHAR
            STAMNT(:LENST) = BUFFER(:LENST)
            OVFLW = .TRUE.
            GO TO 99
         ENDIF
!
!   Whether inside character string
         IF (BUFFER(L2:L2) .EQ. "'") NAPOST = 1-NAPOST
         IF (NAPOST.EQ.1) GO TO 3
!
!   Add blank padding according to character
         SELECT CASE (BUFFER(L2:L2))
         CASE ( ')' )
            STAMNT(LENST+1:LENST+2) = ') '
            LENST = LENST+2
         CASE ( '(' )
            STAMNT(LENST+1:LENST+2) = ' ('
            LENST = LENST + 2
         CASE ( ',' )
            STAMNT(LENST+1:LENST+2) = ', '
            LENST = LENST + 2
         CASE ( '=' )
            STAMNT(LENST+1:LENST+3) = ' = '
            LENST = LENST + 3
         CASE ( '*' )
            IF (BUFFER(L2-1:L2-1) .NE. '*' .AND. BUFFER(L2+1:L2+1)     &
            .NE. '*') THEN
               STAMNT(LENST+1:LENST+3) = ' * '
               LENST = LENST + 3
            ELSE
               GO TO 3
            ENDIF
         CASE ( '/' )
            IF (BUFFER(L2-1:L2-1) .NE. '/' .AND. BUFFER(L2+1:L2+1)     &
            .NE. '/') THEN
               STAMNT(LENST+1:LENST+3) = ' / '
               LENST = LENST + 3
            ELSE
               GO TO 3
            ENDIF
         CASE ('+')
            IF (BUFFER(L2-1:L2-1) .NE. 'E' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'e' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'D' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'd' .OR.                        &
          LLT(BUFFER(L2+1:L2+1), '0') .AND. LGT(BUFFER(L2+1:L2+1), '9')&
               ) THEN
               STAMNT(LENST+1:LENST+3) = ' + '
               LENST = LENST + 3
            ELSE
               GO TO 3
            ENDIF
         CASE ('-')
            IF (BUFFER(L2-1:L2-1) .NE. 'E' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'e' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'D' .AND.                       &
                BUFFER(L2-1:L2-1) .NE. 'd' .OR.                        &
          LLT(BUFFER(L2+1:L2+1), '0') .AND. LGT(BUFFER(L2+1:L2+1), '9')&
               ) THEN
               STAMNT(LENST+1:LENST+3) = ' - '
               LENST = LENST + 3
            ELSE
               GO TO 3
            ENDIF
         CASE DEFAULT
            GO TO 3
         END SELECT
         CYCLE
    3    STAMNT(LENST+1:LENST+1) = BUFFER(L2:L2)
         LENST = LENST +1
      END DO
!
!   Blank out end of statement
      IF (LENOLD.GT.LENST) STAMNT(LENST+1:LENOLD) = ' '
      IF (LENST.LT.LEN .AND. MOD(LENST, 66).NE.0)                      &
     &    STAMNT(LENST+1: LENST+66-MOD(LENST, 66)) = ' '
!
99 END SUBROUTINE FX2FR_BLANK
   SUBROUTINE FX2FR_IDENTIFY (IRET)
!
!***********************************************************************
!   To identify statement as beginning or end of DO-loop or            *
!   IF-block, or as probable FORMAT.                                   *
!   Attempt to scan as few of the input characters as possible.        *
!***********************************************************************
!
      USE FX2FR_STRUCTURE
      USE FX2FR_DATA
!
      CHARACTER(LEN=5), PARAMETER :: ENDIF='ENDIF' , THEN='NEHT)',     &
                                     THENLC='neht)'
      CHARACTER(LEN=3), PARAMETER :: BIF='IF('
      CHARACTER(LEN=2), PARAMETER :: DO='DO'
      CHARACTER(LEN=7), PARAMETER :: FORMAT='FORMAT('
      CHARACTER(LEN=4), PARAMETER :: ELSE='ELSE'
      CHARACTER(LEN=5)            :: INTFIL
      INTEGER, INTENT(OUT)        :: IRET
!
!
      IRET = 0
!
!   Check whether end of DO-loop
      IF (KNTDO /= 0) THEN
         IF (LABEL == LABLDO(KNTDO)) THEN
            IRET = 2
            GO TO 99
         ENDIF
      ENDIF
!
!   Check whether any of remaining possibilities
      DO L7 = 1 , LENST
         IF (STAMNT(L7:L7) == ' ') CYCLE
         IF (STAMNT(L7:L7) == 'E') THEN
            DO L11 = L7+1 , LENST
               IF (STAMNT(L11:L11) == ' ') CYCLE
               IF (STAMNT(L11:L11) == ENDIF(2:2)) GO TO 6
               IF (STAMNT(L11:L11) == ELSE(2:2)) GO TO 3
               GO TO 99
            END DO
         ENDIF
         IF (STAMNT(L7:L7) == BIF(:1)) GO TO 9
         IF (STAMNT(L7:L7) == DO(:1)) GO TO 15
         IF (STAMNT(L7:L7) == FORMAT(:1)) GO TO 31
         GO TO 99
      END DO
      GO TO  99
!
!   Check whether ELSE or ELSEIF
    3 K8 = 3
      DO L12 = L11+1 , LENST
         IF (STAMNT(L12:L12) == ' ') CYCLE
         IF (STAMNT(L12:L12) /= ELSE(K8:K8)) GO TO 99
         IF (K8 == 4) GO TO 5
         K8 = K8+1
      END DO
      GO TO  99
    5 IF (L12 >= LENST) THEN
         IRET = 6
         GO TO 99
      ENDIF
      IF (STAMNT(L12+1:LENST) == ' ') THEN
         IRET = 6
         GO TO 99
      ENDIF
      K2 = 1
      IRET = 6
      L7 = L12
      GO TO  10
!
!   Check whether end of IF-block
    6 K1 = 3
      DO L1 = L11+1 , LENST
         IF (STAMNT(L1:L1) == ' ') CYCLE
         IF (STAMNT(L1:L1) /= ENDIF(K1:K1)) GO TO 99
         IF (K1 == 5) EXIT
         K1 = K1+1
      END DO
      IF (L1 >= LENST) THEN
         IRET = 4
         GO TO 99
      ENDIF
      IF (STAMNT(L1+1:LENST) == ' ') IRET = 4
      GO TO  99
!
!   Check whether beginning of IF-block
    9 K2 = 2
      IRET = 3
   10 DO L2 = L7+1 , LENST
         IF (STAMNT(L2:L2) == ' ') CYCLE
         IF (STAMNT(L2:L2) /= BIF(K2:K2)) THEN
            IRET = 0
            GO TO 99
         ENDIF
         IF (K2 == 3) GO TO 12
         K2 = K2+1
      END DO
      IRET = 0
      GO TO  99
!
!   Backward search for )THEN at end of IF statement (to save
!   scanning the condition).
   12 K3 = 1
      DO L3 = LENST , L2+1 , -1
         IF (STAMNT(L3:L3) == ' ') CYCLE
         IF (STAMNT(L3:L3) /= THEN(K3:K3) .AND.                        &
             STAMNT(L3:L3) /= THENLC(K3:K3)) THEN
            IRET = 0
            GO TO 99
         ENDIF
         IF (K3 == 5) GO TO 99
         K3 = K3+1
      END DO
      IRET = 0
      GO TO  99
!
!   Check whether beginning of DO-loop
   15 DO L4 = L7+1 , LENST
         IF (STAMNT(L4:L4) == ' ') CYCLE
         IF (STAMNT(L4:L4) == DO(2:2)) GO TO 17
         GO TO 99
      END DO
      GO TO  99
!
!   Have DO - check label
   17 K5 = 0
      INTFIL = ' '
      DO L5 = L4+1 , LENST
         IF (STAMNT(L5:L5) == ' ') CYCLE
         IF (LLT(STAMNT(L5:L5) , '0') .OR. LGT(STAMNT(L5:L5) , '9'))   &
         EXIT
         K5 = K5+1
         IF (K5 > 5) GO TO 20
         INTFIL(K5:K5) = STAMNT(L5:L5)
      END DO
      IF (K5 == 0) GO TO 99
   20 READ (INTFIL , '(BN , I5)') LABLNO
      IF (LABLNO == 0) GO TO 99
!
!   Have label - check comma
      DO L8 = L5, LENST
         IF (STAMNT(L8:L8) == ' ') CYCLE
         IF (STAMNT(L8:L8) == ',') EXIT
         GO TO 23
      END DO
      IRET = 1
      GO TO  99
!
!   Have a DO and label with no comma.
!   Check for variable whose first of maximum of six
!   characters is alphabetic, followed by an equals sign,
!   followed by a character string containing a comma which is
!   not enclosed in parentheses.
   23 K6 = 0
      DO L9 = L8 , LENST
         IF (STAMNT(L9:L9) == ' ') CYCLE
         IF (K6 == 0) THEN
            IF ((LLT(STAMNT(L9:L9), 'A') .OR. LGT(STAMNT(L9:L9), 'Z')) &
           .AND.(LLT(STAMNT(L9:L9), 'a') .OR. LGT(STAMNT(L9:L9), 'z')))&
            GO TO 99
            K6 = 1
         ELSEIF (LGE(STAMNT(L9:L9) , 'A') .AND. LLE(STAMNT(L9:L9) ,'Z')&
            .OR. LGE(STAMNT(L9:L9) , 'a') .AND. LLE(STAMNT(L9:L9) ,'z')&
         .OR. LGE(STAMNT(L9:L9) , '0') .AND. LLE(STAMNT(L9:L9) , '9')) &
         THEN
            K6 = K6+1
            IF (K6 == 6) GO TO 26
         ELSE
            IF (K6 == 0) GO TO 99
            GO TO 25
         ENDIF
      END DO
      GO TO  99
!
!   Expect an equals sign
   25 L9=L9-1
   26 DO L10 = L9+1 , LENST
         IF (STAMNT(L10:L10) == ' ') CYCLE
         IF (STAMNT(L10:L10) == '=') GO TO 28
         GO TO 99
      END DO
      GO TO  99
!
!   Search for bare comma
   28 LPAREN = 0
      KNTCH = 0
      NAPOS = 0
      DO L6 = L10+1 , LENST
         IF (STAMNT(L6:L6) == ' ') CYCLE
         IF (STAMNT(L6:L6) == "'") NAPOS = 1 - NAPOS
         IF (NAPOS == 1) CYCLE
         IF (STAMNT(L6:L6) == ',') THEN
            IF (KNTCH /= 0) THEN
               IF (LPAREN == 0) GO TO 30
               CYCLE
            ELSE
               GO TO 99
            ENDIF
         ELSEIF (STAMNT(L6:L6) == '(') THEN
            LPAREN = LPAREN+1
         ELSEIF (STAMNT(L6:L6) == ')') THEN
            LPAREN = LPAREN-1
         ENDIF
         KNTCH = 1
      END DO
      GO TO  99
   30 IRET = 1
!
!   Insert blank after label
      IF (.NOT.BLANKS .OR. LENST.GE.LEN) GO TO 99
      DO L10 = LENST, L5, -1
         STAMNT(L10+1:L10+1) = STAMNT(L10:L10)
      END DO
      STAMNT(L5:L5) = ' '
      LENST = LENST  + 1
      GO TO  99
!
!   Identify FORMAT statement
   31 IF (LABEL == 0) GO TO 99
      K7 = 2
      DO L11 = L7+1 , LENST
         IF (STAMNT(L11:L11) == ' ') CYCLE
         IF (STAMNT(L11:L11) /= FORMAT(K7:K7)) GO TO 99
         IF (K7 == 7) GO TO 33
         K7 = K7+1
      END DO
      GO TO  99
   33 IRET = 5
!
99 END SUBROUTINE FX2FR_IDENTIFY
   SUBROUTINE FX2FR_KEYWORD(ASSIGN, SKIP)
!
!   To check whether those initial keywords of the statement which
!   require it are followed by a blank, to add one if necessary, and
!   to suppress any embedded blanks.
!
      USE FX2FR_STATISTICS
!
      USE FX2FR_STRUCTURE
!
      LOGICAL, INTENT(OUT) :: ASSIGN, SKIP
INTERFACE
   SUBROUTINE FX2FR_ARGUMENT(ARGNAM, LENARG, STAMNT, LENST, NOARG)
      CHARACTER(LEN=*), INTENT(INOUT) :: ARGNAM(:)
      INTEGER, INTENT(OUT)            :: LENARG(:)
      CHARACTER(LEN=*), INTENT(IN)    :: STAMNT
      INTEGER, INTENT(IN)             :: LENST
      INTEGER, INTENT(OUT)            :: NOARG
   END SUBROUTINE FX2FR_ARGUMENT
END INTERFACE
!
      INTEGER, PARAMETER :: NKEY = 42, MAXLEN = 15
      CHARACTER(LEN=MAXLEN) KEYS(NKEY), BEGIN, KEYSLC(NKEY)
      CHARACTER(LEN=LEN) BUFFER
      CHARACTER(LEN=3) THREE
      CHARACTER(LEN=32) FX2FR_NAMEOF
      CHARACTER(LEN=6), SAVE :: ARGNAM(445)
      LOGICAL BLANK(NKEY), IFASS, FOLLOW(NKEY)
      INTEGER LK(NKEY)
      INTEGER, SAVE :: LENARG(445)
!
      PARAMETER (KEYS= (/                                              &
     &'ASSIGN         ', 'BACKSPACE      ', 'BLOCKDATA      ',         &
     &'CALL           ', 'CHARACTER      ', 'CLOSE          ',         &
     &'COMMON         ', 'COMPLEX        ', 'CONTINUE       ',         &
     &'DATA           ', 'DIMENSION      ', 'DOUBLEPRECISION',         &
     &'DO             ', 'ELSEIF         ', 'ELSE           ',         &
     &'ENDFILE        ', 'ENDIF          ', 'ENTRY          ',         &
     &'EXTERNAL       ', 'EQUIVALENCE    ', 'FORMAT         ',         &
     &'FUNCTION       ', 'GOTO           ', 'IF             ',         &
     &'IMPLICIT       ', 'INQUIRE        ', 'INTEGER        ',         &
     &'INTRINSIC      ', 'LOGICAL        ', 'OPEN           ',         &
     &'PARAMETER      ', 'PAUSE          ', 'PRINT          ',         &
     &'PROGRAM        ', 'READ           ', 'REAL           ',         &
     &'RETURN         ', 'REWIND         ', 'SAVE           ',         &
     &'STOP           ', 'SUBROUTINE     ', 'WRITE          '/),       &
     &   LK=(/6, 9, 9, 4,                                              &
     &        9, 5, 6, 7, 8, 4,                                        &
     &        9,15, 2, 6, 4,                                           &
     &        7, 5, 5, 8,11,                                           &
     &        6, 8, 4, 2, 8, 7,                                        &
     &        7, 9, 7, 4, 9,                                           &
     &        5, 5, 7, 4, 4, 6,                                        &
     &        6, 4, 4,10, 5    /),                                     &
     &   BLANK=(/.TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,                    &
     &           .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  &
     &           .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .FALSE.,          &
     &           .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE.,          &
     &           .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., &
     &           .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .TRUE.,           &
     &           .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  &
     &           .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .FALSE./),        &
     &  FOLLOW=(/.TRUE.,  .TRUE.,  .FALSE., .TRUE.,                    &
     &           .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
     &           .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,          &
     &           .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE.,          &
     &           .FALSE., .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., &
     &           .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,          &
     &           .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., .TRUE.,  &
     &           .TRUE.,  .FALSE., .TRUE.,  .FALSE., .FALSE./) )
!
      PARAMETER (KEYSLC= (/                                            &
     &'assign         ', 'backspace      ', 'blockdata      ',         &
     &'call           ', 'character      ', 'close          ',         &
     &'common         ', 'complex        ', 'continue       ',         &
     &'data           ', 'dimension      ', 'doubleprecision',         &
     &'do             ', 'elseif         ', 'else           ',         &
     &'endfile        ', 'endif          ', 'entry          ',         &
     &'external       ', 'equivalence    ', 'format         ',         &
     &'function       ', 'goto           ', 'if             ',         &
     &'implicit       ', 'inquire        ', 'integer        ',         &
     &'intrinsic      ', 'logical        ', 'open           ',         &
     &'parameter      ', 'pause          ', 'print          ',         &
     &'program        ', 'read           ', 'real           ',         &
     &'return         ', 'rewind         ', 'save           ',         &
     &'stop           ', 'subroutine     ', 'write          '/))
!
!   Test for statement function statement or assignment statement
      SKIP = INTFL
      ASSIGN = .FALSE.
      IFASS = .FALSE.
      THREE = ' '
      LPAREN = 0
      KNTAP = 0
      KNTCH = 0
      DO L1 = 1, LENST
         IF (STAMNT(L1:L1)==' ') CYCLE
         IF (STAMNT(L1:L1)=='=') THEN
            IF (KNTCH==0) SYNERR = .TRUE.
            IF (LPAREN==0 .AND. KNTAP==0) THEN
               ASSIGN = .TRUE.
               GO TO 9
            ELSE
               EXIT
            ENDIF
         ELSEIF (STAMNT(L1:L1)=='(') THEN
            LPAREN = LPAREN+1
         ELSEIF (STAMNT(L1:L1)==')') THEN
            LPAREN = LPAREN-1
         ELSEIF (STAMNT(L1:L1)=="'") THEN
            KNTAP = 1-KNTAP
         ENDIF
         KNTCH = KNTCH+1
         IF (KNTCH<=3) THREE(KNTCH:KNTCH) = STAMNT(L1:L1)
      END DO
!
!   Suppress blanks in first 15 non-blank characters
   10 BEGIN = ' '
      LC = 0
      DO L2 = 1, LENST
         IF (STAMNT(L2:L2)==' ') CYCLE
         LC = LC+1
         BEGIN(LC:LC) = STAMNT(L2:L2)
         IF (LC==MAXLEN) GO TO 3
      END DO
      L2 = L2-1
!
!   Is this a keyword? Failure of this test is not fatal, in order to
!   allow for non-standard syntax extensions.
    3 DO L3 = 1, NKEY
         IF     (BEGIN(:LK(L3)) == KEYS(L3)(:LK(L3))) THEN
            GO TO 5
         ELSEIF (BEGIN(:LK(L3)) == KEYSLC(L3)(:LK(L3))) THEN
            LCC = 0
            DO  L33 = 1, L2
               IF (STAMNT(L33:L33).EQ.' ') CYCLE
               LCC = LCC + 1
               IF (LCC.EQ.LK(L3)) EXIT
            END DO
            STAMNT(:L33) = KEYS(L3)(:LK(L3))
            GO TO 5
         ENDIF
      END DO
      NONSTD = .TRUE.
      GO TO  98
!
!   Test for embedded blanks in keyword
    5 IF (L2 /= LC) THEN
         LC = 0
         DO L4 =1, LENST
            IF (STAMNT(L4:L4)==' ') CYCLE
            LC = LC+1
            IF (LC==LK(L3)) GO TO 7
         END DO
         GO TO 8
    7    IF (L4==LC) GO TO 8
         STAMNT(:L4) = KEYS(L3)(:LC)
         GO TO 99
      ENDIF
!
!   Keyword has no blanks - is it followed by a blank if it needs one?
    8 IF (.NOT.BLANK(L3)) GO TO 99
      NEXT = 0
      DO L8 = 1, LK(L3)
   17    NEXT = NEXT+1
         IF (STAMNT(NEXT:NEXT)==' ') GO TO 17
      END DO
      NEXT = NEXT+1
      IF (STAMNT(NEXT:NEXT)==' ') GO TO 99
!
!   Sometimes a delimiter may be present
      IF (L3==2.OR.L3==16.OR.L3==23.OR.L3==35.OR.L3==38) THEN
         IF (STAMNT(NEXT:NEXT)=='(') GO TO 99
      ELSEIF (L3==5) THEN
         IF (STAMNT(NEXT:NEXT)=='*') GO TO 99
      ELSEIF (L3==7.OR.L3==39) THEN
         IF (STAMNT(NEXT:NEXT)=='/') GO TO 99
      ENDIF
      IF (LENST==LEN) THEN
         OVFLW = .TRUE.
         GO TO 99
      ENDIF
!
!   Insert the blank
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      LENST = LENST+1
      STAMNT(NEXT:NEXT) = ' '
      STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
      BLNKFL = .TRUE.
      GO TO  99
!
!   Check whether, in fact, a DO-loop
    9 IF (THREE(:2) /= 'DO' .AND. THREE(:2) /= 'do') GO TO 12
      LPAREN = 0
      NAPOS = 0
      DO L5 = L1+2, LENST
         IF (STAMNT(L5:L5)==' ') CYCLE
         IF (STAMNT(L5:L5) == "'") NAPOS = 1 - NAPOS
         IF (NAPOS == 1) CYCLE
         IF (STAMNT(L5:L5)==',') THEN
            IF (LPAREN==0) THEN
               ASSIGN = .FALSE.
               GO TO 10
            ENDIF
         ELSEIF (STAMNT(L5:L5)=='(') THEN
            LPAREN = LPAREN+1
         ELSEIF (STAMNT(L5:L5)==')') THEN
            LPAREN = LPAREN-1
         ENDIF
      END DO
      GO TO  99
!
!   Check whether, in fact, a logical IF followed by an assignment
   12 IF (THREE /= 'IF(' .AND. THREE /= 'if(') GO TO 99
      IFASS = .TRUE.
      DO L6 = L1-1, 1, -1
         IF (STAMNT(L6:L6)==' ') CYCLE
         IF (STAMNT(L6:L6)==')') THEN
!
!   Is there a second pair of first-level parentheses
            IF (index(STAMNT(:L6), ')')==0) GO TO 99
            LPAREN = 1
            DO L7 = L6-1, 4, -1
               IF (STAMNT(L7:L7)==' ') CYCLE
               IF (STAMNT(L7:L7)==')') THEN
                  IF (LPAREN==0) THEN
                     GO TO 14
                  ELSE
                     LPAREN = LPAREN+1
                  ENDIF
               ELSEIF (STAMNT(L7:L7)=='(') THEN
                  LPAREN = LPAREN-1
               ENDIF
            END DO
            GO TO 99
   14       ASSIGN = .FALSE.
            GO TO 10
         ELSE
            ASSIGN = .FALSE.
            GO TO 10
         ENDIF
      END DO
!
!   Test for non-executable statement keyword
   99 IF (ASSIGN) GO TO 98
      IF (.NOT.INTFL) GO TO 97
      SKIP = L3.EQ. 3.OR.L3.EQ. 5.OR.L3.EQ. 8                          &
         .OR.L3.EQ.11.OR.L3.EQ.12.OR.L3.EQ.19.OR.L3.EQ.22              &
         .OR.L3.EQ.25.OR.L3.EQ.27.OR.L3.EQ.29                          &
         .OR.L3.EQ.34.OR.L3.EQ.36.OR.L3.EQ.41
      SKIP = .NOT.SKIP
      IF (SKIP) GO TO 98
!
!   Check whether this statement refers to an argument or a function
!   name
      IF (L3.EQ.3 .OR. L3.EQ.22 .OR. L3.EQ.25 .OR.                     &
                       L3.EQ.34 .OR. L3.EQ.41) GO TO 97
      IF(index(STAMNT(LK(L3)+1:LENST), 'FUNCTION').NE.0 .OR.           &
         index(STAMNT(LK(L3)+1:LENST), 'function').NE.0) GO TO 97
      DO L20 = 1, NOARG
         IF(index(STAMNT(LK(L3)+1:LENST), ARGNAM(L20)(:LENARG(L20)))   &
         .NE. 0) GO TO 97
      END DO
      SKIP = .TRUE.
      GO TO 98
!
!   Keep procedure name for END statement
   97 IF(L3.EQ.3.OR.L3.EQ.22.OR.L3.EQ.34.OR.                           &
      L3.EQ.41) NAME = KEYS(L3)(:LK(L3))//FX2FR_NAMEOF(STAMNT(LK(L3)+2:LENST))
!
!   Get argument names for later use in skipping unnecessary
!   specifications
   21 IF (INTFL) THEN
         IF (L3.EQ.22) THEN
            ARGNAM(1) = NAME(10:15)
            NOARG = 1
         ENDIF
         IF (L3.EQ.22 .OR. L3.EQ.41)                                   &
         CALL FX2FR_ARGUMENT(ARGNAM, LENARG, STAMNT, LENST, NOARG)
      ENDIF
!
!   Deal with awkward cases
      LSAVE = L3
      IF(L3.EQ.1.OR.L3.EQ.5.OR.L3.EQ.8.OR.L3.EQ.12 .OR. L3.EQ.13       &
     &.OR. L3.EQ.25                                                    &
     &.OR.L3.EQ.24.AND..NOT.IFASS.OR.L3.EQ.27.OR.L3.EQ.29.OR.L3.EQ.36) &
     &  CALL SPECIAL(L3, NEXT, BUFFER, NKEY, KEYS, KEYSLC, LK, FOLLOW)
!
!   Was, in fact, a function
      IF (INTFL.AND.L3.EQ.22.AND.LSAVE.NE.22) THEN
         SKIP = .FALSE.
         GO TO 21
      ENDIF
!
!   Print procedure name
   98 IF(.NOT.ASSIGN.AND.(L3.EQ.3.OR.L3.EQ.22.OR.L3.EQ.34.OR.L3.EQ.41))&
      WRITE (*, '('' Starting '', A)') NAME
   END SUBROUTINE FX2FR_KEYWORD
   FUNCTION FX2FR_NAMEOF(HEADER)  result (FX2FR_NAMEOF_OUTPUT)
!
!   Pick out name of procedure
      CHARACTER(LEN=*), INTENT(IN) :: HEADER
     character(len=32)  :: FX2FR_NAMEOF_OUTPUT
!
      FX2FR_NAMEOF_OUTPUT = ' '
!
!   Is there a left parenthesis or an asterisk?
      IND = index(HEADER, '(' )
      INDAST = index(HEADER, '*')
      IF (IND.NE.0 .AND. INDAST.NE.0) IND = MIN(IND, INDAST)
      IF (IND.LE.LEN(FX2FR_NAMEOF_OUTPUT)) THEN
         IF (IND .EQ. 0) THEN
            FX2FR_NAMEOF_OUTPUT(2:) = HEADER(:LEN(HEADER))
         ELSE
            FX2FR_NAMEOF_OUTPUT(2:IND) = HEADER(:IND-1)
         ENDIF
      ENDIF
   END FUNCTION FX2FR_NAMEOF
   SUBROUTINE FX2FR_PROGRAM_UNITS
!
!***********************************************************************
!   The principal subroutine of FX2FR processes the                  *
!   input stream, which is assumed to contain syntactically correct    *
!   Fortran program units. To protect itself from bad data, failure    *
!   to pass a primitive syntax check will cause the program to copy    *
!   the input stream to the output unit unchanged, until an END line is*
!   encountered.                                                       *
!***********************************************************************
!
      USE FX2FR_DATA
!
      USE FX2FR_STATISTICS
!
      USE FX2FR_STRUCTURE
!
!***********************************************************************
!   USER is a character which may be defined to identify lines         *
!   in the input stream which are to be treated as                     *
!   comment lines ( + in this example).                                *
!***********************************************************************
!
      CHARACTER CONTIN
      CHARACTER(LEN=3), PARAMETER :: FIN='END', FINLC='end'
      CHARACTER(LEN=66) FIELD
      CHARACTER(LEN=72) LINE
      CHARACTER(LEN=72), PARAMETER :: BLANKV=' '
!
      LOGICAL  NEWDO , NEWIF , FORM ,  ELSEBL , ASSIGN
!
      CHARACTER, PARAMETER :: USER = '+'
      LOGICAL :: STAT = .FALSE. , SKIP = .FALSE.
!
!   Start processing program units
      MXDO = 0
      MXIF = 0
      KARD = 0
      KNTPU = 0
      SYNTAX = .FALSE.
      SYNERR = .FALSE.
      OVFLW = .FALSE.
      NONSTD = .FALSE.
      KNTDO = 0
      KNTIF = 0
      KNTCOM = 0
      NAME = ' '
      NOARG = 0
      INTFL = INTBFL
!
!   Set continuation line counter
    1 KNTCON = 0
!
!   Set statement length counter
      LENST = 0
!
!   Read one line into an internal file,
!   columns 73-80 of all lines are ignored.
    2 READ (NIN , '(A)' , END = 100 , ERR = 100) LINE
      KARD = KARD+1
!
!   Check whether a comment line and if so copy to buffer.
      IF (LINE(:1) == 'C' .OR. LINE(:1) == '*' .OR. LINE(:1) ==        &
      USER .OR. LINE == ' ' .OR. LINE(:1) == 'c'                       &
                            .OR. LINE(:1) == '!') THEN
         IF (INTFL) GO TO 2
         IF (LINE(:1) == 'C' .OR. LINE(:1) == '*'                      &
         .OR. LINE(:1) == 'c') LINE(:1) = '!'
         IF (KNTCOM == KKLIM) THEN
            WRITE (NOUT , '(A72)') (CBUF(72*L5-71:72*L5) , L5 = 1 ,    &
            KNTCOM) , LINE
            KNTCOM = 0
         ELSEIF (SYNERR .OR. .NOT.STAT) THEN
            WRITE (NOUT , '(A72)') LINE
         ELSE
            KNTCOM = KNTCOM+1
            CBUF(72*KNTCOM-71:72*KNTCOM) = LINE
         ENDIF
         GO TO 2
      ENDIF
!
!   Some form of embedded comment?
      NAPO = 0
      DO L22 = 2, 72
         IF (LINE(L22:L22) .EQ. '''') NAPO = 1 - NAPO
         IF (LINE(L22:L22) .NE. '!') CYCLE
         IF (NAPO .NE. 0) CYCLE
         IF (.NOT. INTFL) THEN
            IF (KNTCOM .LT. KKLIM) THEN
               KNTCOM = KNTCOM +1
               CBUF(72*KNTCOM-71:72*KNTCOM) =                          &
                                BLANKV(:L22-1)//LINE(L22:72)
            ELSE
               WRITE (NOUT, '(A)') BLANKV(:L22-1)//LINE(L22:72)
            ENDIF
         ENDIF
         LINE(L22:72) = ' '
         IF (LINE .EQ. ' ') GO TO 2
         EXIT
      END DO
!
!   Line is some form of statement; re-read.
      READ (LINE , '(BN , I5 , A1 , A66)') LAB , CONTIN , FIELD
      STAT = .TRUE.
!
!   Check on syntax and copy to statement buffer
    3 IF (CONTIN == '0') CONTIN = ' '
      IF (CONTIN /= ' ') THEN
         CONTIN = '&'
         IF (SYNERR) THEN
            GO TO 6
         ELSEIF (LENST == 0 .OR. LENST+66 > LEN .OR. LAB /= 0) THEN
            SYNERR = .TRUE.
            IF (LENST > 0) THEN
               IF (LABEL /= 0) THEN
                  WRITE (NOUT , '(I5, 1X, A66:"&"/(5X, "&", A66:       &
     &            "&"))') LABEL ,                                      &
     &            (STAMNT(66*L9-65:66*L9) , L9 = 1 , (LENST+65)/66)
               ELSE
                  WRITE (NOUT , '(6X, A66:"&"/(5X, "&", A66:"&"        &
     &            ))') (STAMNT(66*L9-65:66*L9) , L9 = 1 , (LENST+65)/66)
               ENDIF
            ENDIF
            IF (LAB /= 0) THEN
               WRITE (NOUT , 1000) LAB , CONTIN , FIELD
            ELSE
               WRITE (NOUT , 1006) CONTIN , FIELD
            ENDIF
            GO TO 1
         ELSE
            KNTCON = KNTCON+1
            STAMNT(LENST+1:LENST+66) = FIELD
            LENST = LENST+66
            GO TO 2
         ENDIF
      ELSEIF (KNTCON == 0) THEN
         IF (LENST /= 0) GO TO 4
         STAMNT(1:66) = FIELD
         LENST = 66
         LABEL = LAB
         IF (SYNERR) GO TO 4
         GO TO 2
      ENDIF
      IF (KNTCON > 0) GO TO 6
!
!   Have a complete statement ready for processing (the last line
!   read is still waiting in LINE). The statement now needs to be
!   identified.
!   The END statement is a special case - if found it will be copied
!   and the next program unit processed.
    4 K1 = 1
      DO  L1 = 1 , LENST
         IF (STAMNT(L1:L1) == ' ') CYCLE
         IF (STAMNT(L1:L1) /= FIN(K1:K1) .AND.                         &
             STAMNT(L1:L1) /= FINLC(K1:K1)) THEN
            EXIT
         ELSE
            K1 = K1+1
            IF (K1 > 3 .AND. (L1 >= LENST .OR. STAMNT(L1+1:LENST)      &
            == ' ')) THEN
               IF (.NOT.SYNERR) THEN
                  KNTPU=KNTPU+1
                  IF (LABEL == 0) THEN
                     WRITE (NOUT , 1001) FIN, NAME
                  ELSE
                     WRITE (NOUT , 1002) LABEL , FIN, NAME
                  ENDIF
               ENDIF
!
!   Set counters for new program unit
               SYNTAX = SYNTAX .OR. SYNERR
               KNTDO = 0
               KNTIF = 0
               SYNERR = .FALSE.
               KNTCON = 0
               LENST = 0
               IF (KNTCOM /= 0) WRITE (NOUT , '(A72)') (CBUF(72*L5-71: &
               72*L5) , L5 = 1 , KNTCOM)
               KNTCOM = 0
               NAME = ' '
               NOARG = 0
               GO TO 3
            ELSE
               IF (K1 > 3) EXIT
            ENDIF
         ENDIF
      END DO
!
!   If syntax error flag set, copy and take next statement
    6 IF (SYNERR) THEN
         IF (LAB /= 0) THEN
            WRITE (NOUT , 1000) LAB , CONTIN , FIELD
         ELSE
            WRITE (NOUT , 1006) CONTIN , FIELD
         ENDIF
         LENST = 0
         GO TO 2
      ENDIF
!
!   Compress blanks and insert blanks around special characters
      CALL FX2FR_BLANK
!
!   Handle Fortran keywords
      NEWDO = .FALSE.
      NEWIF = .FALSE.
      FORM  = .FALSE.
      ELSEBL = .FALSE.
      ASSIGN = .FALSE.
      IF (BLANKS) CALL FX2FR_KEYWORD(ASSIGN, SKIP)
      IF (SKIP) GO TO 16
      IF (SYNERR) GO TO 6
      IF (BLANKS .AND. ASSIGN .AND. LABEL.EQ.0) GO TO 14
!
!   Have a valid statement which is not an END line or assignment
!   Identify statement as    DO
!                            IF ( ) THEN
!                            DO terminator
!                            ENDIF
!                            FORMAT
!                            ELSE or ELSEIF
!                            none of these.
      CALL FX2FR_IDENTIFY(IRTCOD)
      SELECT CASE (IRTCOD)
         CASE (0)
            GO TO  14
!
!   New DO-loop
         CASE (1)
            IF (KNTDO == NEST) GO TO 14
            NEWDO = .TRUE.
            LABLDO(KNTDO+1) = LABLNO
!
!   End of DO-loop(s)
         CASE (2)
            NEND = 0
            DO  L5 = KNTDO , 1 , -1
               IF (LABLDO(L5) /= LABEL) EXIT
               NEND = NEND + 1
            END DO
!
!   Replace CONTINUE by END DO
      KNTDO = KNTDO - NEND
      IF (NEND.EQ.1 .AND. LENST.EQ.10 .AND. STAMNT(:LENST).EQ.         &
     &'CONTINUE  ') THEN
         STAMNT(:8) = 'END DO  '
         LENST = 6
      ENDIF
!
!   Beginning of IF-block
         CASE (3)
            NEWIF = .TRUE.
!
!   End of IF-block
         CASE (4)
            KNTIF = KNTIF-1
            IF (KNTIF < 0) THEN
               SYNERR = .TRUE.
               KNTIF = 0
            ENDIF
!
!   FORMAT statement
         CASE (5)
            FORM =.TRUE.
!
!   Beginning of ELSE-block
         CASE (6)
            IF (KNTIF .GT. 0) THEN
               ELSEBL = .TRUE.
            ELSE
              SYNERR = .TRUE.
            ENDIF
      END SELECT
!
!   Reformat statements and write
   14 CALL FX2FR_REFORM (FORM , ELSEBL)
!
!   Set variables for next statement
      IF (NEWDO) KNTDO = KNTDO+1
      IF (NEWIF) KNTIF = KNTIF+1
      MXDO = MAX(MXDO , KNTDO)
      MXIF = MAX(MXIF , KNTIF)
   16 KNTCON = 0
      LENST = 0
      GO TO   3
!
!   End of data. Last line must be an END.
  100 IF (LABEL == 0) WRITE (NOUT , 1001) FIN, NAME
      IF (LABEL /= 0) WRITE (NOUT , 1002) LABEL , FIN, NAME
      KNTPU=KNTPU+1
      IF (INTFL) WRITE (NOUT, '(6X, ''END INTERFACE'')')
!
!   Note: if there is a syntax error, continued
!         statements do not have a trailing &
 1000 FORMAT(I5 , A1 , A)
 1001 FORMAT(6X , A3 ,1X, A)
 1002 FORMAT(I5 , 1X , A3 ,1X, A)
 1006 FORMAT(5X , A1 , A66)
!
   END SUBROUTINE FX2FR_PROGRAM_UNITS
   SUBROUTINE FX2FR_REFORM (FORM , ELSEBL)
!
!   Performs reformatting and output of accepted statements
!
      USE FX2FR_DATA
!
      USE FX2FR_STRUCTURE
!
      INTEGER, PARAMETER :: LLIMIT = LEN-(LEN/66-1)*6
      CHARACTER(LEN=LEN)  OUT
      CHARACTER AMP
!
      LOGICAL, INTENT(IN) :: FORM , ELSEBL
!
!   If FORMAT statement, do not indent
      IF (FORM) GO TO 9
!
!   Remove the blanks before commas if no character string
      IF (BLNKFL .AND. INDEX(STAMNT(:LENST), "'") == 0) THEN
         IPNT = 1
         DO
            IND = INDEX(STAMNT(IPNT:LENST), ' , ')
            IF (IND == 0) EXIT
            IND = IPNT + IND - 1
            STAMNT(IND:IND+2) = ',  '
            IPNT = IND + 3
         END DO
      END IF
!
!   Reformat indented statement and write. If reformatting causes it
!   to exceed LEN characters, it will be copied unchanged.
      IDEPTH = MIN(KNTDO+KNTIF , MXDPTH)
      IF (IDEPTH == 0 .AND. .NOT.BLNKFL) GO TO  9
      IF (ELSEBL) IDEPTH = IDEPTH-1
      IPNT = 1
      JPNT = 1
    1 IF (MOD(IPNT , 66) == 1) THEN
         IF (IPNT+65 > LEN) GO TO 9
         OUT(IPNT:IPNT+65) = ' '
         IPNT = IPNT+IDEPTH*ISHIFT
      ENDIF
!
!   Find first non-blank character
      DO  L2 = JPNT , LENST
         IF (STAMNT(L2:L2) /= ' ') GO TO 3
      END DO
      IF (JPNT == 1) THEN
         SYNERR = .TRUE.
         GO TO 9
      ELSE
         GO TO 10
      ENDIF
!
!   Find first multiple blank (but not in a character string)
    3 KNTAP = 0
      DO  L3 = L2, LENST-1
         IF (STAMNT(L3:L3) == "'") KNTAP = 1-KNTAP
         IF (STAMNT(L3:L3+1) == '  ') THEN
            IF (KNTAP == 0) GO TO 5
            GO TO 9
         ENDIF
      END DO
      L3 = LENST
!
!   Have section with no multiple blanks. This can be copied to OUT
!   if there is room on the current line. Otherwise cut the
!   section after the non-alphanumeric character nearest to the end of
!   the line, if one exists.
!   An apostrophe and period are considered to be alphanumeric
!   characters, in order to hold character strings,
!   and real and logical constants together;
!   underscores and dollars are so treated to handle common extensions,
!   and the ** and // operators and real literal constants are treated.
    5 KADD = 0
      IF (L3-L2 .LE. 66-MOD(IPNT , 66)) GO TO  8
      DO L4 = 66+L2-MOD(IPNT , 66) , L2 , -1
         IF (STAMNT(L4:L4) == ' ') GO TO 7
         IF (LGE(STAMNT(L4:L4) , 'A') .AND. LLE(STAMNT(L4:L4) , 'Z'))  &
         CYCLE
         IF(LGE(STAMNT(L4:L4), '0') .AND.                              &
                  LLE(STAMNT(L4:L4), '9')) CYCLE
         IF (STAMNT(L4:L4) .EQ. "'" .OR.                               &
         STAMNT(L4:L4) .EQ. '_' .OR. STAMNT(L4:L4) .EQ. '$' .OR.       &
         STAMNT(L4:L4) .EQ. '.') CYCLE
         IF (L4.NE.LENST) THEN
            IF (STAMNT(L4:L4+1) .EQ. '**' .OR.                         &
                STAMNT(L4:L4+1) .EQ. '//' ) CYCLE
            IF (L4.NE.L2) THEN
               IF(LGE(STAMNT(L4+1:L4+1), '0') .AND.                    &
                  LLE(STAMNT(L4+1:L4+1), '9')) THEN
                  IF (STAMNT(L4-1:L4) .EQ. 'E+' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'e+' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'E-' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'e-' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'D+' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'd+' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'D-' .OR.                   &
                      STAMNT(L4-1:L4) .EQ. 'd-' ) CYCLE
               ENDIF
            ENDIF
         ENDIF
         IF (LGE(STAMNT(L4:L4) , 'a') .AND. LLE(STAMNT(L4:L4) , 'z'))  &
         CYCLE
         GO TO 7
      END DO
!
!   No break character found
      IF (BLNKFL) GO TO 9
      L4 = 66-MOD(IPNT , 66)+L2
!
!   Cut here
    7 L3 = L4
      KADD = 1
    8 LOUT = IPNT+L3-L2
      IF (LOUT > LEN) GO TO  9
      OUT(IPNT:LOUT) = STAMNT(L2:L3)
      IF (L3 == LENST) GO TO 10
!
!   Set pointers for next section of statement
      IPNT = LOUT+1
      IF (KADD == 1 .AND. MOD(IPNT , 66) /= 1 .OR. MOD(IPNT , 66)      &
     & >= 60) IPNT = ((IPNT+65)/66)*66+1
      IF (MOD(IPNT , 66) == 0) IPNT = IPNT+1
      JPNT = L3+1
      IF (KADD == 0) JPNT = JPNT+1
      GO TO   1
!
!   Copied statement (if adding 6 cols. to initial line would cause
!   total length to exceed 2640, must start it in col.1)
    9 LENST = LEN_TRIM(STAMNT(:LENST))
      IF (LENST > 66) THEN
         AMP = '&'
      ELSE
         AMP = ' '
      ENDIF
      IF (LABEL /= 0) THEN
         WRITE (NOUT , 1003) LABEL , STAMNT(:MIN(LENST, 66)), AMP
      ELSE
         IF (LENST < LEN-6) THEN
            WRITE (NOUT , 1004) STAMNT(:MIN(LENST,66)), AMP
         ELSE
            WRITE (NOUT , '(A,A1)') STAMNT(:MIN(LENST, 66)), AMP
         ENDIF
      ENDIF
      IF (LENST > 66) WRITE (NOUT , 1005)                              &
     &('&', STAMNT(66*L6-65:66*L6) , L6 = 2 , (LENST+65)/66)
      GO TO  11
!
!   Write OUT to output unit
   10 LOUT = LEN_TRIM(OUT(:LOUT))
      IF (LOUT > 66) THEN
         AMP = '&'
      ELSE
         AMP =' '
      ENDIF
      IF (LABEL /= 0) THEN
         WRITE (NOUT , 1003) LABEL , OUT(:MIN(LOUT, 66)), AMP
      ELSE
         WRITE (NOUT , 1004) OUT(:MIN(LOUT, 66)), AMP
      ENDIF
!
!   An & is required in col. 6 if statement has more than 2412
!   characters, otherwise total length including cols. 1-6 will
!   exceed 2640. Also if making interface blocks, in order to be
!   compatible with both old and new source forms.
      IF (LOUT > 66) THEN
         IF (LOUT > LLIMIT .OR. INTFL) THEN
            AMP = '&'
         ELSE
            AMP = ' '
         ENDIF
         WRITE (NOUT , 1005) (AMP , OUT(66*L5-65:66*L5) , L5 = 2 , (   &
         LOUT+65)/66)
      ENDIF
!
!   Write any comments following statement
   11 IF (KNTCOM /= 0) THEN
         WRITE (NOUT ,'(A72)') (CBUF(72*L5-71:72*L5) , L5 = 1 , KNTCOM)
         KNTCOM = 0
      ENDIF
!
 1003 FORMAT(I5 , 1X , A, A)
 1004 FORMAT(6X , A, A)
 1005 FORMAT(5X , A , A:'&' )
!
   END SUBROUTINE FX2FR_REFORM
   SUBROUTINE SPECIAL(L3, NEXT, BUFFER, NKEY, KEYS, KEYSLC,         &
      LK, FOLLOW)
!
!   Special treatment for peculiar Fortran syntax
!
      USE FX2FR_STRUCTURE
!
      USE FX2FR_STATISTICS
!
      INTEGER, PARAMETER :: NUMLEN = 5
!
      CHARACTER(LEN=*), INTENT(OUT) :: BUFFER
      CHARACTER(LEN=*), INTENT(IN) :: KEYS(NKEY), KEYSLC(NKEY)
      CHARACTER(LEN=32) FX2FR_NAMEOF
      CHARACTER(LEN=NUMLEN) :: NUMBER
!
      INTEGER, INTENT(IN) :: LK(NKEY), NKEY
      INTEGER, INTENT(INOUT) :: L3, NEXT
      LOGICAL, INTENT(IN) :: FOLLOW(NKEY)
      LOGICAL IFASSIGN
!
      IFASSIGN = .FALSE.
!
!  Deal with labelled DO WHILE
      IF (L3.EQ.13) THEN
         IND = index(STAMNT(:LENST), 'WHILE')
         IF (IND.EQ.0) IND = index(STAMNT(:LENST), 'while')
         IF (IND.NE.0) THEN
            IF(LGE(STAMNT(IND-1:IND-1), '0') .AND.                     &
               LLE(STAMNT(IND-1:IND-1), '9'))                          &
               STAMNT(IND:IND+5) = ' WHILE'
         ENDIF
         GO TO 99
      ENDIF
!
!   Deal with IMPLICIT with non-standard length specifiers
      IF (L3 .EQ. 25) THEN
         IF (index(STAMNT(:LENST), '*') .NE. 0) THEN
!
!   first, CHARACTER*(len)
   11       IND = index(STAMNT(:LENST), 'CHARACTER *  (')
            IF (IND .EQ. 0) IND = index(STAMNT(:LENST),'character *  (')
            IF (IND.NE. 0) THEN
               STAMNT(IND+10:IND+10) = ' '
               GO TO 11
            ENDIF
!
!   then, type*nn
            NPAREN = 0
            NAPOS = 0
            DO L10 = 15, LENST
               IF (STAMNT(L10:L10) .EQ. "'") THEN
                  NAPOS = 1 - NAPOS
               ELSEIF (STAMNT(L10:L10) .EQ. '(') THEN
                  IF (NAPOS .EQ. 0) NPAREN = NPAREN + 1
               ELSEIF (STAMNT(L10:L10) .EQ. ')') THEN
                  IF (NAPOS .EQ. 0) NPAREN = NPAREN - 1
               ELSEIF (STAMNT(L10:L10) .EQ. '*') THEN
                  IF (NPAREN .EQ. 0) THEN
                     STAMNT(L10:L10+1) = ' ('
                     ILP = index(STAMNT(L10+2:LENST), '(')
                     IF (ILP .EQ. 0) THEN
                        SYNERR = .TRUE.
                        GO TO 99
                     ELSE
                        STAMNT(L10+ILP:L10+ILP) = ')'
                     ENDIF
                     IF (STAMNT(L10+1:L10+3) .EQ. '(4)') THEN
                        IF (STAMNT(L10-5:L10-5) .NE. 'C' .AND.         &
                            STAMNT(L10-5:L10-5) .NE. 'c')              &
                            STAMNT(L10+1:L10+3) = '   '
                     ELSEIF (STAMNT(L10-2:L10+3) .EQ. 'X  (8)' .OR.    &
                             STAMNT(L10-2:L10+3) .EQ. 'x  (8)')THEN
                        STAMNT(L10+1:L10+3) = '   '
                     ENDIF
 
                  ENDIF
               ENDIF
            END DO
         ENDIF
         GO TO 99
      ENDIF
!
!   An ASSIGN label must be followed by a blank and a * specifier
!   converted to (...)
      IF(L3.EQ.1 .AND. STAMNT(:7 ) .EQ. 'ASSIGN '     .OR.             &
         L3.EQ.5 .AND. STAMNT(:11) .EQ. 'CHARACTER *' .OR.             &
         L3.EQ.8 .AND. STAMNT(:9 ) .EQ. 'COMPLEX *'   .OR.             &
         L3.EQ.27.AND. STAMNT(:9 ) .EQ. 'INTEGER *'   .OR.             &
         L3.EQ.29.AND. STAMNT(:9 ) .EQ. 'LOGICAL *'   .OR.             &
         L3.EQ.36.AND. STAMNT(:6 ) .EQ. 'REAL *'          ) THEN
         IF (L3.LT.8) THEN
            ISSS = L3+7
         ELSEIF (L3.LT.30) THEN
            ISSS = 10
         ELSE
            ISSS = 7
         ENDIF
!
!   Extract the length parameter
         NDIGIT = 1
         NUMBER = '  '
         DO L20 = ISSS, LENST
            IF(STAMNT(L20:L20) .EQ. ' ') CYCLE
            NUMBER(:1) = STAMNT(L20:L20)
            IF(LGE(STAMNT(L20:L20),'0') .AND. LLE(STAMNT(L20:L20),'9') &
            ) GO TO 21
            GO TO 1
         END DO
         SYNERR = .TRUE.
         GO TO 99
   21    DO NEXT = L20+1, LENST
            IF(STAMNT(NEXT:NEXT) .EQ. ' ') CYCLE
            IF(LLT(STAMNT(NEXT:NEXT), '0') .OR. LGT(STAMNT(NEXT:NEXT), &
            '9')) GO TO 19
            NDIGIT = NDIGIT + 1
            IF (NDIGIT .GT. NUMLEN) THEN
               SYNERR = .TRUE.
               GO TO 99
            ENDIF
            NUMBER(NDIGIT:NDIGIT) = STAMNT(NEXT:NEXT)
         END DO
         SYNERR = .TRUE.
         GO TO 99
      ENDIF
      GO TO 1
!
!   Insert the blank or parentheses
   19 IF (LENST.GE.LEN-1) THEN
         OVFLW = .TRUE.
         GO TO 99
      ENDIF
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      IF (L3.EQ.1) THEN
         LENST = LENST+2
         STAMNT(NEXT:NEXT+3) = ' TO '
         STAMNT(NEXT+4:LENST) = BUFFER(NEXT+2:LENST-2)
      ELSE
         LENST = LENST+1
         STAMNT(NEXT:NEXT) = ' '
         STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
         IF (L3.NE.5.AND. NDIGIT .EQ. 1 .AND. NUMBER(:1) .EQ. '4') THEN
            STAMNT(NEXT-4:NEXT-1) = '    '
         ELSEIF (L3.EQ.8.AND.NDIGIT .EQ. 1 .AND. NUMBER(:1) .EQ. '8')  &
         THEN
            STAMNT(NEXT-4:NEXT-1) = '    '
         ELSE
            STAMNT(NEXT-3-NDIGIT:NEXT-1) = '('//NUMBER(:NDIGIT)//')'
         ENDIF
      ENDIF
      GO TO 2
!
!   Handle (*) case
    1 IF(L3.EQ.5 .AND. STAMNT(:18) .EQ. 'CHARACTER *  ( * )') THEN
         NEXT = 19
         STAMNT(11:11) = ' '
      ENDIF
!
!   IF statement may be followed by a keyword
    2 IF (L3 .EQ. 24 ) THEN
         LPAREN = 1
         NAPOS = 0
         DO NEXT = 5, LENST
            IF (STAMNT(NEXT:NEXT) .EQ. "'") NAPOS = 1 - NAPOS
            IF (NAPOS .EQ. 1) CYCLE
            IF (STAMNT(NEXT:NEXT) .EQ. '(' ) LPAREN = LPAREN+1
            IF (STAMNT(NEXT:NEXT) .EQ. ')' ) LPAREN = LPAREN-1
            IF (LPAREN .EQ. 0) GO TO 5
         END DO
         GO TO 99
    5    NEXT = NEXT+1
         DO L3 = 1, NKEY
            IF (FOLLOW(L3) .AND.(STAMNT(NEXT+1:NEXT+LK(L3)).EQ.KEYS(L3)&
                          .OR. STAMNT(NEXT+1:NEXT+LK(L3)).EQ.KEYSLC(L3)&
             )) THEN
               NEXT = NEXT+LK(L3)+1
               IF(L3.EQ.1) IFASSIGN = .TRUE.
               GO TO 9
            ENDIF
         END DO
      ELSE
!
! Typed function
         IF(STAMNT(NEXT+1:NEXT+8) .EQ. 'FUNCTION'  .OR.                &
            STAMNT(NEXT+1:NEXT+8) .EQ. 'function') THEN
            NEXT = NEXT+9
            NAME = 'FUNCTION'//FX2FR_NAMEOF(STAMNT(NEXT:LENST))
            L3 = 22
!
!   Deal with any *
            LIMIT = index(STAMNT(:LENST), '(')
            IF (LIMIT.NE.0) THEN
               ISTAR = index(STAMNT(:LIMIT), '*')
               IF (ISTAR .NE. 0) THEN
                  NDIGIT = LIMIT - ISTAR -3
                  IF (NDIGIT .GT. NUMLEN) THEN
                     SYNERR = .TRUE.
                     GO TO 99
                  ENDIF
                  NUMBER(:NDIGIT) = STAMNT(ISTAR+2:LIMIT-2)
                  STAMNT(NEXT-5+NDIGIT:LIMIT-2) =                      &
                                      'FUNCTION'//NAME(10:ISTAR-NEXT+8)
                  STAMNT(NEXT-8:NEXT-6+NDIGIT)  =                      &
                                      '('//NUMBER(:NDIGIT)//') '
                  IF (NDIGIT .EQ. 1 .AND. NUMBER(:1) .EQ. '4') THEN
                     STAMNT(NEXT-8:NEXT-5) = '    '
                  ELSEIF (NDIGIT .EQ. 1 .AND. NUMBER(:1) .EQ. '8' .AND.&
                     (STAMNT(7:7).EQ.'X'.OR.STAMNT(7:7).EQ.'x')) THEN
                     STAMNT(NEXT-8:NEXT-5) = '    '
                  ENDIF
                  NEXT = NEXT + 3 + NDIGIT
               ENDIF
            ELSE
               SYNERR = .TRUE.
               GO TO 99
            ENDIF
            GO TO 9
         ENDIF
      ENDIF
      GO TO 99
!
!   Insert the blank
    9 IF (LENST.GE.LEN-2) THEN
         OVFLW = .TRUE.
         GO TO 99
      ENDIF
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      LENST = LENST+1
      STAMNT(NEXT:NEXT) = ' '
      STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
!
!   ASSIGN may follow IF
      IF(.NOT.IFASSIGN) GO TO 99
      NEXT = index(STAMNT(:LENST), 'TO')
      IF (NEXT.EQ.0) NEXT = index(STAMNT(:LENST), 'to')
      IF(NEXT.EQ.0) THEN
         SYNERR = .TRUE.
         GO TO 99
      ELSE
         LENST = LENST+2
         STAMNT(NEXT:NEXT+3) = ' TO '
         STAMNT(NEXT+4:LENST) = BUFFER(NEXT+1:LENST-3)
      ENDIF
99 END SUBROUTINE SPECIAL
   SUBROUTINE FX2FR_START
!
!   To prepare for FX2FR_PROGRAM_UNITS
!
      USE FX2FR_DATA
      CHARACTER*16 NAME
!
!   Prompt for interactive use
      WRITE (*,'(" Type name of file, shift, max. indent level, T or F &
        &for blank treatment,",/ " T or F for interface blocks only.")')
      WRITE (*,'(" For simple use type only the name of the file ",    &
            &"followed by a slash (/) and RETURN.",/                   &
            &" Note that the name should be given WITHOUT extension!")')
!
!   Does standard input unit contain an input record
      NIN = 1
      NOUT = 2
      ISHIFT = 0
      MXDPTH = 0
      BLANKS = .FALSE.
      INTBFL = .FALSE.
      READ (* , * , END = 1 , ERR = 1) NAME, ISHIFT , MXDPTH ,         &
      BLANKS, INTBFL
!
!   If record present, check input values are reasonable
      ISHIFT = MIN(MAX(ISHIFT , 0) , 10)
      MXDPTH = MIN(MAX(MXDPTH , 0) , 36/MAX(ISHIFT,1))
      IF (INTBFL.AND..NOT.BLANKS) WRITE (*, '('' Interface block proces&
     &sing cancelled as blank processing not requested'')')
      INTBFL = BLANKS.AND.INTBFL
      GO TO  2
!
!   Set default values
    1 ISHIFT = 3
      MXDPTH = 10
      BLANKS = .TRUE.
      INTBFL = .FALSE.
      NAME = 'name'
    2 OPEN (UNIT=NIN, FILE=TRIM(NAME)//'.f', ACTION='READ')
      OPEN (UNIT=NOUT, FILE=TRIM(NAME)//'.f90', ACTION='WRITE')
!
!   Print values to be used
      Write (*,'(" Loop bodies will be indented by",I3/                &
     &           " Maximum indenting level is     ",I3)')              &
     &        ISHIFT , MXDPTH
      IF (BLANKS) WRITE (*,                                            &
     &'(" Significant blank proccessing requested")')
      IF (INTBFL) WRITE (*,                                            &
      '('' Only interface blocks will be produced'')')
      IF (INTBFL) WRITE (NOUT, '(6X, ''INTERFACE'')')
!
      CALL SYSTEM_CLOCK(TIME0)
   END SUBROUTINE FX2FR_START
   SUBROUTINE FX2FR_TERMINATE
!
!   To print the final summary
!
      USE FX2FR_STATISTICS
      USE FX2FR_DATA
!
      CALL SYSTEM_CLOCK(ITIME, ITICK)
      IF (ITICK /= 0)                                                  &
      WRITE (*,'(" Processing complete in ", F7.3, " seconds")')       &
            REAL(ITIME-TIME0)/REAL(ITICK)
      WRITE (*,'(" Maximum depth of DO-loop nesting ",I3/              &
     &           " Maximum depth of IF-block nesting",I3/              &
     &" No. of lines read  ",I17/" No. of program units read   ",I8/   &
     &           " Global syntax error flag",L12)')                    &
     &          MXDO , MXIF , KARD , KNTPU , SYNTAX
!
      IF (OVFLW) WRITE(*,  '(" At least one statement was too long to h&
     &ave a necessary blank added")')
      IF (NONSTD) WRITE (*,  '(" At least one statement began with a no&
     &n-standard keyword")')
!
    END SUBROUTINE FX2FR_TERMINATE













