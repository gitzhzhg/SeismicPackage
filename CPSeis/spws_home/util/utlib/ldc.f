      SUBROUTINE  LDCR(N,NC,DATCRD,RVAL,NAME,LAST) 
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
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name: LDC(real,int,chr,linked-arrays,setup)
C        Author: John B Sinton
C       Written: 87/01/08
C  Last revised: 90/11/06   Sinton
C
C  Purpose: Write an array or table of data into CPS DCODE format.             
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
C                          REVISION HISTORY
C      Date     Author     Description
C      ----     ------     -----------
C  9.  90/11/06 Sinton     Up NFIELD from 500 to 2000
C  8.  90/10/23 Peterson   Adjust MC to zero if NC = 0 upon entry so
C                          arrays can start on the proper new line.   
C  7.  89/10/02 Sinton     Reduced maximum number of characters/line for
C                          arrays and linked arrays from 69 to 68.
C  6.  88/09/26 Sinton     NWIH conversion.
C  5.  88/07/26 Sinton     Changed output for all blank characters.
C  4.  88/07/21 Sinton     Added table output and unified CRAY and
C                          VAX version of this routine.
C  3.  88/07/07 Sinton     Added calls to NUM2ALFR and NUM2ALFI,
C                          this is a major change and simplification.
C  2.  88/03/23 Sinton     Changed from G12.5 to G14.7 formats.
C  1.  87/03/31 Sinton     Added entry Ldcchrl.
C      87/01/08 Sinton     First version  
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE                            
C                                                                       
C        CALL LDCR   (N,NC,DATCRD,RVAL,NAME,LAST)       For real
C        CALL LDCI   (N,NC,DATCRD,IVAL,NAME,LAST)       For integer
C        CALL LDCC   (N,NC,DATCRD,BVAL,NAME,LAST,NCVAL) for Character
C        CALL LDCLASU(N,NLARR)            Setup table output.
C        CALL LDCILAR(N,IVAL,NAME)        Add integer array to table.
C        CALL LDCRLAR(N,RVAL,NAME)        Add real array to table.
C        CALL LDCCLAR(N,BVAL,NAME,NCVAL)  Add character array to table.
C        CALL LDCXLAR(N,BVAL,NAME,NCVAL)  Skip table position.
C        CALL LDCLAFI(NC,DATCRD,LAST)     Finish up table output.
C        CALL LDCSETU(LENWRD,CPUTYPE)     Setup these routines.
C
C  N       - Number of values in the array rval,ival, and/or bval.
C  NC      - Number of cards. Coming in this is the number of cards
C            that already exist.  NC will be reset to NC(in)+number of
C            new cards.
C  NLARR   - Number of array in the table.
C  DATCRD  - Character*80 array of cards.  The first new card will be
C            placed in DATCRD(2).  Please define DATCRD in the 
C            calling routine as   CHARACTER*80 DATCRD(some dimension).
C  RVAL    - Real array of values to be put into DCODE format.
C  IVAL    - Integer array of values to be put into DCODE format.
C  BVAL    - Integer array containing character data to be put into 
C            DCODE format.
C            Length of arrays elements MUST .le. 64 characters.   
C            BVAL MUST BE DEMENSIONED AS:
C            DIMENSION BVAL(NCVAL*N/LENWRD)
C            See discription of LENWRD and NCVAL below.
C  NAME    - Character variable with the name of the array. CHR*8       
C  LAST    - Logical variable.  
C            =.TRUE. if this is the last DCODE parameter to setup.
C            =.FALSE. if other DCODE parameters will follow.
C  NCVAL   - Integer containing the # of characters in one array 
C            element of BVAL.  The max length is 64.
C  LENWRD  - Number of bytes/characters per word.
C  CPUTYPE - Logical switch =.TRUE. if Cray cpu and .FALSE. otherwise.
C_______________________________________________________________________
C                                 NOTES
C
C 1. For entry points LDCC AND LDCCLAR only: imbedded blanks will be 
C    replaced by the character '|'. The input character array will not 
C    be altered, only the output card images.
C    Note BVAL is an integer dummy argument.  This allows you to
C    pass only the address of the character data.  You may get junk 
C    if you call LDCC or LDCCLAR with a character actual argument.
C 2. To output a table call LDCLASU first to set up the table.  Then
C    for each array in the table call LDC[I,R,C]LAR to load it's data
C    into the table.  To skip an array use the LDCXLAR call.  You MUST
C    pass LDC each array in the order you want them output.  Finally, 
C    call LDCLAFI to finish outputing the table. 
C 3. Use the LDCSETU call to set machine dependent parameters.  This 
C    entry point should be call BEFORE any other call to the routines
C    in this subroutine. 
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  LDCR
C Functions    : none
C Entrys       :
C  LDCI   ,LDCC  ,LDCLASU ,LDCILAR ,LDCRLAR ,LDCCLAR ,LDCXLAR ,LDCLAFI
C  LDCSETU
C Common blocks: none
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  NUM2ALFR NUM2ALFI
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
      PARAMETER (NFIELD = 2000)
      DIMENSION RVAL(N) ,IVAL(N) ,BVAL(N*NCVAL) 
      DIMENSION NAMES(20) ,MAXCH(20)
      INTEGER   BVAL
      CHARACTER FIELD(NFIELD)*64 ,C*64 ,FMT1*160,FMT2*160 ,CNF*2
      CHARACTER NAMES*8 ,MODULE*4 ,NCPW*2 ,NWPDV*2
      CHARACTER*(*) DATCRD(*) ,NAME
      LOGICAL LAST ,SEARCH ,CRAY,CPUTYPE
      DATA LENWORD/8/ ,CRAY/.TRUE./
      SAVE NS,NLA,NPOS,MAXCHRT,TABLELN,FIELD,NAMES,MAXCH
      MODULE = 'LDCR'
      MAXCHRS = 0                                                     
      DO 10 I=1,N
       CALL NUM2ALFR (RVAL(I),FIELD(I)(:14),*20,NCHARS)
       MAXCHRS = MAX( MAXCHRS, NCHARS )
 10   CONTINUE
      GOTO 800
 20   CONTINUE
      WRITE(C,'(G14.7)') RVAL(I)
      GOTO 999
      ENTRY       LDCI (N,NC,DATCRD,IVAL,NAME,LAST)
      MODULE = 'LDCI'
      MAXCHRS = 0
      DO 30 I=1,N
       CALL NUM2ALFI (IVAL(I),FIELD(I)(:8),*40,NCHARS)
       MAXCHRS = MAX( MAXCHRS, NCHARS )      
 30   CONTINUE
      GOTO 800
 40   CONTINUE
      WRITE(C,'(I14)') IVAL(I)
      GOTO 999
      ENTRY       LDCC (N,NC,DATCRD,BVAL,NAME,LAST,NCVAL)
      MODULE = 'LDCC'
      MAXCHRS = 1
      WRITE(NWPDV,'(I2)') NCVAL/LENWORD
      WRITE(NCPW ,'(I2)') LENWORD
      FMT1 = '(' // NWPDV // 'A' // NCPW // ')'
      C = 'NCVAL is >64!'
      IF (NCVAL.GT.64) GOTO 999
      NWORDS = NCVAL/LENWORD
      DO 70 I=1,N
       LNBC = NCVAL
       SEARCH = .TRUE.         
       WRITE(FIELD(I),FMT1) (BVAL(J),J=1+(I-1)*NWORDS,I*NWORDS)
       DO 50 J=NCVAL,1,-1
        IF (FIELD(I)(J:J).EQ.CHAR(0)) FIELD(I)(J:J) = ' '
        IF (FIELD(I)(J:J).EQ.' '.AND.SEARCH) THEN
         LNBC = J-1
        ELSE    
         SEARCH = .FALSE.
        ENDIF 
 50    CONTINUE
       DO 60 J=1,LNBC   
        IF (FIELD(I)(J:J).EQ.' ') FIELD(I)(J:J) = '|'
 60    CONTINUE
       MAXCHRS = MAX( MAXCHRS ,LNBC)  
 70   CONTINUE
      GOTO 800 
      ENTRY       LDCLASU(N,NLARR)
      NS   = N
      NLA  = NLARR
      IF (NS*NLA.GT.NFIELD) GOTO 997
      NPOS = 0
      MAXCHRT = 0
      TABLELN= 0
      WRITE(FIELD,'(64X)')
      DO 80 I=2,NFIELD
       FIELD(I)=FIELD(1)
 80   CONTINUE
      RETURN
      ENTRY       LDCILAR(N,IVAL,NAME)
      MODULE = 'ILAR'
      NPOS = NPOS+1
      MAXCHRS = 0                                           
      DO 90 I=1,NS                     
       CALL NUM2ALFI (IVAL(I),FIELD((I-1)*NLA+NPOS)(:8),*100,NCHARS)
       MAXCHRS = MAX( MAXCHRS, NCHARS )
 90   CONTINUE
      GOTO 700
 100  CONTINUE
      WRITE(C,'(I14)') IVAL(I)
      GOTO 999
      ENTRY       LDCRLAR(N,RVAL,NAME)
      MODULE = 'RLAR'          
      NPOS = NPOS+1
      MAXCHRS = 0
      DO 110 I=1,N
       CALL NUM2ALFR (RVAL(I),FIELD((I-1)*NLA+NPOS)(:14),*120,NCHARS)
       MAXCHRS = MAX( MAXCHRS, NCHARS )
 110  CONTINUE
      GOTO 700   
 120  CONTINUE
      WRITE(C,'(G14.7)') RVAL(I)
      GOTO 999
      ENTRY       LDCCLAR(N,BVAL,NAME,NCVAL)
      MODULE = 'CLAR'
      NPOS = NPOS+1
      WRITE(NWPDV,'(I2)') NCVAL/LENWORD
      WRITE(NCPW ,'(I2)') LENWORD
      FMT1 = '(' // NWPDV // 'A' // NCPW // ')'
      C = 'NCVAL is >64!'
      IF (NCVAL.GT.64) GOTO 999
      NWORDS = NCVAL/LENWORD
      MAXCHRS = 1
      DO 150 I=1,N
       LNBC = NCVAL
       SEARCH = .TRUE. 
       WRITE(FIELD((I-1)*NLA+NPOS),FMT1) 
     + (BVAL(J),J=1+(I-1)*NWORDS,I*NWORDS)
       DO 130 J=NCVAL,1,-1
        IF (FIELD((I-1)*NLA+NPOS)(J:J).EQ.CHAR(0)) THEN
         FIELD((I-1)*NLA+NPOS)(J:J) = ' ' 
        ENDIF
        IF (FIELD((I-1)*NLA+NPOS)(J:J).EQ.' '.AND.SEARCH) THEN
         LNBC = J-1                         
        ELSE           
         SEARCH = .FALSE.
        ENDIF 
 130   CONTINUE
       DO 140 J=1,LNBC
        IF      (FIELD((I-1)*NLA+NPOS)(J:J).EQ.' ') THEN
         FIELD((I-1)*NLA+NPOS)(J:J) = '|' 
        ENDIF
 140   CONTINUE
       MAXCHRS = MAX( MAXCHRS ,LNBC)  
 150  CONTINUE
      GOTO 700
      ENTRY       LDCXLAR(N,BVAL,NAME,NCVAL)
      MODULE = 'XLAR'
      NPOS = NPOS+1 
      MAXCHRS = 0
      GOTO 700
      ENTRY       LDCLAFI(NC,DATCRD,LAST) 
      MC = 1
      IF (NC.LE.0) MC = 0
      NC = NC+1
      MC = MC+1
      NCC = 2
      DATCRD(MC)(:2) = ' ['
      WRITE(C,'(64X)')
      DO 160 I=1,NLA
       IF (MAXCH(I).GT.0) THEN
        IF ( (I.NE.NLA.AND.NCC+MAXCH(I)+1.GT.80) .OR.
     +       (I.EQ.NLA.AND.NCC+MAXCH(I)+2.GT.80)) THEN
         NC = NC+1
         MC = MC+1                                
         NCC = 2
        ENDIF                   
        C(1:) = NAMES(I)
        DATCRD(MC)(NCC+1:NCC+MAXCH(I)+1) = C(:MAXCH(I))//','
        NCC = NCC+MAXCH(I)+1
       ENDIF
 160  CONTINUE
      DATCRD(MC)(NCC:NCC+1) = ']='
      DO 180 I=1,NS
       NC = NC+1
       MC = MC+1
       NCC= 2
       DATCRD(MC)(:2) = ' ('
       DO 170 J=1,NLA
        IF (MAXCH(J).GT.0) THEN
         IF ( (J.NE.NLA.AND.NCC+MAXCH(J)+1.GT.80) .OR.
     +        (J.EQ.NLA.AND.NCC+MAXCH(J)+2.GT.80)) THEN
          NC = NC+1
          MC = MC+1                                
          NCC = 2
         ENDIF
         K = (I-1)*NLA+J
         DATCRD(MC)(NCC+1:NCC+MAXCH(J)+1) = FIELD(K)(:MAXCH(J))//','
         NCC = NCC+MAXCH(J)+1
        ENDIF
 170   CONTINUE
       DATCRD(MC)(NCC:NCC+1) = '),'
 180  CONTINUE
      IF (LAST) DATCRD(MC)(NCC+1:) = ' '
      RETURN
      ENTRY       LDCSETU(LENWRD,CPUTYPE)
      IF (LENWRD.GT.0) LENWORD = LENWRD
      CRAY = CPUTYPE
      RETURN                                     
 700  CONTINUE
      IF (MAXCHRS.GT.0) THEN
       LENNAME    = LEN(NAME)           
       IF (LENNAME.GT.8 .OR. LENNAME.LT.1) GOTO 998
       NAMES(NPOS) = NAME
       DO 710 I=LENNAME,1,-1
 710   IF (NAME(I:I).NE.' ') GOTO 720
       I = 1
 720   CONTINUE
       LENNAME = I
      ELSE
       LENNAME    = 0
      ENDIF
      MAXCH(NPOS) = MAX( MAXCHRS ,LENNAME)
      MAXCHRT    = MAX( MAXCHRS ,MAXCHRT)
      TABLELN   = TABLELN+MAXCHRS
      RETURN
 800  CONTINUE
      NF = 68/(MAXCHRS+1)
      WRITE(CNF,'(I2)') MAX(NF-1,1)
      FMT1 = '(''         =('','//CNF//'(A,:,'',''),A,T79,'','')'
      FMT2 = '(''           '','//CNF//'(A,:,'',''),A,T79,'','')'
      LENNAME = LEN(NAME)
      IF (LENNAME.GT.8 .OR. LENNAME.LT.1) GOTO 998
      FMT1(4:LENNAME+3) = NAME
      MC = 1
      IF (NC.LE.0) MC = 0
      DO 810 I=1,N,NF
       NC = NC+1  
       MC = MC+1
       IF (I.EQ.1) THEN
        WRITE(DATCRD(MC),FMT1) (FIELD(J)(:MAXCHRS),J=I,MIN0(I+NF-1,N))
       ELSE                     
        WRITE(DATCRD(MC),FMT2) (FIELD(J)(:MAXCHRS),J=I,MIN0(I+NF-1,N))
       ENDIF
       IF (LAST) THEN
        IF (I+NF-1.GE.N) DATCRD(MC)(79:79) = ')'
       ELSE
        IF (I+NF-1.GE.N) DATCRD(MC)(79:80) = '),'
       ENDIF
 810  CONTINUE
      RETURN      
 997  CONTINUE
      PRINT*,'=>LDC: Not enough room in FIELD array for your data!'
      GOTO 1000
 998  CONTINUE
      PRINT*,'=>LDC: Length of the NAME for your variable is <1 or >8, 
     + name length=',LENNAME
      GOTO 1000
 999  CONTINUE
      PRINT'('' =>'',A,'': Error encoding the '',i4,'' value='',a)', 
     +MODULE,I,C(:14)
 1000 PRINT*,'=>Program terminating! *** LDC***'
      STOP '***LDC***'
      END      
