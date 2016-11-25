      SUBROUTINE ENCRVAL (CHR,NCD,NCH,RVAL,NAME,LAST)
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
C  Process name: ENCVAL                   
C        Author: John B. Sinton           
C  Last revised: 89/04/12                 
C  Purpose: Write a scalar value into CPS DCODE format.             
C           Entry point R  is used for real    data.
C           Entry point I  is used for integer data.
C           Entry point A  is used for charact data.
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
C  89/04/12: H.BALL     Documentation changes only.
C  88/09/26: JB Sinton, NWIH conversion.
C  88/09/06; B Baumel, Remove remaining differences from
C            VAX version.
C  88/07/29; B Baumel, Take out space after value in
C            ENCAVAL.
C  88/07/21; JB Sinton, Unified CRAY and VAX versions.
C  88/07/07; JB Sinton, Added calls to NUM2ALFR and
C            NUM2ALFI.
C  87/03/30 - First version
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE                            
C                                                                       
C        CALL ENCRVAL (CHR,NCD,NCH,RVAL,      NAME,LAST) For real.
C        CALL ENCIVAL (CHR,NCD,NCH,IVAL,      NAME,LAST) For integer.
C        CALL ENCAVAL (CHR,NCD,NCH,BVAL,NCVAL,NAME,LAST) For charact.
C                                                                       
C  CHR     - Array of 80 character card images.
C  NCD     - Number of card image to write into.  If the data will go
C            passed the end of a card, NCD will be incremented and
C            the next card will be written into.
C  NCH     - Last character used in CHR(NCD).  NCH will be updated 
C            on return to calling routine.
C  RVAL    - Real value to be put into DCODE format.
C  IVAL    - Integer value to be put into DCODE format.
C  BVAL    - Character value to be put into DCODE format.
C            Actual argument must be type real or integer.
C            The max character length of BVAL is 64 characters.
C            BVAL must be dimensioned:
C            DIMENSION BVAL(NCVAL/LENWRD),
C            where NCVAL and LENWORD are discribed below.
C  NAME    - Character variable with the name of the array. CHR*8       
C  LAST    - Logical variable.  
C            =.TRUE. if this is the last DCODE parameter to setup.
C            =.FALSE. if other DCODE parameters will follow.
C  NCVAL   - Integer containing the # of characters in BVAL.
C  LENWRD  - Number of bytes/characters per word.
C  CPUTYPE - Logical argument, =.TRUE. for CRAY and .FALSE. for VAX.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. For entry point ENCAVAL only, imbedded blanks will be replaced by 
C    the character '|'. The input data will not be altered.  
C 2. Machine dependent parameters may be set by calling ENCSETU.  Call
C    ENCSETU before all other calls to routines in this subroutine.
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  ENCRVAL
C Functions    : none
C Entrys       :
C  ENCIVAL ,ENCAVAL ,ENCSETU ,ENCDB
C Common blocks: none
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  NUM2ALFR ,NUM2ALFI
C Functions:   none
C  Note- All routines called are CPS primitives.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C\END DOC
      INTEGER BVAL(*)
      CHARACTER CHR(100)*80 ,NAME*8 ,MODULE*7 ,NWPDV*2 ,NCPW*2
      CHARACTER*80 FMT,VALUE
      LOGICAL LAST ,SEARCH ,CRAY,CPUTYPE ,ERRPRT,ERRPRTU
      DATA NVAL /1/
      DATA LENWORD/8/ ,CRAY/.TRUE./ ,ERRPRT/.FALSE./
      SAVE NVAL,LENWORD,CRAY
      MODULE = 'ENCRVAL'
      IF (ERRPRT) PRINT*,'ENCRVAL: REAL VALUE=',RVAL
C***************************************************
C *** The real number RVAL is encoded as follows ***
C *** If    RVAL>1 then VALUE='ffffffff.'        ***
C *** If  0<RVAL<1 then VALUE='0.fffffff'        ***
C *** If    RVAL=0 then VALUE='0'                ***
C *** If -1<RVAL<0 then VALUE='-0.fffffff'       ***
C *** If   RVAL<-1 then VALUE='-ffffffff.'       ***
      CALL NUM2ALFR (RVAL,VALUE(:14)  ,*2,NCHARS)
C***************************************************
      GOTO 800  !Go to load encoded value into card image.
 2    WRITE(VALUE(2:),'(G14.7)') RVAL
      MC = 15
      GOTO 999
      ENTRY    ENCIVAL (CHR,NCD,NCH,IVAL,NAME,LAST)
      MODULE = 'ENCIVAL'
      IF (ERRPRT) PRINT*,'ENCIVAL: INTEGER VALUE=',IVAL
C***************************************************
C *** The integer #   IVAL is encoded as follows ***
C *** If    IVAL>0 then VALUE='iiiiiiii'         ***
C *** If    IVAL=0 then VALUE='0'                ***
C *** If    IVAL<0 then VALUE='-iiiiiiii'        ***
      CALL NUM2ALFI (IVAL,VALUE(:9) ,*3,NCHARS) 
C***************************************************
      GOTO 800  !Go to load encoded value into card image.
 3    WRITE(VALUE(2:),'(I14)') IVAL
      MC = 15
      GOTO 999
      ENTRY    ENCAVAL (CHR,NCD,NCH,BVAL,NCVAL,NAME,LAST) 
      MODULE = 'ENCAVAL'
      MC = 19
      VALUE = 'ZERO LENGTH STRING!'
      IF (NCVAL.LE.0) GOTO 999
      NCHARS = NCVAL                       
      MC     = NCVAL
      SEARCH = .TRUE. 
      NWORDS = NCVAL/LENWORD
      WRITE(NWPDV,'(I2)') NWORDS
      WRITE(NCPW ,'(I2)') LENWORD
      FMT = '(' // NWPDV // 'A' // NCPW // ')'
      WRITE(VALUE,FMT) (BVAL(I),I=1,NWORDS)
      IF (ERRPRT) WRITE(6,FMT(:1)//''' ENCAVAL: CHAR VALUE='''//FMT(2:))
     +(BVAL(I),I=1,NWORDS)
      DO 10 I=NCVAL,1,-1
       IF (VALUE(I:I).EQ.CHAR(0)) VALUE(I:I) = ' '
       IF (VALUE(I:I).EQ.' '.AND.SEARCH) THEN
        NCHARS = I-1
       ELSE
        SEARCH = .FALSE.
       ENDIF
 10   CONTINUE
      DO 20 I=1,NCHARS 
       IF (VALUE(I:I).EQ.' ') VALUE(I:I) = '|'
 20   CONTINUE
 800  CONTINUE
      NCHARS = MAX( 1 ,NCHARS)
      DO 810 I=8,1,-1
 810  IF (NAME(I:I).NE.' ') GOTO 820
      I = I-1
 820  NCN = I
      IF (NCH.EQ.0) NCN = 8
      N = 3 + NCN + NCHARS
      IF (NCH+N.GT.79) THEN
       NCD = NCD+1
       NCH = 0
       NCN = 8
       N = 3 + NCN + NCHARS
      ENDIF           
      IF (LAST) THEN
       FMT = '('' '',A,''='',A)'
      ELSE
       FMT = '('' '',A,''='',A,'','')'
      ENDIF
      NVAL= NVAL+1
      WRITE (CHR(NCD)(NCH+1:NCH+N), FMT) NAME(:NCN),VALUE(:NCHARS)
      NCH = NCH+N
      RETURN
      ENTRY    ENCSETU (LENWRD,CPUTYPE)
      IF (LENWRD.GT.0) LENWORD = LENWRD
      CRAY = CPUTYPE
      RETURN
      ENTRY    ENCDB   (ERRPRTU)
      ERRPRT = ERRPRTU
      RETURN
 999  CONTINUE
      PRINT'('' =>'',A,'': Error encoding value='',A)',MODULE,VALUE(:MC)
      STOP '***ENCODE_VALUE!***'
      END
