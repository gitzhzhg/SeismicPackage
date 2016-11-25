      SUBROUTINE DCODE (UFMT,NAMES,NNAM,LOC,*,*)
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
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name: DCODE  (See note A below).
C        Author: John B. Sinton
C  Date Written: 86/07/01
C  Last revised: 96/06/25   K. Goodger
C
C  Purpose: This program has two purposes.
C        1. To decode data read in on lfn=5 (or anyone the user wants)
C           as 80 character card images.
C        2. To read to the next EOF and reset default parameters with
C           entry RDEOF.
C                                                                   
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS                         
C
C  User defined, see program documentation.
C-----------------------------------------------------------------------
C  This program is re-enterable.
C-----------------------------------------------------------------------
C                                 NOTES
C
C A. The entry DCODEBW should be used to setup DCODE for use on the 
C    particular CPU you are using.  DCODEBW must be call before any
C    other call to DCODE, DCODES, or DCODET.  You do not have to make
C    a call to DCODEBW if you are using DCODE on the CRAY as it will 
C    default to the CRAY.  For the VAX use the following statement.
C        CALL DCODEBW(4,.FALSE.)
C    See "CALLING SEQUENCE" heading below for more information.
C B. On the VAX character variables may be put in common with other 
C    types.  If the FIRST variable in the common block is of type 
C    CHARACTER then you must equivalence it to a variable of type
C    BYTE and use the BYTE variable in the call to DCODE. You can also
C    use the VAX %REF function.
C    EXAMPLE 1:
C      COMMOM /A/ CHAR,INT,REAL
C      CHARACTER*12 CHAR
C      BYTE BCHAR(1)
C      EQUIVALENCE (CHAR,BCHAR)
C      ...
C      CALL DCODE (FORMAT,NAMES,3,BCHAR,*,*)
C    EXAMPLE 2:
C      COMMOM /A/ CHAR,INT,REAL
C      CHARACTER*12 CHAR  
C      ...
C      CALL DCODE (FORMAT,NAMES,3,%REF(CHAR),*,*)
C
C 1. DCODE reads character data from a disk file and formats it accord-
C    ing to a user defined format.
C 2. The disk file to read is by default lfn=5.  The user may
C    change this by using the call DCODES (see "arguments for DCODES").
C    On the cray you may make use of the CFT din feature.  An lfn is an 
C    integer value, eg. 29. A din is a left justified Hollerith value 
C    up to 7 characters long, eg. '%DIN123'L.
C 3. All error messages generated and card images read by DCODE are 
C    printed to lfn=6.  The user may turn off the printing of
C    the card images by using the call DCODES (see "arguments for 
C    DCODES").
C 4. DCODES must be call before DCODE if the reading lfn is changed.
C 5. On the CRAY RDEOF MUST be call after the LAST call to DCODE to 
C    position the read lfn after the next EOF, to reset the read lfn 
C    to 5, and to reset the printing of card images.
C 6. You may use DCODE to read from any number of different lfns, or
C    dins.  However, the sequence for calling DCODES, DCODE, and RDEOF 
C    is important.  For example, you wish to read from 5, 30, and 40
C    in that order.  Then the call sequence MUST be:
C          CALL DCODE  (... )   !Reading from 5.
C              ...
C          CALL DCODE  (... )   !Reading from 5 for the last time.
C              ...                                                   
C          CALL RDEOF (*)       !Finish up on 5
C              ...
C          CALL DCODES (30,0)   !Setting LFN=30 and all printing.
C          CALL DCODE  (... )   !Reading from 30.
C              ...    
C          CALL DCODE  (... )   !Reading from 30 for the last time.
C              ...
C          CALL RDEOF (*)       !Finish up on 30.
C              ...
C          CALL DCODES (40,1)   !Setting LFN=40 and error printing.
C          CALL DCODE  (... )   !Reading from 40.
C              ...
C          CALL DCODE  (... )   !Reading from 40 for the last time.
C              ...
C          CALL RDEOF (*)       !Finish up on LFN 40.
C 7. If you wish to turn off the printing of card read by DCODE and 
C    still read from 5 then call DCODES as follows:
C          CALL DCODES (5,1)
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     -----    ------      ----------------
C 28. 96/06/25 Goodger     Put variables ifc and ilc in a save 
c                          statement.
C 27. 93/03/25 Goodger     Remove unused variable "index".  New   
c                          compiler ver 5.04.
C 26. 92/01/22 Ball        Put Howards .CFT ck. for /EOF  and .FOR
C                          ck for *nodata* in source so the source
C                          can remain the same for .CFT and .FOR
C 25. 90/11/07 Sinton      Increased max # of input cards to 200.
C 24. 89/05/03 Sinton      Fixed bug with NAME= as last characters.
C 23. 89/03/03 Sinton      corrected problem with bracket characters in
C                          values.
C 22. 89/02/03 Sinton      Made consistant changes to NCODE and DCODE.
C 21. 89/01/30 Sinton      Increased a few internal sizes (see calling
C                          sequence for UFMT), and added more error 
C                          checking.
C 20. 89/01/04 Sinton      Took out an unused warning message.
C 19. 88/12/05 Sinton      Cleaned up a problem with continuation with
C                          an'='as the last character(see note 7 below)
C                          Attemped to reduce chances of "Invalid
C                          Expansion Character" abort happening.
C 18. 88/10/24 Sinton      Added entry DCODEIX.
C 17. 88/09/26 Sinton      NWIH conversion.
C 16. 88/09/21 Sinton      Added "L" format type and changed "X" to
C                          work on scalars, arrays, and tables.
C 15. 88/08/05 Sinton      Modified to ignore '/' characters in 
C                          format (NCODE uses '/' for linefeed control).
C 14. 88/07/27 Sinton      Added warning message for array counters.
C 13. 88/07/21 Sinton      Added linked array type (see notes below).
C                          Made Cray and VAX programs identical.
C 12. 88/07/05 Baumel      Fix bug involving arrays taking up only one
C                          character on the input card.
C 11. 88/05/03 Sinton      Correct array input so that constructions 
C                          like "NAME=()" or "NAME=" are the same as
C                          scalars, i.e. DCODE will pass the array
C                          without changing anything. 
C 10. 87/10/27 Baumel      Eliminate BACKSPACE when hit EOF - needed for
C                          Cray I/O but not Vax.
C  9. 87/08/24 Sinton      Format types A and H return left justified 
C                          blank filled data.
C  8. 87/05/04 Sinton      Made VAX version of DCODE identical to CRAY
C                          version.
C  7. 87/03/27 Baumel      Fix minor bug involving EOF on LFN .ne. 5.
C  6. 87/01/13 Sinton      Added ability to handle NAME1=,NAME2=v1.
C  5. 87/01/09 Sinton      Changed so the 80th character in an input 
C                          card could be used.
C  3. 86/10/06 Sinton      Increased input limit to 100 cards or
C                          8000 characters.
C  2. 86/07/29 Sinton      Added entry point DCODES (see below).
C  1. 86/07/14 Sinton      Added entry point RDEOF (see below).
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL DCODE (UFMT,NAMES,NNAM,LOC,*,*) Process LFN(5) input.
C        CALL DCODES(LFN,IPRT)                Set LFN and print.
C        CALL DCODET(LFN,IPRT)                Return LFN and print switch.
C        CALL DCODEBW(LENWRD,CPUTYPE)         Set # of bytes/word.
C        CALL DCODEIX(IGNXN)                  Set IGNXN.
C        CALL RDEOF (*)                       Read to eof and reset.
C
C Arguments for DCODE:
C UFMT     - User specified format statement for data convertion. See 
C            format conventions below. [Character variable]
C            Maximum length is 256 characters.
C NAMES    - List of names (NAMELIST) to search through. Each name in
C            this list must be left justified, blank filled, and
C            8 characters long (including blanks).  See name 
C            conventions below. [Character variable]
C NNAM     - Number of items in 'NAMES'. [Integer variable]
C            NNAM should be .le. 100.
C LOC      - Beginning address of the returned data.
C *ERR     - Alternate return 1 is for an error condition.
C *EOF     - Alternate return 2 is for an eof during read.
C
C Arguments for DCODES: ====> This call is OPTIONAL <==== 
C LFN      - Logical file number (or Dataset Identifier) to read from.
C            The default (DCODES is not called) is LFN=5.
C IPRT     - Print switch. The default (DCODES is not called) is 0.
C            ====> If you wish to set only IPRT then set LFN=5!
C            =0, then in addition to error messages, each card DCODE 
C            reads is printed to lfn=6.
C            =1, then only error messages are printed to lfn=6.
C
C Arguments for DCODET: ====> Same as DCODES but this call returns the
C            current values of LFN and IPRT.
C
C Arguments for DCODEBW: ====> Use this call before DCODE! <====
C LENWRD   - Number of bytes/characters per word.  The default is 8,
C            which is valid for the CRAY.  [Integer variable].
C CPUTYPE  - If .TRUE.  then setup for CRAY, this is the default.
C            If .FALSE. then setup for VAX. 
C            [Logical variable].
C
C Arguments for DCODEIX:
C IGNXN    - If .TRUE. then DCODE will ignore any names in the input
C            cards which donot match to the names in NAMES.  There is
C            no message when this happens!
C            If .FALSE. then DECODE will error return if a name in the
C            input cards does not match a name in NAMES.  This is the
C            default if DCODEIX is not called.
C
C Arguments for RDEOF: ====> Must be call after last call to DCODE.
C *        - Alternate return if an error occures.
C            RDEOF resets LFN, IPRT, and IGNXN to their default values.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. FORMAT STATEMENT CONVENTIONS:            EXAMPLE: (A,I,10F2)
C    (     - Must be first non-blank character of format statement.
C    ,     - Format field delimiter. 
C    )     - Must be last non-blank character of format statement. 
C    [...] - Tables.
C    other - Format fields, e.g. A , I , 10F2.
C          - All blanks are ignored.
C
C 2A.FORMAT FIELD CONVENTIONS-Tables:         EXAMPLE: [nT.i]m
C    [...] - All fields within square brackets are treated as a table.
C    n,T,i - Use any format field, as outlined below, within the 
C            table.  Leave off the 'm' parameter after each field!
C    m     - # of array elements in each field of the table.
C          - See note 3.E below concerning table counter.    
C
C 2B.FORMAT FIELD CONVENTIONS-Scalars&Arrays: EXAMPLE:  n T m.i
C    n     - # of names in list using this format. If this is not pre-
C            sent then only one name uses this format.
C    T     - Data type, must be one of the following;
C        F - real returned,
C        I - integer returned,
C        H - left  justified Hollerith returned:
C            blank filled on VAX and null, CHAR(0), filled on CRAY,
C        L - left  justified blank filled Hollerith returned.
C        A - left  justified character returned:
C            blank filled on VAX and null, CHAR(0), filled on CRAY,
C        R - right justified blank filled character returned  only
C            on the CRAY. On the VAX R is the same as A.
C    m     - Maximum # of array elements to return. If this is not pre-
C            sent then the variable is of type SCALAR.
C    .     - Character size delimiter. This is only used for types H, A,
C            and R. 
C    i     - Maximum # of characters to return in each element.
C            If not present then LENWRD characters will be returned!  
C            This format parameter MUST be a multiple of LENWRD. See
C            the CALLING SEQUENCE discription for DCODEBW for a de-
C            finition of LENWRD.
C
C 3. NAME LIST/FORMAT CONVENTIONS AND OTHERS:
C    A. Each name in the NAMELIST must have a corresponding format 
C       field.
C    B. Format fields MUST have the same sequence as the names in the 
C       NAMELIST.
C    C. If a name is not used in the input data then NO data will be 
C       returned for that location.
C    D. The number of array elements decoded is pass to the calling 
C       routine as an integer stored one position in front of the 
C       array.  See example 1 below. 
C       +-------------------------------------------------------+
C       | Note a space must be provided for these values, but   |
C       | they are not included in the count of names, NNAM, or |
C       | the number of format fields in the format statement.  |
C       +-------------------------------------------------------+
C    E. The number of elements in each array of a table is passed to
C       the calling routine as an integer stored one position in 
C       front of the table.  See example 2 below.  This is similar
C       to the array counter (3.D), but the table counter covers
C       all arrays in the table.
C
C 4A.INPUT DATA CONVENTIONS-tables:           EXAMPLE: [I,F]=(1,2),
C    =     - Same as for (4.B).
C    [...] - Delimites the list of names from the table. 
C    (     - Delimiter for start of row within table (not required).
C    )     - Delimiter for end   of row within table (not required).
C            See example 2 below.
C
C 4B.INPUT DATA CONVENTIONS-nontable:         EXAMPLE: I=1,NAM=(abcd,z)
C    =     - Delimiter to separate the name from the value(s) of a
C            scalar(array).
C    ,     - Delimiter to separate values.
C    (     - Delimiter for start of an array (not required).
C    )     - Delimiter for end   of an array (not required).
C    A. Input data to be decoded as scalar(array) elements cannot 
C       contain the characters "="   ","   "("  ")"  " "  OR  "*".
C    B. Anything following a "*" character is a comment and is not 
C       decoded.
C           EX. * This is a comment.
C           EX. I1=(2,3,4,5),H=(the,end) * This is a comment
C    C. The case of a name in the input data must agree character for
C       character with the case of the name in the NAMELIST.
C    D. Character data is returned as is, no conversion to uppercase.
C    E. Continuation to the next card is accomplished by putting a 
C       comma or equals sign as the last non-comment character of a line.
C           EX. i=2,f=12345.234,  * first card, note comma after 4
C           EX. G=(1,2,3,4,5)     * continuation card
C           EX. [G, NAM]=         * continuation card
C    F. Default values within table or array variables are used when 
C       two or more commas are adjacent.
C           EX. G=(,2,3,4,,,7,8)  * array positions 1, 5, and 6 will
C               * not be set by DCODE.
C
C 5. EXAMPLE SETUP 1:        
C    A. The user call routine should have the following statements.
C       On the CRAY, character and non-character data types may not
C       be mixed in the common block.  On the VAX mixing data types
C       is OK.
C           COMMON /DC/ NI1,I1(5),NI2,I2(5),R,NH,H(9*160),A
C           CHARACTER UFMT*24,NAMES(5)*8
C           UFMT = '(2I5,F,H9.160,A.8)'
C           NAMES(1) = 'I1      '
C           NAMES(2) = 'I2      '
C           NAMES(3) = 'R       '
C           NAMES(4) = 'H       '
C           NAMES(5) = 'A       '
C           CALL DCODE(UFMT,NAMES,5,I1,*999,*1000)
C    B. The input data might look like.
C           I1=(1,2,3,4),F=5.5,H=(ABCDEFGHIJK,123456,----///),A=A|AAA
C 6. EXAMPLE SETUP 2:        TABLES                   
C    A. The user call routine should have the following statements.
C           COMMON /DC/ N,I1(5),I2(5),F2(5),H(40),I3
C           CHARACTER UFMT*24,NAMES(5)*8 
C           UFMT = '([2I,F,H.64]5,I)'
C           NAMES(1) = 'I1      '
C           NAMES(2) = 'I2      '
C           NAMES(3) = 'F2      '
C           NAMES(4) = 'H       '
C           NAMES(5) = 'I3      '
C           CALL DCODE(UFMT,NAMES,5,N ,*999,*1000)
C    B. The input data might look like.
C           I3=100,
C           [I1, F2, H]=     *Note that '=' is continued on next line.
C           (1000 ,100.555 ,TEST1),
C           (2000 ,200.555 ,TEST2),
C           (3000 ,300.555 ,TEST3)
C       DCODE will set the arrays I1, F2, and H to,
C           I1(1) = 1000    F2(1) = 100.555  H(1) = 'TEST1'
C           I1(2) = 2000    F2(2) = 200.555  H(2) = 'TEST2'
C           I1(3) = 3000    F3(3) = 300.555  H(3) = 'TEST3'.
C       The table counter N=3.  Note that array I2 will not be modified
C       since it was not included in the list of [I1, F2, H].
C
C 7. There are two continuation characters: , and =.  However, they are
C    not treated equivalently.  A ',' is more general as a continuation
C    than an '='.  The ',' should be used to continue anywhere it 
C    naturally occures.  The '=' should ONLY be used to continue table
C    input.  If an '=' is the last character in a card, DCODE must 
C    check the next card to decide what must be done.  If this card is
C    not a comment card and the first nonblank character is not a '(',
C    DCODE assumes that this card belongs to another DCODE call.
C    Under this condition DCODE will backspace the file one record and 
C    stop reading.
C    NOTE: if the input device can not be backspaced bad things will 
C          happen!  This should never happen unless does something 
C          unnecessary.
C
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  DCODE
C Functions    : none
C Entrys       :
C  RDEOF   DCODEDB DCODES  DCODET  DCODEBW
C Common blocks: none
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  DECSETU  CFILL    RBLCH    FAOCH    DECFMTA  DECFMTB  FFOCH    DECFMT  
C  FLOCH    DECVAL  
C Functions:
C  CTUC
C  Note- All routines called are CPS primitives.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C\END DOC
C
      PARAMETER (NCRDSP = 200,  !# of cards in I/O buffer.
     +           NCHRSP = NCRDSP*80, !# of characters in I/O buffer.
     +           NPARMS = 100,  !Max # of parameter names (NNAM).
     +           NCHRSF = 256,  !Max # of characters in format.
     +           NFIELDS= 1000) !Max # of fields in the I/O buffer.
      DIMENSION ICP1(NFIELDS),ICP2(NFIELDS),ICP3(NFIELDS),ICP4(NFIELDS)
      DIMENSION MAXL(NPARMS),TYPE(NPARMS),LENS(NPARMS),SNVA(NPARMS),
     +          SARR(NPARMS)
      DIMENSION LOC(*)
      DIMENSION NI(4,NPARMS),VI(2,NPARMS),LNAMES(NPARMS)
      CHARACTER*(*) UFMT,NAMES(NNAM)
      CHARACTER FMT*256,CD*256,T*1,INPUT*16000,TYPE*1,NULSTR*8
      CHARACTER CIOUNIT*8
      CHARACTER*1 CTUC
      INTEGER VI,LENS,NI ,SNVA,SARR ,IOUNIT
      LOGICAL EOF ,ERRPRT ,TABLE ,LAST ,CRAY ,CPUTYPE ,ERRPRTU
      LOGICAL IGNXNU ,IGNXN
      EXTERNAL CTUC
      SAVE LFND,IPRTD,LFN,IPRT,LEN4R8,ERRPRT,CRAY,IOUNIT,CIOUNIT
      SAVE IFC,ILC
      DATA LFND,IPRTD,LFN,IPRT,LEN4R8/ 5, 0, 5, 0, 8/
      DATA ERRPRT /.FALSE./ ,CRAY/.TRUE./ ,CIOUNIT/'%DCODE0'/
      DATA IGNXN /.FALSE./

      IF (LEN(UFMT).GT.NCHRSF) GOTO 915
      IF (NNAM.GT.NPARMS)      GOTO 919
      IF (CRAY) THEN
       T = CHAR(0)
      ELSE
       T = ' '
      ENDIF
      DO 3 I=1,8
 3    NULSTR(I:I) = T  

C
C*******************************************
C ****DECODE THE FORMAT IN UFMT         ****
      CALL DECSETU (LEN4R8,CRAY)
      CALL CFILL (FMT,' ')
      CALL RBLCH (UFMT,FMT,NFC)       
      IF (ERRPRT .AND. LEN(UFMT).GT.0 .and. NFC.GT.0) THEN
       PRINT*,'=>DCODE: User format statement: ',UFMT(:LEN(UFMT))
       PRINT*,'=>DCODE: Conditioned format   : ',FMT(:NFC)
       PRINT*,'=>DCODE: Number of names      : ',NNAM
      ELSE IF (LEN(UFMT).LE.0) THEN
       GOTO 914
      ELSE IF (NFC.LE.0) THEN
       GOTO 914
      ENDIF
      IF (FMT(1:1).NE.'(' .OR. FMT(NFC:NFC).NE.')') GOTO 913
      CALL FAOCH (FMT(2:NFC-1),ICP1,ICP2,J,',')
      IF (J.GT.NFIELDS) GOTO 917
      DO 10 I=1,J
       ICP1(I) = ICP1(I)+1
       ICP2(I) = ICP2(I)+1
 10   CONTINUE
      NN   = 0
      TABLE= .FALSE.
      LAST = .FALSE.
      IPNTR= 1
      DO 30 IT=1,J
       IF      (FMT(ICP1(IT):ICP1(IT)).EQ.'[') THEN 
        IPNTRS = IPNTR
        IPNTR  = IPNTR+1                                   
        CALL DECFMTA (FMT(ICP1(IT):NFC),NTAB,NFIELD,*910)
        CALL DECFMTB (FMT(ICP1(IT):ICP2(IT)),CD,NCFIELD)
        TABLE = .TRUE.
        LAST  = .FALSE.
        IF (NFIELD.EQ.1) LAST = .TRUE.
       ELSE IF (TABLE) THEN
        CALL FFOCH (FMT(ICP1(IT):ICP2(IT)),NLB,']')         
        CALL DECFMTB (FMT(ICP1(IT):ICP2(IT)),CD,NCFIELD) 
        IF (NLB.NE.0) LAST = .TRUE.
       ELSE             
        IF (FMT(ICP2(IT):ICP2(IT)).EQ.'/') ICP2(IT) = ICP2(IT)-1
        CD = FMT(ICP1(IT):ICP2(IT))
        NCFIELD = ICP2(IT)-ICP1(IT)+1
       ENDIF
       CALL DECFMT(CD(:NCFIELD),N,T,M,LS,*906)
       DO 20 IN=1,N
        NN = NN+1
        IF (NN.GT.NPARMS) GOTO 918
        TYPE(NN) = T
        LENS(NN) = LS
        MAXL(NN) = M
        IF (TABLE) THEN
         SNVA(NN) = IPNTRS
         SARR(NN) = IPNTR
         IPNTR    = IPNTR + (LENS(NN)/LEN4R8)*MAXL(NN) 
        ELSE
         SNVA(NN) = IPNTR
         SARR(NN) = IPNTR
         IPNTR    = IPNTR + (LENS(NN)/LEN4R8)*MAXL(NN) 
         IF (MAXL(NN).GT.1) THEN
          SARR(NN) = SARR(NN)+1
          IPNTR = IPNTR+1
         ENDIF
        ENDIF
        IF (ERRPRT) THEN
         IF (NCFIELD.GT.0) THEN
          PRINT'('' =>DCODE: NN='',I4,1X,A1,1X,2I3,2I6,''| '',A)',
     +    NN,TYPE(NN),LENS(NN),MAXL(NN),SNVA(NN),SARR(NN),CD(:NCFIELD)
         ELSE
          PRINT*,'=>DCODE: NN=',NN,' Error, zero length format field!'
         ENDIF
        ENDIF
 20    CONTINUE 
       IF (LAST) TABLE = .FALSE.
 30   CONTINUE
      IF (NN.NE.NNAM) GOTO 908
C*******************************************
C
C                     
C***********************************************************************
C ****READ IN CARDS, FILLING CHARACTER BUFFER 'INPUT', UNTIL AN EOF ****
C ****OR A CARD WITH NO COMMA AS THE LAST NON-COMMENT CHARACTER IS  ****
C ****ENCOUNTERED.                                                  ****
      NC = 0     
      CALL CFILL (INPUT,' ')
      CALL CFILL (CD   ,' ')
      DO 50 I=1,NCRDSP
C*******************************************
C ****Read data into temporary buffer CD.  *
       CALL CFILL (CD(:80),' ')
       EOF = .TRUE.
       READ (LFN,'(A80)',END=40,ERR=901) CD(:80)
       IF(cd(1:8).EQ.'*nodata*') RETURN   !for the .FOR
       IF(CD(1:4).EQ.'/EOF') GOTO 40      !for the .CFT
       IF (IPRT.EQ.0..OR.ERRPRT) THEN
        CALL FLNBC (CD(:80),II,32)
        PRINT *,' ',CD(:II)
       ENDIF
       EOF = .FALSE.
 40    CONTINUE
C*******************************************
C
C*******************************************
C ****Check for comments by looking for an *
C ****'*' character.                       *
       CALL FFOCH (CD(:80),NCOM,'*') !find first occurance of '*'.
       IF      (NCOM.EQ.0) THEN 
        NCOM = 80          ! Did not find an * character(comment).
       ELSE IF (NCOM.EQ.1) THEN     
        NIC = 0
        GOTO 45            ! Entire line is a comment, ignore line.
       ELSE
        NCOM = NCOM-1      ! Found an * character, ignore comment.
       ENDIF
       CALL RBLCH (CD(:NCOM),INPUT(1+NC:),NIC)  !Remove blank characters.
       IF (NC+NIC.GT.NCHRSP) GOTO 916
 45    CONTINUE
C*******************************************
C
C*******************************************
C ****If check state of data if an EOF was *
C ****encountered.                         *
       IF (EOF) THEN
        IF ((IPRT.EQ.0.OR.ERRPRT) .AND.
     +      INPUT(NC:NC).NE.'='   ) PRINT *,'=>DCODE: EOF AT CARD# ',I
        IF (NC.NE.0.AND.INPUT(NC:NC).EQ.',') THEN
         IF (NIC.EQ.0) THEN
          GOTO 905  ! Premature EOF.
         ELSE 
          NC = NC+NIC
         ENDIF
        ENDIF
        GOTO 100
       ENDIF
C*******************************************
C
C*******************************************
C ****Check state of data if this is a con-*
C ****tinuation card.
C ****Note, an error will occure if the    *
C ****input device is not backspaceable.   *
       IF (NC.NE.0) THEN 
        IF (INPUT(NC:NC).EQ.'='.AND.INPUT(NC+1:NC+1).NE.'(') THEN
         IF (ERRPRT) PRINT*,'=>DCODE: EOD state =, and new DCODE! ',I
         BACKSPACE LFN     ! Encountered data for new DCODE call.
         CALL CFILL (INPUT(NC+1:NC+NIC),' ')
         GOTO 100
        ENDIF
       ENDIF
C*******************************************
C
C*******************************************
C ****Added new characters to array INPUT. *
       NC = NC+NIC
       IF (NC.NE.0) THEN
        IF (INPUT(NC:NC).NE.',' .AND. INPUT(NC:NC).NE.'=') GOTO 100
       ENDIF
C*******************************************
 50   CONTINUE
      IF (I.GT.NCRDSP) THEN
       MCRDSP = NCRDSP
C23456789*123456789*123456789*123456789*123456789*123456789*123456789*12
       PRINT*,'=>DCODE: Warning, more than ',MCRDSP,' cards input.  The 
     +extras will be skipped!'
      ENDIF
 100  CONTINUE          
      IF      (ERRPRT.AND.NC.GT.0) THEN
       PRINT*,'=>DCODE: input>',input(:nc),'<'
      ELSE IF (ERRPRT) THEN
       PRINT*,'=>DCODE: No data read from input device, NC=',NC
      ENDIF
C***********************************************************************
C
C
C*******************************************
C ****PARSE THE BUFFER 'INPUT' LOOKING  ****
C ****FOR "=" AND "," TO DELINEATE VARI-****
C ****ABLE NAMES AND PARAMETER VALUES   ****
      IF (NC.EQ.0) GOTO 900
      CALL FAOCH (INPUT(:NC),ICP1,ICP2,JEQ,'=')
      JEQ = JEQ-1    !Number of equal signs in input string.
      JN = 0         !Number of names.
      DO 120 I=1,JEQ
       IFN = ICP1(I)
       ILN = ICP2(I)
       IFV = ICP1(I+1)
       ILV = ICP2(I+1)  
       CALL FLOCH (INPUT(IFN:ILN),INRB,'[')
       CALL FFOCH (INPUT(IFV:ILV),IVRB,'[')
       IF (INRB.NE.0) THEN
        CALL FLOCH (INPUT(IFN:ILN),INLB,']') 
        IF (INLB.EQ.0) GOTO 911  !No "]" for "[", yuk! 
        INLB = (IFN-1)+INLB
C**********************************************************
C *** If the input string looks like "[...]," ie. not   ***
C *** like "[...]=", then DONOT treat this as a table!  ***
        IF (INPUT(INLB+1:INLB+1).NE.'=') INRB = 0
C**********************************************************
       ENDIF
       IF (IVRB.EQ.0) THEN
        IEND = ILV
       ELSE
        CALL FLOCH (INPUT(IFV:ILV),IVLB,']')
        IVLB = MAX( 1, IVLB )
        IVLB = (IFV-1)+IVLB
C**********************************************************
C *** If the input string looks like "[...]," ie. not   ***
C *** like "[...]=", then DONOT treat this as a table!  ***
        IF (INPUT(IVLB+1:IVLB+1).NE.'=') THEN    
         IEND = ILV
        ELSE
         CALL FLOCH (INPUT(IFV:ILV),IEND,'[')  !Case "N1=[value],[T1,T2]="
         IEND = (IFV-1)+IEND
        ENDIF
C**********************************************************
       ENDIF
C**********************************************************
C *** If this is not the last keyword/falue combo then  ***
C *** look for the comma in the value.  For example     ***
C *** given the input string ...KEYWORD=JJJJJ,NEXT=...  ***
C *** the name="...KEYWORD" and the value="JJJJJ,NEXT". ***
C *** The comma in the value marks the end of value.    ***
       IF (I.EQ.JEQ) THEN
        ICOM = 0
       ELSE
        CALL FLOCH (INPUT(IFV:IEND),ICOM,',')  !Search for limiting comma.
       ENDIF
 105   CONTINUE
       IF (INRB.NE.0) THEN
        INRB = (IFN-1)+INRB
        CALL FLOCH (INPUT(IFN:ILN),INLB,']') 
        INLB = (IFN-1)+INLB
        CALL FAOCH (INPUT(INRB+1:INLB-1),ICP3,ICP4,JCM,',')
        IF (JCM.EQ.0) THEN !One name, ie. "[NAME]".
         JN = JN+1
         NI(1,JN) = INRB+1  !First character after "["
         NI(2,JN) = INLB-1  !Character before "]"
         NI(3,JN) = 1                 
         NI(4,JN) = 1
         VI(1,JN) = IFV                
         IF (ICOM.EQ.0) THEN
          VI(2,JN) = IEND
         ELSE
          VI(2,JN) = (IFV-1)+(ICOM-1)
         ENDIF
        ELSE               !Multiple names if you get to here.
         DO 115 J=1,JCM
          JN = JN+1
          NI(1,JN) = INRB+ICP3(J) !First character of name.
          NI(2,JN) = INRB+ICP4(J) !Last character of name.
          NI(3,JN) = J 
          NI(4,JN) = JCM
          VI(1,JN) = IFV                
          IF (ICOM.EQ.0) THEN
           VI(2,JN) = IEND
          ELSE
           VI(2,JN) = (IFV-1)+(ICOM-1)
          ENDIF
 115     CONTINUE 
        ENDIF        
       ELSE 
        CALL FLOCH (INPUT(IFN:ILN),ICO,',') 
        JN = JN+1                
        NI(3,JN) = 1
        NI(4,JN) = 1
        IF (ICO.EQ.0) THEN
         NI(1,JN) = ICP1(I)  !First character of input string.
         NI(2,JN) = ICP2(I)  !Character before "=".
        ELSE
         NI(1,JN) = IFN+ICO  !First character after ",".
         NI(2,JN) = ILN      !Character before "=".
        ENDIF
        VI(1,JN) = IFV
        IF (ICOM.EQ.0) THEN
         VI(2,JN) = IEND
        ELSE
         VI(2,JN) = (IFV-1)+(ICOM-1)
        ENDIF
       ENDIF
 120  CONTINUE
      IF (ERRPRT) THEN
       PRINT*,'=>DCODE: JN ',JN,', names and values follow.'
       PRINT*,'=>DCODE:  I   -NAME-  NI1 NI2 NI3 NI4 --VALUE-->'
       DO 130 I=1,JN 
        IF (NI(2,I).GE.NI(1,I) .AND. VI(2,I).GE.VI(2,I)) THEN
         PRINT'('' =>DCODE: '',I3,1X,A8,4I4,1X,A)',
     +   I,INPUT(NI(1,I):NI(2,I)),(NI(J,I),J=1,4),
     +     INPUT(VI(1,I):VI(2,I))
        ELSE
         PRINT'('' =>DCODE: '',I3,1X,A8,1X,4I4)',
     +   I,'ZLS-ERR:',NI(1,I),NI(2,I),VI(1,I),VI(2,I)
        ENDIF
 130   CONTINUE
      ENDIF
C*******************************************
C

C
C*******************************************
C ****PARSE THE LIST 'NAMES' TO ELIMIN- ****
C ****ATE TRAILING BLANKS               ****
      IF (ERRPRT) PRINT*,'=>DCODE: NNAM=',NNAM
      DO 150 I=1,NNAM
       CALL FFOCH (NAMES(I),LNAMES(I),' ')
       IF (LNAMES(I).NE.0) THEN
        LNAMES(I) = LNAMES(I)-1
       ELSE
        LNAMES(I) = LEN(NAMES(I))
       ENDIF
       IF (ERRPRT) 
     + PRINT*,'=>DCODE: Name ',NAMES(I),I,' has length of ',LNAMES(I)
 150  CONTINUE
C*******************************************
C                           

C
C*******************************************
C ****DECODE THE NAMELIST HELD IN BUFFER****
C ****'INPUT' BY SEARCHING FOR A ONE TO ****
C ****ONE MATCH BETWEEN 'NAMES' AND THE ****
C ****NAMES IN 'INPUT'.                 ****
      DO 250 I=1,JN
       NJ = 0                                            
       DO 201 J=1,NNAM
        IF (INPUT(NI(1,I):NI(2,I)) .EQ. NAMES(J)(:LNAMES(J))) NJ = J
 201   CONTINUE
       IF      (NJ.EQ.0 .AND. IGNXN) THEN
        GOTO 250
       ELSE IF (NJ.EQ.0) THEN
        GOTO 902
       ENDIF 
       IF (ERRPRT.AND.LNAMES(NJ).NE.0) THEN
        PRINT*,'DCODE: before decoding nj=',nj,
     +  ' >',names(nj)(:lnames(nj)),'<',maxl(nj),snva(nj),sarr(nj)
       ELSE IF (ERRPRT) THEN
        PRINT*,'DCODE: before decoding nj=',nj,
     +  ' Length of name is zero!'
       ENDIF
       IF (MAXL(NJ).EQ.1) THEN
C
C **** Check if this is a zero length string.
C **** VI(2,i)<VI(1,I) when input looks like NAME1=,NAME2=value1.
        if (VI(2,I).lt.VI(1,I)) goto 222  ! No value, skip parameter.
C        
C **** If-block for scalar variable input.
C                                 
        IF (CTUC(TYPE(NJ)).NE.'A' .AND. 
     +      CTUC(TYPE(NJ)).NE.'L' .AND.
     +      CTUC(TYPE(NJ)).NE.'R' .AND. 
     +      CTUC(TYPE(NJ)).NE.'H') THEN
         CALL DECVAL(INPUT(VI(1,I):VI(2,I)),TYPE(NJ),LOC(SARR(NJ)),
     +   *903)
        ELSE
         IF (VI(2,I)-VI(1,I)+1.GT.LENS(NJ)) GOTO 907 !Input string too long.
C***********************************************************
C ***    Fill out character variable with input string.  ***
         IWC = 0
         DO 220 K=VI(1,I),VI(2,I),LEN4R8
          KE = MIN0(VI(2,I),K+LEN4R8-1)
          J  = SARR(NJ)+IWC
          CALL DECVAL(INPUT(K:KE),TYPE(NJ),LOC(J),*903)
          IWC= IWC+1
 220     CONTINUE
C ***                                                    ***
C***********************************************************
C ***    Fill out character variable with blanks.        ***
         DO 221 K=IWC*LEN4R8+1,LENS(NJ),LEN4R8
          J  = SARR(NJ)+IWC
          CALL DECVAL(NULSTR,TYPE(NJ),LOC(J),*903)
          IWC= IWC+1
 221     CONTINUE
C***********************************************************
        ENDIF
 222    CONTINUE
C
C **** End scalar input.
C               
       ELSE
C
C **** If-block for array variable input.
C
        IFC = VI(1,I)+1
        ILC = VI(2,I)-1
        IF (INPUT(VI(1,I):VI(1,I)).NE.'(') IFC = VI(1,I)
        IF (INPUT(VI(2,I):VI(2,I)).NE.')') ILC = VI(2,I) 
C        
C **** Check if this is a zero length string.
C **** ILC.lt.IFC when input looks like NAME=(), or NAME= .
C **** Thus, take the defaults for entire array!
        IF (ILC.LT.IFC) goto 240
C
        CALL FAOCH (INPUT(IFC:ILC),ICP1,ICP2,NV,',')
        IF (NV.EQ.1) THEN
         ICP1(1) = 1
         ICP2(2) = ILC-IFC+1
        ENDIF
C
C **** Loop over NV input values.
        NVAL = 0
        DO 230 K=NI(3,I),NV,NI(4,I)
         NVAL = NVAL+1
         JFC = (IFC-1)+ICP1(K)
         JLC = (IFC-1)+ICP2(K)
         IF (INPUT(JFC:JFC).EQ.'(') JFC = JFC+1
         IF (INPUT(JLC:JLC).EQ.')') JLC = JLC-1
C
C **** Check if this is a zero length string.
C **** JLC<JFC when input looks like NAME=(,,,value1,value2).
C **** for the 3 adjacent comas.
         IF (JLC.LT.JFC) GOTO 230                
C
         IF (CTUC(TYPE(NJ)).NE.'A' .AND.           
     +       CTUC(TYPE(NJ)).NE.'L' .AND. 
     +       CTUC(TYPE(NJ)).NE.'R' .AND. 
     +       CTUC(TYPE(NJ)).NE.'H') THEN
          J = SARR(NJ)+NVAL-1    
          CALL DECVAL (INPUT(JFC:JLC),TYPE(NJ),LOC(J),*904)
         ELSE
          IF (JLC-JFC+1.GT.LENS(NJ)) GOTO 907
          IWC = 0
          NWCOFF = (NVAL-1)*LENS(NJ)/LEN4R8
C****************************************************************
C ***     Fill out character array element with input string. *** 
          DO 225 L=JFC,JLC,LEN4R8
           LE = MIN0(JLC,L+LEN4R8-1)
           J  = SARR(NJ)+IWC+NWCOFF
           CALL DECVAL(INPUT(L:LE),TYPE(NJ),LOC(J),*903)
           IWC= IWC+1
 225      CONTINUE                     
C ***                                                         ***
C****************************************************************
C ***     Fill out character array element with ' ' or char(0)***
          DO 226 L=IWC*LEN4R8+1,LENS(NJ),LEN4R8
           J  = SARR(NJ)+IWC+NWCOFF
           CALL DECVAL(NULSTR,TYPE(NJ),LOC(J),*903)
           IWC= IWC+1
 226      CONTINUE
C****************************************************************
         ENDIF
 230    CONTINUE
        IF (ERRPRT)                        
     +  PRINT*,'DCODE: after decoding array, NJ=',
     +  NJ,NVAL,LOC(SNVA(NJ)),SNVA(NJ)
        IF (LOC(SNVA(NJ)).GT.MAXL(NJ)) THEN
         PRINT*,'=>DCODE:           ***************************** '
         PRINT*,'=>DCODE: Warning, the old array counter for field ',
     +   NJ,' is ignored because it is >',MAXL(NJ),SNVA(NJ)
         PRINT*,'=>DCODE: This should not happen! Check the array ',
     +   'counter initialization.'
         PRINT'(A,A9,1X,I22)',' =>DCODE: Old array counter value=',
     +   LOC(SNVA(NJ)),LOC(SNVA(NJ))
         PRINT*,'=>DCODE: Offending field name=', NAMES(NJ)
         PRINT*,'=>DCODE: Number of fields=',nnam
         PRINT*,'=>DCODE: names in list are:'
         DO 232 K=1,NNAM
          PRINT*,'=>DCODE: ',K,' ',NAMES(K),' ',LNAMES(K)
 232     CONTINUE
         PRINT*,'=>DCODE:           ***************************** '
         LOC(SNVA(NJ)) = 0
        ENDIF
        LOC(SNVA(NJ)) = MAX( NVAL ,LOC(SNVA(NJ)) )
 240    CONTINUE
C
C **** End array input.
C
       ENDIF
 250  CONTINUE
 900  CONTINUE
C*******************************************
C
      IF (EOF) THEN
       IF (CRAY) BACKSPACE LFN  
       IF (INPUT(NC:NC).NE.'=') RETURN 2
      ENDIF                 
      RETURN
C
C*******************************************
C ****READ TO EOF & reset defaults      ****
      ENTRY RDEOF(*)
      I = 0
 1    CONTINUE            
       I = I+1
       READ (LFN,'(A80)',END=2,ERR=909) CD(:80)
       IF(CD(1:4).EQ.'/EOF') GOTO 2      !for the .CFT
       IF (IPRT.EQ.0..OR.ERRPRT) THEN
        CALL FLNBC (CD(:80),II,32)
        PRINT *,' ',CD(:II)
       ENDIF
      GOTO 1
 2    CONTINUE
      IF (CRAY .AND. LFN.EQ.IOUNIT) REWIND (LFN) 
      LFN = LFND
      IPRT= IPRTD
      IGNXN = .FALSE.
      RETURN
C
C*******************************************
C
C*******************************************
C ****This entry point is like DCODESS  ****
C ****its extra argument can be used to ****
C ****turn on some debugging print      ****
C ****statements.                       ****
      ENTRY DCODEDB(LFNU,IPRTU,ERRPRTU)  
      ERRPRT = ERRPRTU
C
C*******************************************
C
C*******************************************
C ****Set reading LFN and print switch  ****
      ENTRY DCODES (LFNU,IPRTU) 
      IF (CRAY .AND. LFNU.EQ.-1) then 
       CIOUNIT(8:) = CHAR(0)
       READ(CIOUNIT,'(A8)') IOUNIT
       LFN = IOUNIT
      ELSE                 
       LFN = LFNU
      ENDIF                
      IPRT = IPRTU
      IF (IPRT.EQ.0) then
       IF (LFN.GE.0.AND.LFN.LE.99) THEN
        PRINT *,'=>DCODES: DCODE set to read from=',LFN
       ELSE
        PRINT '('' =>DCODES: DCODE set to read from='',a7)',LFN
       ENDIF
      ENDIF
      IF (CRAY .AND. LFN.EQ.IOUNIT) REWIND (LFN)
      RETURN
C*************************************************** 
C** Tell user current settings of LFN and IPRT *****
      ENTRY DCODET (LFNU,IPRTU)
      LFNU = LFN
      IPRTU = IPRT
      RETURN
C***************************************************
C
C*******************************************
C ****Set number of bytes per word.     ****
      ENTRY DCODEBW (LENWRD,CPUTYPE)
      CRAY = CPUTYPE
      IF (LENWRD.GT.0) LEN4R8 = LENWRD
      IF (ERRPRT) THEN
       PRINT*,'=>DCODEBW: DCODE set to use ',LEN4R8,' bytes/word.'
       PRINT*,'=>DCODEBW: DCODE set to use CRAY cpu=',CRAY
      ENDIF
      RETURN
C*******************************************
C
C*******************************************
C ****Set DCODE to ignore extra names in****
C ****input cards.                      ****
      ENTRY DCODEIX (IGNXNU)
      IGNXN = IGNXNU
      RETURN
C*******************************************
 901  PRINT*,'=>DCODE: read err at card #=',I
      PRINT*,'=>DCODE: CD=',CD(:80)
      RETURN 1
 902  PRINT*,'=>DCODE: name =',INPUT(NI(1,I):NI(2,I)),
     +' does not exist in list.'
      PRINT*,'=>DCODE: names in list are:'
      DO 920 I=1,NNAM
       PRINT*,'=>DCODE: ',I,' ',NAMES(I)(:LNAMES(I)),' ',LNAMES(I)
 920  CONTINUE
      RETURN 1
 903  PRINT*,'=>DCODE: ERR in decoding parameter, name=',
     +NAMES(NJ),' value=',INPUT(VI(1,I):VI(2,I))
      RETURN 1
 904  PRINT*,'=>DCODE: ERR in decoding array, name=',NAMES(NJ),
     +' NV=',NV,' value=',INPUT(JFC:JLC)
      RETURN 1
 905  PRINT*,'=>DCODE: ERR in reading continuation, EOF encountered.'
      RETURN 1
 906  PRINT*,'=>DCODE: ERR in decoding format.'
      RETURN 1
 907  PRINT*,'=>DCODE: ERR the input character string is too long.'
      PRINT*,'=>DCODE: Working on parameter # ',NJ,' name=',
     +NAMES(NJ)(:LNAMES(NJ)),' with a maximum length of ',LENS(NJ)
      RETURN 1
 908  PRINT*,'=>DCODE: ERR Format has different number'
     +,' of format fields then number of names in NAMELIST.'
      PRINT*,'=>DCODE: # of names in NAMELIST is ',NNAM
      PRINT*,'=>DCODE: # of format fields is     ',NN
      GOTO 914
 909  PRINT*,'=>RDEOF: read err at card #=',I
      PRINT*,'=>RDEOF: CD=',CD(:80)
      RETURN 1
 910  PRINT*,'=>DCODE: Error in format, found "[" but no "]".' 
      GOTO 914
 911  PRINT*,'=>DCODE: Error in input data format, "[" and "]" not match
     +ed.'
      PRINT*,'=>DCODE: Offensive string is ',input(ifc:ilc)
      RETURN 1
 912  PRINT*,'=>DCODE: Error in format of input, no "," before "["!'
      PRINT*,'=>DCODE: Offensive string is ',input(ifc:ilc)
      RETURN 1
 913  PRINT*,'=>DCODE: Error, Your format must start with a "(" and end'
     +,' with a ")".'
 914  CONTINUE
      IF      (LEN(UFMT).LE.0) THEN
       PRINT*,'=>DCODE: Error, your format is 0 length, thats a no-no!'
      ELSE IF (NFC.GT.0)       THEN
       PRINT*,'=>DCODE: Your format=',FMT(:NFC)
      ELSE
       PRINT*,'=>DCODE: Your format is all blanks??!! Is something',
     + 'wrong?!?!'
      ENDIF
      RETURN 1
 915  MCHRSF = NCHRSF
      PRINT*,'=>DCODE: Your format is longer than ',MCHRSF,
     +' characters.'
      RETURN 1
 916  MCHRSP = NCHRSP
      PRINT*,'=>DCODE: Your input data is longer than ',MCHRSP,
     +' characters in this call to DCODE.'
      RETURN 1
 917  MFIELDS = NFIELDS
      PRINT*,'=>DCODE: You have more than ',MFIELDS,' commas in your ',
     +'format.'
      RETURN 1
 918  MPARMS = NPARMS
      PRINT*,'=>DCODE: You have more than ',MPARMS,' fields in your ',
     +'format.'
      RETURN 1
 919  MPARMS = NPARMS
      PRINT*,'=>DCODE: You are trying to use more than ',MPARMS,' ',
     +'parameters.'
      RETURN 1
      END
