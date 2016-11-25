      SUBROUTINE NCODE (UFMT,NAMES,NNAM,LOC,IPN,NCARDS,*)
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
C  Process name: NCODE (See note A below).
C        Author: John B. Sinton
C  Last revised: 90/10/23
C
C  Purpose: This program has two purposes.
C        1. To encode data into 80 character card images.
C        2. To write the card images to the CFE work file (using CID_). 
C-----------------------------------------------------------------------
C                           INPUT PARAMETERS
C  NONE
C-----------------------------------------------------------------------
C  This program is re-enterable.
C-----------------------------------------------------------------------
C                                 NOTES
C
C A. The entry NCODEBW should be used to setup NCODE for use on the 
C    particular CPU you are using.  NCODEBW must be call before any
C    other call to NCODE, NCODES, or NCODET.  You do not have to make
C    a call to NCODEBW if you are using NCODE on the CRAY as it will 
C    default to the CRAY.  For the VAX use the following statement.
C        CALL NCODEBW(4,.FALSE.)
C    See "CALLING SEQUENCE" heading below for more information.
C B. On the VAX character variables may be put in common with other 
C    types.  If the FIRST variable in the common block is of type 
C    CHARACTER then you must equivalence it to a variable of type
C    BYTE and use the BYTE variable in the call to NCODE. You can also
C    use the VAX %REF function.
C    EXAMPLE 1:                
C      COMMOM /A/ CHAR,INT,REAL
C      CHARACTER*12 CHAR
C      BYTE BCHAR(1)
C      EQUIVALENCE (CHAR,BCHAR)
C      ...
C      CALL NCODE (FORMAT,NAMES,3,BCHAR,*,*)
C    EXAMPLE 2:
C      COMMOM /A/ CHAR,INT,REAL
C      CHARACTER*12 CHAR  
C      ...
C      CALL NCODE (FORMAT,NAMES,3,%REF(CHAR),*,*)
C
C 1. NCODE is an opposite operation to DCODE, with the following excep-
C    tion.
C    a. Embedded blanks in character strings will be replaced by the 
C       '|' character.  DCODE recognizes this character as a blank. 

C 2A.For the VAX:
C    For CFE processing, use IPN.ne.0 in a call to NCODE after the 
C    the call to CIP_.
C    This generally means that you can use the Cray
C    fortran statements that control NCODE for NCODE on the VAX.  You
C    will have to be carefull to specify the length of character 
C    parameters, because the VAX word size is 4 bytes.  For example, if
C    your Cray format is (H,H.16) then to use this on the VAX change 
C    it to (H.8,H.16).
C 2B.For the CRAY:
C    For history processing, IPN is the process number used in the set
C    up call to the process.
C     In the main program you have,
C      CALL BYTES (2,0,*9) 
C     In the CPS process BYTES you have,
C      ENTRY BYTES (IPN,IS,*)
C      CALL NCODE (UFMT,NAMES,NNAM,loc ,IPN,*9)
C 3. The card images are always placed in the common block /NCODEA/.
C    To access this common block you should put the following statements
C    in your program.
C      COMMON /NCODEA/ CARDS(100)
C      CHARACTER*80 CARDS
C    The number of none blank cards is returned in NCARDS in the call.
C 4. If no values are set in an array, then NCODE will skip it.  Each 
C    array is preceeded by a count of the number of elements that are
C    to be used.  If this count=0, then the parameter=(value,,) will 
C    not appear in the card images.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date      Author     Description
C 10. 90/10/23: RD Peterson Don't allow zero subscript on INPUT(NCD) if
C                          an array is the 1st parameter set for output.
C  9. 89/02/03: JB Sinton, Made consistant changes to NCODE and DCODE.
C  8. 88/09/26: JB Sinton, NWIH conversion.
C  7. 88/09/21: JB Sinton, Added "L" format type and changed "X" to
C               work on scalars, arrays, and tables.
C  6. 88/07/29; B Baumel, Insert missing RETURN before entry NCODEDB.
C  5. 88/07/28; JB Sinton, Add "/" or charage-return to formats (See 
C               documentation).
C  4. 88/07/21; JB Sinton, Added tables like DCODE.
C               Also made the same for the CRAY or VAX.
C  3. 88/07/07; JB Sinton, Changed calls to LDC...
C  2. 87/08/26; JB Sinton, Added X data type for scalar fields.
C  1. 87/03/30; JB Sinton, First version.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL NCODE (UFMT,NAMES,NNAM,LOC,IPN,NCARDS,*) Process data.
C        CALL NCODES(???)                       Not used at this time.
C        CALL NCODEBW(LENWRD,CPUTYPE)           Setup for NCODE.
C
C Arguments for NCODE:
C UFMT     - User specified format statement for data convertion. See 
C            format conventions below. [Character variable]
C            Maximum length is 256 characters.
C NAMES    - List of names (NAMELIST) to search through. Each name in
C            this list must be left justified, blank filled, and
C            8 characters long (including blanks).  See name 
C            conventions below. [Character variable]
C NNAM     - Number of items in 'NAMES'. [Integer variable]
C            NNAM should be .le. 100.
C LOC      - Beginning address of the common block containing the 
C            data to be encoded. See examples below. [actual argument 
C            must be real, integer, or byte].
C IPN      - Controls access to the card images produced by NCODE.  
C            The card images are always written to common 
C                /NCODEA/ INPUT(100)*80 .
C            =0, cards written only to common block. 
C =>For VAX: .ne.0, also write cards to CFE work file.
C =>For CRAY:>0, also write cards to history database for process 
C                number IPN.
C            <0, also write cards to DCODE internal file (See DCODE 
C                concerning '%DCODE0').
C NCARDS   - Number of cards created.
C *ERR     - Alternate return 1 is for an error condition.
C
C Arguments for NCODES: ====> This call is not used at this time <====
C Arguments for NCODEBW: ====> Use this call before NCODE! <====
C LENWRD   - Number of bytes/characters per word.  The default is 8,
C            which is valid for the CRAY.  [Integer variable].
C CPUTYPE  - If .TRUE.  then setup for CRAY, this is the default.
C            If .FALSE. then setup for VAX. 
C            [Logical variable].
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
C 2B.FORMAT FIELD CONVENTIONS-Scalar&array    EXAMPLE:  n T m.i /
C    n     - # of names in list using this field. If this is not pre-
C            sent then only one name uses this field.
C    T     - Data type, must be one of the following;
C        F - real number formatted,
C        I - integer number formatted,
C        X - Skip the location associated with this field.
C            If the data type is A, H, or R then ".i" is required.
C        The next 4 NCODE data types are treated as A type.
C        A - left justified character string.
C        H - left justified character string.
C        R - left justified character string.
C        L - left justified character string.
C    m     - Maximum # of array elements to return. If this is not pre-
C            sent then the variable is type SCALAR.
C    .     - Character size delimiter. This is only used for types H, A,
C            and R. 
C    i     - Maximum # of characters to return in each array element.
C            If not present then LENWRD characters will be returned!  
C            This format parameter MUST be a multiple of LENWRD. See
C            the CALLING SEQUENCE discription for NCODEBW for a de-
C            finition of LENWRD.
C    /     - If present in a SCALAR FIELD then start a new card for 
C            the next format field. DO NOT use this in tables or 
C            arrays!.  The new card is started after all the names
C            using the current format field are encoded.
C
C 3. NAME LIST/FORMAT CONVENTIONS AND OTHERS:
C    A. Each name in the NAMELIST must have a corresponding format 
C       field.
C    B. Format fields MUST have the same sequence as the names in the 
C       NAMELIST.
C    C. All names in the NAMELIST will be encoded, except for arrays
C       or tables for which the counter is zero, and scalars or arrays 
C       within tables which are marked for skipping by the X data
C       type.
C    D. The number of array elements to be encoded is pass as an 
C       integer stored one position in front of the array. See example
C       1 below. 
C       +-------------------------------------------------------+
C       | Note a space must be provided for these values, but   |
C       | they are not included in the count of names, NNAM, or |
C       | the number of format fields in the format statement.  |
C       +-------------------------------------------------------+
C    E. The number of elements in each array of a table is passed as
C       as an integer stored one position in front of the table.  
C       See example 2 below.  This is similar to the array counter 
C       (3.D), but the table counter covers all arrays in the table.
C
C 4A.INPUT DATA CONVENTIONS-tables:           EXAMPLE: [I,F]=(1,2),
C    =     - Same as for (4.B).
C    [...] - Delimites the list of names from the table. 
C    (     - Delimiter for start of row within table (not required).
C    )     - Delimiter for end   of row within table (not required).
C            See example 2 below.
C
C 4B.OUTPUT DATA CONVENTIONS-Scalar&array: EXAMPLE: I=1,NAM=(abcd,z)
C    =     - Delimiter to separate the name from the value(s) of a
C            scalar(array).
C    ,     - Delimiter to separate values.
C    (     - Delimiter for start of an array (not required).
C    )     - Delimiter for end   of an array (not required).
C    A. Continuation to the next card is accomplished by putting a 
C       comma or equals sign as the last non-comment character of a line.
C           EX. i=2,f=12345.234,  * first card, note comma after 4
C           EX. G=(1,2,3,4,5)     * continuation card
C           EX. [G, NAM]=         * continuation card
C                         
C 5. EXAMPLE SETUP 1:        
C    A. The user call routine should have the following statements.
C       On the CRAY, character and non-character data types may not
C       be mixed in the common block.  On the VAX mixing data types
C       is OK.
C           COMMON /DC/ NI1,I1(5),NI2,I2(5),R,NH,H(18),A
C           CHARACTER UFMT*24,NAMES(5)*8
C           UFMT = '(2I5,F,H9.16,A.8)'
C           NAMES(1) = 'I1      '
C           NAMES(2) = 'I2      '
C           NAMES(3) = 'R       '
C           NAMES(4) = 'H       '   
C           NAMES(5) = 'A       '
C           NI1      = 2
C           NI2      = 2
C           NH       = 2
C           CALL NCODE(UFMT,NAMES,5,NI1,IPN,*1000)
C    B. The output data may look like (depending on the current 
C       version). 
C123456789*123456789*123456789*123456789*... to 79 and 80 -->9*
C I1      =(          1,          1      ...                 ),
C I2      =(          2,          3      ...                 ),
C F       =     5.5000000, 
C H       =(ABCDEFGHIJK                  ...                 ,
C           123456456789abc              ...                 ),
C A       =JUNK
C
C 6. EXAMPLE SETUP 2:        TABLES                   
C    A. The user call routine should have the following statements.
C           COMMON /DC/ N,I1(5),I2(5),F2(5),H(40),I3
C           CHARACTER UFMT*24,NAMES(5)*8 
C           UFMT = '([I,X,F,H.64]5,I)'
C           NAMES(1) = 'I1      '
C           NAMES(2) = 'I2      '
C           NAMES(3) = 'F2      '
C           NAMES(4) = 'H       '
C           NAMES(5) = 'I3      '
C           I3 = 100
C           N = 3
C           DO 10 I=1,3
C            I1(I) = 100+I
C            F2(I) = 100.5+I
C            H(I)  = 'TEST'
C 10        CONTINUE
C           CALL DCODE(UFMT,NAMES,5,N ,*999,*1000)
C    B. The output data might look like.
C123456789*123456789*123456789*123456789*... to 79 and 80 -->9*
C [I1     ,F2     ,H       ]=
C (101    ,101.5  ,TEST    ),
C (102    ,102.5  ,TEST    ),
C (103    ,103.5  ,TEST    ),
C I3 = 100
C       The table counter N=3.  Note that array I2 is not be in output
C       since it was skipped in the list of [I1, X, F2, H].
C
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C                            IN THIS MODULE
C Subroutines  :
C  NCODE
C Functions    : none
C Entrys       :
C  NCODEDB NCODES NCODET NCODEBW
C Common blocks:
C  /NCODEA/
C-----------------------------------------------------------------------
C           LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C Subroutines:
C  LDCSETU  DECSETU  ENCSETU  CFILL    RBLCH    FAOCH    DECFMTA DECFMTB 
C  FFOCH    DECFMT   ENCRVAL  ENCIVAL  ENCAVAL  LDCLASU  LDCILAR LDCRLAR 
C  LDCCLAR  LDCXLAR  LDCLAFI  LDCR     LDCI     LDCC     WUHIST  CID1    
C  ENCDB    MOVE
C Functions:
C  CTUC
C  Note- all routines called are CPS programs.
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  Storage - none
C  Scratch - none
C  Parms   - none
C-----------------------------------------------------------------------
C\END DOC
C
      PARAMETER (NCRDSP = 200,  !# of cards in I/O buffer.
     +           NPARMS = 100,  !Max # of parameter names (NNAM).
     +           NCHRSF = 256,  !Max # of characters in format.
     +           NFIELDS= 1000) !Max # of fields in the I/O buffer.
      COMMON /NCODEA/ INPUT(NCRDSP)
      DIMENSION ICP1(NFIELDS),ICP2(NFIELDS),ICP3(NFIELDS),ICP4(NFIELDS)
      DIMENSION MAXL(NPARMS),TYPE(NPARMS),LENS(NPARMS),SNVA(NPARMS),
     +          SARR(NPARMS),TABL(NPARMS),LFED(NPARMS)
      DIMENSION LNAMES(NPARMS)
      DIMENSION LOC(*)
      CHARACTER*(*) UFMT,NAMES(NNAM)
      CHARACTER FMT*256,CD*256,T*1,INPUT*80,TYPE*1,CIOUNIT*8
      CHARACTER*1 CTUC
      INTEGER LENS,SNVA,SARR
      LOGICAL LAST ,ERRPRT ,TABLE ,TABL ,CRAY ,CPUTYPE ,ERRPRTU
      LOGICAL LFEED,LFED
      EXTERNAL CTUC
      DATA LEN4R8/8/
      DATA ERRPRT /.FALSE./ ,CRAY/.TRUE./ ,CIOUNIT/'%DCODE0'/
                
      IF (LEN(UFMT).GT.NCHRSF) GOTO 915
      IF (NNAM.GT.NPARMS)      GOTO 919
C
C*******************************************
C ****DECODE THE FORMAT IN UFMT         ****
      CALL LDCSETU (LEN4R8,CRAY)
      CALL DECSETU (LEN4R8,CRAY)
      CALL ENCSETU (LEN4R8,CRAY)
      CALL CFILL (FMT,' ')
      CALL RBLCH (UFMT,FMT,NFC)
      IF (ERRPRT .AND. LEN(UFMT).GT.0 .and. NFC.GT.0) THEN
       PRINT*,'=>NCODE: User format statement: ',UFMT(:LEN(UFMT))
       PRINT*,'=>NCODE: Conditioned format   : ',FMT(:NFC)
       PRINT*,'=>NCODE: Number of names      : ',NNAM
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
      NN = 0
      TABLE= .FALSE.
      LAST = .FALSE.                                           
      IPNTR= 1
      DO 30 IT=1,J
       LFEED = .FALSE.
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
        IF (FMT(ICP2(IT):ICP2(IT)).EQ.'/') THEN
         ICP2(IT) = ICP2(IT)-1
         LFEED = .TRUE.
        ENDIF
        CD = FMT(ICP1(IT):ICP2(IT))
        NCFIELD = ICP2(IT)-ICP1(IT)+1 
       ENDIF
       CALL DECFMT(CD(:NCFIELD),N,T,M,LS,*906)
       IF (M.GT.1) LFEED = .FALSE.  !No "/" allowed for arrays!
       DO 20 IN=1,N
        NN = NN+1
        IF (NN.GT.NPARMS) GOTO 918
        TYPE(NN) = T
        LENS(NN) = LS
        MAXL(NN) = M
        TABL(NN) = TABLE
        LFED(NN) = .FALSE.
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
         PRINT'('' NCODE: NN='',I4,1X,A1,1X,2I3,2I6,''| '',A)',
     +   NN,TYPE(NN),LENS(NN),MAXL(NN),SNVA(NN),SARR(NN),CD(:NCFIELD)
        ENDIF
 20    CONTINUE 
       LFED(NN) = LFEED
       IF (LAST) TABLE = .FALSE.
 30   CONTINUE
      IF (NN.NE.NNAM) GOTO 908
C*******************************************
C
C
C*******************************************
C ****PARSE THE LIST 'NAMES' TO ELIMIN- ****
C ****ATE TRAILING BLANKS               ****
      DO 40 I=1,NNAM
       CALL FFOCH (NAMES(I),LNAMES(I),' ')
       IF (LNAMES(I).NE.0) THEN
        LNAMES(I) = LNAMES(I)-1
       ELSE       
        LNAMES(I) = LEN(NAMES(I))
       ENDIF
 40   CONTINUE
C*******************************************
C
C
C*******************************************
C ****Load the array INPUT with blanks. ****
      CALL CFILL (CD   ,' ')
      DO 50 I=1,NCRDSP
 50   INPUT(I) = CD(:80)
C*******************************************
C
C*******************************************
C ****Encode data starting at LOC into  ****
C ****common block array INPUT          ****
      NCD = 1
      NCH = 0
      ILOC = 1
      IF (MAXL(1).GT.1) NCD = 0
      I = 0
 60   I = I+1    !Loop over names from 1 to NNAM by 1.
       IF      (I.GT.NNAM) THEN
        GOTO 200
       ELSE IF (I.EQ.NNAM) THEN
        LAST = .TRUE.
       ELSE
        LAST = .FALSE.
       ENDIF
       IF (MAXL(I).EQ.1) THEN
C*******************************************
C ****  This is a scalar variable.      **** 
        IF     (CTUC(TYPE(I)).EQ.'F') THEN     ! Data type is real.
         CALL ENCRVAL (INPUT,NCD,NCH,LOC(SARR(I)),NAMES(I),LAST)
        ELSEIF (CTUC(TYPE(I)).EQ.'I') THEN     ! Data type is integer.
         CALL ENCIVAL (INPUT,NCD,NCH,LOC(SARR(I)),NAMES(I),LAST)
        ELSEIF((CTUC(TYPE(I)).EQ.'H') .OR.     ! Data type is character.
     +         (CTUC(TYPE(I)).EQ.'L') .OR.
     +         (CTUC(TYPE(I)).EQ.'A') .OR.
     +         (CTUC(TYPE(I)).EQ.'R'))THEN
         CALL ENCAVAL (INPUT,NCD,NCH,LOC(SARR(I)),LENS(I),
     +   NAMES(I),LAST)
        ENDIF
        IF (LFED(I)) NCH = 80  !This will force a new card.
       ELSE IF (TABL(I)) THEN                  ! Linked arrays.
C*******************************************
C ****  This is a table/linked-array.   **** 
        NLARR = 0
        DO 70 J=I,NNAM          
         IF (SNVA(I).EQ.SNVA(J)) NLARR = NLARR+1
 70     CONTINUE
        N = LOC(SNVA(I))
        IF (N.NE.0) THEN
         IF (I+NLARR-1.GE.NNAM) LAST = .TRUE.
         CALL LDCLASU (N,NLARR)
         DO 80 J=I,I+NLARR-1
          IF      (CTUC(TYPE(J)).EQ.'I') THEN
           CALL LDCILAR (N,LOC(SARR(J)),NAMES(J))
          ELSE IF (CTUC(TYPE(J)).EQ.'F') THEN
           CALL LDCRLAR (N,LOC(SARR(J)),NAMES(J))
          ELSE IF ((CTUC(TYPE(J)).EQ.'H') .OR. 
     +             (CTUC(TYPE(J)).EQ.'L') .OR. 
     +             (CTUC(TYPE(J)).EQ.'A') .OR. 
     +             (CTUC(TYPE(J)).EQ.'R'))THEN
           CALL LDCCLAR (N,LOC(SARR(J)),NAMES(J),LENS(J))
          ELSE 
           CALL LDCXLAR (N,LOC(SARR(J)),NAMES(J),LENS(J))
          ENDIF
 80      CONTINUE
         NCD1 = NCD
         IF (NCD1.LE.0) NCD1 = 1    
         CALL LDCLAFI (NCD,INPUT(NCD1),LAST)
        ENDIF
        I = I+NLARR-1
        IF (N.NE.0) NCH = 80  !This will force scalars to new card.
       ELSE                                    ! Regular arrays.
C*******************************************
C ****  This is a vector variable.      **** 
C ****  Setting NCH=80 at the end of    ****
C ****  array encoding will force NCODE ****
C ****  to the next card for subsequent ****
C ****  parameters.                     ****
        N = LOC(SNVA(I))
        IF (N.NE.0) THEN
         NCD1 = NCD
         IF (NCD1.LE.0) NCD1 = 1    
         IF     (CTUC(TYPE(I)).EQ.'F') THEN     ! Data type is real.
          CALL LDCR (N,NCD,INPUT(NCD1),LOC(SARR(I)),NAMES(I),LAST)
          NCH = 80          
         ELSEIF (CTUC(TYPE(I)).EQ.'I') THEN     ! Data type is integer.
          CALL LDCI (N,NCD,INPUT(NCD1),LOC(SARR(I)),NAMES(I),LAST)
          NCH = 80  
         ELSEIF ((CTUC(TYPE(I)).EQ.'H') .OR.    ! Data type is character.
     +           (CTUC(TYPE(I)).EQ.'L') .OR. 
     +           (CTUC(TYPE(I)).EQ.'A') .OR. 
     +           (CTUC(TYPE(I)).EQ.'R'))THEN
          CALL LDCC (N,NCD,INPUT(NCD1),LOC(SARR(I)),NAMES(I),LAST,
     +    LENS(I))
          NCH = 80  
         ENDIF
        ENDIF
       ENDIF
       IF (LAST) THEN
 95     CONTINUE
        DO 100 J=80,1,-1
         IF (INPUT(NCD)(J:J).NE.' ') GOTO 110
 100    CONTINUE           
C ****Last card is blank so go back one card!
         NCD = NCD-1
         IF (NCD.GT.0) GOTO 95
 110    CONTINUE
        IF (J.GT.0.AND.NCD.GT.0) THEN
         IF (INPUT(NCD)(J:J).EQ.',') INPUT(NCD)(J:J) = ' '
        ENDIF
       ENDIF
       IF (ERRPRT) THEN
        PRINT*,'=>NCODE: FINISHED I=',I,' CARDS SO FAR LOOK LIKE:'
        PRINT'(A)',(INPUT(II),II=1,NCD)
       ENDIF
       IF (NCD.GT.NCRDSP) GOTO 916
      GOTO 60
 200  CONTINUE
C*******************************************
C
C
C*******************************************
C ****Write out the card images if IPN  ****
C ****.NE.0                             ****
      IF (CRAY) THEN
       CIOUNIT(8:) = CHAR(0)
       READ(CIOUNIT,'(A8)') IOUNIT
       LFN = IOUNIT
       IF     (IPN.LT.0) THEN
        LFN = IOUNIT
        DO 300 I=1,NCD
         WRITE (LFN,'(A80)',ERR=901) INPUT(I)
 300    CONTINUE
       ELSEIF (IPN.GE.0) THEN
        DO 305 I=1,NCD
         CALL WUHIST(IPN,INPUT(I))
 305    CONTINUE
       ENDIF
      ELSE
       IF (IPN.NE.0) THEN
        DO 310 I=1,NCD
         CALL CID1 ( INPUT(I) )  !CID1 is CID_ on VAX
 310    CONTINUE
       ENDIF
      ENDIF       
      NCARDS = NCD
      RETURN
C*******************************************
C
C*******************************************
C ****This entry provided for future use.***
      ENTRY NCODEDB (ERRPRTU)
      ERRPRT = ERRPRTU
      CALL ENCDB (ERRPRT)
      ENTRY NCODES 
      return
C*******************************************
C
C*******************************************
C ****This entry provided for future use.***
      ENTRY NCODET
      return
C*******************************************
C
C*******************************************
C ****Set number of bytes per word.     ****
      ENTRY NCODEBW (LENWRD,CPUTYPE)
      CRAY = CPUTYPE
      IF (LENWRD.GT.0) LEN4R8 = LENWRD
      IF (ERRPRT) THEN
       PRINT*,'=>NCODEBW: NCODE set to use ',LEN4R8,' bytes/word.'
       PRINT*,'=>NCODEBW: NCODE set to use CRAY cpu=',CRAY
      ENDIF
      RETURN
C*******************************************
C             
 901  PRINT'(''=>NCODE: write err at card #='',I5)',I
      PRINT*,'=>NCODE: CD=',INPUT(I)(1:80)
      GOTO 1000
 906  PRINT*,'=>NCODE: ERR in decoding format.'
      GOTO 1000        
 908  PRINT*,'=>NCODE: ERR Format has different number'
     +,' of format fields then number of names in NAMELIST.' 
      PRINT*,'=>NCODE: # of names in NAMELIST is ',NNAM
      PRINT*,'=>NCODE: # of format fields is     ',NN
      GOTO 914
 910  PRINT*,'=>NCODE: Error in format, found "[" but no "]".' 
      GOTO 914
 913  PRINT*,'=>NCODE: Error, Your format must start with a "(" and end'
     +,' with a ")".'
 914  CONTINUE
      IF      (LEN(UFMT).LE.0) THEN
       PRINT*,'=>NCODE: Error, your format is 0 length, thats a no-no!'
      ELSE IF (NFC.GT.0)       THEN
       PRINT*,'=>NCODE: Your format=',FMT(:NFC)
      ELSE
       PRINT*,'=>NCODE: Your format is all blanks??!! Is something',
     + 'wrong?!?!'
      ENDIF
      GOTO 1000
 915  MCHRSF = NCHRSF
      PRINT*,'=>NCODE: Your format is longer than ',MCHRSF,
     +' characters.'
      GOTO 1000
 916  MCRDSP = NCRDSP
      PRINT*,'=>NCODE: You have created more than ',MCRDSP,' cards ',
     +'in this call to NCODE.'
      GOTO 1000
 917  MFIELDS = NFIELDS
      PRINT*,'=>NCODE: You have more than ',MFIELDS,' commas in your ',
     +'format.'
      GOTO 1000
 918  MPARMS = NPARMS
      PRINT*,'=>NCODE: You have more than ',MPARMS,' fields in your ',
     +'format.'
      GOTO 1000
 919  MPARMS = NPARMS
      PRINT*,'=>NCODE: You are trying to use more than ',MPARMS,' ',
     +'parameters.'
 1000 CONTINUE
      IF (CRAY) RETURN 1
      PRINT *,'=>NCODE: Press return to continue.'
      read (5,*) cd(1:1)
      RETURN 1
      END
