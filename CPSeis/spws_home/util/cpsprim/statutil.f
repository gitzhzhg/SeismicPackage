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
C                       C P S   P R I M I T I V E
C
C    Primitive name:  STATUTIL    (STATIC FILE UTILITIES) 
C  Source directory:  primitives/math
C           Library:  conlib
C           Written:  90/05/15  by:  Tom Stoeckley
C      Last revised:  92/07/21  by:  Tom Stoeckley
C
C  Purpose:   Set of static file interface routines on the Cray.
C             To open, allocate memory for, and read a static file into
C             memory.  Also to get a static value from a static file,
C             or to save a static file to the Vax.  Several additional
C             routines are also available here for manipulating a static
C             file.
C
C  Related Documentation:  Calls the STATRII primitive.  Called from
C             processes such as GRAB, AER, SHFT, FISH, SISC, etc.
C-----------------------------------------------------------------------
C       warning - This primitive has been modified to work in an
C        interactive program with the SHFT process.  It is not in
C        a state to be ported back the the Cray batch CPS system.
C       recommendation - it would be more sensible for this routine
C        to use character variables rather than hollerith.  But this
C        would mean changing the argument lists, which means changing
C        the calling routines too.
C       warning - STATSAVE and STATTRAN are not yet finished.
C-----------------------------------------------------------------------
C        THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C  Parameters are real or integer based on Fortram conventions.
C  Arrays and hollerith values are identified as such.
C-----------------------------------------------------------------------
C  To get a static file from the Vax, allocate memory for the file from
C  the heap, and read the file into memory:
C                                 i     o    o
C               CALL STATOPEN (VAXFILE,TYPE,KKKS,*err,
C                  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C                   o   o   o    o   o  o   o    o   o  o
C  VAXFILE = name of static file to read (ten hollerith words).
C  TYPE    = type of static file (one hollerith word).
c  KKKS    = pointer to array S(NX,NY) containing static values.
c  *err    = error return label.
C  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
c    NHX,NHY   = header words used to define X,Y ground positions.
c    NHX2,NHY2 = second set of header words for source=receiver file.
c    X1,Y1     = X,Y ground position of first static in the file.
c    XINC,YINC = X,Y increments between ground positions (non-zero).
c    NX,NY     = number of ground positions in X,Y directions.
c          (NHY,NHX2,NHY2=0 if not used)   (NY=1 for 2-D)
C-----------------------------------------------------------------------
C  Same as STATOPEN, but also allocates memory for comment cards, and
C  reads the cards into this memory:
C                                  i     o    o    o     o
C               CALL STATOPEN2 (VAXFILE,TYPE,KKKS,KKKC,NCARD,*err,
C                  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C                   o   o   o    o   o  o   o    o   o  o
c  KKKC    = pointer to hollerith comment card array CARD(10,NCARD).
c  NCARD   = number of comment cards.
C-----------------------------------------------------------------------
C  To get the correct static value from the static file, as determined
C  by the trace header words:
C       o              i  i  i   i   i    i   i  i   i    i   i  i
C      AAA = STATGET1 (HD,S,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)   
C      BBB = STATGET2 (HD,S,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C
C  AAA      = nearest static value.
C  BBB      = interpolated static value, treating NILs as nearly zero.
C  HD(*)    = trace header word array.
C  S(NX,NY) = array containing the static values.
C  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
C-----------------------------------------------------------------------
C  To put NCODE cards into a static file comment card array, when you
C  plan later to save a new file:  Should be called immediately after
C  calling NCODE.
C                                     o     i
C                     CALL STATCARD (KKKC,NCARD)
C
C  KKKC  = pointer to hollerith comment card array CARD(10,NCARD).
c  NCARD = number of cards generated by NCODE (becomes number of
C             comment cards in the static file).
C-----------------------------------------------------------------------
C  To write and save a static file to the Vax:
C                      i    i     i     i   i  i     i
C      CALL STATSAVE (FILE,EXT,PROCESS,TYPE,S,CARD,NCARD,*err,
C     $                     NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C                            i   i   i    i   i  i  i    i    i  i
C  FILE     = name of static file to save (ONE hollerith word if EXT is
C               specified, or TEN hollerith words if EXT is blank).
C  EXT      = extension to add to name of static file (one holl word).
C  PROCESS  = name of process creating this static file (one holl word).
C  TYPE     = type of static file (one hollerith word).
C  S(NX,NY) = array containing the static values.
C  CARD(10,NCARD) = hollerith array containing the comment cards.
C  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
C-----------------------------------------------------------------------
C  To smooth a static file:
C                               i  i  b     i      i
C                CALL STATSMOO (NX,NY,S,T,NXSMOO,NYSMOO)
C
C  S(NX,NY) = array containing the static values.
C  T(NX,NY) = scratch array.
C  NXSMOO   = number of values to smooth in X direction.
C  NYSMOO   = number of values to smooth in Y direction.
C-----------------------------------------------------------------------
C  To replace NILs by values interpolated first in one direction,
C  then in the other direction:
C                        i  i  b
C         CALL STATREPL (NX,NY,S)  interpolate first in X direction
C         CALL STATREPY (NX,NY,S)  interpolate first in Y direction
C
C  S(NX,NY) = array containing the static values.
C-----------------------------------------------------------------------
C  To replace NILs by interpolated values:
C                                    i  b    i
C                     CALL STATREPL1 (NX,S,ISTRIDE)
C
C  NX      = number of elements in array S(*).
C  S(*)    = array containing the static values.
C  ISTRIDE = stride of elements in the array (1 means values are
C             in adjacent storage locations).
C-----------------------------------------------------------------------
C  To build a static file from averages of header word values:
C  First, initialize a static array and counter array to zero:
C                                i  i  o o
C                 CALL STATBLD1 (NX,NY,S,M)
C
C  Then, add a static value to the appropriate element in the static
C  array, and to increment the corresponding counter:
C                 i    i    b b  i   i   i    i   i  i   i    i   i  i
C  CALL STATBLD2 (HD,STATIC,S,M,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C
C  Finally, divide each element of the static array by its corresponding
C  counter, or to set the array element to NIL if its counter is zero:
C                                i  i  b i
C                 CALL STATBLD3 (NX,NY,S,M)
C
C  S(NX,NY) = array containing the static values.
C  M(NX,NY) = array containing counts of values summed into S(NX,NY).
C  HD(*)    = trace header word array.
C  STATIC   = static value to add to element of array S(NX,NY).
C  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
C-----------------------------------------------------------------------
C  To get an interpolated ordinate from an abscissa, using a function
C  defined by a pair of arrays:
C                  o                 i     i    i   i
C                YTERP = STATTERP (XTERP,XARRAY,L,YARRAY)
C
C  YTERP      = interpolated ordinate (dependent variable).
C  XTERP      = abscissa (independent variable).
C  XARRAY(L)  = array of abscissae.
C  YARRAY(L)  = array of ordinates.
C  L          = length of arrays (cannot be zero).
C                    If L is positive, flat extrapolation is done.
C                    If L is negative, sloping extrapolation is done.
C-----------------------------------------------------------------------
C  To get a point on a line:
C                  o                 i   i  i  i  i
C                YROOT = STATROOT (XROOT,XA,XB,YA,YB)
C
C  YROOT = ordinate of point on a line (dependent variable).
C  XROOT = abscissa of point on a line (independent variable).
C  XA    = abscissa of the first point defining the line.
C  XB    = abscissa of the second point defining the line.
C  YA    = ordinate of the first point defining the line.
C  YB    = ordinate of the second point defining the line.
C-----------------------------------------------------------------------
C  To allocate space for translation arrays, and to fill them with
C  information from comment cards:
C                   i  i   i   i   i   i   i     i    i    i    o   o
C    CALL STATTRAN (K,NHX,NHX2,X1,XINC,NX,CARD,NCARD,KKXN,KKXL,LHX,LHX2)
C    CALL STATTRAN (K,NHY,NHY2,Y1,YINC,NY,CARD,NCARD,KKYN,KKYL,LHY,LHY2)
C
C  K = 26 for X coordinate, and 28 for Y coordinate.
C  NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY = static file parameters.
C  CARD(10,NCARD) = array of comment cards.
C  NCARD = number of comment cards.
C  KKXN = pointer to X abscissa translation array.
C  KKYN = pointer to Y abscissa translation array.
C  KKXL = pointer to X ordinate translation array.
C  KKYL = pointer to Y ordinate translation array.
C  LHX,LHY,LHX2,LHY2 = transformed header word numbers = 26,27,28,29.
C    If any of NHX,NHX2,NHY,NHY2=0, the corresponding value will also=0.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 9.  93/10/21 R.Day        Added #include for cpscom.inc
C 8.  92/07/21 Stoeckley    Make parts of these primitives ansi-standard
C                             to work with the SHFT process in an
C                             interactive program (CBYT).
C 7.  92/01/16 Stoeckley    Add subroutine entry STATREPY.
C 6.  92/01/10 Stoeckley    Add subroutine entry STATOPEN2, and add
C                           subroutines STATTERP, STATROOT, STATTRAN,
C                           and add Y-direction interpolation in
C                           STATREPL.
C 5.  90/11/07 Ball         Change CRD from 1000 to 2000 like NCODE
C 4.  90/10/23 Peterson     Include error and abort arguments on calls 
C                           to HPALLOC.
C 3.  90/07/26 Stoeckley    Fix bug in STATGET2 for 2-D (X,Y) static
C                           files, and speed up STATGET2 for 1-D files.
C 2.  90/05/25 Stoeckley    Add file name length flexibility to STATSAVE.
C 1.  90/05/15 Stoeckley    Initial version.
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     STATOPEN    STATCARD   STATSAVE STATTRAN
C                   STATSMOO    STATREPL   STATBLD1 STATBLD2 STATBLD3
C                   STATREPL1
C  Functions:       STATGET1    STATGET2   STATTERP   STATROOT
C  Entry points:    STATOPEN2   STATREPY
C  Common blocks:   NCODEA
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C                (deleted externals are in parentheses)
C
C  (OPNFIL)  (CLOSFIL) (HPALLOC)  ADDEXT  (DATE)   (CLOCK)
C  STATRII  STATRCC  STATREAD  STATWII  STATWCC  STATWRIT
C  sizeof_integer  convert_hh2cc  textfile_open_old  textfile_close_old
C                                 textfile_open_new  textfile_close_new
C  alloc_words     convert_cc2hh  offset_index_words 
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -  0
C  Heap (dynamic)  -  space needed for static file
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE STATOPEN (VAXFILE,TYPE,KKKS,*,
     $                         NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C     GET A STATIC FILE, ALLOCATE MEMORY, AND READ THE FILE.
C----------DIMENSION AND DATA STATEMENTS.
      DIMENSION VAXFILE(*)
      INTEGER   sizeof_integer, LFN, array(1)
      REAL S(1),CARD(1)
      EQUIVALENCE (S,array),(CARD,array)
C     POINTER (KKKS,S(*)),(KKKC,CARD(10,*))
      CHARACTER*80 CFILE
      character*8 type2
      save array
C     DATA LFN/'%STAT%0'L/
C----------GET STARTED.
      IFLAG=1
      GO TO 10
      ENTRY STATOPEN2 (VAXFILE,TYPE,KKKS,KKKC,NCARD,*,
     $                         NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
      IFLAG=2
C----------READ STATIC FILE.
 10   call convert_hh2cc (vaxfile,0,   cfile,0)
      PRINT '(//)'
      PRINT *, 'STATOPEN:  GET STATIC FILE  ',CFILE
c     CALL GETLUN(LFN,*999)
C     CALL OPNFIL (CFILE,LFN,*999)
      call textfile_open_old (lfn,cfile,ierr)
      if (ierr.ne.0) go to 999
      CALL STATRII (LFN,TYPE2,NHX,NHY,NHX2,NHY2,
     $                  X1,Y1,XINC,YINC,NX,NY,*888,NCARD2)
      MFAC=8/sizeof_integer()    ! number of hollerith words for 8 bytes.
      call convert_cc2hh (type2,-1,   type,MFAC)
      IF (IFLAG.EQ.2) THEN
           NCARD=NCARD2
           call alloc_words (kkkc,ncard*10*mfac)
           call offset_index_words (moff,array,kkkc)
           CALL STATRCC (LFN,CARD(MOFF+1),NCARD,*888)
C          IABORT = 0
C          CALL HPALLOC (KKKC,10*NCARD,IER1,IABORT)
C          CALL STATRCC (LFN,CARD,NCARD,*888)
      END IF
      call alloc_words (kkks,NX*NY)
      call offset_index_words (moff,array,kkks)
      CALL STATREAD (LFN,NX,NY,S(MOFF+1),*888)
      call textfile_close_old (lfn,cfile,ierr)
      if (ierr.ne.0) go to 999
C     IABORT = 0
C     CALL HPALLOC (KKKS,NX*NY,IER1,IABORT)
C     CALL STATREAD (LFN,NX,NY,S,*888)
C     CLOSE (LFN,STATUS='DELETE')
      RETURN                                                          
C----------ERROR.
888   call textfile_close_old (lfn,cfile,ierr)
999   PRINT *, 'STATOPEN:  GET/READ ERROR ON STATIC FILE'
      RETURN 1
      END

  
                      


      FUNCTION STATGET1 (HD,S,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C     GET STATIC VALUE FROM STATIC FILE.
C     STATIC IS DETERMINED WITH HEADER WORDS (NHX,NHY) AND ALSO
C        WITH HEADER WORDS (NHX2,NHY2).
C     THE NEAREST STATIC IN THE STATIC FILE IS USED.
C     NHX IS ALWAYS GREATER THAN ZERO.
C     NHY,NHX2,NHY2 CAN BE ZERO.
C----------DIMENSION STATEMENTS.
      DIMENSION HD(*),S(NX,NY)
C----------GET X INDEX.
      KX=NINT((HD(NHX)-X1)/XINC)+1
      KX=MIN0(NX,MAX0(1,KX))
C----------GET Y INDEX.
      KY=1                   
      IF (NHY.GT.0) THEN
           KY=NINT((HD(NHY)-Y1)/YINC)+1
           KY=MIN0(NY,MAX0(1,KY))
      END IF
C----------RETURN THE CORRECT STATIC.      
      STATGET1=S(KX,KY)
      IF (NHX2.EQ.0) RETURN
C----------GET X3 INDEX.
      KX=NINT((HD(NHX2)-X1)/XINC)+1
      KX=MIN0(NX,MAX0(1,KX))
C----------GET Y3 INDEX.
      KY=1                   
      IF (NHY2.GT.0) THEN
           KY=NINT((HD(NHY2)-Y1)/YINC)+1
           KY=MIN0(NY,MAX0(1,KY))
      END IF
C----------ADD TO THE CORRECT STATIC.      
      STATGET1=STATGET1+S(KX,KY)
      RETURN
      END




      FUNCTION STATGET2 (HD,S,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C     GET STATIC VALUE FROM STATIC FILE.
C     STATIC IS DETERMINED WITH HEADER WORDS (NHX,NHY) AND ALSO
C        WITH HEADER WORDS (NHX2,NHY2).
C     STATIC IS INTERPOLATED BETWEEN VALUES IN THE FILE.
C     NILS ARE TREATED AS NEARLY ZERO.
C     NHX IS ALWAYS GREATER THAN ZERO.
C     NHY,NHX2,NHY2 CAN BE ZERO.
C----------DIMENSION STATEMENTS.
      DIMENSION HD(*),S(NX,NY)
C----------GET BRACKETING X INDICES.
      X=(HD(NHX)-X1)/XINC+1.
      KX=X
      LX=KX+1
      KX=MIN0(NX,MAX0(1,KX))
      LX=MIN0(NX,MAX0(1,LX))
C----------GET BRACKETING Y INDICES (AND GET THE CORRECT STATIC).
      IF (NHY.GT.0) THEN
           Y=(HD(NHY)-Y1)/YINC+1.
           KY=Y
           LY=KY+1
           KY=MIN0(NY,MAX0(1,KY))
           LY=MIN0(NY,MAX0(1,LY))
           SKY=S(KX,KY)+(X-KX)*(S(LX,KY)-S(KX,KY))
           SLY=S(KX,LY)+(X-KX)*(S(LX,LY)-S(KX,LY))
           STATGET2=SKY+(Y-KY)*(SLY-SKY)
      ELSE
           STATGET2=S(KX,1)+(X-KX)*(S(LX,1)-S(KX,1))
      END IF
C----------RETURN THE CORRECT STATIC.
      IF (NHX2.EQ.0) RETURN
C----------GET BRACKETING X3 INDICES.
      X=(HD(NHX2)-X1)/XINC+1.
      KX=X
      LX=KX+1
      KX=MIN0(NX,MAX0(1,KX))
      LX=MIN0(NX,MAX0(1,LX))
C----------GET BRACKETING Y3 INDICES (AND ADD TO THE CORRECT STATIC).
      IF (NHY2.GT.0) THEN
           Y=(HD(NHY2)-Y1)/YINC+1.
           KY=Y
           LY=KY+1
           KY=MIN0(NY,MAX0(1,KY))
           LY=MIN0(NY,MAX0(1,LY))
           SKY=S(KX,KY)+(X-KX)*(S(LX,KY)-S(KX,KY))
           SLY=S(KX,LY)+(X-KX)*(S(LX,LY)-S(KX,LY))
           STATGET2=STATGET2+SKY+(Y-KY)*(SLY-SKY)
      ELSE
           STATGET2=STATGET2+S(KX,1)+(X-KX)*(S(LX,1)-S(KX,1))
      END IF
C----------RETURN THE CORRECT STATIC.      
      RETURN
      END





      SUBROUTINE STATCARD (KKKC,NCARD)
C     ALLOCATES MEMORY FROM THE HEAP FOR A STATIC FILE COMMENT CARD ARRAY,
C        AND PUTS NCODE CARDS INTO THIS ARRAY.
C     KKKC = POINTER FOR THE COMMENT CARD ARRAY.
C     NCARD = NUMBER OF CARDS WRITTEN BY NCODE (RETURNED BY NCODE) AND
C        PLACED INTO THE COMMENT CARD ARRAY.
      INTEGER sizeof_integer
      COMMON/NCODEA/CRD(2000)
      REAL CARD(1)
      EQUIVALENCE (CARD,CRD)
C     POINTER (KKKC,CARD(*))
      
C     IABORT = 0
C     CALL HPALLOC (KKKC,10*NCARD,IER1,IABORT)
      MFAC=80/sizeof_integer()    ! number of hollerith words for 80 bytes.
      call alloc_words (kkkc,mfac*ncard)
      call offset_index_words (moff,crd,kkkc)
      DO 10 I=1,MFAC*NCARD
10    CARD(I+MOFF)=CRD(I)
      RETURN
      END



cccccccc---------the following routine is incompletely converted:
                          
      SUBROUTINE STATSAVE (FILE,EXT,PROCESS,TYPE,S,CARD,NCARD,*,
     $                      NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C     WRITE AND SAVE A STATIC FILE.
C     FILE = ONE HOLLERITH WORD  IF  EXT  IS SPECIFIED.
C     FILE = TEN HOLLERITH WORDS IF  EXT  IS BLANK.
C----------DIMENSION AND DATA STATEMENTS.
      integer sizeof_integer
c     DIMENSION FILE(10),S(NX,NY),CARD(10,NCARD)
      DIMENSION FILE(20),S(NX,NY),CARD(*)
      CHARACTER*8 EXT,PROCESS,TYPE
      CHARACTER*80 FULLNAME,CHARCARD
      CHARACTER*8 DDD,TTT
C     DATA LFN/'%STAT%0'L/
C----------ADD EXTENSION TO STATIC FILE NAME.
      MFAC=8/sizeof_integer()  ! number of hollerith words for 8 characters.
      IF (EXT.EQ.' ') THEN
           WRITE (FULLNAME,'(10A8)') FILE
      ELSE
           WRITE (FULLNAME,'(10A8)') FILE(1)
           CALL ADDEXT (FULLNAME,EXT)
      END IF
C----------GET STARTED.
      PRINT '(//)'                                    
      PRINT *, 'STATSAVE:  STATIC FILE  ',FULLNAME,
     $   '  SAVED BY PROCESS  ',PROCESS
C----------WRITE STATIC FILE.
c     CALL GETLUN(LFN,*999)
      call textfile_open_new (lfn,fullname,ierr)
      if (ierr.ne.0) go to 999
      CALL STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,
     $   NX,NY,*888)
      CHARCARD='STATIC FILE CREATED BY PROCESS  '//PROCESS
      CALL STATWCC (LFN,CHARCARD,1,*888)
      CALL STATWCC (LFN,CARD,NCARD,*888)
      DDD='datedate'
      TTT='timetime'
C     CALL DATE (DDD)
C     CALL CLOCK (TTT)
      CHARCARD='STATIC FILE  '//FULLNAME(1:30)//'  SAVED  '//
     $   DDD//'  '//TTT
      CALL STATWCC (LFN,CHARCARD,1,*888)
      CALL STATWRIT (LFN,NX,NY,S,*888)
C----------SAVE STATIC FILE.
C     CALL CLOSFIL (FULLNAME,LFN,*999)
C     CLOSE (LFN,STATUS='DELETE')
      call textfile_close_new (lfn,fullname,ierr)
      if (ierr.ne.0) go to 999
      PRINT '(//)'
      RETURN
C----------ERROR.
888   call textfile_close_new (lfn,fullname,ierr)
999   PRINT *, 'STATSAVE:  WRITE/SAVE ERROR ON STATIC FILE  ',FULLNAME
      RETURN 1
      END
 




      SUBROUTINE STATREPL (NX,NY,S)
C     REPLACE NILS WITH INTERPOLATED/EXTRAPOLATED VALUES.
C     ENTRY STATREPL: INTERPOLATION IS DONE FIRST IN THE X DIRECTION, THEN Y.
C     ENTRY STATREPY: INTERPOLATION IS DONE FIRST IN THE Y DIRECTION, THEN X.
C----------DIMENSION AND DATA STATEMENTS.
      IMPLICIT NONE
      INTEGER NX,NY,IX,IY
      REAL S(NX,NY)
C----------INTERPOLATE FIRST IN THE X DIRECTION, THEN Y.
CCC   ENTRY STATREPL (NX,NY,S)
      DO 5 IY=1,NY
5     CALL STATREPL1 (NX,S(1,IY),1)
      DO 6 IX=1,NX
6     CALL STATREPL1 (NY,S(IX,1),NX)
      RETURN
C----------INTERPOLATE FIRST IN THE Y DIRECTION, THEN X.
      ENTRY STATREPY (NX,NY,S)
      DO 7 IX=1,NX
7     CALL STATREPL1 (NY,S(IX,1),NX)
      DO 8 IY=1,NY
8     CALL STATREPL1 (NX,S(1,IY),1)
      RETURN
      END




      SUBROUTINE STATREPL1 (N,S,ISTRIDE)
C     REPLACE NILS WITH INTERPOLATED/EXTRAPOLATED VALUES.
C     IF EVERYTHING IS NILS, NOTHING IS CHANGED.
C----------DIMENSION AND DATA STATEMENTS.
      IMPLICIT NONE
      INTEGER N,ISTRIDE,I,IA,IB,J
      REAL S(ISTRIDE,*),ZNIL
      PARAMETER (ZNIL=-1.E-30)
C----------FIND FIRST NON-NIL VALUE.
      DO I=1,N 
           IF (S(1,I).NE.ZNIL) GO TO 11
      END DO
      RETURN           ! everything is nils.
11    S(1,1)=S(1,I) 
C----------FIND LAST NON-NIL VALUE.
      DO I=N,1,-1
           IF (S(1,I).NE.ZNIL) GO TO 13
      END DO
      RETURN           ! we should not get to here.
13    S(1,N)=S(1,I) 
C----------DO THE WORK.
      DO I=2,N 
           IF (S(1,I-1).NE.ZNIL.AND.S(1,I).EQ.ZNIL) THEN 
                IA=I-1
           ELSE IF (S(1,I-1).EQ.ZNIL.AND.S(1,I).NE.ZNIL) THEN
                IB=I
                DO J=IA+1,IB-1
                     S(1,J)=S(1,IA)+(J-IA)*(S(1,IB)-S(1,IA))/(IB-IA) 
                END DO
           END IF
      END DO
      RETURN
      END





      SUBROUTINE STATSMOO (NX,NY,S,T,NXSMOO,NYSMOO)
C     SMOOTH THE STATIC FILE ARRAY.
C     THE FILE SHOULD NOT CONTAIN ANY NILS.
C     S(NX,NY) = ORIGINAL FILE (INPUT) AND SMOOTHED FILE (OUTPUT).
C     T(NX,NY) = SCRATCH SPACE.
      DIMENSION S(NX,NY),T(NX,NY)
C----------DO THE SMOOTHING IN THE X DIRECTION.
      IF (NXSMOO.LE.1.AND.NYSMOO.LE.1) RETURN
      IF (NX.GT.1.AND.NXSMOO.GT.1) THEN
           K=NXSMOO/2
           DO 8 IY=1,NY
           DO 8 IX=1,NX
           IA=MAX0(IX-K,1)
           IB=MIN0(IX+K,NX)
           SUM=0.
           DO 6 I=IA,IB
6          SUM=SUM+S(I,IY)
8          T(IX,IY)=SUM/(IB-IA+1)
      ELSE
           DO 9 IY=1,NY
           DO 9 IX=1,NX
9          T(IX,IY)=S(IX,IY)
      END IF
C----------DO THE SMOOTHING IN THE Y DIRECTION.
      IF (NY.GT.1.AND.NYSMOO.GT.1) THEN
           K=NYSMOO/2
           DO 18 IX=1,NX
           DO 18 IY=1,NY
           IA=MAX0(IY-K,1)
           IB=MIN0(IY+K,NY)
           SUM=0.
           DO 16 I=IA,IB
16         SUM=SUM+T(IX,I)
18         S(IX,IY)=SUM/(IB-IA+1)
      ELSE
           DO 19 IX=1,NX
           DO 19 IY=1,NY
19         S(IX,IY)=T(IX,IY)
      END IF
      RETURN
      END


  


      SUBROUTINE STATBLD1 (NX,NY,S,M)
C     STATIC FILE INITIALIZATION ROUTINE.
C     PART OF TRIO STATBLD1,STATBLD2,STATBLD3.
      DIMENSION S(*),M(*)
      DO 4 I=1,NX*NY
      S(I)=0.
4     M(I)=0
      RETURN
      END




      SUBROUTINE STATBLD3 (NX,NY,S,M)
C     STATIC FILE FINALIZATION ROUTINE.
C     PART OF TRIO STATBLD1,STATBLD2,STATBLD3.
      DIMENSION S(*),M(*)
      PARAMETER (ZNIL=-1.E-30)
      DO 10 I=1,NX*NY
      IF (M(I).EQ.0) THEN
           S(I)=ZNIL
      ELSE
           S(I)=S(I)/M(I)
      END IF
10    CONTINUE
      RETURN
      END





      SUBROUTINE STATBLD2 (HD,STATIC,S,M,
     $   NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY)
C     STATIC FILE UPDATE ROUTINE.
C     PART OF TRIO STATBLD1,STATBLD2,STATBLD3.
C     STATIC GROUND POSITION IS DETERMINED FROM HEADER WORDS (NHX,NHY)
C        AND ALSO FROM HEADER WORDS (NHX2,NHY2).
C     TRACES OUTSIDE THE RANGE OF THE STATIC FILE ARE NOT USED.
C     NHX IS ALWAYS GREATER THAN ZERO.
C     NHY,NHX2,NHY2 CAN BE ZERO.
C----------DIMENSION STATEMENTS.
      DIMENSION HD(*),S(NX,NY),M(NX,NY)
C----------GET X INDEX.
      KX=NINT((HD(NHX)-X1)/XINC)+1
      IF (KX.LT.1.OR.KX.GT.NX) GO TO 600
C----------GET Y INDEX.
      KY=1                   
      IF (NHY.GT.0) THEN
           KY=NINT((HD(NHY)-Y1)/YINC)+1
           IF (KY.LT.1.OR.KY.GT.NY) GO TO 600
      END IF
C----------UPDATE THE STATIC AND COUNTER ARRAYS.
      S(KX,KY)=S(KX,KY)+STATIC
      M(KX,KY)=M(KX,KY)+1
600   IF (NHX2.EQ.0) RETURN
C----------GET X2 INDEX.
      KX=NINT((HD(NHX2)-X1)/XINC)+1
      IF (KX.LT.1.OR.KX.GT.NX) RETURN
C----------GET Y2 INDEX.
      KY=1                   
      IF (NHY2.GT.0) THEN
           KY=NINT((HD(NHY2)-Y1)/YINC)+1
           IF (KY.LT.1.OR.KY.GT.NY) RETURN
      END IF
C----------UPDATE THE STATIC AND COUNTER ARRAYS.
      S(KX,KY)=S(KX,KY)+STATIC
      M(KX,KY)=M(KX,KY)+1
      RETURN
      END







      FUNCTION STATTERP (XTERP,X,L,T)
C     L=POSITIVE GIVES FLAT EXTRAPOLATION.
C     L=NEGATIVE GIVES SLOPING EXTRAPOLATION. 
C     L=ZERO IS NOT ALLOWED.
C     X(L) MUST BE EITHER INCREASING OR DECREASING.
C     IT IS OK TO HAVE TWO ADJACENT EQUAL VALUES IN X(L).
      DIMENSION X(*),T(*) 
      LA=1
      LB=IABS(L)
      XRANGE=X(LB)-X(LA)
6     IF (LB-LA.LE.1) GO TO 100 
      LC=(LA+LB)/2
      IF (XRANGE*(X(LC)-XTERP).GE.0.) GO TO 7 
      LA=LC 
      GO TO 6 
7     LB=LC 
      GO TO 6 
100   STATTERP=STATROOT(XTERP,X(LA),X(LB),T(LA),T(LB))
      IF (L.LT.0) RETURN
      IF (XRANGE*(X(1)-XTERP).GE.0.) STATTERP=T(1) 
      IF (XRANGE*(X(L)-XTERP).LE.0.) STATTERP=T(L) 
      RETURN
      END 

      


      FUNCTION STATROOT (XROOT,XA,XB,YA,YB)
      IF (XA.EQ.XB) GO TO 10
      STATROOT=YA+(XROOT-XA)*(YB-YA)/(XB-XA) 
      RETURN
10    STATROOT=(YA+YB)/2.
      RETURN
      END 
                 


ccccc-------the following routine is not checked for compatibility
ccccc       on all machines.  In particular, CARD may be hollerith
ccccc       rather than character.

      SUBROUTINE STATTRAN (K,NHX,NHX2,X1,XINC,NX,CARD,NCARD,KKXN,KKXL,
     $   LHX,LHX2)
C     K=26 FOR X HEADER AND K=28 FOR Y HEADER.
C     POINTER (KKXN,XN(*)),(KKXL,XL(*))
      INTEGER LOCATION,SIZEOF_REAL
      REAL  XN(1),XL(1)
C#include <cpscom.inc>
      COMMON /SCRSIZ/ ISCRSIZEp,ISTOSIZEp,IWIDTHp
      INTEGER   ISCRSIZEp,ISTOSIZEp,IWIDTHp
C...... ISCRSIZEp The size of the SCRATCH common block
C...... ISTOSIZEp The size of the SCRATCH common block
C...... IWIDTHp   The size of the SCRATCH common block

      COMMON /SCRATCH/ SCRBUFp(24000)
      REAL      SCRBUFp
C...... SCRBUFp   The scratch memory buffer. Volatile memory.

      COMMON /STORAGE/ ISTOPNTRp,STOBUFp(24000)
      REAL      STOBUFp
      INTEGER   ISTOPNTRp
C...... STOBUFp   The scratch memory buffer. Non-Volatile memory.
C...... ISTOPNTRp Pointer to the current end of storage.
      EQUIVALENCE (XN,STOBUFp),(XL,STOBUFp)
      CHARACTER*80 CARD(*)
      CHARACTER*8 TEST
      LHX=NHX
      LHX2=NHX2
      IF (NCARD.EQ.0) RETURN
      IF (NHX.EQ.0.AND.NHX2.EQ.0) RETURN
      IF (NHX.GT.0) LHX=K
      IF (NHX2.GT.0) LHX2=K+1
      IABORT = 0
      LENR = SIZEOF_REAL()
      MFAC = 20
      IF(LENR.NE.0) MFAC = 80/LENR  !VALID FOR 2,4, OR 8 BYTE MACHINES
      CALL GETS(KKXN,NX)
      MO1 = KKXN - LOCATION(STOBUFp(1))   ! should not do this way
C     CALL HPALLOC (KKXN,NX,IER1,IABORT)
      IABORT = 0
      CALL GETS(KKXL,NX)
      MO2 = KKXL - LOCATION(STOBUFp(1))      ! change
C     CALL HPALLOC (KKXL,NX,IER1,IABORT)
      TEST=' offs:  '
      IF (K.EQ.28) TEST='  IDs:  '
      ICARD=0
      J=5
      DO I=1,NX
           IF (J.EQ.5) THEN
10              ICARD=ICARD+1
                IF (ICARD.GT.NCARD) GO TO 999
                IF (CARD(ICARD)(1:8).NE.TEST) GO TO 10
                J=0
           END IF
           J=J+1
           JA=8+(J-1)*14
           JB=JA+13
           READ (CARD(ICARD)(JA:JB),2000,ERR=999) XN(I+MO1)
2000       FORMAT (F14.0)
           XL(I+MO2)=X1+(I-1)*XINC
      END DO
      PRINT *, 'STATTRAN:  false ',TEST,(XL(I+MO1),I=1,NX)
      PRINT *, 'STATTRAN:  actual',TEST,(XN(I+MO2),I=1,NX)
      RETURN
999   LHX=NHX
      LHX2=NHX2
      RETURN
      END




