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
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C  Process name:  STATRII    (STATIC FILE READ AND WRITE)
C        Author:  TOM STOECKLEY , 15 NOVEMBER 1988
C  Last revised: 01/21/92  Stoeckley
C                                                     
C  Purpose:       Read and write static files on the CRAY or the VAX.
C-----------------------------------------------------------------------
C                   INPUT PARAMETERS: Passed as arguments
C-----------------------------------------------------------------------
C\END DOC                           
C\PROG DOC                                                             
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 01/21/92  Stoeckley    Remove unused variable.
C 01/16/92  Peterson     Change 2000 FORMAT from 5G14.6 to 5G14.0
C 06/20/91  Troutt       Made all error returns for reads and writes
C                        unique for making trouble shooting easier.
C 05/08/89  Stoeckley    Minor change in documentation.
C 01/30/89  Stoeckley    Add documentation for use on the VAX.
C 12/05/88  Stoeckley    Add some print statements.
C 11/23/88  Stoeckley    Add second set of headers, and type code.
C-----------------------------------------------------------------------
C            STATIC FILE WRITE CALLING SEQUENCE ON THE CRAY
C
C  REAL CARD,S
c  POINTER (IPCARD,CARD(10,*))          Set up pointers for comment
c  POINTER (IPS,S(*))                   cards and static values.
c                                 
c  CALL HPALLOC (IPCARD,10*NCARD)       Allocate storage for comment
c  CALL HPALLOC (IPS,NX*NY)             cards and static values.
C                                
c  LFN=...                              Get local file name assigned.
c
c  CALL STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,*err)
C                                       Write initial information.
c
C  CALL STATWCC (LFN,CARD,NUMBER,*err)  Write previous comment cards,
c                                       plus any new comment from 
c                                       current process.  These cards
c                                       can be written with one or
c                                       or more calls to this entry.
c                                       Blank cards will not be written.
c
C  CALL STATWRIT (LFN,NX,NY,S,*err)     Write static values.
C
C  CALL HPDEALLC (IPCARD)               Deallocate storage for arrays
C  CALL HPDEALLC (IPS)                  no longer needed.
C                                                       
c  CALL CLOSFIL ('vaxfilename.ext',LFN,*err)     Send file to VAX.
C-----------------------------------------------------------------------
C            STATIC FILE READ CALLING SEQUENCE ON THE CRAY
C
C  REAL CARD,S                                                         
c  POINTER (IPCARD,CARD(10,*))          Set up pointers for comment
c  POINTER (IPS,S(*))                   cards and static values.
c
c  LFN=...                              Get local file name assigned.
c
c  CALL OPNFIL ('vaxfilename.ext',LFN,*err)     Fetch file from VAX.
c
c  CALL STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,X1,Y1,XINC,YINC,NX,NY,*err,
C                                                                NCARD)
C                                       Read initial information.
c                                                         
c  CALL HPALLOC (IPCARD,10*NCARD)       Allocate storage for comment
c  CALL HPALLOC (IPS,NX*NY)             cards and static values.
C                                
C  CALL STATRCC (LFN,CARD,NUMBER,*err)  Read comment cards (one or more
C                                       calls, so that total number of 
c                                       cards read = NCARD).  If you
c                                       do not call this entry, or do
c                                       not read all the cards, nothing
c                                       bad will happen.  If you try to 
c                                       read too many cards, the error
c                                       return will be used, with
c                                       unfilled cards set to blank.
c                                       This is the only case where the
c                                       error return may be considered 
c                                       non-fatal.
c
c  CALL STATREAD (LFN,NX,NY,S,*err)     Read static values.      
C
C  CALL HPDEALLC (IPCARD)               Deallocate storage for arrays
C  CALL HPDEALLC (IPS)                  no longer needed.
C-----------------------------------------------------------------------
C                 CALLING SEQUENCE PARAMETER DEFINITIONS
C
C  READ OR WRITE INITIAL INFORMATION:
c
C        CALL STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
C                          X1,Y1,XINC,YINC,NX,NY,*err,NCARD)
C
C        CALL STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
C                          X1,Y1,XINC,YINC,NX,NY,*err)
c
C     LFN  = local file unit number.
C     TYPE = type of static file (CHARACTER*8).                  
c     NHX  = header word used to define the static X ground position.
c     NHY  = header word used to define the static Y ground position.
c     NHX2 = second X header word for source=receiver file.
c     NHY2 = second Y header word for source=receiver file.
c                 (NHY,NHX2,NHY2=0 if not used.)
c     X1   = X ground position of first static in the file.
c     Y1   = Y ground position of first static in the file.
c     XINC = increment between X ground positions (non-zero).
c     YINC = increment between Y ground positions (non-zero).
c     NX   = number of ground positions in X direction.
c     NY   = number of ground positions in Y direction (=one for 2-D).
c     *err  = error return label.
c     NCARD = number of comment cards in the static file.
C
C  READ OR WRITE COMMENT CARDS:
c
C        CALL STATRCC (LFN,CARD,NUMBER,*err)
C        CALL STATWCC (LFN,CARD,NUMBER,*err)
C
C     LFN   = local file name or unit number.   
c     CARD  = CHARACTER*80 comment card image array (dimension>=NUMBER).
c     NUMBER = number of cards to read or write.
c     *err  = error return label.
c
C  READ OR WRITE STATIC VALUES:
C
C        CALL STATREAD (LFN,NX,NY,S,*err)
C        CALL STATWRIT (LFN,NX,NY,S,*err)
C
C     LFN   = local file name or unit number.
C     NX    = number of ground positions in X direction.
c     NY    = number of ground positions in Y direction.
c               (NX and NY are returned from STATRII.)
c     S     = array of static values with dimension (NX,NY).
c     *err  = error return label.
C_______________________________________________________________________
C                                NOTES
C
C  1. If only one header word is to be used, set NHY=0, NY=1, 
c     Y1=anything, and YINC=nonzero.                                    
C
C  2. The second set of header words is used only for source=receiver
C     static files.  Otherwise they are set to zero.
C
C  3. The TYPE word can be one of these codes (others will also be
C     defined in the future):
C                'DATUM'    = datum static file
C                'REFR'     = refraction static file
C                'RESID'    = residual static file
C
C  4. The one-dimensional index for  S(IX,IY)  is  S(IX+(NX-1)*IY).
C
C  5. Static files are VAX-resident text files with the following
C     format:
C     First record:        (A8,4I4/4G14.6,2I7)  TYPE,NHX,NHY,NHX2,NHY2,
C                                               X1,Y1,XINC,YINC,NX,NY
C     Each comment card:        (A80)           CARD
C     Special card after all comment cards: (A80)  CARD (='+++END+++')
C     Static values written:    (I8,5G14.6)     S (NX*NY values)
C     Static values read:       (8X,5G14.6)     S (NX*NY values)
C     The field designated I8 contains the 1-dimensional index of the
C     first value of array S on that line.  This field is skipped while
C     reading the file.  It is intended as help to the user if he is
C     viewing or editing the static file with a general-purpose editor
C     such as EVV.
C
C  6. The CRAY and VAX versions of this primitive are identical, except
C     for the fact that the CRAY version prints some useful information.
C_______________________________________________________________________
C
C  Contains the following entry points:  STATRII, STATRCC, STATREAD,
C                                        STATWII, STATWCC, STATWRIT.
C-----------------------------------------------------------------------
C\END DOC
      SUBROUTINE STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $                        X1,Y1,XINC,YINC,NX,NY,*,NCARD)
C     THIS IS THE VAX VERSION OF THE SAME PRIMITIVE ON THE CRAY.
C     IT IS IDENTICAL TO THE CRAY VERSION EXCEPT FOR THE OMISSION OF
C        PRINT STATEMENTS.
C----------DIMENSION STATEMENTS.
      DIMENSION S(NX,NY),T(*)
      CHARACTER*8 TYPE
      CHARACTER*80 CARD(*),MSG,ENDFLAG
      DATA ENDFLAG/'+++END+++'/
C----------READ INITIAL INFORMATION.
      NCARD=-1    
5     REWIND LFN
      READ (LFN,4000,ERR=992) TYPE,NHX,NHY,NHX2,NHY2,
     $                        X1,Y1,XINC,YINC,NX,NY
      IF (NCARD.GE.0) RETURN
C----------READ AND PRINT HISTORY CARDS.
10    NCARD=NCARD+1
      READ (LFN,5000) MSG
      IF (MSG.EQ.ENDFLAG) GO TO 5
      GO TO 10
C----------READ COMMENT CARDS.
      ENTRY STATRCC (LFN,CARD,NUMBER,*)
      DO 19 I=1,NUMBER
19    CARD(I)=' '
      DO 20 I=1,NUMBER
      READ (LFN,5000,ERR=993) CARD(I)
      IF (CARD(I).EQ.ENDFLAG) THEN
           BACKSPACE LFN
           RETURN 1
      END IF
20    CONTINUE
      RETURN
C----------READ STATIC VALUES.
      ENTRY STATREAD (LFN,NX,NY,S,*)
50    READ (LFN,5000,ERR=994) MSG
      IF (MSG.NE.ENDFLAG) GO TO 50
      READ (LFN,2000,ERR=995) S 
      RETURN
C----------WRITE INITIAL INFORMATION.
      ENTRY STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $                   X1,Y1,XINC,YINC,NX,NY,*)
C----------WRITE FIRST CARD.
      REWIND LFN
      WRITE (LFN,4000,ERR=996) TYPE,NHX,NHY,NHX2,NHY2,
     $                         X1,Y1,XINC,YINC,NX,NY
      RETURN     
C----------WRITE COMMENT CARDS.
      ENTRY STATWCC (LFN,CARD,NUMBER,*)
      DO 60 I=1,NUMBER
      IF (CARD(I).EQ.' ') GO TO 60
      WRITE (LFN,5000,ERR=997) CARD(I)
60    CONTINUE
      RETURN
C----------WRITE STATIC VALUES.
      ENTRY STATWRIT (LFN,NX,NY,T,*)
      WRITE (LFN,5000,ERR=998) ENDFLAG 
      N=NX*NY
      DO 40 I=1,N,5
      I2=MIN0(I+4,N)
40    WRITE (LFN,1000,ERR=999) I,(T(J),J=I,I2)      
      RETURN
C----------ERROR RETURN.
992   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ 992 '
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
993   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ 993'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
994   PRINT *, 'STATREAD:  ERROR IN STATIC FILE READ 994'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
995   PRINT *, 'STATREAD:  ERROR IN STATIC FILE READ 995'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
996   PRINT *, 'STATWII:  ERROR IN STATIC FILE WRITE 996'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
997   PRINT *, 'STATWCC:  ERROR IN STATIC FILE WRITE 997'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
998   PRINT *, 'STATWRIT:  ERROR IN STATIC FILE WRITE 998'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
999   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ OR WRITE'
      RETURN 1
C----------FORMAT STATEMENTS.
1000  FORMAT (I8,5G14.6)
2000  FORMAT (8X,5G14.0)
4000  FORMAT (A8,4I4/4G14.6,2I7)
5000  FORMAT (A80)       
5002  format (' lfn=',i3,' or lfn=',a8)
      END
