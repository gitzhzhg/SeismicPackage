C*******************************************************************************
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
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*******************************************************************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: ICPSCAN
C        Author: R. S. DAY
C       Written: 90/06/22 ?
C  Last revised: 92/06/08 Day
C
C  Purpose:      IO ROUTINES THAT OPEN, CLOSE, READ, WRITE AND CONVERT
C                ICP MODEL FILES AND GWS GENERIC PICK FILES.
C                
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPOPMF(LUNGMF,IOS,NAMEN,IO,FTYPE)
C       ENTRY ICPCLMF(LUNGMF,IOS)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C LUNGMF......INTEGER   IN       FORTRAN LOGICAL UNIT NUMBER
C IOS.........INTEGER   OUT      RETURN ERROR STATUS
C NAMEN.......CHARACTER IN       FILE NAME
C IO..........CHARACTER IN       R OR W FOR TYPE OF FILE ACCESS
C FTYPE.......CHARACTER OUT      FILE TYPE= ICP, GWS OR ???
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. OPENS AND CLOSES ICP MODEL FILES AND GWS GENERIC PICK FILES. WILL
C    RETURN THE FILE TYPE WHEN IT IS OPENED.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE GWSREAD(IUNIT,MODLINE,MODTYPE,X1,Y1,NHOR,HORIZ,HORIZO
C      +NPIECES,NL,NPOINTS,SCALE)
C       ENTRY GWSWRITE (IUNIT,MODLINE,MODTYPE,NHOR,HORIZON,
C      +      NPIECES,NL,NPOINTS,X1,Y1,SCALE)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C IUNIT.......INTEGER   IN       FORTRAN LOGICAL UNIT NUMBER
C MODLINE.....CHARACTER IN-OUT   LINE NAME TO READ OR WRITE
C MODTYPE.....CHARACTER IN-OUT   LINE TYPE TO READ OR WRITE
C X1..........REAL      IN-OUT   ARRAY OF X COORDINATES
C Y1..........REAL      IN-OUT   ARRAY OF Y COORDINATES
C NHOR........INTEGER   IN-OUT   NUMBER OF HORIZONS TO READ,WRITE
C HORIZ.......INTEGER   IN-OUT   ARRAY WITH NHOR HORIZON ID VALUES
C HORIZON.....CHARACTER IN-OUT   ARRAY WITH NHOR HORIZON NAMES
C NPIECES.....INTEGER   IN-OUT   ARRAY WITH NHOR ENTRYS. NUMBER OF
C                                SEGMENTS IN EACH HORIZON
C NL..........INTEGER   IN-OUT   TOTAL NUMBER OF SEGMENTS
C NPOINTS.....INTEGER   IN-OUT   NUMBER OF POINTS IN EACH OF THE NL SEG.
C SCALE.......REAL      IN       CONSTANTS TO RESCALE THE PICKS AS THEY
C                                ARE READ IN OR WRITTEN OUT. SEE NOTES.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPSCAN (IUNIT,IOS,NLINES,LNAMES,LTYPES,FTYPE)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C IUNIT.......INTEGER   IN       FORTRAN UNIT NUMBER
C IOS.........INTEGER   OUT      RETURN ERROR STATUS
C NLINES......INTEGER   OUT      NUMBER OF LINES IN THE FILE
C LNAMES......CHARACTER OUT      NAMES OF THE LINES IN THE FILE
C LTYPES......CHARACTER OUT      LINE TYPE OF EACH LINE IN THE FILE.
C                                SHOULD BE DEPTH OR TIME
C FTYPE.......CHARACTER IN       FILE TYPE THAT IS BEING SCANNED.ICP,GWS
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. SCAN A PICK FILE AND RETURN A LIST OF THE LINE NAMES AND TYPES.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPWRLN(LUNO,NLINE,MODLINE,MODTYPE,INFOTEXT,RLIMITS,
C     + SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NHOR,HORIZ,
C     + HORIZON,NPIECES)
C       ENTRY ICPWRPCK(LUNO,LSET,NSET,NATTR,FPLABEL,CHTMP,RUC,RNC,
C      + SCALE,NCRDS,X1,Y1,A1)
C       ENTRY ICPRDLN(IOR,LUNI,LGET,MODLINE,MODTYPE,INFOTEXT,RLIMITS,
C      + SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NHOR,
C      + HORIZ,HORIZON,NL,NPIECES)
C       ENTRY ICPRDPCK (IOR,LUNI,LGET,NGET,NATTR,FPLABEL,CHTMP,RUC,
C      + RNC,NCRDS,X1,Y1,A1,SCALE)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C LUNO........
C NLINE.......
C MODLINE.....
C MODTYPE.....
C INFOTEXT....
C RLIMITS.....
C SPUNT.......CHARACTER           SHOT POINT UNITS.
C SPINT.......REAL                VALUE OF SHOT POINT INTERVAL IN UNITS
C                                 OF SPUNT.
C SPORG.......REAL                SHOT VALUE SPORG IS EQUIVALENT TO
C                                 TRACE VALUE TRORG. USEFUL IF YOU WANT TO
C                                 MATCH TRACE TO SHOT POINT X-COORDINATES.
C TRORG.......REAL                SEE SPORG.
C SCALE.......REAL()              USED TO SCALE FROM DISK UNITS TO
C                                 ARBITRARY USER UNITS. TO RETAIN THE
C                                 DISK UNITS SET SCALE=1.,1.,0.,0.
C XUNITS......CHARACTER           UNITS OF X COORDINATES ON DISK.
C ZUNITS......CHARACTER           UNITS OF Z COORDINATES ON DISK.
C NHOR........
C HORIZ.......
C HORIZON.....
C NPIECES.....
C X1..........
C Y1..........
C A1..........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. READ AND WRITE THE ICP MODEL FILE HEADER, AND THE PICK DATA.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPWRVM(IOS,LUNO,NDOF,SCALE,NCRDS,VBUF)
C       ENTRY ICPRDVM(IOS,LUNI,NDOF,SCALE,NCRDS,VBUF)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C IOS.........
C LUNO........INTEGER            FORTRAN LOGICAL UNIT NUMBER
C NDOF........INTEGER            DEGREES OF FREEDOM PER VELOCITY FUNCTION
C SCALE.......REAL()             SCALING ARRAY FOR X AND Z COORDINATES
C NCRDS.......INTEGER            NUMBER OF CARD IMAGES
C VBUF........REAL()             ARRAY WITH VELOCITYS, VELOCITY TYPES,
C                                MATERIAL ID VALUES, X-Z COORDINATES.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. READ AND WRITE THE VELOCITY FUNCTIONS FOR AN ICP DEPTH MODEL.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPWRCV(IOS,LUNO,SCALE,NCRDS,VBUF)
C       ENTRY ICPRDCV(IOS,LUNI,SCALE,NCRDS,VBUF)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C IOS.........
C LUNO........INTEGER            LOGICAL UNIT NUMBER
C SCALE.......REAL()             SCALING ARRAY
C NCRDS.......INTEGER            RECORDS READ, OR TO WRITE
C VBUF........REAL()             BUFFER TO HOLD X-Z COORDINATES AND
C                                MATERIAL ID VALUE
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICPWRSC(LUNO,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
C      +           HX1,HZ1,HY1)
C       ENTRY ICPRDSC(LUNI,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
C      +      HX1,HZ1,HY1)
C
C READ AND WRITE SCALING FACTORS TO ICP MODEL FILE
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C LUNO........INTEGER            FORTRAN LOGICAL IO UNIT
C    X = xscale*(Xdisk-xp1) + hx1    , similar for x and z
C Xdisk= (X-hx1)/xscale     + xp1    , similar for x and z
C XSCALE......REAL               
C ZSCALE......REAL
C YSCALE......REAL
C PX1.........REAL
C PZ1.........REAL
C PY1.........REAL
C HX1.........REAL
C HZ1.........REAL
C HY1.........REAL
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C       SUBROUTINE ICP2GWS(IOS,NCRDS,X1,Y1,A1,NATTR,NHOR,NPIECES,HORIZ,
C      +          NSEG,NPOINTS,NPNTR,WRK,LIST)
C       ENTRY GWS2ICP(NHOR,NPIECES,NSEG,NPOINTS,NPNTR,NCRDS,HORIZ,A1)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C IOS.........
C NCRDS.......
C X1..........
C Y1..........
C A1..........
C NATTR.......
C NHOR........
C NPIECES.....
C HORIZ.......
C NSEG........
C NPOINTS.....
C NPNTR.......
C WRK.........
C LIST........
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author     Description
C     ----     ------     -----------
C 10. 92/06/08 Day        Allowing number of picks to be zero.
C 9.  91/02/18 DAY        ICPRD1,ICPWR1-->ICPRDF,ICPWRF. SOLVES PROBLEM
C                         ON UNIX SYSTEM.
C 8.  91/01/29 DAY        ALTERED OUTPUT X-Y FORMAT SLIGHTLY
C 7.  12/05/90 DAY        ALTERED GWSWRITE FORMATS.
C 6.  11/19/90 DAY        ADDED TRORG TO ARG LIST OF ICPRDF,ICPWRF,
C                         ICPRDLN AND ICPWRLN. 
C 5.  11/12/90 DAY        FIXED ICPRDLN SO LINE TYPE IS CORRECT.
C 4.  09/18/90 DAY        ICPWRSC AND ICPRDSC ADDED
C 3.  07/15/90 DAY        ICP2GWS WILL HANDLE NATTR=1 CASE NOW
C 2.  07/09/90 DAY        ALTERED SOME ROUTINES TO RUN ON THE CRAY.
C 1.  06/22/90 DAY        ADDED SHOT POINT INTERVAL AS SEPERATE ARGUMENT
C                         TO ICPWRLN AND ICPRDLN.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C EXTERNAL CALLS:PIKSCALE
C
C SUBROUTINES   :GWSREAD
C
C ENTRY POINTS  :GWSWRITE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
C*******************************************************************************
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*******************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                   INTERACTIVE CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C  Process name: ICPWRF
C        Author: DAY
C  Last revised: 90/09/18
C
C  Purpose:      READ OR WRITE A PORTION OR ALL OF AN ICP MODEL FILE.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C      SUBROUTINE ICPWRF(LINE,LUNO,OTYP,OPTION,
C     +  LNAME,LTYPE,COMMENT,RLIMITS,SPINT,SPORG,TRORG,
C     +  XUNITS,ZUNITS,YUNITS,SPUNT,TUC,TNC,
C     +  XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,HX1,HZ1,HY1,
C     +  NCLASS,CLLAB,CLID,NSUBCL,NSEG,NPTSUBCL,NPNTR,IBTYP,
C     +  NUM,NATTR,X1,Z1,A1,
C     +  KCRDS,CBUF,
C     +  JDOF,JCRDS,VBUF)
C
C       ENTRY ICPRDF(LINE,LUNI,ITYP,OPTION,
C      +  LNAME,LTYPE,COMMENT,RLIMITS,SPINT,SPORG,TRORG,
C      +  XUNITS,ZUNITS,YUNITS,SPUNT,TUC,TNC,
C      +  XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,HX1,HZ1,HY1,
C      +  NCLASS,CLLAB,CLID,NSUBCL,NSEG,NPTSUBCL,NPNTR,IBTYP,
C      +  NUM,NATTR,X1,Z1,A1,
C      +  KCRDS,CBUF,
C      +  JDOF,JCRDS,VBUF,WRK,IGRP)
C
C                           INPUT PARAMETERS
C
C  Name       Type      InOut    Description
C  ----       -------   -----    -----------
C LINE........CHARACTER          =OK IF CALL WAS SUCCESSFUL
C LUNO........INTEGER            FORTRAN LOGICAL UNIT NUMBER
C OTYP........CHARACTER          ICP OR GWS - INDICATES FILE TYPE
C OPTION......CHARACTER          FLAG THAT CONTROLS THE ACTION TAKEN.
C             WP and RP are only allowed options for GWS files.
C                                WP---> write *LINE,*PICK sections
C                                WC---> write *CELL section
C                                WV---> write *VELOCITY section 
C                                WM---> write all of the above.
C                                RP---> read *LINE,*PICK sections.
C                                RC---> read *CELL section
C                                RV---> read *VELOCITY section 
C                                RM---> read all of the above.
C
C LNAME.......CHARACTER          LINE NAME
C LTYPE.......CHARACTER          LINE TYPE  - TIME OR DEPTH
C COMMENT.....CHARACTER          ANY USER COMMENT UP TO 70 CHARACTERS
C RLIMITS.....REAL(4)            MODEL LIMITS..XMIN,XMAX,ZMIN,ZMAX
C                                USED IN CELL COMPUTATIONS
C SPINT.......REAL               SHOT POINT INTERVAL
C SPORG.......REAL               NOT USED AT PRESENT
C XUNITS......CHARACTER          UNITS OF X-COORDINATES ON DISK
C ZUNITS......CHARACTER          UNITS OF Z-COORDINATES ON DISK
C YUNITS......CHARACTER          UNITS OF Y-COORDINATES ON DISK
C SPUNT.......CHARACTER          UNITS OF THE SHOT INTERVAL
C TUC.........REAL(4)            USER COORDIANTES
C TNC.........REAL(8)            NORMAL COORDINATES
C XSCALE......REAL               X = (Xdisk-px1)*xscale + hx1
C ZSCALE......REAL                          .
C YSCALE......REAL                          .
C PX1.........REAL                          .
C PZ1.........REAL                          . etc.
C PY1.........REAL                          .
C HX1.........REAL                          .
C HZ1.........REAL                          .
C HY1.........REAL                          .
C NCLASS......INTEGER            NUMBER OF PICK CLASSES(I.E.HORIZONS)
C CLLAB.......CHARACTER()        CLASS OR HORIZON LABELS
C CLID........INTEGER()          CLASS ID VALUE
C NSUBCL......INTEGER()          NUMBER OF SUBCLASSES IN EACH CLASS
C NSEG........INTEGER            TOTAL NUMBER OF SEGMENTS
C NPTSUBCL....INTEGER()          NUMBER OF POINTS IN EACH SUBCLASS OR
C                                SEGMENT
C NPNTR.......INTEGER()          MEMORY POINTER TO START OF SUBCLASSES
C IBTYP.......INTEGER()
C NUM.........INTEGER            TOTAL NUMBER OF PICK RECORDS
C NATTR.......INTEGER            NUMBER OF ATTRIBUTES PER PICK RECORD
C X1..........REAL()             PICK X- COORDINATES
C Z1..........REAL()             PICK Z- COORDINATES
C A1..........REAL()             ATTRIBUTES ASSOCIATED WITH PICKS
C KCRDS.......INTEGER            RECORD COUNT FOR THE CBUF BUFFER
C CBUF........REAL()             BUFFER FOR MATERIAL IDS...
C JDOF........INTEGER()          DEGREES OF FREEDOM PER VELOCITY FUNC.
C JCRDS.......INTEGER            RECORDS IN VBUF BUFFER
C VBUF........REAL()             BUFFER FOR VELOCITY FUNCTIONS
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1.
C-----------------------------------------------------------------------
C      LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C-----------------------------------------------------------------------
C EXTERNAL CALLS:GWSWRITE            ICPWRLN             ICPWRSC
C EXTERNAL CALLS:GWS2ICP             ICPWRPCK            ICPWRVM
C EXTERNAL CALLS:ICPWRCV             GWSREAD             ICPRDLN
C EXTERNAL CALLS:TO                  ICPRDSC             ICPRDPCK
C EXTERNAL CALLS:ICP2GWS             FAILED              ICPRDCV
C EXTERNAL CALLS:ICPRDVM
C
C SUBROUTINES   :ICPWRF
C
C ENTRY POINTS  :ICPRDF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C\END DOC
CCC
      SUBROUTINE ICPWRF(LINE,LUNO,OTYP,OPTION,
     +  LNAME,LTYPE,COMMENT,RLIMITS,SPINT,SPORG,TRORG,
     +  XUNITS,ZUNITS,YUNITS,SPUNT,TUC,TNC,
     +  XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,HX1,HZ1,HY1,
     +  NCLASS,CLLAB,CLID,NSUBCL,NSEG,NPTSUBCL,NPNTR,IBTYP,
     +  NUM,NATTR,X1,Z1,A1,
     +  KCRDS,CBUF,
     +  JDOF,JCRDS,VBUF)
C
      REAL          SCALE(4),FPLABEL
      REAL          XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,HX1,HZ1,HY1
      REAL          TUC(*),TNC(*),WRK(*)
      REAL          X1(*),Z1(*),A1(*),RLIMITS(*),CBUF(*),VBUF(*)
      REAL          SPINT,SPORG
      CHARACTER*(*) LINE,OTYP,OPTION,LNAME,LTYPE,COMMENT,ITYP
      CHARACTER*(*) XUNITS,ZUNITS,YUNITS,SPUNT,CLLAB(*)
      INTEGER       LUNO,NCLASS
      INTEGER       CLID(*),NSUBCL(*),NPTSUBCL(*),NPNTR(*),IBTYP(*)
      INTEGER       JCRDS,KCRDS,JDOF,NATTR
      CHARACTER     IO,CHTMP*8
      LINE='OK'
      IF(LUNO.LE.0 .OR. LUNO.GT.119) THEN
         LINE='ICPWRF: INVALID OUTPUT UNIT GIVEN'//CHAR(0)
         RETURN
      END IF
      IF(XSCALE.EQ.0.) XSCALE=1.0
      IF(ZSCALE.EQ.0.) ZSCALE=1.0
      IF(YSCALE.EQ.0.) YSCALE=1.0
      IF(OPTION(3:3).EQ.'N') THEN  !do not unscale x values
       SCALE(1)=1.0
       SCALE(2)=1.0 
       SCALE(3)=0.0
       SCALE(4)=0.0
      ELSE                         !do unscale x values
       SCALE(1)=XSCALE
       SCALE(2)=ZSCALE
       SCALE(3)=HX1-PX1*XSCALE
       SCALE(4)=HZ1-PZ1*ZSCALE
      END IF
C
C ***** GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS *****
C OUTPUT A SIMPLE GENERIC GWS PICK FILE
      IF(OTYP.EQ.'GWS') THEN
        IF(OPTION(1:2).EQ.'WP') THEN
        IF(NUM.LT.0) THEN
         LINE='ICPWRF: NO PICKS TO SAVE'//CHAR(0)
         RETURN
        END IF
        CALL GWSWRITE (LUNO,LNAME,LTYPE,NCLASS,CLLAB,
     +      NSUBCL,NSEG,NPTSUBCL,X1,Z1,SCALE)
        OPTION='XXX'
        LINE='OK'
        RETURN
        ELSE
        OPTION='WP'
        LINE='ICPWRF: ONLY WP OPTION SUPPORTED FOR GWS FILES'//CHAR(0)
        RETURN
        END IF
      END IF
C
C ***** ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP *****
C OUTPUT PART OR ALL OF AN ICP MODEL FILE
      OTYP='ICP'
      IF(OPTION(1:2).EQ.'WP') THEN
        NATTR=2
        FPLABEL=1.5
        IF(NUM.LT.0) THEN
         LINE='ICPWRF: NO PICKS TO SAVE'//CHAR(0)
         RETURN
        END IF
        CALL ICPWRLN(LUNO,OGRP,LNAME,LTYPE,COMMENT,RLIMITS,
     +  SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NCLASS,
     +  CLID,CLLAB,NSUBCL)
        NHX=1
        NHY=1
        CALL ICPWRSC(LUNO,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
     +           HX1,HZ1,HY1)
        CALL GWS2ICP(NCLASS,NSUBCL,NSEG,NPTSUBCL,NPNTR,NUM,CLID,A1)
        CALL ICPWRPCK(LUNO,LN,NSET,NATTR,FPLABEL,CHTMP,TUC,TNC,
     +  SCALE,NUM,X1,Z1,A1)
        OPTION='XXX'
        LINE='OK'
        RETURN
      END IF
C
      IF(OPTION(1:2).EQ.'WV') THEN
        CALL ICPWRVM(IOS,LUNO,JDOF,SCALE,JCRDS,VBUF)
        OPTION='XXX'
        LINE='OK'
        RETURN
      END IF
C
      IF(OPTION(1:2).EQ.'WC') THEN
        CALL ICPWRCV(IOS,LUNO,SCALE,KCRDS,CBUF)
        OPTION='XXX'
        LINE='OK'
        RETURN
      END IF
C
      IF(OPTION(1:2).EQ.'WM') THEN
        NATTR=2
        FPLABEL=1.5
        IF(NUM.LT.0) THEN
         LINE='ICPWRF: NO PICKS TO SAVE'//CHAR(0)
         RETURN
        END IF
        CALL ICPWRLN(LUNO,OGRP,LNAME,LTYPE,COMMENT,RLIMITS,
     +  SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NCLASS,
     +  CLID,CLLAB,NSUBCL)
        CALL ICPWRSC(LUNO,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
     +           HX1,HZ1,HY1)
        CALL GWS2ICP(NCLASS,NSUBCL,NSEG,NPTSUBCL,NPNTR,NUM,CLID,A1)
        CALL ICPWRPCK(LUNO,LN,NSET,NATTR,FPLABEL,CHTMP,TUC,TNC,
     +  SCALE,NUM,X1,Z1,A1)
        CALL ICPWRCV(IOS,LUNO,SCALE,KCRDS,CBUF)
        CALL ICPWRVM(IOS,LUNO,JDOF,SCALE,JCRDS,VBUF)
        OPTION='XXX'
        LINE='OK'
        RETURN
      END IF
      LINE='ICPWRF: UNRECOGNIZED OUTPUT OPTION'//CHAR(0)
      RETURN
CCC
      ENTRY ICPRDF(LINE,LUNI,ITYP,OPTION,
     +  LNAME,LTYPE,COMMENT,RLIMITS,SPINT,SPORG,TRORG,
     +  XUNITS,ZUNITS,YUNITS,SPUNT,TUC,TNC,
     +  XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,HX1,HZ1,HY1,
     +  NCLASS,CLLAB,CLID,NSUBCL,NSEG,NPTSUBCL,NPNTR,IBTYP,
     +  NUM,NATTR,X1,Z1,A1,
     +  KCRDS,CBUF,
     +  JDOF,JCRDS,VBUF,WRK,IGRP)
      LINE='OK'
      IF(LUNI.LE.0 .OR. LUNI.GT.119) THEN
         LINE='ICPRDF: INVALID INPUT UNIT GIVEN'//CHAR(0)
         RETURN
      END IF
      IF(XSCALE.EQ.0.) XSCALE=1.0
      IF(ZSCALE.EQ.0.) ZSCALE=1.0
      IF(YSCALE.EQ.0.) YSCALE=1.0
      SCALE(1)=1.0
      SCALE(2)=1.0
      SCALE(3)=0.0
      SCALE(4)=0.0
      IF(OPTION(4:4).NE.'F') THEN  ! SCALING FROM ARGUMENTS ?
       SCALE(1)=XSCALE
       SCALE(2)=ZSCALE
       SCALE(3)=HX1-PX1*XSCALE
       SCALE(4)=HZ1-PZ1*ZSCALE
      END IF
C
C ***** GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS-GWS *****
C INPUT A SIMPLE GENERIC GWS PICK FILE
      IF(ITYP.EQ.'GWS') THEN
        IF(OPTION.EQ.'RP') THEN
          CALL GWSREAD (LUNI,LNAME,LTYPE,X1,Z1,NCLASS,CLID,CLLAB,
     +         NSUBCL,NSEG,NPTSUBCL,SCALE)
          CALL GWS2ICP(NCLASS,NSUBCL,NSEG,NPTSUBCL,NPNTR,NUM,CLID,A1)
          NATTR=2
C          RLIMITS(1)=0.
C          RLIMITS(2)=0.
C          RLIMITS(3)=0.
C          RLIMITS(4)=0.
          XUNITS='SHOTPOINT'
          ZUNITS='XXXXXXXXX'
          YUNITS='XXXXXXXXX'
C          SPUNT ='XXXXXXXXX'
C          SPINT =1.0
C          SPORG =0.0
          COMMENT='THE INPUT FILE IS A GWS GENERIC PICK FILE '
          OPTION='XXX'
          LINE='OK'
          RETURN
        ELSE
          OPTION='RP'
          LINE='ICPRDF: ONLY RP OPTION VALID FOR GWS FILES'//CHAR(0)
          RETURN
        END IF
      END IF
      ITYP='ICP'
C
C ***** ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP-ICP *****
C INPUT PART OR ALL OF AN ICP MODEL FILE
      IGRP=MAX(IGRP,1)
      CALL ICPRDLN(IOS,LUNI,IGRP,LNAME,LTYPE,
     + COMMENT,RLIMITS,SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,
     + YUNITS,NCLASS,CLID,CLLAB,NSEG,NSUBCL)
      IF(IOS.NE. 0) THEN
         LINE='ICPRDF: CALL TO ICPRDLN FAILED'
         GO TO 79
      END IF
      IF(OPTION(4:4).EQ.'F') THEN    !USE SCALING FROM FILE
        CALL ICPRDSC(LUNI,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
     +       HX1,HZ1,HY1)
        RLIMITS(1)=(RLIMITS(1)-SCALE(3))/SCALE(1)
        RLIMITS(2)=(RLIMITS(2)-SCALE(3))/SCALE(1)
        RLIMITS(3)=(RLIMITS(3)-SCALE(4))/SCALE(2)
        RLIMITS(4)=(RLIMITS(4)-SCALE(4))/SCALE(2)
        SCALE(1)=XSCALE
        SCALE(2)=ZSCALE
        SCALE(3)=HX1-PX1*XSCALE
        SCALE(4)=HZ1-PZ1*ZSCALE
        RLIMITS(1)=RLIMITS(1)*SCALE(1) + SCALE(3)
        RLIMITS(2)=RLIMITS(2)*SCALE(1) + SCALE(3)
        RLIMITS(3)=RLIMITS(3)*SCALE(2) + SCALE(4)
        RLIMITS(4)=RLIMITS(4)*SCALE(2) + SCALE(4)
      END IF
      IF(OPTION(2:2).EQ.'M' .OR. OPTION(2:2).EQ.'P') THEN
        NGET=1
        CALL ICPRDPCK (IOS,LUNI,IGRP,NGET,NATTR,FPLABEL,CHTMP,
     +       TUC,TNC,NUM,X1,Z1,A1,SCALE)
        IF(IOS.NE. 0) THEN
         LINE='ICPRDF: CALL TO ICPRDPCK FAILED'
         GO TO 79
        END IF
        IF(NATTR.EQ.0) THEN
         LINE='ICPRDF: NUMBER OF ATTRIBUTES < 1 '
         GO TO 79
        END IF
        CALL ICP2GWS(IOS,NUM,X1,Z1,A1,NATTR,NCLASS,NSUBCL,CLID,
     +       NSEG,NPTSUBCL,NPNTR,WRK,WRK(NUM+2))
        IF(IOS.NE.0) THEN
         LINE='ICPRDF: ICP2GWS CALL FAILED '
         GO TO 79
        END IF
      END IF
      IF(OPTION(1:2).EQ.'RM' .OR. OPTION(1:2).EQ.'RC') THEN
        CALL ICPRDCV(IOS,LUNI,SCALE,KCRDS,CBUF)
        IF(IOS.EQ.1) THEN
         LINE='ICPRDF: LUN<=0 FOR ICPRDCV CALL'
         GO TO 79
        END IF
      END IF
      IF(OPTION(1:2).EQ.'RM' .OR. OPTION(1:2).EQ.'RV') THEN
        CALL ICPRDVM(IOS,LUNI,JDOF,SCALE,JCRDS,VBUF)
        IF(IOS.EQ.1) THEN
         LINE='ICPRDVM: LUN<=0 '
         GO TO 79
        ELSE IF(IOS.EQ.2) THEN
         LINE='ICPRDVM: DEGREES OF FREEDOM <1 '
         GO TO 79
        END IF
      END IF
 79   RETURN
      END
CCC
      SUBROUTINE GWSREAD(IUNIT,MODLINE,MODTYPE,X1,Y1,NHOR,HORIZ,HORIZON,
     +NPIECES,NL,NPOINTS,SCALE)
      CHARACTER*(*) MODLINE,MODTYPE,HORIZON(*)
      INTEGER   IUNIT,HORIZ
      DIMENSION X1(*),Y1(*),NPIECES(*),NPOINTS(*),HORIZ(*),SCALE(*)
      CHARACTER CLINE*30,CHTYPE*4,CPTYPE*4,WHICH*4
      CHARACTER HTYPE*4
      CHARACTER RECORD*120, CA*14, ERRTEXT*30
      LOGICAL FOUND
C   PARAMETERS
C    NHOR  = NUMBER OF GEOLOGICAL HORIZONS
C    HORIZON ( ) = THE NAME OF EACH HORIZON
C    NL    = NUMBER OF DIGITISED ELEMENTS. EACH HORIZON MAY BE SPLIT BY A
C       FAULT OR SALT WALL CAUSING IT TO BE DIGITISED IN SEVERAL ELEMENTS
C    NPIECES ( ) = THE NUMBER OF ELEMENTS IN EACH HORIZON
C    NPOINTS ( ) = NUMBER OF POINTS IN EACH ELEMENT

C  START TO READ THE PICK DATA
      NCRDS=0
      CA='              '
      NL=0
      NHOR=0
      WHICH='FAUL'

C Check for pick information that is for the model line (ie found)

  100 READ (IUNIT,'(A120)',END=190) RECORD
      IF (RECORD(2:6).EQ.'PICKS') GOTO 120
      IF (.NOT.FOUND) GOTO 100

  104 IF (RECORD(2:6).EQ.'PICKN') GOTO 140
      IF (RECORD(2:6).EQ.'PICKZ') GOTO 150
         ERRTEXT=' NOT A SEISMIC HORIZON PICK'

  106  WRITE(6,*) ERRTEXT
       GOTO 100

C is this pick of right type? from right line?

  120 READ (RECORD,121) CLINE,CHTYPE,CPTYPE
  121 FORMAT (6X,A30,30X,A4,A4)
      FOUND=.FALSE.

      ERRTEXT=' PICK IS NOT OF TYPE '//WHICH
      IF (CHTYPE.NE.WHICH) GOTO 100
      ERRTEXT=' PICK IS NOT IN '//MODTYPE
      IF (CPTYPE.NE.MODTYPE(1:4)) GOTO 100
      ERRTEXT=CLINE//' NOT MODEL LINE '//MODLINE
      IF (CLINE.NE.MODLINE) GOTO 100

C  FOUND the right line data. Read faults first, always horizon 1.
      FOUND=.TRUE.
      IF (WHICH.EQ.'FAUL') THEN
        NHOR=NHOR+1
        IF (CHTYPE.NE.'FAUL')  GO TO 100
        NPIECES(NHOR)=0
        HORIZON(NHOR)='FAULTS'
        HORIZ(NHOR)=0
      ELSE
        NHOR=NHOR+1
        NPIECES(NHOR)=0
        HORIZON(NHOR)=RECORD(37:66)
        HORIZ(NHOR)=NHOR
        IF(NHOR.GT.1) HORIZ(NHOR)=HORIZ(NHOR-1)+1
      ENDIF
      GOTO 100

C accept picks
  140 NL=NL+1
      NPIECES(NHOR)=NPIECES(NHOR)+1
      NPOINTS(NL)=0
      GOTO 160

  150 IF (NPIECES(NHOR).EQ.0) THEN
      NL=NL+1
      NPIECES(NHOR)=1
      NPOINTS(NL)=0
      ENDIF

  160 I=8
      DO 165 J=9,107,14
  165 IF ((RECORD(J:J+13).EQ.CA).AND.(I.EQ.8)) I=(J-9)/14
      J1=NCRDS+1
      NCRDS=NCRDS+I
      J2=NCRDS
      READ (RECORD,'(8X,16F7.0)') (X1(J),Y1(J),J=J1,J2)
      NPOINTS(NL)=NPOINTS(NL)+I
      GOTO 100

 190  REWIND IUNIT
      IF (WHICH.EQ.'HORI') THEN
        CALL PIKSCALE(NCRDS,X1,Y1,1,SCALE)
        RETURN
      END IF
      WHICH='HORI'
      GOTO 100
CCC
      ENTRY GWSWRITE (IUNIT,MODLINE,MODTYPE,NHOR,HORIZON,
     +      NPIECES,NL,NPOINTS,X1,Y1,SCALE)
      NPERL=8
      L=0
C SCALE IS FACTOR TO CONVERT BACK TO SHOT POINTS
      SCX=1.0
      IF(SCALE(1).NE.0) SCX=1.0/SCALE(1)
      SCY=1.0
      IF(SCALE(2).NE.0) SCY=1.0/SCALE(2)
      N1=0
      N2=0
      DO 630 I=1,NHOR        !LOOP OVER HORIZONS
       HTYPE='HORI'
       IF (HORIZON(I).EQ.'FAULTS') HTYPE='FAUL'
       RECORD=' PICKS '
       WRITE (RECORD(7:),'(2A30,2A4)') MODLINE,HORIZON(I),HTYPE,MODTYPE
       WRITE (IUNIT,'(A120)') RECORD
       DO 630 J=1,NPIECES(I) !LOOP OVER SEGMENTS PER HORIZON
        L=L+1
        N1=N2+1
        N2=N2+NPOINTS(L)
        BIGX=0.
        BIGY=0.
        DO 619 M=N1,N2
         BIGX=MAX(ABS(X1(M)),BIGX)
         BIGY=MAX(ABS(Y1(M)),BIGY)
 619    CONTINUE
        CA='(8(F7.3,F7.2))'
        IF (BIGX.GT.999.) CA='(8(F7.2,F7.2))'
        IF (BIGX.GT.9999.) CA='(8(F7.1,F7.2))'
        IF (BIGY.GT.9999.) CA='(8(F7.2,F7.1))'
        RECORD=' PICKN  '
        DO 620 N=N1,N2,NPERL
         K1=N
         K2=MIN(K1+7,N2)
         WRITE (RECORD(9:),CA) (SCX*(X1(K)-SCALE(3)),
     +          SCY*(Y1(K)-SCALE(4)),K=K1,K2)
         NCH=8 + (K2-K1+1)*14
         WRITE (IUNIT,'(A)') RECORD(:NCH)
         RECORD=' PICKZ  '
 620    CONTINUE
 630   CONTINUE
      RETURN
      END
CCC
      SUBROUTINE ICPSCAN (IUNIT,IOS,NLINES,LNAMES,LTYPES,FTYPE)
      CHARACTER*(*) LNAMES(*),LTYPES(*),FTYPE
      CHARACTER     RECORD*120,CLINE*30,CHOR*30,CHTYPE*8,CPTYPE*8
      IOS=0
      REWIND (IUNIT)
C--Read through the file to see how many seismic lines it contains.
C--FTYPE....FILE TYPE. CVM, ICP OR GWS IS RECOGNIZED.  (INPUT)
C--NLINES...NUMBER OF LINES FOUND.                     (OUTPUT)
C--LNAMES().NAMES OF THE LINES RETURNED.               (OUTPUT)
C--LTYPES()..WHAT ARE THE LINE TYPES RETURNED.         (OUTPUT)
      NLINES=0
 1    READ (IUNIT,'(A120)',END=9) RECORD
      IF(FTYPE(1:3).EQ.'ICP') THEN
       IF (RECORD(2:11).NE.'*LINE NAME') GOTO 1
       READ (RECORD,3) CLINE,CPTYPE
       CHTYPE='ICP'
 3     FORMAT (14X,A30,18X,A8)
      ELSE IF(FTYPE(1:3).EQ.'GWS') THEN
       IF (RECORD(2:6).NE.'PICKS') GOTO 1
       READ (RECORD,5) CLINE,CHOR,CHTYPE(1:4),CPTYPE(1:4)
 5     FORMAT (6X,A30,A30,A4,A4)
      ELSE IF(FTYPE(1:3).EQ.'CVM') THEN
       IF (RECORD(2:10).NE.'*LINENAME') GOTO 1
       J = INDEX(RECORD(1:),'=')
       CLINE=RECORD(J+1:)
       CHTYPE='CVM'
      ELSE
       IOS=1
       RETURN
      END IF
      IF(CPTYPE(1:3).EQ.'TIM') CPTYPE='TIME'
      IF(CPTYPE(1:3).EQ.'DEP') CPTYPE='DEPTH'

      IF (NLINES.LE.0) THEN
        NLINES=1
        LNAMES(NLINES)=CLINE
        LTYPES(NLINES)=CPTYPE
      ELSE
        I=-2
        DO 7 J=1,NLINES
  7     IF ((CLINE.EQ.LNAMES(J)).AND.(CPTYPE.EQ.LTYPES(J))) I=2
         IF (I.LT.0) THEN
          NLINES=NLINES+1
          LNAMES(NLINES)=CLINE
          LTYPES(NLINES)=CPTYPE
         END IF
      END IF

      GOTO 1

 9    REWIND IUNIT
      RETURN
      END
CCC
      SUBROUTINE ICPWRLN(LUNO,NLINE,MODLINE,MODTYPE,INFOTEXT,RLIMITS,
     + SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NHOR,HORIZ,
     + HORIZON,NPIECES)
      CHARACTER*(*) MODLINE,MODTYPE,INFOTEXT
      CHARACTER*(*) SPUNT,XUNITS,ZUNITS,YUNITS
      CHARACTER*(*) HORIZON(*),CHTMP
      CHARACTER     RECORD*120,LINE*120,ALPHA*10
      REAL          RLIMITS(*),RUC(*),RNC(*),X1(*),Y1(*),A1(*),SCALE(*)
      REAL          FPLABEL,TRORG
      INTEGER       NCRDS,NHOR,NSEG,NPIECES(*),HORIZ(*)
      DIMENSION ARR(4),AV(10)

      IOS=0
      IF(LUNO.LE.0) THEN
        LINE='CLS_GMF: LUN<=0'//CHAR(0)
        IOS=1
        RETURN
      END IF
C
C Position to the end of output file. Count lines,groups as we read.
      REWIND (LUNO)
      NLINE=0      !LINE COUNTER
      NGRP=0       !GROUP COUNTER FOR LAST LINE
 45   READ(LUNO,'(A80)',END=50) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 45
      IF(RECORD(ISTAR:ISTAR+4).EQ.'*PICK') THEN
       NGRP=NGRP+1
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*LINE') THEN
       NLINE=NLINE+1
       NGRP=0
       GO TO 45
      END IF
      GO TO 45
 50   CONTINUE
C
      SCX=1.0
      IF(SCALE(1).NE.0) SCX=1.0/SCALE(1)
      SCY=1.0
      IF(SCALE(2).NE.0) SCY=1.0/SCALE(2)
      ARR(1)=SCX*(RLIMITS(1)-SCALE(3))
      ARR(2)=SCX*(RLIMITS(2)-SCALE(3))
      ARR(3)=SCY*(RLIMITS(3)-SCALE(4))
      ARR(4)=SCY*(RLIMITS(4)-SCALE(4))
      NSEG=0
      DO 55 J=1,NHOR
       NSEG=NSEG+NPIECES(J)
 55   CONTINUE
      NLINE=NLINE+1    !POINTER TO CURRENT LINE
      IF(MODTYPE(1:4).EQ.'TIME') LINE='TIME'
      IF(MODTYPE(1:4).EQ.'DEPT') LINE='DEPTH'
      WRITE (LUNO,402) MODLINE,LINE
      WRITE (LUNO,'(2X,A)') INFOTEXT
      WRITE (LUNO,404) (ARR(I),I=1,4)
      WRITE (LUNO,406) SPINT,SPORG,TRORG
      WRITE (LUNO,407) XUNITS,ZUNITS,YUNITS,SPUNT
      WRITE (LUNO,408) NHOR,NSEG
      WRITE (LUNO,410) (HORIZ(J),HORIZON(J),NPIECES(J),J=1,NHOR)

  402 FORMAT (' *LINE NAME = ',A30,6X,'PICK TYPE = ',A8)
  403 FORMAT (2X,A75)
  404 FORMAT ('  MODEL LIMITS ',4(F10.3,1X))
  406 FORMAT ('  SP INTERVAL=',F7.3,' SP ORIGIN=',F7.3,
     +        '  TR ORIGIN=',F10.3)
  407 FORMAT ('  X-UNITS=',A9,' Z-UNITS=',A9,' Y-UNITS=',A9,
     +        ' SP-UNITS=',A9)
  408 FORMAT ('  NO. OF HORIZONS = ',I6,4X,' IN ',I6,' ELEMENTS')
  410 FORMAT (X,I6,2X,A30,4X,I6)
      RETURN
CCC
      ENTRY ICPWRPCK(LUNO,LSET,NSET,NATTR,FPLABEL,CHTMP,RUC,RNC,
     + SCALE,NCRDS,X1,Y1,A1)

      IOS=0
      IF(NCRDS.LT.0) RETURN
      IF(LUNO.LE.0) THEN
        LINE='ICPWRPCK: LUN<=0'
        IOS=1
        RETURN
      END IF

      NATTR=MAX(NATTR,0)
C
C Position to the end of output file. Count lines,groups as we read.
      REWIND (LUNO)
      LSET=0
      NSET =0       !NUMBER OF GROUPS IN THE LAST LINE
 80   READ(LUNO,'(A)',END=85) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 80
      IF(RECORD(ISTAR:ISTAR+4).EQ.'*PICK') THEN
       NSET=NSET+1
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*LINE') THEN
       LSET=LSET+1
       NSET=0
       GO TO 80
      END IF
      GO TO 80
 85   CONTINUE
      NSET=NSET+1   !POINTER VALUE FOR CURRENT GROUP
C
      SCX=1.0
      IF(SCALE(1).NE.0) SCX=1.0/SCALE(1)
      SCY=1.0
      IF(SCALE(2).NE.0) SCY=1.0/SCALE(2)
      SUM=0.
      DO 79 M=1,8
       SUM=SUM+RNC(M)
 79   CONTINUE
      IF(SUM.EQ.0) THEN
       RNC(1)=.2
       RNC(3)=.9
       RNC(2)=RNC(1)
       RNC(4)=RNC(3)
       RNC(5)=.9
       RNC(6)=.2
       RNC(7)=RNC(6)
       RNC(8)=RNC(1)
      END IF
      WRITE (LUNO,412) NATTR,FPLABEL,CHTMP
C
      LINE=' '
      NCH=3
      DO 103 K=1,4
       TEMP=RLIMITS(K)
       IF(K.LT.3) TEMP=SCX*(RUC(K)-SCALE(3))  !CONVERT BACK TO SHOT UNITS
       IF(K.GT.4) TEMP=SCY*(RUC(K)-SCALE(4))
       CALL NUM2ALFR(TEMP,ALPHA,*82,NCHARS)
 82    LINE(NCH:)=ALPHA
       NCH=NCH+NCHARS+2
 103  CONTINUE
      WRITE (LUNO,'(A)') LINE(:NCH)
C
      LINE=' '
      NCH=3
      DO 107 K=1,8
       CALL NUM2ALFR (RNC(K),ALPHA,*86,NCHARS)
 86    LINE(NCH:)=ALPHA
       NCH=NCH+NCHARS+2
 107  CONTINUE
      WRITE (LUNO,'(A)') LINE(:NCH)
C
      IF(NATTR.EQ.0) THEN
        WRITE(LUNO,418)(SCX*(X1(J)-SCALE(3)),
     +                  SCY*(Y1(J)-SCALE(4)),J=1,NCRDS)
      ELSE
        LINE=' '
        DO 111 J=1,NCRDS
        NCH=3
        TEMP=SCX*(X1(J)-SCALE(3))       !CONVERT TO SHOT UNITS
        CALL NUM2ALFR (TEMP,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        TEMP=SCY*(Y1(J)-SCALE(4))
        CALL NUM2ALFR (TEMP,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        DO 109 L=1,NATTR
         L1=(J-1)*NATTR+L
         CALL NUM2ALFR (A1(L1),ALPHA,*87,NCHARS)
         LINE(NCH:)=ALPHA
         NCH=NCH+10
 109    CONTINUE
        WRITE(LUNO,'(A)') LINE(1:NCH)
 87     CONTINUE
 111    CONTINUE
      END IF
C
  412 FORMAT (' *PICKS',2X,I2,2X,F10.4,2X,A8)
  414 FORMAT (2X,4G12.5)
  416 FORMAT (2X,8F8.7)
  418 FORMAT (5(2X,G11.6))
      RETURN
CCC
      ENTRY ICPRDLN(IOR,LUNI,LGET,MODLINE,MODTYPE,INFOTEXT,RLIMITS,
     + SCALE,SPUNT,SPINT,SPORG,TRORG,XUNITS,ZUNITS,YUNITS,NHOR,
     + HORIZ,HORIZON,NL,NPIECES)

      IOR  =0
      XUNITS=' '
      YUNITS=' '
      ZUNITS=' '
      SPUNT=' '
      IF(LUNI.LE.0) THEN
        LINE='ICPRDLN: LUN <= 0, FILE NOT OPEN??'
        IOR=2
        RETURN
      END IF
      LGET=MAX(LGET,0)           !IF ZERO WE RETURN 1ST LINE

      REWIND(LUNI)
      LCHK=0
 90   READ (LUNI,'(A80)',END=95) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.LE.0) GO TO 90
      IF (RECORD(ISTAR:ISTAR+4).EQ.'*LINE') THEN
        LCHK=LCHK+1
        IF(LCHK.LT.LGET) GO TO 90
        ICHK=INDEX(RECORD,'*LINE NAME =')
        IF(ICHK.NE.0) READ(RECORD(ICHK+12:ICHK+41),'(A30)') MODLINE
        ICHK=INDEX(RECORD,'PICK TYPE =')
        IF(ICHK.NE.0) LINE =RECORD(ICHK+11:)
        MODTYPE='TIME'
        IF(INDEX(LINE,'D').GT.0) MODTYPE='DEPTH'
        READ (LUNI,'(2X,A)') INFOTEXT
        IF(ICHAR(INFOTEXT(1:1)).EQ.0) INFOTEXT=' '
        READ (LUNI,'(A)') RECORD
        CALL CH2FP(RECORD(16:),NVALS,RLIMITS)
        READ (LUNI,'(A)') RECORD
        ICHK=INDEX(RECORD,'SP INTERVAL=')
        IF(ICHK.NE.0) READ(RECORD(ICHK+12:ICHK+18),'(F7.3)') SPINT
        ICHK=INDEX(RECORD,'SP ORIGIN=')
        IF(ICHK.NE.0) READ(RECORD(ICHK+10:ICHK+16),'(F7.3)') SPORG
        ICHK=INDEX(RECORD,'TR ORIGIN=')
        IF(ICHK.NE.0) READ(RECORD(ICHK+10:ICHK+16),'(F10.3)') TRORG
        READ (LUNI,'(A)') RECORD
        ICHK=INDEX(RECORD,'X-UNITS=')
        IF(ICHK.NE.0) XUNITS=RECORD(ICHK+8:ICHK+16)
        ICHK=INDEX(RECORD,'Z-UNITS=')
        IF(ICHK.NE.0) ZUNITS=RECORD(ICHK+8:ICHK+16)
        ICHK=INDEX(RECORD,'Y-UNITS=')
        IF(ICHK.NE.0) YUNITS=RECORD(ICHK+8:ICHK+16)
        ICHK=INDEX(RECORD,'SP-UNITS=')
        IF(ICHK.NE.0) SPUNT=RECORD(ICHK+9:ICHK+17)
        READ (LUNI,'(20X,I6,8X,I6)') NHOR,NL
        READ (LUNI,410) (HORIZ(J),HORIZON(J),NPIECES(J),J=1,NHOR)
        CALL PIKSCALE(2,RLIMITS(1),RLIMITS(3),1,SCALE)
        RETURN
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*PICK') THEN
        GO TO 90
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*CELL') THEN
        GO TO 90
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*VELO') THEN
        GO TO 90
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*ENDE') THEN
        IOR=1
        LINE='ICPRDLN: *ENDED ENDCOUNTERED'
        RETURN
      ELSE
        IOR=1
        LINE='ICPRDLN: *UNRECOGNIZED HEADER CARD ENCOUNTERED'
        RETURN
      END IF
 95   CONTINUE
      IOR=1
      RETURN
CCC
      ENTRY ICPRDPCK (IOR,LUNI,LGET,NGET,NATTR,FPLABEL,CHTMP,RUC,
     + RNC,NCRDS,X1,Y1,A1,SCALE)

      IOR  =0
      IF(LUNI.LE.0) THEN
        IOR=2
        RETURN
      END IF

      REWIND(LUNI)
      LGET =MAX(LGET,0)      !SEQUENTIAL LINE TO RETRIEVE, 0==>IGNORE
      NGET =MAX(NGET,1)      !SEQUENTIAL PICK GROUP IN LINE TO RETRIEVE
      LN   =0
      NSEQ =0                !SEQUENTIAL COUNTER
      NCRDS=0                !COUNT OF CARDS RETURNED
      NATTR=MAX(NATTR,0)     !EXTRA DATA PTS FOR EACH X,Y RETURNED
 110  READ (LUNI,'(A80)',END=115) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.LE.0) GO TO 110
      IF (RECORD(ISTAR:ISTAR+4).EQ.'*LINE') THEN
        LN=LN+1
        GO TO 110
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*PICK') THEN
        NSEQ=NSEQ+1
        IF(LN.LT.LGET .OR. NSEQ.LT.NGET) GO TO 110
        CALL CH2FP(RECORD(ISTAR+7:22),NVALS,AV)
        IF(NVALS.GE.1) NATTR=NINT(AV(1))
        IF(NVALS.GE.2) FPLABEL=AV(2)
        READ(RECORD(26:),'(A)') CHTMP
        NATTR=MAX(0,NATTR)
        READ(LUNI,*) (RUC(I),I=1,4) !old user coordinate limits
        CALL PIKSCALE(2,RUC(1),RUC(3),1,SCALE)
        READ(LUNI,*) (RNC(I),I=1,8) !old screen coordinate limits
        GO TO 120
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*CELL') THEN
        GO TO 110
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*VELO') THEN
        GO TO 110
      ELSE IF(RECORD(ISTAR:ISTAR+4).EQ.'*ENDE') THEN
        LINE='ICPRDLN: *ENDED ENDCOUNTERED'//CHAR(0)
        RETURN
      ELSE
        IOR=4
        LINE='ICPRDLN: *UNRECOGNIZED HEADER CARD ENCOUNTERED'//CHAR(0)
        LINE=RECORD
        RETURN
      END IF
 115  CONTINUE
      IOR=1
 116  RETURN
C READ IN ASCII IMAGE AND DECODE IT.

 120  CONTINUE
 122  READ (LUNI,'(A80)',END=123) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.NE.0) GO TO 123       !HEADER CARD ENCOUNTERED
      CALL CH2FP(RECORD,NVALS,AV)
      L1=NCRDS*NATTR + 1
      L2=L1 + MIN(NATTR - 1,(NVALS -3))
      IF(NVALS.GE.1) X1(NCRDS+1)=AV(1)
      IF(NVALS.GE.2) Y1(NCRDS+1)=AV(2)
      IF(NVALS.GE.3) THEN
        DO 173 L=L1,L2
         A1(L)=AV(L-L1+3)
  173   CONTINUE
      END IF
      NCRDS=NCRDS+1
      GO TO 122
 123  CONTINUE
      CALL PIKSCALE(NCRDS,X1,Y1,1,SCALE)
      RETURN
      END
CCC
C IOS=1  INVALID UNIT NUMBER
C IOS=2  NDOF IS < 1
C IOS=3  VELOCITY MODEL ALREADY EXISTS IN THIS FILE. LIMIT OF 1.
      SUBROUTINE ICPWRVM(IOS,LUNO,NDOF,SCALE,NCRDS,VBUF)
      CHARACTER  LINE*80,RECORD*120,ALPHA*10
      INTEGER    ID,ITYP
      REAL       X1,Y1,A1
      DIMENSION  VBUF(*),A1(6),SCALE(*),AV(10)
      IOS=0
      LINE=' '
      RECORD=' '
      IF(LUNO.LE.0) THEN
        IOS=1
        RETURN
      END IF
      IF(NDOF.LT.1) THEN
        IOS=2
        RETURN
      END IF
C
C Position to the end of output file.
      REWIND (LUNO)
      NSET=0
 80   READ(LUNO,'(A80)',END=85) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 80
      IF(RECORD(ISTAR:ISTAR+8).EQ.'*VELOCITY') THEN
       NSET=NSET+1
      END IF
      GO TO 80
 85   CONTINUE
      NSET=NSET+1   !POINTER VALUE FOR CURRENT VELOCITY GROUP
      IF(NSET.GT.1) THEN
        IOS=3
        RETURN
      END IF
C
      WRITE (LUNO,'('' *VELOCITY  '',I2)') NDOF
      IF(NCRDS.LE.0) RETURN
C
      SCX=1.0
      IF(SCALE(1).NE.0) SCX=1.0/SCALE(1)
      SCY=1.0
      IF(SCALE(2).NE.0) SCY=1.0/SCALE(2)
      LINE=' '
      N=1
      DO 103 J=1,NCRDS
        X1  =(VBUF(N)-SCALE(3))*SCX
        N   =N+1
        Y1  =(VBUF(N)-SCALE(4))*SCY
        N   =N+1
        ID  =NINT(VBUF(N))
        N   =N+1
        ITYP=NINT(VBUF(N))
        N   =N+1
        ITYP=MIN(ITYP,9)
        ID  =MIN(ID,999)
        LINE=' '
        NCH=3
        CALL NUM2ALFR (X1,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        CALL NUM2ALFR (Y1,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        WRITE(LINE(NCH:NCH+2),'(I3)') ID
        NCH=NCH+5
        WRITE(LINE(NCH:NCH),'(I1)') ITYP
        NCH=NCH+5
        DO 96 L=1,NDOF
         CALL NUM2ALFR (VBUF(N),ALPHA,*87,NCHARS)
         N   =N+1
         LINE(NCH:)=ALPHA
         NCH=NCH+10
 96     CONTINUE
 87     CONTINUE
        NCH=NCH-10+NCHARS
        WRITE(LUNO,'(A)') LINE(1:NCH)
 103  CONTINUE
      RETURN
CCC
      ENTRY ICPRDVM(IOS,LUNI,NDOF,SCALE,NCRDS,VBUF)
      IOS=0
      LINE=' '
      RECORD=' '
      IF(LUNI.LE.0) THEN
        IOS=1
        RETURN
      END IF
C
C Position to the *VELOCITY card.
      REWIND (LUNI)
      NCRDS=0
 90   READ(LUNI,'(A)',END=105) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 90
      IF(RECORD(ISTAR:ISTAR+8).EQ.'*VELOCITY') THEN
       CALL CH2FP(RECORD(ISTAR+9:),NVALS,AV)
       IF(NVALS.GE.1) NDOF=NINT(AV(1))
       IF(NDOF.LT.1) THEN
        IOS=2
        RETURN
       END IF
      ELSE
       GO TO 90
      END IF
      N =0
      M =1
 95   CONTINUE
      READ(LUNI,'(A)',END=105) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.NE.0) GO TO 105
      N=N+1
      CALL CH2FP(RECORD,NVALS,AV)
      IF(NVALS.GE.1) X1=AV(1)
      IF(NVALS.GE.2) Y1=AV(2)
      IF(NVALS.GE.3) RID=AV(3)
      IF(NVALS.GE.4) RITYP=AV(4)
      VBUF(M)=X1
      M=M+1
      VBUF(M)=Y1
      M=M+1
      VBUF(M)=RID
      M=M+1
      VBUF(M)=RITYP
      M=M+1
      DO 161 L=1,NDOF
       VBUF(M)=AV(L+4)
       M=M+1
 161  CONTINUE
      NCRDS=N
      GO TO 95
 105  CONTINUE
      INC=4+NDOF
      CALL PIKSCALE(NCRDS,VBUF(1),VBUF(2),INC,SCALE)
C
      RETURN
      END
CCC
      SUBROUTINE ICPWRCV(IOS,LUNO,SCALE,NCRDS,VBUF)
      CHARACTER  LINE*80,RECORD*120,ALPHA*10
      INTEGER    ID,ITYP
      REAL       X1,Y1
      DIMENSION  VBUF(*),SCALE(*),AV(10)
      IOS=0
      LINE=' '
      RECORD=' '
      IF(LUNO.LE.0) THEN
        IOS=1
        RETURN
      END IF
C
C Position to the end of output file.
      REWIND (LUNO)
      NSET=0
 80   READ(LUNO,'(A80)',END=85) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 80
      IF(RECORD(ISTAR:ISTAR+4).EQ.'*CELL') THEN
       NSET=NSET+1
      END IF
      GO TO 80
 85   CONTINUE
      NSET=NSET+1   !POINTER VALUE FOR CURRENT VELOCITY GROUP
      IF(NSET.GT.1) THEN
        IOS=3
        RETURN
      END IF
C
      WRITE (LUNO,'('' *CELL'')')
      IF(NCRDS.LE.0) RETURN
C
      LINE=' '
      N=1
      SCX=1.0
      IF(SCALE(1).NE.0) SCX=1.0/SCALE(1)
      SCY=1.0
      IF(SCALE(2).NE.0) SCY=1.0/SCALE(2)
      DO 94 J=1,NCRDS
        X1  =(VBUF(N)-SCALE(3))*SCX
        N   =N+1
        Y1  =(VBUF(N)-SCALE(4))*SCY
        N   =N+1
        ID  =NINT(VBUF(N))
        N=N+1
        ID  =MAX(ID,1)
        ID  =MIN(ID,99)
        LINE=' '
        NCH=3
        CALL NUM2ALFR (X1,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        CALL NUM2ALFR (Y1,ALPHA,*87,NCHARS)
        LINE(NCH:)=ALPHA
        NCH=NCH+11
        WRITE(LINE(NCH:NCH+1),'(I2)') ID
        NCH=NCH+1
 87     CONTINUE
        WRITE(LUNO,'(A)') LINE(1:NCH)
 94   CONTINUE
      RETURN
CCC
      ENTRY ICPRDCV(IOS,LUNI,SCALE,NCRDS,VBUF)
      IOS=0
      LINE=' '
      RECORD=' '
      IF(LUNI.LE.0) THEN
        IOS=1
        RETURN
      END IF
C
C Position to the *VELOCITY card.
      REWIND (LUNI)
      NCRDS=0
 90   READ(LUNI,'(A)',END=105) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.EQ.0) GO TO 90
      IF(RECORD(ISTAR:ISTAR+4).NE.'*CELL') GO TO 90
      N =0
      M =1
 95   CONTINUE
      READ(LUNI,'(A)',END=105) RECORD
      ISTAR=INDEX(RECORD(:10),'*')
      IF(ISTAR.NE.0) GO TO 105
      N=N+1
      CALL CH2FP(RECORD,NVALS,AV)
      IF(NVALS.GE.1) X1=AV(1)
      IF(NVALS.GE.2) Y1=AV(2)
      IF(NVALS.GE.3) RID=AV(3)
      VBUF(M)=X1
      M=M+1
      VBUF(M)=Y1
      M=M+1
      VBUF(M)=RID
      M=M+1
      NCRDS=N
      GO TO 95
 105  CONTINUE
      CALL PIKSCALE(NCRDS,VBUF(1),VBUF(2),3,SCALE)
C
      RETURN
      END
CCC
      SUBROUTINE ICPWRSC(LUNO,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
     +           HX1,HZ1,HY1)
      CHARACTER     RECORD*120,LINE*80,ALPHA*10
      DIMENSION     ARR(6)
C Save scale factors to go from x,y,z corrdinates in disk units
C to x,y,z coordinates that the user desires.
C Write this information at the current position in the file.
C I.E.    Save a convenient transformation.
C    X = xscale*(Xdisk-xp1) + hx1
C Xdisk= (X-hx1)/xscale     + xp1
C MODIFIED SO FACTORS ARE ALWAYS SAVED.
      IOS=0
      IF(LUNO.LE.0) THEN
        LINE='ICPWRSC: INVALID UNIT NUMBER'//CHAR(0)
        IOS=1
        GO TO 89
      END IF
C
      IF(XSCALE.EQ.0) XSCALE=1.
      IF(YSCALE.EQ.0) YSCALE=1.
      IF(ZSCALE.EQ.0) ZSCALE=1.
C      IF((PX1.NE.HX1) .OR. XSCALE.NE.1.0) THEN
       NCH=1
       CALL NUM2ALFR (XSCALE,ALPHA,*87,NCHARS)
       LINE='  XSCALE='//ALPHA
       NCH=NCH+NCHARS+10
       CALL NUM2ALFR (PX1,ALPHA,*87,NCHARS)
       LINE(NCH:)='XPIK1='//ALPHA
       NCH=NCH+NCHARS+7
       CALL NUM2ALFR (HX1,ALPHA,*87,NCHARS)
       LINE(NCH:)='XUSR1='//ALPHA
       NCH=NCH+NCHARS+7
       WRITE (LUNO,'(A)') LINE(1:NCH)
C      END IF
 87   CONTINUE
C
C      IF((PY1.NE.HY1) .OR. YSCALE.NE.1.0) THEN
       NCH=1
       CALL NUM2ALFR (YSCALE,ALPHA,*88,NCHARS)
       LINE='  YSCALE='//ALPHA
       NCH=NCH+NCHARS+10
       CALL NUM2ALFR (PY1,ALPHA,*88,NCHARS)
       LINE(NCH:)='YPIK1='//ALPHA
       NCH=NCH+NCHARS+7
       CALL NUM2ALFR (HY1,ALPHA,*88,NCHARS)
       LINE(NCH:)='YUSR1='//ALPHA
       NCH=NCH+NCHARS+7
       WRITE (LUNO,'(A)') LINE(1:NCH)
C      END IF
 88   CONTINUE
C
C      IF((PZ1.NE.HZ1) .OR. ZSCALE.NE.1.0) THEN
       NCH=1
       CALL NUM2ALFR (ZSCALE,ALPHA,*90,NCHARS)
       LINE='  ZSCALE='//ALPHA
       NCH=NCH+NCHARS+10
       CALL NUM2ALFR (PZ1,ALPHA,*90,NCHARS)
       LINE(NCH:)='ZPIK1='//ALPHA
       NCH=NCH+NCHARS+7
       CALL NUM2ALFR (HZ1,ALPHA,*90,NCHARS)
       LINE(NCH:)='ZUSR1='//ALPHA
       NCH=NCH+NCHARS+7
       WRITE (LUNO,'(A)') LINE(1:NCH)
C      END IF
 90   CONTINUE
      RETURN
 89   WRITE(6,*) LINE
      RETURN
CCC
      ENTRY ICPRDSC(LUNI,XSCALE,ZSCALE,YSCALE,PX1,PZ1,PY1,
     +      HX1,HZ1,HY1)
C Search for scale factors to go from x,y corrdinates in pick units
C to x,y coordinates in the coordinate system of some header word.
      NHX=0
      NHY=0
      XSCALE=1.0
      ZSCALE=1.0
      YSCALE=1.0
      PX1=0.
      PZ1=0.
      PY1=0.
      HX1=0.
      HZ1=0.
      HY1=0.
      IOS=0
      IF(LUNI.LE.0) THEN
        LINE='ICPRDSC: INVALID UNIT NUMBER'//CHAR(0)
        IOS=1
        GO TO 69
      END IF
C
 15   READ(LUNI,'(A)') RECORD
      ICHKX=INDEX(RECORD,'XSCALE=')
      ICHKZ=INDEX(RECORD,'ZSCALE=')
      ICHKY=INDEX(RECORD,'YSCALE=')
      IF(ICHKX.LE.0 .AND. ICHKY.LE.0 .AND. ICHKZ.LE.0) THEN
        BACKSPACE LUNI
        RETURN
      END IF
      IF(ICHKX.NE.0) THEN
       CALL CH2FP(RECORD(ICHKX+7:),NVALS,ARR)
       NHX=1
       XSCALE=ARR(1)
       PX1=ARR(2)
       HX1=ARR(3)
       GO TO 15
      ELSE IF(ICHKZ.NE. 0) THEN
       CALL CH2FP(RECORD(ICHKZ+7:),NVALS,ARR)
       NHZ=1
       ZSCALE=ARR(1)
       PZ1=ARR(2)
       HZ1=ARR(3)
       GO TO 15
      ELSE IF(ICHKY.NE. 0) THEN
       CALL CH2FP(RECORD(ICHKY+7:),NVALS,ARR)
       NHY=1
       YSCALE=ARR(1)
       PY1=ARR(2)
       HY1=ARR(3)
       GO TO 15
      END IF
      RETURN
 69   WRITE(6,*) LINE
      RETURN
      END
CCC
      SUBROUTINE ICP2GWS(IOS,NCRDS,X1,Y1,A1,NATTR,NHOR,NPIECES,HORIZ,
     +          NSEG,NPOINTS,NPNTR,WRK,LIST)
      INTEGER    HORIZ
      PARAMETER  (MAXHOR=99)
      DIMENSION  X1(*),Y1(*),A1(*),WRK(*),LIST(*)
      DIMENSION  HORIZ(*),NPIECES(*),NPOINTS(*),NPNTR(*)
      DIMENSION  NPTS(MAXHOR),NBIN(MAXHOR)
      IOS=0
C 1. REORDER THE ARRAYS X1,Y1,A1. USE A1 AS THE KEY FOR REORDERING.
C 2. OUTPUT NHOR,NSEG,NPIECES(),NPOINTS(),HORIZ()
C    REQUIRES THAT A1 CONTAIN CLASS AND SUBCLASS INFORMATION!
C GROUP CARDS OF SIMILAR CLASSES TOGETHER. DO NOT REODER FOR
C THE SUBCLASSES.
C NCRDS......NUMBER OF ARRAY ENTRYS IN X1(),Y1(),A1()
C NATTR......MEMORY STRIDE FOR DATA IN A1
C NHOR.......NUMBER OF DISTINCT CLASSES(HORIZONS)
C NSEG.......NUMBER OF SUB CLASSES(SEGMENTS OF HORIZONS)
C HORIZ(I)...INTEGER CLASS VALUE OF THE I'TH CLASS
C NPIECES(I).NUMBER OF SUB CLASSES IN THE I'TH CLASS
C NPOINTS(I).NUMBER OF X,Y POINTS IN THE I'TH SUBCLASS
C WRK(J).....WORK BUFFER OF LENGTH NCRDS
C LIST(J)....WORK BUFFER OF LENGTH NCRDS
      NHOR=0
      IF(NATTR.LT.1) THEN
        IOS=1
        RETURN
      END IF
      IF(NCRDS.LE.0) THEN
        IOS=2
        RETURN
      END IF
C
C 1ST...FIND THE NUMBER OF UNIQUE CLASSES
C DETERMINE THE NUMBER OF POINTS IN EACH CLASS
      IVAL=NINT(A1(1))
      SVAL=A1(2)
      NHOR=1
      HORIZ(1)=NINT(A1(1))
      NPTS(1)=1
      LIST(1)=1
      DO 50 L=2,NCRDS
       L1=(L-1)*NATTR+1
       IVAL=NINT(A1(L1))
       JTES=0
       DO 40 M=1,NHOR
        IF(IVAL.EQ.HORIZ(M)) THEN
          JTES=M
          LIST(L)=M         !ENTRY L BELONGS TO CLASS M
          NPTS(M)=NPTS(M)+1
          GO TO 41
        END IF
 40    CONTINUE
 41    CONTINUE
       IF(JTES.EQ.0) THEN
         NHOR=NHOR+1
         LIST(L)=NHOR       !ENTRY L BELONGS TO NEW CLASS
         HORIZ(NHOR)=IVAL
         NPTS(NHOR)=1
       END IF
       IF(NHOR.GT.MAXHOR) GO TO 99
 50    CONTINUE 
C
C 2ND...SORT (BIN) THE HORIZONS BY HORIZON ID.
C       STORE HORIZON SEGMENTS TOGETHER.
      NBIN(1)=0
      DO 60 J=2,NHOR
       NBIN(J)=NBIN(J-1)+NPTS(J-1)
 60   CONTINUE

      DO 70 J=1,NCRDS
        L1=(J-1)*NATTR+1
        M=LIST(J)     !CLASS OF CARD J
        MM=NBIN(M)+1  !RUNNING POINTER FOR CLASS M
        LIST(J)=MM    !CARD J WILL BE MOVED TO POSITION MM
        NBIN(M)=MM
 70   CONTINUE
      DO 80 J =1,NCRDS
        WRK(LIST(J))=X1(J)
 80   CONTINUE
      DO 90 J =1,NCRDS
        X1(J)=WRK(J)
 90   CONTINUE
      DO 95 J =1,NCRDS
        WRK(LIST(J))=Y1(J)
  95  CONTINUE
      DO 100 J =1,NCRDS
        Y1(J)=WRK(J)
 100  CONTINUE
      DO 110 J=1,NATTR
        N=J
        DO 102 L=1,NCRDS
         WRK(LIST(L))=A1(N)
         N=N+NATTR
 102    CONTINUE
        N=J
        DO 106 I=1,NCRDS
         A1(N)=WRK(I)
         N=N+NATTR
 106    CONTINUE
 110  CONTINUE
C
C COUNT THE POINTS IN EACH SEGMENT OR SUBCLASS.
      IH=-520
      IS=-50
      NH=0
      NS=0
      NUMC=0
      ISEG=1    !FIX FOR FILES WITH NO SUBCLASS DATA
      IF(NATTR.EQ.1) ISEG=0
      DO 120 J=1,NCRDS
       M=(J-1)*NATTR+1
       JH=NINT(A1(M))
       JS=NINT(A1(M+ISEG))
       IF(JH.EQ.IH) THEN
        IF(JS.EQ.IS) THEN
         NUMC=NUMC+1
         NPOINTS(NS)=NUMC
        ELSE
         IF(NS.GT.0) THEN
          NPOINTS(NS)=NUMC
         END IF
         NS=NS+1
         IS=JS
         LS=LS+1
         NUMC=1
         NPOINTS(NS)=NUMC
        END IF
       ELSE
        IF(NS.GT.0) THEN
         NPOINTS(NS)=NUMC
         NPIECES(NH)=LS
        END IF
        NH=NH+1
        NS=NS+1
        IH=JH
        IS=JS
        LS=1
        NUMC=1
        NPOINTS(NS)=NUMC
       END IF
 120  CONTINUE
      NHOR=NH
      NPIECES(NHOR)=LS
C
C--COUNT THE SUBCLASSES, AND COMPUTE A POINTER ARRAY
      NSEG=0
      NCRDS=0
      DO 130 I=1,NHOR
      DO 125 J=1,NPIECES(I)
       NSEG=NSEG+1
       NPNTR(NSEG)=NCRDS
       NCRDS=NCRDS+NPOINTS(NSEG)
 125  CONTINUE
 130  CONTINUE 
      RETURN
C
  99  CONTINUE
      IOS=3
      RETURN
CCC
      ENTRY GWS2ICP(NHOR,NPIECES,NSEG,NPOINTS,NPNTR,NCRDS,HORIZ,A1)
C GIVEN NHOR,HORIZ(NHOR),NPIECES(NHOR), AND NPOINTS()
C STORE THE VALUES FOR HORIZON AND SEGMENT IN A1 .
C NCRDS = SUM OF NPOINTS ENTRYS.
      NA=2
      NSEG=0
      NCRDS=0
      N=1
      DO 150 I=1,NHOR
      DO 145 J=1,NPIECES(I)
       NSEG=NSEG+1
       NPNTR(NSEG)=NCRDS
       DO 142 K=1,NPOINTS(NSEG)
        NCRDS=NCRDS+1
        A1(N)=FLOAT(HORIZ(I))
        A1(N+1)=J
        N=N+NA
 142   CONTINUE
 145  CONTINUE
 150  CONTINUE
      RETURN
      END
CCC
      SUBROUTINE PIKSCALE(NCRDS,X1,Y1,INC,SCALE)
      DIMENSION X1(*),Y1(*),SCALE(*)
C SCALE PICKS FROM DISK UNITS TO DISPLAY UNITS
C INC.....A MEMORY STRIDE THRU X1 AND Y1
C SCALE...4 ELEMENT SCALING ARRAY XSCALE,YSCALE,XORIG,YORIG
      IF(NCRDS.LE.0) RETURN
      SCX =SCALE(1)
      SCY =SCALE(2)
      DELX=SCALE(3)
      DELY=SCALE(4)
      IF(SCX.EQ.0.) SCX=1.0
      IF(SCY.EQ.0.) SCY=1.0
      L=1
      DO 10 N=1,NCRDS
       X1(L)=X1(L)*SCX + DELX
       Y1(L)=Y1(L)*SCY + DELY
       L=L+INC
 10   CONTINUE
      SCALE(1)=SCX
      SCALE(2)=SCY
      RETURN
      ENTRY PIKUNSC(NCRDS,X1,Y1,INC,SCALE)
C UNSCALE PICKS FROM DISPLAY UNITS TO DISK UNITS
      IF(NCRDS.LE.0) RETURN
      SCX =SCALE(1)
      SCY =SCALE(2)
      DELX=SCALE(3)
      DELY=SCALE(4)
      IF(SCX.EQ.0.) SCX=1.0
      IF(SCY.EQ.0.) SCY=1.0
      SCXL=1.0/SCX
      SCYL=1.0/SCY
      L=1
      DO  20 N=1,NCRDS
       X1(L)=(X1(L) - DELX)*SCXL
       Y1(L)=(Y1(L) - DELY)*SCYL
       L=L+INC
 20   CONTINUE
      RETURN
      END
