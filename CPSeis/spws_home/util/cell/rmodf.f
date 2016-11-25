C23456789012345678901234567890123456789012345678901234567890123456789012
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
C       SUBROUTINE RMOD
c     1    ,readonly,err=999)
C***************************** COPYRIGHT NOTICE ************************
C*                                                                      
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION             
C*                              OF CONOCO INC.                          
C*                      PROTECTED BY THE COPYRIGHT LAW                  
C*                          AS AN UNPUBLIBW2ED WORK
C*                                                                      
C***************************** COPYRIGHT NOTICE ************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                              CONOCO, INC.
C
C
CPrimitive name: RMOD
C        Author: D W Hanson
C       Written: 92/09/09
C  Last revised: 98/12/04 Day
C
C  Purpose:  To read and write velocity models for depth migration.
C-----------------------------------------------------------------------
C\PROG DOC
C-----------------------------------------------------------------------
C                           callING SEQUENCE
C      call RMODRLAY(HFILE,DFILE,TFILE,TYPE
C     1,CXI,CZI,CXO,CZO,n_cord,x_cord,CORD,XMIN,XMAX,ZMIN,ZMAX,ZDATUM
C     1,MB,MXB,NB,IMB,ITB,IXB,NXB,XB,ZB,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL
C     1,NCV,ICV,XCV,ZCV,NXG,XGMIN,XGINC,NZG,ZGMIN,ZGINC,LU,TITLE,*)
C
C ARGUMENTS
C Name    Type*  Valid  Description         *Type: I=IN, O=OUT, B=BOTH
C ----    ----   -----  -----------
C HFILE    I     CHAR   HEADER FILE NAME - THIS POINTS TO OTHER FILES
C DFILE    O     CHAR   DATA FILE NAME - READ FROM HFILE
C TFILE    O     CHAR   TRANSFORM FILE NAME - READ FROM HFILE
C TYPE     O     CHAR   TYPE OF DATA FILE - LAYER, GRID
C n_cord    I     INT>0  NUMBER OF COORDINATE TRANSFORMS
C CORD     I   CH ARRAY DIMENSION (n_cord)*16 SEE NOTE 2 BELOW
C x_cord    O     REAL   DIMENSION (2,n_cord) COORDINATE VALUES
C CXI      O     CHAR   ORIGINAL INPUT X COORIDNATES
C CZI      O     CHAR   ORIGINAL INPUT Z COORIDNATES
C CXO      I     CHAR   DESIRED OUTPUT X COORIDNATES
C CZO      I     CHAR   DESIRED OUTPUT Z COORIDNATES
C XMODMIN  O     REAL   MIN X VALUE FOR LAYERED MODEL
C XMODMAX  O     REAL   MAX X VALUE FOR LAYERED MODEL
C ZMODMIN  O     REAL   MIN Z VALUE FOR LAYERED MODEL
C ZMODMAX  O     REAL   MAX Z VALUE FOR LAYERED MODEL
C MVG      I     INT>0  MAX # OF POINTS IN GRIDDED FILE
C NXG      O     INT>0  # OF X VALUES IN GRIDDED FILE
C XGMIN    O     REAL   MINIMUM X VALUE IN GRIDDED FILE
C XGINC    O     REAL   X VALUE SPACING IN GRIDDED FILE
C NZG      O     INT>0  # OF Z VALUES IN GRIDDED FILE
C ZGMIN    O     REAL   MINIMUM Z VALUE IN GRIDDED FILE
C ZGINC    O     REAL   Z VALUE SPACING IN GRIDDED FILE
C VG       O     ARRAY  GRIDDED SLOWNESS DIMENSION (NZ,NX,NY)
C
C MB       I     INT>0  MAX # OF ELEMENTS IN ARRAYS
C                       IMB,IXB,NXB,IMV,ITV,IXV,NXV,XCV,ZCV
C MXB      I     INT>0  MAX # OF ELEMENTS IN ARRAYS
C                       XB,ZB,XV,ZV,VEL
C NB       O     INT    # OF BOUNDARIES IN LAYERED MODEL
C IMB      O     ARRAY  BOUNDARY TYPE 1
C ITB      O     ARRAY  BOUNDARY TYPE 2
C IXB      O     ARRAY  BOUNDARY POINTER WITHIN X,Z ARRAYS
C NXB      O     ARRAY  # OF POINTS IN EACH BOUNDARY
C     BOUNDARY I OCCUPIES ELEMENTS IXB(I)+1 - IXB(I)+NXB(I) OF XB,ZB
C XB       O     ARRAY  BOUNDARY X VALUES
C ZB       O     ARRAY  BOUNDARY Z VALUES
C NV       O     INT    # OF VELOCITY DEFINITIONS
C IMV      O     ARRAY  VELOCITY ID NUMBER
C ITV      O     ARRAY  VELOCITY TYPE
C 0 = CONSTANT, 1 = V(X), 2 = V(Z), 3 = V(X,Z)
C IXV      O     ARRAY  POINTER ARRAY FOR XV,ZV,VEL
C NXV      O     ARRAY  # OF X,Z,VEL POINTS IN EACH VELOCITY DEFINITION
C     DEFINITION I OCCUPIES ELEMENTS IXV(I)+1 - IXV(I)+NXV(I) OF XV,ZV,V
C XV       O     ARRAY  VELOCITY DEFINITION X VALUES
C ZV       O     ARRAY  VELOCITY DEFINITION Z VALUES
C VEL      O     ARRAY  VELOCITY VALUES
C NCV      O     ARRAY  # OF CELL POINTERS IN MODEL
C XCV      O     ARRAY  CELL POINTER X VALUES
C ZCV      O     ARRAY  CELL POINTER Z VALUES
C LU       I     INT    LOGICAL UNIT # TO PRINT INFO TO LU<0 - NO PRINT
C *        I            ERROR RETURN ADDRESS
C
C  THE FOLLOWING PARAMETERS ARE USED FOR CELLED MODELS
C MC       I     INT>0  MAX # OF ELEMENTS IN ARRAYS
C                       IMC,IXC,NXC
C MXC      I     INT>0  MAX # OF ELEMENTS IN ARRAYS
C                       XC,ZC
C NC       O     INT    # OF CELLS IN MODEL
C IMC      O     ARRAY  CELL VELOCITY TYPE
C                CELL IC USES VELOCITY # IMC(IC)
C IXC      O     ARRAY  CELL POINTER WITHIN X,Z ARRAYS
C NXC      O     ARRAY  # OF POINTS IN EACH CELL
C     CELL I OCCUPIES ELEMENTS IXC(I)+1 - IXC(I)+NXC(I) OF XC ETC.
C XC       O     ARRAY  CELL X VALUES
C ZC       O     ARRAY  CELL Z VALUES
C
C-----------------------------------------------------------------------
C                                 NOTES
C  NOTE 1
C
C  This set of subroutines is used for reading and writing model
C  files.  Several different model formats are supported
C  including gridded models, layered models and celled models.
C
C  Velocity models consist of three components, the header, data
C  and TRANSFORM sections, which may or maynot be in the same
C  file. The header section conatins general information and
C  points to the data and TRANSFORM sections. The data section
C  contains the model information itself.  The TRANSFORM section
C  contains information on how to change from one coordinate
C  system to another.
C
C  The header and TRANSFORM sections of any model are in ascci
C  files. For layered or celled models the data section is also
C  in an ascii file. For gridded models the data section is a
C  direct access unformatted file whose records are NZ long.
C
C  The header seciton contains the following parameters
C
C  FILE - file name containg data section.
C  If FILE is not defined or FILE = SAME the data section is
C  assumed to be in the same file as the Header section.
C
C  TRANSFORM - name of file containing coordinate TRANSFORM
C  If TRANSFORM is not defined or TRANSFORM = SAME the TRANSFORM
C  section is assumed to be in the same file as the Header
C  section.
C
C  TYPE - type of file for data section.  LAYER and GRID are
C  valid.
C
C  XCOORDINATE - type of ccordinate system x values are in.
C  XBASEMENT hw(17), XANNOTATION hw(37), and XGRID, hw(7) are
C  valid.  XMODMIN, XMODMAX, XMIN, and XINC are in these units.
C
C  ZCOORDINATE - type of ccordinate system z values are in.
C  DEPTH and TIME are valid.
C  ZMODMIN, ZMODMAX, ZMIN, AND ZINC are in these units.
C
C  XMODMIN - minimum X value for layered model.
C  XMODMAX - maximum X value for layered model.
C  ZMODMIN - minimum Z value for layered model.
C  ZMODMAX - maximum Z value for layered model.
C
C  NX - Number of X grid locations in gridded model.
C  XMIN - Minimum X location in gridded model.
C  XINC - X location spacing in gridded model
C  NZ - Number of Z grid locations in gridded model.
C  ZMIN - Minimum Z location in gridded model.
C  ZINC - Z location spacing in gridded model
C
C  Note the header section may also have definitions for YMODMIN,
C  YMODMAX, NY, YMIN, YINC.  Hoowever these are not currently
C  used.
C
C  The TRANSFORM seciton contains the following parameters
C
C  XBASEMENT1,   XBASEMENT2,   two arbitray X locations in hw (17) units
C  XANNOTATION1, XANNOTATION2, the same two X locations in hw (37) units
C  XGRID1,       XGRID2,       the same two X locations in hw (7) units
C
C  DEPTH1,       DEPTH2,       two arbitray depth values.
C  TIME1,        TIME2,        two equivalent time values.
C
C  Note the TRANSFORM section may also contain the parameters
C  YBASEMENT1, YBASEMENT2, YANNOTATION1, YANNOTATION2, YGRID1,
C  and YGRID1.  However these are not curently used.
C
C  For a layer model the data section will itself have three
C  sections.  The PICKS, CELL pointer and cell VELOCIT secitons.
C  Note all three sections may have independant definitions of
C  what coordinate system the X,Z values are in.  These
C  defineitions appear on the title card of each section.  If
C  these are not included the definition used in the header
C  section is used.
C
C  The PICKS section will start with a card with the character
C  string *PICKS in columns 2 - 7. it contains the X,Z locations
C  of boundaries within the layered model.
C
C  The CELL pointer section will start with a card with the
C  character string *CELL in columns 2 - 6. it contains the X,Z
C  locations of cell pointers.  These are points within cells and
C  a flag indicating what velocity to use within that cell.
C
C  The VELOCITY section will start with a card with the character
C  string *VELOCITY in columns 2 - 10.  It contains a list of
C  velocity values to use within the model.
C
C
C  SIMPLE LAYERED MODEL FILE
C
C    #=" *HEADER SECTION"
C    FILE="V0.MOD"         TYPE="LAYER"
C    TRANSFORM="V0.MOD"  XCOORDINATE="XANNOTATION"  ZCOORDINATE="DEPTH"
C    XMODMIN=-50.0000       XMODMAX=150.000
C    ZMODMIN=0.000000E+00   ZMODMAX=20000.0
C    NX=101     XMIN=0.000000E+00   XINC=1.00000
C    NZ=101     ZMIN=0.000000E+00   ZINC=1.00000
C    #=" *TRANSFORM SECTION"
C    XBASEMENT1=0.000000E+00       XBASEMENT2=100.000
C    XANNOTATION1=0.000000E+00     XANNOTATION2=1.00000
C    XGRID1=0.000000E+00           XGRID2=1.00000
C    DEPTH1=0.000000E+00           DEPTH2=1.00000
C    TIME1=0.000000E+00            TIME2=1.00000
C   *PICKS     NDOF=4  XCOORDINATE=XANNOTATION  ZCOORDINATE=DEPTH
C           -50.0000        1000.0000        1        1
C           150.0000        1000.0000        1        1
C           -50.0000        2000.0000        2        1
C           150.0000        2000.0000        2        1
C   *CELL      NDOF=3  XCOORDINATE=XANNOTATION  ZCOORDINATE=DEPTH
C            50.0000         500.0000        1
C            50.0000        1500.0000        2
C            50.0000        2500.0000        3
C   *VELOCITY  NDOF=5  XCOORDINATE=XANNOTATION  ZCOORDINATE=DEPTH
C            50.0000           0.0000        1        1        5000.0000
C            50.0000           0.0000        2        1        5500.0000
C            50.0000           0.0000        3        1        6000.0000
C   *END
C
C  NOTE 2
C
C  TRANSFORMation between different coordinate systems is defined
C  by the parmaters CXI,CZI,CXO,CZO,n_cord,x_cord,CORD
C  CXI,CZI are character variables defineing the input x,z coordinates
C  CXO,CZO are character variables defineing the output x,z coordinates
C  n_cord is the number of coordinate TRANSFORMs within x_cord and cord
C  CORD is a character array dimensioned CORD(n_cord)*16
C  this should be assigned before reading or writing any model files
C  A typical fortran setup would be
C  Note we leave 3 blank spaces in case the header file has some
C  other TRANSFORM.
C
C      PARAMETER (m_cord=11)
C      CHARACTER *16 CXI,CZI,CXO,CZO,CORD(m_cord)
C      DIMENSION x_cord(2,m_cord)
C      DATA CYO/'YBASEMENT'/,n_cord/m_cord/,CORD/'XBASEMENT','XANNOTATION
C     1,'XGRID','YBASEMENT','YANNOTATION','YGRID','DEPTH','TIME'
C     1,' ',' ',' '/
C
C  This allows TRANSFORMation between any of the coordinates defined wit
C  cord.  Note you can TRANSFORM between any 2 cordinates,
C  e.g. XBASEMENT and DEPTH, wether it makes sense or not.
C
C  NOTE 3
C
C  RMODRLAY WILL READ ALL INFO IN A LAYERED MODEL FILE
C  OTHER USEFUL ROUTINES ARE
C  RBND READ ONLY BOUNDARY INFO FROM A LAYERED MODEL FILE
C  RMORALL READ MODEL INFO REGARDLESS OF TYPE - GRID LAYERED CELLED
C  RMOWALL WRITE MODEL INFO REGARDLESS OF TYPE - GRID LAYERED CELLED
C  RMODRDHD READ HEADER FILE
C  RMODWRHD WRITE HEADER FILE
C  RMODRTRN READ TRANSFORM FILE
C  RMODWTRN WRITE TRANSFORM FILE
C  RMODRXCV READ X,Z CARDS, CELL POINTER CARDS AND VELOCITY CARDS
C  RMODWXCV WRITE X,Z CARDS, CELL POINTER CARDS AND VELOCITY CARDS
C  RMODRXZV READ X,Z CARDS
C  RMODWXZV WRITE X,Z CARDS
C  RMODRXZV READ CELL POINTER CARDS
C  RMODWXZV WRITE CELL POINTER CARDS
C  RMODRXZV READ VELOCITY CARDS
C  RMODWXZV WRITE VELOCITY CARDS
C  RMODRVAL READ VALUES
C  RMODWVAL WRITE VALUES
C  RMODWEND WRITE *END CARD
C  RMODOPEN OPEN FILE
C  RMODCLOS CLOSE FILE  (NOTE DOES NOT DISPOSE FILE FROM CRAY TO VAX)
C  RMODLTOG CONVERT LAYERED MODEL TO GRIDDED MODEL
C
C**********************************************************************
C  TO READ A COMPLETE FILE
C  RMODRRHD - READ HEADER FILE AND TRANSFORM FILE
C  RMODOPEN - RETURNS UNIT #
C    REPEAT RMODRCHD, RMORWVAL FOR PICKS, CELL POINTER,
C         AND VELOCITY SECTIONS.
C  RMODRCHD - READ DATA SECTION TITLE CARD
C  RMODRVAL - READ DATA CARDS INTO A SINGLE ARRAY
C
C  callS TO READ HEADER FILE
C
C      call RMODRDHD(HFILE,DFILE,WDTYPE,MFILE,TFILE,TYPE
C     1,CXI,CYI,CZI,CXO,CYO,CZO,m_cord,n_cord,x_cord,CORD
C     1,XMODMIN,XMODMAX,YMODMIN,YMODMAX,ZMODMIN,ZMODMAX,ZDATUM
C     1,NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC,NDIM,LU,*999)
C
C  callS TO READ TRANSFORM FILE
C      call RMODRTRN(TFILE,m_cord,n_cord,x_cord,CORD,*)
C
C  call TO OPEN FILE FOR READING
C      NOTE IF INPUT FILE DFILE IS CURRENTLY OPEN
C  RMODOPEN WILL NOT REOPEN FILE, IT ONLY RETURNS UNIT NUMBER IFILE
c        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
c     1,'IEEE',n0,ierr)
C      IF IERR .NE. 0 ERROR ON CLOSE OR DISPOSE
C
C  call TO FIND AND READ THE TITLE CARD OF A DATA SECTION
C      call RMODFCRD(IFILE,IREWIND,STR,CX,CZ,NDOF,*)
C
C  call TO READ A MAX OF MX CARDS WITH NDOF VALUES INTO ARRAY X(NDOF,NX)
C  NUMBER OF VALUES READ IS NX
C      call RMODRVAL(IFILE,NDOF,MX,NX,X,*)
C
C  callS TO READ BOUNDARY, CELL POINTER AND VELOCITY CARDS
C
C  READ BOUNDARY CARDS - *PICKS CARDS
C      call RMODRXZV(IFILE,CXP,CZP
C     1,2,MB,MXB,NB,IMB,ITB,IXB,NXB,XB,ZB,VEL,*999)
C  READ CELL POINTER CARDS - * CELL CARDS
C      call RMODRXZV(IFILE,CXC,CZC
C     1,MB,MB,ICV,ICV,ICV,ICV,XCV,ZCV,VEL,*999)
C  READ VELOCITY CARDS - *VELOCITY CARDS
C      call RMODRXZV(IFILE,CXV,CZV
C     1,3,MB,MXB,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,*999)
C
C**********************************************************************
C  TO WRITE A COMPLETE FILE
C  RMODWRHD
C  RMODWTRN - WRITE TRANSFORM FILE
C  RMODOPEN - RETURNS UNIT #
C    REPEAT RMODWCHD, RMOWWVAL FOR PICKS, CELL POINTER,
C         AND VELOCITY SECTIONS.
C  RMODWCHD - WRITE DATA SECTION TITLE CARD
C  RMODWVAL - WRITE DATA CARDS FROM A ARRAY
C  RMODWEND - WRITE *END CARD
C
C  call TO WRITE HEADER FILE
C      call RMODWRHD(HFILE,DFILE,WDTYPE,MFILE,TFILE,TYPE,STAT,ITRANS
C     1,CXI,CYI,CZI,CXO,CYO,CZO,m_cord,n_cord,x_cord,CORD
C     1,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ZDATUM
C     1,NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC,*999)
C
C  call TO WRITE TRANSFORM FILE
C      call RMODWTRN(TFILE,m_cord,n_cord,x_cord,CORD,*)
C
C  call TO WRITE HORIZON FILE
C      call RMODWHOR(HFILE,IBW1,IBW2,IBW3
C     1,NBH,IBID,NBSEG,BNAME,BCOLOR,*)
C  call TO OPEN FILE FOR WRITING
C      NOTE IF OUPUT FILE DFILE IS CURRENTLY OPEN
C  RMODOPEN WILL NOT REOPEN FILE, IT ONLY RETURNS UNIT NUMBER IFILE
c        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
c     1,'IEEE',n0,ierr)
C      IF (IERR .NE. 0 THERE IS AN ERROR ON OPEN
C
C  call TO WRITE A TILE CARD FOR A DATA SECTION
C      call RMODWCHD(IFILE,STR,CX,CY,CZ,NDOF)
C
C  call TO WRITE NX CARDS WITH NDOF VALUES EACH FROM ARRAY X(NDOF,NX)
C      call RMODWVAL(IFILE,NDOF,NX,X)
C
C  callS TO WRITE BOUNDARY, CELL POINTER AND VELOCITY CARDS
C
C  WRITE BOUNDARY CARDS
C      call RMODWXZV(IFILE,INIT,CXO,CZO,2,NB,IMB,ITB,IXB,NXB,XB,ZB,VEL)
C  WRITE CELL POINTER CARDS
C      call RMODWXZV(IFILE,INIT,CXO,CZO,1,NCV,ICV,ICV,XCV,ZCV,VEL)
C  WRITE VELOCITY CARDS
C      call RMODWXZV(IFILE,INIT,CXO,CZO,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL)
C
C  call TO CONVERT XMODMIN,XMODMAX,ZMODMIN,ZMODMAX,
C  BOUNDARY, CELL POINTER AND VELOCITY CARDS FROM ONE
C  COORDINATE SYSTEM (CXI,CZI) TO TO ANOTHER (CXO,CZO)
C      call RMODCNLR(XMODMIN,XMODMAX,ZMODMIN,ZMODMAX,NB,IXB,NXB,XB,ZB
C     1,NV,IXV,NXV,XV,ZV,NCV,XCV,ZCV,CXI,CZI,CXO,CZO,n_cord,x_cord,CORD)
C
C**********************************************************************
C  call TO FIND START OF DATA SECTION LABELED STR
C      call RMODFSTR(IFILE,IREWIND,NRD,STR,CARD,*)
C
C  call TO DETERMINE HOW MANY CARDS IN A DATA SECTION STARTING WITH STR
C      call RMODLSEC(IFILE,IREWIND,LSEC,STR)
C
C**********************************************************************
C  call TO CONVERT A GRID FILE TO A LAYER FILE
C  OR CREATE A GRID FILE FROM A LAYER FILE
C      call RMODGTOL(IDIR,HFILE,LFILE,GFILE,mwork,WORK,LU,*)
C  IDIR=1 LAYER TO GRID, IDIR=-1 GRID TO LAYER
C
C-----------------------------------------------------------------------
C                          REVISION HISTORY
C     Date     Author      Description
C     ----     ------      -----------
C 32. 03/06/13 Stoeckley   Rename filename from rmod.f to rmodf.f.
C 31. 98/12/04 Day         Replace get_itime1, get_idate1 with
C                          time_date_stringf
C 30. 94/05/04 Hanson      ADD HISTORY SECTION, MODIFY APEND
C 29. 93/04/29 Hanson      REMOVE CLOSE FROM RODRDDA
C 28. 93/04/20 Hanson      ADD RMODAPEN TO COPY EXTRA SECTIONS TO NEW FI
C 27. 93/04/06 Hanson      SIMPLIFY ZTOT AND GTOL callS
C 26. 93/02/12 Hanson      ADD G3DL MODEL TYPE
C 25. 93/02/09 Hanson      MODIFY GTOLGTON call
C 24. 93/01/13 Hanson      CHANGE RMODRBND call
C 23. 93/01/08 Hanson      CHANGE CELLCLVP call
C 22. 92/12/11 Hanson      ADOPT FITCELL VELOCITY CONVENTIONS
C 21. 92/12/03 Hanson      CHANGE WALL, WRHD call TO INCLUDE FILE STATUS
C 20. 92/11/20 Hanson      FIX CZO BUG IN RMODVGRD
C 19. 92/11/18 Hanson      ADD HORIZON SECTION TO MODEL FILE.
C 18. 92/10/26 Hanson      SPLIT OFF ZTOT AND GTOL
C 17. 92/10/22 Hanson      CHECK CELL VELOCITIES IN GTOL
C 16. 92/10/19 Hanson      FIX GTOV BUG
C 16. 92/10/16 Hanson      RMODWALL ABLE TO CONVERT GRID TO LAYER
C 15. 92/10/15 Hanson      CHANGE USE OF FITCELL TO DECREASE MEMORY
C 14. 92/10/08 Hanson      NEW RMODRDHD call
C 13. 92/10/03 Hanson      FIX MEMORY SIZE TO CELL IN GTON
C 12. 92/10/01 Hanson      MODIFY LTOG call
C 11. 92/09/30 Hanson      FIX CNMM BUG
C 10. 92/09/29 Hanson      CORRECT CRAY VERSION OF IERR
C                          FIX CXI=CXO IN CNV0
C                          ADD ERROR RETURN TO SHEI, WTRN, RTRN
C                          ADD IERR TO RMODOPEN RMODCLOSE
C 9.  92/09/24 Hanson      READ SHEINS FORMAT WITH COMMENTS
C 8.  92/09/23 Hanson      FIX NZINC BUG
C 7.  92/09/22 Hanson      ADD RMODGTOL
C 6.  92/09/18 Hanson      ADD INPUT TRANSFORM TO LIST
C 5.  92/09/17 Hanson      ADD WCV0 ENTRY
C 4.  92/09/15 Hanson      ADD RMODRVAL, RMODWVAL
C 3.  92/09/14 Hanson      FIX CHAR VARIABLES
C 2.  92/09/11 Hanson      FIX BUG IN WRDA
C 1.  92/09/09 Hanson      Original version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES IN THIS MODULE
C
C   SUBROUTINE NAMES IN THIS MODULE
C RMOD     RMODRDHD RMODRDGL RMODGV2  RMODGVR  RMODDRCT RMODOPEN RMODCLO
C RMODFINF RMODNSEC RMODFSEC RMODESEC RMODFEND RMODFCOP
C RMODWRHD RMODWXCV RMODRXCV RMODRTRN RMODATRN RMODWTRN RMODSHEI RMODROL
C RMODRLAY RMODRBND RMODRALL RMODRECL RMODWALL RMODWCEL RMODWRDA RMODWEN
C RMODWCHD RMODWRIT RMODWCRD RMODRDCL RMODRDSC RMODOICP RMODRCRD
C RMODRVAL RMODDCOD RMODRSTR RMODWVAL RMODWCIM RMOWWLEF RMODNCOD
C RMODFCRD RMODNCRD RMODNOLD RMODWXZV RMODRXZV RMODRDDA RMODFTCH RMODRRE
C RMODLTOG RMODRGRD RMODVGRD RMODRDCW RMODWRKB RMODWRKC RMODBPNT
C RMODCTYP RMODPRHD RMODPHDR RMODPCOR RMODPGRD RMODPLAY RMODPVG
C RMODLINE RMODCNIN RMODCNLR RMODCNVN RMODCNV9 RMODCNV0 RMODCNM4
C RMODCNMM RMODSCOR RMODFCOR RMODACOR RMODCCOR RMODLNLR
C RMODLENL RMODLENR RMODFONE RMODFCHR RMODNCHR RMODFSTS RMODFSTR RMODWPL
C RMODCAPS RMODRHOR RMODWHOR RMODSHOR RMODSHDR RMODSRTC
C
C   ENTRY NAMES IN THIS MODULE
C RMODPLIN RMODGLIN RMODPFIN RMODGFIN RMODGVI  RMODGVC  RMODCLOS
C RMODFALL RMODIOFF RMODFOFI RMODROFI RMODSOFI RMODFCLR
C
C   FUNCTION NAMES IN THIS MODULE
C RMODSPAC
C
C   COMMON BLOCK NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C
C HEADGV1  HEADLENL HEADDCOD CLOSFIL  GETLUN
C OPNFIL   HEADCCAT util_setr util_copy
C rmod_wors rmod_work CELLPRNB CELLPRNV CELLCLVP HEADGVCC HEADGVIC VXSCT
C GTOL     rmod_worl ztot_convert_velocity_n CELLVMIX CELLPRNX CELLPRNC 
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C
C  STORAGE       - NONE
C  HEAP(dynamic) - NONE
C-----------------------------------------------------------------------
C\END DOC
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdhd(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ndim,lu,*)
c  read a header file
      character *(*) hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,cord
      integer   wdtyp,nc,dbutil_putwordtype
      character crd132*132

      ierr = 0
c  set defaults
      dfile = ' '
      mfile = ' '
      tfile = ' '
      type = 'LAYER'
      cxi = ' '
      cyi = ' '
      czi = ' '
      wtype = 'VMS'
      wdtyp = 3 !assume old files are VMS type
      n0 = 0

      call rmodopen(ifile,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
      if (ierr .ne. 0) goto 999

c  find the header section
      irewind = 0
      call rmodfsts(ifile,irewind,'*HEADER',*999)

      call rmodgvc(dfile,nc,'FILE',ifile)
      call rmodgvc(wtype,nc,'WORDTYPE',ifile)
      call rmodgvc(mfile,nc,'MFILE',ifile)
      call rmodgvc(type,nc,'TYPE',ifile)
      call rmodgvc(tfile,nc,'TFILE',ifile)
      call rmodgvc(tfile,nc,'TRANSFORM',ifile)
      call rmodgvc(cxi,nc,'XCOORDINATE',ifile)
      call rmodgvc(cyi,nc,'YCOORDINATE',ifile)
      call rmodgvc(czi,nc,'ZCOORDINATE',ifile)

      if (type .eq. 'AGRID') then
        type = 'GRID'
        wtype = 'AGRID'
      endif    ! if (type .eq. 'AGRID') then
      call rmod_wordtype_ctoi(wtype,wdtyp)
      ierr = dbutil_putwordtype(ifile,wdtyp)
      call rmodcaps(cxi,cxi)
      call rmodcaps(cyi,cyi)
      call rmodcaps(czi,czi)
c      call rmodcaps(tfile,tfile)
c      call rmodcaps(dfile,dfile)
      if (cxi(1:4)   .eq. 'NONE') cxi   = 'XBASEMENT'
      if (cyi(1:4)   .eq. 'NONE') cyi   = 'YBASEMENT'
      if (czi(1:4)   .eq. 'NONE') czi   = 'DEPTH'
      if (cxi(1:1)   .eq. ' '   ) cxi   = 'XBASEMENT'
      if (cyi(1:1)   .eq. ' '   ) cyi   = 'YBASEMENT'
      if (czi(1:1)   .eq. ' '   ) czi   = 'DEPTH'
      if (tfile(1:4) .eq. 'NONE') tfile = 'SAME'
      if (dfile(1:4) .eq. 'NONE') dfile = 'SAME'
      call rmod_same_name(tfile)
      call rmod_same_name(dfile)
      call rmod_same_name(mfile)

c      print'('' hfile='',a40,/,'' dfile='',a40,/,'' tfile='',a40
c     1,/,'' type='',a16/,'' wordtype='',a16)'
c     1,hfile,dfile,tfile,type,wtype

      call rmodctyp(type,lu,*999)        ! check model type
      call rmodacor(cxi,0.,1.,m_cord,n_cord,x_cord,cord)    ! add transfo
      call rmodacor(cyi,0.,1.,m_cord,n_cord,x_cord,cord)    ! add transfo
      call rmodacor(czi,0.,1.,m_cord,n_cord,x_cord,cord)    ! add transfo
      call rmodccor(n_cord,x_cord,cord,*999)     ! check transforms for 

c  get the gridded model limits
      call rmodrdgl(hfile
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ierr)
      if (ierr .ne. 0) goto 999

      if(dfile.eq.' ') dfile = 'SAME'
      if(tfile.eq.' ') tfile = 'SAME'
c     call rmoddrct(hfile,tfile)
c     call rmoddrct(hfile,dfile)
c     call rmoddrct(hfile,wtype)
      call rmod_trupath(hfile,tfile)
      call rmod_trupath(hfile,dfile)

c  get the transformation parameters
      call rmodrtrn(tfile,m_cord,n_cord,x_cord,cord,*999)
      call rmodsrtc(n_cord,x_cord,cord)    ! sort transforms
      call rmodccor(n_cord,x_cord,cord,*999)     ! check transforms for 

      if (type .eq. 'LAYER') nyg = 1
c  set extension on data file
      if (type .eq. 'GRID' .and. dfile .eq. hfile) then
        print'(/,'' error in rmodrdhd data file and header file''
     1,'' cannot be the same when model type is grid''
     1,/,'' hfile='',a80,/,'' dfile='',a80,/,'' type='',a20)'
     1,hfile,dfile,type
        goto 999
      endif

      if (mfile(1:4) .eq. 'NONE') mfile = ' '
      if (mfile .eq. ' ') then
        call rmodfstr(ifile,irewind,nrd,'*MODIFY',crd132,*1)
        call headgvcc(crd132,mfile,nchar,'file')
      endif
    1 continue
c     if (mfile .ne. ' ') call rmoddrct(hfile,mfile)    ! add extension
      if (mfile .ne. ' ') call rmod_trupath(hfile,mfile)    ! add extens
      call rmodmdpf(mfile)    ! add the modify file to the list
      call rmodmdpy(ymin)     ! set the y value for modifying

c  print info
      call rmodprhd(lu,' rmodrdhd',hfile,dfile,tfile,type
     1,cxi,cyi,czi,n_cord,x_cord,cord,xmin,xmax,ymin,ymax,zmin,zmax
     1,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)

c  transform x,y,z to output coordinates
      call rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord)

c      print'('' end rdhd hfile='',a40,/,'' dfile='',a40,/
c     1,'' tfile='',a40,/,'' type='',a16/,'' wtype='',a16)'
c     1,hfile,dfile,tfile,type,wtype

      return
  999 continue
      if (lu .gt. 0) write(lu,'(/,'' error in rmodrdhd'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_same_name(file)
c  if file is same set it to SAME
      implicit none
      character file*(*)
      character file_same*80
      call rmodcaps(file,file_same)
      if (file_same(1:4) .eq. 'SAME') file = 'SAME'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodmdfy(n1,im1,it1,ix1,nx1,x1,v1
     1,m2,mx2,im2,it2,ix2,nx2,x2,y2,v2)
c  apply the modifications definded by the list currently in file
c  if the parameter imodify has been set to one no modifications are applied
      parameter (mfile=10)
      character file0*(*),file(mfile)*64,cx*16,cz*16
      data irewind,imodify,yfile,nfile,file/1,0,0,0,10*' '/

c      print'('' mdfy imodify='',i8)',imodify
c  if the modification flag has been turned off, 
c  return without doing any modifications
      if (imodify .eq. 1) return

c      print'('' mdfy yfile='',f10.2,'' nfile='',i5)',yfile,nfile

      do 1 i = 1 , nfile

c        print'('' mdfy i='',i5,'' file='',a)',i,file(i)
        if (file(i)(1:1) .eq. ' ' .or. file(i)(1:4) .eq. 'NONE') goto 1

c  open the modification file
        call rmodopen(ifile,file(i),'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 1

c  read the modify section
        call rmodrxzv(ifile,'*MODIFY',cx,cz
     1,m2,mx2,n2,im2,it2,ix2,nx2,x2,y2,v2,*999)

c  apply these modifications to the velocity cards
        call rmodmdf1(n1,im1,it1,ix1,nx1,x1,yfile,v1
     1,n2,im2,it2,ix2,nx2,x2,y2,v2)

c      print'('' mdf0 n1='',i5,'' n2='',i5,'' yfile='',f10.2
c     1,/,'' file='',a)',n1,n2,yfile,file(i)

    1 continue
      return
      entry rmodmdpf(file0)
      if (file0(1:1) .ne. ' ' .and. file0(1:4) .ne. 'NONE') then
        do 2 i = 1 , nfile
          if (file0 .eq. file(i)) return
    2   continue
        nfile = min(nfile+1,mfile)
        file(nfile) = file0
c        print'('' mdpf n='',i5,'' f='',a)',nfile,file(nfile)
      endif
      return

      entry rmodmdpy(yfile0)
      yfile = yfile0
      return

      entry rmodmdgy(yfile0)
      yfile0 = yfile
      return

      entry rmodmdpn
      nfile = 0
c      print'('' mdpn n='',i5)',nfile
      return

      entry rmodmdpi(jmodify)
      imodify = max(0,min(1,jmodify))
      return

      entry rmodmdgi(jmodify)
      jmodify = imodify
      return

  999 continue
      print'(/,'' error in rmodmdfy'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodmdf1(n1,im1,it1,ix1,nx1,x1,y1,v1
     1,n2,im2,it2,ix2,nx2,x2,y2,v2)
c  apply n1 modifications to n2 velocities
      dimension im1(1),it1(1),ix1(1),nx1(1),x1(1),v1(1)
     1,im2(1),it2(1),ix2(1),nx2(1),x2(1),y2(1),v2(1)
c      print'('' mdf1 n1='',i5,'' n2='',i5,'' y1='',f10.2)',n1,n2,y1
      do 1 i1 = 1 , n1
        j1 = ix1(i1) + 1
        do 2 i2 = 1 , n2
          if (im1(i1) .eq. im2(i2)) then
            j2 = ix2(i2) + 1
            call rmodmdf2(nx1(i1),it1(j1),x1(j1),y1,v1(j1)
     1,nx2(i2),it2(j2),x2(j2),y2(j2),v2(j2))
          endif
    2   continue
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodmdf2(n1,it1,x1,y1,v1,n2,it2,x2,y2,v2)
c  apply n1 modifications to n2 velocities
      dimension it1(1),x1(1),v1(1),it2(1),x2(1),y2(1),v2(1)

c      print'('' mdf2 n1='',i5,'' n2='',i5,'' y1='',f10.2)',n1,n2,y1
      j1 = 1
      do 1 i1 = 1 , n1
c  if this it1 is not the same as the last it1 change points j1 to i1 - 
        if (it1(j1) .ne. it1(min(i1+1,n1)) .or. i1 .eq. n1) then
c      print*,' j1=',j1,' i1=',i1,' it1=',it1(j1),it1(i1)
c     1,it1(min(i1+1,n1))
        call rmodmdf3(i1-j1+1,it1(j1),x1(j1),y1,v1(j1),n2,it2,x2,y2,v2)
          j1 = i1 + 1
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodmdf3(n1,it1,x1,y1,v1,n2,it2,x2,y2,v2)
      dimension x1(1),v1(1),it2(1),x2(1),y2(1),v2(1)
c      print'('' mdf3 n1='',i5,'' n2='',i5,'' it1='',i5
c     1,'' y1='',f10.2)',n1,n2,it1,y1
      do 1 i2 = 1 , n2
c      print'('' it2='',i5,'' y2='',f10.2)',it2(i2),y2(i2)
        if (abs(y1-y2(i2)) .le. 1. .and. it1 .eq. it2(i2)) then
          do 2 j2 = i2 , n2
            if (j2 .eq. n2 .or. it2(min(j2+1,n2)) .ne. it2(i2)
     1 .or. abs(y2(min(j2+1,n2))-y2(i2)) .gt. .1) then
c              print'('' i2='',i5,'' j2='',i5,'' it1='',i5
c     1,'' it2='',3(1x,i5))',i2,j2,it1,it2(i2),it2(j2),it2(min(j2+1,n2))
              call rmodmdf4(n1,x1,v1,j2-i2+1,x2(i2),v2(i2))
              goto 3
            endif
    2     continue
        endif
    1 continue
    3 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodmdf4(n1,x1,v1,n2,x2,v2)
      dimension x1(1),v1(1)
c      print'('' mdf4 n1='',i5,'' n2='',i5,/,'' i vold v vpercent'')'
c     1,n1,n2
c      write(88,'('' mdf4 n1='',i5,'' n2='',i5
c     1,/,'' i x  vold v vpercent'')')n1,n2
      do 1 i1 = 1 , n1
        call rmodfit1(n2,x2,v2,x1(i1),v0)
        v1old = v1(i1)
        v1(i1) = v1(i1) * (1. + v0 / 100.)
c      write(88,'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.2,1x,f10.4)')
c     1i1,x1(i1),v1old,v1(i1),v0
c      print'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.4)',i1,v1old,v1,v0
    1 continue
c      write(88,'('' end mdf4'')')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfitc(nxi,xi,zi,nxo,xo,zo)
      dimension xi(1),zi(1),xo(1),zo(1)
      do 1 ixo = 1 , nxo
        call fitcell1(xo(ixo),0.,nxi,xi,ia,ib,wa,wb)
        zo(ixo) = wa * zi(ia) + wb * zi(ib)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfit1(nxi,xi,zi,xo,zo)
      dimension xi(1),zi(1)
      call fitcell1(xo,0.,nxi,xi,ia,ib,wa,wb)
      zo = wa * zi(ia) + wb * zi(ib)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfitn(nxi,xi,zi,nxo,xomin,xoinc,zo)
      dimension xi(1),zi(1),zo(1)
      do 1 i = 1 , nxo
        xo = (i - 1) * xoinc + xomin
        call fitcell1(xo,0.,nxi,xi,ia,ib,wa,wb)
        zo(i) = wa * zi(ia) + wb * zi(ib)
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodgv2(value,lvalue,name0,key)
c get the value of a variable 'name' from device ifile
c start from current line number and reposition after search
c search through a max of nline lines if nline<0 search through all
c stop after nfind occurences
      character *(*) value,name0
      save nline,nfind
      data nline,nfind/100,1/
c  use format   'name=value',  returned value is a string.
c  string is delimited on right by blank, comma, or parenthesis.
c  keep last assignment in file
c first open ifile with
      character crd132*132,crdstrt*132,crd80*80,name*80
      integer ifile,key,dbutil_getlun
      ifile = dbutil_getlun(key)
      if(ifile.le.0) return
c  read in current card later we will search back until we find it
      call rmodcaps(name0,name)
      read(ifile,'(a)')crdstrt
      backspace(ifile)
c      write(6,'(/,'' gv2 ifile='',i5,'' crd='',a60)')ifile,crd132
c      write(88,'(/,'' gv2 ifile='',i5,'' crd='',a60)')ifile,crd132
      lvalue = 0
      iline = 0
      ifind = 0
    1 continue
c  on cray count lines before read to end
c  on vax we would have iline increment after read
        iline = iline + 1
        crd132 = ' '
        read(ifile,'(a)',end=2,err=999) crd132
c      write(6,'('' il='',i5,'' crd='',a60)')iline,crd132
c      write(88,'('' il='',i5,'' crd='',a60)')iline,crd132
c  see if there is a star in this line if so it is the start of a new se
        call rmodfchr(i_star,'*',1,len(crd132),crd132)
        if (i_star .ne. 0) goto 2
c  decode for name
        call headgv1(crd80,lcrd,crd132,name)
c  if we find name get value and check for number of finds
        if (lcrd .gt. 0) then
          value = crd80(:lcrd)
          lvalue = lcrd
c  if we have found up to nfind occurences jump out of loop
          ifind = ifind + 1
          if (ifind .eq. nfind) goto 2
        endif
        if (iline .lt. nline) goto 1

c  done with search
    2 continue
c  repositon back to where we started
      call rmodback(key,iline,crdstrt)

c      read(ifile,'(a)')crd132
c      backspace(ifile)
c      write(6,'('' gv2 il='',i5,'' if='',i2,'' name='',a12
c     1,'' value='',a12,'' crd='',a12)')iline,ifind,name,value,crd132
c      write(88,'('' gv2 il='',i5,'' if='',i2,'' name='',a12
c     1,'' value='',a12,'' crd='',a12)')iline,ifind,name,value,crd132
c      call headlenl(nname,name)
c      write(88,'('' nname='',i5,'' name='',a12,'' value='',a20)')
c     1nname,name(:nname),value(:lvalue)
      return
  999 continue
C      print'(/,'' error in rmodgv2'')'
      return
      entry rmodplin(jline)
c set the number of lines to read in gv2
      nline = jline
      return
      entry rmodglin(jline)
c return the number of lines to read in gv2
      jline = nline
      return
      entry rmodpfin(jfind)
c  se the number of occurences to search for
      nfind = jfind
      return
      entry rmodgfin(jfind)
c  return the number of occurences to search for
      jfind = nfind
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodback(key,nline,crdstrt)
      implicit none
      integer nline,i
      character crdstrt*(*),crd132*132
      integer ifile,key,dbutil_getlun
      ifile = dbutil_getlun(key)
      if(ifile.le.0) return
c  note cray and vax do differen things at read to end
c  so have to search backwards for starting point
      do i = 1 ,nline-2
        backspace(ifile)
      enddo    ! i = 1 ,nline-1

c      print'('' n='',i5,'' crd='',a40)',nline,crdstrt
      do i = 1 , 5
        backspace(ifile)
        read(ifile,'(a)',end=1,err=1)crd132
c      print'('' i='',i5,'' crd='',a40)',i,crd132
        backspace(ifile)
        if (crd132 .eq. crdstrt) goto 1
      enddo    ! do i = 1 , 5

      print'('' could not find start il='',i5,'' crdstrt='',a12
     1,'' crd='',a12)',i,crdstrt,crd132
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodgvr(rvalue,name,ifile)
c get real value from file; "name=rvalue"
      character *(*) name,card
      character value*80,crd20*20
      call rmodgv2(value,lvalue,name,ifile)
      if (lvalue .ge. 1) call headdcod(rvalue,value(:lvalue))
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmodgvi(ivalue,name,ifile)
c get an integer value from a parameter file
      call rmodgv2(value,lvalue,name,ifile)
      if (lvalue .ge. 1) then
        call headdcod(r,value(:lvalue))
        ivalue = int(r+0.499)
      endif
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmodgvc(card,nchar,name,ifile)
c get a character string from a parameter file
      nchar = 0
      call rmodgv2(value,lvalue,name,ifile)
      if (lvalue .ge. 1) then
        card = ' '
        card = value(:lvalue)
        nchar = lvalue
      endif
c      write(88,'('' nchar='',i5,'' name='',a16,'' c='',a16)')
c     1nchar,name,card
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmoddrct(file1,file2)
c  set the default directory for file 2 using that of file1
c  if file 1 doesn't have a directory or file 2 has one do nothing
      character *(*) file1,file2
      iclose1 = index(file1,']')
      if (iclose1 .eq. 0)  iclose1 = index(file1,'>')
      iclose2 = index(file2,']')
      if (iclose2 .eq. 0)  iclose2 = index(file2,'>')
c      print'('' ic1='',i5,'' file1='',a40
c     1    ,/,'' ic2='',i5,'' file2='',a40)',iclose1,file1,iclose2,file2
      if (file2 .eq. 'SAME' .or. file2 .eq. 'same'
     1.or. file2 .eq. ' ') then
        file2 = file1
      elseif (iclose1 .ne. 0 .and. iclose2 .eq. 0) then
        file2 = file1(1:iclose1)//file2(iclose2+1:)
      endif
c      print'('' ic1='',i5,'' file1='',a40
c     1    ,/,'' ic2='',i5,'' file2='',a40)',iclose1,file1,iclose2,file2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodopen(key,file_in,format,status,access,wtype,nrecl
     1,ierr)
c  open file by name, if file is already open return unit number?
      character *(*) file_in,format,status,access,wtype
      character *132 file
      character *16  format0,status0,access0
      character *16  format1,status1,access1
      integer        nrecl,nrecl0,lun,key,ifile,wdtyp
      integer        dbutil_getlun,dbutil_getwordtype,sizeof_real
      integer        dbutil_putwordtype
      integer        inname(30),outname(30)
      integer        zero

      ierr = 0
      call rmodcaps(format,format1)
      call rmodcaps(status,status1)
      call rmodcaps(access,access1)
c
c Build full file name when on cray
      zero=0
      outname(1) = 0
      n=len(file_in)/sizeof_real()
      call convert_cc2hh(file_in,zero,inname,-n)
      call rmod_name_bld(inname,outname)
      call convert_hh2cc(outname,0,file,0)

      wdtyp = 1
c      print'('' rmodopen access='',a16,'' wtype='',a16
c     1,'' file='',a20)',access,wtype,file(1:20)

      if (access .eq. 'DIRECT') call rmod_wordtype_ctoi(wtype,wdtyp)
c....Check for existing data base entry on this file.
c      if(status.eq.'NEW') wdtyp=1
      call rmoddb_qfat(file,key,lun,wdtyp
     1,nrecl0,format0,access0,status0)
      ifile = lun

c     call rmodfall(key,file,format0,status0,access0,nrecl0)
c      print'('' rmodopen ifile='',i5,'' file='',a40
c     1,/,'' f='',a16,'' f0='',a16
c     1,/,'' s='',a16,'' s0='',a16
c     1,/,'' a='',a16,'' a0='',a16
c     1)',ifile,file,format,format0,status,status0,access,access0

c  if file is not yet open or its parameters are different we open it he
      if (key .le. 0 .or. format1 .ne. format0
     1 .or. status1 .ne. status0  .or. access1 .ne. access0
     1 .or. (access1 .eq. 'DIRECT' .and. nrecl  .ne. nrecl0 )) then
c        if (ifile .ne. 0) print'('' rmodopen ifile='',i5)',ifile
        if (key .gt. 0) call rmodclos(key)
        call rmodfinf(key,file,format1,status1,access1,nrecl,wdtyp,ierr)

        IF (ierr .ne. 0) goto 999
      endif

      if(wdtyp.lt.1) wdtyp = 3
      istat = dbutil_putwordtype(key,wdtyp)

      return

  999 continue
      call rmodlenr(lf,file)
      print'(/,'' error in rmodopen ifile='',i8,'' file='',a
     1,/,'' format='',a16,'' status='',a16
     1,/,'' access='',a16,'' wtype ='',a16
     1)',ifile,file(1:lf),format,status,access,wtype
      ierr = 1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodclof(full_file_name)
c  close file by name
      character full_file_name*(*)
      integer   key_val,key, rmodf2nuf,dbutil_getlun,ierr,lun
      integer   istat,rmod_putfil,can_remove,rmodclosec
      integer   rmod_canrm
      character rfile*80,file*80,status*16
      character node*32,user*16,msg*80
      character lnode*32,los*16,local_file*80
      call rmod_file2key(full_file_name,key)
      rfile = full_file_name
      if(key.le.0) return
      goto 1

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmodclos(key_val)
c  close file by key number
      key = key_val
      if(key.le.0) return
      call rmodfofi(key,rfile)    ! get file name
    1 continue
c      print'('' closeing file file='',i5)',ifile
      call rmodsofi(key,status)   ! get file status
      call rmod_locfile(key,local_file)
      ierr = rmodf2nuf(rfile,node,user,file,msg)
      call rmodglf(ilocal)

    2 continue
      can_remove = rmod_canrm(key)
      lun = dbutil_getlun(key)
      if(lun.gt.0) then
        close(lun)
        call dbutil_del(key)
      else
        ierr = rmodclc(key)
      endif
      if(status.ne.'OLD') then
       ierr  = rmod_putfil(local_file,node,user,file)
      endif
      if(can_remove.eq.0) then
       call rmod_rmfil_w(local_file)
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfinf(key,nfile,format,status,access,nrecl,wdtyp,
     + ierr)
c
c....Get data base key & save file information
      character *(*) nfile,format,status,access
      character node*16,user*16,file*80,msg*80
      character temp*80,lfile*80,path*80
      integer   rmodf2nuf,istat,rmod_lochost_w
      integer   rmod_getfil,rmod_putfil,gotit,putit
      integer   can_remove,wdtyp,lwdtyp
      character local_file_name*80,node_info*32,user_info*16
      character lnode*32,los*16
      character rfile*80
      integer   zero,sizeof_real,n,dbutil_getnewindex
      integer   dbutil_key_available,dbutil_getnew_ckey
      parameter (mlen=100)
      integer   lfile0(mlen),rfile0(mlen),acc0(8),form0(8),stat0(8)
      integer   key,ifile,lun,rmodopc,dbutil_getlun
      integer   dbutil_putwordtype

      parameter (mfile=99)
      character cfile*80,stat1*16
c     integer   nrecl0(mfile),rmit(mfile),lun0(mfile)
c     character file0(mfile)*80,format0(mfile)*16
c     character status0(mfile)*16,access0(mfile)*16
c     character node0(mfile)*16,user0(mfile)*16,lfile0(mfile)*80
c     data file0/99*' '/
c     data format0/99*' '/,status0/99*' '/,access0/99*' '/
c     data nrecl0/99*0/
c     data node0/99*'NONE'/
c     data user0/99*'NONE'/
c     data lfile0/99*' '/
c     data rmit/99*-1/
c     data lun0/99*-1/
      ierr  = 0
      key = -1
      ifile = -1

c
c....Get some information about the current node,local_word_type
c....Set the cray flag to the appropriate constant. 1=>cray
      lnode = ' '
      los   = ' '
      lwdtyp = rmod_lochost_w(lnode,los)
      if(wdtyp.lt.1) wdtyp=3

      call rmodglf(ilocal)
c
c....Check legality of format, status and access
      if (format .ne. 'FORMATTED' .and. format .ne. 'UNFORMATTED')
     1 goto 993
      if(status .ne. 'NEW' .and. status .ne. 'OLD' .and.
     1  status .ne. 'SCRATCH' .and. status.ne.'APPEND') goto 993
      if(access.ne.'DIRECT' .and. access.ne.'SEQUENTIAL' .and.
     1 access.ne.'STREAM' ) goto 993
c
c....Crack the network file name i.e. node::user;;file
      node = 'NONE'
      user = 'NONE'
      file = nfile
      temp = nfile
      ierr = rmodf2nuf(temp,node,user,file,msg)

      if(ierr .ne. 0) goto 10
c
c....Seperate file into its path and local name parts
      lfile=file
      if(node.ne.'NONE') then
       call rmod_fparse_w( file, lfile, path )
      endif

c
c....OLD file may be on another node but will be made
c....local before it is opened.
c....NEW or SCRATCH files are always local though they
c....may be sent to another node when they are closed.
c....ilocal flag can force the issue. LOCAL if ilocal=1
      stat1 = status
      if(status .eq. 'OLD') then
       lfile = ' '
       gotit = rmod_getfil(lfile,node,user,file)
      else if(status.eq.'NEW') then
       ilocal= 1  !new files are always opened local
       gotit = -1
       if(node(1:4).eq.'NONE') gotit = -1
       if(node(1:4).eq.'none') gotit = -1
       if(node(1:1).eq.' ') gotit = -1
      else
       gotit = 0
      endif
      if(gotit.ge.1) goto 996
c
c....Convert strings to null padded form
      zero=0
      n=len(format)/sizeof_real()
      call convert_cc2hh(format,zero,form0,-n)
      n=len(status)/sizeof_real()
      call convert_cc2hh(status,zero,stat0,-n)
      n=len(access)/sizeof_real()
      call convert_cc2hh(access,zero,acc0,-n)
      n=min(mlen,len(nfile)/sizeof_real())
      call convert_cc2hh(nfile,zero,rfile0,-n)
      n=min(mlen,len(lfile)/sizeof_real())
      call convert_cc2hh(lfile,zero,lfile0,-n)
c
c....Get available unit number
      if(access.eq.'SEQUENTIAL') then
       call getlun(lun,*994)
       if (lun .lt. 1 .or. lun .gt. mfile) goto 995
       ifile = lun
      else
       lun = -1
      endif

    1 continue    ! return to here to retry opening file
ccc

c
c....Open the file as requested
c....Get data base index & save the file information
c  for direct access file
      if (access .eq. 'DIRECT') then
        cfile = lfile
c  fetch file if on cray. nrecl = nz*bytes_per_wordtyp .
        lrecl = 4*nrecl
        if(sizeof_real() .eq. 8 .and. ilocal .eq. 1) lrecl = 8*nrecl
c  open file cray has record length in bytes so need 4 * nzg
c  c-style fopen does not need record length at open time

c if this is a new file set word type to IEEE and set owrd length to 4 b
        if(stat1.ne.'OLD') then
          wdtyp = 1
          lrecl = 4*nrecl
        endif    ! if(stat1.ne.'OLD') then
c      print'('' setting lrecl='',i8,'' nrecl='',i8)',lrecl,nrecl

c        write(6,*) 'ilocal=',ilocal
c        write(6,*) 'lrecl to rmodopc =',lrecl, ' nrecl=',nrecl
        key = rmodopc(lun,lfile0,rfile0,acc0,stat0,form0,lrecl,
     1        wdtyp)
        if(wdtyp.lt.1) wdtyp=lwdtyp
        if(key.le.0) goto 997
c       open(unit=lun,file=lfile,form=format,status=stat1
c    1  ,access=access,recl=lrecl,err=999)

      else if(access.eq.'SEQUENTIAL') then
        if (stat1(1:3) .eq. 'OLD') then
c  top of vax change
c  on vax use the readonly on the cray comment it out
          open(unit=lun,file=lfile,form=format,status=stat1
c     1    ,readonly,err=999)
     1    ,err=999)
c  end of vax change
        elseif(stat1.eq.'APPEND') then
          open(unit=lun,file=lfile,form=format,status='OLD',err=999)
        else
          open(unit=lun,file=lfile,form=format,status=stat1,err=999)
        endif
c        key = dbutil_getnewindex()
        key = -1
        istat = dbutil_key_available(lun)
        if(istat .ne. 0) key = lun
        if(key.lt.0) then
         ierr=1
         goto 10
        endif
        call dbutil_putlun(key,lun)
        lrecl = 0
        call dbutil_putreclength(key,lrecl)
        call dbutil_putaccesstype(key,acc0)
        call dbutil_putformattype(key,form0)
        call dbutil_putstatus(key,stat0)
        call dbutil_putremotefile(key,rfile0)
        call dbutil_putlocalfile(key,lfile0)
        ierr = dbutil_putwordtype(key,wdtyp)
      else   !c-language stream file?
       ierr=1
       goto 10
      endif

   10 continue

      return
  993 continue
      print'(/,'' error in input values'')'
      ierr = 1
      goto 999
  994 continue
      print'(/,'' error in getlun'')'
      goto 999
  995 continue
      print'(/,'' error in ifile value ifile='',i5)',ifile
      goto 999
  996 continue
      print'(/,'' error in network file transfer'')'
      goto 999
  997 continue
      print'(/,'' error in direct access open'')'
      goto 999
  998 continue
      print'(/,'' error in opnfil open'')'
      goto 999
  999 continue

c  if failed to open new try opening old - this will destroy old file
      if (stat1 .eq. 'NEW') then
        stat1 = 'APPEND'
        goto 1
      endif

      call rmodlenr(lf,file)
      print'(/,'' error in rmodfinf open file='',a
     1,/,''  form='',a16,'' status='',a16,'' access='',a16)'
     1,file(1:lf),format,status,access
c
c....If we get here we have not made a data base entry
c....delete the file if allowed
      if(gotit.eq.0) call rmod_rmfil_w(lfile)
      ierr = 1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfall(ifile,nfile,format,status,access,nrecl)
      integer ifile,nrecl
      integer key,lun
      character*(*) nfile,format,access,status
c
c....return all values for file name stored in nfile
      ifile = 0
      nrecl = 0
      format = ' '
      status = ' '
      access = ' '
c
c....Crack the network file name i.e. node::user;;file
c     node = 'NONE'
c     user = 'NONE'
c     file = nfile
c     temp = nfile
c     istat = rmodf2nuf(temp,node,user,file,msg)
c     if(istat.ne.0) return
c
c....Check for existing data base entry on this file.
      call rmoddb_qfat(nfile,key,lun,wdtyp,reclen,format,
     +     access,status)
      ifile = lun
      if(ifile.lt.0) ifile=0
c     do 1001 i = 1 , mfile
c       if (file .eq. file0(i) .and.node.eq.node0(i)) then
c         ifile = i
c         format = format0(ifile)
c         status = status0(ifile)
c         access = access0(ifile)
c         nrecl = nrecl0(ifile)
c         goto 1002
c       endif
c1001 continue
c1002 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_file2key(file,key)
      character*(*) file
      integer lun,dbutil_rfile2key,dbutil_getlun
      integer zero,n,sizeof_real,rfile0(30)
      zero = 0
      n = len(file)/sizeof_real()
      call convert_cc2hh(file,zero,rfile0,-n)
      key = dbutil_rfile2key(rfile0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodioff(lun,nfile)
      character*(*) nfile
      integer ifile,zero,sizeof_real,rfile0(30)
      integer key,n,dbutil_getlun,dbutil_rfile2key
c  get the logical unit number given the full file name nfile
      lun  = 0
      istat= 0
      zero =0
      n= len(nfile)/sizeof_real()
      call convert_cc2hh(nfile,zero,rfile0,-n)
      key = dbutil_rfile2key(rfile0)
      lun = dbutil_getlun(key)
c      print'('' rmodioff ifile='',i3,'' file='',a40)',ifile,nfile
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfofi(key,file)
      character*(*) file
      integer key,zero,sizeof_real
      integer       rfile0(30),sizof_real,dbutil_getrfile
c  get the full file name given the data base key
      file = ' '
      istat = dbutil_getrfile(key,rfile0)
      if(istat.eq.0) then
       call convert_hh2cc(rfile0,0,file,0)
      endif
c      print'('' rmodfofi ifile='',i3,'' file='',a40)',ifile,file
      return
      end

      subroutine rmod_locfile(key,local_file_name)
      character*(*) local_file_name
      integer       key,lfile0(20),istat,dbutil_getlocalfile
c get local file name entry in the data base
      local_file_name=' '
      istat = dbutil_getlocalfile(key,lfile0)
      if(istat.eq.0) then
       call convert_hh2cc(lfile0,0,local_file_name,0)
      endif
      return
      end

      subroutine rmod_nodeinfo(key,node_info)
      character*(*) node_info
      integer       key,node0(8),istat,rmodf2nuf
      character     rfile*80,node*32,user*16,file*80,msg*80
      node_info='NONE'
c  get node entry for data base entry key
      call rmodfofi(key,rfile)
      if(rfile.eq.' ') return
      istat = rmodf2nuf(rfile,node,user,file,msg)
      if(istat.ne.0) return
      node_info=node
      return
      end

      subroutine rmod_userinfo(key,user_info)
      character*(*) user_info
      integer       key,user0(8),istat,rmodf2nuf
      character     rfile*80,node*32,user*16,file*80,msg*80
c  get user entry for data base entry key
      user_info='NONE'
      call rmodfofi(key,rfile)
      if(rfile.eq.' ') return
      istat = rmodf2nuf(rfile,node,user,file,msg)
      if(istat.ne.0) return
      user_info=user
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrofi(key,format_info)
      character*(*) format_info
      integer key,dbutil_getformattype,format0(8)
c  get format entry for data base entry key
      format_info= ' '
      istat = dbutil_getformattype(key,format0)
      if(istat.ne.0) return
      call convert_hh2cc(format0,0,format_info,0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodsofi(key,status_info)
      character*(*) status_info
      integer key,dbutil_getstatus,status0(8)
c  get status entry for data base entry key
      status_info= ' '
      istat = dbutil_getstatus(key,status0)
      if(istat.ne.0) return
      call convert_hh2cc(status0,0,status_info,0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfclr(key)
      integer key
c  clear the data base entry for key
      call dbutil_del(key)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodplf(jlocal)
      data ilocal/0/
      ilocal = jlocal
      return
      entry rmodglf(jlocal)
      jlocal = ilocal
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrrec(key,irec,nrec,n,vg,work,*)
c  read a record from a direct access file
      dimension vg(1),work(1)
      integer wrdc_fcnvrt,msg0(30),word_in,lwdtyp,istat,reclen
      integer key,dbutil_getlun,dbutil_getwordtype,dbutil_getreclength
      integer sizeof_real
      character msg*160
      word_in = dbutil_getwordtype(key)
c      print*,' rrec word_in=',word_in

      do jrec = irec , irec+nrec-1
        i1 = (jrec - irec) * n + 1
        i2 = i1 + n - 1
        if (sizeof_real() .ne. 8) then
          call rmodrdc(key,jrec, 1, n, vg(i1),
     1         word_in,istat, msg0)
          if(istat .ne.0) goto 999
c         read(ifile,rec=jrec,err=999) (work(i),i=1,n/2)
c         call vxsctc(work,1,vg(i1),2*(n/2),1)
          if (mod(n,2) .ne. 0 .and. n .gt. 1) vg(i2) = vg(i2-1)
        else
          call rmodrdc(key,jrec, 1, n, vg(i1),
     1         word_in,istat, msg0)
          if(istat .ne.0) goto 999
c         read(ifile,rec=jrec,err=999) (vg(i),i=i1,i2)
c      print*,' jrec=',jrec,' vg=',vg(i1),vg((i2+i1)/2),vg(i2)

        endif
      enddo    ! do jrec = irec , irec+nrec-1
      return

  999 continue
      print'(/,'' error in rmodrrec istat='',i8
     1,'' key='',i8,'' word_in='',i8
     1,/,'' jrec='',i8,'' irec='',i8,'' nrec='',i8,'' n='',i8)'
     1,istat,key,word_in,jrec,irec,nrec,n
      call convert_hh2cc(msg0,0,msg,0)
      reclen= dbutil_getreclength(key)
      write(6,*) ' record length =',reclen
      write(6,*) msg

      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcloa(file2)
c  close file2 with file1 appended
      character *(*) file2
      character format1*16,format2*16,file1*80
      integer   ifile1,key1,key2,dbutil_getlun
      data file1/'rmodap.dat'/,icall/0/
      save ifile1,file1,key1,key2
c  get file unit number
      call rmod_file2key(file2,key2)
      call rmodioff(ifile2,file2)
      goto 1

      entry rmodcloi(jfile)
      key2 = jfile
      ifile2 = dbutil_getlun(key2)

c  apend ifile1 to ifile2
    1 continue

c  get file names
      call rmodfofi(key1,file1)
      call rmodfofi(key2,file2)
      call rmodrofi(key1,format1)
      call rmodrofi(key2,format2)

c  both files must be defined and formatted
      call rmodlenr(lf1,file1)
      call rmodlenr(lf2,file2)
c      print'('' cloa ifile1='',i3,'' file1='',a,/,'' ifile2='',i3
c     1,'' file2='',a)',ifile1,file1(1:lf1),ifile2,file2(1:lf2)
      if (file1 .ne. ' ' .and. file2 .ne. ' '
     1.and. format1 .eq. 'FORMATTED'
     1.and. format2 .eq. 'FORMATTED') call rmodapen(key1,key2)

c  close file
      call rmodclos(key2)
      return

      entry rmodopap(file2)
c  if this is the first time this has been called open a scratch file
      call rmodioff(ifile1,file1)
c      print'('' rmodopap file1='',a20,'' file2='',a20)',file1,file2
      if (ifile1 .le. 0) then
        n0=0
        call rmodopen(key1,file1,'FORMATTED','SCRATCH'
     1,'SEQUENTIAL','IEEE',n0,ierr)
        ifile1 = dbutil_getlun(key1)
        if (ierr .ne. 0) goto 999
      endif

      call rmodioff(ifile2,file2)
      call rmod_file2key(file2,key2)
      call rmodrofi(key2,format1)
c      print'('' rmodopap ifile1='',i3,'' ifile2='',i3
c     1,'' ,file1='',a20,'' file2='',a20)',ifile1,ifile2,file1,file2
c      print'('' format='',a16)',format1
      if (format1 .ne. 'FORMATTED') goto 2
      rewind(ifile1)
      rewind(ifile2)
      call rmodfcop(key2,key1,-1,ncop)
      rewind(ifile1)
      rewind(ifile2)
    2 continue
      return

      entry rmodclap
c  close apend file
      call rmodclof(file1)

  999 continue
c      print'(/,'' error in rmodopap'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodapen(keya,keyb)
c  apend sections from filea to fileb
      parameter (msec=100,mclr=100)
      dimension
     1 iseca(msec),jseca(msec),lseca(msec),lcla(mclr),lcra(mclr)
     1,isecb(msec),jsecb(msec),lsecb(msec),lclb(mclr),lcrb(mclr)
      character sec0*16,crd132*132
     1,seca(msec)*16,cla(mclr)*16,cra(mclr)*40
     1,secb(msec)*16,clb(mclr)*16,crb(mclr)*40
      integer keya,ifilea,keyb,ifileb,keyc,ifilec
      integer dbutil_getlun
      ifilea = dbutil_getlun(keya)
      ifileb = dbutil_getlun(keyb)

c  --  cHECK THE lOGICAL uNIT nUMBERS
      if ( ifilea .lt. 1 .or. ifilea .gt. 99
     1.or. ifileb .lt. 1 .or. ifileb .gt. 99) return
c  open a scratch work file
      call rmodopen(keyc,'rmod01.scr','FORMATTED','SCRATCH'
     1,'SEQUENTIAL','IEEE',n0,ierr)
      if (ierr .ne. 0) return
      ifilec = dbutil_getlun(keyc)
c  rewind files
      rewind(ifilea)
      rewind(ifileb)
      rewind(ifilec)

c  make a list of all star cards in filea,fileb
      call rmodnsec(keya,msec,nseca,jseca,iseca,lseca,seca)
      call rmodnsec(keyb,msec,nsecb,jsecb,isecb,lsecb,secb)
c      print*,' nseca=',nseca,' nsecb=',nsecb

c  copy each section in b to c
c  for each section see if a has the same section
c  if it does look for variables in a that are not in b and copy them to
      do 2 ib = 1 , nsecb
c      print'('' copy section from b to c ib='',i2,'' l='',i5
c     1,'' s='',a16)',ib,lsecb(ib),secb(ib)
        call rmodfcop(keyb,keyc,lsecb(ib),ncop)
        call rmodlenr(lb,secb(ib))
c  see if this b section is also in a
        do 3 ia = 1 , nseca
          if (seca(ia)(1:lb) .eq. secb(ib)(1:lb)) then
c  this section is in b, check a for parameters not in b
c  if seca is the same as secb and one of them is a decode section ...
            if (jseca(ia) .gt. 0 .or. jsecb(ib) .gt. 0) then
c  make a list of the parameter name and values that are in a but not b
              call rmodnclr(keya,iseca(ia),lseca(ia)
     1,mclr,nca,lcla,lcra,cla,cra)
              call rmodnclr(keyb,isecb(ib),lsecb(ib)
     1,mclr,ncb,lclb,lcrb,clb,crb)
c      print*,' ia=',ia,' ib=',ib,' nca=',nca,' ncb=',ncb
              do 4 ica = 1 , nca
                do 5 icb = 1 , ncb
      if (cla(ica)(1:lcla(ica)) .eq. clb(icb)(1:lclb(icb))) goto 4
    5           continue
                write(crd132,'(a16,''='',a40)')
     1cla(ica)(1:lcla(ica)),cra(ica)(1:lcra(ica))
                call rmodbdel(crd132)
                call rmodwcim(keyc,crd132)
    4         continue
            endif    ! if (jseca(ia) .gt. 0 .or. jsecb(ib) .gt. 0) then
            jseca(ia) = -1    ! set flag so seca will not be copied latt
            goto 2
          endif    ! if (seca(ia)(1:lb) .eq. secb(ib)(1:lb)) then
    3   continue
    2 continue
c  add sections from file a that are not in file b
      rewind(ifilea)
      do 7 ia = 1 , nseca
        if (jseca(ia) .lt. 0) then
          call rmodskip(keya,lseca(ia))
        else
          call rmodfcop(keya,keyc,lseca(ia),ncop)
        endif
    7 continue

c  copy filec back to fileb
      rewind(ifileb)
      rewind(ifilec)
      call rmodfcop(keyc,keyb,-1,ncop)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodskip(key,n)
c  skip n lines from file ifile
      integer key,ifile,dbutil_getlun
      character c*1
      ifile= dbutil_getlun(key)
      do 1 i = 1 , n
        read(ifile,'(a)',err=2,end=2)c
    1 continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodnsec(key,msec,nsec,jsec,isec,lsec,sec)
c  determine how many sections in file ifile
      dimension jsec(1),isec(1),lsec(1)
      integer key,ifile,dbutil_getlun
      character sec(1)*16
      character crd132*132
c      print'('' nsec'')'
      ifile= dbutil_getlun(key)
      rewind(ifile)
      nsec = 0
      nrd = 0
      do 1 i = 1 , msec
c  find the next section card
        nrd0 = 0
        call rmodfstr(key,0,nrd0,'*',crd132,*2)
        nrd = nrd + nrd0
        nsec = i
        isec(i) = nrd - 1
        call rmodesec(sec(i),crd132)
        call rmodlenr(lc,crd132)
c        print'('' nsec '',i5,'' is='',i5,'' s='',a16,'' c='',a)'
c     1,nsec,isec(nsec),sec(nsec),crd132(1:lc)
c  read the next card
        jsec(i) = 0
        read(ifile,'(a)',err=1,end=1)crd132
        call rmodnnum(jsec(i),crd132)  ! is this is not a decode section
        backspace(ifile)
    1 continue
    2 continue
      nrd = nrd + nrd0
      do 3 i = 1 , nsec - 1
        lsec(i) = isec(i+1) - isec(i)
    3 continue
      lsec(nsec) = nrd - isec(nsec)
c      print'(/,'' nsec='',i5,/,'' i  jsec  isec  lsec  sec'')',nsec
c      print'(1x,i5,1x,i5,1x,i5,1x,i5,1x,a16)'
c     1,(i,jsec(i),isec(i),lsec(i),sec(i),i=1,nsec)
      rewind(ifile)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfsec(key,isec,sec,card)
c  find next data section in ifile
      character sec*(*),card*(*),crd132*132
      integer key,ifile,dbutil_getlun
      ifile= dbutil_getlun(key)
      irewind = 0
      isec = 0
      sec = ' '
      card = ' '
      call rmodfstr(key,irewind,'*',card,*1)
      call rmodesec(sec,card)
      read(ifile,'(a)',err=1,end=1)crd132
      call rmodnnum(isec,crd132)    ! is this is not a decode section is
      backspace(ifile)
    1 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodnnum(ifind,c1)
c  ifind = location of first non numeric character
      character c1*(*)
      parameter (mc2=16)
      character c2(16)*1
      data c2/'0','1','2','3','4','5','6','7','8','9'
     1,'.','+','-','E','e',' '/
      ifind = 0
      call rmodlenr(mc1,c1)
      do 1 ic1 = 1 , mc1
        do 2 ic2 = 1 , mc2
          if (c1(ic1:ic1) .eq. c2(ic2)) goto 1
    2   continue
        ifind = ic1
        return
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodnclr(key,isec,lsec,mc,nc,lcl,lcr,cl,cr)
c  make a list of the parameters and there values in this section
      character cl(1)*16,cr(1)*40
      dimension lcl(1),lcr(1)
      character crd132*132
      integer key,ifile,dbutil_getlun
      ifile= dbutil_getlun(key)
      rewind(ifile)
      call rmodskip(key,isec+1)
      lcl0 = len(cl(1))
      lcr0 = len(cr(1))
c      print*,' l0=',lcl0,' lr=',lcr0
c     1,' isec=',isec,' lsec=',lsec
      nc = 0
      do 1 i = 1 , lsec-1
        read(ifile,'(a)',err=1,end=1)crd132
        call rmodlenr(ic2,crd132)
        ic1 = 1
c      print'('' i='',i5,'' c='',a)',i,crd132(ic1:ic2)
    2   continue
c  find the location of the next equal sign within crd132
        call rmodflrc(il1,il2,ir1,ir2,ic1,ic2,crd132)
        if (il1 .ge. ic1) then
          nc = min(mc,nc+1)
          lcl(nc) = min(lcl0,il2-il1+1)
          cl(nc) = crd132(il1:il2)
          lcr(nc) = min(lcr0,ir2-ir1+1)
          cr(nc) = crd132(ir1:ir2)
          ic1  = ir2 + 1
          goto 2
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodflrc(il1,il2,ir1,ir2,ic1,ic2,c)
c  using c(ic1:ic2) find the next = sign
c  then find the cahracter strings to left and right
      character c*(*)
      character*1 equal
      data equal/'='/
      logical headspac
c  first find the character '='
c  find the first = sign after icard
      call rmodfchr(iequal,equal,ic1,ic2,c)
c      print'('' flrc ie='',i3,'' ic='',i3,1x,i3,'' c='',a)'
c     1,iequal,ic1,ic2,c(ic1:ic2)
      if (iequal .ne. 0) then
c  working backwards from iequal-1 find the first non blank character
        call rmodnchr(il2,' ',iequal-1,ic1,c)
        call rmodfspc(il1,il2,ic1,c)
c  working forwards from iequal-1 find the first non blank character
        call rmodnchr(ir1,' ',iequal+1,ic2,c)
        call rmodfspc(ir2,ir1,ic2,c)
c      print'('' ie='',i3,'' il='',i3,1x,i3,'' ir='',i3,1x,i3
c     1,'' cl='',a16,'' cr='',a40)'
c     1,iequal,il1,il2,ir1,ir2,c(il1:il2),c(ir1:ir2)

      else
        il1 = 0
        il2 = 0
        ir1 = 0
        ir2 = 0
      endif    ! if (iequal .ne. 0) then
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfspc(is,ic1,ic2,c)
c  find the next character that is a space or , or ( or )
      character c*(*)
      logical headspac
      idir = sign(1,ic2-ic1)
      is = ic1
      do 1 i = ic1+idir , ic2 , idir
        if (headspac(c(i:i))) goto 2
        is = i
    1 continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodesec(sec,card)
c  extract section name from card
      character *(*) sec,card
      sec = ' '
      call rmodlenr(lencard,card)
c      print'('' esec len='',i5,'' c='',a40)',lencard,card
      call rmodfchr(i_star,'*',1,lencard,card)
      if (i_star .gt. 0) then
        call rmodfchr(iblk,' ',i_star+1,lencard,card)
        if (iblk .eq. 0) iblk = lencard + 1
        sec = card(i_star+1:iblk-1)
      endif    ! if (i_star .gt. 0) then
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfend(key)
      integer key,ifile,dbutil_getlun
      character crd132*132
      ifile= dbutil_getlun(key)
c  skip to end of file
      ncrd = 0
    1 continue
      read(ifile,'(a)',err=2,end=2)crd132
      ncrd = ncrd + 1
      goto 1
    2 continue
c      print'('' rmodfend ifile='',i3,'' ncrd='',i8)',ifile,ncrd
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfcop(key1,key2,mcop,ncop)
c  copy file 1 to file 2 from current positions
      character crd132*132
      integer key1,key2,ifile1,ifile2,dbutil_getlun
      ifile1= dbutil_getlun(key1)
      ifile2= dbutil_getlun(key2)
      ncop = 0
    1 continue
      read(ifile1,'(a)',err=3,end=2)crd132
      call rmodlenr(lc,crd132)
      write(ifile2,'(a)',err=4)crd132(1:lc)
      ncop = ncop + 1
      if (mcop .lt. 0 .or. ncop .lt. mcop) goto 1

    2 continue
c      print'('' rmodfcop read end ifile1='',i3,'' ifile2='',i3
c     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

    3 continue
      print'('' rmodfcop read err ifile1='',i3,'' ifile2='',i3
     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

    4 continue
      print'('' rmodfcop write err ifile1='',i3,'' ifile2='',i3
     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwrhd(hfile,dfile,wtype,mfile,tfile,type,stat,itrans
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,*)
c put sampling rate and filenames into header file.
c  if itrans .ne. 0 write out transform file
      character *(*) hfile,dfile,wtype,mfile,tfile,type,stat
     1,cxi,cyi,czi,cxo,cyo,czo,cord
      character *64 dfile0,tfile0
      character *16 wtype0
      integer wdtyp,dbutil_putwordtype
      integer key,ifile,dbutil_getlun,dbutil_getwordtype

c  convert x,y,z values to output coordinates
      call rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord)

c  write out header cards
      if (stat .eq. 'NEW') call rmodclof(hfile)
      if (stat .eq. 'OLD') goto 999 !opens are read only for headers
      call rmodopen(key,hfile,'FORMATTED',stat,'SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999
      ifile= dbutil_getlun(key)
c      print'('' wrhd ifile='',i3,'' hfile='',a20,'' dfile='',a20
c     1,/,'' tfile='',a20,'' type='',a20)'
c     1,ifile,hfile,dfile,tfile,type
      if (tfile .eq. hfile) then
        tfile0='SAME'
      else
        tfile0 = tfile
        call rmodclof(tfile)
      endif
      if (dfile .eq. hfile) then
        dfile0='SAME'
      else
        dfile0 = dfile
        call rmodclof(dfile)
      endif

      call rmodcaps(wtype,wtype0)
      if (wtype0 .ne. 'IEEE'
     1 .and. wtype0 .ne. 'IBM'
     1 .and. wtype0 .ne. 'VMS'
     1 .and. wtype0 .ne. 'AGRID'
     1 .and. wtype0 .ne. 'ASCII'
     1 .and. wtype0 .ne. 'CRAY') wtype0 = 'IEEE'

c      print'('' wrhd ifile='',i3,'' hfile='',a20,'' dfile='',a20
c     1,/,'' tfile='',a20,'' type='',a20,'' wtype='',a20)'
c     1,ifile,hfile,dfile,tfile,type,wtype0
      call rmodlnlr(id1,id2,dfile0)
      call rmodlnlr(ie1,ie2,wtype0)
      call rmodlnlr(im1,im2,mfile)
      call rmodlnlr(ir1,ir2,tfile0)
      call rmodlnlr(ip1,ip2,type)
      call rmodlnlr(ix1,ix2,cxo)
      call rmodlnlr(iy1,iy2,cyo)
      call rmodlnlr(iz1,iz2,czo)
c      wdtyp = dbutil_getwordtype(key)
      call rmod_wordtype_ctoi(wtype0,wdtyp)
      ierr = dbutil_putwordtype(key,wdtyp)

      write(ifile,'('' *HEADER SECTION'')')
      write(ifile,'('' FILE ='',a)')dfile0(id1:id2)
      if (mfile .ne. ' ' .and. mfile(1:4) .ne. 'NONE')
     1write(ifile,'('' MFILE='',a)')mfile(im1:im2)

c     1,/,'' XMODMIN='',g16.9,'' XMODMAX='',g16.9,'' XCOORDINATE='',a16
c     1,/,'' YMODMIN='',g16.9,'' YMODMAX='',g16.9,'' YCOORDINATE='',a16
c     1,/,'' ZMODMIN='',g16.9,'' ZMODMAX='',g16.9,'' ZCOORDINATE='',a16

      write(ifile,'('' TRANSFORM='',a
     1,/,'' TYPE='',a
     1,/,'' WORDTYPE='',a
     1,/,'' layered model limits''
     1,/,'' XMODMIN='',g16.9,'' XMODMAX='',g16.9,'' XCOORDINATE='',a
     1,/,'' YMODMIN='',g16.9,'' YMODMAX='',g16.9,'' YCOORDINATE='',a
     1,/,'' ZMODMIN='',g16.9,'' ZMODMAX='',g16.9,'' ZCOORDINATE='',a
     1,/,'' gridded model limits''
     1,/,'' NX='',i10,'' XMIN='',g16.9,'' XINC='',g16.9,'' XMAX='',g16.9
     1,/,'' NY='',i10,'' YMIN='',g16.9,'' YINC='',g16.9,'' YMAX='',g16.9
     1,/,'' NZ='',i10,'' ZMIN='',g16.9,'' ZINC='',g16.9,'' ZMAX='',g16.9
     1,/,'' datum level'',/,'' ZDATUM='',g16.9)')
     1tfile0(ir1:ir2),type(ip1:ip2),wtype0(ie1:ie2)
     1,xmin,xmax,cxo(ix1:ix2),ymin,ymax,cyo(iy1:iy2)
     1,zmin,zmax,czo(iz1:iz2)
     1,nxg,xgmin,xginc,(nxg-1)*xginc+xgmin
     1,nyg,ygmin,yginc,(nyg-1)*yginc+ygmin
     1,nzg,zgmin,zginc,(nzg-1)*zginc+zgmin,zdatum

c  convert x,y,z values to input coordinates
      call rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxo,cyo,czo,cxi,cyi,czi,n_cord,x_cord,cord)
c      print'('' bef wtrn ifile='',i5,'' tfile='',a20)',ifile,tfile
      call rmod_trupath(hfile,tfile)
      if (itrans .ne. 0)
     1call rmodwtrn(tfile,m_cord,n_cord,x_cord,cord,*999)

      return
  999 continue
      print'(/,'' error in rmodwrhd'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwxcv(dfile,cxi,czi,cxo,czo,n_cord,x_cord,cord
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*)
c  write all 3 data cards
      character *(*) dfile,cxi,czi,cxo,czo

      call rmodopen(ifile,dfile,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999

      init = 1     ! write * cards
      call rmodcnlr(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,cxi,czi,cxo,czo,n_cord,x_cord,cord)
c  write boundary cards
      call rmodwxzv(ifile,init,'*PICKS',cxo,czo
     1,nb,imb,itb,ixb,nxb,xb,zb,vel)
c  write cell pointer cards
      call rmodwxzv(ifile,init,'*CELL',cxo,czo
     1,ncv,icv,icv,icv,icv,xcv,zcv,vel)
c  write velocity cards
      call rmodwxzv(ifile,init,'*VELOCITY',cxo,czo
     1,nv,imv,itv,ixv,nxv,xv,zv,vel)

      call rmodcnlr(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,cxo,czo,cxi,czi,n_cord,x_cord,cord)
c  write end
c      call rmodwend(ifile)

      return
  999 continue
      print'(/,'' error in rmodwxcv'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrxcv(dfile
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*)
c  read in boundary x,z points, cell pointers and velocity from dfile
      dimension ixb(1),nxb(1),ixv(1),nxv(1),zb(1)
      character *(*) dfile,cxi,czi,cxo,czo,cord
      character *16 cxp,cxc,cxv,czp,czc,czv

      call rmodopen(ifile,dfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999

      cxp = cxi
      cxc = cxi
      cxv = cxi
      czp = czi
      czc = czi
      czv = czi

c  if this is an old icp file read xmin,xmax,zmin,zmax
      call rmodoicp(ifile,xmin,xmax,zmin,zmax,nold
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord)

c  read velocity cards
      call rmodrxzv(ifile,'*VELOCITY',cxv,czv
     1,mb,mxb,nv,imv,itv,ixv,nxv,xv,zv,vel,*999)
C      print*,' scaling velocities inside rxcv'
c  scale velocities use xb,zb  as work arrays
      call rmodmdfy(nv,imv,itv,ixv,nxv,xv,vel
     1,mb,mxb/2,imb,itb,ixb,nxb,xb,zb(1),zb(mxb/2+1))
c  read boundary cards
      call rmodrxzv(ifile,'*PICKS',cxp,czp
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,vel,*999)
c  read cell pointer cards
      call rmodrxzv(ifile,'*CELL',cxv,czv
     1,mb,mb,ncv,icv,icv,icv,icv,xcv,zcv,vel,*999)

c  convert x coordinates to basement
      call rmodcnvn(ixb(nb)+nxb(nb),xb,cxp,cxo,n_cord,x_cord,cord)
      call rmodcnvn(ncv,xcv,cxc,cxo,n_cord,x_cord,cord)
      call rmodcnvn(ixv(nv)+nxv(nv),xv,cxv,cxo,n_cord,x_cord,cord)

c  convert z coordinates
      call rmodcnvn(ixb(nb)+nxb(nb),zb,czp,czo,n_cord,x_cord,cord)
      call rmodcnvn(ncv,zcv,czc,czo,n_cord,x_cord,cord)
      call rmodcnvn(ixv(nv)+nxv(nv),zv,czv,czo,n_cord,x_cord,cord)

      return
  999 continue
      print'(/,'' error in rmodrxcv'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodoicp(ifile,xmin,xmax,zmin,zmax,nold
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord)
c  read xmin,xmax,zmin,zmax from old icp file
      character *(*) cxi,czi,cxo,czo,cord
      character crd80*80
      dimension x(4)
      data irewind/1/
      call rmodnold(ifile,nold)
      if (nold .ne. 0) then
        call rmodfstr(ifile,irewind,nrd,'model limits',crd80,*1)
        call rmoddcod(crd80(16:80),4,n,x)
        xmin = x(1)
        xmax = x(2)
        zmin = x(3)
        zmax = x(4)
        call rmodcnm4(xmin,xmax,zmin,zmax,cxi,czi,cxo,czo
     1,n_cord,x_cord,cord)
      endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodshei(dfile,itype,*)
c  determine if this is icp format itype=0 or sheins format itype=1
      parameter (mchar=52)
      character *(*) dfile
      character crd132*132,char*52
      integer   ifile,key,dbutil_getlun
      data nchar/mchar/
     1,char/'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'/
      call rmodopen(key,dfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      ifile=dbutil_getlun(key)
      if (ierr .ne. 0) goto 999
      rewind(ifile)
      itype = 0
      do 1 k = 1 , 10
        crd132 = ' '
        read(ifile,'(a)',err=4,end=4)crd132
        do 2 i = 1 , len(crd132)
          if (crd132(i:i) .eq. ' ') goto 2
          if (crd132(i:i) .eq. '='
     1   .or. crd132(i:i) .eq. '*') return
          if (k .gt. 2) goto 2
          do 3 j = 1 , nchar
c            if (crd132(i:i) .eq. char(j:j)) print'('' k='',i2
c     1,'' i='',i3,'' j='',i2,'' char='',a1)',k,i,j,char(j:j)
            if (crd132(i:i) .eq. char(j:j)) return
    3     continue
    2   continue
    1 continue
    4 continue
      itype = 1
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrlay(hfile,dfile,tfile,type
     1,cxi,czi,cxo,czo,m_cord,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,zdatum
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,nxg,xgmin,xginc,nzg,zgmin,zginc,lu,*)
c  read info for both head and data file
      character *(*) hfile,dfile,tfile,type,cxi,czi,cxo,czo,cord
      character wtype*64,mfile*64
      character *16 cyi,cyo
      dimension vg(1)
      data ndim/2/,cyo/'ybasement'/

      call rmodrdhd(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ndim,lu,*999)
      if (type(1:1) .eq. 'G') goto 999
      call rmodrdsc(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      return
  999 continue
      print'(/,'' error in rmodrlay'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrbnd(hfile,cxo,czo
     1,ixcv,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,vb,*)
c  read in boundary info only
      parameter (m_cord=20)
      character *(*) hfile,cxo,czo
      dimension x_cord(2,m_cord)
      character *64 dfile,wtype,mfile,tfile,type
      character title(3)*16
      character *16 cyi,cyo,cxp,czp,cxi,czi,cord(m_cord)
      dimension ixb(1),nxb(1)
      data cyo/'ybasement'/,ndim/2/,lu/-1/,n_cord/0/
      data title/'*CELL','*PICKS','*VELOCITY'/

      nb = 0
      call rmodrdhd(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ndim,lu,*999)

      if (type(1:1) .ne. 'L') return

      call rmodopen(ifile,dfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999
      cxp = cxi
      czp = czi

c  read boundary cards
      call rmodrxzv(ifile,title(ixcv),cxp,czp
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,vb,*999)

c  convert x,z coordinates to basement
      call rmodcnin(nb,ixb,nxb,xb,zb,cxp,czp,cxo,czo,n_cord,x_cord,cord)

      return
  999 continue
      print'(/,'' error in rmodrbnd'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrall(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,bname,bcolor
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,vname,vcolor
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,cname,ccolor
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mvg,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,makec,mwork,work,lu,*)
c  read info for both head and data file output both cells and boundarie
c  if makec .ne. 0 make cells
      dimension vg(1)
      character *(*) hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cxo,cyo,czo
     1,cord(*),bname(*),bcolor(*),vname(*),vcolor(*),cname(*),ccolor(*)
      data ndec,ndim/0,2/

c  read header info
      call rmodrdhd(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi
     1,cxo,cyo,czo
     1,m_cord,n_cord,x_cord,cord
     1,xmin,xmax
     1,ymin,ymax
     1,zmin,zmax
     1,zdatum
     1,nxg,xgmin,xginc
     1,nyg,ygmin,yginc
     1,nzg,zgmin,zginc
     1,ndim,lu,*999)

c  read boundary horizon info
      call rmodrhor(hfile,'*HORIZON',ibw1,ibw2,ibw3
     1,mbh,nbh,ibid,nbseg,bname,bcolor,*999)

c  read velocity horizon info
      call rmodrhor(hfile,'*VHORIZON',ivw1,ivw2,ivw3
     1,mvh,nvh,ivid,nvseg,vname,vcolor,*999)

c  read cell horizon info
      call rmodrhor(hfile,'*CHORIZON',icw1,icw2,icw3
     1,mch,nch,icid,ncseg,cname,ccolor,*999)

      if (type(1:1) .eq. 'G') then
        if (mvg .lt. nxg*nyg*nzg .or. mwork .lt. nzg) then
          print'('' not enough space for velocity grid''
     1/,'' nxg='',i8,'' nyg='',i8,'' nzg='',i8,'' tot='',i8
     1,'' mvg='',i8,'' mwork='',i8)',nxg,nyg,nzg,nxg*nyg*nzg,mvg,mwork
          goto 999
        endif

        call rmodvgrd(dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,mwork,work,*999)
        if (type .eq. 'G3DL') then
          call rmodvxyz(0,hfile,'*CELL'    ,cxi,cyi,czi
     1,mxb,ncv,xcv,ycv,zcv,vel,icv,itv,ixv)
          call rmodvxyz(0,hfile,'*VELOCITY',cxi,cyi,czi
     1,mxb,nv,xv,yv,zv,vel,itv,ixv,nxv)
        endif
      else
        call rmodrdcl(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,makec,mwork,work,lu,*999)
      endif

      return
  999 continue
      print'(/,'' error in rmodrall'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrecl(type,n_rec,l_rec,nxg,nyg,nzg)
c  determine length of record for different model grids
      character type*(*)
c  for a gridded 3d model with layers
      if (type(1:4) .eq. 'G3DL') then
        l_rec = nxg       ! length of record for grid
        n_rec = nyg * nzg ! number of records
c  for regular 2d or 3d grids
      elseif (type(1:4) .eq. 'GRID') then
        l_rec = nzg
        n_rec = nxg * nyg
      elseif (type(1:5) .eq. 'GOCAD') then
        l_rec = nxg       ! length of record for grid
        n_rec = nyg * nzg ! number of records
      endif
c      print'('' recl type='',a5,'' nx='',i5,'' ny='',i5,'' nz='',i5
c     1,'' n_rec='',i5,'' l_rec='',i5)',type,nxg,nyg,nzg,n_rec,l_rec
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrecl_old(type,n1,n2,nxg,nyg,nzg)
c  determine length of record for different model grids
      character type*(*)
c  for a gridded 3d model with layers
      if (type(1:4) .eq. 'G3DL') then
        n2 = nxg       ! length of record for grid
        n1 = nyg * nzg ! number of records
c  for regular 2d or 3d grids
      else
        n2 = nzg
        n1 = nxg * nyg
      endif
c      print'('' recl type='',a4,'' nx='',i5,'' ny='',i5,'' nz='',i5
c     1,'' n1='',i5,'' n2='',i5)',type,nxg,nyg,nzg,n1,n2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwall(hfile,dfile,wtype,mfile,tfile,type,stat,itrans
     1,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,ibw1,ibw2,ibw3,nbh,ibid,nbseg,bname,bcolor
     1,ivw1,ivw2,ivw3,nvh,ivid,nvseg,vname,vcolor
     1,icw1,icw2,icw3,nch,icid,ncseg,cname,ccolor
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg,zg,*)
c  write both a header file and a data file
c  if itrans .ne. 0 write transform file also
      character *(*) hfile,dfile,wtype,mfile,tfile,type,stat
     1,cxo,cyo,czo,cord
     1,bname(*),bcolor(*),vname(*),vcolor(*),cname(*),ccolor(*)
      character *16 cx,cy,cz
      data ndec,ipr,lu,ifile/0,0,0,1/
      dimension imv(1),icv(1),imc(1)

c  write info to both head and data file
      call rmodwrhd(hfile,dfile,wtype,mfile,tfile,type,stat,itrans
     1,cxo,cyo,czo,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,*999)

c  write horizon info
      call rmodwhor(hfile,'*HORIZON',ibw1,ibw2,ibw3
     1,nbh,ibid,nbseg,bname,bcolor,*999)
c  write velocity horizon info
      call rmodwhor(hfile,'*VHORIZON',ivw1,ivw2,ivw3
     1,nvh,ivid,nvseg,vname,vcolor,*999)
c  write cell horizon info
      call rmodwhor(hfile,'*CHORIZON',icw1,icw2,icw3
     1,nch,icid,ncseg,cname,ccolor,*999)
c  write model
      if (type .eq. 'LAYER') then
        call rmodwxcv(dfile,cxo,czo,cxo,czo,n_cord,x_cord,cord
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      elseif (type .eq. 'CELL') then
        call rmodwcel(dfile,cxo,czo,cxo,czo,n_cord,x_cord,cord
     1,nc,imc,ixc,nxc,xc,zc,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,mwork,work,*999)
      else    ! if (type .eq. 'CELL') then
        call rmodrecl(type,n1,n2,nxg,nyg,nzg)
        if (type .eq. 'G3DL') then
          call rmodvxyz(1,hfile,'*CELL'    ,cxo,cyo,czo
     1,mxb,ncv,xcv,ycv,zcv,vel,icv,itv,ixv)
          call rmodvxyz(1,hfile,'*VELOCITY',cxo,cyo,czo
     1,mxb,nv,xv,yv,zv,vel,itv,ixv,nxv)
          call rmodwrda(dfile,wtype,n1,n2,vg,*999)
        else
          call util_invert(n1*n2,vg)
          call rmodwrda(dfile,wtype,n1,n2,vg,*999)
          call util_invert(n1*n2,vg)
        endif
      endif    ! if (type .eq. 'LAYER') then

      return
  999 continue
      print'(/,'' error in rmodwall'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwcel(dfile,cxi,czi,cxo,czo,n_cord,x_cord,cord
     1,nc,imc,ixc,nxc,xc,zc,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,mwork,iwork,*)
c  write out a celled model
      character *(*) dfile,cxi,czi,cxo,czo,cord
      dimension iwork(1),xc(1)
      IF (NC .LE. mwork) GOTO 999
      call util_wors(i_work_i,i_work_n,mwork)
      call util_work(i_work_i,i_work_n,ic,nc)
      do 1 jc = 1 , nc
        iwork(jc) = jc
    1 continue
      call rmodwxcv(dfile,cxi,czi,cxo,czo,n_cord,x_cord,cord
     1,nc,iwork(ic),imc,ixc,nxc,xc,zc,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      return
  999 continue
      print'(/,'' error in rmodwcel'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwrda(dfile,wtype,n1,n2,v,*)
c  write a grid file
      character dfile*(*),wtype*(*)
      character lnode*32,los*16
      integer   key,ifile,dbutil_getlun,one,wdtypo,wdtypi
      integer   msg0(30),istat,rmod_lochost_w,j,i
      dimension v(1)

      one = 1
      call rmodopen(key,dfile,'UNFORMATTED','NEW','DIRECT'
     1,wtype,n2,ierr)
      call rmod_wordtype_ctoi(wtype,wdtypo)
c      wdtypo = 1 ! IEEE word type
      lnode = ' '
      los   = ' '
      wdtypi= rmod_lochost_w(lnode,los)
c      print'('' wdtypi='',i5,'' wdtypo='',i5,'' wtype='',a)'
c     1,wdtypi,wdtypo,wtype

      if (ierr .ne. 0) goto 999
      do 1 i = 1 , n1

        call rmodwrc(key,i, one, n2, v((i-1)*n2+1),
     1 wdtypo,wdtypi,istat, msg0)
cvoid rmodwrc_(long *key,long *irec, long *nrec, long *n, char *data,

      if (istat .ne.0) then
        print'(/,'' error during rmodwrc istat='',i8
     1,/,'' i='',i8,'' n1='',i8,'' n2='',i8)'
     1,istat,i,n1,n2
        print'('' j='',i8,'' msg='',a8)',(j,msg0(j),j=1,30)
        goto 999
      endif    ! if (istat .ne.0) then

    1 continue
c      print*,' key=',key,' n1=',n1,' n2=',n2
      return
  999 continue
      print'(/,'' error in rmodwrda'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwend(key)
c  write out *END CARD
      integer   key,ifile,dbutil_getlun
      ifile=dbutil_getlun(key)
      write(ifile,'(a)')' *END'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwchd(key,str,cx,cy,cz,ndof)
c  write out the header card
      character str*(*),cx*(*),cy*(*),cz*(*),crd132*132
      integer   key,ifile,dbutil_getlun
      ifile=dbutil_getlun(key)
      crd132 = ' '
      write(crd132(2:len(str)+1),'(a)')str
      write(crd132(13:),'(''NDOF='',i5)')NDOF
C      if (cy .eq. ' ') then
C        write(crd132(13:),'(''NDOF='',i5,'' XCOORDINATE='',a12
C     1,'' ZCOORDINATE='',a12)')ndof,cx(1:12),cz(1:12)
C        write(crd132(13:),'(''NDOF='',i5,'' XCOORDINATE='',a12
C     1,'' ZCOORDINATE='',a12)')ndof,cx(1:12),cz(1:12)
C      else
C        write(crd132(13:),'(''NDOF='',i5,'' XCOORDINATE='',a12
C     1,'' YCOORDINATE='',a12,'' ZCOORDINATE='',a12)')
C     1ndof,cx(1:12),cy(1:12),cz(1:12)
C      endif
      call rmodbdel(crd132)
      write(ifile,'(a)')crd132
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwrit(ifile,str,cx,cz,ndof,init,n,i1,i2,ix,nx,x,z,v)
c  read x,z points from ifile
      dimension i1(1),i2(1),ix(1),nx(1),x(1),z(1),v(1)
      character *(*) str,cx,cz
      if (init .ne. 0) call rmodwchd(ifile,str,cx,' ',cz,ndof)
      do 1 i = 1 , n
        do 2 j = ix(i)+1 , ix(i)+nx(i)
          call rmodwcrd(ifile,ndof,x(j),z(j),i1(i),i2(i),v(j))
    2   continue
    1 continue
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwcrd(ifile,ndof,x1,x2,ix3,ix4,x5)
c  write x1,x2,ix3,ix4,x5 to ifile
      parameter (mdof=5)
      dimension x(mdof)
      x(1) = x1
      x(2) = x2
      x(3) = ix3
      x(4) = ix4
      x(5) = x5
      call rmodwval(ifile,min(mdof,ndof),1,x)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrcrd(ifile,ndof,x1,x2,ix3,ix4,x5,*)
c  read ndof from ifile
      character crd80*80
      parameter (mdof=10)
      dimension x(mdof)
      data mx/1/
      x1 = 0
      x2 = 0
      ix3 = 0
      ix4 = 0
      x5 = 0
      call rmodrval(ifile,min(mdof,ndof),mx,nx,x,*2)
c      print'('' ndof='',i2,'' n='',i2,'' v='',6(1x,f6.0))'
c     1,ndof,nx,(x(i),i=1,ndof)
      if (nx .eq. 0) goto 2
      if (ndof .ge. 1) then
        x1 = x(1)
      endif
      if (ndof .ge. 2) then
        x2 = x(2)
      endif
      if (ndof .ge. 3) then
        ix3 = nint(x(3))
      endif
      if (ndof .ge. 4) then
        ix4 = nint(x(4))
      endif
      if (ndof .ge. 5) then
        x5 = x(5)
      endif
    1 return
    2 return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrval(key,ndof,mx,nx,x,*)
c  read a max of mx cards decode into ndof values within x
      character crd132*132
      dimension x(ndof,1)
      integer   key,ifile,dbutil_getlun,i_star,i_pound,ix
      ifile=dbutil_getlun(key)
      nx = 0
      do 1 ix = 1 , mx
    3   continue
        read(ifile,'(a)',err=2,end=2)crd132
        call rmodfchr(i_pound,'#',1,len(crd132),crd132)
        call rmodfchr(i_star,'*',1,len(crd132),crd132)
c        if (i_pound .ne. 0 .or. i_star .ne. 0)
c     1print'('' i#='',i8,'' i*='',i8,'' ix='',i5,'' nx='',i5
c     1,'' c='',a40)',i_pound,i_star,ix,nx,crd132(1:40)
        if (i_star .ne. 0) goto 2
        if (i_pound .ne. 0) goto 3
        nx = nx + 1
        call util_setr(ndof,x(1,nx),0.)
        call rmoddcod(crd132,ndof,n,x(1,nx))
    1 continue
    2 continue

      return
  999 continue
      print'(/,'' error in rmodrval'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmoddcod(card,mx,nx,x)
c  decode card into a max of mx real values within x
      character card*(*),cx*20
      dimension x(*)
      nx = 0
      ix = 0
      do 1 i = 1 , len(card)
        if (card(i:i) .ne. ' ') then
          ix = ix + 1
          if (ix .eq. 1) nx = nx + 1
          if (nx .gt. mx) then
            nx = mx
            return
          endif
          if (ix .eq. 1) cx = ' '
          cx(ix:ix) = card(i:i)
        elseif (ix .ne. 0) then
          call headdcod(x(nx),cx)
          ix = 0
        endif
    1 continue
      if (ix .ne. 0) call headdcod(x(nx),cx)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrstr(title,str)
c  read a character string
      integer   util_r
      character *(*) title,str
      character crd80*80
      integer   ic1,ic2,ic3,ic4,ic5,ic6,n80

      n80 = 80
      crd80 = ' '
      ic1 = 1
      ic2 = min(n80,ic1 + util_r(title) - 1)
      ic3 = min(n80,ic2+1)
      ic4 = min(n80,ic3 + 12 - 1)
      ic5 = min(n80,ic4 + 1)
      ic6 = min(n80,ic5 + util_r(str) - 1)
c      print*,' ic=',ic1,ic2,ic3,ic4,ic5,ic6
c      print'('' title='',a)',title(1:util_r(title))
c      print'('' str='',a)',str(1:util_r(str))

      crd80(ic1:ic2) = title(1:ic2-ic1+1)
      crd80(ic3:ic4) = ' - default= '
      crd80(ic5:ic6) = str(1:ic6-ic5+1)
      write(6,'(a)')crd80(ic1:ic6)
c      write(6,'(a)')  title(1:util_r(title)),' - default= '
c     1,str(1:util_r(str))
c     print '(a)',title//' - default= '//str
      crd80 = ' '
      read(*,'(a)',err=1,end=1)crd80
      call rmodcaps(crd80,crd80)
      if (crd80 .ne. ' ') read(crd80,'(a)')str
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwval(ifile,ndof,nx,x)
c  write nx cards encoded with ndof values of x each
      character crd132*132
      dimension x(ndof,1)
      do 1 ix = 1 , nx
        call rmodncod(ndof,x(1,ix),crd132)
        if (ndof .gt. 5) call rmodbdel(crd132)
        call rmodwcim(ifile,crd132)

    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwcim(key,card)
c  write card image to last blank
      character card*(*)
      integer   key,ifile,dbutil_getlun
      ifile=dbutil_getlun(key)
      call rmodlenr(lenc,card)
      write(ifile,'(a)')card(1:lenc)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmowwlef(str,card)
c  write str to card left justified
      character *(*)str,card
      call rmodlenr(lens,str)
      lenc = len(card)
      card(1:min(lens,lenc)) = str(1:min(lens,lenc))
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodncod(n,x,card)
c  encode card with n values of x
      character card*(*)
      dimension x(*)
      data lenfmt/16/
      lencard = len(card)
      card = ' '
c      print*,' n=',n,' lencard=',lencard
      do 1 i = 1 , n
        i1 = (i - 1) * lenfmt + 2
        i2 = min(lencard,i*lenfmt)
        j = nint(x(i))
        if (x(i) .eq. float(j)) then
          write(card(i1:),'(i9)')j
        else
          write(card(i1:),'(g15.6)')x(i)
        endif
c      print'('' i1='',i3,'' i2='',i3,'' c='',a20)',i1,i2,card(i1:i2)
        if (i2 .ge. lencard) return
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfcrd(ifile,irewind,str0,cx,cz,ndof,*)
c  find start of a section for old picks files skip 2 cards
      character *(*) str0,cx,cz
      character crd132*132,str*80
c  determine if this is an old or new file format old starts with *line
      call rmodcaps(str0,str)
      if (str(1:6) .eq. '*PICKS') call rmodnold(ifile,nold)
c  find start read cx,cz
      call rmodfstr(ifile,irewind,nrd,str,crd132,*1)
c  read cx,cz from card
      call headgvcc(crd132,cx,nchar,'XCOORDINATE')
      call headgvcc(crd132,cz,nchar,'ZCOORDINATE')
      call headgvic(crd132,ndof,'ndof')
c  if this is an old icp file skip the first 2 lines
      if (str(1:6) .eq. '*PICKS') call rmodncrd(ifile,nold,crd132,*1)
      return
    1 return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodncrd(key,ncrd,card,*)
c  read ncrd cards from ifile
      character *(*)card
      integer   key,ifile,dbutil_getlun
      ifile=dbutil_getlun(key)
      do 1 i = 1 , ncrd
        read(ifile,'(a)',err=2,end=2)card
    1 continue
      return
    2 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodnold(key,nold)
c  determine if this is an old icp (no nold=0, yes nold = 2)
      character crd132*132
      integer   key,ifile,dbutil_getlun
      ifile=dbutil_getlun(key)
      nold = 0
      rewind(ifile)
      read(ifile,'(a)',err=1,end=1)crd132
      if (crd132(2:6) .eq. '*LINE') nold = 2
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwxzv(ifile,init,title,cx,cz,n,im,it,ix,nx,x,z,v)
c  write data cards to ifile
      character *(*) title,cx,cz
      dimension ix(1),nx(1),im(1),it(1),x(1),z(1),v(1)
      ndof = 3
      if (title .eq. '*CELL') then
        ndof = 3
      elseif (title .eq. '*PICKS') then
        ndof = 4
      elseif(title .eq. '*VELOCITY') then
        ndof = 5
      endif
      ndof = min(5,ndof)
      if (init .ne. 0) call rmodwchd(ifile,title,cx,' ',cz,ndof)
      do 1 i = 1 , n
        if (ndof .eq. 3) then
          call rmodwcrd(ifile,ndof,x(i),z(i),im(i),i,v)
        else
          do 2 j = ix(i)+1 , ix(i)+nx(i)
            call rmodwcrd(ifile,ndof,x(j),z(j),im(i),it(j),v(j))
    2     continue
        endif
    1 continue
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrxzv(ifile,title,cx,cz,m,mx,n,im,it,ix,nx,x,z,v,*)
c  read velocity points from ifile
      dimension im(1),it(1),ix(1),nx(1),x(1),z(1),v(1)
      character *(*) title,cx,cz
c  find start read cx,cz
      call rmodrxz0(ifile,title,cx,cz,m,mx,n,im,it,ix,nx,x,z,v,*999)
      return
  999 continue
      print'(/,'' error in rmodrxzv m='',i8,'' n='',i8
     1,'' mx='',i8,'' nx='',i8)',m,n,mx,nx
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrxz0(ifile,title,cx,cz,m,mx,n,im,it,ix,nx,x,z,v,*)
c  read velocity points from ifile
      dimension im(1),it(1),ix(1),nx(1),x(1),z(1),v(1)
      character *(*) title,cx,cz
      data irewind/1/
      n = 0
c  find start read cx,cz
      call rmodfcrd(ifile,irewind,title,cx,cz,ndof,*2)
      if (title .eq. '*CELL') then
        ndof = 3
      elseif (title .eq. '*PICKS') then
        ndof = 4
      elseif(title .eq. '*VELOCITY' .or. title .eq. '*MODIFY') then
        ndof = 5
      endif
      ndof = max(3,min(5,ndof))

      j1 = 0
      j2 = 0
      kx = 0
      do 1 jx = 1 , mx
        call rmodrcrd(ifile,ndof,x(jx),z(jx),i1,i2,v0,*2)
        kx = kx + 1
        if (ndof .eq. 3) then
          n = jx
          im(jx) = i1
        else
          it(jx) = i2
          if (ndof .eq. 5) v(jx) = v0
          if (jx .eq. 1
     1.or. (ndof .eq. 4 .and. (i1 .ne. j1 .or. i2 .ne. j2))
     1.or. (ndof .eq. 5 .and. i1 .ne. j1)) then
            n = n + 1
            if (n .gt. m) goto 999
            ix(n) = jx - 1
            nx(n) = 0
            im(n) = i1
          endif
          nx(n) = nx(n) + 1
          j1 = i1
          j2 = i2
        endif
    1 continue
    2 continue

      if (n .ge. m .or. kx .ge. mx) goto 999

      return
  999 continue
      print'(/,'' error in rmodrxz0 m='',i8,'' n='',i8
     1,'' mx='',i8,'' nx='',i8)',m,n,mx,kx
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdda(file,wtype,n1,n2,v,work,*)
c read a grid file n1 records, n2 words per record
c  for cray work needs n2 words
      character*(*) file,wtype
      integer wdtyp,dbutil_putwordtype

      call rmodlenr(lf,file)
      if (file(max(1,lf-3):lf) .eq. 'NONE' .or. n1*n2 .le. 0) return
      call rmodopen(ifile,file,'UNFORMATTED','OLD','DIRECT'
     1,wtype,n2,ierr)
c      call rmod_wordtype_ctoi(wtype,wdtyp)
c      call dbutil_putwordtype(ifile,wdtyp)

      if (ierr .ne. 0) goto 999
c read grid,
      call rmodrrec(ifile,1,n1,n2,v,work,*998)
      return

  998 continue
      print'(/,'' error in rmodrdda read ierr='',i5,'' n1='',i8
     1,'' n2='',i8,'' wtype='',a16,/,'' file='',a)'
     1,ierr,n1,n2,wtype,file(1:lf)
      call rmodclof(file)
      return 1

  999 continue
      print'(/,'' error in rmodrdda open ierr='',i5,'' n1='',i8
     1,'' n2='',i8,'' wtype='',a16,/,'' file='',a)'
     1,ierr,n1,n2,wtype,file(1:lf)
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodftch(file,*)
c compose fetch command
      integer sizeof_real,n,cmd0(40),zero
      character file*(*)
      character cmd*132
      call rmodlenr(lf,file)
      if (lf .lt. 1) goto 998
      cmd = 'fetch '//file(1:lf)//' -f TR -t '//file(1:lf)
      call rmodlenr(lc,cmd)
c      print'('' fetch file='',a,/,'' cmd='',a)',file(1:lf),cmd(1:lf)
      zero = 0
      n = len(cmd)/sizeof_real()
      call convert_cc2hh(cmd,zero,cmd0,-n)
C&& DAY changed from ishell to rmod_shell_w
      ierr = rmod_shell_w(cmd)
      if (ierr.ne.0) goto 999
      return
  998 continue
c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(/,'' error in rmodftch file name '',a)',file(1:lf)
      return 1
  999 continue
      print'(/,'' error in rmodftch fetch ierr='',i5,'' file='',a)',ierr
     1,file(1:lf)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdvg(hfile
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,igrid,ndim,mg,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,mwork,work,lu,ierr)
c  get a gridded model from either a layered or gridded file
c  if (igrid .eq. 1 use the input values of nxg,nyg etc
      implicit none
      integer m_cord,n_cord,ndim,mg,nxg,nyg,nzg,igrid,mwork,lu,ierr
      real x_cord(2,*),xgmin,xginc,ygmin,yginc,zgmin,zginc,vg(mg)
      real work(mwork)
      character *(*) hfile,cxi,cyi,czi,cxo,cyo,czo,cord

      integer n1,n2,nxg0,nyg0,nzg0,mg0,iwk,ivg,nwork
      integer i_work_i,i_work_n
      real xmin,xmax,ymin,ymax,zmin,zmax,zdatum
      real xgmin0,xginc0,ygmin0,yginc0,zgmin0,zginc0
      character *80 dfile,mfile,tfile,type
      character *16 cx,cy,cz,cztemp
      data cx,cy,cz/' ',' ',' '/
c  read in a gridded velocity model
      ierr = 0

c  read in the model header
      call rmodrdgl(hfile
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg0,xgmin0,xginc0,nyg0,ygmin0,yginc0,nzg0,zgmin0,zginc0,ierr)
      if (ierr .ne. 0) goto 999

      mg0 = nxg0 * nyg0 * nzg0
      call util_wors(i_work_i,i_work_n,mwork)        ! setup work space
      call util_work(i_work_i,i_work_n,ivg,mg0)      ! get space for grid
      call util_worl(i_work_i,i_work_n,nwork)        ! how much work left
      if (nwork .lt. 0) goto 999
      call util_work(i_work_i,i_work_n,iwk,nwork)    ! get space for grid

c  read original grid
      call rmodrgrd(hfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,mg0,nxg0,xgmin0,xginc0,nyg0,ygmin0,yginc0,nzg0,zgmin0,zginc0
     1,work(ivg),nwork,work(iwk),lu,ierr)
      if (ierr .ne. 0) goto 999

c  if flag is not set use the input values
      if (igrid .eq. 0) then
        nxg = nxg0
        xgmin = xgmin0
        xginc = xginc0
        nyg = nyg0
        ygmin = ygmin0
        yginc = yginc0
        nzg = nzg0
        zgmin = zgmin0
        zginc = zginc0
        call util_copy(mg0,work(ivg),vg)
      else    ! if flag is not set use the input values
c  interpolate from input grid to output grid
      if (lu .ge. 0)
     1call rmodpvel(lu,' rmodrdvg - original grid','GRID'
     1,cxo,cyo,czo,n_cord,x_cord,cord
     1,nxg0,xgmin0,xginc0,nyg0,ygmin0,yginc0,nzg0,zgmin0,zginc0
     1,work(ivg),work(ivg),max(1,nyg0/1),max(1,nzg0/50))
        call util_interpolate_1(
     1 nzg0,zgmin0,zginc0
     1,nxg0,xgmin0,xginc0
     1,nyg0,ygmin0,yginc0
     1,work(ivg)
     1,nzg,zgmin,zginc
     1,nxg,xgmin,xginc
     1,nyg,ygmin,yginc
     1,vg
     1,nwork,work(iwk)
     1,ierr)
        if (ierr .ne. 0) goto 999
      endif    ! if flag is not set use the input values

      if (lu .ge. 0)
     1call rmodpvel(lu,' rmodrdvg - final grid','GRID'
     1,cxo,cyo,czo,n_cord,x_cord,cord
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,vg,vg,max(1,nyg/1),max(1,nzg/50))

      return
  999 continue
      ierr = -1
      print'(/,'' error in rmodrdvg'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrgrd(hfile,type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,mg,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,mwork,work,lu,i_err)
      character *(*) hfile,type,cxi,cyi,czi,cxo,cyo,czo,cord
      character *64 dfile,wtype,mfile,tfile
      character *16 cx,cy,cz
      data ndim/2/,cx,cy,cz/' ',' ',' '/
      dimension work(1)

c  read header file
      i_err = 0
      call rmodrdhd(hfile,dfile,wtype,mfile,tfile,type
     1,cxi,cyi,czi,cx,cy,cz,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ndim,lu,*996)
c      print'('' rgrd cxi='',a16,'' cxo='',a16
c     1,'' czi='',a16,'' czo='',a16
c     1,/,'' zmin='',f10.2,'' zmax='',f10.2,/,'' zgmin='',f12.4
c     1,'' zginc='',f12.4)',cxi,cxo,czi,czo,zmin,zmax,zgmin,zginc
c      print'('' wtype='',a16,'' hfile='',a
c     1,/,'' dfile='',a)',wtype,hfile,dfile
c  note if mg< 0 vg and work share space so shift work by nxg*nzg
      if (mg .lt. 0) then
        call util_wors(i_work_i,i_work_n,mwork)        ! setup work space
        call util_work(i_work_i,i_work_n,ivg,nxg*nzg)  ! get space for grid
        call util_worl(i_work_i,i_work_n,nwork)        ! how much work left
        if (nwork .lt. 0) goto 997
        call util_work(i_work_i,i_work_n,iwork,nwork)  ! set work space to after velocity
      else
        if (nxg*nyg*nzg .gt. mg) then
          print'('' not enough space for grid file mg='',i8
     1,'' nxg='',i8,'' nyg='',i8,'' nzg='',i8)',mg,nxg,nyg,nzg
          goto 997
        endif
        iwork = 1
        nwork = mwork
      endif
c  read grid - note velocity is slowness
      call rmodvgrd(dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,nwork,work(iwork),*998)
      return

  996 continue
      print'(/,'' error in rmodrgrd during rdhd'')'
      goto 999
  997 continue
      print'(/,'' error in rmodrgrd during memory allocation'')'
      goto 999
  998 continue
      print'(/,'' error in rmodrgrd during vgrd'')'
      goto 999
  999 continue
      print'(/,'' error in rmodrgrd'')'
      i_err = -1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodvgrd(dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,mwork,work,*)
c  get a gridded model from either a layered or gridded file
      character *(*) dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,cord
      character *16 cx,cy,cz,cztemp
      data cx,cy,cz/' ',' ',' '/
c  read in a gridded velocity model
      dimension vg(1),work(mwork)

c  read in the gridded model from a vax file
      call rmodrecl(type,n1,n2,nxg,nyg,nzg)

      if (type(1:1) .eq. 'G') then

c  read the grid file for the 3d layered model
        if (wtype(1:5) .eq. 'AGRID') then

          call rmod_read_agrid(dfile,n1*n2,vg,i_err)
          if (i_err .ne. 0) goto 997
          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! convert

        elseif (wtype(1:5) .eq. 'ASCII') then

c  top of vax change
c  on cray use the following, on the vax comment it out
c          call rmod_read_ascii(type,dfile,n1*n2,vg,i_err)
c          if (i_err .ne. 0) goto 997
c          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! conver
c  end of vax change

        elseif (wtype(1:7) .ne. 'GENERIC'
     1.and. wtype(1:8) .ne. 'CHARISMA') then

          call rmodrdda(dfile,wtype,n1,n2,vg,work,*997)
          call rmod_gocad_to_grid(type,nxg,nyg,nzg,vg,mwork,work,i_err)
          if (i_err .ne. 0) goto 996
          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! convert

        endif    ! if (wtype(1:5) .eq. 'AGRID') then

      else    ! if (type(1:1) .eq. 'G') then
c  read the layered model and convert to gridded model
        call rmodltog(dfile,type
     1,cxi,czi,cx,cz,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,nxg,xgmin,xginc,nzg,zgmin,zginc,vg,mwork,work,lu,*998)
      endif    ! if (type(1:1) .eq. 'G') then
      if (type(1:4) .eq. 'G3DL') then
        if (czi .eq. 'TIME' .and. czo .eq. 'DEPTH'
     1 .or. czo .eq. 'TIME' .and. czi .eq. 'DEPTH') then
c          call rmodlrzt(nxg*nyg,nzg,vg,vg,zdatum,czi)
        else
          call rmodcnvn(nxg*nyg*nzg,vg,czi,czo,n_cord,x_cord,cord)
        endif

c  if need be convert the grid from time to depth or depth to time
      else
        if ((czi .eq. 'DEPTH' .and. czo .eq. 'TIME' )
     1 .or. (czi .eq. 'TIME'  .and. czo .eq. 'DEPTH') ) then
          zgmin0 = zgmin
          zginc0 = zginc
          cztemp = czi
          call ztot_convert_velocity_n(cztemp,n_cord,x_cord,cord,zdatum
     1,nxg*nyg,nzg,zgmin0,zginc0,vg,mwork,work,*999)
        endif
      endif

      call rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord)

      call rmod_reverse_negative_increment(type
     1,nxg,xgmin,xginc
     1,nyg,ygmin,yginc
     1,nzg,zgmin,zginc
     1,vg
     1)

      return

  996 continue
      print'(/,'' error in rmodvgrd during rmod_gocad_to_grid''
     1,/,'' n1='',i5,'' n2='',i5
     1,/,'' dfile='',a,/,'' wtype='',a)',n1,n2,dfile,wtype
      return 1

  997 continue
      print'(/,'' error in rmodvgrd during rmodrdda''
     1,/,'' n1='',i5,'' n2='',i5
     1,/,'' dfile='',a,/,'' wtype='',a)',n1,n2,dfile,wtype
      return 1

  998 continue
      print'(/,'' error in rmodvgrd during ltog''
     1,/,'' dfile='',a)',dfile
      return 1

  999 continue
      print'(/,'' error in rmodvgrd during ztot'')'
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_grid(mod_file
     1,cxo,cyo,czo
     1,m_cord,n_cord,x_cord,cord
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,m_vel,vel
     1,m_work,work
     1,i_err)
c  read a gridded velocity model
c  output slowness in x,y,z coordinates defined by cxo,cyo,zo
c
c  input parameters
c
c mod_file char*(*)= velocity model file name
c cxo      char*16 = output x coordinate
c cyo      char*16 = output y coordinate
c czo      char*16 = output z coordinate
c m_cord   int     = dimension of arrays cord and x_cord
c m_vel    int     = dimension of velocity array - vel
c m_work   int     = dimension of work array - work
c work     real    = work array
c
c  output parameters
c
c n_cord   int     = number of transform pairs in arrays cord and x_cord
c x_cord   real    = array of transform pairs
c cord     char*16 = array of transform names
c
c nx_vel   int     = number of velocity grid x points
c x0_vel   real    = first velocity grid x value in cxo units
c dx_vel   real    = velocity grid x value spacing in cxo unitsc
c ny_vel   int     = number of velocity grid y points
c y0_vel   real    = first velocity grid y value in cyo units
c dy_vel   real    = velocity grid y value spacing in cyo units
c
c nz_vel   int     = number of velocity grid z points
c z0_vel   real    = first velocity grid z value in czo units
c dz_vel   real    = velocity grid z value spacing in czo units
c
c vel      real    = velocity array (values are returned as slowness)
c
c i_err    int     = error flag i_err = 0 all is o.k.
c
c  use the following in the shell routine to define the transform pairs
c
c      character cxo*16,cyo*16,czo*16
c      data      cxo,cyo,czo/'XBASEMENT','YBASEMENT','DEPTH'/
c      integer   m_cord
c      parameter (m_cord=20)
c      character cord(m_cord)*16
c      integer   n_cord
c      real      x_cord(2,m_cord)

      character mod_file*(*)

      character cxo*16,cyo*16,czo*16

      integer   m_cord
      character cord(m_cord)*16
      integer   n_cord
      real      x_cord(2,m_cord)

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      integer   m_vel
      real      vel(m_vel)

      integer   m_work
      real      work(m_work)

      integer   i_err

      character modify*80
      character d_file*80
      character m_file*80
      character t_file*80
      character w_type*8
      character mod_type*8

      integer   n_vel
      integer   n_dim
      real      z_datum

      character cxi*16,cyi*16,czi*16

      character cx*16,cy*16,cz*16
      data      cx,cy,cz/ ' ',' ',' '/

      i_err   = 0

      modify  = 'NONE' ! modify file name
      n_dim   = 3      ! dimensionality
      z_datum = 0.     ! datum level
      lu      = -1     ! logical unit for printout -1 = no printout
      nx_mix  = 1      ! x grid smoothing diameter
      ny_mix  = 1      ! y grid smoothing diameter
      nz_mix  = 1      ! z grid smoothing diameter

c  read in the header file 
c  do not change coordinates for x,y,z values
      call rmodrdhd(mod_file
     1,d_file,w_type,m_file,t_file,mod_type
     1,cxi,cyi,czi
     1,cx ,cy ,cz 
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,n_dim,lu,*999)
      call rmodmdpf(modify)

c  total amount of memory needed for velocity grid
      n_vel = nx_vel * ny_vel * nz_vel

      if (n_vel .gt. m_vel) then
        print'(/,'' error in rtdm_read_model ''
     1,/,'' not enough memory for velocity grid n_vel='',i8
     1,'' m_vel='',i8)',n_vel,m_vel
        goto 999
      endif

c  read in the velocity grid - SLOWNESS is returned in vel
c  convert all x,y,z values to output coordinates defined by cxo,cyo,czo
      call rmod_read_grid_1(d_file,w_type,mod_type
     1,cxi,cyi,czi
     1,cxo,cyo,czo
     1,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,nx_mix,ny_mix,nz_mix
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_work,work
     1,*999)

      return

  999 continue
      print'(/,'' error in rmod_read_grid'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_grid_1(dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,vg
     1,mwork,work,*)
c  get a gridded model from either a layered or gridded file
      character *(*) dfile,wtype,type
     1,cxi,cyi,czi,cxo,cyo,czo,cord
      character *16 cx,cy,cz,cztemp
      data cx,cy,cz/' ',' ',' '/
c  read in a gridded velocity model
      dimension vg(1),work(mwork)

c  read in the gridded model from a vax file
      call rmodrecl(type,n1,n2,nxg,nyg,nzg)

      if (type(1:1) .eq. 'G') then

c  read the grid file for the 3d layered model
        if (wtype(1:5) .eq. 'AGRID') then

          call rmod_read_agrid(dfile,n1*n2,vg,i_err)
          if (i_err .ne. 0) goto 997
          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! convert

        elseif (wtype(1:5) .eq. 'ASCII') then

c  top of vax change
c  on cray use the following, on the vax comment it out
c          call rmod_read_ascii(type,dfile,n1*n2,vg,i_err)
c          if (i_err .ne. 0) goto 997
c          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! conver
c  end of vax change

        elseif (wtype(1:7) .ne. 'GENERIC'
     1.and. wtype(1:8) .ne. 'CHARISMA') then

          call rmodrdda(dfile,wtype,n1,n2,vg,work,*997)
          call rmod_gocad_to_grid(type,nxg,nyg,nzg,vg,mwork,work,i_err)
          if (i_err .ne. 0) goto 996
          if (type(1:4) .eq. 'GRID') call util_invert(n1*n2,vg)! convert

        endif    ! if (wtype(1:5) .eq. 'AGRID') then

      else    ! if (type(1:1) .eq. 'G') then

c  read the layered model and convert to gridded model
        call rmodltog(dfile,type
     1,cxi,czi,cx,cz,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,nxg,xgmin,xginc,nzg,zgmin,zginc,vg,mwork,work,lu,*998)

      endif    ! if (type(1:1) .eq. 'G') then

      if (type(1:4) .eq. 'G3DL') then

        if (czi .eq. 'TIME' .and. czo .eq. 'DEPTH'
     1 .or. czo .eq. 'TIME' .and. czi .eq. 'DEPTH') then

c          call rmodlrzt(nxg*nyg,nzg,vg,zg,zdatum,czi)

        else

          call rmodcnvn(nxg*nyg*nzg,vg,czi,czo,n_cord,x_cord,cord)

        endif

c  if need be convert the grid from time to depth or depth to time
      else

        if (  (czi .eq. 'DEPTH' .and. czo .eq. 'TIME')
     1 .or. (czi .eq. 'TIME' .and. czo .eq. 'DEPTH') ) then

          zgmin0 = zgmin
          zginc0 = zginc
          cztemp = czi
          call ztot_convert_velocity_n(cztemp,n_cord,x_cord,cord,zdatum
     1,nxg*nyg,nzg,zgmin0,zginc0,vg,mwork,work,*999)

        endif

      endif

      call rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord)

      return

  996 continue
      print'(/,'' error in rmodvgrd during rmod_gocad_to_grid''
     1,/,'' n1='',i5,'' n2='',i5
     1,/,'' dfile='',a,/,'' wtype='',a)',n1,n2,dfile,wtype
      return 1

  997 continue
      print'(/,'' error in rmodvgrd during rmodrdda''
     1,/,'' n1='',i5,'' n2='',i5
     1,/,'' dfile='',a,/,'' wtype='',a)',n1,n2,dfile,wtype
      return 1

  998 continue
      print'(/,'' error in rmodvgrd during ltog''
     1,/,'' dfile='',a)',dfile
      return 1

  999 continue
      print'(/,'' error in rmodvgrd during ztot'')'
      return 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_agrid(dfile,n_vel,vel,i_err)
c  read an agrid file
      implicit none
      integer   rmod_len_r
      character dfile*(*)
      integer   n_vel
      real      vel(n_vel)
      integer   i_err

      integer   i_file
      integer   i_vel

      call rmodopen(i_file,dfile,'FORMATTED','OLD','SEQUENTIAL','IEEE'
     1,0,i_err)
      if (i_err .ne. 0) goto 998

      do i_vel = 1 , n_vel
        read(i_file,*,err=996,end=997)vel(i_vel)
      enddo    ! do i_vel = 1 , n_vel

      call rmodclos(i_file)

      return

  996 continue
      print'(/,'' error in rmod_read_agrid during read'')'
      goto 999

  997 continue
      print'(/,'' end in rmod_read_agrid during read'')'
      goto 999

  998 continue
      print'(/,'' error in rmod_read_agrid during open'')'
      goto 999

  999 continue
      print'(/,'' error in rmod_read_agrid n_vel='',i8,'' file='',a)'
     1,n_vel,dfile(1:rmod_len_r(dfile))
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_ascii(type,dfile,n_vel,vel,i_err)
c  read an ascii file
      implicit none
      integer   rmod_len_r
      character type*(*),dfile*(*)
      integer   n_vel
      real      vel(n_vel)
      integer   i_err

      integer   i_file
      integer   i_vel
      integer   x,y,z,v

      call rmodopen(i_file,dfile,'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
      if (i_err .ne. 0) goto 998

      if (type(1:4) .eq. 'G3DL') then
        do i_vel = 1 , n_vel
          read(i_file,*,err=996,end=997)x,y,z
          vel(i_vel) = z
        enddo    ! do i_vel = 1 , n_vel
      else    ! if (type(1:4) .eq. 'G3DL') then
        do i_vel = 1 , n_vel
          read(i_file,*,err=996,end=997)x,y,v
          vel(i_vel) = v
        enddo    ! do i_vel = 1 , n_vel
      endif    ! if (type(1:4) .eq. 'G3DL') then

      call rmodclos(i_file)

      return

  996 continue
      print'(/,'' error in rmod_read_ascii during read'')'
      goto 999

  997 continue
      print'(/,'' end in rmod_read_ascii during read'')'
      goto 999

  998 continue
      print'(/,'' error in rmod_read_ascii during open'')'
      goto 999

  999 continue
      print'(/,'' error in rmod_read_ascii n_vel='',i8,'' file='',a)'
     1,n_vel,dfile(1:rmod_len_r(dfile))
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodltog(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,nxg,xgmin,xginc,nzg,zgmin,zginc,vg,mwork,work,lu,*)
c  read a laered model and output a gridded model
c  vg = sllowness
      dimension work(1)
      character *(*) dfile,type,cxi,czi,cxo,czo,cord
      data ipr/0/
c  read cell model into work array
      call rmodrdcw(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,nv,imv,itv,ixv,nxv,jxv,izv,ivel,nc,imc,ixc,nxc,jxc,izc
     1,nwork,iwork,mwork,work,lu,*999)

c  convert layer model to gridded model
      idir = 1
      call gtol(idir,nv,work(imv),work(itv),work(ixv),work(nxv)
     1,work(jxv),work(izv),work(ivel)
     1,nc,work(imc),work(ixc),work(nxc),work(jxc),work(izc)
     1,nxg,xgmin,xginc,nzg,zgmin,zginc,vg,nwork,work(iwork),ierr)
      call util_invert(nxg*nzg,vg)
      if (ierr .ne. 0) goto 999

      return
  999 continue
      print'(/,'' error in rmodltog'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdcw(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,nv,imv,itv,ixv,nxv,jxv,izv,ivel,nc,imc,ixc,nxc,jxc,izc
     1,nwork,iwork,mwork,work,lu,*)
c  read a celled model into work output pointers
      dimension work(mwork)
      character *(*) dfile,type,cxi,czi,cxo,czo,cord
      data icall/0/
      icall = icall + 1
      init = 0                  ! init = 0 do not initialize work counte
      mb = 200                  ! max number of boundaries
      mc = 200                  ! max number of cells
      mxb = 25000               ! max number of points in boundaries
      mxc = 25000               ! max number of points in cells

      call util_wors(i_work_i,i_work_n,mwork)     ! setup work space
      call util_setr(mwork,work,0.)

c  boundary stuff init = 0 do not initialize work counter
      call rmodwrkb(init,i_work_i,i_work_n
     1,iwork,nwork,mb,mxb,imb,itb,ixb,nxb
     1,jxb,izb,imv,itv,ixv,nxv,jxv,izv,ivel,icv,ixcv,izcv,*999)

c  cell stuff init = 0 do not initialize work counter
      call rmodwrkc(init,i_work_i,i_work_n
     1,iwork,mwork,mc,mxc,imc,ixc,nxc,jxc,izc,*999)

      call util_worl(i_work_i,i_work_n,nwork)
      call util_work(i_work_i,i_work_n,iwork,nwork)
      ndec = 0    ! # of points to decimate
      makec = 1    ! make cells

      call rmodrdcl(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb
     1,work(imb),work(itb),work(ixb),work(nxb),work(jxb),work(izb)
     1,nv,work(imv),work(itv),work(ixv),work(nxv),work(jxv)
     1,work(izv),work(ivel),ncv,work(icv),work(ixcv),work(izcv)
     1,mc,mxc,nc,work(imc),work(ixc),work(nxc),work(jxc),work(izc)
     1,makec,nwork,work(iwork),lu,*999)

      return
  999 continue
      print'(/,'' error in rmodrdcw'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwrkb(init,i_work_i,i_work_n
     1,iwork,nwork,mb,mxb,imb,itb,ixb,nxb
     1,jxb,izb,imv,itv,ixv,nxv,jxv,izv,ivel,icv,ixcv,izcv,*)
c  get boundary pointers init = 0 do not initialize work counter

      if (init .ne. 0) then
        call util_wors(i_work_i,i_work_n,mwork)
c        call util_wori(i_work_i,i_work_n,iwork,mwork)
      endif

      call util_work(i_work_i,i_work_n,ixb,mb)
      call util_work(i_work_i,i_work_n,nxb,mb)
      call util_work(i_work_i,i_work_n,imb,mxb)
      call util_work(i_work_i,i_work_n,itb,mxb)
      call util_work(i_work_i,i_work_n,jxb,mxb)
      call util_work(i_work_i,i_work_n,izb,mxb)
      call util_work(i_work_i,i_work_n,ixv,mb)
      call util_work(i_work_i,i_work_n,nxv,mb)
      call util_work(i_work_i,i_work_n,imv,mxb)
      call util_work(i_work_i,i_work_n,itv,mxb)
      call util_work(i_work_i,i_work_n,jxv,mxb)
      call util_work(i_work_i,i_work_n,izv,mxb)
      call util_work(i_work_i,i_work_n,ivel,mxb)
      call util_work(i_work_i,i_work_n,icv,mb)
      call util_work(i_work_i,i_work_n,ixcv,mb)
      call util_work(i_work_i,i_work_n,izcv,mb)
      call util_worc(i_work_i,i_work_n,ierr)
      if (ierr .ne. 0) goto 999

      return
  999 continue
      print'(/,'' error in rmodwrkb'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwrkc(init,i_work_i,i_work_n
     1,iwork,mwork,mc,mxc,imc,ixc,nxc,jxc,izc,*)
c  get cell pointers init = 0 do not initialize work counter
      if (init .ne. 0) then
        call util_wors(i_work_i,i_work_n,mwork)
c        call util_wori(i_work_i,i_work_n,iwork,mwork)
      endif

      call util_work(i_work_i,i_work_n,imc,mc)
      call util_work(i_work_i,i_work_n,ixc,mc)
      call util_work(i_work_i,i_work_n,nxc,mc)
      call util_work(i_work_i,i_work_n,jxc,mxc)
      call util_work(i_work_i,i_work_n,izc,mxc)
      call util_worc(i_work_i,i_work_n,ierr)
      if (ierr .ne. 0) goto 999

      return
  999 continue
      print'(/,'' error in rmodwrkc'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdcl(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,makec,mwork,work,lu,*)
c  read in a data file and create cells
      character *(*) dfile,type,cxi,czi,cxo,czo,cord
      dimension ixc(1),nxc(1)
      data ipr/0/

      call rmodctyp(type,lu,*999)
      if (type .eq. 'CELL') then    ! cell format
        call rmodrdsc(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mc,mxc,nc,imb,imc,ixc,nxc,xc,zc,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      else    ! if (type .eq. 'CELL') then
        call rmodrdsc(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
c      call rmodwplt(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb,6)
c      call cellprnb(' aft rdsc',nb,imb,itb,ixb,nxb,xb,zb)
c      call cellprnv(' aft rdsc'
c     1,ncv,icv,xcv,zcv,nv,imv,itv,ixv,nxv,xv,zv,vel)

c  construct cells
        if (makec .ne. 0) then

          call cellclvp(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc,mwork,work,ierr)
          if (ierr .ne. 0) goto 999

        endif    ! if (makec .ne. 0) then

      endif ! if (type .eq. 'CELL') then

      return
  999 continue
      if (lu .gt. 0) write(lu,'(/,'' error in rmodrdcl'')')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdsc(dfile,type
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*)
c  read boundaryies in sheins format or icp format
      character *(*) dfile,type,cxi,czi,cxo,czo,cord
      data lu/6/

      call rmodshei(dfile,itype,*999)
      call rmodctyp(type,lu,*999)
      if (type .eq. 'SHEIN' .or. itype .eq. 1) then
        call rmodrold(dfile
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      else
        call rmodrxcv(dfile
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*999)
      endif
      return
  999 continue
      print'(/,'' error in rmodrdsc'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrold(dfile
     1,cxi,czi,cxo,czo,n_cord,x_cord,cord,xmin,xmax,zmin,zmax
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,*)
c  read model in sheins format
      dimension imb(1),itb(1),ixb(1),nxb(1),xb(1),zb(1),imv(1),itv(1)
     1,ixv(1),nxv(1),xv(1),zv(1),vel(1),xcv(1),zcv(1),icv(1)
     1,xtop(2),ztop(2)
      character *(*) dfile,cxi,czi,cxo,czo,cord
      character cxp*16,czp*16
      integer ifile,key,dbutil_getlun

      call rmodopen(key,dfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999
      ifile = dbutil_getlun(key)

      cxp = cxi
      czp = czi

      rewind(ifile)
      read(ifile,*,err=999)shot,base,baseinc,nbhmod
      if (nbhmod .eq. 7) then
        cxp = 'xannotation'
      else
        cxp = 'xbasement'
      endif
      read(ifile,*,err=999)nb,nxb1,(nxb(ib),ib=1,nb-1)
      nb = nb - 1    ! get rid of the top interface
      if (nb .gt. mb) then
        print*,' error in rmodrold have mb=',mb,' need nb=',nb
        goto 999
      endif
c  read the first line
      read(ifile,*,err=999)idb,xtop(1),ztop(1)
     1,(idb,xtop(2),ztop(2),j=1,nxb1-1)
c  for each subsequent boundary read the velocity and then x,z
      ixv(1) = 0
      ixb(1) = 0
      do 2 ib = 1 , nb
        if (ib .gt. 1) ixv(ib) = ixv(ib-1) + nxv(ib-1)
        if (ib .gt. 1) ixb(ib) = ixb(ib-1) + nxb(ib-1)
        read(ifile,*,err=999)nxv(ib),(xv(j),vel(j)
     1,j=ixv(ib)+1,min(mxb,ixv(ib)+iabs(nxv(ib))))
        read(ifile,*,err=999)(idb,xb(j),zb(j)
     1,j=ixb(ib)+1,min(mxb,ixb(ib)+nxb(ib)))
        if (ixv(ib)+iabs(nxv(ib)) .gt. mxb) then
          print*,' error in rmodrold need more space in arrays'
          print*,' xv,v arrays have dimension =',mxb
     1,' and need',ixv(ib)+iabs(nxv(ib))
          goto 999
        endif
        if (ixb(ib)+nxb(ib) .gt. mxb) then
          print*,' error in rmodrold need more space in arrays'
          print*,' x,z arrays have dimension =',mxb
     1,' and need',ixb(ib)+nxb(ib)
          goto 999
        endif
    2 continue

c  set min and max values
      xmin = xtop(1)
      xmax = xtop(2)
      zmin = ztop(1)
      zmax = zb(ixb(nb)+1)

c  set velocity values
      nv = nb
      ncv = nb
      call util_setr(ixv(nb)+iabs(nxv(nb)),zv,0.)
      do 3 ib = 1 , nb
        itv(ib) = ib
        if (nxv(ib) .lt. 0) then
          nxv(ib) = iabs(nxv(ib))
          call util_copy(nxv(ib),xv(ixv(ib)+1),zv(ixv(ib)+1))
          call util_setr(nxv(ib),xv(ixv(ib)+1),xmin)
        endif
        imb(ib) = ib
        itb(ib) = 1
        imv(ib) = ib
        icv(ib) = ib
        call rmodbpnt(xcv(ib),zcv(ib)
     1,nxb(ib),xb(ixb(ib)+1),zb(ixb(ib)+1),ierr,*999)
    3 continue

      nb = nb - 1    ! get rid of the bottom interface

c  convert x,z values to output coordinates
      call rmodcnlr(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,cxp,czp,cxo,czo,n_cord,x_cord,cord)
c      call rmodwplt(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb,88)

      return
  999 continue
      print*,' error in rmodrold'
      ierr = 101
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodbpnt(x0,z0,n,x,z,ierr,*)
c  find a point x0,z0 just above an array x,z
      dimension x(1),z(1)
      i2 = n - 1
      i1 = min(n/2+1,i2)
      do 1 is = 1 , -1 , -2
        do 2 i = i1 , i2 , is
          if (x(i) .ne. x(i+1)) then
            x0 = (x(i) + x(i+1)) / 2.
            z0 = (z(i) + z(i+1)) / 2. - 10.
            return
          endif
    2   continue
        i1 = i1 - 1
        i2 = 1
    1 continue
  999 continue
      print*,' error in rmodbpnt'
      ierr = 102
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodctyp(type,lu,*)
c  make sure type is acceptable
c  if lu .gt. 0 write error message to logical unit lu
      parameter (mtype=7)
      character type*(*)
      character gtype(mtype)*16
      data ntype/mtype/
     1,gtype/'LAYER','GRID','SHEIN','CELL','G3DL','G3DG','GOCAD'/
      call rmodcaps(type,type)
      do 1 i = 1 , ntype
        if (type(1:3) .eq. gtype(i)(1:3)) type = gtype(i)
        if (type .eq. gtype(i)) return
    1 continue
      if (lu .gt. 0) then
        write(lu,'(/,'' error in rmodctyp model type is not supported''
     1,/,'' input model type='',a20)')type
        do 2 i = 1 , ntype
          write(lu,'('' supported model type='',a20)')gtype(i)
    2   continue
      endif
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodprhd(lu,title,hfile,dfile,tfile,type
     1,cx,cy,cz,n_cord,x_cord,cord,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)
c  if lu .gt. 0 print header file parameters
      character *(*) hfile,dfile,tfile,type,cx,cy,cz,cord,title
      if (lu .lt. 0) return

      call rmodphdr(lu,title,hfile,dfile,tfile,type,cx,cy,cz)
      call rmodpcor(lu,n_cord,x_cord,cord)

      call rmodpgrd(lu,title,cx,cy,cz,n_cord,x_cord,cord
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)

      call rmodplay(lu,title,cx,cy,cz,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodphdr(lu,title,hfile,dfile,tfile,type,cx,cy,cz)
c  print info about files
      character *(*) title,hfile,dfile,tfile,type,cx,cy,cz
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return
      if (lu .lt. 0) return
      call rmodlenr(li,title)
      call rmodlenr(lh,hfile)
      call rmodlenr(lt,tfile)
      call rmodlenr(ld,dfile)
      call rmodlenr(lp,type)
      write(lu,'(/,'' model parameters '',a
     1,/,'' header file name         = '',a
     1,/,'' model file name          = '',a
     1,/,'' transformation file name = '',a
     1,/,'' model type               = '',a
     1,/,'' input x coordinate       = '',a16
     1,/,'' input y coordinate       = '',a16
     1,/,'' input z coordinate       = '',a16)')
     1title(1:li),hfile(1:lh),dfile(1:ld),tfile(1:lt),type,cx,cy,cz
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodplay(lu,title,cxi,cyi,czi,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum)
c  print layered model limits

      implicit none

      character *(*) title,cxi,cyi,czi,cord
      integer lu,n_cord
      real    xmin,xmax,ymin,ymax,zmin,zmax,zdatum
      real    x_cord(*)
      integer lt
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return

      if (lu .lt. 0) return

      call rmodlenr(lt,title)
      write(lu,'(/,'' Layered model limits '',a)')title(1:lt)
      write(lu,'('' ZDATUM='',f10.2)')zdatum

      call rmodpla1(lu,xmin,xmax,'X',cxi,n_cord,x_cord,cord)
      call rmodpla1(lu,ymin,ymax,'Y',cyi,n_cord,x_cord,cord)
      call rmodpla1(lu,zmin,zmax,'Z',czi,n_cord,x_cord,cord)

      return
      end


C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpla1(lu,xmin,xmax,c0,cxi,n_cord,x_cord,cord)

      implicit none

      character c0*1,cxi*(*),cord(*)*16
      integer lu,n_cord
      real xmin,xmax
      real x_cord(*)

      integer isame,i
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return

      isame = 0

      write(lu,'(/,'' '',a1,'' Coordinate units='',a16
     1,/,''       minimum           maximum     units'')')c0,cxi

      do i = 1 , n_cord

        if (
     1( (c0.eq.'X' .or. c0.eq.'Y')  .and. cord(i)(1:1).eq.c0)
     1 .or.
     1(c0.eq.'Z' .and. cord(i)(1:1).ne.'X' .and. cord(i)(1:1).ne.'Y')
     1     ) then

          if (cord(i) .eq. cxi) isame = i
          call rmodcnmm(xmin,xmax,cxi,cord(i),n_cord,x_cord,cord)
          write(lu,'(1x,g16.9,1x,g16.9,1x,a16)')xmin,xmax,cord(i)
          call rmodcnmm(xmin,xmax,cord(i),cxi,n_cord,x_cord,cord)

         endif    ! if (

      enddo    ! do i = 1 , n_cord

      if (isame .eq. 0)
     1write(lu,'(1x,g16.9,1x,g16.9,1x,a16)')xmin,xmax,cxi

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpgrd(lu,title,cxi,cyi,czi,n_cord,x_cord,cord
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)
c  print grid model limits
      implicit none

      character *(*) title,cxi,cyi,czi,cord(*)
      integer lu,nxg,nyg,nzg,n_cord
      real xgmin,xginc,ygmin,yginc,zgmin,zginc
      real x_cord(*)

      integer lt

      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return
      if (lu .lt. 0 .or. lu .gt. 100) then
        print*,' error in rmodpgrd lu=',lu
        return
      endif

      call rmodlenr(lt,title)
      write(lu,'(/,'' Gridded model limits '',a)')title(1:lT)

      call rmodpgr1(lu,nxg,xgmin,xginc,'X',cxi,n_cord,x_cord,cord)
      call rmodpgr1(lu,nyg,ygmin,yginc,'Y',cyi,n_cord,x_cord,cord)
      call rmodpgr1(lu,nzg,zgmin,zginc,'Z',czi,n_cord,x_cord,cord)

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpgr1(lu,nxg,xgmin,xginc,c0,cxi,n_cord,x_cord,cord)

      implicit none

      character c0*1,cxi*(*),cord(*)*16
      integer lu,nxg,n_cord
      real xgmin,xginc
      real x_cord(*)

      integer isame,i
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return

      write(lu,'(/,'' '',a1,'' Coordinate units='',a16
     1,/,''        no. pts.  minimum           maximum         ''
     1,''increment    units'')')c0,cxi

      isame = 0

      do i = 1 , n_cord

        if (
     1( (c0.eq.'X' .or. c0.eq.'Y')  .and. cord(i)(1:1).eq.c0)
     1 .or.
     1(c0.eq.'Z' .and. cord(i)(1:1).ne.'X' .and. cord(i)(1:1).ne.'Y')
     1     ) then

          if (cord(i) .eq. cxi) isame = i
          call rmodcnv0(nxg,xgmin,xginc,cxi,cord(i),n_cord,x_cord,cord)
          write(lu,'(1x,i10,1x,g16.9,1x,g16.9,1x,g16.9,1x,a16)')
     1nxg,xgmin,(nxg-1)*xginc+xgmin,xginc,cord(i)
          call rmodcnv0(nxg,xgmin,xginc,cord(i),cxi,n_cord,x_cord,cord)

        endif    ! if (

      enddo    ! do i = 1 , n_cord

      if (isame .eq. 0)
     1write(lu,'(1x,i10,1x,g16.9,1x,g16.9,1x,g16.9,1x,a16)')
     1nxg,xgmin,(nxg-1)*xginc+xgmin,xginc,cxi

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpvg(lu,title,nzinc,cx,cy,cz,n_cord,x_cord,cord
     1,nxg,xgmin,xginc,nzg,zgmin,zginc,vg)
c  print gridded velocity info
c  vg = slowness
      real  util_invert_1
      dimension vg(nzg,1)
      character *(*) title,cx,cy,cz,cord
      character *16 cxa,cxb
      data cxa,cxb/'xannotation','xbasement'/
      data ndim/2/
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return
      if (lu .lt. 0) return

      call rmodpgrd(lu,title,cx,cy,cz,n_cord,x_cord,cord
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)

      call rmodcnv0(nxg,xgmin,xginc,cx,cxa,n_cord,x_cord,cord)
      write(lu,'('' iz     z     '',6(1x,f8.0))')
     1((ix-1)*xginc+xgmin,ix=1,nxg,max(1,nxg/5))
      call rmodcnv0(nxg,xgmin,xginc,cxa,cxb,n_cord,x_cord,cord)
      write(lu,'(''               '',6(1x,f8.1))')
     1((ix-1)*xginc+xgmin,ix=1,nxg,max(1,nxg/5))
      call rmodcnv0(nxg,xgmin,xginc,cxb,cx,n_cord,x_cord,cord)

      do iz = 1 , nzg , nzinc
        write(lu,'(1x,i5,1x,f8.0,6(1x,f8.1))')
     1iz,(iz-1)*zginc+zgmin,(util_invert_1(vg(iz,ix))
     1,ix=1,nxg,max(1,nxg/5))
      enddo    ! do iz = 1 , nzg , nzinc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodsrtc(n_cord,x_cord,cord)
c  sort the coordinate transform array
      implicit none
      integer n_cord,m_cord,m0
      parameter (m_cord=100,m0=14)
      real x_cord(2,1),xc(2,m_cord)
      integer ic(m_cord)
      character cord(*)*16,c0(m0)*16,c1(m_cord)*16
      integer i,i0,n0,i1,n1

      data c0/'XANNOTATION','XGRID','XBASEMENT'
     1,'YANNOTATION','YGRID','YBASEMENT'
     1,'DEPTH','KILOMETER','METER','KILOFEET','FEET'
     1,'TIME','SECONDS','MILS'/,n0/m0/

C      print'('' rmodsrtc original coordinates'')'
C      call rmodpcor(6,n_cord,x_cord,cord)

c  make a copy of the coordinates
      n1 = n_cord
      do i = 1 , n1
        c1(i) = cord(i)
        xc(1,i) = x_cord(1,i)
        xc(2,i) = x_cord(2,i)
        ic(i) = 0
      enddo    ! do i = 1 , n1

c  search for coordinates in list place them in order
      i = 0
      do i0 = 1 , n0
        do i1 = 1 , n1
          if (c0(i0) .eq. c1(i1)) then
            i = i + 1
            cord(i) = c1(i1)
            x_cord(1,i) = xc(1,i1)
            x_cord(2,i) = xc(2,i1)
            ic(i1) = 1
C      print'('' i='',i2,'' i0='',i2,'' i1='',i2,'' xc='',f8.2
C     1,1x,f8.2,'' c='',a16)',i,i0,i1,x_cord(1,i),x_cord(2,i),cord(i)
          endif
        enddo    ! i1 = 1 , n1
      enddo    ! i0 = 1 , n0

c  add the rest of these
      do i1 = 1 , n1
        if (ic(i1) .eq. 0) then
          i = i + 1
          cord(i) = c1(i1)
          x_cord(1,i) = xc(1,i1)
          x_cord(2,i) = xc(2,i1)
          ic(i1) = 1
        endif
      enddo    ! do i1 = 1 , n1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpcor(key,n_cord,x_cord,cord)
c  print the coordinate transform array
      integer key,lu,dbutil_getlun
      character cord(*)*16,cord1*16,cord2*16
      dimension x_cord(2,1)
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return
      if (key .eq. 6) then
        lu = key
      else
        lu = dbutil_getlun(key)
      endif

      if (lu .lt. 0) return
      n = 0
      do 1 i = 1 , n_cord
        if (cord(i) .ne. ' ') n = n + 1
    1 continue
      write(lu,'('' *TRANSFORM SECTION''
     1,/,'' NUMBER OF TRANSFORMATION COORDINATES ='',i8)')n
      do 2 i = 1 , n_cord
        if (cord(i) .eq. ' ') goto 2
        call headccat(cord1,cord(i),'1')
        call headccat(cord2,cord(i),'2')
        write(lu,'(1x,a16,'' = '',g16.9,5x,a16,'' = '',g16.9)')
     1cord1,x_cord(1,i),cord2,x_cord(2,i)
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnin(n,ix,nx,x,z,cxi,czi,cxo,czo,n_cord,x_cord
     1,cord)
c  convert layered model x,z coordinates
      character *(*) cxi,czi,cxo,czo,cord
      dimension ix(1),nx(1)
      call rmodcnvn(ix(n)+nx(n),x,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnvn(ix(n)+nx(n),z,czi,czo,n_cord,x_cord,cord)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnlr(xmin,xmax,zmin,zmax,nb,ixb,nxb,xb,zb
     1,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,cxi,czi,cxo,czo,n_cord,x_cord,cord)
c  convert layered model x,z coordinates
      dimension ixb(1),nxb(1),ixv(1),nxv(1)
      character *(*) cxi,czi,cxo,czo,cord

c  convert min,max
      call rmodcnm4(xmin,xmax,zmin,zmax,cxi,czi,cxo,czo
     1,n_cord,x_cord,cord)
c  convert boundaries
      call rmodcnin(nb,ixb,nxb,xb,zb,cxi,czi,cxo,czo,n_cord,x_cord,cord)
c  convert boundaries
      call rmodcnin(nv,ixv,nxv,xv,zv,cxi,czi,cxo,czo,n_cord,x_cord,cord)
c  convert cell pointers
      call rmodcnvn(ncv,xcv,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnvn(ncv,zcv,czi,czo,n_cord,x_cord,cord)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function rmodcnv1(x,cxi,cxo,n_cord,x_cord,cord)
      implicit none
      real     x,x0
      character cxi*(*),cxo*(*)
      integer   n_cord
      real      x_cord(2,1)
      character cord(*)*16
      x0 = x
      call rmodcnvn(1,x0,cxi,cxo,n_cord,x_cord,cord)
      rmodcnv1 = x0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnvn(n,x,cxi,cxo,n_cord,x_cord,cord)
c  convert n points from one coordinate system to another
      real      x(1)
      character cxi*(*),cxo*(*)
      integer   n_cord
      real      x_cord(2,1)
      character cord(*)*16
      if (cxi(1:1) .eq. ' ' .or. cxo(1:1) .eq. ' '
     1 .or. cxi .eq. cxo) return
      call rmodscor(y1,y2,dx,cxi,cxo,n_cord,x_cord,cord)
      do 1 i = 1 , n
        x(i) = y2 + (x(i) - y1) * dx
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnv9(xmin,xmax,ymin,ymax,zmin,zmax
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord)
c  convert x,y,z values
      character *(*) cxi,cyi,czi,cxo,cyo,czo,cord
      call rmodcnmm(xmin,xmax,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnmm(ymin,ymax,cyi,cyo,n_cord,x_cord,cord)
      call rmodcnmm(zmin,zmax,czi,czo,n_cord,x_cord,cord)
      call rmodcnv0(nxg,xgmin,xginc,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnv0(nyg,ygmin,yginc,cyi,cyo,n_cord,x_cord,cord)
      call rmodcnv0(nzg,zgmin,zginc,czi,czo,n_cord,x_cord,cord)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnv0(nx,xmin,xinc,cxi,cxo,n_cord,x_cord,cord)
c  convert xmin,xinc cfrom one coordinate system to another
      character *(*) cxi,cxo,cord
      if (cxi(1:1) .eq. ' ' .or. cxo(1:1) .eq. ' '
     1 .or. cxi .eq. cxo) return
      call rmodscor(x1,x2,dx,cxi,cxo,n_cord,x_cord,cord)
      xmin = x2 + (xmin - x1) * dx
      xinc = xinc * dx
c      xmin = min(xmin,xmin+(nx-1)*xinc)
c      xinc = abs(xinc)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnm4(xmin,xmax,zmin,zmax,cxi,czi,cxo,czo
     1,n_cord,x_cord,cord)
c  convert xmin,xmax to new coordinates
      character *(*) cxi,czi,cxo,czo,cord
      call rmodcnmm(xmin,xmax,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnmm(zmin,zmax,czi,czo,n_cord,x_cord,cord)
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcnmm(xmin,xmax,cxi,cxo,n_cord,x_cord,cord)
c  convert xmin,xmax to new coordinates
      character *(*) cxi,cxo,cord
      x1 = xmin
      x2 = xmax
      call rmodcnvn(1,x1,cxi,cxo,n_cord,x_cord,cord)
      call rmodcnvn(1,x2,cxi,cxo,n_cord,x_cord,cord)
      xmin = min(x1,x2)
      xmax = max(x1,x2)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodscor(x1,x2,dx,cxi,cxo,n_cord,x_cord,cord)
c  determine transformation from cxi to cxo
c xnew = x2 + (xold - x1) * dx
      dimension x_cord(2,1)
      character *(*) cxi,cxo,cord
      data iprint/0/
      x1 = 0.
      x2 = 0.
      dx = 1.
      if (cxi(1:1) .eq. ' ' .or. cxo(1:1) .eq. ' '
     1.or. cxi .eq. cxo) return
      call rmodfcor(i1,cxi,n_cord,cord)
      call rmodfcor(i2,cxo,n_cord,cord)
      if (i1 .le. 0 .or. i2 .le. 0) then
        iprint = iprint + 1
        if (iprint .eq. 1) call rmodpcor(6,n_cord,x_cord,cord)
        if (iprint .le. 10)
     1print'(/,'' error in transform cxi='',a16,'' cxo='',a16)',cxi,cxo
        return
      endif
      x1 = x_cord(1,i1)
      x2 = x_cord(1,i2)
      dx = 1.
      if (x_cord(1,i1) .ne. x_cord(2,i1))
     1dx = (x_cord(2,i2) - x_cord(1,i2)) / (x_cord(2,i1) - x_cord(1,i1))
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodjcor(cx1,cx2)
c  left justify a coordinate variable
      implicit none
      character *16 cx1,cx2
      integer il,ir
      call rmodlenl(il,cx1)
      call rmodlenr(ir,cx1)
c      print'('' jcor il='',i2,'' ir='',i2,'' cx1='',a16)',il,ir,cx1
      if (cx1(ir:ir) .eq. '1' .or. cx1(ir:ir) .eq. '2') ir = ir - 1
      cx2(1:ir-il+1)  = cx1(il:ir)
      cx2(ir-il+2:16) = ' '
      call rmodcaps(cx2,cx2)
c      print'('' cx2='',a16)',cx2
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodacor(cx0,x1,x2,m_cord,n_cord,x_cord,cord)
c  add cx to cord
      implicit none
      integer m_cord,n_cord
      real x1,x2,x_cord(2,1)
      character cx0*(*),cord(*)*16,cx*16
      integer lc,i_cord
c  copy this input coordinate
      call rmodjcor(cx0,cx)
c  determine if this coordinate is already in list
      call rmodfcor(i_cord,cx,n_cord,cord)
c  add this coordinate to the list
      if (i_cord .eq. 0 .and. n_cord .lt. m_cord) then
      call rmodlenr(lc,cx)
c      print'('' acor nc='',i5,'' mc='',i5,'' lc='',i5,'' c='',a16)'
c     1,n_cord,m_cord,lc,cx
        n_cord = n_cord + 1
        cord(n_cord) = cx
        x_cord(1,n_cord) = x1
        x_cord(2,n_cord) = x2
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodccor(n_cord,x_cord,cord,*)
c  check coordinate transform to make sure they are different
      implicit none
      integer n_cord
      character cord(*)*16
      real      x_cord(2,1)
      integer i,j,iflag,li,lj

      iflag = 0

      do i = 1 , n_cord

        if (cord(i) .ne. ' ') then
          if (x_cord(1,i) .eq. x_cord(2,i)) then
            iflag = iflag + 1
            print'('' both values the same i='',i5
     1,'' x_cord='',f14.2)',i,x_cord(1,i)
          endif

          call rmodlenr(li,cord(i))
          if (cord(i)(li:li) .eq. 'S') li = li - 1
          do j = 1 , n_cord
            if (i .ne. j) then
              call rmodlenr(lj,cord(j))
              if (cord(j)(lj:lj) .eq. 'S') lj = lj - 1
              lj = min(li,lj)

              if (cord(i)(1:lj) .eq. cord(j)(1:lj)) then
                iflag = iflag + 1
                print'('' two coord names are the same i='',i5
     1,'' cord='',a16,'' i='',i5,'' cord='',a16)'
     1,i,cord(i),j,cord(j)
              endif    ! if (cord(i)(1:lj) .eq. cord(j)(1:lj)) then
            endif    ! if (i .ne. j) then
          enddo    ! do j = 1 , n_cord

        endif    ! if (cord(i) .ne. ' ') then

      enddo    ! do i = 1 , n_cord

      if (iflag .ne. 0 ) goto 999

      return
  999 continue
      print'(/,'' error in rmodccor - have duplicate transform types'')'
      call rmodpcor(6,n_cord,x_cord,cord)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfcor(i_cord,cx0,n_cord,cord)
c  find i_cord where cord(i_cord) = cx - if no match i_cord=0
      implicit none
      integer i_cord,n_cord,i,lx,lc
      character cx0*(*),cord(*)*16,cx*16
      i_cord = 0
      call rmodjcor(cx0,cx)
      if (cx(1:1) .eq. ' ' .or. n_cord .eq. 0) return
      call rmodlenr(lx,cx)
      if (lx .gt. 1 .and. cx(lx:lx) .eq. 'S') lx = lx - 1
      do i = 1 , n_cord
        call rmodlenr(lc,cord(i))
        if (lc .gt. 1 .and. cord(i)(lc:lc) .eq. 'S') lc = lc - 1
        lc = min(lc,lx)
        if (cx(1:lc) .eq. cord(i)(1:lc)) i_cord = i
      enddo    ! do i = 1 , n_cord
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrtrn(tfile,m_cord,n_cord,x_cord,cord,*)
c  read transformation parameters from file tfile
      implicit none
      integer m_cord,n_cord
      character tfile*(*),cord(*)*16,cord0*16,cend(2)*1
      real x_cord(2,1)
      data cend/'1','2'/,irewind/1/
      integer i,j,ifile,n0,ierr,irewind,lt

c  add transform name that are in the file itself
      if (tfile .ne. 'NONE' .and. tfile .ne. 'none'
     1.and. tfile .ne. ' ')
     1call rmodatrn(tfile,m_cord,n_cord,x_cord,cord,*998)
  998 continue

c  add defaults to transform list
      call rmoddtrn(m_cord,n_cord,x_cord,cord)

      if (tfile .ne. 'NONE' .and. tfile .ne. 'none'
     1.and. tfile .ne. ' ') then
        call rmodopen(ifile,tfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

        if (ierr .ne. 0) goto 999
        call rmodfsts(ifile,irewind,'*TRANSFORM',*999)
c      print'('' rtrn ifile='',i5,'' n_cord='',i5,'' tfile='',a20)'
c     1,ifile,n_cord,tfile
c      call rmodpcor(6,n_cord,x_cord,cord)
        do i = 1 , n_cord
          do j = 1 , 2
c          x_cord(j,i) = j - 1
            if (cord(i) .ne. ' ') then
              call headccat(cord0,cord(i),cend(j))
              call rmodgvr(x_cord(j,i),cord0,ifile)
c      print'('' i='',i2,'' j='',i1,'' cord='',a16,'' x='',f10.2)'
c     1,i,j,cord0,x_cord(j,i)
            endif    ! if (cord(i) .ne. ' ') then
          enddo    ! do j = 1 , 2
        enddo    ! 1 i = 1 , n_cord
c      call rmodpcor(6,n_cord,x_cord,cord)
      endif

      return
  999 continue
      call rmodlenr(lt,tfile)
      print'(/,'' error in rmodrtrn tfile='',a)',tfile(1:lt)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmoddtrn(m_cord,n_cord,x_cord,cord)
c  add defaults to transform list
      implicit none
      integer m_cord,n_cord
      character cord(*)*16
      real x_cord(2,1)
      character *16 DEPTH,KILOMETER,METER,KILOFEET,FEET,TIME,SECOND,MIL
     1,XANNOTATION,XBASEMENT,XGRID,YANNOTATION,YBASEMENT,YGRID
      data DEPTH,KILOMETER,METER,KILOFEET,FEET,TIME,SECOND,MIL
     1,XANNOTATION,XBASEMENT,XGRID,YANNOTATION,YBASEMENT,YGRID
     1/'DEPTH','KILOMETER','METER','KILOFEET','FEET'
     1,'TIME','SECOND','MIL','XANNOTATION','XBASEMENT','XGRID'
     1,'YANNOTATION','YBASEMENT','YGRID'/

      integer imet

      call rmodgmet(imet)    ! metric flag 0=metric 1=engslish

      call rmodacor(XBASEMENT  ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(XANNOTATION,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(XGRID      ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YBASEMENT  ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YANNOTATION,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YGRID      ,0.,   1.,m_cord,n_cord,x_cord,cord)

      if (imet .eq. 0) then    ! metric units
        call rmodacor(DEPTH    ,0.,1000.,m_cord,n_cord,x_cord,cord)
      elseif (imet .lt. 0) then    ! metric units
        call rmodacor(DEPTH    ,0.,1000.,m_cord,n_cord,x_cord,cord)
        call rmodacor(KILOMETER,0.,   1.,m_cord,n_cord,x_cord,cord)
        call rmodacor(METER    ,0.,1000.,m_cord,n_cord,x_cord,cord)
      elseif (imet .gt. 0) then    ! english units
        call rmodacor(DEPTH    ,0.,5000.,m_cord,n_cord,x_cord,cord)
        call rmodacor(KILOFEET ,0.,   5.,m_cord,n_cord,x_cord,cord)
        call rmodacor(FEET     ,0.,5000.,m_cord,n_cord,x_cord,cord)
      endif    ! if (imet .eq. 0) then    ! metric units
      call rmodacor(TIME     ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(SECOND   ,0.,   1.,m_cord,n_cord,x_cord,cord)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpmet(jmet)
      implicit none
      integer imet,jmet
      data imet/0/    ! metric flag imet=0 metric imet=1 english
      imet = jmet
      return
      entry rmodgmet(jmet)
      jmet = imet
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodatrn(tfile,m_cord,n_cord,x_cord,cord,*)
c  search trasformation section for transform pairs add them to list
      implicit none
      integer m_cord,n_cord
      character tfile*(*),cord(*)*16,crd132*132,cnew*16
      real x_cord(2,1)
      integer icrd132,i_cord,ione,iequal
      integer ifile,key,dbutil_getlun,irewind,ierr,n0,i_star,lc
      data irewind/1/

      call rmodopen(key,tfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999
      ifile = dbutil_getlun(key)
      call rmodfsts(key,irewind,'*TRANSFORM',*999)
c      print'('' atrn ifile='',i5,'' n_cord='',i5,'' tfile='',a20)'
c     1,ifile,n_cord,tfile
c  read this crd132 from the file
c  if this is the end of file or a star crd132 all done
    1 continue
      read(ifile,'(a)',err=999,end=998)crd132
      call rmodfchr(i_star,'*',1,len(crd132),crd132)
      if (i_star .ne. 0) goto 998
      call rmodlenr(lc,crd132)
c      print'(/,'' lc='',i8,'' crd132='',a60)',lc,crd132(1:lc)
      icrd132 = 1
    2 continue
c  starting at i1 find the next occurence of the string '1='
      call rmodfone(i_cord,ione,iequal,icrd132,crd132)
c      print'('' nc='',i3,'' icrd132='',i3,'' iequal='',i3,'' ione='',i3
c     1,'' ic1='',i3,1x,i3,'' c='',a16)',n_cord,icrd132,iequal,ione
c     1,i_cord,ione-1,crd132(icrd132:ione-1)
      if (n_cord .lt. m_cord .and. i_cord .ne. 0) then
c      print'('' i_cord='',i8,'' ione='',i8
c     1,'' c='',a16)',i_cord,ione,crd132(i_cord:ione)
        cnew = ' '
        cnew = crd132(i_cord:ione-1)
c        call rmodacor(crd132(i_cord:ione-1),0.,1.,m_cord,n_cord,x_cord,c
        call rmodacor(cnew,0.,1.,m_cord,n_cord,x_cord,cord)
c      print'('' nc='',i3,'' c='',a16)',n_cord,cord(n_cord)
        icrd132 = iequal + 1        ! update location within crd132
        if (icrd132 .lt. len(crd132)) goto 2    ! search for next = sign
      endif
      if (n_cord .lt. m_cord) goto 1

  998 continue
      return
  999 continue
      return 1
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwtrn(tfile,m_cord,n_cord,x_cord,cord,*)
c  write trasformation parameters to file tfile
      implicit none
      integer    m_cord,n_cord
      character tfile*(*),cord(*)*16
      real      x_cord(2,1)
      integer   ifile,n0,ierr

c  open file
      call rmodopen(ifile,tfile,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999

c  add defaults
      call rmoddtrn(m_cord,n_cord,x_cord,cord)

c  sort transforms
      call rmodsrtc(n_cord,x_cord,cord)

c  write transforms
      call rmodpcor(ifile,n_cord,x_cord,cord)

      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodlnlr(i1,i2,str)
c  find the first and last non blank character
      character str*(*)
      call rmodlenl(i1,str)
      call rmodlenr(i2,str)
c      print'('' flbn i1='',i2,'' i2='',i2,'' l='',i2,'' str='',a20)'
c     1,i1,i2,len(str),str
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodlenl(i,str)
c  find the first non blank character
      character str*(*)
c      print'('' fbnk l='',i5,'' str='',a20)',len(str),str
      do 1 j = 1 , len(str)
        i = j
c      if (str(j:j).ne.' ')print'('' j='',i5,'' str='',a20)',j,str(j:j)
        if (str(j:j) .ne. ' ') return
    1 continue
      i = len(str) + 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodlenr(i,str)
c  find the last non blank character
      character str*(*)
c      print'('' lbnk l='',i5,'' str='',a20)',len(str),str
      do 1 j = len(str) , 1 , -1
        i = j
c      if (str(j:j).ne.' ')print'('' j='',i5,'' str='',a20)',j,str(j:j)
        if (str(j:j) .ne. ' ') return
    1 continue
      i = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function rmod_len_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        rmod_len_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      rmod_len_r = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfone(i_cord,ione,iequal,icard,card)
c  starting at icard find the next occurence of the string '1='
      character card*(*)
      character*1 equal,one
      data equal,one/'=','1'/
      logical headspac
      call rmodlenr(lc,card)
c      print'('' fone lc='',i8,'' card='',a)',lc,card(1:lc)

c  first find the character '='
      i_cord = 0
c  find the first = sign after icard
      call rmodfchr(iequal,equal,icard,len(card),card)
      if (iequal .ne. 0) then
c  working backwards from iequal-1 find the first non blank character
        call rmodnchr(ione,' ',iequal-1,icard,card)
        if (ione .ne. 0 .and. card(ione:ione) .eq. one) then
c  find the first character before 1 which is a blank or , or ; or (
          i_cord = ione-1
          do 1 i = ione-1 , icard , -1
            if (headspac(card(i:i))) goto 2
            i_cord = i
    1     continue
    2     continue
c      print'('' i_cord='',i8,'' ione='',i8,'' iequal='',i8)'
c     1,i_cord,ione,iequal
        endif    ! if (iequal .ne. 0) then
      endif    ! if (iequal .ne. 0) then
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_find_string(i_string,string_0,card_0)
c  find the string string in card
c  if irewind .ne. 0 rewind file first
      implicit  none
      character string_0*(*),card_0*(*)
      character string*132,card*132
      integer   i_string,j_string
      integer   i,i1,i2,l_string
c  find the first and last blanks
      call rmodcaps(string_0,string)
      call rmodcaps(card_0,card)
      i_string = 0
      call rmodlnlr(i1,i2,string)
      l_string = i2 - i1 + 1
      if (l_string .le. 0) return
      do i = 1 , len(card)-l_string+1
        i_string = i
        if (card(i:i+l_string-1) .eq. string(i1:i2)) return
      enddo    ! do i = 1 , len(card)-l_string+1
      i_string = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfchr(ichr,chr,ic1,ic2,card)
c  find location of single character within a card
      character chr*1,card*(*)
      ichr = 0
      lenc = len(card)
      idir = isign(1,ic2-ic1)

      do 1 i = max(1,min(lenc,ic1)) , max(1,min(lenc,ic2))  , idir
        if (card(i:i) .eq. chr) then
          ichr = i
          goto 2
        endif
    1   continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodnchr(ichr,chr,ic1,ic2,card)
c  find location of next space not the same as chr
      character chr*1,card*(*)
      ichr = 0
      lenc = len(card)
      idir = isign(1,ic2-ic1)

      do 1 i = max(1,min(lenc,ic1)) , max(1,min(lenc,ic2))  , idir
        if (card(i:i) .ne. chr) then
          ichr = i
          goto 2
        endif
    1   continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfsts(key,irewind,str,*)
c  find card with str in it if cannot find it start at top
      character str*(*),crd132*132
      integer ifile,key,dbutil_getlun
      ifile = dbutil_getlun(key)
      call rmodfstr(key,irewind,nrd,str,crd132,*1)
      goto 2
    1 continue
      rewind(ifile)
      read(ifile,'(a)',err=999)crd132
      if (crd132(2:2) .ne. '*') rewind(ifile)
    2 continue
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodfstr(key,irewind,nrd,str0,card,*)
c  find the string str in ifile
c  if irewind .ne. 0 rewind file first
      character *(*) str0,card
      character file*80,str*80
      integer ifile,key,dbutil_getlun
      ifile = dbutil_getlun(key)

      call rmodcaps(str0,str)
      if (irewind .ne. 0) rewind(ifile)
c  find the first and last blanks
      call rmodlnlr(i1,i2,str)
      lstr = i2 - i1 + 1
      if (lstr .le. 0) goto 998
      nrd = 0
    1 continue
      read(ifile,'(a)',err=999,end=998)card
      nrd = nrd + 1
      call rmodcaps(card,card)
      do 2 i = 1 , len(card)-lstr+1
        if (card(i:i+lstr-1) .eq. str(i1:i2)) return
    2 continue
      goto 1
  998 continue
      return 1
  999 continue
      call rmodfofi(key,file)
      call rmodlenr(lf,file)
c      print'(/,'' error in rmodfstr ifile='',i2,'' str='',a
c     1,/,'' file='',a)',ifile,str(i1:min(i1+11,i2)),file(1:lf)
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwplt(xmin,xmax,zmin,zmax,n,ix,nx,x,z,lpr)
c  write info for plotting
      dimension ix(1),nx(1),x(1),z(1)
      if (lpr .le. 0) return
      write(lpr,'(2(1x,f10.2),1x,i5)')
     1xmin,zmin,0,xmax,zmin,0,xmax,zmax,0,xmin,zmax,0,xmin,zmin,0
      do 1 i = 1 , n
        write(lpr,'(2(1x,f10.2),1x,i5)')
     1(x(j),z(j),i,j=ix(i)+1,ix(i)+nx(i))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodcaps(cin,cout)
c  capitalize cin can be inplace
      character *(*) cin,cout
      character c0*132
      c0 = cin
      call headcaps(cout,c0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrhor(hfile,title,ibw1,ibw2,ibw3
     1,mbh,nbh,ibid,nbseg,bname,bcolor,*)
c  read horizon info
      character hfile*(*),bname(1)*16,bcolor(1)*16,title*(*)
      dimension ibid(1),nbseg(1)
      character crd132*132
      integer ifile,key,dbutil_getlun
c      print'('' rhor title='',a16,'' ifile='',i5,'' mbh='',i5)'
c     1,title,ifile,mbh
c  find the horizon section
      call rmodopen(key,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

      if (ierr .ne. 0) goto 999
      ifile = dbutil_getlun(key)
      ibw1 = 37
      ibw3 = 0
      ibw2 = 0
      nbh = 0
      do 1 i = 1 , mbh
        bname(i) = ' '
        bcolor(i) = ' '
    1 continue
      call rmodfstr(key,1,nrd,title,crd132,*999)
      call headgvic(crd132,ibw1,'P-header')
      call headgvic(crd132,ibw2,'S-header')
      call headgvic(crd132,ibw3,'T-header')
      do 2 i = 1 , mbh
        read(ifile,'(a)',end=2,err=999) crd132
        call rmodfchr(i_star,'*',1,len(crd132),crd132)
        if (i_star .ne. 0) goto 3
        call headgvcc(crd132,bname(i),nchar,'name')
        call headgvcc(crd132,bcolor(i),nchar,'color')
        call headgvic(crd132,nbseg(i),'nseg')
        call headgvic(crd132,ibid(i),'hid')
        nbh = i
    2 continue
    3 continue
c      print'('' rhor title='',a16,'' ifile='',i5,'' nbh='',i5)'
c     1,title,ifile,nbh
      return
  999 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodwhor(hfile,title
     1,ibw1,ibw2,ibw3,nbh,ibid,nbseg,bname,bcolor,*)
c  write horizon info
      dimension ibid(1),nbseg(1)
      character hfile*(*),title*(*),bname(1)*16,bcolor(1)*16
      character crd80*80
      integer ifile,key,dbutil_getlun
      ifile = dbutil_getlun(key)
c  open the file
      call rmodopen(key,hfile,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,ierr)
      if (ierr .ne. 0) goto 999
      ifile = dbutil_getlun(key)
c  write the horizon section
      crd80 = ' '
      call rmowwlef(title,crd80(2:))
      write(crd80(17:),'('' P-header='',i2,'' S-header='',i2
     1,'' T-header='',i2)')
     1max(1,min(64,ibw1)),max(0,min(64,ibw2)),max(0,min(64,ibw3))
      call rmodwcim(key,crd80)

      do 1 i = 1 , nbh
        call rmodlnlr(in1,in2,bname(i))
        call rmodlnlr(ic1,ic2,bcolor(i))
        write(ifile,'(''  HID='',i3,'' NAME='',a16,'' NSEG='',i3
     1,'' COLOR='',a16)')
     1ibid(i),bname(i)(in1:in2),nbseg(i),bcolor(i)(ic1:ic2)
    1 continue
      return
  999 continue
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodshor(nb,imb,mbh,nbh,ibid,nbseg,bname,bcolor)
c  initialize horizon info
      character bname(1)*16,bcolor(1)*16
      dimension imb(1),ibid(1),nbseg(1)
      parameter (mcolor=6)
      character color(mcolor)*16
      data ncolor/mcolor/,color/'BLUE','GREEN','YELLOW','PURPLE','RED'
     1,'CYAN'/
      nbh = max(0,nbh)
c      print*,' shor nbh=',nbh,' mbh=',mbh,' nb=',nb
      do 1 i = 1 , nb
c      print*,' i=',i,' imb=',imb(i)
        do 2 j = 1 , nbh
          if (imb(i) .eq. ibid(j)) then
            nbseg(j) = nbseg(j) + 1
c      print'('' i='',i3,'' j='',i3,'' imb='',i3,'' ibid='',i3
c     1,'' n='',i3)',i,j,imb(i),ibid(j),nbseg(j)
            goto 1
          endif
    2   continue
c      print*,' i=',i,' nbh=',nbh,' imb=',imb(i)
        if (nbh .ge. mbh) goto 1
        nbh = nbh + 1
        ibid(nbh) = imb(i)
        nbseg(nbh) = 1
    1 continue
      do 3 i = 1 , nbh
        if (bname(i) .eq. ' ') then
        if (i .le. 9) then
          write(bname(i),'(''horizon00'',i1)')i
        elseif (i .le. 99) then
          write(bname(i),'(''horizon0'',i2)')i
        else
          write(bname(i),'(''horizon'',i3)')min(i,999)
        endif
        endif
        if (bcolor(i) .eq. ' ') bcolor(i) = color(mod(i-1,ncolor)+1)
c      print*,' i=',i,' nbseg=',nbseg(i)
    3 continue
c      print*,' shor nbh=',nbh,' mbh=',mbh,' nb=',nb
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodshdr(x,y,hd,cxi,cyi,n_cord,x_cord,cord)
      character *(*) cxi,cyi,cord(*)
      dimension hd(*)
      character *16 cx07,cx17,cx37,cy08,cy18,cy38
      data cx07,cx17,cx37,cy08,cy18,cy38
     1/'xgrid','xbasement','xannotation'
     1,'ygrid','ybasement','yannotation'/
      hd( 7) = x
      hd(17) = x
      hd(37) = x
      hd( 8) = y
      hd(18) = y
      hd(38) = y
      call rmodcnvn(1,hd(07),cxi,cx07,n_cord,x_cord,cord)
      call rmodcnvn(1,hd(17),cxi,cx17,n_cord,x_cord,cord)
      call rmodcnvn(1,hd(37),cxi,cx37,n_cord,x_cord,cord)
      call rmodcnvn(1,hd(08),cyi,cy08,n_cord,x_cord,cord)
      call rmodcnvn(1,hd(18),cyi,cy18,n_cord,x_cord,cord)
      call rmodcnvn(1,hd(38),cyi,cy38,n_cord,x_cord,cord)
      hd(11) = hd(17)
      hd(14) = hd(17)
      hd(12) = hd(18)
      hd(15) = hd(18)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodlrzt(n,nz,v,z,zdatum,czi)
      dimension z(n,1),v(n,1)
      character czi*(*)
      print'('' rmodlrzt n='',i5,'' nz='',i5,'' czi='',a16)',n,nz,czi
      if (czi .eq. 'TIME') then
        do 1 i = 1 , n
          t0 = 0
          z0 = zdatum
          do 2 j = 1 , nz
            iv1 = max(2*j-2,1)
            iv2 = 2 * j - 1
      if (v(i,iv1)+v(i,iv2).eq.0.)then
      print*,' i=',i,' iv1=',iv1,' iv2=',iv2
      print*,' v=',v(i,iv1),v(i,iv2)
      stop
      endif
            dz = (z(i,j) - t0) / (v(i,iv1) + v(i,iv2))
            t0 = z(i,j)
            z(i,j) = z0 + dz
            z0 = z(i,j)
      if(i.eq.1)print'('' j='',i3,'' iv='',i3,1x,i3,'' t='',f6.3
     1,'' z='',f8.1,'' v='',f6.0,1x,f6.0)'
     1,j,iv1,iv2,t0,z0,1./v(i,iv1),1./v(i,iv2)
    2     continue

    1   continue
      elseif (czi .eq. 'DEPTH') then
        do 3 i = 1 , n
          t0 = 0
          z0 = zdatum
          do 4 j = 1 , nz
            iv1 = max(2*j-2,1)
            iv2 = 2 * j - 1
            dt = (z(i,j) - z0) * (v(i,iv1) + v(i,iv2))
            z0 = z(i,j)
            z(i,j) = t0 + dt
            t0 = z(i,j)
      if(i.eq.1)print'('' j='',i3,'' iv='',i3,1x,i3,'' t='',f6.3
     1,'' z='',f8.1,'' v='',f6.0)'
     1,j,iv1,iv2,t0,z0,1./(v(i,iv1)+v(i,iv2))
    4     continue
    3   continue
      else
        print'(/,'' error in dm3dztot input type must be time or depth''
     1,'' czi='',a16)',czi
        stop
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpvel(lu,title,type
     1,cx,cy,cz,n_cord,x_cord,cord
     1,nx,x0,dx
     1,ny,y0,dy
     1,nz,z0,dz
     1,vg,zg,ny_inc,nz_inc)
      implicit none

      real      util_invert_1

      integer   lu
      character title*(*),type*(*)
      character cx*(*),cy*(*),cz*(*)
      integer   n_cord
      real      x_cord(2,1)
      character cord*(*)

      integer   nx
      real      x0,dx

      integer   ny
      real      y0,dy

      integer   nz
      real      z0,dz

      real      vg(1),zg(1)
      integer   ny_inc,nz_inc

      integer   ix,iy,iz,ixy
      integer   iv1,iv2
      integer   iv_min,iv_max
      real      v_min,v_max
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return

      if (lu .lt. 0. or. lu .gt. 100) return

      call rmodpgrd(lu,title,cx,cy,cz,n_cord,x_cord,cord
     1,nx,x0,dx,ny,y0,dy,nz,z0,dz)

      call util_min_max(v_min,v_max,nx*ny*nz,vg)
      call util_min_max_i(iv_min,iv_max,nx*ny*nz,vg)

      print'('' v_min='',f12.2,'' v_max='',f12.2)'
     1,util_invert_1(v_max),util_invert_1(v_min)
      print'('' iv_min='',i12,'' iv_max='',i12)',iv_max,iv_min

      if (type .eq. 'G3DL') then

        do iz = 1 , nz , nz_inc

          write(lu,'(/,'' depth at bottom of layer '',i5,'' x''
     1,/,'' iy      y     ''
     1,6(1x,f8.0))')iz,((ix-1)*dx+x0,ix=1,nx,max(1,nx/5))
          iv1 = iz * 2 - 1
          iv2 = iz * 2

          do iy = 1 , ny , ny_inc

            if (dz .gt. 1) then

              write(lu,'(1x,i5,1x,f9.0,6(1x,f8.0))')
     1iy,(iy-1)*dy+y0
     1,(zg(ix+(iy-1)*nx+(iz-1)*nx*ny),ix=1,nx,max(1,nx/5))

            else    ! if (dz .gt. 1) then

              write(lu,'(1x,i5,1x,f9.0,6(1x,f8.3))')
     1iy,(iy-1)*dy+y0
     1,(zg(ix+(iy-1)*nx+(iz-1)*nx*ny),ix=1,nx,max(1,nx/5))

            endif    ! if (dz .gt. 1) then

          enddo    ! do iy = 1 , ny , ny_inc

          write(lu,'(/,'' velocity above x'',/,'' iy      y     ''
     1,6(1x,f8.0))')((ix-1)*dx+x0,ix=1,nx,max(1,nx/5))

          do iy = 1 , ny , ny_inc

            write(lu,'(1x,i5,1x,f9.0,6(1x,f8.0))')
     1iy,(iy-1)*dy+y0
     1,(util_invert_1(vg(ix+(iy-1)*nx+(iv1-1)*nx*ny))
     1,ix=1,nx,max(1,nx/5))

          enddo    ! do iy = 1 , ny , ny_inc

          write(lu,'(/,'' velocity below x'',/,'' iy      y     ''
     1,6(1x,f8.0))')((ix-1)*dx+x0,ix=1,nx,max(1,nx/5))

          do iy = 1 , ny , ny_inc

            write(lu,'(1x,i5,1x,f9.0,6(1x,f8.0))')
     1iy,(iy-1)*dy+y0
     1,(util_invert_1(vg(ix+(iy-1)*nx+(iv2-1)*nx*ny))
     1,ix=1,nx,max(1,nx/5))

          enddo    ! do iy = 1 , ny , ny_inc

        enddo    ! do iz = 1 , nz , nz_inc

      else    ! if (type .eq. 'G3DL') then

        do iy = 1 , ny , ny_inc

          write(lu,'(/,'' iy ='',i5,'' y='',f12.2
     1,/,'' iz      z     '',6(1x,f8.0))')iy,y0+(iy-1)*dy
     1,((ix-1)*dx+x0,ix=1,nx,max(1,nx/5))
          ixy = (iy - 1) * nz * nx

          do iz = 1 , nz , nz_inc

            if (dz .gt. 1) then

              write(lu,'(1x,i5,1x,f8.0,6(1x,f8.1))')
     1iz,(iz-1)*dz+z0
     1,(util_invert_1(vg(iz+(ix-1)*nz+ixy))
     1,ix=1,nx,max(1,nx/5))

            else    ! if (dz .gt. 1) then

              write(lu,'(1x,i5,1x,f8.3,6(1x,f8.1))')
     1iz,(iz-1)*dz+z0
     1,(util_invert_1(vg(iz+(ix-1)*nz+ixy))
     1,ix=1,nx,max(1,nx/5))

            endif    ! if (dz .gt. 1) then

          enddo    ! do iz = 1 , nz , nz_inc

        enddo    ! do iy = 1 , ny , ny_inc

      endif

      write(lu,'(//)')

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodbdel(str)
      character str*(*)
      nstr = 0
      n = 1
      do 1 i = 2 , len(str)
        if (str(i:i) .eq. ' ' .and. str(n:n) .eq. ' ') goto 1
        n = n + 1
        str(n:n) = str(i:i)
    1 continue
      do 2 i = n+1 , len(str)
        str(i:i) = ' '
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpnt0(i0,n1,i1,m2,n2,i2)
      dimension i1(1),i2(1)
      n2 = 0
      do 1 i = 1 , n1
        if (i0 .eq. i1(i)) then
          n2 = n2 + 1
          if (n2 .le. m2) i2(n2) = i
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodpnt1(idir,n,i,x1,x2)
      dimension i(1),x1(1),x2(1)
      if (idir .le. 0) then
        do 1 j = 1 , n
          x2(j) = x1(i(j))
    1   continue
      else
        do 2 j = 1 , n
          x1(i(j)) = x2(j)
    2   continue
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodvxyz(idir,hfile,title,cx,cy,cz
     1,m,n,x,y,z,v,i1,i2,i3)
c  read(idir=0) write(idir=1) data
      character *(*) hfile,title,cx,cy,cz
      dimension x(1),y(1),z(1),v(1),i1(1),i2(1),i3(1)
      parameter (ma=10)
      dimension a(ma)
      if (idir .eq. 0) then
      call rmodopen(ifile,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
      call rmodfcrd(ifile,1,title,cx,cz,ndof,*2)
      ndof = min(ndof,ma)
      n = 0
      do 1 i = 1 , m
        call util_setr(ma,a,0.)
        call rmodrval(ifile,ndof,1,na,a,*2)
        if (na .eq. 0) goto 2
        x(i) = a(1)
        z(i) = a(2)
        i1(i) = nint(a(3))
        if (title .eq. '*VELOCITY') then
          y(i) = a(6)
          v(i) = a(5)
          i2(i) = nint(a(4))
          i3(i) = nint(a(7))
        else
          y(i) = a(4)
        endif
        n = i
    1 continue
    2 continue
      else
      call rmodopen(ifile,hfile,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
        if (title .eq. '*VELOCITY') then
          ndof = 7
        else
          ndof = 5
        endif
        call rmodwchd(ifile,title,cx,cy,cz,ndof)
        do 3 i = 1 , n
        a(1) = x(i)
        a(2) = z(i)
        a(3) = i1(i)
        if (title .eq. '*VELOCITY') then
          a(6)= y(i)
          a(5) = v(i)
          a(4) = i2(i)
          a(7) = i3(i)
        else
          a(4) = y(i)
        endif
        call rmodwval(ifile,ndof,1,a)
    3   continue
       endif
c      call rmodlenr(lt,title)
c      call rmodlenr(lh,hfile)
c      print'('' rodvxyz m='',i5,'' n='',i5,'' title='',a16
c     1,'' hfile='',a)',m,n,title(1:lt),hfile(1:lh)
c      if (title .eq. '*VELOCITY') then
c        print'('' i       x          y          z           v''
c     1,''      i1    i2    i3'')'
c        print'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.2,1x,f10.2
c     1,1x,i5,1x,i5,1x,i5)'
c     1,(i,x(i),y(i),z(i),v(i),i1(i),i2(i),i3(i),i=1,n)
c      else
c        print'('' i       x          y          z      i1'')'
c        print'(1x,i5,1x,f10.2,1x,f10.2,1x,f10.2,1x,i5)'
c     1,(i,x(i),y(i),z(i),i1(i),i=1,n)
c      endif
      return
  999 continue
      print'(/,'' error in rmodvxyz'')'
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdgl(hfile,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ierr)
c  get the gridded model limits
c  read a header file
      implicit none
      integer nxg,nyg,nzg,ierr
      real xmin,xmax,ymin,ymax,zmin,zmax,zdatum
      real xgmin,xginc,ygmin,yginc,zgmin,zginc
      character *(*) hfile
      integer ifile
      ierr = 0
      call rmodopen(ifile,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',0,ierr)
      if (ierr .ne. 0) goto 999
      call rmodfsts(ifile,1,'*HEADER',*999)
      nxg = 1
      xgmin = 0
      xginc = 1
      nyg = 1
      ygmin = 0
      yginc = 1
      nzg = 1
      zgmin = 0
      zginc = 1
      call rmodgvr(zginc,'dz',ifile)
      call rmodgvr(xginc,'dx',ifile)
      call rmodgvr(yginc,'dy',ifile)

      call rmodgvi(nzg,'n1',ifile)
      call rmodgvi(nxg,'n2',ifile)
      call rmodgvi(nyg,'n3',ifile)
      call rmodgvr(zgmin,'o1',ifile)
      call rmodgvr(xgmin,'o2',ifile)
      call rmodgvr(ygmin,'o3',ifile)
      call rmodgvr(zginc,'d1',ifile)
      call rmodgvr(xginc,'d2',ifile)
      call rmodgvr(yginc,'d3',ifile)

c the following are official names and will override previous names
      call rmodgvi(nzg,'nz',ifile)
      call rmodgvi(nxg,'nx',ifile)
      call rmodgvi(nyg,'ny',ifile)
      call rmodgvr(zgmin,'zmin',ifile)
      call rmodgvr(xgmin,'xmin',ifile)
      call rmodgvr(ygmin,'ymin',ifile)
      call rmodgvr(zginc,'zinc',ifile)
      call rmodgvr(xginc,'xinc',ifile)
      call rmodgvr(yginc,'yinc',ifile)

c  do not allow zero increment
      if (xginc .eq. 0.) xginc = 1.
      if (yginc .eq. 0.) yginc = 1.
      if (zginc .eq. 0.) zginc = 1.

c  get the layered model limits

      xmin = xgmin
      xmax = (nxg - 1) * xginc + xgmin
      ymin = ygmin
      ymax = (nyg - 1) * yginc + ygmin
      zmin = zgmin
      zmax = (nzg - 1) * zginc + zgmin
      zdatum = 0
      call rmodgvr(xmin,'xmodmin',ifile)
      call rmodgvr(xmax,'xmodmax',ifile)
      call rmodgvr(ymin,'ymodmin',ifile)
      call rmodgvr(ymax,'ymodmax',ifile)
      call rmodgvr(zmin,'zmodmin',ifile)
      call rmodgvr(zmax,'zmodmax',ifile)
      call rmodgvr(zdatum,'zdatum',ifile)
      return
  999 continue
      ierr = -1
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodrdng(hfile,nxg,nyg,nzg,ierr)
c  get the gridded model limits
c  read a header file
      implicit none
      integer nxg,nyg,nzg,ierr
      real xmin,xmax,ymin,ymax,zmin,zmax,zdatum
      real xgmin,xginc,ygmin,yginc,zgmin,zginc
      character *(*) hfile
      integer ifile
      ierr = 0
      call rmodrdgl(hfile,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ierr)
      return
  999 continue
      ierr = -1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodvmin(hfile,vmin,vmax,mvg,vg,ierr)
      implicit none
      character hfile*(*)
      integer mvg,ierr
      real vmin,vmax,vg(1)
      character dfile*80,type*16,wtype*16,cx*16,cz*16
      integer ifile,jfile,n0,nxg,nyg,nzg,i,nc,lf,i1,i2,ndof
      real xmin,xmax,ymin,ymax,zmin,zmax,zdatum,v1,v2,x,z
      real xgmin,xginc,ygmin,yginc,zgmin,zginc
      ierr = 0

c  open the header file
      dfile = ' '
      type = 'LAYER'
      call rmodopen(ifile,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
      if (ierr .ne. 0) goto 999
c  find the header section
      call rmodfsts(ifile,1,'*HEADER',*999)
      call rmodgvc(dfile,nc,'FILE',ifile)
      call rmodgvc(type,nc,'TYPE',ifile)
      call rmodgvc(wtype,nc,'WORDTYPE',ifile)
      call rmodrdgl(hfile
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc,ierr)
      if (ierr .ne. 0) goto 999
      call rmoddrct(hfile,dfile)
      ierr = 0

      vmin = 0.
      vmax = 0.

c  for a gridded model read records
      if (type(1:4)  .eq. 'GRID') then
        call rmodopen(jfile,dfile,'UNFORMATTED','OLD','DIRECT'
     1,wtype,nzg,ierr)
        if (ierr .ne. 0) goto 999
        if (mvg .lt. nzg*2) then
          print'('' not enough work space need='',i8,'' have='',i8)'
     1,mvg,nzg*2
          goto 999
        endif

        do i = 1 , nxg*nyg
          call rmodrrec(jfile,i,1,nzg,vg,vg(nzg+1),*999)
          call cellmnmx(v1,v2,nzg,vg)
          if (i .eq. 1) then
            vmin = v1
            vmax = v2
          else
            vmin = min(vmin,v1)
            vmax = max(vmax,v2)
          endif
        enddo    ! do i = 1 , nxg*nyg
      else    ! if (type(1:4)  .eq. 'GRID') then
c  for a layered model read velocity cards
        call rmodopen(ifile,dfile,'FORMATTED','OLD','SEQUENTIAL'
     1,wtype,n0,ierr)
        if (ierr .ne. 0) goto 999
        call rmodfcrd(ifile,1,'*VELOCITY',cx,cz,ndof,*2)
        i = 0
    1   continue
        call rmodrcrd(ifile,ndof,x,z,i1,i2,v1,*2)
        i = i + 1
        if (i .eq. 1) then
          vmin = v1
          vmax = v1
        else
          vmin = min(vmin,v1)
          vmax = max(vmax,v1)
        endif
        goto 1
    2 continue
      endif    ! if (type(1:4)  .eq. 'GRID') then

      return
  999 continue
      ierr = -1
      return
      end

c&&&
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodter3(
     1 nx,xmin,xinc
     1,ny,ymin,yinc
     1,nz,v,z
     1,nxg,xgmin,xginc
     1,nyg,ygmin,yginc
     1,nzg,zgminc,zginc
     1,vg
     1,mwork,work)
      dimension v(nx,ny,nz),z(nx,ny,nz),vg(nzg,nxg,nyg),work(1)
      nzinc = 5
      nzg0 = (nzg - 1) * nzinc + 1
      zginc0 = zginc / nzinc
      dz = zginc * 1e-6
      if (dz .eq. 0.) dz = 1e-6
      call util_wors(i_work_i,i_work_n,mwork)
      call util_work(i_work_i,i_work_n,izw,nz*2)
      call util_work(i_work_i,i_work_n,ivw,nz*2)
      call util_work(i_work_i,i_work_n,ivg,nzg0)
      call util_worc(i_work_i,i_work_n,ierr)
      if (ierr .ne. 0) goto 999

      do 1 iyg = 1 , nyg
        yg = (iyg - 1) * yginc + ygmin
        do 2 ixg = 1 , nxg
          xg = (ixg - 1) * xginc + xgmin
c  interpolate layer depth and velocities to this lcoation
c  note the first and last layer have 1 velocity each the rest have 2
          call util_interpolate(
     1 nx,xmin,xinc
     1,ny,ymin,yinc
     1,nz*2,0.,1.
     1,v
     1,1,xg,1.
     1,1,yg,1.
     1,nz*2,0.,1.
     1,work(ivw))
          call util_interpolate(
     1 nx,xmin,xinc
     1,ny,ymin,yinc
     1,nz  ,0.,1.
     1,z
     1,1,xg,1.
     1,1,yg,1.
     1,nz  ,0.,1.
     1,work(izw))
c  make copy of layer depth for top and bottom velocities
          do 3 i = nz*2 , 2 , -2
            iz0 = izw - 1 + i / 2
            iz1 = izw - 1 + i - 1
            iz2 = izw - 1 + i
            work(iz2) = work(iz0) + dz
            work(iz1) = work(iz0)
    3     continue
          call ztot_fill(nz*2,work(izw),work(ivw)
     1,nzg0,zgmin,zginc0,work(ivg))
          call gtol_ave_sum(1.,nzg0,nzinc,work(ivg),vg(1,ixg,iyg))
    2   continue
    1 continue
      return
  999 continue
      print'(/,'' error in rmodter3'')'
      stop
      end

c&&&
c  the following routines have been extracted from hanson's code
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_shein(hfile,dfile,type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,xmin,xmax,ymin,ymax,zmin,zmax,zdatum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,bname,bcolor
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,vname,vcolor
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,cname,ccolor
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nxg,xgmin,xginc,nyg,ygmin,yginc,nzg,zgmin,zginc)
c  read a header file
      implicit  none
      integer   m_cord,n_cord,nxg,nyg,nzg
      integer   mb,mxb,nb,imb(1),itb(1),ixb(1),nxb(1)
      integer   nv,imv(1),itv(1),ixv(1),nxv(1)
      integer   ncv,icv(1)
      integer   ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg
      integer   ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg
      integer   icw1,icw2,icw3,mch,nch,icid,ncseg

      real      x_cord(2,2)
      real      xmin,xmax,ymin,ymax,zmin,zmax,zdatum
      real      xgmin,xginc,ygmin,yginc,zgmin,zginc
      real      xb(1),zb(1),xv(1),yv(1),zv(1),vel(1)
      real      xcv(1),ycv(1),zcv(1)
      character *(*) hfile,dfile,type,cxi,cyi,czi,cord(*)
     1,bname(*),bcolor(*),vname(*),vcolor(*),cname(*),ccolor(*)

      integer   ifile,n0,ierr,nhmod,ib,idb,j,nxb1
      real      shot,base,baseinc,xtop(2),ztop(2)

c  read model in sheins format
c  add defaults to transform list

      dfile = 'SAME'
      type = 'LAYER'

c      print'(/,'' This option reads in a model file in Sheins format'')
c    1 continue
c      call r1efile(hfile,hfile)
c      call r1fname(' Enter file name',hfile,'mod')
c      call r1inq(hfile,*1)

        call rmodopen(ifile,hfile,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
      if (ierr .ne. 0) goto 999

      rewind(ifile)
      read(ifile,*,err=999)shot,base,baseinc,nhmod

      zdatum = 0.
      if (nhmod .eq. 7) then
        cxi = 'XANNOTATION'
        nhmod = 37
      else
        cxi = 'XBASEMENT'
      endif
      cyi = 'YANNOTATION'
      czi = 'DEPTH'

      ibw1 = nhmod
      ibw2 = 0
      ibw3 = 0
      ivw1 = nhmod
      ivw2 = 0
      ivw3 = 0
      icw1 = nhmod
      icw2 = 0
      icw3 = 0

      n_cord = 2
      cord(1) = 'XANNOTATION'
      x_cord(1,1) = 0.
      x_cord(2,1) = 1.
      cord(2) = 'XBASEMENT'
      x_cord(1,2) = base
      x_cord(2,2) = base + baseinc
      call rmoddtrn(m_cord,n_cord,x_cord,cord)

      read(ifile,*,err=999)nb,nxb1,(nxb(ib),ib=1,nb-1)
      nb = nb - 1    ! get rid of the top interface
      if (nb .gt. mb) then
        print*,' error in rmodrold have mb=',mb,' need nb=',nb
        goto 999
      endif

c  read the first line
      read(ifile,*,err=999)idb,xtop(1),ztop(1)
     1,(idb,xtop(2),ztop(2),j=1,nxb1-1)
c  for each subsequent boundary read the velocity and then x,z
      ixv(1) = 0
      ixb(1) = 0
      do 2 ib = 1 , nb
        if (ib .gt. 1) ixv(ib) = ixv(ib-1) + nxv(ib-1)
        if (ib .gt. 1) ixb(ib) = ixb(ib-1) + nxb(ib-1)
        read(ifile,*,err=999)nxv(ib),(xv(j),vel(j)
     1,j=ixv(ib)+1,min(mxb,ixv(ib)+iabs(nxv(ib))))
        read(ifile,*,err=999)(idb,xb(j),zb(j)
     1,j=ixb(ib)+1,min(mxb,ixb(ib)+nxb(ib)))

        if (ixv(ib)+iabs(nxv(ib)) .gt. mxb) then
          print*,' error in rmodrold need more space in arrays'
          print*,' xv,v arrays have dimension =',mxb
     1,' and need',ixv(ib)+iabs(nxv(ib))
          goto 999
        endif
        if (ixb(ib)+nxb(ib) .gt. mxb) then
          print*,' error in rmodrold need more space in arrays'
          print*,' x,z arrays have dimension =',mxb
     1,' and need',ixb(ib)+nxb(ib)
          goto 999
        endif
    2 continue

c  set min and max values
      xmin = xtop(1)
      xmax = xtop(2)
      xgmin = 1
      nxg = 101
      xginc = (xmax - xmin) / (nxg - 1)

      zmin = ztop(1)
      zmax = zb(ixb(nb)+1)
      zgmin = zmin
      nzg = 101
      zginc = (zmax - zmin) / (nzg - 1)

      ymin = 1
      ymax = 1
      nyg = 1
      ygmin = ymin
      yginc = 1

c  set velocity values
      nv = nb
      ncv = nb
      call util_setr(ixv(nb)+iabs(nxv(nb)),zv,0.)
      do 3 ib = 1 , nb
        itv(ib) = ib
        if (nxv(ib) .lt. 0) then
          nxv(ib) = iabs(nxv(ib))
          call util_copy(nxv(ib),xv(ixv(ib)+1),zv(ixv(ib)+1))
          call util_setr(nxv(ib),xv(ixv(ib)+1),xmin)
        endif
        imb(ib) = ib
        imv(ib) = ib
        icv(ib) = ib
        call rmodbpnt(xcv(ib),zcv(ib)
     1,nxb(ib),xb(ixb(ib)+1),zb(ixb(ib)+1),ierr,*999)
    3 continue

      nb = nb - 1    ! get rid of the bottom interface

      call util_setr(ixv(nv)+nxv(nv),itv,1)
      call util_setr(ixb(nb)+nxb(nb),itb,1)

c  set boundary horizon info
      call rmodshor(nb,imb,mbh,nbh,ibid,nbseg,bname,bcolor)
c  set velocity horizon info
      call rmodshor(nv,imv,mvh,nvh,ivid,nvseg,vname,vcolor)
c  set cell horizon info
      call rmodshor(ncv,icv,mch,nch,icid,ncseg,cname,ccolor)

      return

  999 continue
      print*,' error in rmodrold'
      ierr = 101
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmodscop(ifile1,ifile2,mcop,ncop)
c  copy the current section of file 1 to file 2 from current positions
      implicit  none
      integer   ifile1,ifile2,mcop,ncop,i_star,lc
      character crd132*132,star*1
      star = '*'
      ncop = 0

    1 continue
      read(ifile1,'(a)',err=3,end=2)crd132
      call rmodlenr(lc,crd132)
c      print'('' scop c='',a)',crd132(1:lc)
c  search for a * , if it is there quit the copy
      call rmodfchr(i_star,star,1,lc,crd132)
      if (i_star .ne. 0) return

      write(ifile2,'(a)',err=4)crd132(1:lc)
      ncop = ncop + 1
      if (mcop .lt. 0 .or. ncop .lt. mcop) goto 1

    2 continue
c      print'('' rmodscop read end ifile1='',i3,'' ifile2='',i3
c     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

    3 continue
      print'('' rmodscop read err ifile1='',i3,'' ifile2='',i3
     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

    4 continue
      print'('' rmodscop write err ifile1='',i3,'' ifile2='',i3
     1,'' mcop='',i8,'' ncop='',i8)',ifile1,ifile2,mcop,ncop
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_select_picks(file,n_select,i_select_1,i_select_2
     1,mx,nx,x,z,i1,i2,ierr)
c  read select picks from model file
c  file = data file name
c  n_select = # flags to select on (1,2 are valid)
c  i_select_1 = first flag to select on
c  i_select_2 = second flag to select on
c  mx = max number of points to read
c  nx = number of points actualy read
c  x = array for x values            - dimension mx
c  z = array for z values            - dimension mx
c  i1 = array for first flag values  - dimension mx
c  i2 = array for second flag values - dimension mx
c  ierr = error flag ierr = 0 every thing is o.k.
      implicit  none
      character file*(*)
      integer   mx,nx,i1(mx),i2(mx),ierr
      real      x(mx),z(mx)
      integer   n_select,i_select_1,i_select_2

      integer   ifile,n0,ndof,j_select_1,j_select_2,lf
      real      x0,z0,v0
      character crd132*132

      ierr = 0

c  open data file
      call rmodlenr(lf,file)
      call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)

c  find *PICKS section
      call rmodfstr(ifile,1,n0,'*PICKS',crd132,*1)

      ndof = 4    ! max number of columns to read
      nx   = 0    ! saved pick counter

    1 continue    ! come to here for next card

c  read the next card if end of section go to 2
      call rmodrcrd(ifile,ndof,x0,z0,j_select_1,j_select_2,v0,*2)

c  if the flags match save this pick
      if ((j_select_1 .eq. i_select_1)
     1.and. (j_select_2 .eq. i_select_2 .or. n_select .eq. 1)) then
        nx = min(mx,nx+1)
        x(nx) = x0
        z(nx) = z0
        i1(nx) = j_select_1
        i2(nx) = j_select_2
      endif    ! if ((j_select_1 .eq. i_select_1)

c  if there is room go back for more
      if (nx .lt. mx) goto 1

c  come to here if end of section or out of room
    2 continue

      return
  999 continue
      print'(/,'' error during open in rmod_read_horizon'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_apend(ifilea,ifileb)
c  apend data file sections from filea to fileb
      implicit  none
      integer   ifilea,ifileb,msec,mclr
      parameter (msec=100,mclr=100)
      integer iseca(msec),jseca(msec),lseca(msec),lcla(mclr),lcra(mclr)
      integer isecb(msec),jsecb(msec),lsecb(msec),lclb(mclr),lcrb(mclr)
      integer ifilec,n0,ierr,ncop
      integer nseca,ia,la,ica,nca,nsecb,ib,lb,icb,ncb,lsb
      character sec0*16,crd132*132,filea*80,fileb*80
      character seca(msec)*16,cla(mclr)*16,cra(mclr)*40
      character secb(msec)*16,clb(mclr)*16,crb(mclr)*40
      character rfilea*16,rfileb*16

c  --  Check the logical unit numbers for validity
      if ( ifilea .lt. 1 .or. ifilea .gt. 99
     1.or. ifileb .lt. 1 .or. ifileb .gt. 99) return

c  --  Check the file formats for validity
      call rmodrofi(ifilea,rfilea)
      call rmodrofi(ifileb,rfileb)
      if (rfilea .ne. 'FORMATTED'
     1.or. rfileb .ne. 'FORMATTED') return

c  get file names assoicated with these file numbers
      call rmodfofi(ifilea,filea)    ! get file name
      call rmodlenr(la,filea)
      call rmodfofi(ifileb,fileb)    ! get file name
      call rmodlenr(lb,fileb)

c  open a scratch work file
      call rmodopen(ifilec,'rmod.scratch','FORMATTED','SCRATCH'
     1,'SEQUENTIAL','IEEE',n0,ierr)
      if (ierr .ne. 0) return

c  rewind files
      rewind(ifilea)
      rewind(ifileb)
      rewind(ifilec)

c      print'(/,'' rmod_apend appending file '',a,/,'' to file '',a)'
c     1,filea(1:la),fileb(1:lb)

c  make a list of all star cards in filea,fileb
      call rmodnsec(ifilea,msec,nseca,jseca,iseca,lseca,seca)
      call rmodnsec(ifileb,msec,nsecb,jsecb,isecb,lsecb,secb)
c      print*,' nseca=',nseca,' nsecb=',nsecb

c  copy each section in b to c
c  for each section see if a has the same section
c  if it does look for variables in a that are not in b and copy them to
c  if this is a history section skip it , it is handled by history secti
      do ib = 1 , nsecb

c  copy this section from b to c - if it is a history add a comment
c      print'('' copy section from b to c ib='',i2,'' l='',i5
c     1,'' s='',a16)',ib,lsecb(ib),secb(ib)
        if (secb(ib)(1:7) .eq. 'HISTORY') goto 2
        call rmodfcop(ifileb,ifilec,lsecb(ib),ncop)
        call rmodlenr(lsb,secb(ib))

        do ia = 1 , nseca

c  if this b section is also in a we check its contents
          if (seca(ia)(1:lsb) .eq. secb(ib)(1:lsb)) then

c  this section is in b, check a for parameters not in b
            if (jseca(ia) .gt. 0 .or. jsecb(ib) .gt. 0) then

c  make a list of the parameter name and values that are in a but not b
              call rmodnclr(ifilea,iseca(ia),lseca(ia)
     1,mclr,nca,lcla,lcra,cla,cra)
              call rmodnclr(ifileb,isecb(ib),lsecb(ib)
     1,mclr,ncb,lclb,lcrb,clb,crb)
c      print*,' ia=',ia,' ib=',ib,' nca=',nca,' ncb=',ncb

              do ica = 1 , nca

                do icb = 1 , ncb
      if (cla(ica)(1:lcla(ica)) .eq. clb(icb)(1:lclb(icb))) goto 1
                enddo    ! do icb = 1 , ncb
                write(crd132,'(a16,''='',a40)')
     1cla(ica)(1:lcla(ica)),cra(ica)(1:lcra(ica))
                call rmodbdel(crd132)
                call rmodwcim(ifilec,crd132)
    1         continue

              enddo    ! do ica = 1 , nca

            endif    ! if (jseca(ia) .gt. 0 .or. jsecb(ib) .gt. 0) then
            jseca(ia) = -1    ! set flag so seca will not be copied latt
            goto 2

          endif    ! if (seca(ia)(1:lsb) .eq. secb(ib)(1:lsb)) then

        enddo    ! do ia = 1 , nseca

    2 continue

      enddo    ! do ib = 1 , nsecb

c  add sections from file a that are not in file b
      rewind(ifilea)
      do ia = 1 , nseca
        if (jseca(ia) .lt. 0 .or. seca(ia)(1:7) .eq. 'HISTORY') then
          call rmodskip(ifilea,lseca(ia))
        else
          call rmodfcop(ifilea,ifilec,lseca(ia),ncop)
        endif
      enddo    ! do ia = 1 , nseca

c  copy filec back to fileb
      rewind(ifileb)
      rewind(ifilec)
      call rmodfcop(ifilec,ifileb,-1,ncop)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_apend_open(japen,ierr)
c  open the apend file
      implicit  none
      integer   japen,iapen,ierr,n0
      character fapen*80
      data      fapen/'rmod.apend'/,iapen/-1/

      ierr = 0
      if (iapen .lt. 0) then
        call rmodopen(iapen,fapen,'FORMATTED','SCRATCH','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
      endif
      japen = iapen
c      print'('' apend_open ierr='',i3,'' iapen='',i8)',ierr,iapen

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_apend_close
c  close the apend file
c      print'('' closing apend file'')'
      call rmodclos(iapen)
      iapen = -1
      return

  999 continue
      print'(/,'' error in rmod_apend_open'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_apend_to_file(file0)
c  add the apend file to the file file0
      implicit   none
      character file0*(*),file*80
      integer   iapen,ifile,ifile0,ierr,n0

c  get file unit number
      file = file0
      call rmodioff(ifile,file)
      if (ifile .lt. 0) then
        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
      endif

      goto 1

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_apend_to_file_i(ifile0)
c  add the apend file to unit number ifile0
      ifile = ifile0
      call rmodfofi(ifile,file)

c  apend iapen to ifile
    1 continue

      call rmod_apend_open(iapen,ierr)
      if (ierr .ne. 0 .or. ifile .lt. 0) goto 999
c      write(88,'('' adding apend to ifile='',i8,'' iapen='',i8)')
c     1ifile,iapen
      call rmod_apend(iapen,ifile)       ! add the apend file to ifile
c      write(88,'('' adding history to ifile='',i8)')ifile
      call rmod_history_to_file_i(ifile) ! add the history file to ifile
c      call rmod_history_to_file_i(88) ! add the history file to ifile

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_file_to_apend(file0)
c  add the file file0 to the apend file
      file = file0
      call rmodioff(ifile,file)
      if (ifile .lt. 0) then
        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
      endif
c      print'('' file to apend ifile='',i8,'' ierr='',i3,'' f='',a)'
c     1,ifile,ierr,file(1:20)
      goto 2

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_file_to_apend_i(ifile0)
c  add the logical unit number ifile0 to the apend file

      ifile = ifile0
      call rmodfofi(ifile,file)

    2 continue

c      print'('' rmod_file_to_apend file='',a20)',file

c  if this is the first time this has been called open a scratch file
      call rmod_apend_open(iapen,ierr)
      if (ierr .ne. 0 .or. ifile .lt. 0) goto 999

c  apend ifile to iapen
      call rmod_apend(ifile,iapen)
      call rmod_file_to_history_i(ifile)  ! add this file to history

    3 continue
      return


  999 continue
      print'(/,'' error in rmod_apend_to_file'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_history_open(jhist,ierr)
c  open the history file
      implicit  none
      integer   jhist,ihist,ierr,n0,lc
      character fhist*80,crd80*80
      data      fhist/'rmod.history'/,ihist/-1/

      ierr = 0
      jhist = ihist
      if (ihist .lt. 0) then
        call rmodopen(ihist,fhist,'FORMATTED','SCRATCH','SEQUENTIAL'
     1,'IEEE',n0,ierr)
c      write(88,*)' opening history file ihist=',ihist,' ierr=',ierr
        if (ierr .ne. 0) goto 999
        crd80 = ' *HISTORY'
        call rmod_date_to_card(crd80,crd80)
        call rmodlenr(lc,crd80)
        write(ihist,'(a)')crd80(1:lc)
c        write(88,'(a)')crd80(1:lc)

      endif

      jhist = ihist

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_history_close
c  close the history file
c      write(88,'('' closing history file'')')
      call rmodclos(ihist)
      ihist = -1
      return


  999 continue
      print'(/,'' error in rmod_history_open'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_history_to_file(file0)
c  add the history file to file file0
      implicit  none
      character file0*(*),file*80,crd80*80
      integer   ihist,ierr,lc,ifile,ifile0,ncop,n0

      file = file0
      call rmodioff(ifile,file)
      if (ifile .lt. 0) then
        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
      endif
      goto 1

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_history_to_file_i(ifile0)
c  add history file to logical unit number ifile0
      ifile = ifile0
      call rmodfofi(ifile,file)

    1 continue

c  add current history file to ifile
c      write(88,'('' adding history to ifile='',i8,'' ihist='',i8)')
c     1ifile,ihist

      call rmod_history_open(ihist,ierr)
      if (ierr .ne. 0 .or. ifile .lt. 0) goto 999
      rewind(ihist)
      call rmodfend(ifile)
      call rmodfcop(ihist,ifile,-1,ncop)

c      rewind(ihist)
c      call rmodfcop(ihist,88,-1,ncop)

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_file_to_history(file0)
c  add the file file0 to the history file
      file = file0
      call rmodioff(ifile,file)
      if (ifile .lt. 0) then
        call rmodopen(ifile,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,ierr)
        if (ierr .ne. 0) goto 999
      endif
      goto 2

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_file_to_history_i(ifile0)
c  add the logical unit number file ifile0 to the history file
      ifile = ifile0
      call rmodfofi(ifile,file)

    2 continue

      call rmod_history_open(ihist,ierr)
      if (ierr .ne. 0 .or. ifile .lt. 0) goto 999

c      print'('' adding ifile='',i8,'' to ihist='',i8)',ifile,ihist
      call rmodfstr(ifile,1,ncop,'*HISTORY',crd80,*3)
      call rmodfend(ihist)
      crd80 = ' '
      call rmodlenr(lc,file)
      write(crd80,'('' history from file '',a)')file(1:lc)
      call rmod_title_to_history(crd80)
      call rmodscop(ifile,ihist,-1,ncop)

    3 continue

      return

  999 continue
      print'(/,'' error in rmod_history_to_file'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_title_to_history(card)
c  write a title card plus time and date to the history file
      implicit  none
      character card*(*)

      call rmod_date_to_history
      call rmod_card_to_history(card)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_card_to_history(card)
c  write a card to the history file
      implicit  none
      character card*(*)
      integer   ihist,ierr,lc

      call rmod_history_open(ihist,ierr)
      if (ierr .ne. 0) goto 999

c  write this card to the  current history file
      call rmodfend(ihist)
      call rmodlenr(lc,card)
      write(ihist,'(''# '',a)')card(1:lc)

      return

  999 continue
      print'(/,'' error in rmod_title_to_history'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_date_to_card(c1,c2)
c  add the date and time to a card image c2 must be at least *80
      implicit  none
      character c1*80,c2*80,crd80*80,bufd*16,buft*8
      integer   sizeof_real,flag,ibufd(8)

      crd80 = c1
      if (sizeof_real() .ne. 8) then
        flag=0
        call time_date_stringf(ibufd,flag)
        call convert_hh2cc(ibufd,8,bufd,0)
        bufd(11:)=' '
        call time(buft)
        write(crd80(61:79),'(a8,1x,a9)')buft,bufd
      endif    ! if (sizeof_real() .ne. 8) then
      c2 = crd80

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_date_to_history
c  add the date and time to a card image c2 must be at least *80
      implicit  none
      character crd80*80,bufd*16,buft*8
      integer   ihist,ierr,sizeof_real,flag,ibufd(8)

      if (sizeof_real() .ne. 8) then
        flag=0
        call time_date_stringf(ibufd,flag)
        call convert_hh2cc(ibufd,8,bufd,0)
        bufd(11:)=' '
        call time(buft)
        write(crd80(61:79),'(a8,1x,a9)')buft,bufd
        crd80 = ' '
        write(crd80(2:20),'(a8,1x,a9)')buft,bufd
        call rmod_card_to_history(crd80)
      endif    ! if (sizeof_real() .ne. 8) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_wordtype_ctoi(wtype,iwtype)
c  convert character flag for wordtype to integer flag for wordtype
      implicit none
      character *(*) wtype
      integer iwtype

      character ctype *16

      call rmodcaps(wtype,ctype)
      if (ctype .eq. 'IEEE') then
        iwtype = 1 !see wrdcnvrt.h
      elseif (ctype .eq. 'IBM') then
        iwtype = 2
      elseif (ctype .eq. 'VMS') then
        iwtype = 3
      elseif (ctype .eq. 'CRAY') then
        iwtype = 4
      elseif (ctype .eq. 'ASCII') then
        iwtype = 5
      elseif (ctype .eq. 'CHARISMA') then
        iwtype = 6
      elseif (ctype .eq. 'GENERIC') then
        iwtype = 7
      elseif (ctype .eq. 'AGRID') then
        iwtype = 8
      endif

      entry rmod_wordtype_itoc(wtype,iwtype)
c  convert integer flag for wordtype to character flag for wordtype
      wtype = ' '
      if (iwtype .eq. 1) then
        wtype = 'IEEE' !see wrdcnvrt.h
      elseif (iwtype .eq. 2) then
        wtype = 'IBM'
      elseif (iwtype .eq. 3) then
        wtype = 'VMS'
      elseif (iwtype .eq. 4) then
        wtype = 'CRAY'
      elseif (iwtype .eq. 5) then
        wtype = 'ASCII'
      elseif (iwtype .eq. 6) then
        wtype = 'GENERIC'
      elseif (iwtype .eq. 7) then
        wtype = 'CHARISMA'
      elseif (iwtype .eq. 8) then
        wtype = 'AGRID'
      else
        wtype = 'VMS'
      endif

      return
      end

C&&&
C***********************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                 C P S   S U P P O R T  R O U T I N E S
C    Primitive name:  See below
C  Source directory:  ~spws/util/model/
C           Library:  picklib.a
C           Written:  92/10/12   by:  R.S.Day
C      Last revised:             by:
C
C  Purpose: The routines in this file are designed to be an interface
C           between some C and Fortran language routines.
C - The Fortran C routines are in the file RMOD.f.
C   The file RMOD.f was written by Doug Hanson and Bill Harlan.
C - The C-language routines are in the file model_io.c
C   The file model_io was written by R.S. Day.
C-----------------------------------------------------------------------
C ROUTINES defined in the current file(wrapper routines)
C     RMODWRHD_W(...)
C     RMODRDHD_W(...)
C     RMODOPEN_W(...)
C     RMODCLOS_W(...)
C     RMODWCHD_W(...)
C     RMODFCRD_W(...)
C     RMODFSTR_W(...)
C     RMODRVAL_W(...)
C     RMODWRCARD(...)
C     RMODRDCARD(...)
C     RMODNUF2F(...)      Node,User,File packed to network file name
C     RMODF2NUF(...)      Network file name unpacked to Node User and Fi
C     RMOD_TRUPATH(...)   Puts correct path on file name
C     RMOD_FPARSE_W(...)  breaks file into path and name parts
C     RMOD_LOCHOST_W(...) Gets info on local node.
C     RMOD_GETFIL(...)    Get a remote file if necessary
C     RMOD_PUTFIL(...)    Put a file to remote node if necessary
C     RMOD_RMFIL_W(...)   Removes a file.
C     GRIDOPDA(LU,FILE,OPSTAT,NRECL,ISTAT) for direct access
C     GRIDWREC(LU,IREC,DATA,NPTS,ISTAT)
C See documentation for RMOD routines by D. Hanson
C
C EXTERNALS callED: CONVERT_HH2CC   CONVERT_CC2HH  SIZEOF_REAL
C See cpsprim.a
C-----------------------------------------------------------------------
C\END DOC
C**********************************************************************/
      SUBROUTINE RMODWRHD_W(HFILE,DFILE,TRANS,TYPE,OPSTAT,ITRANS,
     +CXI,CYI,CZI,CXO,CYO,CZO,n_cord,x_cord,CORD,
     +XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ZDATUM,
     +NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC,WDTYP,
     +ISTAT)

C PUT SAMPLING RATE AND FILENAMES INTO HEADER FILE.
C  IF ITRANS .NE. 0 WRITE OUT TRANSFORM FILE
      REAL          x_cord(*)
      CHARACTER     CHFILE*80,CDFILE*80,CTRANS*80,CTYPE*8
      CHARACTER     WDTYPE*16,MFILE*96
      CHARACTER*16  CCXI,CCYI,CCZI,CCXO,CCYO,CCZO,CSTATUS
      CHARACTER     FORM*16,ACC*16,STAT*16,DCNAME*16
      CHARACTER     CCORD*256
      INTEGER       WDTYP
      INTEGER HFILE(*),DFILE(*),TRANS(*),OPSTAT,IWRD,LU
      INTEGER CXI(*),CYI(*),CZI(*),CXO(*),CYO(*),CZO(*),CORD(*)
      ISTAT = 1

      CSTATUS='NEW'
      IF(OPSTAT.EQ.1) CSTATUS='OLD'
      IF(OPSTAT.EQ.2) CSTATUS='UNKNOWN'
      IF(OPSTAT.EQ.3) CSTATUS='APPEND'
C Take the ascii data in the integer arrays and convert to true
C character variables. The ascii data must be NULL terminated.
C Then call Dougs routine.
      MFILE=' '
      call CONVERT_HH2CC(HFILE,0,CHFILE,0)
      call CONVERT_HH2CC(DFILE,0,CDFILE,0)
      call CONVERT_HH2CC(TRANS,0,CTRANS,0)
      call CONVERT_HH2CC(TYPE,0,CTYPE,0)
      call CONVERT_HH2CC(CXI,0,CCXI,0)
      call CONVERT_HH2CC(CYI,0,CCYI,0)
      call CONVERT_HH2CC(CZI,0,CCZI,0)
      call CONVERT_HH2CC(CXO,0,CCXO,0)
      call CONVERT_HH2CC(CYO,0,CCYO,0)
      call CONVERT_HH2CC(CZO,0,CCZO,0)
      call CONVERT_HH2CC(CORD,0,CCORD,0)
      m_cord = 16
      call RMOD_WORDTYPE_ITOC(WDTYPE,WDTYP)
      call  RMODWRHD(CHFILE,CDFILE,WDTYPE,MFILE,CTRANS,CTYPE,CSTATUS,
     +ITRANS,CCXI,CCYI,CCZI,CCXO,CCYO,CCZO,m_cord,n_cord,x_cord,CCORD
     +,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ZDATUM
     +,NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC,*90)

      ISTAT = 0
      RETURN
 90   CONTINUE
      RETURN
      END

C***************************************************************
C Read in the header section for a LAYER or GRID file.
C***************************************************************
      SUBROUTINE RMODRDHD_W(HFILE,DFILE,TRANS,TYPE
     +,CXI,CYI,CZI,CXO,CYO,CZO,m_cord,n_cord,x_cord,CORD
     +,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ZDATUM
     +,NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC,WDTYP
     +,ISTAT)
C PUT SAMPLING RATE AND FILENAMES INTO HEADER FILE.
C  IF ITRANS .NE. 0 WRITE OUT TRANSFORM FILE
      INTEGER       LUPR,LU,NCH,NRECL,IREWIND,IWRD,WDTYP
      REAL          x_cord(*)
      CHARACTER     CHFILE*80,CDFILE*80,CTRANS*80,CTYPE*16
      CHARACTER     WDTYPE*16,MFILE*96
      CHARACTER*16  CCXI,CCYI,CCZI,CCXO,CCYO,CCZO
      CHARACTER     CCORD*256,CARD*132
      CHARACTER     CWORD*16,FORM*16,ACC*16,STAT*16,DCNAME*16
      INTEGER       DBUTIL_RFILE2KEY,DBUTIL_GETWORDTYPE,KEY
      INTEGER HFILE(*),DFILE(*),TRANS(*),SIZEOF_REAL
      INTEGER CXI(*),CYI(*),CZI(*),CXO(*),CYO(*),CZO(*),CORD(*)
      DATA LUPR/6/
      ISTAT = 1

      NDIM  = 1
      call CONVERT_HH2CC(HFILE,0,CHFILE,0)
      call CONVERT_HH2CC(CXO,0,CCXO,0)
      call CONVERT_HH2CC(CYO,0,CCYO,0)
      call CONVERT_HH2CC(CZO,0,CCZO,0)
      CCXI=' '
      CCYI=' '
      CCZI=' '
      m_cord = 32
      call  RMODRDHD(CHFILE,CDFILE,WDTYPE,MFILE,CTRANS,CTYPE
     +,CCXI,CCYI,CCZI,CCXO,CCYO,CCZO,m_cord,n_cord,x_cord,CCORD
     +,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,ZDATUM
     +,NXG,XGMIN,XGINC,NYG,YGMIN,YGINC,NZG,ZGMIN,ZGINC
     +,NDIM,LUPR,*90)
      call RMOD_WORDTYPE_CTOI(WDTYPE,WDTYP)

      KEY   = DBUTIL_RFILE2KEY(HFILE)
C     DBUTIL_PUTWORDTYPE(KEY,WDTYP)
C     WDTYP = DBUTIL_GETWORDTYPE(KEY)
C Take the character data and store in integer variables.
C The ascii data will be NULL terminated.
      N = LEN(CHFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(CHFILE,0,HFILE,-N)
      call CONVERT_CC2HH(CDFILE,0,DFILE,-N)
      call CONVERT_CC2HH(CTRANS,0,TRANS,-N)
      N = LEN(CCXI)/SIZEOF_REAL()
      call CONVERT_CC2HH(CTYPE,0,TYPE,-N)
      call CONVERT_CC2HH(CCXI,0,CXI,-N)
      call CONVERT_CC2HH(CCYI,0,CYI,-N)
      call CONVERT_CC2HH(CCZI,0,CZI,-N)
      call CONVERT_CC2HH(CCXO,0,CXO,-N)
      call CONVERT_CC2HH(CCYO,0,CYO,-N)
      call CONVERT_CC2HH(CCZO,0,CZO,-N)
      N = LEN(CCORD)/SIZEOF_REAL()
      call CONVERT_CC2HH(CCORD,0,CORD,-N)

      ISTAT = 0
      RETURN
 90   CONTINUE
      RETURN
      ENTRY RMODHDPR_W(LUNO)
      LUPR = LUNO
      RETURN
      END
CCC
      SUBROUTINE RMODOPEN_W(IFILE,FILE,IFORM,OPSTAT,IACCESS,
     1 NRECL,WDTYP,ISTAT)
C  OPEN FILE
      CHARACTER CFILE*80
      INTEGER   FILE(*),IFILE,LFLAG
      CHARACTER CFORM*16,CSTATUS*16,CACCESS*16,CWDTYP*8
      INTEGER   IFORM,OPSTAT,IACCESS,STDO,WDTYP,NRECL
      INTEGER  SIZEOF_REAL
      EXTERNAL SIZEOF_REAL
      DATA STDO/6/

      ISTAT=1
      LFLAG = 1 !force files to be local files
      call RMODPLF(LFLAG)
C Take the ascii data in the integer arrays and convert to true
C character variables. The ascii data must be NULL terminated.
C Then call Dougs routine.
      CFORM='FORMATTED'
      IF(IFORM.NE.0) CFORM = 'UNFORMATTED'
      CSTATUS='NEW'
      IF(OPSTAT.EQ.1) CSTATUS='OLD'
      IF(OPSTAT.EQ.2) CSTATUS='UNKNOWN'
      IF(OPSTAT.EQ.3) CSTATUS='APPEND'
      CACCESS='SEQUENTIAL'
      IF(IACCESS.NE.0) CACCESS='DIRECT'
      call CONVERT_HH2CC(FILE,0,CFILE,0)
C
      IF(NRECL.LT.0) NRECL = 0
      IF(WDTYP.LT.1) WDTYP = 3 ! VMS WORD TYPE
      call RMOD_WORDTYPE_ITOC(CWDTYP,WDTYP)
      call RMODOPEN(IFILE,CFILE,CFORM,CSTATUS,CACCESS,CWDTYP,NRECL
     1,ISTAT)
      IF(ISTAT.NE.0) GOTO 10
      RETURN
 10   CONTINUE
      IF(OPSTAT .EQ. 0) THEN
       WRITE(STDO,'(A)') 'RMODOPEN_W- ERROR OPENING AS NEW'
      ELSE
       WRITE(STDO,'(A)') 'RMODOPEN_W- ERROR OPENING AS OLD'
      END IF
      RETURN
      END

      SUBROUTINE RMODCLOS_W(JFILE)
C  CLOSE FILE WITH UNIT NUMBER JFILE
      call  RMODCLOS(JFILE)
      RETURN
      END

      SUBROUTINE RMODRDVL_W(IFILE,TITLE,CX,CZ
     +,MV,MXV,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,ISTAT)
C  READ VELOCITY POINTS FROM IFILE
      DIMENSION IMV(1),ITV(1),IXV(1),NXV(1),XV(1),ZV(1),VEL(1)
      INTEGER   CX(*),CZ(*),TITLE(*)
      INTEGER   ISTAT,SIZEOF_REAL
      CHARACTER CCX*16,CCZ*16,CTITLE*80
      ISTAT = 0
      call CONVERT_HH2CC(CX,0,CCX,0)
      call CONVERT_HH2CC(CZ,0,CCZ,0)
      call CONVERT_HH2CC(TITLE,0,CTITLE,0)
      call RMODRXZV(IFILE,CTITLE,CCX,CCZ,
     +MV,MXV,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL,*99)

      N = LEN(CCX)/SIZEOF_REAL()
      call CONVERT_CC2HH(CCX,0,CX,-N)
      call CONVERT_CC2HH(CCZ,0,CZ,-N)
      RETURN
 99   ISTAT=1
      RETURN
      END

      SUBROUTINE RMODWRVL_W(IFILE,INIT,TITLE,CX,CZ,
     +NV,IMV,ITV,IXV,NXV,XV,ZV,VEL)
C  READ VELOCITY POINTS FROM IFILE
      INTEGER   CX(*),CZ(*),TITLE(*)
      DIMENSION IMV(1),ITV(1),IXV(1),NXV(1),XV(1),ZV(1),VEL(1)
      CHARACTER CCX*16,CCZ*16,CTITLE*80
      call CONVERT_HH2CC(CX,0,CCX,0)
      call CONVERT_HH2CC(CZ,0,CCZ,0)
      call CONVERT_HH2CC(TITLE,0,CTITLE,0)
      call RMODWXZV(IFILE,INIT,CTITLE,CCX,CCZ
     +,NV,IMV,ITV,IXV,NXV,XV,ZV,VEL)

      RETURN
      END

C***************************************************************
C Write the *PICK header card information to a LAYER or GRID file
C***************************************************************
      SUBROUTINE RMODWCHD_W( IFILE,ISTR,ICX,ICY,ICZ,NDOF)
      INTEGER ISTR(*),ICX(*),ICZ(*), NDOF
      CHARACTER*32 CX,CY,CZ,STR
C Take the ascii data in the integer arrays and convert to true
C character variables. The ascii data must be NULL terminated.
C Then call Dougs routine.
      call CONVERT_HH2CC(ISTR,0,STR,0)
      call CONVERT_HH2CC(ICX,0,CX,0)
      call CONVERT_HH2CC(ICY,0,CY,0)
      call CONVERT_HH2CC(ICZ,0,CZ,0)
      call RMODWCHD(IFILE,STR,CX,CY,CZ,NDOF)
      RETURN
      END

C***************************************************************
C Read the data header card information from a LAYER or GRID file
C IREWIND # 0 if you want to rewind the file first.
C***************************************************************
      SUBROUTINE RMODFCRD_W( IFILE,IREWIND, ISTR,ICX,ICZ,NDOF,ISTAT)
      INTEGER ISTR(*),ICX(*),ICZ(*), IREWIND, NDOF
      CHARACTER*32 CX,CZ,STR
      INTEGER      SIZEOF_REAL
      ISTAT = 1
      call CONVERT_HH2CC(ISTR,0,STR,0)
      call RMODFCRD(IFILE,IREWIND, STR,CX,CZ,NDOF,*10)
      N = LEN(CX)/SIZEOF_REAL()
      call CONVERT_CC2HH(CX,0,ICX,-N)
      call CONVERT_CC2HH(CZ,0,ICZ,-N)
      ISTAT = 0
      RETURN
 10   CONTINUE
      RETURN
      END

      SUBROUTINE RMODFSTR_W( IFILE,IREWIND,ISTR,ICARD,ISTAT)
      INTEGER ISTR(*),ICARD(*), IREWIND
      CHARACTER STR*32,CARD*132
      INTEGER   SIZEOF_REAL,NRD
      ISTAT = 1
      ICARD(1)=0
      call CONVERT_HH2CC(ISTR,0,STR,0)
      call RMODFSTR(IFILE,IREWIND,NRD,STR,CARD,*10)
      N = LEN(CARD)/SIZEOF_REAL()
      call CONVERT_CC2HH(CARD,0,ICARD,-N)
      ISTAT = 0
      RETURN
 10   CONTINUE
      RETURN
      END


      SUBROUTINE RMODRVAL_W(IFILE,NDOF,MX,NX,X,ISTAT)
      REAL X(NDOF,1)
      ISTAT = 0
      call RMODRVAL(IFILE,NDOF,MX,NX,X,*10)
      RETURN
 10   ISTAT = 1
      RETURN
      END

C Dump a card image to a disk file.
C KEY..... KEY INTO RMOD FILE DATA BASE
C LUN..... Logical unit number
C ICARD... Integer array containing NULL terminated ascii data.
C ISTAT... return error status. zero if all is OK.
C dumps out to the last non-blank or non-null character.
      SUBROUTINE RMODWRCARD(KEY, ICARD, ISTAT)
      INTEGER   KEY
      INTEGER   ICARD(*),LUN,ISTAT,LENC,LENS,DBUTIL_GETLUN
      CHARACTER CARD*132,ZERO,BLANK
      LUN = DBUTIL_GETLUN(KEY)
      ISTAT=1
      IF(LUN.LE.0) RETURN
      call CONVERT_HH2CC(ICARD,0,CARD,0)
      ZERO = CHAR(0)
      BLANK= CHAR(32)
      LENC = LEN(CARD)
      DO 130 I=LENC,1,-1
       IF (CARD(I:I).NE.BLANK.AND.CARD(I:I).NE.ZERO) GO TO 140
  130 CONTINUE
  140 LENS = I
      IF(LENS.GT.0) THEN
        WRITE(LUN,'(A)',ERR=90) CARD(1:LENS)
      END IF
      ISTAT = 0
      RETURN
90    ISTAT=2
      RETURN
      END
C Read a card image from disk file.
C KEY..... KEY INTO RMOD FILE DATA BASE
C LUN..... Logical unit number
C ICARD... Integer array containing NULL terminated ascii data.
C ISTAT... return error status. zero if all is OK.
      SUBROUTINE RMODRDCARD(KEY, ICARD, ISTAT)
      INTEGER   KEY
      INTEGER   ICARD(*),LUN,ISTAT,SIZEOF_REAL,DBUTIL_GETLUN
      CHARACTER CARD*132
      ISTAT=1
      LUN = DBUTIL_GETLUN(KEY)
      IF(LUN.LE.0) RETURN
      CARD = ' '
      READ(LUN,'(A)',ERR=90,END=92) CARD
      N = LEN(CARD)/SIZEOF_REAL()
      call CONVERT_CC2HH(CARD,0,ICARD,-N)
      ISTAT = 0
      RETURN
 90   ISTAT=2
      RETURN
 92   ISTAT=3
      RETURN
      END

      INTEGER FUNCTION RMODNUF2F(CARD,NODE,USER,FILE)
      CHARACTER*(*) CARD,NODE,USER,FILE
      INTEGER       ZERO,N,SIZEOF_REAL,IERR,PARSE_FBLD
      INTEGER  C0(30),N0(10),U0(10),F0(30)
      RMODNUF2F = 0
      ZERO = 0
      IF(NODE.EQ.' ') NODE='NONE'
      IF(USER.EQ.' ') USER='NONE'
      N = LEN(NODE)/SIZEOF_REAL()
      call CONVERT_CC2HH(NODE,0,N0,-N)
      N = LEN(USER)/SIZEOF_REAL()
      call CONVERT_CC2HH(USER,0,U0,-N)
      N = LEN(FILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(FILE,0,F0,-N)
      RMODNUF2F = PARSE_FBLD(C0,N0,U0,F0)
      call CONVERT_HH2CC(C0,0,CARD,0)
      RETURN
      END

      INTEGER FUNCTION RMODF2NUF(CARD,NODE,USER,FILE,MSG)
      CHARACTER*(*) CARD,NODE,USER,FILE,MSG
      INTEGER       ZERO,N,SIZEOF_REAL,IERR,PARSE_FNET
      INTEGER  C0(30),N0(10),U0(10),F0(30),M0(30)
      RMODF2NUF = 0
      ZERO = 0
      N = LEN(CARD)/SIZEOF_REAL()
      call CONVERT_CC2HH(CARD,0,C0,-N)
      RMODF2NUF = PARSE_FNET(C0,N0,U0,F0,M0)
      call CONVERT_HH2CC(N0,0,NODE,0)
      call CONVERT_HH2CC(U0,0,USER,0)
      call CONVERT_HH2CC(F0,0,FILE,0)
      call CONVERT_HH2CC(M0,0,MSG,0)
      RETURN
      END
CCC
      SUBROUTINE RMOD_TRUPATH(FILE, OFILE)
      CHARACTER*(*) FILE, OFILE
      INTEGER  FILE0(30),OFILE0(30)
      INTEGER  ZERO,N,SIZEOF_REAL
      ZERO = 0
      N = LEN(FILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(FILE,0,FILE0,-N)
      N = LEN(OFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(OFILE,0,OFILE0,-N)
      call PARSE_FILE_CPPATH(FILE0,OFILE0)
      call CONVERT_HH2CC(OFILE0,0,OFILE,0)

      RETURN
      END
CCC
      SUBROUTINE RMOD_FPARSE_W(FILE,NAME,PATH)
      CHARACTER*(*) FILE,PATH,NAME
      INTEGER  FILE0(30),PATH0(30),NAME0(30)
      INTEGER  ZERO,N,SIZEOF_REAL,JUNK,PARSE_FILE
      ZERO = 0
      N = LEN(FILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(FILE,0,FILE0,-N)
      JUNK = PARSE_FILE(FILE0,NAME0,PATH0)
      call CONVERT_HH2CC(PATH0,0,PATH,0)
      call CONVERT_HH2CC(NAME0,0,NAME,0)

      RETURN
      END
CCC
      INTEGER FUNCTION RMOD_GETFIL(LFILE,RNODE,RUSER,RFILE)
      CHARACTER*(*) LFILE,RNODE,RUSER,RFILE
      INTEGER       LFILE0(30),RNODE0(8),RUSER0(4),RFILE0(30),MSG0(30)
      INTEGER  ZERO,N,SIZEOF_REAL,WDTYP,NETW_GETFIL,GOTIT
      RMOD_GETFIL = 1
      IF(RFILE(1:4).EQ.'NONE' .OR. RFILE.EQ.' ') RETURN
      ZERO = 0
      N = LEN(RFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(RFILE,0,RFILE0,-N)
      N = LEN(RNODE)/SIZEOF_REAL()
      call CONVERT_CC2HH(RNODE,0,RNODE0,-N)
      N = LEN(RUSER)/SIZEOF_REAL()
      call CONVERT_CC2HH(RUSER,0,RUSER0,-N)
      IF(LFILE.EQ.' ' .OR. LFILE(1:4).EQ.'NONE') THEN
        LFILE0(1)=0
      ELSE
        N = LEN(LFILE)/SIZEOF_REAL()
        call CONVERT_CC2HH(LFILE,0,LFILE0,-N)
      ENDIF
      RMOD_GETFIL = NETW_GETFIL(LFILE0,RNODE0,RUSER0,RFILE0,WDTYP,MSG0)
      call CONVERT_HH2CC(LFILE0,0,LFILE,0)
      RETURN
      END
CCC
      INTEGER FUNCTION RMOD_PUTFIL(LFILE,RNODE,RUSER,RFILE)
      CHARACTER*(*) LFILE,RNODE,RUSER,RFILE
      INTEGER       LFILE0(30),RNODE0(8),RUSER0(4),RFILE0(30),MSG0(30)
      INTEGER  ZERO,N,SIZEOF_REAL,WDTYP,NETW_GETFIL,GOTIT
      RMOD_PUTFIL = 1
      IF(LFILE(1:4).EQ.'NONE' .OR. LFILE.EQ.' ') RETURN
      ZERO = 0
      N = LEN(LFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(LFILE,0,LFILE0,-N)
      N = LEN(RFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(RFILE,0,RFILE0,-N)
      N = LEN(RNODE)/SIZEOF_REAL()
      call CONVERT_CC2HH(RNODE,0,RNODE0,-N)
      N = LEN(RUSER)/SIZEOF_REAL()
      call CONVERT_CC2HH(RUSER,0,RUSER0,-N)
      IF(RFILE.EQ.' ' .OR. RFILE(1:4).EQ.'NONE') THEN
        RFILE0(1)=0
      ELSE
        N = LEN(RFILE)/SIZEOF_REAL()
        call CONVERT_CC2HH(RFILE,0,RFILE0,-N)
      ENDIF
      RMOD_PUTFIL = NETW_PUTFIL(LFILE0,RNODE0,RUSER0,RFILE0,WDTYP,MSG0)
      call CONVERT_HH2CC(RFILE0,0,RFILE,0)
      RETURN
      END

CCC
      INTEGER FUNCTION RMOD_LOCHOST_W(LOC_NAM,LOC_OS)
      CHARACTER*(*) LOC_NAM,LOC_OS
      INTEGER  ZERO,N,SIZEOF_REAL,RMOD_LOCHOST
      INTEGER  WORD_TYP,LOC_NAM0(10),LOC_OS0(4)
      ZERO = 0
      LOC_NAM0(1)=0
      LOC_OS0(1)=0
      RMOD_LOCHOST_W = RMOD_LOCHOST(LOC_NAM0,LOC_OS0)
      call CONVERT_HH2CC(LOC_NAM0,0,LOC_NAM,0)
      call CONVERT_HH2CC(LOC_OS0,0,LOC_OS,0)
      RETURN
      END

CCC R Day   Deletes files that were copied
      SUBROUTINE RMOD_RMFIL_W(LFILE)
      CHARACTER*(*) LFILE
      INTEGER ZERO,LINE0(30),SIZEOF_REAL,N
      ZERO = 0
      IF(LFILE(1:1).EQ.' ') RETURN
      N= LEN(LFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(LFILE,ZERO,LINE0,-N)
      call RMOD_RMFIL(LINE0)
      RETURN
      END
CCC
C**********************************************************
C Querys to the data base on open files
C**********************************************************
CCC     Obtain the network and local file name from the db index
      SUBROUTINE RMODDB_QNETW(INDEX,LFILE,RNODE,RUSER,RFILE)
      CHARACTER*(*) LFILE,RNODE,RUSER,RFILE
      INTEGER  INDEX
      INTEGER  RNODE0(8),RUSER0(8),RFILE0(30),LFILE0(30)
C     call RMODCDB_QNETW(INDEX,LFILE0,RNODE0,RUSER0,RFILE0)
      call CONVERT_HH2CC(RNODE0,0,RNODE,0)
      call CONVERT_HH2CC(RUSER0,0,RUSER,0)
      call CONVERT_HH2CC(RFILE0,0,RFILE,0)
      call CONVERT_HH2CC(LFILE0,0,LFILE,0)
      RETURN
      END
CCC     Obtain the file attributes & db key from the remote file name
      SUBROUTINE RMODDB_QFAT(RFILE,KEY,LUN,WDTYP,RECLEN,FORM,
     + ACCESS,STATUS)
      CHARACTER*(*) RFILE,ACCESS,STATUS,FORM
      INTEGER  N,KEY,LUN,WDTYP,RECLEN,ZERO,SIZEOF_REAL
      INTEGER  DBUTIL_RFILE2KEY,DBUTIL_GETLUN,DBUTIL_GETRECLENGTH
      INTEGER  DBUTIL_GETWORDTYPE
      INTEGER  RFILE0(30),ACCESS0(4),STATUS0(4),FORM0(4)
C...LUN = fortran unit number when access='SEQUENTIAL'
C...LUN should = 0 for all other cases.
C...RECLEN=record length in bytes when ACCESS='DIRECT'
      ZERO = 0
      N= LEN(RFILE)/SIZEOF_REAL()
      call CONVERT_CC2HH(RFILE,ZERO,RFILE0,-N)
      KEY  = DBUTIL_RFILE2KEY(RFILE0)
c  if file has been open get characteristics, otherwise do nothing
      if (key .ge. 0) then
        LUN    = DBUTIL_GETLUN(KEY)
        WDTYP  = DBUTIL_GETWORDTYPE(KEY)
        RECLEN = DBUTIL_GETRECLENGTH(KEY)
        call DBUTIL_GETFAST(KEY,FORM0,ACCESS0,STATUS0)
        call CONVERT_HH2CC(ACCESS0,0,ACCESS,0)
        call CONVERT_HH2CC(STATUS0,0,STATUS,0)
        call CONVERT_HH2CC(FORM0,0,FORM,0)
      endif    ! if (key .ge. 0) then
      RETURN
      END

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_cray_name(file_inp,node,user,file_out)
c Build full file name when on cray
      implicit  none
      character *(*) file_inp,node,user,file_out

      integer   name_inp(80),name_out(80),zero,n,sizeof_real,lm
      integer   rmodf2nuf,ierr
      character *80  file_tmp,msg

c  construct name_out with node, user, path
      zero = 0
      name_out(1) = 0
      n=len(file_inp)/sizeof_real()
      call convert_cc2hh(file_inp,zero,name_inp,-n)
      call rmod_name_bld(name_inp,name_out)
      call convert_hh2cc(name_out,0,file_tmp,0)

c  decompose into node, user, path
      ierr = rmodf2nuf(file_tmp,node,user,file_out,msg)
      if (ierr .ne. 0) then
        call rmodlenr(lm,msg)
        print'(/,'' error in rmod_cray_name msg='',/,a)',msg(1:lm)
      endif

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_interpolate_skip(n_inp,n_inc,x_inp,x_out)
      implicit none
      integer  n_inp,n_inc
      real     x_inp(n_inp),x_out((n_inp-1)*n_inc+1)
      integer  i,j,k,j1,j2
      real     f

      j = (n_inp - 1) * n_inc + 1
      do i = n_inp , 1  , -1
        x_out(j) = x_inp(i)
        j = j - n_inc
      enddo    ! do i = n_inp , 1  , -1

      do k = 2 , n_inc
        j1 = 1
        j2 = 1 + n_inc
        j = k
        f = float((k - 1)) / float(n_inc)
        do i = 1 , n_inp
          x_out(j) = x_out(j1) + f * (x_out(j2) - x_out(j1))
          j  = j  + n_inc
          j1 = j1 + n_inc
          j2 = j2 + n_inc
        enddo    ! do i = 1 , n_inp
      enddo    ! do k = 2 , n_inc
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_gocad_to_grid(type,nx_vel,ny_vel,nz_vel,vel
     1,m_work,work,i_err)
      implicit  none
      character type*(*)
      integer   nx_vel,ny_vel,nz_vel,m_work,i_err
      real      vel(nz_vel,nx_vel,ny_vel),work(ny_vel,nx_vel,nz_vel)

      integer   ix,iy,iz

      i_err = 0

      if (type(1:5) .eq. 'GOCAD') then

        print'(/,'' rmod_gocad_to_grid''
     1,/,'' transposing x and z order for GOCAD model''
     1,/,'' nx_vel='',i10,'' ny_vel='',i10,'' nz_vel='',i10
     1,'' m_work='',i10)',nx_vel,ny_vel,nz_vel,m_work

        if (m_work .lt. nx_vel*ny_vel*nz_vel) then
          print'('' insufficent space to copy velocity''
     1,/,'' m_work='',i10,'' need='',i10)'
     1,m_work,nx_vel,ny_vel,nz_vel
          i_err = -1
          return
        endif    ! if (m_work .lt. nx_vel*ny_vel*nz_vel) then

        call util_copy(nx_vel*ny_vel*nz_vel,vel,work)
        do iz = 1 , nz_vel
          do ix = 1 , nx_vel
            do iy = 1 , ny_vel
              vel(iz,ix,iy) = work(iy,ix,iz)
            enddo    ! do iy = 1 , ny_vel
          enddo    ! do ix = 1 , nx_vel
      print'('' iz='',i10,'' vel='',4(1x,f10.2))'
     1,iz,work(1,1,iz),work(nx_vel,1,iz)
     1,work(1,ny_vel,iz),work(nx_vel,ny_vel,iz)
        enddo    ! do iz = 1 , nz_vel

        type = 'GRID'

      endif    ! if (type(1:5) .eq. 'GOCAD') then

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_average_vz(type,nx_i,ny_i,nz_i,v_i
     1,nx_o,ny_o,nz_o,v_o)
      implicit  none
      integer   nx_i,ny_i,nz_i,nx_o,ny_o,nz_o
      real      v_i(nz_i,nx_i,ny_i),v_o(nz_i)
      character type*(*)
      print'('' this module averages the slowness grid to ''
     1,'' a single surface location.'')'
      if (type .ne. 'GRID') then
        print'('' Model type must be GRID for this option type= '',a16)'
     1,type
        return
      endif
      call util_ave_n(nz_i,nx_i*ny_i,v_i,v_o)
      nx_o = 1
      ny_o = 1
      nz_o = nz_i
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_reverse_negative_increment(mod_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1)
      implicit none

      character mod_type*(*)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel*nx_vel*ny_vel)

      integer  nx_inc,ny_inc,nz_inc
      integer  ix,iy,iz,i_xyz
      integer  jx,jy,jz,j_xyz
      real     vel_i_xyz,vel_j_xyz

      if (mod_type(1:4) .eq. 'GRID') then

        nx_inc = nz_vel
        ny_inc = nz_vel * nx_vel
        nz_inc = 1

      elseif (mod_type(1:4) .eq. 'GRID') then

        nx_inc = 1
        ny_inc = nx_vel
        nz_inc = nx_vel * ny_vel

      else    ! if (mod_type(1:4) .eq. 'GRID') then

        return

      endif    ! if (mod_type(1:4) .eq. 'GRID') then

      if (dx_vel .lt. 0.) then

        print'(/,'' reversing the gridded model x direction''
     1,'' because dx_vel='',f10.2)',dx_vel

c      print*,' nx_inc=',nx_inc,' ny_inc=',ny_inc,' nx_inc=',nx_inc

        do ix = 1 , nx_vel / 2

          jx = nx_vel - ix + 1

          do iy = 1 , ny_vel

            do iz = 1 , nz_vel

              i_xyz = (ix - 1) * nx_inc
     1              + (iy - 1) * ny_inc
     1              + (iz - 1) * nz_inc + 1

              j_xyz = (jx - 1) * nx_inc
     1              + (iy - 1) * ny_inc
     1              + (iz - 1) * nz_inc + 1

              vel_i_xyz  = vel(i_xyz)
              vel_j_xyz  = vel(j_xyz)
              vel(i_xyz) = vel_j_xyz
              vel(j_xyz) = vel_i_xyz

            enddo    ! do iz = 1 , nz_vel

          enddo    ! do iy = 1 , ny_vel

c      print'('' ix='',i5,'' jx='',i5
c     1,'' i='',i8,'' j='',i8
c     1,'' bef v='',f8.0,1x,f8.0
c     1,'' aft v='',f8.0,1x,f8.0)'
c     1,ix,jx,i_xyz,j_xyz
c     1,1./vel_i_xyz,1./vel_j_xyz,1./vel(i_xyz),1./vel(j_xyz)

        enddo    ! do ix = 1 , nx_vel / 2

        x0_vel = (nx_vel - 1) * dx_vel + x0_vel
        dx_vel = abs(dx_vel)

      endif    ! if (dx_vel .lt. 0.) then

      if (dy_vel .lt. 0.) then

        print'(/,'' reversing the gridded model y direction''
     1,'' because dy_vel='',f10.2)',dy_vel

        do iy = 1 , ny_vel / 2

          jy = ny_vel - iy + 1

          do ix = 1 , nx_vel

            do iz = 1 , nz_vel

              i_xyz = (ix - 1) * nx_inc
     1              + (iy - 1) * ny_inc
     1              + (iz - 1) * nz_inc + 1

              j_xyz = (ix - 1) * nx_inc
     1              + (jy - 1) * ny_inc
     1              + (iz - 1) * nz_inc + 1

              vel_i_xyz  = vel(i_xyz)
              vel_j_xyz  = vel(j_xyz)
              vel(i_xyz) = vel_j_xyz
              vel(j_xyz) = vel_i_xyz

            enddo    ! do iz = 1 , nz_vel

          enddo    ! do ix = 1 , nx_vel

        enddo    ! do iy = 1 , ny_vel / 2

        y0_vel = (ny_vel - 1) * dy_vel + y0_vel
        dy_vel = abs(dy_vel)

      endif    ! if (dy_vel .lt. 0.) then

      if (dz_vel .lt. 0.) then

        print'(/,'' reversing the gridded model z direction''
     1,'' because dz_vel='',f10.2)',dz_vel

        do iz = 1 , nz_vel / 2

          jz = nz_vel - iz + 1

          do iy = 1 , ny_vel

            do ix = 1 , nx_vel

              i_xyz = (ix - 1) * nx_inc
     1              + (iy - 1) * ny_inc
     1              + (iz - 1) * nz_inc + 1

              j_xyz = (ix - 1) * nx_inc
     1              + (iy - 1) * ny_inc
     1              + (jz - 1) * nz_inc + 1

              vel_i_xyz  = vel(i_xyz)
              vel_j_xyz  = vel(j_xyz)
              vel(i_xyz) = vel_j_xyz
              vel(j_xyz) = vel_i_xyz

            enddo    ! do ix = 1 , nx_vel

          enddo    ! do iy = 1 , ny_vel

        enddo    ! do iz = 1 , nz_vel / 2

        z0_vel = (nz_vel - 1) * dz_vel + z0_vel
        dz_vel = abs(dz_vel)

      endif    ! if (dz_vel .lt. 0.) then

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_replace_negative_increment(
     1 nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_work
     1,work
     1,i_err)
      implicit none

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(1)
      integer  m_work
      real     work(1)
      integer  i_err
      integer  i_vel,n_vel
      integer  i_work,n_work
      integer  i_work_i,i_work_n

      integer  nx_tmp
      real     x0_tmp,dx_tmp

      integer  ny_tmp
      real     y0_tmp,dy_tmp

      integer  nz_tmp
      real     z0_tmp,dz_tmp

      i_err = 0

      if (
     1      (dx_vel .lt. 0. .and. nx_vel .ne. 1)
     1 .or. (dy_vel .lt. 0. .and. ny_vel .ne. 1)
     1 .or. (dz_vel .lt. 0. .and. nz_vel .ne. 1)
     1) then

        x0_tmp = min(x0_vel,(nx_vel-1)*dx_vel+x0_vel)
        y0_tmp = min(y0_vel,(ny_vel-1)*dy_vel+y0_vel)
        z0_tmp = min(z0_vel,(nz_vel-1)*dz_vel+z0_vel)
        dx_tmp = abs(dx_vel)
        dy_tmp = abs(dy_vel)
        dz_tmp = abs(dz_vel)
        n_vel = nx_vel * ny_vel * nz_vel

        call util_wors(i_work_i,i_work_n,m_work)
        call util_work(i_work_i,i_work_n,i_vel,n_vel)
        call util_worl(i_work_i,i_work_n,n_work)
        call util_work(i_work_i,i_work_n,i_work,n_work)
        call util_worc(i_work_i,i_work_n,i_err)
        if (i_err .ne. 0) goto 999

        call util_copy(nx_vel*ny_vel*nz_vel,vel,work(i_vel))

        call util_interpolate_1(
     1 nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,work(i_vel)
     1,nx_vel,x0_tmp,dx_tmp
     1,ny_vel,y0_tmp,dy_tmp
     1,nz_vel,z0_tmp,dz_tmp
     1,vel
     1,n_work,work(i_work)
     1,i_err)
        if (i_err .ne. 0) goto 999

        x0_vel = x0_tmp
        y0_vel = y0_tmp
        z0_vel = z0_tmp
        dx_vel = dx_tmp
        dy_vel = dy_tmp
        dz_vel = dz_tmp

      endif    ! if (

      return

  999 continue
      print'(/,'' error in memory allocation in ''
     1,'' rmod_replace_negative_increment'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_velocities(lu,title,type
     1,cx,cy,cz,n_cord,x_cord,cord
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,jx_inc,jy_inc,jz_inc)
      implicit none

      real      util_invert_1

      integer   lu
      character title*(*),type*(*)
      character cx*(*),cy*(*),cz*(*)
      integer   n_cord
      real      x_cord(2,1)
      character cord*(*)

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      vel(1)
      integer   jx_inc,jy_inc,jz_inc

      integer   nx_inc,ny_inc,nz_inc

      integer   ix_vel,iy_vel,iz_vel,ixy
      real      v_min,v_max
      integer   cmpi_i_pel
      if (cmpi_i_pel() .ne. 0) return

      if (jx_inc .gt. 0) then
        nx_inc = jx_inc
      elseif (nx_vel .le. 10) then
        nx_inc = 1
      else
        nx_inc = max(1,nx_vel/5)
      endif

      if (jy_inc .gt. 0) then
        ny_inc = jy_inc
      elseif (nz_vel .le. 10) then
        nz_inc = 1
      else
        ny_inc = max(1,ny_vel/5)
      endif

      if (jz_inc .gt. 0) then
        nz_inc = jz_inc
      elseif (nz_vel .le. 10) then
        nz_inc = 1
      else
        nz_inc = max(1,nz_vel/5)
      endif

      if (lu .lt. 0. or. lu .gt. 100) return

      call rmodpgrd(lu,title
     1,cx,cy,cz
     1,n_cord,x_cord,cord
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel)

      call util_min_max(v_min,v_max,nx_vel*ny_vel*nz_vel,vel)

      if (type .eq. 'G3DL') then

        print'('' v_min='',f12.2,'' v_max='',f12.2)'
     1,(v_max),(v_min)

        do iz_vel = 1 , nz_vel , nz_inc

          write(lu,'(/,'' depth at bottom of layer '',i5,'' x''
     1,/,'' iy_vel      y     ''
     1,11(1x,f8.0))')
     1iz_vel,((ix_vel-1)*dx_vel+x0_vel
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)

          do iy_vel = 1 , ny_vel , ny_inc

            if (dz_vel .gt. 1) then

              write(lu,'(1x,i5,1x,f9.0,11(1x,f8.0))')
     1iy_vel,(iy_vel-1)*dy_vel+y0_vel
     1,(vel(ix_vel+(iy_vel-1)*nx_vel+(iz_vel-1)*nx_vel*ny_vel)
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)

            else    ! if (dz_vel .gt. 1) then

              write(lu,'(1x,i5,1x,f9.0,11(1x,f8.3))')
     1iy_vel,(iy_vel-1)*dy_vel+y0_vel
     1,(vel(ix_vel+(iy_vel-1)*nx_vel+(iz_vel-1)*nx_vel*ny_vel)
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)

            endif    ! if (dz_vel .gt. 1) then

          enddo    ! do iy_vel = 1 , ny_vel , ny_inc

        enddo    ! do iz_vel = 1 , nz_vel , nz_inc

      else    ! if (type .eq. 'G3DL') then

        print'('' v_min='',f12.2,'' v_max='',f12.2)'
     1,util_invert_1(v_max),util_invert_1(v_min)

        do iy_vel = 1 , ny_vel , ny_inc

          write(lu,'(/,'' iy_vel ='',i5,'' y='',f12.2
     1,/,'' iz_vel      z     '',11(1x,f8.0))')
     1iy_vel,y0_vel+(iy_vel-1)*dy_vel
     1,((ix_vel-1)*dx_vel+x0_vel
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)
          ixy = (iy_vel - 1) * nz_vel * nx_vel

          do iz_vel = 1 , nz_vel , nz_inc

            if (dz_vel .gt. 1) then

              write(lu,'(1x,i5,1x,f8.0,11(1x,f8.1))')
     1iz_vel,(iz_vel-1)*dz_vel+z0_vel
     1,(util_invert_1(vel(iz_vel+(ix_vel-1)*nz_vel+ixy))
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)

            else    ! if (dz_vel .gt. 1) then

              write(lu,'(1x,i5,1x,f8.3,11(1x,f8.1))')
     1iz_vel,(iz_vel-1)*dz_vel+z0_vel
     1,(util_invert_1(vel(iz_vel+(ix_vel-1)*nz_vel+ixy))
     1,ix_vel=1,nx_vel,max(1,nx_vel/10))
c     1,ix_vel=1,nx_vel,nx_inc)

            endif    ! if (dz_vel .gt. 1) then

          enddo    ! do iz_vel = 1 , nz_vel , nz_inc

        enddo    ! do iy_vel = 1 , ny_vel , ny_inc

      endif

      write(lu,'(//)')

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_array_size(nx,x0,dx,n,x,i_err)
c  determine the regularity of a set of numbers
      implicit none

      integer  nx,n,i_err
      real     x0,dx,x(n)

      integer  i
      real     x_min,x_max,eps

      call util_min_max(x_min,x_max,n,x)
      nx = 1
      x0 = x_min
      dx = 1.

      if (x_min .eq. x_max) then

        if (n .le. 0) nx = 0

      else    ! if (x_min .eq. x_max) then

        eps = (x_max - x_min) / 10000.
        dx = x_max - x_min

        do i = 2 , n

          if (abs(x(i)-x(i-1)) .gt. eps)
     1dx = min(dx,abs(x(i)-x(i-1)))

        enddo    ! do 1 = 1 , n

        nx = nint((x_max - x_min) / dx) + 1

      endif    ! if (x_min .eq. x_max) then

c      print'(/,'' rmod_array_size''
c     1,/,'' n    ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
c     1,/,'' nx   ='',i10,'' x0   ='',G14.7,'' xl   ='',G14.7
c     1,'' dx='',G14.7)'
c     1,n,x_min,x_max,nx,x0,(nx-1)*dx+x0,dx

      if (mod(n,nx) .ne. 0 .or. abs((nx-1)*dx+x0-x_max) .gt. eps) then

        print'(/,'' error in rmod_array_size - grid is not regular'')'
        print'(/,'' rmod_array_size''
     1,/,'' n    ='',i10,'' x_min='',G14.7,'' x_max='',G14.7
     1,/,'' nx   ='',i10,'' x0   ='',G14.7,'' xl   ='',G14.7
     1,'' dx='',G14.7
     1,/,''       i     x_inp      x_comp''
     1)'
     1,n,x_min,x_max,nx,x0,(nx-1)*dx+x0,dx
        print'(1x,i8,1x,f10.2,1x,f10.2,1x,i8)'
     1,(i,x(i),(i-1)*dx+x0,1,i=1,nx)
        i_err = i_err + 1

      endif    ! if (mod(n,nx) .ne. 0) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_set_cord(m_cord,n_cord,x_cord,cord)
c  add defaults to transform list
      implicit none

      integer m_cord,n_cord
      character cord(*)*16
      real x_cord(2,1)

      character *16 DEPTH,KILOMETER,METER,KILOFEET,FEET,TIME,SECOND,MIL
     1,XANNOTATION,XBASEMENT,XGRID,YANNOTATION,YBASEMENT,YGRID

      data DEPTH,KILOMETER,METER,KILOFEET,FEET,TIME,SECOND,MIL
     1,XANNOTATION,XBASEMENT,XGRID,YANNOTATION,YBASEMENT,YGRID
     1/'DEPTH','KILOMETER','METER','KILOFEET','FEET'
     1,'TIME','SECOND','MIL','XANNOTATION','XBASEMENT','XGRID'
     1,'YANNOTATION','YBASEMENT','YGRID'/

      integer imet

c      call rmodgmet(imet)    ! metric flag 0=metric 1=engslish
      imet = 0

      call rmodacor(XBASEMENT  ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(XANNOTATION,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(XGRID      ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YBASEMENT  ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YANNOTATION,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(YGRID      ,0.,   1.,m_cord,n_cord,x_cord,cord)

      if (imet .eq. 0) then    ! metric units
        call rmodacor(DEPTH    ,0.,1000.,m_cord,n_cord,x_cord,cord)
      elseif (imet .lt. 0) then    ! metric units
        call rmodacor(DEPTH    ,0.,1000.,m_cord,n_cord,x_cord,cord)
        call rmodacor(KILOMETER,0.,   1.,m_cord,n_cord,x_cord,cord)
        call rmodacor(METER    ,0.,1000.,m_cord,n_cord,x_cord,cord)
      elseif (imet .gt. 0) then    ! english units
        call rmodacor(DEPTH    ,0.,5000.,m_cord,n_cord,x_cord,cord)
        call rmodacor(KILOFEET ,0.,   5.,m_cord,n_cord,x_cord,cord)
        call rmodacor(FEET     ,0.,5000.,m_cord,n_cord,x_cord,cord)
      endif    ! if (imet .eq. 0) then    ! metric units
      call rmodacor(TIME     ,0.,   1.,m_cord,n_cord,x_cord,cord)
      call rmodacor(SECOND   ,0.,   1.,m_cord,n_cord,x_cord,cord)

      return
      end

