C                        cpsvf_interface.f
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
C
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
C  Process name:  CPSVF_GETLUN
C                 CPSVF_OPEN_CPS_VELFILE
C                 CPSVF_CLOSE_CPS_VELFILE
C                 CPSVF_READ_CPS_VELNAME
C                 CPSVF_READ_CPS_VELFILE
C                 CPSVF_WRITE_CPS_VELFILE
C
C        Author:  Tom Stoeckley
C
C  Last revised:  00/10/10  Schmauch
C
C       Purpose:  This is a C-callable interface to functions in
C                 IO_CPSVF for managing CPS velocity function files
C                 for input and output.  See IO_CPSVF for documentation.
C
C-----------------------------------------------------------------------
C                            REVISION HISTORY
c  2. 00/10/10  Schmauch      Increased size of char_filename in
c                             cpsvf_open_cps_velfile from 80 to 120 characters.
C  1. 97/09/16  Stoeckley     Initial version.
C-----------------------------------------------------------------------
C                            CALLING SEQUENCE
C
C      CALL CPSVF_GETLUN            (LUN)
C
C      CALL CPSVF_OPEN_CPS_VELFILE  (LUN,FILENAME,IOSTAT,NVPP,NVF,NHX,NHY,
C     +                              NMC_SIGN,NMC_POWER,error,eof)
C
C      CALL CPSVF_CLOSE_CPS_VELFILE (LUN,IOSTAT)
C
C      CALL CPSVF_READ_CPS_VELNAME  (LUN,NVPP,VN,NVP,X,Y,CTVF,
C     +                              PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,
C     +                              error,eof)
C
C      CALL CPSVF_READ_CPS_VELFILE  (LUN,NVPP,VN,NVP,X,Y,CTVF,T,V,
C     +                              PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,
C     +                              SV,error,eof)
C
C      CALL CPSVF_WRITE_CPS_VELFILE (LUN,NVPP,VN,NVP,X,Y,CTVF,T,V,
C     +                              PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,
C     +                              SV)
C
C The variables below which are designated HOLLERITH are hollerith (integer)
C arrays which are equivalent to C-language null-terminated character strings.
C            
C integer      LUN        - Fortran logical file number, 1-999.
C hollerith    FILENAME   - Name of the velocity file for input or output.
C                           On return FILENAME will contain the complete name
C                           including all references to disks and directories.
C hollerith    IOSTAT     - 'READ' OR 'WRITE'.
C integer      NVPP       - Number of points in each velocity pair. 
C                           This parameter is returned by OPEN_CPS_VELFILE.
C                           This parameter must be passed to all other calls.
C integer      NVF        - Number of velocity functions in the file.
C                           This parameter is returned by OPEN_CPS_VELFILE.
C hollerith*8  VN         - Velocity function name.
C integer      NVP        - Number of pairs in the velocity function.
C real         X          - X coordinate of the velocity function.
C real         Y          - Y coordinate of the velocity function.
C integer      NHX        - CPS trace header word containing X coord.
C integer      NHY        - CPS trace header word containing Y coord.
C real         NMC_SIGN   - The sign used for the moveout computation; 
C real         NMC_POWER  - The exponent used for the moveout computation
C integer      error      - 1 if error occurred and 0 otherwise.
C integer      eof        - 1 if EOF encountered and 0 otherwise.
C--------------- VELOCITY FUNCTION ARRAYS ------------------
C real         T(i)       - Array that holds 'TIME' values.
C real         V(i)       - Array that holds the 'VELOCITY' values.
C real         SV()       - Array that holds optional values when NVPP>2.
C                           If NVPP>3, then SV will look like a
C                           multidimensional array, ie. SV(NVPP,NVP).
C---------- OPTIONAL FUNCTION CHARACTERISTICS --------------
C hollerith*4  CTVF       - Type of velocity function.
C                           If CTVF=' ', then default is 'VTRM' for time,RMS.
C                           See VA for other possibilities.
C hollerith*10 PROJECT    - Project name. Starts in column 40.
C hollerith*10 LINE       - Line name. Starts IN COLUMN 51
C hollerith*5  RDATE      - Recording date. 
C hollerith*5  PDATE      - Project data.
C hollerith*3  USERID     - User identification.
C hollerith*15 COMMENT    - Comment.
C-----------------------------------------------------------------------
C                                 NOTES
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC



      subroutine cpsvf_getlun (lun)
      implicit none
      integer lun

      call getlun (lun, *992)
      return
992   lun=0
      return
      end



      subroutine cpsvf_open_cps_velfile (lun,filename,iostat,nvpp,nvf,
     +                           nhx,nhy,nmc_sign,nmc_power,error,eof)
      implicit none
      integer lun,filename(*),iostat(*),nvpp,nvf
      integer nhx,nhy,error,eof
      real nmc_sign,nmc_power
      character*120 char_filename
      character*8   char_iostat

      call cpsvf_setup   (.false.,.false.)
      call convert_hh2cc (filename,0,   char_filename,0)
      call convert_hh2cc (iostat  ,0,   char_iostat  ,0)
      error=0
      eof=0
      call OPEN_CPS_VELFILE3 (LuN,char_filename,char_iostat,
     +                           NVPP,NVF,*999,*888,
     +                           NHX,NHY,NMC_SIGN,NMC_POWER)
      return
999   error=1
      return
888   eof=1
      return
      end



      subroutine cpsvf_close_cps_velfile (lun,iostat)
      implicit none
      integer lun,iostat(*)
      character*8  char_iostat

      call convert_hh2cc (iostat,0,   char_iostat,0)
      call CLOSE_CPS_VELFILE (LuN,char_IOSTAT)
      return
      end



      subroutine cpsvf_READ_CPS_VELNAME (LuN,NVPP,VN,NVP,X,Y,CTVF,
     +        PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,error,eof)
      implicit none
      integer lun,nvpp,vn(*),nvp,ctvf(*)
      real x,y
      integer project(*),line(*),rdate(*),pdate(*),userid(*),comment(*)
      integer error,eof
      character*8  char_vn
      character*4  char_ctvf
      character*8  char_project
      character*10  char_line
      character*10  char_rdate
      character*5  char_pdate
      character*3  char_userid
      character*15  char_comment

      error=0
      eof=0
      call READ_CPS_VELNAME (LuN,NVPP,char_VN,NVP,X,Y,char_CTVF,
     +                       *999,*888,
     +                       char_PROJECT,char_LINE,char_RDATE,
     +                       char_PDATE,char_USERID,char_COMMENT)
      call convert_cc2hh (char_vn     ,0,   vn     ,0)
      call convert_cc2hh (char_ctvf   ,0,   ctvf   ,0)
      call convert_cc2hh (char_project,0,   project,0)
      call convert_cc2hh (char_line   ,0,   line   ,0)
      call convert_cc2hh (char_rdate  ,0,   rdate  ,0)
      call convert_cc2hh (char_pdate  ,0,   pdate  ,0)
      call convert_cc2hh (char_userid ,0,   userid ,0)
      call convert_cc2hh (char_comment,0,   comment,0)
      return
999   error=1
      return
888   eof=1
      return
      end





      subroutine cpsvf_READ_CPS_VELfile (LuN,NVPP,VN,NVP,X,Y,CTVF,t,v,
     +        PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,sv,error,eof)
      implicit none
      integer lun,nvpp,vn(*),nvp,ctvf(*)
      real x,y,t(*),v(*),sv(*)
      integer project(*),line(*),rdate(*),pdate(*),userid(*),comment(*)
      integer error,eof
      character*8   char_vn
      character*4   char_ctvf
      character*10  char_project
      character*10  char_line
      character*5   char_rdate
      character*5   char_pdate
      character*3   char_userid
      character*15  char_comment

      error=0
      eof=0
      call READ_CPS_VELfile (LuN,NVPP,char_VN,NVP,X,Y,char_CTVF,
     +                       t,v,*999,*888,
     +                       char_PROJECT,char_LINE,char_RDATE,
     +                       char_PDATE,char_USERID,char_COMMENT,sv)
      call convert_cc2hh (char_vn     ,0,   vn     ,0)
      call convert_cc2hh (char_ctvf   ,0,   ctvf   ,0)
      call convert_cc2hh (char_project,0,   project,0)
      call convert_cc2hh (char_line   ,0,   line   ,0)
      call convert_cc2hh (char_rdate  ,0,   rdate  ,0)
      call convert_cc2hh (char_pdate  ,0,   pdate  ,0)
      call convert_cc2hh (char_userid ,0,   userid ,0)
      call convert_cc2hh (char_comment,0,   comment,0)
      return
999   error=1
      return
888   eof=1
      return
      end





      subroutine cpsvf_write_CPS_VELfile (LuN,NVPP,VN,NVP,X,Y,CTVF,t,v,
     +        PROJECT,LINE,RDATE,PDATE,USERID,COMMENT,sv)
      implicit none
      integer lun,nvpp,vn(*),nvp,ctvf(*)
      real x,y,t(*),v(*),sv(*)
      integer project(*),line(*),rdate(*),pdate(*),userid(*),comment(*)
      character*8   char_vn
      character*4   char_ctvf
      character*10  char_project
      character*10  char_line
      character*5   char_rdate
      character*5   char_pdate
      character*3   char_userid
      character*15  char_comment

      call convert_hh2cc (vn     ,0,   char_vn     ,0)
      call convert_hh2cc (ctvf   ,0,   char_ctvf   ,0)
      call convert_hh2cc (project,0,   char_project,0)
      call convert_hh2cc (line   ,0,   char_line   ,0)
      call convert_hh2cc (rdate  ,0,   char_rdate  ,0)
      call convert_hh2cc (pdate  ,0,   char_pdate  ,0)
      call convert_hh2cc (userid ,0,   char_userid ,0)
      call convert_hh2cc (comment,0,   char_comment,0)
      call write_CPS_VELfile (LuN,NVPP,char_VN,NVP,X,Y,char_CTVF,
     +                        t,v,
     +                        char_PROJECT,char_LINE,char_RDATE,
     +                        char_PDATE,char_USERID,char_COMMENT,sv)
      return
      end




