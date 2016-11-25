
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velfile.f90 --------------------------------!!
!!------------------------------- velfile.f90 --------------------------------!!
!!------------------------------- velfile.f90 --------------------------------!!


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

 
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E           
!
! Name       : VELFILE
! Category   : velocity
! Written    : 2000-06-20   by: Tom Stoeckley
! Revised    : 2007-01-16   by: Karen Goodger
! Maturity   : beta
! Purpose    : To manage velocity functions read from a velocity file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading a velocity file into memory,
! saving and sorting only selected velocity functions, and returning the
! velocity functions to the calling program as needed.
!
! This primitive uses the VELIO module to read velocity files and the VELSET
! module to store the velocity functions.
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS           
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                  
!
! Create this object and open the file:
!                                                              opt    opt
!                              o     i      o    o   o    o     o      o
!    call velfile_create     (obj,filename,nhx,nhy,error,msg,nmosign,nmoexp)
!                            
!
! Read and close the file, saving (and sorting) only some of the velocity
! functions:
!
!                              b  i  i  i  i   o      o       o    o
!    call velfile_read_file  (obj,x1,y1,x2,y2,nfun,maxpicks,error,msg)
!                            
!                            
! Get the next velocity function:
!                            
!                              b    o      o      o      o      o    
!    call velfile_get_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!           velname,veltype,project,line,rdate,pdate,userid,comment)
!              o       o       o     o     o     o     o       o   
!             opt     opt     opt   opt   opt   opt   opt     opt  
!
!
! Delete this object:
!
!                              b
!    call velfile_delete     (obj)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velfile_struct),pointer obj = pointer to VELFILE data structure.
! logical                    error = error flag (true if error occurred).
! char(*)                      msg = message for possible printing (returned).
!
! char(*) filename  = Name of the velocity file to read.
! integer nhx       = CPS trace header word containing X coordinate.
! integer nhy       = CPS trace header word containing Y coordinate.
! real    nmosign   = The   sign   used for the moveout (normally 1.0).
! real    nmoexp    = The exponent used for the moveout (normally 2.0).
!
! double  x1        = X coordinates for first trace (in header NHX).
! double  y1        = Y coordinates for first trace (in header NHY).
! double  x2        = X coordinates for second trace (in header NHX).
! double  y2        = Y coordinates for second trace (in header NHY).
! integer nfun      = Number of velocity functions saved and sorted.
! integer maxpicks  = maximum number of picks in any saved velocity function.
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = Number of time/vel picks in the velocity function.
! real    tpicks(:) = Array that holds the abscissae (  TIME   picks).
! real    vpicks(:) = Array that holds the ordinates (VELOCITY picks).
!
! char(*) velname   = Name of velocity function  (8 characters).
! char(*) veltype   = Type of velocity function  (4 characters).
! char(*) project   = Project name              (10 characters).
! char(*) line      = Line name                 (10 characters).
! char(*) rdate     = Recording date             (5 characters).
! char(*) pdate     = Processing date            (5 characters).
! char(*) userid    = User ID                    (3 characters).
! char(*) comment   = Comment                   (15 characters).
!
!-------------------------------------------------------------------------------
!                           SUBROUTINE DETAILS
!
! VELFILE_CREATE:
!  (1) Allocates the VELFILE data structure.
!  (2) Opens the velocity file,
!  (3) Returns values found in the file header.
!  (4) Returns defaults for values not in the file.
!
! VELFILE_READ_FILE:
!  (1) Reads through the file, saving only some of the velocity functions.
!  (2) Closes the velocity file.
!  (3) Sorts the saved velocity functions.
!  (4) Returns the number of saved velocity functions.
!
! VELFILE_READ_VELFUN:
!  (1) Returns information for the next velocity function saved.
!
! VELFILE_CLOSE:
!  (1) Deallocates the VELFILE data structure.
!
!-------------------------------------------------------------------------------
!              HOW VELOCITY FUNCTIONS ARE SAVED AND SORTED
!
! If X1 == X2, only functions with XCOORD = X1 = X2 are saved.
! If X1 <  X2, only functions with XCOORD >= X1 are saved.
! If X1 >  X2, only functions with XCOORD <= X1 are saved.
!
! If Y1 == Y2, only functions with YCOORD = Y1 = Y2 are saved.
! If Y1 <  Y2, only functions with YCOORD >= Y1 are saved.
! If Y1 >  Y2, only functions with YCOORD <= Y1 are saved.
!
! Comparisons are made to the nearest integer.
! It is expected that either X1 == X2 or Y1 == Y2 but not both.
!
! If X1 < X2, functions are sorted into increasing order of XCOORD.
! If X1 > X2, functions are sorted into decreasing order of XCOORD.
!
! If Y1 < Y2, functions are sorted into increasing order of YCOORD.
! If Y1 > Y2, functions are sorted into decreasing order of YCOORD.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2007-01-16  Goodger    Add logical argument 'all' to routine 
!                            velfile_read_file.  If true, all functions in
!                            the velocity file are read, rather than some
!                            being rejected based on header information.
!                            SPLT needs this flag.
!  5. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  4. 2006-01-10  B. Menger  Removed Unused Variables.
!  3. 2003-12-09  Stoeckley  Move some code to the new VELSET primitive.
!  2. 2000-10-20  Stoeckley  Add missing required documentation section.
!  1. 2000-09-27  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module velfile_module
      use velio_module
      use velset_module
      implicit none
      public

      character(len=100),public,save :: VELFILE_IDENT = &
       '$Id: velfile.f90,v 1.6 2007/01/17 14:15:31 Goodger beta sps $'

      type,public :: velfile_struct
           private
           type(velio_struct)    ,pointer :: velio
           type(velset_struct)   ,pointer :: velset
           integer                        :: ifun
      end type velfile_struct

      contains


!!------------------------ velfile create ---------------------------------!!
!!------------------------ velfile create ---------------------------------!!
!!------------------------ velfile create ---------------------------------!!


      subroutine velfile_create (obj,filename,nhx,nhy,error,msg,nmosign,nmoexp)
      implicit none
      type(velfile_struct),pointer          :: obj               ! arguments
      character(len=*),intent(in)           :: filename          ! arguments
      integer         ,intent(out)          :: nhx,nhy           ! arguments
      logical         ,intent(out)          :: error             ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      real            ,intent(out),optional :: nmosign,nmoexp    ! arguments
      integer                               :: err,nfunmax       ! local

      allocate (obj)
      nullify (obj%velio) ! jpa
      nullify (obj%velset) ! jpa
      obj%ifun = 0
      call velio_open_read (obj%velio,filename,nfunmax,err,msg,  &
                            nhx,nhy,nmosign,nmoexp)
      call velset_create   (obj%velset,nfunmax,nhx,nhy,nmosign,nmoexp)
      error = (err /= VELIO_OK)
      return
      end subroutine velfile_create


!!------------------------ velfile delete ---------------------------------!!
!!------------------------ velfile delete ---------------------------------!!
!!------------------------ velfile delete ---------------------------------!!


      subroutine velfile_delete (obj)
      implicit none
      type(velfile_struct),pointer          :: obj               ! arguments


      if (associated(obj)) then
           call velio_close   (obj%velio)
           call velset_delete (obj%velset)
           deallocate (obj)
      end if
      return
      end subroutine velfile_delete


!!--------------------------- velfile read file ----------------------------!!
!!--------------------------- velfile read file ----------------------------!!
!!--------------------------- velfile read file ----------------------------!!


      subroutine velfile_read_file (obj,x1,y1,x2,y2,nfun,maxpicks,error,msg,all_arg)
      implicit none
      type(velfile_struct),intent(inout)    :: obj               ! arguments
      double precision,intent(in)           :: x1,y1,x2,y2       ! arguments
      integer         ,intent(out)          :: nfun,maxpicks     ! arguments
      logical         ,intent(out)          :: error             ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      logical,optional,intent(in)           :: all_arg           ! arguments

      integer                               :: ifun,err,npicks   ! local
      real                                  :: xcoord,ycoord     ! local
      real                                  :: tpicks(999)       ! local
      real                                  :: vpicks(999)       ! local
      character(len= 8)                     :: velname           ! local
      character(len= 4)                     :: veltype           ! local
      character(len=10)                     :: project,line      ! local
      character(len= 5)                     :: rdate,pdate       ! local
      character(len= 3)                     :: userid            ! local
      character(len=15)                     :: comment           ! local
      integer                               :: ix1,iy1,ix2,iy2   ! local
      integer                               :: ixcoord,iycoord   ! local
      integer                               :: nfunmax           ! local
      logical                               :: all               ! local

      ix1      = nint(x1)
      iy1      = nint(y1)
      ix2      = nint(x2)
      iy2      = nint(y2)
      nfun     = 0
      maxpicks = 0
      error    = .false.
      msg      = ' '
      nfunmax  = velset_get_nfunmax (obj%velset)
      all=.false.
      if(present(all_arg))all=all_arg

      do ifun = 1,nfunmax 
           call velio_read_velfun                                         &
                  (obj%velio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                   velname,veltype,project,line,rdate,pdate,userid,comment)
           if (err /= VELIO_OK) then
                error = .true.
                return
           end if
           ixcoord = nint(xcoord)
           iycoord = nint(ycoord)


       if(.not.all)then

           if      (ix1 == ix2) then ; if (ixcoord /= ix1) cycle
           else if (ix1 <  ix2) then ; if (ixcoord <  ix1) cycle
           else if (ix1 >  ix2) then ; if (ixcoord >  ix1) cycle
           end if

           if      (iy1 == iy2) then ; if (iycoord /= iy1) cycle
           else if (iy1 <  iy2) then ; if (iycoord <  iy1) cycle
           else if (iy1 >  iy2) then ; if (iycoord >  iy1) cycle
           end if
       endif

           call velset_add_velfun (obj%velset,                         &
                                   xcoord,ycoord,npicks,tpicks,vpicks, &
                                   velname,veltype,                    &
                                   project,line,rdate,pdate,userid,comment)
      end do

      call velio_close (obj%velio)
      nfun     = velset_get_nfun     (obj%velset)
      call velset_sort (obj%velset,ix2-ix1,iy2-iy1)

      maxpicks = velset_get_maxpicks (obj%velset)
      obj%ifun = 0
      return
      end subroutine velfile_read_file


!!--------------------------- velfile get velfun -------------------------!!
!!--------------------------- velfile get velfun -------------------------!!
!!--------------------------- velfile get velfun -------------------------!!


      subroutine velfile_get_velfun &
                   (obj,xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      implicit none
      type(velfile_struct),intent(inout)    :: obj                 ! arguments
      real            ,intent(out)          :: xcoord,ycoord       ! arguments
      integer         ,intent(out)          :: npicks              ! arguments
      real            ,intent(out)          :: tpicks(:),vpicks(:) ! arguments
      character(len=*),intent(out),optional :: velname,veltype     ! arguments
      character(len=*),intent(out),optional :: project,line        ! arguments
      character(len=*),intent(out),optional :: rdate,pdate         ! arguments
      character(len=*),intent(out),optional :: userid              ! arguments
      character(len=*),intent(out),optional :: comment             ! arguments

      obj%ifun = obj%ifun + 1
      call velset_get_velfun (obj%velset,obj%ifun,       &
                    xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      return
      end subroutine velfile_get_velfun


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velfile_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
