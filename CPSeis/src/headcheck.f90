!<CPS_v1 type="PROCESS"/>
!!--------------------------- headcheck.f90 ---------------------------------!!
!!--------------------------- headcheck.f90 ---------------------------------!!
!!--------------------------- headcheck.f90 ---------------------------------!!


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
!                         C P S   P R O C E S S             
!
! Name       : HEADCHECK                        (check headers)
! Category   : headers
! Written    : 2001-10-02   by: Tom Stoeckley
! Revised    : 2001-10-02   by: Tom Stoeckley
! Maturity   : production   2001-12-17
! Purpose    : Check trace headers for validity.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process looks at most header words on every trace and prints
! information on invalid header words.  User defined and scratch header
! words are ignored, as are some other header words which can contain any
! value.  Header words which depend on trace order are verified to be
! consistent with the trace order.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs the same traces as it receives without modification.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! GRID      grid transformation structure           used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! No header words are changed.
! A complete list of header words are shown here.
! The action taken with each header word (if any) is shown.
!
! Hwd#    Description                       Verification Performed
! ----    -----------                       ----------------------
!   1     trace sequence number             consistent with trace order
!   2     head mute index                   within range 1 thru NDPT
!   3     current gather number             >= 1 and not decreasing          
!   4     trace number in current gather    >= 1 and not decreasing in gather
!   5     fold of stack                     >= 1
!   6     offset                            >= 0
!   7     CMP X grid coordinate             consistent with headers 17 and 18
!   8     CMP Y grid coordinate             consistent with headers 17 and 18
!   9     original shot profile number      >= 1
!  10     trace number in original shot     >= 1
!  11     source surveyed easting coord     none
!  12     source surveyed northing coord    none
!  13     source elevation                  none
!  14     receiver surveyed easting coord   none
!  15     receiver surveyed northing coord  none
!  16     receiver elevation                none
!  17     CMP surveyed easting coord        none
!  18     CMP surveyed northing coord       none
!  19     CMP elevation                     none
!  20     source hole depth                 none
!  21     receiver hole depth               none
!  22     source component numbers          none
!  23     receiver component numbers        none
!  24     panel number                      >= 0
!  25     largest absolute value            >= 0
!  26     source line number                none
!  27     receiver line number              none
!  28     receiver shotpoint                none
!  29     source shotpoint                  none
! 30-32   scratch                           none
!  33     source X grid coord               consistent with headers 11 and 12
!  34     source Y grid coord               consistent with headers 11 and 12
!  35     receiver X grid coord             consistent with headers 14 and 15
!  36     receiver Y grid coord             consistent with headers 14 and 15
!  37     CMP shotpoint                     none
!  38     CMP line number                   none
!  39     pre-NMO datum shift               none
!  40     post-NMO datum shift              none
!  41     cumulative datum static           none
!  42     cumulative refraction static      none
!  43     cumulative residual static        none
!  44     source uphole time                none
!  45     receiver uphole time              none
!  46     source sequ ground position       none
!  47     receiver sequ ground position     none
! 48-55   user defined                      none
!  56     pre-NMO refraction shift          none
!  57     post-NMO refraction shift         none
! 58-62   scratch                           none
!  63     GVS modifier                      none
!  64     tail mute index                   within range header(2) thru NDPT
! >64     user defined                      none
!
! The grid coordinate header words are allowed to be integerized.
!
! In addition, any trace with more than 55 header words set to zero is
! considered to be bad.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2.
!  1. 2001-12-17  Stoeckley  Initial version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS HEADCHECK Process/NC=80>
!
!                   (This process has no parameters)
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module headcheck_module
      use pc_module
      use named_constants_module
      use grid_module
      implicit none
      private
      public :: headcheck_create
      public :: headcheck_initialize
      public :: headcheck_update
      public :: headcheck_delete
!<execute_only>
      public :: headcheck
      public :: headcheck_wrapup
!</execute_only>


      character(len=100),public,save :: HEADCHECK_IDENT = &
       '$Id: headcheck.f90,v 1.1 2001/12/13 19:30:57 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: headcheck_struct              
 
        private
        logical           :: skip_wrapup     ! wrapup flag.
        integer           :: nwih,ndpt       ! global parameters.
        type(grid_struct) :: grid            ! global parameters.

        integer          :: ntraces          ! total number of traces.
        integer          :: last3            ! last value of header 3.
        integer          :: last4            ! last value of header 4.

        integer          :: nbad_total       ! total number of bad traces.
        integer          :: nfirst_total     ! first bad trace.
        integer          :: nlast_total      ! last bad trace.

        integer          :: nbad_zeroes   ! total number of excess zero traces.
        integer          :: nfirst_zeroes ! first excess zero trace.
        integer          :: nlast_zeroes  ! last excess zero trace.

        integer          :: nbad  (HDR_NOMINAL_SIZE)  ! number of bad traces.
        integer          :: nfirst(HDR_NOMINAL_SIZE)  ! first bad trace.
        integer          :: nlast (HDR_NOMINAL_SIZE)  ! last bad trace.
        double precision :: xmin  (HDR_NOMINAL_SIZE)  ! minimum non-zero value.
        double precision :: xmax  (HDR_NOMINAL_SIZE)  ! maximum non-zero value.
        integer          :: nzero (HDR_NOMINAL_SIZE)  ! number of zero values.

      end type headcheck_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(headcheck_struct),pointer,save :: object      ! needed for traps.

      integer,private,parameter :: TOO_MANY_ZEROES = 55

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine headcheck_create (obj)
      implicit none
      type(headcheck_struct),pointer :: obj       ! arguments

      allocate (obj)

      call headcheck_initialize (obj)
      return
      end subroutine headcheck_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine headcheck_delete (obj)
      implicit none
      type(headcheck_struct),pointer :: obj       ! arguments

!<execute_only>
      call headcheck_wrapup (obj)
!</execute_only>

      deallocate(obj)
      return
      end subroutine headcheck_delete


!!----------------------------- clear --------------------------------------!!
!!----------------------------- clear --------------------------------------!!
!!----------------------------- clear --------------------------------------!!


      subroutine headcheck_clear (obj)
      implicit none
      type(headcheck_struct),intent(inout) :: obj       ! arguments

      obj%ntraces       = 0
      obj%last3         = 0
      obj%last4         = 0

      obj%nbad_total    = 0
      obj%nfirst_total  = 0
      obj%nlast_total   = 0

      obj%nbad_zeroes   = 0
      obj%nfirst_zeroes = 0
      obj%nlast_zeroes  = 0

      obj%nbad  (:)     = 0
      obj%nfirst(:)     = 0
      obj%nlast (:)     = 0
      obj%xmin  (:)     = 0.0
      obj%xmax  (:)     = 0.0
      obj%nzero (:)     = 0
      return
      end subroutine headcheck_clear


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine headcheck_initialize (obj)
      implicit none
      type(headcheck_struct),intent(inout) :: obj       ! arguments

      call headcheck_clear  (obj)
      call headcheck_update (obj)
      return
      end subroutine headcheck_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine headcheck_update (obj)
      implicit none
      type(headcheck_struct),intent(inout),target :: obj        ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('grid'    , obj%grid)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!



!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!



!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call headcheck_clear (obj)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>


      return
      end subroutine headcheck_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine headcheck (obj,ntr,hd,tr)
      implicit none
      type(headcheck_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(inout) :: ntr                ! arguments
      double precision      ,intent(in)    :: hd(:,:)            ! arguments
      real                  ,intent(in)    :: tr(:,:)            ! arguments
      integer                              :: i                  ! local

      do i = 1,ntr
           call headcheck_execute (obj,hd(:,i))
      end do

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call headcheck_wrapup (obj)
      end if
      return
      end subroutine headcheck


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine headcheck_wrapup (obj)
      implicit none
      type(headcheck_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call headcheck_print (obj)
      return
      end subroutine headcheck_wrapup


!!----------------------------- execute ------------------------------------!!
!!----------------------------- execute ------------------------------------!!
!!----------------------------- execute ------------------------------------!!


      subroutine headcheck_execute (obj,hd)
      implicit none
      type(headcheck_struct),intent(inout) :: obj              ! arguments
      double precision      ,intent(in)    :: hd(:)            ! arguments
      integer                              :: indx             ! local
      double precision                     :: head             ! local
      logical                              :: whoops           ! local
      logical                              :: whoops_total     ! local
      integer                              :: kount_zeroes     ! local

      obj%ntraces  = obj%ntraces + 1
      whoops_total = .false.
      kount_zeroes = 0

      do indx = 1,HDR_NOMINAL_SIZE
           whoops = headcheck_specialize (obj,indx,hd)
           head   = hd(indx)

           if (head == 0.0) then
                kount_zeroes    = kount_zeroes    + 1
                obj%nzero(indx) = obj%nzero(indx) + 1
           else if (obj%xmin(indx) == 0.0) then
                obj%xmin(indx) = head
                obj%xmax(indx) = head
           else
                obj%xmin(indx) = min(head, obj%xmin(indx))
                obj%xmax(indx) = max(head, obj%xmax(indx))
           end if

           if (whoops) then
                if (obj%nfirst(indx) == 0) obj%nfirst(indx) = obj%ntraces
                                           obj%nlast (indx) = obj%ntraces
                                           obj%nbad  (indx) = obj%nbad(indx) + 1
                whoops_total = .true.
           end if
      end do

      if (kount_zeroes > TOO_MANY_ZEROES) then
           if (obj%nfirst_zeroes == 0) obj%nfirst_zeroes = obj%ntraces
                                       obj%nlast_zeroes  = obj%ntraces
                                       obj%nbad_zeroes   = obj%nbad_zeroes + 1
           whoops_total = .true.
      end if


      if (whoops_total) then
           if (obj%nfirst_total == 0) obj%nfirst_total = obj%ntraces
                                      obj%nlast_total  = obj%ntraces
                                      obj%nbad_total   = obj%nbad_total + 1
      end if

      obj%last3 = nint(hd(3))
      obj%last4 = nint(hd(4))
      return
      end subroutine headcheck_execute


!!-------------------------- specialize ------------------------------------!!
!!-------------------------- specialize ------------------------------------!!
!!-------------------------- specialize ------------------------------------!!


      function headcheck_specialize (obj,indx,hd) result (whoops)
      implicit none
      type(headcheck_struct),intent(in)    :: obj              ! arguments
      integer               ,intent(in)    :: indx             ! arguments
      double precision      ,intent(in)    :: hd(:)            ! arguments
      logical                              :: whoops           ! result
      double precision                     :: head             ! local
      integer                              :: ihead            ! local

      head  = hd(indx)
      ihead = nint(head)

      select case (indx)

   case ( 1) ; whoops = (ihead /= obj%ntraces)
   case ( 2) ; whoops = (ihead < 1 .or. ihead > obj%ndpt)
   case ( 3) ; whoops = (ihead < 1 .or. ihead < obj%last3)
   case ( 4) ; whoops = (ihead < 1 .or. &
                            (nint(hd(3)) == obj%last3 .and. ihead < obj%last4))
   case ( 5) ; whoops = (ihead < 1)
   case ( 6) ; whoops = (head < 0.0)
   case ( 7) ; whoops = &
                  (nint(grid_get_xgrid_coord(obj%grid,hd(17),hd(18))) /= ihead)
   case ( 8) ; whoops = &
                  (nint(grid_get_ygrid_coord(obj%grid,hd(17),hd(18))) /= ihead)
   case ( 9) ; whoops = (ihead < 1)
   case (10) ; whoops = (ihead < 1)
   case (11) ; whoops = .false.                             ! anything
   case (12) ; whoops = .false.                             ! anything
   case (13) ; whoops = .false.                             ! anything
   case (14) ; whoops = .false.                             ! anything
   case (15) ; whoops = .false.                             ! anything
   case (16) ; whoops = .false.                             ! anything
   case (17) ; whoops = .false.                             ! anything
   case (18) ; whoops = .false.                             ! anything
   case (19) ; whoops = .false.                             ! anything
   case (20) ; whoops = .false.                             ! anything
   case (21) ; whoops = .false.                             ! anything
   case (22) ; whoops = .false.                             ! anything
   case (23) ; whoops = .false.                             ! anything
   case (24) ; whoops = (ihead < 0)
   case (25) ; whoops = (head < 0.0)
   case (26) ; whoops = .false.                             ! anything
   case (27) ; whoops = .false.                             ! anything
   case (28) ; whoops = .false.                             ! anything
   case (29) ; whoops = .false.                             ! anything
   case (30) ; whoops = .false.                             ! scratch
   case (31) ; whoops = .false.                             ! scratch
   case (32) ; whoops = .false.                             ! scratch
   case (33) ; whoops = &
                  (nint(grid_get_xgrid_coord(obj%grid,hd(11),hd(12))) /= ihead)
   case (34) ; whoops = &
                  (nint(grid_get_ygrid_coord(obj%grid,hd(11),hd(12))) /= ihead)
   case (35) ; whoops = &
                  (nint(grid_get_xgrid_coord(obj%grid,hd(14),hd(15))) /= ihead)
   case (36) ; whoops = &
                  (nint(grid_get_ygrid_coord(obj%grid,hd(14),hd(15))) /= ihead)
   case (37) ; whoops = .false.                             ! anything
   case (38) ; whoops = .false.                             ! anything
   case (39) ; whoops = .false.                             ! anything
   case (40) ; whoops = .false.                             ! anything
   case (41) ; whoops = .false.                             ! anything
   case (42) ; whoops = .false.                             ! anything
   case (43) ; whoops = .false.                             ! anything
   case (44) ; whoops = .false.                             ! anything
   case (45) ; whoops = .false.                             ! anything
   case (46) ; whoops = (ihead < 0)
   case (47) ; whoops = (ihead < 0)
   case (48) ; whoops = .false.                             ! user defined
   case (49) ; whoops = .false.                             ! user defined
   case (50) ; whoops = .false.                             ! user defined
   case (51) ; whoops = .false.                             ! user defined
   case (52) ; whoops = .false.                             ! user defined
   case (53) ; whoops = .false.                             ! user defined
   case (54) ; whoops = .false.                             ! user defined
   case (55) ; whoops = .false.                             ! user defined
   case (56) ; whoops = .false.                             ! anything
   case (57) ; whoops = .false.                             ! anything
   case (58) ; whoops = .false.                             ! scratch
   case (59) ; whoops = .false.                             ! scratch
   case (60) ; whoops = .false.                             ! scratch
   case (61) ; whoops = .false.                             ! scratch
   case (62) ; whoops = .false.                             ! scratch
   case (63) ; whoops = .false.                             ! anything
   case (64) ; whoops = (ihead < nint(hd(2)) .or. ihead > obj%ndpt)

   case default ; whoops = .false.

      end select
      return
      end function headcheck_specialize


!!----------------------------- print ------------------------------------!!
!!----------------------------- print ------------------------------------!!
!!----------------------------- print ------------------------------------!!


      subroutine headcheck_print (obj)
      implicit none
      type(headcheck_struct),intent(in)    :: obj             ! arguments
      integer                              :: indx,lun        ! local

      lun = pc_get_lun()

      write(lun,*) ' '
      write(lun,*) '++++++++++++++++++++++++++++++++ HEADCHECK STATISTICS', &
                   ' ++++++++++++++++++++++++++++++++'
      write(lun,*) ' '
      write(lun,*) 'total number of traces examined = ',obj%ntraces
      write(lun,*) 'total number of bad traces      = ',obj%nbad_total
      write(lun,*) 'first bad trace number          = ',obj%nfirst_total
      write(lun,*) 'last bad trace number           = ',obj%nlast_total
      write(lun,*) ' '
      write(lun,*) 'total number of traces with more than ', &
                             TOO_MANY_ZEROES,' zeroes = ',obj%nbad_zeroes
      write(lun,*) 'first trace number     with more than ', &
                             TOO_MANY_ZEROES,' zeroes = ',obj%nfirst_zeroes
      write(lun,*) 'last trace number      with more than ', &
                             TOO_MANY_ZEROES,' zeroes = ',obj%nlast_zeroes
      write(lun,*) ' '
      write(lun,1001)
      write(lun,1002)
      write(lun,1003)
      write(lun,*) ' '

      do indx = 1,HDR_NOMINAL_SIZE
           write(lun,2000) indx,                                            &
                           obj%nbad(indx),obj%nfirst(indx),obj%nlast(indx), &
                           obj%xmin(indx),obj%nzero(indx),obj%xmax(indx)
      end do

1001  format (' header   number    trace number   trace number ', &
                       '  minimum         number       maximum ')
1002  format ('  word    of bad      of first       of last    ', &
                       '  non-zero        of zero      non-zero')
1003  format (' number   headers    bad header     bad header  ', &
                       '  value           values       value   ')

2000  format (2x,i3,2x,i9,3x,i9,6x,i9,4x,g15.8,1x,i9,4x,g15.8)

      write(lun,*) ' '
      write(lun,1001)
      write(lun,1002)
      write(lun,1003)
      write(lun,*) ' '
      write(lun,*) '++++++++++++++++++++++++++++++ END HEADCHECK STATISTICS', &
                   ' ++++++++++++++++++++++++++++++'
      write(lun,*) ' '
      return
      end subroutine headcheck_print


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>


      end module headcheck_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

