!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ multwin.f90 --------------------------------!!
!!------------------------------ multwin.f90 --------------------------------!!
!!------------------------------ multwin.f90 --------------------------------!!


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
! Name       : MULTWIN 
! Category   : math
! Written    : 2000-05-01   by: Tom Stoeckley
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Manager of multiple laterally-varying trace windows.
! Portability: No known limitations, but see Portablility Issues.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is to be used by process modules which need multiple trace
! windows which vary with lateral location specified by a pair of coordinates.
! This primitive owns and manages all parameters needed for this functionality,
! and also manages the interpolation and other calculations required to return
! the appropriate windows for a given trace.
!
! This primitive uses the parameter cache to read and write the parameters
! it needs, to report error messages, etc.
!
! To use this primitive from a process module named xxxx:
!
!    (1) multwin_create      should be called from xxxx_create.
!    (2) multwin_initialize  should be called from xxxx_initialize.
!    (3) multwin_update      should be called from xxxx_update.
!    (4) multwin_get_windows should be called from xxxx.
!    (5) multwin_delete      should be called from xxxx_delete.
!
! The purpose of this primitive is to encapsulate a standard method of
! specifying trace windows for uniformity for the user, to encapsulate
! the code dealing with these trace windows, and to encapsulate the screen
! layout and context-sensitive help so that they need not be repeated in
! the individual processes.  Any changes to this primitive can be made
! without affecting the processes which use it.
!
! This primitive is similar to the LATWIN primitive except that this
! primitive supports more than one trace window at each location, but does
! not support windows which vary with mute time.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                             Action taken
! ----       -----------                             ------------
! NWIH       number of words in trace header         used but not changed
! NDPT       number of sample values in trace        used but not changed
! TSTRT      starting time on trace                  used but not changed
! DT         trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#       Description                            Action taken
! ----       -----------                            ------------
! 2          Top mute index                         used but not changed
! 64         Bottom mute index                      used but not changed
! WIN_HDR_X  first coordinate of window location    used but not changed
! WIN_HDR_Y  second coordinate of window location   used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>


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
!                                        o
!             call multwin_create      (obj)
!
!                                                opt
!                                        b   i    i
!             call multwin_initialize  (obj,nwt)
!             call multwin_update      (obj,nwt,screen)
!
!                                        i  i   o 
!             call multwin_get_windows (obj,hd,twin)
!
!                                        b
!             call multwin_delete      (obj)
!
!
! type(multwin_struct) obj = pointer to the MULTWIN structure.
! integer              nwt = number of windows on a trace (>= 1).
! character(len=*)  screen = screen keyword (if multiple screens).
! real         twin(2,nwt) = top and bottom of correlation window in seconds,
!                             specified in pairs, for any number of windows.
! double             hd(:) = trace header word array.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! The GUI_DEF section for a process which uses this primitive should contain
! an INCLUDE line which says to include the GUI_DEF section of this primitive.
! The HELPSECTION for the parameters in this primitive will also be made
! available to the process when this is done.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  8. 2006-09-11  Stoeckley  Replace pc_register_tab_group w HelpSection line.
!  7. 2006-06-20  Stoeckley  Add pc_register_tab_group for SeisSpace.
!006. 2006-01-10  B. Menger   Removed Unused Variables.
!  5. 2004-01-07  Stoeckley  Simplify code by calling PC_REGISTER_ARRAY_NAMES.
!  4. 2000-09-27  Stoeckley  Add documentation to the HelpSection for 2D data.
!  3. 2000-08-01  Stoeckley  Change coordinate parameter names to use X and Y
!                             instead of A and B.
!  2. 2000-05-17  Stoeckley  Add code to allow inserting and deleting
!                             coordinates in the GUI, and add calls to the
!                             parameter cache minsize and maxsize routines.
!  1. 2000-05-01  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES
!
! Portland Group compiler generates the following fatal execution-time
! error when the following three items are all true:
!
! Item (1): TWIN is dimensioned (:,:).
! Item (2): HD   is dimensioned (:).
! Item (3): The subroutine is called like this from RTC:
!   call multwin_get_windows (obj%multwin, obj%hdmix(1:,obj%nccdp), obj%twin)
!
!   0: COPY_IN: invalid actual argument descriptor (missing or incorrect
!   interface block in caller, or wrong number of arguments?)
!
! If any ONE of the following steps are taken, the error does not occur:
!
! Item (1): TWIN is dimensioned (2,*) instead of (:,:).
! Item (2): HD   is dimensioned (*)   instead of (:).
! Item (3): The subroutine is called like this from RTC:
!   hdum(1:obj%nwih) = obj%hdmix(1:obj%nwih,obj%nccdp)
!   call multwin_get_windows (obj%multwin, hdum, obj%twin)
!
! A similar error also occurs with the Portland Group compiler in MUTEHW.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!   WIN_HDR_X=`I  WIN_HDR_Y=`I 
!   WIN_COOR_X    WIN_COOR_Y     coor_x  coor_y  win WIN_TIM_BEGWIN_TIM_END
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!   `FFFFFFFFF    `FFFFFFFFF     `XXXXXXX`XXXXXXX`XXX`FFFFFFFFFF`FFFFFFFFFF
!<PARMS win_coor_x [/XST/YST]> 
!<PARMS win_coor_y [/XST/YST]> 
!<PARMS coor_x_ARRAYSET [/XST/YST]> 
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!            tabgroup = Window Parameters
!
!<Help KEYWORD="WIN_HDR_X">
!<Tip> Header word designating window coordinate X. </Tip>
! Default = 7
! Allowed = any value from 1 through NWIH.
!
! Header word containing coordinate X to use for specifying spatially varying
! windows.
!</Help>
!
!
!<Help KEYWORD="WIN_HDR_Y">
!<Tip> Header word designating window coordinate Y. </Tip>
! Default = 8
! Allowed = any value from 1 through NWIH.
!
! Header word containing coordinate Y to use for specifying spatially varying
! windows.
!</Help>
!
!
!<Help KEYWORD="WIN_COOR_X">
!<Tip> Array of coord X values for specifying spatially varying windows. </Tip>
! Default = 0.0 (one location).
! Allowed = up to any number of locations specified by WIN_HDR_X values.
!
! If only one X coordinate is specified, its value is irrelevant.
!</Help>
!
!
!<Help KEYWORD="WIN_COOR_Y">
!<Tip> Array of coord Y values for specifying spatially varying windows. </Tip>
! Default = 0.0 (one location).
! Allowed = up to any number of locations specified by WIN_HDR_Y values.
!
! If only one Y coordinate is specified, its value is irrelevant.
! For 2D data, only one Y coordinate should be specified.
!</Help>
!
!
!<Help KEYWORD="COOR_X">
!<Tip> Coordinate from WIN_COOR_X array. </Tip>
!</Help>
!
!
!<Help KEYWORD="COOR_Y">
!<Tip> Coordinate from WIN_COOR_Y array. </Tip>
!</Help>
!
!
!<Help KEYWORD="WIN">
!<Tip> Window number. </Tip>
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_BEG">
!<Tip> Array of window top times (in seconds). </Tip>
! Default = top of trace (TSTRT) for one window.
! Default = equally spaced for multiple windows.
! Allowed = any number of values >= top of trace.
!
! These windows are spatially varying as given at the (WIN_COOR_X,WIN_COOR_Y)
! locations.  Each correlation window is interpolated between the specified
! coordinates, and extrapolated unchanged outside of the grid of coordinates.
!
! The number of values specified is the number of windows on a trace times
! the number of window locations.
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_END">
!<Tip> Array of window bottom times (in seconds). </Tip>
! Default = bottom of trace (TSTRT+(NDPT-1)*DT) for one window.
! Default = equally spaced for multiple windows.
! Allowed = any number of values > WIN_TIM_BEG and <= bottom of trace.
!
! These windows are spatially varying as given at the (WIN_COOR_X,WIN_COOR_Y)
! locations.  Each correlation window is interpolated between the specified
! coordinates, and extrapolated unchanged outside of the grid of coordinates.
!
! The number of values specified is the number of windows on a trace times
! the number of window locations.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module multwin_module
      use named_constants_module
      use pc_module
      use interp_module
      use mth_module
      use mem_module
      implicit none
      private
      public :: multwin_create
      public :: multwin_initialize
      public :: multwin_update
      public :: multwin_get_windows
      public :: multwin_delete

      character(len=100),public,save :: MULTWIN_IDENT = &
       '$Id: multwin.f90,v 1.8 2006/09/11 13:15:48 Stoeckley prod sps $'


      type,public :: multwin_struct              

       private
       integer          :: nwt                       ! provided
       integer          :: nwih,ndpt                 ! globals
       real             :: tstrt,dt,tstop,tlen       ! globals
       integer          :: win_hdr_x,win_hdr_y       ! process parameters
       integer          :: ncoorx,ncoory,nwins       ! process parameters
       real   ,pointer  :: win_coor_x(:)             ! process parameters
       real   ,pointer  :: win_coor_y(:)             ! process parameters
       real   ,pointer  :: coor_x(:)                 ! process parameters
       real   ,pointer  :: coor_y(:)                 ! process parameters
       integer,pointer  :: win(:)                    ! process parameters
       real   ,pointer  :: win_tim_beg(:)            ! process parameters
       real   ,pointer  :: win_tim_end(:)            ! process parameters
       real   ,pointer  :: windows(:,:,:)            ! dependent

      end type multwin_struct

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine multwin_create (obj)
      implicit none
      type(multwin_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%win_coor_x)        ! process parameter
      nullify (obj%win_coor_y)        ! process parameter
      nullify (obj%coor_x)            ! process parameter (gui only)
      nullify (obj%coor_y)            ! process parameter (gui only)
      nullify (obj%win)               ! process parameter (gui only)
      nullify (obj%win_tim_beg)       ! process parameter
      nullify (obj%win_tim_end)       ! process parameter
      nullify (obj%windows)           ! dependent

      call multwin_initialize (obj,1)
      return
      end subroutine multwin_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine multwin_delete (obj)
      implicit none
      type(multwin_struct),pointer :: obj       ! arguments

      call mem_free (obj%win_coor_x )
      call mem_free (obj%win_coor_y )
      call mem_free (obj%coor_x     )
      call mem_free (obj%coor_y     )
      call mem_free (obj%win        )
      call mem_free (obj%win_tim_beg)
      call mem_free (obj%win_tim_end)
      call mem_free (obj%windows    )

      deallocate(obj)
      return
      end subroutine multwin_delete


!!----------------------- private get globals ----------------------------!!
!!----------------------- private get globals ----------------------------!!
!!----------------------- private get globals ----------------------------!!


      subroutine multwin_private_get_globals (obj,nwt)
      implicit none
      type(multwin_struct),intent(inout) :: obj                 ! arguments
      integer             ,intent(in)    :: nwt                 ! arguments

      obj%nwih  = 0
      obj%ndpt  = 0
      obj%tstrt = 0.0
      obj%dt    = 0.0

      call pc_get_global ('nwih' , obj%nwih )
      call pc_get_global ('ndpt' , obj%ndpt )
      call pc_get_global ('tstrt', obj%tstrt)
      call pc_get_global ('dt'   , obj%dt   )

      if (obj%dt <= 0.0) then
           call pc_error ('MULTWIN: global DT not set')
           obj%dt = 0.004
      end if

      if (obj%nwih < HDR_NOMINAL_SIZE) then
           call pc_error ('MULTWIN: global NWIH not set')
           obj%nwih = HDR_NOMINAL_SIZE
      end if

      if (obj%ndpt <= 1) then
           call pc_error ('MULTWIN: global NDPT not set')
           obj%ndpt = 2
      end if

      obj%tstop = obj%tstrt + (obj%ndpt-1) * obj%dt
      obj%tlen  = (obj%tstop - obj%tstrt) / nwt
      return
      end subroutine multwin_private_get_globals


!!------------------------------- index ----------------------------------!!
!!------------------------------- index ----------------------------------!!
!!------------------------------- index ----------------------------------!!


      function multwin_index (icoorx,icoory,iwin, ncoorx,nwt) result (indx)
      implicit none
      integer,intent(in) :: icoorx,icoory,iwin,ncoorx,nwt      ! local
      integer            :: indx                               ! result

      indx = iwin + (icoorx - 1 + (icoory - 1)*ncoorx)*nwt
      return
      end function multwin_index


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine multwin_initialize (obj,nwt)
      implicit none
      type(multwin_struct),intent(inout) :: obj                 ! arguments
      integer             ,intent(in)    :: nwt                 ! arguments
      integer                            :: indx                ! local
      integer                            :: icoorx,icoory,iwin  ! local

      call multwin_private_get_globals (obj,nwt)

      obj%nwt       = nwt
      obj%win_hdr_x = 7
      obj%win_hdr_y = 8
      obj%ncoorx    = 1
      obj%ncoory    = 1
      obj%nwins     = obj%ncoorx * obj%ncoory * obj%nwt

      call mem_alloc (obj%win_coor_x ,obj%ncoorx)
      call mem_alloc (obj%win_coor_y ,obj%ncoory)
      call mem_alloc (obj%win_tim_beg,obj%nwins)
      call mem_alloc (obj%win_tim_end,obj%nwins)
      call mem_free  (obj%windows)

      obj%win_coor_x(1:obj%ncoorx) = 0.0
      obj%win_coor_y(1:obj%ncoory) = 0.0

      do icoory = 1,obj%ncoory
      do icoorx = 1,obj%ncoorx
      do iwin   = 1,obj%nwt
           indx = multwin_index (icoorx,icoory,iwin, obj%ncoorx,obj%nwt)
           obj%win_tim_beg(indx) = obj%tstrt+(iwin-1)*obj%tlen
           obj%win_tim_end(indx) = obj%tstrt+ iwin   *obj%tlen
      end do
      end do
      end do
      return
      end subroutine multwin_initialize


!!----------------------------- update ------------------------------------!!
!!----------------------------- update ------------------------------------!!
!!----------------------------- update ------------------------------------!!


      subroutine multwin_update (obj,nwt,screen)
      implicit none
      type(multwin_struct)     ,intent(inout) :: obj               ! arguments
      integer                  ,intent(in)    :: nwt               ! arguments
      character(len=*),optional,intent(in)    :: screen            ! arguments
      integer      :: indx,lun,icoorx,icoory,iwin,indx1,indx2      ! local

      integer      :: keep_ncoorx,keep_ncoory,keep_nwins ! local
      real         :: keep_win_coor_x (obj%ncoorx)                 ! local
      real         :: keep_win_coor_y (obj%ncoory)                 ! local
      real         :: keep_win_tim_beg(obj%nwins)                  ! local
      real         :: keep_win_tim_end(obj%nwins)                  ! local
      character(len=40) :: screen2                                 ! local
      logical      :: verifya,verifyb                              ! local
      integer      :: indxx,indxy,actionx,actiony                  ! local

      if (present(screen)) then
           screen2 = screen
      else
           screen2 = ' '
      end if

!----------save copy of parameters:

      keep_ncoorx = obj%ncoorx
      keep_ncoory = obj%ncoory
      keep_nwins  = obj%nwins 
      keep_win_coor_x (1:obj%ncoorx) = obj%win_coor_x  (1:obj%ncoorx)
      keep_win_coor_y (1:obj%ncoory) = obj%win_coor_y  (1:obj%ncoory)
      keep_win_tim_beg(1:obj%nwins ) = obj%win_tim_beg (1:obj%nwins )
      keep_win_tim_end(1:obj%nwins ) = obj%win_tim_end (1:obj%nwins )

!----------read parameters:

      call pc_register_array_names ('coor_x_arrayset', (/ 'coor_x     ' ,  &
                                                          'coor_y     ' ,  &
                                                          'win        ' ,  &
                                                          'win_tim_beg' ,  &
                                                          'win_tim_end' /))

      call multwin_private_get_globals (obj,nwt)

      call pc_get        ('win_hdr_x'   , obj%win_hdr_x          )
      call pc_get        ('win_hdr_y'   , obj%win_hdr_y          )
      call pc_alloc      ('win_coor_x'  , obj%win_coor_x  ,obj%ncoorx)
      call pc_alloc      ('win_coor_y'  , obj%win_coor_y  ,obj%ncoory)
      call pc_alloc      ('win_tim_beg' , obj%win_tim_beg ,obj%nwins)
      call pc_alloc      ('win_tim_end' , obj%win_tim_end ,obj%nwins)

!----------validate header words:

      if (pc_verify_scalar('WIN_HDR_X') .or. pc_verify_screen(screen2)) then
      if (obj%win_hdr_x <= 0 .or. obj%win_hdr_x > obj%nwih) then
           call pc_error ('MULTWIN: Invalid WIN_HDR_X value ', obj%win_hdr_x)
      end if
      end if
      if (pc_verify_scalar('WIN_HDR_Y') .or. pc_verify_screen(screen2)) then
      if (obj%win_hdr_y <= 0 .or. obj%win_hdr_y > obj%nwih) then
           call pc_error ('MULTWIN: Invalid WIN_HDR_Y value ', obj%win_hdr_y)
      end if
      end if
      if (pc_verify_scalar('WIN_HDR_X') .or. &
          pc_verify_scalar('WIN_HDR_Y') .or. pc_verify_screen(screen2)) then
      if (obj%win_hdr_x == obj%win_hdr_y) then
           call pc_error &
               ('MULTWIN: WIN_HDR_X and WIN_HDR_Y values cannot be the same')
      end if
      end if

!----------make sure we have at least one pair of coordinates:

      if (obj%ncoorx == 0) then
           call pc_info ('MULTWIN: there must be at least one X coordinate')
           obj%ncoorx = 1
           call mem_alloc (obj%win_coor_x, obj%ncoorx)
           obj%win_coor_x(:) = 0.0
           if (keep_ncoorx == 1) obj%win_coor_x(1) = keep_win_coor_x(1)
      end if

      if (obj%ncoory == 0) then
           call pc_info ('MULTWIN: there must be at least one Y coordinate')
           obj%ncoory = 1
           call mem_alloc (obj%win_coor_y, obj%ncoory)
           obj%win_coor_y(:) = 0.0
           if (keep_ncoory == 1) obj%win_coor_y(1) = keep_win_coor_y(1)
      end if

!----------make sure the coordinates are in ascending order:
!----------also check for nil values:

      if (pc_verify_array('WIN_COOR_X') .or. pc_verify_screen(screen2)) then
      do icoorx = 1,obj%ncoorx
           if (obj%win_coor_x(icoorx) == FNIL) then
                call pc_error &
                       ('MULTWIN: some of coordinates X have not been set')
                exit
           end if
      end do
      do icoorx = 2,obj%ncoorx
           if (obj%win_coor_x(icoorx  ) /= FNIL .and.  &
               obj%win_coor_x(icoorx-1) /= FNIL .and.  &
               obj%win_coor_x(icoorx) <= obj%win_coor_x(icoorx-1)) then
                call pc_error &
                       ('MULTWIN: coordinates X are not in ascending order')
                exit
           end if
      end do
      end if

      if (pc_verify_array('WIN_COOR_Y') .or. pc_verify_screen(screen2)) then
      do icoory = 1,obj%ncoory
           if (obj%win_coor_y(icoory) == FNIL) then
                call pc_error &
                       ('MULTWIN: some of coordinates Y have not been set')
                exit
           end if
      end do
      do icoory = 2,obj%ncoory
           if (obj%win_coor_y(icoory  ) /= FNIL .and.  &
               obj%win_coor_y(icoory-1) /= FNIL .and.  &
               obj%win_coor_y(icoory) <= obj%win_coor_y(icoory-1)) then
                call pc_error &
                       ('MULTWIN: coordinates Y are not in ascending order')
                exit
           end if
      end do
      end if

!----------be happy and JUMP AHEAD if everything is consistent:

      if (obj%nwins == nwt * obj%ncoorx * obj%ncoory) then
           obj%nwt = nwt
           go to 500
      end if

!----------restore everything and JUMP AHEAD if this is not a GUI update:

      if (pc_get_update_state() /= PC_GUI) then
           obj%ncoorx = keep_ncoorx
           obj%ncoory = keep_ncoory
           obj%nwins  = keep_nwins
           call mem_alloc (obj%win_coor_x , obj%ncoorx)
           call mem_alloc (obj%win_coor_y , obj%ncoory)
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           obj%win_coor_x (1:obj%ncoorx) = keep_win_coor_x  (1:obj%ncoorx)
           obj%win_coor_y (1:obj%ncoory) = keep_win_coor_y  (1:obj%ncoory)
           obj%win_tim_beg(1:obj%nwins ) = keep_win_tim_beg (1:obj%nwins )
           obj%win_tim_end(1:obj%nwins ) = keep_win_tim_end (1:obj%nwins )
           call pc_error &
             ('MULTWIN: wrong number of WIN_TIM_BEG and WIN_TIM_END values')
           go to 500       ! note that nwt /= obj%nwt now.
      end if

!----------restore old windows if there was an attempt to insert or remove:
!            (GUI update only)

      if (obj%nwins /= keep_nwins) then
           obj%nwins = keep_nwins
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           obj%win_tim_beg(1:obj%nwins) = keep_win_tim_beg(1:obj%nwins)
           obj%win_tim_end(1:obj%nwins) = keep_win_tim_end(1:obj%nwins)
           call pc_info ('MULTWIN: You cannot insert or remove rows')
           call pc_info (' from the WIN_TIM_BEG and WIN_TIM_END arrays.')
           call pc_info (' Instead, you can insert or remove rows')
           call pc_info (' from the WIN_COOR_X or WIN_COOR_Y array,')
           call pc_info (' or change the number of windows per trace.')
      end if

!----------update the number of windows per trace using the old coords:
!            (GUI update only)

      if (nwt > obj%nwt) then
           call pc_error ('MULTWIN: The number of windows has increased.')
           call pc_error ('MULTWIN: You must set these new window times.')
           call pc_error ('MULTWIN: You should recheck the old window times.')
      else if (nwt < obj%nwt) then
           call pc_warning ('MULTWIN: The number of windows has decreased.')
           call pc_warning ('MULTWIN: You should recheck the window times.')
      end if

      if (nwt /= obj%nwt) then
           obj%nwins = nwt * keep_ncoorx * keep_ncoory
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           do icoory = 1,keep_ncoory
           do icoorx = 1,keep_ncoorx
           do iwin   = 1,min(obj%nwt,nwt)
             indx1 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,    nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           do iwin   = obj%nwt+1,nwt
             indx2 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,    nwt)
             obj%win_tim_beg(indx2) = FNIL
             obj%win_tim_end(indx2) = FNIL
           end do
           end do
           end do
           obj%nwt = nwt
      end if

!----------check for changed number of coordinates and JUMP ahead if error:
!            (GUI update only)

      verifya = pc_verify_element ('win_coor_x', indxx, actionx)
      verifyb = pc_verify_element ('win_coor_y', indxy, actiony)

      if ( (actionx /= PC_NOACTION .and. actiony    /= PC_NOACTION    ) .or. &
           (actionx == PC_INSERT   .and. obj%ncoorx /= keep_ncoorx + 1) .or. &
           (actionx == PC_REMOVE   .and. obj%ncoorx /= keep_ncoorx - 1) .or. &
           (actiony == PC_INSERT   .and. obj%ncoory /= keep_ncoory + 1) .or. &
           (actiony == PC_REMOVE   .and. obj%ncoory /= keep_ncoory - 1) .or. &
           (actionx /= PC_INSERT   .and. actionx    /= PC_REMOVE   .and.     &
                                         obj%ncoorx /= keep_ncoorx    ) .or. &
           (actiony /= PC_INSERT   .and. actiony    /= PC_REMOVE   .and.     &
                                         obj%ncoory /= keep_ncoory    ) ) then
        call pc_error ('MULTWIN: parameter cache INSERT/REMOVE discrepancy.')
        if (obj%ncoorx /= keep_ncoorx) then
           call mem_alloc (obj%win_coor_x, keep_ncoorx)
           obj%win_coor_x(1:keep_ncoorx) = keep_win_coor_x(1:keep_ncoorx)
           obj%ncoorx = keep_ncoorx
        end if
        if (obj%ncoory /= keep_ncoory) then
           call mem_alloc (obj%win_coor_y, keep_ncoory)
           obj%win_coor_y(1:keep_ncoory) = keep_win_coor_y(1:keep_ncoory)
           obj%ncoory = keep_ncoory
        end if
        go to 500
      end if

!----------adjust windows for changed number of coordinates:
!            (GUI update only)

      if (actionx == PC_INSERT) then
           obj%nwins = obj%nwt * obj%ncoorx * obj%ncoory
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           do iwin   = 1,obj%nwt
           do icoory = 1,obj%ncoory
           do icoorx = 1,indxx-1
             indx1 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
             indx2 = multwin_index (indxx ,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = FNIL
             obj%win_tim_end(indx2) = FNIL
           do icoorx = indxx+1,obj%ncoorx
             indx1 = multwin_index (icoorx-1,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx  ,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           end do
           end do
      else if (actionx == PC_REMOVE) then
           obj%nwins = obj%nwt * obj%ncoorx * obj%ncoory
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           do iwin   = 1,obj%nwt
           do icoory = 1,obj%ncoory
           do icoorx = 1,indxx-1
             indx1 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           do icoorx = indxx,obj%ncoorx
             indx1 = multwin_index (icoorx+1,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx  ,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           end do
           end do
      else if (actiony == PC_INSERT) then
           obj%nwins = obj%nwt * obj%ncoorx * obj%ncoory
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           do iwin   = 1,obj%nwt
           do icoorx = 1,obj%ncoorx
           do icoory = 1,indxy-1
             indx1 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
             indx2 = multwin_index (icoorx,indxy ,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = FNIL
             obj%win_tim_end(indx2) = FNIL
           do icoory = indxy+1,obj%ncoory
             indx1 = multwin_index (icoorx,icoory-1,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory  ,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           end do
           end do
      else if (actiony == PC_REMOVE) then
           obj%nwins = obj%nwt * obj%ncoorx * obj%ncoory
           call mem_alloc (obj%win_tim_beg, obj%nwins)
           call mem_alloc (obj%win_tim_end, obj%nwins)
           do iwin   = 1,obj%nwt
           do icoorx = 1,obj%ncoorx
           do icoory = 1,indxy-1
             indx1 = multwin_index (icoorx,icoory,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           do icoory = indxy,obj%ncoory
             indx1 = multwin_index (icoorx,icoory+1,iwin, keep_ncoorx,obj%nwt)
             indx2 = multwin_index (icoorx,icoory  ,iwin,  obj%ncoorx,obj%nwt)
             obj%win_tim_beg(indx2) = keep_win_tim_beg(indx1)
             obj%win_tim_end(indx2) = keep_win_tim_end(indx1)
           end do
           end do
           end do
      end if

!     if (obj%ncoorx /= keep_ncoorx) then

!  call pc_info ('MULTWIN: temporarily cannot insert or remove coordinates')
!  call pc_info (' (pending decision as to whether to use LATWIN-style input)')
!          call mem_alloc (obj%win_coor_x, keep_ncoorx)
!          obj%win_coor_x(1:keep_ncoorx) = keep_win_coor_x(1:keep_ncoorx)
!          obj%ncoorx = keep_ncoorx

!     end if

!     if (obj%ncoory /= keep_ncoory) then

!  call pc_info ('MULTWIN: temporarily cannot insert or remove coordinates')
!  call pc_info (' (pending decision as to whether to use LATWIN-style input)')
!          call mem_alloc (obj%win_coor_y, keep_ncoory)
!          obj%win_coor_y(1:keep_ncoory) = keep_win_coor_y(1:keep_ncoory)
!          obj%ncoory = keep_ncoory

!     end if

!----------check windows for nil values:

500   if (pc_verify_arrayset('COOR_X_ARRAYSET') .or. &
          pc_verify_screen(screen2)) then
!!!!!!!!!!!!!!!!500   if (pc_verify_screen(screen2)) then
           do indx = 1,obj%nwins
                if (obj%win_tim_beg(indx) == FNIL .or. &
                    obj%win_tim_end(indx) == FNIL) then
                      call pc_error &
                            ('MULTWIN: some of the windows have not been set')
                      exit
                end if
           end do
      end if

!----------constrain windows to be within range:

      do indx = 1,obj%nwins
           if (obj%win_tim_beg(indx) /= FNIL) call mth_constrain &
              (obj%win_tim_beg(indx), obj%tstrt, obj%tstop-0.01)
           if (obj%win_tim_end(indx) /= FNIL) call mth_constrain &
              (obj%win_tim_end(indx), obj%win_tim_beg(indx)+0.01, obj%tstop)
      end do

!----------generate gui-only arrays:

      call mem_alloc (obj%coor_x, obj%nwins)
      call mem_alloc (obj%coor_y, obj%nwins)
      call mem_alloc (obj%win   , obj%nwins)

      do icoory = 1,obj%ncoory
      do icoorx = 1,obj%ncoorx
      do iwin   = 1,obj%nwt
           indx = multwin_index (icoorx,icoory,iwin, obj%ncoorx,obj%nwt)
           obj%coor_x(indx) = obj%win_coor_x(icoorx)
           obj%coor_y(indx) = obj%win_coor_y(icoory)
           obj%win   (indx) = iwin
      end do
      end do
      end do

!----------write parameters:

      call pc_put  ('win_hdr_x'   , obj%win_hdr_x               ,  2)
      call pc_put  ('win_hdr_y'   , obj%win_hdr_y               ,  2)
      call pc_put  ('win_coor_x'  , obj%win_coor_x  , obj%ncoorx,  9)
      call pc_put  ('win_coor_y'  , obj%win_coor_y  , obj%ncoory,  9)
      call pc_put  ('win_tim_beg' , obj%win_tim_beg , obj%nwins ,  9)
      call pc_put  ('win_tim_end' , obj%win_tim_end , obj%nwins ,  9)

      call pc_put_gui_only  ('coor_x'  , obj%coor_x  , obj%nwins,  9)
      call pc_put_gui_only  ('coor_y'  , obj%coor_y  , obj%nwins,  9)
      call pc_put_gui_only  ('win'     , obj%win     , obj%nwins    )

      call pc_put_minsize_array     ('win_coor_x'  , 1)
      call pc_put_minsize_array     ('win_coor_y'  , 1)

      call pc_put_minsize_array     ('coor_x'      , obj%nwins)
      call pc_put_minsize_array     ('coor_y'      , obj%nwins)
      call pc_put_minsize_array     ('win'         , obj%nwins)
      call pc_put_minsize_array     ('win_tim_beg' , obj%nwins)
      call pc_put_minsize_array     ('win_tim_end' , obj%nwins)

      call pc_put_maxsize_array     ('coor_x'      , obj%nwins)
      call pc_put_maxsize_array     ('coor_y'      , obj%nwins)
      call pc_put_maxsize_array     ('win'         , obj%nwins)
      call pc_put_maxsize_array     ('win_tim_beg' , obj%nwins)
      call pc_put_maxsize_array     ('win_tim_end' , obj%nwins)

!----------prepare for trace processing.

      call mem_free (obj%windows)

      if (pc_do_not_process_traces()) return

!----------create windows array for interpolation:

      call mem_alloc (obj%windows, 2*obj%nwt, obj%ncoorx, obj%ncoory)

      do icoory = 1,obj%ncoory
      do icoorx = 1,obj%ncoorx
      do iwin   = 1,obj%nwt
        indx  = multwin_index (icoorx,icoory,iwin, obj%ncoorx,obj%nwt)
        indx1 = 2*iwin - 1
        indx2 = 2*iwin
        obj%windows(indx1,icoorx,icoory) = obj%win_tim_beg(indx)
        obj%windows(indx2,icoorx,icoory) = obj%win_tim_end(indx)
      end do
      end do
      end do

!----------print information:

      lun = pc_get_lun()
      write(lun,*) ' '
      call pc_print ('MULTWIN: win_hdr_x =', obj%win_hdr_x  )
      call pc_print ('MULTWIN: win_hdr_y =', obj%win_hdr_y  )
      call pc_print ('MULTWIN: number of windows per trace =', obj%nwt   )
      call pc_print ('MULTWIN: number of X coordinates     =', obj%ncoorx)
      call pc_print ('MULTWIN: number of Y coordinates     =', obj%ncoory)
      write(lun,1000) 'win_coor_x','win_coor_y','win_tim_beg','win_tim_end'
      do icoory = 1,obj%ncoory
      do icoorx = 1,obj%ncoorx
      do iwin   = 1,obj%nwt
        indx1 = 2*iwin - 1
        indx2 = 2*iwin
        write(lun,2000) icoorx,icoory,iwin,               &
                        obj%win_coor_x(icoorx),           &
                        obj%win_coor_y(icoory),           &
                        obj%windows(indx1,icoorx,icoory), &
                        obj%windows(indx2,icoorx,icoory)
      end do
      end do
      end do
1000  format (' MULTWIN: ',3x,1x,3x,1x,3x,1x,a12  ,1x,a12  ,1x,a12  ,1x,a12  )
2000  format (' MULTWIN: ',i3,1x,i3,1x,i3,1x,f12.3,1x,f12.3,1x,f12.3,1x,f12.3)
      write(lun,*) ' '
      return
      end subroutine multwin_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!     subroutine multwin_win_coor_x_element_trap (keyword,indx,action)
!     implicit none
!     character(len=*),intent(in) :: keyword                 ! arguments
!     integer         ,intent(in) :: indx                    ! arguments
!     integer         ,intent(in) :: action                  ! arguments
!     real         :: keep_win_tim_beg(object%nwins)                  ! local
!     real         :: keep_win_tim_end(object%nwins)                  ! local

!     keep_win_tim_beg(1:obj%nwins ) = obj%win_tim_beg (1:obj%nwins )
!     keep_win_tim_end(1:obj%nwins ) = obj%win_tim_end (1:obj%nwins )

!     return
!     end subroutine multwin_win_coor_x_element_trap


!!------------------------ multwin get windows -----------------------------!!
!!------------------------ multwin get windows -----------------------------!!
!!------------------------ multwin get windows -----------------------------!!

!! HD is dimensioned (*) instead of (:) to get around a Portland Group
!! compiler bug (see Portability Issues above).


      subroutine multwin_get_windows (obj,hd,twin)
      implicit none
      type(multwin_struct),intent(in)  :: obj                    ! arguments
      double precision    ,intent(in)  :: hd(*)                  ! arguments
      real                ,intent(out) :: twin(:,:)              ! arguments
      real                             :: xcoord,ycoord          ! local
      real                             :: window(2*obj%nwt)      ! local

      xcoord = hd(obj%win_hdr_x)
      ycoord = hd(obj%win_hdr_y)
      call interp_2d_var_lin_real                                     &
               (obj%win_coor_x,obj%win_coor_y,obj%ncoorx,obj%ncoory,  &
                obj%windows,2*obj%nwt,xcoord,ycoord,window)

      twin(1,1:obj%nwt) = window(1:2*obj%nwt:2)
      twin(2,1:obj%nwt) = window(2:2*obj%nwt:2)
      return
      end subroutine multwin_get_windows


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module multwin_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

