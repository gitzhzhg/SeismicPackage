!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ latwin.f90 --------------------------------!!
!!------------------------------ latwin.f90 --------------------------------!!
!!------------------------------ latwin.f90 --------------------------------!!


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
! Name       : LATWIN 
! Category   : math
! Written    : 2000-01-18   by: Tom Stoeckley
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Manager of laterally-varying trace windows for process modules.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is to be used by process modules which need trace windows
! which vary with lateral location (specified by a pair of coordinates) or
! with mute time.  This primitive owns and manages all parameters needed for
! this functionality, and also manages the interpolation and other calculations
! required to return the appropriate window for a given trace.
!
! This primitive uses the parameter cache to read and write the parameters
! it needs, to report error messages, etc.
!
! To use this primitive from a process module named xxxx:
!
!    (1) latwin_create     should be called from xxxx_create.
!    (2) latwin_initialize should be called from xxxx_initialize.
!    (3) latwin_update     should be called from xxxx_update.
!    (4) latwin_get_window should be called from xxxx.
!    (5) latwin_delete     should be called from xxxx_delete.
!
! The purpose of this primitive is to encapsulate a standard method of
! specifying trace windows for uniformity for the user, to encapsulate
! the code dealing with these trace windows, and to encapsulate the screen
! layout and context-sensitive help so that they need not be repeated in
! the individual processes.  Any changes to this primitive can be made
! without affecting the processes which use it.
!
! Currently this primitive works with only a single laterally-varying
! window.  It is recognized that there is a need to support multiple windows.
! This support will be provided when it is needed either in this primitive
! or in other similar primitives.
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
!                                         opt
!                                    o     i
!           call latwin_create     (obj,no_mute)
!
!                                    b
!           call latwin_initialize (obj)
!
!                                        opt  opt   opt
!                                    b    o    o     o
!           call latwin_update     (obj,nwin,itwin,ibwin)
!
!                                             opt opt   opt   opt    opt
!                                    i  i  i   o   o     o     o      o
!           call latwin_get_window (obj,hd,tr,win,nwin,nlive,index1,index2)
!
!                                    b
!           call latwin_delete     (obj)
!
! type(latwin_struct) obj    = pointer to the window structure.
! logical            no_mute = present and true to disable the MUTE option.
! integer             nwin   = number of trace samples in INCLUSIVE window.
! integer             itwin  = index of  top   of INCLUSIVE window on trace.
! integer             ibwin  = index of bottom of INCLUSIVE window on trace.
! double precision    hd(:)  = trace header word array.
! real                tr(:)  = array of trace samples.
! real             win(NWIN) = array of trace samples within INCLUSIVE window.
! integer             nlive  = number of live trace samples in ACTUAL window.
! integer            index1  = index of  top   of ACTUAL window on trace.
! integer            index2  = index of bottom of ACTUAL window on trace.
!
! The variables ITWIN, IBWIN, and NWIN refer to a large "INCLUSIVE" window
! which is the union of all possible windows.  For option GRID, these refer
! to a large window which is the union of all of the windows over the
! specified grid.  For option MUTE, these are ITWIN = 1, IBWIN = NDPT, and
! NWIN = NDPT since any part of a trace could conceivably be in the window.
!
! The "ACTUAL" trace window for any particular trace will always lie within
! the INCLUSIVE window and will depend on the trace coordinates for option
! GRID and on the trace mute time for option MUTE.  The ACTUAL trace window
! will always exclude muted regions (given by header words 2 and 64) for both
! options.
!
! The LATWIN_GET_WINDOW routine can be used to get WIN (the inclusive window
! with unwanted trace values outside the actual window for this trace set to
! zero), or to get INDEX1 and INDEX2 (the trace indices for the first and
! last trace values in the actual trace window), or both.  The inclusive
! window always has the same starting point ITWIN, ending point IBWIN, and
! length NWIN.  The actual window varies with each trace.  If the actual
! window has zero size, NLIVE, INDEX1, and INDEX2 will be set to zero.
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
!     Date        Author       Description
!     ----        ------       -----------
! 16. 2006-09-11  Stoeckley    Replace pc_register_tab_group w HelpSection line.
! 15. 2006-06-20  Stoeckley    Add pc_register_tab_group to improve SeisSpace
!                               menu.
! 14. 2006-06-05  Stoeckley    Add pc_register_array_names to improve SeisSpace
!                               menu.
! 13. 2005-01-31  Stoeckley    Move some print statements in latwin_update
!                               into an if block where they will not abort.
! 12. 2000-09-27  Stoeckley    Add documentation to the HelpSection for 2D data.
! 11. 2000-08-22  Stoeckley    Make MUTE option fields invisible when disabled.
! 10. 2000-08-01  Stoeckley    Change coordinate parameter names to use X and Y
!                               instead of A and B.
!  9. 2000-05-26  Stoeckley    Add ability to disable the MUTE option.
!  8. 2000-04-25  Stoeckley    Improve trapping logic and screen layout.
!  7. 2000-04-07  Stoeckley    Remove <NS> line in GUI definition section so
!                               that the GUI info will not go to a separate
!                               tabbed screen.
!  6. 2000-04-06  Stoeckley    Improve printouts to identify this primitive.
!  5. 2000-03-31  Stoeckley    Fix GUI definition section.
!  4. 2000-03-09  Stoeckley    Add call to pc_put_sensitive_arrayset_flag.
!  3. 2000-02-15  Stoeckley    Fix bug in setting arrays insensitive.
!  2. 2000-02-04  Stoeckley    Fix bug in arguments for minval and maxval.
!  1. 2000-01-18  Stoeckley    Initial version.
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


!-------------------------------------------------------------------------------
!<gui_def>
!     WIN_OPT=`CCC   WIN_TIM_ADD=`FFFFFF   WIN_TIM_LEN=`FFFFFF
!
!     WIN_HDR_X=`I   WIN_HDR_Y=`I
!
!        WIN_COOR_XWIN_COOR_YWIN_TIM_BEGWIN_TIM_END
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!        `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFF`FFFFFFFFFF
!<PARMS WIN_COOR_X_ARRAYSET[/XST/YST]> 
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!    tabgroup = Window Parameters
!
!<Help KEYWORD="WIN_OPT">
!<Tip> Method to use for specifying trace windows. </Tip>
! Default = MUTE
! Allowed = MUTE   (window defined by time measure from mute)
! Allowed = GRID   (window varies laterally, or same window everywhere)
!
! Segments of the input traces used in this process are defined by these
! windows.
!
! Option GRID specifies the trace windows by linked arrays of window top and
! window bottom values which may vary lateraly (spatially) over arbitrarily
! chosen coordinates.
!
! Option MUTE specifies the trace windows by specifying the time from the
! mute time to the top of the window, and the length of the window.
!
! Some processes might disable the MUTE option if it is not appropriate
! for that process.  In this case, the GRID option will be the default
! and only option.
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_ADD">
!<Tip> Time at top of window = mute time + WIN_TIM_ADD. </Tip>
! Default = 0.0.
! Allowed = real value >= 0.0.
! Used only if WIN_OPT = MUTE.
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_LEN">
!<Tip> Time at bottom of window = time at top of window + WIN_TIM_LEN. </Tip>
! Default = 1.0.
! Allowed = real value >= 0.0.
! Used only if WIN_OPT = MUTE.
!</Help>
!
!
!<Help KEYWORD="WIN_HDR_X">
!<Tip> Header word designating window coordinate X. </Tip>
! Default = 7
! Allowed = any value from 1 through NWIH.
! Used only if WIN_OPT = GRID.
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
! Used only if WIN_OPT = GRID.
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
! Used only if WIN_OPT = GRID.
!
! The (WIN_COOR_X,WIN_COOR_Y) window locations must be on a rectangular grid,
! but not necessarily of regular spacing in either coordinate.  Therefore the
! total number of window locations must be the product of the number of
! separate WIN_COOR_X and WIN_COOR_Y locations.
!
! If only one X coordinate is specified, its value is irrelevant.
!</Help>
!
!
!<Help KEYWORD="WIN_COOR_Y">
!<Tip> Array of coord Y values for specifying spatially varying windows. </Tip>
! Default = 0.0 (one location).
! Allowed = up to any number of locations specified by WIN_HDR_Y values.
! Used only if WIN_OPT = GRID.
!
! The (WIN_COOR_X,WIN_COOR_Y) window locations must be on a rectangular grid,
! but not necessarily of regular spacing in either coordinate.  Therefore the
! total number of window locations must be the product of the number of
! separate WIN_COOR_X and WIN_COOR_Y locations.
!
! If only one Y coordinate is specified, its value is irrelevant.
! For 2D data, only one Y coordinate should be specified.
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_BEG">
!<Tip> Array of window top times (in seconds). </Tip>
! Default = top of trace (TSTRT) (one window).
! Allowed = any number of values >= top of trace.
! Used only if WIN_OPT = GRID.
!
! This window is spatially varying as given at the (WIN_COOR_X,WIN_COOR_Y)
! locations.  The window is interpolated between the specified coordinates,
! and extrapolated unchanged outside of the grid of coordinates.
!</Help>
!
!
!<Help KEYWORD="WIN_TIM_END">
!<Tip> Array of window bottom times (in seconds). </Tip>
! Default = bottom of trace (TSTRT+(NDPT-1)*DT) (one window).
! Allowed = any number of values > WIN_TIM_BEG and <= bottom of trace.
! Used only if WIN_OPT = GRID.
!
! This window is spatially varying as given at the (WIN_COOR_X,WIN_COOR_Y)
! locations.  The window is interpolated between the specified coordinates,
! and extrapolated unchanged outside of the grid of coordinates.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module latwin_module
      use named_constants_module
      use pc_module
      use gridcheck_module
      use interp_module
      use mth_module
      implicit none
      public

      character(len=100),public,save :: LATWIN_IDENT = &
       '$Id: latwin.f90,v 1.16 2006/09/11 13:15:46 Stoeckley prod sps $'


      type,public :: latwin_struct              

       private
       character(len=4) :: win_opt                       ! process parameters
       real             :: win_tim_add,win_tim_len       ! process parameters
       integer          :: win_hdr_x,win_hdr_y,nwins     ! process parameters
       real   ,pointer  :: win_coor_x(:),win_coor_y(:)   ! process parameters
       real   ,pointer  :: win_tim_beg(:),win_tim_end(:) ! process parameters
       integer          :: nxcoords,nycoords             ! dependent parameters
       integer          :: itwin,ibwin,nwin              ! dependent parameters
       integer          :: iadd,ilen                     ! dependent parameters
       real   ,pointer  :: xcoords(:),ycoords(:)         ! dependent parameters
       integer,pointer  :: iwindows(:,:,:)               ! dependent parameters
       logical          :: no_mute                       ! dependent parameters

      end type latwin_struct

      integer,parameter,private     :: noptions = 2
      character(len=4),private,save :: win_opt_options(noptions)

      data win_opt_options /'MUTE','GRID'/

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine latwin_create (obj,no_mute)
      implicit none
      type(latwin_struct),pointer :: obj       ! arguments
      logical,optional,intent(in) :: no_mute   ! arguments

      allocate (obj)

      if (present(no_mute)) then
           obj%no_mute = no_mute
      else
           obj%no_mute = .false.
      end if

      nullify (obj%win_coor_x)        ! process parameter
      nullify (obj%win_coor_y)        ! process parameter
      nullify (obj%win_tim_beg)       ! process parameter
      nullify (obj%win_tim_end)       ! process parameter

      nullify (obj%xcoords)
      nullify (obj%ycoords)
      nullify (obj%iwindows)
      return
      end subroutine latwin_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine latwin_delete (obj)
      implicit none
      type(latwin_struct),pointer :: obj       ! arguments

      if (associated(obj%win_coor_x )) deallocate (obj%win_coor_x )
      if (associated(obj%win_coor_y )) deallocate (obj%win_coor_y )
      if (associated(obj%win_tim_beg)) deallocate (obj%win_tim_beg)
      if (associated(obj%win_tim_end)) deallocate (obj%win_tim_end)
      if (associated(obj%xcoords    )) deallocate (obj%xcoords    )
      if (associated(obj%ycoords    )) deallocate (obj%ycoords    )
      if (associated(obj%iwindows   )) deallocate (obj%iwindows   )

      deallocate(obj)
      return
      end subroutine latwin_delete


!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!
!!--------------------------- initialize ----------------------------------!!


      subroutine latwin_initialize (obj)
      implicit none
      type(latwin_struct)  :: obj                 ! arguments
      integer              :: ndpt                ! local
      real                 :: tstrt,dt            ! local

      call pc_get_global ('ndpt' , ndpt )
      call pc_get_global ('tstrt', tstrt)
      call pc_get_global ('dt'   , dt   )

      obj%win_opt   = 'MUTE'
      obj%win_hdr_x = 7
      obj%win_hdr_y = 8

      if (associated(obj%win_coor_x )) deallocate (obj%win_coor_x )
      if (associated(obj%win_coor_y )) deallocate (obj%win_coor_y )
      if (associated(obj%win_tim_beg)) deallocate (obj%win_tim_beg)
      if (associated(obj%win_tim_end)) deallocate (obj%win_tim_end)

      allocate (obj%win_coor_x (1))
      allocate (obj%win_coor_y (1))
      allocate (obj%win_tim_beg(1))
      allocate (obj%win_tim_end(1))

      obj%nwins          = 1
      obj%win_coor_x (1) = 0.0
      obj%win_coor_y (1) = 0.0
      obj%win_tim_beg(1) = tstrt
      obj%win_tim_end(1) = tstrt+(ndpt-1)*dt
      obj%win_tim_add    = 0.0
      obj%win_tim_len    = 1.0
      return
      end subroutine latwin_initialize


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine latwin_update (obj,nwin,itwin,ibwin)
      implicit none
      type(latwin_struct) ,intent(inout) :: obj                    ! arguments
      integer,optional    ,intent(out)   :: nwin,itwin,ibwin       ! arguments
      integer           :: nwih,ndpt,indx,lun,ix,iy                ! local
      real              :: tstrt,dt,tstop,twin,bwin                ! local
      integer           :: nwins1,nwins2,nwins3,nwins4             ! local
      character(len=80) :: msg                                     ! local

      call pc_register_array_names ("win_coor_x_arrayset", (/ "win_coor_x ", &
                                                              "win_coor_y ", &
                                                              "win_tim_beg", &
                                                              "win_tim_end" /))

!----------read parameters:

      nwins1 = obj%nwins
      nwins2 = obj%nwins
      nwins3 = obj%nwins
      nwins4 = obj%nwins

      call pc_get_global ('nwih'        , nwih                   )
      call pc_get_global ('ndpt'        , ndpt                   )
      call pc_get_global ('tstrt'       , tstrt                  )
      call pc_get_global ('dt'          , dt                     )
      call pc_get        ('win_opt'     , obj%win_opt            )
      call pc_get        ('win_tim_add' , obj%win_tim_add        )
      call pc_get        ('win_tim_len' , obj%win_tim_len        )
      call pc_get        ('win_hdr_x'   , obj%win_hdr_x          )
      call pc_get        ('win_hdr_y'   , obj%win_hdr_y          )
      call pc_alloc      ('win_coor_x'  , obj%win_coor_x  ,nwins1)
      call pc_alloc      ('win_coor_y'  , obj%win_coor_y  ,nwins2)
      call pc_alloc      ('win_tim_beg' , obj%win_tim_beg ,nwins3)
      call pc_alloc      ('win_tim_end' , obj%win_tim_end ,nwins4)

      if (nwins2 /= nwins1 .or. nwins3 /= nwins1 .or. nwins4 /= nwins1) then
           call pc_error ('WIN_COOR_X,WIN_COOR_Y,WIN_TIM_BEG,WIN_TIM_END'// &
                           ' arrays have different lengths')
           obj%nwins = min(nwins1,nwins2,nwins3,nwins4)
      else
           obj%nwins = nwins1
      end if

!----------verify parameters:

      if (dt <= 0.0 .or. nwih < HDR_NOMINAL_SIZE .or. ndpt <= 1) then
           call pc_error ('Global DT or NWIH or NDPT not set')
      end if

      if (obj%no_mute) obj%win_opt = 'GRID'

      if (obj%win_opt /= 'MUTE' .and. obj%win_opt /= 'GRID') then
        call pc_error ('Invalid WIN_OPT value '//obj%win_opt)
      end if

      if (pc_verify_scalar('win_hdr_x') .or. &
          pc_verify_scalar('win_hdr_y')) then
      if (obj%win_opt == 'GRID') then
        if (obj%win_hdr_x <= 0 .or. obj%win_hdr_x > nwih) then
           call pc_error ('Invalid WIN_HDR_X value ', obj%win_hdr_x)
        end if
        if (obj%win_hdr_y <= 0 .or. obj%win_hdr_y > nwih) then
           call pc_error ('Invalid WIN_HDR_Y value ', obj%win_hdr_y)
        end if
        if (obj%win_hdr_x == obj%win_hdr_y) then
           call pc_error ('WIN_HDR_X and WIN_HDR_Y values cannot be the same')
        end if
      end if
      end if

      TSTOP = TSTRT + (NDPT-1)*DT
      call mth_constrain (obj%win_tim_add, 0.0 , tstop      -0.01)
      call mth_constrain (obj%win_tim_len, 0.01, tstop-tstrt-0.01)

      do indx = 1,obj%nwins
           if (obj%win_coor_x (indx) == FNIL) then
                if (indx == 1) obj%win_coor_x (indx) = 0.0
                if (indx >  1) obj%win_coor_x (indx) = obj%win_coor_x (indx-1)
           end if
           if (obj%win_coor_y (indx) == FNIL) then
                if (indx == 1) obj%win_coor_y (indx) = 0.0
                if (indx >  1) obj%win_coor_y (indx) = obj%win_coor_y (indx-1)
           end if
           if (obj%win_tim_end(indx) == FNIL) obj%win_tim_end(indx) = tstop
           if (obj%win_tim_beg(indx) == FNIL) obj%win_tim_beg(indx) = tstrt
           if (obj%win_tim_end(indx) == FNIL) obj%win_tim_end(indx) = tstop
           CALL MTH_CONSTRAIN &
                  (obj%win_tim_beg(indx),     TSTRT            , TSTOP-0.01)
           CALL MTH_CONSTRAIN &
                  (obj%win_tim_end(indx), obj%win_tim_beg(indx)+0.01, TSTOP)
      end do

      if (obj%nwins == 0) then
           obj%nwins = 1
           obj%win_tim_beg(1) = tstrt
           obj%win_tim_end(1) = tstop
           obj%win_coor_x(1) = 0.0
           obj%win_coor_y(1) = 0.0
      end if

      call gridcheck (obj%nwins,obj%win_coor_x,obj%win_coor_y,   &
                             obj%nxcoords,obj%nycoords,msg)
      if (msg(1:1) /= ' ') then
           obj%nxcoords = 0
           obj%nycoords = 0
           if (obj%win_opt == 'GRID' .and. &
               pc_verify_arrayset('win_coor_x_arrayset')) call pc_error (msg)
      end if

!----------calculate dependencies:

      if (obj%win_opt == 'MUTE') then
           TWIN = TSTRT
           BWIN = TSTOP
      else if (obj%nwins > 0) then
           TWIN = minval (obj%win_tim_beg(1:obj%nwins))
           BWIN = maxval (obj%win_tim_end(1:obj%nwins))
      else
           TWIN = TSTRT
           BWIN = TSTOP
      end if

      if (dt <= 0.0) dt = 0.004    ! to keep from aborting if DT is not set.

      obj%itwin   = nint((twin-tstrt)/dt + 1.0)
      obj%ibwin   = nint((bwin-tstrt)/dt + 1.0)
      obj%nwin    = obj%ibwin - obj%itwin + 1
      obj%iadd    = nint(obj%win_tim_add/dt)
      obj%ilen    = nint(obj%win_tim_len/dt)

      if (present(itwin)) itwin = obj%itwin
      if (present(ibwin)) ibwin = obj%ibwin
      if (present(nwin )) nwin  = obj%nwin

      if (obj%itwin < 1 .or. obj%ibwin > ndpt) then
          call pc_error ('WINDOW (WIN_TIM_BEG and/or WIN_TIM_END) OUT OF RANGE')
      end if

      if (obj%nwin < 5) then
           call pc_error ('WINDOW SIZE (WIN_TIM_END-WIN_TIM_BEG) TOO SMALL')
      end if

!----------write parameters:

      call pc_put_options_field ('win_opt',win_opt_options, noptions)

      call pc_put  ('win_opt'     , obj%win_opt                    )
      call pc_put  ('win_tim_add' , obj%win_tim_add            ,  7)
      call pc_put  ('win_tim_len' , obj%win_tim_len            ,  7)
      call pc_put  ('win_hdr_x'   , obj%win_hdr_x              ,  2)
      call pc_put  ('win_hdr_y'   , obj%win_hdr_y              ,  2)
      call pc_put  ('win_coor_x'  , obj%win_coor_x  , obj%nwins, 10)
      call pc_put  ('win_coor_y'  , obj%win_coor_y  , obj%nwins, 10)
      call pc_put  ('win_tim_beg' , obj%win_tim_beg , obj%nwins, 11)
      call pc_put  ('win_tim_end' , obj%win_tim_end , obj%nwins, 11)

      call pc_put_sensitive_field_flag ('win_opt'     , .not.obj%no_mute)
      call pc_put_sensitive_field_flag ('win_tim_add' , obj%win_opt == 'MUTE')
      call pc_put_sensitive_field_flag ('win_tim_len' , obj%win_opt == 'MUTE')
      call pc_put_sensitive_field_flag ('win_hdr_x'   , obj%win_opt == 'GRID')
      call pc_put_sensitive_field_flag ('win_hdr_y'   , obj%win_opt == 'GRID')
      call pc_put_sensitive_array_flag ('win_coor_x'  , obj%win_opt == 'GRID')
      call pc_put_sensitive_array_flag ('win_coor_y'  , obj%win_opt == 'GRID')
      call pc_put_sensitive_array_flag ('win_tim_beg' , obj%win_opt == 'GRID')
      call pc_put_sensitive_array_flag ('win_tim_end' , obj%win_opt == 'GRID')
      call pc_put_sensitive_arrayset_flag  &
                               ('win_coor_x_arrayset' , obj%win_opt == 'GRID')

      call pc_put_visible_flag         ('win_opt'     , .not.obj%no_mute)
      call pc_put_visible_flag         ('win_tim_add' , .not.obj%no_mute)
      call pc_put_visible_flag         ('win_tim_len' , .not.obj%no_mute)

!----------create arrays for interpolating trace window top and bottom:

      if (associated(obj%xcoords )) deallocate (obj%xcoords )
      if (associated(obj%ycoords )) deallocate (obj%ycoords )
      if (associated(obj%iwindows)) deallocate (obj%iwindows)

      if (pc_do_not_process_traces()) return

      lun = pc_get_lun()
      write(lun,*) ' '
      call pc_print  ('LATWIN: win_opt     =' , obj%win_opt    )
      call pc_print  ('LATWIN: win_tim_add =' , obj%win_tim_add)
      call pc_print  ('LATWIN: win_tim_len =' , obj%win_tim_len)
      call pc_print  ('LATWIN: win_hdr_x   =' , obj%win_hdr_x  )
      call pc_print  ('LATWIN: win_hdr_y   =' , obj%win_hdr_y  )
      if (obj%win_opt == 'GRID' .and.  &
          obj%nxcoords > 0 .and. obj%nycoords > 0) then
           allocate (obj%xcoords    (obj%nxcoords))
           allocate (obj%ycoords    (obj%nycoords))
           allocate (obj%iwindows (2,obj%nxcoords,obj%nycoords))
           obj%xcoords = obj%win_coor_x(1:obj%nxcoords)
           obj%ycoords = obj%win_coor_y(1:obj%nwins:obj%nxcoords)
           write(lun,1000) 'win_coor_x','win_coor_y','win_tim_beg','win_tim_end'
           do ix = 1,obj%nxcoords
           do iy = 1,obj%nycoords
             indx = ix + (iy - 1)*obj%nxcoords
             obj%iwindows(1,ix,iy)=nint((obj%win_tim_beg(indx)-tstrt)/dt+1.0)
             obj%iwindows(2,ix,iy)=nint((obj%win_tim_end(indx)-tstrt)/dt+1.0)
             write(lun,2000) ix,iy,obj%xcoords(ix),obj%ycoords(iy),  &
                                   obj%win_tim_beg(indx),obj%win_tim_end(indx)
1000         format (' LATWIN: ',3x,1x,3x,1x,a12  ,1x,a12  ,1x,a12  ,1x,a12  )
2000         format (' LATWIN: ',i3,1x,i3,1x,f12.2,1x,f12.2,1x,f12.3,1x,f12.3)
           end do
           end do
           write(lun,*) 'LATWIN: xcoords   = ',obj%xcoords
           write(lun,*) 'LATWIN: ycoords   = ',obj%ycoords
           write(lun,*) 'LATWIN: iwindows  = ',obj%iwindows
      end if
      write(lun,*) 'LATWIN: nxcoords,nycoords = ',obj%nxcoords,obj%nycoords
      write(lun,*) 'LATWIN: itwin,ibwin,nwin  = ',obj%itwin,obj%ibwin,obj%nwin
      write(lun,*) 'LATWIN: iadd,ilen = ',obj%iadd,obj%ilen             
      write(lun,*) ' '
      return
      end subroutine latwin_update


!!------------------------ latwin get window -----------------------------!!
!!------------------------ latwin get window -----------------------------!!
!!------------------------ latwin get window -----------------------------!!


      subroutine latwin_get_window (obj,hd,tr,win,nwin,nlive,index1,index2)
      implicit none
      type(latwin_struct) ,intent(in)  :: obj                    ! arguments
      double precision    ,intent(in)  :: hd(:)                  ! arguments
      real                ,intent(in)  :: tr(:)                  ! arguments
      real   ,optional    ,intent(out) :: win(:)                 ! arguments
      integer,optional    ,intent(out) :: nwin,nlive             ! arguments
      integer,optional    ,intent(out) :: index1,index2          ! arguments
      integer                          :: itop,ibottom,ioffset   ! local
      real                             :: xcoord,ycoord          ! local
      integer                          :: iwindow(2),j           ! local

!----------get window indices from mute time:

      if (obj%win_opt == 'MUTE') then
         itop    = hd(2) + obj%iadd
         ibottom = hd(2) + obj%iadd + obj%ilen

!----------get window indices from laterally varying windows:

      else if (obj%nxcoords > 1 .or. obj%nycoords > 1) then
         xcoord = hd(obj%win_hdr_x)
         ycoord = hd(obj%win_hdr_y)
         call interp_2d_var_lin_int                                    &
                  (obj%xcoords,obj%ycoords,obj%Nxcoords,obj%Nycoords,  &
                   obj%iwindows,2,xcoord,ycoord,iwindow)
         itop    = iwindow(1)
         ibottom = iwindow(2)

!----------get default window indices:

      else
         itop    = obj%itwin
         ibottom = obj%ibwin
      end if

!----------put window indices within range for this trace:

      itop    = max(obj%itwin,nint(hd( 2)),itop   )
      ibottom = min(obj%ibwin,nint(hd(64)),ibottom)

      if (itop > ibottom) then
           itop    = 0
           ibottom = 0
      end if

!----------return optional arguments:

      if (present(win)) then
           win(1:obj%nwin) = 0.0
           if(itop > 0) then
                ioffset = obj%itwin - 1
                win(itop-ioffset:ibottom-ioffset) = tr(itop:ibottom)
           end if
      end if

      if (present(nlive)) then
           nlive = 0
           if (itop > 0) then
                do j = itop,ibottom
                     if (tr(j) /= 0.0) nlive = nlive + 1
                end do
           end if
      end if

      if (present(nwin  )) nwin   = obj%nwin
      if (present(index1)) index1 = itop   
      if (present(index2)) index2 = ibottom
      return
      end subroutine latwin_get_window


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module latwin_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

