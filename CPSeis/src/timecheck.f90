
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- timecheck.f90 --------------------------------!!
!!---------------------------- timecheck.f90 --------------------------------!!
!!---------------------------- timecheck.f90 --------------------------------!!


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
! Name       : TIMECHECK
! Category   : math
! Written    : 2000-03-09   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-19
! Purpose    : Facilitate checking times and indices on a seismic trace.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This is a convenience routine to facilitate checking and (and updating
! if necessary) the beginning and ending times of a segment of a seismic
! trace, and returning the corresponding trace indices.
!
! This primitive is intended to be used by process modules to check and
! adjust a process parameter specifying a time which must correspond to a
! valid index on a seismic trace, or a pair of process parameters specifying
! the beginning and ending times of a segment of a seismic trace.
!
! The purpose of this primitive is to consolidate often-used code, to allow
! the calculations to be done in a uniform manner, and to provide a standard
! behavior in the user interface.
!
! This primitive accesses the the PC primitive to get global values TSTRT,
! DT, and NDPT, and to generate informational messages when appropriate.
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
!                           CALLING SEQUENCE
!
!                                           opt
!                       i      b      o      i
!    CALL TIMECHECK (KEYWORD, TIME, ITIME, SCREEN)
!
!    CALL TIMECHECK
!            (KEY_BEG, KEY_END, TIM_BEG, TIM_END, ITIM_BEG, ITIM_END, SCREEN)
!                i        i        b        b         o         o       i
!                                                                      opt
!
! char(len=*) KEYWORD  = keyword of a time on a seismic trace.
! char(len=*) KEY_BEG  = keyword of a beginning time on a seismic trace.
! char(len=*) KEY_END  = keyword of an ending time on a seismic trace.
! real        TIME     = process parameter corresponding to KEYWORD.
! real        TIM_BEG  = process parameter corresponding to KEY_BEG.
! real        TIM_END  = process parameter corresponding to KEY_END.
! integer     ITIME    = trace index corresponging to TIME.
! integer     ITIM_BEG = trace index corresponging to TIM_BEG.
! integer     ITIM_END = trace index corresponging to TIM_END.
! char(len=*) SCREEN   = keyword of the screen (if there are multiple screens).
!
! These routines adjust the time to be a multiple of the trace sample interval.
!
! These routines also adjust the time to fall within the range of the trace
! if necessary, and provide an informational message when that happens.
!
! The second routine also makes sure that TIM_END >= TIM_BEG and provides an
! informational message if an adjustment is made to TIM_END.
!
! The second routine also generates an informational message if TIM_BEG and
! TIM_END are the same, indicating that the trace segment contains only one
! trace sample.
!
! If TIME    is FNIL upon input, it is set to the starting time on the trace.
! If TIM_BEG is FNIL upon input, it is set to the starting time on the trace.
! If TIM_END is FNIL upon input, it is set to the ending time on the trace.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                     HOW TO USE THIS PRIMITIVE
!
! Process modules should take the following (example) steps to use this
! primitive effectively.  Both a single time parameter, and a pair of
! beginning and ending time parameters, are shown.  A process may need only
! a single parameter or a pair.
!
! (1) In the process module data structure:
!
!         real    ::  time            ! process parameter
!         real    ::  tim_beg         ! process parameter
!         real    ::  tim_end         ! process parameter
!         integer ::  itime           ! dependent variable
!         integer ::  itim_beg        ! dependent variable
!         integer ::  itim_end        ! dependent variable
!
! (2) In the process module initialize subroutine:
!
!         obj%time    = FNIL
!         obj%tim_beg = FNIL
!         obj%tim_end = FNIL
!
! (3) In the process module update subroutine:
!
!         call pc_get    ('time'   , obj%time)
!         call pc_get    ('tim_beg', obj%tim_beg)
!         call pc_get    ('tim_end', obj%tim_end)
!
!         call timecheck ('time', obj%time, obj%itime, 'screen1')
!
!         call timecheck ('tim_beg', 'tim_end', obj%tim_beg, obj%tim_end,  &
!                          obj%itim_beg, obj%itim_end, 'screen1')
!
!         call pc_put    ('time'   , obj%time)
!         call pc_put    ('tim_beg', obj%tim_beg)
!         call pc_put    ('tim_end', obj%tim_end)
!
! If the TIMECHECK subroutine is called from inside traps:
!   (a) It should be called from a scalar trap with the SCREEN argument absent.
!   (b) If there are multiple screens, it should be called from a screen trap
!         with the SCREEN argument present.
!
! If the TIMECHECK subroutine is called from outside any trap:
!   (a) If there are multiple screens, the SCREEN argument should be present.
!
! Messages are reported at the following times:
!   (a) For GUI updates, messages will not be reported unless the user
!         changed a time or the user is leaving the screen.
!   (b) For initial and final frontend updates (i.e. when the dialog box is
!         popped up or down, or a workfile is being updated), messages
!         are always reported.
!   (b) For backend updates, messages are always reported.
!
!                           +++++++++++++++++
!
! It would be a simple matter to include the calls to PC_GET and PC_PUT in
! the TIMECHECK subroutines, but it has been decided not to do so because this
! would obscure the fact that the times are being obtained and reported
! like all other process parameters.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2000-10-19  Stoeckley  Add missing required documentation section.
!  1. 2000-03-09  Stoeckley  Initial version.
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


      module timecheck_module
      use pc_module
      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: TIMECHECK_IDENT = &
       '$Id: timecheck.f90,v 1.2 2000/10/19 17:33:13 sps prod sps $'

      private :: timecheck_private

      interface timecheck
          module procedure timecheck_one
          module procedure timecheck_two
      end interface

      contains


!!-------------------------- timecheck private ------------------------------!!
!!-------------------------- timecheck private ------------------------------!!
!!-------------------------- timecheck private ------------------------------!!

! START = true means replace FNIL with TSTRT.
! START = false means replace FNIL with TEND.


      subroutine timecheck_private (keyword,time,itime,screen,start,doit)
      implicit none
      character(len=*),intent(in)           :: keyword          ! arguments
      real            ,intent(inout)        :: time             ! arguments
      integer         ,intent(out)          :: itime            ! arguments
      character(len=*),intent(in) ,optional :: screen           ! arguments
      logical         ,intent(in)           :: start            ! arguments
      logical         ,intent(out)          :: doit             ! arguments
      integer                               :: ndpt             ! local
      real                                  :: tstrt,dt,tend    ! local
      logical                               :: out_of_range     ! local

!!!!!!!!!! get globals:

      ndpt  = 0
      tstrt = 0.0
      dt    = 0.0

      call pc_get_global ('NDPT'  , ndpt)
      call pc_get_global ('TSTRT' , tstrt)
      call pc_get_global ('DT'    , dt)

      if (ndpt <= 0) then
           call pc_error ('NDPT is <= 0 or not in the parameter cache')
           ndpt = 1
      end if

      if (dt <= 0.0) then
           call pc_error ('DT is <= 0 or not in the parameter cache')
           dt = 0.004
      end if

      tend = tstrt + (ndpt-1) * dt

!!!!!!!!!! preset parameter if nil:

      if (time == FNIL) then
           if (start) then
                time = tstrt
           else
                time = tend
           end if
      end if

!!!!!!!!!! calculate index and adjust parameter:

      itime = 1 + nint((time - tstrt) / dt)

      if (itime < 1) then
           out_of_range = .true.
           itime = 1
      else if (itime > ndpt) then
           out_of_range = .true.
           itime = ndpt
      else
           out_of_range = .false.
      end if

      time = tstrt + (itime - 1) * dt

!!!!!!!!!! decide whether messages need reporting:

      doit = pc_verify_scalar (keyword)

      if (present(screen)) then
           if (pc_verify_screen(screen)) then
                 doit = .true.
           end if
      end if

!!!!!!!!!! report messages:

      if (doit .and. out_of_range) then
         call pc_info (trim(keyword)//' reset to be between',tstrt,'and',  &
                        tend,'inclusively.')
      end if
      return
      end subroutine timecheck_private


!!--------------------------- timecheck one ------------------------------!!
!!--------------------------- timecheck one ------------------------------!!
!!--------------------------- timecheck one ------------------------------!!


      subroutine timecheck_one (keyword,time,itime,screen)
      implicit none
      character(len=*),intent(in)           :: keyword          ! arguments
      real            ,intent(inout)        :: time             ! arguments
      integer         ,intent(out)          :: itime            ! arguments
      character(len=*),intent(in) ,optional :: screen           ! arguments
      logical                               :: doit             ! local

      call timecheck_private (keyword,time,itime,screen, .true., doit)
      return
      end subroutine timecheck_one


!!--------------------------- timecheck two ------------------------------!!
!!--------------------------- timecheck two ------------------------------!!
!!--------------------------- timecheck two ------------------------------!!


      subroutine timecheck_two  &
            (key_beg, key_end, tim_beg, tim_end, itim_beg, itim_end, screen)
      implicit none
      character(len=*),intent(in)           :: key_beg          ! arguments
      character(len=*),intent(in)           :: key_end          ! arguments
      real            ,intent(inout)        :: tim_beg          ! arguments
      real            ,intent(inout)        :: tim_end          ! arguments
      integer         ,intent(out)          :: itim_beg         ! arguments
      integer         ,intent(out)          :: itim_end         ! arguments
      character(len=*),intent(in) ,optional :: screen           ! arguments
      logical                               :: doit,doit1,doit2 ! local

      call timecheck_private (key_beg,tim_beg,itim_beg,screen,.true. ,doit1)
      call timecheck_private (key_end,tim_end,itim_end,screen,.false.,doit2)

      doit = doit1 .or. doit2

      if (itim_end == itim_beg) then
           if (doit) call pc_info (key_end,'matches',key_beg,'.')
      else if (itim_end < itim_beg) then
           tim_end  = tim_beg
           itim_end = itim_beg
           if (doit) call pc_info  &
                           (key_end,'too small - reset to match',key_beg,'.')
      end if

      if (itim_end == itim_beg .and. doit) then
           call pc_info ('the range',key_beg,'to',key_end,  &
                         'defines only one trace sample point.')
      end if
      return
      end subroutine timecheck_two


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module timecheck_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

