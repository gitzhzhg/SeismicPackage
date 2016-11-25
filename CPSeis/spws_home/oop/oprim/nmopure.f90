!<CPS_v1 type="PRIMITIVE"/>
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



   ! other files are:  nmopure.f90  nmopure_wrapper.cc  nmopure_wrapper.hh


!<brief_doc>
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : NMOPURE        (pure normal moveout primitive)
! Category   : velocity
! Written    : 2003-01-14   by: Tom Stoeckley
! Revised    : 2003-01-14   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To apply or remove NMO corrections to seismic data.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive is equivalent to a subset of the NMOPRIM primitive, which
! contains all of the normal moveout code used by the NMO process.  Whereas
! the NMOPRIM primitive is intended to be called from the NMO process, this
! primitive is to be called from the VA program, which does not need some
! of the features of the NMO process.  In particular, VA does not need the
! velocity function I/O and interpolation capabilities, or the demult
! capabilities.
!
! NMOPURE has several modes of operation: perform normal moveout correction
! on seismic data, restore normal moveout to seismic data, perform partial
! moveout, and put velocity values into output traces.
!
!------------------------------------------------------------------------------
!                         MAKEUP OF THIS PRIMITIVE
!
! This primitive is broken down into several lower-level primitives to
! make the various parts of the code more easily reusable and to simplify
! maintenance.
!
! This primitive uses the TIMEZERO primitive to shift the trace to and from
! zero time if necessary.  The TIMEZERO primitive uses the TIMESEL primitive
! to do the actual shifts.
!
! This primitive uses the MOVEOUT primitive to perform the actual normal
! moveout and to mute the trace.  The MOVEOUT primitive uses the DYNSAMP
! primitive to dynamically resample traces using FFT or linear or cubic
! interpolation.  The DYNSAMP primitive uses the DENSIFY primitive which
! uses the FFT primitive for the FFT resampling.
!
! This primitive uses the TERPUTIL primitive for linear velocity function
! resampling.

!------------------------------------------------------------------------------
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
!                             SUBROUTINES
!
!                           o   i     i   i    i      i      i   
!     call nmopure_create (obj,ndpt,tstrt,dt,action,order,terpmode,
!                          doppler,tracemute,error,msg)
!                             i        i       o    o
!
!                           b     i       i       i    
!     call nmopure_velfun (obj,npicks2,tpicks2,vpicks2,
!                              npicks4,tpicks4,vpicks4)
!                                 i       i       i    
!
!                           b    b      b    b   b      b
!     call nmopure_apply  (obj,offset,offnew,tr,mtop,mbottom)
!
!                           b
!     call nmopure_delete (obj)
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(nmopure_struct)   obj = pointer to the NMOPURE object.
! integer               ndpt = number of trace values.
! real                 tstrt = trace starting time (seconds).
! real                    dt = trace sample interval (seconds).
! integer             action = normal moveout action to perform.
! integer              order = normal moveout order (2 or 4).
! integer           terpmode = trace interpolation option to use.
! real               doppler = doppler stretch factor.
! logical          tracemute = whether to mute the trace.
! logical              error = error flag (true if an error occurred).
! character(len=*)       msg = message for possible printing.
!
! integer            npicks2 = number of 2nd order time/velocity picks.
! real      tpicks2(npicks2) = 2nd order time picks.
! real      vpicks2(npicks2) = 2nd order velocity picks.
!
! integer            npicks4 = number of 4th order (or eta) time/velocity picks.
! real      tpicks2(npicks4) = 4th order (or eta) time picks.
! real      vpicks2(npicks4) = 4th order (or eta) velocity picks.
!
! real                offset = offset of trace (changed if doing partial NMO).
! real                offnew = new offset of trace (if doing partial NMO).
! real              tr(ndpt) = trace values.
! integer               mtop = head mute index of trace (used and changed).
! integer            mbottom = tail mute index of trace (used and changed).
!
!-------------------------------------------------------------------------------
!                    SUBROUTINE ARGUMENT DETAILS
!
! ACTION = NMOPURE_FORWARD means apply forward NMO correction.
! ACTION = NMOPURE_REVERSE means apply reverse NMO correction.
! ACTION = NMOPURE_PARTIAL means apply partial NMO correction.
! ACTION = NMOPURE_VNMO    means put NMO (stacking) velocities into TR.
! ACTION = NMOPURE_VINT    means put Dix interval velocities into TR.
!
! OFFSET and OFFNEW are swapped when doing partial NMO.
! Otherwise, OFFSET is not changed and OFFNEW is not used.
!
! ORDER = NMOPURE_2           =  normal    2nd order   hyperbolic moveout.
! ORDER = NMOPURE_4           = residual   4th order   non-hyperbolic moveout.
! ORDER = NMOPURE_ETA4        = residual 4th order eta non-hyperbolic moveout.
! ORDER = NMOPURE_ETA         = residual  "exact" eta  non-hyperbolic moveout.
! ORDER = NMOPURE_2_PLUS_4    = 2nd order plus   4th order   moveout combined.
! ORDER = NMOPURE_2_PLUS_ETA4 = 2nd order plus 4th order eta moveout combined.
! ORDER = NMOPURE_2_PLUS_ETA  = 2nd order plus  "exact" eta  moveout combined.
!
! TERPMODE = NMOPURE_LINEAR means 2-point linear interpolation.
! TERPMODE = NMOPURE_CUBIC  means 4-point cubic interpolation.
! TERPMODE = NMOPURE_FFT2   means FFT densification plus linear interpolation.
! TERPMODE = NMOPURE_FFT4   means FFT densification plus linear interpolation.
! TERPMODE = NMOPURE_FFT8   means FFT densification plus linear interpolation.
!
! See the MOVEOUT primitive for details about the DOPPLER, TRACEMUTE, MTOP,
! and MBOTTOM parameters.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!  1. 2003-01-14 Stoeckley  Initial version made from a subset of the NMOPRIM
!                            primitive for use by the VA program.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module nmopure_module
      use timezero_module
      use moveout_module
      use terputil_module
      implicit none
      public


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: nmopure_struct
      private

        type(timezero_struct),pointer :: timezero
        type(moveout_struct) ,pointer :: moveout  
        real                          :: dt
        integer                       :: nfine

      end type nmopure_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer,parameter,public :: NMOPURE_FORWARD = MOVEOUT_FORWARD  ! action
      integer,parameter,public :: NMOPURE_REVERSE = MOVEOUT_REVERSE  ! action
      integer,parameter,public :: NMOPURE_PARTIAL = MOVEOUT_PARTIAL  ! action
      integer,parameter,public :: NMOPURE_VNMO    = MOVEOUT_VNMO     ! action
      integer,parameter,public :: NMOPURE_VINT    = MOVEOUT_VINT     ! action

      integer,parameter,public :: NMOPRIM_2           = MOVEOUT_2           ! order.
      integer,parameter,public :: NMOPRIM_4           = MOVEOUT_4           ! order.
      integer,parameter,public :: NMOPRIM_ETA4        = MOVEOUT_ETA4        ! order.
      integer,parameter,public :: NMOPRIM_ETA         = MOVEOUT_ETA         ! order.
      integer,parameter,public :: NMOPRIM_2_PLUS_4    = MOVEOUT_2_PLUS_4    ! order.
      integer,parameter,public :: NMOPRIM_2_PLUS_ETA4 = MOVEOUT_2_PLUS_ETA4 ! order.
      integer,parameter,public :: NMOPRIM_2_PLUS_ETA  = MOVEOUT_2_PLUS_ETA  ! order.

      integer,parameter,public :: NMOPURE_LINEAR  = MOVEOUT_LINEAR   ! terpmode
      integer,parameter,public :: NMOPURE_CUBIC   = MOVEOUT_CUBIC    ! terpmode
      integer,parameter,public :: NMOPURE_FFT2    = MOVEOUT_FFT2     ! terpmode
      integer,parameter,public :: NMOPURE_FFT4    = MOVEOUT_FFT4     ! terpmode
      integer,parameter,public :: NMOPURE_FFT8    = MOVEOUT_FFT8     ! terpmode

      character(len=100),public :: nmopure_ident = &
        "$Id: nmo.f90,v 1.76 2001/12/06 17:04:24 Selzler prod sps $"

      contains


!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!


      subroutine nmopure_create (obj,ndpt,tstrt,dt,action,  &
                                 order,terpmode,doppler,tracemute,error,msg)

      type(nmopure_struct),pointer       :: obj              ! arguments
      integer             ,intent(in)    :: ndpt             ! arguments
      real                ,intent(in)    :: tstrt            ! arguments
      real                ,intent(in)    :: dt               ! arguments
      integer             ,intent(in)    :: action           ! arguments
      integer             ,intent(in)    :: order            ! arguments
      integer             ,intent(in)    :: terpmode         ! arguments
      real                ,intent(in)    :: doppler          ! arguments
      logical             ,intent(in)    :: tracemute        ! arguments
      logical             ,intent(out)   :: error            ! arguments
      character(len=*)    ,intent(out)   :: msg              ! arguments

      allocate (obj)

      nullify (obj%timezero)
      nullify (obj%moveout)

      obj%nfine = ndpt          ! reset below by timezero_create.
      obj%dt    = dt

      call timezero_create (obj%timezero, ndpt, tstrt, dt, obj%nfine)

      call moveout_create (obj%moveout, obj%nfine, dt, action,  &
                           order, error, msg, terpmode, doppler, tracemute)
      if (error) return

      msg = 'NMOPURE module successfully created'

      end subroutine nmopure_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine nmopure_delete (obj)

      type(nmopure_struct),pointer :: obj       ! arguments

      if (associated(obj%timezero))   call timezero_delete (obj%timezero)
      if (associated(obj%moveout))    call moveout_delete  (obj%moveout)

      deallocate(obj)

      end subroutine nmopure_delete


!!--------------------------- velfun ----------------------------------------!!
!!--------------------------- velfun ----------------------------------------!!
!!--------------------------- velfun ----------------------------------------!!


      subroutine nmopure_velfun (obj,npicks2,tpicks2,vpicks2,  &
                                     npicks4,tpicks4,vpicks4,error,msg)

      type(nmopure_struct),intent(inout) :: obj                  ! arguments
      integer             ,intent(in)    :: npicks2              ! arguments
      real                ,intent(in)    :: tpicks2(:)           ! arguments
      real                ,intent(in)    :: vpicks2(:)           ! arguments
      integer             ,intent(in)    :: npicks4              ! arguments
      real                ,intent(in)    :: tpicks4(:)           ! arguments
      real                ,intent(in)    :: vpicks4(:)           ! arguments
      logical             ,intent(out)   :: error                ! arguments
      character(len=*)    ,intent(out)   :: msg                  ! arguments
      real                               :: vel2  (obj%nfine)    ! local
      real                               :: vel4  (obj%nfine)    ! local

      call terputil_fastsamp (tpicks2, npicks2, vpicks2, &
                              0.0, obj%dt, obj%nfine, vel2)

      call terputil_fastsamp (tpicks4, npicks4, vpicks4, &
                              0.0, obj%dt, obj%nfine, vel4)

      call moveout_velfun (obj%moveout, error, msg, vel2, vel4)

      end subroutine nmopure_velfun


!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!


      subroutine nmopure_apply (obj,offset,offnew,tr,mtop,mbottom)

      type(nmopure_struct),intent(inout) :: obj                  ! arguments
      real                ,intent(inout) :: offset,offnew        ! arguments
      real                ,intent(inout) :: tr(:)                ! arguments
      integer   ,optional ,intent(inout) :: mtop,mbottom         ! arguments
      real                               :: trfine(obj%nfine)    ! local

      call timezero_forward (obj%timezero, tr, trfine, mtop, mbottom)

      call moveout_apply (obj%moveout, offset, offnew, trfine, mtop, mbottom)

      call timezero_reverse (obj%timezero, trfine, tr, mtop, mbottom)

      end subroutine nmopure_apply


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module nmopure_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

