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


!<brief_doc>
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : NMOPRIM        (normal moveout primitive)
! Category   : velocity
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : To apply or remove NMO corrections to seismic data.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive contains all of the normal moveout code used by the NMO
! process.
!
! This primitive was made from the NMO process, which was converted to the
! new system by Randy Selzler.  The purpose of splitting this primitive out
! of the NMO process was to make it more easily reusable and to simplify
! maintenance.
!
! NMOPRIM has several modes of operation: perform normal moveout correction
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
! This primitive uses the VELTERP primitive to read the velocity file and
! interpolate the velocity functions in time and space.  The VELTERP primitive
! uses the VELIO primitive to read the velocity file, the VELSET primitive to
! store the velocity functions, the GRIDCHECK primitive to make sure the
! velocity functions fall on a grid, the TERPUTIL primitive to help interpolate
! the velocity functions to the desired (X,Y) location, the TERPUTIL primitive
! also for linear velocity function resampling, and the INTPVELF primitive
! for spline velocity function resampling.
!
! This primitive uses the TIMEZERO primitive to shift the trace to and from
! zero time if necessary.  The TIMEZERO primitive uses the TIMESEL primitive
! to do the actual shifts.
!
! This primitive uses the STATUTIL primitive to smooth the velocity functions
! if they are going to be used for demult.
!
! This primitive uses the CVPTOM primitive to optionally convert the velocity
! functions to demult functions.
!
! This primitive uses the MOVEOUT primitive to perform the actual normal
! moveout and to mute the trace.  The MOVEOUT primitive uses the DYNSAMP
! primitive to dynamically resample traces using FFT or linear or cubic
! interpolation.  The DYNSAMP primitive uses the DENSIFY primitive which
! uses the FFT primitive for the FFT resampling.
!
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
!                         o     i      i    i     i   i    i      i      i   
!   call nmoprim_create (obj,lunprint,nwih,ndpt,tstrt,dt,action,order,terpmode,
!                        doppler,tracemute,pathname2,pathname4,
!                           i        i        i         i
!
!                        velbias,velscale,sampmode,nhx,nhy,error,msg,velsource)
!                           i       i        i      b   b    o    o      i
!                                                                       opt
!
!                         b     i        i       i     i     i
!   call nmoprim_demult (obj, demult, tsmooth, ratio, omax, omin,
!                        nfold, fmean, tmute, vmute, omute, vmin)
!                          i      i      i      i      i     i
!
!                         b    i      i      b      b    b   b      b
!   call nmoprim_apply  (obj,xcoord,ycoord,offset,offnew,tr,mtop,mbottom,
!                        error,msg,veltrace2,veltrace4,changed)
!                          o    o     i         i         i
!                                    opt       opt       opt
!
!                         b
!   call nmoprim_delete (obj)
!
!
! NMOPRIM_DEMULT must be called right after NMOPRIM_CREATE.
! NMOPRIM_DEMULT need not be called unless demult velocities are to be used.
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(nmoprim_struct)   obj = pointer to the NMOPRIM object.
! integer           lunprint = unit number for printing.
! integer               nwih = number of trace headers.
! integer               ndpt = number of trace values.
! real                 tstrt = trace starting time (seconds).
! real                    dt = trace sample interval (seconds).
! integer             action = normal moveout action to perform.
! integer              order = normal moveout order parameter.
! integer           terpmode = trace interpolation option to use.
! real               doppler = doppler stretch factor.
! logical          tracemute = whether to mute the trace.
! character(len=*) pathname2 = name of velocity file     for 2nd order moveout.
! character(len=*) pathname4 = name of velocity/eta file for 4th order moveout.
! real               velbias = bias to add to velocities        (2nd order).
! real              velscale = factor to multiply velocities by (2nd order).
! integer           sampmode = velocity uniform sampling option.
! integer                nhx = trace header word containing X coordinate.
! integer                nhy = trace header word containing Y coordinate.
! logical              error = error flag (true if an error occurred).
! character(len=*)       msg = message for possible printing.
! integer          velsource = source of velocity functions.
!
! logical             demult = whether to convert velocities to demult vels.
! real               tsmooth = velocity smoothing window for demult (seconds).
! real                 ratio = demult velocity ratio (between 0.0 and 1.0).
! real                  omax = maximum offset in data.
! real                  omin = minimum offset in data.
! integer              nfold = fold of stack.
! real                 fmean = mean frequency in data.
! real                 tmute = TIM_ADD parameter in MUTE process (seconds).
! real                 vmute = VEL_MUTE parameter in MUTE process.
! real                 omute = minimum offset out of mute calculation (zero).
! real                  vmin = minimum converted demult velocity.
!
! real                xcoord = X coordinate of trace.
! real                ycoord = Y coordinate of trace.
! real                offset = offset of trace (changed if doing partial NMO).
! real                offnew = new offset of trace (if doing partial NMO).
! real              tr(ndpt) = trace values.
! integer               mtop = head mute index of trace (used and changed).
! integer            mbottom = tail mute index of trace (used and changed).
! real       veltrace2(ndpt) = velocity values     for 2nd order moveout.
! real       veltrace4(ndpt) = velocity/eta values for 4th order moveout.
! logical            changed = true if veltrace2 or veltrace4 has changed.
!
! The default value of VELSOURCE is NMOPRIM_CPSFILE.
!
! VELTRACE2 and/or VELTRACE4 must be supplied if VELSOURCE is NMOPRIM_VELTRACE.
!
! CHANGED (default true) specifies whether any values stored in VELTRACE2 or
! VELTRACE4 have changed since the previous call to NMOPRIM_APPLY.  CHANGED
! must be true for the first call to NMOPRIM_APPLY.  CHANGED is not used unless
! VELSOURCE is NMOPRIM_VELTRACE.  When CHANGED is false, increased efficiency
! is attained because some calculations based on the values in VELTRACE2 and
! VELTRACE4 need not be repeated.
!
! NHX and NHY must be intent(in)  when VELSOURCE is NMOPRIM_TRACEFILE.
! NHX and NHY     are intent(out) when VELSOURCE is NMOPRIM_CPSFILE.
! NHX and NHY     are not needed  when VELSOURCE is NMOPRIM_VELTRACE.
!
!-------------------------------------------------------------------------------
!                    SUBROUTINE ARGUMENT DETAILS
!
! ACTION = NMOPRIM_FORWARD means apply forward NMO correction.
! ACTION = NMOPRIM_REVERSE means apply reverse NMO correction.
! ACTION = NMOPRIM_PARTIAL means apply partial NMO correction.
! ACTION = NMOPRIM_VNMO    means put NMO (stacking) velocities into TR.
! ACTION = NMOPRIM_VINT    means put Dix interval velocities into TR.
!
! OFFSET and OFFNEW are swapped when doing partial NMO.
! Otherwise, OFFSET is not changed and OFFNEW is not used.
!
! SAMPMODE = NMOPRIM_LSAMP means linear velocity resampling to constant step.
! SAMPMODE = NMOPRIM_SSAMP means spline velocity resampling to constant step.
!
! ORDER = NMOPRIM_2           =  normal    2nd order   hyperbolic moveout.
! ORDER = NMOPRIM_4           = residual   4th order   non-hyperbolic moveout.
! ORDER = NMOPRIM_ETA4        = residual 4th order eta non-hyperbolic moveout.
! ORDER = NMOPRIM_ETA         = residual  "exact" eta  non-hyperbolic moveout.
! ORDER = NMOPRIM_2_PLUS_4    = 2nd order plus   4th order   moveout combined.
! ORDER = NMOPRIM_2_PLUS_ETA4 = 2nd order plus 4th order eta moveout combined.
! ORDER = NMOPRIM_2_PLUS_ETA  = 2nd order plus  "exact" eta  moveout combined.
!
! TERPMODE = NMOPRIM_LINEAR means 2-point linear interpolation.
! TERPMODE = NMOPRIM_CUBIC  means 4-point cubic interpolation.
! TERPMODE = NMOPRIM_FFT2   means FFT densification plus linear interpolation.
! TERPMODE = NMOPRIM_FFT4   means FFT densification plus linear interpolation.
! TERPMODE = NMOPRIM_FFT8   means FFT densification plus linear interpolation.
!
! VELSOURCE = NMOPRIM_CPSFILE   means velocities are on a traditional CPS
!                                velocity file (old style or self-defining
!                                style or modspec file).
!
! VELSOURCE = NMOPRIM_TRACEFILE means velocities are on velocity trace file.
!
! VELSOURCE = NMOPRIM_VELTRACE  means velocity functions VELTRACE2 and/or
!                                VELTRACE4 are passed in.
!
! Demult velocities will be used only if ORDER is NMOPRIM_2.
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
!008. 2006-06-20  B. Menger   Removed Unused Variables.
!  7. 2005-01-31 Stoeckley  Add ability to read modspec files (documentation
!                            change only).
!  6. 2004-10-12 Stoeckley  Fix bug whereby the OFFSET and OFFNEW arguments
!                            were not switched when ACTION=NMOPRIM_PARTIAL.
!  5. 2004-05-11 Stoeckley  Add optional arguments VELSOURCE, VELTRACE2,
!                            VELTRACE4, and CHANGED; add several new
!                            fourth-order options including eta.
!  4. 2003-11-03 Stoeckley  Make arguments MTOP and MBOTTOM required.
!  3. 2003-10-27 Stoeckley  Add new options for TERPMODE; remove the FINERATIO
!                            argument; replace TRACEPREP calls with TIMEZERO
!                            calls.
!  2. 2003-10-16 Stoeckley  Make arguments MTOP and MBOTTOM optional.
!  1. 2003-10-02 Stoeckley  Initial version made from the NMO process, nearly
!                            completely rewritten with several bug fixes,
!                            and with most code moved into several new
!                            primitives called from NMOPRIM.
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

      module nmoprim_module
      use velterp_module
      use timezero_module
      use moveout_module
      use statutil_module
      use cvptom_module
      use rantfile_module
      implicit none
      public


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: nmoprim_struct
      private

        type(velterp_struct)  ,pointer :: velterp2
        type(velterp_struct)  ,pointer :: velterp4
        type(timezero_struct) ,pointer :: timezero
        type(moveout_struct)  ,pointer :: moveout  
        type(cvptom_struct)   ,pointer :: cvptom
        type(rantfile_struct) ,pointer :: rantfile2
        type(rantfile_struct) ,pointer :: rantfile4
        integer                        :: nwih 
        real                           :: dt
        integer                        :: nfine
        logical                        :: demult
        integer                        :: nsmooth
        integer                        :: velsource
        integer                        :: action
        logical                        :: need_two
        logical                        :: need_four

      end type nmoprim_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer,parameter,public :: NMOPRIM_FORWARD = MOVEOUT_FORWARD  ! action
      integer,parameter,public :: NMOPRIM_REVERSE = MOVEOUT_REVERSE  ! action
      integer,parameter,public :: NMOPRIM_PARTIAL = MOVEOUT_PARTIAL  ! action
      integer,parameter,public :: NMOPRIM_VNMO    = MOVEOUT_VNMO     ! action
      integer,parameter,public :: NMOPRIM_VINT    = MOVEOUT_VINT     ! action

 integer,parameter,public :: NMOPRIM_2           = MOVEOUT_2           ! order.
 integer,parameter,public :: NMOPRIM_4           = MOVEOUT_4           ! order.
 integer,parameter,public :: NMOPRIM_ETA4        = MOVEOUT_ETA4        ! order.
 integer,parameter,public :: NMOPRIM_ETA         = MOVEOUT_ETA         ! order.
 integer,parameter,public :: NMOPRIM_2_PLUS_4    = MOVEOUT_2_PLUS_4    ! order.
 integer,parameter,public :: NMOPRIM_2_PLUS_ETA4 = MOVEOUT_2_PLUS_ETA4 ! order.
 integer,parameter,public :: NMOPRIM_2_PLUS_ETA  = MOVEOUT_2_PLUS_ETA  ! order.

      integer,parameter,public :: NMOPRIM_LINEAR  = MOVEOUT_LINEAR   ! terpmode
      integer,parameter,public :: NMOPRIM_CUBIC   = MOVEOUT_CUBIC    ! terpmode
      integer,parameter,public :: NMOPRIM_FFT2    = MOVEOUT_FFT2     ! terpmode
      integer,parameter,public :: NMOPRIM_FFT4    = MOVEOUT_FFT4     ! terpmode
      integer,parameter,public :: NMOPRIM_FFT8    = MOVEOUT_FFT8     ! terpmode

      integer,parameter,public :: NMOPRIM_LSAMP   = VELTERP_LSAMP    ! sampmode
      integer,parameter,public :: NMOPRIM_SSAMP   = VELTERP_SSAMP    ! sampmode

      integer,parameter,public :: NMOPRIM_CPSFILE   = 1    ! velsource
      integer,parameter,public :: NMOPRIM_TRACEFILE = 2    ! velsource
      integer,parameter,public :: NMOPRIM_VELTRACE  = 3    ! velsource

      character(len=100),public :: nmoprim_ident = &
        "$Id: nmoprim.f90,v 1.8 2006/06/20 13:12:01 Menger prod sps $"

      contains


!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!


      subroutine nmoprim_create (obj,lunprint,nwih,ndpt,tstrt,dt,action,  &
                                 order,terpmode,doppler,tracemute,        &
                                 pathname2,pathname4,                     &
                                 velbias,velscale,sampmode,               &
                                 nhx,nhy,error,msg,velsource)
      implicit none
      type(nmoprim_struct),pointer       :: obj              ! arguments
      integer             ,intent(in)    :: lunprint         ! arguments
      integer             ,intent(in)    :: nwih             ! arguments
      integer             ,intent(in)    :: ndpt             ! arguments
      real                ,intent(in)    :: tstrt            ! arguments
      real                ,intent(in)    :: dt               ! arguments
      integer             ,intent(in)    :: action           ! arguments
      integer             ,intent(in)    :: order            ! arguments
      integer             ,intent(in)    :: terpmode         ! arguments
      real                ,intent(in)    :: doppler          ! arguments
      logical             ,intent(in)    :: tracemute        ! arguments
      character(len=*)    ,intent(in)    :: pathname2        ! arguments
      character(len=*)    ,intent(in)    :: pathname4        ! arguments
      real                ,intent(in)    :: velbias          ! arguments
      real                ,intent(in)    :: velscale         ! arguments
      integer             ,intent(in)    :: sampmode         ! arguments
      integer             ,intent(inout) :: nhx,nhy          ! arguments
      logical             ,intent(out)   :: error            ! arguments
      character(len=*)    ,intent(out)   :: msg              ! arguments
      integer  ,optional  ,intent(in)    :: velsource        ! arguments

      allocate (obj)

      nullify (obj%velterp2)
      nullify (obj%velterp4)
      nullify (obj%timezero)
      nullify (obj%moveout)
      nullify (obj%cvptom)
      nullify (obj%rantfile2)
      nullify (obj%rantfile4)

      obj%nwih      = nwih
      obj%dt        = dt
      obj%nfine     = ndpt              ! reset below by timezero_create.
      obj%demult    = .false.           ! reset in nmoprim_demult.
      obj%nsmooth   = 1                 ! reset in nmoprim_demult.
      obj%velsource = NMOPRIM_CPSFILE
      obj%action    = action
      obj%need_two  = (order /= NMOPRIM_4)
      obj%need_four = (order /= NMOPRIM_2)

      if (present(velsource)) obj%velsource = velsource

      call timezero_create (obj%timezero, ndpt, tstrt, dt, obj%nfine)

      call moveout_create (obj%moveout, obj%nfine, dt, action,  &
                           order, error, msg, terpmode, doppler, tracemute)
      if (error) return

      select case (obj%velsource)

        case (NMOPRIM_CPSFILE)

           if (obj%need_two) then
                call velterp_create (obj%velterp2, pathname2, obj%nfine, dt,  &
                                     velbias, velscale, sampmode,             &
                                     nhx, nhy, error, msg, 2, 'VTNM')
                if (error) return
           end if

           if (obj%need_four) then
                call velterp_create (obj%velterp4, pathname4, obj%nfine, dt,  &
                                     0.0, 1.0, sampmode,                      &
                                     nhx, nhy, error, msg, 4, 'VTNM')
                if (error) return
           end if

        case (NMOPRIM_TRACEFILE)

           if (obj%need_two) then
                call rantfile_create (obj%rantfile2, pathname2, nwih, &
                                      obj%nfine, 0.0, dt, nhx, nhy,   &
                                      lunprint, error, msg)
                if (error) return
           end if

           if (obj%need_four) then
                call rantfile_create (obj%rantfile4, pathname4, nwih, &
                                      obj%nfine, 0.0, dt, nhx, nhy,   &
                                      lunprint, error, msg)
                if (error) return
           end if

        case (NMOPRIM_VELTRACE)

           if (tstrt /= 0.0) then
                error = .true.
                msg = 'TSTRT must be zero when passing in velocity traces.'
                return
           end if
           !! now nfine == ndpt since tstrt == zero.

        case default

           error = .true.
           msg = 'illegal value for VELSOURCE option'
           return

      end select

      msg = 'NMOPRIM module successfully created'
      return
      end subroutine nmoprim_create


!!--------------------------------- demult -------------------------------!!
!!--------------------------------- demult -------------------------------!!
!!--------------------------------- demult -------------------------------!!


      subroutine nmoprim_demult (obj, demult, tsmooth, ratio,  &
                                 omax, omin, nfold, fmean,     &
                                 tmute, vmute, omute, vmin)
      implicit none
      type(nmoprim_struct),intent(inout) :: obj              ! arguments
      logical             ,intent(in)    :: demult           ! arguments
      real                ,intent(in)    :: tsmooth          ! arguments
      real                ,intent(in)    :: ratio            ! arguments
      real                ,intent(in)    :: omax             ! arguments
      real                ,intent(in)    :: omin             ! arguments
      integer             ,intent(in)    :: nfold            ! arguments
      real                ,intent(in)    :: fmean            ! arguments
      real                ,intent(in)    :: tmute            ! arguments
      real                ,intent(in)    :: vmute            ! arguments
      real                ,intent(in)    :: omute            ! arguments
      real                ,intent(in)    :: vmin             ! arguments

      if (.not.demult) return
      if (.not.obj%need_two .or. obj%need_four) return

      obj%demult  = demult
      obj%nsmooth = tsmooth / obj%dt

      call cvptom_create (obj%cvptom, ratio, omax, omin, nfold, &
                          fmean, tmute, vmute, omute, vmin)
      return
      end subroutine nmoprim_demult


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine nmoprim_delete (obj)
      implicit none
      type(nmoprim_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%velterp2))    call velterp_delete   (obj%velterp2)
      if (associated(obj%velterp4))    call velterp_delete   (obj%velterp4)
      if (associated(obj%timezero))    call timezero_delete  (obj%timezero)
      if (associated(obj%moveout))     call moveout_delete   (obj%moveout)
      if (associated(obj%rantfile2))   call rantfile_delete  (obj%rantfile2)
      if (associated(obj%rantfile4))   call rantfile_delete  (obj%rantfile4)
      if (associated(obj%cvptom))      call cvptom_delete    (obj%cvptom)

      deallocate(obj)
      return
      end subroutine nmoprim_delete


!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!


      subroutine nmoprim_apply (obj,xcoord,ycoord,offset,offnew,tr, &
                                mtop,mbottom,error,msg,             &
                                veltrace2,veltrace4,changed)
      implicit none
      type(nmoprim_struct),intent(inout) :: obj                  ! arguments
      real                ,intent(in)    :: xcoord,ycoord        ! arguments
      real                ,intent(inout) :: offset,offnew        ! arguments
      real                ,intent(inout) :: tr(:)                ! arguments
      integer             ,intent(inout) :: mtop,mbottom         ! arguments
      logical             ,intent(out)   :: error                ! arguments
      character(len=*)    ,intent(out)   :: msg                  ! arguments
      real      ,optional ,intent(in)    :: veltrace2(:)         ! arguments
      real      ,optional ,intent(in)    :: veltrace4(:)         ! arguments
      logical   ,optional ,intent(in)    :: changed              ! arguments
      logical                            :: changed2,changed4    ! local
      double precision                   :: hd    (obj%nwih)     ! local
      real                               :: vel2  (obj%nfine)    ! local
      real                               :: vel4  (obj%nfine)    ! local
      real                               :: trfine(obj%nfine)    ! local
      real                               :: work  (obj%nfine*3)  ! local

      real                               :: flip                 ! local

      error    = .false.
      msg      = 'OK'
      vel2(:)  = 0.0
      vel4(:)  = 0.0
      changed2 = .false.
      changed4 = .false.

      select case (obj%velsource)

        case (NMOPRIM_CPSFILE)

           if (obj%need_two) then
                call velterp_find (obj%velterp2,xcoord,ycoord,vel2,changed2)
           end if

           if (obj%need_four) then
                call velterp_find (obj%velterp4,xcoord,ycoord,vel4,changed4)
           end if

        case (NMOPRIM_TRACEFILE)

           if (obj%need_two) then
                call rantfile_find (obj%rantfile2, xcoord, ycoord, hd, vel2, &
                                    error, msg, changed2)
                if (error) return
           end if

           if (obj%need_four) then
                call rantfile_find (obj%rantfile4, xcoord, ycoord, hd, vel4, &
                                    error, msg, changed4)
                if (error) return
           end if

        case (NMOPRIM_VELTRACE)

           if (obj%need_two) then
                if (.not.present(veltrace2)) then
                     error = .true.
                     msg = 'VELTRACE2 must be specified'
                     return
                end if
                vel2(1:obj%nfine) = veltrace2(1:obj%nfine)
                changed2 = .true.
                if (present(changed)) changed2 = changed
           end if

           if (obj%need_four) then
                if (.not.present(veltrace4)) then
                     error = .true.
                     msg = 'VELTRACE4 must be specified'
                     return
                end if
                vel4(1:obj%nfine) = veltrace4(1:obj%nfine)
                changed4 = .true.
                if (present(changed)) changed4 = changed
           end if

      end select

      if (changed2 .or. changed4) then
           if (obj%demult) then
                call statutil_1d_smooth_quick (vel2, obj%nfine, obj%nsmooth)
                call cvptom                   (obj%cvptom, obj%nfine, vel2, &
                                               0.0, obj%dt, work)
           end if
           call moveout_velfun (obj%moveout, error, msg, vel2, vel4)
           if (error) return
      end if

      call timezero_forward (obj%timezero, tr, trfine, mtop, mbottom)

      call moveout_apply (obj%moveout, offset, offnew, trfine, mtop, mbottom)

      call timezero_reverse (obj%timezero, trfine, tr, mtop, mbottom)

      if (obj%action == NMOPRIM_PARTIAL) then
           flip   = offset
           offset = offnew
           offnew = flip
      end if

      return
      end subroutine nmoprim_apply


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module nmoprim_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

