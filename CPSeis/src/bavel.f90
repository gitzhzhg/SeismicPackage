!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- bavel.f90 --------------------------------!!
!!------------------------------- bavel.f90 --------------------------------!!
!!------------------------------- bavel.f90 --------------------------------!!
 
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
! Name       : B_AVEL
! Category   : velocity
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : AVO and Velocity Analysis (BAVEL) primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive serves as the framework for the AVO & Velocity Analysis
! module and the 1st half of the AVO Velocity Iteration module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
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
!                        SUBROUTINE ARGUMENTS
!                         SUBROUTINE DETAILS
!
! B_AVEL_CREATE:      
!
!                                  b
!  call bavel_create (obj)
!
!  type(bavel_struct) obj = bavel_structure.
!
!
! B_AVEL_DELETE:
!
!                                  b
!  call bavel_delete (obj)
!
!  type(bavel_struct) obj = bavel structure.
!
!
! B_AVEL_INIT:
!                                b
!  call bavel_init (obj)
!
!  type(bavel_struct) obj = bavel structure.
!
!
! B_AVEL_WORK:
!      
!                                b   b  b  b
!  call bavel_work (obj,ntr,hd,tr)
!
!  type(bavel_struct) obj = bavel structure.
!  integer             ntr = number of traces in 'tr' array.
!  real                tr  = trace data.
!  double precision    hd  = trace headers.
!
!
! B_AVEL_INIT_PARMS:
!  (1) Initializes parameter flags.
!
!                           b
!  call bavel_init_parms (obj)
!
!  type(bavel_struct) obj = bavel structure.
!      
!
! B_AVEL_GET_PARM:
!  (1) Gets the value for a specific parameter.
!
!                         b    i     i
!  call bavel_get_parm (obj, name, parm)
!
!  type(bavel_struct) obj  = bavel structure.
!  i,r,c               parm = parameter value.
!  character(*)        name = paramater name.
!
!
! B_AVEL_SET_PARM:
!  (1) Sets the value for a specific parameter.
!
!                         b    i     o
!  call bavel_set_parm (obj, name, parm)
!
!  type(bavel_struct) obj  = bavel structure.
!  i,r,c               parm = parameter value.
!  character(*)        name = paramater name.
!
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
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2007-11-29  Stoeckley  Remove unused reference to memman.
!005. 2006-06-20  B. Menger   Removed Unused Variables.
!  4. 2005-01-31  B. Lucas   Fixed bug in calls to NMO by passing the
!                            original CPS headers instead of the altered
!                            headers set by AVOVIT,AVOVAN,etc.
!  3. 2004-04-27  B. Lucas   Added code to generate and average velocity
!                            function from the stacking velocity file, and
!                            to use this average function when checking
!                            the angle limits. Also, fixed some parameter
!                            retrieval bugs for ANGL_MAX, RIC_FREQ, etc.
!  2. 2003-09-23  Stoeckley  Remove declarations of index, dsqrt, and dcos,
!                             as these are fortran intrinsics.  Problem
!                             caught by portland compiler.
!  1. 2003-08-26  B. Lucas   Initial version.
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
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module bavel_module
      use ppavo_module
      use pc_module
      use named_constants_module
      use grid_module
      use pathchoose_module
      use pathcheck_module
      use velfile_module
      use velio_module
      use intpvelf_module
      use nmo_module
      implicit none

      private
      public :: bavel_create
      public :: bavel_delete
      public :: bavel_init
      public :: bavel_work
      public :: bavel_init_parms
      public :: bavel_get_parm
      public :: bavel_set_parm
      public :: bavel_get_cparm
      public :: bavel_get_iparm
      public :: bavel_get_rparm
      public :: bavel_set_cparm
      public :: bavel_set_iparm
      public :: bavel_set_rparm
      public :: bavel_read_velocity
      public :: bavel_convert_vels
      public :: bavel_rms_to_int_vel
      public :: bavel_int_to_rms_vel
      public :: bavel_smooth

      character(len=100),public,save :: BAVEL_IDENT = &
'$Id: bavel.f90,v 1.6 2007/11/30 13:55:16 Stoeckley beta sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type,public :: bavel_struct              
!      private

        integer            :: ipn       ! process number.
        integer            :: numtr     ! max number of input traces.
        logical            :: gathered  ! whether properly gathered.
        integer            :: nwih      ! number of header words.
        integer            :: ndpt      ! number of trace samples.
        real               :: tstrt     ! time of 1st trace sample (sec).
        real               :: dt        ! trace sample interval (sec).
        type(grid_struct)  :: grid      ! grid transform.

        integer            :: print_lun

        type(velfile_struct), pointer :: svel_obj  ! stacking velocity file
        integer            :: svel_nhx
        integer            :: svel_nhy
        real               :: svel_nmosign
        real               :: svel_nmoexp

        type(velfile_struct), pointer :: ivel_obj  ! interval velocity file 
        integer            :: ivel_nhx
        integer            :: ivel_nhy
        real               :: ivel_nmosign
        real               :: ivel_nmoexp

        type(velio_struct), pointer :: velio_obj ! velocity averaging

        integer            :: num_vels  ! number of velocity functions to try
        integer            :: velf_ref  ! index of reference velocity function
        integer            :: len_sflt  ! length of stacking vel. smooth. filter
        integer            :: len_oflt  ! length of output smoothing filter
        integer            :: stim_win  ! start of analysis window
        integer            :: etim_win  ! end of analysis window
        integer            :: method_a  ! method of computing 'a'
        integer            :: method_b  ! method of computing 'b'
        integer            :: nrma_opt  ! normalizing product trace by |a|
        integer            :: nrmb_opt  ! normalizing product traces by |b|
        integer            :: scas_opt  ! suppress scaling - 1st live trace
        integer            :: incr_opt  ! include curved ray paths
        integer            :: hdiv_opt  ! do horizontal divergence correction
        integer            :: vdiv_opt  ! do vertical divergence correction
        integer            :: nmsc_opt  ! suppress nmo stretch correction
        integer            :: ovrv_opt  ! override velocity with constant value
        integer            :: pass_opt  ! pass pre-stack data flag
        integer            :: id_table  ! id of velocity table
        integer            :: itpflg    ! 0=x-t data; 1=tau-p data
        integer            :: errs_opt   ! 0=none; 1=residual; 3=all errors
        integer            :: ilen      ! number of filter coefficients
        integer            :: mintr     ! min. trace no. within cdp to process
        integer            :: maxtr     ! max. trace no. within cdp to process
        integer            :: ista      ! starting analysis window time
        integer            :: ieta      ! end analysis window time
        integer            :: itpapt    ! tau-p aperture
        integer            :: ihilbert  ! length of hilbert transform
        integer            :: nfilts    ! number of inversion filters
        integer            :: lfltr(5)  ! length of individual inversion filters
        integer            :: itaperwin    ! taperwin length( msec)
        integer            :: nparm     ! number of parameters in parm array
        integer            :: parm(15)  ! array of parameters for ppavo_sahci
        integer            :: pdat_opt   ! selection of primary data attributes
        integer            :: targtime  ! target time
        integer            :: iticda    ! ticd code of 'a' trace
        integer            :: iticdb    ! ticd code of 'b' trace
        integer            :: ifsflag   ! first live value scaling flag
        integer            :: ntpv      ! # of traces output per vel. (2 or 5)
        integer            :: tcodes(6) ! list of trace id codes for avel
        integer            :: angfmt    ! format of common angle (0=x-t data)
        integer            :: trfo_opt  ! use variable trace weighting option
        integer            :: use_ivel  ! flag for using interval velocity file
        integer            :: movefg    ! true if data is to be nmo corrected
        integer            :: gmem_flag ! memory flag
        integer            :: flt_type  ! type of filter (ricker or wavelet)
        integer            :: gath_opt  ! type of gather (offset or angle)
        integer            :: avf_comp  ! flag for averging velocity function

        real               :: avf_slen  ! smooth length for avg. vel. function
        real               :: velf_inc  ! velocity function increment
        real               :: vel_zero     ! v0 for spherical divergence
        real               :: agc_wina  ! agc window length for nrma_opt
        real               :: agc_winb  ! agc window length for nrmb_opt
        real               :: alim1     ! squared sin of min. incidence angle
        real               :: alim2     ! squared sin of min. incidence angle
        real               :: avo_stab  ! avo stabilization term
        real               :: w0(5)     ! ricker wavetlet center frequency
        real               :: fs        ! sampling frequency (samples/sec)
        real               :: phas_rot     ! phase rotation angle
        real               :: rotab     ! phase rotation angle in a-b plane
        real               :: fscale    ! scaling factor
        real               :: ang_min   ! minimum angle of incidence
        real               :: angl_max   ! maximum angle of incidence
        real               :: h1(-100:100) ! first h1 inversion filter
        real               :: h2(-100:100) ! first h2 inversion filter
        real               :: icoef(1200)  ! interpolation coefficients
        character(len=256) :: svelname  ! name of stacking velocity database
        character(len=256) :: ivelname  ! name of interval velocity database

        integer            :: ndptvel
        integer            :: numvels
        integer            :: nsmooth
        real               :: xlast
        real               :: ylast
        logical            :: opt_sr

        integer            :: ipr
        character(len=5)   :: wtypes(5)
        integer            :: ilen2
        integer            :: nobj
        integer            :: itooltype
        integer            :: l_xy_svel
        integer            :: l_xy_ivel

!       B_AVEL parameter flags.
        logical            :: f_svelname
        logical            :: f_num_vels
        logical            :: f_velf_ref
        logical            :: f_velf_inc
        logical            :: f_angl_max 
        logical            :: f_angl_rng
        logical            :: f_avf_comp 
        logical            :: f_avf_slen
        logical            :: f_len_sflt
        logical            :: f_len_oflt
        logical            :: f_stim_win
        logical            :: f_etim_win
        logical            :: f_twin_tpr   
        logical            :: f_avo_stab
        logical            :: f_method_a
        logical            :: f_method_b
        logical            :: f_pdat_opt 
        logical            :: f_vel_zero   
        logical            :: f_flt_type
        logical            :: f_gath_opt
        logical            :: f_nrma_opt
        logical            :: f_nrmb_opt
        logical            :: f_agc_wina
        logical            :: f_agc_winb
        logical            :: f_scas_opt
        logical            :: f_pass_opt
        logical            :: f_incr_opt
        logical            :: f_hdiv_opt
        logical            :: f_vdiv_opt
        logical            :: f_nmsc_opt
        logical            :: f_trfo_opt
        logical            :: f_phas_rot   
        logical            :: f_ric_freq
        logical            :: f_bp_freqs
        logical            :: f_errs_opt 
        logical            :: f_use_ivel
        logical            :: f_ivelname

!       B_AVEL integer parameters.
        integer            :: p_avf_comp
        integer            :: p_num_vels
        integer            :: p_velf_ref
        integer            :: p_len_sflt
        integer            :: p_len_oflt
        integer            :: p_stim_win
        integer            :: p_etim_win
        integer            :: p_twin_tpr
        integer            :: p_method_a
        integer            :: p_method_b
        integer            :: p_pdat_opt
        integer            :: p_flt_type
        integer            :: p_gath_opt
        integer            :: p_nrma_opt
        integer            :: p_nrmb_opt
        integer            :: p_scas_opt
        integer            :: p_pass_opt
        integer            :: p_incr_opt
        integer            :: p_hdiv_opt
        integer            :: p_vdiv_opt
        integer            :: p_nmsc_opt
        integer            :: p_trfo_opt
        integer            :: p_errs_opt        
        integer            :: p_use_ivel

!       B_AVEL real parameters.
        real               :: p_avf_slen
        real               :: p_velf_inc
        real               :: p_angl_max
        real               :: p_avo_stab
        real               :: p_vel_zero
        real               :: p_ric_freq
        real               :: p_agc_wina
        real               :: p_agc_winb
        real               :: p_phas_rot

!       B_AVEL character parameters.
        character(len=256) :: p_svelname
        character(len=256) :: p_ivelname
        character(len=256) :: p_bp_freqs
        character(len=256) :: p_angl_rng

!       ProMAX temporary storage arrays.
        real               :: rtempz(50000) ! temporary buffer - real
        complex            :: ztempz(50000) ! temporary buffer - complex

!       ProMAX run-time parameters
        real               :: sampratz  ! sample rate (msec)
        integer            :: numsmpz   ! number of trace samples
        integer            :: ipsortz   ! physical primary sort flag
        integer            :: maxdtrz   ! maximum number of traces per ensemble
        integer            :: idtypez   ! primary data type
        integer            :: nthz      ! number of trace headers (=64)
        integer            :: iounitz   ! i/o unit for output diagnostics
        integer            :: ipkeyz    ! trace header index of primary sort key
        integer            :: iskeyz    ! trace header index of secndry sort key
        integer            :: idomainz  ! the domain
        logical            :: cleanupz  ! system clean-up flag
        logical            :: ierrorz   ! system error flag.

!       ProMAX miscellaneous parameters.
        integer            :: iunitsz   ! type of units (ienglishpz, imetricpz)
        integer            :: i3dz      ! 3d flag (1=3d, 0=other)
        integer            :: imultcz   ! multi-component flag (1=multi-comp.)
        integer            :: ntracez   ! maximum sequential trace number
        integer            :: icdpasnz  ! CDP bin assigned flag  (1=assigned)
        integer            :: igeoasnz  ! geometry assigned flag (1=assigned)

!       AVEL allocatable storage arrays.
        double precision, pointer, dimension(:,:) :: hdnmo
        integer                                 :: l_hdnmo
        real, pointer, dimension(:,:)           :: velscr
        integer                                 :: l_velscr
        real, pointer, dimension(:)             :: xy_svel
        integer                                 :: l_svel
        real, pointer, dimension(:)             :: xy_ivel
        integer                                 :: l_ivel
        real, pointer, dimension(:)             :: sloths
        integer                                 :: l_sloths
        real, pointer, dimension(:)             :: slothi
        integer                                 :: l_slothi
        real, pointer, dimension(:)             :: slothsx
        integer                                 :: l_slothsx
        real, pointer, dimension(:)             :: slothix
        integer                                 :: l_slothix
        real, pointer, dimension(:)             :: vel_avg
        integer                                 :: l_vel_avg
        real, pointer, dimension(:)             :: vel_interp
        integer                                 :: l_vel_interp
        real, pointer, dimension(:)             :: itrsave
        integer                                 :: l_itrsave
        real, pointer, dimension(:)             :: otrsave
        integer                                 :: l_otrsave
        real, pointer, dimension(:)             :: trcoef
        integer                                 :: l_trcoef
        real, pointer, dimension(:)             :: nmot
        integer                                 :: l_nmot
        real, pointer, dimension(:)             :: rscratch
        integer                                 :: l_rscratch
        double precision, pointer, dimension(:) :: dscratch
        integer                                 :: l_dscratch
        real, pointer, dimension(:)             :: rsum
        integer                                 :: l_rsum
        integer, pointer, dimension(:)          :: runs
        integer                                 :: l_runs
        real, pointer, dimension(:)             :: pra
        integer                                 :: l_pra
        real, pointer, dimension(:)             :: prb
        integer                                 :: l_prb

        logical :: initialized

!       CPS NMO storage.
        type(nmo_struct), pointer               :: svel_nmo
        type(nmo_struct), pointer               :: ivel_nmo

      end type bavel_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface bavel_get_parm
        module procedure bavel_get_iparm
        module procedure bavel_get_rparm
        module procedure bavel_get_cparm
      end interface

      interface bavel_set_parm
        module procedure bavel_set_iparm
        module procedure bavel_set_rparm
        module procedure bavel_set_cparm
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      integer, parameter :: stride = 50  ! for table printouts

      contains


!!--------------------------- bavel_create ----------------------------------!!
!!--------------------------- bavel_create ----------------------------------!!
!!--------------------------- bavel_create ----------------------------------!!

      subroutine bavel_create (obj)
      type(bavel_struct),pointer :: obj       ! arguments

      allocate (obj)    ! omit if OBJ is not to be passed as a pointer.

      nullify(obj%svel_nmo)
      nullify(obj%ivel_nmo)
      nullify(obj%svel_obj)
      nullify(obj%ivel_obj)
      nullify(obj%velio_obj)

      nullify(obj%hdnmo)
      nullify(obj%velscr)
      nullify(obj%xy_svel)
      nullify(obj%xy_ivel)
      nullify(obj%sloths)
      nullify(obj%slothi)
      nullify(obj%slothsx)
      nullify(obj%slothix)
      nullify(obj%vel_avg)
      nullify(obj%vel_interp)
      nullify(obj%itrsave)
      nullify(obj%otrsave)
      nullify(obj%trcoef)
      nullify(obj%nmot)
      nullify(obj%rsum)
      nullify(obj%rscratch)
      nullify(obj%dscratch)
      nullify(obj%pra)
      nullify(obj%prb)
      nullify(obj%runs)

      obj%initialized = .false.
      obj%gmem_flag = 0

      end subroutine bavel_create


!!--------------------------- bavel_delete ----------------------------------!!
!!--------------------------- bavel_delete ----------------------------------!!
!!--------------------------- bavel_delete ----------------------------------!!

      subroutine bavel_delete (obj)
      type(bavel_struct),pointer :: obj       ! arguments

      if (associated(obj%svel_nmo)) then
         call nmo_delete(obj%svel_nmo)
      end if
      if (associated(obj%ivel_nmo)) then
         call nmo_delete(obj%ivel_nmo)
      end if
      if (associated(obj%svel_obj)) then
         call velfile_delete(obj%svel_obj)
      end if
      if (associated(obj%ivel_obj)) then
         call velfile_delete(obj%ivel_obj)
      end if
      if (associated(obj%velio_obj)) then
         call velio_close(obj%velio_obj)
!         call velfile_delete(obj%velio_obj)
      end if
           
      deallocate(obj)    ! omit if OBJ is not to be passed as a pointer.
      end subroutine bavel_delete


!!---------------------------- bavel_init -----------------------------------!!
!!---------------------------- bavel_init -----------------------------------!!
!!---------------------------- bavel_init -----------------------------------!!

      subroutine bavel_init (obj, ntr)
      type(bavel_struct),intent(inout) :: obj       ! arguments
      integer,            intent(inout) :: ntr


      integer            :: ierror   
      integer            ::        colon 
      integer            ::                  nchars  
      integer            :: ifilt, nh, mmxh, ks, lfour
      integer            :: nz, ns, n, nfreqs
      integer            :: mnffti(1), mnffto(1)
      
      logical            :: error

      real               :: radian, w0t
      real               :: bw(6), rw(12), g(14), ff(14), a(14)
      real               :: pctw(5), pctc(5), cut(5), apars(4)
      real               :: pcvect(1000), pcorr(1000)
  
      double precision   :: xtemp, pole
      double precision   :: pwork(1000)


      character(len=256) :: angl_rng
      character(len=256) :: bp_freqs

      character(len=256) :: msg

      parameter (radian=57.2958)
      parameter (mmxh=100)
      parameter (ks=3)
      parameter (lfour=128)

      data bw        / 0.12, 0.45, 0.60, 0.70, 0.75, 0.80 /

      obj%print_lun = pc_get_lun()

      obj%cleanupz = .false.
      
      obj%ipr = obj%iounitz

      obj%opt_sr = .false.

      ierror = 0
      ntr = 0

      obj%dt = obj%sampratz / 1000.0
      obj%ndpt = obj%numsmpz

!     Warn if the data is stacked.
      if (obj%idtypez .eq. ppavo_stackedpz) then
         write (obj%print_lun, *) 'B_AVEL: You are applying NMO on &
     &stacked data.'
      end if

!     Get stacking velocity file name.
      call bavel_get_cparm(obj, 'svelname', obj%svelname, nchars)
      obj%id_table = -1
      call velfile_create(obj%svel_obj, obj%svelname,&
     &   obj%svel_nhx, obj%svel_nhy, error, msg,     &
     &   obj%svel_nmosign, obj%svel_nmoexp)

!     Get number of velocity functions to try and moveout flag.
      obj%num_vels = 0
      obj%movefg   = 0
      call bavel_get_iparm(obj, 'num_vels', obj%num_vels)
      if (obj%num_vels .eq. 0) then
         obj%num_vels = 1
      else if (obj%num_vels .lt. 0) then
         obj%movefg   = 2
         obj%num_vels = 1
      else
         obj%movefg   = 1
      end if

!     Get index of reference velocity function.
      obj%velf_ref = 1
      call bavel_get_iparm(obj, 'velf_ref', obj%velf_ref)
      if (obj%velf_ref .le. 0) obj%velf_ref = obj%num_vels/2 + 1
      if (obj%velf_ref .gt. obj%num_vels) obj%velf_ref = obj%num_vels/2 + 1

!     Get velocity function increment.
      obj%velf_inc  = 10.0
      call bavel_get_rparm(obj, 'velf_inc', obj%velf_inc)

!     Get maximum incidence angle.
      obj%ang_min   = 0.0               
      obj%angl_max   = 40.0              
      call bavel_get_rparm(obj, 'angl_max', obj%angl_max)

!     Get incidence angle range (this overrides maximum incidence angle).
      angl_rng = ' '
      call bavel_get_cparm(obj, 'angl_rng', angl_rng, nchars)
      if (angl_rng .ne. ' ') then
         colon = index(angl_rng(:nchars), ':')
      if (colon .gt. 1) read (angl_rng(:colon-1), *) obj%ang_min
         colon = colon + 1
         if (colon .lt. nchars) then
            read (angl_rng(colon:nchars), *) obj%angl_max
         end if
      endif
      if (obj%angl_max .le. obj%ang_min) then
         write (obj%print_lun, *) 'B_AVEL: Minimum incidence angle must &
     &be less than maximum.'
         ntr = FATAL_ERROR
         return
      end if
      obj%alim1 = (sin(obj%ang_min/radian)) ** 2
      obj%alim2 = (sin(obj%angl_max/radian)) ** 2

!     Get average velocity function flag.
      obj%avf_comp = 0
      call bavel_get_iparm(obj, 'avf_comp', obj%avf_comp)

!     Get average velocity function flag.
      obj%avf_slen = 0.2
      call bavel_get_rparm(obj, 'avf_slen', obj%avf_slen)

!     Get length of stacking velocity smoothing filter.
      obj%len_sflt = 5
      call bavel_get_iparm(obj, 'len_sflt', obj%len_sflt)
      if (obj%len_sflt .lt. 0) obj%len_sflt = 0
      obj%len_sflt = obj%len_sflt / obj%sampratz   ! convert to samples

!     Get length of output filter.
      obj%len_oflt = 0
      call bavel_get_iparm(obj, 'len_oflt', obj%len_oflt)
      if (obj%len_oflt .lt. 0) obj%len_oflt = 0
      obj%len_oflt = obj%len_oflt / obj%sampratz   ! convert to samples

!     Get start of analysis window.
      obj%ista = 0
      call bavel_get_iparm(obj, 'stim_win', obj%ista)
      if(obj%ista .lt. 0) obj%ista = 0

!     Get end of analysis window.
      obj%ieta = 99999
      call bavel_get_iparm(obj, 'etim_win', obj%ieta)
      if ((obj%ieta/obj%sampratz)+1 .gt. obj%numsmpz) then
         obj%ieta = (obj%numsmpz-1)*obj%sampratz
      end if
      if (obj%ieta .le. obj%ista) then
         write (obj%print_lun, *) 'B_AVEL: Ending time of analysis less &
     &than starting time.'
         ntr = FATAL_ERROR
         return
      end if      

!     Get AVO stabilization term.
      obj%avo_stab  = 80.0
      call bavel_get_rparm(obj, 'avo_stab', obj%avo_stab)

!     Get method of computing 'A'.
!     1 = "stack" -> conventional 'AFU' rap stack
!     2 = "z2"    -> 2nd order zero-offset stack
!     3 = "z4"    -> 4th order zero-offset stack
      call bavel_get_iparm(obj, 'method_a', obj%method_a)
      if (obj%method_a .lt. 1 .or. obj%method_a .gt. 3) obj%method_a = 3

!     Get method of computing 'B'.
!     1 = "b2" -> 2nd order slope
!     2 = "b4" -> 4th order slope
!     3 = "c4" -> 4th orderquartic slope
      call bavel_get_iparm(obj, 'method_b', obj%method_b)
      if (obj%method_b .lt. 1 .or. obj%method_b .gt. 3) obj%method_b = 2

!     Get selection of primary data and attributes.
!     1 = "anal" -> re(ab*) (ticd=42), im(ab*) (ticd=46)
!     2 = "avel" -> re(a)   (ticd=43), im(ab*) (ticd=46)
!     3 = "avo"  -> re(a)   (ticd=43), re(ab*) (ticd=42)
!     4 = "ab"   -> re(a)   (ticd=43), re(b)   (ticd=44)
      call bavel_get_iparm(obj, 'pdat_opt', obj%pdat_opt)
      obj%iticda = ppavo_atrpz
      obj%iticdb = ppavo_rvitrpz
      if (obj%pdat_opt .eq. 1) obj%iticda = ppavo_hcitrpz
      if (obj%pdat_opt .eq. 3) obj%iticdb = ppavo_hcitrpz
      if (obj%pdat_opt .eq. 4) obj%iticdb = ppavo_btrpz
      obj%tcodes(1) = obj%iticda
      obj%tcodes(2) = obj%iticdb
      obj%tcodes(3) = ppavo_residpz
      obj%tcodes(4) = ppavo_aresidpz
      obj%tcodes(5) = ppavo_bresidpz

!     Get v0 for spherical divergence.
      if(obj%iunitsz .eq. ppavo_englishpz) then
         obj%vel_zero  = 4800.0
      else
         obj%vel_zero  = 1480.0
      endif
      call bavel_get_rparm(obj, 'vel_zero', obj%vel_zero)

!     Get filter type.
      obj%flt_type = 1
      call bavel_get_iparm(obj, 'flt_type', obj%flt_type)

!     Get gather type.
      obj%gath_opt = 0
      call bavel_get_iparm(obj, 'gath_opt', obj%gath_opt)
      obj%angfmt = 0
      if(obj%gath_opt .eq. 1) obj%angfmt = 0
      if(obj%gath_opt .eq. 2) obj%angfmt = 2

!     Check filter type.
      if (obj%flt_type .eq. 1) then 
!        Ricker, get wavelet center frequency.
         obj%w0(1) = 30.0
         obj%wtypes(1) = 'rick'
         call bavel_get_rparm(obj, 'ric_freq', obj%w0(1))
      else
!        Bandpass, get corner frequencies.
         call bavel_get_cparm(obj, 'bp_freqs', bp_freqs, nchars)
         call ppavo_slash2dash(bp_freqs, nchars, nz)

!        Replace any '-' delimiters with commas.
         do n = 1, nz
            if (bp_freqs(n:n) .eq. '-') bp_freqs(n:n) = ','
         end do

         call ppavo_chars2real (bp_freqs, nz, apars, nfreqs)
!        Check for illegal filter specification.
!        f1 < f2 <= f3 < f4 for iflt_spec = 1 
         if ( apars(1) .gt. apars(2) .or. apars(2) .gt. apars(3)&
     &      .or. apars(3) .gt. apars(4) ) then
            write (obj%print_lun, *) 'B_AVEL: Invalid filter &
     &frequency specification.'
            ntr = FATAL_ERROR
            return
         end if
         obj%w0(1) = 0.5 * (apars(2) + apars(3))
         obj%wtypes(1) = 'band'   
      end if

!     Get normalize 'A' flag.
      obj%nrma_opt = 0
      call bavel_get_iparm(obj, 'nrma_opt', obj%nrma_opt)

!     Get normalize 'B' flag.
      obj%nrmb_opt = 0
      call bavel_get_iparm(obj, 'nrmb_opt', obj%nrmb_opt)

!     Get 'A' option - normalize product traces by |A|.
      obj%agc_wina = 0.0
      call bavel_get_rparm(obj, 'agc_wina', obj%agc_wina)

!     Get 'B' option - normalize product traces by |B|.
      obj%agc_winb = 0.0
      call bavel_get_rparm(obj, 'agc_winb', obj%agc_winb)

!     Get 'F' option - suppress scaling based on first live trace.
      obj%scas_opt = 1
      call bavel_get_iparm(obj, 'scas_opt', obj%scas_opt)

!     Get error analysis flags.
      obj%ntpv    = 2
      obj%errs_opt = 0
      if (obj%method_b + 1 .eq. obj%method_a) then
         call bavel_get_iparm(obj, 'errs_opt',  obj%errs_opt)
      end if  
      if (obj%errs_opt .ne. 0) obj%ntpv = 5
      if (obj%errs_opt .eq. 4) then
         obj%ntpv = 3
         obj%tcodes(obj%ntpv) = ppavo_runstrpz
      endif

!     Get 'P' option - pass prestack data along with attributes.
      obj%pass_opt = 0
      call bavel_get_iparm(obj, 'pass_opt', obj%pass_opt) 
      obj%maxdtrz = max0(obj%maxdtrz, obj%ntpv*obj%num_vels)
      if (obj%pass_opt .ne. 0) then
         obj%maxdtrz = obj%maxdtrz + obj%ntpv*obj%num_vels + 1
      else
         obj%maxdtrz = obj%maxdtrz + obj%ntpv*obj%num_vels
      endif

!     Get 'I' option - include curved raypaths.
      obj%incr_opt = 0
      call bavel_get_iparm(obj, 'incr_opt', obj%incr_opt)

!     Get interval velocity file name (if necessary).
      obj%use_ivel = 0
      call bavel_get_iparm(obj, 'use_ivel', obj%use_ivel)
      if (obj%use_ivel .eq. 1) then
         call bavel_get_cparm(obj, 'ivelname', obj%ivelname, nchars)
         call velfile_create(obj%ivel_obj, obj%ivelname,&
     &      obj%ivel_nhx, obj%ivel_nhy, error, msg,     &
     &      obj%ivel_nmosign, obj%ivel_nmoexp)
      end if  

!     Get 'H' option - perform horizontal divergence correction.
      obj%hdiv_opt = 0
      call bavel_get_iparm(obj, 'hdiv_opt', obj%hdiv_opt)   

!     Get 'V' option - perform vertical divergence correction.
      obj%vdiv_opt = 0
      call bavel_get_iparm(obj, 'vdiv_opt', obj%vdiv_opt)   

!     Get 'N' option - suppress NMO stretch correction.
      obj%nmsc_opt = 1
      call bavel_get_iparm(obj, 'nmsc_opt', obj%nmsc_opt)
      if (obj%method_b .eq. 3) obj%nmsc_opt = 1

!     Get 'T' option - variable trace weighting option.
      obj%trfo_opt = 0
      call bavel_get_iparm(obj, 'trfo_opt', obj%trfo_opt)

!     Set minimum and maximum CDP trace number to process.
      obj%mintr = 1
      obj%maxtr = obj%maxdtrz

!     Set target time.
      obj%targtime = 0.0
      obj%ovrv_opt  = 0

!     Set taperwin length (msec).
      obj%itaperwin = 50
      call bavel_get_iparm(obj, 'twin_tpr', obj%itaperwin)

!     Set tau-p flag and aperture.
      obj%itpflg = 0
      obj%itpapt = 100

!     Set first live value scaling.
      obj%ifsflag = 0
      obj%fscale  = 1.0

!     Set rotation phase angle.
      obj%phas_rot = 0
      call bavel_get_rparm(obj, 'phas_rot', obj%phas_rot)

!     Set rotation angle in A-B plane.
      obj%rotab = 0.0

!     Set filter defaults.
      obj%ihilbert = 43
      obj%ilen     = 12
      if (obj%sampratz .le. 3.0) obj%ilen = 10
      if (obj%sampratz .le. 2.0) obj%ilen = 8
      if (obj%sampratz .le. 1.0) obj%ilen = 4
      obj%fs                = 1000.0 / obj%sampratz
      obj%nfilts            = 1
      pctw(obj%nfilts)      = 10
      cut(obj%nfilts)       = obj%fs / 2
      pctc(obj%nfilts)      = 0
      obj%lfltr(obj%nfilts) = 50
      if (obj%sampratz .gt. 1.0) obj%lfltr(obj%nfilts) = 40

!     Generate the filters.
      do 590 ifilt = 1, obj%nfilts
         nh = obj%lfltr(ifilt)
         w0t = 2.0 * pi * obj%w0(ifilt) / obj%fs
         xtemp = 2d0 - dcos(2d0*pi*cut(ifilt)/obj%fs)
         pole = xtemp - dsqrt(xtemp**2-1)
         call ppavo_vzero (obj%h1, 1, 2*mmxh+1)
         call ppavo_vzero (obj%h2, 1, 2*mmxh+1)
         call ppavo_vzero (pcorr, 1, 1000)
         call ppavo_vzero (pcvect, 1, 1000)
         if (obj%wtypes(ifilt) .eq. 'rick') then
            call ppavo_sarkft(obj%h1, obj%h2, pcorr,&
     &                      pcvect, pwork, mmxh, nh, w0t,&
     &                      pctw(ifilt), pctc(ifilt), pole,& 
     &                      ks, ierror)
         else
!!!            fttype = 'cr1d'
            mnffti(1) = obj%numsmpz
!!!            call ppavo_ftsize(fttype, mnffti, mnffto, ierror)
            mnffto(1) = max(mnffti(1), 512)
            mnffto(1) = 8
            do while (mnffto(1) .le. mnffti(1))
               mnffto(1) = mnffto(1) * 2
            end do
!            mnffto(1) = 1920
            ns = mnffto(1)
            call ppavo_sabwave(obj%rtempz, obj%ztempz, ns, pcorr, pcvect,&
     &                       mmxh, nh, obj%fs, apars)
            call ppavo_sabnft(obj%h1, obj%h2, pcorr, pcvect,&
     &                      pwork, mmxh, nh, pctw(ifilt),&
     &                      pctc(ifilt), pole, ks, ierror)
         end if
         if (ierror .ne. 0) then
            write (obj%print_lun, *) 'B_AVEL: Error in filter design.'
            ntr = FATAL_ERROR
            return
         end if
590   continue

!     Compute interpolation coefficients.
      if (obj%movefg .gt. 0) then
!        Update the scratch memory buffer for coefficient.
         obj%ilen2 = (obj%ilen-1)/2 + 1
         call ppavo_mcofgn(obj%ilen, bw(obj%ilen2), rw, g, ff, a, obj%icoef)
      endif

!     Update parameter array.
      obj%nparm = 15
      obj%parm(1) = obj%ihilbert
      obj%parm(2) = obj%method_a
      obj%parm(3) = obj%method_b
      obj%parm(4) = obj%nmsc_opt
      obj%parm(5) = obj%hdiv_opt
      obj%parm(6) = obj%vdiv_opt
      obj%parm(7) = obj%vel_zero
      obj%parm(8) = obj%pdat_opt - 1
      obj%parm(9) = obj%len_oflt
      if (obj%nrma_opt .eq. 0) then
         obj%parm(10) = 0
      else
         obj%parm(10) = obj%agc_wina/obj%sampratz
         if (obj%parm(10) .lt. 1) obj%parm(10) = 1
         if (obj%nrma_opt .gt. 1) obj%parm(10) = -obj%parm(10)
      endif
      if (obj%nrmb_opt .eq. 0) then
         obj%parm(11) = 0
      else
         obj%parm(11) = obj%agc_winb/obj%sampratz
         if (obj%parm(11) .lt. 1) obj%parm(11) = 1
         if (obj%nrmb_opt .gt. 1) obj%parm(11) = -obj%parm(11)
      endif
      obj%parm(12) = obj%itaperwin/obj%sampratz
      obj%parm(13) = obj%phas_rot
      obj%parm(14) = obj%rotab*10
      obj%parm(15) = obj%errs_opt

      obj%numtr = obj%maxdtrz

      obj%itooltype = ppavo_ensemblepz

      obj%initialized = .true.

      end subroutine bavel_init


!!---------------------------- bavel_work -----------------------------------!!
!!---------------------------- bavel_work -----------------------------------!!
!!---------------------------- bavel_work -----------------------------------!!

      subroutine bavel_work (obj,nhd,ntr,hd,tr)
      type(bavel_struct),intent(inout) :: obj            ! arguments
      integer,          intent(inout) :: nhd              ! arguments
      integer,          intent(inout) :: ntr              ! arguments
      double precision, intent(inout) :: hd(:,:)          ! arguments
      real,             intent(inout) :: tr(:,:)          ! arguments

! --> Insert declarations of local variables.

!     Local variables.

      integer          :: ivel

      integer          :: itrace
      integer          :: iflv
      integer          :: illv
      integer          :: newflv
      integer          :: newllv
      integer          :: nlv
      integer          :: ifsmp
      integer          :: ilsmp
      integer          :: ifsa
      integer          :: ilsa
      integer          :: nsa
      integer          :: isz
      integer          :: mmxh
      integer          :: ifndx
      integer          :: fold
      integer          :: min_fold
      integer          :: ndxout
      integer          :: ntpc
      integer          :: iticd
      integer          :: otr1
      integer          :: runspos
      integer          :: iveltr
      integer          :: desttr
      integer          :: nstt
      integer          :: nsd
      integer          :: ifirst
      integer          :: ilast




      integer          :: yes
      integer          :: no




      integer          :: i, j         
      integer          :: jerror

  
      logical          :: runsflag
      logical          :: do_runs
      logical          :: got_a
      logical          :: got_b

      real             :: rcdp
      real             :: t0
      real             :: t0s
      real             :: tsamp
      real             :: vinc
      real             :: fsc1
      real             :: offset
      real             :: live_s
      real             :: live_e
      real             :: trc_fold






      integer            ::       maxpicks 


  



      integer          :: nvelscr
      integer          :: nwih








      character(len=4) :: veltype
      character(len=4)   :: veltype_last
      character(len=128) :: message

      integer            :: errflag, status2, numvels
      integer            :: nxbins, nybins
      integer            :: nhx, nhy
      real, pointer      :: xbins(:)
      real, pointer      :: ybins(:)

      parameter ( isz = 9 )
      parameter ( mmxh = 100 )
      parameter ( min_fold = 4 )
      parameter ( yes = 1 )
      parameter ( no = 0 )
  
! --> Insert code for processing logic.
      jerror = 0

!     No action required for cleanup.   
      if (obj%cleanupz) then

         write(obj%print_lun,*) 'B_AVEL: Memory deallocation.'

         if(associated(obj%hdnmo)) then
!            write(obj%print_lun,*) '  Deallocating nmo header storage.'
            deallocate(obj%hdnmo)
         end if

         if(associated(obj%velscr)) then
!            write(obj%print_lun,*) '  Deallocating velocity scratch storage.'
            deallocate(obj%velscr)
         end if

         if(associated(obj%xy_svel)) then
!            write(obj%print_lun,*) '  Deallocating stacking velocity storage.'
            deallocate(obj%xy_svel)
         end if

         if(associated(obj%xy_ivel)) then
!            write(obj%print_lun,*) '  Deallocating interval velocity storage.'
            deallocate(obj%xy_ivel)
         end if

         if(associated(obj%sloths)) then
!            write(obj%print_lun,*) '  Deallocating stacking sloth storage.'
            deallocate(obj%sloths)
         end if

         if(associated(obj%slothi)) then
!            write(obj%print_lun,*) '  Deallocating interval sloth storage.'
            deallocate(obj%slothi)
         end if

         if(associated(obj%slothsx)) then
!            write(obj%print_lun,*) '  Deallocating stacking sloth storage.'
            deallocate(obj%slothsx)
         end if

         if(associated(obj%slothix)) then
!            write(obj%print_lun,*) '  Deallocating interval sloth storage.'
            deallocate(obj%slothix)
         end if

         if(associated(obj%vel_avg)) then
!            write(obj%print_lun,*) '  Deallocating velocity sum storage.'
            deallocate(obj%vel_avg)
         end if

         if(associated(obj%vel_interp)) then
!            write(obj%print_lun,*) '  Deallocating velocity interp storage.'
            deallocate(obj%vel_interp)
         end if

         if(associated(obj%itrsave)) then
!            write(obj%print_lun,*) '  Deallocating input trace save storage.'
            deallocate(obj%itrsave)
         end if

         if(associated(obj%otrsave)) then
!            write(obj%print_lun,*) '  Deallocating output trace save storage.'
            deallocate(obj%otrsave)
         end if

         if(associated(obj%trcoef)) then
!            write(obj%print_lun,*) '  Deallocating trace coefficient storage.'
            deallocate(obj%trcoef)
         end if

         if(associated(obj%nmot)) then
!            write(obj%print_lun,*) '  Deallocating normal moveout storage.'
            deallocate(obj%nmot)
         end if

         if(associated(obj%rsum)) then
!            write(obj%print_lun,*) '  Deallocating running sums storage.'
            deallocate(obj%rsum)
         end if

         if(associated(obj%rscratch)) then
!            write(obj%print_lun,*) '  Deallocating real scratch storage.'
            deallocate(obj%rscratch)
         end if

         if(associated(obj%dscratch)) then
!            write(obj%print_lun,*) '  Deallocating double scratch storage.'
            deallocate(obj%dscratch)
         end if

         if(associated(obj%pra)) then
!            write(obj%print_lun,*) '  Deallocating previous A storage.'
            deallocate(obj%pra)
         end if

         if(associated(obj%prb)) then
!            write(obj%print_lun,*) '  Deallocating previous B storage.'
            deallocate(obj%prb)
         end if

         if(associated(obj%runs)) then
!            write(obj%print_lun,*) '  Deallocating RUNS statistic storage.'
            deallocate(obj%runs)
         end if

         obj%gmem_flag = 0
         nullify(obj%hdnmo)
         nullify(obj%velscr)
         nullify(obj%svel_nmo)
         nullify(obj%ivel_nmo)
         nullify(obj%svel_obj)
         nullify(obj%ivel_obj)
         nullify(obj%xy_svel)
         nullify(obj%xy_ivel)
         nullify(obj%sloths)
         nullify(obj%slothi)
         nullify(obj%slothsx)
         nullify(obj%slothix)
         nullify(obj%vel_avg)
         nullify(obj%vel_interp)
         nullify(obj%itrsave)
         nullify(obj%otrsave)
         nullify(obj%trcoef)
         nullify(obj%nmot)
         nullify(obj%rsum)
         nullify(obj%rscratch)
         nullify(obj%dscratch)
         nullify(obj%pra)
         nullify(obj%prb)
         nullify(obj%runs)

         return
      end if


!     No action required if no traces passed.
      if (ntr .eq. 0) return

!     Preliminary initialization.
      tsamp    = obj%sampratz * 0.001
      ifsa     = obj%ista / obj%sampratz + 1
      ilsa     = obj%ieta / obj%sampratz + 1
      nsa      = ilsa - ifsa + 1
      t0       = 0.0
      t0s      = (ifsa - 1) * tsamp + t0
      got_a    = .false.
      got_b    = .false.
      do_runs  = .false.  
      runsflag = obj%errs_opt .eq. 4

      if (obj%gmem_flag .eq. 0) then

!        Allocate memory.
         obj%gmem_flag = 1

         write(obj%print_lun,*) 'B_AVEL: Memory allocation.'

!         write(obj%print_lun,*) '  Allocating nmo header storage.'
         obj%l_hdnmo = obj%nthz
         allocate (obj%hdnmo(obj%l_hdnmo,1))

!         write(obj%print_lun,*) '  Allocating velocity scratch storage.'
         obj%l_velscr = obj%numsmpz
         allocate (obj%velscr(obj%l_velscr,1))

!         write(obj%print_lun,*) '  Allocating stacking velocity storage.'
         obj%l_xy_svel = obj%numsmpz
         allocate (obj%xy_svel(obj%l_xy_svel))

!         write(obj%print_lun,*) '  Allocating interval velocity storage.'
         obj%l_xy_ivel = obj%numsmpz
         allocate (obj%xy_ivel(obj%l_xy_ivel))

!         write(obj%print_lun,*) '  Allocating stacking sloth storage.'
         obj%l_sloths = obj%numsmpz + 2
         allocate (obj%sloths(obj%l_sloths))

!         write(obj%print_lun,*) '  Allocating interval sloth storage.'
         obj%l_slothi = obj%numsmpz + 2
         allocate (obj%slothi(obj%l_slothi))

!         write(obj%print_lun,*) '  Allocating stacking sloth storage.'
         obj%l_slothsx = obj%numsmpz + 2
         allocate (obj%slothsx(obj%l_slothsx))

!         write(obj%print_lun,*) '  Allocating interval sloth storage.'
         obj%l_slothix = obj%numsmpz + 2
         allocate (obj%slothix(obj%l_slothix))

!         write(obj%print_lun,*) '  Allocating velocity sum storage.'
         obj%l_vel_avg = obj%numsmpz + 2
         allocate (obj%vel_avg(obj%l_vel_avg))

!         write(obj%print_lun,*) '  Allocating velocity interpolated storage.'
         obj%l_vel_interp = obj%numsmpz + 2
         allocate (obj%vel_interp(obj%l_vel_interp))

!         write(obj%print_lun,*) '  Allocating input trace save storage.'
         obj%l_itrsave = obj%numsmpz + (obj%ilen * 2)
         allocate (obj%itrsave(obj%l_itrsave))

!         write(obj%print_lun,*) '  Allocating output trace save storage.'
         obj%ntpv = 2 + obj%errs_opt
         if (runsflag) obj%ntpv = 3
         obj%l_otrsave = nsa * obj%num_vels * obj%ntpv
         allocate (obj%otrsave(obj%l_otrsave))

!         write(obj%print_lun,*) '  Allocating trace coefficient storage.'
         obj%l_trcoef = obj%ilen * obj%numsmpz * 2
         allocate (obj%trcoef(obj%l_trcoef))

!         write(obj%print_lun,*) '  Allocating normal moveout storage.'
         obj%l_nmot = obj%numsmpz
         allocate (obj%nmot(obj%l_nmot))

!         write(obj%print_lun,*) '  Allocating running sums storage.'
         obj%l_rsum = nsa * isz
         allocate (obj%rsum(obj%l_rsum))

!         write(obj%print_lun,*) '  Allocating real scratch storage.'
         obj%l_rscratch = (obj%numsmpz * 2) + obj%ilen
         allocate (obj%rscratch(obj%l_rscratch))

!         write(obj%print_lun,*) '  Allocating double scratch storage.'
         obj%l_dscratch = (obj%numsmpz * 2) + obj%ilen
         allocate (obj%dscratch(obj%l_dscratch))

         if (runsflag) then

!            write(obj%print_lun,*) '  Allocating previous A storage.'
            obj%l_pra = obj%numsmpz
            allocate (obj%pra(obj%l_pra))

!            write(obj%print_lun,*) '  Allocating previous B storage.'
            obj%l_prb = obj%numsmpz
            allocate (obj%prb(obj%l_prb))

!            write(obj%print_lun,*) '  Allocating RUNS statistic storage.'
            obj%l_runs = nsa * 3
            allocate (obj%runs(obj%l_runs))

         end if

         call ppavo_vzero (obj%vel_avg,      1, obj%l_vel_avg)
         call ppavo_vzero (obj%vel_interp,   1, obj%l_vel_interp)

         if(obj%avf_comp .ne. 0) then

            obj%ndptvel = obj%ndpt
            obj%nsmooth = 2 * nint(0.5 * obj%avf_slen / obj%dt) + 1
            call velio_scan_alloc(obj%svelname, numvels, status2,&
     &         message, nhx=nhx, nhy=nhy, maxpicks=maxpicks,&
     &         xbins=xbins, ybins=ybins, nxbins=nxbins, nybins=nybins)
            if(status2 .ne. VELIO_OK) then
               call pc_error('Error scanning velocity file'//obj%svelname)
               ntr = FATAL_ERROR
               return
            end if

            call velio_open_read(obj%velio_obj, obj%svelname, numvels,&
     &         status2, message)
            if(status2 .ne. VELIO_OK) then
               call pc_error('Error opening velocity file'//obj%svelname)
               ntr = FATAL_ERROR
               return
            end if

            obj%vel_avg = 0.0
            do ivel = 1, numvels
               call bavel_read_velocity (obj, &
     &            maxpicks, obj%vel_interp, veltype, errflag)
               if (errflag /= 0) then
                  call pc_error ('Problem in velocity at location X=',&
     &               obj%xlast,', Y=',obj%ylast)
                  ntr = FATAL_ERROR
                  return
               end if
               if (ivel > 1) then
                  if (veltype_last=='VTNM' .or. veltype_last=='VTRM') then
                     if (veltype == 'VTIN') errflag = 1
                  else if (veltype=='VTNM' .or. veltype=='VTRM') then
                     errflag = 1
                  end if
                  if (errflag /= 0) then
                     call pc_error ('Velocity type '//veltype//' different &
     &from type of previous function.')
                     call pc_error ('Problem in velocity at location X=',&
     &                  obj%xlast,', Y=',obj%ylast)
                     ntr = FATAL_ERROR
                     return
                  end if
               end if
               veltype_last = veltype
               obj%vel_avg = obj%vel_avg + obj%vel_interp
            end do
            obj%vel_avg = obj%vel_avg / numvels
            write(obj%print_lun, *)
            if (veltype == 'VTIN') then
               write(obj%print_lun, *) 'Interval velocities were input'
            else
               write(obj%print_lun, *) 'Stacking or RMS velocities were input'
            end if
            if ((.not. obj%opt_sr) .or. (veltype == 'VTIN')) then
               write(obj%print_lun, *) 'Composite input velocity @',&
     &            stride*obj%dt, 's'
               write(obj%print_lun, '(5(F7.2,I6))')&
     &            ((j-1)*obj%dt,nint(obj%vel_avg(j)),j=1,obj%ndptvel,stride)
               write(obj%print_lun, *)
            end if
            call bavel_convert_vels (obj, obj%vel_avg, veltype,&
     &         obj%vel_interp, errflag)
            if (errflag /= 0) then
               ntr = FATAL_ERROR
               return
            end if

            call velio_close(obj%velio_obj)

         end if

      else

!        Clear out allocated memory.
         call ppavo_vzero (obj%hdnmo(1:,1),  1, obj%l_hdnmo)
         call ppavo_vzero (obj%velscr(1:,1), 1, obj%l_velscr)
         call ppavo_vzero (obj%xy_svel,      1, obj%l_xy_svel)
         call ppavo_vzero (obj%xy_ivel,      1, obj%l_xy_ivel)
         call ppavo_vzero (obj%sloths,       1, obj%l_sloths)
         call ppavo_vzero (obj%slothi,       1, obj%l_slothi)
         call ppavo_vzero (obj%slothsx,      1, obj%l_slothsx)
         call ppavo_vzero (obj%slothix,      1, obj%l_slothix)
         call ppavo_vzero (obj%itrsave,      1, obj%l_itrsave)
         call ppavo_vzero (obj%otrsave,      1, obj%l_otrsave)
         call ppavo_vzero (obj%trcoef,       1, obj%l_trcoef)
         call ppavo_vzero (obj%nmot,         1, obj%l_nmot)
         call ppavo_vzero (obj%rscratch,     1, obj%l_rscratch)
         call ppavo_vzero (obj%dscratch,     1, obj%l_dscratch)
         call ppavo_vzero (obj%rsum,         1, obj%l_rsum)
         if (runsflag) then
            call ppavo_vzero (obj%pra,       1, obj%l_pra)
            call ppavo_vzero (obj%prb,       1, obj%l_prb)
            call ppavo_vzero (obj%runs,      1, obj%l_runs)
         end if
      end if

!     Identify and count velocity traces.
      iveltr = 0
      desttr = 1
      do 140 itrace = 1, ntr
         iticd = hd(ppavo_trc_typez, itrace)

!        Save first velocity trace.
         if (iticd .eq. ppavo_velpz) then
            if (iveltr .eq. 0) then
               call ppavo_vmove(tr(1:,itrace), 1,&
     &            obj%xy_svel(1:), 1, obj%numsmpz)
            end if
            iveltr = desttr

!        Remember 'A' traces of previous iterations.
         else if (iticd .eq. ppavo_atrpz .and. runsflag) then
            call ppavo_vmove(tr(1:,itrace), 1, obj%pra, 1, obj%numsmpz)
            got_a = .true.
            goto 140

!        Remember 'B' traces of previous iterations.   
         else if (iticd .eq. ppavo_btrpz .and. runsflag) then
            call ppavo_vmove(tr(1:,itrace), 1, obj%prb, 1, obj%numsmpz)
            got_b = .true.
            goto 140

!        Collapse all 'junk' traces out of gather.
         else if (iticd .ne. 1) then
            goto 140
         end if

!        Save front end velocity traces.
         if (itrace .gt. desttr) then
            call ppavo_vmove(tr(1:,itrace), 1, tr(1:,desttr), 1, obj%numsmpz)
            call ppavo_vmove(hd(1:,itrace), 1, hd(1:,desttr), 1, obj%nthz)
         end if
         desttr = desttr + 1
140   continue
      ntr = desttr - 1

!     Retrieve stacking velocity function from file if none is found.
      if (iveltr .eq. 0) then

!        Set desired velocity attributes.        
         rcdp = hd(ppavo_cdpz, 1)

         nvelscr = 1

!        Pass original CPS headers to NMO module.
         nwih = obj%nthz - ppavo_nhdrpz
         obj%hdnmo(1:nwih,1) = hd(ppavo_nhdrpz+1:obj%nthz,1)
         call nmo(obj%svel_nmo, nvelscr, obj%hdnmo, obj%velscr)
         call ppavo_vmove(obj%velscr(1:,1), 1, obj%xy_svel, 1, obj%numsmpz)

      end if

!     Retrieve interval velocity function from file, if necessary.
      if(obj%use_ivel .eq. 1) then
         
         rcdp = hd(ppavo_cdpz, 1)

         nvelscr = 1

!        Pass original CPS headers to NMO module.
         nwih = obj%nthz - ppavo_nhdrpz
         obj%hdnmo(1:nwih,1) = hd(ppavo_nhdrpz+1:obj%nthz,1)
         call nmo(obj%ivel_nmo, nvelscr, obj%hdnmo, obj%velscr)
         call ppavo_vmove(obj%velscr(1:,1), 1, obj%xy_ivel, 1, obj%numsmpz)

      endif

!     Additional initialization.
      ndxout = 0
      ifndx = 1
      ivel = 0
      do_runs = got_a .and. got_b

!     Set first and last live values for every trace in the gather
!     (cannot depend upon header values for this, since synthetic 
!     data may not have been muted).
      do 80 itrace = iveltr+1, ntr
         iticd = hd(ppavo_trc_typez, itrace)
         if (iticd .ne. ppavo_livepz) goto 80
         iflv = 1
         illv = obj%numsmpz
         nlv = illv - iflv + 1
         call ppavo_trlive (tr(iflv:,itrace), nlv, newflv, newllv)
         illv = iflv + newllv - 1
         iflv = iflv + newflv - 1
         hd(ppavo_live_sz, itrace) = (iflv - 1) * obj%sampratz
         hd(ppavo_live_ez, itrace) = (illv - 1) * obj%sampratz
80    continue

!     Begin next velocity function.
100   ivel = ivel + 1
      vinc = (ivel - obj%velf_ref) * obj%velf_inc

      call ppavo_vzero (obj%rsum(1:), 1, obj%l_rsum)
      if (do_runs) then
         call ppavo_vzero (obj%runs(1:), 1, obj%l_runs)
      end if

!     Convert to stacking sloth (v**-2) and
!     interval sloth if option 'i' is given.
      call ppavo_savelc (obj%xy_svel(1:), obj%sloths(1:),&
     &                  obj%slothi(1:), obj%numsmpz,&
     &                  obj%len_sflt, obj%incr_opt, t0, vinc, tsamp,& 
     &                  obj%ovrv_opt, obj%targtime, jerror )
      if(obj%avf_comp .ne. 0) then
         call ppavo_savelc (obj%vel_avg(1:), obj%slothsx(1:),&
     &                  obj%slothix(1:), obj%numsmpz,&
     &                  obj%len_sflt, obj%incr_opt, t0, vinc, tsamp,& 
     &                  obj%ovrv_opt, obj%targtime, jerror )
      end if
!      write(obj%print_lun,*) 'cdp = ', hd(ppavo_cdpz, 1)
!      do i = 1, obj%numsmpz
!      write(obj%print_lun,*) 'velslo = ',i,&
!      &obj%xy_svel(i),obj%sloths(i),obj%slothi(i)
!     end do
      if (jerror .gt. 0) then
         write(obj%print_lun,*) 'CDP ',hd(ppavo_cdpz, 1),&
     &      ': The ',jerror,'-th velocity sample is invalid(',&
     &      obj%xy_svel(jerror),').'
         ntr = FATAL_ERROR
         return
      end if

!     Compute slothi from interval velocity function.
      if(obj%use_ivel .eq. 1) then
         do 106 i=1, obj%numsmpz
            obj%slothi(i) = 1.0 / obj%xy_ivel(i)**2
106      continue
      end if

!     Operate on each input trace.
      ifirst = obj%numsmpz
      ilast  = 1
      do 200 itrace = iveltr+1, ntr
         iticd = hd(ppavo_trc_typez, itrace)
         trc_fold  = hd(ppavo_trc_foldz, itrace)

         if (obj%trfo_opt .eq. 0) trc_fold = 1.0
         if (trc_fold .le. 0.0) trc_fold = 1.0
         if (iticd .ne. ppavo_livepz) goto 200

!        Copy trace to the save area.
         iflv   = hd(ppavo_live_sz, itrace) / obj%sampratz + 1.5
         illv   = hd(ppavo_live_ez, itrace) / obj%sampratz + 1.5
         offset = hd(ppavo_offsetz, itrace)

         if(obj%angfmt .gt. 0) then
!           Replace call to get central angle value from trace header.
            offset = hd(ppavo_avo_anglez, itrace)
         end if

         call ppavo_vmove(tr(1:, itrace), 1, obj%itrsave(1+obj%ilen:),&
     &      1, obj%numsmpz)

!        Scale this trace, based on first live trace
!        unless suppression option is specified.
         if (obj%scas_opt .eq. 1) then
            call ppavo_sahcsc (obj%itrsave(1+obj%ilen:),&
     &                        illv, iflv, obj%fscale, obj%ifsflag)
         end if

!        NMO correct the data if necessary, otherwise just copy it.
         ifsmp = iflv
         ilsmp = illv
         if(obj%movefg .ne. 0) then
            call ppavo_vzero (obj%itrsave(1:), 1, obj%ilen)
            call ppavo_vzero (obj%itrsave(1+obj%ilen+obj%numsmpz:), 1, obj%ilen)

            call ppavo_samvot (obj%itrsave(1:),&
     &         obj%sloths(1:), obj%icoef(1:), obj%nmot(1:),&
     &         obj%trcoef(1:), obj%rscratch(1:),&
     &         obj%numsmpz, obj%ilen, ifsa, ilsa, offset, iflv,&
     &         t0, tsamp, obj%itpflg, ifsmp, ilsmp)

            if (obj%movefg .eq. 2) then
               call ppavo_vmove(obj%nmot(1:), 1,&
     &                          tr(1:, itrace), 1, obj%numsmpz)
            end if
         else
            call ppavo_vmove(obj%itrsave(1+obj%ilen:), 1,&
     &                       obj%nmot(1:), 1, obj%numsmpz)
         end if

!        Collect the runs statistic.
         if (do_runs) then
            if(obj%avf_comp .eq. 0) then
               call ppavo_saruns (obj%nmot(ifsa:),&
     &            obj%pra(ifsa:), obj%prb(ifsa:),&
     &            obj%sloths(ifsa:), obj%runs(1:),&
     &            nsa, offset, obj%alim1, obj%alim2, tsamp, t0s)
            else
               call ppavo_saruns (obj%nmot(ifsa:),&
     &            obj%pra(ifsa:), obj%prb(ifsa:),&
     &            obj%slothsx(ifsa:), obj%runs(1:),&
     &            nsa, offset, obj%alim1, obj%alim2, tsamp, t0s)
            end if
         end if  

!        Collect running sums.
         if(obj%avf_comp .eq. 0) then
            call ppavo_sahcic (obj%rsum(1:), obj%nmot(1:),&
     &         obj%sloths(1:), obj%slothi(1:), obj%dscratch(1:), isz,&
     &         ifsmp, ilsmp, ifsa, ilsa, t0, tsamp,&
     &         offset, obj%alim1, obj%alim2, obj%angfmt, trc_fold)
         else
            call ppavo_sahcic (obj%rsum(1:), obj%nmot(1:),&
     &         obj%slothsx(1:), obj%slothix(1:), obj%dscratch(1:), isz,&
     &         ifsmp, ilsmp, ifsa, ilsa, t0, tsamp,&
     &         offset, obj%alim1, obj%alim2, obj%angfmt, trc_fold)
         end if

!        Store the earliest and latest sample collected.
         if (ifsmp .lt. ifirst) ifirst = ifsmp
         if (ilsmp .gt. ilast)  ilast  = ilsmp

200   continue

!     Collection is complete, now do the AVO analysis.
      nstt = ifirst - ifsa + 1
      nsd  = ilast  - ifsa + 1
      if (nstt .le. 1) nstt = 1
      if (nsd  .ge. nsa) nsd = nsa

!     Clear output trace storage ((2*nsa) instead of just (1*nsa)).
      call ppavo_vzero (obj%otrsave(1+ndxout:), 1, (2*nsa))
      if (nsd .gt. nstt) then
         call ppavo_sahci (obj%rsum(1:), obj%h1, obj%h2,&
     &      obj%sloths(ifsa:),&
     &      obj%slothi(ifsa:),&
     &      obj%parm, obj%otrsave(1+ndxout:),&
     &      nsa, mmxh, obj%nparm, nstt, nsd,&
     &      obj%lfltr(ifndx), isz, t0s, tsamp, obj%avo_stab)
      end if
      ifirst = nstt + ifsa - 1
      ilast  = nsd  + ifsa - 1

!     Calculate the runs statistic.
      if (do_runs .and. ilast .gt. ifirst) then
         runspos = 3
         call ppavo_runsc (obj%runs(1:),&
     &     obj%otrsave(1+ndxout:), nsa, runspos)
      end if

!     Reset ndxout for the next velocity function.
      ndxout = nsa * ivel * obj%ntpv

!     Now decide whether we need to go back.
      if (ivel .lt. obj%num_vels) goto 100

!     If there were no velocity traces, put one in now 
!     (this is necessary only in the 'pass_opt' option).
      if (iveltr .eq. 0 .and. obj%pass_opt .ne. 0) then
         do itrace = ntr, 1, -1
            call ppavo_vmove(tr(1:, itrace), 1,&
     &                      tr(1:, itrace+1), 1, obj%numsmpz)
            call ppavo_vmove(hd(1:, itrace), 1,&
     &                      hd(1:, itrace+1), 1, obj%nthz)
         end do
         call ppavo_vmove(obj%xy_svel(1:), 1, tr(1:,1), 1, obj%numsmpz)
         hd(ppavo_trc_typez, 1) = ppavo_velpz
         ntr = ntr + 1
      end if

!     Set number of output traces.
      otr1 = 1
      if (obj%pass_opt .ne. 0) otr1 = ntr + 1
      fold = ntr - iveltr
      ntpc = obj%num_vels * obj%ntpv
      ntr = otr1 + ntpc - 1

!     Copy output traces to output area.
      do 500 itrace = 0, ntpc
         ndxout = (nsa * itrace) + 1
         call ppavo_vzero(tr(1:, otr1+itrace), 1, obj%numsmpz)
         call ppavo_vmove(obj%otrsave(ndxout:), 1,&
     &      tr(ifsa:, otr1+itrace), 1, nsa)
         call ppavo_vmove(hd(1:, 1), 1,&
     &      hd(1:, otr1+itrace), 1, obj%nthz)
500   continue

!     Set new trace headers.
      itrace = 0
      do 1400 ndxout = 1, otr1-1
         iticd = hd(ppavo_trc_typez, ndxout)
         fsc1  = hd(ppavo_amp_normz, ndxout)
         if (fsc1 .eq. 0.0) fsc1 = 1.0
         fsc1 = fsc1 * obj%fscale

!        Filter the velocity trace, if present.
         if (iticd .eq. ppavo_velpz) then

!           Filter first velocity trace only.
            if (itrace .eq. 0) then
               itrace = 1
               call ppavo_samodv (tr(1:, ndxout),&
     &            obj%sloths(1:), obj%numsmpz, obj%len_sflt)
               hd(ppavo_trc_foldz, ndxout) = iveltr - 1
               if (hd(ppavo_trc_foldz, ndxout) .lt. 0) then
                  hd(ppavo_trc_foldz, ndxout) = 0
               end if
            end if
            hd(ppavo_amp_normz, ndxout) = 1.0
         else
            hd(ppavo_amp_normz, ndxout) = fsc1
         end if

         hd(ppavo_dom_freqz, ndxout) = obj%w0(1)
         hd(ppavo_end_ensz,  ndxout) = ppavo_nlastpz
         hd(ppavo_seq_noz,   ndxout) = ndxout
1400  continue

!     Set trace attributes.
      do 1600 ivel = 1, obj%num_vels
         vinc = (ivel - obj%velf_ref) * obj%velf_inc
         do 1500 itrace = 1, obj%ntpv
            ndxout = (ivel - 1) * obj%ntpv + itrace + otr1 - 1
            live_s = (ifirst - 1) * obj%sampratz + t0
            live_e = (ilast  - 1) * obj%sampratz + t0
            hd(ppavo_trc_typez, ndxout)  = obj%tcodes(itrace)
            if (ilast .lt. ifirst) hd(ppavo_trc_typez, ndxout) = 2
            hd(ppavo_seq_noz,   ndxout) = ndxout
            hd(ppavo_end_ensz,  ndxout) = ppavo_nlastpz
            hd(ppavo_live_sz,   ndxout) = live_s
            hd(ppavo_live_ez,   ndxout) = live_e
            hd(ppavo_full_sz,   ndxout) = live_s + obj%itaperwin
            hd(ppavo_full_ez,   ndxout) = live_e - obj%itaperwin
            hd(ppavo_trc_foldz, ndxout) = fold
            hd(ppavo_dom_freqz, ndxout) = obj%w0(1)
            hd(ppavo_amp_normz, ndxout) = fsc1
            hd(ppavo_offsetz, ndxout)   = vinc
1500     continue
1600  continue

!     Set the last trace to be end of ensemble.
      hd(ppavo_end_ensz, ntr) = ppavo_lasttrpz

      return

      end subroutine bavel_work


!!------------------------- bavel_init_parms --------------------------------!!
!!------------------------- bavel_init_parms --------------------------------!!
!!------------------------- bavel_init_parms --------------------------------!!

      subroutine bavel_init_parms (obj)
      type(bavel_struct),intent(inout) :: obj              ! arguments

      obj%f_svelname  = .false.
      obj%f_num_vels  = .false.
      obj%f_velf_ref  = .false.
      obj%f_velf_inc  = .false.
      obj%f_angl_max  = .false.
      obj%f_angl_rng  = .false.
      obj%f_avf_comp  = .false.
      obj%f_len_sflt  = .false.
      obj%f_len_oflt  = .false.
      obj%f_stim_win  = .false.
      obj%f_etim_win  = .false.
      obj%f_twin_tpr  = .false.
      obj%f_avo_stab  = .false.
      obj%f_method_a  = .false.
      obj%f_method_b  = .false.
      obj%f_pdat_opt  = .false.
      obj%f_vel_zero  = .false.
      obj%f_flt_type  = .false.
      obj%f_gath_opt  = .false.
      obj%f_nrma_opt  = .false.
      obj%f_nrmb_opt  = .false.
      obj%f_agc_wina  = .false.
      obj%f_agc_winb  = .false.
      obj%f_scas_opt  = .false.
      obj%f_pass_opt  = .false.
      obj%f_incr_opt  = .false.
      obj%f_hdiv_opt  = .false.
      obj%f_vdiv_opt  = .false.
      obj%f_nmsc_opt  = .false.
      obj%f_trfo_opt  = .false.
      obj%f_phas_rot  = .false.
      obj%f_ric_freq  = .false.
      obj%f_bp_freqs  = .false.
      obj%f_errs_opt  = .false.
      obj%f_use_ivel  = .false.
      obj%f_ivelname  = .false.

      end subroutine bavel_init_parms


!!-------------------------- bavel_get_iparm --------------------------------!!
!!-------------------------- bavel_get_iparm --------------------------------!!
!!-------------------------- bavel_get_iparm --------------------------------!!

      subroutine bavel_get_iparm (obj, name, parm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      integer            ,intent(out)   :: parm

      select case (name)
      case ('avf_comp')
         if (obj%f_avf_comp) parm = obj%p_avf_comp
      case ('num_vels')
         if (obj%f_num_vels) parm = obj%p_num_vels
      case ('velf_ref')
         if (obj%f_velf_ref) parm = obj%p_velf_ref
      case ('len_sflt')
         if (obj%f_len_sflt) parm = obj%p_len_sflt
      case ('len_oflt')
         if (obj%f_len_oflt) parm = obj%p_len_oflt
      case ('stim_win')
         if (obj%f_stim_win) parm = obj%p_stim_win
      case ('etim_win')
         if (obj%f_etim_win) parm = obj%p_etim_win
      case ('twin_tpr')
         if (obj%f_twin_tpr) parm = obj%p_twin_tpr
      case ('method_a')
         if (obj%f_method_a) parm = obj%p_method_a
      case ('method_b')
         if (obj%f_method_b) parm = obj%p_method_b
      case ('pdat_opt')
         if (obj%f_pdat_opt) parm = obj%p_pdat_opt
      case ('flt_type')
         if (obj%f_flt_type) parm = obj%p_flt_type
      case ('gath_opt')
         if (obj%f_gath_opt) parm = obj%p_gath_opt
      case ('nrma_opt')
         if (obj%f_nrma_opt) parm = obj%p_nrma_opt
      case ('nrmb_opt')
         if (obj%f_nrmb_opt) parm = obj%p_nrmb_opt
      case ('scas_opt')
         if (obj%f_scas_opt) parm = obj%p_scas_opt
      case ('pass_opt')
         if (obj%f_pass_opt) parm = obj%p_pass_opt
      case ('incr_opt')
         if (obj%f_incr_opt) parm = obj%p_incr_opt
      case ('hdiv_opt')
         if (obj%f_hdiv_opt) parm = obj%p_hdiv_opt
      case ('vdiv_opt')
         if (obj%f_vdiv_opt) parm = obj%p_vdiv_opt
      case ('nmsc_opt')
         if (obj%f_nmsc_opt) parm = obj%p_nmsc_opt
      case ('trfo_opt')
         if (obj%f_trfo_opt) parm = obj%p_trfo_opt
      case ('errs_opt')
         if (obj%f_errs_opt) parm = obj%p_errs_opt
      case ('use_ivel')
         if (obj%f_use_ivel) parm = obj%p_use_ivel
      end select

      end subroutine bavel_get_iparm


!!-------------------------- bavel_set_iparm --------------------------------!!
!!-------------------------- bavel_set_iparm --------------------------------!!
!!-------------------------- bavel_set_iparm --------------------------------!!

      subroutine bavel_set_iparm (obj, name, parm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      integer            ,intent(in)    :: parm

      select case (name)
      case ('avf_comp')
         obj%p_avf_comp = parm
         obj%f_avf_comp = .true.
      case ('num_vels')
         obj%p_num_vels = parm
         obj%f_num_vels = .true.
      case ('velf_ref')
         obj%p_velf_ref = parm
         obj%f_velf_ref = .true.
      case ('len_sflt')
         obj%p_len_sflt = parm
         obj%f_len_sflt = .true.
      case ('len_oflt')
         obj%p_len_oflt = parm
         obj%f_len_oflt = .true.
      case ('stim_win')
         obj%p_stim_win = parm
         obj%f_stim_win = .true.
      case ('etim_win')
         obj%p_etim_win = parm
         obj%f_etim_win = .true.
      case ('method_a')
         obj%p_method_a = parm
         obj%f_method_a = .true.
      case ('method_b')
         obj%p_method_b = parm
         obj%f_method_b = .true.
      case ('pdat_opt')
         obj%p_pdat_opt = parm
         obj%f_pdat_opt = .true.
      case ('flt_type')
         obj%p_flt_type = parm
         obj%f_flt_type = .true.
      case ('gath_opt')
         obj%p_gath_opt = parm
         obj%f_gath_opt = .true.
      case ('nrma_opt')
         obj%p_nrma_opt = parm
         obj%f_nrma_opt = .true.
      case ('nrmb_opt')
         obj%p_nrmb_opt = parm
         obj%f_nrmb_opt = .true.
      case ('scas_opt')
         obj%p_scas_opt = parm
         obj%f_scas_opt = .true.
      case ('pass_opt')
         obj%p_pass_opt = parm
         obj%f_pass_opt = .true.
      case ('incr_opt')
         obj%p_incr_opt = parm
         obj%f_incr_opt = .true.
      case ('hdiv_opt')
         obj%p_hdiv_opt = parm
         obj%f_hdiv_opt = .true.
      case ('vdiv_opt')
         obj%p_vdiv_opt = parm
         obj%f_vdiv_opt = .true.
      case ('nmsc_opt')
         obj%p_nmsc_opt = parm
         obj%f_nmsc_opt = .true.
      case ('trfo_opt')
         obj%p_trfo_opt = parm
         obj%f_trfo_opt = .true.
      case ('errs_opt')
         obj%p_errs_opt = parm
         obj%f_errs_opt = .true.
      case ('twin_tpr')
         obj%p_twin_tpr = parm
         obj%f_twin_tpr = .true.
      case ('use_ivel')
         obj%p_use_ivel = parm
         obj%f_use_ivel = .true.
      end select

      end subroutine bavel_set_iparm


!!-------------------------- bavel_get_rparm --------------------------------!!
!!-------------------------- bavel_get_rparm --------------------------------!!
!!-------------------------- bavel_get_rparm --------------------------------!!

      subroutine bavel_get_rparm (obj, name, parm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      real               ,intent(out)   :: parm

      select case (name)
      case ('avf_slen')
         if(obj%f_avf_slen) parm = obj%p_avf_slen
      case ('velf_inc')
         if(obj%f_velf_inc) parm = obj%p_velf_inc
      case ('angl_max')
         if(obj%f_angl_max) parm = obj%p_angl_max
      case ('avo_stab')
         if(obj%f_avo_stab) parm = obj%p_avo_stab
      case ('vel_zero')
         if(obj%f_vel_zero) parm = obj%p_vel_zero
      case ('ric_freq')
         if(obj%f_ric_freq) parm = obj%p_ric_freq
      case ('agc_wina')
         if(obj%f_agc_wina) parm = obj%p_agc_wina
      case ('agc_winb')         
         if(obj%f_agc_winb) parm = obj%p_agc_winb
      case ('phas_rot')
         if(obj%f_phas_rot) parm = obj%p_phas_rot
      end select

      end subroutine bavel_get_rparm


!!-------------------------- bavel_set_rparm --------------------------------!!
!!-------------------------- bavel_set_rparm --------------------------------!!
!!-------------------------- bavel_set_rparm --------------------------------!!

      subroutine bavel_set_rparm (obj, name, parm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      real               ,intent(in)    :: parm

      select case (name)
      case ('avf_slen')
         obj%p_avf_slen = parm
         obj%f_avf_slen = .true.
      case ('velf_inc')
         obj%p_velf_inc = parm
         obj%f_velf_inc = .true.
      case ('angl_max')
         obj%p_angl_max = parm
         obj%f_angl_max = .true.
      case ('avo_stab')
         obj%p_avo_stab = parm
         obj%f_avo_stab = .true.
      case ('vel_zero')
         obj%p_vel_zero = parm
         obj%f_vel_zero = .true.
      case ('ric_freq')
         obj%p_ric_freq = parm
         obj%f_ric_freq = .true.
      case ('agc_wina')
         obj%p_agc_wina = parm
         obj%f_agc_wina = .true.
      case ('agc_winb')
         obj%p_agc_winb = parm
         obj%f_agc_winb = .true.
      case ('phas_rot')
         obj%p_phas_rot = parm
         obj%f_phas_rot = .true.
      end select

      end subroutine bavel_set_rparm

  
!!-------------------------- bavel_get_cparm --------------------------------!!
!!-------------------------- bavel_get_cparm --------------------------------!!
!!-------------------------- bavel_get_cparm --------------------------------!!

      subroutine bavel_get_cparm (obj, name, parm, lparm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      character(len=*)   ,intent(out)   :: parm
      integer            ,intent(out)   :: lparm
      integer          :: i

      parm = ' '
      lparm = 0
      select case (name)
      case ('svelname')
         if(obj%f_svelname) parm = obj%p_svelname
      case ('ivelname')
         if(obj%f_ivelname) parm = obj%p_ivelname
      case ('bp_freqs')
         if(obj%f_bp_freqs) parm = obj%p_bp_freqs
      case ('angl_rng')
         if(obj%f_angl_rng) parm = obj%p_angl_rng
      end select

      i = 1
100   if (parm(i:i) .ne. ' ' .and. i .lt. 256) then
         i = i + 1
         goto 100
      end if
      lparm = i - 1

      end subroutine bavel_get_cparm


!!-------------------------- bavel_set_cparm --------------------------------!!
!!-------------------------- bavel_set_cparm --------------------------------!!
!!-------------------------- bavel_set_cparm --------------------------------!!
  
      subroutine bavel_set_cparm (obj, name, parm)
      type(bavel_struct),intent(inout) :: obj              ! arguments
      character(len=*)   ,intent(in)    :: name
      character(len=*)   ,intent(in)    :: parm

      select case (name)
      case ('svelname')
         obj%p_svelname = parm
         obj%f_svelname = .true.
      case ('ivelname')
         obj%p_ivelname = parm
         obj%f_ivelname = .true.
      case ('bp_freqs')
         obj%p_bp_freqs = parm
         obj%f_bp_freqs = .true.
      case ('angl_rng')
         obj%p_angl_rng = parm
         obj%f_angl_rng = .true.
      end select

      end subroutine bavel_set_cparm


!!------------------------ bavel_read_velocity --------------------------!!
!!------------------------ bavel_read_velocity --------------------------!!
!!------------------------ bavel_read_velocity --------------------------!!
!
! This routine reads a single velocity function from the velocity file and
! interpolates it to the trace sample rate. Because this routine is called
! only by bavel_vels_to_tables, which is itself called only by
! bavel_update, we can use the parameter cache.
! Note that in addition to returning vel_interp, veltype and errflag, this
! routine also sets obj%xlast and obj%ylast in the process data structure.
!
      subroutine bavel_read_velocity (obj, &
     &   maxpicks, vel_interp, veltype, errflag)
      implicit none
      type(bavel_struct)  ,intent(inout) :: obj    ! arguments
      integer             ,intent(in)    :: maxpicks        ! arguments
      real                ,intent(inout) :: vel_interp(:)   ! arguments
      character(len=4)    ,intent(inout) :: veltype         ! arguments
      integer             ,intent(inout) :: errflag         ! arguments

      integer            :: npicks, i, ipick                ! local
      real               :: ttest                           ! local
      real               :: tpicks(maxpicks)                ! local
      real               :: vpicks(maxpicks)                ! local
      real               :: work(1)                         ! local
      character(len=128) :: message                         ! local

      call velio_read_velfun (obj%velio_obj, obj%xlast, obj%ylast, npicks, &
                     tpicks, vpicks, errflag, message, veltype=veltype)
      if (errflag /= VELIO_OK) then
        call pc_error ('Error reading velocity function.')
        return
      end if
      if (veltype=='VTNM' .or. veltype=='VTRM') then
        call intpvelf (npicks, tpicks(:npicks), vpicks(:npicks), work,  &
           4, 0., obj%dt, obj%ndptvel, vel_interp(:obj%ndptvel), errflag)
        if (errflag /= 0) return
      else if (veltype == 'VTIN') then
        ipick = 1
        do i = 1, obj%ndptvel
          ttest = (i-1) * obj%dt
          do
            if (ttest<=tpicks(ipick) .or. ipick==npicks) exit
            ipick = ipick + 1
          end do
          vel_interp(i) = vpicks(ipick)
        end do
      else
        errflag = 1
        call pc_error ('Illegal velocity type '//veltype//'. Velocity type &
                       &must be VTRM, VTNM, or VTIN.')
      end if

      return
      end subroutine bavel_read_velocity


!!------------------------ bavel_convert_vels ---------------------------!!
!!------------------------ bavel_convert_vels ---------------------------!!
!!------------------------ bavel_convert_vels ---------------------------!!
!
! Input for this routine is a velocity of type VELTYPE in array VEL_SUM.
! Upon output, if OPT_SR is false, VEL_INTERP contains smoothed interval
! velocity while VEL_SUM contains the corresponding RMS velocity.
! If OPT_SR is true, there is no smoothing, and VEL_INTERP and VEL_SUM
! both contain the RMS velocity.
! Arrays VEL_SUM and VEL_INTERP must both be of length obj%ndptvel.
! The routine also prints a table of decimated velocity to unit obj%lun.
!
      subroutine bavel_convert_vels (obj, vel_avg, veltype, &
                                       vel_interp, errflag)
      implicit none
      type(bavel_struct)  ,intent(in)    :: obj             ! arguments
      real                ,intent(inout) :: vel_avg(:)      ! arguments
      character(len=4)    ,intent(in)    :: veltype         ! arguments
      real                ,intent(out)   :: vel_interp(:)   ! arguments
      integer             ,intent(out)   :: errflag         ! arguments

      integer             :: j                              ! local

      errflag = 0
      if (obj%opt_sr) then
        if (veltype == 'VTIN') then
          call bavel_int_to_rms_vel (obj%ndptvel, vel_avg, vel_interp)
          vel_avg = vel_interp
        else
          vel_interp = vel_avg
        end if
        write(obj%print_lun, *) 'Composite RMS velocity @', stride*obj%dt, 's'
      else
        if (veltype /= 'VTIN') then
          call bavel_rms_to_int_vel (obj%ndptvel, vel_avg, errflag)
          if (errflag /= 0) return
        end if
        call bavel_smooth (vel_avg, obj%ndptvel, obj%nsmooth, vel_interp)
        call bavel_int_to_rms_vel (obj%ndptvel, vel_interp, vel_avg)
        write(obj%print_lun, *) 'Smoothed, composite INTERVAL velocity @', &
     &                     stride*obj%dt, 's'
      end if
      write(obj%print_lun, '(5(F7.2,I6))')  &
             ((j-1)*obj%dt,nint(vel_interp(j)),j=1,obj%ndptvel,stride)
      return
      end subroutine bavel_convert_vels


!!----------------------- bavel_rms_to_int_vel --------------------------!!
!!----------------------- bavel_rms_to_int_vel --------------------------!!
!!----------------------- bavel_rms_to_int_vel --------------------------!!
!
! This routine converts RMS to Interval velocity (in place within "velocity"
! array). It is assumed that the array begins at time zero. This routine
! calls pc_error to report errors, and also returns errflag = 1 in case of
! error.
!
      subroutine bavel_rms_to_int_vel (ndptvel, velocity, errflag)
      implicit none
      integer , intent(in)    :: ndptvel               ! arguments
      real    , intent(inout) :: velocity(ndptvel)     ! arguments
      integer , intent(out)   :: errflag               ! arguments

      integer     :: j                                 ! local

      errflag = 0
      if (ndptvel < 2) return

      do j = 1, ndptvel
        velocity(j) = (j-1) * velocity(j)**2
      end do

      velocity(2:ndptvel) = velocity(2:ndptvel) - velocity(1:ndptvel-1)

      if (minval(velocity(2:ndptvel)) <= 0.0) then
        call pc_error ('Error converting RMS to Interval velocity.')
        errflag = 1
        return
      end if

      velocity(2:ndptvel) = sqrt (velocity(2:ndptvel))
      velocity(1) = velocity(2)

      return
      end subroutine bavel_rms_to_int_vel


!!----------------------- bavel_int_to_rms_vel --------------------------!!
!!----------------------- bavel_int_to_rms_vel --------------------------!!
!!----------------------- bavel_int_to_rms_vel --------------------------!!
!
! This routine converts Interval to RMS velocity and the conversion is NOT
! done in place (input is in vel_int, output in vel_rms). It is assumed that
! the arrays begin at time zero.
!
      subroutine bavel_int_to_rms_vel (ndptvel, vel_int, vel_rms)
      implicit none
      integer , intent(in)  :: ndptvel                 ! arguments
      real    , intent(in)  :: vel_int(ndptvel)        ! arguments
      real    , intent(out) :: vel_rms(ndptvel)        ! arguments

      integer     :: j                                 ! local
      real        :: sumv2                             ! local

      if (ndptvel < 2) then
        vel_rms = vel_int
      else
        sumv2 = 0.0
        do j = 2, ndptvel
          sumv2 = sumv2 + vel_int(j)**2
          vel_rms(j) = sqrt(sumv2/(j-1))
        end do
        vel_rms(1) = vel_rms(2)
      end if

      return
      end subroutine bavel_int_to_rms_vel


!!--------------------------- bavel_smooth ------------------------------!!
!!--------------------------- bavel_smooth ------------------------------!!
!!--------------------------- bavel_smooth ------------------------------!!
!
! Input array XIN is smoothed by applying a running average of length NAVG;
! output in XOUT. The running average is always performed over an ODD number
! of points centered around a desired output point. If NAVG is even, an
! averaging length of NAVG+1 is used. Averaging length is tapered down when
! approaching the beginning and end of the XIN array, to keep the averaging
! operator from running off the ends of that array.
!
      subroutine bavel_smooth (xin, n, navg, xout)
      implicit none
!
      integer , intent(in)  :: n          ! Arguments
      real    , intent(in)  :: xin(n)     ! Arguments
      integer , intent(in)  :: navg       ! Arguments
      real    , intent(out) :: xout(n)    ! Arguments
!
      integer  :: navg2, i, j             ! local variables
      real     :: sfact                   ! local variables
!
      if (navg <= 1) then
        xout = xin
        return
      end if
!
      navg2 = min(navg,n-1)/2
      sfact = 1.0/(2*navg2 + 1)
      xout = 0.
      do j = -navg2, navg2
        do i = 1+abs(j), n-abs(j)
          xout(i) = xout(i) + xin(i+j)
        end do
      end do
      do i = 2, navg2
        xout(i) = xout(i) / (2*i-1)
      end do
      xout(navg2+1:n-navg2) = xout(navg2+1:n-navg2) * sfact
      do i=n-navg2+1,n-1
        xout(i) = xout(i) / (2*(n-i)+1)
      end do
!
      return
      end subroutine bavel_smooth


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module bavel_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
