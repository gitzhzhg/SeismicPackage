!<CPS_v1 type="PROCESS"/>
!!------------------------------ avopcomp.f90 --------------------------------!!
!!------------------------------ avopcomp.f90 --------------------------------!!
!!------------------------------ avopcomp.f90 --------------------------------!!

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
! Name       : AVOPCOMP
! Category   : amplitude_mod
! Written    : 2004-02-18   by: Michael Ried
! Revised    : 2006-10-31   by: B. Menger
! Maturity   : production
! Purpose    : Emphasizes gradient anomalies
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This tool rotates the complex AVO data so as eliminate mutual correlations
! and to align the data into a dominant (principal) component and a minor
! (outlying) component. The complex A trace will be replaced by the dominant
! component, while the outlying component will replace the complex B trace.
! Mathematically, this operation is described by
!
! A <-- (cos q cos f + i sin q sin f) A + (sin q cos f + i cos q sin f) B
!
! B <-- (-sin q cos f + i cos q sin f) A + (cos q cos f + i cos q sin f) B,
!
! where q and f are the rotation angles necessary to achieve decoupling of A
! and B.
!
! Inputs and Outputs:
!
! Minimum input to this tool is a Standard Suite, consisting of the following
! traces:
!
!
! Trace                   Header trc_type
!                         (Stored in Header #49)
! Real AVO "A" trace      43
! Real AVO "B" trace      44
! Imag AVO "A" trace      17
! Imag AVO "B" trace      18
! Standard deviation "A"  45
! Standard deviation "B"  47
! Real correlation        48
! Imag correlation        41
!
! Parameters:
!
! Do you wish to output the rotation angles?
!
! Specify "Yes" if you wish to append the rotation angle traces to the 
! modified standard suite. Otherwise, only the rotated standard suite will 
! be passed.
!
! Threshold of sphericity:
!
! If the distribution of data is spherical, it will not be possible to
! determine unique rotation angles q and f. For this reason, it is necessary
! for this tool to gravitate in a controlled and orderly manner towards
! preferred rotation angles for q and f as the data distribution becomes more
! and more spherical. These preferred rotation angles are permanently set
! within the tool to be -p/4 and 0 radians, respectively. However, the user
! does have control over the degree of sphericity that will cause the tool to
! begin gravitating towards these preferred rotation angles. This sphericity
! threshold can vary between 0, meaning perfect sphericity to 1.0 for perfect
! alignment along a single line. A good default value for this parameter is
! 0.1.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Optional input to this tool are prestack data, passed through AVEL and the
! Standard Suite tool. This data may or may not be moved out, and must follow
! the standard suite traces in each ensemble. A velocity trace (trc_type 51)
! must appear between the standard suite and the prestack data. The prestack
! data is assumed to be x-t data, unless the ang_value trace header is present.
! In this case, it is assumed to be constant angle data, whose angle is
! contained in that header.
!
! The output from this tool is a possibly augmented standard suite, containing
! the principal and minor data components and their corresponding statistics.
! The correlation coefficient will be set to zero. Optional outputs are the
! rotation angles f and q (in degrees) which were necessary to decouple the
! output data. These rotation angle traces are appended to the end of each
! input ensemble. These angle traces have trace types of 52 for q and 53 for f,
! respectively. If prestack data was appended to the suite, it will be
! transformed in such a way as to produce the same intercept and gradient
! traces as the transformed traces passed in the suite. 
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!  Live seismic data      (Trace type #1)
!  Real AVO "A" trace     (Trace type #43)
!  Real AVO "B" trace     (Trace type #44)
!  Imag AVO "A" trace     (Trace type #17)
!  Imag AVO "B" trace     (Trace type #18)
!  Standard deviation "A" (Trace type #45)
!  Standard deviation "B" (Trace type #47)
!  Real correlation       (Trace type #48)
!  Imag correlation       (Trace type #41)
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!  Live seismic data      (Trace type #1)
!  Real AVO "A" trace     (Trace type #43)
!  Real AVO "B" trace     (Trace type #44)
!  Imag AVO "A" trace     (Trace type #17)
!  Imag AVO "B" trace     (Trace type #18)
!  Standard deviation "A" (Trace type #45)
!  Standard deviation "B" (Trace type #47)
!  Real correlation       (Trace type #48)
!  Imag correlation       (Trace type #41)
!  Theta trace            (Trace type #52)
!  Phi trace              (Trace type #53)
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! --> Insert globals that this process uses or changes:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> specify action taken.
! GATHERED  whether traces are a legitimate gather  --> specify action taken.
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
! TSTRT     starting time on trace                  --> specify action taken.
! DT        trace sample interval                   --> specify action taken.
! GRID      grid transformation structure           --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! --> Insert header words used or changed by this process:
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!  3      HDR_CURRENT_GROUP          Input
!  6      HDR_OFFSET                 Input
! 49      HDR_USER_49                Input/Output (Trace Type)
! 53      HDR_USER_53                Input/Output (Dominant Frequency)
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2006-10-31  B. Menger  Removed Unused Variables.
!  2. 2005-10-10  Goodger    Change argument a in routine hermtp from intent
!                            out to intent inout to satisfy absoft 9.0 
!                            compiler.
!  1. 2005-01-03  Mike Ried  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.  --> Change to add any platform dependencies.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.  --> Change if any special compiler/linking required.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! --> Default values are shown below - edit as needed:
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! --> Edit the following lines as needed:
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.  --> Change if this statement is inappropriate.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! --> Insert description of algorithms used.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! --> Insert any useful programming notes here.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS avopcomp Process/NC=80>
!                          AVO Principal Components
!`--------------------------------------------------------------------------
!            Output the rotation angles?[/R]  COMP_OUT=`KKK
!                Threshold of sphericity[/R]  THRSHOLD=`FFFFFFFFFFFFFFFFFFFF
!     Has NMO been applied to the traces[/R]  NMO_APPL=`KKK
!`--------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="COMP_OUT"
!<Tip> -->Choose whether to output the rotation angles?</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'Yes', the rotation angles will be outputted
!</Help>
!
!<Help KEYWORD="THRSHOLD">
!<Tip> --> Enter the threshold of sphericity</Tip>
! Default = --> 0.1
! Allowed = --> Real
! --> Enter a real value is greater than 0.0 and less than 1.0
!</Help>
!
!<Help KEYWORD="NMO_APPL"
!<Tip> -->Has NMO been applied to the prestack traces?</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'Yes', then the program will assume nmo has been applied
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module avopcomp_module
      use pc_module
      use named_constants_module
      use mem_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: avopcomp_create
      public :: avopcomp_initialize
      public :: avopcomp_update
      public :: avopcomp_delete
      public :: avopcomp            ! main trace processing routine.
      public :: avopcomp_wrapup

      character(len=100),public,save :: AVOPCOMP_IDENT = &
'$Id: avopcomp.f90,v 1.3 2006/10/30 14:01:44 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: avopcomp_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        character(len=3)           :: cnmo_appl
        integer                    :: AVO_Ar
        integer                    :: AVO_Ai
        integer                    :: AVO_Br
        integer                    :: AVO_Bi
        integer                    :: AVO_Rr
        integer                    :: AVO_Ri
        integer                    :: AVO_Sa
        integer                    :: AVO_Sb
        logical                    :: comp_out
        integer                    :: ftrlen
        integer                    :: Nh
        logical                    :: nmo_appl
        integer                    :: Noh2
        real                       :: thrshold
!       
        integer                    :: t0_ptr
        integer                    :: bstr_ptr
        integer                    :: bqstr_ptr
        integer                    :: h1_ptr
        integer                    :: h2_ptr
        integer                    :: sq_ptr
        integer                    :: corr1_ptr
        integer                    :: corr2_ptr
        integer                    :: work_ptr
        integer                    :: aux_ptr

! --> Insert any other needed variables or pointers here.
        real              ,pointer :: wa1(:)  ! work array
        double precision  ,pointer :: dwa1(:)  ! work array


! --> Insert any other needed variables or pointers here.

      end type avopcomp_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(avopcomp_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine avopcomp_create (obj)
      type(avopcomp_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in avopcomp_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%wa1)   ! must be done for all pointers.
      nullify(obj%dwa1)

      call avopcomp_initialize (obj)
      end subroutine avopcomp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine avopcomp_delete (obj)
      type(avopcomp_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call avopcomp_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.
      if (associated(obj%wa1)) deallocate(obj%wa1)
      if (associated(obj%dwa1)) deallocate(obj%dwa1)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in &
        & avopcomp_delete")
      end subroutine avopcomp_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine avopcomp_initialize (obj)
      type(avopcomp_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%AVO_Ar = 1
      obj%AVO_Ai = 3
      obj%AVO_Br = 2
      obj%AVO_Bi = 4
      obj%AVO_Rr = 7
      obj%AVO_Ri = 8
      obj%AVO_Sa = 5
      obj%AVO_Sb = 6
      obj%cnmo_appl = 'YES'
      obj%comp_out = .TRUE.
      obj%ftrlen = 31
      obj%Nh = 30
      obj%nmo_appl = .TRUE.
      obj%Noh2 = 1
      obj%thrshold = 0.1

      call avopcomp_update (obj)
      end subroutine avopcomp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine avopcomp_update (obj)
      type(avopcomp_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.

      integer :: lwa1, ldwa1, max_samps

      real :: samp_int_in

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('COMP_OUT', obj%comp_out)
      call pc_get('NMO_APPL', obj%nmo_appl)
      call pc_get('THRSHOLD', obj%thrshold)

!
!      Initialize needed variables
!
      max_samps = obj%ndpt

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).

!
!      change options into character values
!
      if (obj%nmo_appl) then
        obj%cnmo_appl='YES'
      else
        obj%cnmo_appl='NO'
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('COMP_OUT', obj%comp_out)
      call pc_put('NMO_APPL', obj%nmo_appl)
      call pc_put('THRSHOLD', obj%thrshold)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

! --> Add here any other parameter cache calls such as to set sensitivities.


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.

!
!  Init the indicies of the traces within the AVO Standard Suite
!
!  AVO A traces, real and imaginary
!
      obj%AVO_Ar = 1
      obj%AVO_Ai = 3
!
!  AVO B traces, real and imaginary
!
      obj%AVO_Br = 2
      obj%AVO_Bi = 4
!
!  AVO Standard Deviation traces
!
      obj%AVO_Sa = 5
      obj%AVO_Sb = 6
!
!  AVO Standard Deviation traces
!
      obj%AVO_Rr = 7
      obj%AVO_Ri = 8
!
!  Set the Hilbert transform length, based on the sampling interval
!
      samp_int_in=obj%dt*1000.0
      obj%ftrlen = 31
      if ( samp_int_in .lt. 4.5 ) obj%ftrlen = 43
      if ( samp_int_in .lt. 1.5 ) obj%ftrlen = 63
      if ( samp_int_in .lt. 0.5 ) obj%ftrlen = 95
!
!  Determine the length of the stretch filter
!
      obj%Nh = 30
      if ( samp_int_in .lt. 2.5 ) obj%Nh = 60
      if ( samp_int_in .lt. 1.5 ) obj%Nh = 100
!
!  Init the flags to determine Ricker wavelet calculation.
!
      obj%Noh2  = 1
!
! --> Insert code to allocate needed permanent memory.
!
!     Set up the work array
!
      lwa1 = 0
      ldwa1 = 0
!
!       Now Increase the size of the work buffer
!
! .....   T0_PTR  :  zero-offset time
      OBJ%T0_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... BSTR_PTR  :  Calculation of the h array with the br trace
      OBJ%BSTR_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... BQSTR_PTR :  Hilbert transform of the bstr array
      OBJ%BQSTR_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... H1_PTR     :  Helbert transformed data
      OBJ%H1_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... H2_PTR     :  Helbert transformed data
      OBJ%H2_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... SQ_PTR    :  
      OBJ%SQ_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... CORR1_PTR  :  Autocorrelation of a wavelet (#1)
      OBJ%CORR1_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... CORR2_PTR  :  Autocorrelation of a wavelet (#2)
      OBJ%CORR2_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS
! ..... WORK_PTR  :  Work array
      OBJ%WORK_PTR = LWA1 + 1
      LWA1 = LWA1 + MAX_SAMPS

! ..... AUX_PTR  :  Auxiliary or Scratch array
      OBJ%AUX_PTR = LDWA1 + 1
      LDWA1 = LDWA1 + MAX_SAMPS
!
!   Allocate your permanent memory like this:
!   Allocation errors will be reported to the parameter cache.
!
      IF (LWA1.GT.0) THEN
        CALL MEM_ALLOC (OBJ%WA1, LWA1)
      END IF
      IF (LDWA1.GT.0) THEN
        CALL MEM_ALLOC (OBJ%DWA1, LDWA1)
      END IF

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine avopcomp_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine avopcomp (obj,ntr,hd,tr)
      type(avopcomp_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      character(len=3) cnmo_appl

!      Output headers
      double precision, dimension(size(hd,1),size(hd,2)+2) :: hdo
      real            , dimension(size(tr,1),size(tr,2)+2) :: tro
!
      double precision :: M_PI, PI2, Pole, source_detect_dist
      integer :: aux_ptr, AVO_Ar, AVO_Ai, AVO_Br, AVO_Bi, AVO_Sa, AVO_Sb
      integer :: AVO_Rr, AVO_Ri, bqstr_ptr, bstr_ptr, corr1_ptr, corr2_ptr
      integer :: data_trace_type, ftrlen, h1_ptr, h2_ptr
      integer :: i1, idead, ie, ierr, iphitr, ist1, istrc, it, ithetatr
      integer :: jerror, ltrsam, max_samps, max_trcs
      integer :: Nh, nhdrs, Noh2, ntrc_gath_in, ntrc_gath_out
      integer :: phi_ptr, SampStart, SampEnd, sq_ptr, t0_ptr, theta_ptr, v_ptr
      integer :: work_ptr
      logical :: comp_out, nmo_appl
      real :: dominant_freq, f0, Pctwns, Pctcns
      real :: avo_central_angle, samp_int_in, thrshold, W0t
!
      parameter (M_PI   = 3.14159265358979323846 ) ! PI
      parameter (PI2 = 1.57079632679489661923 ) ! PI/2
!
      data  idead    / 0 /
      data  iphitr   / 53 /
      data  istrc    / 1 /
      data  ithetatr / 52 /
!
      AVO_Ar = obj%AVO_Ar
      AVO_Ai = obj%AVO_Ai
      AVO_Br = obj%AVO_Br
      AVO_Bi = obj%AVO_Bi
      AVO_Sa = obj%AVO_Sa
      AVO_Sb = obj%AVO_Sb
      AVO_Rr = obj%AVO_Rr
      AVO_Ri = obj%AVO_Ri
      cnmo_appl = obj%cnmo_appl
      comp_out = obj%comp_out
      ftrlen = obj%ftrlen
      ist1=0
      Nh = obj%Nh
      Noh2  = obj%Noh2
      nmo_appl = obj%nmo_appl
      thrshold=obj%thrshold

! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      if (ntr == no_more_traces .or. ntr == fatal_error) then
        call avopcomp_wrapup(obj)
        return
      end if
!
!      set pointers into the work array
!
      aux_ptr=obj%aux_ptr
      bqstr_ptr=obj%bqstr_ptr
      bstr_ptr=obj%bstr_ptr
      corr1_ptr=obj%corr1_ptr
      corr2_ptr=obj%corr2_ptr
      h1_ptr=obj%h1_ptr
      h2_ptr=obj%h2_ptr
      sq_ptr=obj%sq_ptr
      t0_ptr=obj%t0_ptr
      work_ptr=obj%work_ptr
!
      AVO_Ar = obj%AVO_Ar
      AVO_Ai = obj%AVO_Ai
      AVO_Br = obj%AVO_Br
      AVO_Bi = obj%AVO_Bi
      AVO_Sa = obj%AVO_Sa
      AVO_Sb = obj%AVO_Sb
      AVO_Rr = obj%AVO_Rr
      AVO_Ri = obj%AVO_Ri
      ftrlen = obj%ftrlen
      Nh = obj%Nh
      Noh2  = obj%Noh2
      thrshold=obj%thrshold
!
!      INITIALIZE
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%numtr is max number of traces input/output)
!      (obj%dt is trace sample interval)
!
      max_samps=obj%ndpt
      ltrsam = obj%ndpt
      nhdrs=obj%nwih
      ntrc_gath_in=ntr
      samp_int_in=obj%dt*1000.0
      ntrc_gath_out = ntrc_gath_in
      if (comp_out) ntrc_gath_out = ntrc_gath_out + 2
      max_trcs = ntrc_gath_in + 2
!
!      initialize the output traces to the input traces
!
      ie=ntrc_gath_in
      hdo(1:nhdrs,1:ie) = hd(1:nhdrs,1:ie)
      tro(1:max_samps,1:ie) = tr(1:max_samps,1:ie)
!
!      set header values
      dominant_freq = hd(hdr_user_53, 1)
!
      theta_ptr = ntrc_gath_in + 1
      phi_ptr   = ntrc_gath_in + 2
!
      call avopcomp_phaseangles(tro, tro(1:,theta_ptr), tro(1:,phi_ptr), &
        thrshold, max_samps, max_trcs, AVO_Sa, AVO_Sb, AVO_Rr, AVO_Ri )
!
      if ( ntrc_gath_in .gt. 8 ) then
        f0 = dominant_freq
!
        if ( f0 .gt. 0.0 .and. Noh2 .gt. 0 ) then
          W0t = 0.001 * PI2 * f0 * samp_int_in
          Pctwns = 1.0
          Pctcns = 0.0
          Pole   = 0.0
          ierr    = -1
          call avopcomp_rikflt(obj%wa1(h1_ptr:), obj%wa1(h2_ptr:),        &
            obj%wa1(corr1_ptr:), obj%wa1(corr2_ptr:), obj%wa1(work_ptr:), &
            obj%dwa1(aux_ptr:), Nh, Nh, W0t, Pctwns, Pctcns, Pole, ierr)
          Noh2 = ierr
        end if
!
        call avopcomp_scantrace(tro(1:,AVO_Br), ltrsam, SampStart, SampEnd)
!
        call avopcomp_auxtraces(tro(1:,AVO_Br), obj%wa1(bstr_ptr:),   &
          obj%wa1(bqstr_ptr:), tro(1:,v_ptr), obj%wa1(h1_ptr:),       &
          samp_int_in, ltrsam, SampStart, SampEnd,  Nh, Noh2, ftrlen)
      end if
!
      jerror   = 0
!
      do i1 = 1, ntrc_gath_in

        data_trace_type  = hd (hdr_user_49, i1)
!
        if (ist1.eq.0) then
          if (data_trace_type.eq.istrc .or. data_trace_type.eq.idead) then
!            Set the first seismic trace number
            ist1 = i1
          end if
        end if
!
!   Check for seismic traces, V_PTR points to the Velocity trace
!
        if ( i1.gt.8 .and. i1.ne.v_ptr ) then
!
!   Get trace attributes for the ith trace
!
          source_detect_dist = hd (hdr_offset, i1)
!
          avo_central_angle = hd (hdr_user_53, i1)
!
          call avopcomp_t0(ltrsam, samp_int_in, source_detect_dist, &
            tro(1:,v_ptr), obj%wa1(t0_ptr:))
!
          call avopcomp_scantrace(tro(1:,i1), ltrsam, SampStart, SampEnd)
!
          call avopcomp_hilba(tro(1:,i1), obj%wa1(Sq_PTR:), ltrsam, &
            SampStart, SampEnd, ftrlen)
!
          call avopcomp_prestack(max_samps, max_trcs, tro, samp_int_in,     &
            cnmo_appl, avo_central_angle, obj%wa1(t0_ptr:), tro(1:,v_ptr),  &
            obj%wa1(sq_ptr:), obj%wa1(bstr_ptr:), obj%wa1(bqstr_ptr:),      &
            tro(1:,theta_ptr), tro(1:,phi_ptr), source_detect_dist, ltrsam, &
            i1, AVO_Br, AVO_Bi, AVO_Sa, AVO_Sb)
!
          call avopcomp_remute(ltrsam, tro(1:,i1), sampstart, sampend)
        end if
      end do
!
      call avopcomp_rotation(tro, tro(1:,theta_ptr), tro(1:,phi_ptr), &
        m_pi, max_samps, max_trcs, avo_ar, avo_ai, avo_br, avo_bi)
!
!      set output traces & headers
!
      if (ist1.eq.0) ist1=1
!
      it=ntrc_gath_in+1
!
!      set theta output headers
!
      if (comp_out) then
        hdo(1:nhdrs,it)=hd(1:nhdrs, ist1)
        hdo(hdr_user_49,it)=ithetatr
        hdo(hdr_user_53,it)=dominant_freq
        it=it+1
!
!      set phi output headers
!
        hdo(1:nhdrs,it)=hd(1:nhdrs, ist1)
        hdo(hdr_user_49,it)=iphitr
        hdo(hdr_user_53,it)=dominant_freq
        it=it+1
      end if
!
      ntr=ntrc_gath_out
      hd=hdo
      tr=tro
!
      if ( jerror .ne. 0 )   then
        ierr = jerror
      end if

      end subroutine avopcomp
!!-------------------------- avopcomp_auxtraces -----------------------------!!
!!-------------------------- avopcomp_auxtraces -----------------------------!!
!!-------------------------- avopcomp_auxtraces -----------------------------!!
      subroutine avopcomp_auxtraces(AVO_Br, Bstr, Bqstr, Vrms, h1, si, &
                                    ns, is1, is2, nh, noh2, ftrlen)
!
      implicit none
      real             ,intent(in)    :: AVO_Br(:)   ! arguments (len=ns)
      real             ,intent(out)   :: Bstr(:)     ! arguments (len=ns)
      real             ,intent(out)   :: Bqstr(:)    ! arguments (len=ns)
      real             ,intent(in)    :: Vrms(:)     ! arguments (len=ns)
      real             ,intent(in)    :: h1(:)        ! arguments (len=ns)
      real             ,intent(in)    :: si          ! arguments
      integer          ,intent(in)    :: ns          ! arguments
      integer          ,intent(in)    :: is1         ! arguments
      integer          ,intent(in)    :: is2         ! arguments
      integer          ,intent(in)    :: nh          ! arguments
      integer          ,intent(in)    :: noh2        ! arguments
      integer          ,intent(in)    :: ftrlen      ! arguments
! 
!   Compute certain auxiliary traces, which are sometimes necessary
!   to correct prestack data in conformity with the AVO intercept and
!   gradient.  More specifically, the responsibilities of this
!   procedure are:
! 
!   1)  Verify that "noh2" is zero.  If it is nonzero, some error has
!       occurred in the calculation of the NMO stretch operator, "h2".
!       In this case, simply zero out the "Bstr" and "Bqstr" arrays,
!       and do nothing else.
! 
!   2)  If "noh2" is zero, convolve the "h2" array with the "Br" trace
!       between sample numbers "is1" and "is2".  The "h2" array is
!       multiplexed within the "h" array, and the convolution is to
!       be performed according to the following formula:
! 
!                  n2
!       Bstr[j] = SUM {h2[k] Br[j-k]}   for j=(is1,...,is2) ,
!               k=-n1
!
!       where n1=max(j-is2, -nh), and n2=min(j-is1, nh).
!
!       Note that h2[k] = h[2*(nh+k) + 1].  The "h1" array, corresponding
!       to even array indices of "h", is not used in this calculation.
!
!   3)  Set "Bqstr" to be the Hilbert transform of "Bstr", using a
!       transform filter length of "ftrlen" samples.
!
!   4)  The "Bstr" and the "Bqstr" are then both scaled by -0.5 * D[j],
!       where D[j] = 1 + 2*t(k)*V'[k]/V[k].  t(k) is the time
!       in seconds of the k-th sample, and V[k] is the k-th velocity
!       sample in (ft/s) or (m/s).  V'[k] is the derivative of the
!       velocity at sample k.   
!
      integer ::          iptr, j, n1, n2, k
      double precision :: value, time, veloc, vpr, dt
!
!  Check noh2.  Zero both arrays if noh2 .ne. 0
!
      do j = 1, ns
        Bstr(j) = 0.0
        Bqstr(j) = 0.0
      end do
!
      if ( noh2 .eq. 0 ) then
!
!  Perform the convolution.
!
      do j = is1, is2

         value = 0
         n1    = j - is2
         n2    = j - is1
         if ( n1 .lt. -nh ) n1 = -nh
         if ( n2 .gt.  nh ) n2 =  nh

         do k = n1, n2
            iptr=2*(nh+k)+1
            if (iptr.gt.nh) then
              write(6,9100) nh,iptr
  9100        format('iptr>nh',2(i5,1x))
            end if
            value = value + h1(2*(nh+k)+1) * AVO_Br(j-k)
         end do

         Bstr(j) = value

      end do
!
!  Form the Hilbert transform of Bstr.
!
      call avopcomp_hilba ( Bstr, Bqstr, ns, is1, is2, ftrlen )
!
!  Scale the data by dt.
!
      do j = is1, is2
!
         time  = 1e-3 * j * si
         veloc = Vrms(j)

         if ( veloc .ge. 0.0 ) then

            vpr   = 0
            if ( j .gt. 0 .and. j .lt. ns-1 ) then
               vpr = -5e2 * (Vrms(j+1) - Vrms(j-1)) / si
            end if
!
            dt       = 1 + 2*time*vpr/veloc
            dt = 1
            Bstr(j)  = Bstr(j)  * dt
            Bqstr(j) = Bqstr(j) * dt
!
         end if
!
      end do
!
      end if
!
!  Return to the calling routine.
!
      return
!
      end subroutine avopcomp_auxtraces

!!---------------------------- avopcomp_hermtp ------------------------------!!
!!---------------------------- avopcomp_hermtp ------------------------------!!
!!---------------------------- avopcomp_hermtp ------------------------------!!
!
      subroutine avopcomp_hermtp (m,t0,t,z,x,a,istat)
!
      implicit none
      integer          ,intent(in)    :: m        ! arguments
      real             ,intent(in)    :: t0       ! arguments
      real             ,intent(in)    :: t(:)     ! arguments (len=m)
      real             ,intent(in)    :: z(:)     ! arguments (len=m+1)
      double precision ,intent(out)   :: x(:)     ! arguments (len=m+1)
      double precision ,intent(inout)   :: a(:)     ! arguments (len=m)
      integer          ,intent(out)   :: istat    ! arguments
!
!A      Author                 H. W. Swan
!A      Designer               H. W. Swan
!A      Language               FORTRAN
!A      Written                01-01-93
!       Revised    01-25-94    ESN - Converted to ProSPARC.
!A
!A      CALLING SEQUENCE:
!A           call avopcomp_hermtp (M,T0,T,Z,X,A,ISTAT)
!A
!
!   Solves the set of real linear simultaneous equations
!                           TX = Z
!   by a variation of the Levinson algorithm.  T is a complex  M+1
!   by  M+1  Hermitian Toeplitz matrix, Z is the known right-hand-
!   side real column vector of  M+1  elements,  and  X  is the
!   solution vector of  M+1  complex elements.
!
!   Input Parameters:
!
!     M  - Order of matrix T (integer)
!     T0 - Scalar corresponding to real matrix element t(0)
!     T  - Array of  M  real matrix elements t(1),...,t(M)
!          from the left column of the Toeplitz matrix
!     Z  - Array of M+1 real elements of the right-hand-side
!          vector.   Program element Z(k+1) corresponds to text
!          element z(k), for k=0 to k=M
!
!   Output Parameters:
!
!     X  - Array of  M+1  real elements of solution vector.
!          Program element X(k+1) corresponds to text element
!          x(k), for k=0 to k=M
!     A  - A scratch array of dimension M
!     ISTAT - Integer status indicator at time of exit
!             0 for normal exit
!             1 if P=0. (singular matrix)
!
!   Notes:
!
!   External array T must be dimensioned .GE. M and arrays X,Z must
!   be dimensioned .GE. M+1 in the calling program.  External array
!   A must be dimensioned .GE. M .
!
      real :: temp,save,alpha,beta
      real :: p
      integer :: j, k, khalf, kj

      p     = t0
      istat = 1

      if (p .eq. 0.0)  return
!
!   handle  m = 0  as a special case
!
      x(1) = z(1)/t0                            
      if (m .le. 0)  return
!
!   main recursion
!
      k = 0

100   k    = k+1
      save = t(k)
      beta = x(1)*t(k)

      if (k .eq. 1)  go to 20
      do 10 j = 1, k-1
        save = save + a(j)*t(k-j)                 
        beta = beta + x(j+1)*t(k-j)               
10    continue

20    temp = -save/p
      p    = p*(1. - temp**2)                        
      if (p .le. 0.0)  return

30    a(k)  = temp                               
      alpha = (z(k+1) - beta)/p                   

      if (k .eq. 1)  go to 50
      khalf = k/2
      do 40 j = 1, khalf
        kj   = k - j
        save = a(j)
        a(j) = save + temp*a(kj)                  
        if (j .eq. kj)  go to 40
        a(kj) = a(kj) + temp*save                 
40    continue

50    x(k+1) = alpha
      do 60 j = 1, k
        x(j) = x(j) + alpha*a(k-j+1)              
60    continue

      if (k .lt. m)  go to 100

      istat = 0

      return

      end subroutine avopcomp_hermtp

!!---------------------------- avopcomp_hilba ------------------------------!!
!!---------------------------- avopcomp_hilba ------------------------------!!
!!---------------------------- avopcomp_hilba ------------------------------!!
!
      subroutine avopcomp_hilba ( a, h, nt, is1, is2, ftrlen )
!
      implicit none
      real             ,intent(in)    :: a(:)     ! arguments (len=nt)
      real             ,intent(out)   :: h(:)     ! arguments (len=nt)
      integer          ,intent(in)    :: nt       ! arguments
      integer          ,intent(in)    :: is1      ! arguments
      integer          ,intent(in)    :: is2      ! arguments
      integer          ,intent(in)    :: ftrlen   ! arguments
!
!  This subroutine uses the real part of complex array "a" to
!  compute its Hilbert transform, which it places in the
!  imaginary part of the array.
!
!  Parameter list:
!
!  a    (in)      (r4)   A size "nt" real vector, which contains
!                        the data to be Hilbert transformed.
!  h    (out)     (r4)   A size "nt" real vector to receive the
!                        transformed data.
!  nt   (in)      (i4)   The length of the complex trace, "a".
!  is1  (in)      (i4)   The first sample to transform
!  is2  (in)      (i4)   The last sample to transform
!  ftrlen  (in)   (i4)   The transform filter length
!
!  Method:
!
!  The data are convolved with an FIR Hilbert transformer shown in
!  Rabiner and Schafer, "On the behavior of minimax FIR digital Hilbert
!  transformers", Bell Syst. Tech. J. Vol. 53 no. 2, p 380, (1974).
!
      integer ::   nlen, maxlen, len1, len2, len3, len4
      integer ::   l1, l2, l3, l4, maxl
      parameter (nlen=4, maxlen=95)
      parameter (len1=95,len2=63,len3=43,len4=31)
      parameter (l1=((len1+1)/4))
      parameter (l2=((len2+1)/4))
      parameter (l3=((len3+1)/4))
      parameter (l4=((len4+1)/4))
      parameter (maxl=((maxlen+1)/4))
      real ::      hltr(-maxlen:maxlen)
      real ::      filter(maxl,nlen)
      real ::      h1(l1), h2(l2), h3(l3), h4(l4)
      equivalence (filter(1,1),h1)
      equivalence (filter(1,2),h2)
      equivalence (filter(1,3),h3)
      equivalence (filter(1,4),h4)
      integer ::   ftsave, valid(nlen)
      integer ::   ndx, l, m, m2, lim2, lim1, n
      integer ::   i, lt1, lt2
      save ftsave, hltr
      data      valid   /len1,len2,len3,len4/
!
!  Coefficients for 95 point filter, fl=0.01
!
      data h1/-0.0130099, -0.0045718, -0.0053689, -0.0062800, &
              -0.0072616, -0.0083873, -0.0096455, -0.0110350, &
              -0.0126022, -0.0143770, -0.0163895, -0.0186922, &
              -0.0213465, -0.0244424, -0.0281175, -0.0325665, &
              -0.0380864, -0.0451608, -0.0546233, -0.0680547, &
              -0.0888468, -0.1258168, -0.2112989, -0.6363167/
!
!  Coefficients for 63 point filter, fl=0.02
!
      data h2/-0.0055706, -0.0044618, -0.0062078, -0.0083775, &
              -0.0110475, -0.0143195, -0.0183266, -0.0232716, &
              -0.0294397, -0.0373177, -0.0477262, -0.0622273, &
              -0.0841979, -0.1224340, -0.2092445, -0.6356280/
!
!  Coefficients for 43 point filter, fl=0.02
!
      data h3/-0.0216528, -0.0146215, -0.0196329, -0.0259590, &
              -0.0340917, -0.0448233, -0.0597557, -0.0821807, &
              -0.1209598, -0.2083547, -0.6353344/
!
!  Coefficients for 31 point filter, fl=0.05
!
      data h4/-0.0041956, -0.0092821, -0.0188358, -0.0344010, &
              -0.0595516, -0.1030376, -0.1968315, -0.6313536/
!
!  Initialize the output array.
!
      do 10 i=1,nt
        h(i) = 0.0
 10   continue
!
!  Is this ftrlen a valid length?
!
      if (ftsave .ne. ftrlen) then
        do 20 i=1,nlen
          ndx = i
          ftsave = valid(ndx)
          if (ftsave .eq. ftrlen) go to 30
 20     continue
        ftsave = 0
        return
!
!  Initialize the filter function.
!
 30     l = (ftsave + 1)/4
        do 40 i=0,l-1
          hltr(i)    = -filter(l-i,ndx)
          hltr(-i-1) = -hltr(i)
 40     continue
      endif
!
!  Perform the convolution
!
      l = (ftrlen + 1)/4
      lt1 = max0(1,is1)
      lt2 = min0(nt,is2)
      if (lt1 .gt. lt2) return

      do 100 m=-l,l-1
        m2 = m*2
        lim1 = m2 + 2
        lim2 = nt + m2 + 1
        if (lim1 .lt. lt1) lim1 = lt1
        if (lim2 .gt. lt2) lim2 = lt2
        do 150 n=lim1, lim2
          h(n) = h(n) + hltr(m)*a(n-m2-1)
 150    continue
 100  continue

      return

      end subroutine avopcomp_hilba
!!---------------------------- avopcomp_mirror ------------------------------!!
!!---------------------------- avopcomp_mirror ------------------------------!!
!!---------------------------- avopcomp_mirror ------------------------------!!
      function avopcomp_mirror ( angle_in, M_PI, M_PI_2 )
      implicit none
      double precision                :: avopcomp_mirror
!
      double precision ,intent(in)    :: angle_in  ! arguments
      double precision ,intent(in)    :: M_PI   ! arguments
      double precision ,intent(in)    :: M_PI_2 ! arguments
!
      double precision :: angle

      angle=angle_in
!
      do while (angle .gt. M_PI_2)
         angle = angle - M_PI
      end do

      do while (angle .lt. -M_PI_2)
         angle = angle + M_PI
      end do

      avopcomp_mirror = angle

      end function avopcomp_mirror

!!------------------------- avopcomp_phaseangles ----------------------------!!
!!------------------------- avopcomp_phaseangles ----------------------------!!
!!------------------------- avopcomp_phaseangles ----------------------------!!
      subroutine avopcomp_phaseangles(ensemble, thout, phiout, x0, &
        ns, nt, AVO_Sa, AVO_Sb, AVO_Rr, AVO_Ri)
!
      implicit none
      real             ,intent(inout) :: ensemble(:,:) ! arguments (len=ns,nt)
      real             ,intent(out)   :: thout(:)      ! arguments (len=ns)
      real             ,intent(out)   :: phiout(:)     ! arguments (len=ns)
      real             ,intent(in)    :: x0            ! arguments
      integer          ,intent(in)    :: ns            ! arguments
      integer          ,intent(in)    :: nt            ! arguments
      integer          ,intent(in)    :: AVO_Sa        ! arguments
      integer          ,intent(in)    :: AVO_Sb        ! arguments
      integer          ,intent(in)    :: AVO_Rr        ! arguments
      integer          ,intent(in)    :: AVO_Ri        ! arguments
!
!  This routine actually performs the AVO principal component analysis.
!
      double precision :: M_PI, M_PI_2, M_PI_4, T_PI_8, quotThresh, phi0
!
      parameter ( M_PI   = 3.14159265358979323846 ) ! PI
      parameter ( M_PI_2 = 1.57079632679489661923 ) ! PI/2
      parameter ( M_PI_4 = 0.78539816339744830962 ) ! PI/4
      parameter ( T_PI_8 = 1.1780972 )              ! 3*PI/8
      parameter ( phi0   = 0.0 )
!
      double precision :: thhat, phhat, theta, phi, P, Q, R, theta0
      double precision :: sa2, sb2, sm2, Trr, TR, rr2, rad, num, rip
      double precision :: sap2, sbp2, smp2, D, rmag, sapp, sbpp
      double precision :: theta_bias, phi_bias, oldth, oldphi, sapsbp
      double precision :: xMirror, sa, sb, rr, ri
!
      integer :: i, first, last
!
      quotThresh = 0.0001
      theta0     = M_PI_4
      theta_bias = 0.0
      phi_bias   = 0.0
      first      = 0
      last       = 0
!
      do i = ns, 1, -1
!
!  Retrieve the data at the i-th sample.
!
         sa = ensemble(i,AVO_Sa)
         sb = ensemble(i,AVO_Sb)
         rr = ensemble(i,AVO_Rr)
         ri = ensemble(i,AVO_Ri)
!
!  Skip this point if either standard deviation is zero.
!
         theta = theta0 + theta_bias
         phi   = phi0   + phi_bias
         sa2   = sa * sa
         sb2   = sb * sb
!
         if ( sa2 .ne. 0.0 .and. sb2 .ne. 0.0 ) then
!
            last = i
            if ( first .eq. 0 ) first = i
!
!  Calculate the tentative in-phase angle, thhat.
!
            thhat = theta0                ! pi/4
            sm2   = sa2 - sb2
            if ( abs(sm2) .le. (sa * sb * quotThresh) ) then
               if (rr .ge. 0.0) then
                  thhat = theta0
               else
                  thhat = -theta0
               end if
            else
               Q     = 2.0 * rr * sa * sb / sm2
               thhat = 0.5 * atan(Q)
               if ( sm2 .lt. 0.0 ) then
                  if (Q .lt. 0.0 ) then
                     thhat = thhat + M_PI_2
                  else
                     thhat = thhat - M_PI_2
                  end if
               end if
            end if
!
!  Stabilize it if the distribution is nearly spherical.
!
            theta = thhat
            R     = sqrt( abs(sm2) / (sa2 + sb2) )
            if ( abs(rr) .lt. x0 .and. R .lt. x0 ) then
               Trr    = avopcomp_thresh(rr / x0)
               TR     = avopcomp_thresh(R  / x0)
               theta  = (Trr + TR - Trr*TR) * thhat
               theta  = theta + (1.0 - Trr) * (1.0 - TR) * theta0
            end if
!
!  Phase unwrap the in-phase rotation angle.
!
            if ( i .eq. first ) oldth = theta
            xMirror = avopcomp_mirror( oldth, M_PI, M_PI_2 )
            if ( theta .lt. -T_PI_8 .and. xMirror .gt.  T_PI_8 ) &
               theta_bias = theta_bias + M_PI
            if ( theta .gt.  T_PI_8 .and. xMirror .lt. -T_PI_8 ) &
               theta_bias = theta_bias - M_PI
            theta = theta + theta_bias
            oldth = theta
!
!  Calculate the modified statistics after the first rotation.
!
            rr2  = rr*rr
            rad  = sqrt(sm2*sm2 + 4.0*rr2*sa2*sb2)
            num  = 2.0*(1-rr2)*sa2*sb2
            sap2 = num / (sa2 + sb2 - rad)
            sbp2 = num / (sa2 + sb2 + rad)
            rip  = ri  / sqrt(1.0 - rr2)
!
!  Calculate the tentative quadrature angle, phhat
!
            phhat  = phi0
            smp2   = sap2 - sbp2
            sapsbp = sqrt(sap2 * sbp2)
            if ( abs(smp2) .le. sapsbp * quotThresh ) then 
               if ( ri .ge. 0.0 ) then
                  phhat = theta0
               else
                  phhat = -theta0
               end if
            else 
               P = 2.0 * rip * sapsbp / smp2
               phhat = 0.5 * atan(P)
               if ( smp2 .lt. 0 ) then
                  if ( P .lt. 0 ) then
                     phhat = phhat + M_PI_2
                  else
                     phhat = phhat - M_PI_2
                  end if
               end if
            end if
!
!  Stabilize it if the distribution is nearly spherical.
!
            phi = phhat
            D   = sqrt(abs(smp2) / (sap2 + sbp2))
            if ( abs(rip) .lt. x0 .and. D .lt. x0) then
               Trr = avopcomp_thresh(rip / x0)
               TR  = avopcomp_thresh(D  /  x0)
               phi = (Trr + TR - Trr*TR) * phhat
               phi = phi + (1.0 - Trr) * (1.0 - TR) * phi0
            end if
!
!  Phase unwrap the quadrature rotation angle.
!
            if ( i .eq. first ) oldphi = phi
            xMirror = avopcomp_mirror( oldphi, M_PI, M_PI_2 )
            if ( phi .lt. -T_PI_8 .and. xMirror .gt.  T_PI_8 ) &
               phi_bias = phi_bias + M_PI
            if ( phi .gt.  T_PI_8 .and. xMirror .lt. -T_PI_8 ) &
               phi_bias = phi_bias - M_PI
            phi    = phi + phi_bias
            oldphi = phi
!
!  Calculate the modified statistics after the second rotation.
!
            rmag  = rr*rr + ri*ri
            rad   = sqrt( sm2*sm2 + 4.0*rmag*sa2*sb2 )
            num   = 2.0*(1.0 - rmag)*sa2*sb2
            sapp  = sqrt( num / (sa2 + sb2 - rad) )
            sbpp  = sqrt( num / (sa2 + sb2 + rad) )
!
!  Stuff the rotated statistics back into the ensemble.
!
            ensemble(i,AVO_Sa) = sapp
            ensemble(i,AVO_Sb) = sbpp
            ensemble(i,AVO_Rr) = 0.0
            ensemble(i,AVO_Ri) = 0.0

         end if
!
!  Convert from radians into degrees.
!
         theta     = theta * 180.0 / M_PI
         phi       = phi * 180.0 / M_PI
         thout(i)  = theta
         phiout(i) = phi

      end do
!
!  Extend the angles beyond the first live value.
!
      do i = first+2, ns
         thout(i)  = thout(first)
         phiout(i) = phiout(first)
      end do
!
      do i = 1, last
         thout(i)  = thout(last)
         phiout(i) = phiout(last)
      end do
!
!  Return to the calling routine.
!
      return
!
      end subroutine avopcomp_phaseangles
!!---------------------------- avopcomp_prestack ---------------------------!!
!!---------------------------- avopcomp_prestack ---------------------------!!
!!---------------------------- avopcomp_prestack ---------------------------!!
!
      subroutine avopcomp_prestack(ne, nt, ens, sims, nmo, angle, T0, Vrms,  &
        Sq, Bstr, Bqstr, theta, phi, offset, ns, trace, AVO_Br, AVO_Bi,      &
        AVO_Sa, AVO_Sb)
!
      implicit none
      integer          ,intent(in)    :: ne        ! arguments
      integer          ,intent(in)    :: nt        ! arguments
      real             ,intent(inout) :: ens(:,:)  ! arguments (len=ne,nt)
      real             ,intent(in)    :: sims      ! arguments
      character(len=*) ,intent(in)    :: nmo       ! arguments (len=3)
      real             ,intent(in)    :: angle     ! arguments
      real             ,intent(in)    :: T0(:)     ! arguments (len=ns)
      real             ,intent(in)    :: Vrms(:)   ! arguments (len=ns)
      real             ,intent(in)    :: Sq(:)     ! arguments (len=ns)
      real             ,intent(in)    :: Bstr(:)   ! arguments (len=ns)
      real             ,intent(in)    :: Bqstr(:)  ! arguments (len=ns)
      real             ,intent(in)    :: theta(:)  ! arguments (len=ns)
      real             ,intent(in)    :: phi(:)    ! arguments (len=ns)
      double precision ,intent(in)    :: offset    ! arguments
      integer          ,intent(in)    :: ns        ! arguments
      integer          ,intent(in)    :: trace     ! arguments
      integer          ,intent(in)    :: AVO_Br    ! arguments
      integer          ,intent(in)    :: AVO_Bi    ! arguments
      integer          ,intent(in)    :: AVO_Sa    ! arguments
      integer          ,intent(in)    :: AVO_Sb    ! arguments
!
!  Perform a transformation of a prestack trace consistent
!  with the rotational transformation being performed on the AVO
!  intercept A and gradient B.
!
!
!  Declare the local variables:
!
      integer :: i, j
      double precision :: x, velocity, off2, tv2, vr, frac
      double precision :: o1r, o1i, o2r, o2i, time, vi, br, bi, sa, sb
      double precision :: sinth, costh, sinph, cosph, si, bs, bq
      double precision :: c11r, c11i, c12r, c12i, c21r, c21i, c22r, c22i

      double precision :: RADIAN
      parameter ( RADIAN = 57.29578 )
!
!  Get the time & incidence angle.
!
      si   = 1e-3 * sims     ! convert from ms to sec

      if ( angle .ne. 0.0 ) then
         x = sin(angle/RADIAN)
         x = x * x
      end if
!

      do i = 1, ns
!
         j    = i
         frac = 0
         time = i * si
!
!  Get zero-offset time, if OMN (reverse NMO)
!
         if ( nmo .eq. 'NO' ) then
            time = T0(i)
            j    = time / si
            frac = (time - j*si) / si

            if ( j .eq. 0 ) j = j + 1

         end if
!
         velocity = Vrms(j)
!
         if ( angle .eq. 0.0 ) then
            x = 0
            off2 = offset * offset
            tv2  = time * velocity
            tv2  = tv2 * tv2
            if ( off2 .gt. 0.0 .or. tv2 .gt. 0.0 ) then
               x = off2 / (off2 + tv2)
            end if
         end if
!
!  Ignore the muted zones.
!
         if (ens(j,AVO_Sa) .le. 0.0 .or. ens(j,AVO_Sb) .le. 0.0 ) then
            ens(i,trace) = 0
         end if
!
!  Compute the sines and cosines of the angles.
!
         sinth = sin(theta(j)/RADIAN)
         costh = cos(theta(j)/RADIAN)
         sinph = sin(phi(j)/RADIAN)
         cosph = cos(phi(j)/RADIAN)
!
!  Compute the "c" matrix.
!
         c11r =  cosph * costh
         c11i = -sinph * sinth
         c12r =  cosph * sinth
         c12i =  sinph * costh
         c21r = -c12r
         c21i =  c12i
         c22r =  c11r
         c22i = -c11i

!
!  Compute the "O" matrix.
!
         o1r  = c11r + c21r * x
         o1i  = c11i + c21i * x
         o2r  = c12r + (c22r - c11r) * x
         o2i  = c12i + (c22i - c11i) * x
!
!  Linearly interpolate the current complex sample.
!
         vr  = ens(i,trace)
         vi  = Sq(i)
         br  = ens(j,AVO_Br)
         bi  = ens(j,AVO_Bi)
         sa  = ens(j,AVO_Sa)
         sb  = ens(j,AVO_Sb)

!
         if ( nmo .eq. 'YES' ) then 
            bs = Bstr(j)
            bq = Bqstr(j)
         else
            bs = 0.0
            bq = 0.0
         end if
!
         if ( frac .ne. 0.0 .and. j .lt. (ns-1) ) then
            br = br + frac * (ens(j+1,AVO_Br) - br)
            bi = bi + frac * (ens(j+1,AVO_Bi) - bi)
            if ( nmo .eq. 'YES' ) then
               bs = bs + frac * (Bstr(j+1)  - bs)
               bq = bq + frac * (Bqstr(j+1) - bq)
            end if
         end if
!
!  Perform the correction of the prestack sample.
!
         vr = vr * o1r
         vr = vr - o1i * vi
         vr = vr + o2r * br
         vr = vr - o2i * bi
         vr = vr - c12r * x * bs
         vr = vr + c12i * x * bq
         ens(i,trace) = vr
!
      end do
!
!  Return to the calling routine
!
      return
!
      end subroutine avopcomp_prestack
!!---------------------------- avopcomp_remute -----------------------------!!
!!---------------------------- avopcomp_remute -----------------------------!!
!!---------------------------- avopcomp_remute -----------------------------!!
!
      subroutine avopcomp_remute(NS, Trace, Sstart, Send)
!
      implicit none
      integer          ,intent(in)    :: NS        ! arguments
      real             ,intent(out)   :: Trace(:)  ! arguments (len=NS)
      integer          ,intent(in)    :: Sstart    ! arguments
      integer          ,intent(in)    :: Send      ! arguments
!
      integer :: I
!
!  Reapply the mute pattern after the application of avopcomp_prestack
!  overburden correction.
!
      Do I = 1, Sstart-1
         Trace(I) = 0.0
      End Do
!
      Do I = NS, Send+1, -1
         Trace(I) = 0.0
      End Do
!
      Return
!
      end subroutine avopcomp_remute
!!---------------------------- avopcomp_rikflt -----------------------------!!
!!---------------------------- avopcomp_rikflt -----------------------------!!
!!---------------------------- avopcomp_rikflt -----------------------------!!
!
      subroutine avopcomp_rikflt(h1, h2, corr1, corr2, work, aux, mxh, nh, &
        w0t, pctwns, pctcns, a, ier)
!
      implicit none
      integer          ,intent(in)    :: mxh            ! arguments
      real             ,intent(out)   :: h1(-mxh:mxh)   ! arguments (-mxh:mxh)
      real             ,intent(out)   :: h2(-mxh:mxh)   ! arguments (-mxh:mxh)
      real             ,intent(out)   :: corr1(0:mxh)   ! arguments (len=0:mxh)
      real             ,intent(out)   :: corr2(:)       ! arguments (len=mxh)
      real             ,intent(out)   :: work(-mxh:mxh) ! arguments (-mxh:mxh)
      double precision ,intent(out)   :: aux(:)         ! arguments (4*mxh)
      integer          ,intent(in)    :: nh             ! arguments
      real             ,intent(in)    :: w0t            ! arguments
      real             ,intent(in)    :: pctwns         ! arguments
      real             ,intent(in)    :: pctcns         ! arguments
      double precision ,intent(in)    :: a              ! arguments
      integer          ,intent(out)   :: ier            !    arguments
!
!  subroutine avopcomp_rikflt
!  __________________________
!
!
!  This subroutine generates the h1(t) and h2(t) inversion
!  filters for NMO destretching, specifically for a Ricker wavelet.
!
!  Parameters entered:
!
!       MXPTS           The dimensioned size of the WAVELT and
!                       TWVLTP arrays  (I4, scalar)
!
!       MXH             The dimensioned size of the output inversion
!                       filters  (I4, scalar)
!
!       NH              The actual size of the output inversion
!                       filters  (I4, scalar)
!
!       W0T             Omega 0 times the time step size:  (unitless,
!                       R4, scalar)
!
!       PCTWNS          The additional white noise percentage (R4, scalar)
!
!       PCTCNS          The additional colored noise percentage (R4, scalar)
!
!       A               The autoregressive noise coefficient.
!                       Rn(n) = PCTNS * 1e-2 * Rw(0) * a ** n
!
!  Parameters returned:
!
!       H1              The h1 inversion filters.  R4
!                       dimensioned:  (-MXH:MXH)
!
!       H2              The h2 inversion filters.  R4
!                       dimensioned:  (-MXH:MXH)
!
!       CORR1           The autocorreltation of the wavelet.
!                       R4, 1-D array dimensioned:  (0:MAXH)
!
!       CORR2           The autocorreltation of the wavelet.
!                       R4, 1-D array dimensioned:  (MXH+1:2*MXH)
!
!       WORK            A work array.  R4, 1-D array,
!                           dimensioned:  (-MXH:MXH)
!
!       AUX             A scratch array dimensioned KS*MXH
!                           where "KS" is 3 for the PC version,
!                           and 4 for the IBM mainframe version.
!                       KS=1:  The cross correlation between w(t) and
!                               tw'(t)
!                       KS>1:  A work area for subroutine avopcomp_swleve.
!
!       IER             An error flag:  0 = normal completion,
!                       1 = Autocorrelation matrix was singular.
!                       2 = MXH   was not large enough.
!                       (I4, scalar)
!
!   Method:
!
!       Using the functional form for a Ricker wavelet:
!
!                                  2                  2
!               w(t) = (1 - 0.5*W0T ) * exp (-0.25*W0T ) ,
!
!       the autocorrelation of w(t) and cross correlation between
!       w(t) and [tw'(t)] are explicitly evaluated.  The inversion
!       filters are then obtained through conventional Levinson
!       inversions.
!
!   Restrictions:  (Checked by IER flag)
!
!       1)  PCTNS > 0.0
!       2)  NH <= MXH
!
!
!   Subroutines called:
!
!       avopcomp_swleve -  ESSV routine to perform a Levinson inversion.
!
!
        integer KS, maxlag, naux, lag, ier1
        integer i, ip, ier2
        real t, t2
        real c0, c1, cval, THRESH
        parameter (KS=3)
        parameter (THRESH=180.0)
        double precision an
!
!  Check the array sizes.
!
        ier = 0
        maxlag = 2*nh
        naux = 4*mxh
        if (pctwns .le. 0.0) ier = 1
        if (nh .gt. mxh) ier = 2
        if (abs(a) .ge. 1.0) ier = 3
        if (ier .ne. 0) return
!
!  Compute the wavelet's autocorrelation function.
!
        do 200 lag = 0, maxlag
          t = lag * w0t
          t2 = t*t
          corr1(lag) = 0.0
          if (t2.lt.THRESH) then
            cval = (48 - (24 - t2)*t2) * exp(-t2/8)
            if (lag.gt.mxh) then
              ip=lag-mxh
              corr2(ip) = cval
            else
              corr1(lag) = cval
            end if
          end if
 200    continue
!
!  Compute the h1(t) inversion filter.
!
        do 300 lag = 0, nh
          if (lag.gt.mxh) then
            ip=lag-mxh
            work(lag)  = corr2(ip)
            work(-lag) = corr2(ip)
          else
            work(lag)  = corr1(lag)
            work(-lag) = corr1(lag)
          end if
 300    continue

        c0 = corr1(0) * pctwns * 1e-2
        c1 = corr1(0) * pctcns * 1e-2
        corr1(0) = corr1(0) + c0
        an = c1

        do 350 i=0, maxlag
          if (i.gt.mxh) then
            ip=i-mxh
            corr2(ip) = corr2(ip) + an
          else
            corr1(i) = corr1(i) + an
          end if
          an      = an * a
 350    continue

        call avopcomp_swleve(corr1, corr2, 1, work(-nh:), 1, h1(-nh:), 2, &
          maxlag+1, aux, naux, ier1)
!
!  Compute the cross correlation between w(t) and tw'(t).
!
        do 500 lag = 0, nh
          t = lag * w0t
          t2 = t*t
          work(lag) = 0.0
          if (t2.le.THRESH) then
            work(lag) = -(((t2 - 36)*t2 + 144)*t2 + 192) * &
              exp(-t2/8)/8
          end if
          work(-lag) = work(lag)
 500    continue
!
!  Compute the h2(t) filter.
!
        call avopcomp_swleve(corr1, corr2, 1, work(-nh:), 1, h2(-nh:), 2, &
            maxlag+1, aux, naux, ier2)

        an = c1
        corr1(0) = corr1(0) - c0
        do 600 i=0, maxlag
          if (i.gt.mxh) then
            ip=i-mxh
            corr2(ip) = corr2(ip) - an
          else
            corr1(i) = corr1(i) - an
          end if
          an      = an * a
 600    continue
        ier = max0(ier1, ier2)

        return
        end subroutine avopcomp_rikflt
!!---------------------------- avopcomp_rotation ---------------------------!!
!!---------------------------- avopcomp_rotation ---------------------------!!
!!---------------------------- avopcomp_rotation ---------------------------!!
!
      subroutine avopcomp_rotation(ensemble, thin, phiin, M_PI, ns, nt, &
                                   AVO_Ar, AVO_Ai, AVO_Br, AVO_Bi)
!
      implicit none
      real             ,intent(inout) :: ensemble(:,:)  ! arguments (len=ns,nt)
      real             ,intent(in)    :: thin(:)        ! arguments (len=ns)
      real             ,intent(in)    :: phiin(:)       ! arguments (len=ns)
      double precision ,intent(in)    :: M_PI           ! arguments
      integer          ,intent(in)    :: ns             ! arguments
      integer          ,intent(in)    :: nt             ! arguments
      integer          ,intent(in)    :: AVO_Ar         ! arguments
      integer          ,intent(in)    :: AVO_Ai         ! arguments
      integer          ,intent(in)    :: AVO_Br         ! arguments
      integer          ,intent(in)    :: AVO_Bi         ! arguments
!
      integer ::  i
!
      double precision :: theta, phi, costh, sinth, cosph, sinph
      double precision :: ar, br, ai, bi, arp, aip, brp, bip
      double precision :: arpp, aipp, brpp, bipp
!
!  Convert from degrees to radians.
!
      do i = 1, ns

         theta = thin(i)  * M_PI / 180.0
         phi   = phiin(i) * M_PI / 180.0
!
!  Retrieve the original data.
!
         ar = ensemble(i,AVO_Ar)
         br = ensemble(i,AVO_Br)
         ai = ensemble(i,AVO_Ai)
         bi = ensemble(i,AVO_Bi)

!  Perform the first data rotation.
!
         costh =  cos(theta)
         sinth =  sin(theta)
         arp   =  ar*costh + br*sinth
         aip   =  ai*costh + bi*sinth
         brp   = -ar*sinth + br*costh
         bip   = -ai*sinth + bi*costh
!
!  Perform the second data rotation.
!
         sinph =  sin(phi)
         cosph =  cos(phi)
         arpp  =  arp*cosph - bip*sinph
         aipp  =  aip*cosph + brp*sinph
         brpp  = -aip*sinph + brp*cosph
         bipp  =  arp*sinph + bip*cosph
!
!  Return the rotated data.
!
         ensemble(i,AVO_Ar) = arpp
         ensemble(i,AVO_Br) = brpp
         ensemble(i,AVO_Ai) = aipp
         ensemble(i,AVO_Bi) = bipp

      end do
!
!  Return to the calling routine.
!
      return
!
      end subroutine avopcomp_rotation
!!--------------------------- avopcomp_scantrace ----------------------------!!
!!--------------------------- avopcomp_scantrace ----------------------------!!
!!--------------------------- avopcomp_scantrace ----------------------------!!
      subroutine avopcomp_scantrace(data, ns, is1, is2)
!
      implicit none
      real             ,intent(in)    :: data(:)   ! arguments (len=ns)
      integer          ,intent(in)    :: ns        ! arguments
      integer          ,intent(out)   :: is1       ! arguments
      integer          ,intent(out)   :: is2       ! arguments
!
      integer :: i
!
!  First, scan for the first live value
!
      i   = 0
      is1 = 0
      do while ( i .lt. ns .and. is1 .eq. 0 )
         i = i + 1
         if ( data(i) .ne. 0.0 ) is1 = i
      end do
!
!  Scan for its last live value
!
      i   = ns + 1
      is2 = 0
      do while ( i .gt. 0 .and. is2 .eq. 0 )
         i = i - 1
         if ( data(i) .ne. 0.0 ) is2 = i
      end do

      return

      end subroutine avopcomp_scantrace

!!---------------------------- avopcomp_swleve -----------------------------!!
!!---------------------------- avopcomp_swleve -----------------------------!!
!!---------------------------- avopcomp_swleve -----------------------------!!
      subroutine avopcomp_swleve(x1, x2, incx, u, incu, y, incy, n, aux, &
                                 naux, ier)
!
      implicit none
      real             ,intent(in)    :: x1(:)   ! arguments (len=incx)
      real             ,intent(in)    :: x2(:)   ! arguments (len=incx)
      integer          ,intent(in)    :: incx    ! arguments
      real             ,intent(in)    :: u(:)    ! arguments (len=incu)
      integer          ,intent(in)    :: incu    ! arguments
      real             ,intent(out)   :: y(:)    ! arguments (len=incy)
      integer          ,intent(in)    :: incy    ! arguments
      integer          ,intent(in)    :: n       ! arguments
      double precision ,intent(out)   :: aux(:)  ! arguments (len=naux)
      integer          ,intent(in)    :: naux    ! arguments
      integer          ,intent(out)   :: ier     ! arguments

      integer :: i

      call avopcomp_hermtp(n-1, x1(1), x2, u, aux, aux(n+1:), ier)
      do 100 i = 1, n
         y(i) = aux(i)
100   continue

      return

      end subroutine avopcomp_swleve

!!------------------------------ avopcomp_t0 -------------------------------!!
!!------------------------------ avopcomp_t0 -------------------------------!!
!!------------------------------ avopcomp_t0 -------------------------------!!
      subroutine avopcomp_t0 (NumSamp, SampInt, Offset, Velocity, AuxT0)
!
      implicit none
      integer          ,intent(in)    :: NumSamp     ! arguments
      real             ,intent(in)    :: SampInt     ! arguments
      double precision ,intent(in)    :: Offset      ! arguments
      real             ,intent(in)    :: Velocity(:) ! arguments (len=numsamp)
      real             ,intent(out)   :: AuxT0(:)    ! arguments (len=numsamp)
!
!  This routine computes T0(t), the zero-offset time as
!  a function of a moved-out time, t.  This operation is not
!  necessary for NMO-corrected data.
!
      Double Precision t0, t, v2, tArg, Time, tOld
      integer :: I, J
      Real    SIsec
!
      I = 1
      J = 1
      SIsec = 0.001 * SampInt
      tArg  = SIsec
      tOld  = Offset / Velocity(I)
      AuxT0(1) = 0
!
      Do While ( J .LT. NumSamp )
!
         t0 = I * SIsec
         v2 = Offset / Velocity(I)
         t  = SQRT( t0*t0 + v2*v2 )
!
         Do While ( tArg .LE. t .AND. J .LT. NumSamp )
!
            Time = ( I + (tArg-tOld) / (t-tOld) - 1 ) * SIsec
            IF ( Time .LT. 0 ) Time = 0.0
            AuxT0(J) = Time
            J    = J + 1
            tArg = SIsec * J
!
         End Do
!
         tOld = t
         I    = I + 1
!
      End Do
!
      Return
!
      end subroutine avopcomp_t0

!!---------------------------- avopcomp_thresh -----------------------------!!
!!---------------------------- avopcomp_thresh -----------------------------!!
!!---------------------------- avopcomp_thresh -----------------------------!!
      function avopcomp_thresh (value)
      implicit none
      Double Precision                :: avopcomp_thresh
!
      Double Precision ,intent(in)    :: value     ! arguments

      Double Precision output, fin

      output = 1.0

      fin    = abs ( value )

      if ( fin .lt. 1.0 ) output = fin

      avopcomp_thresh = output

      end function avopcomp_thresh

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine avopcomp_wrapup (obj)
      type(avopcomp_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine avopcomp_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module avopcomp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

