!<CPS_v1 type="PROCESS"/>
!!------------------------------ avocorr.f90 --------------------------------!!
!!------------------------------ avocorr.f90 --------------------------------!!
!!------------------------------ avocorr.f90 --------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:  avocorr_crou.c  avocorr_frou.f90  avocorr.h

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
! Name       : AVOCORR
! Category   : velocity_analysis
! Written    : 2004-01-29   by: Michael Ried
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Perform AVO Correlation Matching
! Portability: No known limitations. --> Change if needed.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This tool forms a linear combination of `A' and `B' such that the
! correlation coefficient between the sum and one of the original two inputs
! is a user specified value. This method of correlation is consistent with the
! correction of physical overburden losses.
!
! The angle-dependent component of overburden loss may be described by a
! function O(x), where x=sin2 q and where O(0)=1. The AVO model for seismic
! data S(x) is (A+Bx)O(x). Under this model, The apparent slope Bap = B +
! AO'(0), where B is the true slope, and O'(0) is the leakage term from A
! into B. This leakage term is generally unknown. However, if we do know from
! prior information that the correlation between A and B should be some
! number, then we can determine O'(0) so as to force the correlation between
! A and Bap to be this number. This effectively removes the effect of
! angle-dependent overburden loss.
!
! Another use of this tool is to compute DA and DB, which are the horizontal
! and vertical deviations of the data away their best estimate given the other
! variable.  They are defined as follows:
!
! DA = A - r (sa / sb) B ;
!
! DB = B - r* (sb / sa) A ,
!
! where sa and sb are the standard deviations of A and B, respectively and r
! is their correlation coefficient.
!
! Inputs and Outputs:
!
! Minimum input to this tool is a Standard Suite, consisting of the traces
! shown on the following page. Optional input to this tool are prestack data,
! passed through AVEL and the Standard Suite tool. This data may or may not be
! moved out, and must follow the standard suite traces in each ensemble. A
! velocity trace (trc_type 51) must appear between the standard suite and the
! prestack data. The prestack data is assumed to be x-t data, unless the
! ang_value trace header is present. In this case, it is assumed to be constant
! angle data, whose angle is contained in that header.
!
!   Trace                   Header trc_type
!
! Real AVO "A" trace              43
!
! Real AVO "B" trace              44
!
! Imag AVO "A" trace              17
!
! Imag AVO "B" trace              18
!
! Standard deviation "A"          45
!
! Standard deviation "B"          47
!
! Real correlation                48
!
! Imag correlation                41
!
!
! The output from this tool another Standard Suite whose component traces
! have been normalized. If prestack data was appended to the suite, it will
! be transformed in such a way as to produce the same intercept and gradient
! traces as the transformed traces passed in the suite.
!
! Parameters:
!
! Type of matching desired:
!
! You may specify one of the following options:
!
! D A - The complex `A' trace will be replaced by the horizontal deviation of
! `A' from the best estimate of `A' given `B'. The new correlation coefficient
! will be set to zero.
!
! D B - The complex `A' trace will be replaced by the horizontal deviation of
! `A' from the best estimate of `A' given `B'. The new correlation coefficient
! will be set to zero.
!
! Correlation Matching on A - The complex `A' trace will be replaced by
! A+kB, where k is chosen to make the correlation coefficient between
! the new `A' trace and `B' be the value specified below.
!
! Correlation Matching on B - The complex `B' trace will be replaced by
! B + kA, where k is chosen to make the correlation coefficient between
! `A' and the new `B' trace be the value specified below. This technique
! effectively reverses the physical effects of offset-dependent attenuation
! described above, since k will estimate -O'(0).
! 
! Real part of desired correlation coefficient:
!
! You may enter the real part of the desired correlation coefficient
! between the new `A' and `B' traces here. If either DA or DB correlation
! matching was specified, the program will internally set the desired
! correlation coefficient to zero.
!
! Do you wish a nonzero imaginary part?
! 
! Under most normal situations, you will want the imaginary part of the
! desired correlation coefficient to be zero. If the incoming data had
! quadrature components between `A' and `B,' they will be removed.
! However, it is possible to reintroduce a controlled amount of quadrature
! components into the data. One reason to do this might be to simulate the
! effects of an overall velocity error and test the robustness of downstream
! processing under these conditions.
!
! Imag part of desired correlation coefficient:
! 
! This field appears only if you answered the previous question affirmatively.
! It allows to specify an imaginary part of the desired correlation
! coefficient. Note that the magnitude of any correlation coefficient must
! be less than unity.
!
! Preserve the original variances of A and B?
!
! If `No,' then only the operations, A <-- A + kB or B <-- B + kA will be
! performed. These operations can change the standard deviations of the
! new seismic data in dramatic ways. A `Yes' to this question will subsequently
! scale the new `A' or `B' so as the retain their original standard deviations
! (variances).
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  Minimum input to this tool is a Standard Suite of AVO attributes, which may 
!  be generated with either AVOSTS (Standard Suite)or AVOANS (Alternate Norm
!  Suite).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!  Live seismic data           (Trace type #1)
!  Real AVO "A" trace          (Trace type #43)
!  Real AVO "B" trace          (Trace type #44)
!  Imag AVO "A" trace          (Trace type #17)
!  Imag AVO "B" trace          (Trace type #18)
!  Standard deviation "A"      (Trace type #45)
!  Standard deviation "B"      (Trace type #47)
!  Real correlation            (Trace type #48)
!  Imag correlation            (Trace type #41)
!  The stacking velocity trace (Trace type #51)
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!  Live seismic data           (Trace type #1)
!  Real AVO "A" trace          (Trace type #43)
!  Real AVO "B" trace          (Trace type #44)
!  Imag AVO "A" trace          (Trace type #17)
!  Imag AVO "B" trace          (Trace type #18)
!  Standard deviation "A"      (Trace type #45)
!  Standard deviation "B"      (Trace type #47)
!  Real correlation            (Trace type #48)
!  Imag correlation            (Trace type #41)
!  The stacking velocity trace (Trace type #51)
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
!   2     HDR_TOP_MUTE               Input Start time
!   6     HDR_OFFSET                 Input AVO central angle
!  49     HDR_USER_49                Input/Output trace type
!  53     HDR_USER_53                Input Dominate Frequency 
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  3. 2006-06-20  B. Menger     Removed Unused Variables.
!  2. 2005-01-03  Michael Ried  Updated the documentation to the program
!  1. 2004-01-29  Michael Ried  Initial version.
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
!<NS AVOCORR Process/NC=80>
!                       AVOCORR - AVO Correlation Matching
!`-------------------------------------------------------------------------
!Select the type of correlation matching[/R]  CORRTYP=`CCCCCCCCCCCCCCCCCCCC
!Preserve original variances of A and B?[/R]  LPRESRV=`KKK
!          Real part of correlation coef[/R]    COEFR=`FFFFFFFFFFFFFFFFFFFF
!     Imaginary part of correlation coef[/R]    COEFI=`FFFFFFFFFFFFFFFFFFFF
!           NMO applied to input traces?[/R]     LNMO=`KKK
!`-------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="COEFI">
!<Tip> -->Enter the imaginary part of the desired correlation coefficient</Tip>
! Default = --> 0.0
! Allowed = --> Real
! --> The imaginary part of the desired correlation coefficient
!</Help>
!
!<Help KEYWORD="COEFR">
!<Tip> -->Enter the real part of the desired correlation coefficient</Tip>
! Default = --> -0.5
! Allowed = --> Real
! --> The real part of the desired correlation coefficient
!</Help>
!
!<Help KEYWORD="CORRTYP">
!<Tip> -->Select the type of correlation matching wanted</Tip>
! Default = --> Matching on A
! Allowed = --> Matching on A
! Allowed = --> Matching on B
! Allowed = --> Delta-A
! Allowed = --> Delta-B
! --> Matching on A -- Correlation Matching on A
! --> Matching on B -- Correlation Matching on B
! --> Delta-A -- Correlation Matching on Delta-A
! --> Delta-B -- Correlation Matching on Delta-B
!</Help>
!
!<Help KEYWORD="LNMO">
!<Tip> -->Enter YES if the Input prestack traces are NMO'd</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'Yes', the program will assume that the traces are NMO'd
!</Help>
!
!<Help KEYWORD="LPRESRV">
!<Tip> -->Choose whether to preserve original variances of A and B</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> If 'Yes', then the original variances of A and B will be preserved.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module avocorr_module
      use pc_module
      use named_constants_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: avocorr_create
      public :: avocorr_initialize
      public :: avocorr_update
      public :: avocorr_delete
      public :: avocorr            ! main trace processing routine.
      public :: avocorr_wrapup

      character(len=100),public,save :: AVOCORR_IDENT = &
'$Id: avocorr.f90,v 1.3 2006/06/20 13:11:44 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: avocorr_struct

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

        real                       :: coefi
        real                       :: coefr
        character(len=21)          :: corrtyp
        logical                    :: lnmo
        logical                    :: lpresrv



! --> Insert any other needed variables or pointers here.
        integer                    :: icorrtyp
        integer                    :: ipresrv
        integer                    :: inmo
        integer                    :: ioffang

      end type avocorr_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(avocorr_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine avocorr_create (obj)
      type(avocorr_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in avocorr_create")


! --> Nullify any additional pointers in the OBJ data structure here.

      call avocorr_initialize (obj)
      end subroutine avocorr_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine avocorr_delete (obj)
      type(avocorr_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call avocorr_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in &
        &avocorr_delete")
      end subroutine avocorr_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine avocorr_initialize (obj)
      type(avocorr_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%coefi   = 0.0
      obj%coefr   = -0.5
      obj%corrtyp = 'Delta-A       '
      obj%lnmo = .TRUE.
      obj%lpresrv = .TRUE.

      obj%icorrtyp = 0
      obj%inmo = 1
      obj%ipresrv = 1
      obj%ioffang = 1


      call avocorr_update (obj)
      end subroutine avocorr_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine avocorr_update (obj)
      type(avocorr_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.

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

      call pc_get('COEFI  ', obj%coefi)
      call pc_get('COEFR  ', obj%coefr)
      call pc_get('CORRTYP', obj%corrtyp)
      call pc_get('LNMO   ', obj%lnmo)
      call pc_get('LPRESRV', obj%lpresrv)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).

!
! ..... Change options into integer values
!
      if (obj%corrtyp.eq.'Matching on A        ') then
        obj%icorrtyp=2
      else if (obj%corrtyp.eq.'Matching on B        ') then
        obj%icorrtyp=3
      else if (obj%corrtyp.eq.'Delta-A              ') then
        obj%icorrtyp=0
      else if (obj%corrtyp.eq.'Delta-B              ') then
        obj%icorrtyp=1
      else
        obj%icorrtyp=0
      end if
!
      if (obj%lpresrv) then
        obj%ipresrv = 1
      else
        obj%ipresrv = 0
      end if
!
      if (obj%lnmo) then
        obj%inmo = 1
      else
        obj%inmo = 0
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

! take out items that are not needed when an option is selected
      if (obj%icorrtyp.gt.1) then
        call pc_put_sensitive_field_flag ('COEFR',  .true.)
        call pc_put_sensitive_field_flag ('COEFI', .true.)
      else
        call pc_put_sensitive_field_flag ('COEFR', .false.)
        call pc_put_sensitive_field_flag ('COEFI', .false.)
      end if


      call pc_put_options_field('CORRTYP', (/'Matching on A        ',          &
        'Matching on B        ','Delta-A              ',                       &
        'Delta-B              ' /) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('COEFI  ', obj%coefi)
      call pc_put('COEFR  ', obj%coefr)
      call pc_put('CORRTYP', obj%corrtyp)
      call pc_put('LNMO   ', obj%lnmo)
      call pc_put('LPRESRV', obj%lpresrv)

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


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine avocorr_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine avocorr (obj,ntr,hd,tr)
      type(avocorr_struct),intent(inout) :: obj                 ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.

!      Output traces
      integer icorrtyp, inmo, ipresrv, ioffang, max_samps, max_trcs
      integer nhdrs, ntrc_gath_in, ntrc_gath_out
      real coefi, coefr, samp_int_in
      real            , dimension(size(tr,1),size(tr,2)) :: tro


! --> Insert code for processing logic.
      icorrtyp=obj%icorrtyp
      ipresrv=obj%ipresrv
      inmo=obj%inmo
      coefi=obj%coefi
      coefr=obj%coefr
      ioffang=obj%ioffang
!
!
!      INITIALIZE
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%numtr is max number of traces input/output)
!      (obj%dt is trace sample interval)
!
      max_samps=obj%ndpt
      nhdrs=obj%nwih
      ntrc_gath_in=ntr
      samp_int_in=obj%dt*1000.0
      ntrc_gath_out = ntrc_gath_in
      max_trcs = ntrc_gath_in
!
!      (Note: passing START_TIME to used for muting)
!
      call avocorr_ms(icorrtyp, ipresrv, ioffang, inmo, coefi, coefr, tr, &
     +  tro, max_samps, ntrc_gath_in, max_trcs, nhdrs, hdr_user_49,       &
     +  hdr_offset, hdr_top_mute, hdr_user_53, samp_int_in, hd)

      end subroutine avocorr
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine avocorr_wrapup (obj)
      type(avocorr_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine avocorr_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module avocorr_module

!!--------------------------- avocorr_hermtp -------------------------------!!
!!--------------------------- avocorr_hermtp -------------------------------!!
!!--------------------------- avocorr_hermtp -------------------------------!!
!A      Author                 H. W. Swan
!A      Designer               H. W. Swan
!A      Language               FORTRAN
!A      Written                01-01-93
!       Revised    01-25-94    ESN - Converted to ProSPARC.
!A
!A      CALLING SEQUENCE:
!A           CALL AVOCORR_HERMTP (M,T0,T,Z,X,A,ISTAT)
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
      SUBROUTINE AVOCORR_HERMTP (M,T0,T,Z,X,A,ISTAT)
!
      INTEGER ISTAT, M
      REAL   T0,T(M),Z(M+1)
!
      DOUBLE PRECISION TEMP,SAVE,ALPHA,BETA
      DOUBLE PRECISION X(M+1), A(M)
      DOUBLE PRECISION P
      INTEGER J, K, KHALF, KJ
!
      P=T0
      ISTAT=1
      IF (P .EQ. 0.)  RETURN

!   Handle  M=0  as a special case
      X(1)=Z(1)/T0                            
      IF (M .LE. 0)  RETURN
!
!   Main recursion
!
      K=0
100   K=K+1
      SAVE=T(K)
      BETA=X(1)*T(K)
      IF (K .EQ. 1)  GO TO 20
      DO 10 J=1,K-1
        SAVE=SAVE+A(J)*T(K-J)                 
        BETA=BETA+X(J+1)*T(K-J)
10    CONTINUE 
              
20    TEMP=-SAVE/P
      P=P*(1.-TEMP**2)                        
      IF (P .LE. 0.)  RETURN

      A(K)=TEMP 
                                   
      ALPHA=(Z(K+1)-BETA)/P                   
      IF (K .EQ. 1)  GO TO 50
      KHALF=K/2

      DO 40 J=1,KHALF
        KJ=K-J
        SAVE=A(J)
        A(J)=SAVE+TEMP*A(KJ)                  
        IF (J .EQ. KJ)  GO TO 40
        A(KJ)=A(KJ)+TEMP*SAVE                 
40    CONTINUE
        
50    X(K+1)=ALPHA
      DO 60 J=1,K
        X(J)=X(J)+ALPHA*A(K-J+1)
60    CONTINUE

                    
      IF (K .LT. M-1)  GO TO 100

      ISTAT=0
!
      END SUBROUTINE AVOCORR_HERMTP

!!--------------------------- avocorr_hilba --------------------------------!!
!!--------------------------- avocorr_hilba --------------------------------!!
!!--------------------------- avocorr_hilba --------------------------------!!
      subroutine avocorr_hilba(a,h,nt,is1,is2,ftrlen)
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
      integer   nlen, maxlen, len1, len2, len3, len4
      integer   l1, l2, l3, l4, maxl
      parameter (nlen=4, maxlen=95)
      parameter (len1=95,len2=63,len3=43,len4=31)
      parameter (l1=((len1+1)/4))
      parameter (l2=((len2+1)/4))
      parameter (l3=((len3+1)/4))
      parameter (l4=((len4+1)/4))
      parameter (maxl=((maxlen+1)/4))
      real      a(nt), h(nt)
      real      hltr(-maxlen:maxlen)
      real      filter(maxl,nlen)
      real      h1(l1), h2(l2), h3(l3), h4(l4)
      equivalence (filter(1,1),h1)
      equivalence (filter(1,2),h2)
      equivalence (filter(1,3),h3)
      equivalence (filter(1,4),h4)
      integer   ftsave, ftrlen, valid(nlen)
      integer   nt, is1, is2, ndx, l, m, m2, lim2, lim1, n
      integer   i, lt1, lt2
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
!     call arset(h,nt,0.0)
!       print *,'nt,is1,is2  ',nt,is1,is2
      do 10 i=1,nt
 10        h(i) = 0.0

!
!  Is this ftrlen a valid length?
!
      if (ftsave .ne. ftrlen) then
           do 20 i=1,nlen
                 ndx = i
                 ftsave = valid(ndx)
 20              if (ftsave .eq. ftrlen) go to 30
           ftsave = 0
           return
!
!  Initialize the filter function.
!
 30        l = (ftsave + 1)/4
           do 40 i=0,l-1
                 hltr(i)    = -filter(l-i,ndx)
                 hltr(-i-1) = -hltr(i)
 40              continue
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
 150             continue
 100        continue
      return
!
      end subroutine avocorr_hilba
!!-------------------------- avocorr_rikflt --------------------------------!!
!!-------------------------- avocorr_rikflt --------------------------------!!
!!-------------------------- avocorr_rikflt --------------------------------!!
!
!  subroutine avocorr_rikflt
!  _________________________
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
!       H               The inversion filters.  R4, 2-D array,
!                       dimensioned:  (2,-MXH:MXH)
!                       The first subscript means:
!                       1:  The h1(t) filter
!                       2:  The h2(t) filter
!
!       CORR            The autocorreltation of the wavelet.
!                       R4, 1-D array dimensioned:  (0:2*MAXH)
!
!       WORK            A work array.  R4, 1-D array,
!                           dimensioned:  (-MXH:MXH)
!
!       AUX             A scratch array dimensioned KS*MXH
!                           where "KS" is 3 for the PC version,
!                           and 4 for the IBM mainframe version.
!                       KS=1:  The cross correlation between w(t) and
!                               tw'(t)
!                       KS>1:  A work area for subroutine SWLEVE.
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
!       SWLEVE   -  ESSV routine to perform a Levinson inversion.
!
        subroutine avocorr_rikflt(h, corr, work, aux, mxh, nh, w0t, pctwns, &
            pctcns, a, ier)

        integer mxh,nh, ier, maxlag, naux, lag, ier1
        integer i, ier2 
        real h, corr, work, pctwns, pctcns, w0t, t, t2
        real c0, c1, THRESH
        
         
        parameter (THRESH=180.0)
        dimension corr(0:2*mxh)
        dimension h(2,-mxh:mxh), work(-mxh:mxh)
        double precision a, an, aux(4*mxh)
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
            corr(lag) = 0.0
            if (t2 .lt. THRESH) then
              corr(lag) = (48 - (24 - t2)*t2) * exp(-t2/8)
            endif
 200    continue
!
!  Compute the h1(t) inversion filter.
!
        
        do 300 lag = 0, nh
            work(lag)  = corr(lag)
            work(-lag) = corr(lag)
 300    continue   
            
 
        c0 = corr(0) * pctwns * 1e-2
        c1 = corr(0) * pctcns * 1e-2
        corr(0) = corr(0) + c0
        an = c1

        do 350 i=0, maxlag
            corr(i) = corr(i) + an
            an      = an * a
 350    continue   

           
        call avocorr_swleve(corr, 1, work(-nh), 1, h(1,-nh), 2, &
            maxlag+1, aux, naux, ier1)

           
         
!
!  Compute the cross correlation between w(t) and tw'(t).
!
        do 500 lag = 0, nh
            t = lag * w0t
            t2 = t*t
            work(lag) = 0.0
            if (t2 .le. THRESH) then
              work(lag)= -(((t2 - 36)*t2 + 144)*t2 + 192)*exp(-t2/8)/8
            endif
            work(-lag) = work(lag)
 500    continue   
!         if (lag.gt.0) goto 666
!
!  Compute the h2(t) filter.
!
        call avocorr_swleve(corr, 1, work(-nh), 1, h(2,-nh), 2, &
            maxlag+1, aux, naux, ier2)

        an = c1
        corr(0) = corr(0) - c0
        do 600 i=0, maxlag
            corr(i) = corr(i) - an
            an      = an * a
 600    continue   
  
        ier = max0(ier1, ier2)
        return
        end subroutine avocorr_rikflt

!!-------------------------- avocorr_swleve --------------------------------!!
!!-------------------------- avocorr_swleve --------------------------------!!
!!-------------------------- avocorr_swleve --------------------------------!!
!
!  Do a Levinsion inversion.  See IBM  ESSV manual for
!  documentation of calling parameters.
!
        subroutine avocorr_swleve(x,incx,u,incu,y,incy,n,aux,naux,ier)
        integer incx, incu, incy, n, naux, ier, i 
        real x, u, y
        dimension x(incx,n), u(incu,n), y(incy,n), aux(naux)
        double precision aux

        call avocorr_hermtp(n-1, x(1,1), x(1,2), u, aux, aux(n+1), ier)

        do 100 i=1,n
          y(1,i) = aux(i)
 100    continue
        return
        end subroutine avocorr_swleve


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

