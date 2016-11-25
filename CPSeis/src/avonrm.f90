!<CPS_v1 type="PROCESS"/>
!!------------------------------ avonrm.f90 --------------------------------!!
!!------------------------------ avonrm.f90 --------------------------------!!
!!------------------------------ avonrm.f90 --------------------------------!!

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
! Name       : AVONRM
! Category   : velocity_analysis
! Written    : 2004-01-27   by: walucas
! Revised    : 2005-01-03   by: walucas
! Maturity   : production
! Purpose    : AVO Normalization (AVONRM).
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! --> Insert description information here.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! --> Insert advice to the user here.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! --> Insert trace input requirements here.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! --> Insert how this process affects output traces.
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
!  1. 2005-01-03  walucas    Initial version.
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
!<NS General Parameters>
!                              AVO Normalization
!
!   `-General--------------------------------------------------------------------
!   | Normalization Basis =~~~~~~~~~~~~~`CCC
!   | Normalization Degree =~~~~~~~~~~~~`CCCCCC
!   | Normalization Level =~~~~~~~~~~~~~`FFFFFFFFF
!   | NMO Applied =~~~~~~~~~~~~~~~~~~~~~`CC
!   `----------------------------------------------------------------------------
!   <PARMS Normalization Basis   [NORM_BAS]>
!   <PARMS Normalization Degree  [NORM_DEG]>
!   <PARMS Normalization Level   [NORM_LVL]>
!   <PARMS NMO Applied           [NMO_APLD]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="NMO_APLD">
!<Tip> NMO application.</Tip>
! Default = YES
! Allowed = NO, YES
! Has NMO been applied to pre-stack traces?
!</Help>
!
!<Help KEYWORD="NORM_BAS">
!<Tip> Normalization basis.</Tip>
! Default = EACH
! Allowed = EACH, A, B
! Normalization basis:
!   EACH -> normalize A and B by their own standard deviations.
!   A    -> normalize A and B by the standard deviation of A.
!   B    -> normalize A and B by the standard deviation of B.
!</Help>
!
!<Help KEYWORD="NORM_DEG">
!<Tip> Degree of normalization.</Tip>
! Default = --> Insert default value
! Allowed = --> Insert allowed values
! Normalization degree:
!   FULL    -> full normalization by the standard deviation.
!   PARTIAL -> partial normalization by square root of the standard deviation.
!</Help>
!
!<Help KEYWORD="NORM_LVL">
!<Tip> Normalization level.</Tip>
! Default = 1.0
! Allowed = greater than or equal to 0, less than or equal to 1E+15
! Normalization level. Scale trace by this number after normalization.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module avonrm_module
      use pc_module
      use named_constants_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use ppavo_module

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: avonrm_create
      public :: avonrm_initialize
      public :: avonrm_update
      public :: avonrm_delete
      public :: avonrm            ! main trace processing routine.
      public :: avonrm_wrapup
      public :: avonrm_compT0
      public :: avonrm_find_mute
      public :: avonrm_apply_mute
      public :: avonrm_normalize
      public :: avonrm_norm_prestack
      public :: avonrm_scan_prestack
      public :: avonrm_check_suite

      character(len=100),public,save :: AVONRM_IDENT = &
'$Id: avonrm.f90,v 1.1 2005/01/03 13:59:52 walucas prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: avonrm_struct

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

        character(len=3)           :: nmo_apld
        character(len=4)           :: norm_bas
        character(len=7)           :: norm_deg
        real                       :: norm_lvl

        real, pointer, dimension(:) :: work

        integer                     :: hdtype(8)
        integer                     :: maxdtrz
        integer                     :: numsmpz
        integer                     :: nthz

        integer                     :: gath_type

        logical                     :: initialized



! --> Insert any other needed variables or pointers here.

      end type avonrm_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(avonrm_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine avonrm_create (obj)
      type(avonrm_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in avonrm_create")

! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%work)

      obj%initialized = .false.

      call avonrm_initialize (obj)
      end subroutine avonrm_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine avonrm_delete (obj)
      type(avonrm_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call avonrm_wrapup (obj)

! --> Deallocate any additional pointers in the OBJ data structure here.
      if(associated(obj%work)) deallocate(obj%work)
      nullify(obj%work)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in&
     &avonrm_delete")
      end subroutine avonrm_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine avonrm_initialize (obj)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%nmo_apld = 'YES'
      obj%norm_bas = 'EACH'
      obj%norm_deg = 'FULL'
      obj%norm_lvl = 1.0

      obj%hdtype(1) = ppavo_HVO_Ar
      obj%hdtype(2) = ppavo_HVO_Br
      obj%hdtype(3) = ppavo_HVO_Ai
      obj%hdtype(4) = ppavo_HVO_Bi
      obj%hdtype(5) = ppavo_HVO_Sa
      obj%hdtype(6) = ppavo_HVO_Sb
      obj%hdtype(7) = ppavo_HVO_Rr
      obj%hdtype(8) = ppavo_HVO_Ri

      obj%gath_type = 0

      call avonrm_update (obj)
      end subroutine avonrm_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine avonrm_update (obj)
      type(avonrm_struct),intent(inout),target :: obj             ! arguments

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

      call pc_get('NMO_APLD', obj%nmo_apld, avonrm_nmo_apld)
      call pc_get('NORM_BAS', obj%norm_bas, avonrm_norm_bas)
      call pc_get('NORM_DEG', obj%norm_deg, avonrm_norm_deg)
      call pc_get('NORM_LVL', obj%norm_lvl, avonrm_norm_lvl)

!      *** screen traps ***

      call pc_call_screen_trap('GENERALPARAMETERS', avonrm_generalparameters)

!      *** end trap ***

      call pc_call_end_trap(avonrm_end)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('NMO_APLD', (/'NO ', 'YES'/) )
      call pc_put_options_field('NORM_BAS', (/'EACH', 'A   ', 'B   '/) )
      call pc_put_options_field('NORM_DEG', (/'FULL   ', 'PARTIAL'/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('NMO_APLD', obj%nmo_apld)
      call pc_put('NORM_BAS', obj%norm_bas)
      call pc_put('NORM_DEG', obj%norm_deg)
      call pc_put('NORM_LVL', obj%norm_lvl)

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


      end subroutine avonrm_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! *** Trap for variable NMO_APLD ***

      subroutine avonrm_nmo_apld(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avonrm_nmo_apld

! *** Trap for variable NORM_BAS ***

      subroutine avonrm_norm_bas(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avonrm_norm_bas

! *** Trap for variable NORM_DEG ***

      subroutine avonrm_norm_deg(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avonrm_norm_deg

! *** Trap for variable NORM_LVL ***

      subroutine avonrm_norm_lvl(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%norm_lvl)
      if(object%norm_lvl .lt. 0.0 .or. object%norm_lvl .gt. 2.0) then
         write (msg, '(a,a)') keyword,&
     &' must be greater than 0.0.&
     & Restoring to default of 1.0.'
         call pc_error(msg)
         object%norm_lvl = 1.0
         call pc_put(keyword, object%norm_lvl)
      end if

      return
      end subroutine avonrm_norm_lvl


! *** Screen trap for  GENERALPARAMETERS ***

      subroutine avonrm_generalparameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

! --> Insert code to validate data input
      return
      end subroutine avonrm_generalparameters

! *** End Trap ***

      subroutine avonrm_end
      implicit none

! --> Insert code to validate data input
      return
      end subroutine avonrm_end


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine avonrm (obj,ntr,hd,tr)
      type(avonrm_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: i, j
      integer :: index
      integer :: basisA, basisB
      integer :: flv, llv
      integer :: trc_type
      integer :: invalid
      integer :: prestack
      double precision :: offset

      obj%maxdtrz = obj%numtr
      obj%numsmpz = obj%ndpt
      obj%nthz    = obj%nwih

      if(.not. obj%initialized) then
         allocate(obj%work(obj%ndpt))
         obj%initialized = .true.
      end if

! --> Insert code for processing logic.
      select case(obj%norm_bas)
         case ('A')
            basisA = ppavo_AVO_Sa
            basisB = basisA
         case ('B')
            basisA = ppavo_AVO_Sb
            basisB = basisA
         case ('EACH')
            basisA = ppavo_AVO_Sa
            basisB = ppavo_AVO_Sb
      end select

      invalid = avonrm_check_suite(obj, hd, ntr, obj%nthz)
      if(invalid .lt. 0) then
         do i = 1, ntr
            do j = 1, obj%numsmpz
               tr(j,i) = 0.0
            end do
         end do
      else if(invalid .gt. 0) then
         do i = 1, ntr
            if(hd(ppavo_trc_typez,i) .eq. ppavo_HVO_K) then
               do j = 1, obj%numsmpz
                  tr(j,i) = 0.0
               end do
            end if
         end do
      else
         obj%gath_type = hd(ppavo_gath_typez, 1)
         prestack = avonrm_scan_prestack(obj, hd, ntr, obj%nthz)
         if(prestack .gt. 0) then
            do i = prestack, ntr
               index = i
               trc_type = hd(ppavo_trc_typez, i)
               offset   = hd(ppavo_offsetz, i)
               if(trc_type .eq. ppavo_HVO_Live) then
                  if(.not. obj%nmo_apld .eq. 'YES') then
                     call avonrm_compT0(obj, obj%ndpt, obj%dt, offset, &
     &                                   tr(1:,prestack), obj%work(1:))
                  end if
                  call avonrm_find_mute(obj, obj%ndpt, tr(1:,i), flv, llv)
                  call avonrm_norm_prestack(obj, obj%numtr, obj%ndpt, obj%dt,&
     &               obj%nmo_apld, obj%norm_lvl, obj%norm_deg, basisA, basisB,&
     &               offset, index, tr, tr(1:,prestack), obj%work(1:))
                  call avonrm_apply_mute(obj, obj%ndpt, tr(1:,i), flv, llv)

               end if
            end do
         end if

!           Now perform post-stack normalizations.
            call avonrm_normalize(obj, tr(1:, ppavo_AVO_Ar), tr(1:, basisA),&
     &         obj%norm_lvl, obj%norm_deg, obj%ndpt)
            call avonrm_normalize(obj, tr(1:, ppavo_AVO_Ai), tr(1:, basisA),&
     &         obj%norm_lvl, obj%norm_deg, obj%ndpt)
            call avonrm_normalize(obj, tr(1:, ppavo_AVO_Br), tr(1:, basisB),&
     &         obj%norm_lvl, obj%norm_deg, obj%ndpt)
            call avonrm_normalize(obj, tr(1:, ppavo_AVO_Bi), tr(1:, basisB),&
     &         obj%norm_lvl, obj%norm_deg, obj%ndpt)

            select case(obj%norm_bas)
               case ('A')
                  basisA = ppavo_AVO_Sa
                  basisB = ppavo_AVO_Sa
               case ('B')
                  basisA = ppavo_AVO_Sb
                  basisB = ppavo_AVO_Sb
               case ('EACH')
                  basisA = ppavo_AVO_Sa
                  basisB = ppavo_AVO_Sb
            end select

!           Set the new standard deviations.
!           Be careful! Don't change a statistic you are going to need later.
            if(obj%norm_bas .eq. 'A') then
               call avonrm_normalize(obj, tr(1:, ppavo_AVO_Sb), tr(1:, basisB),&
     &            obj%norm_lvl, obj%norm_deg, obj%ndpt)
               call avonrm_normalize(obj, tr(1:, ppavo_AVO_Sa), tr(1:, basisA),&
     &         obj%norm_lvl, obj%norm_deg, obj%ndpt)
            else
               call avonrm_normalize(obj, tr(1:, ppavo_AVO_Sa), tr(1:, basisA),&
     &            obj%norm_lvl, obj%norm_deg, obj%ndpt)
               call avonrm_normalize(obj, tr(1:, ppavo_AVO_Sb), tr(1:, basisB),&
     &            obj%norm_lvl, obj%norm_deg, obj%ndpt)
            end if

      end if

      end subroutine avonrm


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine avonrm_wrapup (obj)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine avonrm_wrapup


!!-------------------------- avonrm_compT0 --------------------------------!!
!!-------------------------- avonrm_compT0 --------------------------------!!
!!-------------------------- avonrm_compT0 --------------------------------!!

      subroutine avonrm_compT0 (obj, nsmp, smprt, offset, vel, aux)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

!     This routine computes t0(t), the zero-offset time as a
!     function of a moved-out time, t. This operation is not
!     necessary for nmo-corrected data.

      integer,            intent(in)    :: nsmp
      real,               intent(in)    :: smprt
      real, dimension(:), intent(in)    :: vel
      real, dimension(:), intent(inout) :: aux
      double precision,   intent(in)    :: offset

      integer          :: i, j
      real             :: sisec
      double precision :: t0, t, v2
      double precision :: targ, time, told

      i = 1
      j = 1
      sisec = smprt
      targ  = sisec
      told  = offset / vel(i)
      aux(1) = 0

      do while (j .lt. nsmp)
         t0 = i * sisec
         v2 = offset / vel(i)
         t = sqrt( t0*t0 + v2*v2 )
         do while (targ .le. t .and. j .lt. nsmp)
            time = (i + (targ-told) / (t-told) - 1) * sisec
            if (time .lt. 0) time = 0.0
            aux(j) = time
            j = j + 1
            targ = sisec * j
         end do
         told = t
         i = i + 1
      end do

      return
      end subroutine avonrm_compT0


!!------------------------ avonrm_find_mute --------------------------------!!
!!------------------------ avonrm_find_mute --------------------------------!!
!!------------------------ avonrm_find_mute --------------------------------!!

      subroutine avonrm_find_mute (obj, nsmp, trc, flv, llv)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

!     Search the data for the first and last non-zero values.

      integer,            intent(in)    :: nsmp
      integer,            intent(out)   :: flv
      integer,            intent(out)   :: llv
      real, dimension(:), intent(in)    :: trc

      integer :: i

      i  = 0
      flv = 0
      do while (i .lt. nsmp .and. flv .eq. 0)
         i = i + 1
         if (trc(i) .ne. 0.0) flv = i
      end do

      i   = nsmp+1
      llv = 0
      do while (i .gt. 0 .and. llv .eq. 0)
         i = i - 1
         if (trc(i) .ne. 0.0) llv = i
      end do

      return
      end subroutine avonrm_find_mute


!!----------------------- avonrm_apply_mute --------------------------------!!
!!----------------------- avonrm_apply_mute --------------------------------!!
!!----------------------- avonrm_apply_mute --------------------------------!!

      subroutine avonrm_apply_mute (obj, nsmp, trc, flv, llv)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

!     Apply the mute pattern (done after application of pre-stack
!     overburden correction).

      integer,            intent(in)  :: nsmp
      integer,            intent(in)  :: flv
      integer,            intent(in)  :: llv
      real, dimension(:), intent(out) :: trc

      integer :: i

      do i = 1, flv-1
         trc(i) = 0.0
      end do

      do i = nsmp, llv+1, -1
         trc(i) = 0.0
      end do
 
      return
      end subroutine avonrm_apply_mute


!!----------------------- avonrm_norm_prestack -----------------------------!!
!!----------------------- avonrm_norm_prestack -----------------------------!!
!!----------------------- avonrm_norm_prestack -----------------------------!!

      subroutine avonrm_norm_prestack (obj, ntr, nsmp, smprt,&
     &            nmo_apld, norm_lvl, norm_deg, basisA, basisB,&
     &            offset, index, tr, vel, t0)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

      integer,                                intent(in)    :: index
      integer,                                intent(in)    :: basisA
      integer,                                intent(in)    :: basisB
      integer,                                intent(in)    :: ntr
      integer,                                intent(in)    :: nsmp
      real,                                   intent(in)    :: smprt
      real,                                   intent(in)    :: norm_lvl
      real,             dimension(nsmp, ntr), intent(inout) :: tr
      real,             dimension(nsmp),      intent(inout) :: vel
      real,             dimension(nsmp),      intent(inout) :: t0
      double precision,                       intent(inout) :: offset
      character(len=*),                       intent(in)    :: nmo_apld
      character(len=*),                       intent(in)    :: norm_deg

      integer :: i, j
      logical :: nmo
      double precision :: angle, time, velocity
      double precision :: c11, c22, off2, tv2, x
      double precision :: basis, value
      double precision :: frac, br, radian
      character(len=6) :: angl_off
      parameter (radian=57.29578)

      angl_off = 'OFFSET'
      if(obj%gath_type .eq. 2) angl_off = 'ANGLE'

!     Get time and incidence angle.
      nmo = nmo_apld .eq. 'YES'
      if(angl_off .eq. 'ANGLE') then
         x = sin(offset/radian)
         x = x * x
      end if

      do i = 1, nsmp
         angle = 0.0
         time = (i-1) * smprt
         j = i
         frac = 0.0
         if(.not. nmo) then
            time = t0(i)
            j = time / smprt
            frac = (time - j*smprt) / smprt
         end if
         velocity = vel(j)
         if(angl_off .eq. 'OFFSET') then
            x = 0.0
            off2 = offset * offset
            tv2 = time * velocity
            tv2 = tv2 * tv2
            if(off2 .gt. 0.0 .or. tv2 .gt. 0.0) then
               x = off2 / (off2 + tv2)
            end if
         end if

!        Deduce c11 and c22.
         c11 = 0.0
         c22 = 0.0
         basis = tr(j, basisA)
         if(basis .gt. 0.0) then
            c11 = norm_lvl / basis
         end if
         basis = tr(j, basisB)
         if(basis .gt. 0.0) then
            c22 = norm_lvl / basis
         end if
         if(norm_deg .eq. 'PARTIAL') then
            c11 = sqrt(c11)
            c22 = sqrt(c22)
         end if

!        Construct the corrected pre-stack trace.
         value = tr(i, index)
         br    = tr(j, ppavo_AVO_Br)
         if(frac .ne. 0.0 .and. j .lt. nsmp) then
            br = br + frac * (tr(j+1, ppavo_AVO_Br) - br)
         end if
         value = c11 * value + (c22 - c11) * x * br
         tr(i, index) = value
      end do

      return
      end subroutine avonrm_norm_prestack


!!---------------------- avonrm_scan_prestack ------------------------------!!
!!---------------------- avonrm_scan_prestack ------------------------------!!
!!---------------------- avonrm_scan_prestack ------------------------------!!

      integer function avonrm_scan_prestack(obj, hd, ntr, nhd)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

      integer,                              intent(in) :: ntr
      integer,                              intent(in) :: nhd
      double precision, dimension(nhd,ntr), intent(in) :: hd

      integer :: i
      integer :: code
      integer :: trc_type

      code = 0
      do i = ppavo_AVO_NStd + 1, ntr
         trc_type = hd(ppavo_trc_typez, i)
         if(trc_type .eq. ppavo_HVO_Vs) then
            code = i
            exit
         end if
      end do
      avonrm_scan_prestack = code

      end function avonrm_scan_prestack


!!----------------------- avonrm_check_suite -------------------------------!!
!!----------------------- avonrm_check_suite -------------------------------!!
!!----------------------- avonrm_check_suite -------------------------------!!

      integer function avonrm_check_suite(obj, hd, ntr, nhd)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

      integer,                              intent(in) :: ntr
      integer,                              intent(in) :: nhd
      double precision, dimension(nhd,ntr), intent(inout) :: hd

      integer :: i, j
      integer :: code
      integer :: trc_type

      code = 0

!     Make sure ensemble is large enough. If not, pad and kill
!     the entire ensemble.
      if(ntr .lt. ppavo_AVO_NStd) then
         do i = 1, obj%maxdtrz
            if(i .gt. 1) then
               do j = 1, nhd
                  hd(j,i) = hd(j,1)
               end do
            end if
            hd(ppavo_trc_typez,i) = ppavo_HVO_K
            hd(ppavo_end_ensz,i)  = ppavo_nlastpz
            hd(ppavo_seq_noz,i)   = i
         end do
         hd(ppavo_end_ensz,obj%maxdtrz) = ppavo_lasttrpz
         avonrm_check_suite = -1
         return
      end if

!     Loop through each trace header in the suite.
!     If a trace is found which does not conform, kill it.
!     Also resequence them, just to make sure.
      do i = 1, ppavo_AVO_NStd
         hd(ppavo_seq_noz,i) = i
         trc_type = hd(ppavo_trc_typez, i)
         if(trc_type .ne. obj%hdtype(i)) then
            if(code .eq. 0) code = i
            hd(ppavo_trc_typez,i) = ppavo_HVO_K
         end if
      end do

!     If invalid, make sure at least the CDP numbers match.
      if(code .ne. 0) then
         do i = 2, ntr
            hd(ppavo_cdpz,i) = hd(ppavo_cdpz,1)
         end do
      end if

      avonrm_check_suite = code

      return
      end function avonrm_check_suite


!!------------------------ avonrm_normalize --------------------------------!!
!!------------------------ avonrm_normalize --------------------------------!!
!!------------------------ avonrm_normalize --------------------------------!!

      subroutine avonrm_normalize(obj, trc, basis, norm_lvl, norm_deg, nsmp)
      type(avonrm_struct),intent(inout) :: obj       ! arguments

!     Search the data for the first and last non-zero values.

      integer,            intent(in)    :: nsmp
      real,               intent(in)    :: norm_lvl
      character(len=*),   intent(in)    :: norm_deg
      real, dimension(:), intent(in)    :: basis
      real, dimension(:), intent(inout) :: trc

      integer          :: i
      double precision :: normv

      do i = 1, nsmp
         normv = basis(i)
         if(normv .gt. 0.0) then
            if(norm_deg .eq. 'PARTIAL') normv = sqrt(normv)
            trc(i) = trc(i) * (norm_lvl / normv)
         else
            trc(i) = 0.0
         end if
      end do

      return
      end subroutine avonrm_normalize


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module avonrm_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

