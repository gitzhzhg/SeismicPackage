!<CPS_v1 type="PROCESS"/>


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
!                        C P S   P R O C E S S
!
! Name       : PARALLELSORT
! Category   : sorts
! Written    : 2010-02-22   by: Tom Stoeckley
! Revised    : 2010-02-22   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Perform a parallel sort of large trace datasets.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! PARALLELSORT performs a parallel sort of large trace datasets across nodes
! in a parallel job.  This process first sorts the input trace streams on
! individual nodes, then merges the output sorted trace streams from the
! various nodes, putting the first traces on the first node and the last
! traces on the last node.  Alternatively, there is an option to put all of
! the sorted traces onto a single node.
!
! PARALLELSORT calls the TSORT process internally to sort the input trace streams
! on each node, and then uses the TMERGE primitive to merge the traces coming
! out of TSORT.  The TMERGE primitive uses MPI to move the traces to the correct
! node.
!
! If the job contains only a single node, PARALLELSORT works just like TSORT.
!
! See the TSORT process for further information about the sorting procedure.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace or all-trace (loop-splitting) process.
! Traces may be input as single traces or in gathers.
! No other special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces.
! This process outputs the same traces as it receives, but in different order.
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                           Action taken
! ----      -----------                           ------------
! NWIH      number of words in trace header       used but not changed
! NDPT      number of sample values in trace      used but not changed
! NUMTR     maximum number of traces in a gather  set to 1
! GATHERED  whether traces are gathered           set to false
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#           Description                      Action taken
! ----           -----------                      ------------
! 1              sequential trace count           Renumbered (starting with 1).
! 3              current gather                   Renumbered (starting with 1).
! 4              sequential within gather         Renumbered (starting with 1).
! HDR_PRI        primary sort header word         Used but not changed.
! HDR_SEC        secondary sort header word       Used but not changed.
! HDR_TERT       tertiary sort header word        Used but not changed.
! HDR_PRI_BIN    hdr location for primary bin     Changed.
! HDR_SEC_BIN    hdr location for secondary bin   Changed.
! HDR_TERT_BIN   hdr location for tertiary bin    Changed.
!
! Header word 1 increments for each trace.
!
! Header word 3 increments as follows:
! If HDR_TERT == 1: each time the primary sort bin changes.
! If HDR_TERT >  1: each time the primary or secondary sort bin changes.
!
! Header word 4 is set to 1 whenever header word 3 changes, and then
! increments by 1 until header word 3 changes again.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!      Date       Author     Description
!      ----       ------     -----------
!   1. 2010-02-22 Stoeckley  First version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                    SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
!
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK           >0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    NTR == NEED_TRACES    if another process is requesting more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                  ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS PARALLELSORT Process/NC=80>
!
!
!                  Perform a parallel trace sort across nodes
!
!                   Go to the TSORT tab to enter parameters
!
!<include tsort.f90>
!
! OUTPUT_CHOICE=`CCCCCCCCCCCC    PRI_FIRST=`FFFFFFFFFF   PRI_LAST=`FFFFFFFFFF
!
!           ABORT_CHOICE=`CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OUTPUT_CHOICE">
!<Tip> Whether to output the traces to all nodes or a single node. </Tip>
! Default = all nodes
! Allowed = all nodes
! Allowed = single node
!
! All output traces are normally distributed across all nodes based on the
! PRI_FIRST and PRI_LAST parameters, so that the first traces will be moved
! to the first node, and the last traces will be moved to the last node, in
! a parallel job.
!
! However, if the SINGLE NODE choice is made, all of the output traces will
! be placed onto the boss node (node number 0), and the PRI_FIRST and PRI_LAST
! parameters will be ignored.
!</Help>
!
!
!<Help KEYWORD="PRI_FIRST">
!<Tip> Value for the center of the FIRST primary sort bin. </Tip>
! Default = 1.0
! Allowed = real
!
! Traces will be sorted so that the first traces will be moved to the first
! node, and the last traces will be moved to the last node, in a parallel job.
! To be able to do this, the parameters PRI_FIRST and PRI_LAST must be specified
! so that the traces will be dispersed relatively evenly to all of the nodes.
!
! If the parameters PRI_FIRST and PRI_LAST are not accurate, then the first
! and last nodes might receive an excess or deficiency of traces compared with
! the middle nodes, which will result in a less efficient distribution of
! traces.  However, the traces will still be sorted properly.
!
! Note that the parameter PRI_FIRST must be the value for the center of the
! FIRST primary sort bin, whereas the parameter PRI_INIT can be the value for
! the center of ANY primary sort bin.  If PRI_INIT corresponds to the first
! primary sort bin, then parameters PRI_FIRST and PRI_INIT can be the same.
!
! The PRI_FIRST and PRI_LAST parameters are ignored if the SINGLE NODE option
! is chosen for the OUTPUT_CHOICE parameter.  In this case, all of the output
! traces will be put on the first (boss) node.
!</Help>
!
!
!<Help KEYWORD="PRI_LAST">
!<Tip> Value for the center of the LAST primary sort bin. </Tip>
! Default = 1.0
! Allowed = real
!
! Traces will be sorted so that the first traces will be moved to the first
! node, and the last traces will be moved to the last node, in a parallel job.
! To be able to do this, the parameters PRI_FIRST and PRI_LAST must be specified
! so that the traces will be dispersed relatively evenly to all of the nodes.
!
! If the parameters PRI_FIRST and PRI_LAST are not accurate, then the first
! and last nodes might receive an excess or deficiency of traces compared with
! the middle nodes, which will result in a less efficient distribution of
! traces.  However, the traces will still be sorted properly.
!
! The PRI_FIRST and PRI_LAST parameters are ignored if the SINGLE NODE option
! is chosen for the OUTPUT_CHOICE parameter.  In this case, all of the output
! traces will be put on the first (boss) node.
!</Help>
!
!
!<Help KEYWORD="ABORT_CHOICE">
!<Tip> Conditions under which the job should abort. </Tip>
! Default = abort if MPI initialization error or only one node.
! Allowed = abort if MPI initialization error or only one node.
! Allowed = proceed if MPI initialization error or only one node.
!
! If there is an MPI initialization error, or if only one node is being used
! in the job, this process will work exactly like the TSORT process if you
! choose to proceed.  Otherwise, if there is an MPI initialization error, or
! if only one node is being used in the job, a fatal error will be generated. 
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module parallelsort_module

      use pc_module
      use named_constants_module
      use tsort_module
      use tmerge_module
      use triplesort_module

      implicit none

      private
      public :: parallelsort_create
      public :: parallelsort_initialize
      public :: parallelsort_update
      public :: parallelsort_delete
      public :: parallelsort
      public :: parallelsort_wrapup

      character(len=100),public,save :: PARALLELSORT_IDENT = &
'$Id: tsort.f90,v 1.53 2006/10/17 13:45:49 Glover prod sps $'


!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!


      type,public :: parallelsort_struct              

        private
        logical                     :: skip_wrapup      ! wrapup flag.
        character(len=11)           :: output_choice    ! parameter.
        character(len=60)           :: abort_choice     ! parameter.
        double precision            :: pri_first        ! parameter.
        double precision            :: pri_last         ! parameter.
        type(tsort_struct) ,pointer :: tsort
        type(tmerge_struct),pointer :: tmerge

      end type parallelsort_struct

      character(len=11),parameter :: OUTPUT_CHOICES(2) = &
            (/'all nodes  ', &
              'single node'/)

      character(len=60),parameter :: ABORT_CHOICES(2) = &
            (/'abort if MPI initialization error or only one node  ', &
              'proceed if MPI initialization error or only one node'/)

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine parallelsort_create (obj)
      implicit none
      type(parallelsort_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%tsort)
      nullify  (obj%tmerge)

      call parallelsort_initialize   (obj)
      return
      end subroutine parallelsort_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine parallelsort_delete (obj)
      implicit none
      type(parallelsort_struct),pointer :: obj       ! arguments

      call parallelsort_wrapup (obj)

      if (associated(obj%tsort))  call tsort_delete  (obj%tsort)
      if (associated(obj%tmerge)) call tmerge_delete (obj%tmerge)

      deallocate(obj)
      return
      end subroutine parallelsort_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine parallelsort_initialize (obj)
      implicit none
      type(parallelsort_struct),pointer :: obj       ! arguments

      obj%output_choice = OUTPUT_CHOICES(1)
      obj%abort_choice  = ABORT_CHOICES(1)
      obj%pri_first     = 1.0
      obj%pri_last      = 1.0

      call parallelsort_update (obj)
      return
      end subroutine parallelsort_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine parallelsort_update (obj)
      implicit none
      type(parallelsort_struct),target :: obj                    ! arguments
      integer                          :: nwih,ndpt              ! local
      integer                          :: bin_first,bin_last     ! local
      integer                          :: hdr_pri,pri_bin        ! local
      integer                          :: receiver,ichoice       ! local
      double precision                 :: pri_val                ! local
      integer                          :: num_cpus               ! local
      logical                          :: no_mpi                 ! local
      logical                          :: all_nodes              ! local
      logical                          :: abort                  ! local
      logical                          :: whoops                 ! local

      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('NWIH' , nwih)
      call pc_get_global ('NDPT' , ndpt)

      call pc_get ('OUTPUT_CHOICE' , obj%output_choice)
      call pc_get ('ABORT_CHOICE'  , obj%abort_choice)
      call pc_get ('PRI_FIRST'     , obj%pri_first)
      call pc_get ('PRI_LAST'      , obj%pri_last)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (associated(obj%tsort)) then
           call tsort_update (obj%tsort)
      else
           call tsort_create (obj%tsort)
      endif

      ! all pc process parameters are passed through to tsort.
      ! all pc input and output global parameters are the same as tsort.
      ! all pc output control parameters are the same as tsort.

      do ichoice = 1,size(OUTPUT_CHOICES)
           if (obj%output_choice(1:1) == OUTPUT_CHOICES(ichoice)(1:1)) &
               obj%output_choice       = OUTPUT_CHOICES(ichoice)
      enddo

      do ichoice = 1,size(ABORT_CHOICES)
           if (obj%abort_choice(1:1) == ABORT_CHOICES(ichoice)(1:1)) &
               obj%abort_choice       = ABORT_CHOICES(ichoice)
      enddo

      all_nodes = (obj%output_choice(1:1) == 'a')
      abort     = (obj%abort_choice(1:1) == 'a')
      bin_first = tsort_get_primary_bin (obj%tsort, obj%pri_first)
      bin_last  = tsort_get_primary_bin (obj%tsort, obj%pri_last)

      if (all_nodes .and. bin_first == bin_last) then
        call pc_print   (' ')
        call pc_warning (' All output traces will be put onto a single node')
        call pc_warning ('       (in spite of the ALL NODES choice)')
        call pc_warning ('because PRI_FIRST and PRI_LAST are nearly the same.')
        call pc_print   (' ')
      endif
  
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OUTPUT_CHOICE' , OUTPUT_CHOICES)
      call pc_put_options_field ('ABORT_CHOICE'  , ABORT_CHOICES)

      call pc_put ('OUTPUT_CHOICE' , obj%output_choice)
      call pc_put ('ABORT_CHOICE'  , obj%abort_choice)
      call pc_put ('PRI_FIRST'     , obj%pri_first)
      call pc_put ('PRI_LAST'      , obj%pri_last)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      num_cpus = 1        ! in case pc_get_jdata does not contain this.

      call pc_get_jdata  ('num_cpus', num_cpus)

      no_mpi = (num_cpus <= 1)

      call tmerge_create (obj%tmerge, no_mpi, nwih, ndpt, all_nodes, bin_first, bin_last, whoops)

      if (whoops .and. abort) call pc_error (' MPI initialization error or only one node')

      hdr_pri = 0
      call pc_get ('HDR_PRI', hdr_pri)   ! tsort parameter

      do pri_bin = bin_first,bin_last
           if (pri_bin > bin_first + 5 .and. pri_bin < bin_last - 5) continue
           pri_val  = tsort_get_primary_bin_center (obj%tsort, pri_bin)
           receiver = tmerge_get_output_node       (obj%tmerge, pri_bin)
           print 100, hdr_pri, pri_val, pri_bin, receiver
100        format (" PARALLELSORT: primary header word ", I3, &
               " value ", F8.2, "  primary bin ", I6, "  node ", I5)
      enddo

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine parallelsort_update


!!------------------------- example job flow ----------------------------!!
!!------------------------- example job flow ----------------------------!!
!!------------------------- example job flow ----------------------------!!


!!!!!!    222               ...
!!!!!!                      ...
!!!!!!                      ...
!!!!!!    333   call tsort (obj%tsort, ntr, hdi, tri, hdo, tro)
!!!!!!          if (ntr == FATAL_ERROR) go to 999
!!!!!!          if (ntr == NEED_TRACES) go to 222
!!!!!!    444   call tmerge (obj%tmerge, ntr, hdo, tro)
!!!!!!          if (ntr == FATAL_ERROR) go to 999
!!!!!!          if (ntr == NEED_TRACES) go to 333
!!!!!!                      ...
!!!!!!                      ...
!!!!!!          if (ntr == NEED_TRACES) go to 444
!!!!!!                      ...
!!!!!!                      ...
!!!!!!    999               ...


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine parallelsort (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(parallelsort_struct),intent(inout) :: obj               ! arguments
      integer                  ,intent(inout) :: ntr               ! arguments
      double precision         ,intent(in)    :: hdi(:,:)          ! arguments
      real                     ,intent(in)    :: tri(:,:)          ! arguments
      double precision         ,intent(out)   :: hdo(:,:)          ! arguments
      real                     ,intent(out)   :: tro(:,:)          ! arguments
      type(triplesort_ints)                   :: bin               ! local

      if (ntr == NEED_TRACES) go to 444

!----------CALL TSORT:

333   call tsort (obj%tsort, ntr, hdi, tri, hdo, tro)

      if (ntr == FATAL_ERROR) go to 999
      if (ntr == NEED_TRACES) return

      if (ntr == 1) bin = tsort_get_bin (obj%tsort, hdo(:,1))

!----------CALL TMERGE:

444   call tmerge_execute (obj%tmerge, ntr, hdo(:,1), tro(:,1), bin)

      if (ntr == FATAL_ERROR) go to 999
      if (ntr == NEED_TRACES) go to 333

!----------FINISH UP:

      if (ntr == NO_MORE_TRACES) call parallelsort_wrapup (obj)
999   if (ntr == FATAL_ERROR)    call parallelsort_wrapup (obj)
      return
      end subroutine parallelsort


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine parallelsort_wrapup (obj)
      implicit none
      type(parallelsort_struct),intent(inout) :: obj       ! arguments

      if (associated (obj%tsort)) call tsort_wrapup (obj%tsort)

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine parallelsort_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module parallelsort_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

