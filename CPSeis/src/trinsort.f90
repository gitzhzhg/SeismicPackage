!<CPS_v1 type="PROCESS"/>
!!----------------------------- trinsort.f90 -------------------------------!!
!!----------------------------- trinsort.f90 -------------------------------!!
!!----------------------------- trinsort.f90 -------------------------------!!


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
! Name       : TRINSORT                 (TRIN with trace sort)
! Category   : io
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Read trace files in desired sorted order using trace file table.
! Portability: No known limitations, but see note regarding pgf90 bug below.
! Parallel   : No.
!
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process reads a trace file from disk in the order specified by
! a table of the traces on the trace file.
!
! This process should be used instead of TRIN if you want to read the traces
! in an order different from the actual order of the traces on the file.
! This action may eliminate the need to sort the data using TSORT.
!
! You must have previously saved a trace file table using the TABLESAVE
! process at the time you saved the trace file itself using the TROT process.
! The trace file saved by TROT and read by this process must be a TRCIO file.
!
! Any of the header words which were saved on the table can be used to choose
! the order in which the trace data is to be read by this process.
!
!-------------------------------------------------------------------------------
!                         TRIO OF PROCESSES
!
! This process is one of a trio of processes designed to allow reading trace
! files randomly in order to reduce the need to sort data using TSORT:
!
! TABLESAVE:  Saves a trace file table to disk.  This process should
!             immediately precede or follow TROT in the job.  TROT will
!             save the trace file as usual.
!
! TABLESORT:  Reads, sorts, and saves a trace file table in a different
!             order (setup-only process).  This is optional, since TRINSORT
!             (below) can also sort (but not output) the table.
!
! TRINSORT:   Reads a trace file in desired sort order using the trace file
!             table.  This process should be used instead of TRIN to read
!             the trace file.  TRINSORT can read traces in the order
!             specified on the table, or in a user-specified order.
!
! If you wish to create a trace file table for an existing TROT file, you
! can read the traces using TRIN followed by TABLESAVE.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a trace supplying process (at the top of a loop).
!
! But if MERGE == APPEND, this process is not at the top of a loop, and
! instead passes through all traces received, and then adds to the trace
! flow from the trace file being read.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs traces one at a time.
!
! But if MERGE == APPEND, this process first outputs the traces it receives,
! in the ensembles it receives, until it receives no more traces.  Then it
! outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                         Action taken
! ----      -----------                         ------------
! NWIH      Number of words in header           used but not changed
! NDPT      Number of samples in trace          used but not changed
! TSTRT     Starting time on trace              used but not changed
! DT        Trace sample interval in seconds    used but not changed
! NUNTR     Number of traces in gather          set to 1
! GATHERED  Whether traces are gathered         set to false
!
! But if MERGE == APPEND, this process does not set the NUMTR global.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#         Description                        Action taken
! ----         -----------                        ------------
! 1            Trace sequence number              Reset as appropriate.
! 3            Current group                      Reset as appropriate.
! 4            Current channel                    Reset as appropriate.
! HDRS(list)   Header words in trace file table   Used but not changed.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!010. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  9. 2005-10-24  Stoeckley  Enhance to allow more than one trace file input;
!                             add history option.
!  8. 2004-03-17  Stoeckley  Change to not test for selected trace a second
!                             time (after reading the trace) using a header
!                             word which was used for selecting the trace the
!                             first time (before reading the trace).
!  7. 2003-12-09  Stoeckley  Change to double precision from real.
!  6. 2002-08-01  Stoeckley  Change the trace selection algorithm to call the
!                             MTH module.
!  5. 2002-07-11  Stoeckley  Change the trace selection algorithm so that the
!                             trace selection increment does not have to be
!                             a multiple of the bin width.
!  4. 2002-04-08  Stoeckley  Add ability to select traces by pattern.
!  3. 2002-02-25  Stoeckley  Add MERGE parameter with options APPEND and NO.
!  2. 2002-02-06  Stoeckley  Add capability to select traces while reading.
!  1. 2002-02-04  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS           
!
! No known limitations, but I had to work around a Portland Group compiler
! bug in the verify parameters section and the prepare for execution section,
! where you can see a comment.
! The Portland Group compiler claims the following:
!   PGF90-S-0099-Illegal use of derived type (../trinsort.f90: 542)
!   PGF90-S-0099-Illegal use of derived type (../trinsort.f90: 542)
!   PGF90-S-0099-Illegal use of derived type (../trinsort.f90: 607)
! for a test for inequality or equality between two triplesort_ints variables.
! The compiler seems not to be able to deal with some overloaded operator
! interfaces in the triplesort_module.  The compiler will not complain if
! any of several unrelated parts of the code in this module are commented
! out.  A small test program also does not complain.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! Upon input, NTR must have one of these values:
!  NTR == NEED_TRACES    means a trace is being requested.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting a trace.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
! If MERGE == 'APPEND', the following modifications to the above occur:
!  (1) NEED_REQUEST will be true.
!  (2) Upon input, NTR >= 1 means a trace is being received.
!  (3) Upon input, NTR == NO_MORE_TRACES means no more traces being received.
!  (4) Upon output, NTR == NEED_TRACES means this process is requesting a trace.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
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


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TRINSORT Process/NC=80>
!                  Read traces from trace files in desired order
!
!<include mfile.f90>
!
! Select TABLEPATH[TABLEPATH]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [TABLEPATH_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                  [INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! HOW_READ_TRACES =`CCCCCCCCCCCCCCC      MERGE=`CC      HISTORY=`CC
!
!<include tsortparams.f90>
!
! [/L]Trace Selection Criteria:
!  HDR_A=`II   A_MIN=`FFFFFFFFF   A_WID=`FFFFF   A_INC=`FFFFF   A_MAX=`FFFFFFFFF
!  HDR_B=`II   B_MIN=`FFFFFFFFF   B_WID=`FFFFF   B_INC=`FFFFF   B_MAX=`FFFFFFFFF
!  HDR_C=`II   C_MIN=`FFFFFFFFF   C_WID=`FFFFF   C_INC=`FFFFF   C_MAX=`FFFFFFFFF
!<PARMS TABLEPATH[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="TABLEPATH_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TABLEPATH. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TABLEPATH">
!<Tip> Choose TABLEPATH using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Information regarding contents of TABLEPATH. </Tip>
!</Help>
!
!
!<Help KEYWORD="TABLEPATH">
!<Tip> Name of trace file table to use. </Tip>
! Default = NONE
! Allowed = char or NONE
!
! This trace file table must match the specified trace files.
!
! Any specified file extension will be changed to ".table".
!
! TABLEPATH must NOT be specified if HOW_READ_TRACES is set to SEQUENTIAL.
! TABLEPATH must be specified if HOW_READ_TRACES is set to OLD_SORT or NEW_SORT.
!</Help>
!
!
!<Help KEYWORD="MERGE">
!<Tip> Whether to merge traces with those from previous processes. </Tip>
! Default = NO
! Allowed = NO     (Do not merge traces.)
! Allowed = APPEND (Append traces after those from previous processes.)
!
! The first process in a loop cannot be a TRINSORT with MERGE = APPEND.
!</Help>
!
!
!<Help KEYWORD="HISTORY">
!<Tip> Whether to read histories from the files. </Tip>
! Default = YES
! Allowed = YES    (Do read histories.)
! Allowed = NO     (Do not read histories.)
!</Help>
!
!
!<Help KEYWORD="HOW_READ_TRACES">
!<Tip> Whether to read traces sequentially or in a chosen order. </Tip>
! Default = SEQUENTIAL
! Allowed = SEQUENTIAL (read traces sequentially and do not use table)
! Allowed = OLD_SORT   (read traces in old order specified on table)
! Allowed = NEW_SORT   (read traces in new order specified by parameters here)
!
! HOW_READ_TRACES must be set to SEQUENTIAL if TABLEPATH is NOT specified.
! HOW_READ_TRACES must be set to OLD_SORT or NEW_SORT if TABLEPATH is specified.
!
! For OLD_SORT, if the trace file table has not been sorted into a new trace
! file table, the trace order will be the original trace order when the
! trace file table was created.
!</Help>
!
!
!<Help KEYWORD="HDR_A">
!<Tip> Header word for selecting traces. </Tip>
! Default = 0
! Allowed = 2 - NWIH (or 0 to disable selections).
!
! This header word need not match HDR_PRI or HDR_SEC or HDR_TERT.
!
! Any header word (except header word 1) can be used for trace selections,
! but if HOW_READ_TRACES is not SEQUENTIAL and the header word is on the
! TABLEPATH file, the trace selections will be faster because the selection
! criterion for that header word will be applied BEFORE attempting to read
! the trace from the file.
!
! The order of HDR_A and HDR_B and HDR_C is unimportant.
!</Help>
!
!
!<Help KEYWORD="HDR_B">
!<Tip> Header word for selecting traces. </Tip>
! Default = 0
! Allowed = 2 - NWIH (or 0 to disable selections).
!
! This header word need not match HDR_PRI or HDR_SEC or HDR_TERT.
!
! Any header word (except header word 1) can be used for trace selections,
! but if HOW_READ_TRACES is not SEQUENTIAL and the header word is on the
! TABLEPATH file, the trace selections will be faster because the selection
! criterion for that header word will be applied BEFORE attempting to read
! the trace from the file.
!
! The order of HDR_A and HDR_B and HDR_C is unimportant.
!</Help>
!
!
!<Help KEYWORD="HDR_C">
!<Tip> Header word for selecting traces. </Tip>
! Default = 0
! Allowed = 2 - NWIH (or 0 to disable selections).
!
! This header word need not match HDR_PRI or HDR_SEC or HDR_TERT.
!
! Any header word (except header word 1) can be used for trace selections,
! but if HOW_READ_TRACES is not SEQUENTIAL and the header word is on the
! TABLEPATH file, the trace selections will be faster because the selection
! criterion for that header word will be applied BEFORE attempting to read
! the trace from the file.
!
! The order of HDR_A and HDR_B and HDR_C is unimportant.
!</Help>
!
!
!<Help KEYWORD="A_MIN">
!<Tip> Value for the center of the MINIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
!
! Traces in bins with bin center smaller than A_MIN will be skipped.
!
! This value is irrelevant if HDR_A is 0.
!</Help>
!
!
!<Help KEYWORD="B_MIN">
!<Tip> Value for the center of the MINIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
!
! Traces in bins with bin center smaller than B_MIN will be skipped.
!
! This value is irrelevant if HDR_B is 0.
!</Help>
!
!
!<Help KEYWORD="C_MIN">
!<Tip> Value for the center of the MINIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
!
! Traces in bins with bin center smaller than C_MIN will be skipped.
!
! This value is irrelevant if HDR_C is 0.
!</Help>
!
!
!<Help KEYWORD="A_WID">
!<Tip> Bin width for header word HDR_A. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This width need not bear any relationship to the bin widths (or increments)
! used for sorting.
!
! This value is irrelevant if HDR_A is 0.
!</Help>
!
!
!<Help KEYWORD="B_WID">
!<Tip> Bin width for header word HDR_B. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This width need not bear any relationship to the bin widths (or increments)
! used for sorting.
!
! This value is irrelevant if HDR_B is 0.
!</Help>
!
!
!<Help KEYWORD="C_WID">
!<Tip> Bin width for header word HDR_C. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This width need not bear any relationship to the bin widths (or increments)
! used for sorting.
!
! This value is irrelevant if HDR_C is 0.
!</Help>
!
!
!<Help KEYWORD="A_INC">
!<Tip> Increment for header word HDR_A. </Tip>
! Default = 1.0
! Allowed = real >= A_WID
!
! This is the increment from the center of one selected bin to the center
! of the next selected bin.  If it is equal to A_WID, the selected
! bins will butt up to each other with no gap.
!
! This increment need not be a multiple of A_WID.
!
! This increment need not bear any relationship to the bin widths (or
! increments) used for sorting.
!
! This value is irrelevant if HDR_A is 0.
!</Help>
!
!
!<Help KEYWORD="B_INC">
!<Tip> Increment for header word HDR_B. </Tip>
! Default = 1.0
! Allowed = real >= B_WID
!
! This is the increment from the center of one selected bin to the center
! of the next selected bin.  If it is equal to B_WID, the selected
! bins will butt up to each other with no gap.
!
! This increment need not be a multiple of B_WID.
!
! This increment need not bear any relationship to the bin widths (or
! increments) used for sorting.
!
! This value is irrelevant if HDR_B is 0.
!</Help>
!
!
!<Help KEYWORD="C_INC">
!<Tip> Increment for header word HDR_C. </Tip>
! Default = 1.0
! Allowed = real >= C_WID
!
! This is the increment from the center of one selected bin to the center
! of the next selected bin.  If it is equal to C_WID, the selected
! bins will butt up to each other with no gap.
!
! This increment need not be a multiple of C_WID.
!
! This increment need not bear any relationship to the bin widths (or
! increments) used for sorting.
!
! This value is irrelevant if HDR_C is 0.
!</Help>
!
!
!<Help KEYWORD="A_MAX">
!<Tip> Value for the center of the MAXIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
! Allowed = real >= A_MIN
!
! This should be equal to A_MIN plus a multiple of A_INC, and will be
! adjusted slightly if necessary to make this true.
!
! Traces in bins with bin center larger than A_MAX will be skipped.
!
! This value is irrelevant if HDR_A is 0.
!</Help>
!
!
!<Help KEYWORD="B_MAX">
!<Tip> Value for the center of the MAXIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
! Allowed = real >= B_MIN
!
! This should be equal to B_MIN plus a multiple of B_INC, and will be
! adjusted slightly if necessary to make this true.
!
! Traces in bins with bin center larger than B_MAX will be skipped.
!
! This value is irrelevant if HDR_B is 0.
!</Help>
!
!
!<Help KEYWORD="C_MAX">
!<Tip> Value for the center of the MAXIMUM bin to select. </Tip>
! Default = 1.0
! Allowed = real
! Allowed = real >= C_MIN
!
! This should be equal to C_MIN plus a multiple of C_INC, and will be
! adjusted slightly if necessary to make this true.
!
! Traces in bins with bin center larger than C_MAX will be skipped.
!
! This value is irrelevant if HDR_C is 0.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module trinsort_module
      use pc_module
      use named_constants_module
      use tftable_module
      use permtset_module
      use mfile_module
      use pathcheck_module
      use pathchoose_module
      use tsortparams_module
      use triplesort_module
      use mth_module
      implicit none
      private
      public :: trinsort_create
      public :: trinsort_initialize
      public :: trinsort_update
      public :: trinsort_delete
      public :: trinsort
      public :: trinsort_wrapup


      character(len=100),public,save :: TRINSORT_IDENT = &
'$Id: trinsort.f90,v 1.10 2006/10/17 13:45:48 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: trinsort_struct              
 
        private
        logical                        :: skip_wrapup     ! wrapup flag.

        character(len=FILENAME_LENGTH) :: tablepath       ! process parameters.
        character(len=12)              :: how_read_traces ! process parameters.
        character(len=6)               :: merge           ! process parameters.
        logical                        :: history         ! process parameters.
        type(triplesort_ints)          :: selhdr          ! process parameters.
        type(triplesort_doubles)       :: selmin          ! process parameters.
        type(triplesort_doubles)       :: selwid          ! process parameters.
        type(triplesort_doubles)       :: selinc          ! process parameters.
        type(triplesort_doubles)       :: selmax          ! process parameters.
        type(triplesort_ints)          :: selnum          ! dependent.
        character(len=80)              :: info            ! gui-only parameter.

        type(mfile_struct)      ,pointer :: mfile         ! dependent.
        type(pathchoose_struct) ,pointer :: pathchoose2   ! dependent.
        type(tsortparams_struct),pointer :: tsortparams   ! dependent.
        type(tftable_struct)    ,pointer :: tftable       ! dependent.
        type(permtset_struct)   ,pointer :: permtset      ! dependent.
        type(triplesort_doubles),pointer :: values(:)     ! dependent.
        type(triplesort_ints)    :: hdr          ! from tsortparams or tftable.
        type(triplesort_doubles) :: init         ! from tsortparams or tftable.
        type(triplesort_doubles) :: inc          ! from tsortparams or tftable.
        integer                  :: sequence     ! dependent.
        integer                  :: group        ! dependent.
        integer                  :: channel      ! dependent.
        type(triplesort_ints)    :: binlast      ! dependent.
        logical                  :: waiting      ! dependent.
        logical                  :: selecting    ! dependent.

      end type trinsort_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(trinsort_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      ::  merge_noptions = 2
      integer,parameter      ::    how_noptions = 3

      character(len= 6),save ::  merge_options (merge_noptions)
      character(len=12),save ::    how_options (  how_noptions)

      data merge_options /'NO', 'APPEND'/
      data   how_options /'SEQUENTIAL', 'OLD_SORT', 'NEW_SORT'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine trinsort_create (obj)

      type(trinsort_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%tftable)
      nullify (obj%permtset)
      nullify (obj%values)
      nullify (obj%mfile) ! jpa
      nullify (obj%pathchoose2) ! jpa
      nullify (obj%tsortparams) ! jpa

      call mfile_create        (obj%mfile)
      call mfile_set_type      (obj%mfile, MFILE_READ_TRC_FILE)
      call pathchoose_create   (obj%pathchoose2, 'tablepath' , 'table')
      call tsortparams_create  (obj%tsortparams)
      call trinsort_initialize (obj)

      end subroutine trinsort_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine trinsort_delete (obj)

      type(trinsort_struct),pointer :: obj       ! arguments

      call trinsort_wrapup  (obj)
      call tftable_close    (obj%tftable)
      call permtset_close   (obj%permtset)

      if (associated(obj%values)) deallocate (obj%values)

      call mfile_delete       (obj%mfile)
      call pathchoose_delete  (obj%pathchoose2)
      call tsortparams_delete (obj%tsortparams)

      deallocate(obj)

      end subroutine trinsort_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine trinsort_initialize (obj)

      type(trinsort_struct),intent(inout) :: obj       ! arguments

      obj%tablepath       = PATHCHECK_EMPTY
      obj%merge           = 'NO'
      obj%history         = .true.
      obj%how_read_traces = 'SEQUENTIAL'
      obj%info            = ' '
      obj%selhdr          = 0
      obj%selmin          = 1.0D0
      obj%selwid          = 1.0D0
      obj%selinc          = 1.0D0
      obj%selmax          = 1.0D0
      obj%selnum          = 1

      call tsortparams_initialize (obj%tsortparams)
      call trinsort_update        (obj)

      end subroutine trinsort_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine trinsort_update (obj)

      type(trinsort_struct),intent(inout),target :: obj          ! arguments
      integer                                    :: nwih,ndpt    ! local
      real                                       :: tstrt,dt     ! local
      integer                                    :: ntraces,err  ! local
      character(len=80)                          :: msg          ! local
      character(len=FILENAME_LENGTH)             :: tablekeep    ! local
      type(triplesort_ints)                      :: hdr,keep     ! local
      integer                                    :: lun,ipn      ! local
      logical                                    :: status       ! local
      character(len=FILENAME_LENGTH),pointer     :: pathnames(:) ! local
      integer                                    :: npaths       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      lun = pc_get_lun()
      ipn = pc_get_ipn()

      tablekeep = obj%tablepath

      if (pathchoose_update(obj%pathchoose2, obj%tablepath )) return

      call pc_get_global ('nwih' , nwih)
      call pc_get_global ('ndpt' , ndpt)
      call pc_get_global ('tstrt', tstrt)
      call pc_get_global ('dt'   , dt)

      call pc_get ('tablepath'       , obj%tablepath        )
      call pc_get ('merge'           , obj%merge            )
      call pc_get ('history'         , obj%history          )
      call pc_get ('how_read_traces' , obj%how_read_traces  )
      call pc_get ('hdr_a'           , obj%selhdr%primary   )
      call pc_get ('hdr_b'           , obj%selhdr%secondary )
      call pc_get ('hdr_c'           , obj%selhdr%tertiary  )
      call pc_get ('a_min'           , obj%selmin%primary   )
      call pc_get ('b_min'           , obj%selmin%secondary )
      call pc_get ('c_min'           , obj%selmin%tertiary  )
      call pc_get ('a_wid'           , obj%selwid%primary   )
      call pc_get ('b_wid'           , obj%selwid%secondary )
      call pc_get ('c_wid'           , obj%selwid%tertiary  )
      call pc_get ('a_inc'           , obj%selinc%primary   )
      call pc_get ('b_inc'           , obj%selinc%secondary )
      call pc_get ('c_inc'           , obj%selinc%tertiary  )
      call pc_get ('a_max'           , obj%selmax%primary   )
      call pc_get ('b_max'           , obj%selmax%secondary )
      call pc_get ('c_max'           , obj%selmax%tertiary  )

      ! Note: The keywords for selhdr, selmin, selwid, selinc, and selmax
      ! do not include the "sel" characters because they will not fit
      ! into the GUI without making lines too long to show up.


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call pathcheck ('tablepath', obj%tablepath, '.table',  &
                      required=.false., show=PATHCHECK_INFO_INPUT)

      select case (obj%merge)
        case ('NO    ') ; continue
        case ('APPEND') ; continue
        case default    ; obj%merge = 'NO'
      end select

      select case (obj%how_read_traces)
        case ('SEQUENTIAL') ; continue
        case ('OLD_SORT  ') ; continue
        case ('NEW_SORT  ') ; continue
        case default        ; obj%how_read_traces = 'SEQUENTIAL'
      end select

      keep = tsortparams_get_hdr (obj%tsortparams)

   call tsortparams_update (obj%tsortparams, (obj%how_read_traces=='NEW_SORT'))

      hdr  = tsortparams_get_hdr (obj%tsortparams)

 !!!  if (obj%tablepath /= tablekeep .or. hdr /= keep) then     ! pgf90 bug.

      if (obj%tablepath /= tablekeep      .or. &     ! pgf90 bug workaround.
          hdr%primary   /= keep%primary   .or. &     ! pgf90 bug workaround.
          hdr%secondary /= keep%secondary .or. &     ! pgf90 bug workaround.
          hdr%tertiary  /= keep%tertiary) then       ! pgf90 bug workaround.

           if (obj%tablepath == PATHCHECK_EMPTY) then
                obj%info = ' '
           else
                call tftable_checkout (obj%tablepath,msg,obj%info,hdr)
                call pc_print (obj%info)
                if (msg /= ' ') call pc_error (msg)
           end if
      end if

      if (obj%how_read_traces == 'SEQUENTIAL' .and. &
          obj%tablepath       /= PATHCHECK_EMPTY) then

        call pc_error ('TABLEPATH must NOT be specified &
                       &if HOW_READ_TRACES = SEQUENTIAL')

      else if (obj%how_read_traces /= 'SEQUENTIAL' .and. &
               obj%tablepath       == PATHCHECK_EMPTY) then

        call pc_error ('TABLEPATH must be specified &
                       &if HOW_READ_TRACES = OLD_SORT or NEW_SORT')

      end if

      if (obj%selhdr%primary <  0 .or. &
          obj%selhdr%primary == 1 .or. &
          obj%selhdr%primary > nwih) then
           call pc_warning ('HDR_A outside of valid range - reset to 0')
           obj%selhdr%primary = 0
      end if

      if (obj%selhdr%secondary <  0 .or. &
          obj%selhdr%secondary == 1 .or. &
         obj%selhdr%secondary > nwih) then
           call pc_warning ('HDR_B outside of valid range - reset to 0')
           obj%selhdr%secondary = 0
      end if

      if (obj%selhdr%tertiary <  0 .or. &
          obj%selhdr%tertiary == 1 .or. &
          obj%selhdr%tertiary > nwih) then
           call pc_warning ('HDR_C outside of valid range - reset to 0')
           obj%selhdr%tertiary = 0
      end if

      if (obj%selwid%primary <= 0.0) then
           call pc_warning ('A_WID must be > 0 and has been reset to 1')
           obj%selwid%primary = 1.0
      end if

      if (obj%selwid%secondary <= 0.0) then
           call pc_warning ('B_WID must be > 0 and has been reset to 1')
           obj%selwid%secondary = 1.0
      end if

      if (obj%selwid%tertiary <= 0.0) then
           call pc_warning ('C_WID must be > 0 and has been reset to 1')
           obj%selwid%tertiary = 1.0
      end if

      if (obj%selinc%primary < obj%selwid%primary) then
        call pc_warning ('A_INC must be >= A_WID and has been reset to A_WID')
        obj%selinc%primary = obj%selwid%primary
      end if

      if (obj%selinc%secondary < obj%selwid%secondary) then
        call pc_warning ('B_INC must be >= B_WID and has been reset to B_WID')
        obj%selinc%secondary = obj%selwid%secondary
      end if

      if (obj%selinc%tertiary < obj%selwid%tertiary) then
        call pc_warning ('C_INC must be >= C_WID and has been reset to C_WID')
        obj%selinc%tertiary = obj%selwid%tertiary
      end if

      obj%selnum%primary   = mth_bin_number (obj%selmin%primary,  &
                                             obj%selinc%primary,  &
                                             obj%selmax%primary)

      obj%selnum%secondary = mth_bin_number (obj%selmin%secondary,  &
                                             obj%selinc%secondary,  &
                                             obj%selmax%secondary)

      obj%selnum%tertiary  = mth_bin_number (obj%selmin%tertiary,  &
                                             obj%selinc%tertiary,  &
                                             obj%selmax%tertiary)

      if (obj%selnum%primary < 1) then
           call pc_warning &
                  ('A_MAX must be >= A_MIN and has been reset to A_MIN')
           obj%selnum%primary = 1
      end if

      if (obj%selnum%secondary < 1) then
           call pc_warning &
                  ('B_MAX must be >= B_MIN and has been reset to B_MIN')
           obj%selnum%secondary = 1
      end if

      if (obj%selnum%tertiary < 1) then
           call pc_warning &
                  ('C_MAX must be >= C_MIN and has been reset to C_MIN')
           obj%selnum%tertiary = 1
      end if

      obj%selmax%primary   = mth_bin_center (obj%selmin%primary,  &
                                             obj%selinc%primary,  &
                                             obj%selnum%primary)

      obj%selmax%secondary = mth_bin_center (obj%selmin%secondary,  &
                                             obj%selinc%secondary,  &
                                             obj%selnum%secondary)

      obj%selmax%tertiary  = mth_bin_center (obj%selmin%tertiary,  &
                                             obj%selinc%tertiary,  &
                                             obj%selnum%tertiary)


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call mfile_update (obj%mfile)

      call pc_put_options_field ('merge' ,merge_options, merge_noptions)
      call pc_put_options_field ('how_read_traces' ,how_options, how_noptions)

      if (obj%merge == 'APPEND') then
           call pc_put_global   ('gathered' , .false.)
           if (ipn == 3) call pc_error &
                  ('TRINSORT cannot be the first process when MERGE = APPEND')
      else
           call pc_put_global   ('numtr'    , 1)
           call pc_put_global   ('gathered' , .false.)
      end if

      call pc_put ('tablepath'       , obj%tablepath        )
      call pc_put ('merge'           , obj%merge            )
      call pc_put ('history'         , obj%history          )
      call pc_put ('how_read_traces' , obj%how_read_traces  )
      call pc_put ('hdr_a'           , obj%selhdr%primary   )
      call pc_put ('hdr_b'           , obj%selhdr%secondary )
      call pc_put ('hdr_c'           , obj%selhdr%tertiary  )
      call pc_put ('a_min'           , obj%selmin%primary   ,10)
      call pc_put ('b_min'           , obj%selmin%secondary ,10)
      call pc_put ('c_min'           , obj%selmin%tertiary  ,10)
      call pc_put ('a_wid'           , obj%selwid%primary   , 6)
      call pc_put ('b_wid'           , obj%selwid%secondary , 6)
      call pc_put ('c_wid'           , obj%selwid%tertiary  , 6)
      call pc_put ('a_inc'           , obj%selinc%primary   , 6)
      call pc_put ('b_inc'           , obj%selinc%secondary , 6)
      call pc_put ('c_inc'           , obj%selinc%tertiary  , 6)
      call pc_put ('a_max'           , obj%selmax%primary   ,10)
      call pc_put ('b_max'           , obj%selmax%secondary ,10)
      call pc_put ('c_max'           , obj%selmax%tertiary  ,10)

      call pc_put_sensitive_field_flag ('a_min' , obj%selhdr%primary   /= 0)
      call pc_put_sensitive_field_flag ('b_min' , obj%selhdr%secondary /= 0)
      call pc_put_sensitive_field_flag ('c_min' , obj%selhdr%tertiary  /= 0)
      call pc_put_sensitive_field_flag ('a_wid' , obj%selhdr%primary   /= 0)
      call pc_put_sensitive_field_flag ('b_wid' , obj%selhdr%secondary /= 0)
      call pc_put_sensitive_field_flag ('c_wid' , obj%selhdr%tertiary  /= 0)
      call pc_put_sensitive_field_flag ('a_inc' , obj%selhdr%primary   /= 0)
      call pc_put_sensitive_field_flag ('b_inc' , obj%selhdr%secondary /= 0)
      call pc_put_sensitive_field_flag ('c_inc' , obj%selhdr%tertiary  /= 0)
      call pc_put_sensitive_field_flag ('a_max' , obj%selhdr%primary   /= 0)
      call pc_put_sensitive_field_flag ('b_max' , obj%selhdr%secondary /= 0)
      call pc_put_sensitive_field_flag ('c_max' , obj%selhdr%tertiary  /= 0)

      call pc_put_gui_only ('info'   , obj%info )

      if (obj%merge == 'APPEND') then
          call pc_put_control ('need_label'  , .true.)
          call pc_put_control ('need_request', .true.)
      else
          call pc_put_control ('need_label'  , .true.)
          call pc_put_control ('need_request', .false.)
      end if


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call tftable_close   (obj%tftable)
      call permtset_close  (obj%permtset)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!!!!!!!!!! get trace selection information:

      obj%selecting = (obj%selhdr%primary   /= 0 .or. &
                       obj%selhdr%secondary /= 0 .or. &
                       obj%selhdr%tertiary  /= 0)

      if (obj%selecting .and. obj%tablepath /= PATHCHECK_EMPTY) then
           call pc_print ('doing trace selections')
           call tftable_read_values &
                             (obj%tablepath,msg,obj%selhdr,obj%values,ntraces)
           if (msg /= ' ') call pc_error (msg)
      end if

!!!!!!!!!! get trace sorting information:

      if (obj%how_read_traces == 'OLD_SORT') then

           !!! HDR,INIT,INC are intent(out).
           call tftable_open_read (obj%tftable,obj%tablepath,msg,ntraces, &
                                   obj%hdr,obj%init,obj%inc)
           if (msg /= ' ') call pc_error (msg)
  !!!      if (obj%hdr == 0) then              ! pgf90 compiler bug.
           if (obj%hdr%primary == 0) then      ! pgf90 compiler bug workaround.
                call pc_print &
                  ('using original trace order on unsorted trace file table')
           else
                call pc_print ('using trace order on sorted trace file table:')
                write (lun,*) 'hdr  = ',obj%hdr
                write (lun,*) 'init = ',obj%init
                write (lun,*) 'inc  = ',obj%inc
           end if

      else if (obj%how_read_traces == 'NEW_SORT') then

           obj%hdr  = tsortparams_get_hdr  (obj%tsortparams)
           obj%init = tsortparams_get_init (obj%tsortparams)
           obj%inc  = tsortparams_get_inc  (obj%tsortparams)
           call pc_print ('using sort order specified by process parameters:')
           write (lun,*) 'hdr  = ',obj%hdr
           write (lun,*) 'init = ',obj%init
           write (lun,*) 'inc  = ',obj%inc
           !!! HDR,INIT,INC are intent(in).
           call tftable_open_sort (obj%tftable,obj%tablepath,msg,ntraces, &
                                   obj%hdr,obj%init,obj%inc)
           if (msg /= ' ') call pc_error (msg)

      else
           call pc_print ('reading traces sequentially')
      end if

!!!!!!!!!! prepare to read traces:

      obj%sequence = 0
      obj%group    = 0
      obj%channel  = 0
      obj%binlast  = (/0,0,0/)
      obj%waiting  = (obj%merge == 'APPEND')

      nullify (pathnames)
      status = mfile_get_filenames (obj%mfile, pathnames, npaths)

      if (.not.status) then
           call pc_error ('error trying to get filenames from MFILE')
           return
      endif

      call permtset_open_read  (obj%permtset,pathnames,npaths,  &
                                nwih,ndpt,tstrt,dt,             &
                                lun,err,obj%history)

      if (err /= PERMTSET_OK) then
           call pc_error ('error opening trace files')
      end if

      if (associated(pathnames)) deallocate (pathnames)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine trinsort_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!----------------------- private skip trace -------------------------------!!
!!----------------------- private skip trace -------------------------------!!
!!----------------------- private skip trace -------------------------------!!


      function trinsort_skip_trace (obj,val) result (skip)

      type(trinsort_struct)   ,intent(inout) :: obj              ! arguments
      type(triplesort_doubles),intent(in)    :: val              ! arguments
      logical                                :: skip             ! result

      skip = .true.

      if (obj%selhdr%primary > 0 .and. val%primary /= DNIL) then
        if (.not. mth_included (val%primary,  &
                         obj%selmin%primary,  &
                         obj%selwid%primary,  &
                         obj%selinc%primary,  &
                         obj%selnum%primary)) return
      end if

      if (obj%selhdr%secondary > 0 .and. val%secondary /= DNIL) then
        if (.not. mth_included (val%secondary,  &
                         obj%selmin%secondary,  &
                         obj%selwid%secondary,  &
                         obj%selinc%secondary,  &
                         obj%selnum%secondary)) return
      end if

      if (obj%selhdr%tertiary > 0 .and. val%tertiary /= DNIL) then
        if (.not. mth_included (val%tertiary,  &
                         obj%selmin%tertiary,  &
                         obj%selwid%tertiary,  &
                         obj%selinc%tertiary,  &
                         obj%selnum%tertiary)) return
      end if

      skip = .false.

      end function trinsort_skip_trace


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine trinsort (obj,ntr,hd,tr)

      type(trinsort_struct),intent(inout) :: obj              ! arguments
      integer              ,intent(inout) :: ntr              ! arguments
      double precision     ,intent(out)   :: hd(:,:)          ! arguments
      real                 ,intent(out)   :: tr(:,:)          ! arguments
      integer                             :: irec,err         ! local
      character(len=80)                   :: msg              ! local
      type(triplesort_ints)               :: bin              ! local
      type(triplesort_doubles)            :: val              ! local
      logical                             :: new_group        ! local

!----------wait until no more traces if appending.

      if (obj%waiting) then
           if (ntr == NEED_TRACES .or. ntr >= 1) return
           obj%waiting = .false.
      end if

!----------get trace record number to read.

500   if (associated(obj%tftable)) then
           irec = tftable_next_record (obj%tftable,msg)

           if (irec == 0) then
                call pc_error (msg)
                call pc_error ('TRINSORT: FATAL ERROR')
                ntr = FATAL_ERROR
                call trinsort_wrapup (obj)
                return
           else if (irec == -1) then
                call pc_print ('TRINSORT: FINISHED WITH TRACE FILE TABLE')
                ntr = NO_MORE_TRACES
                call trinsort_wrapup (obj)
                return
           end if
      else
           irec = 0      ! read traces sequentially.
      end if

!----------decide whether to select trace (before reading trace from file).

      if (obj%selecting .and. irec > 0) then
           val = obj%values(irec)
           if (trinsort_skip_trace(obj,val)) go to 500
      end if

!----------read desired trace.

      call permtset_read (obj%permtset, hd(:,1), tr(:,1), err, irec)

      if (err == PERMTSET_ERROR) then
           call pc_error ('TRINSORT: FATAL ERROR - IREC =',irec)
           ntr = FATAL_ERROR
           call trinsort_wrapup (obj)
           return
      else if (err == PERMTSET_EOF) then
           call pc_print ('TRINSORT: FINISHED WITH TRACES ON TRACE FILES')
           ntr = NO_MORE_TRACES
           call trinsort_wrapup (obj)
           return
      end if

!----------decide whether to select trace (after reading trace from file).

      if (obj%selecting) then
   !       val = 1.0D0
   !       if (obj%selhdr%primary  >0) val%primary  =hd(obj%selhdr%primary  ,1)
   !       if (obj%selhdr%secondary>0) val%secondary=hd(obj%selhdr%secondary,1)
   !       if (obj%selhdr%tertiary >0) val%tertiary =hd(obj%selhdr%tertiary ,1)
           if (val%primary   == DNIL .and. obj%selhdr%primary  >0) then
                          val%primary  =hd(obj%selhdr%primary  ,1)
           else
                          val%primary  =DNIL
           end if
           if (val%secondary == DNIL .and. obj%selhdr%secondary>0) then
                          val%secondary=hd(obj%selhdr%secondary,1)
           else
                          val%secondary=DNIL
           end if
           if (val%tertiary  == DNIL .and. obj%selhdr%tertiary >0) then
                          val%tertiary =hd(obj%selhdr%tertiary ,1)
           else
                          val%tertiary =DNIL
           end if
           if (trinsort_skip_trace(obj,val)) go to 500
      end if

!----------output the trace and set header word 1.

      ntr                = 1
      obj%sequence       = obj%sequence + 1
      hd(HDR_SEQUENCE,1) = obj%sequence

!----------return if reading traces sequentially.

      if (obj%how_read_traces == 'SEQUENTIAL') return

!----------return if OLD_SORT and table file contains original trace order.

      if (obj%hdr%primary == 0) return

!----------set header words 3 and 4 since trace order is changing.

      val%primary   = hd(obj%hdr%primary  ,1)
      val%secondary = hd(obj%hdr%secondary,1)
      val%tertiary  = hd(obj%hdr%tertiary ,1)

      bin = triplesort_binning (val,obj%init,obj%inc)

      if (obj%sequence == 1) then
           new_group = .true.
      else if (obj%hdr%tertiary <= 1) then
           new_group = (bin%primary   /= obj%binlast%primary)
      else
           new_group = (bin%secondary /= obj%binlast%secondary .or. &
                        bin%primary   /= obj%binlast%primary)
      endif

      if (new_group) then
           obj%group   = obj%group + 1
           obj%channel = 0
      end if

      obj%channel               = obj%channel  + 1
      hd(HDR_CURRENT_GROUP  ,1) = obj%group
      hd(HDR_CURRENT_CHANNEL,1) = obj%channel
      obj%binlast               = bin

      end subroutine trinsort


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine trinsort_wrapup (obj)

      type(trinsort_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call tftable_close   (obj%tftable)
      call permtset_close (obj%permtset)

      end subroutine trinsort_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module trinsort_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

