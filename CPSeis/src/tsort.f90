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
! Name       : TSORT   (Trace SORT)
! Category   : sorts
! Written    : 1990-03-15   by: Tom Stoeckley
! Revised    : 2010-07-22   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Sort traces by header word values.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! TSORT sorts (reorders) traces using a binning scheme in which traces are
! assigned to as many as three levels of bins according to the header words
! HDR_PRI, HDR_SEC and HDR_TERT.  Traces are output so that the traces in each
! primary bin are output before those in the next primary bin.  Traces are
! ordered within each primary bin so that those in each secondary bin are
! output before those in the next secondary bin.  Similarly, traces are ordered
! within each secondary bin so that those in each tertiary bin are output
! before those in the next tertiary bin.
!
! If one of the sort header words is 1, that level of sort is based on the
! sequential trace number, the order of the input traces.  If you want fewer
! than three levels of sort, the lowest level(s) should use header word 1.
!
! TSORT will store input traces on disk until MNTOD traces have been stored.
! When MNTOD traces have been stored on disk, TSORT will begin to pass out
! traces while reading in new traces until all traces have been sorted.
!
! If at any time TSORT receives a trace that is assigned to a bin that was
! previously output, it will delete that trace and print a message indicating
! how many such traces were deleted.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
! Setting MNTOD:
! Because TSORT may pass out traces before it has read in all input traces,
! MNTOD must be set large enough so that the first MNTOD traces will include
! all the traces assigned to the first primary bin.
!
! For "easy" sorts, such as sorting from shot profile order to CMP order, MNTOD
! should be set to approximately N squared, where N is the number of traces in
! the shot profile.  For "hard" sorts, such as sorting from shot profile or CMP
! order to common offset order, MNTOD will have to be set to the size of the
! whole dataset.
!
! Setting ATOD:
! If it is necessary for all traces to be stored on disk at once, MNTOD must be
! set at least as large as the number of traces in the dataset.  When ATOD is
! set to YES then TSORT will print a warning message if the number of input
! traces exceeds MNTOD.
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
!  55. 2010-07-22 Stoeckley  Generate error if MNTOD is <= 0 instead of
!                             resetting it to the default 10000.
!  54. 2010-02-22 Stoeckley  Add public subroutines tsort_get_primary_bin and
!                             tsort_get_primary_bin_center and tsort_get_bin
!                             to be called by PARALLELSORT.
!053. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!052. 2006-01-10  B. Menger  Removed Unused Variables.
!  51. 2004-05-26 Done       Add gui fields HDR_PRI_BIN, HDR_SEC_BIN,
!                             and HDR_TERT_BIN. If the contents of these
!                             fields are > 1, the corresponding bin center
!                             value is saved in that header location for
!                             each output trace.
!  50. 2003-12-09 Stoeckley  Increase output format field to display larger
!                             numbers.
!  49. 2003-10-07 Stoeckley  Change from real to double precision.
!  48. 2002-08-26 Stoeckley  Add error check for memory allocations.
!  47. 2002-02-04 Stoeckley  Remove some code to new TSORTPARAMS primitive for
!                             reuse by new processes TABLESORT and TRINSORT.
!  46. 2001-05-14 Stoeckley  Modify to reset extent size for large files.
!  45. 2001-04-04 Stoeckley  Fix GUI problem with sort option and header word
!                             fields.
!  44. 2001-03-20 Stoeckley  Remove the FILL option (it has been placed into
!                             a new process called FILL instead for simplicity,
!                             user flexibility, maintainability, and easier
!                             CPU parallelization).  Replace sorting code with
!                             calls to the TRIPLESORT primitive.  Fix some
!                             logic which occasionally caused a few traces to
!                             be sorted incorrectly.  Change some other logic
!                             to speed up the reduced-disk sort user CPU time
!                             (by a factor of about 4 in my tests) and the
!                             reduced-disk sort clock time (by a factor of
!                             about 2 in my tests).  This new logic is more
!                             like the old Cray TSORT logic.
!  43. 2001-01-23 Stoeckley  Modify to use TEMPTFILE instead of TRCIO.
!  42. 2000-12-04 Stoeckley  Fix bug where bin number was set into hwd3 before
!                             using hwd3 when hwd3 is a sort header; fix bug
!                             where the FILL option wrongly threw out traces
!                             when one of the sort header words was 1; fix
!                             printout bug when traces outside of user bin
!                             range are dropped.
!  41. 2000-11-14 Stoeckley  Fix bug getting fold if tertiary sort is not hwd1.
!  40. 2000-10-13 Stoeckley  Fix bug when number of header words exceeds 64.
!  39. 2000-08-27 O'Brien    Desensitize headerword selection for predefined
!                             sort options.Only sensitive for CUSTOM sorts.
!  38. 2000-07-12 O'Brien    tmp file i/o switched from native FORTRAN i/o to
!                             trcio. File is still invisible to user.
!  37. 2000-05-25 O'Brien    Implement optional bin range limits when FILL==YES.
!  36. 2000-04-27 O'Brien    Add counters for writes to and read from tmpfile.
!                             Remove forced header defs for OPT_SORT=='CUSTOM'
!                             Fix bug when gathered data are input.
!                             Implemented GUI_DEFS.
!  35. 2000-03-09 O'Brien    Set global variables GATHERED and NUMTR.
!  34. 2000-02-23 O'Brien    Made header calculation for fill traces
!                             consistent with bin calculation from input
!                             headers.
!  33. 2000-02-16 O'Brien    Allow selection of common sorts with OPT_SORT.
!  32. 2000-02-10 O'Brien    Removed parameter DISKNAME all together, using
!                             scratch file instead.
!  31. 2000-02-08 O'Brien    Removed username and node from DISKNAME in order
!                             for fortran OPEN to work.
!  30. 2000-02-03 O'Brien    Put TWOSETS, NEED_LABEL and NEED_REQUEST control
!                             parameters in the parameter cache.
!  29. 2000-02-02 O'Brien    Pieced in new document from C.I. Burch.
!  28. 2000-01-31 O'Brien    Removed FILENAME_LENGTH parameter as it's now
!                             available through pathcheck_module.
!  27. 2000-01-28 O'Brien    Implemented pc_put_options_field for OPT_PRINT,
!                             FILL, and ATOD parameters.
!                            Added PATHCHECK logic for parameter DISKNAME.
!  26. 2000-01-13 O'Brien    Test for pc_do_not_process_traces() in wrapup
!                             routine to avoid accessing files that weren't
!                             ever created.
!  25. 2000-01-13 O'Brien    Fixed 'premature' test in reporter routine.
!  24. 2000-01-11 O'Brien    Added diagnostic messages to printed summary.
!  23. 2000-01-08 O'Brien    Modified tsort_delete to call tsort_wrapup
!                             when running from <execute_only> mode.
!  22. 1999-12-29 O'Brien    Implemented derived data type(triplesort_ints)
!                             for sort keys and many of the variables
!                             they're compared with.
!                             Removed SORT_TYPE paramerter.
!                             Renamed tsort_tkshell to tsort_shellisort.
!  21. 1999-12-16 O'Brien    Added RCS identifier variable.
!                             Added development & testing param SORT_TYPE.
!                             Inserted routine tsort_tkshell
!                               (Three Key SHELL sort).
!                             "PROGRAMMING NOTES" section brought up to date.
!  20. 1999-10-20 O'Brien    Implemented fill option.
!  19. 1999-10-08 O'Brien    Full f90 conversion.
!  18. 1998-11-05 Goodger    Begin using the f90 compiler.
!  17. 1994-06-21 Troutt     Remove the "-f" that was in the rm command for
!                             deleting the file from $SORTDIR (the -f option
!                             returns zero status even if the file does not
!                             exist!!).
!  16. 1993-05-12 Peterson   New parameter SAVE for sorting disk = $SORTDIR.
!  15. 1993-05-07 Peterson   Hardwired DTROT-DTRIN sorting disk = $SORTDIR.
!  14. 1993-04-15 Peterson   Changed name of subroutine TSORTB to TSORTC so
!                             it would not interfere with new sort method
!                             using routines called TSORTA and TSORTB.
!  13. 1992-09-24 Troutt     Add ALLONDSK to argument list for TSORTW2.
!  12. 1992-07-08 Howard     Call DTROT instead of DTWRITE if sequential.
!  11. 1992-02-14 Howard     Call DTLIST (for read ahead) if ALLONDSK=YES.
!  10. 1991-09-18 Troutt     Correct mispelled variable from 09-17.
!   9. 1991-09-17 Troutt     Remove DTROT file when finished.
!   8. 1991-08-07 Troutt     Print out maximum fold at N=0.
!   7. 1991-07-30 Troutt     Add warning when ALLONDSK=YES and MAXONDSK not
!                             large enough.
!   6. 1991-04-03 Troutt     Add PRINT parameter to provide option to limit
!                             printout.
!   5. 1990-08-06 Stoeckley  Change header 3 to be incremented differently.
!   4. 1990-07-02 Stoeckley  Increase max# bins limit from 48576 to 1097152.
!   3. 1990-05-04 Stoeckley  Remove restriction on VAL and VINC for header
!                             word 1.
!   2. 1990-03-21 Stoeckley  Fix bug to use all traces in a gather.
!   1. 1990-03-15 Stoeckley  First version.
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
! Note that PRI_INIT values, etc., specify header word values at the center
! of bin number 1 and that zero or negative numbered bins are possible.
!
! The first thing TSORT does is reduce HDR_PRI, HDR_SEC, and HDR_TERT to their
! respective bin numbers by applying PRI_INC, SEC_INC, TERT_INC, PRI_INIT,
! SEC_INIT, and TERT_INIT.  The resulting integer bin values are loaded into
! a derived data type called TRIPLESORT_INTS. This is done to make the code
! more readable by allowing comparisons of multiple sort keys with
! easy-to-understand single operations.
!
! As traces are passed in, an array of TRIPLESORT_INTS called BINS() is
! accumulated, while trace data and headers are saved to a temporary disk
! file.  When MNTOD traces have accumulated, BINS() is sorted by the
! TRIPLESORT primitive, and the first trace is passed out.
!
! Each additional trace passed in is stored on disk at the index location of
! the last trace that was passed out.  As new traces arrive, the smallest
! unsorted value in BINS() is remembered.  Whenever the next sorted trace to
! be passed out follows the smallest unsorted trace, or all of the sorted
! traces have been passed out, BINS() is sorted again.  When there are
! NO_MORE_TRACES, a final sort of BINS() is performed and the traces
! remaining on disk are written in the order determined by that final sort.
!
!-------------------------------------------------------------------------------
!        POSSIBLE ENHANCEMENTS RECOMMENDED BY THE CONVERTER TO NEWCPS
!                               (Year 2000)
!
! Performing all the intermediate MINLOC operations and the final sort might be
! avoided if a linked list is built after the first sort. The link information
! could be stored in the BINS() structure if it were extended to hold more
! than 3 parameters. If a linked list were used, only one link in the list
! needs to be found and altered for each new trace (an operation of order
! log N), and the output order is already known when NO_MORE_TRACES are
! being passed in.
!
! For 'very large' MNTOD, it might be more efficient to implement either the
! quick sort or heap sort algorithms. The term 'very large' is hard to define.
! It's probably within the range 500-100000, but algorithmic advantages will
! probably be noticed only when MNTOD > ~15000.
!
!-------------------------------------------------------------------------------
!             COMMENTS REGARDING THE ABOVE POSSIBLE ENHANCEMENTS
!                               (March 2001)
!
! The MINLOC operations mentioned above were subsequently replaced by
! additional sorts for a large speedup, because a MINLOC operation had to
! be executed for each trace passed out while the BINS() array contained
! both sorted and unsorted traces.  The MINLOC operation was of order N for
! each trace and therefore N squared for all the traces, whereas the sorts
! are of order N log N and need to be performed much less frequently.
!
! The speedup is achieved only when using the reduced-disk sort (ATOD = NO).
!
! I think that searching a linked list would also be of order N for each trace
! because I do not know how a linked list can be searched except linearly.
! Instead, a binary search would be of order log N for each trace if an array
! was used (as at present) instead of a linked list, but inserting each trace
! in the proper location would require lots of copying.  Perhaps a binary tree
! could be used, which would be of order log N for each trace if it could be
! kept reasonably balanced.  I think that any of these methods could in
! principle eliminate all sorting, including the first sort.
!
! Please would anyone reading this let me know if my impressions are incorrect
! or other possibilities can be tried.
!
! A FINAL NOTE: In my tests, using the initial and final sort and the
! intermediate MINLOC operations resulted in approximately equal timings
! for the trace I/O and the sorting/MINLOC operations (for reduced-disk
! sort).  Replacing the MINLOC operations with additional sorts reduced the
! sorting/MINLOC timing by a large factor (about 4) and hence nearly doubled
! the throughput.  Further sorting enhancements will probably not buy us
! much because the trace I/O now dominates the clock time.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TSORT Process/NC=80>
!                   Sort traces by header word values.
!
!     MNTOD=~~~`IIIIIIIIII     ATOD=`CC       OPT_PRINT=`CCCCCCCC
!
!
!<include tsortparams.f90>
!
! `----------------------------------------------------------------------------
! | Header Locations for Storing Bin Values From Sort
! |
! | HDR_PRI_BIN = `IIIII     HDR_SEC_BIN = `IIIII     HDR_TERT_BIN = `IIIII
! `----------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MNTOD">
!<Tip> Maximum Number of Traces On Disk at once. </Tip>
! Default = 10000
! Allowed = int > 0
!
! MNTOD is the maximum number of traces to store on disk at once for the
! sorting operation.
!</Help>
!
!
!<Help KEYWORD="ATOD">
!<Tip> Whether to store All input Traces On Disk at once?. </Tip>
! Default = N
! Allowed = YES/NO
!</Help>
!
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Option whether to print detailed or summary sort statistics. </Tip>
! Default = DETAILED
! Allowed = DETAILED   (Print detailed sort statistics.)
! Allowed = SUMMARY   (Print summary sort statistics.)
!</Help>
!
!<Help KEYWORD="HDR_PRI_BIN">
!<Tip> Header word in which to store value of primary sort bin center. </Tip>
! Default = 0
! Allowed = 0 or any valid header word number
!
! If HDR_PRI_BIN > 1, then store the value of the primary bin center in
! header location HDR_PRI_BIN of the output trace header.
!</Help>
!
!<Help KEYWORD="HDR_SEC_BIN">
!<Tip> Header word in which to store value of secondary sort bin center. </Tip>
! Default = 0
! Allowed = 0 or any valid header word number
!
! If HDR_SEC_BIN > 1, then store the value of the secondary bin center in
! header location HDR_SEC_BIN of the output trace header.
!</Help>
!
!<Help KEYWORD="HDR_TERT_BIN">
!<Tip> Header word in which to store value of tertiary sort bin center. </Tip>
! Default = 0
! Allowed = 0 or any valid header word number
!
! If HDR_TERT_BIN > 1, then store the value of the tertiary bin center in
! header location HDR_TERT_BIN of the output trace header.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tsort_module

      use pc_module
      use named_constants_module
      use getlun_module
      use string_module
      use temptfile_module
      use triplesort_module
      use tsortparams_module

      implicit none

      private
      public :: tsort_create
      public :: tsort_initialize
      public :: tsort_update
      public :: tsort_delete
      public :: tsort
      public :: tsort_wrapup
      public :: tsort_get_primary_bin         ! called from parallelsort
      public :: tsort_get_primary_bin_center  ! called from parallelsort
      public :: tsort_get_bin                 ! called from parallelsort

      character(len=100),public,save :: TSORT_IDENT = &
'$Id: tsort.f90,v 1.53 2006/10/17 13:45:49 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: tsort_struct              

        private
        logical                  :: skip_wrapup      ! wrapup flag.
        integer                  :: nwih,ndpt        ! global parameters.
        integer                  :: mntod            ! process parameters.
        logical                  :: atod             ! process parameters.
        character(len=32)        :: opt_print        ! process parameters.

        type(triplesort_ints)    :: hdr        ! parameters from tsortparams.
        type(triplesort_doubles) :: init       ! parameters from tsortparams.
        type(triplesort_doubles) :: inc        ! parameters from tsortparams.

        type(tsortparams_struct),pointer :: tsortparams  ! dependent.

        type(temptfile_struct),pointer :: temptfile
        integer                        :: lun,infolu,debuglu
        integer                        :: input,unsorted,deleted,output,groups
        integer                        :: fold,primfold    
        integer                        :: ikeep,nkeep
        type(triplesort_ints)          :: bin_min_in     
        type(triplesort_ints)          :: bin_max_in    
        type(triplesort_ints)          :: bin_min_out  
        type(triplesort_ints)          :: bin_max_out 
        type(triplesort_ints)          :: binlast          
        type(triplesort_ints)          :: minbin 
        type(triplesort_ints),pointer  :: bins(:)    
        integer              ,pointer  :: indices(:)       
        integer                        :: npoint,ipoint   
        integer                        :: k1,k2,k3,k4
        integer                        :: maxfold 
        integer                        :: traces_to_file     
        integer                        :: traces_from_file  
        integer                        :: hdr_pri_bin  
        integer                        :: hdr_sec_bin  
        integer                        :: hdr_tert_bin  

      end type tsort_struct


!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(tsort_struct),pointer,save :: object ! needed for traps.

      integer,parameter      :: SINGLE_TRACE = 1     ! special value of NTR.

  !!  logical,parameter      :: OPT_DEBUG = .true.
      logical,parameter      :: OPT_DEBUG = .false.

      real   ,parameter      :: FHUGE = HUGE(1.0)
      integer,parameter      :: IHUGE = HUGE(1) 

      integer,parameter      :: print_nopt = 2

      character(len= 8),save :: print_options (print_nopt)

      data print_options /'DETAILED','SUMMARY '/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tsort_create (obj)
      implicit none
      type(tsort_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%bins) 
      nullify  (obj%indices)
      nullify  (obj%temptfile)
      nullify  (obj%tsortparams) ! jpa

      obj%infolu  = 0
      obj%debuglu = 0

      call tsortparams_create (obj%tsortparams)
      call tsort_initialize   (obj)
      return
      end subroutine tsort_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tsort_delete (obj)
      implicit none
      type(tsort_struct),pointer :: obj       ! arguments

      call tsort_wrapup       (obj)
      call tsort_free         (obj)
      call tsortparams_delete (obj%tsortparams)

      deallocate(obj)
      return
      end subroutine tsort_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine tsort_initialize (obj)
      implicit none
      type(tsort_struct),pointer :: obj       ! arguments

      obj%mntod          = 10000
      obj%atod           = .false.
      obj%opt_print      = 'DETAILED'

      obj%hdr_pri_bin  = 0
      obj%hdr_sec_bin  = 0
      obj%hdr_tert_bin = 0

      call tsortparams_initialize (obj%tsortparams)
      call tsort_update           (obj)
      return
      end subroutine tsort_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine tsort_update (obj)
      implicit none

      type(tsort_struct),target :: obj                    ! arguments
      integer                   :: ier,indx               ! local

      type(triplesort_ints),target :: tripsortints        ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!


      call pc_get_global ('NWIH' , obj%nwih)
      call pc_get_global ('NDPT' , obj%ndpt)

      call pc_get ('MNTOD'     , obj%mntod         )
      call pc_get ('ATOD'      , obj%atod          )
      call pc_get ('OPT_PRINT' , obj%opt_print     )

      call pc_get ('HDR_PRI_BIN' , obj%hdr_pri_bin     )
      call pc_get ('HDR_SEC_BIN' , obj%hdr_sec_bin     )
      call pc_get ('HDR_TERT_BIN', obj%hdr_tert_bin    )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%mntod <= 0) then
        call pc_error ('MNTOD must be > zero.')
!!!!!   obj%mntod = 10000
      endif

      if (obj%mntod > 2000000000 / 16) then
        call pc_warning ('MNTOD appears unrealistically large - please check!')
      endif

      select case (obj%opt_print)
        case ('DETAILED') ; continue
        case ('SUMMARY' ) ; continue
        case default      ; obj%opt_print = 'DETAILED'
      end select

      call tsortparams_update (obj%tsortparams)

!     set header store location values depending on header values from
!     tsortparams
      tripsortints =  tsortparams_get_hdr(obj%tsortparams)
      if (tripsortints%primary <= 1) then
        obj%hdr_pri_bin = 0
        call pc_put ('HDR_PRI_BIN' , obj%hdr_pri_bin     )
      end if
      if (tripsortints%secondary <= 1) then
        obj%hdr_sec_bin = 0
        call pc_put ('HDR_SEC_BIN' , obj%hdr_sec_bin     )
      end if
      if (tripsortints%tertiary <= 1) then
        obj%hdr_tert_bin = 0
        call pc_put ('HDR_TERT_BIN' , obj%hdr_tert_bin     )
      end if
      if (obj%hdr_pri_bin > obj%nwih) then
        call pc_error('HDR_PRI_BIN must be <= ', obj%nwih)
      end if
      if (obj%hdr_sec_bin > obj%nwih) then
        call pc_error('HDR_SEC_BIN must be <= ', obj%nwih)
      end if
      if (obj%hdr_tert_bin > obj%nwih) then
        call pc_error('HDR_TERT_BIN must be <= ', obj%nwih)
      end if


!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put_options_field ('OPT_PRINT',print_options, print_nopt)

      call pc_put ('MNTOD'     , obj%mntod         )
      call pc_put ('ATOD'      , obj%atod          )
      call pc_put ('OPT_PRINT' , obj%opt_print     )

      call pc_put ('HDR_PRI_BIN' , obj%hdr_pri_bin     )
      call pc_put ('HDR_SEC_BIN' , obj%hdr_sec_bin     )
      call pc_put ('HDR_TERT_BIN', obj%hdr_tert_bin    )

      call pc_put_control ('NSTORE'      , 4 * obj%mntod)
      call pc_put_control ('NDISK'       , obj%mntod * (obj%ndpt + 2*obj%nwih))
      call pc_put_control ('TWOSETS'     , .true.)
      call pc_put_control ('NEED_LABEL'  , .true.)
      call pc_put_control ('NEED_REQUEST', .true.)

      call pc_put_global ('GATHERED', .false.)
      call pc_put_global ('NUMTR'   ,    1   )


!!-------------------------- set sensitivity -------------------------------!!
!!-------------------------- set sensitivity -------------------------------!!
!!-------------------------- set sensitivity -------------------------------!!


      tripsortints =  tsortparams_get_hdr(obj%tsortparams)
      call pc_put_sensitive_field_flag ('HDR_PRI_BIN', &
                                        tripsortints%primary > 1)
      call pc_put_sensitive_field_flag ('HDR_SEC_BIN', &
                                        tripsortints%secondary > 1)
      call pc_put_sensitive_field_flag ('HDR_TERT_BIN', &
                                        tripsortints%tertiary > 1)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call tsort_free (obj)

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!----------INITIALIZE DEPENDENT VARIABLES.

      obj%hdr  = tsortparams_get_hdr  (obj%tsortparams)
      obj%init = tsortparams_get_init (obj%tsortparams)
      obj%inc  = tsortparams_get_inc  (obj%tsortparams)

      obj%lun = pc_get_lun()

      obj%input    = 0   ! number of traces input.
      obj%unsorted = 0   ! number of improperly sorted traces passed out.
      obj%deleted  = 0   ! number of traces received too late for output.
      obj%output   = 0   ! number of traces output.
      obj%groups   = 0   ! number of groups output.
      obj%primfold = 0   ! fold in each primary sort bin.
      obj%fold     = 0   ! fold in each group passed out.

      obj%ikeep = 0   ! number of traces already used from input trace array.
      obj%nkeep = 0   ! total number of traces in the input trace array.

      obj%bin_min_in   =  IHUGE   ! minimum input bin.
      obj%bin_max_in   = -IHUGE   ! maximum input bin.
      obj%bin_min_out  =  IHUGE   ! minimum output bin.
      obj%bin_max_out  = -IHUGE   ! maximum output bin.
      obj%binlast      = -IHUGE   ! last bin output.
      obj%minbin       = -IHUGE   ! minimum UNSORTED bin on disk.

      obj%maxfold      = -IHUGE   ! maximum fold in any group passed out.

      obj%npoint = 0    ! number of traces on disk.
      obj%ipoint = 0    ! next trace to read or write.

      obj%k1 = 0      ! counter for first sort.
      obj%k2 = 0      ! counter for second sorts.
      obj%k3 = 0      ! counter for third sorts.
      obj%k4 = 0      ! counter for fourth sort.

      obj%traces_to_file   = 0
      obj%traces_from_file = 0

!----------GET AN I/O UNIT FOR DETAILED INFORMATION.

      if (obj%opt_print == 'DETAILED') then
        call getlun (obj%infolu,ier)
        if (ier /= 0) then
          call pc_error ("TSORT failed to get an i/o unit for detailed info.")
        else
          open (unit=obj%infolu, status='SCRATCH', iostat=ier, &
                access='SEQUENTIAL', action='READWRITE')
          if (ier /= 0) then
            call pc_error ("TSORT had trouble opening file for detailed info.")
          endif
        endif
      endif

!----------GET AN I/O UNIT FOR DEBUGGING INFORMATION.

      if (OPT_DEBUG) then
        call getlun (obj%debuglu,ier)
        if (ier /= 0) then
          call pc_error ("TSORT failed to get an i/o unit for debug info.")
        else
          open (unit=obj%debuglu, status='REPLACE', iostat=ier)
          if (ier /= 0) then
            call pc_error ("TSORT had trouble opening file for debug info.")
          endif
        endif
      endif

!----------Must set up a temporary file for trace and header storage.

      call temptfile_open &
                (obj%temptfile, 'TSORT', obj%nwih, obj%ndpt, obj%lun, ier, &
                                     maxrecords=obj%mntod, vartypes='DR')
      if (ier /= TEMPTFILE_OK) then
        call pc_error ('TSORT: error opening TEMPTFILE')
      endif

!----------ALLOCATE PERMANENT MEMORY.

      allocate(obj%bins   (obj%mntod), stat=ier)
      if (ier /= 0) then
        call pc_error ('TSORT: error allocating BINS to size',obj%mntod)
      endif
      allocate(obj%indices(obj%mntod), stat=ier)
      if (ier /= 0) then
        call pc_error ('TSORT: error allocating INDICES to size',obj%mntod)
      endif
      if (pc_do_not_process_traces()) return

!----------THE INDICES VECTOR CAN BE FILLED RIGHT AWAY.

      obj%indices(:) = (/(indx,indx=1,obj%mntod)/)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine tsort_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine tsort (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(tsort_struct),intent(inout) :: obj                  ! arguments
      integer           ,intent(inout) :: ntr                  ! arguments
      double precision  ,intent(in)    :: hdi(:,:)             ! arguments
      real              ,intent(in)    :: tri(:,:)             ! arguments
      double precision  ,intent(out)   :: hdo(:,:)             ! arguments
      real              ,intent(out)   :: tro(:,:)             ! arguments

      type(triplesort_ints)            :: bin,binout           ! local

!----------CHECK WHETHER WE ARE RECEIVING NEW INPUT TRACES.

   if (ntr >= 0) then      ! we are receiving traces from above.
        obj%nkeep = ntr    ! this will be positive or NO_MORE_TRACES.
        obj%ikeep = 0      ! this is the number of input traces already used
                           !  from the input trace array.
   end if

!----------PROCESS THE NEXT INPUT TRACE(S).

   if (obj%nkeep > 0) then
        do

             obj%ikeep = obj%ikeep + 1
             if (obj%ikeep > obj%nkeep) then
                  ntr = NEED_TRACES
                  return
             end if
             obj%input = obj%input + 1

             call tsort_pre_process   (obj,hdi(:,obj%ikeep),bin)
             call tsort_here_is_input (obj,bin,                            &
                                       hdi(:,obj%ikeep), tri(:,obj%ikeep), &
                                       hdo(:,1), tro(:,1), ntr, binout)
             select case (ntr)
                case (NEED_TRACES)
                  cycle
                case (SINGLE_TRACE)
                  exit
                case (FATAL_ERROR)
                  call tsort_wrapup (obj)
                  return
                case default
                  call pc_error ('we should not get to here with NTR =',ntr)
                  call pc_error ('programming error in TSORT_HERE_IS_INPUT')
                  ntr = FATAL_ERROR
                  call tsort_wrapup (obj)
                  return
             end select

        end do

!----------THERE ARE NO MORE INPUT TRACES.

   else

        call tsort_no_more_input (obj, hdo(:,1), tro(:,1), ntr, binout)

        select case (ntr)
           case (NO_MORE_TRACES)
             call tsort_wrapup (obj)
             return
           case (FATAL_ERROR)
             call tsort_wrapup (obj)
             return
           case (SINGLE_TRACE)
             continue
           case default
             call pc_error ('we should not get to here with NTR =',ntr)
             call pc_error ('programming error in TSORT_NO_MORE_INPUT')
             ntr = FATAL_ERROR
             call tsort_wrapup (obj)
             return
        end select

   end if

!----------NOW WE HAVE AN OUTPUT TRACE AND NTR HAS BEEN SET TO 1.

   if (ntr /= SINGLE_TRACE) then
        call pc_error ('we should not get to here with NTR =',ntr)
        call pc_error ('programming error in TSORT main routine')
        ntr = FATAL_ERROR
        call tsort_wrapup (obj)
        return
   end if

   call tsort_post_process (obj,binout,hdo(:,1))
   return
   end subroutine tsort


!!------------------------ get primary bin -------------------------------!!
!!------------------------ get primary bin -------------------------------!!
!!------------------------ get primary bin -------------------------------!!

             ! public routine to be called by parallelsort.

   function tsort_get_primary_bin (obj,pri_val) result (pri_bin)
   implicit none
   type(tsort_struct)   ,intent(inout) :: obj                  ! arguments
   double precision     ,intent(in)    :: pri_val              ! arguments
   integer                             :: pri_bin              ! result
   type(triplesort_doubles)            :: val                  ! local
   type(triplesort_ints)               :: bin                  ! local

   val%primary   = pri_val
   val%secondary = 0.0
   val%tertiary  = 0.0

   bin = triplesort_binning (val, obj%init, obj%inc)

   pri_bin = bin%primary
   return
   end function tsort_get_primary_bin


!!------------------------ get primary bin center -------------------------!!
!!------------------------ get primary bin center -------------------------!!
!!------------------------ get primary bin center -------------------------!!

             ! public routine to be called by parallelsort.

   function tsort_get_primary_bin_center (obj,pri_bin) result (pri_val)
   implicit none
   type(tsort_struct)   ,intent(inout) :: obj                  ! arguments
   integer              ,intent(in)    :: pri_bin              ! arguments
   double precision                    :: pri_val              ! result
   type(triplesort_ints)               :: bin                  ! local
   type(triplesort_doubles)            :: val                  ! local

   bin%primary   = pri_bin
   bin%secondary = 0
   bin%tertiary  = 0

   val = triplesort_unbinning (bin, obj%init, obj%inc)

   pri_val = val%primary
   return
   end function tsort_get_primary_bin_center


!!-------------------------- get bin ------------------------------------!!
!!-------------------------- get bin ------------------------------------!!
!!-------------------------- get bin ------------------------------------!!

             ! public routine to be called by parallelsort.

   function tsort_get_bin (obj,hdi) result (bin)
   implicit none
   type(tsort_struct)   ,intent(inout) :: obj                  ! arguments
   double precision     ,intent(in)    :: hdi(:)               ! arguments
   type(triplesort_ints)               :: bin                  ! result
   type(triplesort_doubles)            :: val                  ! local

!----------GET THE INPUT TRACE VALUES.

   val%primary   = hdi(obj%hdr%primary)
   val%secondary = hdi(obj%hdr%secondary)
   val%tertiary  = hdi(obj%hdr%tertiary)

!----------GET THE INPUT TRACE BIN TO SORT ON.

   bin = triplesort_binning (val, obj%init, obj%inc)

   return
   end function tsort_get_bin


!!-------------------------- pre process ---------------------------------!!
!!-------------------------- pre process ---------------------------------!!
!!-------------------------- pre process ---------------------------------!!


   subroutine tsort_pre_process (obj,hdi,bin)
   implicit none
   type(tsort_struct)   ,intent(inout) :: obj                  ! arguments
   double precision     ,intent(in)    :: hdi(:)               ! arguments
   type(triplesort_ints),intent(out)   :: bin                  ! arguments
   type(triplesort_doubles)            :: val                  ! local

!----------GET THE INPUT TRACE VALUES.

   val%primary   = hdi(obj%hdr%primary)
   val%secondary = hdi(obj%hdr%secondary)
   val%tertiary  = hdi(obj%hdr%tertiary)

!----------GET THE INPUT TRACE BIN TO SORT ON.

   bin = triplesort_binning (val, obj%init, obj%inc)

   if (obj%debuglu > 0) then
     write(obj%debuglu,"(3i7)") bin%primary,bin%secondary,bin%tertiary 
   end if

!----------UPDATE THE INPUT LIMITS.

   obj%bin_min_in = triplesort_minima (bin, obj%bin_min_in)
   obj%bin_max_in = triplesort_maxima (bin, obj%bin_max_in)
   return
   end subroutine tsort_pre_process


!!------------------------- post process ----------------------------------!!
!!------------------------- post process ----------------------------------!!
!!------------------------- post process ----------------------------------!!


   subroutine tsort_post_process (obj,bin,hdo)
   implicit none
   type(tsort_struct)   ,intent(inout) :: obj                  ! arguments
   type(triplesort_ints),intent(in)    :: bin                  ! arguments
   double precision     ,intent(out)   :: hdo(:)               ! arguments
   logical                             :: new_group            ! local
   double precision                    :: center               ! local

!----------CHECK THE PRIMARY SORT BIN AND SAVE INFO IF IT HAS CHANGED.

   if (obj%debuglu > 0) then
     write(obj%debuglu,"(20x,3i7)") bin%primary,bin%secondary,bin%tertiary
   end if

   if (obj%infolu > 0) then
     if (obj%primfold > 0 .and. bin%primary /= obj%binlast%primary) then
       write(obj%infolu,*) obj%binlast%primary,obj%primfold
       obj%primfold = 0
     endif
   endif

!----------RESET HEADER WORDS.

   if (obj%hdr%tertiary <= 1) then
        new_group = (bin%primary   /= obj%binlast%primary)
   else
        new_group = (bin%secondary /= obj%binlast%secondary .or. &
                     bin%primary   /= obj%binlast%primary)
   endif

   if (new_group) then
        obj%groups = obj%groups + 1
        obj%fold   = 0
   end if

   obj%primfold             = obj%primfold + 1
   obj%fold                 = obj%fold   + 1
   obj%output               = obj%output + 1
   hdo(HDR_SEQUENCE       ) = obj%output
   hdo(HDR_CURRENT_GROUP  ) = obj%groups
   hdo(HDR_CURRENT_CHANNEL) = obj%fold
   obj%maxfold              = max(obj%fold,obj%maxfold)

!----------IF SELECTED, SAVE VALUES FOR PRIMARY, SECONDARY, AND TERTIARY
!----------KEYS IN DESIRED HEADERS
   if (obj%hdr_pri_bin > 1) then
     center = obj%init%primary + (bin%primary - 1) * obj%inc%primary
     hdo(obj%hdr_pri_bin) = center
   end if
   if (obj%hdr_sec_bin > 1) then
     center = obj%init%secondary + (bin%secondary - 1) * obj%inc%secondary
     hdo(obj%hdr_sec_bin) = center
   end if
   if (obj%hdr_tert_bin > 1) then
     center = obj%init%tertiary + (bin%tertiary - 1) * obj%inc%tertiary
     hdo(obj%hdr_tert_bin) = center
   end if

!----------MAKE SURE THE TRACES ARE SORTED PROPERLY.

   if (bin < obj%binlast) obj%unsorted = obj%unsorted + 1

!----------REMEMBER THE OUTPUT TRACE BIN.

   obj%binlast = bin

!----------UPDATE THE OUTPUT LIMITS.

   obj%bin_min_out = triplesort_minima (bin, obj%bin_min_out)
   obj%bin_max_out = triplesort_maxima (bin, obj%bin_max_out)
   return
   end subroutine tsort_post_process


!!--------------------------- here is input --------------------------------!!
!!--------------------------- here is input --------------------------------!!
!!--------------------------- here is input --------------------------------!!
 

      subroutine tsort_here_is_input (obj, bin, hdi,tri, hdo,tro, ntr, binout)
      implicit none
      type(tsort_struct)   ,intent(inout) :: obj             ! arguments
      type(triplesort_ints),intent(in)    :: bin             ! arguments
      double precision     ,intent(in)    :: hdi(:)          ! arguments
      real                 ,intent(in)    :: tri(:)          ! arguments
      double precision     ,intent(out)   :: hdo(:)          ! arguments
      real                 ,intent(out)   :: tro(:)          ! arguments
      integer              ,intent(out)   :: ntr             ! arguments
      type(triplesort_ints),intent(out)   :: binout          ! arguments

      integer                             :: ier,irecord     ! local

!----SKIP THIS INPUT TRACE IF IT PRECEDES THE LAST TRACE OUTPUT.

      if (bin < obj%binlast) then
        obj%deleted = obj%deleted + 1
        ntr         = NEED_TRACES
        return
      endif

!----WE HAVE NOT YET FILLED UP THE DISK SPACE - WRITE TRACE TO DISK.

      if (obj%npoint < obj%mntod) then
        obj%npoint         = obj%npoint + 1
        irecord            = obj%npoint
        obj%bins(irecord)  = bin
        obj%traces_to_file = obj%traces_to_file + 1
        call temptfile_write (obj%temptfile,irecord,hdi,tri,ier)
        if (ier /= TEMPTFILE_OK) then
          call pc_error ("TSORT: Error writing tmp file at trace",irecord)
          ntr = FATAL_ERROR
          return
        endif
        ntr = NEED_TRACES
        return
      endif

!----NOW WE HAVE TO PASS OUT A TRACE - WE MAY HAVE TO SORT FIRST.

!----SORT IF WE HAVE NEVER SORTED BEFORE.

      if (obj%ipoint == 0) then
        if(obj%atod) then
          write(obj%lun,*)' '
          write(obj%lun,*)'   *********************************************'
          write(obj%lun,*)'   ********* TSORT ** WARNING ** TSORT *********'
          write(obj%lun,*)'   ****                                     ****'
          write(obj%lun,*)'   **** ATOD = YES, but more than MNTOD     ****'
          write(obj%lun,*)'   **** traces have been passed to TSORT.   ****'
          write(obj%lun,*)'   **** We will try to continue by sorting  ****'
          write(obj%lun,*)'   **** at this point. Your traces may not  ****'
          write(obj%lun,*)'   **** be completely sorted by TSORT.      ****'
          write(obj%lun,*)'   ****                                     ****'
          write(obj%lun,*)'   ********* TSORT ** WARNING ** TSORT *********'
          write(obj%lun,*)'   *********************************************'
          write(obj%lun,*)' '
        endif
        ! The first sort.
        call triplesort_sort (obj%indices,obj%bins,obj%npoint)
        obj%k1     = obj%k1 + 1
        obj%ipoint = 1
        obj%minbin = IHUGE

!----SORT IF WE HAVE ALREADY OUTPUT ALL SORTED TRACES FROM DISK.

      else if (obj%ipoint > obj%npoint) then
        call triplesort_sort (obj%indices,obj%bins,obj%npoint)
        obj%k2     = obj%k2 + 1
        obj%ipoint = 1
        obj%minbin = IHUGE

!----SORT IF AN UNSORTED TRACE ON DISK PRECEDES THE NEXT SORTED TRACE TO OUTPUT.

      else if (obj%minbin < obj%bins(obj%indices(obj%ipoint))) then
                       ! an unsorted trace comes before the next sorted trace.
        call triplesort_sort (obj%indices,obj%bins,obj%npoint)
        obj%k3     = obj%k3 + 1
        obj%ipoint = 1
        obj%minbin = IHUGE
      end if

!----IMMEDIATELY OUTPUT THIS INPUT TRACE IF IT PRECEDES ALL TRACES ON DISK.
!   (we already know that this trace does not precede the last trace output)
!   (we already know that the next sorted trace precedes all unsorted traces)

      irecord = obj%indices(obj%ipoint)    ! this is the next sorted trace.

      if (bin < obj%bins(irecord)) then
        hdo(1:obj%nwih) = hdi(1:obj%nwih)
        tro(1:obj%ndpt) = tri(1:obj%ndpt)
        binout          = bin
        ntr             = SINGLE_TRACE
        return
      endif

!----RETRIEVE THE OUTPUT TRACE FROM DISK AND SAVE THE INPUT TRACE IN ITS SLOT.

      obj%traces_from_file = obj%traces_from_file + 1
      call temptfile_read (obj%temptfile,irecord,hdo,tro,ier)
      if (ier /= TEMPTFILE_OK) then
        call pc_error ("TSORT: Error reading tmp file at trace",irecord)
        ntr = FATAL_ERROR
        return
      endif

      obj%traces_to_file = obj%traces_to_file + 1
      call temptfile_write (obj%temptfile,irecord,hdi,tri,ier)
      if (ier /= TEMPTFILE_OK) then
        call pc_error ("TSORT: Error rewriting tmp file at trace",irecord)
        ntr = FATAL_ERROR
        return
      endif

      obj%minbin        = min(bin,obj%minbin)   ! no longer a complete sort.
      binout            = obj%bins(irecord)
      obj%bins(irecord) = bin
      obj%ipoint        = obj%ipoint + 1
      ntr               = SINGLE_TRACE
      return
      end subroutine tsort_here_is_input


!!------------------------ no more input -----------------------------------!!
!!------------------------ no more input -----------------------------------!!
!!------------------------ no more input -----------------------------------!!


      subroutine tsort_no_more_input (obj, hdo,tro, ntr, binout)
      implicit none
      type(tsort_struct)   ,intent(inout) :: obj            ! arguments
      double precision     ,intent(out)   :: hdo(:)         ! arguments
      real                 ,intent(out)   :: tro(:)         ! arguments
      integer              ,intent(out)   :: ntr            ! arguments
      type(triplesort_ints),intent(out)   :: binout         ! arguments
      integer                             :: ier,irecord    ! local

!----------HOW MANY TRACES ARE WE STARTING WITH.

      if (obj%npoint == 0) then         ! there never were any traces.
        ntr = NO_MORE_TRACES
        return 
      endif

!----------WE MAY HAVE TO SORT THE DATA ONE LAST TIME.

      if (obj%minbin < IHUGE) then
        call triplesort_sort (obj%indices,obj%bins,obj%npoint)
        obj%k4     = obj%k4 + 1
        obj%ipoint = 1
        obj%minbin = IHUGE
      endif

!----------CHECK IF WE'VE REACHED THE END OF THE TMP FILE.

      if (obj%ipoint > obj%npoint) then
        ntr = NO_MORE_TRACES
        return
      endif

!----------RETRIEVE A TRACE FROM DISK.

      irecord              = obj%indices(obj%ipoint)
      obj%traces_from_file = obj%traces_from_file + 1
      call temptfile_read (obj%temptfile,irecord,hdo,tro,ier)
      if (ier /= TEMPTFILE_OK) then
        call pc_error ("TSORT: Error reading tmp file at trace",irecord)
        ntr = FATAL_ERROR
        return
      endif

      binout     = obj%bins(irecord)
      obj%ipoint = obj%ipoint + 1
      ntr        = SINGLE_TRACE
      return
      end subroutine tsort_no_more_input


!!--------------------------- reporter ------------------------------------!!
!!--------------------------- reporter ------------------------------------!!
!!--------------------------- reporter ------------------------------------!!


   subroutine tsort_reporter (obj)
   implicit none
   type(tsort_struct),intent(in) :: obj                         ! arguments
   integer                       :: ibin,lastbin,ier            ! local
   integer                       :: primfold,totfold            ! local
   double precision              :: center                      ! local
   type(triplesort_doubles)      :: val_min_in,val_max_in       ! local
   character(len=*),parameter    :: space = '     '             ! local
   character(len=*),parameter    :: pre   = space//'+++++++ '   ! local
   character(len=*),parameter    :: warn  = space// &           ! local
      '++++++++++++++++++++++++++++ WARNING +++++++++++++++++++++++++++++++'
   character(len=*),parameter    :: bad   = space// &           ! local
      '++++++++++++++++ TSORT WAS WRAPPED UP PREMATURELY ++++++++++++++++++'

!----------PRINT PREMATURE WRAPUP WARNINGS.

   if (obj%k1 == 0 .and. obj%k2 == 0 .and. &
       obj%k3 == 0 .and. obj%k4 == 0) then
     write(obj%lun,*) space
     write(obj%lun,*) bad
     write(obj%lun,*) bad
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'wrapup was called before sort was ever executed'
     write(obj%lun,*) pre
     write(obj%lun,*) bad
     write(obj%lun,*) bad
     write(obj%lun,*) space
   endif

   if (obj%traces_to_file /= obj%traces_from_file) then
     write(obj%lun,*) space
     write(obj%lun,*) bad
     write(obj%lun,*) bad
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'wrapup was called before tmp file was emptied'
     write(obj%lun,*) pre
     write(obj%lun,*) bad
     write(obj%lun,*) bad
     write(obj%lun,*) space
   endif

!----------PRINT SUMMARY INFORMATION.

2000 format (10x,'                                               ',    &
                        '-----INPUT VALUES------   ---INPUT BINS---'/  &
             10x,'   HDR           INIT            INC           ',    &
                        'MINIMUM         MAXIMUM   MINIMUM  MAXIMUM')
3000 format (1x,a10,i5,4f16.5,i10,i9)

   val_min_in = triplesort_unbinning (obj%bin_min_in, obj%init, obj%inc)
   val_max_in = triplesort_unbinning (obj%bin_max_in, obj%init, obj%inc)

   write(obj%lun,2000)
   write(obj%lun,3000) 'primary:  ',                                   &
                  obj%hdr       %primary   ,                           &
                  obj%init      %primary   ,obj%inc       %primary  ,  &
                      val_min_in%primary   ,    val_max_in%primary  ,  &
                  obj%bin_min_in%primary   ,obj%bin_max_in%primary  

   write(obj%lun,3000) 'secondary:',                                   &
                  obj%hdr       %secondary ,                           &
                  obj%init      %secondary ,obj%inc       %secondary,  &
                      val_min_in%secondary ,    val_max_in%secondary,  &
                  obj%bin_min_in%secondary ,obj%bin_max_in%secondary

   write(obj%lun,3000) 'tertiary: ',                                   &
                  obj%hdr       %tertiary  ,                           &
                  obj%init      %tertiary  ,obj%inc       %tertiary ,  &
                      val_min_in%tertiary  ,    val_max_in%tertiary ,  &
                  obj%bin_min_in%tertiary  ,obj%bin_max_in%tertiary 

   write(obj%lun,*) space
   write(obj%lun,*) space,'minimum output bin = ',obj%bin_min_out
   write(obj%lun,*) space,'maximum output bin = ',obj%bin_max_out
   write(obj%lun,*) space
   write(obj%lun,*) space,'maximum fold = ',obj%maxfold
   write(obj%lun,*) space
   write(obj%lun,*) space,obj%traces_to_file,' traces written to tmp file'
   write(obj%lun,*) space,obj%traces_from_file,' traces read from tmp file'
   write(obj%lun,*) space
   write(obj%lun,*) space,obj%input ,' traces input'
   write(obj%lun,*) space,obj%output,' traces output'
   write(obj%lun,*) space,obj%groups,' groups output'
   write(obj%lun,*) space
   write(obj%lun,*) space,obj%deleted,' traces received too late to output'
   write(obj%lun,*) space,obj%unsorted   ,' traces improperly sorted'
   write(obj%lun,*) space
   write(obj%lun,*) space,'headers were sorted ', &
                                      obj%k1,obj%k2,obj%k3,obj%k4,' times'

!----------PRINT ADDITIONAL WARNING MESSAGES.

   if (obj%deleted >= 1) then
     write(obj%lun,*) space
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) pre
     write(obj%lun,*) pre,obj%deleted,' traces received too late to output'
     write(obj%lun,*) pre,'            You must increase MNTOD'
     write(obj%lun,*) pre
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) space
   endif

   if (obj%unsorted >= 1) then
     write(obj%lun,*) space
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) pre
     write(obj%lun,*) pre,obj%unsorted,' traces improperly sorted'
     write(obj%lun,*) pre
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) space
   endif

   if (obj%bin_min_out /= obj%bin_min_in .or. &
       obj%bin_max_out /= obj%bin_max_in) then
     write(obj%lun,*) space
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'input and output bin ranges do not match'
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'minimum input bin = ',obj%bin_min_in
     write(obj%lun,*) pre,'maximum input bin = ',obj%bin_max_in
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'minimum output bin = ',obj%bin_min_out
     write(obj%lun,*) pre,'maximum output bin = ',obj%bin_max_out
     write(obj%lun,*) pre
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) space
   endif

!----------PRINT TABLE IF REQUESTED.

4000 format (' PRIMARY BIN NUMBER   PRIMARY BIN CENTER   PRIMARY BIN FOLD &
                            &  CUMULATIVE TRACES')
5000 format (1x,i11,f24.4,i16,i21,i12,' missing bins above')

   if (obj%infolu > 0) then
     if (obj%primfold > 0) &
                write (obj%infolu,*) obj%binlast%primary,obj%primfold
     rewind obj%infolu
     write(obj%lun,*) space
     write(obj%lun,4000)
     totfold = 0
     lastbin = obj%binlast%primary
     do
       read (obj%infolu,*,iostat=ier) ibin,primfold
       if(ier<0) exit  ! negative iostat means end of file/record

       if (totfold == 0) lastbin = ibin - 1
       center  = obj%init%primary + (ibin-1) * obj%inc%primary
       totfold = totfold + primfold
       if (ibin - lastbin == 1) then
         write(obj%lun,5000) ibin,center,primfold,totfold
       else
         write(obj%lun,5000) ibin,center,primfold,totfold, ibin-lastbin-1
       endif
       lastbin = ibin
     enddo
     write(obj%lun,4000)
   endif
   write(obj%lun,*) space
   return
   end subroutine tsort_reporter


!!------------------------------- free -------------------------------------!!
!!------------------------------- free -------------------------------------!!
!!------------------------------- free -------------------------------------!!


      subroutine tsort_free (obj)
      implicit none
      type(tsort_struct),intent(inout) :: obj       ! arguments

      if (associated(obj%bins   )) deallocate (obj%bins)
      if (associated(obj%indices)) deallocate (obj%indices)

      call temptfile_close (obj%temptfile)

      if (obj%infolu  > 0) close (unit=obj%infolu)
      if (obj%debuglu > 0) close (unit=obj%debuglu)

      obj%infolu  = 0
      obj%debuglu = 0
      return
      end subroutine tsort_free


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine tsort_wrapup (obj)
      implicit none
      type(tsort_struct),intent(inout) :: obj       ! arguments
      character(len=*),parameter :: stars = '******************************'

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      write(obj%lun,*) ''
      write(obj%lun,*) stars,'*******************',stars
      write(obj%lun,*) stars,'** TSORT SUMMARY **',stars
      write(obj%lun,*) stars,'*******************',stars
      write(obj%lun,*) ''

      call tsort_reporter (obj)
      call tsort_free     (obj)

      write(obj%lun,*) ''
      write(obj%lun,*) stars,'*******************',stars
      write(obj%lun,*) stars,' END TSORT SUMMARY ',stars
      write(obj%lun,*) stars,'*******************',stars
      write(obj%lun,*) ''
      return
      end subroutine tsort_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tsort_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

