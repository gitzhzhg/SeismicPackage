!<CPS_v1 type="PROCESS"/>
!!------------------------------- regbin.f90 ---------------------------------!!
!!------------------------------- regbin.f90 ---------------------------------!!
!!------------------------------- regbin.f90 ---------------------------------!!


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
! Name       : REGBIN          (regularize binning)
! Category   : filters
! Written    : 2001-12-04   by: Tom Stoeckley
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Regularize the binning of traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! REGBIN regularizes traces within CMP bins as follows:
!
!  (1) The user specifies a rectangular (X,Y) output grid.
!
!  (2) The user specifies search distances in the X and Y directions.
!      These search distances are measured from the center of the bin.
!      These search distances can be (and usually will be) large enough
!      to extend into neighboring bins, although they can also be smaller
!      than the distance to the edge of a bin.
!
!  (3) Each bin will contain at most one output trace, which will be the
!      closest available trace (within the search distance) to the bin center.
!
!  (4) If there are one or more traces within the specified search distance
!      of a bin center, and also within that bin, the bin will contain the
!      closest trace to the bin center.
!
!  (5) If there are no traces within a bin, and if the specified search
!      distance extends into neighboring bins, the bin will contain the
!      closest available trace from a neighboring bin.  An available trace
!      will never be a trace in a neighboring bin which is the closest trace
!      to its own bin center.
!
!  (6) If there are no available traces within the specified search distance
!      of a bin center, the bin will remain empty.
!
! Dead traces are deleted.
!
! If REGBIN is run on pre-stack data, it should be run on individual offset
! sections separately.
!
!-------------------------------------------------------------------------------
!                        POSSIBLE FUTURE PLANS
!
! REGBIN may grow to be an alternative to the FLEXBIN process if there is
! a demand.  The advantages (and disadvantages) would be these:
!
! Advantages of REGBIN:
!
!  (1) The flex binning (or regularizing binning) in REGBIN would be a single
!      pass process instead of a two-pass process.  (FLEXBIN requires a first
!      pass to gather the information and a second pass to perform the flex
!      binning.)
!
!  (2) No flexfile would be required by REGBIN to store information about
!      all the traces.  (FLEXBIN uses a flexfile.)
!
!  (3) REGBIN would not be a central memory hog (like FLEXBIN), but would
!      be able to work equally easily on any number of traces.
!
! Disadvantages of REGBIN:
!
!  (1) The traces for REGBIN must be sorted into approximate CMP order, with
!      the X coordinate changing the fastest.  (FLEXBIN accepts traces in any
!      order.)
!
!  (2) REGBIN will have to store a few traces at a time on a small revolving
!      disk file (a minor disadvantage).  (FLEXBIN does not use a temporary
!      disk file for traces.)
!
!  (3) Traces (actually only the trace headers or a subset thereof) would
!      have to be run through REGBIN each time new flex binning criteria
!      are tested, whereas FLEXBIN can be used to test flex binning criteria
!      by using only the flexfile.
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
! This process is a multiple-trace process.
! This process requires traces to be input one at a time.
!
! This process requires traces to be input in CMP order, with the primary
! sort in the Y direction and the secondary sort in the X direction.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process does not alter input traces.
! This process outputs a subset of the traces it receives.
! This process outputs one trace at a time (at most one trace per bin).
!
! This process outputs traces in CMP order, with the primary sort in the
! Y direction and the secondary sort in the X direction.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                           Action taken
! ----      -----------                           ------------
! NUMTR     max number of traces input/output     verified to be equal to 1
! NWIH      number of words in trace header       used but not changed
! NDPT      number of sample values in trace      used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#          Description                     Action taken
! ----          -----------                     ------------
! 1             sequence number                 reset as appropriate
! 25            largest absolute value          used but not changed
! HDR_X         actual X coordinate             used but not changed
! HDR_Y         actual Y coordinate             used but not changed
! HDR_X_NEW     new bin center X coordinate     set to center of output bin
! HDR_Y_NEW     new bin center Y coordinate     set to center of output bin
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!005. 2006-06-12  B. Menger   Removed Unused Variables.
!  4. 2005-01-31  Stoeckley  Add protective code to work better with traces
!                             which fall at the edge of a bin.
!  3. 2004-06-15  Stoeckley  Completely rewritten.
!  2. 2002-10-10  Stoeckley  Change to use the MTH module for binning.
!  1. 2001-12-26  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES           
!
! No known limitations.
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
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR == 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting traces.
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
!<NS REGBIN Process/NC=80>
!                          Regularize Binning
!
! `----------------- `-----------------
!  HDR_X~~~~=`II      HDR_Y~~~~=`II     [/L]Header word with actual coords.
!  X_INIT~~~=`FFFFF   Y_INIT~~~=`FFFFF  [/L]First output bin center.
!  X_INC~~~~=`FFFFF   Y_INC~~~~=`FFFFF  [/L]Output bin increment.
!  X_LAST~~~=`FFFFF   Y_LAST~~~=`FFFFF  [/L]Last output bin center.
!  X_TOT~~~~=`FFFFF   Y_TOT~~~~=`FFFFF  [/L]Number of output bins.
!
!  X_SEARCH =`FFFFF   Y_SEARCH =`FFFFF  [/L]Search distance for flex search.
!  HDR_X_NEW=`II      HDR_Y_NEW=`II     [/L]Header word with new bin center coords.
! `----------------- `-----------------
!
!  MAX_FOLD~~=`IIIII   [/L]Maximum number of input traces in a bin.
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="MAX_FOLD">
!<Tip> Maximum number of input traces in any one bin. </Tip>
! Default = 1
! Allowed = integer > 0
!
! This value is used, together with parameters X_SEARCH, X_INC, Y_SEARCH,
! and Y_INC, to calculate the maximum number of traces to search through
! for a given bin.
!
! Some traces may not be used if this value is too low.
!</Help>
!
!
!<Help KEYWORD="X_SEARCH">
!<Tip> Search distance in X direction for flex search. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This is the distance from the center of a bin (in the X direction)
! to search for candidates to be reassigned to the bin.
!
! Examples:
!
!  X_SEARCH = 0.5 * X_INC :  No traces beyond the edge of the 
!                            bin (in the X direction) will be searched.
!
!  X_SEARCH = X_INC       :  The search distance will extend to the center
!                            of the neighboring bins.
!
!  X_SEARCH = 1.5 * X_INC :  The search distance will extend to
!                            the far edges of the neighboring bins.
!
!  X_SEARCH < 0.5 * X_INC :  Only the center part of the 
!                            bin will be searched.
!</Help>
!
!
!<Help KEYWORD="Y_SEARCH">
!<Tip> Search distance in Y direction for flex search. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!
! This is the distance from the center of a bin (in the Y direction)
! to search for candidates to be reassigned to the bin.
!
! Examples:
!
!  Y_SEARCH = 0.5 * Y_INC :  No traces beyond the edge of the 
!                            bin (in the Y direction) will be searched.
!
!  Y_SEARCH = Y_INC       :  The search distance will extend to the center
!                            of the neighboring bins.
!
!  Y_SEARCH = 1.5 * Y_INC :  The search distance will extend to
!                            the far edges of the neighboring bins.
!
!  Y_SEARCH < 0.5 * Y_INC :  Only the center part of the 
!                            bin will be searched.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_X_NEW">
!<Tip> Header word containing the X coordinate of new bin center. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! This should be a user-defined header word.
! The new bin center X coordinate is not saved if this number is zero.
!</Help>
!
!
!<Help KEYWORD="HDR_Y_NEW">
!<Tip> Header word containing the Y coordinate of new bin center. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! This should be a user-defined header word.
! The new bin center Y coordinate is not saved if this number is zero.
!</Help>
!
!
!<Help KEYWORD="X_INIT">
!<Tip> X coordinate of the first output bin center in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of the first output bin measured in the X
! direction.
!</Help>
!
!
!<Help KEYWORD="Y_INIT">
!<Tip> Y coordinate of the first output bin center in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of the first output bin measured in the Y
! direction.
!</Help>
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X between output bin centers in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the output bin increment (or width) in the X direction.
!</Help>
!
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y between output bin centers in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the output bin increment (or width) in the Y direction.
!</Help>
!
!
!<Help KEYWORD="X_LAST">
!<Tip> X coordinate of the last output bin center in the X direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of the last output bin measured in the X
! direction.
!</Help>
!
!
!<Help KEYWORD="Y_LAST">
!<Tip> Y coordinate of the last output bin center in the Y direction. </Tip>
! Default = 1.0
! Allowed = real
!
! This value must be the center of the last output bin measured in the Y
! direction.
!</Help>
!
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of output bins in the X direction. </Tip>
! Default = 1
! Allowed = real
!
! This value must be the total number of output bins counting in the X
! direction.
!</Help>
!
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of output bins in the Y direction. </Tip>
! Default = 1
! Allowed = real
!
! This value must be the total number of output bins counting in the Y
! direction.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module regbin_module
      use pc_module
      use named_constants_module
      use rollingbins_module
      use temptfile_module
  !   use temptstore_module
      use mth_module
      implicit none
      private
      public :: regbin_create
      public :: regbin_initialize
      public :: regbin_update
      public :: regbin_delete
      public :: regbin
      public :: regbin_wrapup


      character(len=100),public,save :: REGBIN_IDENT = &
'$Id: regbin.f90,v 1.5 2006/06/12 13:03:55 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: regbin_struct

        private
        logical             :: skip_wrapup               ! wrapup flag
        integer             :: nwih,ndpt                 ! globals

        integer             :: hdr_x                     ! process parameters
        integer             :: hdr_y                     ! process parameters
        real                :: x_init                    ! process parameters
        real                :: y_init                    ! process parameters
        real                :: x_inc                     ! process parameters
        real                :: y_inc                     ! process parameters
        real                :: x_last                    ! process parameters
        real                :: y_last                    ! process parameters
        integer             :: x_tot                     ! process parameters
        integer             :: y_tot                     ! process parameters
        real                :: x_search                  ! process parameters
        real                :: y_search                  ! process parameters
        integer             :: hdr_x_new                 ! process parameters
        integer             :: hdr_y_new                 ! process parameters
        integer             :: max_fold                  ! process parameters

        integer                          :: nx             ! dependent
        integer                          :: ny             ! dependent
        real                             :: xlook          ! dependent
        real                             :: ylook          ! dependent
        integer                          :: ixlook         ! dependent
        integer                          :: iylook         ! dependent
        integer                          :: ixmin          ! dependent
        integer                          :: iymin          ! dependent
        integer                          :: ixmax          ! dependent
        integer                          :: iymax          ! dependent
        integer                          :: ixnext         ! dependent
        integer                          :: iynext         ! dependent
        integer                          :: iylast         ! dependent
        logical                          :: input_done     ! dependent
        logical                          :: output_done    ! dependent
        integer                          :: kount_input    ! dependent
        integer                          :: kount_range    ! dependent
        integer                          :: kount_late     ! dependent
        integer                          :: kount_dead     ! dependent
        integer                          :: kount_fold     ! dependent
        integer                          :: kount_output   ! dependent
        type(rollingbins_struct),pointer :: rollingbins    ! dependent
        type(temptfile_struct)  ,pointer :: temptfile      ! dependent
    !   type(temptstore_struct) ,pointer :: temptstore     ! dependent

      end type regbin_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                    ,save :: lunprint  ! unit number for printing.
      type(regbin_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine regbin_create (obj)

      type(regbin_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()

      allocate (obj)

      nullify (obj%rollingbins)
      nullify (obj%temptfile)
  !   nullify (obj%temptstore)

      call regbin_initialize (obj)

      end subroutine regbin_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine regbin_delete (obj)

      type(regbin_struct),pointer :: obj       ! arguments

      call regbin_wrapup (obj)
      deallocate(obj)

      end subroutine regbin_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine regbin_initialize (obj)

      type(regbin_struct),intent(inout) :: obj       ! arguments

      obj%hdr_x          = 7
      obj%hdr_y          = 8
      obj%x_init         = 1.0
      obj%y_init         = 1.0
      obj%x_inc          = 1.0
      obj%y_inc          = 1.0
      obj%x_last         = 1.0
      obj%y_last         = 1.0
      obj%x_tot          = 1
      obj%y_tot          = 1
      obj%x_search       = 1.0
      obj%y_search       = 1.0
      obj%hdr_x_new      = 0
      obj%hdr_y_new      = 0
      obj%max_fold       = 1

      call regbin_update (obj)

      end subroutine regbin_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine regbin_update (obj)

      type(regbin_struct),intent(inout),target :: obj              ! arguments
      integer                                  :: err,numtr,nyroll ! local
      integer                                  :: nstore,nscratch  ! local
      real                                     :: x_last_keep      ! local
      real                                     :: y_last_keep      ! local
      integer                                  :: x_tot_keep       ! local
      integer                                  :: y_tot_keep       ! local
      integer                                  :: maxrecords       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      x_last_keep = obj%x_last
      y_last_keep = obj%y_last
      x_tot_keep  = obj%x_tot
      y_tot_keep  = obj%y_tot

      call pc_get_global ('numtr'             , numtr)
      call pc_get_global ('nwih'              , obj%nwih)
      call pc_get_global ('ndpt'              , obj%ndpt)

      call pc_get        ('hdr_x'             , obj%hdr_x             )
      call pc_get        ('hdr_y'             , obj%hdr_y             )
      call pc_get        ('x_init'            , obj%x_init            )
      call pc_get        ('y_init'            , obj%y_init            )
      call pc_get        ('x_inc'             , obj%x_inc             )
      call pc_get        ('y_inc'             , obj%y_inc             )
      call pc_get        ('x_last'            , obj%x_last            )
      call pc_get        ('y_last'            , obj%y_last            )
      call pc_get        ('x_tot'             , obj%x_tot             )
      call pc_get        ('y_tot'             , obj%y_tot             )
      call pc_get        ('x_search'          , obj%x_search          )
      call pc_get        ('y_search'          , obj%y_search          )
      call pc_get        ('hdr_x_new'         , obj%hdr_x_new         )
      call pc_get        ('hdr_y_new'         , obj%hdr_y_new         )
      call pc_get        ('max_fold'          , obj%max_fold          )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call mth_fix_pattern (obj%x_init, obj%x_inc, obj%x_last,  &
                            obj%x_tot, x_last_keep, x_tot_keep)

      call mth_fix_pattern (obj%y_init, obj%y_inc, obj%y_last,  &
                            obj%y_tot, y_last_keep, y_tot_keep)

      if (numtr > 1) then
           call pc_error ('REGBIN can accept only one trace at a time.')
           call pc_error ('Please insert an UNGATHER process before REGBIN.')
      endif

      if (obj%hdr_x <= 0 .or. obj%hdr_x > obj%nwih) then
           call pc_warning ('Bad HDR_X value - reset to 7')
           obj%hdr_x = 7
      endif

      if (obj%hdr_y <= 0 .or. obj%hdr_y > obj%nwih) then
           call pc_warning ('Bad HDR_Y value - reset to 8')
           obj%hdr_y = 8
      endif

      if (obj%hdr_x_new < 0 .or. obj%hdr_x_new > obj%nwih) then
           call pc_warning ('Bad HDR_X_NEW value - reset to 0')
           obj%hdr_x_new = 0
      endif

      if (obj%hdr_y_new < 0 .or. obj%hdr_y_new > obj%nwih) then
           call pc_warning ('Bad HDR_Y_NEW value - reset to 0')
           obj%hdr_y_new = 0
      endif

      if (obj%x_search <= 0.0) then
           call pc_warning ('X SEARCH DISTANCE MUST BE > ZERO - reset to 1.0')
           obj%x_search = 1.0
      endif

      if (obj%y_search <= 0.0) then
           call pc_warning ('Y SEARCH DISTANCE MUST BE > ZERO - reset to 1.0')
           obj%y_search = 1.0
      endif

      if (obj%max_fold <= 0) then
           call pc_warning ('MAX_FOLD must be > zero - reset to 1')
           obj%max_fold = 1
      endif

      obj%xlook = obj%x_search / obj%x_inc    ! distance to search (bin units).
      obj%ylook = obj%y_search / obj%y_inc    ! distance to search (bin units).

      obj%ixlook = max(ceiling(obj%xlook - 0.5),0)    ! #extra bins to search.
      obj%iylook = max(ceiling(obj%ylook - 0.5),0)    ! #extra bins to search.

      obj%nx = 2 * obj%ixlook + 1
      obj%ny = 2 * obj%iylook + 1
      nyroll = 2 * obj%iylook + 3    ! make sure is at least large enough.

      maxrecords = obj%max_fold * obj%x_tot * nyroll


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put        ('hdr_x'             , obj%hdr_x             )
      call pc_put        ('hdr_y'             , obj%hdr_y             )
      call pc_put        ('x_init'            , obj%x_init            )
      call pc_put        ('y_init'            , obj%y_init            )
      call pc_put        ('x_inc'             , obj%x_inc             )
      call pc_put        ('y_inc'             , obj%y_inc             )
      call pc_put        ('x_last'            , obj%x_last            )
      call pc_put        ('y_last'            , obj%y_last            )
      call pc_put        ('x_tot'             , obj%x_tot             )
      call pc_put        ('y_tot'             , obj%y_tot             )
      call pc_put        ('x_search'          , obj%x_search          )
      call pc_put        ('y_search'          , obj%y_search          )
      call pc_put        ('hdr_x_new'         , obj%hdr_x_new         )
      call pc_put        ('hdr_y_new'         , obj%hdr_y_new         )
      call pc_put        ('max_fold'          , obj%max_fold          )

      nstore   = 2 * maxrecords                      ! in rollingbins.
      nscratch = obj%max_fold * obj%nx * obj%ny      ! automatic array.

      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .false.)
      call pc_put_control ('nstore'      , nstore)
      call pc_put_control ('nscratch'    , nscratch)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call rollingbins_delete (obj%rollingbins)
      call temptfile_close    (obj%temptfile)
  !   call temptstore_close   (obj%temptstore)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call pc_print (' ')
      call pc_print ('maxrecords =', maxrecords)
      call pc_print ('xlook      =', obj%xlook)
      call pc_print ('ylook      =', obj%ylook)
      call pc_print ('ixlook     =', obj%ixlook)
      call pc_print ('iylook     =', obj%iylook)
      call pc_print ('nyroll     =', nyroll)
      call pc_print (' ')

      call rollingbins_create &
                      (obj%rollingbins, obj%max_fold, obj%x_tot, nyroll)

  !   call temptstore_open (obj%temptstore, 'REGBIN', obj%nwih, obj%ndpt, &
      call temptfile_open  (obj%temptfile, 'REGBIN', obj%nwih, obj%ndpt,  &
                            lunprint, err, maxrecords=maxrecords,         &
                            vartypes='DR')

      if (err /= TEMPTFILE_OK) then
           call pc_error ('error opening temporary trace file')
           call temptfile_close (obj%temptfile)
  !        call temptstore_close (obj%temptstore)
      endif

      obj%ixmin        = 0        ! smallest X bin number that was input.
      obj%iymin        = 0        ! smallest Y bin number that was input.
      obj%ixmax        = 0        ! largest X bin number that was input.
      obj%iymax        = 0        ! largest Y bin number that was input.
      obj%ixnext       = 1        ! next X bin number to output.
      obj%iynext       = 1        ! next Y bin number to output.
      obj%iylast       = 0        ! last Y bin number successfully input.
      obj%input_done   = .false.  ! true if NO_MORE_TRACES was received.
      obj%output_done  = .false.  ! true if last bin was output or skipped.
      obj%kount_input  = 0        ! number of input traces.
      obj%kount_range  = 0        ! number of input traces out of range.
      obj%kount_late   = 0        ! number of input traces received too late.
      obj%kount_dead   = 0        ! number of dead input traces.
      obj%kount_fold   = 0        ! number of input traces past maximum fold.
      obj%kount_output = 0        ! number of output traces.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine regbin_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine regbin (obj,ntr,hd,tr)

      type(regbin_struct),intent(inout) :: obj                 ! arguments
      integer            ,intent(inout) :: ntr                 ! arguments
      double precision   ,intent(inout) :: hd(:,:)             ! arguments
      real               ,intent(inout) :: tr(:,:)             ! arguments

!---------------input:

   ! REGBIN_INPUT stores the input trace.
   ! NTR is set to NEED_TRACES if we did not save trace and need another one.
   ! NTR is set to     one     if we saved the trace to the cache.
   ! NTR is set to FATAL_ERROR if an error occurs.

      if (ntr == 1) then

           if (obj%output_done) then
                ntr = NEED_TRACES
                return
           endif

           call regbin_input (obj,hd(:,1),tr(:,1),ntr)

           if (ntr == FATAL_ERROR) then
                call regbin_wrapup (obj)
                return
           endif

           ! now ntr is one or NEED_TRACES.

      elseif (ntr == NO_MORE_TRACES) then

           obj%input_done = .true.

      endif

!---------------output:

   ! REGBIN_OUTPUT returns an output trace for the next bin.
   ! NTR is set to NEED_TRACES if cannot find a trace to output for the bin.
   ! NTR is set to     one     if a trace is being output for the bin.
   ! NTR is set to FATAL_ERROR if an error occurs.

      do

           if (obj%output_done) then
                if (obj%input_done) then
                     call regbin_wrapup (obj)
                     ntr = NO_MORE_TRACES
                     return
                else
                     ntr = NEED_TRACES
                     return
                endif
           endif

           if (.not. obj%input_done) then
                if (obj%iynext + obj%iylook >= obj%iylast) then
                     ntr = NEED_TRACES
                     return
                endif
           endif

           call regbin_output (obj,hd(:,1),tr(:,1),ntr)

           if (ntr == FATAL_ERROR) then
                call regbin_wrapup (obj)
                return
           endif

           if (obj%ixnext < obj%x_tot) then        ! step to next bin.
                obj%ixnext = obj%ixnext + 1        ! step to next bin.
           elseif (obj%iynext == obj%y_tot) then   ! step to next bin.
                obj%output_done = .true.           ! step to next bin.
           else                                    ! step to next bin.
                obj%iynext = obj%iynext + 1        ! step to next bin.
                obj%ixnext = 1                     ! step to next bin.
           endif

           if (ntr == 1) return

           ! now ntr is NEED_TRACES.

      enddo

      end subroutine regbin


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine regbin_wrapup (obj)

      type(regbin_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print (' ')
      call pc_print ('REGBIN: WRAPUP')
      call pc_print ('REGBIN: number of input traces                   =', &
                                 obj%kount_input)
      call pc_print ('REGBIN: number of input traces out of range      =', &
                                 obj%kount_range)
      call pc_print ('REGBIN: number of input traces received too late =', &
                                 obj%kount_late)
      call pc_print ('REGBIN: number of dead input traces              =', &
                                 obj%kount_dead)
      call pc_print ('REGBIN: number of input traces past maximum fold =', &
                                 obj%kount_fold)
      call pc_print ('REGBIN: number of output traces                  =', &
                                 obj%kount_output)
      call pc_print ('REGBIN: input X bin range =',obj%ixmin,'thru',obj%ixmax)
      call pc_print ('REGBIN: input Y bin range =',obj%iymin,'thru',obj%iymax)
      call pc_print ('REGBIN: requested X bin range = 1 thru',obj%x_tot)
      call pc_print ('REGBIN: requested Y bin range = 1 thru',obj%y_tot)

      call rollingbins_delete (obj%rollingbins)
      call temptfile_close    (obj%temptfile)
  !   call temptstore_close   (obj%temptstore)

      call pc_print ('REGBIN: FINISHED')
      call pc_print (' ')

      end subroutine regbin_wrapup


!!------------------------------- input -----------------------------------!!
!!------------------------------- input -----------------------------------!!
!!------------------------------- input -----------------------------------!!

! This routine stores the input trace.
! NTR is set to NEED_TRACES if we did not save the trace and need another one.
! NTR is set to     one     if we saved the trace to the cache.
! NTR is set to FATAL_ERROR if an error occurs.


      subroutine regbin_input (obj,hd,tr,ntr)

      type(regbin_struct),intent(inout) :: obj                   ! arguments
      double precision   ,intent(in)    :: hd(:)                 ! arguments
      real               ,intent(in)    :: tr(:)                 ! arguments
      integer            ,intent(out)   :: ntr                   ! arguments
      real                              :: xbin,ybin             ! local
      integer                           :: ixbin,iybin           ! local
      integer                           :: err,irec,ifold        ! local

!----------get bin coordinates and bin numbers for this trace.

      xbin  = 1.0 + (hd(obj%hdr_x) - obj%x_init) / obj%x_inc
      ybin  = 1.0 + (hd(obj%hdr_y) - obj%y_init) / obj%y_inc
      ixbin = nint(xbin)
      iybin = nint(ybin)

!----------adjust iybin if just past edge of previous iybin:

      ! This is to keep from treating the traces following this trace
      ! as being received too late simply because this trace falls at
      ! the bin boundary and might have previously been considered to
      ! fall in the lower bin when the traces were sorted.

      if (obj%kount_input > 0) then
           if (iybin > obj%iylast) then
                if (ybin - obj%iylast < 0.51) then
                     iybin = obj%iylast
                endif
           endif
      endif

!----------update counters and ranges:

      obj%kount_input = obj%kount_input + 1

      if (obj%kount_input == 1) then
           obj%ixmin = ixbin
           obj%ixmax = ixbin
           obj%iymin = iybin
           obj%iymax = iybin
      else
           obj%ixmin = min(obj%ixmin,ixbin)
           obj%ixmax = max(obj%ixmax,ixbin)
           obj%iymin = min(obj%iymin,iybin)
           obj%iymax = max(obj%iymax,iybin)
      endif

!----------immediately skip trace if it is out of range:

      if (ixbin < 1 .or. ixbin > obj%x_tot .or. &
          iybin < 1 .or. iybin > obj%y_tot) then
           obj%kount_range = obj%kount_range + 1
           ntr = NEED_TRACES
           return
      endif

!----------immediately skip trace if it is received too late (out of order):
!                (enforced only for the Y bin)

      if (iybin < obj%iylast) then
           obj%kount_late = obj%kount_late + 1
           ntr = NEED_TRACES
           return
      endif

!----------immediately skip trace if it is dead:

      if (hd(25) == 0.0) then
           obj%kount_dead = obj%kount_dead + 1
           ntr = NEED_TRACES
           return
      endif

!----------save bin coordinates for this trace:

      ifold = 0
      call rollingbins_put (obj%rollingbins,ifold,ixbin,iybin,xbin,ybin,irec)

!----------immediately skip trace if the specified fold is too small:

      if (ifold == 0) then
           obj%kount_fold = obj%kount_fold + 1
           ntr = NEED_TRACES
           return
      endif

!----------write the trace to disk:

  !   call temptstore_write (obj%temptstore,irec,hd,tr,err)
      call temptfile_write (obj%temptfile,irec,hd,tr,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ("REGBIN: Error writing temp file at record",irec)
           ntr = FATAL_ERROR
           return
      endif

!----------finish up and return:

      obj%iylast = iybin                 ! last Y bin number saved to cache.

      end subroutine regbin_input


!!----------------------------- output -----------------------------------!!
!!----------------------------- output -----------------------------------!!
!!----------------------------- output -----------------------------------!!

! This routine returns an output trace for the next bin.
! NTR is set to NEED_TRACES if cannot find a trace to output for the bin.
! NTR is set to     one     if a trace is being output for the bin.
! NTR is set to FATAL_ERROR if an error occurs.


      subroutine regbin_output (obj,hd,tr,ntr)

      type(regbin_struct),intent(inout) :: obj                    ! arguments
      double precision   ,intent(out)   :: hd(:)                  ! arguments
      real               ,intent(out)   :: tr(:)                  ! arguments
      integer            ,intent(out)   :: ntr                    ! arguments
      integer                           :: err,irec,ix,iy    ! local
      integer                           :: ixbin,iybin,ifold      ! local
      real                              :: xdistance,ydistance    ! local
      real                              :: xcenter,ycenter        ! local
      real                              :: xbin,ybin              ! local
      real                              :: distance               ! local
      integer                           :: ifold_closest          ! local
      integer                           :: ixbin_closest          ! local
      integer                           :: iybin_closest          ! local
      real                              :: xdistance_closest      ! local
      real                              :: ydistance_closest      ! local
      real                              :: distance_closest       ! local
      integer                           :: irec_closest           ! local
      integer                           :: ixstart,iystart        ! local
      integer                           :: ixstop,iystop          ! local
      logical               :: skip(obj%max_fold,obj%nx,obj%ny)   ! local

!----------get started:

      ixstart = obj%ixnext - obj%ixlook
      ixstop  = obj%ixnext + obj%ixlook
      iystart = obj%iynext - obj%iylook
      iystop  = obj%iynext + obj%iylook

!----------skip all traces in neighboring bins closest to their own bin center.

      skip(:,:,:) = .false.

      do iybin = iystart,iystop
      do ixbin = ixstart,ixstop

        if (iybin <  obj%iynext) cycle
        if (iybin == obj%iynext .and. ixbin <= obj%ixnext) cycle

        ! now we are only checking future bins past the current bin.

        ix = ixbin - ixstart + 1
        iy = iybin - iystart + 1

        ifold_closest    = INIL
        distance_closest = FNIL

        do ifold = 1,obj%max_fold

           call rollingbins_get (obj%rollingbins,ifold,ixbin,iybin,xbin,ybin)

           if (xbin == FNIL) cycle

           xdistance = xbin - ixbin
           ydistance = ybin - iybin
           distance  = xdistance**2 + ydistance**2

           if (ifold_closest == INIL .or. distance < distance_closest) then
                ifold_closest    = ifold
                distance_closest = distance
           endif

        enddo

        if (ifold_closest /= INIL) skip(ifold_closest,ix,iy) = .true.

      enddo
      enddo

!----------find closest trace to bin center:

      ifold_closest     = INIL
      ixbin_closest     = INIL
      iybin_closest     = INIL
      xdistance_closest = FNIL
      ydistance_closest = FNIL
      distance_closest  = FNIL
      irec_closest      = INIL

      do iybin = iystart,iystop
      do ixbin = ixstart,ixstop

        ix = ixbin - ixstart + 1
        iy = iybin - iystart + 1

        do ifold = 1,obj%max_fold

           if (skip(ifold,ix,iy)) cycle   ! skip centerest trace in future bin.

           call rollingbins_get &
                      (obj%rollingbins,ifold,ixbin,iybin,xbin,ybin,irec)

           if (xbin == FNIL) cycle

           xdistance = xbin - obj%ixnext
           ydistance = ybin - obj%iynext
           distance  = xdistance**2 + ydistance**2

           if (ifold_closest == INIL .or. distance < distance_closest) then
                ifold_closest     = ifold
                ixbin_closest     = ixbin
                iybin_closest     = iybin
                xdistance_closest = xdistance
                ydistance_closest = ydistance
                distance_closest  = distance
                irec_closest      = irec    
           endif

        enddo

      enddo
      enddo

      ifold     = ifold_closest
      ixbin     = ixbin_closest
      iybin     = iybin_closest
      xdistance = xdistance_closest
      ydistance = ydistance_closest
      irec      = irec_closest

!----------return if this trace is not close enough:

      if (ifold == INIL) then
           ntr = NEED_TRACES
           return
      endif

      if (abs(xdistance) > obj%xlook .or. abs(ydistance) > obj%ylook) then
           ntr = NEED_TRACES
           return
      endif

!----------read the trace from disk:

  !   call temptstore_read (obj%temptstore,irec,hd,tr,err)
      call temptfile_read (obj%temptfile,irec,hd,tr,err)
      if (err /= TEMPTFILE_OK) then
           call pc_error ("REGBIN: Error reading temp file at record",irec)
           ntr = FATAL_ERROR
           return
      endif

!----------update the trace headers:

      obj%kount_output = obj%kount_output + 1
      hd(1)            = obj%kount_output

      xcenter = mth_bin_center (obj%x_init, obj%x_inc, obj%ixnext)
      ycenter = mth_bin_center (obj%y_init, obj%y_inc, obj%iynext)

      if (obj%hdr_x_new > 0) hd(obj%hdr_x_new) = xcenter
      if (obj%hdr_y_new > 0) hd(obj%hdr_y_new) = ycenter

!----------mark the trace as used and return:

      call rollingbins_put (obj%rollingbins, ifold_closest, &
                            ixbin_closest,iybin_closest,FNIL,FNIL)
      ntr = 1

      end subroutine regbin_output


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module regbin_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

