!<CPS_v1 type="PROCESS"/>
!!------------------------------- fill.f90 ---------------------------------!!
!!------------------------------- fill.f90 ---------------------------------!!
!!------------------------------- fill.f90 ---------------------------------!!

 
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
! Name       : FILL      (Fill in missing traces)
! Category   : miscellaneous
! Written    : 2001-03-20   by: Tom Stoeckley
! Revised    : 2002-04-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Place a dead trace in any bin not occupied by a live trace.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! FILL places a dead trace in any specified bin that is not occupied by at 
! least one input trace, and removes bins which are outside of the specified
! range.  FILL expects input data sort order to be consistent with the fill
! bin specifications.  Usually, this means that the parameters should have
! the same values as the corresponding parameters in a preceding TSORT
! operation (or the TSORT parameters which could have been specified to put
! the traces into the order in which they are received by this FILL operation).
! An exception is that bins with fill option NO do not have to be in any
! particular order.
! 
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
! To fill out bins within 1D sorted data:
! Specify the primary fill parameters and set HDR_SEC = 1 and HDR_TERT = 1.
! Input data must be sorted in order of HDR_PRI.
! Primary bin filling will take place only if OPT_PRI is YES.
! Nothing will happen if OPT_PRI is NO.
!
! To fill out bins within 2D sorted data:
! Specify the primary and secondary fill parameters and set HDR_TERT = 1.
! Input data must be sorted in order of HDR_PRI and then HDR_SEC.
! Primary   bin filling will take place only if OPT_PRI is YES.
! Secondary bin filling will take place only if OPT_SEC is YES.
! Nothing will happen if both OPT_PRI and OPT_SEC are NO.
!
! To fill out bins within 3D sorted data:
! Specify the primary, secondary and tertiary fill parameters.
! Input data must be sorted in order of HDR_PRI, then HDR_SEC and then HDR_TERT.
! Primary   bin filling will take place only if OPT_PRI  is YES.
! Secondary bin filling will take place only if OPT_SEC  is YES.
! Tertiary  bin filling will take place only if OPT_TERT is YES.
! Nothing will happen if OPT_PRI, OPT_SEC, and OPT_TERT are all NO.
!
!-------------------------------------------------------------------------------
!                  POSSIBLE COMBINATIONS OF OPTIONS
!
! There are 8 possible combinations of the three fill options.
! Here are the possibilities:
!
!             OPT_PRI   OPT_SEC   OPT_TERT 
!             -------   -------   -------- 
!               YES       YES       YES     
!               YES       YES       NO      
!               YES       NO        YES
!               YES       NO        NO      
!
!               NO        YES       YES
!               NO        YES       NO
!               NO        NO        YES
!               NO        NO        NO      <-- does nothing useful
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! Process requires traces to be input with sort order consistent with the fill 
! bin specifications.
!
! Traces may be input as single traces or in gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
! This process does not alter input traces.
!
! This process outputs the same traces as it receives, in the same order,
! with these exceptions:
!
!  (1) All traces outside of the specified bin ranges are deleted.
!  (2) A single dead fill trace is added to any empty bin.
!  (3) Any improperly sorted input traces are DELETED WITH A WARNING MESSAGE.
!
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
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
! For input traces which are retained:
!
!    Hwd#       Description                   Action taken
!    ----       -----------                   ------------
!    1          sequential trace count        Renumbered (starting with 1).
!    3          current gather                Renumbered (starting with 1).
!    4          sequential within gather      Renumbered (starting with 1).
!    HDR_PRI    primary fill header word      Used but not changed.
!    HDR_SEC    secondary fill header word    Used but not changed.
!    HDR_TERT   tertiary fill header word     Used but not changed.
!
! For dead fill traces created by this process:
!
!    Hwd#       Description                   Action taken
!    ----       -----------                   ------------
!    1          sequential trace count        Numbered (starting with 1).
!    3          current gather                Numbered (starting with 1).
!    4          sequential within gather      Numbered (starting with 1).
!    2          top mute                      Set to one.
!    64         bottom mute                   Set to NDPT.
!    25         largest absolute value        Set to zero.
!    HDR_PRI    primary fill header word      Set to the appropriate value.
!    HDR_SEC    secondary fill header word    Set to the appropriate value.
!    HDR_TERT   tertiary fill header word     Set to the appropriate value.
!    ------     all other header words        Set to zero.
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
! Please note that if HDR_PRI or HDR_SEC or HDR_TERT is set to 3 or 4, the
! input values of those header words will be used as for any other header
! word, but the output values will change.
!
! Please also note that if HDR_SEC or HDR_TERT is set to 1, the input values
! of those header words will not be used, and the output values will change.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2002-04-29  Stoeckley  Move one error check timing to when user hits OK.
!  3. 2001-06-04  Stoeckley  Fix bugs with options FTT and FTF.  This might
!                             not be the last of the bugs, but all my tests
!                             with random numbers work for all options.
!  2. 2001-03-30  Stoeckley  Fix bug regarding setting output trace header 3.
!                             Some other bugs remain, which will take awhile
!                             to sort out with improved logic, but this
!                             revision is being turned in as Max requested.
!  1. 2001-03-20  Stoeckley  Initial version, replacing the FILL option in
!                             the TSORT process.
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
!                      SPECIAL COMPILING REQUIREMENTS             
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
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
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
!                    ALTERNATE INTERNAL CALLING METHODS          
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS FILL Process/NC=80>
!
!             Fill out missing traces by header word values.
!
!     HDR_PRI= `IIIII     PRI_INIT= `FFFFFFFFFF   PRI_INC= `FFFFFFFFFF
!     OPT_PRI= `CCCCCCC   PRI_LAST= `FFFFFFFFFF   PRI_TOT= `IIIIIIII
!
!     HDR_SEC= `IIIII     SEC_INIT= `FFFFFFFFFF   SEC_INC= `FFFFFFFFFF
!     OPT_SEC= `CCCCCCC   SEC_LAST= `FFFFFFFFFF   SEC_TOT= `IIIIIIII
!
!     HDR_TERT=`IIIII     TERT_INIT=`FFFFFFFFFF   TERT_INC=`FFFFFFFFFF
!     OPT_TERT=`CCCCCCC   TERT_LAST=`FFFFFFFFFF   TERT_TOT=`IIIIIIII
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="HDR_PRI">
!<Tip> Header word designating primary sort bins. </Tip>
! Default = 8
! Allowed = 2 - NWIH
!
! HDR_PRI is the least rapidly changing sort header for input traces. 
! If OPT_PRI == NO, the fill operation will not be performed on primary bins.
!</Help>
!
!
!<Help KEYWORD="HDR_SEC">
!<Tip> Header word designating secondary sort bins. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! HDR_SEC has intermediate rapidity of change for input traces.  
! If OPT_SEC == NO, the fill operation will not be performed on secondary bins.
! If HDR_SEC = 1, then OPT_SEC is forced to NO.
!</Help>
!
!
!<Help KEYWORD="HDR_TERT">
!<Tip> Header word designating tertiary sort bins. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!
! HDR_TERT is the most rapidly changing sort header for input traces.  
! If OPT_TERT == NO, the fill operation will not be performed on tertiary bins.
! If HDR_TERT = 1, then OPT_TERT is forced to NO.
! HDR_TERT must be 1 if HDR_SEC is 1.
!</Help>
!
!
!<Help KEYWORD="OPT_PRI">
!<Tip> Fill option for primary sort bins. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If OPT_PRI == YES:
!  Any missing primary bins between PRI_INIT and PRI_LAST are inserted.
!  Any primary bins before PRI_INIT and after PRI_LAST are deleted.
!
! If OPT_PRI == NO:
!  No missing primary bins are inserted and no primary bins are deleted.
!  Parameters PRI_LAST and PRI_TOT are ignored.
!
! In either case:
!  The contents of each retained or newly created primary bin are filled
!   in according to the parameters specified for secondary bins.
!  If OPT_SEC == YES, parameters PRI_INIT and PRI_INC are always required
!   to define the primary bins so that the secondary bin parameters will
!   be properly applied to the contents of each primary bin.
!</Help>
!
!
!<Help KEYWORD="OPT_SEC">
!<Tip> Fill option for secondary sort bins. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If OPT_SEC == YES:
!  Within each retained or newly created primary sort bin:
!   Any missing secondary bins between SEC_INIT and SEC_LAST are inserted.
!   Any secondary bins before SEC_INIT and after SEC_LAST are deleted.
!
! If OPT_SEC == NO:
!  Within each retained primary sort bin:
!   No missing secondary bins are inserted and no secondary bins are deleted.
!   Parameters SEC_LAST and SEC_TOT are ignored.
!  Within each newly created primary sort bin:
!   A single secondary bin is inserted with HDR_SEC set to SEC_INIT.
!   Or if HDR_SEC == 1, a single trace is inserted.
!
! In either case:
!  The contents of each retained or newly created secondary bin are filled
!   in according to the parameters specified for tertiary bins.
!  If OPT_TERT == YES, parameters SEC_INIT and SEC_INC are always required
!   to define the secondary bins so that the tertiary bin parameters will
!   be properly applied to the contents of each secondary bin.
!
! If HDR_SEC == 1:
!  OPT_SEC is forced to NO.
!  Parameters SEC_INIT, SEC_INC, SEC_LAST, and SEC_TOT are ignored.
!  All tertiary bin parameters are ignored.
!</Help>
!
!
!<Help KEYWORD="OPT_TERT">
!<Tip> Fill option for tertiary sort bins. </Tip>
! Default = YES
! Allowed = YES or NO
!
! If OPT_TERT == YES:
!  Within each retained or newly created secondary sort bin:
!   Any missing tertiary bins between TERT_INIT and TERT_LAST are inserted.
!   Any tertiary bins before TERT_INIT and after TERT_LAST are deleted.
!
! If OPT_TERT == NO:
!  Within each retained secondary sort bin:
!   No missing tertiary bins are inserted and no tertiary bins are deleted.
!   Parameters TERT_INIT, TERT_INC, TERT_LAST, and TERT_TOT are ignored.
!  Within each newly created secondary sort bin:
!   A single trace is inserted with HDR_TERT set to TERT_INIT.
!   Or if HDR_TERT == 1, a single trace is inserted.
!
! If HDR_TERT == 1:
!  OPT_TERT is forced to NO.
!  Parameters TERT_INIT, TERT_INC, TERT_LAST, and TERT_TOT are ignored.
!</Help>
!
!
!<Help KEYWORD="PRI_INIT">
!<Tip> Initial value for primary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! PRI_INIT is the value of HDR_PRI at the center of the FIRST primary bin. 
! PRI_INIT can be the center of ANY primary bin if OPT_PRI == NO.
!</Help>
!
!
!<Help KEYWORD="SEC_INIT">
!<Tip> Initial value for secondary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! SEC_INIT is the value of HDR_SEC at the center of the FIRST secondary bin. 
! SEC_INIT can be the center of ANY secondary bin if OPT_SEC == NO.
! SEC_INIT is ignored if HDR_SEC == 1.
!</Help>
!
!
!<Help KEYWORD="TERT_INIT">
!<Tip> Initial value for tertiary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! TERT_INIT is the value of HDR_TERT at the center of the FIRST tertiary bin.
! TERT_INIT can be the center of ANY tertiary bin if OPT_TERT == NO.
! TERT_INIT is ignored if HDR_TERT == 1.
!</Help>
!
!
!<Help KEYWORD="PRI_INC">
!<Tip> Increment for primary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! PRI_INC is the increment between, and the width of, primary sort bins.
! PRI_INC should be < 0 if traces are input in descending order of HDR_PRI.
!</Help>
!
!
!<Help KEYWORD="SEC_INC">
!<Tip> Increment for secondary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! SEC_INC is the increment between, and the width of, secondary sort bins.
! SEC_INC should be < 0 if traces are input in descending order of HDR_SEC.
! SEC_INC is ignored if HDR_SEC == 1.
!</Help>
!
!
!<Help KEYWORD="TERT_INC">
!<Tip> Increment for tertiary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! TERT_INC is the increment between, and the width of, tertiary sort bins.
! TERT_INC should be < 0 if traces are input in descending order of HDR_TERT.
! TERT_INC is ignored if HDR_TERT == 1.
!</Help>
!
!
!<Help KEYWORD="PRI_LAST">
!<Tip> Last value for primary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! PRI_LAST is the value of HDR_PRI at the center of the LAST primary bin.
! PRI_LAST is ignored if OPT_PRI == NO.
!</Help>
!
!
!<Help KEYWORD="SEC_LAST">
!<Tip> Last value for secondary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! SEC_LAST is the value of HDR_SEC at the center of the LAST secondary bin.
! SEC_LAST is ignored if OPT_SEC == NO.
! SEC_LAST is ignored if HDR_SEC == 1.
!</Help>
!
!
!<Help KEYWORD="TERT_LAST">
!<Tip> Last value for tertiary header word. </Tip>
! Default = 1.0
! Allowed = real
!
! TERT_LAST is the value of HDR_TERT at the center of the LAST tertiary bin.
! TERT_LAST is ignored if OPT_TERT == NO.
! TERT_LAST is ignored if HDR_TERT == 1.
!</Help>
!
!
!<Help KEYWORD="PRI_TOT">
!<Tip> Total number of primary sort bins. </Tip>
! Default = 1
! Allowed = int
!
! PRI_TOT is ignored if OPT_PRI == NO.
!</Help>
!
!
!<Help KEYWORD="SEC_TOT">
!<Tip> Total number of secondary sort bins. </Tip>
! Default = 1
! Allowed = int
!
! SEC_TOT is ignored if OPT_SEC == NO.
! SEC_TOT is ignored if HDR_SEC == 1.
!</Help>
!
!
!<Help KEYWORD="TERT_TOT">
!<Tip> Total number of tertiary sort bins. </Tip>
! Default = 1
! Allowed = int
!
! TERT_TOT is ignored if OPT_TERT == NO.
! TERT_TOT is ignored if HDR_TERT == 1.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


   module fill_module

   use pc_module
   use named_constants_module
   use triplesort_module

   implicit none

   private
   public :: fill_create
   public :: fill_initialize
   public :: fill_update
   public :: fill_delete
!<execute_only>
   public :: fill
   public :: fill_wrapup
!</execute_only>


      character(len=100),public,save :: FILL_IDENT = &
'$Id: fill.f90,v 1.4 2002/04/26 12:54:51 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


   type,public :: fill_struct              
 
     private
     logical                 :: skip_wrapup      ! wrapup flag.
     integer                 :: nwih,ndpt        ! global parameters.
     type(triplesort_bools)  :: opt              ! process parameters.
     type(triplesort_ints)   :: hdr              ! process parameters.
     type(triplesort_floats) :: init             ! process parameters.
     type(triplesort_floats) :: inc              ! process parameters.
     type(triplesort_floats) :: last             ! process parameters.
     type(triplesort_ints)   :: tot              ! process parameters.

!<execute_only>

     integer                 :: input,unsorted,deleted,retained,created,output
     integer                 :: groups,fold
     type(triplesort_ints)   :: binprev        ! previous retained input trace.
     type(triplesort_ints)   :: binlast        ! previous output trace.
     type(triplesort_floats) :: val_min_in,val_min_out
     type(triplesort_floats) :: val_max_in,val_max_out
     type(triplesort_ints)   :: grp_min_in,grp_min_out
     type(triplesort_ints)   :: grp_max_in,grp_max_out
     integer                 :: ikeep,nkeep,lun

!</execute_only>

   end type fill_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(fill_struct),pointer,save :: object       ! needed for traps.

      integer,parameter      :: DELETE_TRACE = 333   ! special value of NTR.
      integer,parameter      :: BAD_SORT     = 555   ! special value of NTR.
      integer,parameter      :: FILL_GAP     = 999   ! special value of NTR.
      integer,parameter      :: SINGLE_TRACE = 1     ! special value of NTR.

  !!  logical,parameter      :: OPT_DEBUG = .true.
      logical,parameter      :: OPT_DEBUG = .false.

      real   ,parameter      :: FHUGE = HUGE(1.0)
      integer,parameter      :: IHUGE = HUGE(1) 

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


   subroutine fill_create (obj)
   implicit none
   type(fill_struct),pointer :: obj       ! arguments

   allocate (obj)

   call fill_initialize (obj)
   return
   end subroutine fill_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


   subroutine fill_delete (obj)
   implicit none
   type(fill_struct),pointer :: obj       ! arguments

!<execute_only>
   call fill_wrapup (obj)
!</execute_only>

   deallocate(obj)
   return
   end subroutine fill_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


   subroutine fill_initialize (obj)
   implicit none
   type(fill_struct),intent(inout) :: obj       ! arguments

   obj%opt  = .true.
   obj%hdr  = (/8,7,6/)
   obj%init = 1.0
   obj%inc  = 1.0
   obj%last = 1.0
   obj%tot  = 1

   call fill_update (obj)
   return
   end subroutine fill_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


   subroutine fill_update (obj)
   implicit none
   type(fill_struct),intent(inout),target :: obj                 ! arguments
   type(triplesort_floats)                :: remember            ! local


   object => obj               ! needed for traps.
   obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


   remember = obj%last

   call pc_get_global ('NWIH' , obj%nwih)
   call pc_get_global ('NDPT' , obj%ndpt)

   call pc_get ('OPT_PRI'     , obj%opt%primary   )
   call pc_get ('OPT_SEC'     , obj%opt%secondary )
   call pc_get ('OPT_TERT'    , obj%opt%tertiary  )
   call pc_get ('HDR_PRI'     , obj%hdr%primary   )
   call pc_get ('HDR_SEC'     , obj%hdr%secondary )
   call pc_get ('HDR_TERT'    , obj%hdr%tertiary  )
   call pc_get ('PRI_INIT'    , obj%init%primary  )
   call pc_get ('SEC_INIT'    , obj%init%secondary)
   call pc_get ('TERT_INIT'   , obj%init%tertiary )
   call pc_get ('PRI_INC'     , obj%inc%primary   )
   call pc_get ('SEC_INC'     , obj%inc%secondary )
   call pc_get ('TERT_INC'    , obj%inc%tertiary  )
   call pc_get ('PRI_LAST'    , obj%last%primary  )
   call pc_get ('SEC_LAST'    , obj%last%secondary)
   call pc_get ('TERT_LAST'   , obj%last%tertiary )
   call pc_get ('PRI_TOT'     , obj%tot%primary   )
   call pc_get ('SEC_TOT'     , obj%tot%secondary )
   call pc_get ('TERT_TOT'    , obj%tot%tertiary  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


   call fill_fixup (obj%opt %primary,  &
                    obj%hdr %primary,  &
                    obj%init%primary,  &
                    obj%inc %primary,  &
                    obj%last%primary,  &
                    obj%tot %primary,  &
                    remember%primary, obj%nwih, 'PRI')

   call fill_fixup (obj%opt %secondary,  &
                    obj%hdr %secondary,  &
                    obj%init%secondary,  &
                    obj%inc %secondary,  &
                    obj%last%secondary,  &
                    obj%tot %secondary,  &
                    remember%secondary, obj%nwih, 'SEC')

   call fill_fixup (obj%opt %tertiary,  &
                    obj%hdr %tertiary,  &
                    obj%init%tertiary,  &
                    obj%inc %tertiary,  &
                    obj%last%tertiary,  &
                    obj%tot %tertiary,  &
                    remember%tertiary, obj%nwih, 'TERT')

   if (obj%hdr%primary == 1) then
       call pc_error ('HDR_PRI must be greater than one')
   end if

   if (obj%hdr%secondary == 1 .and. obj%hdr%tertiary /= 1) then
       call pc_error ('HDR_TERT must be one if HDR_SEC is one')
   end if

   if (obj%hdr%primary == obj%hdr%secondary) then
       call pc_error ('HDR_PRI and HDR_SEC cannot be the same')
   end if

   if (obj%hdr%primary == obj%hdr%tertiary) then
       call pc_error ('HDR_PRI and HDR_TERT cannot be the same')
   end if

   if (pc_verify_end()) then
   if (obj%hdr%secondary == obj%hdr%tertiary .and. obj%hdr%tertiary /= 1) then
       call pc_error ('HDR_SEC and HDR_TERT cannot be the same')
   end if
   end if

   if (obj%opt == .false.) then
       call pc_warning &
    ('Nothing will be done because OPT_PRI, OPT_SEC, and OPT_TERT are all NO')
   end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


   call pc_put ('OPT_PRI'     , obj%opt%primary   )
   call pc_put ('OPT_SEC'     , obj%opt%secondary )
   call pc_put ('OPT_TERT'    , obj%opt%tertiary  )
   call pc_put ('HDR_PRI'     , obj%hdr%primary   )
   call pc_put ('HDR_SEC'     , obj%hdr%secondary )
   call pc_put ('HDR_TERT'    , obj%hdr%tertiary  )
   call pc_put ('PRI_INIT'    , obj%init%primary  )
   call pc_put ('SEC_INIT'    , obj%init%secondary)
   call pc_put ('TERT_INIT'   , obj%init%tertiary )
   call pc_put ('PRI_INC'     , obj%inc%primary   )
   call pc_put ('SEC_INC'     , obj%inc%secondary )
   call pc_put ('TERT_INC'    , obj%inc%tertiary  )
   call pc_put ('PRI_LAST'    , obj%last%primary  )
   call pc_put ('SEC_LAST'    , obj%last%secondary)
   call pc_put ('TERT_LAST'   , obj%last%tertiary )
   call pc_put ('PRI_TOT'     , obj%tot%primary   )
   call pc_put ('SEC_TOT'     , obj%tot%secondary )
   call pc_put ('TERT_TOT'    , obj%tot%tertiary  )

   call pc_put_control ('TWOSETS'     , .true.)
   call pc_put_control ('NEED_LABEL'  , .true.)
   call pc_put_control ('NEED_REQUEST', .true.)

   call pc_put_global ('GATHERED', .false.)
   call pc_put_global ('NUMTR'   ,    1   )

   call pc_put_sensitive_field_flag ('OPT_SEC'   , obj%hdr%secondary > 1)
   call pc_put_sensitive_field_flag ('OPT_TERT'  , obj%hdr%tertiary  > 1)
   call pc_put_sensitive_field_flag ('SEC_INIT'  , obj%hdr%secondary > 1)
   call pc_put_sensitive_field_flag ('TERT_INIT' , obj%hdr%tertiary  > 1)
   call pc_put_sensitive_field_flag ('SEC_INC'   , obj%hdr%secondary > 1)
   call pc_put_sensitive_field_flag ('TERT_INC'  , obj%hdr%tertiary  > 1)
   call pc_put_sensitive_field_flag ('SEC_LAST'  , obj%hdr%secondary > 1)
   call pc_put_sensitive_field_flag ('TERT_LAST' , obj%hdr%tertiary  > 1)
   call pc_put_sensitive_field_flag ('SEC_TOT'   , obj%hdr%secondary > 1)
   call pc_put_sensitive_field_flag ('TERT_TOT'  , obj%hdr%tertiary  > 1)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

   if (pc_do_not_process_traces()) return
   obj%skip_wrapup = .false.

   obj%lun = pc_get_lun()

   obj%input    = 0   ! number of traces input.
   obj%unsorted = 0   ! number of traces deleted (improperly sorted).
   obj%deleted  = 0   ! number of traces deleted (outside of specified range).
   obj%retained = 0   ! number of original traces retained.
   obj%created  = 0   ! number of traces created.
   obj%output   = 0   ! number of traces output.

   obj%groups   = 0   ! number of groups output.
   obj%fold     = 0   ! fold in each group passed out.

   obj%ikeep = 0   ! number of traces already used from the input trace array.
   obj%nkeep = 0   ! total number of traces in the input trace array.

   obj%val_min_in   =  FHUGE  ! minimum input  value for headers PRI,SEC,TERT.
   obj%val_max_in   = -FHUGE  ! maximum input  value for headers PRI,SEC,TERT.
   obj%val_min_out  =  FHUGE  ! minimum output value for headers PRI,SEC,TERT.
   obj%val_max_out  = -FHUGE  ! maximum output value for headers PRI,SEC,TERT.

   obj%grp_min_in   =  IHUGE  ! minimum input  value for headers 1,3,4.
   obj%grp_max_in   = -IHUGE  ! maximum input  value for headers 1,3,4.
   obj%grp_min_out  =  IHUGE  ! minimum output value for headers 1,3,4.
   obj%grp_max_out  = -IHUGE  ! maximum output value for headers 1,3,4.

   obj%binprev = -IHUGE    ! last retained bin input.
   obj%binlast = 0         ! last bin output.

   
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>

      return
      end subroutine fill_update


!!----------------------------- fixup --------------------------------------!!
!!----------------------------- fixup --------------------------------------!!
!!----------------------------- fixup --------------------------------------!!


   subroutine fill_fixup (opt,hdr,init,inc,last,tot,remember,nwih,which)
   implicit none
   logical          ,intent(inout) :: opt                    ! arguments
   integer          ,intent(inout) :: hdr,tot                ! arguments
   real             ,intent(inout) :: init,inc,last          ! arguments
   real             ,intent(in)    :: remember               ! arguments
   integer          ,intent(in)    :: nwih                   ! arguments
   character(len=*) ,intent(in)    :: which                  ! arguments

   if (hdr <= 1) then
        opt  = .false.
        hdr  = 1
        init = 1.0
        inc  = 1.0
        last = 1.0
        tot  = 1
        return
   end if

   if (hdr > nwih) then
        call pc_error ('HDR_'//trim(which)//' cannot be larger than',nwih)
   end if

   if (inc == 0.0) then
        inc = 1.0
        call pc_warning (trim(which)//'_INC cannot be zero. Resetting to 1.0')
   endif

   if (last /= remember) tot = 1 + nint((last - init) / inc)

   last = init + (tot - 1) * inc
   return
   end subroutine fill_fixup


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>


   subroutine fill (obj,ntr,hdi,tri,hdo,tro)
   implicit none
   type(fill_struct),intent(inout) :: obj                  ! arguments
   integer          ,intent(inout) :: ntr                  ! arguments
   double precision ,intent(in)    :: hdi(:,:)             ! arguments
   real             ,intent(in)    :: tri(:,:)             ! arguments
   double precision ,intent(out)   :: hdo(:,:)             ! arguments
   real             ,intent(out)   :: tro(:,:)             ! arguments
   type(triplesort_ints)           :: bin,binout           ! local
   integer                         :: level                ! local

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

             call fill_pre_process    (obj,hdi(:,obj%ikeep),bin)
             call fill_here_is_input  (obj,bin,ntr,level,binout)
             call fill_debug_printout (obj,bin,ntr,level,binout)

             select case (ntr)
                case (DELETE_TRACE)
                  obj%deleted = obj%deleted + 1
                  cycle
                case (BAD_SORT)
                  obj%unsorted = obj%unsorted + 1
                  if (obj%unsorted < 10) then
                       write (obj%lun,*)      &
                         'input trace with bins ',bin,          &
                         ' out of order at sort level ', level, &
                         ' -- previous bin input was ',obj%binprev
                  end if
                  cycle
                case (FILL_GAP)
                  obj%ikeep = obj%ikeep - 1   ! will try this same trace again.
                  obj%input = obj%input - 1   ! will try this same trace again.
                  exit
                case (SINGLE_TRACE)
                  obj%retained      = obj%retained + 1
                  hdo(1:obj%nwih,1) = hdi(1:obj%nwih,obj%ikeep)
                  tro(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%ikeep)
                  obj%binprev       = bin
                  exit
                case (FATAL_ERROR)
                  call fill_wrapup (obj)
                  return
                case default
                  call pc_error ('FILL programming error')
                  call pc_error ('we should not get to here with NTR =',ntr)
                  call pc_error ('programming error in FILL_HERE_IS_INPUT')
                  ntr = FATAL_ERROR
                  call fill_wrapup (obj)
                  return
             end select

        end do

!----------THERE ARE NO MORE INPUT TRACES.

   else

        call fill_no_more_input   (obj,ntr,binout)
        call fill_debug_printout2 (obj,ntr,binout)

        select case (ntr)
           case (FILL_GAP)
             continue
           case (NO_MORE_TRACES)
             call fill_wrapup (obj)
             return
           case (FATAL_ERROR)
             call fill_wrapup (obj)
             return
           case default
             call pc_error ('FILL programming error')
             call pc_error ('we should not get to here with NTR =',ntr)
             call pc_error ('programming error in FILL_NO_MORE_INPUT')
             ntr = FATAL_ERROR
             call fill_wrapup (obj)
             return
        end select

   end if

!----------NOW WE MIGHT HAVE TO CREATE A FILL TRACE.

   if (ntr == FILL_GAP) then
        call fill_create_fill_trace (obj,binout,hdo(:,1),tro(:,1))
        ntr = SINGLE_TRACE
   end if

!----------NOW WE HAVE AN OUTPUT TRACE AND NTR HAS BEEN SET TO 1.

   if (ntr /= SINGLE_TRACE) then
        call pc_error ('FILL programming error')
        call pc_error ('we should not get to here with NTR =',ntr)
        call pc_error ('programming error in FILL main routine')
        ntr = FATAL_ERROR
        call fill_wrapup (obj)
        return
   end if

   call fill_post_process (obj,binout,hdo(:,1))
   return
   end subroutine fill


!!-------------------------- pre process ---------------------------------!!
!!-------------------------- pre process ---------------------------------!!
!!-------------------------- pre process ---------------------------------!!


   subroutine fill_pre_process (obj,hdi,bin)
   implicit none
   type(fill_struct)    ,intent(inout) :: obj                  ! arguments
   double precision     ,intent(in)    :: hdi(:)               ! arguments
   type(triplesort_ints),intent(out)   :: bin                  ! arguments
   type(triplesort_ints)               :: grp                  ! local
   type(triplesort_floats)             :: val                  ! local

!----------GET THE INPUT TRACE VALUES.

   grp%primary   = nint(hdi(HDR_SEQUENCE       ))
   grp%secondary = nint(hdi(HDR_CURRENT_GROUP  ))
   grp%tertiary  = nint(hdi(HDR_CURRENT_CHANNEL))

   val%primary   = hdi(obj%hdr%primary  )
   val%secondary = hdi(obj%hdr%secondary)
   val%tertiary  = hdi(obj%hdr%tertiary )

!----------GET THE INPUT TRACE BIN.

   bin = triplesort_binning (val, obj%init, obj%inc)

!----------UPDATE THE INPUT LIMITS.

   obj%grp_min_in = triplesort_minima (grp, obj%grp_min_in)
   obj%grp_max_in = triplesort_maxima (grp, obj%grp_max_in)
   obj%val_min_in = triplesort_minima (val, obj%val_min_in)
   obj%val_max_in = triplesort_maxima (val, obj%val_max_in)
   return
   end subroutine fill_pre_process


!!------------------------- post process ----------------------------------!!
!!------------------------- post process ----------------------------------!!
!!------------------------- post process ----------------------------------!!


   subroutine fill_post_process (obj,binout,hdo)
   implicit none
   type(fill_struct)    ,intent(inout) :: obj                  ! arguments
   type(triplesort_ints),intent(in)    :: binout               ! arguments
   double precision     ,intent(inout) :: hdo(:)               ! arguments
   logical                             :: new_group            ! local
   type(triplesort_ints)               :: grp                  ! local
   type(triplesort_floats)             :: val                  ! local

!----------GET THE OUTPUT pri,sec,tert HEADER WORD VALUES.

   val%primary   = hdo(obj%hdr%primary  )
   val%secondary = hdo(obj%hdr%secondary)
   val%tertiary  = hdo(obj%hdr%tertiary )

!----------RESET THE 1,3,4 HEADER WORDS.

   if (obj%hdr%tertiary <= 1) then
        new_group = (binout%primary   /= obj%binlast%primary)
   else
        new_group = (binout%secondary /= obj%binlast%secondary .or. &
                     binout%primary   /= obj%binlast%primary)
   endif

   if (new_group) then
        obj%groups = obj%groups + 1
        obj%fold   = 0
   end if

   obj%fold                 = obj%fold   + 1
   obj%output               = obj%output + 1
   hdo(HDR_SEQUENCE       ) = obj%output
   hdo(HDR_CURRENT_GROUP  ) = obj%groups
   hdo(HDR_CURRENT_CHANNEL) = obj%fold

!----------GET THE OUTPUT 1,3,4 HEADER WORD VALUES.

   grp%primary   = nint(hdo(HDR_SEQUENCE       ))
   grp%secondary = nint(hdo(HDR_CURRENT_GROUP  ))
   grp%tertiary  = nint(hdo(HDR_CURRENT_CHANNEL))

!----------REMEMBER THE OUTPUT TRACE BIN.

   obj%binlast = binout

!----------UPDATE THE OUTPUT LIMITS.

   obj%grp_min_out = triplesort_minima (grp, obj%grp_min_out)
   obj%grp_max_out = triplesort_maxima (grp, obj%grp_max_out)
   obj%val_min_out = triplesort_minima (val, obj%val_min_out)
   obj%val_max_out = triplesort_maxima (val, obj%val_max_out)
   return
   end subroutine fill_post_process


!!---------------------------- debug printout ------------------------------!!
!!---------------------------- debug printout ------------------------------!!
!!---------------------------- debug printout ------------------------------!!


      subroutine fill_debug_printout (obj, bin, ntr, level, binout)
      implicit none
      type(fill_struct)    ,intent(in) :: obj                  ! arguments
      type(triplesort_ints),intent(in) :: bin,binout           ! arguments
      integer              ,intent(in) :: ntr,level            ! arguments
      character(len=36)                :: msg                  ! local

      if (.not.OPT_DEBUG) return

      select case (ntr)
         case (DELETE_TRACE) ; msg = '--deleted'
         case (BAD_SORT    ) ; msg = '----------unsorted'
         case (FILL_GAP    ) ; msg = '-------------------fillgap'
         case (SINGLE_TRACE) ; msg = '---------------------------use'
         case default        ; msg = '@@@@@@@@@@ whoops @@@@@@@@@@@@'
      end select

      write(obj%lun,*) 'opt=',obj%opt,'  tot=',obj%tot,   &
                       '  binlast=',obj%binlast, &
                       '  bin=',bin,'  binout=',binout, &
                       '  level=',level,' ',msg
      return
      end subroutine fill_debug_printout



      subroutine fill_debug_printout2 (obj, ntr, binout)
      implicit none
      type(fill_struct)    ,intent(in) :: obj                  ! arguments
      integer              ,intent(in) :: ntr                  ! arguments
      type(triplesort_ints),intent(in) :: binout               ! arguments
      character(len=36)                :: msg                  ! local

      if (.not.OPT_DEBUG) return

      select case (ntr)
         case (FILL_GAP      ) ; msg = '+++++++++ created ++++++++++++'
         case (NO_MORE_TRACES) ; msg = '+++++++++ finished +++++++++++'
         case default          ; msg = '@@@@@@@@@ whoops2 @@@@@@@@@@@@'
      end select

      write(obj%lun,*) 'opt=',obj%opt,'  tot=',obj%tot,   &
                       '  binlast=',obj%binlast, &
                       '                binout=',binout,' ',msg
      return
      end subroutine fill_debug_printout2


!!------------------------ here is input -----------------------------------!!
!!------------------------ here is input -----------------------------------!!
!!------------------------ here is input -----------------------------------!!

! returns DELETE_TRACE, BAD_SORT, FILL_GAP, SINGLE_TRACE, or FATAL_ERROR.
! if ntr == FILL_GAP    , returns the output bin to create to fill a gap.
! if ntr == SINGLE_TRACE, returns the output bin equal to the input bin.
! if ntr == DELETE_TRACE, returns the input bin (which should not be used).
! if ntr == BAD_SORT    , returns the input bin (which should not be used).
! if ntr == FATAL_ERROR , returns the input bin (which should not be used).


      subroutine fill_here_is_input (obj, bin, ntr, level, binout)
      implicit none
      type(fill_struct)    ,intent(inout) :: obj                  ! arguments
      type(triplesort_ints),intent(in)    :: bin                  ! arguments
      integer              ,intent(out)   :: ntr,level            ! arguments
      type(triplesort_ints),intent(out)   :: binout               ! arguments

!----------GET STARTED.

      level  = 0
      binout = bin

!----------DELETE THIS TRACE IF IT IS OUT OF RANGE.

      if (obj%opt%primary) then
           if (bin%primary < 1 .or. bin%primary > obj%tot%primary) then
                ntr    = DELETE_TRACE
                level  = 1
                return
           end if
      end if

      if (obj%opt%secondary) then
           if (bin%secondary < 1 .or. bin%secondary > obj%tot%secondary) then
                ntr    = DELETE_TRACE
                level  = 2
                return
           end if
      end if

      if (obj%opt%tertiary) then
           if (bin%tertiary < 1 .or. bin%tertiary > obj%tot%tertiary) then
               ntr    = DELETE_TRACE
               level  = 3
               return
          end if
      end if

!----------DELETE THIS TRACE IF IT PRECEDES THE LAST INPUT TRACE.
!                    (this would be a user error)

      if (obj%opt%primary) then
           if (bin%primary < obj%binprev%primary) then
                ntr   = BAD_SORT
                level = 1
                return
           end if
      end if

      if (obj%opt%secondary) then
           if (bin%primary == obj%binprev%primary) then
                if (bin%secondary < obj%binprev%secondary) then
                     ntr   = BAD_SORT
                     level = 2
                     return
                end if
           end if
      end if

      if (obj%opt%tertiary) then
           if (bin%primary   == obj%binprev%primary .and. &
               bin%secondary == obj%binprev%secondary) then
                if (bin%tertiary < obj%binprev%tertiary) then
                     ntr   = BAD_SORT
                     level = 3
                     return
                end if
           end if
      end if

      obj%binprev = bin

!----------DELETE THIS TRACE IF IT PRECEDES THE LAST OUTPUT TRACE.
!                    (this would be a programming error)

      if (obj%opt%primary) then
           if (bin%primary < obj%binlast%primary) then
                call pc_error ('FILL programming error')
                call pc_error ('primary bin precedes last output trace')
                call pc_error ('programming error in FILL_HERE_IS_INPUT')
                ntr   = FATAL_ERROR
                level = 1
                return
           end if
      end if

      if (obj%opt%secondary) then
           if (bin%primary == obj%binlast%primary) then
                if (bin%secondary < obj%binlast%secondary) then
                     call pc_error ('FILL programming error')
                     call pc_error ('secondary bin precedes last output trace')
                     call pc_error ('programming error in FILL_HERE_IS_INPUT')
                     ntr   = FATAL_ERROR
                     level = 2
                     return
                end if
           end if
      end if

      if (obj%opt%tertiary) then
           if (bin%primary   == obj%binlast%primary .and. &
               bin%secondary == obj%binlast%secondary) then
                if (bin%tertiary < obj%binlast%tertiary) then
                     call pc_error ('FILL programming error')
                     call pc_error ('tertiary bin precedes last output trace')
                     call pc_error ('programming error in FILL_HERE_IS_INPUT')
                     ntr   = FATAL_ERROR
                     level = 3
                     return
                end if
           end if
      end if

!----------MAKE CHOICE IF THIS IS THE FIRST POTENTIAL OUTPUT TRACE.

      binout = bin    ! also already set above.

      if (obj%output == 0) then
           if (obj%opt%primary)                   binout%primary   = 1 
           if (obj%opt%secondary .or. &
               bin%primary   /= binout%primary)   binout%secondary = 1 
           if (obj%opt%tertiary .or. &
               bin%primary   /= binout%primary .or. &
               bin%secondary /= binout%secondary) binout%tertiary  = 1 
           if (binout == bin) then
                ntr = SINGLE_TRACE
           else
                ntr = FILL_GAP
           end if
           return
      end if

!----------RETURN THIS TRACE IF IT FALLS IN THE SAME BIN PREVIOUSLY OUTPUT.

      if (binout == obj%binlast) then
           ntr = SINGLE_TRACE
           return
      end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!! The code below this point is not in good shape.               !!!!
    !!!! Sometime I would like to redo it completely.                  !!!!
    !!!! It is not general and difficult to maintain.                  !!!!
    !!!! There may be undetected bugs because of its piecemeal design. !!!!
    !!!! In fact this code is not really designed  - only patched up   !!!!
    !!!! to get rid of known bugs.                                     !!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!----------SPECIAL CASES.

      if (binout%primary   == obj%binlast%primary .and. &
          binout%secondary == obj%binlast%secondary) then
           if (obj%opt%tertiary) then
                if (binout%tertiary == obj%binlast%tertiary .or. &
                    binout%tertiary == obj%binlast%tertiary + 1) then
                     ntr = SINGLE_TRACE
                     return
                else
                     binout%tertiary = obj%binlast%tertiary + 1
                     level = 3
                     ntr = FILL_GAP
                     return
                end if
           else
                ntr = SINGLE_TRACE
                return
           end if
      end if

!----------TFT CASE.

      binout = bin
       
      if (     obj%opt%primary   .and. &
          .not.obj%opt%secondary .and. &
               obj%opt%tertiary) then

           if (binout%primary == obj%binlast%primary .and. &
               obj%binlast%tertiary == obj%tot%tertiary .and. &
               binout%secondary /= obj%binlast%secondary) then
                     binout%tertiary = 1
                     if (bin == binout) then
                          ntr = SINGLE_TRACE
                     else
                          ntr = FILL_GAP
                     end if
                     return
           end if

      end if

!----------FTT CASE.

      if (.not.obj%opt%primary   .and. &
               obj%opt%secondary .and. &
               obj%opt%tertiary) then

          if (obj%binlast%secondary == obj%tot%secondary .and. &
              obj%binlast%tertiary  == obj%tot%tertiary) then
               binout%primary   = bin%primary
               binout%secondary = 1
               binout%tertiary  = 1
                if (bin == binout) then
                     ntr = SINGLE_TRACE
                else
                     ntr = FILL_GAP
                end if
                return
          end if

      end if

!----------FFT CASE.

      if (.not.obj%opt%primary   .and. &
          .not.obj%opt%secondary .and. &
               obj%opt%tertiary) then

           if (obj%binlast%tertiary == obj%tot%tertiary) then
                if (bin%tertiary == 1) then
                     ntr = SINGLE_TRACE
                     return
                else
                     binout%tertiary = 1
                     level = 3
                     ntr = FILL_GAP
                     return
                end if
           else
                binout = obj%binlast
                binout%tertiary  = obj%binlast%tertiary + 1
                level = 3
                ntr = FILL_GAP
                return
           end if

     end if

!----------DECIDE WHETHER WE MUST FILL A GAP BEFORE USING THIS TRACE.

   do
       if (obj%opt%primary) then
            if (bin%primary > obj%binlast%primary + 1) then
                level = 1
                exit
            end if
       end if

       if (obj%opt%secondary) then
            if (bin%primary == obj%binlast%primary) then
                 if (bin%secondary > obj%binlast%secondary + 1) then
                      level = 2
                      exit
                 end if
            else
                 if (obj%binlast%secondary < obj%tot%secondary) then
                      level = 2
                      exit
                 else if (bin%secondary > 1) then
                      level = 2
                      exit
                 end if
            end if
       end if

       if (obj%opt%tertiary) then
            if (bin%primary   == obj%binlast%primary .and. &
                bin%secondary == obj%binlast%secondary) then
                 if (bin%tertiary > obj%binlast%tertiary + 1) then
                      level = 3
                      exit
                 end if
            else
                 if (obj%binlast%tertiary < obj%tot%tertiary) then
                      level = 3
                      exit
                 else if (bin%tertiary > 1) then
                      level = 3
                      exit
                 end if
            end if
       end if

       ntr = SINGLE_TRACE
       return
   end do

!----------CALCULATE THE NEXT REQUIRED OUTPUT BIN.

   binout = obj%binlast
   if      (obj%opt%tertiary  .and. binout%tertiary  < obj%tot%tertiary ) then
             binout%tertiary  = binout%tertiary + 1
   else if (obj%opt%secondary .and. binout%secondary < obj%tot%secondary) then
             binout%secondary = binout%secondary + 1
             binout%tertiary  = 1
             if (.not.obj%opt%tertiary) binout%tertiary = bin%tertiary
   else if (obj%opt%primary   .and. binout%primary   < obj%tot%primary  ) then
             binout%primary   = binout%primary + 1
             binout%secondary = 1
             binout%tertiary  = 1
             if (.not.obj%opt%secondary) binout%secondary = bin%secondary
             if (.not.obj%opt%tertiary) binout%tertiary = bin%tertiary
   else if (.not.obj%opt%primary .and. bin%primary /= obj%binlast%primary)then
             binout%primary   = bin%primary
             binout%secondary = 1
             binout%tertiary  = 1
             if (.not.obj%opt%secondary) binout%secondary = bin%secondary
             if (.not.obj%opt%tertiary) binout%tertiary = bin%tertiary
   else
             call pc_error ('FILL programming error')
             call pc_error ('should not have arrived at this location')
             call pc_error ('programming error in FILL_HERE_IS_INPUT')
             ntr = FATAL_ERROR
             return
   end if

   ntr = FILL_GAP
   return
   end subroutine fill_here_is_input


!!-------------------------- no more input ---------------------------------!!
!!-------------------------- no more input ---------------------------------!!
!!-------------------------- no more input ---------------------------------!!

! returns FILL_GAP or NO_MORE_TRACES.
! returns the output bin to create to fill a gap.


   subroutine fill_no_more_input (obj, ntr, binout)
   implicit none
   type(fill_struct)    ,intent(inout) :: obj             ! arguments
   integer              ,intent(out)   :: ntr             ! arguments
   type(triplesort_ints),intent(out)   :: binout          ! arguments

   if (obj%output == 0) then
        binout = (/1,1,1/)
        if (obj%opt == .false.) then
                  ntr = NO_MORE_TRACES
        else
                  ntr = FILL_GAP
        end if
        return
   end if

   binout = obj%binlast

   if      (obj%opt%tertiary  .and. binout%tertiary  < obj%tot%tertiary ) then
             binout%tertiary  = binout%tertiary + 1
             ntr = FILL_GAP
   else if (obj%opt%secondary .and. binout%secondary < obj%tot%secondary) then
             binout%secondary = binout%secondary + 1
             binout%tertiary  = 1
             ntr = FILL_GAP
   else if (obj%opt%primary   .and. binout%primary   < obj%tot%primary  ) then
             binout%primary   = binout%primary + 1
             binout%secondary = 1
             binout%tertiary  = 1
             ntr = FILL_GAP
   else
             ntr = NO_MORE_TRACES
   end if
   return
   end subroutine fill_no_more_input


!!---------------------- create fill trace ---------------------------------!!
!!---------------------- create fill trace ---------------------------------!!
!!---------------------- create fill trace ---------------------------------!!


   subroutine fill_create_fill_trace (obj, binout, hdo, tro)
   implicit none
   type(fill_struct)    ,intent(inout) :: obj                  ! arguments
   type(triplesort_ints),intent(in)    :: binout               ! arguments
   double precision     ,intent(out)   :: hdo(:)               ! arguments
   real                 ,intent(out)   :: tro(:)               ! arguments
   type(triplesort_floats)             :: val                  ! local

   obj%created            = obj%created + 1
   val                    = triplesort_unbinning (binout, obj%init, obj%inc)
   hdo(1:obj%nwih)        = 0.0
   tro(1:obj%ndpt)        = 0.0
   hdo(2)                 = 1
   hdo(64)                = obj%ndpt
   hdo(obj%hdr%primary)   = val%primary
   hdo(obj%hdr%secondary) = val%secondary
   hdo(obj%hdr%tertiary)  = val%tertiary
   return
   end subroutine fill_create_fill_trace


!!--------------------------- reporter ------------------------------------!!
!!--------------------------- reporter ------------------------------------!!
!!--------------------------- reporter ------------------------------------!!


   subroutine fill_reporter (obj)
   implicit none
   type(fill_struct),intent(in) :: obj                            ! arguments
   character(len=3)             :: word1,word2,word3              ! local
   character(len=30)            :: phrase1,phrase2,phrase3        ! local
   character(len=*),parameter   :: space = '     '                ! local
   character(len=*),parameter   :: pre   = space//'+++++++     '  ! local
   character(len=*),parameter   :: warn  = space// &              ! local
      '++++++++++++++++++++++++++++ WARNING +++++++++++++++++++++++++++++++'

!----------PRINT WARNING MESSAGE.

   if (obj%unsorted > 0) then
     write(obj%lun,*) space
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) pre
     write(obj%lun,*) pre,obj%unsorted,' traces deleted (improperly sorted)'
     write(obj%lun,*) pre
     write(obj%lun,*) pre,'the first few of these improperly sorted input'
     write(obj%lun,*) pre,'  traces are listed in the report file above'
     write(obj%lun,*) pre
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) warn
     write(obj%lun,*) space
   end if

!----------FORMAT STATEMENTS.

7000 format(22x,'HDR  OPT',8x,'   INIT          INC         LAST       TOT')

3000 format(22x,'        ',8x,'----INPUT VALUES----',8x,'---OUTPUT VALUES----'/&
            22x,'HDR  OPT',8x,'MINIMUM      MAXIMUM',8x,'MINIMUM      MAXIMUM')

7001 format (1x,a17,2x,i5,2x,a3,2x,3f13.3,i10)
3001 format (1x,a17,2x,i5,2x,a3,1x,2i13  ,2x,2i13  )
3002 format (1x,a17,2x,i5,2x,a3,2x,2f13.3,2x,2f13.3,2x,a)

   word1 = 'NO'
   word2 = 'NO'
   word3 = 'NO'

   if (obj%opt%primary  ) word1 = 'YES'
   if (obj%opt%secondary) word2 = 'YES'
   if (obj%opt%tertiary ) word3 = 'YES'

   phrase1 = ' '
   phrase2 = ' '
   phrase3 = ' '

   if (obj%hdr%primary == HDR_CURRENT_GROUP .or. &
       obj%hdr%primary == HDR_CURRENT_CHANNEL) then
            phrase1 = '(before resetting below)'
   end if

   if (obj%hdr%secondary == HDR_CURRENT_GROUP .or. &
       obj%hdr%secondary == HDR_CURRENT_CHANNEL) then
            phrase2 = '(before resetting below)'
   end if

   if (obj%hdr%tertiary == HDR_CURRENT_GROUP .or. &
       obj%hdr%tertiary == HDR_CURRENT_CHANNEL) then
            phrase3 = '(before resetting below)'
   end if

!----------PRINT PARAMETER INFORMATION.

   write(obj%lun,7000)

   if (obj%opt%primary) then
     write(obj%lun,7001) 'PRIMARY:',                                  &
               obj%hdr        %primary   ,word1                    ,  &
               obj%init       %primary   ,obj%inc        %primary  ,  &
               obj%last       %primary   ,obj%tot        %primary
   else if (obj%hdr%primary > 1) then
     write(obj%lun,7001) 'PRIMARY:',                                  &
               obj%hdr        %primary   ,word1                    ,  &
               obj%init       %primary   ,obj%inc        %primary
   else
     write(obj%lun,7001) 'PRIMARY:',                                  &
               obj%hdr        %primary   ,word1
   end if

   if (obj%opt%secondary) then
     write(obj%lun,7001) 'SECONDARY:',                                &
               obj%hdr        %secondary ,word2                    ,  &
               obj%init       %secondary ,obj%inc        %secondary,  &
               obj%last       %secondary ,obj%tot        %secondary
   else if (obj%hdr%secondary > 1) then
     write(obj%lun,7001) 'SECONDARY:',                                &
               obj%hdr        %secondary ,word2                    ,  &
               obj%init       %secondary ,obj%inc        %secondary
   else
     write(obj%lun,7001) 'SECONDARY:',                                &
               obj%hdr        %secondary ,word2
   end if

   if (obj%opt%tertiary) then
     write(obj%lun,7001) 'TERTIARY:',                                 &
               obj%hdr        %tertiary  ,word3                    ,  &
               obj%init       %tertiary  ,obj%inc        %tertiary ,  &
               obj%last       %tertiary  ,obj%tot        %tertiary
   else if (obj%hdr%tertiary > 1) then
     write(obj%lun,7001) 'TERTIARY:',                                 &
               obj%hdr        %tertiary  ,word3                    ,  &
               obj%init       %tertiary  ,obj%inc        %tertiary
   else
     write(obj%lun,7001) 'TERTIARY:',                                 &
               obj%hdr        %tertiary  ,word3
   end if

!----------PRINT HEADER WORD pri,sec,tert INFORMATION.

   write(obj%lun,3001)
   write(obj%lun,3000)

   if (obj%hdr%primary > 1) then
     write(obj%lun,3002) 'PRIMARY:',                                  &
               obj%hdr        %primary   ,word1                    ,  &
               obj%val_min_in %primary   ,obj%val_max_in %primary  ,  &
               obj%val_min_out%primary   ,obj%val_max_out%primary  ,  &
               trim(phrase1)
   else
     write(obj%lun,3002) 'PRIMARY:',                                  &
               obj%hdr        %primary   ,word1
   end if

   if (obj%hdr%secondary > 1) then
     write(obj%lun,3002) 'SECONDARY:',                                &
               obj%hdr        %secondary ,word2                    ,  &
               obj%val_min_in %secondary ,obj%val_max_in %secondary,  &
               obj%val_min_out%secondary ,obj%val_max_out%secondary,  &
               trim(phrase2)
   else
     write(obj%lun,3002) 'SECONDARY:',                                &
               obj%hdr        %secondary ,word2
   end if

   if (obj%hdr%tertiary > 1) then
     write(obj%lun,3002) 'TERTIARY:',                                 &
               obj%hdr        %tertiary  ,word3                    ,  &
               obj%val_min_in %tertiary  ,obj%val_max_in %tertiary ,  &
               obj%val_min_out%tertiary  ,obj%val_max_out%tertiary ,  &
               trim(phrase3)
   else
     write(obj%lun,3002) 'TERTIARY:',                                 &
               obj%hdr        %tertiary  ,word3
   end if

!----------PRINT HEADER WORD 1,3,4 INFORMATION.

   write(obj%lun,3001) 'trace number:',                             &
             HDR_SEQUENCE              ,' '                      ,  &
             obj%grp_min_in %primary   ,obj%grp_max_in %primary  ,  &
             obj%grp_min_out%primary   ,obj%grp_max_out%primary

   write(obj%lun,3001) 'current group:',                            &
             HDR_CURRENT_GROUP         ,' '                      ,  &
             obj%grp_min_in %secondary ,obj%grp_max_in %secondary,  &
             obj%grp_min_out%secondary ,obj%grp_max_out%secondary

   write(obj%lun,3001) 'trace in group:',                           &
             HDR_CURRENT_CHANNEL       ,' '                      ,  &
             obj%grp_min_in %tertiary  ,obj%grp_max_in %tertiary ,  &
             obj%grp_min_out%tertiary  ,obj%grp_max_out%tertiary

!----------PRINT TRACE INFORMATION.

   write(obj%lun,*) space
   write(obj%lun,*) space,obj%input   ,' traces input'
   write(obj%lun,*) space,obj%unsorted, &
                               ' traces deleted (improperly sorted input)'
   write(obj%lun,*) space,obj%deleted , &
                               ' traces deleted (outside of specified ranges)'
   write(obj%lun,*) space,obj%retained,' original traces retained'
   write(obj%lun,*) space,obj%created ,' dead traces created'
   write(obj%lun,*) space,obj%output  ,' traces output'
   return
   end subroutine fill_reporter


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


   subroutine fill_wrapup (obj)
   implicit none
   type(fill_struct),intent(inout) :: obj       ! arguments
   character(len=*),parameter :: stars = '**********************************'

   if (obj%skip_wrapup) return
   obj%skip_wrapup = .true.

   write(obj%lun,*) ''
   write(obj%lun,*) stars,'******************',stars
   write(obj%lun,*) stars,'** FILL SUMMARY **',stars
   write(obj%lun,*) stars,'******************',stars
   write(obj%lun,*) ''

   call fill_reporter (obj)

   write(obj%lun,*) ''
   write(obj%lun,*) stars,'******************',stars
   write(obj%lun,*) stars,' END FILL SUMMARY ',stars
   write(obj%lun,*) stars,'******************',stars
   write(obj%lun,*) ''
   return
   end subroutine fill_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

   end module fill_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

