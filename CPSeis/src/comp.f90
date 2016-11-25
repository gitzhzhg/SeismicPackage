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
!                         C P S   P R O C E S S                  
!
! Name       : COMP   (COMPosite traces)
! Category   : stacks
! Written    : 2000-05-01   by: Mike O'Brien
! Revised    : 2002-09-03   by: Tom Stoeckley
! Maturity   : production   2002-09-11
! Purpose    : General trace compositing process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! COMP is a general trace compositing process.  COMP is not designed to
! perform CMP composites or other composites that require stack scaling.
!
! Output Trace Scaling:
!
!   Amplitudes of composited samples are the average of the live trace samples
!   that are composited together.  If all traces composited together are dead,
!   the amplitudes will be zero.
!
! Output Header Words:
!
!   Output header word values, for composited traces, are the average of the 
!   corresponding header word values of all of the traces (live or dead)
!   composited together.  Exceptions:  Header word 1 is reset to the correct
!   sequential trace number.  Header word 5 is the sum of the header word 5
!   values of the live traces composited together.  Header word 25 is set
!   to the largest absolute value on the trace.  Note that header words
!   which are usually integers (such as the mute headers) may have fractional
!   parts after averaging.
! 
!-------------------------------------------------------------------------------
!                      DESCRIPTION FOR MODE = COUNT
!
! If MODE = COUNT, then COMP composites consecutive traces based on trace
! count (analogous to do-skip).
! 
!       SKIP_INIT       Number of traces to skip (pass through) initially,
!       NUM_COMP        Number of traces to composite at a time,
!       NUM_SKIP        Number of traces to skip (pass through) at a time,
!       TOT_COMP        Total number of traces composited
!
! Input Trace Order for MODE = COUNT:
!
!   For MODE = COUNT input traces may arrive in any desired order.
!
!-------------------------------------------------------------------------------
!                       DESCRIPTION FOR MODE = HDR 
!
! If MODE = HDR, then COMP composites traces that fall within the same lowest 
! level bin, where the bins are defined by header words HDR_PRI and HDR_SEC.
! If HDR_SEC = 0, then only one level of bins is used. 
!
! Input Trace Order for MODE = HDR:
!
!   All input traces occupying the same primary bin must be received
!   consecutively.  Within a primary bin, all input traces occupying the
!   same secondary bin must be received consecutively.  Input traces that
!   fall outside the bin array (or within gaps) will be deleted and will
!   not trigger a change in the current bin number.  The primary and
!   secondary bins need not be received in any particular order.
!
! Additional Output Header Words for MODE = HDR:
!
!   Header word 3 is set to a counter which starts with 1 and increments
!   each time the primary bin number changes.  Header word 4 is set to a
!   counter which starts with 1 for the first trace in each binary bin, and
!   increments for each trace within the binary bin.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! For MODE = COUNT, input traces may arrive in any desired order.
!
! For MODE = HDR, all input traces occupying the same primary bin must be
! received consecutively.  Within a primary bin, all input traces occupying
! the same secondary bin must be received consecutively.  The primary and
! secondary bins need not be received in any particular order.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                       TRACE OUTPUT CHARACTERISTICS             
!
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
! 
! Name      Description                           Action taken
! ----      -----------                           ------------
! NDPT      Number of sample values in trace      used but not changed
! NWIH      Number of words in header             used but not changed
! NUMTR     Max number of traces input/output     changed to 1
! GATHERED  Whether traces are gathered           changed to .false.
! 
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
!  Hwd#    Description             Action taken
!  ----    -----------             ------------
!    1     Trace sequence number   Reset to correct sequential value.
!    3     Current gather number   Reset based on primary bin if MODE == HDR.
!    4     Current channel number  Reset based on primary bin if MODE == HDR.
!    5     Fold                    Reset to number of live traces composited.
! HDR_PRI  Primary bin header      Used if MODE == HDR.
! HDR_SEC  Secondary bin header    Used if MODE == HDR.
!   25     Largest absolute value  Inspected to avoid compositing dead traces,
!                                   and recomputed for output traces.
!
! Output header word values, for composited traces, are the average of the 
! corresponding header word values of all of the traces (live or dead)
! composited together, except for header words 1, 5, and 25 (also 3 and 4
! if MODE == HDR), which are reset as above.  Note that header words which
! are usually integers (such as the mute headers) may have fractional parts
! after averaging.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
!  9. 2002-09-11  Stoeckley   Change to use the MTH module for binning.
!  8. 2002-06-10  Stoeckley   Change operation of the COUNT mode back to the
!                              original design of outputting uncomposited
!                              traces instead of skipping them.
!  7. 2002-05-22  Stoeckley   Completely re-written from scratch to fix
!                              bugs, reduce the amount of code, and enhance
!                              clarity and maintainability.
!  6. 2001-08-02  Stoeckley   Fix problem with non-vanishing error box.
!  5. 2001-04-26  Stoeckley   Change wrapup flag.
!  4. 2000-07-26  O'Brien     Improved PRI_LAST,PRI_TOT,SEC_LAST,SEC_TOT traps.
!                              Set SEC_*** fields insensitive when HDR_SEC=0.
!                              Add warning when COMP sets ntr to NO_MORE_TRACES.
!                              Fix bug with PRI_TOT counter.
!                              Fix bug with NUM_SKIP=0 logic.
!                              Fix bad normalization logic.
!  3. 2000-06-08  O'Brien     Initialize NUMTR global.
!  2. 2000-05-01  O'Brien     First version.
!  1. 1999-11-16  C I Burch   Initial design.
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
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
! For clarity, and to facilitate maintenance, the following user_parameter to
! program_variable_name translation is used:
!
!        USER_PARAMETER       PROGRAM_VARIABLE_NAME
!        HDR_PRI              hdr1
!        PRI_INIT             init1
!        PRI_INC              inc1
!        PRI_WID              width1
!        PRI_TOT              tot1
!        PRI_LAST             last1
!
!        HDR_SEC              hdr2
!        SEC_INIT             init2
!        SEC_INC              inc2
!        SEC_WID              width2
!        SEC_TOT              tot2
!        SEC_LAST             last2
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS COMP Process/NC=80>
!
!                    General trace compositing process
!
!  MODE=~~~~~`CCCCC  
!
!
!  SKIP_INIT=`IIIIIIII  NUM_COMP=`IIIIIIII
!
!  NUM_SKIP= `IIIIIIII  TOT_COMP=`IIIIIIII    
!
!
!  HDR_PRI=~~`IIIIII
!
!  PRI_INIT= `FFFFFFFFFFF    PRI_INC=`FFFFFFFFFFF    PRI_WID=`FFFFFFFFFFF
!  PRI_LAST= `FFFFFFFFFFF    PRI_TOT=`IIIIIIII    
!
!
!  HDR_SEC=~~`IIIIII
!
!  SEC_INIT= `FFFFFFFFFFF    SEC_INC=`FFFFFFFFFFF    SEC_WID=`FFFFFFFFFFF
!  SEC_LAST= `FFFFFFFFFFF    SEC_TOT=`IIIIIIII
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Method to use for identifying traces to be composited. </Tip>
! Default = HDR
! Allowed = COUNT (Composite traces by sequential trace count.)
! Allowed = HDR   (Composite traces that fall into the same lowest level bin.)
!</Help>
!
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially in the COUNT method. </Tip>
! Default = 0
! Allowed = int>=0
!
! Skipped traces are passed through unchanged.
!
! Used only if MODE = COUNT.
!</Help>
!
!
!<Help KEYWORD="NUM_COMP">
!<Tip> Number of traces to composite together at a time. </Tip>
! Default = 1
! Allowed = int>0
!
! Used only if MODE = COUNT.
!</Help>
!
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of traces to skip at a time. </Tip>
! Default = 0
! Allowed = int>=0
!
! Skipped traces are passed through unchanged.
!
! Used only if MODE = COUNT.
!</Help>
!
!
!<Help KEYWORD="TOT_COMP">
!<Tip> Total number of traces to composite. </Tip>
! Default = 999999999
! Allowed = int>0
!
! TOT_COMP is the total number of traces actually composited together.
!
! After TOT_COMP traces are composited, all additional traces are passed
! through unchanged.
!
! Used only if MODE = COUNT.
!</Help>
!
!
!<Help KEYWORD="HDR_PRI">
!<Tip> Header word designating primary bins. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! Traces falling into the same lowest level bin are composited together.
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="PRI_INIT">
!<Tip> Value of HDR_PRI for center of the first primary bin. </Tip>
! Default = 1.0
! Allowed = real
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="PRI_INC">
!<Tip> Increment between primary bins. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="PRI_WID">
!<Tip> Width of primary bins. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="PRI_LAST">
!<Tip> Value of HDR_PRI for center of last primary bin. </Tip>
! Default = 1.0
! Allowed = real>=PRI_INIT
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="PRI_TOT">
!<Tip> Total number of primary bins. </Tip>
! Default = 1
! Allowed = int>0
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="HDR_SEC">
!<Tip> Header word designating secondary bins. </Tip>
! Default = 6
! Allowed = 0 - NWIH
!
! Traces falling into the same lowest level bin are composited together.
!
! If HDR_SEC = 0, then use only primary bins.
!
! Used only if MODE = HDR.
!</Help>
!
!
!<Help KEYWORD="SEC_INIT">
!<Tip> Value of HDR_SEC for center of first secondary bin. </Tip>
! Default = 1.0
! Allowed = real
!
! Used only if MODE = HDR and HDR_SEC > 0.
!</Help>
!
!
!<Help KEYWORD="SEC_INC">
!<Tip> Increment between secondary bins. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Used only if MODE = HDR and HDR_SEC > 0.
!</Help>
!
!
!<Help KEYWORD="SEC_WID">
!<Tip> Width of secondary bins. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Used only if MODE = HDR and HDR_SEC > 0.
!</Help>
!
!
!<Help KEYWORD="SEC_LAST">
!<Tip> Value of HDR_SEC for center of last secondary bin. </Tip>
! Default = 1.0
! Allowed = real>=SEC_INIT
!
! Used only if MODE = HDR and HDR_SEC > 0.
!</Help>
!
!
!<Help KEYWORD="SEC_TOT">
!<Tip> Total number of secondary bins. </Tip>
! Default = 1
! Allowed = int>0
!
! Used only if MODE = HDR and HDR_SEC > 0.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module comp_module

      use pc_module
      use named_constants_module
      use string_module
      use lav_module
      use mth_module

      implicit none
      private
      public :: comp_create   
      public :: comp_initialize
      public :: comp_update   
      public :: comp_delete
      public :: comp         
      public :: comp_wrapup

      character(len=100),public,save :: COMP_IDENT = &
             '$Id: comp.f90,v 1.9 2002/09/10 13:36:11 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: comp_struct              

        logical           :: skip_wrapup          ! wrapup flag.
        integer           :: nwih,ndpt,lun        ! globals.  

        character(len=9)  :: mode                 ! process parameters.
        integer           :: skip_init            ! process parameters.
        integer           :: num_comp             ! process parameters.
        integer           :: num_skip             ! process parameters.
        integer           :: tot_comp             ! process parameters.
        integer           :: hdr1,    hdr2        ! process parameters.
        real              :: init1,   init2       ! process parameters.
        real              :: inc1,    inc2        ! process parameters.
        real              :: width1,  width2      ! process parameters.
        real              :: last1,   last2       ! process parameters.
        integer           :: tot1,    tot2        ! process parameters.

        double precision,pointer :: hdsum(:)
        real            ,pointer :: trsum(:)

        integer     :: kount_input       ! total number of input traces.
        integer     :: kount_skipped     ! number of input traces skipped.
        integer     :: kount_passed      ! number of traces passed through.
        integer     :: kount_composited  ! number of input traces composited.
        integer     :: kount_stacked     ! number of composite output traces.
        integer     :: kount_output      ! total number of output traces.

        integer     :: nfold             ! live fold of composite trace.
        integer     :: nhdsum            ! number of trace headers composited.
        integer     :: ntrsum            ! number of live traces composited.
        integer     :: current_group     ! tracks the current group.
        integer     :: current_channel   ! tracks the current channel.
        integer     :: bin1_keep         ! last used input primary bin.
        integer     :: bin2_keep         ! last used input secondary bin.
        integer     :: bin1_out          ! last primary bin output.
        real        :: half1             ! half of width1.
        real        :: half2             ! half of width2.
        integer     :: nkeep             ! number of traces in input gather.
        integer     :: ikeep             ! last trace used in input gather.

      end type comp_struct


!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!
!--------------------------- interface blocks --------------------------------!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      logical,parameter,private :: DEBUG_PRINT = .false.
   !  logical,parameter,private :: DEBUG_PRINT = .true.

      integer,parameter,private :: CONDITION_SKIP = 1
      integer,parameter,private :: CONDITION_NEW  = 2
      integer,parameter,private :: CONDITION_ADD  = 3
      integer,parameter,private :: CONDITION_PASS = 4
      integer,parameter,private :: CONDITION_LAST = 5

      character(len=4),parameter,private :: conditions(5) = &
                                  (/'SKIP','NEW ','ADD ', 'PASS', 'LAST'/)

      type(comp_struct),pointer,save :: object       ! needed for traps.

      integer         ,parameter :: mode_nopt = 2
      character(len=5),save      :: mode_options(mode_nopt)

      data mode_options /'HDR','COUNT'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine comp_create (obj)
      implicit none
      type(comp_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%hdsum)
      nullify  (obj%trsum)

      call comp_initialize (obj)
      return
      end subroutine comp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine comp_delete (obj)
      implicit none
      type(comp_struct),pointer :: obj       ! arguments

      call comp_wrapup(obj)

      if(associated(obj%hdsum)) deallocate (obj%hdsum)
      if(associated(obj%trsum)) deallocate (obj%trsum)

      deallocate(obj)
      return
      end subroutine comp_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine comp_initialize (obj)
      implicit none
      type(comp_struct),intent(inout) :: obj                   ! arguments
      integer,parameter               :: IHUGE=HUGE(1)         ! local

      obj%mode      = 'HDR'

      obj%skip_init = 0
      obj%num_comp  = 1
      obj%num_skip  = 0
      obj%tot_comp  = 999999999

      obj%hdr1      = 7
      obj%init1     = 1.0
      obj%inc1      = 1.0
      obj%width1    = 1.0
      obj%last1     = 1.0
      obj%tot1      = 1

      obj%hdr2      = 6
      obj%init2     = 1.0
      obj%inc2      = 1.0
      obj%width2    = 1.0
      obj%last2     = 1.0
      obj%tot2      = 1

      call comp_update (obj)
      return
      end subroutine comp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine comp_update (obj)
      implicit none
      type(comp_struct),target  :: obj                       ! arguments
      logical                   :: is_count,is_hdr,is_hdr2   ! local
      real                      :: last1_keep,last2_keep     ! local
      integer                   :: tot1_keep,tot2_keep       ! local

      object => obj                             ! needed for traps.
      obj%skip_wrapup = .true.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      obj%lun = pc_get_lun()

      last1_keep = obj%last1
      tot1_keep  = obj%tot1
      last2_keep = obj%last2
      tot2_keep  = obj%tot2

      call pc_get_global ('NWIH' ,obj%nwih )
      call pc_get_global ('NDPT' ,obj%ndpt )

      call pc_get ('MODE'     , obj%mode     )
      call pc_get ('SKIP_INIT', obj%skip_init)
      call pc_get ('NUM_COMP' , obj%num_comp )
      call pc_get ('NUM_SKIP' , obj%num_skip )
      call pc_get ('TOT_COMP' , obj%tot_comp )

      call pc_get ('HDR_PRI'  , obj%hdr1     )
      call pc_get ('PRI_INIT' , obj%init1    )
      call pc_get ('PRI_INC'  , obj%inc1     )
      call pc_get ('PRI_WID'  , obj%width1   )
      call pc_get ('PRI_LAST' , obj%last1    )
      call pc_get ('PRI_TOT'  , obj%tot1     )

      call pc_get ('HDR_SEC'  , obj%hdr2     )
      call pc_get ('SEC_INIT' , obj%init2    )
      call pc_get ('SEC_INC'  , obj%inc2     )
      call pc_get ('SEC_WID'  , obj%width2   )
      call pc_get ('SEC_LAST' , obj%last2    )
      call pc_get ('SEC_TOT'  , obj%tot2     )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call string_to_upper (obj%mode)

      if (all(mode_options /= obj%mode)) then
        call pc_warning('Invalid value for MODE:',obj%mode)
        call pc_warning('Resetting to default: HDR')
        obj%mode = 'HDR'
      endif

      if (obj%mode == 'COUNT') then
          if (obj%skip_init < 0) then
            call pc_warning('SKIP_INIT must be >= 0')
            call pc_warning('Resetting to default: 0')
            obj%skip_init = 0
          endif

          if (obj%num_comp < 1) then
            call pc_warning('NUM_COMP must be > 0')
            call pc_warning('Resetting to default: 1')
            obj%num_comp = 1
          endif

          if (obj%num_skip < 0) then
            call pc_warning('NUM_SKIP must be >= 0')
            call pc_warning('Resetting to default: 0')
            obj%num_skip = 0
          endif

          if (obj%tot_comp < 1) then
            call pc_warning('TOT_COMP must be > 0')
            call pc_warning('Resetting to default: 999999999')
            obj%tot_comp = 999999999
          endif
      endif

      if (obj%mode == 'HDR') then
          if (obj%hdr1 < 1 .or. obj%hdr1 > obj%nwih) then
            call pc_error('HDR_PRI is out of range ',1,' to ',obj%nwih)
          endif

          if (obj%hdr2 < 0 .or. obj%hdr2 > obj%nwih) then
            call pc_error('HDR_SEC is out of range ',0,' to ',obj%nwih)
          endif

          call comp_fixup ('PRI_INIT','PRI_INC','PRI_WID','PRI_LAST',  &
                           'PRI_TOT',                                  &
                           obj%init1,obj%inc1,obj%width1,obj%last1,    &
                           obj%tot1,last1_keep,tot1_keep)

          if (obj%hdr2 > 0) then
            call comp_fixup ('SEC_INIT','SEC_INC','SEC_WID','SEC_LAST',  &
                             'SEC_TOT',                                  &
                             obj%init2,obj%inc2,obj%width2,obj%last2,    &
                             obj%tot2,last2_keep,tot2_keep)
          endif
      endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('MODE', mode_options, mode_nopt)

      call pc_put ('MODE'     , obj%mode     )
      call pc_put ('SKIP_INIT', obj%skip_init)
      call pc_put ('NUM_COMP' , obj%num_comp )
      call pc_put ('NUM_SKIP' , obj%num_skip )
      call pc_put ('TOT_COMP' , obj%tot_comp )

      call pc_put ('HDR_PRI'  , obj%hdr1     )
      call pc_put ('PRI_INIT' , obj%init1    )
      call pc_put ('PRI_INC'  , obj%inc1     )
      call pc_put ('PRI_WID'  , obj%width1   )
      call pc_put ('PRI_LAST' , obj%last1    )
      call pc_put ('PRI_TOT'  , obj%tot1     )

      call pc_put ('HDR_SEC'  , obj%hdr2     )
      call pc_put ('SEC_INIT' , obj%init2    )
      call pc_put ('SEC_INC'  , obj%inc2     )
      call pc_put ('SEC_WID'  , obj%width2   )
      call pc_put ('SEC_LAST' , obj%last2    )
      call pc_put ('SEC_TOT'  , obj%tot2     )

      call pc_put_control ('NEED_LABEL'  , .true.)
      call pc_put_control ('NEED_REQUEST', .true.)
      call pc_put_control ('twosets'     , .true.)

      call pc_put_global ('GATHERED', .false.)
      call pc_put_global ('NUMTR'   ,    1   )

      is_count = (obj%mode == 'COUNT')
      is_hdr   = (obj%mode == 'HDR')
      is_hdr2  = (obj%mode == 'HDR' .and. obj%hdr2 > 0)

      call pc_put_sensitive_field_flag ('SKIP_INIT', is_count)
      call pc_put_sensitive_field_flag ('NUM_COMP',  is_count)
      call pc_put_sensitive_field_flag ('NUM_SKIP',  is_count)
      call pc_put_sensitive_field_flag ('TOT_COMP',  is_count)

      call pc_put_sensitive_field_flag ('HDR_PRI',   is_hdr)
      call pc_put_sensitive_field_flag ('PRI_INIT',  is_hdr)
      call pc_put_sensitive_field_flag ('PRI_INC',   is_hdr)
      call pc_put_sensitive_field_flag ('PRI_WID',   is_hdr)
      call pc_put_sensitive_field_flag ('PRI_LAST',  is_hdr)
      call pc_put_sensitive_field_flag ('PRI_TOT',   is_hdr)

      call pc_put_sensitive_field_flag ('HDR_SEC',   is_hdr)
      call pc_put_sensitive_field_flag ('SEC_INIT',  is_hdr2)
      call pc_put_sensitive_field_flag ('SEC_INC',   is_hdr2)
      call pc_put_sensitive_field_flag ('SEC_WID',   is_hdr2)
      call pc_put_sensitive_field_flag ('SEC_LAST',  is_hdr2)
      call pc_put_sensitive_field_flag ('SEC_TOT',   is_hdr2)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if(associated(obj%hdsum)) deallocate (obj%hdsum)
      if(associated(obj%trsum)) deallocate (obj%trsum)

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      allocate (obj%hdsum(obj%nwih))
      allocate (obj%trsum(obj%ndpt))

      obj%kount_input      = 0
      obj%kount_skipped    = 0
      obj%kount_passed     = 0
      obj%kount_composited = 0
      obj%kount_stacked    = 0
      obj%kount_output     = 0

      obj%nfold            = 0
      obj%nhdsum           = 0
      obj%ntrsum           = 0
      obj%hdsum(:)         = 0.0
      obj%trsum(:)         = 0.0
      obj%current_group    = 0
      obj%current_channel  = 0
      obj%bin1_keep        = 0
      obj%bin2_keep        = 0
      obj%bin1_out         = 0
      obj%half1            = 0.5 * obj%width1
      obj%half2            = 0.5 * obj%width2
      obj%nkeep            = 0
      obj%ikeep            = 0


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine comp_update


!!-------------------------- fixup ----------------------------------------!!
!!-------------------------- fixup ----------------------------------------!!
!!-------------------------- fixup ----------------------------------------!!


      subroutine comp_fixup (key_init,key_inc,key_wid,key_last,key_tot,  &
                             init,inc,wid,last,tot,last_keep,tot_keep)
      implicit none
      character(len=*),intent(in)    :: key_init           ! arguments
      character(len=*),intent(in)    :: key_inc            ! arguments
      character(len=*),intent(in)    :: key_wid            ! arguments
      character(len=*),intent(in)    :: key_last           ! arguments
      character(len=*),intent(in)    :: key_tot            ! arguments
      real            ,intent(inout) :: init               ! arguments
      real            ,intent(inout) :: inc                ! arguments
      real            ,intent(inout) :: wid                ! arguments
      real            ,intent(inout) :: last               ! arguments
      integer         ,intent(inout) :: tot                ! arguments
      real            ,intent(in)    :: last_keep          ! arguments
      integer         ,intent(in)    :: tot_keep           ! arguments

      if (inc <= 0.0) then
              call pc_warning(key_inc,'must be > 0')
              call pc_warning('Resetting to default: 1.0')
              inc = 1.0
      endif

      if (wid <= 0.0) then
              call pc_warning(key_wid,'must be > 0')
              call pc_warning('Resetting to default: 1.0')
              wid = 1.0
      endif

      if(tot == tot_keep .and. last /= last_keep) then
              if (last < init) then
                   call pc_warning(key_last,'must be >=',key_init)
                   call pc_warning('Resetting to',key_init)
                   last = init
              endif
              tot = mth_bin_number (init, inc, last)
      endif

      if (tot < 1) then
            call pc_warning(key_tot,'must be > 0')
            call pc_warning('Resetting to default: 999999999')
            tot = 999999999
      endif

      last = init + inc*(tot-1)
      return
      end subroutine comp_fixup


!!---------------------------- traps ---------------------------------------!!
!!---------------------------- traps ---------------------------------------!!
!!---------------------------- traps ---------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine comp (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(comp_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(in)    :: hdi(:,:)               ! arguments
      real             ,intent(in)    :: tri(:,:)               ! arguments
      double precision ,intent(out)   :: hdo(:,:)               ! arguments
      real             ,intent(out)   :: tro(:,:)               ! arguments
      integer                         :: condition              ! local

!----------CHECK WHETHER WE ARE RECEIVING NEW INPUT TRACES.

      if (ntr >= 0) then      ! we are receiving traces from above.
           obj%nkeep = ntr    ! this will be positive or NO_MORE_TRACES.
           obj%ikeep = 0      ! this is the number of input traces already
                              !  used from the input trace array.
      end if

!----------PROCESS THE NEXT INPUT TRACE(S).

      if (obj%nkeep > 0) then
           do
                obj%ikeep = obj%ikeep + 1
                if (obj%ikeep > obj%nkeep) then
                     ntr = NEED_TRACES
                     return
                end if

                call comp_new_input_trace (obj,hdi(:,obj%ikeep),condition)

                if (condition == CONDITION_LAST) then
                     call comp_add_to_summed_trace &
                                   (obj,hdi(:,obj%ikeep),tri(:,obj%ikeep))
                     call comp_get_output_trace   (obj,hdo(:,1),tro(:,1))
                     call comp_clear_summed_trace (obj)
                     ntr = 1
                     return
                end if

                if (condition == CONDITION_NEW) then
                     call comp_get_output_trace   (obj,hdo(:,1),tro(:,1))
                     call comp_clear_summed_trace (obj)
                     call comp_add_to_summed_trace &
                                   (obj,hdi(:,obj%ikeep),tri(:,obj%ikeep))
                     ntr = 1
                     return
                end if

                if (condition == CONDITION_SKIP) then
                     obj%kount_skipped = obj%kount_skipped + 1
                     cycle
                end if

                if (condition == CONDITION_PASS) then
                     hdo(1:obj%nwih,1) = hdi(1:obj%nwih,obj%ikeep)
                     tro(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%ikeep)
                     obj%kount_passed  = obj%kount_passed + 1
                     obj%kount_output  = obj%kount_output + 1
                     hdo(1,1)          = obj%kount_output
                     ntr = 1
                     return
                end if

                ! now condition == CONDITION_ADD.
                call comp_add_to_summed_trace &
                                   (obj,hdi(:,obj%ikeep),tri(:,obj%ikeep))
           end do

!----------THERE ARE NO MORE INPUT TRACES.

      else if (obj%nhdsum == 0) then
           call comp_wrapup (obj)
           ntr = NO_MORE_TRACES
      else
           call comp_get_output_trace   (obj,hdo(:,1),tro(:,1))
           call comp_clear_summed_trace (obj)     ! sets obj%nhdsum to zero.
           ntr = 1
      end if
      return
      end subroutine comp
 

!!--------------------------- new input trace ----------------------------!! 
!!--------------------------- new input trace ----------------------------!! 
!!--------------------------- new input trace ----------------------------!! 

! for mode COUNT:
! returns CONDITION_PASS if this trace is to be passed out unchanged.
! returns CONDITION_LAST if this trace ends a new composite.
! returns CONDITION_ADD  if this trace is to be added to the same composite.

! for mode HDR:
! returns CONDITION_SKIP if this trace is not to be used.
! returns CONDITION_NEW  if this trace starts a new composite.
! returns CONDITION_ADD  if this trace is to be added to the same composite.
! returns CONDITION_ADD  if this is the first input trace to be used.

! member variables changed in this routine:
!   kount_input   bin1_keep   bin2_keep


      subroutine comp_new_input_trace (obj,hdi,condition)
      implicit none
      type(comp_struct),intent(inout) :: obj                      ! arguments
      double precision ,intent(in)    :: hdi(:)                   ! arguments
      integer          ,intent(out)   :: condition                ! arguments
      integer                         :: bin1,bin2                ! local
      real                            :: center1,center2          ! local
      integer                         :: current,increment,count  ! local

      obj%kount_input = obj%kount_input + 1

      if (obj%mode == 'COUNT') then

            current   = obj%kount_input - obj%skip_init - 1
            increment = obj%num_comp + obj%num_skip
            count     = mod(current, increment)
             

            if      (obj%kount_input <= obj%skip_init) then
                              condition = CONDITION_PASS
            else if (obj%kount_composited >= obj%tot_comp) then
                              condition = CONDITION_PASS
            else if (count == obj%num_comp - 1) then
                              condition = CONDITION_LAST
            else if (count >= obj%num_comp) then
                              condition = CONDITION_PASS
            else if (obj%kount_composited == obj%tot_comp - 1) then
                              condition = CONDITION_LAST
            else
                              condition = CONDITION_ADD 
            end if

      else if (obj%hdr2 > 0) then   ! obj%mode == 'HDR' and using 2 headers.

            bin1    = mth_bin_number (obj%init1, obj%inc1, real(hdi(obj%hdr1)))
            bin2    = mth_bin_number (obj%init2, obj%inc2, real(hdi(obj%hdr2)))
            center1 = mth_bin_center (obj%init1, obj%inc1, bin1)
            center2 = mth_bin_center (obj%init2, obj%inc2, bin2)

            if      (bin1 < 1 .or. bin1 > obj%tot1) then
                              condition = CONDITION_SKIP
            else if (bin2 < 1 .or. bin2 > obj%tot2) then
                              condition = CONDITION_SKIP
            else if (abs(hdi(obj%hdr1) - center1) > obj%half1) then
                              condition = CONDITION_SKIP
            else if (abs(hdi(obj%hdr2) - center2) > obj%half2) then
                              condition = CONDITION_SKIP
            else if (obj%kount_composited == 0) then
                              obj%bin1_keep = bin1
                              obj%bin2_keep = bin2
                              condition = CONDITION_ADD 
            else if (bin1 == obj%bin1_keep .and. bin2 == obj%bin2_keep) then
                              condition = CONDITION_ADD 
            else
                              obj%bin1_keep = bin1
                              obj%bin2_keep = bin2
                              condition = CONDITION_NEW
            end if

      else                          ! obj%mode == 'HDR' and using 1 header.

            bin1    = mth_bin_number (obj%init1, obj%inc1, real(hdi(obj%hdr1)))
            center1 = mth_bin_center (obj%init1, obj%inc1, bin1)

            if      (bin1 < 1 .or. bin1 > obj%tot1) then
                              condition = CONDITION_SKIP
            else if (abs(hdi(obj%hdr1) - center1) > obj%half1) then
                              condition = CONDITION_SKIP
            else if (obj%kount_composited == 0) then
                              obj%bin1_keep = bin1
                              condition = CONDITION_ADD 
            else if (bin1 == obj%bin1_keep) then
                              condition = CONDITION_ADD 
            else
                              obj%bin1_keep = bin1
                              condition = CONDITION_NEW
            end if

      end if

      if (DEBUG_PRINT) then
            if (obj%kount_input == 1) write(obj%lun,1000)
            if (obj%mode == 'COUNT') then
                 write(obj%lun,2000) nint(hdi(1)),nint(hdi(5)), &
                                     0.0,0.0,hdi(25),conditions(condition)
            else if (obj%hdr2 > 0) then
                 write(obj%lun,2000) nint(hdi(1)),nint(hdi(5)),   &
                                     hdi(obj%hdr1),hdi(obj%hdr2), &
                                     hdi(25),conditions(condition)
            else
                 write(obj%lun,2000) nint(hdi(1)),nint(hdi(5)), &
                                     hdi(obj%hdr1),0.0,hdi(25), &
                                     conditions(condition)
            end if
1000       format (10x,'hdr1  hdr5  hdr_pri hdr_sec   hdr25  condition')
2000       format (' input ',2i6,1x,2f8.1,f9.3,2x,a4)
      end if
      return
      end subroutine comp_new_input_trace
 

!!----------------------- clear summed trace ------------------------------!! 
!!----------------------- clear summed trace ------------------------------!! 
!!----------------------- clear summed trace ------------------------------!! 

! member variables changed in this routine:
!   nhdsum   ntrsum   hdsum(:)   trsum(:)   nfold


      subroutine comp_clear_summed_trace (obj)
      implicit none
      type(comp_struct),intent(inout) :: obj           ! arguments

      obj%nhdsum   = 0
      obj%ntrsum   = 0
      obj%hdsum(:) = 0.0
      obj%trsum(:) = 0.0
      obj%nfold    = 0
      return
      end subroutine comp_clear_summed_trace
 

!!---------------------- add to summed trace ------------------------------!! 
!!---------------------- add to summed trace ------------------------------!! 
!!---------------------- add to summed trace ------------------------------!! 

! member variables changed in this routine:
!   kount_composited   nhdsum   ntrsum   hdsum(:)   trsum(:)   nfold


      subroutine comp_add_to_summed_trace (obj,hdi,tri)
      implicit none
      type(comp_struct),intent(inout) :: obj           ! arguments
      double precision ,intent(in)    :: hdi(:)        ! arguments
      real             ,intent(in)    :: tri(:)        ! arguments

      obj%kount_composited = obj%kount_composited + 1
      obj%nhdsum           = obj%nhdsum           + 1
      obj%hdsum(:)         = obj%hdsum(:)         + hdi(1:obj%nwih)
      if (hdi(25) > 0.0) then
           obj%ntrsum   = obj%ntrsum   + 1
           obj%trsum(:) = obj%trsum(:) + tri(1:obj%ndpt)
           obj%nfold    = obj%nfold    + max(nint(hdi(5)),1)
      end if
      return
      end subroutine comp_add_to_summed_trace
 

!!--------------------------- get output trace ----------------------------!! 
!!--------------------------- get output trace ----------------------------!! 
!!--------------------------- get output trace ----------------------------!! 

! member variables changed in this routine:
!   kount_stacked   kount_output   current_group   current_channel   bin1_out


      subroutine comp_get_output_trace (obj,hdo,tro)
      implicit none
      type(comp_struct),intent(inout) :: obj           ! arguments
      double precision ,intent(out)   :: hdo(:)        ! arguments
      real             ,intent(out)   :: tro(:)        ! arguments
      integer                         :: bin1          ! local

      if (obj%nhdsum > 0) then
           hdo(1:obj%nwih) = obj%hdsum(:) / obj%nhdsum
      else
           hdo(1:obj%nwih) = 0.0
      end if

      if (obj%ntrsum > 0) then
           tro(1:obj%ndpt) = obj%trsum(:) / obj%ntrsum
      else
           tro(1:obj%nwih) = 0.0
      end if

      obj%kount_stacked = obj%kount_stacked + 1
      obj%kount_output  = obj%kount_output  + 1
      hdo(1)            = obj%kount_output
      hdo(5)            = obj%nfold
      hdo(25)           = lav(tro,obj%ndpt)

      if (obj%mode == 'HDR') then
           bin1 = mth_bin_number (obj%init1, obj%inc1, real(hdo(obj%hdr1)))

           if (obj%kount_output == 1) then
                obj%current_group   = 1
                obj%current_channel = 1
           else if (bin1 == obj%bin1_out) then
                obj%current_channel = obj%current_channel + 1
           else
                obj%current_group   = obj%current_group + 1
                obj%current_channel = 1
           end if

           hdo(3)       = obj%current_group
           hdo(4)       = obj%current_channel
           obj%bin1_out = bin1
      end if

      if (DEBUG_PRINT) then
            if (obj%kount_output == 1) write(obj%lun,1000)
            if (obj%mode == 'COUNT') then
                 write(obj%lun,2000) nint(hdo(1)),nint(hdo(5)), &
                                     0.0,0.0,hdo(25),hdo(3),hdo(4)
            else if (obj%hdr2 > 0) then
                 write(obj%lun,2000) nint(hdo(1)),nint(hdo(5)),   &
                                     hdo(obj%hdr1),hdo(obj%hdr2), &
                                     hdo(25),hdo(3),hdo(4)
            else
                 write(obj%lun,2000) nint(hdo(1)),nint(hdo(5)), &
                                     hdo(obj%hdr1),0.0,hdo(25), &
                                     hdo(3),hdo(4)
            end if
1000       format (63x,'hdr1  hdr5  hdr_pri hdr_sec   hdr25    hdr3    hdr4')
2000       format (53x,'output ',2i6,1x,2f8.1,f9.3,2f8.1)
      end if
      return
      end subroutine comp_get_output_trace


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine comp_wrapup (obj)
      implicit none
      type(comp_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

   call pc_print (' ')
   call pc_print ('++++++++++++++++ COMP_WRAPUP ++++++++++++++++')
   call pc_print ('total number of input traces       =',obj%kount_input)
   call pc_print ('number of input traces skipped     =',obj%kount_skipped)
   call pc_print ('number of input traces passed thru =',obj%kount_passed)
   call pc_print ('number of input traces composited  =',obj%kount_composited)
   call pc_print ('number of composite traces output  =',obj%kount_stacked)
   call pc_print ('total number of output traces      =',obj%kount_output)
   call pc_print ('++++++++++++++++ COMP_WRAPUP ++++++++++++++++')
   call pc_print (' ')

      if (obj%kount_skipped + obj%kount_passed + obj%kount_composited &
                       /= obj%kount_input) then
           call pc_error ('skipped + passed + composited /= input')
      end if

      if (obj%kount_passed + obj%kount_stacked /= obj%kount_output) then
           call pc_error ('passed + stacked /= output')
      end if
      return
      end subroutine comp_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module comp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

