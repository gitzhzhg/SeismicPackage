!<CPS_v1 type="PROCESS"/>

!!----------------------------- tredit.f90 ---------------------------------!!
!!----------------------------- tredit.f90 ---------------------------------!!
!!----------------------------- tredit.f90 ---------------------------------!!

!<-- This documentation header was last revised by S Cook on 2001-04-25. />

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
! Name       : TREDIT    (TRace EDit)   [Includes former BYFIL]
! Category   : headers
! Written    : 1994-12-22   by: Kruger Corn and Tom Stoeckley
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Edit traces according to information in a TREDIT file or BYFIL
!              spreadsheet.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! TREDIT can operate in the TREDIT mode or the BYFIL mode.
!
! In the TREDIT mode, traces are edited according to information residing in a 
! trace-edit file previously created in CBYT.  This file specifies by header 
! words which traces are to be selected and the action to be taken on each 
! selected trace.  Allowed actions are
!
!                    KILL, DELETE, RP, and FLAG.
!
! In the BYFIL mode, traces are edited according to information residing in a 
! list or "spreadsheet" detailing actions and identifying header word numbers.
! This spreadsheet is part of the new CPS front end, and closely resembles the
! GUI that the user encounters in CBYT when creating a '.tred' file.  One minor
! difference is that a '.tred' file has DO/UNDO flags, whereas the BYFIL GUI
! has no reason to include UNDO functionality.
!
! The same actions are allowed in either mode.  Any or all of the actions can be
! used, and they can be used in any order.
!
! In either mode, if more than one action is to be taken on the same trace, the 
! following priorities will apply:
!
!                    DELETE will override KILL.
!                    KILL will override REVERSE POLARITY.
!                    FLAG is ignored if trace is to be deleted.
!
!
! EXAMPLE:
!
! Suppose HDR_FLAG is set to 48, and the spreadsheet (or TREDIT file) looks
! like this:
!
!   ACTION HDR_A A_BEG  A_END   HDR_B B_BEG  B_END   HDR_C C_BEG   C_END   
!
!      REV     9    10     10      10     2     12
!      DEL     9    24     40      10     6      6      37     2      22
!     KILL     9    30     32      10    47     48
!     KILL     9    39     40      10    17     18      37    20      34
!     FLAG     9    45     45      10     1      1
!      DEL     9     2      2
!     KILL    25    35 999999
!
! Most cards are using header word 9 (original sort group) for file selection.
! Most cards are using header word 10 for trace selection within that group.
! Some cards use header word 37 (shotpoint for 2D) to further subset the data.
!
! This set of example cards will:
!   -- Reverse the polarity of traces 2-12 in sort group 10.
!   -- Delete trace 6 in sort groups 24-40, shotpoints 2-22.
!   -- Kill traces 47 and 48 in sort groups 30-32.
!   -- Kill traces 17 and 18 in sort groups 39-40, shotpoints 20-34.
!   -- Flag trace 1 in sort group 45.  Header word 48 will be set to 1, and set
!       to zero for all other traces.
!   -- Delete sort group 2 entirely.
!   -- Kill all traces having LAV greater than or equal to 35 (and less than or
!       equal to 999999.
!
!
! Note 1:
!
! The new CPS version has slightly more general capability than the old BYFIL.
! The old version required than the header words used for selecting traces were
! always the same (typically HDR_A = 9 in this example).  This does not now have
! to be the case.
!
! Note 2:
!
! The old BYFIL program allowed the user to use either ascending or descending
! edit ranges.  This new version allows ascending ranges only.  In other words,
! an action card like
!
!      KILL    9    40    30
!
! is not allowed.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! If you're using BYFIL mode, the quickest way to fill in the spreadsheet is to
!   1. Set the DEFAULT_ACTION to the most common ACTION you're using, e.g. KILL.
!   2. Enter values into HDR_A column (ACTION is filled in automatically).
!   3. Enter/overwrite values in other columns as needed.
!   4. To overwrite an entry in the ACTION column, highlight it and type the
!      first letter, e.g. 'k' or 'K' for 'KILL' (followed by RETURN).
!   5. Hit the Apply button periodically to verify/update your input.
!
! A hard-coded limit of 4000 action cards exists in the program design.  At
! least one user has reported problems with the old version using as few as
! 1600 TREDIT actions.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process may alter input traces.
!
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
! NDPT      number of samples per trace             used but not changed
! GATHERED  true or false                           used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description          Action taken
! ----    -----------          ------------
!         HDR_A                Used for selecting traces in BYFIL mode
!         HDR_B                Used for selecting traces in BYFIL mode
!         HDR_C                Used for selecting traces in BYFIL mode
!         HDR_FLAG             Changed if flagging actions are requested
!         HDR_SEQUENCE         Changed if trace deletions occur
!         HDR_CURRENT_CHANNEL  Changed if deletions occur AND data is gathered
! 
! Normally header words 9 and 10 are used to identify traces, but the user can
! select based on any header word.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          TREDIT REVISION HISTORY             
!
!     Date       Author     Description
!     ----       ------     -----------
! 11. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
!010. 2006-01-10  B. Menger   Removed Unused Variables.
! 9.  2004-02-02 SMCook     Fixed problem wherein HDR_FLAG word was being
!                            zeroed when flagging was not requested.
! 8.  2001-04-30 SMCook     Changed GUI slightly.
!                           Default header words in BYFIL mode now default to
!                            value from previous line.
! 7.  2001-01-03 SMCook     Changed wrapped_up to skip_wrapup.
! 6.  2000-09-06 SMCook     Fixed memory leak (deallocation was conditional
!                           erroneously in the tredit_delete function), plus
!                           made very minor documentation changes.
! 5.  2000-08-15 SMCook     Disallowed rcp syntax (user@node:).  Changed VERBOSE
!                           to OPT_PRINT for consistency with other processes.
!                           Improved advice for users. Fixed bug related to
!                           attempting to read byfil 'actions' when in TREDIT
!                           mode.  Removed extraneous pc_put call located in a
!                           trap.  Improved comments to enhance readability.
!                           Improved user printout (involved tredit_crou also).
!                           Changed ACTION_MODE to DEFAULT_ACTION.
! 4.  2000-07-26 SMCook     Fixed NTR/NEED_REQUEST bug affecting delete logic.
!                           Fixed automatic mode detection.
!                           Added more useful printed info to report file.
!                           Added VERBOSE combo box to allow suppressing info.
! 3.  2000-06-20 SMCook     (MAJOR) Converted from old system, merging the
!                           functionality of the old TREDIT and BYFIL.
! 2.  1998-11-20 Goodger    Begin using fortran90 compiler.
! 1.  1994-12-22 Corn &
!                 Stoeckley Initial version.
!
!
!                          BYFIL REVISION HISTORY
! 
!10.  2000-06-20 Cook       (MAJOR) No longer supported as separate process.
! 9.  1998-11-25 Goodger    Begin using the fortran90 compiler.    
! 8.  1996-09-16 Goodger    Do not write byfile cards to history file.
! 7.  1994-03-07 Cooper     Fixed problem with DCODE format on first card
! 6.  1994-03-07 Cooper     If HWFLG is non 0,the header word is reset to
!                           0 if not selected by the FLAG option or set to
!                           1.0 if the trace is selected.
!                           If HWFLG is 0, FLAG option not allowed.
! 5.  1994-03-03 Cooper     Changed method of getting the parameters in
!                           due to DCODE & NCODE limits.
! 4.  1994-03-02 Cooper     Added the option to FLAG a trace.
! 3.  1994-03-01 Cooper     Increase number of actions from 200 to 600
! 2.  1994-02-01 Cooper     Added a third selection parameter(HWSHT)
! 1.  1993-10-18 Cooper     Original version.
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
!                     SPECIAL COMPILING REQUIREMENTS        
!
!
! other files are:
!
!     tredit_crou.c
!
!     c2f_interface.h
!
!     tredfile.h
!     tredfile.c
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
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
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
! No "algorithms" per se are involved.  This module doesn't operate on trace
! samples other than to kill (zero) them or reverse their polarity.  These
! operations are largely self-explanatory.
!
! Attention must be paid to NTR and NEED_REQUEST since this process may delete
! traces.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! Some of the TREDIT code is written in C and could be shared with CBYT.
! Specifically, the file
!
!     tredfile.c
!
! represents a tredit disk file, and has the needed i/o functions.  (The
! tredit.f90 module only reads .tred files--it has no reason to write them.)
!
!
! In order to share/isolate code to the fullest extent possible, the approach
! taken herein is simply to funnel the BYFIL GUI information to the existing C
! TREDIT logic.  In TREDIT mode, a data structure is created from information
! read in from a disk file, normally 'something.tred'.  In BYFIL mode, the
! data structure is instead created from the GUI arrays themselves in a
! function that "mimics" the disk file read.
!
!     TREDIT mode (old code):  tredit_crou_read_file()
!
!     BYFIL  mode (new code):  tredit_crou_mimic_read_file()
!
!
! The primary trapping mechanism used herein is the "screen trap".  Extensive
! trapping was found to be annoying, tending to produce error messages while the
! user was still entering data.  Two simple functions contain the majority of
! the verification code:
!
!     TREDIT mode:  tredit_verify_screen_tredit()
!
!     BYFIL  mode:  tredit_verify_screen_byfile()
!
! (only one of these is called, depending on the mode).
!
!
! Note:
!
!  At the time of this writing, the old BYFIL code appeared to have at least
!  2 bugs.  One was a clear typo/negligence error of unknown significance.  The
!  other was causing the order of the "action cards" to be significant.  No
!  effort was made to retain these 2 features--the behavior is supposed to be
!  completely independent of the "action card" order.
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TREDIT Process/NC=80>
!
! ~~~~~~~~~~~~~~~TRace EDIT (Includes former BYFIL) Process~~~~~~~~~~~~~~~~~~~
! ~~~~Edit traces according to information in a TREDIT file or BYFIL cards~~~~
!
!
! ~~~~~~~~PATHNAME (file name) is only applicable to TREDIT mode.~~~~~~~~~~~~~
! PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
! ~~~~~~~MODE is automatically set to TREDIT if a PATHNAME is entered~~~~~~~~~
! ~~~~~~~(and automatically set to BYFIL if PATHNAME is blank or NONE).~~~~~~~
! MODE=`CCCCC
!
!
! ~~~~~~~~~~~~~HDR_FLAG is ignored if no flagging is specified.~~~~~~~~~~~~~~~
! HDR_FLAG=`II
!
!
! ~~~OPT_PRINT causes the editing details to be included in the .rpt file.~~~~
! OPT_PRINT=`CC
!
!
! ~~~~The following "action spreadsheet" is only applicable to BYFIL mode.~~~~
! DEFAULT_ACTION=`CCCCC
!
! ACTION HDR_A A_BEG   A_END   HDR_B B_BEG   B_END   HDR_C C_BEG   C_END   
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
! `SSSSSS`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF`IIIII`FFFFFFF`FFFFFFF
!
!
!<PARMS PATHNAME[/ML=128/XST]>
!<PARMS ACTION_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the trace-edit file (externally created in CBYT). </Tip>
! Default =  
! Allowed = character
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Mode of operation -- TREDIT or BYFIL. </Tip>
! Default = BYFIL 
! Allowed = TREDIT
! Allowed = BYFIL
! MODE is a display-only parameter that is set to TREDIT if an entry is
! made in PATHNAME.  Otherwise, MODE is set to BYFIL.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word to use for flagging traces in either mode. </Tip>
! Default = 48 
! Allowed = 1 - NWIH
! TREDIT will insert a flag in header word HDR_FLAG that can be recognized by 
! subsequent processes.  TREDIT will put a flag in header word HDR_FLAG for a 
! trace if that trace is identified for flagging by the trace-edit file in 
! TREDIT mode or the action/header word list in BYFIL mode.
!</Help>
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Whether to include ACTION details in the .rpt file. </Tip>
! Default = YES
! Allowed = YES/NO
! Warning:  In TREDIT mode, this could produce a fairly large .rpt file.
!</Help>
!
!<Help KEYWORD="DEFAULT_ACTION">
!<Tip> Linked array of actions to take on selected traces. </Tip>
! Default = KILL
! Allowed = KILL
! Allowed = DELETE
! Allowed = RP
! Allowed = FLAG
! Any or all of the ACTIONS can be used in the action/header word list and in
! any order.
!</Help>
!
!<Help KEYWORD="ACTION">
!<Tip> DELETE, KILL, RP, OR FLAG. </Tip>
! Default =
! Allowed = char (linked array)
!</Help>
!
!<Help KEYWORD="HDR_A">
!<Tip> Header word A for selecting traces. </Tip>
! Default =
! Allowed = 1 - NWIH (linked array)
!</Help>
!
!<Help KEYWORD="A_BEG">
!<Tip> Starting value of header word A for selecting traces. </Tip>
! Default =
! Allowed = real (linked array)
!</Help>
!
!<Help KEYWORD="A_END">
!<Tip> Ending value of header word A for selecting traces. </Tip>
! Default =
! Allowed = real (linked array)
!</Help>
!
!<Help KEYWORD="HDR_B">
!<Tip> Header word B for selecting traces. </Tip>
! Default =
! Allowed = 0 - NWIH (linked array)
! If HDR_B = 0, then traces will be selected only with HDR_A.
!</Help>
!
!<Help KEYWORD="B_BEG">
!<Tip> Starting value of header word B for selecting traces. </Tip>
! Default =
! Allowed = real (linked array)
!</Help>
!
!<Help KEYWORD="B_END">
!<Tip> Ending value of header word B for selecting traces. </Tip>
! Default =
! Allowed = real (linked array)
!</Help>
!
!<Help KEYWORD="HDR_C">
!<Tip> Header word C for selecting traces. </Tip>
! Default =
! Allowed = 0 - NWIH (linked array)
! If HDR_C = 0, then traces will be selected only with HDR_A and HDR_B.
!</Help>
!
!<Help KEYWORD="C_BEG">
!<Tip> Starting value of header word C for selecting traces. </Tip>
! Default =
! Allowed = real (linked array)
!</Help>
!
!<Help KEYWORD="C_END">
!<Tip> Ending value of header word C for selecting traces. </Tip>
! Default = 0
! Allowed = real (linked array)
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!


      module tredit_module
      use pc_module
      use mem_module
      use named_constants_module
!      use finquire_module      ! eliminated check for existence of .tred file
      use pathcheck_module

      implicit none

      private
      public :: tredit_create
      public :: tredit_initialize
      public :: tredit_update
      public :: tredit_delete
!<execute_only>
      public :: tredit            ! main execution (trace processing) routine.
      public :: tredit_wrapup
!</execute_only>


      character(len=100),public,save :: TREDIT_IDENT = &
       '$Id: tredit.f90,v 1.11 2006/09/11 13:15:52 Stoeckley prod sps $'

!!------------------------ parameter structure -----------------------------!!
!!------------------------ parameter structure -----------------------------!!
!!------------------------ parameter structure -----------------------------!!

      type,public :: tredit_struct              
 
        private
        logical               :: skip_wrapup       ! wrapup flag.

        integer               :: nwih,ndpt         ! globals.
        logical               :: gathered          ! globals.

        logical               :: tredfile_has_been_read

        type(CPOINTER)        :: tredfile_pointer  ! portable pointer?

        integer               :: flagging_requested! user doesn't set this

        integer               :: hdr_flag          ! byfil mode (from GUI)

        integer               :: ncards            ! tredit mode
        integer               :: nactions          ! byfil mode

        character(len=FILENAME_LENGTH) &
                              :: pathname          ! TREDIT file name (from GUI)

        character(len=6)      :: mode              ! BYFIL or TREDIT
        character(len=6)      :: default_action    ! KILL, DELETE, RP, FLAG
        logical               :: opt_print

        character(len=6), pointer &
                              :: action(:)         ! byfil mode (from GUI)

        integer, pointer      :: hdr_a(:)          ! byfil mode (from GUI)
        integer, pointer      :: hdr_b(:)          ! byfil mode (from GUI)
        integer, pointer      :: hdr_c(:)          ! byfil mode (from GUI)

        real, pointer         :: a_beg(:)          ! byfil mode (from GUI)
        real, pointer         :: b_beg(:)          ! byfil mode (from GUI)
        real, pointer         :: c_beg(:)          ! byfil mode (from GUI)

        real, pointer         :: a_end(:)          ! byfil mode (from GUI)
        real, pointer         :: b_end(:)          ! byfil mode (from GUI)
        real, pointer         :: c_end(:)          ! byfil mode (from GUI)

        integer               :: sequential_count,input_count
        integer               :: flag_count, rev_count, del_count, kill_count

      end type tredit_struct


!!------------------------------ interfaces --------------------------------!!
!!------------------------------ interfaces --------------------------------!!
!!------------------------------ interfaces --------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(tredit_struct),pointer,save :: object      ! needed for traps.

      integer,parameter       :: mode_noptions = 2
      character(len=6),save   :: mode_options(mode_noptions)

      integer,parameter       :: action_noptions = 4
      character(len=6),save   :: action_options(action_noptions)

      data mode_options /'BYFIL', 'TREDIT'/
      data action_options /'KILL', 'DELETE', 'RP', 'FLAG'/

      contains


!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!

      subroutine tredit_create (obj)
      implicit none
      type(tredit_struct),pointer :: obj       ! arguments


      allocate (obj)

      nullify (obj%action)

      nullify (obj%hdr_a)
      nullify (obj%a_beg)
      nullify (obj%a_end)

      nullify (obj%hdr_b)
      nullify (obj%b_beg)
      nullify (obj%b_end)

      nullify (obj%hdr_c)
      nullify (obj%c_beg)
      nullify (obj%c_end)

      call tredit_initialize (obj)
      return
      end subroutine tredit_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine tredit_delete (obj)
      implicit none
      type(tredit_struct),pointer :: obj

!<execute_only>
      call tredit_wrapup (obj)
!</execute_only>

      call tredit_deallocate_spreadsheet(obj)

      deallocate(obj)

      end subroutine tredit_delete

!-------------------------------------------------------------------------------
! Subroutine to deallocate arrayset.
!-------------------------------------------------------------------------------
      subroutine tredit_deallocate_spreadsheet (obj)
      implicit none
      type(tredit_struct),pointer :: obj              ! arguments


      call mem_free (obj%action)

      call mem_free (obj%hdr_a)
      call mem_free (obj%a_beg)
      call mem_free (obj%a_end)

      call mem_free (obj%hdr_b)
      call mem_free (obj%b_beg)
      call mem_free (obj%b_end)

      call mem_free (obj%hdr_c)
      call mem_free (obj%c_beg)
      call mem_free (obj%c_end)

      end subroutine tredit_deallocate_spreadsheet


!-------------------------------------------------------------------------------
! Subroutine to allocate arrayset.
!-------------------------------------------------------------------------------
      subroutine tredit_allocate_spreadsheet (obj)
      implicit none
      type(tredit_struct),pointer :: obj

      integer  :: n1,n2,n3,n4,n5,n6,n7,n8,n9

      n1 = obj%nactions
      n2 = n1
      n3 = n1
      n4 = n1
      n5 = n1
      n6 = n1
      n7 = n1
      n8 = n1
      n9 = n1


      call pc_alloc ('action', obj%action, obj%nactions, tredit_actions_trap)

      call pc_alloc ('hdr_a' , obj%hdr_a , n1, tredit_insert_row_trap)
      call pc_alloc ('a_beg' , obj%a_beg , n2)
      call pc_alloc ('a_end' , obj%a_end , n3)

      call pc_alloc ('hdr_b' , obj%hdr_b , n4, tredit_insert_row_trap)
      call pc_alloc ('b_beg' , obj%b_beg , n5)
      call pc_alloc ('b_end' , obj%b_end , n6)

      call pc_alloc ('hdr_c' , obj%hdr_c , n7, tredit_insert_row_trap)
      call pc_alloc ('c_beg' , obj%c_beg , n8)
      call pc_alloc ('c_end' , obj%c_end , n9)


      if(n1 /= obj%nactions .or. &
         n2 /= obj%nactions .or. &
         n3 /= obj%nactions .or. &
         n4 /= obj%nactions .or. &
         n5 /= obj%nactions .or. &
         n6 /= obj%nactions .or. &
         n7 /= obj%nactions .or. &
         n8 /= obj%nactions .or. &
         n9 /= obj%nactions) then

         call pc_error('TREDIT: Mismatch with nactions varible.')

         obj%nactions = min(n1,n2,n3,n4,n5,n6,n7,n8,n9)

       end if

      end subroutine tredit_allocate_spreadsheet


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine tredit_initialize (obj)
      implicit none
      type(tredit_struct),intent(inout) :: obj


      obj%nwih         = 0
      obj%ndpt         = 0
      obj%gathered     = .false.

      obj%pathname     = CNIL

      obj%mode         = CNIL

      obj%hdr_flag     = 48

      obj%opt_print    = .true.

      obj%default_action = 'KILL'

      obj%ncards       = 0
      obj%nactions     = 0

      obj%sequential_count=0
      obj%input_count=0
      obj%del_count  =0
      obj%kill_count =0
      obj%rev_count  =0
      obj%flag_count =0

      obj%tredfile_has_been_read = .false.

!call update
      call tredit_update (obj)

      return
      end subroutine tredit_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine tredit_update (obj)
      implicit none
      type(tredit_struct),intent(inout),target :: obj



      character(len=120)                :: mesg
      integer                           :: hpath(120),hmesg(120)
      integer,pointer                   :: hactions(:)


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("action_arrayset", (/  &
                                    "action",              &
                                    "hdr_a ",              &
                                    "a_beg ",              &
                                    "a_end ",              &
                                    "hdr_b ",              &
                                    "b_beg ",              &
                                    "b_end ",              &
                                    "hdr_c ",              &
                                    "c_beg ",              &
                                    "c_end " /))

!---- get globals
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('gathered', obj%gathered)

!---- if pathname is attempted, put GUI into TREDIT mode
      call pc_get ('pathname', obj%pathname, tredit_pathname_trap)

!---- strip off rcp syntax
      obj%pathname = tredit_absolute_path(obj%pathname)

      if(obj%pathname /= CNIL .and. obj%pathname /= PATHCHECK_EMPTY) then
        obj%mode='TREDIT'
      else
        obj%mode='BYFIL'
      end if

      call pc_get ('hdr_flag'   , obj%hdr_flag)
      call pc_get ('opt_print'  , obj%opt_print)
      call pc_get ('default_action', obj%default_action)
!
!---- for byfil mode, linked arrays are needed
!---  for tredit mode, they'll be ignored
!
      call tredit_allocate_spreadsheet(object)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

!---- verify screen, depending on the mode
      if(pc_verify_screen('screen1')) then
        if(obj%mode == 'BYFIL') then
          call tredit_verify_screen_byfil()
        else if(obj%mode == 'TREDIT') then
          call tredit_verify_screen_tredit()
        else
          call pc_error('TREDIT: program logic error--invalid mode')
        end if
      end if

!!----------------------- call processes internally ------------------------!!
!!----------------------- call processes internally ------------------------!!
!!----------------------- call processes internally ------------------------!!


!!-------------------------- write parameters ------------------------------!!
!!-------------------------- write parameters ------------------------------!!
!!-------------------------- write parameters ------------------------------!!

      call pc_put_control ('need_request',.true.)

      call pc_put ('pathname', obj%pathname)

      call pc_put_options_field('mode'       ,mode_options   ,mode_noptions)
      call pc_put ('mode'       , obj%mode)
      call pc_put_sensitive_field_flag('mode', .false.)

      call pc_put ('hdr_flag', obj%hdr_flag)

      call pc_put ('opt_print'  , obj%opt_print)

      call pc_put_options_field( &
        'default_action',action_options ,action_noptions)
      call pc_put ('default_action', obj%default_action)


      call pc_put ('action' , obj%action, obj%nactions)

      call pc_put ('hdr_a'  , obj%hdr_a, obj%nactions)
      call pc_put ('a_beg'  , obj%a_beg, obj%nactions)
      call pc_put ('a_end'  , obj%a_end, obj%nactions)

      call pc_put ('hdr_b'  , obj%hdr_b, obj%nactions)
      call pc_put ('b_beg'  , obj%b_beg, obj%nactions)
      call pc_put ('b_end'  , obj%b_end, obj%nactions)

      call pc_put ('hdr_c'  , obj%hdr_c, obj%nactions)
      call pc_put ('c_beg'  , obj%c_beg, obj%nactions)
      call pc_put ('c_end'  , obj%c_end, obj%nactions)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      obj%tredfile_has_been_read = .false.

!<execute_only>

      if (pc_do_not_process_traces()) return

!---- permanent memory allocation goes here

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!
!---- STUFF TO DO ONCE (confirmed with 'tredfile_has_been_read')
!
!---- Either read the tredit file or, for BYFIL mode, simulate reading one
!
!---- read_file and mimic_read_file notify whether flagging is requested
!     (a value of 1 means flagging was requested)
!
      if(obj%mode == 'BYFIL') then
        call pc_print ('BYFIL mode, nactions=', obj%nactions)

        if(obj%nactions > 0) then

!-------- hactions (hollerith actions) allocated
          allocate(hactions(obj%nactions))

          call string_cc2hh( &
            tredit_first_chars_only(obj%nactions,obj%action), hactions);

          call tredit_crou_mimic_read_file( &
            obj%tredfile_pointer, hmesg, &
            obj%nactions, hactions, &
            obj%hdr_a, obj%a_beg, obj%a_end, &
            obj%hdr_b, obj%b_beg, obj%b_end, &
            obj%hdr_c, obj%c_beg, obj%c_end, &
            obj%flagging_requested)

!-------- hactions deallocated
          call mem_free(hactions)

        end if

      else if(obj%mode == 'TREDIT') then
        call pc_print ('TREDIT mode, file=',obj%pathname)

        call string_cc2hh(obj%pathname,hpath)

        call tredit_crou_read_file( &
          obj%tredfile_pointer, hmesg, &
          hpath, &
          obj%ncards, &                      ! ncards is like nactions
          obj%flagging_requested)            ! but has no GUI implications

      else
        call pc_error ('TREDIT: bug found, bad mode (neither BYFIL nor TREDIT)')
        return
      end if

      call string_hh2cc(hmesg,mesg)

      if(mesg(1:5) == "Error") then
        call pc_error (mesg)
        return
      end if

      obj%tredfile_has_been_read = .true.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine tredit_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!-------------------------------------------------------------------------------
! 3 helper functions for traps--validity checks of hdr_a, hdr_b, and hdr_c.
!-------------------------------------------------------------------------------
!---- traps helper function #1 (hdr_a cannot be NIL)
      function tredit_valid_hdr_a(ival) result(is_valid)
      implicit none
      integer     :: ival
      logical     :: is_valid

      is_valid = .true.
      if(ival < 1 .or. ival > object%nwih) is_valid = .false.

      end function tredit_valid_hdr_a


!---- traps helper function #2 (hdr_b can be NIL)
      function tredit_valid_hdr_b(ival) result(is_valid)
      implicit none
      integer     :: ival
      logical     :: is_valid

      is_valid = .true.
      if(ival /= INIL) then
        if(ival < 1 .or. ival > object%nwih) is_valid = .false.
      end if

      end function tredit_valid_hdr_b


!---- traps helper function #3 (hdr_c can be NIL)
      function tredit_valid_hdr_c(ival) result(is_valid)
      implicit none
      integer     :: ival
      logical     :: is_valid

      is_valid = .true.
      if(ival /= INIL) then
        if(ival < 1 .or. ival > object%nwih) is_valid = .false.
      end if

      end function tredit_valid_hdr_c

!-------------------------------------------------------------------------------
! Pathname trap.
!-------------------------------------------------------------------------------
      subroutine tredit_pathname_trap()
      implicit none

      call pathcheck('pathname',object%pathname,'.tred',.false.)

!---- strip off rcp syntax
      object%pathname = tredit_absolute_path(object%pathname)

      if(object%pathname /= CNIL .and. object%pathname /= PATHCHECK_EMPTY) then
        object%mode='TREDIT'
      else
        object%mode='BYFIL'
      end if

      end subroutine tredit_pathname_trap

!-------------------------------------------------------------------------------
! Action cards trap.  User types first letter, trap fills in the rest.
!-------------------------------------------------------------------------------
      subroutine tredit_actions_trap ()
      implicit none
      character(len=6)         :: tmp
      integer                  :: i

!---- DO loop to replace letters with action words
      do i=1,object%nactions

!------ handle CNIL case
        if(object%action(i) == CNIL) then
          tmp = 'JUNK'
        else
          tmp = object%action(i)(1:1)
        end if

!------ force caps and check 4 possible cases
        call string_to_upper(tmp)

        if(     tmp == 'D') then
          object%action(i) = 'DELETE'

        else if(tmp == 'K') then
          object%action(i) = 'KILL'

        else if(tmp == 'R') then
          object%action(i) = 'RP'

        else if(tmp == 'F') then
          object%action(i) = 'FLAG'

        else
          object%action(i) = object%default_action   ! replace erroneous values

        end if

      end do


!---- autofill the first line
      if(object%nactions == 1 .and. object%hdr_a(1) == INIL) then
        object%hdr_a(1) = 9
        object%a_beg(1) = FNIL
        object%a_end(1) = FNIL
      end if


      end subroutine tredit_actions_trap


!-------------------------------------------------------------------------------
! Insert row trap.  Copies values from previous row.
!-------------------------------------------------------------------------------
      subroutine tredit_insert_row_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)    :: keyword
      integer         ,intent(in)    :: indx
      integer         ,intent(in)    :: action

      integer                  :: i


!---- when inserting, autofill hdr_x based on value on previous line

      if(action /= PC_INSERT) return

      if(object%nactions < 2) then
        return
      else
        i = object%nactions
      end if

      if(object%hdr_a(i) == INIL) object%hdr_a(i) = object%hdr_a(i-1)
      if(object%hdr_b(i) == INIL) object%hdr_b(i) = object%hdr_b(i-1)
      if(object%hdr_c(i) == INIL) object%hdr_c(i) = object%hdr_c(i-1)

!---- conceivably might want to copy all data from previous line
!      object%a_beg(i) = object%a_beg(i-1)
!      object%b_beg(i) = object%b_beg(i-1)
!      object%c_beg(i) = object%c_beg(i-1)

!      object%a_end(i) = object%a_end(i-1)
!      object%b_end(i) = object%b_end(i-1)
!      object%c_end(i) = object%c_end(i-1)


      end subroutine tredit_insert_row_trap


!-------------------------------------------------------------------------------
! Screen verification for BYFIL mode.
!-------------------------------------------------------------------------------
      subroutine tredit_verify_screen_byfil()
      implicit none
      logical  :: found_error
      character(len=6) :: tmp
      integer  :: i     ,min_allowed=0 

      if(object%mode == 'TREDIT') then
        call pc_error( &
          'TREDIT: logic error, mode in screen_byfil is ',object%mode)
        return
      end if

!---- warning for non-standard flag positions
      if (object%hdr_flag < 48 .or. object%hdr_flag > 55) then
         call pc_warning ( &
           'HDR_FLAG normally is between 48 and 55, inclusive, or exceeds 64.')
      end if

!---- error for invalid flag position
      if (object%hdr_flag < 1 .or. object%hdr_flag > object%nwih) then
        call pc_error( &
          'TREDIT: HDR_FLAG must be between 1 and NWIH, inclusive.')
      end if

!---- lack of effect
      if(object%nactions < 1) then
        call pc_warning ( &
          'TREDIT: Mode is BYFIL, but BYFIL action count = ', object%nactions)
      end if

!---- DO loop to verify action column
      do i=1,object%nactions

!------ handle CNIL case
        if(object%action(i) == CNIL) then
          tmp = 'JUNK'
        else
          tmp = object%action(i)(1:1)
        end if

        call string_to_upper(tmp)

        if(     tmp == 'D') then
          object%action(i) = 'DELETE'

        else if(tmp == 'K') then
          object%action(i) = 'KILL'

        else if(tmp == 'R') then
          object%action(i) = 'RP'

        else if(tmp == 'F') then
          object%action(i) = 'FLAG'

        else
          object%action(i) = object%default_action

        end if

      end do

!---- DO loop to verify action column
      do i=1,object%nactions

        if (.not. tredit_valid_hdr_a(object%hdr_a(i))) then
          call pc_error('TREDIT: Invalid value for HDR_A in row #', i)
        end if

        if (.not. tredit_valid_hdr_b(object%hdr_b(i))) then
            call pc_error ('TREDIT: Invalid value for HDR_B in row #', i)
        end if

        if (.not. tredit_valid_hdr_c(object%hdr_c(i))) then
            call pc_error ('TREDIT: Invalid value for HDR_C in row #', i)
        end if

      end do

!---- DO loop to verify 'spreadsheet' values in detail
      found_error=.false.

      do i=1,object%nactions

        if(.not. found_error) then        !avoid producing too many messages

          if(object%hdr_a(i) == INIL) then
            call pc_error('TREDIT: NIL value for HDR_A at row ',i)
            found_error=.true.
          else
            if(object%a_beg(i) == FNIL) then
             call pc_error('TREDIT: NIL value for A_BEG at row ',i)
             found_error=.true.
            end if
            if(object%a_end(i) == FNIL) then
              call pc_error('TREDIT: NIL value for A_END at row ',i)
              found_error=.true.
            end if
          end if

          if(object%hdr_b(i) /= INIL) then
            if(object%b_beg(i) == FNIL) then
             call pc_error('TREDIT: NIL value for B_BEG at row ',i)
             found_error=.true.
            end if
            if(object%b_end(i) == FNIL) then
              call pc_error('TREDIT: NIL value for B_END at row ',i)
              found_error=.true.
            end if
          end if

          if(object%hdr_c(i) /= INIL) then
            if(object%c_beg(i) == FNIL) then
             call pc_error('TREDIT: NIL value for C_BEG at row ',i)
             found_error=.true.
            end if
            if(object%c_end(i) == FNIL) then
              call pc_error('TREDIT: NIL value for C_END at row ',i)
              found_error=.true.
            end if
          end if

          if(object%a_end(i) < object%a_beg(i)) then
            call pc_error( &
              'TREDIT: A_END must be greater than or equal to A_BEG.')
            found_error=.true.
          end if

          if(object%b_end(i) < object%b_beg(i)) then
            call pc_error( &
              'TREDIT: B_END must be greater than or equal to B_BEG.')
            found_error=.true.
          end if

          if(object%c_end(i) < object%c_beg(i)) then
            call pc_error( &
              'TREDIT: C_END must be greater than or equal to C_BEG.')
            found_error=.true.
          end if

        end if

      end do

      end subroutine tredit_verify_screen_byfil

!-------------------------------------------------------------------------------
! Screen verification for TREDIT mode.
!-------------------------------------------------------------------------------
      subroutine tredit_verify_screen_tredit ()
      implicit none



!---- if(object%nactions > 0) then
      call pc_warning( &
        'TREDIT: Note -- BYFIL cards will be ignored since MODE = TREDIT.')

!---- strip off rcp syntax
      object%pathname = tredit_absolute_path(object%pathname)

!---- check for existence of file?  forget it, too many user complaints.
!      if(finquire_input(object%pathname) /= FINQUIRE_FOUND) then
!        call pc_warning('File not found warning: ', object%pathname)
!        call pc_warning('Please double-check PATHNAME before submitting job.')
!      end if

      end subroutine tredit_verify_screen_tredit

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine tredit(obj,ntr,hd,tr)
      implicit none

      type(tredit_struct),intent(inout) :: obj
      integer            ,intent(inout) :: ntr
      double precision   ,intent(inout) :: hd(:,:)
      real               ,intent(inout) :: tr(:,:)

      integer                           :: i,j

      integer                           :: previous_group  = -999
      integer                           :: current_channel = 1

      integer                           :: del_trace,kill_trace
      integer                           :: rev_trace,flag_trace


!---- bypass entirely if no more traces
      if (ntr == NO_MORE_TRACES) go to 1000

!---- PROCESS A GROUP OF TRACES.  Core logic is largely from old CSP TREDIT.
      j = 0 
      do i = 1, ntr

        obj%input_count = obj%input_count + 1

        del_trace =0
        kill_trace=0
        rev_trace =0
        flag_trace=0

!---- call to C-language routine to determine what to do with this trace
        call tredit_crou_get_choice(obj%tredfile_pointer, &
          hd(:,i), obj%nwih, &
          del_trace, kill_trace, rev_trace, flag_trace)

!---- TAKE DELETE, KILL, OR REVERSE ACTION.
        if (del_trace /= 0) then               !  DELETE causes 'cycle'
          obj%del_count = obj%del_count + 1 
          cycle

        else if (kill_trace /= 0) then         !  KILL 
          tr(:obj%ndpt,i) = 0. 
          hd(HDR_LAV,i) = 0                    !  flag dead w/LAV
          obj%kill_count = obj%kill_count + 1

        else if (rev_trace /= 0) then          !  REVERSE POLARITY 
          tr(:obj%ndpt,i) = -tr(:obj%ndpt,i) 
          obj%rev_count = obj%rev_count + 1 

        end if 

!---- TAKE FLAG ACTION (if and only if somewhere there was a flag card).
        if (obj%flagging_requested /= 0) then
          if(flag_trace /= 0) then 
            hd(obj%hdr_flag,i) = 1. 
            obj%flag_count = obj%flag_count + 1 
          else 
            hd(obj%hdr_flag,i) = 0. 
          end if
        end if 

!------ HDR_SEQUENCE renumbering
        obj%sequential_count = obj%sequential_count + 1
        hd(HDR_SEQUENCE,i) = obj%sequential_count

!------ HDR_CURRENT_CHANNEL renumbering only if gathered data
        if (obj%gathered) then

          if(hd(HDR_CURRENT_GROUP,i) /= previous_group) then
            current_channel = 1
          end if

          hd(HDR_CURRENT_CHANNEL,i) = current_channel

          previous_group = hd(HDR_CURRENT_GROUP,i)
          current_channel = current_channel + 1

        end if

!------ MOVE TRACES IF NECESSARY (part of delete logic).
        j = j + 1 

        if (j == i) then
          cycle
        else if (j > i) then
          call pc_error('TREDIT: j > i, program logic error')
          go to 999
        end if

        hd(:obj%nwih,j) = hd(:obj%nwih,i) 
        tr(:obj%ndpt,j) = tr(:obj%ndpt,i) 

      end do

!---- DO WRAP-UP PROCESSING.
      if(j == 0) then
        ntr = NEED_TRACES
      else
        ntr = j
      end if

      return

!---- conditional call to the WRAPUP routine
  999 ntr = FATAL_ERROR
 1000 call tredit_wrapup (obj)
      return

      end subroutine tredit

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine tredit_wrapup (obj)
      implicit none
      type(tredit_struct),intent(inout) :: obj

      character(len=120)                :: mesg
      integer                           :: i,imax,hmesg(120) ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.


!---- start of user summary messaging
      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '---------------------------- TREDIT start -----------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print  ('TREDIT summary, mode = ',obj%mode)

      if(obj%mode == 'TREDIT') then
        imax = obj%ncards
        call pc_print('TREDIT file          = ',obj%pathname)
      else
        imax = obj%nactions
      end if

      call pc_print ('  Number of action cards = ', imax)

      if(obj%tredfile_has_been_read) then

        if(obj%opt_print) then
          do i=1,imax
            call tredit_crou_sprintf_record(obj%tredfile_pointer, hmesg, i-1)
            call string_hh2cc(hmesg,mesg)
            call pc_print(mesg)
          end do
        end if

        call tredit_crou_kill_data(obj%tredfile_pointer) ! frees mem (c code)

      end if

      call pc_print (' ')
      call pc_print ('Traces INPUT    = ', obj%input_count)
      call pc_print ('Traces OUTPUT   = ', obj%sequential_count)
      call pc_print ('Traces DELETED  by edit criteria = ', obj%del_count)
      call pc_print ('Traces KILLED   by edit criteria = ', obj%kill_count)
      call pc_print ('Traces REVERSED by edit criteria = ', obj%rev_count) 
      call pc_print ('Traces FLAGGED  by edit criteria = ', obj%flag_count)

      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '---------------------------- TREDIT end -------------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')

      return
      end subroutine tredit_wrapup

!</execute_only>

!!------------------------ tredit_first_chars_only -------------------------!!
!!------------------------ tredit_first_chars_only -------------------------!!
!!------------------------ tredit_first_chars_only -------------------------!!

      function tredit_first_chars_only(n,act) result(first_chars)
      implicit none
      integer, intent(in)           :: n
      character(len=6), intent(in)  :: act(:)

      character(len=n)              :: first_chars
      integer                       :: i

      do i=1,n

        first_chars(i:i) = act(i)(1:1)

      end do

      return

      end function tredit_first_chars_only
      

!-------------------------------------------------------------------------------
!           subroutine to strip off pathcheck username and nodename
!-------------------------------------------------------------------------------
      function tredit_absolute_path(pathname) result(relpath)
      character(len=FILENAME_LENGTH), intent(in)   :: pathname
      character(len=FILENAME_LENGTH)               :: relpath

      integer                          :: i

      do i=1,FILENAME_LENGTH
        if (pathname(i:i) == ':') then
          relpath = pathname(i+1:FILENAME_LENGTH)
!          call pc_warning ( &
!            'TREDIT does not support rcp (e.g. user@node:/path/file.dat)')
          return
        end if
      end do

      relpath = pathname

      end function tredit_absolute_path

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module tredit_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
