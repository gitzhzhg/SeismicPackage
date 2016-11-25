!<CPS_v1 type="PROCESS"/>
!!------------------------------ headmap.f90 -------------------------------!!
!!------------------------------ headmap.f90 -------------------------------!!
!!------------------------------ headmap.f90 -------------------------------!!

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
! Name       : HEADMAP    (HEADer word MAPping function)
! Category   : headers
! Written    : 2001-03-28   by: Brad Kruse
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Set values for any header word based on values from another.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! HEADMAP uses a look-up table to set values of HDR_MAP based on values of
! HDR_TRIGGER.  Based on user specified arrays TRIGGER and MAP, if HDR_TRIGGER
! takes on a value in the TRIGGER array, then the value in the HDR_MAP header
! word is set to the corresponding value in the MAP array.
!
! The MODE parameter determines the scheme used for determining whether there
! is a match between a value in the HDR_TRIGGER header word, and a value in 
! the TRIGGER array.
!
!     MODE = EXACT produces a match only if the values are identical (real
!     number comparison).
!
!     MODE = ROUND or TRUNCATE first convert the values to integers and 
!     produce a match if the integers are identical.  ROUND and TRUNCATE use
!     the common mathematical intepretation of rounding and truncating
!     functions.  ROUND (6.5) will match 7, but not 6, and TRUNCATE (6.5) will
!     match 6.
!
!     MODE = NEAR produces a match if the absolute value of the difference 
!     between the HDR_TRIGGER value and a TRIGGER value is equal to or less 
!     than the value of TOLERANCE.
!
! HEADMAP searches for a match starting from the top of the TRIGGER array.  If
! a match is found, the search is stopped for that trace.  (only the first of 
! any possible multiple matches is use.)
!
! If OPT_MISSES = YES, then the value of HDR_MAP is set to MISSES_DEFAULT if no
! match is found for that trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
!  
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!  
! This process does not alter input traces.
! This process may alter input headers.
! This process outputs the same traces as it receives, possibly with altered
! headers.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
! 
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! HDR_TRIGGER  Input lookup value               Used but not changed.
! HDR_MAP      Output value                     Altered.
! HDR_FLAG     Flagword                         Used but not changed.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!    Date        Author      Description
!    ----        ------      -----------
!003. 2006-10-16 D. Glover   Added NULLIFY statements for Intel compiler.
! 2. 2005-10-24  Stoeckley   Remove 100-line limit on table entries for
!                             TRIGGER and MAP; add ability to input the
!                             table from a file.
! 1. 2001-03-30  Brad Kruse  Initial version.  
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
!
! 
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
! Perform a table lookup, using optional methods of evaluating matches
! and modifying the results.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS HEADMAP Process/NC=80/NR=40>
!                               HEADMAP 
!                    HEADer word MAPping function
!
!     Lookup table and match strategy
!     `-----------------------------------------------------------------
!     |  HDR_TRIGGER = `IIII      HDR_MAP = `IIII      HDR_FLAG~~= `IIII
!     | 
!     |  MODE~~~~~~~~= `CCCCCCCCCCCCC     TOLERANCE = `FFFFFFFFFF
!     | 
!     |  TRIGGER       MAP
!     |  `FFFFFFFFFFFFF`FFFFFFFFFFFFF
!     |  `FFFFFFFFFFFFF`FFFFFFFFFFFFF
!     |  `FFFFFFFFFFFFF`FFFFFFFFFFFFF
!     |  `FFFFFFFFFFFFF`FFFFFFFFFFFFF
!     |  `FFFFFFFFFFFFF`FFFFFFFFFFFFF
!     `-----------------------------------------------------------------
!
!Select TABLE_PATHNAME[TABLE_PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [TABLE_PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!     Miss strategy
!     `----------------------------------------------------------
!     |  OPT_MISSES = `CCC          MISSES_DEFAULT = `FFFFFFFF
!     `----------------------------------------------------------
!
!<PARMS TABLE_PATHNAME[/ML=128/XST]>
!<PARMS TRIGGER_ARRAYSET[/XSF/YST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="HDR_MAP">
!<Tip> Header word whose values are to be set with the look-up table. </Tip>
! Default = 31
! Allowed = 1 - NWIH
!
! If HDR_TRIGGER takes on a value in the TRIGGER array, then the value in 
! HDR_MAP is set to the corresponding value in the MAP array.
!</Help>
!
!<Help KEYWORD="HDR_TRIGGER">
!<Tip> Header word whose value determines the value placed in HDR_MAP. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!
! If HDR_TRIGGER takes on a value in the TRIGGER array, then the value in 
! HDR_MAP is set to the corresponding value in the MAP array.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! If HDR_FLAG = 0, then header mapping will be done on all traces.  Otherwise,
! mapping will be done only on traces with a flag set in header word HDR_FLAG.
!</Help>
!
!<Help KEYWORD="TRIGGER">
!<Tip> Array of constant values to check against HDR_TRIGGER values. </Tip>
! Default = -
! Allowed = real array (linked).  no limit on number of entries.
!
! If HDR_TRIGGER takes on a value in the TRIGGER array, then the value in 
! HDR_MAP is set to the corresponding value in the MAP array.
!</Help>
!
!<Help KEYWORD="MAP">
!<Tip> Array of values associated with values in the TRIGGER array. </Tip>
! Default = -
! Allowed = real array (linked).  no limit on number of entries.
!
! If HDR_TRIGGER takes on a value in the TRIGGER array, then the value in 
! HDR_MAP is set to the corresponding value in the MAP array.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Method for comparing HDR_TRIGGER values with TRIGGER values. </Tip>
! Default = EXACT
! Allowed = EXACT
! Allowed = ROUND
! Allowed = TRUNCATE
! Allowed = NEAR
!
! MODE = EXACT produces a match only if the values are identical (real number 
! comparison.)
!
! MODE = ROUND or TRUNCATE first convert the values to integers and matches if
! the integers are identical.  ROUND and TRUNCATE use the common mathematical
! interpretation of rounding and truncating functions.  ROUND (6.5) will match
! 7, but not 6, and TRUNCATE (6.5) will match 6.
!
! MODE = NEAR produces a match if the absolute value of the difference between 
! the HDR_TRIGGER value and TRIGGER value is equal to or less than the value of
! TOLERANCE.  
!</Help>
!
!<Help KEYWORD="TOLERANCE">
!<Tip> Tolerance value used with MODE = NEAR. </Tip>
! Default = 0.5
! Allowed = real >= 0.0
!</Help>
!
!<Help KEYWORD="OPT_MISSES">
!<Tip> Whether to set MISSES_DEFAULT into HDR_MAP when no match is found. </Tip>
! Default = NO
! Allowed = YES
! Allowed = NO
!
! If OPT_MISSES = NO, then the value in HDR_MAP will be unchanged if no match
! is found for that trace.
!</Help>
!
!<Help KEYWORD="MISSES_DEFAULT">
!<Tip> Value to write to HDR_MAP when no TRIGGER match occurs. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="SELECT_TABLE_PATHNAME">
!<Tip> Choose TABLE_PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="TABLE_PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TABLE_PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="TABLE_PATHNAME">
!<Tip> Name for the input trigger table file. </Tip>
! Default = NONE
! Allowed = char
!
! If TABLE_PATHNAME = NONE, then input the trigger table manually.
!
! If TABLE_PATHNAME is specified, the trigger table is read from the
! file and the manual table entry is not used.
!
! The file should have the same appearance as the manual table entry area:
!        two columns.
!        first column containing TRIGGER.
!        second column containing MAP.
!        free-field format is allowed.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module headmap_module
  !
  ! - Module references
  !
  use pc_module
  use named_constants_module
  use pathcheck_module
  use pathchoose_module
  use floatio_module
  !
  implicit none
  !
  private
  !
  public :: headmap_create
  public :: headmap_initialize
  public :: headmap_update
  public :: headmap_delete
  public :: headmap            ! main execution (trace processing) routine.
  public :: headmap_wrapup


  character(len=100),public,save :: HEADMAP_IDENT = &
    '$Id: headmap.f90,v 1.3 2006/10/17 13:45:44 Glover prod sps $'

  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!

  type, public :: headmap_struct              
    !
    private
    !
    logical                    :: skip_wrapup      ! wrapup flag.
    logical                    :: hdr_flag_flag
    integer                    :: hdr_flag
    !
    ! - Process parameters
    !      Available from the parameter cache
    !
    character (len = 8)      :: mode             ! EXACT,ROUND,TRUNCATE,NEAR
    integer                  :: hdr_map          ! 1 - NWIH
    integer                  :: hdr_trigger      ! 1 - NWIH
    double precision         :: tolerance        ! real > 0.0
    double precision,pointer :: map       (:)
    double precision,pointer :: trigger   (:)
    integer         ,pointer :: i_trigger (:)
    integer                  :: num_triggers 
    logical                  :: opt_misses
    double precision         :: misses_default
    !
    character(len=FILENAME_LENGTH) :: table_pathname
    !
    type(pathchoose_struct),pointer :: pathchoose
    !
    ! - Statistics
    !
    integer               :: trace_count
    integer               :: non_flagged_trace_count
    integer,pointer       :: match_count (:)
    integer               :: misses_default_count
    !
  end type headmap_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


  type(headmap_struct),pointer,save :: object      ! needed for traps.
  !
  character (len = *), parameter :: exact       = 'EXACT   '
  character (len = *), parameter :: round       = 'ROUND   '
  character (len = *), parameter :: truncate    = 'TRUNCATE'
  character (len = *), parameter :: near        = 'NEAR    '


contains

  !!--------------------------- headmap_create -----------------------------!!
  !!--------------------------- headmap_create -----------------------------!!
  !!--------------------------- headmap_create -----------------------------!!


  subroutine headmap_create (obj)
    !
    ! - Arguments
    !
    type (headmap_struct), pointer :: obj
    !
    ! - Begin headmap_create
    !
    allocate (obj)
    !
    nullify (obj%map)
    nullify (obj%trigger)
    nullify (obj%i_trigger)
    nullify (obj%match_count)
    nullify (obj%pathchoose) ! jpa
    !
    call pathchoose_create (obj%pathchoose, 'table_pathname', 'triggers')
    !
    call headmap_initialize (obj)
    !
  end subroutine headmap_create


  !!--------------------------- headmap_delete -----------------------------!!
  !!--------------------------- headmap_delete -----------------------------!!
  !!--------------------------- headmap_delete -----------------------------!!


  subroutine headmap_delete (obj)
    !
    ! - Arguments
    !
    type (headmap_struct), pointer :: obj
    !
    ! - Begin headmap_delete
    !
    call headmap_wrapup (obj)
    !
    if (associated(obj%map))         deallocate (obj%map)
    if (associated(obj%trigger))     deallocate (obj%trigger)
    if (associated(obj%i_trigger))   deallocate (obj%i_trigger)
    if (associated(obj%match_count)) deallocate (obj%match_count)
    !
    call pathchoose_delete (obj%pathchoose)
    !
    deallocate(obj)
    !
  end subroutine headmap_delete


  !!------------------------- headmap_initialize ---------------------------!!
  !!------------------------- headmap_initialize ---------------------------!!
  !!------------------------- headmap_initialize ---------------------------!!


  subroutine headmap_initialize (obj)
    !
    ! - Arguments
    !
    type(headmap_struct),intent(inout) :: obj       ! arguments
    !
    ! - Begin headmap_initialize
    !
    obj % hdr_map                 = 31
    obj % hdr_trigger             = 1
    obj % mode                    = EXACT
    obj % tolerance               = 0.5
    obj % num_triggers            = 0
    obj % opt_misses              = .false.
    obj % misses_default          = 0.0
    obj % hdr_flag                = 0
    obj % trace_count             = 0
    obj % non_flagged_trace_count = 0
    obj % match_count             = 0
    obj % misses_default_count    = 0
    obj % table_pathname          = PATHCHECK_EMPTY
    !
    call headmap_update (obj)
    !
  end subroutine headmap_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


  subroutine headmap_update (obj)
    !
    ! - Arguments
    !
    type (headmap_struct), intent (inout), target :: obj
    ! 
    ! - Local variables
    !
    integer                      :: num_triggers, num_maps, i
    integer                      :: nwih, ncolumns, err
    character(len=80)            :: msg
    double precision             :: vline(2)
    type(floatio_struct),pointer :: floatio
    !
    ! - Begin headmap_update
    !
      object => obj                 ! needed for traps.
      obj % skip_wrapup = .true.    ! needed for the wrapup routine.


    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!

    if (pathchoose_update(obj%pathchoose, obj%table_pathname)) return

    call pc_register_array_names ('TRIGGER_ARRAYSET',                         &
                                  (/ 'trigger', 'map    ' /),&
                                  2)
    !
    call pc_get_global ('NWIH', nwih)
    !
    call pc_get ('HDR_MAP',     obj % hdr_map)
    call pc_get ('HDR_FLAG',    obj % hdr_flag)
    call pc_get ('HDR_TRIGGER', obj % hdr_trigger)
    call pc_get ('HDR_FLAG',    obj % hdr_flag)
    !
    call pc_get ('MODE', obj % mode)
    call string_to_upper (obj % mode)
    !
    call pc_get ('TOLERANCE', obj % tolerance)
    !
    num_triggers = obj % num_triggers
    call pc_alloc ('TRIGGER', obj % trigger, num_triggers)
    !
    num_maps = obj % num_triggers
    call pc_alloc ('MAP', obj % map, num_maps)

    if (num_triggers /= num_maps) then
      call pc_error ('headmap: Numbers of entries in TRIGGER and '   &
                     // ' MAP do not match. ',                        &
                     min (num_triggers, num_maps),             &
                     ' does not match ',                                    &
                     max (num_triggers, num_maps))
    else
      obj % num_triggers = num_triggers
    end if
    !
    call pc_get ('OPT_MISSES',     obj % opt_misses)
    call pc_get ('MISSES_DEFAULT', obj % misses_default)
    call pc_get ('TABLE_PATHNAME', obj % table_pathname)


    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!


    !
    ! - Check HDR_MAP
    !
  check_hdr_map:   &
    if ((obj % hdr_map < 1) .or. (obj % hdr_map > nwih)) then   ! 1 - NWIH
      !
      call pc_error (msg1 = 'Bad value for HDR_MAP (',   &
                     var1 = obj % hdr_map,               &
                     msg2 = ')')
      !
    end if check_hdr_map

    !
    ! - Check HDR_TRIGGER
    !
  check_hdr_trigger:   &
    if ((obj % hdr_trigger < 1) .or. (obj % hdr_trigger > nwih)) then
      !
      call pc_error (msg1 = 'Bad value for HDR_TRIGGER (',   &
                     var1 = obj % hdr_trigger,               &
                     msg2 = ')')
      !
    end if check_hdr_trigger

    !
    ! - HDR_FLAG
    !
    if (obj % hdr_flag < 0 .or. obj % hdr_flag > nwih) then
      call pc_error ('HDR_FLAG is ', obj % hdr_flag,   &
                     ' but must be in the range 0 through ', nwih)
      obj % hdr_flag = 0
    end if
    !
    obj % hdr_flag_flag = obj % hdr_flag /= 0

    !
    ! - Check MODE
    !
  check_mode:   &
    select case (obj % mode) 
    case (exact, round, truncate, near)
    case default 
      !
      call pc_error (msg1 = 'Bad value for MODE (',   &
                     var1 = obj % mode,               &
                     msg2 = ')')
      !
    end select check_mode

    !
    ! - Check TOLERANCE
    !
  check_tolerance:   &
    if (obj % tolerance < 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'Bad value for TOLERANCE (',   &
                     var1 = obj % tolerance,               &
                     msg2 = ')')
      !
    end if check_tolerance

    !
    ! - TRIGGER is not checked
    !
! check_num_triggers:    &
!   if (obj % num_triggers > 100) then
!     !
!     call pc_error ('HEADMAP: Number of entries in TRIGGER and MAP exceed '  &
!                    // '100.  Only the first one hundred are kept.')
!     obj % num_triggers = 100
!     !
!   end if check_num_triggers

    !
    ! - MAP_MULT is not checked

    !
    ! - MAP_CONST is not checked

    !
    ! - OPT_MISSES is not checked
    !

    !
    ! - MISSES_DEFAULT is not checked
    !

    call pathcheck &
            ('table_pathname', obj%table_pathname, show=PATHCHECK_INFO_INPUT)


    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!


    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!

    call pc_put ('HDR_MAP',     obj % hdr_map)
    call pc_put ('HDR_TRIGGER', obj % hdr_trigger)
    call pc_put ('HDR_FLAG',    obj % hdr_flag  )
    !
    call pc_put_options_field ('MODE', (/exact, round, truncate, near/), 5)
    call pc_put ('MODE', obj % mode)
    !
    call pc_put ('TOLERANCE', obj % tolerance)
    call pc_put_sensitive_field_flag ('TOLERANCE',    &
                                      obj % mode == near)
    !
    call pc_put ('TRIGGER',   obj % trigger,   obj % num_triggers)
    call pc_put ('MAP',       obj % map,       obj % num_triggers)
 !  call pc_put_minsize_arrayset ('TRIGGER_ARRAYSET', 1)
 !  call pc_put_maxsize_arrayset ('TRIGGER_ARRAYSET', 100)
    !
    call pc_put ('OPT_MISSES',     obj % opt_misses)
    call pc_put ('MISSES_DEFAULT', obj % misses_default)
    call pc_put ('TABLE_PATHNAME', obj % table_pathname)

    call pc_put_sensitive_arrayset_flag &
                    ('trigger_arrayset', obj%table_pathname == PATHCHECK_EMPTY)
    call pc_put_sensitive_array_flag &
                    ('trigger', obj%table_pathname == PATHCHECK_EMPTY)
    call pc_put_sensitive_array_flag &
                    ('map', obj%table_pathname == PATHCHECK_EMPTY)


    if (obj%table_pathname == PATHCHECK_EMPTY) then
         call pc_put_minsize_arrayset ('TRIGGER_ARRAYSET', 1)
    else
         call pc_put_minsize_arrayset ('TRIGGER_ARRAYSET', 0)
    endif


    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!
    !!----------------------- prepare for execution ------------------------!!


    if (associated(obj%match_count)) deallocate (obj%match_count)

    if (pc_do_not_process_traces()) return
    !
    obj % skip_wrapup = .false.     ! to run wrapup code after processing.
    !
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !

    if (obj%table_pathname /= PATHCHECK_EMPTY) then

        if (associated(obj%map))         deallocate (obj%map)
        if (associated(obj%trigger))     deallocate (obj%trigger)
        if (associated(obj%i_trigger))   deallocate (obj%i_trigger)
        if (associated(obj%match_count)) deallocate (obj%match_count)

        call floatio_easy_read (floatio,obj%table_pathname,obj%num_triggers, &
                                ncolumns,err,msg)
        if (err /= FLOATIO_OK) then
             call pc_error (msg)
             call floatio_close (floatio)
             return
        endif
        if (ncolumns /= 2) then
             call pc_error &
            ("wrong number of columns",ncolumns,"on table file - should be 2")
             call floatio_close (floatio)
             return
        endif
        if (obj%num_triggers < 1) then
             call pc_error ("no lines on table file")
             call floatio_close (floatio)
             return
        endif

        allocate (obj%map        (obj%num_triggers))
        allocate (obj%trigger    (obj%num_triggers))
        allocate (obj%i_trigger  (obj%num_triggers))
        allocate (obj%match_count(obj%num_triggers))

        do i = 1,obj%num_triggers
             call floatio_read_line (floatio,err,msg,vline)
             if (err /= FLOATIO_OK) then
                  call pc_error (msg)
                  call floatio_close (floatio)
                  return
             endif
             obj%trigger(i) = vline(1)
             obj%map    (i) = vline(2)
        enddo

        call floatio_close (floatio)

    endif

    do i = 1, obj % num_triggers
      if (obj % mode == round) then
        obj % i_trigger (i) = nint (obj % trigger (i))
      else if (obj % mode == truncate) then
        obj % i_trigger (i) = int (obj % trigger (i))
      end if
    end do

    allocate (obj%match_count(obj%num_triggers))

    obj % trace_count             = 0
    obj % non_flagged_trace_count = 0
    obj % match_count             = 0
    obj % misses_default_count    = 0


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    !
  end subroutine headmap_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!


  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!


  subroutine headmap (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (headmap_struct), intent (inout) :: obj
    integer,               intent (inout) :: ntr
    double precision,      intent (inout) :: hd (:,:)
    real             ,     intent (inout) :: tr (:,:)
    !
    ! - Local variables
    !
    integer :: i, t, i_trigger
    integer :: match
    double precision :: trigger, nearness
    !
    ! - Begin headmap
    !
  loop_through_headers:    &
    do i = 1, ntr
      !
      ! - skip traces that are not flagged.
      !
      if (obj%hdr_flag_flag) then
        if (hd (obj % hdr_flag, i) == 0.0d0) then 
          obj % non_flagged_trace_count = obj % non_flagged_trace_count + 1
          cycle loop_through_headers
        end if
      end if
      !
      match   = 0
      trigger = hd (obj % hdr_trigger, i)
      !
      select case (obj % mode)
      case (exact)
        !
      loop_exact:    &
        do t = 1, obj % num_triggers
          if (trigger == obj % trigger (t)) then
            match = t
            exit loop_exact
          end if
        end do loop_exact
        !
      case (round, truncate)
        !
        if (obj % mode == round) then
          i_trigger = nint (trigger)
        else
          i_trigger = int (trigger)
        end if
        !
      loop_integer:    &
        do t = 1, obj % num_triggers
          if (i_trigger == obj % i_trigger (t)) then
            match = t
            exit loop_integer
          end if
        end do loop_integer
        !
      case (near)
        !
      loop_near:    &
        do t = 1, obj % num_triggers
          !
          nearness = abs (trigger - obj % trigger (t))
          !
          if (nearness <= obj % tolerance) then
            match = t
            exit loop_near
          end if
        end do loop_near
        !
      case default
        !
        call pc_error ('headmap: Invalid match-mode ' // obj % mode)
        ntr = FATAL_ERROR
        exit loop_through_headers
        !
      end select
      !
      ! - Compute output value
      !
      if (match > 0) then
        !
        hd (obj % hdr_map, i)     = obj % map (match)
        obj % match_count (match) = obj % match_count (match) + 1
        !
      else if (obj % opt_misses) then
        !
        hd (obj % hdr_map, i)      = obj % misses_default
        obj % misses_default_count = obj % misses_default_count + 1
        !
      end if
      !
    end do loop_through_headers
    !
    if (ntr > 0) obj % trace_count = obj % trace_count + ntr
    !
    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call headmap_wrapup (obj)
    end if
    !
  end subroutine headmap


  !!--------------------------- headmap_wrapup -----------------------------!!
  !!--------------------------- headmap_wrapup -----------------------------!!
  !!--------------------------- headmap_wrapup -----------------------------!!


  subroutine headmap_wrapup (obj)
    !
    ! - Arguments
    !
    type (headmap_struct), intent (inout) :: obj       ! arguments
    !
    ! - Local variables
    !
    character (len =  3) :: c_m
    character (len = 11) :: c_match
    character (len = 16) :: c_trigger
    character (len = 16) :: c_map
    character (len = 16) :: val
    character (len = 16), parameter :: long_field = ' _______________'
    character (len = 85) :: line
    integer :: m
    integer :: matches
    integer :: match_count
    !
    ! - Begin headmap_wrapup
    !
    if (obj % skip_wrapup) return
    obj % skip_wrapup = .true.
    !
    ! - List statistics
    !
    call pc_print (' ')
    call pc_print ('HEADMAP:  Total traces         ', obj % trace_count)
    !
    if (obj%hdr_flag_flag) then
      call pc_print ('HEADMAP:  Traces Not Flagged ',    &
                     obj % non_flagged_trace_count)
    end if
    !
    matches = 0
    match_count = 0
    call pc_print (' ')
    call pc_print ('HEADMAP:               Matches ')
    call pc_print ('HEADMAP:   TRIGGER')
    call pc_print ('HEADMAP:    Index   Trigger Value       Map Value   '   &
                   // '  # Matches')
    call pc_print ('HEADMAP:    ----  ----------------  ----------------'   &
                   // '  -----------')
    !
    do m = 1, obj % num_triggers
      if (obj % match_count (m) > 0) then
        !
        match_count = match_count + 1
        matches     = matches + obj % match_count (m)
        !
        write (c_m, '(i3)') m
        !
        call string_ii2cc ( obj % match_count (m), c_match)
          if (c_match == ' ') c_match = '0'
        !
        call string_dd2cc (obj % trigger (m), val)
          c_trigger = trim (val) // long_field
        !
        call string_dd2cc (obj % map (m), val)
          c_map = trim (val) // long_field

        write (line, "(a3, '.  ', a, 2x, a, 2x, a)") c_m, c_trigger, c_map,   &
                                                     adjustr (c_match)
        call pc_print ('HEADMAP:    ' // trim (line))
      end if
    end do
    !
    call pc_print ('HEADMAP:    ----  ----------------  ----------------'   &
                   // '  -----------')
    call pc_print (' ')
    call pc_print ('HEADMAP:   ', matches, 'Traces matched to ',     &
                    match_count, 'TRIGGER entries')
    call pc_print ('HEADMAP:  Traces Set by MISSES_DEFAULT ',    &
                   obj % misses_default_count)
    call pc_print ('HEADMAP:  Traces Not Set               ',    &
                   obj % trace_count - matches - obj % misses_default_count)
    !
  end subroutine headmap_wrapup


  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!

end module headmap_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

