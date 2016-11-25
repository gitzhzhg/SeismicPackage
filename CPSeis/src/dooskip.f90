!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- dooskip.f90 --------------------------------!!
!!------------------------------- dooskip.f90 --------------------------------!!
!!------------------------------- dooskip.f90 --------------------------------!!

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
!                        C P S   P R I M I T I V E        
!
! Name       : dooskip  (DO a few, SKIP a few... pattern primitive.)
! Category   : miscellaneous
! Written    : 2000-05-04   by: Bill Menger
! Revised    : 2007-07-12   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Provide do_skip parameters for processes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!  This primitive attempts to assist the programmer by handling the do-skip
!  parameters for him/her.
!  Parameters:    Range
!  skip_init      >=0
!  num_do         >=1
!  num_skip       any integer [Note: can be restricted to >= 0. see below]
!  tr_max         >=0
!
!  [Programming example (TRIN) found in programming_doc]
!
!  Modes: 
!   SEQUENTIAL: (see advice_doc)
!         Positive skipping (num_skip >= 0 )
!         [NOTE: if using gui, you can restrict num_skip to >= 0 by calling
!                the dooskip_neg_skip_ok(obj,.false.) function.]
!   DIRECTED:   (see advice_doc)
!         Positive skipping (num_skip >= 0 )
!         Negative skipping with overall forward movement:
!           (num_skip < 0 AND abs(num_skip) <  num_do )
!         Negative skipping with overall reverse movement:
!           (num_skip < 0 AND abs(num_skip) >  num_do )
!         In Place skipping (num_skip < 0 AND abs(num_skip) == num_do )
!
! [programmers note: if not used inside a "process" that calls the update
!  and trap routines, you call the "put" and "get" routines to access values
!  instead of using the parameter cache.]
! [programmers note: create/initialize are called by all routines but DELETE &
!  the traps.  You can start off by calling a "put" routine which will call
!  create and initialize for you.]
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!   p = intent(inout) = value is a pointer to an object.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
! The following calls  use the dooskip_struct "obj" that is allocated by any
! dooskip function EXCEPT delete, wrapup, and the traps.
!
!  type(dooskip_struct),pointer ::  obj
!  logical                      ::  do_this_one !true = do, false = skip
!  logical                      ::  hit_tr_max  !true = already did tr_max.
!  integer                      ::  tr_max      !absolute max # to do.
!  integer                      ::  skip_init   !initial number to skip
!  integer                      ::  num_skip    !number to skip after doing 
!                                                each group of "num_do" items.
!  integer                      ::  num_do      !number to do in each group,
!                                                after skipping "skip_init"
!                                                items.
!  integer                      ::  nbr_done    !how many items have been done.
!  integer                      ::  this_item   !direct dooskip to position
!                                                the pattern checker to the
!                                                "this_item" position.
!  integer                      ::  next_item   !next item that will be "done"
!  integer                      ::  status      !if status = dooskip_ok then
!                                                no errors. Conversely, if
!                                                status = dooskip_error, then
!                                                not ok.  (2 states only).
!  logical                      ::  neg_skip_ok !if FALSE, do not allow neg
!                                                values of num_skip.
!  integer                      ::  thru_item   !last item that will be hit.
!                                               !or number of items in "file"
!  integer                      ::  tr_max_est  !estimate of good tr_max based
!                                               !on a given number of items

!(-------------- p indicates that this is a pointer argument, thus is "io"--) 
!                                p      i(opt) o(opt)                 
! function dooskip              (obj,this_item,next_item) result (do_this_one)
!   Purpose: Return TRUE if you are to DO, FALSE if you are to SKIP.
!            Accepts THIS_ITEM to tell it to move up or back to this_item in
!            the sequence before telling you whether to DO or SKIP.
!            Provides NEXT_ITEM to let you know the next item that will be DONE.
! function dooskip_hit_tr_max   (obj)                     result(hit_tr_max)
!   Purpose: Return FALSE until tr_max has been hit.  Then always return TRUE
! function dooskip_get_tr_max(obj)                     result(tr_max  )
!   Purpose: Return the tr_max value from the struct.
! function dooskip_get_skip_init(obj)                     result(skip_init)
!   Purpose: Return the skip_init value from the struct.
! function dooskip_get_num_do   (obj)                     result(num_do   )
!   Purpose: Return the num_do value from the struct.
! function dooskip_get_num_skip (obj)                     result(num_skip )
!   Purpose: Return the num_skip value from the struct.
! function dooskip_nbr_done     (obj)                     result(nbr_done )
!   Purpose: Return the number of items done so far (total).
! function dooskip_next_item    (obj)                     result(next_item)
!   Purpose: Return the next item to do. 
! function dooskip_thru_item    (obj)                     result(thru_item)
!   Purpose: Return the last item index number that will be hit.
! function dooskip_calc_trmax(obj,thru_item)              result(tr_max_est)
!
! ---------- The "put" routines are used in case you are not using the "gui"
!            calls that set values throught the "trap" routines below"  If
!            you are not using the "parameter cache" (pc_module) then DO NOT
!            call "update" or "trap" routines below.  Use "Put/Get" instead.
!                                p     i
! function dooskip_put_tr_max(obj,tr_max)                 result(status )
!   Purpose: put tr_max value into the struct.
! function dooskip_put_skip_init(obj,skip_init)           result(status )
!   Purpose: put skip_init value into the struct.
! function dooskip_put_num_do   (obj,num_do   )           result(status )
!   Purpose: put num_do value into the struct.
! function dooskip_put_num_skip (obj,num_skip )           result(status )
!   Purpose: put num_skip value into the struct.
! function dooskip_create       (obj)                     result (status)
!   Purpose: create the structure by allocating the pointer "obj", and
!            initialize values within it.
! function dooskip_delete       (obj)                     result (status)
!   Purpose: delete the structure by deallocating the pointer "obj".
! function dooskip_wrapup       (obj)                     result (status)
!   Purpose: delete the structure by deallocating the pointer "obj".
! function dooskip_initialize   (obj)                     result (status)
!   Purpose: initialize values within the structure (called from create)
! function dooskip_update       (obj)                     result (status)
!   Purpose: create the structure by allocating the pointer "obj",
!            initializing values within it, and call the parameter cache 
!            to update values from the cache, then call traps to verify
!            values and communicate with "gui"
!                                   p     i
! subroutine dooskip_neg_skip_ok  (obj,neg_skip_ok)
!   Purpose: set/unset the availability of negative num_skip values.  The trap
!            for num_skip will test for this and disallow any negative values
!            if this is FALSE.  Default is TRUE.
!                                       i
! subroutine dooskip_tr_max_trap    (tr_max)
!   Purpose: check tr_max for bad value
! subroutine dooskip_skip_init_trap (skip_init)
!   Purpose: check skip_init for bad value
! subroutine dooskip_num_do_trap    (num_do)
!   Purpose: check num_do for bad value
! subroutine dooskip_num_skip_trap  (num_skip)
!   Purpose: check num_skip for bad value
!
! subroutine dooskip_state      (obj)
!   Purpose: Print current state of the structure. (debugging aid)
!
! subroutine dooskip_reset      (obj)
!   Purpose: Reset counters to 0 and "this" to iskip + 1, logicals to false.
!
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2007-07-12  Stoeckley    Add rowgroup declarations to put the four do-skip
!                               parameters on two lines in SeisSpace with the
!                               same arrangement as in CFE.
!  9. 2006-06-20  B. Menger    Removed Unused Variables.
!  8. 2001-03-22  Bill Menger  Took out some fields.
!  7. 2000-10-30  Bill Menger  Fixed dooskip the function.
!  6. 2000-10-23  Bill Menger  Added reset function.
!  5. 2000-10-04  Bill Menger  Modified gui_def, added tips.
!  4. 2000-08-24  Bill Menger  Added dooskip_calc_trmax function
!  3. 2000-08-22  Bill Menger  Added dooskip-thru-item function, fixed doc.
!  2. 2000-05-08  Bill Menger  Added pc_update_error() call to update routine.
!                              Changed pc_warning msg to pc_error.
!  1. 2000-05-04  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES        
!
! Here is an example of how dooskip is used in the TRIN process:
! GUI->
! *inside the gui_def section:
!------- |!HISTORY=`CCCC       MERGE=`CC         METHOD=~`CCCCCC
!------- |!
!------- |!<INCLUDE dooskip.f90>
!------- |!
!------- |!`--------------------------------------------------------------------
!------- |! HDR_PRI=~~~`IIIIIIIII  HDR_SEC= `IIIIIIIIII  HDR_TERT= `IIIIIIIIII
!  
! *After "module trin_module" you find:
!------- |!  use dooskip_module
!
! *The trin_struct includes:
!------- |  type :: trin_struct
!------- |
!------- |    ...
!------- |    integer                    :: numtr
!------- |    logical                    :: gathered
!------- |
!------- |    !--Parms  --
!------- |
!------- |    type(dooskip_struct),pointer :: DS
!------- |    integer                    :: ndone
!------- |    ...
! *inside trin_initialize:
!------- |  subroutine trin_initialize (obj)
!------- |  type(trin_struct),pointer :: obj       ! arguments
!------- |    if(.not. associated(obj) ) return
!------- |    ...
!------- |    obj%nwih       = 0
!------- |    if(dooskip_initialize(obj%DS) /= dooskip_ok) continue
!------- |    obj%pathname   = PATH_EMPTY
!------- |    ...
! *inside trin_update:
!------- |   ...
!------- |   call pc_get ('method'   , obj%method   , trin_method_trap)
!------- |   if(obj%method == 'SEQ') then
!------- |     call pc_put ('num_do' ,   1 )
!------- |     call pc_put ('num_skip' , 0 )
!------- |     call pc_put_sensitive_field_flag('NUM_SKIP'   ,.false.)
!------- |     call pc_put_sensitive_field_flag('NUM_DO'   ,.false.)
!------- |   else
!------- |     call pc_put_sensitive_field_flag('NUM_SKIP'   ,.true.)
!------- |     call pc_put_sensitive_field_flag('NUM_DO'   ,.true.)
!------- |   endif
!------- |   if(dooskip_update(trp%DS) /= dooskip_ok) &
!------- |     call pc_error('TRIN: error applying dooskip_update.')
!------- |   ...
!------- |    !--- Need to SEEK to FIRST TRACE TO READ ...
!------- |    if(obj%method == 'DO_SKIP' .or. obj%method == 'SEQ') then
!------- |      ! skip the initial traces as told by obj%skip_init.
!------- |      status = trcio_seek_trace(obj%file,dooskip_next_item(obj%DS))
!------- |      !--- Position on next trace.
!--------|   ...
! *inside trin_method_trap:
!------- |  ...
!------- |  select case(trp%method)
!------- |  case('SEQ')
!------- |      call pc_put_sensitive_field_flag('NUM_SKIP'   ,.false.)
!------- |      call pc_put_sensitive_field_flag('NUM_DO'   ,.false.)
!------- |      call pc_put ('num_do' ,   1 )
!------- |      call pc_put ('num_skip' , 0 )
!------- |  case('DO_SKIP')
!------- |      call pc_put_sensitive_field_flag('NUM_SKIP'   ,.true.)
!------- |      call pc_put_sensitive_field_flag('NUM_DO'   ,.true.)
!------- |  case('SORT')
!------- |      call pc_put_sensitive_field_flag('NUM_SKIP'   ,.true.)
!------- |      call pc_put_sensitive_field_flag('NUM_DO'   ,.true.)
!------- |  ...
! *inside trin:
!------- |  ...
! !--- If I have exceeded my maximum, I need to quit.
! if(dooskip_hit_tr_max(obj%DS)) then
!   ntr = NO_MORE_TRACES
!   return
! endif
!------- |  ...
! if(.not.dooskip(obj%DS,this_item=obj%trace_pointer,next_item=next_trace)) then
!   if(dbg)call dooskip_state(obj%DS)
!   ntr = NO_MORE_TRACES
!   return
! endif
!------- |  ...
! *inside trin_wrapup:
!------- |  ...
!------- |  status = dooskip_wrapup(obj%DS)
!------- |  ...
!-------------------------------------------------------------------------------
!</programming_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!+------------------------------------------------+
!|dooskip can be used in one of two general modes:|
!+------------------------------------------------+
! +----------------+
! |Sequential mode:|
! +----------------+
!    Usage:  After initialization and setting of parameters, call dooskip from
!            within a do-loop where the loop counter is sequential.  Typically
!            dooskip is called with no optional arguments in this case, since
!            loop control is directed by the caller, not dooskip.
!    Example: 
!            type(dooskip_struct),pointer :: DS
!            integer                      :: status,i
!            if(dooskip_initialize(DS) /= dooskip_ok)stop 'doskip: init err'
!            status = dooskip_put_tr_max   (DS,4)
!            status = dooskip_put_skip_init(DS,2)
!            status = dooskip_put_num_do   (DS,1)
!            status = dooskip_put_num_skip (DS,1)
!            !(should start at item 3, do 1, skip 1, ... up to 4 items.)
!            if using the update and traps, you should call this to disallow
!            negative values of num_skip:
!              call dooskip_neg_skipping_ok(.false.)
!            do i = 1, 10
!               if(dooskip(DS)) print*' I''m Doing Item ',i
!            end do
!     Result: 
!            I'm Doing Item 3
!            I'm Doing Item 5
!            I'm Doing Item 7
!            I'm Doing Item 9
!
!            ** DO-REPEAT in sequential mode: (WILL NOT WORK) 
!              (Use directed mode for this.)
!
!            ** DO-REVERSE in sequential mode:                  
!              (Use directed mode for this.)
!
!            ** DO-RUNNING_AVG in sequential mode:                  
!              (Use directed mode for this.)
!
! +----------------+
! |Directed mode:  |
! +----------------+
!    Usage:  After initialization and setting of parameters, call dooskip from
!            within a do_while loop where the loop counter is directed by
!            dooskip.  This would be useful in a trace-reading program that
!            would prefer to seek to the next appropriate trace and not 
!            attempt to read traces that would never be selected in the first
!            place.
!    Example:
!            type(dooskip_struct),pointer :: DS
!            integer                      :: status,this_item,next_item
!            if(dooskip_initialize(DS) /= dooskip_ok)stop 'doskip: init err'
!            status = dooskip_put_tr_max   (DS,4)
!            status = dooskip_put_skip_init(DS,2)
!            status = dooskip_put_num_do   (DS,1)
!            status = dooskip_put_num_skip (DS,1)
!            !(should start at item 3, do 1, skip 1, ... up to 4 items.)
!            this_item = dooskip_next_item(DS)
!            do while (.not. dooskip_hit_tr_max(DS))
!              if(dooskip(DS,this_item,next_item) ) &
!                print*,' I''m Doing Item ',this_item,&
!                       ' Next item will be ',next_item
!              this_item = next_item
!            end do !while
!
!     Result: 
!            I'm Doing Item 3 Next item will be 5
!            I'm Doing Item 5 Next item will be 7
!            I'm Doing Item 7 Next item will be 9
!            I'm Doing Item 9 Next item will be 11
!
!     Notes: Directed mode allows you to "know" the upcoming item that will 
!            not be skipped, and thus your "loop" can be more efficient if 
!            you are reading from a random-access disk file or memory buffer,
!            since you only execute the "loop" when it is going to do work.
!            (Example: use a seek function to "seek" ahead to the next 
!             record you will "do".)
!
!            DO-REPEAT in directed mode: 
!            Example:
!              Skip_init=10, num_do=3, num_skip=-3 would start on record
!              11, and do 11 12 13, 11 12 13, 11 12 13, 11 12 13.... until
!              tr_max was hit.
!
!            DO-REVERSE in directed mode:                  
!            Example:
!              skip_init=79,999  Num_do=1 Num_skip=-2 tr_max = 80000
!
!              this_item = dooskip_next_item(DS)
!              do while (.not. dooskip_hit_tr_max(DS))
!                if(dooskip(DS,this_item,next_item) ) print*,this_item
!                this_item = next_item
!              end do !while
!              
!              This would start on record 80,000 and do the following:
!              80000 79999 79998 79997 ... 3 2 1
!
!             DO-RUNNING-AVG in directed mode:
!             Let's say you want to composite 3 records into one record, then
!             move up one record and composite the next three, etc.
!             You want: 123   234  345  456 ...
!             You do: skip_init=0,tr_max = 10000, num_do=3, num_skip=-2
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>

!<gui_def>
!`-----------------------------------------------
! Perform do-skip patterns on sequences of items.
! TR_MAX=~~`IIIIIIIIII SKIP_INIT=~~`IIIIIIIIII
! NUM_DO=~~`IIIIIIIIII  NUM_SKIP=~~`IIIIIIIIII
!`-----------------------------------------------
!</gui_def>

! Number_Groups[/en]=~~`IIIIIIIIII Items_per_Grp[/en]=~~`IIIIIIIIII
! Move[moves/en] `IIII items before next grp[Direction/en] `AAAAAAAA

!<HelpSection>
!
!           rowgroup = TR_MAX SKIP_INIT
!           rowgroup = NUM_DO NUM_SKIP
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of items to ultimately select. </Tip>
! Default = 99999999
! Allowed = int >= 0
! Selection will stop once tr_max items have been selected with the do_skip
! pattern. TR_MAX is the maximum number of items SELECTED by do_skip.
!</Help>
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of items to skip over initially before selecting any. </Tip>
! Default = 0
! Allowed = int >= 0
! The DO-SKIP item selection method consists of initially skipping SKIP_INIT
! items, then sequentially processing ("doing") NUM_DO consecutive items 
! and skipping NUM_SKIP consecutive items.
!</Help>
!
!<Help KEYWORD="NUM_DO">
!<Tip> Number of items to process at a time in the DO-SKIP selection. </Tip>
! Default = 1
! Allowed = int > 0
! The DO-SKIP item selection method consists of initially skipping SKIP_INIT
! items, then sequentially processing ("doing") NUM_DO consecutive items 
! and skipping NUM_SKIP consecutive items.
!</Help>
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of items to skip at a time in the DO-SKIP selection. </Tip>
! Default = 0
! Allowed = int (positive,negative, or 0 unless restricted by the programmer.)
! The DO-SKIP item selection method consists of initially skipping SKIP_INIT
! items, then sequentially processing ("doing") NUM_DO consecutive items 
! and skipping NUM_SKIP consecutive items.
!</Help>
!</HelpSection>

!<Help KEYWORD="NUMBER_GROUPS">
!<Tip> Shows how many groups of "num_do" items will be passed through.</Tip>
!</Help>
!<Help KEYWORD="ITEMS_PER_GRP">
!<Tip> Shows how many items are in each group to be passed through.</Tip>
!</Help>
!<Help KEYWORD="MOVES">
!<Tip> Shows how many items forward/backward we move before next group.</Tip>
!</Help>
!<Help KEYWORD="DIRECTION">
!<Tip> Shows the direction of movement within the input data set.</Tip>
!</Help>

!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module dooskip_module

use pc_module
use string_module

implicit none

private

character(len=100),public,save :: dooskip_IDENT = &
'$Id: dooskip.f90,v 1.10 2007/07/13 12:56:22 Stoeckley beta sps $'

public :: dooskip_struct,dooskip_error,dooskip_ok
public :: dooskip,dooskip_create,dooskip_initialize,dooskip_update
public :: dooskip_delete, dooskip_wrapup, dooskip_tr_max_trap
public :: dooskip_skip_init_trap,dooskip_num_do_trap,dooskip_num_skip_trap
public :: dooskip_hit_tr_max,dooskip_get_tr_max,dooskip_put_tr_max
public :: dooskip_get_skip_init,dooskip_put_skip_init
public :: dooskip_get_num_do,dooskip_put_num_do,dooskip_neg_skip_ok
public :: dooskip_get_num_skip,dooskip_put_num_skip
public :: dooskip_state,dooskip_nbr_done,dooskip_next_item
public :: dooskip_thru_item,dooskip_calc_trmax,dooskip_reset

integer,parameter  :: dooskip_error = -1
integer,parameter  :: dooskip_ok    =  0

type :: dooskip_struct              
  private
  integer            :: tr_max  
  integer            :: skip_init
  integer            :: num_do
  integer            :: num_skip
  integer            :: this_item
  integer            :: hit_count
  integer            :: next_item
  integer            :: nbrdone_grp
  integer            :: nbrdone_tot
  logical            :: do_item
  logical            :: hit_tr_max
  logical            :: negative_skipping_ok
  integer            :: stdout
  !-------- These are for display only fields on gui.-------
  character(len=8)   :: direction
  integer            :: items_per_grp
  integer            :: number_groups
  integer            :: moves
end type dooskip_struct

type(dooskip_struct),pointer,save :: trp

contains

subroutine dooskip_reset(obj)
  type(dooskip_struct),pointer :: obj
  !------- reset the object -----------


  obj%this_item    = obj%skip_init
  obj%hit_count    = 0
  obj%nbrdone_grp  = 0
  obj%nbrdone_tot  = 0
  obj%do_item      = .false.
  obj%hit_tr_max   = .false.
  obj%next_item    = obj%this_item + 1
  
end subroutine dooskip_reset

function dooskip_calc_trmax(obj,thru_item) result(tr_max_est)
  !--- calc tr_max based on a given "thru_item" (number of records in file)
  type(dooskip_struct),pointer :: obj
  integer,intent(in)           :: thru_item
  !--- result
  integer                      :: tr_max_est
  !--- local
  integer                      :: rm,nb

  tr_max_est = -1
  if(.not. associated(obj)) return


  if(obj%num_do + obj%num_skip == 0 ) then
    !--- tr-max is indeterminate so put at system max. ---
    tr_max_est = 99999999
    return
  endif
  !--- number of FULL do-skip blocks available ---
  nb = (thru_item - obj%skip_init)/(obj%num_do + obj%num_skip)
  !--- remaining number of traces in last partial block ---
  rm = thru_item - obj%skip_init  - (obj%num_do + obj%num_skip)*nb
  !--- First, tr_max calc is based on only FULL blocks
  tr_max_est = nb*obj%num_do
  !--- Now, look at ndo and rm to see how many to add. ---
  if(rm >= obj%num_do ) then
    tr_max_est = tr_max_est + obj%num_do
  elseif(rm < obj%num_do) then
    tr_max_est = tr_max_est + rm
  endif

  !--- tr_max_est can be negative here... caveat emptor

end function dooskip_calc_trmax
  

function dooskip_thru_item(obj) result (thru_item)
  type(dooskip_struct),pointer :: obj
  integer                      :: thru_item
 
  thru_item = -1
  if(.not. associated(obj)) return
  thru_item = obj%skip_init + &
              (obj%tr_max/obj%num_do)*(obj%num_do+obj%num_skip) + &
              modulo(obj%tr_max,obj%num_do)
end function dooskip_thru_item


subroutine dooskip_state(obj)
  type(dooskip_struct),pointer :: obj
  integer                      :: s
  if(.not. associated(obj)) return
  s=obj%stdout
  write(s,'(A)')'--------------- DO_SKIP STRUCTURE CURRENT STATE --------------'
  write(s,'(3(A,I08),a,L08)')&
                       '      Tr_max: ',obj%tr_max,&
                       '   Skip_init: ',obj%skip_init,&  
                       '      Num_do: ',obj%num_do,&
                       '  NegSkipOk?: ',obj%negative_skipping_ok
  write(s,'(4(a,I08))')&
                       '    Num_skip: ',obj%num_skip,&
                       '   This_item: ',obj%this_item,&
                       '   Hit_count: ',obj%hit_count,&
                       '   Next_item: ',obj%next_item
  write(s,'(2(a,I08),2(a,L08))')&
                       ' Nbrdone_grp: ',obj%nbrdone_grp,&
                       ' Nbrdone_tot: ',obj%nbrdone_tot,&
                       '     Do_item: ',obj%do_item,&
                       '  Hit_tr_max: ',obj%hit_tr_max
  write(s,'(A)')'--------------------------------------------------------------'
end subroutine dooskip_state

subroutine dooskip_neg_skip_ok(obj,neg_skip_ok)
  type(dooskip_struct),pointer :: obj
  logical                      :: neg_skip_ok
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) return
  endif
  obj%negative_skipping_ok      = neg_skip_ok
end subroutine dooskip_neg_skip_ok
  

function dooskip_hit_tr_max(obj) result (hit_tr_max)
  type(dooskip_struct),pointer :: obj
  logical                     :: hit_tr_max
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      hit_tr_max = .true.
      return
    endif
  endif
  hit_tr_max = obj%hit_tr_max
end function dooskip_hit_tr_max
 
function dooskip_nbr_done  (obj) result(nbr_done  )
  type(dooskip_struct),pointer :: obj
  integer                     :: nbr_done
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      nbr_done   = -1
      return
    endif
  endif
  nbr_done=obj%nbrdone_tot
end function dooskip_nbr_done
 
function dooskip_next_item  (obj) result(next_item  )
  type(dooskip_struct),pointer :: obj
  integer                     :: next_item
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      next_item   = -1
      return
    endif
  endif
  next_item=obj%next_item
end function dooskip_next_item
 
function dooskip_get_tr_max  (obj) result(tr_max  )
  type(dooskip_struct),pointer :: obj
  integer                     :: tr_max
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      tr_max   = -1
      return
    endif
  endif
  tr_max=obj%tr_max
end function dooskip_get_tr_max

function dooskip_get_skip_init(obj) result(skip_init)
  type(dooskip_struct),pointer :: obj
  integer                     :: skip_init
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      skip_init = -1
      return
    endif
  endif
  skip_init = obj%skip_init
end function dooskip_get_skip_init

function dooskip_get_num_do   (obj) result(num_do   )
  type(dooskip_struct),pointer :: obj
  integer                     :: num_do
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      num_do = -1
      return
    endif
  endif
  num_do=obj%num_do
end function dooskip_get_num_do

function dooskip_get_num_skip (obj) result(num_skip )
  type(dooskip_struct),pointer :: obj
  integer                     :: num_skip
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      num_skip = -1
      return
    endif
  endif
  num_skip=obj%num_skip
end function dooskip_get_num_skip


function dooskip_put_tr_max(obj,tr_max) result(status )
  type(dooskip_struct),pointer :: obj
  integer                     :: tr_max
  integer                     :: status
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      status = dooskip_error
      return
    endif
  endif
  if(tr_max >= 0 ) then
    status = dooskip_ok
    obj%tr_max=tr_max
  else
    status = dooskip_error
  endif
end function dooskip_put_tr_max

function dooskip_put_skip_init(obj,skip_init) result(status )
  type(dooskip_struct),pointer :: obj
  integer                     :: skip_init
  integer                     :: status
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      status = dooskip_error
      return
    endif
  endif
  if(skip_init >= 0 ) then
    status = dooskip_ok
    obj%skip_init=skip_init
    obj%next_item = obj%skip_init + 1
  else
    status = dooskip_error
  endif
end function dooskip_put_skip_init

function dooskip_put_num_do   (obj,num_do   ) result(status )
  type(dooskip_struct),pointer :: obj
  integer                     :: num_do
  integer                     :: status
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      status = dooskip_error
      return
    endif
  endif
  if(num_do > 0 ) then
    status = dooskip_ok
    obj%num_do=num_do
  else
    status = dooskip_error
  endif
end function dooskip_put_num_do

function dooskip_put_num_skip (obj,num_skip ) result(status )
  type(dooskip_struct),pointer :: obj
  integer                     :: num_skip
  integer                     :: status
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) then
      status = dooskip_error
      return
    endif
  endif
  status = dooskip_ok
  obj%num_skip=num_skip
end function dooskip_put_num_skip

function dooskip(obj,this_item,next_item) result (do_this_one)
  !--- if result = true then DO  ,if result=false, SKIP
  !--- Call this every time you would normally increment the counter of items.
  !--- do-skip will increment the counter, figure out the next item that 
  !--- will be "done", and compare with pattern to see if the item should be
  !--- skipped or done.
  type(dooskip_struct),pointer               :: obj
  integer,optional,intent(  in )            ::  this_item
  integer,optional,intent(  out)            ::  next_item
  logical                                   ::  do_this_one

  !--- assume we won't be doing anything.
  do_this_one = .false.

  !--- bad error
  if(.not. associated(obj)) then 
    if(dooskip_create(obj) /= dooskip_ok) return
  endif

  obj%hit_count  = obj%hit_count + 1

  if(obj%hit_tr_max) go to 9

  if(present(this_item)) then 
    obj%this_item=this_item
  else
    obj%this_item = obj%this_item  + 1
  endif
  if(present(next_item)) next_item=obj%next_item

  if(obj%nbrdone_tot > obj%tr_max ) then
    obj%hit_tr_max = .true. 
    go to 9
  endif

  if(obj%next_item == obj%this_item ) then
    obj%nbrdone_grp = obj%nbrdone_grp + 1
    obj%nbrdone_tot = obj%nbrdone_tot + 1
    if(obj%nbrdone_tot > obj%tr_max ) then
      obj%nbrdone_tot = obj%nbrdone_tot - 1
      obj%nbrdone_grp = obj%nbrdone_grp - 1
      obj%hit_tr_max  = .true. 
      go to 9
    else

      do_this_one = .true.

      if(obj%nbrdone_grp == obj%num_do ) then
        obj%nbrdone_grp = 0
        obj%next_item = obj%this_item  + obj%num_skip
      endif
      obj%next_item = obj%next_item + 1
    endif
  else

    do_this_one = .false.

  endif

9 continue
  obj%do_item = do_this_one
  if(present(next_item)) next_item=obj%next_item

end function dooskip

function dooskip_create (obj) result (status)
  type(dooskip_struct),pointer :: obj       ! arguments
  integer                     :: status
  
  status   = dooskip_ok
  if(associated(obj)) then
    status = dooskip_error
    return
  endif
  
  allocate (obj)
  
  if(.not. associated(obj)) then
    status = dooskip_error
    return
  endif

  status = dooskip_initialize(obj)
  
end function dooskip_create


function dooskip_delete (obj) result (status)
  type(dooskip_struct),pointer :: obj
  integer                     :: status
  
  status = dooskip_ok
  
  if(.not. associated(obj)) return
  
  deallocate(obj)
  
  if(associated(obj)) then
    status = dooskip_error
    return
  endif

end function dooskip_delete

function dooskip_wrapup     (obj) result (status)
  type(dooskip_struct),pointer :: obj
  integer :: status
  status = dooskip_delete(obj)
end function dooskip_wrapup

function dooskip_initialize(obj) result (status)

  type(dooskip_struct),pointer :: obj
  integer                     :: status
  
  status = dooskip_ok
  
  if(.not. associated(obj)) then
    status = dooskip_create(obj)
    return
  endif
  
  obj%tr_max              = 99999999
  obj%skip_init           = 0
  obj%num_do              = 1
  obj%num_skip            = 0
  obj%this_item           = 0
  obj%hit_count           = 0
  obj%next_item           = 1
  obj%nbrdone_grp         = 0
  obj%nbrdone_tot         = 0
  obj%do_item             = .true.
  obj%hit_tr_max          = .false.
  obj%negative_skipping_ok= .true.
  obj%stdout              = 6
  obj%moves               = 1
  obj%direction           = 'forward'
  obj%number_groups       = obj%tr_max
  obj%items_per_grp       = 1

end function dooskip_initialize

function dooskip_update(obj) result (status)
  type(dooskip_struct),pointer :: obj
  integer                     :: status
 
  status = dooskip_ok 

  if(.not. associated(obj)) then
    status = dooskip_create(obj)
    if(status /= dooskip_ok) return
  endif

  trp => obj
  obj%stdout = pc_get_lun()

  !call pc_put_sensitive_field_flag('moves',.false.)
  !call pc_put_sensitive_field_flag('direction',.false.)
  !call pc_put_sensitive_field_flag('number_groups',.false.)
  !call pc_put_sensitive_field_flag('items_per_grp',.false.)

  call pc_get ('tr_max'   , obj%tr_max   , dooskip_tr_max_trap)
  call pc_get ('skip_init', obj%skip_init, dooskip_skip_init_trap)
  call pc_get ('num_do'   , obj%num_do   , dooskip_num_do_trap)
  call pc_get ('num_skip' , obj%num_skip , dooskip_num_skip_trap)

  obj%next_item = obj%skip_init + 1

  call pc_put ('tr_max'   , obj%tr_max   )
  call pc_put ('skip_init', obj%skip_init)
  call pc_put ('num_do'   , obj%num_do   )
  call pc_put ('num_skip' , obj%num_skip )

  !call pc_put ('moves',obj%moves)
  !call pc_put ('direction',obj%direction)
  !call pc_put ('number_groups',obj%number_groups)
  !call pc_put ('items_per_grp',obj%items_per_grp)

  if(pc_update_error()) then
    status = dooskip_error
  else
    status = dooskip_ok
  endif

end function dooskip_update

subroutine dooskip_tr_max_trap(tr_max)
  character(len=*),intent(in) :: tr_max
  if(trp%tr_max < 0 ) then
    trp%tr_max = 0
    call pc_error  ('do_skip: error: tr_max must be 0 (none) or greater.')
    call pc_jump_field(tr_max)
  endif

  call dooskip_update_fields

end subroutine dooskip_tr_max_trap

subroutine dooskip_skip_init_trap(skip_init)
  character(len=*),intent(in) :: skip_init
  !character(len=1)            :: plurali

  !integer                     :: i
  if(trp%skip_init < 0 ) then
    trp%skip_init = 0
    call pc_error  ('do_skip: error: skip_init must always be greater than 0.')
    call pc_jump_field(skip_init)
  endif
  !if(trp%skip_init > 1 )  then
  !  plurali = 's'
  !else
  !  plurali = ''
  !endif
  !if(trp%skip_init > 0 ) then
    !write(string,'(A,I11,A)')'do_skip: You will skip ',trp%skip_init, &
    !  ' item'//plurali//' before selecting any.'
    !call string_compress_blanks(string,i)
    !call pc_info(string(:i))
  !endif

  call dooskip_update_fields

end subroutine dooskip_skip_init_trap

subroutine dooskip_num_do_trap(num_do)
  character(len=*),intent(in) :: num_do
  if(trp%num_do > trp%tr_max) then
    call pc_info('do_skip: note: num_do is greater than tr_max.')
  endif
  if(trp%num_do <= 0 ) then
    call pc_error  ('do_skip: error: num_do must always be greater than 0.')
    trp%num_do = 1
    call pc_jump_field(num_do)
  endif

  call dooskip_update_fields

end subroutine dooskip_num_do_trap

subroutine dooskip_num_skip_trap(num_skip)
  character(len=*),intent(in) :: num_skip



  if(trp%num_skip < 0 .and. (.not. trp%negative_skipping_ok)) then
      call pc_error  ('do_skip: error: num_skip must be >= 0 in this case.')
      trp%num_skip = 0 
      call pc_jump_field(num_skip)
  endif

  call dooskip_update_fields

end subroutine dooskip_num_skip_trap

subroutine dooskip_update_fields
  character(len=132)          :: string
  character(len=1)            :: pluralg , plurali,pluralj 
  character(len=8)            :: fwdbck



  if (trp%num_do == 0 ) trp%num_do = 1

  pluralg = ''
  plurali = ''
  pluralj = ''
  if(trp%tr_max/trp%num_do > 1 )           pluralg = 's'
  if(trp%num_do > 1 )                      plurali = 's'
  if((abs(trp%num_do + trp%num_skip) > 1)) pluralj = 's'
  if(    (trp%num_do+trp%num_skip)  > 0 ) then
    fwdbck='forward'
  elseif((trp%num_do+trp%num_skip) == 0 ) then
    fwdbck='repeat'
  else
    fwdbck='backward'
  endif

  if(trp%num_skip < 0 ) then
    if(trp%num_skip == - trp%num_do ) then
        write(string,'(A,I11,A,I11,A)')'do_skip: You have selected ',&
        trp%tr_max/trp%num_do,' instance'//pluralg//&
        ' of the same group of ',trp%num_do,' item'//plurali//'.'
    else
        write(string,'(A,I11,A,I11,A,I11,A)')'do_skip: You have selected ',&
        trp%tr_max/trp%num_do, ' group'//pluralg//' of ',trp%num_do, &
        ' item'//plurali//' that shift '//fwdbck//' by ',&
        abs(trp%num_do + trp%num_skip),' item'//pluralj//' each time.'
    endif
  elseif(trp%num_skip > 0 ) then
    write(string,'(A,I11,A,I11,A,I11,A)')'do_skip: You have selected ',&
    trp%tr_max/trp%num_do, ' group'//pluralg//' of ',trp%num_do, &
    ' item'//plurali//' that shift '//fwdbck//' by ',&
    (trp%num_do + trp%num_skip),' item'//pluralj//' each time.'
  endif
  !call string_compress_blanks(string,i)
  !call pc_info(string(:i))

  trp%direction      = fwdbck
  trp%moves          = (trp%num_do+trp%num_skip)
  trp%items_per_grp  = trp%num_do
  trp%number_groups  = (trp%tr_max/trp%num_do)

end subroutine dooskip_update_fields

end module dooskip_module
