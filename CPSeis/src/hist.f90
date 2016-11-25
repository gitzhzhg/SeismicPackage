!<CPS_v1 type="PRIMITIVE"/>
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
!                C P S   P R I M I T I V E               
!
! Name       : HIST
! Category   : main_prog
! Written    : 1999-09-14   by: Bill Menger
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Maintain current history in a cardset
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                 GENERAL DESCRIPTION                   
! Routines for maintaining the current history file.  A process may write to
! the current history
! by sending hist its ipn and an 80 byte history card.  Any process may
! read any history card, print out cards, write cards, or send cards to stdout.
! The main program can then write all cards to a file or stdout at job end.
! Standard usage:
! From within a process:
!   if(hist_init(ipn,process_name) /= hist_ok) exit
!   ...
!   if(hist_write(ipn,card) == hist_ok) then
!     exit
!   else
!     call pc_error('error writing history card')
!   endif
!
! From main program:
!   at job end:
!   call hist_print()
! From TROT, TTROT, etc:
!   at job end:
!   call hist_print(unit,maxipn,minipn)
!
!-------------------------------------------------------------------------------
!</descript_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!              INPUT AND OUTPUT ARGUMENTS               
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                  CALLING SEQUENCE and Public Data
!
!  ----------------------------
!  DATA
!  ----------------------------
!    hist_ok    ( all is well                  (=  0) )
!    hist_error ( all is not well              (= -1) )
!    hist_length( the length of a history card (= 80) )
!  ----------------------------
!  SUBROUTINES
!  ----------------------------
!    call hist_open()
!        Purpose: Allocate the history memory structure.
!                 This is only here for the purist who thinks they need to 
!                 open a "file" before writing to it.  Since history
!                 isn't using "files" this is not necessary.
!                 The memory structure is allocated when the first
!                 write occurs.
!    call hist_close()
!        Purpose: Deallocate the history memory structure (also serves to
!                 "wipe" all history cards that have been written previously
!                 in this job.  the next write operation will reallocate a
!                 structure or one could call hist_open() again.
!  ----------------------------
!  FUNCTIONS
!  ----------------------------
!    ARGUMENTS
!     integer                    :: status --> 2 states: hist_ok or 
!                                                        hist_error
!     integer                    :: ipn    --> any integer up to 8 characters
!                                              in length
!                                              (from -9999999 to 99999999 )
!     character(len=hist_length) :: card   --> the history card
!     character(len=hist_length) :: cards(:)-> a set of history cards
!     integer                    :: cardnum -> The specific card number for
!                                              an ipn. 
!     integer                    :: numcards > how many cards are returned.
!                                              The number may exceed the
!                                              size(cards) but no data will
!                                              be written past the end of
!                                              your local cards array.
!     integer                    :: unit   --> The unit number to print to.
!                                              If < 100 it uses Fortran I/O
!                                              If > 100 it uses cio
!                                              If missing it prints to 6.
!     character(len=*)           :: process_name --> the process's name!
!                     i      i
! status = hist_init(ipn,process_name)
!        Purpose: initialize the history section for a process (ipn).
!        (CALL this BEFORE hist_write for each ipn)
!                      i    i
! status = hist_write(ipn,card) 
!        Purpose: write a history card pertaining to process number "ipn"
! status = hist_write(ipn,cards) 
!        Purpose: write a set of history cards for process number "ipn"
!                     i      o       o
! status = hist_read(ipn, numcards, cards)
!        Purpose: read a set of history cards for process number "ipn"
!        Returned: numcards for this ipn (can exceed size(cards))
!                     i      i       o
! status = hist_read(ipn, cardnum , card)
!        Purpose: read history card number "cardnum" for process number "ipn"
!
!                     i(opt)   i(opt)    i(opt)   i(opt)
! status = hist_print(unit   , jobname,  maxipn,  minipn )
!        Purpose: write the histories to standard output or to a file.
!
!                           i(opt)
! numcards = hist_numcards(ipn)
!        Purpose: pre-determine how many cards exist for an ipn OR
!                 If ipn is missing, for the entire history set.
!
!-------------------------------------------------------------------------------
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                   REVISION HISTORY                     
!     Date        Author          Description
!     ----        ------          -----------
!011. 2006-09-18  D. Glover       Added NULLIFY statements for Intel compiler.
! 10. 2006-01-10  B. Menger       Removed Unused Variables.
!  9. 2001-08-27  Karen Goodger   Change hist_print. Add function hist_maxipn.
!  8. 2001-02-13  Bill Menger     Added ident string
!  7. 2001-01-25  Bill Menger     Changed hist_init to wipe out the cards for
!                                 an ipn if it is called, prior to adding crds.
!  6. 2000-04-06  Bill Menger     Modified all functions to test for hist obj.
!  5. 2000-04-05  Bill Menger     Allowed range of ipn's for print-hist.
!  4. 2000-03-16  Bill Menger     Added new format to output on hist_print,
!                                 added optional parameter for hist_print.
!  3. 2000-03-10  Bill Menger     Added hist_init and changed icn to ipn.
!  2. 2000-02-29  Bill Menger     Finished initial version for current history.
!  1. 1999-09-14  Karen Goodger   Stub history_write to replace wuhist.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                 PORTABILITY LIMITATIONS                
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!<programming_doc>
!-------------------------------------------------------------------------------
!                   PROGRAMMING NOTES                    
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module hist_module

use string_module
use cardsetlist_module
use cardset_module
use cio_module

implicit none
private

public :: hist_read,hist_open,hist_close,hist_write,hist_numcards
public :: hist_ok , hist_error,hist_length,hist_print,hist_init
public :: hist_maxipn

character(len=100),save,public :: hist_ident = &
  "$Id: hist.f90,v 1.11 2006/09/18 13:32:48 Glover prod sps $"


!!----------------------------- data --------------------------------------!!
!!----------------------------- data --------------------------------------!!
!!----------------------------- data --------------------------------------!!

!----------------- PARAMETERS  FOR THIS OBJECT ------------------------------
integer,parameter                        :: hist_error  = -1
integer,parameter                        :: hist_ok     =  0
integer,parameter                        :: hist_length = 80
!----------------- GLOBALS FOR THIS OBJECT ----------------------------------
logical,save                             :: initialized = .false.
type(cardsetlist_struct),pointer,save    :: hist
type(cardset_struct),pointer,save        :: this

!!----------------------------- interfaces --------------------------------!!
!!----------------------------- interfaces --------------------------------!!
!!----------------------------- interfaces --------------------------------!!

interface hist_numcards
  module procedure hist_numcards_ipn
  module procedure hist_numcards_all
end interface

interface hist_read
  module procedure hist_read_one
  module procedure hist_read_all
end interface

interface hist_write
  module procedure hist_write_one
  module procedure hist_write_all
end interface

contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

  subroutine hist_open()
    integer                         :: status
    status = hist_create()
  end subroutine hist_open

  subroutine hist_close()
    integer                         :: status
    status = hist_delete()
  end subroutine hist_close

!!----------------------------- functions ---------------------------------!!
!!----------------------------- functions ---------------------------------!!
!!----------------------------- functions ---------------------------------!!

  function hist_create() result (status)
    integer                         :: status
    if(.not.initialized) then
      nullify (hist) ! jpa
      call cardsetlist_create(hist) 
      if ( .not. associated(hist) ) then
        !--- we did not associate the history pointer with a cardsetlist obj.
        status = hist_error
        return
      endif
      initialized = .true.
    endif
    status = hist_ok
  end function hist_create

  function hist_delete() result (status)
    integer                           :: status
    if (hist_create() == hist_ok ) then
      call cardsetlist_delete(hist)
      if( .not. associated(hist) ) then
        initialized = .false.
        status = hist_ok
        return
      endif
    endif
    status = hist_error
  end function hist_delete

  function hist_init(ipn,process_name) result(status)
    integer, intent(in)               :: ipn
    character (len=*),  intent(in)    :: process_name
    integer                           :: status
    ! --- locals ---
    character (len=HIST_LENGTH)       :: localcard
    integer                           :: i
    character (len=8)                 :: name

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    !--- Create title card for the process (the ipn) ---
    write(localcard,'(a14,i3,a10,a)') &
    '*HISTORY IPN: ',ipn,' PROCESS: ',trim(process_name)//' '
    do i = 14+3+10+len_trim(process_name)+2,HIST_LENGTH
      localcard(i:i) = '*'
    end do

    !--- Does this ipn exist yet? If so, wipe the cards.
    name = hist_name_from_ipn(ipn)  ! name is simply ipn converted to character 
    this => cardsetlist_find_cardset(hist,name)
    if(associated(this)) call cardset_clear(this)

    !--- Write the title card
    status = hist_write_one(ipn,localcard)

  end function hist_init

  function hist_write_one(ipn,card) result(status)
    integer, intent(in)               :: ipn
    character (len=*),  intent(in)    :: card
    integer                           :: status
    ! --- locals ---
    character (len=HIST_LENGTH+1)     :: localcard
    character (len=8)                 :: name
    status = hist_ok

    !--- ensure only 80 characters.
    localcard = card(:min(len(card), HIST_LENGTH ))
    localcard(min(len(card), HIST_LENGTH )+1:)=' '

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    !--- does the cardset exist?  If not, create a new one.
    name = hist_name_from_ipn(ipn)
    this => cardsetlist_find_cardset(hist,name)
    if(.not. associated(this)) then
      this => cardsetlist_create_new_cardset(hist,name)
      if(.not. associated(this) ) then
        status = hist_error
        return
      endif
    endif
    call cardset_add_card(this,localcard(:hist_length))

  end function hist_write_one

  function hist_write_all(ipn,cards) result (status)
    integer, intent(in)                         :: ipn
    character (len=*),dimension(:), intent(in)  :: cards
    integer                                     :: status
    ! --- locals ---
    integer                                     :: i

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    do i = 1, size(cards)
      status = hist_write_one(ipn,cards(i))
      if(status /= hist_ok) return
    end do
  end function hist_write_all

  function hist_name_from_ipn(ipn) result(name)
    integer, intent(in)             :: ipn
    character(len=8)                :: name
    !--- the cardset containing an ipn's history is in cardset "name"
    name = string_ii2ss(ipn,8)
  end function hist_name_from_ipn

  function hist_maxipn() result (maxipn)
    integer :: maxipn
    maxipn=cardsetlist_num_cardsets(hist)
  end function hist_maxipn

  function hist_numcards_all() result (numcards)
    integer                         :: numcards
    !-------------------------------------------
    character(len=8)                :: name
    integer                         :: ipn,numipn,i
    integer                         :: istat,status
    character(len=80)               :: errmsg

    numcards = 0
    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    numipn=cardsetlist_num_cardsets(hist)
    do i = 1, numipn
      this  => cardsetlist_get_cardset(hist,i)
      call cardset_get_name(this,name)
      !read(name,'(i8)')ipn
      ipn = string_ss2ii (name,istat,errmsg)
      !if(istat < 0 ) print*, trim(errmsg)
      numcards = numcards + hist_numcards_ipn(ipn)
    end do
  end function hist_numcards_all

  function hist_numcards_ipn(ipn) result (numcards)
    integer, intent(in)             :: ipn
    integer                         :: numcards
    !------------------local--------------------
    character(len=8)                :: name

    integer                         ::       status 
    !------------------start--------------------
    numcards = hist_error

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    !--- does the cardset exist? if not, return error.
    name = hist_name_from_ipn(ipn)
    this => cardsetlist_find_cardset(hist,name)
    if(.not. associated(this)) return
    numcards = cardset_num_cards(this)
  end function hist_numcards_ipn

  function hist_read_one(ipn,cardnum,card) result (status)
    integer, intent(in)               :: ipn
    integer, intent(in)               :: cardnum
    character (len=*),intent(out  )   :: card
    character (len=CARDSET_LENGTH)    :: card_internal,errmsg
    integer                           :: status
    !---------------------------------------------
    integer                           :: numcards

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    status = hist_error

    !--- does this cardnum exist? if not, return error.  
    numcards = hist_numcards(ipn)
    if ( numcards < cardnum) then
      return
    endif

    call cardset_get_card    (this, cardnum,  card_internal,   errmsg)
    if(len_trim(errmsg) > 0 ) then
      return ! error retrieving card.
    endif

    card(1:len(card)) = card_internal(:min(len(card),CARDSET_LENGTH))

    status = hist_ok
  end function hist_read_one

  function hist_read_all(ipn, numcards, cards) result (status)
    integer, intent(in)                          :: ipn
    integer, intent(out)                         :: numcards
    character (len=*),dimension(:),intent(inout) :: cards
    integer                                      :: status
    !---------------------------------------------
    integer                                      :: i

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    numcards = hist_numcards(ipn)
    if(numcards > size(cards) ) status = hist_error

    do i = 1, min(numcards,size(cards))
      status = hist_read_one(ipn,i,cards(i))
      if(status /= hist_ok) return
    end do
  end function hist_read_all

  function hist_print(unit,jobname,maxipn,minipn) result(status)
    integer,intent(in),optional          :: unit
    character(len=*),intent(in),optional :: jobname
    integer,intent(in),optional          :: maxipn
    integer,intent(in),optional          :: minipn
    integer                              :: status
    !--- local variables
    integer                     :: local_unit,local_maxipn,local_minipn
    integer                     :: thisipn(1), thisloc(1)
    integer                     :: numipn,istat,i,j,totnumcards
    integer,allocatable         :: ipn(:)
    character(len=8)            :: name
    character(len=32)           :: local_jobname
    character(len=hist_length)  :: card,errmsg
    character(len=96)           :: line
    !--- start

    !--- Do we have a history object yet?
    status = hist_create()
    if(status /= hist_ok) return
    !--- History object is established.

    if(present(jobname) ) then
      local_jobname = trim(jobname(:min(len(jobname),32)))
    else
      local_jobname='UNTITLED'
    endif

    if(present(unit) ) then
      local_unit = unit
    else
      local_unit = 6
    endif

    if(present(maxipn) ) then
      local_maxipn = maxipn
    else
      local_maxipn = 99999999
    endif

    if(present(minipn) ) then
      local_minipn = minipn
    else
      local_minipn = 1
    endif

    totnumcards = 0
    numipn=cardsetlist_num_cardsets(hist)
    allocate(ipn(numipn),stat=status)
    do i = 1, numipn
      this  => cardsetlist_get_cardset(hist,i)
      call cardset_get_name(this,name)
      ipn(i) = string_ss2ii (name,istat,errmsg)
      !---- Restrict ipn's to print based on user's request.
      if(ipn(i) < local_minipn .or. ipn(i) > local_maxipn ) then
        ipn(i) = 99999999
      else
        totnumcards = totnumcards + cardset_num_cards(this)
      endif
    end do

!!    line= ' Current history record.'
!!    write(card,'(a,i4,a)')' Name='//trim(local_jobname)//' Length= ',&
!!     totnumcards,' '
!!    istat = cio_fputline(trim(line),len_trim(line),local_unit) &
!!            - 1 - len_trim(line)
!!    istat = cio_fputline(trim(card),len_trim(card),local_unit) &
!!            - 1 - len_trim(card)

    do i = 1, numipn
      !--- walk through ipn list in increasing IPN order.
      thisipn      = minval(ipn)
      thisloc      = minloc(ipn)
      ipn(thisloc(1)) = 99999999
      !--- If thisipn is 999999999 then we are finished (early?)
      if(thisipn(1) == 99999999) exit
      j = 1
      do
        if( hist_read_one(thisipn(1),j,card) /= hist_ok ) exit
        j = j + 1
        write(line,'(2x,a4,4x,a80,2x,a4)')'****',card(1:80),'****'
        istat = cio_fputline(line,96,local_unit) - 97
        if(istat /= 0 ) then
          deallocate(ipn)
          status = istat
          return
        endif
      end do
      write(line,'(2x,a4,4x,80x,2x,a4)')'****','****'
      istat = cio_fputline(line,96,local_unit) - 97

    end do
    do i = 1,2
      istat = cio_fputline('',0,local_unit) - 1
    end do
    
    deallocate(ipn)

  end function hist_print
        
end module hist_module
