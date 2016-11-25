!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- fxcard.f90 -------------------------------!!
!!------------------------------- fxcard.f90 -------------------------------!!
!!------------------------------- fxcard.f90 -------------------------------!!

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
!------------------------------------------------------------------------------ 
!                        C P S   P R I M I T I V E
!
! Name       : fxcard    ( FX IO routines )
! Category   : migrations
! Written    : 2002-09-04   by: Douglas Hanson
! Revised    : 2006-08-10   by: D. Glover
! Maturity   : beta
! Purpose    : Card IO routines for FX migrations.
! Portability: No known limitations.
! Parallel   : Yes
!
!------------------------------------------------------------------------------ 
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------ 
!                         GENERAL DESCRIPTION
!
!  This routine handles IO processes for FX migrations.
!
!------------------------------------------------------------------------------ 
!</descript_doc>
 
!<calling_doc>
!------------------------------------------------------------------------------ 
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!------------------------------------------------------------------------------ 
!                          CALLING SEQUENCE
!
!
! create the fxcard structure, f_s
!  call fxcard_create_1 ( &
!                      o
!                     i_err &
!                    )
!
! delete the fxcard structure, f_s
!                         b      o
!  call fxcard_delete ( f_s, i_err )
!
! i_err           integer         error code flag 0 = o.k. < 0 = error.
!
!------------------------------------------------------------------------------ 
!</calling_doc>

!<advice_doc>
!------------------------------------------------------------------------------ 
!                            ADVICE FOR USERS
!
!------------------------------------------------------------------------------ 
!</advice_doc>

!<history_doc>
!------------------------------------------------------------------------------ 
!                           REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!018. 2006-08-10  D. Glover     Added NULLIFY statements for Intel compiler.
! 17  2006-06-22 Douglas Hanson Change f_par to p.
! 16  2005-08-25 Ioan Vlad      Remove unused variables.
! 16  2005-07-12 Douglas Hanson Fix remove file in card_to_file
! 15  2005-05-24 Douglas Hanson remove file in card_to_file
! 14  2005-01-31 R.S.Day        Promoted to beta.
! 13  2004-08-17 R.S.Day        Fix parallel logic in fxcard_file_num_card
! 12  2004-08-12 Douglas Hanson Add fxcard_file_num_card
! 11  2004-06-22 Hanson - Liu   Make file_lock as an option.
! 10  2004-03-26 Douglas Hanson Add if labels.
!  9  2004-03-25 Douglas Hanson Add lock status print.
!  8  2004-01-21 Douglas Hanson Replace fxpar calls with pcpsx calls.
!  7  2003-12-08 R.S.Day        Generate fatal error if expired lock condition
!                               is detected. Change write open to w+ to avoid
!                               file removal which can interfer with locks.
!                               More io error checks.
!  6  2003-06-10 R.S.Day        lock_name seperated from file name
!  5  2003-04-15 R.S.Day        Added cio_update_file_time call before call to
!                               cio_unlock_file call to solve NFS cache
!                               problem in Houston.
!  4  2002-10-28 Douglas Hanson Fix fxcard_check_path.
!  3  2002-10-21 Douglas Hanson Fix fxcard_card_to_card limits.
!  2  2002-09-24 Douglas Hanson Increase a_card to 500.
!  1  2002-09-18 Douglas Hanson Initial version.
!
!------------------------------------------------------------------------------ 
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!                         PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</portability_doc>
!
module fxcard_module
  !
  use cardset_module
  use cio_module
  use finquire_module
  use fxpar_module
  use mem_module
  use pathcheck_module
  use pc_module
  use pcpsx_module
  use string_module
  use timeglob_module
  use ppio_module
  !
  implicit  none
  !
  private
  !
  public :: fxcard_create              ! create and initialize fxcard
  public :: fxcard_create_1            ! create  a fxcard card structure
  public :: fxcard_delete              ! delete  a fxcard card structure
  public :: fxcard_nullify             ! nullify a fxcard card structure
  public :: fxcard_clear               ! clear strucutre
  public :: fxcard_len_inc             ! increase the length of a cardset
  public :: fxcard_len_add             ! add   to the length of a cardset
  public :: fxcard_len_add_put         ! put the card_len_add value
  public :: fxcard_len_add_get         ! get the card_len_add value
  public :: fxcard_add_card            ! add card to   a fxcard card structure
  public :: fxcard_get_card            ! get card from a fxcard card structure
  public :: fxcard_put_name            ! put name to   a fxcard card structure
  public :: fxcard_get_name            ! get name from a fxcard card structure
  public :: fxcard_cset_to_card        ! copy cardset cards to strcture cards
  public :: fxcard_card_to_cset        ! copy cardset cards to strcture cards
  public :: fxcard_h_gat_tig           ! set src and rec header words
  public :: fxcard_card_to_file        ! put card images to file
  public :: fxcard_file_to_card        ! get card images from file
  public :: fxcard_file_num_card       ! get num card images in file
  public :: fxcard_file_open           ! open file with group
  public :: fxcard_file_close          ! close file with group
  public :: fxcard_file_lock           ! lock file with group
  public :: fxcard_file_unlock         ! unlock file with group
  public :: fxcard_card_print          ! print card images 
  public :: fxcard_card_to_card        ! copy file card structure
  public :: fxcard_process_name        ! get process name
  public :: fxcard_file_to_array       ! get array from file
  public :: fxcard_array_to_file       ! put array to   file
  public :: fxcard_replace_string      ! replace a string
  public :: fxcard_check_path          ! check a pathnam
  public :: fxcard_cardset_create      ! create and clear a cardset
  !
  ! functions
  !
  public :: fxcard_num_cards           ! number of cards
  !
  interface fxcard_array_to_file
    !
    module procedure  fxcard_array_to_file_c
    module procedure  fxcard_array_to_file_r
    module procedure  fxcard_array_to_file_i
    module procedure  fxcard_array_to_file_d
    module procedure  fxcard_array_to_file_l
    !
  end interface 
  !
  interface fxcard_file_to_array
    !
    module procedure  fxcard_file_to_array_c
    module procedure  fxcard_file_to_array_r
    module procedure  fxcard_file_to_array_i
    module procedure  fxcard_file_to_array_d
    module procedure  fxcard_file_to_array_l
    !
  end interface 
  !
  interface fxcard_replace_string
    !
    module procedure  fxcard_replace_string
    module procedure  fxcard_replace_string_n
    !
  end interface 
  !
  interface fxcard_check_path
    !
    module procedure  fxcard_check_path
    module procedure  fxcard_check_path_n
    !
  end interface 
  !
  ! rcs identifier string
  character(len=100),public,save :: fxcard_ident = &
  '$Id: fxcard.f90,v 1.18 2006/08/11 13:14:56 Glover beta sps $'
  !
  integer, parameter, public       :: fxcard_card_len = 320
  integer, save,      public       :: fxcard_len_add_0 = 500
  !
  type,  public :: fxcard_struct 
    !
    character(len=filename_length)   :: card_name      ! card name
    integer                          :: l_card         ! length of cards
    integer                          :: m_card         ! num of available cards
    integer                          :: n_card         ! num of active cards
    integer                          :: a_card         ! num of added  cards
    character(len=1),        pointer :: c_card(:,:)    ! card images
    !
  end type fxcard_struct 
  !
  type ( fxcard_struct ), pointer, save :: structure   ! needed for traps.
  !
  contains
  !
  subroutine fxcard_create ( c_nam, c_inf )
    !
    ! create and clear fxcard structure
    !
    character(len=*),     intent(in   ) :: c_nam           ! card name
    type ( fxcard_struct ),     pointer :: c_inf           ! fxcard structure
    !
    ! create the fxcard structure
    !
  !print*,' fxcard_create bef fxcard_create_1 '
    call fxcard_create_1 ( c_inf, fxcard_card_len, 0 )
    !
    ! clear the fxcard structure
    !
  !print*,' fxcard_create bef fxcard_clear '
    call fxcard_clear  ( c_inf )
    !
    ! set the card set name
    !
  !print*,' fxcard_create bef fxcard_put_name '
    call fxcard_put_name ( c_inf, c_nam )
    !
  !print*,' fxcard_create aft fxcard_put_name '
    return
    !
  end subroutine fxcard_create 
  !
  subroutine fxcard_create_1 ( f_card, l_card, m_card )
    !
    ! create a fxcard card strucutre
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    integer,           intent(in   ) :: l_card       ! card image     length
    integer,           intent(in   ) :: m_card       ! card avialable length
    !
    integer                          :: i_err        ! err 0=o.k. -1=error
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure already exists, delete it
    !
  !print*,' fxcard_create_1 associated=',associated ( f_card )
    if (    associated ( f_card ) ) &
    call fxcard_delete ( f_card )
    !
    ! allocate the structure
    !
  !print*,' fxcard_create_1 bef allocate associated=',associated ( f_card )
    !
    allocate ( f_card, stat=i_err )
      !
    if ( i_err .ne. 0 ) then
      !
      call pc_info( 'f_card allocation error')
      !
      goto 999
      !
    end if
    !
    !print*,' fxcard_create_1 aft allocate'
    !
    f_card%l_card = l_card  ! length of card image
    !
    f_card%m_card = m_card  ! number of cards available in card
    !
    f_card%a_card = 500     ! number of card locations to add when needed
    !
    f_card%a_card = fxcard_len_add_0
    !
    f_card%n_card = 0       ! number of cards currently in card
    !
    i_err = 0
    !
  !print*,' fxcard_create_1 bef mem_ralloc '
    call mem_realloc ( f_card%c_card, f_card%l_card, f_card%m_card, i_err )
  !print*,' fxcard_create_1 aft mem_ralloc '
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxcard_create_1 during allocate ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxcard_create_1 m_card=', m_card )
    call pc_info ( ' error in fxcard_create_1 l_card=', l_card )
    !
    if (    associated ( f_card ) ) &
    call fxcard_delete ( f_card )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_create_1
  !
  subroutine fxcard_delete ( f_card )
    !
    ! delete a fxcard card strucutre
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    !
    ! Local variables
    !
    if (     associated ( f_card ) ) &
    call mem_free ( f_card%c_card )
    !
    if ( associated ( f_card ) ) &
         deallocate ( f_card )
    !
    return
    !
  end subroutine fxcard_delete
  !
  subroutine fxcard_nullify ( f_card )
    !
    ! nullify a fxcard card strucutre
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    !
    ! Local variables
    !
    nullify ( f_card%c_card )
    !
  end subroutine fxcard_nullify
  !
  subroutine fxcard_len_inc ( f_card, m_card, i_err )
    !
    ! increase the length of the cards to m_card
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    integer,           intent(in   ) :: m_card       ! card avialable length
    integer,           intent(inout) :: i_err        ! err 0=o.k. -1=error
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure does not exist return with an error
    !
    if ( .not. associated ( f_card ) ) go to 998
    !
    !print'(" top fxcard_len_inc c=",i8,&
    !& " n_card=",i8," m_card=",i8," e_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, m_card
    !
    ! if the current size is bigger than we want do nothing
    !
    if ( m_card .le. f_card%m_card ) return
    !
    ! reallocate the memory
    !
    !print'(" aa1 fxcard_len_inc c=",i8,&
    !& " n_card=",i8," m_card=",i8," e_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, m_card
    !
    call mem_realloc ( f_card%c_card, f_card%l_card, m_card, i_err )
    !
    !print'(" aa2 fxcard_len_inc c=",i8,&
    !& " n_card=",i8," m_card=",i8," e_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, m_card
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! set the new memory to blank
    !
    f_card%c_card ( 1:f_card%l_card , f_card%m_card+1:m_card ) = ' '
    !
    !print'(" aa3 fxcard_len_inc c=",i8,&
    !& " n_card=",i8," m_card=",i8," e_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, m_card
    !
    ! set the available length
    !
    f_card%m_card = m_card
    !
    !print'(" aa4 fxcard_len_inc c=",i8,&
    !& " n_card=",i8," m_card=",i8," e_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, m_card
    !
    return
    !
997 continue
    !
    call pc_info ( ' error in fxcard_len_inc during allocate ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxcard_len_inc the structure does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxcard_len_inc m_card=', m_card )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_len_inc
  !
  subroutine fxcard_len_add ( f_card, a_card, i_err )
    !
    ! increase the length of the cards by a_card
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    integer,           intent(in   ) :: a_card       ! card extra length
    integer,           intent(inout) :: i_err        ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    integer                          :: m_card       ! num of available cards
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure does not exist return with an error
    !
    if ( .not. associated ( f_card ) ) go to 998
    !
    ! set the desired available size
    !
    !print'(" top fxcard_len_add c=",i8,&
    !& " n_card=",i8," m_card=",i8," a_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, a_card
    !
    m_card = f_card%m_card + a_card
    !
    ! reallocate the memory
    !
    !print'(" aa1 fxcard_len_add c=",i8,&
    !& " n_card=",i8," m_card=",i8," a_card=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, a_card
    !
    call fxcard_len_inc ( f_card, m_card, i_err )
    !
    !print'(" end fxcard_len_add c=",i8,&
    !& " n_card=",i8," m_card=",i8," a_card=",i8," e=",i8)', &
    !i_call, f_card%n_card, f_card%m_card, a_card, i_err
    !
    if ( i_err .ne. 0 ) go to 997
    !
    !print'(" top fxcard_len_add n_card=",i8," m_card=",i8," a_card=",i8)', &
    !f_card%n_card, f_card%m_card, a_card
    !
    return
    !
997 continue
    !
    call pc_info ( ' error in fxcard_len_add during fxcard_len_add ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxcard_len_add the structure does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxcard_len_add m_card=', m_card )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_len_add
  !
  subroutine fxcard_len_add_put ( card_len_add )
    !
    ! get the currect fxcard_card_aa value
    !
    integer,           intent(in   ) :: card_len_add
    !
    fxcard_len_add_0 = max ( 500, card_len_add )
    !
    return
    !
  end subroutine fxcard_len_add_put 
  !
  subroutine fxcard_len_add_get ( card_len_add )
    !
    ! get the currect fxcard_len_add_0 value
    !
    integer,           intent(inout) :: card_len_add
    !
    card_len_add = fxcard_len_add_0
    !
    return
    !
  end subroutine fxcard_len_add_get 
  !
  subroutine fxcard_add_card ( f_card, c_card )
    !
    ! add a card to a fxcard card structure
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    character(len=*),  intent(in   ) :: c_card       ! card to add
    !
    ! Local variables
    !
    integer                          :: i_err        ! err 0=o.k. -1=error
    integer                          :: n_char       ! length of copied card
    integer                          :: i_char       ! character index

    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure does not exist return with an error
    !
    if ( .not. associated ( f_card ) ) go to 998
    !
    ! add cards if necessary
    !
    if ( f_card%n_card + 1 .gt. f_card%m_card ) &
    f_card%a_card = fxcard_len_add_0
    !
    if ( f_card%n_card + 1 .gt. f_card%m_card ) &
    call fxcard_len_add ( f_card, f_card%a_card, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! increment the card count
    !
    f_card%n_card = f_card%n_card + 1 
    !
    !print'(" qq1 n=",i8," c=",a)',f_card%n_card,trim(c_card)
    !
    ! add the card
    !
    n_char = min ( f_card%l_card, len ( c_card ) )
    !
    do_i_char : do i_char = 1 , n_char
      !
      f_card%c_card ( i_char , f_card%n_card ) = c_card ( i_char:i_char )
      !
    end do do_i_char
    !
    return
    !
997 continue
    !
    call pc_info ( ' error in fxcard_add_card during fxcard_add_card ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxcard_add_card the structure does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_add_card ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_add_card
  !
  subroutine fxcard_get_card ( f_card, i_card, c_card, c_mess )
    !
    ! add a card
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    integer,           intent(in   ) :: i_card       ! card index to get
    character(len=*),  intent(  out) :: c_card       ! card to get
    character(len=*),  intent(  out) :: c_mess       ! card message
    !
    ! Local variables
    !
    integer                          :: n_char       ! length of copied card
    integer                          :: i_char       ! character index
    !
    c_card = ' '
    !
    c_mess = ' '
    !
    ! if this structure does not exist return with an error
    !
    if ( .not. associated ( f_card ) ) go to 998
    !
    ! if this card index is out of range return with an error
    !
    if ( i_card .lt. 1 .or. i_card .gt. f_card%n_card ) go to 997
    !
    ! get the card
    !
    n_char = min ( f_card%l_card, len ( c_card ) )
    !
    do_i_char : do i_char = 1 , n_char
      !
      c_card ( i_char:i_char ) = f_card%c_card ( i_char, i_card )
      !
    end do do_i_char
    !
    return
    !
997 continue
    !
    write ( c_mess, '( &
    & " error in fxcard_get_card range i_card=", i8, &
    & " n_card=", i8, " m_card=", i8 )' ) &
    i_card, f_card%n_card, f_card%m_card
    !
    go to 999
    !
998 continue
    !
    write ( c_mess, '( &
    & " error in fxcard_get_card the structure does not exist " )')
    !
    go to 999
    !
999 continue
    !
    call pc_info ( c_mess )
    !
    stop
    !
    !return
    !
  end subroutine fxcard_get_card
  !
  subroutine fxcard_put_name ( f_card, card_name )
    !
    ! put the card name
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    character(len=*),  intent(in   ) :: card_name    ! card name
    !
    ! Local variables
    !
    ! if this structure does not exist return 
    !
    if ( .not. associated ( f_card ) ) return
    !
    f_card%card_name = card_name 
    !
    return
    !
  end subroutine fxcard_put_name
  !
  subroutine fxcard_get_name ( f_card, card_name )
    !
    ! get the card name
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    character(len=*),  intent(  out) :: card_name    ! card name
    !
    ! Local variables
    !
    card_name = ' '
    !
    ! if this structure does not exist return 
    !
    if ( .not. associated ( f_card ) ) return
    !
    card_name = f_card%card_name
    !
    return
    !
  end subroutine fxcard_get_name
  !
  subroutine fxcard_clear ( f_card )
    !
    ! clear a fxcard card strucutre
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    !
    ! Local variables
    !
    ! if this structure does not exist return 
    !
    if ( .not. associated ( f_card ) ) return
    !
    f_card%n_card = 0
    !print'(" qq2 n=",i8)', f_card%n_card
    !
    f_card%c_card = ' '
    !
    return
    !
  end subroutine fxcard_clear
  !
  integer function fxcard_num_cards ( f_card )
    !
    ! clear a fxcard card strucutre
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    !
    ! Local variables
    !
    integer                          :: n_card       ! number of cards
    !
    n_card = 0
    !
    ! if this structure does not exist return 
    !
    if ( associated ( f_card ) ) &
    n_card = f_card%n_card
    !
    fxcard_num_cards = n_card
    !
    !print*,' assoc=',associated ( f_card ),' num=',fxcard_num_cards 
    !
    return
    !
  end function fxcard_num_cards           ! return the number of active cards
  !
  subroutine fxcard_cset_to_card ( f_card, f_cset )
    !
    ! copy a cardset to a fxcard cardset
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    type ( cardset_struct ), pointer :: f_cset       ! cardset 
    !
    ! Local variables
    !
    integer                          :: i_err        ! err 0=o.k. -1=error
    integer                          :: i_card
    character(len=fxcard_card_len):: crd_xxx        ! card image    
    character(len=80)                :: crd_80
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure does not exist return with an error
    !
!print*,' fxcard_cset_to_card f_card=',associated ( f_card )
!print*,' fxcard_cset_to_card f_cset=',associated ( f_cset ) 
    !
    if ( .not. associated ( f_card ) ) go to 998
    if ( .not. associated ( f_cset ) ) go to 998
    !
    call fxcard_clear ( f_card )
    !
    ! add cards if necessary
    !
!print*,' fxcard_cset_to_card cardset_num_cards =',cardset_num_cards ( f_cset )
    !
    do_cards : do i_card = 1 , cardset_num_cards ( f_cset )
      !
!print*,' a i_card=',i_card
      crd_xxx = ' '
      !
      call cardset_get_card ( f_cset, i_card, crd_xxx, crd_80 )
!print'(" i=",i8," c=",a)',i_card,trim(crd_xxx)
      !
      call fxcard_add_card ( f_card, crd_xxx )
      !
!print*,' b i_card=',i_card
    end do do_cards 
    !
    call cardset_delete ( f_cset )
    !
    return
    !
998 continue
    !
    call pc_info ( &
    ' error in fxcard_cset_to_card the structure does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_cset_to_card ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_cset_to_card 
  !
  subroutine fxcard_card_to_cset ( f_card, f_cset )
    !
    ! copy a fxcard cardset to a cardset
    !
    type ( fxcard_struct ),  pointer :: f_card       ! fxcard card structure
    type ( cardset_struct ), pointer :: f_cset       ! cardset 
    !
    ! Local variables
    !
    integer                          :: i_err        ! err 0=o.k. -1=error
    integer                          :: i_card
    character(len=fxcard_card_len):: crd_xxx        ! card image    
    character(len=80)                :: crd_80
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! if this structure does not exist return with an error
    !
    if ( .not. associated ( f_card ) ) go to 998
    !
    !print*,' fxcard_card_to_cset bef fxcard_cardset_create '
    !
    call fxcard_cardset_create ( f_cset )
    !
    !print*,' fxcard_card_to_cset aft fxcard_cardset_create '
    !
    ! add cards if necessary
    !
    do_cards : do i_card = 1 , fxcard_num_cards ( f_card )
      !
      call fxcard_get_card ( f_card, i_card, crd_xxx, crd_80 )
      !
      call cardset_add_card ( f_cset, crd_xxx )
      !
    end do do_cards 
    !
    return
    !
998 continue
    !
    call pc_info ( &
    ' error in fxcard_card_to_cset the structure does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_card_to_cset ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_card_to_cset
  !
  subroutine fxcard_card_to_file ( p, file_name, card_info, i_err )
    !
    ! write file card images to file
    !

    type ( fxpar_struct ),      pointer :: p           ! fxpar divide 
    character(len=*),     intent(in   ) :: file_name       ! file name
    type ( fxcard_struct ),      pointer :: card_info       ! fxcard structure
    integer,              intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                             :: file_unit       ! err 0=o.k. -1=error
    !
    integer                             :: j_err           ! err 0=o.k. -1=error
    !
    integer                             :: j_card          ! card number
    integer                             :: i_card          ! card index
    integer                             :: ndidwr          ! card index
    !
    character(len=fxcard_card_len)     :: crd_xxx         ! card image
    character(len= 80)                  :: crd_80          ! card image
    character(len= 64)                  :: c_time          ! card image
    !
    i_err = 0
    !
    j_err = 0
    !
    ndidwr= 0
    !
    !print'(" fxcard_card_to_file file_name=", a)', trim(file_name)
    !
    ! there is a problem if this structure does not exist
    !
    if ( .not. associated ( card_info ) ) go to 998
    !
    ! make sure the output file name is defined
    !
    if ( string_upper_compare ( file_name, pathcheck_empty ) ) go to 997
    !
    ! remove the file
    ! don't remove the file, this can break the lock system
    !
    !i_err = cio_remove ( file_name )
    !
    i_err = 0
    !
    ! open the file
    ! w+ will create the file if needed
    ! if it exists it will reset the length to zero
    !
    call fxcard_file_open ( p, file_name, 'w+', file_unit, i_err )
    !
    if ( i_err .ne. 0 ) i_err = -1
    !
    !if ( p%group_root ) &
    !print'(" aft fxcard_card_to_file cio_fopen file_unit=",i8,&
    !&" file_name=",a)', &
    !file_unit, trim(file_name)
    !
    if ( i_err .lt. 0 ) go to 996
    !
    !!if ( p%group_root ) &
    !print' ( " fxcard_card_to_file n=",i8," file_name=", a )', &
    !fxcard_num_cards ( card_info ), trim ( file_name )
    !
    ! write each card to the output file
    !
    do_cards : do i_card = 1 , fxcard_num_cards ( card_info )
      !
      call fxcard_get_card ( card_info, i_card,  crd_xxx, crd_80 )
      !
      if ( p%group_root ) then
        j_card = cio_fputline ( string = crd_xxx, unit = file_unit )
        if(j_card < 0) then
          if ( p%group_root ) &
            print '("fxcard_card_to_file: cio_fputline error, i_card=",i8)',&
            i_card
            i_err = -1
        else
          ndidwr = ndidwr + 1
        end if
      end if
      !
      !if ( p%group_root ) &
      !print'(" i=", i8," n=", i8, " c=",a)',i_card,j_card,trim(crd_xxx)
      !
    end do do_cards 
    !
1999 continue
    !
    call string_time  ( c_time   ) ! current time
    if( p%group_root ) then
     if( i_err < 0) call pc_error('fxcard_card_to_file: write error')
    end if
    !
    !call pcpsx_broadcast_group ( &
    !p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !call pcpsx_broadcast_group ( &
    !p%p0_grp, p%np_grp, p%jp_grp, ndidwr )
    !if(ndidwr .ne. fxcard_num_cards ( card_info )) then
    !   if ( p%group_root ) &
    !   print'("fxcard_card_to_file: error, to write=",i8," written=",i8)',&
    !    fxcard_num_cards ( card_info ),ndidwr
    !end if
   !if ( p%group_root ) &
   !   print'("fxcard_card_to_file(",i2,"):DBG written=",i8," t=",a)',&
   !    fxpar_i_pel(),ndidwr,trim(c_time)
    !
    j_err = 0
    !
    !if ( p%group_root ) &
    !print'(" bef fxcard_card_to_file cio_fclose file_unit=",i8)', file_unit
    !
    call fxcard_file_close ( p, file_unit, j_err )
    !
    if ( p%group_root .and. j_err .ne. 0 ) &
    call pc_info ( ' error in fxcard_card_to_file during close ' )
    if ( i_err < 0 ) go to 997
    !
    return
    !
996 continue
    !
    call pc_info ( &
    ' error in fxcard_card_to_file during cio_fopen unit=', file_unit, &
    file_name )
    !
    go to 999
    !
997 continue
    !
    call pc_info ( ' error in fxcard_card_to_file file_name=', file_name )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxcard_card_to_file struct does not exist ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_card_to_file pe= ", i8, " file_name=", a &
    & )' ) &
    fxpar_i_pel(), trim(file_name)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_card_to_file             ! put card images to file
  !
  subroutine fxcard_file_to_card ( p, file_name, card_info, i_err )
    !
    type ( fxpar_struct ),      pointer :: p           ! fxpar divide 
    character(len=*),     intent(in   ) :: file_name       ! file name
    type ( fxcard_struct ),     pointer :: card_info       ! fxcard structure
    integer,              intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                             :: file_unit       ! err 0=o.k. -1=error
    !
    integer                             :: j_err           ! err 0=o.k. -1=error
    !
    integer                             :: j_card          ! card number
    integer                             :: k_card          ! card number
    integer                             :: l_card          ! card number
    integer                             :: n_card          ! card index
    !
    character(len=fxcard_card_len)      :: crd_xxx          ! card image
    character(len=64)                   :: c_time
    !
    i_err = 0
    !
    ! make sure this file exists
    !
    if ( finquire_file ( file_name ) .eq. finquire_not_found ) go to 994
    !
    ! count the number of cards in htis file
    !
    call fxcard_file_num_card ( p, file_name, n_card, i_err )
    !
!print'(" fxcard_file_to_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    if ( i_err .lt. 0 ) go to 998
    !
    ! create and clear the fxcard structure
    !
    call fxcard_create ( file_name, card_info )
    !
!print'(" aa1 fxcard_file_to_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    ! set the card addiiton len
    !
    call fxcard_len_add_get ( l_card )
    !
!print'(" aa2 fxcard_file_to_card err=",i8," l_card=", i8," file_name=",a)', &
!i_err, l_card, trim(file_name)
    !
    call fxcard_len_add_put ( n_card )
    !
!print'(" aa3 fxcard_file_to_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    ! open the file
    !
    call fxcard_file_open ( p, file_name, 'r', file_unit, i_err )
    !
!print'(" aa4 fxcard_file_to_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    call string_time  ( c_time   ) ! current time
    !
    if ( i_err .lt. 0 ) go to 997
    !
    ! add the input cards to the fxcard structure
    !
    do_k_card : do k_card = 1 , n_card
      !
      crd_xxx = ' '
      !
      !if ( k_card .ge. 15000 .and. k_card .le. 16000 ) &
      !print'(" aa5 fxcard_file_to_card k_card=", i8," crd_xxx=",a)', &
      !k_card, trim(crd_xxx)
      !
      if ( p%group_root ) &
      j_card = cio_fgetline ( &
      string=crd_xxx, imax=fxcard_card_len, unit=file_unit )
      !
!if ( k_card .le. 1000 .or. mod(k_card,100 ) .eq. 1 ) &
!      if ( k_card .ge. 15000 .and. k_card .le. 16000 ) &
!print'(" aa6 fxcard_file_to_card k_card=",i8," j_card=",i8," crd_xxx=",a)', &
!k_card, j_card, trim(crd_xxx)
      !
      call pcpsx_broadcast_group ( &
      p%p0_grp, p%np_grp, p%jp_grp, j_card )
      !
      call pcpsx_broadcast_group ( &
      p%p0_grp, p%np_grp, p%jp_grp, crd_xxx )
      !
      !if ( p%group_root ) &
      !print'(" j_card=",i8," c=",a)',j_card,trim(crd_xxx)
      !
      if ( j_card .lt. 0 ) go to 996
      !
      ! add this card image to the card
      !
      call fxcard_add_card ( card_info, crd_xxx )
      !
!      if ( k_card .ge. 15000 .and. k_card .le. 16000 ) &
!print'(" aa7 fxcard_file_to_card k_card=",i8," j_card=",i8," crd_xxx=",a)', &
!k_card, j_card, trim(crd_xxx)
      !
      !if ( p%group_root ) &
      !print'(" k_card=",i8," c=",a)',k_card,trim(crd_xxx)
      !
    end do do_k_card 
    !
!print'(" aa8 fxcard_file_to_card k_card=",i8)', n_card
      !
    if ( j_card .lt. 0 .and. j_card .ne. CIO_EOF ) i_err = -1
    !
    if ( i_err .ne. 0 ) go to 995
    !
    call fxcard_len_add_put ( l_card )
!print'(" aa9 fxcard_file_to_card k_card=",i8)', n_card
    !
1999 continue
    !
    !if ( p%group_root ) &
    !print'("fxcard_file_to_card(",i2,"):DBG didread=",i8," t=",a)',&
    !fxpar_i_pel(),n_card,trim(c_time)
    !
    call fxpar_check_worker_errors ( p, i_err )
    !
    ! close the input unit
    !
    call fxcard_file_close ( p, file_unit, j_err )
    !
    if ( p%group_root .and. j_err .ne. 0 ) &
    call pc_info ( ' error in fxcard_file_to_card during close ' )
    !
    return
    !
994 continue
    !
    if ( p%group_root ) &
    print'(" fxcard_file_to_card p=", i4, &
    & " error file not found file=",a &
    & )', &
    fxpar_i_pel(), trim(file_name)
    !
    go to 999
    !
995 continue
    !
    if ( p%group_root ) &
    print'(" fxcard_file_to_card p=", i4, &
    & " error after read n_card=", i8," j_card=",i8 &
    & )', &
    fxpar_i_pel(), n_card, j_card
    !
    go to 999
    !
996 continue
    !
    if ( p%group_root ) &
    print'(" fxcard_file_to_card p=", i4, &
    & " error during read n_card=", i8," j_card=",i8, " k_card=", i8 &
    & )', &
    fxpar_i_pel(), n_card, j_card, k_card
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file_to_card during fxcard_file_open pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    crd_xxx=' '
    !
    print'( &
    & /, " fxcard_file_to_card: error during fxcard_file_open p=", i4, &
    & /, " fxcard_file_to_card: file=", a, &
    & /, " fxcard_file_to_card: file=", a, &
    & /, " fxcard_file_to_card: finquire=", i8, &
    & /, " fxcard_file_to_card: msg=", a &
    & )', &
    fxpar_i_pel(), trim(file_name), &
    finquire_file(file_name,crd_xxx), &
    trim(crd_xxx)
    !
    go to 999
    !
998 continue
    !
    if ( p%group_root ) &
    print'(" fxcard_file_to_card p=", i4, &
    & " error during fxcard_file_num_card " &
    & )', &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file_to_card pe= ", i8, " file_name=", a &
    & )' ) &
    fxpar_i_pel(), trim ( file_name )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_to_card             ! get card images from file
  !
  subroutine fxcard_file_num_card ( p, file_name, n_card, i_err )
    !
    type ( fxpar_struct ),      pointer :: p           ! fxpar divide 
    character(len=*),     intent(in   ) :: file_name       ! file name
    integer,              intent(inout) :: n_card          ! card number
    integer,              intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                             :: file_unit       ! err 0=o.k. -1=error
    integer                             :: j_err           ! err 0=o.k. -1=error
    integer                             :: j_card          ! card index
    character(len=fxcard_card_len)      :: crd_xxx          ! card image
    character(len=64)                   :: c_time
    !
    i_err = 0
    !
    n_card = 0
    j_card = -1
    !
    ! open the file
    !
!print'(" aa1 fxcard_file_num_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    call fxcard_file_open ( p, file_name, 'r', file_unit, i_err )
    !
    call string_time  ( c_time   ) ! current time
    !
!print'(" aa2 fxcard_file_num_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    if ( i_err .lt. 0 ) go to 998
    !
    ! count the input cards 
    !
1   continue
    !
    crd_xxx = ' '
    !
    if ( p%group_root ) &
    j_card = cio_fgetline ( &
    string=crd_xxx, imax=fxcard_card_len, unit=file_unit )
    !
    if ( j_card .lt. 0 ) goto 2
    !
    n_card = n_card + 1
    !
!if ( n_card .le. 1000 .or. mod(n_card,100) .eq. 1 ) &
!print'(" aa1 fxcard_file_num_card n_card=",i8," n_card=", i8," c=",a)', &
!j_card, n_card, trim(crd_xxx)
    !
    go to 1
    !
2   continue
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, n_card )
    !
!print'(" aa3 fxcard_file_num_card err=",i8," n_card=", i8," file_name=",a)', &
!i_err, n_card, trim(file_name)
    !
    if ( n_card .lt. 0 .and. n_card .ne. CIO_EOF ) i_err = -1
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    call fxpar_check_worker_errors ( p, i_err )
    !
    ! close the input unit
    !
    call fxcard_file_close ( p, file_unit, j_err )
    !
    if ( p%group_root .and. j_err .ne. 0 ) &
    call pc_info ( ' error in fxcard_file_to_card during close ' )
    !
    return
    !
997 continue
    !
    if ( p%group_root ) &
    print'(" fxcard_file__card_num p=",i4," error on read j_card=",i8,1x,i8)',&
    fxpar_i_pel(), j_card, n_card
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file__card_num during fxcard_file_open pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file__card_num pe= ", i8, " file_name=", a &
    & )' ) &
    fxpar_i_pel(), trim ( file_name )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_num_card
  !
  subroutine fxcard_file_open ( p, file_name, file_stat, file_unit, i_err )
    !
    type ( fxpar_struct ),      pointer :: p           ! fxpar divide 
    character(len=*),     intent(in   ) :: file_name       ! file name
    character(len=*),     intent(in   ) :: file_stat       ! file name
    integer,              intent(inout) :: file_unit       ! file unit

    integer,              intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    !print'(" fxcard_file_open root=",l2," stat=",a8," name=",a)', &
    !p%group_root , trim(file_stat), trim(file_name)
    !
    i_err = 0
    !
    file_unit = -1
    !
    ! From Menger 06-29-05
    ! I may need to look at the lock detection in pfio/bfio to make sure we 
    ! aren't being unwise.
    ! On your point #1 under "thoughts", if you open a file with 
    ! cio_fopen(file,"w") it will automatically be pre-deleted and recreated, 
    ! you never should need to call
    ! cio_remove first. (Chuck does this inside of pfio_open_write)
    ! share the error status over all pes
    !
    !xxif_file_stat_w : &
    !if ( p%group_root .and. string_upper_compare ( file_stat, 'w' ) ) then
      !
    !  print *,'fxcard_file_open: removing file=',trim(file_name)
      !
    !  i_err = cio_remove ( file_name )
      !
    !end if xxif_file_stat_w 
    !
    !if ( i_err .ne. 0)  then
      !
    !  call pc_error('fxcard_file_open: cio_remove error')
      !
    !  call ppio_abort ( i_err )
      !
    !end if
    !
    ! share the error status over all pes
    !
    ! call fxpar_check_worker_errors ( p, i_err ) 01/20
    !
    !print'(" aa1 fxcard_file_open i_err=",i8)', i_err
    !
    if ( i_err .ne. 0 ) go to 998
    !
    i_err = 0
    !
    ! open the file
    !
    if ( p%group_root ) &
    file_unit = cio_fopen ( file_name, file_stat )
    !
    !print'(" aa1 fxcard_file_open u=",i8)', file_unit
    !
    !call pcpsx_broadcast_group ( &
    !p%p0_grp, p%np_grp, p%jp_grp, file_unit )
    !
    xxif_err : &
    if ( p%group_root .and. file_unit .lt. 0 )  then
      !
      print'(" fxcard_file_open root=",l2," unit=",i8,&
      & " stat=",a8," name=",a)', &
      p%group_root, file_unit, trim(file_stat), trim(file_name)
      !
      call pc_error ( 'fxcard_file_open: Group root open card file error' )
      !
      call ppio_abort ( file_unit )
      !
      go to 997
      !
    end if xxif_err 
    !
    !if ( file_unit .lt. 0 ) go to 997
    !
    ! rewind the input unit
    !
    if ( p%group_root ) &
    call  cio_frewind ( file_unit )
    !
1999 continue
    !
    ! share the error status over all pes
    !
    ! call fxpar_check_worker_errors ( p, i_err ) !01/20
    !
    !print'(" end fxcard_file_open u=",i8)', file_unit
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file_open during cio_fopen pe= ", i8, &
    & " err=", i8 &
    & )' ) &
    fxpar_i_pel(), file_unit
    !
    call pc_info ( ' error in fxcard_file_open during cio_fopen ' )
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file_open during cio_remove pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    call pc_info ( ' error in fxcard_file_open during cio_remove ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxcard_file_open pe= ", i8, " file_name=", a &
    & )' ) &
    fxpar_i_pel(), trim ( file_name )
    !
    if ( p%group_root ) &
    print'(" fxcard_file_open root=",l2," stat=",a8," name=",a)', &
    p%group_root , trim(file_stat), trim(file_name)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_open ! open cio file on root pe
  !
  subroutine fxcard_file_close ( p, file_unit, i_err )
    !
    type ( fxpar_struct ),      pointer :: p           ! fxpar divide 
    integer,              intent(inout) :: file_unit       ! file unit
    integer,              intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    character(len=filename_length)      :: file_name       ! file name
    integer                             :: perms           ! permission value
    integer                             :: status          ! status vlaue
    integer                             :: file_exist      ! file exist flag
    !
    i_err = 0
    !
    ! close the file
    !
    xxif_root : if ( p%group_root ) then
      !
      ! get the file name associated with this file
      !
      status = cio_finquire ( file_unit, file_name )
      !
      ! close the file
      !
      i_err = cio_fclose ( file_unit )
      !
      if ( i_err .ne. 0 ) &
      call pc_info ( ' error in fxcard_file_close during close ' )
      !
      ! set the protection of the file
      !
      perms = 64 * 7 + 8 * 5 + 5
      !
      !perms            character(len=*) rwx,rwx,rwx or rwx------ ...
      !        or
      !  perms            integer         decimal equiv of octal 644, 755,...
      !                                   (for 643 use 64*6 + 8*4 + 3)
      ! check for the existance of this file
      !
      file_exist = finquire_file ( file_name )
      !
      ! if it exists set its protection
      !
      if ( file_exist .ne. finquire_not_found ) &
      status = cio_chmod  ( file_name, perms )
      !
    end if xxif_root 
    !
    ! call pcpsx_broadcast_group ( &
    ! p%p0_grp, p%np_grp, p%jp_grp, i_err )
  
    !
   return
    !
  end subroutine fxcard_file_close ! close cio file on root pe
  !
  subroutine fxcard_file_lock ( p, path_name, time_lock, i_err )
    !
    ! lock file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    character(len=*),  intent(in   ) :: path_name        ! file name
    integer,           intent(in   ) :: time_lock        ! file lock time in sec
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    integer    :: lock_type
    integer    :: lock_status
    character(len=120) :: lock_name
    lock_name = trim(path_name)//'L'
   !lock_name = path_name
    !
    i_err = 0
    !
    !lock_test
    !
    !if ( i_err .ne. -999 ) return
    if( .not. p%lock_file ) return
    !
    !if ( p%group_root ) &
    !write(pc_get_lun(), '( &
    !& /, " fxcard_file_lock time_lock=", i8, " path_name=", a &
    !& )') &
    !time_lock, trim(path_name)
    !
    !if ( p%group_root ) &
    !
    lock_type = cio_normal_lock
    !
    lock_status = 99
    !
!print'(" fxcard_file_lock time_lock=",i8," lock_type=",i8," path_name=",a)', &
!time_lock, lock_type, trim(path_name)
    !
    xxif_group_root : &
    if ( p%group_root ) then
      !
      i_err = cio_lock_file ( lock_name, time_lock, lock_type, lock_status )
      !
!print'(" fxcard_file_lock time_lock=",i8," lock_type=",i8," path_name=",a)', &
!time_lock, lock_type, trim(path_name)
      !
      xxif_lock : &
      if ( lock_status .eq. cio_lock_expired &
      .or. lock_status .eq. cio_lock_error &
      .or. lock_status .eq. cio_lock_inactive ) then
        !
        i_err = -1
        !
        print'(" fxcard_file_lock err pe=", i8, &
        & " lock_status=", i8, &
        & " cio_lock_expired=", i8, &
        & " cio_lock_error=", i8, &
        & " cio_lock_inactive=", i8, &
        & " lock_name=", a &
        & )', &
        fxpar_i_pel(), &
        lock_status, &
        cio_lock_expired, &
        cio_lock_error, &
        cio_lock_inactive, &
        trim(lock_name)
        !
      end if xxif_lock 
      !
    end if xxif_group_root 
    !
    if ( i_err .ne. 0 ) go to 999
    !
1999 continue
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in fxcard_file_lock  pe= ", i8, " file=", a &
    & )' ) &
    fxpar_i_pel(), trim(path_name)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_lock 
  !
  subroutine fxcard_file_unlock ( p, path_name, i_err )
    !
    ! unlock file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    character(len=*),  intent(in   ) :: path_name        ! file name
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    integer     :: lock_status
    character(len=120) :: lock_name
    lock_name = trim(path_name)//'L'
   !lock_name = path_name
    !
    i_err = 0
    !
    !lock_test
    !
    !if ( i_err .ne. -999 ) return
    if( .not. p%lock_file) return
    !
    !if ( p%group_root ) &
    !write(pc_get_lun(), '( &
    !& /, " fxcard_file_unlock path_name=", a &
    !& )') &
    !trim(path_name)
    !
    !if ( p%group_root ) &
    !print'(" fxcard_file_unlock path_name=", a )', trim(path_name)
    !
    lock_status = 99
    !
    xxif_group_root : &
    if ( p%group_root ) then
      !
      call cio_update_file_time ( path_name )
      !
      i_err = cio_unlock_file ( lock_name, lock_status )
      !
      xxif_lock : &
      if ( lock_status .eq. cio_lock_expired &
      .or. lock_status .eq. cio_lock_error &
      .or. lock_status .eq. cio_lock_inactive ) then
        !
        i_err = -1
        !
        print'(" fxcard_file_unlock err pe=", i8, &
        & " lock_status=", i8, &
        & " cio_lock_expired=", i8, &
        & " cio_lock_error=", i8, &
        & " cio_lock_inactive=", i8, &
        & " lock_name=", a &
        & )', &
        fxpar_i_pel(), &
        lock_status, &
        cio_lock_expired, &
        cio_lock_error, &
        cio_lock_inactive, &
        trim(lock_name)
        !
      end if xxif_lock 
      !
    end if xxif_group_root 
    !
    if ( i_err .ne. 0 ) go to 999
    !
1999 continue
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in fxcard_file_unlock  pe= ", i8, " file=", a &
    & )' ) &
    fxpar_i_pel(), trim(path_name)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_unlock 
  !
  subroutine fxcard_card_print ( &
                                 print_title, print_unit, print_flag, &
                                 file_name, file_card &
                               )
    !
    character(len=*),     intent(in   ) :: print_title     ! print title
    integer,              intent(in   ) :: print_unit      ! print unit
    logical,              intent(in   ) :: print_flag      ! print flag
    character(len=*),     intent(in   ) :: file_name       ! file name
    type ( fxcard_struct ),     pointer :: file_card       ! fxcard structure
    !
    integer                             :: n_card          ! card num
    integer                             :: i_card          ! card index
    !
    character(len= 80)                  :: crd_80          ! card image 80
    character(len=fxcard_card_len)     :: crd_xxx          ! card image 
    character(len=fxcard_card_len)     :: c_nam            ! card name
    !
    n_card = fxcard_num_cards ( file_card )
    !
    call fxcard_get_name ( file_card, c_nam )
    !
    if ( print_flag ) &
    write ( pc_get_lun(), ' ( &
    & /, " fxcard_card_print ", a, &
    & /, " file_name=", a, &
    & /, " card_name=", a &
    & /, " number of cards=", i8, &
    & /, " index card " &
    & )' ) &
    trim ( print_title ), trim ( file_name ), trim ( c_nam ), n_card
    !
    do_write_cards : do i_card = 1 , n_card
      !
      crd_xxx = ' '
      !
      call fxcard_get_card ( file_card, i_card,  crd_xxx, crd_80 )
      !
      xxif_print_flag : if ( print_flag ) then
        !
        write ( pc_get_lun(), ' ( &
        & 1x, i8, 1x, a &
        & )' ) &
        i_card, trim ( crd_xxx )
        !
      else xxif_print_flag 
        !
        write ( pc_get_lun(), ' ( &
        & a &
        & )' ) &
        trim ( crd_xxx )
        !
      end if xxif_print_flag 
      !
    end do do_write_cards 
    !
    return
    !
  end subroutine fxcard_card_print           ! print card images 
  !
  subroutine fxcard_card_to_card ( &
  file_append, i1_inp, i2_inp, c_inp, c_out, i_err )
    !
    ! copy a fxcard structure
    !
    logical,              intent(in   ) :: file_append  ! append c_inp to c_out
    integer,              intent(in   ) :: i1_inp       ! c_inp card 1 to copy
    integer,              intent(in   ) :: i2_inp       ! c_inp card 2 to copy
    type ( fxcard_struct ),     pointer :: c_inp        ! inp fxcard structure
    type ( fxcard_struct ),     pointer :: c_out        ! out fxcard structure
    integer,              intent(inout) :: i_err        ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    integer                             :: j1_inp       ! c_inp card 1 to copy
    integer                             :: j2_inp       ! c_inp card 2 to copy
    integer                             :: i_card       ! card index
    character(len= 80)                  :: crd_80       ! card image 80
    character(len=fxcard_card_len)      :: crd_xxx      ! card image 
    character(len=fxcard_card_len)      :: c_nam        ! card name
    !
    i_err = 0
    !
    if ( .not. associated ( c_inp ) ) go to 998
    !
    ! set the range of cards to copy from c_inp
    ! if j2_inp < 0 go to the last card
    !
    j1_inp = max ( i1_inp, 1 )
    !
    j2_inp = min ( i2_inp, fxcard_num_cards ( c_inp ) )
    !
    if ( i2_inp .lt. 0 ) j2_inp = fxcard_num_cards ( c_inp ) 
    !
!print'(" card_to_card i1=",i8," j1=",i8," i2=",i8," j2=",i8," n=",i8)',&
!i1_inp, i2_inp, j1_inp, j2_inp, fxcard_num_cards ( c_inp ) 
    !
    ! delete the output fxcard structure
    !
    if (    associated ( c_out ) .and. .not. file_append ) &
    call fxcard_delete ( c_out )
    !
    ! create the output fxcard structure
    !
    if ( .not. file_append ) &
    call fxcard_create_1 ( c_out, fxcard_card_len, 0 )
    !
    ! clear the fxcard structure
    !
    if ( .not. file_append ) &
    call fxcard_clear  ( c_out )
    !
    ! set the card set name
    !
    call fxcard_get_name ( c_inp, c_nam )
    !
    c_nam = c_nam // '_COPY'
    !
    if ( .not. file_append ) &
    call fxcard_put_name ( c_out, c_nam )
    !
    do_i_card : do i_card = j1_inp, j2_inp
    !
      !
      ! get this card from the input fxcard structure
      !
      call fxcard_get_card ( c_inp, i_card,  crd_xxx, crd_80 )
      !
      ! add this card from the copy fxcard structure
      !
      call fxcard_add_card ( c_out, crd_xxx )
      !
    end do do_i_card 
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxcard_card_to_card c_inp not defined ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_card_to_card ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxcard_card_to_card ! create the file card structure
  !
  subroutine fxcard_process_name ( crd_xxx, proc_name )
    !
    ! return the process name from a card image
    !
    character(len=*),     intent(in   ) :: crd_xxx         ! card image
    character(len=*),     intent(inout) :: proc_name       ! proc name
    !
    character(len=1)                    :: dq_0            ! "
    integer                             :: dq_1            ! loc of first "
    integer                             :: dq_2            ! loc of last  "
    !
    !
    ! get the process name
    !
    dq_0 = '"'
    !
    dq_1 = index ( crd_xxx(     1:), dq_0 )
    !
    dq_2 = index ( crd_xxx(dq_1+1:), dq_0 ) + dq_1
    !
    !print'(" dq=",i4,1x,i4," s=",a)',dq_1,dq_2,crd_xxx(dq_1+1:dq_2-1)
    !
    proc_name = ' '
    !
    if ( dq_1 .gt. 0 .and. dq_2 .gt. 0 ) &
    proc_name = crd_xxx ( dq_1+1:dq_2-1 )
    !
    return
    !
    return
    !
  end subroutine fxcard_process_name ! get the process name from a card 
  !
  subroutine fxcard_file_to_array_c ( &
                                     p, title_data, path_data, &
                                     num_data, val_data, &
                                     i_err &
                                   )
    !
    ! get character array from file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(inout) :: num_data     ! number in val_data
    character(len=*),           pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    character(len= 80)                  :: crd_80       ! card image 80
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    num_data = 0
    !
    ! the file must not be empty
    !
    !print'(" top fxcard_file_to_array_c t=",a16," path_data=", a)', &
    !trim (title_data), trim(path_data)
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! get the cardlist structure from a file
    !
    call fxcard_file_to_card ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the list of fxdata files from the cardlist
    !
    call fxcard_card_to_cset ( c_data, c_cset )
    !
    call cardset_alloc_array ( c_cset, title_data, val_data, num_data, crd_80 )
    !
    call cardset_delete ( c_cset )
    !
    !print'(" aa2 fxcard_file_to_array_c num_data=", i8," c=",a)', &
    !num_data,trim(crd_80)
    !
1999 continue
    !
    ! get the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    !print'(" end fxcard_file_to_array_c num_data=", i8)', num_data
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_file_to_array_c during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_file_to_array_c ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_to_array_c       ! get character array from file
  !
  subroutine fxcard_array_to_file_c ( &
                                    p, title_data, path_data, &
                                    num_data, val_data, &
                                    i_err &
                                  )
    !
    ! put character array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(in   ) :: num_data     ! number in val_data
    character(len=*),           pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    ! only get the list_info file in gui mode
    !
    if ( .not. pc_do_not_process_traces() ) return
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! the file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! put the list of fxdata files to the cardlist
    !
    call fxcard_cardset_create ( c_cset )
    !
    call cardset_put_array ( c_cset, title_data, val_data, num_data )
    !
    call fxcard_cset_to_card ( c_data, c_cset )
    !
    ! put the cardlist structure to a file
    !
    call fxcard_card_to_file ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    ! put the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_array_to_file_c during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_array_to_file_c ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_array_to_file_c            ! put character array to file
  !
  subroutine fxcard_file_to_array_r ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! get real array from file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(inout) :: num_data     ! number in val_data
    real,                       pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    character(len= 80)                  :: crd_80       ! card image 80
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    num_data = 0
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! get the cardlist structure from a file
    !
    call fxcard_file_to_card ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the val_data from the cardlist
    !
    call fxcard_card_to_cset ( c_data, c_cset )
    !
    call cardset_alloc_array ( c_cset, title_data, val_data, num_data, crd_80 )
    !
    call cardset_delete ( c_cset )
    !
1999 continue
    !
    ! get the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_file_to_array_r during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_file_to_array_r ' )
    !
    i_err = -1 
    !
    go to 1999
    !
  end subroutine fxcard_file_to_array_r            ! get real array from file
  !
  subroutine fxcard_array_to_file_r ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! put real array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(in   ) :: num_data     ! number in val_data
    real,                       pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    ! only put the list_info file in gui mode
    !
    if ( .not. pc_do_not_process_traces() ) return
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! put the val_data to the cardlist
    !
    call fxcard_cardset_create ( c_cset )
    !
    call cardset_put_array ( c_cset, title_data, val_data, num_data )
    !
    call fxcard_cset_to_card ( c_data, c_cset )
    !
    ! put the cardlist structure to a file
    !
    call fxcard_card_to_file ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    ! put the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_array_to_file_r during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_array_to_file_r ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_array_to_file_r            ! put real array to file
  !
  subroutine fxcard_file_to_array_i ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! get integer array from file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(inout) :: num_data     ! number in val_data
    integer,                    pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    character(len= 80)                  :: crd_80       ! card image 80
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    num_data = 0
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! get the cardlist structure from a file
    !
    call fxcard_file_to_card ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the val_data from the cardlist
    !
    call fxcard_card_to_cset ( c_data, c_cset )
    !
    call cardset_alloc_array ( c_cset, title_data, val_data, num_data, crd_80 )
    !
    call cardset_delete ( c_cset )
    !
1999 continue
    !
    ! get the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_file_to_array_i during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_file_to_array_i ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_to_array_i            ! get real array from file
  !
  subroutine fxcard_array_to_file_i ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! put integer array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(in   ) :: num_data     ! number in val_data
    integer,                    pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    ! only get the list_info file in gui mode
    !
    if ( .not. pc_do_not_process_traces() ) return
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! put the val_data to the cardlist
    !
    call fxcard_cardset_create ( c_cset )
    !
    call cardset_put_array ( c_cset, title_data, val_data, num_data )
    !
    call fxcard_cset_to_card ( c_data, c_cset )
    !
    ! put the cardlist structure to a file
    !
    call fxcard_card_to_file ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    ! put the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_array_to_file_i during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_array_to_file_i ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_array_to_file_i            ! put integer array to file
  !
  subroutine fxcard_file_to_array_d ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! put dp array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(inout) :: num_data     ! number in val_data
    double precision,           pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    character(len= 80)                  :: crd_80       ! card image 80
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    num_data = 0
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! get the cardlist structure from a file
    !
    call fxcard_file_to_card ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the val_data from the cardlist
    !
    call fxcard_card_to_cset ( c_data, c_cset )
    !
    call cardset_alloc_array ( c_cset, title_data, val_data, num_data, crd_80 )
    !
    call cardset_delete ( c_cset )
    !
1999 continue
    !
    ! get the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_file_to_array_d during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_file_to_array_d ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_to_array_d            ! get dp array to file
  !
  subroutine fxcard_array_to_file_d ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! put dp array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(in   ) :: num_data     ! number in val_data
    double precision,           pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa
    i_err = 0
    !
    ! only get the list_info file in gui mode
    !
    if ( .not. pc_do_not_process_traces() ) return
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! put the val_data to the cardlist
    !
    call fxcard_cardset_create ( c_cset )
    !
    call cardset_put_array ( c_cset, title_data, val_data, num_data )
    !
    call fxcard_cset_to_card ( c_data, c_cset )
    !
    ! put the cardlist structure to a file
    !
    call fxcard_card_to_file ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    ! put the list_info cardset to a file
    !
    if (    associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_array_to_file_d during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_array_to_file_d ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_array_to_file_d            ! put dp array to file
  !
  subroutine fxcard_file_to_array_l ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! get logical array from file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(inout) :: num_data     ! number in val_data
    logical,                    pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    character(len= 80)                  :: crd_80       ! card image 80
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa

    i_err = 0
    !
    num_data = 0
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! get the cardlist structure from a file
    !
    call fxcard_file_to_card ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the val_data from the cardlist
    !
    call fxcard_card_to_cset ( c_data, c_cset )
    !
    call cardset_alloc_array ( c_cset, title_data, val_data, num_data, crd_80 )
    !
    call cardset_delete ( c_cset )
    !
1999 continue
    !
    ! get the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_file_to_array_l during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_file_to_array_l ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_file_to_array_l            ! get logical array from file
  !
  subroutine fxcard_array_to_file_l ( &
                                   p, title_data, path_data, &
                                   num_data, val_data, &
                                   i_err &
                                 )
    !
    ! put logical array to file
    ! parameters:
    !
    type ( fxpar_struct ),      pointer :: p        ! fxpar divide 
    character(len=*),     intent(in   ) :: title_data   ! array title
    character(len=*),     intent(in   ) :: path_data    ! array file name
    integer,              intent(in   ) :: num_data     ! number in val_data
    logical,                    pointer :: val_data(:)  ! data array
    integer,              intent(inout) :: i_err        ! err flag
    !
    ! Local variables
    !
    type ( fxcard_struct ),     pointer :: c_data       ! cardset structure
    type ( cardset_struct ),    pointer :: c_cset       ! cardset 
    !
    nullify (c_data) ! jpa
    nullify (c_cset) ! jpa

    i_err = 0
    !
    ! only get the list_info file in gui mode
    !
    if ( .not. pc_do_not_process_traces() ) return
    !
    ! the list_info file must not be empty
    !
    if ( string_upper_compare ( path_data , pathcheck_empty ) ) go to 998
    !
    ! create a cardlist structure
    !
    call fxcard_create ( path_data, c_data )
    !
    ! put the val_data to the cardlist
    !
    call fxcard_cardset_create ( c_cset )
    !
    call cardset_put_array ( c_cset, title_data, val_data, num_data )
    !
    call fxcard_cset_to_card ( c_data, c_cset )
    !
    ! put the cardlist structure to a file
    !
    call fxcard_card_to_file ( p, path_data, c_data, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    ! put the list_info cardset to a file
    !
    if (     associated ( c_data ) ) &
    call fxcard_delete ( c_data )
    !
    return
    !
997 continue
    !
    call pc_error ( &
    ' error in fxcard_array_to_file_l during fxcard_card_to_file  ' )
    !
    go to 999
    !
998 continue
    !
    ! cannot get an empty array data file
    !
    call pc_info (' empty array data file ', path_data )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxcard_array_to_file_l ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxcard_array_to_file_l            ! put logical array to file
  !
  subroutine fxcard_replace_string ( &
                                      n_change, &
                                      strin0_gat, string_out, &
                                      string_old, string_new &
                                    )
    !
    ! Change the string _old to strin_new in strin0_gat into string_out
    !
    integer,              intent(  out) :: n_change        ! change flag
    character(len=*),     intent(inout) :: strin0_gat
    character(len=*),     intent(inout) :: string_out
    character(len=*),     intent(in   ) :: string_old
    character(len=*),     intent(in   ) :: string_new
    !
    ! Local variables
    !
    integer                             :: n_new           ! new length 
    integer                             :: n_old           ! old length 
    integer                             :: n_inp           ! inp length 
    integer                             :: n_out           ! out length 
    integer                             :: m_out           ! out length max


    integer                             :: i_old_1         ! old index 1
    integer                             :: i_old_2         ! old index 2
    character(len=filename_length)      :: string_tmp      ! string temp
    !
    n_change = 0
    !
    if ( string_upper_compare ( string_old, pathcheck_empty ) ) return
    !
    if ( string_upper_compare ( string_new, pathcheck_empty ) ) return
    !
    n_old = len_trim ( string_old )
    !
    n_new = len_trim ( string_new )
    !
    n_inp = len_trim ( strin0_gat )
    !
    m_out = len      ( string_out ) 
    !
    !if ( index ( crd_xxx, '</PROCESS' ) .gt. 0 ) bot_proc = .true.
    !
    i_old_1 = index ( strin0_gat, trim(string_old) )
    !
    i_old_2 = i_old_1 + n_old - 1
    !
    n_out = n_inp - n_old + n_new
    !
    string_tmp = strin0_gat
    !
     !print'(" string_old=",a)',trim(string_old)
     !print'(" string_new=",a)',trim(string_new)
     !print'(" strin0_gat=",a)',trim(strin0_gat)
    !
    xxif_replace_string : if ( i_old_1 .gt. 0 .and. n_out .le. m_out ) then
      !
      n_change = n_change + 1
      !
      string_out = string_tmp(1:i_old_1-1) &
                // string_new(1:n_new) &
                // string_tmp(i_old_2+1:n_inp)
      !
    else xxif_replace_string 
      !
      string_out = string_tmp
      !
    end if xxif_replace_string 
    !
    !print'(" string_out=",a)',trim(string_out)
    !print'(" n_change=",i2," i_old_1=",i8," i_old_2=",i8)',&
    !n_change,i_old_1,i_old_2
    !
    !print'(" n_old=",i8," n_inp=",i8," n_out=",i8," m_out=",i8)',&
    !n_old,n_inp,n_out,m_out
    !
    return
    !
  end subroutine fxcard_replace_string
  !
  subroutine fxcard_replace_string_n ( &
                                    n_change, string_num, &
                                    strin0_gat, string_out, &
                                    string_old, string_new &
                                  )
    !
    ! Change the string _old to strin_new in strin0_gat into string_out
    !
    integer,              intent(  out) :: n_change        ! change flag
    integer,              intent(in   ) :: string_num
    character(len=*),     intent(inout) :: strin0_gat(:)
    character(len=*),     intent(inout) :: string_out(:)
    character(len=*),     intent(in   ) :: string_old
    character(len=*),     intent(in   ) :: string_new
    !
    ! Local variables
    !
    integer                             :: string_idx
    integer                             :: i_change
    !
    n_change = 0
    !
    do_string : do string_idx = 1 , string_num
      !
      call fxcard_replace_string ( &
                                i_change, &
                                strin0_gat(string_idx), &
                                string_out(string_idx), &
                                string_old, string_new &
                              )
      !
      n_change = n_change + i_change
      !
    end do do_string 
    !
    return
    !
  end subroutine fxcard_replace_string_n
  !
  subroutine fxcard_check_path ( &
                               n_change, &
                               string_1, string_2, &
                               string_a, string_b &
                             )
    !
    ! Change the string_a to string_b from string_1 into string_2
    !
    integer,              intent(  out) :: n_change        ! change flag
    character(len=*),     intent(inout) :: string_1
    character(len=*),     intent(inout) :: string_2
    character(len=*),     intent(in   ) :: string_a
    character(len=*),     intent(in   ) :: string_b
    !
    ! Local variables
    !










    !
    n_change = 0
    !
    if ( string_upper_compare ( string_a, pathcheck_empty ) ) return
    !
    if ( string_upper_compare ( string_b, pathcheck_empty ) ) return
    !
    if ( string_upper_compare ( string_2, pathcheck_empty ) ) &
    call fxcard_replace_string ( &
                                 n_change, &
                                 string_1, string_2, &
                                 string_a, string_b &
                               )
    !
    if ( string_upper_compare ( string_1, pathcheck_empty ) ) &
    call fxcard_replace_string ( &
                                 n_change, &
                                 string_2, string_1, &
                                 string_b, string_a &
                               )
    !
    return
    !
  end subroutine fxcard_check_path 
  !
  subroutine fxcard_check_path_n ( &
                                   n_change, string_num, &
                                   string_1, string_2, &
                                   string_a, string_b &
                                 )
    !
    ! Change the string_a to string_b from string_1 into string_2
    !
    integer,              intent(  out) :: n_change        ! change flag
    integer,              intent(in   ) :: string_num
    character(len=*),     intent(inout) :: string_1(:)
    character(len=*),     intent(inout) :: string_2(:)
    character(len=*),     intent(in   ) :: string_a
    character(len=*),     intent(in   ) :: string_b
    !
    ! Local variables
    !
    integer                             :: string_idx
    integer                             :: i_change
    !
    n_change = 0
    !
    do_string : do string_idx = 1 , string_num
      !
      call fxcard_check_path ( &
                               i_change, &
                               string_1(string_idx), &
                               string_2(string_idx), &
                               string_a, string_b &
                             )
      !
      n_change = n_change + i_change
      !
    end do do_string 
    !
    return
    !
  end subroutine fxcard_check_path_n
  !
  subroutine fxcard_h_gat_tig ( source_gather , hdr_x, hx_gat, hx_tig )
    !
    ! set the source and receiver header words
    !
    logical,           intent(in   ) :: source_gather    ! src gather
    integer,           intent(in   ) :: hdr_x            ! x midpoint header
    integer,           intent(inout) :: hx_tig           ! x receiver header
    integer,           intent(inout) :: hx_gat           ! x source   header
    !
    ! Local variables
    !
    integer                          :: hx_tmp           ! x temp     header
    !
    xxif_hdr_x : if ( hdr_x .eq. hdr_midpoint_xgrid ) then
      !
      hx_tig = hdr_receiver_xgrid
      hx_gat = hdr_source_xgrid
      !
    else if ( hdr_x .eq. hdr_midpoint_ygrid ) then
      !
      hx_tig = hdr_receiver_ygrid
      hx_gat = hdr_source_ygrid
      !
    else if ( hdr_x .eq. hdr_midpoint_xloc ) then
      !
      hx_tig = hdr_receiver_xloc
      hx_gat = hdr_source_xloc
      !
    else if ( hdr_x .eq. hdr_midpoint_yloc ) then
      !
      hx_tig = hdr_receiver_yloc
      hx_gat = hdr_source_yloc
      !
    else xxif_hdr_x
      !
      call pc_error ( ' error in fxcard_h_gat_tig hdr_x=', hdr_x )
      !
    end if xxif_hdr_x
    !
    ! if this is not a shot gather swap the src and rec header words
    !
    xxif_not_source_gather : if ( .not. source_gather ) then
      !
      hx_tmp = hx_gat 
      !
      hx_gat = hx_tig
      !
      hx_tig = hx_tmp 
      !
    end if xxif_not_source_gather
    !
    return
    !
  end subroutine fxcard_h_gat_tig 
  !
  subroutine fxcard_cardset_create ( c_cset )
    !
    ! create and clear cardset structure
    !
    type ( cardset_struct ),    pointer :: c_cset           ! cardset structure
    !
    ! create the cardset structure
    !
    !print*,' fxcard_cardset_create ass=',associated ( c_cset ) 
    if (     associated ( c_cset ) ) &
    call cardset_delete ( c_cset )
    !
    !print*,' fxcard_cardset_create bef cardset_create '
    call cardset_create ( c_cset )
    !
    ! clear the cardset structure
    !
    !print*,' fxcard_cardset_create bef cardset_clear '
    call cardset_clear  ( c_cset )
    !
    ! set the card set name
    !
    !print*,' fxcard_cardset_create bef cardset_set_name '
    call cardset_set_name ( c_cset, 'fxcard_cardset' )
    !print*,' fxcard_cardset_create aft cardset_set_name '
    !
    !stop
    !
    return
    !
  end subroutine fxcard_cardset_create
  !
end module fxcard_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
