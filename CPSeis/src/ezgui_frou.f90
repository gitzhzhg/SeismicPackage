!***********************************************************
!<CPS_v1 type="PROGRAM"/>
!!---------------------------- ezgui_frou.f90 ------------------------------!!
!!---------------------------- ezgui_frou.f90 ------------------------------!!
!!---------------------------- ezgui_frou.f90 ------------------------------!!


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
!                       C P S   P R O G R A M
!
! Name       : EZGUI_FROU
! Category   : stand-alone
! Written    : 2003-01-22   by: Charles C. Burch
! Revised    : 2003-08-26   by: SMCook
! Maturity   : beta
! Purpose    : subroutines for EzGUI utilty program to create XML GUI files
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! These are the Fortran subroutines that provide the main processing needed
! for ezgui.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                                  CALLING DOCUMENTATION
!
! These routines are for use by ezgui and are not intended for general use
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Note that the maturity setting refers to the maturity of the target xml, not
! the maturity of this code.
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2003-08-26  SMCook       Changed logic for docbasepath in custom case --
!                               now defined as same path as source code file
!                               (or, potentially, a link to the source file).
!  4. 2003-08-18  SMCook       Added maturity argument.  Immediate use is to
!                               determine a docbasepath for jpg images, etc.
!                               that developers check in as part of the
!                               "help section" in their CPS processes.
!  3. 2003-08-04  Stoeckley    Change N_INCLUDE_DIRS_SIZE from 12 to 112.
!  2. 2003-05-15  C C Burch    Fix Kombo button labels with ~~s.
!                              Use unicode for tip and help.
!                              Allow help to have html commands.
!                              Include fortran mainline test driver as comments.
!  1. 2003-01-22  C C Burch    Initial Version.
!
!
! Previous history when this was the mainline code:
! 19. 2003-01-20  Charles C. Burch  Increased num_cards for KMIG
! 18. 2001-09-24  Charles C. Burch  Fix bug with X edit fields
!                                   Treat Toggle and Kombobuttons keyword
!                                   handling like other edit fields
! 17. 2001-09-18  Charles C. Burch  Provide keyword for comments based on
!                                    content, eg. content => kw=COMMENT_content
!                                   Added columnSize for array elements in XML
!                                   Changed yStretch default to true for
!                                    pushbuttons and combo boxes
!                                   Provide support for toggle buttons such as
!                                    [keyword]`TTTT and title=`FFFFFFF`TTTT
!                                   and support for kombo buttons such as
!                                    [keyword]`KKKK and title=`FFFFFFF`KKKK
!                                   Added similar support for
!                                    title=`FFFFFFFF`CCCCC
!                                   Note kw for `TTTT in title=`FFFFFFF`TTTT
!                                   will be FLAG_TITLE
! 16. 2001-07-24  Charles C. Burch  Provide EQLAB_label for = in Q edit field
! 15. 2001-06-18  Charles C. Burch  Provide support of ROW and PARMS cards
!                                    in a border definition
! 14. 2001-06-11  Charles C. Burch  Support Q edit fields
!                                     label`QSSSSSSSSSSSSSSSSSSSSSSSSS produces
!                                     a "label" pushbutton when pressed is
!                                     suppose to get data and insert it into
!                                     the variable label_data that get inserted
!                                     into the data field with the QS...S
!                                       the S signifies character data, I and
!                                       F can also be used instead
! 13. 2001-04-23  Charles C. Burch  Support a title with a border
! 12. 2000-12-13  Charles C. Burch  Increased NUM_CARDS(10000) & HELP_SZ(160000)
! 11. 2000-10-16  Charles C. Burch  Allow keywords to be specified with Label
!                                    fields using [keyword] and
!                                    use of <PARMS..> with labels with keywords
!                                   Changed default keyword for labels from
!                                    LABELnn to EZGUI_LABELnn
!                                   Print out Include directories
! 10. 2000-10-13  Charles C. Burch  Increased NUM_CARDS to 5000
!                                   Convert <, > and & in labels to unicodes
!  9. 2000-10-06  Charles C. Burch  Include tip info in displayed help even
!                                    when no other help present
!  8. 2000-10-05  Charles C. Burch  Fixed bug if type="display_only occurs
!                                    after keyword="..." in help statement
!                                   Fixed bug in checking for Default and
!                                    allowed in helps
!  7. 2000-10-04  Charles C. Burch  Removed need for Default and allowed
!                                    on Button helps
!  6. 2000-10-03  Charles C. Burch  Changed CHECKHELP default to yes
!                                    (can be changed with -NOCHECKHELP)
!                                   Added support for display_only type
!                                   Checks for Default and Allowed in HELP
!                                    of editable fields
!  5. 2000-09-05  Charles C. Burch  Added support for <PARMS keyword[/NULL]> to
!                                   (1) Allow entries to have help but not be
!                                       in layout w/o warning
!                                   (2) To indicate a given field is not to be
!                                       part of the XML layout
!                                   Added option -CHECKHELP in Ezgui command to
!                                    print warnings when an entry is in the
!                                    layout but does not have any help/hint
!                                    specified
!  4. 2000-08-24  Charles C. Burch  Increased number of card records(NUM_CARDS)
!                                    and help information(HELP_SZ)
!                                   Added support for columnSize in XML
!                                    (/CS= in layout)
!  3. 2000-08-07  Charles C. Burch  Changed label names of edit fields
!                                    to LABEL_keyword
!  2. 2000-04-06  Charles C. Burch  Skips blanks lines in <HELPSECTION>
!                                    of include files not part of <HELP>
!                                   Warning message for ' found when`
!                                    might be intended
!  1. 2000-03-31  Charles C. Burch  Added ~ to array field definition
!                                   to allow setting xsize
!  0. 2000-03-21  Charles C. Burch  Initial version.
!                                   Prototype November 1999
!
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
! Must compile with the debug flag -g on linux.
! Use mkexe to compile and link.
!
!-------------------------------------------------------------------------------
!</compile_doc>


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

!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!
!!--------------------------- private module -------------------------------!!


module ezgui_module

  use string_module
  implicit none

  type row_col_elem
    integer                           :: screen_num
    integer                           :: x_pos
    integer                           :: y_pos
    integer                           :: length
    character (len=80)                :: label
    character (len=32)                :: keyword
    character (len=1)                 :: type
    character (len=6)                 :: alignment        ! right,left, center
    character (len=3)                 :: editable         ! yes no
    character (len=3)                 :: sensitive        ! yes, no
    character (len=5)                 :: x_stretch        ! true, false
    character (len=5)                 :: y_stretch        ! true, false
    character (len=1)                 :: filler
    integer                           :: link_num
!                           -1 for scalar, 0 for arrayset, >1 array element
    integer                           :: x_size
    integer                           :: y_size
    integer                           :: cell_width
    integer                           :: cell_height
    integer                           :: help_size
    character, dimension(:),pointer   :: help_info
    character (len=80)                :: tip_info
  end type row_col_elem

  integer                             :: window_width, window_height
  integer                             :: window_dialog
  integer                             :: check_help_sw=1

  character (len=20)                  :: editor = "vi"
  character (len=20)                  :: dir_com = "ls"

  character (len=80)                  :: program_title=                        &
   "EzGUI Screen layout conversion to CPS XML (version 1.02 2003-05-15)"

  integer, parameter                  :: XMLVARS_SZ=3000
  integer, parameter                  :: XML_DATA_SZ=100000

  integer, parameter                  :: N_INCLUDE_DIRS_SIZE=112
  integer                             :: n_include_dirs
  character (len=80)                  :: include_dirs(N_INCLUDE_DIRS_SIZE)

  integer                             :: MATURITY
  character (len=80)                  :: HELPDOCBASE

  character (len=1), parameter   :: HELP_CARD_TYPE='H',                        &
   NS_CARD_TYPE='N',                                                           &
   ENDHELP_CARD_TYPE='x',                                                      &
   HELPDATA_CARD_TYPE='h',                                                     &
   LAYOUT_CARD_TYPE='L',                                                       &
   NOMOREDATA_CARD_TYPE='X',                                                   &
   COMMENT_CARD_TYPE='C'

  character (len=1), parameter   ::TOP_AREA_CARD_TYPE="1",                     &
   BOTTOM_AREA_CARD_TYPE="2",                                                  &
   SCROLLABLE_SCREEN_CARD_TYPE="3",                                            &
   HELP_PANEL_CARD_TYPE="H",                                                   &
   MENUBAR_CARD_TYPE="4",                                                      &
   TOOLBAR_CARD_TYPE="5",                                                      &
   POPUP_MENU_CARD_TYPE="6",                                                   &
   NEW_SCREEN_CARD_TYPE="t",                                                   &
   END_SCREEN_CARD_TYPE="e"

  character (len=11) :: html_codes(200)              !stores valid html commands
  integer            :: n_html_codes=-1              !number of html commands
  integer            :: convert_unicode_html_mode=0  !# of of nested <html cmds

  character(len=100),save,public :: EZGUI_FROU_ident =                         &
   "$Id: ezgui_frou.f90,v 1.5 2003/08/26 17:25:25 SMCook beta sps $"

contains

!==============================================================
!                    General purpose routines
!*************************************************************
!  Searches string str(ibeg:iend) for the string srch
!  and returns the starting index in str where srch occurs
!  Return iend+1 if srch is not found
!
!  Written July 1999 by Charles C Burch
!**************************************************************
  integer function ezgui_find_chars(str, ibeg, iend, srch)

    character (len=*), intent(in):: str, srch
    integer, intent(in) :: ibeg, iend

    integer :: i                          !, srch_len

    if(ibeg>iend) then
      ezgui_find_chars=iend+1
      return
    endif

    i=index(str(ibeg:iend),srch)
    if(i==0) then
      ezgui_find_chars=iend+1
    else
      ezgui_find_chars=ibeg+i-1
    endif
    return
  end function ezgui_find_chars

!*************************************************************
!  Sees if string str(ibeg:ibeg+len(str1)-1) is str1
!    returns ibeg if there otherwise iend+1
!
!  Written November 1999 by Charles C Burch
!**************************************************************
  integer function ezgui_check_chars(str, ibeg, iend, str1)
    character (len=*), intent(in)    :: str, str1
    integer, intent(in)              :: ibeg,iend
    integer                          :: n

    n=len(str1)
    ezgui_check_chars=iend+1
    if( n<=(iend-ibeg+1)) then
      if(str(ibeg:ibeg+n-1)==str1) ezgui_check_chars=ibeg
    endif
    return
  end function ezgui_check_chars

!*****************************************************************
!  Searches string str(ibeg:iend) for non occurence of string srch
!  Returns the starting index in str where srch does not occurs
!  Returns iend+1 if srch is not found
!
!  Written July 1999 by Charles C Burch
!*****************************************************************
  integer function ezgui_find_not_chars(str, ibeg, iend, srch)
    character (len=*), intent(in):: str, srch
    integer, intent(in) :: ibeg, iend

    integer :: i, srch_len


    srch_len=len(srch)
    if(iend-ibeg+1<srch_len) then
      ezgui_find_not_chars=ibeg
      return
    endif
    do i=ibeg, iend
      if(str(i:i+srch_len-1)/=srch) then
        ezgui_find_not_chars=i
        return
      endif
    end do
    ezgui_find_not_chars=iend+1
    return
  end function ezgui_find_not_chars

!***********************************************************************
! Count incidnece of specific character in a string
!
! Written September 2001 by Charles C Burch
!***********************************************************************
  subroutine ezgui_get_string_char_count(str,char,count)
    character (len=*), intent(in) :: str, char
    integer, intent(out)          :: count

    integer                       :: i, n_str, n_char

    count=0
    n_char=len(char)
    n_str=len(str)-n_char+1
    do i=1, n_str
      if(char==str(i:i+n_char-1)) count=count+1
    enddo
    return
  end subroutine ezgui_get_string_char_count

!***************************************************************
! Convert character string to upper case
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_chars_upper_case(str)
    character (len=*), intent(inout) :: str

    integer            :: i, str_len, ich
    integer, parameter :: ia=ichar('a'),iz=ichar('z')
    integer, parameter :: i_adj=ichar('A')-ichar('a')

    str_len=len_trim(str)
    do i=1, str_len
      ich=ichar(str(i:i))
      if(ich>=ia .and. ich<=iz) then
        str(i:i)=char(ich+i_adj)
      elseif(ich<31) then
        str(i:i)=' '
      endif
    enddo
    return
  end subroutine ezgui_chars_upper_case

!***************************************************************
! Convert integer to character string with leading zeroes
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_convert_int_to_chars(in, str)
    character (len=*), intent(out) :: str
    integer, intent(in)            :: in

    integer           :: i, str_len, iwork, itemp

    str_len=len(str)
    iwork=in
    do i=1,str_len
      itemp=iwork/10
      str(str_len-i+1:str_len-i+1)=char(ichar('0')+iwork-10*itemp)
      iwork=itemp
    enddo
    return
  end subroutine ezgui_convert_int_to_chars

!***************************************************************
! Simple edit character varable with default value
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_char_prompt_edit(a_out, a_in, prompt)
    character (len=*), intent(in)  :: a_in, prompt
    character (len=*), intent(out) :: a_out

    character (len=132) :: a_save

    if(len(a_in)>0) then
      write(*, '(1x,a)',advance="no") prompt//'('//trim(a_in)//'):'
    else
      write(*, '(1x,a)',advance="no") prompt//':'
    endif
    a_save=a_in                                    !in case a_in same as a_out
    read (*,'(a)') a_out
    if(len_trim(a_out)==0) a_out=a_save
    return
  end subroutine ezgui_char_prompt_edit

!***************************************************************
! return numeric data(in str) as characters as a real
!   istat=0 if no error, /=0 if error
!
! Written July 1999 by Charles C Burch
!***************************************************************
  real function ezgui_chars_to_real(str, istat)
    character (len=*), intent(in) :: str
    integer, intent(out)          :: istat

    integer                       ::  str_len
    character                     :: for_mat*7

    str_len=len_trim(str)
    for_mat='(gxx.0)'
    call ezgui_convert_int_to_chars(str_len,for_mat(3:4))
    read(str(:str_len), for_mat, iostat=istat) ezgui_chars_to_real
    return
  end function ezgui_chars_to_real

!***************************************************************
! Convert real number (f) to string (a) with triming
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_real_to_chars(f, a_out)
    character (len=*), intent(out) :: a_out
    real, intent(in)               :: f

    integer   :: i_pos, i_pos1
    character :: a*20

    write(a,'(g20.7)') f
    i_pos=ezgui_find_chars(a,1,20,".")             !find decimal point
    if(i_pos<=20) then
      i_pos1=ezgui_find_chars(a,i_pos,20,'E')      !find E
      if(i_pos1<=20) then
        do i_pos=i_pos1-1,1,-1               !delete trailing 0 fefore E
          if(a(i_pos:i_pos)/='0') exit
        enddo
        if(a(i_pos:i_pos)=='.') i_pos=i_pos-1
        a(i_pos+1:)=a(:i_pos1)
      else
        do i_pos1=20, i_pos,-1               !delete trailing 0 after decimal pt
          select case (a(i_pos1:i_pos1))
          case (' ')
            cycle
          case ('0')
            a(i_pos1:i_pos1)=' '
            cycle
          case ('.')
            a(i_pos1:i_pos1)=' '
            exit
          case default
            exit
          end select
        enddo
      endif
    endif
    a_out=adjustl(a)
    return
  end subroutine ezgui_real_to_chars

!***************************************************************
! Simple edit real variable with default value
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_real_prompt_edit(f_out, f_in, prompt)
    character (len=*), intent(in) :: prompt
    real, intent(in)              :: f_in
    real, intent(out)             :: f_out

    character          :: a_out*80, a_in*20
    integer            :: istat

! convert f_in to alpha and compress if possible
    call ezgui_real_to_chars(f_in, a_in)

    100 call ezgui_char_prompt_edit(a_out,trim(a_in),prompt)
    f_out=ezgui_chars_to_real(a_out, istat)
    if(istat/=0) then
      write(*,"(1x,a)",advance='no') &
       "Error entering data-Try again-Enter return to continue:"
      read (*,'(a)') a_out
      goto 100
    endif

    return
  end subroutine ezgui_real_prompt_edit

!***************************************************************
! find position in str(ibeg:iend) where val occurs
!  returns ipos where starts(iend+1 if not found)
!    and isize of unicode character found
!
! Written December 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_find_unicode_value(str,ibeg,iend, val, ipos, isize)
    character (len=*), intent(in)    :: str
    integer, intent(in)              :: ibeg, iend, val
    integer, intent(out)             :: ipos, isize

    integer                          :: val1

    ipos=ibeg
    do while(ipos<=iend)
      call ezgui_find_unicode(str,ipos,iend,ipos, isize, val1)
      if(val1==val) return
    enddo

    isize=-1
    return
  end subroutine ezgui_find_unicode_value

!***************************************************************
! searches for unicode in str(ibeg:iend)
!  return val of value found, ipos where it starts
!    and isize of unicode characters
!
! Written December 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_find_unicode(str,ibeg,iend, ipos, isize, val)
    character (len=*), intent(in)    :: str
    integer, intent(in)              :: ibeg, iend
    integer, intent(out)             :: ipos, val, isize

    integer                          :: i_delim, istat

    ipos=ezgui_find_chars(str,ibeg, iend,"&#")
    if(ipos>iend) then
      i_delim=iend+1
    else
      i_delim=ezgui_find_chars(str,ipos,iend,";")
    endif

    if(i_delim>iend) then
      ipos=iend+1
      isize=-1
      val=0
      return
    endif

    select case (str(ipos+2:ipos+2))
    case ('x','X')
      val=ezgui_hex_chars_to_int(str(ipos+3:i_delim-1),istat)
    case ('o','O')
      val=ezgui_octal_chars_to_int(str(ipos+3:i_delim-1),istat)
    case default
      val=ezgui_chars_to_int(str(ipos+2:i_delim-1),istat)
    end select
    isize=i_delim-ipos+1
    return
  end subroutine ezgui_find_unicode

!***************************************************************
! return integer from hex character data in str
!   istat=0 if no error, /=0, if error
!
! Written December 1999 by Charles C Burch
!***************************************************************
  integer function ezgui_hex_chars_to_int(str, istat)
    character (len=*), intent(in)    :: str
    integer, intent(out)             :: istat

    integer                          :: i, str_len

    ezgui_hex_chars_to_int=0
    str_len=len(str)
    istat=0
    do i=1,str_len
      select case(str(i:i))
      case ('0':'9')
        ezgui_hex_chars_to_int=                                                &
         16*ezgui_hex_chars_to_int+ichar(str(i:i))-ichar('0')
      case ('a':'f')
        ezgui_hex_chars_to_int=                                                &
         16*ezgui_hex_chars_to_int+ichar(str(i:i))-ichar('a')+10
      case ('A':'F')
        ezgui_hex_chars_to_int=                                                &
         16*ezgui_hex_chars_to_int+ichar(str(i:i))-ichar('A')+10
      case (' ')
      case default
        istat=-1
      end select
    enddo
    return
  end function ezgui_hex_chars_to_int

!***************************************************************
! return integer from octal character data in str
!   istat=0 if no error, /=0, if error
!
! Written December 1999 by Charles C Burch
!***************************************************************
  integer function ezgui_octal_chars_to_int(str, istat)
    character (len=*), intent(in)    :: str
    integer, intent(out)             :: istat

    integer                          :: i, str_len

    ezgui_octal_chars_to_int=0
    str_len=len(str)
    istat=0
    do i=1,str_len
      select case(str(i:i))
      case ('0':'7')
        ezgui_octal_chars_to_int=                                              &
         8*ezgui_octal_chars_to_int+ichar(str(i:i))-ichar('0')
      case (' ')
      case default
        istat=-1
      end select
    enddo
    return
  end function ezgui_octal_chars_to_int

!***************************************************************
! return integer from decimal character data in str
!   istat=0 if no error, /=0, if error
!
! Written December 1999 by Charles C Burch
!***************************************************************
  integer function ezgui_chars_to_int(str, istat)
    character (len=*), intent(in)    :: str
    integer, optional, intent(out)   :: istat

    integer                          :: i, i1, str_len, isign,ierr

    ezgui_chars_to_int=0
    str_len=len(str)
    ierr=0
    isign=1

    do i1=1, str_len
      if(str(i1:i1)/=' ') exit
    enddo
    if(i1>str_len) goto 900

    if(str(i1:i1)=='-') then
      isign=-1
      i1=i1+1
    else if(str(i1:i1)=='+') then
      i1=i1+1
    endif

    do i=i1,str_len
      select case(str(i:i))
      case ('0':'9')
        ezgui_chars_to_int=10*ezgui_chars_to_int+ichar(str(i:i))-ichar('0')
      case (' ')
      case default
        ierr=-1
      end select
    enddo
    900 if(present(istat)) istat=ierr
    ezgui_chars_to_int=isign*ezgui_chars_to_int
    return
  end function ezgui_chars_to_int

!***************************************************************
! Convert integer j_in into character string a
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_int_to_chars(j_in,a_out)
    integer, intent(in)            ::j_in
    character (len=*), intent(out) :: a_out

    character         :: a*20

    write(a,'(i20)') j_in
    a_out=adjustl(a)
    return
  end subroutine ezgui_int_to_chars

!***************************************************************
! Simple edit integer variable with default value
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_int_prompt_edit(j_out, j_in, prompt)
    character (len=*), intent(in) :: prompt
    integer, intent(in)           :: j_in
    integer, intent(out)          :: j_out

    character          :: a_out*80, a_in*20
    integer            :: istat

    call ezgui_int_to_chars(j_in, a_in)

    100 call ezgui_char_prompt_edit(a_out,trim(a_in),prompt)
    j_out=ezgui_chars_to_int(a_out, istat)
    if(istat/=0) then
      write(*,"(1x,a)",advance="no")                                        &
       "Error entering data-Try again-Enter return to continue:"
      read (*,'(a)') a_out
      goto 100
    endif
    return
  end subroutine ezgui_int_prompt_edit

!*****************************************************************
! print out card num with no carriage control
!
! written January 2003 by Charles C Burch
!*****************************************************************
  subroutine ezgui_print_card_num(icard)
    integer, intent(in) :: icard

    integer, save       :: carriage_control_sw=0

    carriage_control_sw=carriage_control_sw+1
    if(carriage_control_sw<16) then
      write(*,"(i5)",advance="no") icard
    else
      write(*,"(i5)") icard
      carriage_control_sw=0
    endif
    return
  end  subroutine ezgui_print_card_num

!*****************************************************************
! Open an files using an array of directories
! Try each directory entry until file gets opened
!  istat=dir index where file was opened,-1 if unable to find file
!
! Written March 2000 by Charles C Burch
!*****************************************************************
  subroutine ezgui_open_using_dirs (ifile, n_dirs, dirs, fname, istat)
    integer, intent(in)            :: ifile, n_dirs
    character (len=*), intent(in) :: fname
    character (len=*), intent(in) :: dirs(:)
    integer, intent(out)           :: istat

    integer  :: i

    do i=1, n_dirs
      istat=-1
      open(unit=ifile, file=trim(dirs(i))//trim(fname),                        &
       status='old', iostat=istat)
      if(istat==0) then
        istat=i
        return
      endif
    enddo

    istat=-1
    return
  end subroutine ezgui_open_using_dirs

!***************************************************************
! Simple get input file name and see if it exists (istat=0)
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_get_filename_in(prompt,file_name, istat)
    character (len=*), intent(out) :: file_name
    character (len=*), intent(in)  :: prompt
    integer, intent(out)           :: istat

    100  write(*,'(1x,a)', advance="no") prompt
    read(*,'(a)') file_name
    if(len_trim(file_name)==0) then
      istat=0
      return
    endif
    if(file_name(1:4)=="dir " .or. file_name(1:4)=="DIR ") then
      call ezgui_system(dir_com//" "//trim(file_name(5:))  )
      goto 100
    endif
    open(unit=11,file=file_name,status='old',iostat=istat)
    close (11)
    return
  end subroutine ezgui_get_filename_in

!***************************************************************
! Simple get output file name and see if valid (istat=0)
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_get_filename_out(prompt,file_name, istat)
    character (len=*), intent(out) :: file_name
    character (len=*), intent(in)  :: prompt
    integer, intent(out)           :: istat

    100 write(*,'(1x,a)', advance="no") prompt
    read(*,'(a)') file_name
    if(len_trim(file_name)==0) then
      istat=0
      return
    endif
    if(file_name(1:4)=="dir " .or. file_name(1:4)=="DIR ") then
      call ezgui_system(dir_com//" "//trim(file_name(5:))  )
      goto 100
    endif
    open(unit=12,file=file_name,status='unknown',iostat=istat)
    close (12)
    return
  end subroutine ezgui_get_filename_out

!******************************************************************
! Simple filter of non-alpha characters in a character string
!   and replace tabs with spaces with tab staops at every tab_stops
!
! Written July 1999 by Charles C Burch
!******************************************************************
  subroutine ezgui_clean_up_text(str)
    character (len=*), intent(inout) :: str

    integer, parameter  :: tab_stops=8
    integer             :: i, n_str, n_tab, str_len, ich

    str_len=len_trim(str)
    n_str=len(str)
    i=1
    do while(i<=str_len)
      ich=ichar(str(i:i))
      if(ich==9) then
!     print *,'Tab found at position',i
        n_tab=((i+tab_stops-1)/tab_stops)*tab_stops +1 - i
        str(i:n_str)=repeat(' ', n_tab)//str(i+1:n_str)
        i=i+n_tab-1
        str_len=min(n_str,str_len+n_tab-1)
      elseif(ich<31) then
        str(i:i)=' '
      endif
      i=i+1
    enddo
    return
  end subroutine ezgui_clean_up_text

!*********************************************
! convert certain characters to unicode
!   str_in=input string, str_out=output string
!   n=len of trim(str_out), =-1 if error
!
! currently, characters converted: <, >, &
!  other can be added as needed
!
! Written October 2000 by Charles C Burch
!*********************************************
  subroutine ezgui_convert_str_to_unicode(str_in, str_out, n, pad_sw)
    character (len=*), intent(in)  :: str_in
    character (len=*), intent(out) :: str_out
    integer,           intent(out) :: n
    logical, optional, intent(in)  :: pad_sw

    integer                        :: len_str_in, len_str_out, i, j, l
    logical                        :: pad

    if(present(pad_sw)) then
      pad=pad_sw
    else
      pad=.true.
    endif

    len_str_in=len_trim(str_in)
    len_str_out=len(str_out)

    n=0
    i=1
    do while(i.le.len_str_in)
      select case(str_in(i:i))
      case ('<')
        n=n+4
        if(n.gt.len_str_out) exit
        str_out(n-3:n)="&lt;"

      case ('>')
        n=n+4
        if(n.gt.len_str_out) exit
        str_out(n-3:n)="&gt;"

      case ('&')
        j=ezgui_find_chars(str_in,i,len_str_in,";")
        if(j.le.len_str_in) then
          l=j-i+1
          if(str_in(i+1:i+1).eq.'#'     .or.                                   &
           str_in(i:j).eq."&lt;" .or. str_in(i:j).eq."&LT;" .or.               &
           str_in(i:j).eq."&gt;" .or. str_in(i:j).eq."&GT'" .or.               &
           str_in(i:j).eq."&amp;".or. str_in(i:j).eq."&AMP;" ) then
            n=n+l
            if(n.gt.len_str_out) exit
            str_out(n-l+1:n)=str_in(i:j)
            i=j+1
            cycle
          endif
        endif

        n=n+5
        if(n.gt.len_str_out) exit
        str_out(n-4:n)="&amp;"

      case default
        n=n+1
        if(n.gt.len_str_out) exit
        str_out(n:n)=str_in(i:i)
      end select

      i=i+1
    enddo

    if(n.lt.len_str_out) then
      if(pad) str_out(n+1:len_str_out)=" "
    else if(n.gt.len_str_out) then
      n=-1
    endif

    return
  end subroutine ezgui_convert_str_to_unicode

!*******************************************************************
! initialize variables used to see if characters part of html
!
! Written January 2003 by Charles C Burch
!*******************************************************************
  subroutine ezgui_init_convert_to_unicode()
    if(n_html_codes.lt.0) then
      html_codes(  1)="a"
      html_codes(  2)="/a"
      html_codes(  3)="abbr"
      html_codes(  4)="/abbr"
      html_codes(  5)="acronym"
      html_codes(  6)="/acronym"
      html_codes(  7)="address"
      html_codes(  8)="/address"
      html_codes(  9)="applet"
      html_codes( 10)="/applet"
      html_codes( 11)="b"
      html_codes( 12)="/b"
      html_codes( 13)="basefont"
      html_codes( 14)="/basefont"
      html_codes( 15)="bdo"
      html_codes( 16)="/bdo"
      html_codes( 17)="big"
      html_codes( 18)="/big"
      html_codes( 19)="blink"
      html_codes( 20)="/blink"
      html_codes( 21)="isindex"
      html_codes( 22)="blockquote"
      html_codes( 23)="/blockquote"
      html_codes( 24)="bgsound"
      html_codes( 25)="body"
      html_codes( 26)="/body"
      html_codes( 27)="caption"
      html_codes( 28)="/caption"
      html_codes( 29)="center"
      html_codes( 30)="/center"
      html_codes( 31)="cite"
      html_codes( 31)="/cite"
      html_codes( 31)="code"
      html_codes( 32)="/code"
      html_codes( 33)="col"
      html_codes( 34)="colgroup"
      html_codes( 35)="dd"
      html_codes( 36)="/dd"
      html_codes( 37)="del"
      html_codes( 38)="/del"
      html_codes( 39)="dfn"
      html_codes( 40)="/dfn"
      html_codes( 41)="dir"
      html_codes( 42)="/dir"
      html_codes( 43)="div"
      html_codes( 44)="/div"
      html_codes( 45)="dl"
      html_codes( 46)="/dl"
      html_codes( 47)="dt"
      html_codes( 48)="/dt"
      html_codes( 49)="em"
      html_codes( 50)="/em"
      html_codes( 51)="fieldset"
      html_codes( 52)="/fieldset"
      html_codes( 53)="font"
      html_codes( 54)="/font"
      html_codes( 55)="input"
      html_codes( 56)="keygen"
      html_codes( 57)="form"
      html_codes( 58)="/form"
      html_codes( 59)="frame"
      html_codes( 60)="frameset"
      html_codes( 61)="h1"
      html_codes( 62)="/h1"
      html_codes( 63)="h2"
      html_codes( 64)="/h2"
      html_codes( 65)="h3"
      html_codes( 66)="/h3"
      html_codes( 67)="h4"
      html_codes( 68)="/h4"
      html_codes( 69)="h5"
      html_codes( 70)="/h5"
      html_codes( 71)="h6"
      html_codes( 72)="/h6"
      html_codes( 73)="base"
      html_codes( 74)="link"
      html_codes( 75)="meta"
      html_codes( 76)="nextid"
      html_codes( 77)="head"
      html_codes( 78)="/head"
      html_codes( 77)="html"
      html_codes( 78)="/html"
      html_codes( 79)="i"
      html_codes( 80)="/i"
      html_codes( 81)="ilayer"
      html_codes( 82)="/ilayer"
      html_codes( 83)="ins"
      html_codes( 84)="/ins"
      html_codes( 85)="kbd"
      html_codes( 86)="/kbd"
      html_codes( 97)="input"
      html_codes( 98)="form"
      html_codes( 99)="/form"
      html_codes(100)="layer"
      html_codes(101)="/layer"
      html_codes(102)="legend"
      html_codes(103)="/legend"
      html_codes(104)="li"
      html_codes(105)="/li"
      html_codes(106)="listing"
      html_codes(107)="/listing"
      html_codes(108)="area"
      html_codes(109)="map"
      html_codes(110)="/map"
      html_codes(111)="marquee"
      html_codes(112)="/marquee"
      html_codes(113)="menu"
      html_codes(114)="/menu"
      html_codes(115)="multicol"
      html_codes(116)="/multicol"
      html_codes(117)="nobr"
      html_codes(118)="/nobr"
      html_codes(119)="noembed"
      html_codes(120)="/noembed"
      html_codes(121)="noframes"
      html_codes(122)="/noframes"
      html_codes(123)="noscript"
      html_codes(124)="/noscript"
      html_codes(125)="parm"
      html_codes(126)="object"
      html_codes(127)="/object"
      html_codes(128)="ol"
      html_codes(129)="/ol"
      html_codes(130)="optgroup"
      html_codes(131)="/optgroup"
      html_codes(132)="option"
      html_codes(133)="/option"
      html_codes(134)="p"
      html_codes(135)="/p"
      html_codes(136)="br"
      html_codes(137)="hr"
      html_codes(138)="pre"
      html_codes(139)="/pre"
      html_codes(140)="q"
      html_codes(141)="/q"
      html_codes(142)="s"
      html_codes(143)="/s"
      html_codes(144)="samp"
      html_codes(145)="/samp"
      html_codes(146)="script"
      html_codes(147)="/script"
      html_codes(148)="select"
      html_codes(149)="/select"
      html_codes(150)="server"
      html_codes(151)="/server"
      html_codes(152)="small"
      html_codes(153)="/small"
      html_codes(154)="span"
      html_codes(155)="/span"
      html_codes(156)="strike"
      html_codes(157)="/strike"
      html_codes(158)="strong"
      html_codes(159)="/strong"
      html_codes(160)="style"
      html_codes(161)="/style"
      html_codes(162)="sub"
      html_codes(163)="/sub"
      html_codes(164)="sup"
      html_codes(165)="/sup"
      html_codes(166)="tbody"
      html_codes(167)="tfoot"
      html_codes(168)="thead"
      html_codes(169)="table"
      html_codes(170)="/table"
      html_codes(171)="td"
      html_codes(172)="/td"
      html_codes(173)="embed"
      html_codes(174)="iframe"
      html_codes(175)="img"
      html_codes(176)="spacer"
      html_codes(177)="wbr"
      html_codes(178)="textarea"
      html_codes(179)="/textarea"
      html_codes(180)="th"
      html_codes(181)="/th"
      html_codes(182)="title"
      html_codes(183)="/title"
      html_codes(184)="tr"
      html_codes(185)="/tr"
      html_codes(186)="tt"
      html_codes(187)="/tt"
      html_codes(188)="u"
      html_codes(189)="/u"
      html_codes(190)="ul"
      html_codes(191)="/ul"
      html_codes(192)="var"
      html_codes(193)="/var"
      html_codes(194)="xmp"
      html_codes(195)="/xmp"
      html_codes(196)="parm"
      n_html_codes=196
    endif

    convert_unicode_html_mode=0
    return
  end subroutine ezgui_init_convert_to_unicode

!*************************************************************************
! convert certain characters to unicode bypassing converting html commands
!   str_in=input string, str_out=output string
!   n=len of str_out, =-1 if error
!
! currently, characters converted: <, >, &
!  other can be added as needed
!
! Written January 2003 by Charles C Burch
!*************************************************************************
  subroutine ezgui_convert_to_unicode_html(str_in, str_out, n, pad_sw)
    character (len=*), intent(in)  :: str_in
    character (len=*), intent(out) :: str_out
    integer,           intent(out) :: n
    logical, optional, intent(in)  :: pad_sw

    integer                        :: len_str_in, len_str_out, i, j, i1, i2
    logical                        :: pad

    if(present(pad_sw)) then
      pad=pad_sw
    else
      pad=.true.
    endif

    len_str_in=len_trim(str_in)
    len_str_out=len(str_out)

    n=0
    i=1
    do while(i.le.len_str_in)
      select case(str_in(i:i))
      case ('<')     !check if part of html command
        i1=ezgui_find_not_chars(str_in, i+1, len_str_in, " ")
        j=n_html_codes+1
        if(i1.lt.len_str_in) then
          i2=min(ezgui_find_chars(str_in, i1, len_str_in, " "),              &
                 ezgui_find_chars(str_in, i1, len_str_in, ">"))
          do j=1,n_html_codes  !check input with html table
            if(str_in(i1:i2-1).eq.trim(html_codes(j))) exit
           enddo
        endif
        if(j.le.n_html_codes) then
          ! convert_unicode_html_mode=nest level of html command-0=non-html
          convert_unicode_html_mode=convert_unicode_html_mode+1 !html command
          n=n+1
          if(n.gt.len_str_out) exit
          str_out(n:n)="<"
        else
          n=n+4                      !non html command
          if(n.gt.len_str_out) exit
          str_out(n-3:n)="&lt;"
        endif

      case ('>')
        if(convert_unicode_html_mode.gt.0) then !see if end of html command
          convert_unicode_html_mode=convert_unicode_html_mode-1
          n=n+1
          if(n.gt.len_str_out) exit
          str_out(n:n)=">"
        else
          n=n+4
          if(n.gt.len_str_out) exit
          str_out(n-3:n)="&gt;"
        endif

      case ('&')
        j=ezgui_find_chars(str_in,i,len_str_in,";")
        if(j.le.len_str_in) then
          if(str_in(i+1:i+1).eq.'#'     .or.                                   &
           str_in(i:j).eq."&lt;" .or. str_in(i:j).eq."&LT;" .or.               &
           str_in(i:j).eq."&gt;" .or. str_in(i:j).eq."&GT'" .or.               &
           str_in(i:j).eq."&amp;".or. str_in(i:j).eq."&AMP;" ) then
            j=j         !no op--part of input unicode
          else
            j=len_str_in+1 !not part of input unicode
          endif
        endif

        if(j.le.len_str_in) then
          n=n+1            !part of unicode
          if(n.gt.len_str_out) exit
          str_out(n:n)=str_in(i:i)
        else
          n=n+5
          if(n.gt.len_str_out) exit
          str_out(n-4:n)="&amp;"
        endif

      case default
        n=n+1              !not a special character
        if(n.gt.len_str_out) exit
        str_out(n:n)=str_in(i:i)
      end select

      i=i+1
    enddo

    if(n.lt.len_str_out) then
      if(pad) str_out(n+1:len_str_out)=" "
    else if(n.gt.len_str_out) then
      n=-1
    endif

    return
  end subroutine ezgui_convert_to_unicode_html


!*******************************************************************
! convert field type code
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_convert_field_type_code(a_type, field_type)
    character (len=*), intent(in)  :: a_type
    character (len=*), intent(out) :: field_type

    character  :: a_type_upper_case*1

    a_type_upper_case=a_type
    call ezgui_chars_upper_case(a_type_upper_case)
    select case (a_type_upper_case)
    case ("A")
      field_type="ArraySet"
    case ("B")
      field_type="Border"
    case ("C")
      field_type="comboBox"
    case("D")
      field_type="DoubleButton"
    case (END_SCREEN_CARD_TYPE)    !"e"
      field_type="EndScreen"
    case ("F")
      field_type="float"
    case ("H")
      field_type="HelpPanel"
    case ("I")
      field_type="int"
    case ("K")                     !KomboButton
      field_type="comboButton"
    case ("L")
      field_type="Label"
    case ("M")
      field_type="Menu"
    case ("O")
      field_type="option"
    case ("P")
      field_type="Button"
    case ("Q")
      field_type="Query"
    case ("R")
      field_type="RadioButton"
    case ("S")
      field_type="string"
    case ("T")
      field_type="toggleButton"
    case (NEW_SCREEN_CARD_TYPE)    !"t"
      field_type="title"           !Screen
    case ("X")
      field_type="info"
    case ("0")
      field_type="Null"
    case (TOP_AREA_CARD_TYPE)           !"1"
      field_type="TopAreaComponent"
    case (BOTTOM_AREA_CARD_TYPE)        !"2"
      field_type="BottomAreaComponent"
    case (SCROLLABLE_SCREEN_CARD_TYPE)  !"3"
      field_type="ScrollableScreen"
    case (MENUBAR_CARD_TYPE)            !"4"
      field_type="MenuBar"
    case (TOOLBAR_CARD_TYPE)            !"5"
      field_type="ToolBar"
    case (POPUP_MENU_CARD_TYPE)         !"6"
      field_type="PopupMenu"
    case default
      field_type=" "
    end select
    return
  end subroutine ezgui_convert_field_type_code

!*******************************************************************
! get field type code
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_get_field_type_code(field_type, a_type)
    character (len=*), intent(out) :: a_type
    character (len=*), intent(in)  :: field_type

    character             :: field_type_upper_case*20

    field_type_upper_case=field_type
    call ezgui_chars_upper_case(field_type_upper_case)
    select case (trim(field_type_upper_case))
    case ("INT")
      a_type="I"
    case ("FLOAT")
      a_type="F"
    case ("STRING")
      a_type="S"
    case ("OPTION")
      a_type="O"
    case ("RADIOBUTTON")
      a_type="R"
    case ("COMBOBOX")
      a_type="C"
    case ("TOGGLEBUTTON")
      a_type="T"
    case ("COMBOBUTTON")
      a_type="K"
    case ("LABEL")
      a_type="L"
    case ("MENU")
      a_type="M"
    case ("QUERY")
      a_type="Q"
    case ("ARRAYSET")
      a_type="A"
    case ("BUTTON")
      a_type="P"
    case ("BORDER")
      a_type="B"
    case ("INFO")
      a_type="X"
    case ("TITLE")
      a_type=NEW_SCREEN_CARD_TYPE     !"t"
    case("HELPPANEL")
      a_type="H"
    case ("TOPAREACOMPONENT")
      a_type=TOP_AREA_CARD_TYPE      !"1"
    case ("NULL")
      a_type="0"
    case ("BOTTOMAREACOMPONENT")
      a_type=BOTTOM_AREA_CARD_TYPE   !"2"
    case ("SCROLLABLESCREEN")        !"3"
      a_type=SCROLLABLE_SCREEN_CARD_TYPE
    case ("MENUBAR")
      a_type=MENUBAR_CARD_TYPE       !"4"
    case ("TOOLBAR")
      a_type=TOOLBAR_CARD_TYPE       !"5"
    case ("POPUPMENU")
      a_type=POPUP_MENU_CARD_TYPE    !"6"
    case ("ENDSCREEN")               !"e"
      a_type=END_SCREEN_CARD_TYPE
    case ("DOUBLEBUTTON")
      a_type="D"
    case default
      a_type=" "
    end select
    return
  end subroutine ezgui_get_field_type_code

!=================================================================
!                     Specific layout/xml subroutines
!***************************************************************
! strip out characters  between [ and ],
!   report #chars stripped in removed
!     Internal EzGUI usage only
!
! Written Nov 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_condense_label(label, removed)
    character (len=*), intent(inout)  :: label
    integer, intent(out)              :: removed

    integer :: i1,i2, n

    n=len_trim(label)
    i1=1
    do while(i1<n)                          !Convert 2 or more ~ to blanks
      i1=ezgui_find_chars(label,i1,n,'~')
      if(i1>n) exit
      i2=ezgui_find_not_chars(label,i1,n,'~')
      if((i2-i1)>1) label(i1:i2-1)=" "
      i1=i2+1
    enddo

    removed=0
    i1=ezgui_find_chars(label,1,n,'[')
    do while (i1<n)
      i2=ezgui_find_chars(label,i1,n,']')
      if(i2>n) return
      label(i1:)=label(i2+1:n)
      removed=removed+i2-i1+1
      i1=ezgui_find_chars(label,1,n,'[')
    enddo
    return
  end subroutine ezgui_condense_label

!***************************************************************
! Read layout/F90 files and store XML output array info
!     Internal EzGUI usage only
!
! Written July 1999 by Charles C Burch
! Separated into subroutines Jan 2000 by Charles C Burch
!***************************************************************
  subroutine ezgui_read_layout_file(file_name, cards, card_type, n_cards,      &
     n_elems, row_col_elems, istat)
    character (len=*), intent(in)  :: file_name
    integer, intent(in)            :: n_cards
    character (len=*)              :: cards(n_cards), card_type(n_cards)
    integer, intent(inout)         :: n_elems
    integer, intent(out)           :: istat
    type (row_col_elem)            :: row_col_elems(:)

    character (len=256)            :: card
    integer                        :: i_file, i_card, i_apos, i_help, io_stat
    integer                        :: i_mode
    logical                        :: f90_sw, process_file_sw

    i_file=11
    open(unit=i_file,file=file_name,status='old',iostat=istat)
    if(istat/=0) then
      print *,                                                                 &
       'Unable to open input file('//trim(file_name)//') in read-layout_file'
      return
    endif

    i_apos=0
    i_help=0
    i_card=0
    i_mode=1
    process_file_sw=.false.

    card=file_name                           !See if F90 file
    call ezgui_chars_upper_case(card)
    f90_sw=index(card,'.F90')>0
    if(f90_sw) then
      call ezgui_process_layout_input_card(i_file, '<HELP>', cards, card_type, &
       i_card, n_cards, i_apos, i_help, istat)
    endif

! Read cards from layout file

    do while(i_card<n_cards)
      read(i_file,'(a256)', iostat=io_stat) card  !read a line from input file
      if(io_stat/=0) exit

      call ezgui_clean_up_text(card)
      if(f90_sw) then
        call ezgui_preprocess_f90_layout(card, i_mode)          !f90 input
!     print *,'f90 check, mode, card...',i_mode, trim(card(2:))

        if(i_mode>10) then
!       print *,'imode=',i_mode
          if(i_mode==19) then          !gui_def just found
            call ezgui_process_layout_input_card(i_file, '<ENDHELP>', cards,   &
             card_type, i_card, n_cards, i_apos, i_help, istat)
            if(process_file_sw) then
              call ezgui_process_layout_input_card(i_file, '<INCLUDE top.lay>',&
               cards, card_type, i_card, n_cards, i_apos, i_help, istat)
              call ezgui_process_layout_input_card(i_file,                     &
               '<INCLUDE bottom.lay>',cards, card_type, i_card, n_cards,       &
               i_apos, i_help, istat)
            endif

          else if(i_mode==11) then
            process_file_sw=.true.

          else if(i_mode==18) then
            call ezgui_process_layout_input_card(i_file, '<HELPSECTION>',      &
             cards, card_type, i_card, n_cards, i_apos, i_help, istat)
          endif
          i_mode=i_mode-10

        else
          if(i_mode/=1)                                                        &
           call ezgui_process_layout_input_card(i_file, card(2:), cards,       &
           card_type, i_card, n_cards, i_apos, i_help, istat)
        endif

      else
        call ezgui_process_layout_input_card(i_file, card, cards, card_type,   &
         i_card, n_cards, i_apos, i_help, istat)
      endif
    enddo

    close(unit=i_file)
    if(i_card.ge.n_cards) then
      print *,"WARNING: n_cards needs to be expanded in ezgui_read_layout_file"
    endif

! Process the cards

    call ezgui_process_layout_cards(cards, card_type, i_card, i_apos, n_elems, &
     row_col_elems)
    return
  end subroutine ezgui_read_layout_file

!********************************************************************
! pre-process a F90 card and determine if it is part of layout or not
!   Returns i_mode
!     i_mode=1 skip card,                 11-<CPS_v1 type="PROCESS"/> just found
!            2-process help(descrip-doc), 12-descip_doc just found
!            3-process_help(advice_doc),  13-advice_doc just found
!            4-process help (brief_doc),  14-brief_doc just found
!            5-process help (history_doc),15-history-doc just found
!            8-parmater help(HelpSection),18-HelpSection just found
!            9-layout (gui_def).          19-gui_def just found
!     *******Internal EzGUI usage only*******
!
! Written Dec 2000 by Charles C Burch
!********************************************************************
  subroutine ezgui_preprocess_f90_layout(card, i_mode)
    character (len=*), intent(inout)    :: card
    integer, intent(inout)           :: i_mode

    character (len=256)              :: card_upper_case
    integer                          :: card_len, x_pos


    card_len=len_trim(card)
    if(card_len==0) then
      card='!'
      card_len=1
    endif

    x_pos=ezgui_find_not_chars(card,1,card_len," ")
    if(card(x_pos:x_pos)/='!') then                   !non-comment cards skipped
      i_mode=1
      return
    endif

    card_upper_case=card
    call ezgui_chars_upper_case(card_upper_case)

    select case(i_mode)
    case (1)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,                     &
       '!<CPS_V1 TYPE="PROCESS"/>')<card_len) then
        i_mode=11
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<DESCRIPT_DOC>')<  &
       card_len) then
        i_mode=12
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<ADVICE_DOC>')<    &
       card_len)then
        i_mode=13
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<BRIEF_DOC>')<     &
       card_len) then
        i_mode=14
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<HISTORY_DOC>')<   &
       card_len)then
        i_mode=15
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<HELPSECTION>')<   &
       card_len)then
        i_mode=18
        return
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!<GUI_DEF>')<       &
       card_len) then
        i_mode=19          !gets changed to 9
        return
      endif

    case (2)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</DESCRIPT_DOC>')< &
       card_len) i_mode=1

    case(3)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</ADVICE_DOC>')<   &
       card_len) i_mode=1

    case(4)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</BRIEF_DOC>')<    &
       card_len) i_mode=1

    case(5)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</HISTORY_DOC>')<  &
       card_len) i_mode=1

    case(8)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</HELPSECTION>')<  &
       card_len) i_mode=1

    case(9)
      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'!</GUI_DEF>')<      &
       card_len)  i_mode=1

    end select

    return
  end subroutine ezgui_preprocess_f90_layout

!***************************************************************
! process a layout input card  and store into cards array
!   Reads any needed include files
!
!     ************Internal EzGUI usage only***********
!
! Written July 1999 by Charles C Burch
! Made into separate subroutine Jan 2000 by Charles C Burch
!***************************************************************
  subroutine ezgui_process_layout_input_card(in_file, card_in, cards,          &
     card_type, i_card, n_cards, i_apos, i_help, istat)
    integer, intent(in)               :: in_file, n_cards
    integer, intent(inout)            :: i_card, i_apos, i_help
    character (len=*), intent(in)     :: card_in
    character (len=*), intent(inout)  :: cards(:), card_type(:)
    integer, intent(out)              :: istat


    character (len=256)     :: card_upper_case, card
    integer                 :: i_file, io_stat, ibeg, iend, i
    integer                 :: card_len, x_pos, i_mode
    integer                 :: f90_mode_stack(10)

    i_file=in_file
    i_mode=1
    card=card_in
    f90_mode_stack(1)=0 !f90_mode_stack mode ezgui_preprocess_f90_layout

    do while(i_card<=n_cards)
!either use card passed by subroutione or read from file
      select case(i_mode)
      case (1)
        i_mode=2             !Process card just passed into subroutine

      case(2)                !Include card found-read included cards
        if(i_file==11) return
        read(i_file,'(a256)', iostat=io_stat) card !read a line from input file
        if(io_stat/=0) then
          close(unit=i_file) !end of file-close and go to previous data file
          i_file=i_file-1
          if(i_help>0) then
            i_card=i_card+1
            card_type(i_card)=ENDHELP_CARD_TYPE
            cards(i_card)="<ENDHELP>"
            i_help=0
          endif
          if(i_file==11) return
          cycle
        endif
        call ezgui_clean_up_text(card)

      end select

      if(f90_mode_stack(i_file-10)>0) then   !see if reading cards from f90 file
        call ezgui_preprocess_f90_layout(card, f90_mode_stack(i_file-10))
!     print *,'preproc f90...',f90_mode_stack(i_file-10), trim(card)
        if(f90_mode_stack(i_file-10)>10) then
          select case (f90_mode_stack(i_file-10))
          case(12,13,14,15)               !Skip process help from included files
            f90_mode_stack(i_file-10)=1
            cycle
          case(18)
            i_help=2
          case (19)
            i_card=i_card+1
            card_type(i_card)=ENDHELP_CARD_TYPE
            cards(i_card)="<ENDHELP>"
            i_help=0
          end select
          f90_mode_stack(i_file-10)=f90_mode_stack(i_file-10)-10
          cycle
        endif
        if(f90_mode_stack(i_file-10)==1) cycle
        card=card(2:)                                 !bypass ! and process card
      endif

      card_len=len_trim(card)
!   print *,'process layout card....',card_len,i_help, trim(card)
      if(card_len==0 .and. i_help==2) cycle     !skip blank cards in helpsection

      card_upper_case=card
      call ezgui_chars_upper_case(card_upper_case)
      x_pos=ezgui_find_not_chars(card_upper_case,1,card_len," ")
      if(card_upper_case(x_pos:x_pos+1)=="<!") cycle     !bypass comment cards

      ibeg=ezgui_check_chars(card_upper_case,x_pos,                            &
       card_len,'<INCLUDE ') !process INCLUDE, if present
      if(ibeg<card_len) then
        iend=ezgui_find_chars(card_upper_case,ibeg,card_len,'>')
        i_file=i_file+1
        if(index(card_upper_case(ibeg+9:iend-1),'.F90')> 0)                    &
         then   !see if F90 or .lay
          f90_mode_stack(i_file-10)=1
        else
          f90_mode_stack(i_file-10)=0
        endif

        call ezgui_open_using_dirs(i_file,n_include_dirs, include_dirs,        &
         card(ibeg+9:iend-1),istat)
        if(istat<0) then
          print *,                                                             &
           'Unable to open file ('//card(ibeg+9:iend-1)//') from <INCLUDE card'
          print *, '  '//card(1:card_len)
          i_file=i_file-1
        else
          print *,'Include file ('//trim(card(ibeg+9:iend-1))//                &
           ') found in directory ('//trim(include_dirs(istat))//')'
        endif

        istat=0
        cycle
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'<HELPSECTION>')<    &
       card_len)then
        i_help=ior(i_help,2)
        cycle
      endif

      i_card=i_card+1
      cards(i_card)=card
!   print *,'i_card=...',i_card, card_len
!   if(card_len==0) read(5,*) i

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,'<HELP')<            &
       card_len) then
        card_type(i_card)=HELP_CARD_TYPE
        i_help=ior(i_help,1)
      else if(ezgui_check_chars(card_upper_case,x_pos,card_len,'<NS')<         &
         card_len .or.                                                         &
         ezgui_check_chars(card_upper_case,x_pos,card_len,'<SS')<card_len) then
        card_type(i_card)=NS_CARD_TYPE
        i_help=0
      else if(ezgui_check_chars(card_upper_case,x_pos,card_len,'<ENDHELP')<    &
         card_len.or.                                                          &
         ezgui_check_chars(card_upper_case,x_pos,card_len,'</HELPSECTION>')<   &
         card_len) then
        i_help=0
        card_type(i_card)=ENDHELP_CARD_TYPE
      else
        i=ezgui_check_chars(card_upper_case,x_pos,card_len,'</HELP>')
        if(i<card_len) then
          if(i>x_pos) then
            if(len_trim(card(1:i-1))>0) then
              cards(i_card)(i:card_len)=" "
              card_type(i_card)=HELPDATA_CARD_TYPE
              i_card=i_card+1
            endif
          endif
          card_type(i_card)=ENDHELP_CARD_TYPE
          cards(i_card)="<ENDHELP>"
          i_help=iand(i_help,2)
        else
          if(i_help>0) then
            card_type(i_card)=HELPDATA_CARD_TYPE
          else
            card_type(i_card)=LAYOUT_CARD_TYPE
          endif
        endif
      endif

      if(i_apos==0) then
        if(card_type(i_card)==LAYOUT_CARD_TYPE) then
!checking for ' until first one found-' allowed in Help data
          if(ezgui_find_chars(card,1,card_len,"'")<card_len) i_apos=1
        endif
      endif
    enddo
    return
  end subroutine ezgui_process_layout_input_card

!***************************************************************
! raeds input layout cards  and store into XML output array
!   Reads any needed include files
!
!     ************Internal EzGUI usage only***********
!
! Written July 1999 by Charles C Burch
! Made into separate subroutine Jan 2000 by Charles C Burch
!***************************************************************
  subroutine ezgui_process_layout_cards(cards, card_type, n_cards, i_apos,     &
     n_elems, row_col_elems)
    integer, intent(inout)            :: i_apos, n_cards
    character (len=*), intent(inout)  :: cards(:), card_type(:)
    integer, intent(inout)            :: n_elems
    type (row_col_elem)               :: row_col_elems(:)

    integer, parameter      :: HELP_SZ=200000

    integer                 :: str_len
    character (len=256)     :: card, card_upper_case, card1, keyword
    character (len=HELP_SZ) :: helps
    integer                 :: y_pos, x_pos, i_delim, i_label_delim, i_blank
    integer                 :: n_screen, i_elem, i_help, ibeg, iend, i, j, n
    integer                 :: i1, i2, isw
    integer                 :: i_array_delim, i_col_adj, n_screen_sv1
    integer                 :: card_len, i_card, n_labels, n_borders, n_dummy
    integer                 :: n_popup, n_toolbar, n_menubar, n_screen_sv
    character (len=1)       :: str0=char(0), a_type, field_type
    character (len=3)       :: help_display_type
    type (row_col_elem)     :: row_col_work
    logical                 :: tip_mode, text_mode, ba_sw, ta_sw, scroll_sw
    integer                 :: i_save,i_col_adj_save,i_beg,i_end,n_dash,n_tilde
    integer                 :: i_toggle_button, i_combo_box, i_combo_button

    n_cards=n_cards+1
    card_type(n_cards)=NOMOREDATA_CARD_TYPE

! ' found in layout-convert to ` if requested

    if(i_apos==1) then
      print *,                                                                 &
       "WARNING:'(single quote) found in layout, `(back tic) "//               &
       "might have been intended"
    endif

! cards now ready to be processed

    n_elems=0
    call ezgui_init_row_col_elem(row_col_work, -9)
    row_col_work%keyword="~PROCESS_HELP"
    row_col_work%type=HELP_PANEL_CARD_TYPE
    row_col_work%y_pos=-1
    row_col_work%length=0
    row_col_work%link_num=0
    call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)

    n_screen=6
    n_screen_sv=n_screen                !location of last screen completed
    window_width=1024
    window_height=768
    window_dialog=0
    x_pos=0
    y_pos=-1
    n_labels=1
    n_borders=1
    n_dummy=1
    n_popup=0
    n_toolbar=0
    n_menubar=0
    scroll_sw=.false.
    ta_sw=.false.                !indicates if <TA  found
    ba_sw=.false.                !indicates if <BA> found

    i_card=0
    write(*,'(a)') " Processing Card#"

    do while(i_card<n_cards)
      i_card=i_card+1
      call ezgui_print_card_num(i_card)

      if(card_type(i_card)==NOMOREDATA_CARD_TYPE) then
        if(n_screen/=n_screen_sv) then
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        endif

        if(scroll_sw) then                !turn off scroll if activated
          n_screen=n_screen+1
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~SCROLLABLE_SCREEN",i,                  &
           row_col_work%keyword)
          row_col_work%type=SCROLLABLE_SCREEN_CARD_TYPE
          row_col_work%y_pos=-1
          row_col_work%length=0
          row_col_work%link_num=1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          scroll_sw=.false.
        endif
        exit
      endif
      if(card_type(i_card)==ENDHELP_CARD_TYPE) cycle

      card=cards(i_card)
      card_len=len_trim(card)
      card_upper_case=card
      call ezgui_chars_upper_case(card_upper_case)
!   print *,card_type(i_card)//'|'//card(1:card_len)
      i_col_adj=1
      y_pos=y_pos+1
      if(card_len==0) cycle
      x_pos=ezgui_find_not_chars(card,1,card_len," ")

! Process <PARMS card if present

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<PARMS ")<          &
       card_len) then
        y_pos=y_pos-1
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")
        i_beg=ezgui_find_not_chars(card,x_pos+6,card_len,' ')
        i_end=ezgui_find_chars(card,i_beg,i_delim,'[')

        if(i_end>i_delim) then
          print '(/a)'," WARNING: No [ in PARMS card ("//trim(card)//          &
           ")-card skipped"
          cycle
        endif

        call ezgui_make_keyword(card_upper_case(i_beg:i_end-1),                &
         row_col_work%keyword)
        do i_elem=1,n_elems
          if(row_col_elems(i_elem)%keyword==row_col_work%keyword) exit
        enddo

        if(i_elem>n_elems) then
          if(ezgui_find_chars(card_upper_case,i_end,i_delim-1,"/NULL")         &
           <(i_delim-1)) then
            row_col_work%type="0"
            row_col_work%screen_num=n_screen
            row_col_work%length=1
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          else
            print '(/a)',                                                      &
             " WARNING: Unable to match keywords for PARMS card ("//           &
             trim(card)//")-card skipped"
          endif

          cycle
        endif

        call ezgui_process_layout_parms(card(i_end:i_delim-1),                 &
         row_col_elems(i_elem))
        cycle
      endif

! Process <WINDOW card, if present

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<WINDOW")<          &
       card_len) then
        window_dialog=0                            !window
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")
        call ezgui_get_layout_num_info(                                &
         card_upper_case(x_pos:i_delim-1), 'WIDTH=',window_width)
        call ezgui_get_layout_num_info(                                &
         card_upper_case(x_pos:i_delim-1), 'HEIGHT=',window_height)
        cycle
      endif

! Process <DIALOG card, if present

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<DIALOG")<          &
       card_len) then
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")
        window_dialog=1                                     !dialog, modal=false
        if(ezgui_find_chars(card_upper_case,x_pos,i_delim,"MODAL")<i_delim)    &
         window_dialog=2    !dialog modal=true
        call ezgui_get_layout_num_info(                                &
         card_upper_case(x_pos:i_delim-1), 'WIDTH=',window_width)
        call ezgui_get_layout_num_info(                                &
        card_upper_case(x_pos:i_delim-1),'HEIGHT=',window_height)
        cycle
      endif

! Process <ROW card if present  (<ROW=xx>,<ROW+=xx>, <ROW-=xx>)

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<ROW")<card_len) then
        call ezgui_process_row_card(card,x_pos,card_len,y_pos)
        cycle
      endif

! Process tool bar if present

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<TB")<card_len) then
        if(n_toolbar==1) print *,"WARNING: More than 1 toolbar menu"
        n_toolbar=n_toolbar+1
        y_pos=y_pos-1                      !this does not count in the row count

        call ezgui_init_row_col_elem(row_col_work, 0)
        row_col_work%type=TOOLBAR_CARD_TYPE
        row_col_work%y_pos=-2
        row_col_work%link_num=0
        row_col_work%keyword="~TOOLBAR"
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)

        i_card=i_card+1                      !Process button info for popup menu
        do while(i_card<n_cards)
          call ezgui_print_card_num(i_card)
          call ezgui_init_row_col_elem(row_col_work, 0)
          row_col_work%y_pos=-2          !indicate to skip constraint processing
          card=cards(i_card)
          card_len=len_trim(card)
          x_pos=ezgui_find_not_chars(card,1,card_len," ")
          card_upper_case=card
          call ezgui_chars_upper_case(card_upper_case)
          if(ezgui_check_chars(card_upper_case,x_pos,card_len,"</TB>")<        &
           card_len) exit

          i_delim=ezgui_find_chars(card,x_pos,card_len,"`")
          if(i_delim<card_len) then
            a_type=card_upper_case(i_delim+1:i_delim+1)
          else
            a_type='P'
          endif

          call ezgui_process_pushbutton_info(                                  &
           row_col_work,card,x_pos,i_delim-1,cards, i_card,i_col_adj,3,        &
           a_type,n_dummy)
          i_card=i_card+row_col_work%y_size
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        enddo
        cycle
      endif

! Process Menu bar if present

      if(card_upper_case(x_pos:x_pos+3)=="<MB ") then
        if(n_menubar==1) print '(/,a)',"WARNING: More than 1 Menubar"
        n_menubar=n_menubar+1
        y_pos=y_pos-1                        !this do not count in the row count

        do while(i_card<n_cards)
          if(card_upper_case(x_pos:x_pos+3)=="</MB") then
            exit
          else if(card_upper_case(x_pos:x_pos+3)=="<MB ") then
            call ezgui_init_row_col_elem(row_col_work, 0)
            row_col_work%type=MENUBAR_CARD_TYPE
            row_col_work%y_pos=-2
            row_col_work%link_num=0
            ibeg=ezgui_find_not_chars(card_upper_case, x_pos+3, card_len,' ')
            iend=ezgui_find_chars(card_upper_case,ibeg,card_len,'>')
            row_col_work%label=card(ibeg:iend-1)
            i=n_elems+1
            call ezgui_make_new_keyword("~MENU",i,row_col_work%keyword)
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
            i_card=i_card+1
          else
            call ezgui_init_row_col_elem(row_col_work, 0)
            row_col_work%y_pos=-2        !indicate to skip constraint processing
            i_delim=ezgui_find_chars(card,x_pos,card_len,"`")
            if(i_delim<card_len) then
              a_type=card_upper_case(i_delim+1:i_delim+1)
            else
              a_type='P'
            endif
            call ezgui_process_pushbutton_info(row_col_work,card,x_pos,        &
             i_delim-1,cards,i_card,i_col_adj,3,a_type,n_dummy)
            i_card=i_card+row_col_work%y_size
            call ezgui_find_unicode_value(                                     &
             row_col_work%label,1,row_col_work%length, 10,i,j)
            if(i<row_col_work%length) then
              print '(/,a)',                                                   &
               " WARNING: multiple line labels not allowed in menubar menus"
              row_col_work%y_size=1
              row_col_work%label(i:row_col_work%length)=" "
              row_col_work%length=i-1
              row_col_work%x_size=i-1
            endif
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
            call ezgui_print_card_num(i_card)
          endif

          card=cards(i_card)
          card_len=len_trim(card)
          x_pos=ezgui_find_not_chars(card,1,card_len," ")
          card_upper_case=card
          call ezgui_chars_upper_case(card_upper_case)
        enddo
        cycle
      endif

      !  Wipe out formally used keywords in <TA ..> and <BA ..>

      i_delim=ezgui_find_chars(card,x_pos,card_len,'>')
      if(i_delim<=card_len) then
        if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<TA ")<=card_len) &
         then
          print '(/,a)',                                                       &
           " WARNING:Top Area Component keyword invalid-keyword skipped"
          card_upper_case(x_pos:i_delim)='<TA>'
        endif
        if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<BA ")<=card_len) &
         then
          print '(/,a)',                                                       &
           " WARNING:Bottom Area Component keyword invalid-keyword skipped"
          card_upper_case(x_pos:i_delim)='<BA>'
        endif
      endif

!   TopAreaComponent-uses screen numbers 1,2,3

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<TA>")<=card_len)   &
       then
        if(ta_sw) then
          print '(/,a)'," WARNING: More than one <TA found"
        endif
        ta_sw=.true.
        y_pos=y_pos-1                        !this do not count in the row count

        if(n_screen/=n_screen_sv) then
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        endif

        call ezgui_init_row_col_elem(row_col_work, 1)
        row_col_work%type=TOP_AREA_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=0
        i=n_elems+1
        call ezgui_make_new_keyword("~TOP_AREA_COMPONENT", i,                  &
         row_col_work%keyword)
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        n_screen_sv1=n_screen
        n_screen=1
        n_screen_sv=n_screen
        cycle
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"</TA>")<=card_len)  &
       then
        y_pos=y_pos-1                        !this do not count in the row count
        if(n_screen==1) then
          print '(/,a)'," WARNING: <TA> used with no <NS entry"
        else
          if(n_screen>2) then
            print '(/,a)',                                                     &
             " WARNING: TopAreaComponent only supports one screen-"//          &
             "results will be bad"
          endif
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        endif

        call ezgui_init_row_col_elem(row_col_work, 3)
        row_col_work%type=TOP_AREA_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=1
        i=n_elems+1
        call ezgui_make_new_keyword("~END_TOP_AREA_COMPONENT",i,               &
         row_col_work%keyword)
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        n_screen=n_screen_sv1
        n_screen_sv=n_screen
        cycle
      endif

!   BottomAreaComponent
!     --uses screen numbers 4,5,6-that get changed to last screen later

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<BA>")<=card_len)   &
       then
        y_pos=y_pos-1                        !this do not count in the row count
        if(ba_sw) then
          print '(/,a)'," WARNING: More than one <BA found"
        endif

        if(n_screen/=n_screen_sv) then
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        endif

        ba_sw=.true.
        call ezgui_init_row_col_elem(row_col_work, 4)
        row_col_work%type=BOTTOM_AREA_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=0
        i=n_elems+1
        call ezgui_make_new_keyword("~BOTTOM_AREA_COMPONENT",i,                &
         row_col_work%keyword)
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        n_screen_sv1=n_screen
        n_screen=4
        n_screen_sv=n_screen
        cycle
      endif

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"</BA>")<=card_len)  &
       then
        y_pos=y_pos-1                        !this do not count in the row count
        if(n_screen==4) then
          print '(/,a)'," WARNING:<BA> used with no <NS> entry"
        else
          if(n_screen>5) then
            print '(/,a)',                                                     &
             " WARNING:Only one screen is allowed with BottomAreaComponent-"// &
             "results will be bad:"
          endif
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        endif

        call ezgui_init_row_col_elem(row_col_work, 6)
        row_col_work%type=BOTTOM_AREA_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=1
        i=n_elems+1
        call ezgui_make_new_keyword("~END_BOTTOM_AREA_COMPONENT",i,            &
         row_col_work%keyword)
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        n_screen=n_screen_sv1
        n_screen_sv=n_screen
        cycle
      endif

!   Help Panel

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<HP")<card_len) then
        y_pos=y_pos-1                        !this do not count in the row count
        x_pos=ezgui_find_not_chars(card,x_pos+3,card_len," ")
        i_delim=ezgui_find_chars(card,x_pos,card_len," ")
        call ezgui_init_row_col_elem(row_col_work, 0)
        row_col_work%type=HELP_PANEL_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=0
        row_col_work%keyword=card_upper_case(x_pos:i_delim-1)
        x_pos=ezgui_find_not_chars(card,i_delim, card_len," ")
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")

        if(i_delim<=card_len) then
          select case (card_upper_case(x_pos:i_delim-1))
          case ("TOP")
            row_col_work%label="top"
          case ("BOTTOM")
            row_col_work%label="bottom"
          case ("TOPMOST")
            row_col_work%label="topMost"
          case ("BOTTOMMOST")
            row_col_work%label="bottomMost"
          case default
            print '(/,a)',                                                     &
             " Invalid position("//card(x_pos:i_delim-1)//                     &
             ") in HP card-top used"
            row_col_work%label="top"
          end select
        else
          row_col_work%label="top"
        endif

        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        cycle
      endif

!   New Screen

      if(card_type(i_card)==NS_CARD_TYPE) then
        if(n_screen/=n_screen_sv) then
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          i=n_screen
          call ezgui_make_new_keyword("~END_SCREEN",i,row_col_work%keyword)
          row_col_work%type=END_SCREEN_CARD_TYPE
          row_col_work%y_pos=999
          row_col_work%length=0
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          n_screen_sv=n_screen
        endif

        if(card_upper_case(x_pos+1:x_pos+2)=="SS") then  !see if scrollable
          if(n_screen==1) then
            print '(/,a)',                                                     &
             " WARNING: Scrollable screens not allowed inTopAreaComponent"
          elseif(n_screen==4) then
            print '(/,a)',                                                     &
             " WARNING: Scrollable screens not allowed inBottomAreaComponent"
          else
            if(.not.scroll_sw) then           !put in scroll mode if not already
              n_screen=n_screen+1
              call ezgui_init_row_col_elem(row_col_work, n_screen)
              i=n_screen
              call ezgui_make_new_keyword("~SCROLLABLE_SCREEN",i,              &
               row_col_work%keyword)
              row_col_work%type=SCROLLABLE_SCREEN_CARD_TYPE
              row_col_work%y_pos=-1
              row_col_work%length=0
              row_col_work%link_num=0
              call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
              scroll_sw=.true.
            endif
          endif
        else
          if(scroll_sw) then !non-scrollable screen-turn off scroll if activated
            n_screen=n_screen+1
            call ezgui_init_row_col_elem(row_col_work, n_screen)
            i=n_screen
            call ezgui_make_new_keyword("~SCROLLABLE_SCREEN",i,                &
             row_col_work%keyword)
            row_col_work%type=SCROLLABLE_SCREEN_CARD_TYPE
            row_col_work%y_pos=-1
            row_col_work%length=0
            row_col_work%link_num=1
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
            scroll_sw=.false.
          endif
        endif

        n_screen=n_screen+1
        n_popup=0
        y_pos=-1
        x_pos=ezgui_find_not_chars(card,x_pos+3,card_len," ")
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")

        call ezgui_init_row_col_elem(row_col_work, n_screen)
        row_col_work%y_pos=-2
        row_col_work%x_size=0
        row_col_work%y_size=0
        row_col_work%cell_width=-1
        row_col_work%cell_height=-1
        row_col_work%type=NEW_SCREEN_CARD_TYPE
        ibeg=ezgui_find_chars(card,x_pos,i_delim,'/')

        if(ibeg>i_delim) then
          row_col_work%label=card(x_pos:i_delim-1)
          call ezgui_make_keyword(row_col_work%label,row_col_work%keyword)
        else
          row_col_work%label=card(x_pos:ibeg-1)
          if(ibeg>x_pos)                                                       &
           call ezgui_make_keyword(card(x_pos:ibeg-1),row_col_work%keyword)
          call ezgui_get_layout_num_info(                              &
           card_upper_case(ibeg:i_delim-1), '/NC=',row_col_work%x_size)
          call ezgui_get_layout_num_info(                              &
           card_upper_case(ibeg:i_delim-1), '/NR=',row_col_work%y_size)
          call ezgui_get_layout_num_info(                              &
           card_upper_case(ibeg:i_delim-1), '/CH=',row_col_work%cell_height)
          call ezgui_get_layout_num_info(                              &
           card_upper_case(ibeg:i_delim-1), '/CW=',row_col_work%cell_width)
        endif

        if(len_trim(row_col_work%keyword)==0) then
          i=n_screen
          call ezgui_make_new_keyword("SCREEN",i,row_col_work%keyword)
        endif

        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        cycle
      endif


!   Process help cards, if present

      if(card_type(i_card)==HELP_CARD_TYPE) then
!     print *,"processing help", x_pos, card_len,trim(card)
        i_help=0
        help_display_type=" "
        y_pos=y_pos-1
        ibeg=ezgui_find_chars(card_upper_case,x_pos,card_len,"<HELP")
        ibeg=ezgui_find_not_chars(card,ibeg+5,card_len," ")
        i_delim=ezgui_find_chars(card,ibeg,card_len,'>')

        i=ezgui_find_chars(card_upper_case,ibeg, i_delim,"TYPE=")
        if(i.lt.i_delim) then
          j=ezgui_find_not_chars(card_upper_case,i+5,i_delim," ")
          if(j.lt.i_delim) then
            if(card_upper_case(j:j+13)     .eq.'"DISPLAY_ONLY"') then
              help_display_type="no"
              card_upper_case(i:j+13)=" "
              card(i:j+13)=" "
            else if(card_upper_case(j:j+11).eq.'"USER_ENTRY"') then
              help_display_type="yes"
              card_upper_case(i:j+11)=""
            else
              print *,"WARNING: Invalid type specified in HELP card:"
              print *,"  "//card(ibeg:i_delim)
            endif
          endif
          ibeg=ezgui_find_not_chars(card,ibeg,card_len," ")
        endif

        i=min(ezgui_find_chars(card_upper_case,ibeg, i_delim,'KEYWORD="'),     &
         ezgui_find_chars(card_upper_case,ibeg,i_delim,'COMPONENT="'))
        if(i<i_delim) then

!handle XML help that has errors--kluggy logic

          j=ezgui_find_chars(card,i,i_delim,'"')
          i=ezgui_find_chars(card,j+1,i_delim,'"')
          row_col_work%keyword=card_upper_case(j+1:i-1)
!       print *,"kw=",row_col_work%keyword
          row_col_work%tip_info=" "
          tip_mode=.false.
          text_mode=.true.

          do while (i_card<n_cards)
            i_card=i_card+1
            call ezgui_print_card_num(i_card)
            if(card_type(i_card)/=HELPDATA_CARD_TYPE) exit
            card=cards(i_card)
            card_upper_case=card
            call ezgui_chars_upper_case(card_upper_case)
            str_len=len_trim(card)
!         print *,'help..',str_len,trim(card)
            if(str_len==0)str_len=1

            ibeg=1
            do while(ibeg<=str_len)
              iend=ezgui_find_chars(card_upper_case,ibeg,str_len,'<')
              do while (iend<str_len)
                if(card_upper_case(iend:iend+4)=="<TIP>"   .or.                &
                 card_upper_case(iend:iend+5)=="</TIP>"    .or.                &
                 card_upper_case(iend:iend+5)=="<TEXT>"    .or.                &
                 card_upper_case(iend:iend+6)=="</TEXT>"   .or.                &
                 card_upper_case(iend:iend+6)=="<TEXT/>"   .or.                &
                 card_upper_case(iend:iend+5)=="</HELP>"    ) exit
                iend=ezgui_find_chars(card_upper_case,iend+1,str_len,'<')
              enddo

              if(iend>ibeg) then                !see if there is data to process
                if(tip_mode) then
                  row_col_work%tip_info=                                       &
                   trim(row_col_work%tip_info)//card(ibeg:iend-1)
                else if(text_mode) then
                  if(i_help<HELP_SZ) then
                    n=iend-ibeg
                    if (i_help+n+1>HELP_SZ) then
                      print *,"Help array overflow-remaining help discarded"
                      print *, "  "//trim(card)
                      n=HELP_SZ-i_help-1
                    endif
                    helps(i_help+1:i_help+n+1)=card(ibeg:ibeg+n-1)//str0
                    i_help=i_help+n+1
                  endif
                endif
                ibeg=iend
                cycle
              endif

              if(ibeg>str_len) exit
              iend=ezgui_find_chars(card_upper_case,ibeg,str_len,'>')
!Have <..>-process it

              select case (card_upper_case(ibeg:iend))
              case ("<TIP>")
                tip_mode=.true.
                text_mode=.false.
              case ("</TIP>")
                tip_mode=.false.
                text_mode=.true.
              case ("<TEXT>")
                text_mode=.true.
                tip_mode=.false.
              case ("</TEXT>")
                text_mode=.false.
              case ("<TEXT/>")
                text_mode=.false.
              case ("</HELP>")
                card_type(i_card)=ENDHELP_CARD_TYPE
                i_card=i_card-1
                exit
              end select
              ibeg=iend+1
            enddo

          enddo

        else

!non XML version

          if(i_delim>ibeg) then
            row_col_work%keyword=card_upper_case(ibeg:i_delim-1)
          else
            row_col_work%keyword="~PROCESS_HELP"
          endif
          row_col_work%tip_info=card(i_delim+1:)
! ---   Save ending information as the tip

          do while(i_card<n_cards)                 !gather up any help info
            i_card=i_card+1
            if(card_type(i_card)/=HELPDATA_CARD_TYPE) exit

            call ezgui_print_card_num(i_card)
            str_len=len_trim(cards(i_card))
            if(i_help<HELP_SZ) then
              if (i_help+str_len+1>HELP_SZ) then
                print *,"Help array overflow-remaining help discarded"
                print *, "  "//trim(card)
                str_len=HELP_SZ-i_help-1
              endif
              helps(i_help+1:i_help+str_len+1)=cards(i_card)(:str_len)//str0
              i_help=i_help+str_len+1
            endif
          enddo
        endif

!     find keyword in row_elems, if present

        i_elem=0
        do i=1, n_elems
          if(row_col_work%keyword==row_col_elems(i)%keyword)  then
            i_elem=i
            exit
          endif
        enddo

        if(i_elem==0) then
          print '(/,a)'," Unable to find help keyword("//                      &
           trim(row_col_work%keyword)//'-entry added)'
          row_col_work%type="0"
          row_col_work%screen_num=n_screen
          row_col_work%length=1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          i_elem=n_elems
        endif

        call ezgui_convert_str_to_unicode(row_col_work%tip_info,            &
           row_col_elems(i_elem)%tip_info, i)

        if(i_help>0) then
          allocate ( row_col_elems(i_elem)%help_info(1:i_help), stat=i )
          if(i/=0) then
            print *,                                                           &
             "Unable to allocate help storage(size=",i_help,"), i_elem=",i_elem
          else
            do i=1, i_help
              row_col_elems(i_elem)%help_info(i)=helps(i:i)
            enddo
          endif
        endif
        row_col_elems(i_elem)%help_size=i_help
        if(help_display_type.ne." ")                                           &
         row_col_elems(i_elem)%editable=help_display_type

        i_card=i_card-1
        cycle
      endif

      if(n_screen==6 .or. n_screen==1 .or. n_screen==4) then
! - Insert new screen if first card is not one
        if(len_trim(card)==0) cycle    !skip inserting <NS> if card is all blank
        print '(/,a)'," WARNING: <Missing <NS..> card, one is assumed"
        n_screen_sv=n_screen
        n_screen=n_screen+1
        n_popup=0
        call ezgui_init_row_col_elem(row_col_work, n_screen)
        row_col_work%type=NEW_SCREEN_CARD_TYPE
        row_col_work%keyword="SCREEN01"
        row_col_work%label=row_col_work%keyword
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
      endif

!  Process popup menu information if any

      if(ezgui_check_chars(card_upper_case,x_pos,card_len,"<PM")<card_len) then
        if(n_popup==1)                                                         &
         print '(/,a,i3,a)',                                                   &
         " WARNING: More than 1 popup menu for screen(",n_screen,")"
        n_popup=n_popup+1
        y_pos=y_pos-1                     !this does not count in the row count

        x_pos=ezgui_find_not_chars(card,x_pos+3,card_len," ")
        i_delim=ezgui_find_chars(card,x_pos,card_len,">")
        call ezgui_init_row_col_elem(row_col_work, n_screen)
        row_col_work%type=POPUP_MENU_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=0
        row_col_work%label=card(x_pos:i_delim-1)
        call ezgui_make_keyword(row_col_work%label,row_col_work%keyword)

        if(len_trim(row_col_work%keyword)==0) then
          print '(/,a)'," Warning: DUMMY keyword formed for blank menu keyword"
          call ezgui_make_new_keyword("DUMMY",n_dummy,row_col_work%keyword)
        endif
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)

        i_card=i_card+1                     !Process button info for popup menu
        do while(i_card<n_cards)
          call ezgui_print_card_num(i_card)
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          row_col_work%y_pos=-1          !indicate to skip constraint processing
          card=cards(i_card)
          card_len=len_trim(card)
          x_pos=ezgui_find_not_chars(card,1,card_len," ")
          card_upper_case=card
          call ezgui_chars_upper_case(card_upper_case)
          if(ezgui_check_chars(card_upper_case,x_pos,card_len,"</PM>")<        &
           card_len) exit

          i_delim=ezgui_find_chars(card,x_pos,card_len,"`")
          if(i_delim<card_len) then
            a_type=card_upper_case(i_delim+1:i_delim+1)
          else
            a_type='P'
          endif
          call ezgui_process_pushbutton_info(row_col_work,card,x_pos,          &
           i_delim-1,cards, i_card,i_col_adj,1,a_type,n_dummy)
          i_card=i_card+row_col_work%y_size

          call ezgui_find_unicode_value(                                       &
           row_col_work%label,1,row_col_work%length,10,i,j)
          if(i<row_col_work%length) then
            print '(/,a)',                                                     &
             " WARNING: multiple line labels not allowed in popup menus"
            row_col_work%y_size=1
            row_col_work%label(i:row_col_work%length)=" "
            row_col_work%length=i-1
            row_col_work%x_size=i-1
          endif
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        enddo

        call ezgui_init_row_col_elem(row_col_work, n_screen)
        row_col_work%type=POPUP_MENU_CARD_TYPE
        row_col_work%y_pos=-1
        row_col_work%link_num=1
        row_col_work%label=card(x_pos:i_delim-1)
        i=n_elems+1
        call ezgui_make_new_keyword("END_POPUP_MENU",i,row_col_work%keyword)
        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
        cycle
      endif

! ******************NOW EXPECTING LAYOUT CARDS**************************

      if(card_type(i_card)/=LAYOUT_CARD_TYPE) cycle

! layout card, process the line, one field at a time

!First see if any arrays
      if(card_type(i_card+1)==LAYOUT_CARD_TYPE) then
        ibeg=x_pos
        iend=len_trim(cards(i_card+1))
        do while (ibeg<iend)
          ibeg=ezgui_find_not_chars(cards(i_card+1),ibeg,iend," ")
          if(ibeg>iend) exit
          i_delim=ezgui_find_chars(cards(i_card+1),ibeg,iend,"`")
          if(i_delim>iend) exit

          i_blank=ezgui_find_chars(cards(i_card+1),ibeg,iend,"  ")
          if(i_delim>ibeg) then
            if(ibeg<iend) then
              ibeg=i_blank
              cycle
            else
              exit
            endif
          endif

          a_type=cards(i_card+1)(i_delim+1:i_delim+1)
          call ezgui_chars_upper_case(a_type)
          if(a_type=="-" .or. a_type=='R' .or. a_type=='P') then
! ---   skip, if it is part of a border or buttons
            ibeg=i_blank
            cycle
          endif

          call ezgui_init_row_col_elem(row_col_work, n_screen)
          row_col_work%y_stretch="false"
          row_col_work%y_pos=y_pos
          row_col_work%x_pos=i_delim-i_col_adj
          row_col_work%x_size=i_blank-i_delim
          i_apos=ezgui_find_chars(cards(i_card+1),i_delim+1,iend,'`')
          do i=i_card+2,n_cards                                   !find y_size
            if(card_type(i)/=LAYOUT_CARD_TYPE) exit
            if(cards(i)(i_delim-1:i_delim+1)/=                                 &
             cards(i_card+1)(i_delim-1:i_delim+1))  exit
            cards(i)(i_delim:i_blank-1)=' '!wipe out array definition
          enddo
          row_col_work%y_size=i-i_card

! - i_delim is start of array,
! - i_apos location of next `,
! - i_blank location of next double blank
          if(i_blank<=i_apos) then

!single array

!         print *,"processing single array"
            select case(a_type)
            case ("X")
              a_type="S"
              row_col_work%editable="no"
            case("A")
              a_type="S"
            case ("I","F","S")
            case default
              print '(/,a)',                                                   &
               " WARNING: Invalid array element type("//a_type//               &
               ")-converted to string"
              a_type="S"
            end select
            row_col_work%type=a_type

            call ezgui_get_string_char_count(                                  &
             cards(i_card+1)(i_delim:i_blank-1),"-", n_dash)
            call ezgui_get_string_char_count(                                  &
             cards(i_card+1)(i_delim:i_blank-1),"~", n_tilde)

! --- wipe our array definition information
            cards(i_card+1)(i_delim:i_blank-1)=' '

            row_col_work%length=i_blank-i_delim-n_dash-n_tilde
            if(n_tilde==0) n_tilde=3
            row_col_work%x_size=row_col_work%length+n_tilde
            call ezgui_process_layout_label(                                   &
             card(i_delim:i_blank-1),row_col_work,i)
            card(i_delim:i_blank-1)=" "   !wipe out array definition information
            i_col_adj=i_col_adj+min(i,n_dash)
            row_col_work%link_num=1
            if(len_trim(row_col_work%keyword)==0) then
              print '(/,a)'," Warning: DUMMY keyword formed for blank keyword"
              call ezgui_make_new_keyword("DUMMY",n_dummy,row_col_work%keyword)
            endif
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
            ibeg=i_blank                               !get ready for next field

          else

! --- linked array

!         print *,"Processing linked array"
            row_col_work%type="A"                 !save array link variable info
            row_col_work%link_num=0

            call ezgui_get_string_char_count(                                  &
             cards(i_card+1)(i_delim:i_blank),"-",  n_dash)
            call ezgui_get_string_char_count(                                  &
             cards(i_card+1)(i_delim:i_blank),"~", n_tilde)

            row_col_work%label=card(i_delim:i_blank-1)
            row_col_work%length=i_blank-i_delim-n_dash-n_tilde
            if(n_tilde==0) n_tilde=3
            row_col_work%x_size=row_col_work%length+n_tilde
            call ezgui_process_layout_label(                                   &
             card(i_delim:i_blank-1),row_col_work,i)
            i=n_elems+1
            call ezgui_make_new_keyword("~TO_BE_FILLED_IN_LATER",i,            &
             row_col_work%keyword)
            call ezgui_condense_label(row_col_work%label, i)
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
            i_elem=n_elems

            row_col_work%link_num=2
            do while(i_delim<i_blank)          !process elements of linked array
              row_col_work%x_pos=i_delim-i_col_adj
              row_col_work%editable="yes"
              a_type=cards(i_card+1)(i_delim+1:i_delim+1)

              call ezgui_chars_upper_case(a_type)
              select case(a_type)
              case ("X")
                a_type="S"
                row_col_work%editable="no"
              case("A")
                a_type="S"
              case ("I","F","S","O")
              case default
                print '(/,a)',                                                 &
                 " WARNING Invalid array element type("//a_type,               &
                 ")-converted to string"
                a_type="S"
              end select
              row_col_work%type=a_type

              i_array_delim=                                                   &
               ezgui_find_chars(cards(i_card+1),i_delim+1, i_blank-1, '`')
              call ezgui_get_string_char_count(                                &
               cards(i_card+1)(i_delim:i_array_delim), "-",  n_dash)
              call ezgui_get_string_char_count(                                &
               cards(i_card+1)(i_delim:i_array_delim), "~", n_tilde)
              cards(i_card+1)(i_delim:i_array_delim-1)=' '
! --- wipe out array definition information

              row_col_work%length=i_array_delim-i_delim-n_dash-n_tilde
              row_col_work%x_size=row_col_work%length
              call ezgui_process_layout_label(card(i_delim:i_array_delim-1),   &
               row_col_work,i)
              card(i_delim:i_array_delim-1)=" "!wipe out array definition info
              i_col_adj=i_col_adj+min(i,n_dash)
              if(len_trim(row_col_work%keyword)==0) then
                print '(/,a)'," Warning: DUMMY keyword formed for blank keyword"
                call ezgui_make_new_keyword("DUMMY",n_dummy,                   &
                 row_col_work%keyword)
              endif
              call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
              i_delim=i_array_delim       !get ready for next array link element
            enddo

            row_col_elems(i_elem)%keyword=                                     &
             trim(row_col_elems(i_elem+1)%keyword)//"_ARRAYSET"
            ibeg=i_blank                               !get ready for next field
          endif
        enddo
      endif

! process the non array fields
      do while (x_pos<=card_len)
        x_pos=ezgui_find_not_chars(card, x_pos, card_len," ")
        if(x_pos>card_len) exit

        i_delim=ezgui_find_chars(card, x_pos,card_len,"`")
        i_label_delim=ezgui_find_chars(card,x_pos,card_len,"  ")

        if(i_label_delim<=i_delim) then  ! terminator is 2 blanks-treat as label
          call ezgui_init_row_col_elem(row_col_work, n_screen)   !we got a label
          row_col_work%y_stretch="false"
          row_col_work%x_pos=x_pos - i_col_adj
          row_col_work%y_pos=y_pos
          row_col_work%type="L"
          row_col_work%alignment="center"

          call ezgui_process_layout_label(                                     &
           card(x_pos:i_label_delim-1),row_col_work,i)
          i_col_adj=i_col_adj+i
          row_col_work%x_size=i_label_delim-x_pos-i
          row_col_work%length=row_col_work%x_size

! --- see if keyword specified or need to create one

          i=ezgui_find_chars(card,x_pos,i_label_delim,"[")
          if(i.lt.i_label_delim) then
            i=ezgui_find_not_chars(card,i,i_label_delim," ")
            if(i.lt.i_label_delim) then
              if(card(i+1:i+1).eq.'/'.or.card(i+1:i+1).eq.']') i=i_label_delim
            endif
          endif

          if(i.ge.i_label_delim) then
            call ezgui_make_keyword(row_col_work%label,row_col_work%keyword)
            row_col_work%keyword="COMMENT_"//row_col_work%keyword
            do i_elem=1, n_elems
              if(row_col_work%keyword==row_col_elems(i_elem)%keyword) then
                call ezgui_make_new_keyword("EZGUI_COMMENT_",n_labels,         &
                 row_col_work%keyword)
                exit
              endif
            enddo
          endif

          row_col_work%editable='no'
          row_col_work%link_num=-1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          x_pos=i_label_delim
          cycle
        endif

!  Have a ` :  Looks promising as a layout element

        i_blank=ezgui_find_chars(card,i_delim,card_len," ")
        a_type=card(i_delim+1:i_delim+1)
        call ezgui_chars_upper_case(a_type)
        call ezgui_init_row_col_elem(row_col_work, n_screen)
        row_col_work%x_pos=x_pos-i_col_adj
        row_col_work%y_pos=y_pos

!     handle if border

        if(a_type=="-") then
! ---   print *,"processing border",n_borders, i_delim, x_pos, i_blank
          row_col_work%type="B"                              !border
          call ezgui_make_new_keyword("BORDER",n_borders, row_col_work%keyword)

          i1=i_delim+2                  !get the border label if present
          if(card(i1:i1).eq.'-') then
            row_col_work%label=""
          else
            i2=min(ezgui_find_chars(card,i1,card_len,"--"),                    &
             ezgui_find_chars(card,i1,card_len,"- ") )
            if(i2.ge.i_blank) then
              i_blank=ezgui_find_chars(card,i_delim,card_len,"  ")
              i2=min(ezgui_find_chars(card,i1,i_blank,"--"),                   &
               ezgui_find_chars(card,i1,i_blank,"- "), i_blank )
            endif
            row_col_work%label=card(i1:i2-1)
            call ezgui_condense_label(row_col_work%label,i1)
          endif

! --- determine size of box-wipeout border def. in cards

          iend=y_pos
          do i=i_card+1,n_cards
            iend=iend+1
            if(cards(i)(i_delim:i_delim+1)=="`-".or.                           &
             card_type(i)/=LAYOUT_CARD_TYPE) then       !end of border found
              if(card_type(i)==LAYOUT_CARD_TYPE) cards(i)(i_delim:i_blank-1)=" "
              exit
            endif

            if(cards(i)(i_delim:i_delim)=='|') cards(i)(i_delim:i_delim)=' '
            i2=len_trim(cards(i))              !check for <ROW or <PARMS
            i1=ezgui_find_chars(cards(i),1,i2,"<")
            if(i1.lt.i2) then
              card1=cards(i)
              call ezgui_chars_upper_case(card1)
              if(ezgui_check_chars(card1,i1,i2,"<ROW")<i2) then
                call ezgui_process_row_card(card1,i1,i2,iend)  !handle ROW cards
              else if(ezgui_check_chars(card1,i1,i2,"<PARMS ")<i2) then
                iend=iend-1                              !skip PARMS cards
              endif
            endif

          enddo

          row_col_work%x_size=i_blank-x_pos
          row_col_work%y_size=iend-y_pos+1
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)

          x_pos=i_blank
          cycle
        endif   !end of border

!     handle if togglebutton or Kombobutton

        if(a_type=="T" .or. a_type=="K") then
          if(card(x_pos:x_pos)/='`'.and.card(x_pos:x_pos)/='[') then
            i=ezgui_find_chars(card, x_pos,i_delim-1,'[')
            call ezgui_init_row_col_elem(row_col_work, n_screen) !we got a label
            row_col_work%y_stretch="false"
            row_col_work%x_pos=x_pos - i_col_adj
            row_col_work%y_pos=y_pos
            row_col_work%type="L"
            row_col_work%alignment="left"
            row_col_work%x_size=i-x_pos
            row_col_work%length=row_col_work%x_size
            row_col_work%label=card(x_pos:i-1)
            call ezgui_condense_label(row_col_work%label, i)

            call ezgui_make_keyword(row_col_work%label,row_col_work%keyword)
            row_col_work%keyword="COMMENT_"//row_col_work%keyword
            do i_elem=1, n_elems
              if(row_col_work%keyword==row_col_elems(i_elem)%keyword) then
                call ezgui_make_new_keyword("EZGUI_COMMENT_",n_labels,         &
                 row_col_work%keyword)
                exit
              endif
            enddo

            row_col_work%editable='no'
            row_col_work%link_num=-1
            call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          endif

          if(a_type=="T") then
            call ezgui_process_togglebutton_info(                              &
             row_col_work, card(x_pos:i_blank-1), i_col_adj, -1, n_screen,     &
             i_delim, y_pos)
            if(len_trim(row_col_work%keyword)==0) then
              call ezgui_form_new_keyword("EZGUI_TOGGLE_BUTTON",n_dummy,       &
               n_elems, row_col_elems, row_col_work%keyword)
            endif
          else if(a_type=="K") then
            call ezgui_process_combobutton_info(                               &
             row_col_work, card(x_pos:i_blank-1), i_col_adj, -1, n_screen,     &
             i_delim, y_pos)

            if(len_trim(row_col_work%keyword)==0) then
              call ezgui_form_new_keyword("EZGUI_COMBO_BUTTON",n_dummy,       &
               n_elems, row_col_elems, row_col_work%keyword)
            endif
          endif

          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          x_pos=i_blank
          cycle
        endif

!     handle if pushbutton

        if(a_type=="P".or.a_type=='D') then
          call ezgui_process_pushbutton_info(row_col_work,card,x_pos,i_delim-1,&
           cards,i_card,i_col_adj,1,a_type, n_dummy)
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
          x_pos=i_blank
          cycle
        endif    !end of pushbutton

!handle if radiobutton: `R& means radio button definition continues on next card

        if(a_type=="R") then
          i_blank=min(ezgui_find_chars(                                        &
           card_upper_case,x_pos,card_len,"`R& ")+3,                           &
           ezgui_find_chars(card_upper_case,x_pos,card_len,"`R ")+2)
          row_col_work%type="R"                              !RadioButton
          row_col_work%link_num=0
          row_col_work%label=card(x_pos:i_blank-1)
          n=len_trim(row_col_work%label)

          i=1                                     !forming name for radio button
          do while(i<n)
            if(row_col_work%label(i:i+1)=='`R'.or.                             &
             row_col_work%label(i:i+1)=='`r') then
              row_col_work%label(i:n)=row_col_work%label(i+2:n)
              n=n-2
            else
              i=i+1
            endif
          enddo

          call ezgui_make_keyword(row_col_work%label,row_col_work%keyword)
          call ezgui_condense_label(row_col_work%label, i)
          if(len_trim(row_col_work%keyword)==0) then
            print '(/,a)',                                                     &
             " Warning: DUMMY keyword formed for RadioButton blank keyword"
            call ezgui_make_new_keyword("DUMMY",n_dummy,row_col_work%keyword)
          endif
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )

          i_save=i_col_adj   !Now process the cards that define the radio button
          do i=i_card, n_cards
            card1=cards(i)
            call ezgui_chars_upper_case(card1)
            n=len_trim(card1)+1
            i_beg=ezgui_find_not_chars(card1,x_pos,n,' ')
            i_end=min(ezgui_find_chars(card1,x_pos,n,"`R& ")+3,                &
             ezgui_find_chars(card1,x_pos,n,"`R ") +2)
            i_col_adj=i_save

            do while(i_beg<i_end) !Process the Radio pushbuttons on current card
              i_delim=ezgui_find_chars(card1,i_beg, i_end, '`R')
              if(i_delim>i_end) exit

              if(i_delim>i_beg) then
                row_col_work%x_pos=i_beg-i_col_adj
                call ezgui_process_pushbutton_info(row_col_work,cards(i),i_beg,&
                 i_delim-1, cards,i,i_col_adj,3,a_type,n_dummy)
                call ezgui_save_row_col_elem(row_col_elems, n_elems,           &
                 row_col_work)
              endif
              i_beg=i_delim+2
            enddo

            if(i==i_card) i_col_adj_save=i_col_adj
            if(cards(i)(i_end-1:i_end-1)=='&') then
! ---n see if & present to add buttons on next card
              cards(i)(x_pos:i_end-1)=' '
              row_col_work%y_pos=row_col_work%y_pos+1
            else
              cards(i)(x_pos:i_end-1)=' '  !No &, so done with this radio button
              exit
            endif
          enddo

          x_pos=i_blank                             !get ready for next field
          i_col_adj=i_col_adj_save
          cycle
        endif   !end of radio button

! --- Prompt edit field

        if(a_type.eq."Q") then
          field_type="Q"                            !special Q type
        else
          field_type=" "
        endif
        i_end=i_delim-1                             !end of label part

        row_col_work%y_stretch="false"
        row_col_work%x_stretch="false"
        row_col_work%alignment="left"
        row_col_work%type="L"
        row_col_work%editable='no'

        isw=0                                   !indicates if label or not
        if(i_delim>x_pos) then
          call ezgui_process_layout_label(card(x_pos:i_end), row_col_work,i)
          i_col_adj=i_col_adj+i
          row_col_work%x_size=i_delim-x_pos-i
          row_col_work%length=row_col_work%x_size

          if(row_col_work%x_size>0) then
            if(row_col_work%keyword.eq." ") then       !handle label part
              call ezgui_make_new_keyword("EZGUI_LABEL_",n_labels,             &
               row_col_work%keyword)
            else if(field_type.eq."Q") then
              keyword=row_col_work%keyword   !save keyword for EQLAB_ keyword
              row_col_work%keyword="SELECT_"//row_col_work%keyword
            else
              row_col_work%keyword="LABEL_"//row_col_work%keyword
            endif
            isw=1                               !yes, there is a label
          endif
        endif

        if(field_type.eq."Q".and.isw.eq.0) then  ! Q fields must have a label
          print '(/,a)'," WARNING: required label missing, type=("//a_type     &
           //")-converted to a string"
          a_type="S"
          field_type=" "
        endif

        if(isw.ne.0) then                  !process label, if present
          if(field_type.eq."Q") then
! -- query field-make label a pushbutton, then add "=" label
            row_col_work%link_num=1
            row_col_work%type='P'
            row_col_work%x_stretch="false"
            row_col_work%y_stretch="true"
            row_col_work%y_size=1
            call ezgui_save_row_col_elem(row_col_elems,n_elems, row_col_work)

! --------- form "=" label field
            call ezgui_init_row_col_elem(row_col_work, n_screen)
            row_col_work%keyword="EQLAB_"//keyword
            row_col_work%y_stretch="false"
            row_col_work%x_stretch="false"
            row_col_work%alignment="left"
            row_col_work%type="L"
            row_col_work%x_pos=i_delim-i_col_adj
            row_col_work%y_pos=y_pos
            row_col_work%label="="
            row_col_work%length=1
            row_col_work%x_size=1

            a_type=card(i_delim+2:i_delim+2)  !get actual edit field data type
            i_delim=i_delim+1
          endif

          row_col_work%editable="no"          !field is a label
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
        endif

! --- now handle the actual edit field

        call ezgui_init_row_col_elem(row_col_work, n_screen)
        select case(a_type)
        case ("X")
          a_type="S"
          row_col_work%editable="no"
        case("A")
          a_type="S"
        case ("I","F","S","C")
        case default
          print '(/,a)'," WARNING: type=("//a_type//")-converted to a string"
          a_type="S"
        end select

        i_toggle_button= &
         ezgui_find_chars(card_upper_case, i_delim+1, i_blank-1, "`T")
        i_combo_box=     &
         ezgui_find_chars(card_upper_case, i_delim+1, i_blank-1, "`C")
        i_combo_button=  &
         ezgui_find_chars(card_upper_case, i_delim+1, i_blank-1, "`K")

        n=min(i_toggle_button,i_combo_button, i_combo_box)
        call ezgui_process_edit_field_info(row_col_work, card(i_delim:n-1),    &
         i_delim, y_pos, i_col_adj, -1, a_type)

        call ezgui_process_layout_label(card(x_pos:i_end), row_col_work,i)
        if(len_trim(row_col_work%keyword)==0) then
          print '(/,a)'," Warning: DUMMY keyword formed for blank keyword"
          call ezgui_make_new_keyword("DUMMY",n_dummy,row_col_work%keyword)
        endif

        call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
        keyword=row_col_work%keyword

        if(i_toggle_button<i_blank) then      !add toggle if specified
          call ezgui_process_togglebutton_info(                                &
           row_col_work, card(i_toggle_button:i_blank-1),                      &
           i_col_adj, -1, n_screen,i_toggle_button, y_pos)
          row_col_work%keyword="FLAG_"//keyword
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
        endif

        if(i_combo_button<i_blank) then      !add toggle if specified
          call ezgui_process_combobutton_info(                                 &
           row_col_work, card(i_combo_button:i_blank-1),                       &
           i_col_adj, -1, n_screen,i_combo_button, y_pos)
          row_col_work%keyword="FLAG_"//keyword
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
        endif

        if(i_combo_box<i_blank) then       !add combo if specified
          call ezgui_init_row_col_elem(row_col_work, n_screen)
          call ezgui_process_edit_field_info(                                  &
           row_col_work, card(i_combo_box:i_blank-1),                          &
           i_combo_box, y_pos, i_col_adj, -1, "C")
          row_col_work%keyword="FLAG_"//keyword
          call ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work )
        endif
!
! --- get ready for next field
        x_pos=i_blank
      end do                                         !next column field
    enddo                                            !next card

    print '(/,a)'," "
    if(ba_sw) then          !change ba screen # to end so it gets put at the end
      do i_elem=1,n_elems
        select case (row_col_elems(i_elem)%screen_num)
        case (4)
          row_col_elems(i_elem)%screen_num=n_screen+1
        case (5)
          row_col_elems(i_elem)%screen_num=n_screen+2
        case (6)
          row_col_elems(i_elem)%screen_num=n_screen+3
        case default
        end select
      enddo
    endif

    do i_elem=1, n_elems-1
      do i=i_elem+1, n_elems
        if(row_col_elems(i_elem)%keyword==row_col_elems(i)%keyword) then
          print *,                                                             &
           "WARNING: Duplicate keyword ("//                                    &
           trim(row_col_elems(i_elem)%keyword)//") used"
          exit
        endif
      enddo
    enddo

!Check for missing help/tip and missing Default/Allowed, if requested

    if(check_help_sw.ne.0) then
      do i_elem=1, n_elems
        select case(row_col_elems(i_elem)%type)
        case("I","S","F","O","C","T","K")
          i=1
        case("R","P","D")
          i=2
        case default
          i=0
        end select

        if(i.ne.0) then

          if(row_col_elems(i_elem)%editable.eq."yes") then
            if(len_trim(row_col_elems(i_elem)%tip_info)==0) then
              print *,"WARNING: Tip missing for editable keyword ("//          &
               trim(row_col_elems(i_elem)%keyword)//")"
            endif

            if(row_col_elems(i_elem)%help_size==0) then
              print *,"WARNING: HELP missing for editable keyword ("//         &
               trim(row_col_elems(i_elem)%keyword)//")"

            else if(i.eq.1) then
              n=row_col_elems(i_elem)%help_size
              do i=1,n
                helps(i:i)=row_col_elems(i_elem)%help_info(i)
              enddo

              call ezgui_chars_upper_case(helps(1:n))
              if(index(helps(1:n),"DEFAULT").eq.0) then
                print *,                                                       &
                 'WARNING: "Default" missing in HELP for editable keyword ('// &
                 trim(row_col_elems(i_elem)%keyword)//")"
              endif

              if(index(helps(1:n),"ALLOWED").eq.0) then
                print *,                                                       &
                 'WARNING: "Allowed" missing in HELP for editable keyword ('// &
                 trim(row_col_elems(i_elem)%keyword)//")"

              endif
            endif

          else                     !check for help or tip in display_only fields
            if(len_trim(row_col_elems(i_elem)%tip_info)==0) then
              print *,                                                         &
               "WARNING: No tip specified for display-only keyword ("//        &
               trim(row_col_elems(i_elem)%keyword)//")"
            endif

          endif
        endif

      enddo
    endif

    call ezgui_sort_row_col_elems(row_col_elems, n_elems)
    return
  end subroutine ezgui_process_layout_cards

!***********************************************************************
! Process row card
!
! Changed to a subroutine June 2001 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_row_card(card,x_pos,card_len,y_pos)
    character (len=*),intent(in)    :: card
    integer          ,intent(in)    :: x_pos, card_len
    integer          ,intent(inout) :: y_pos

    integer                         :: ibeg, iend, i_delim, y_pos_save, n

    y_pos_save=y_pos

    i_delim=ezgui_find_chars(card,x_pos,card_len,">")
    ibeg=x_pos+4
    iend=y_pos

    if(card(ibeg:ibeg).eq."=") then
      call ezgui_get_layout_num_info(card(x_pos:i_delim-1), '<ROW=',n)
      y_pos=n-1
    else if(card(ibeg:ibeg+1).eq."+=") then
      call ezgui_get_layout_num_info(card(x_pos:i_delim-1), '<ROW+=',n)
      y_pos=y_pos+n-1
    else if(card(ibeg:ibeg+1).eq."-=") then
      call ezgui_get_layout_num_info(card(x_pos:i_delim-1), '<ROW-=',n)
      y_pos=y_pos-n-1
    else
      print '(/,a)'," WARNING:Invalid <Row..> card, card skipped"
      print *,'  '//trim(card)
    endif

    if(y_pos<-1) then
      print '(/,a)'," WARNING: <Row..> card("//trim(card)//                    &
       " caused invalid y_pos, card skipped"
      y_pos=y_pos_save-1
    endif

    return
  end subroutine ezgui_process_row_card

!***********************************************************************
! Extract numeric info from associated string
!
! Written December 1999 by Charles C Burch
!***********************************************************************
  subroutine ezgui_get_layout_num_info(str,srch,result)
    character (len=*), intent(in)     :: str, srch
    integer, intent(out)              :: result

    integer                           :: i, n, i1

    n=len(str)
    i=ezgui_find_chars(str,1,n,srch)
    if(i<n) then
      i=i+len(srch)
      i1=ezgui_find_chars(str,i,n,'/')
      result=ezgui_chars_to_int(str(i:i1-1),n)
    endif
    return
  end subroutine ezgui_get_layout_num_info

!***********************************************************************
! Process edit field information
!
! Written September 2001 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_edit_field_info(row_col_work, str, x_pos, y_pos,    &
     i_col_adj, link_num, a_type)
    type (row_col_elem), intent(inout) :: row_col_work
    integer, intent(in)                :: link_num, x_pos, y_pos
    integer, intent(inout)             :: i_col_adj
    character (len=*), intent(in)      :: a_type, str

    integer                            :: n, n_str

    n_str=len(str)

    if(a_type.eq."C") then
      row_col_work%y_stretch="true"
    else
      row_col_work%y_stretch="false"
    endif

    row_col_work%x_stretch="false"
    row_col_work%type=a_type
    row_col_work%x_pos=x_pos-i_col_adj
    row_col_work%y_pos=y_pos
    row_col_work%link_num=link_num

    call ezgui_get_string_char_count(str, '-',  n)
    row_col_work%length=n_str-n
    i_col_adj=i_col_adj+n
    row_col_work%x_size=row_col_work%length

    return
  end subroutine ezgui_process_edit_field_info

!***********************************************************************
! Process pushbutton information
!
! Written November 1999 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_pushbutton_info(row_col_work, card, i_beg, i_end,   &
     cards, i_card, i_col_adj, link_num, a_type, n_dummy)
    type (row_col_elem), intent(inout) :: row_col_work
    integer, intent(in)                :: i_beg, i_end, i_card, link_num
    integer, intent(inout)             :: i_col_adj, n_dummy
    character (len=*), intent(inout)   :: cards(:), card, a_type

    integer                            :: i

    row_col_work%link_num=link_num
    row_col_work%type='P'
    row_col_work%x_stretch="false"
    row_col_work%y_stretch="true"
    row_col_work%y_size=1
    if(a_type=='D') row_col_work%y_size=2

    call ezgui_process_layout_label(card(i_beg:i_end),row_col_work,i)
    i_col_adj=i_col_adj+i
    row_col_work%length=i_end-i_beg+1-i
    row_col_work%x_size=row_col_work%length

    if(row_col_work%y_size==2) then                        !2 line label
      call ezgui_process_layout_label(                                         &
       card(i_beg:i_end)//cards(i_card+1)(i_beg:i_end),row_col_work,i) !keyword
      row_col_work%label=card(i_beg:i_end)             !form label
      call ezgui_condense_label(row_col_work%label,i)
      row_col_work%label=                                                      &
       trim(row_col_work%label)//"&#xa;"//adjustl(cards(i_card+1)(i_beg:i_end))
      row_col_work%length=len_trim(row_col_work%label)      !adjust label length
      cards(i_card+1)(i_beg:i_end)=" "             !wipe out second line portion
    endif

    if(len_trim(row_col_work%keyword)==0) then
      call ezgui_make_new_keyword(                                             &
       "EZGUI_PUSH_BUTTON_",n_dummy, row_col_work%keyword)
    endif
    return
  end subroutine ezgui_process_pushbutton_info

!***********************************************************************
! Process toggle button information
!
! Written September 2001 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_togglebutton_info(row_col_work, str, i_col_adj,     &
     link_num, n_screen,x_pos, y_pos)
    type (row_col_elem), intent(inout) :: row_col_work
    integer,             intent(in)    :: link_num, n_screen, x_pos, y_pos
    integer,             intent(inout) :: i_col_adj
    character(len=*),    intent(in)    :: str

    integer                            :: i, i_apos, n

    call ezgui_init_row_col_elem(row_col_work, n_screen)    !we got a label
    row_col_work%link_num=link_num
    row_col_work%type='T'
    row_col_work%x_stretch="false"
    row_col_work%y_stretch="true"
    row_col_work%y_size=1

    n=len(str)
    i_apos=ezgui_find_chars(str,1,n,'`')
    if(i_apos>1) then
      call ezgui_process_layout_label(str(1:i_apos-1),row_col_work,i)
      i_col_adj=i_col_adj+i
    endif

    row_col_work%x_pos=x_pos - i_col_adj
    row_col_work%y_pos=y_pos
    row_col_work%length=n-i_apos+1
    row_col_work%x_size=row_col_work%length

    return
  end subroutine ezgui_process_togglebutton_info

!***********************************************************************
! Process combo button information
!
! Written September 2001 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_combobutton_info(row_col_work, str, i_col_adj,     &
     link_num, n_screen,x_pos, y_pos)
    type (row_col_elem), intent(inout) :: row_col_work
    integer,             intent(in)    :: link_num, n_screen, x_pos, y_pos
    integer,             intent(inout) :: i_col_adj
    character(len=*),    intent(in)    :: str

    integer                            :: i, i_apos, n
    call ezgui_init_row_col_elem(row_col_work, n_screen)    !we got a label
    row_col_work%link_num=link_num
    row_col_work%type='K'
    row_col_work%x_stretch="false"
    row_col_work%y_stretch="true"
    row_col_work%y_size=1

    n=len(str)
    i_apos=ezgui_find_chars(str,1,n,'`')
    if(i_apos>1) then
      call ezgui_process_layout_label(str(1:i_apos-1),row_col_work,i)
      i_col_adj=i_col_adj+i
    endif

    row_col_work%x_pos=x_pos - i_col_adj
    row_col_work%y_pos=y_pos
    row_col_work%length=n-i_apos+1
    row_col_work%x_size=row_col_work%length

    return
  end subroutine ezgui_process_combobutton_info

!***********************************************************************
! Process prompt label extracting any keyword, alignment or stretch info
!
! Written November 1999 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_layout_label(str, row_col_work, iadj)
    type (row_col_elem), intent(inout) :: row_col_work
    character (len=*), intent(in)      :: str
    integer, intent(out)               :: iadj

    call ezgui_process_layout_parms(str, row_col_work)
    row_col_work%label=str
    call ezgui_condense_label(row_col_work%label,iadj)
    return
  end subroutine ezgui_process_layout_label

!***********************************************************************
! Process prompt parms extracting any keyword, alignment or stretch info
!
! Written November 1999 by Charles C Burch
!***********************************************************************
  subroutine ezgui_process_layout_parms(str, row_col_work)
    type (row_col_elem), intent(inout) :: row_col_work
    character (len=*), intent(in)      :: str

    integer                            :: n, i1,i2, i3, i4
    character (len=240)                :: a_work

! print *,"parms..",trim(str)
    a_work=str
    n=len_trim(a_work)
    i1=ezgui_find_chars(a_work,1,n,'[')            ! see if string has [...]
    call ezgui_chars_upper_case(a_work)

    do while(i1<n)            !now see if infor in [..] has any /constraints
      i2=ezgui_find_chars(a_work,i1,n,']')

!------------Should check longer names first-------------

! see if it has a /NULL for null layout

      i3=ezgui_find_chars(a_work,i1,i2,"/NULL")
      if(i3<i2) then
        row_col_work%type="0"
        a_work(i3:n)=a_work(i3+5:n)
        i2=i2-5
      endif

! see if it has /xst for xStretch=true

      i3=ezgui_find_chars(a_work,i1,i2,'/XST')
      if(i3<i2) then
        row_col_work%x_stretch="true"
        a_work(i3:n)=a_work(i3+4:n)
        i2=i2-4
      endif

! see if it has /xsf for xStretch=false

      i3=ezgui_find_chars(a_work,i1,i2,'/XSF')
      if(i3<i2) then
        row_col_work%x_stretch="false"
        a_work(i3:n)=a_work(i3+4:n)
        i2=i2-4
      endif

! see if it has /yst for yStretch=true

      i3=ezgui_find_chars(a_work,i1,i2,'/YST')
      if(i3<i2) then
        row_col_work%y_stretch="true"
        a_work(i3:n)=a_work(i3+4:n)
        i2=i2-4
      endif

! see if it has /ysf for yStretch=false

      i3=ezgui_find_chars(a_work,i1,i2,'/YSF')
      if(i3<i2) then
        row_col_work%y_stretch="false"
        a_work(i3:n)=a_work(i3+4:n)
        i2=i2-4
      endif

! see if it has /sy for sensitivity=yes

      i3=ezgui_find_chars(a_work,i1,i2,'/SY')
      if(i3<i2) then
        row_col_work%sensitive="yes"
        a_work(i3:n)=a_work(i3+3:n)
        i2=i2-3
      endif

! see if it has /sn for sensitivity=no

      i3=ezgui_find_chars(a_work,i1,i2,'/SN')
      if(i3<i2) then
        row_col_work%sensitive="no"
        a_work(i3:n)=a_work(i3+3:n)
        i2=i2-3
      endif

! see if it has /ey for editable=yes

      i3=ezgui_find_chars(a_work,i1,i2,'/EY')
      if(i3<i2) then
        row_col_work%editable="yes"
        a_work(i3:n)=a_work(i3+3:n)
        i2=i2-3
      endif

! see if it has /en for editable=no

      i3=ezgui_find_chars(a_work,i1,i2,'/EN')
      if(i3<i2) then
        row_col_work%editable="no"
        a_work(i3:n)=a_work(i3+3:n)
        i2=i2-3
      endif

! see if it has a MaxLength

      i3=ezgui_find_chars(a_work,i1,i2,'/ML=')
      if(i3<i2) then
        i4=ezgui_find_chars(a_work,i3+1,i2-1,'/')
        row_col_work%length=ezgui_chars_to_int(a_work(i3+4:i4-1))
        a_work(i3:n)=a_work(i4:n)
        i2=i2-(i4-i3)
        if(row_col_work%x_size.lt.1) row_col_work%x_size=row_col_work%length
      endif

! see if it has a columnSize

      i3=ezgui_find_chars(a_work,i1,i2,'/CS=')
      if(i3<i2) then
        i4=ezgui_find_chars(a_work,i3+1,i2-1,'/')
        row_col_work%x_size=ezgui_chars_to_int(a_work(i3+4:i4-1))
        a_work(i3:n)=a_work(i4:n)
        i2=i2-(i4-i3)
        if(row_col_work%length.lt.1) row_col_work%length=row_col_work%x_size
      endif

! see if it has /c for alignment = center

      i3=ezgui_find_chars(a_work,i1,i2,'/C')
      if(i3<i2) then
        row_col_work%alignment="center"
        a_work(i3:n)=a_work(i3+2:n)
        i2=i2-2
      endif

! see if it has /l for alignment = left

      i3=ezgui_find_chars(a_work,i1,i2,'/L')
      if(i3<i2) then
        row_col_work%alignment="left"
        a_work(i3:n)=a_work(i3+2:n)
        i2=i2-2
      endif

! see if it has /r for alignment = right

      i3=ezgui_find_chars(a_work,i1,i2,'/R')
      if(i3<i2) then
        row_col_work%alignment="right"
        a_work(i3:n)=a_work(i3+2:n)
        i2=i2-2
      endif

! see if it has a ysize

      i3=ezgui_find_chars(a_work,i1,i2,'/2')
      if(i3<i2) then
        row_col_work%y_size=2
        a_work(i3:n)=a_work(i3+2:n)
        i2=i2-2
      endif

! see if it has any unrecognized /'s

      i3=ezgui_find_chars(a_work,i1,i2,'/')
      if(i3<i2) then
        print *,                                                               &
         "Unrecognized field modifier(s) found that are discarded: "//         &
         a_work(i3:i2-1)
        a_work(i3:i2-1)=" "
      endif

      i3=i1+1                           !strip out -'s
      do while(i3<i2)
        if(a_work(i3:i3)=='-') then
          a_work(i3:n)=a_work(i3+1:n)
          i2=i2-1
        else
          i3=i3+1
        endif
      enddo

      if(i2==i1+1) then
        a_work(i1:n)=a_work(i2+1:n)  !only [] left
        n=len_trim(a_work)
        i1=ezgui_find_chars(a_work,i1,n,'[')           ! see if string has [...]
      else
        n=len_trim(a_work)
        i1=ezgui_find_chars(a_work,i2,n,'[')           ! see if string has [...]
      endif
    enddo

    if(len_trim(a_work)>0) call ezgui_make_keyword(a_work,row_col_work%keyword)

    return
  end subroutine ezgui_process_layout_parms

!***************************************************************
! extract a keyword from text, skipping non keyword chracters
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_make_keyword(label, keyword)
    character (len=*), intent(in)  :: label
    character (len=*), intent(out) :: keyword

    integer                :: i1, i2, k_out, str_len
    character (len=240)    :: a_work

    a_work=label
    str_len=len_trim(a_work)

! see if keyword contained between [ and ]

    i1=ezgui_find_chars(a_work,1, str_len,'[')
    do while (i1<str_len)
      i2=ezgui_find_chars(a_work,i1,str_len,']')
      if(i2>str_len) exit
      k_out=ezgui_find_chars(a_work,i1,i2-1,'/')
      if(k_out==(i1+1) ) then
        a_work(i1:i2)=' '
        i1=ezgui_find_chars(a_work,i2,str_len,'[')
      else
        call ezgui_form_keyword(a_work(i1+1:k_out-1), keyword)
        goto 900
      endif
    enddo

!form keyword from label
    call ezgui_form_keyword(a_work, keyword)

    900 call ezgui_chars_upper_case(keyword)
    return
  end subroutine ezgui_make_keyword

!***************************************************************
! form keyword
!
! Written September 2001 by Charles C Burch
!***************************************************************
  subroutine ezgui_form_keyword(a_work, keyword)
    character(len=*), intent(in)   :: a_work
    character(len=*), intent(out)  :: keyword

    integer :: i, k_out, k_max, str_len

    k_out=0
    k_max=min(32,len(keyword))
    keyword=" "
    i=1
    str_len=len(a_work)

    do while(i<=str_len)
      select case (a_work(i:i))

      case ("a":"z","A":"Z")
        k_out=k_out+1
        keyword(k_out:k_out)=a_work(i:i)
        if(k_out==k_max) exit

      case ("0":"9","_")
        if(k_out>0) then
          k_out=k_out+1
          keyword(k_out:k_out)=a_work(i:i)
          if(k_out==k_max) exit
        endif

      case ('&')                                         !skip unicodes
        if(i<str_len) then
          if(a_work(i+1:i+1)=='#') i=ezgui_find_chars(a_work, i,str_len,';')
        endif

      end select

      i=i+1
    enddo

    return
  end subroutine ezgui_form_keyword

!***************************************************************
! create a keyword using given label and count
!
! Written November 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_make_new_keyword(label, count, keyword)
    character (len=*), intent(in)  :: label
    character (len=*), intent(out) :: keyword
    integer, intent(inout)         :: count

    integer                        :: n

    keyword=label
    n=len_trim(label)
    call ezgui_int_to_chars(count,keyword(n+1:n+3))
    count=count+1
    return
  end subroutine ezgui_make_new_keyword

!***************************************************************
! forms a keyword using base if not already used, otherwise
!  isncrements a counter used in keyword
!
! Written September 2001 by Charles C Burch
!***************************************************************
  subroutine ezgui_form_new_keyword(base,n_base,n_elems,row_col_elems,keyword)
    character (len=*), intent(in)   :: base
    integer, intent(inout)          :: n_base
    integer, intent(in)             :: n_elems
    type (row_col_elem), intent(in) :: row_col_elems(:)
    character (len=*), intent(out)  :: keyword

    integer                         :: i

    do i=1, n_elems
      if(row_col_elems(i)%keyword==base) then
        call ezgui_make_new_keyword(base//"_",n_base,keyword)
        return
      endif
    enddo

    keyword=base
    return
  end subroutine ezgui_form_new_keyword

!***************************************************************
! save a new entry into row_col_elems
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_save_row_col_elem(row_col_elems, n_elems, row_col_work)
    type (row_col_elem), intent(in)    :: row_col_work
    type (row_col_elem), intent(inout) :: row_col_elems(:)
    integer, intent(inout)             :: n_elems
!  integer                            :: i

!  do i=1,n_elems
!    if(row_col_elems(i)%keyword==row_col_work%keyword) then
!      print *,"WARNING: keyword("//trim(row_col_work%keyword)//               &
!       ") being used more than once"
!      exit
!    endif
!  enddo

    if(n_elems<XMLVARS_SZ) then
      n_elems=n_elems+1
      row_col_elems(n_elems)=row_col_work
    else
      print *,"row_col_elems overflow"
      stop
    endif

    return
  end subroutine ezgui_save_row_col_elem

!***************************************************************
! get entry index in row_col_elems of keyword
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_get_row_col_elems_index(row_col_elems, n_elems, n_screen,   &
     keyword, indx)
    type (row_col_elem), intent(inout) :: row_col_elems(:)
    integer, intent(inout)             :: n_elems
    integer, intent(in)                :: n_screen
    integer, intent(out)               :: indx
    character (len=*), intent(in)      :: keyword

    character (len=32)                 ::kw_upper_case

    kw_upper_case=keyword
    call ezgui_chars_upper_case(kw_upper_case)

    do indx=1,n_elems
      if(row_col_elems(indx)%keyword==kw_upper_case) return
    enddo

    if(n_elems<XMLVARS_SZ) then
      n_elems=n_elems+1
      call ezgui_init_row_col_elem(row_col_elems(n_elems), n_screen)
      row_col_elems(n_elems)%keyword=kw_upper_case
    else
      print *,"row_col_elems array overflow"
      stop
    endif

    return
  end subroutine ezgui_get_row_col_elems_index

!***************************************************************
! Extract keyword(keyword) information(info) from xml card
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_get_xml_keyword_info(card, card_upper_case, card_length,    &
     keyword, info)
    character (len=*), intent(in)    :: card, keyword
    character (len=*), intent(inout) :: card_upper_case
    character (len=*), intent(out)   :: info
    integer, intent(in)              :: card_length

    integer :: i0, i1, i2

    i0=ezgui_find_chars(card_upper_case,1,card_length,keyword)
    if(i0<card_length) then
      i1=ezgui_find_chars(card_upper_case,i0,card_length,'"')
      i2=ezgui_find_chars(card_upper_case,i1+1,card_length,'"')
      if(i2>card_length) then
        print *,"Invalid "//keyword//" Constraint"
        print *,"  "//trim(card)
        info=" "
      else
        info=card(i1+1:i2-1)
        card_upper_case(i0:i2)=" "
      endif
    else
      info=" "
    endif
    return
  end subroutine ezgui_get_xml_keyword_info

!***************************************************************
! Initialize a XML entry with default values
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_init_row_col_elem(row_col_work, n_screen)
    type (row_col_elem), intent(out) :: row_col_work
    integer, intent(in)              :: n_screen

    row_col_work%screen_num=n_screen
    row_col_work%x_pos=0
    row_col_work%y_pos=0
    row_col_work%type="L"
    row_col_work%length=0
    row_col_work%keyword=" "
    row_col_work%label=" "
    row_col_work%editable="yes"
    row_col_work%sensitive="yes"
    row_col_work%alignment="center"
    row_col_work%x_stretch="true"
    row_col_work%y_stretch="true"
    row_col_work%x_size=1
    row_col_work%y_size=1
    row_col_work%link_num=-1
    row_col_work%cell_width=8
    row_col_work%cell_height=12
    row_col_work%help_size=0
    row_col_work%tip_info=" "
    return
  end subroutine ezgui_init_row_col_elem

!***************************************************************
! Process an array entry
!
! Written Nov 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_process_xml_array_element(row_col_elems,n_elems,n_screen,   &
     card, card_upper_case,i_end,link_num)
    integer, intent(in)               :: i_end, link_num, n_screen
    integer, intent(inout)            :: n_elems
    character (len=*), intent(in out) :: card, card_upper_case
    type (row_col_elem)               :: row_col_elems(:)

    integer                           :: i_elem, istat
    character (len=80)                :: a_work

    card_upper_case(1:6)=" "
    card_upper_case(i_end:i_end)=""
    if(card_upper_case(i_end-1:i_end-1)=="/")                                  &
     card_upper_case(i_end-1:i_end-1)=" "
    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "KEYWORD",a_work)
    if(len_trim(a_work)==0) then
      print *,"Array entry does not have a keyword"
      print *,"  "//trim(card)
      return
    endif

    call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,        &
     trim(a_work),i_elem)
    if(i_elem/=n_elems) then
      print *,                                                                 &
       "WARNING: keyword("//trim(a_work)//") appears to be used multiple times"
    endif

    row_col_elems(i_elem)%link_num=link_num
    if(link_num==2) row_col_elems(i_elem)%y_stretch="false"

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "MAXLENGTH",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%length=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid MAXLENGTH("//trim(a_work)//") in Array card"
        print *,"  "//trim(card)
      endif
      if(row_col_elems(i_elem)%x_size.le.1)                                    &
       row_col_elems(i_elem)%x_size=row_col_elems(i_elem)%length
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "COLUMNSIZE",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%x_size=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid COLUMNSIZE("//trim(a_work)//") in Array card"
        print *,"  "//trim(card)
      endif
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "COLUMNNAME",a_work)
    if(len_trim(a_work)>0) row_col_elems(i_elem)%label=a_work

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "TYPE", a_work)
    if(len_trim(a_work)>0) then
      call ezgui_get_field_type_code(trim(a_work),row_col_elems(i_elem)%type)
      if(row_col_elems(i_elem)%type==" ") then
        print *,"Invalid type in Array card("//trim(a_work)//")"
        print *,"  "//trim(card)
      endif
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "EDITABLE", a_work)
    if(len_trim(a_work)>0) then
      call ezgui_chars_upper_case(a_work(1:10))
      select case (trim(a_work))
      case ("YES")
        row_col_elems(i_elem)%editable="yes"
      case ("NO")
        row_col_elems(i_elem)%editable="no"
      case default
        print *,"Invalid editable case("//trim(a_work)//" in Array card"
        print *,"  "//trim(card)
      end select
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "SENSITIVE", a_work)
    if(len_trim(a_work)>0) then
      call ezgui_chars_upper_case(a_work(1:10))
      select case (trim(a_work))
      case ("YES")
        row_col_elems(i_elem)%sensitive="yes"
      case ("NO")
        row_col_elems(i_elem)%sensitive="no"
      case default
        print *,"Invalid editable case("//trim(a_work)//" in Array card"
        print *,"  "//trim(card)
      end select
    endif

    if(len_trim(card_upper_case)>0) then
      print *,"The following parameters were not processed"
      print *,"  "//trim(adjustl(card_upper_case))
      print *,"  "//trim(card)
    endif

    return
  end subroutine ezgui_process_xml_array_element

!***************************************************************
! Get xml token from xml_data-place into card
!     Internal EzGUI usage only
!
! Written November 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,             &
     card_upper_case, len_card, idelim)
    character (len=*), intent(in)  :: xml_data
    character (len=*), intent(out) :: card, card_upper_case
    integer, intent(in)            :: xml_end
    integer, intent(inout)         :: xml_beg
    integer, intent(out)           :: len_card, idelim
    integer                        :: i_delim1, i_delim2

    i_delim1=ezgui_find_chars(xml_data, xml_beg,xml_end,"<")
    if(i_delim1>xml_end) then
      card=""
      card_upper_case=""
      len_card=0
      return
    endif

    i_delim2=ezgui_find_chars(xml_data, i_delim1,xml_end,">")
    card=xml_data(i_delim1:i_delim2)
    card_upper_case=card
    call ezgui_chars_upper_case(card_upper_case)
    xml_beg=i_delim2+1
    len_card=i_delim2-i_delim1+1
    idelim=min(len_card, ezgui_find_chars(card,1,len_card," "))

! print *,"xml token..",trim(card)
    return
  end subroutine ezgui_get_xml_token

!*******************************************************************
! Process XML Screen elements
!
! Written Dec 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_process_screen_xml_cards(row_col_elems, n_elems, card,      &
     card_upper_case, xml_data, xml_beg, xml_end, n_screen, i_delim1, i_end)
    integer, intent(inout)             :: n_elems
    type (row_col_elem)                :: row_col_elems(:)
    character (len=*), intent(inout)   :: card, card_upper_case
    integer                            :: i_delim1, xml_beg, xml_end,i_elem
    integer                            :: i_end, n_screen
    character (len=*),intent(in)       :: xml_data

    integer               :: i, i_delim2, istat, n_old
    character (len=80)    :: a_work

    card_upper_case(1:i_delim1)=" "
    card_upper_case(i_end:i_end)=""
    n_screen=n_screen+1

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "KEYWORD",a_work)
    if(len_trim(a_work)==0) then
      print *,"Screen entry does not have a keyword"
      print *,"  "//trim(card)
      i=n_screen
      call ezgui_make_new_keyword("SCREEN",i,a_work)
    endif

    n_old=n_elems
    call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,        &
     trim(a_work),i_elem)
    if(n_old==n_elems) then
      print *,                                                                 &
       "WARNING: keyword("//trim(a_work)//") appears to be used multiple times"
    endif

    row_col_elems(i_elem)%type=NEW_SCREEN_CARD_TYPE
    row_col_elems(i_elem)%y_pos=-2
    row_col_elems(i_elem)%link_num=-1
    row_col_elems(i_elem)%x_size=-1
    row_col_elems(i_elem)%y_size=-1
    row_col_elems(i_elem)%cell_width=-1
    row_col_elems(i_elem)%cell_height=-1

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "TITLE",a_work)
    if(len_trim(a_work)>0) row_col_elems(i_elem)%label=a_work
    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "COLUMNS",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%x_size=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid screen columns value("//trim(a_work)//")"
        print *,"  "//trim(card)
      endif
    endif

    call ezgui_get_xml_keyword_info(card, card_upper_case, i_end, "ROWS",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%y_size=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid screen rows value("//trim(a_work)//")"
        print *,"  "//trim(card)
      endif
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "MINCELLWIDTH",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%cell_width=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid screen minCellWidth value("//trim(a_work)//")"
        print *,"  "//trim(card)
      endif
    endif

    call ezgui_get_xml_keyword_info(                                           &
     card,card_upper_case,i_end,"MINCELLHEIGHT",a_work)
    if(len_trim(a_work)>0) then
      row_col_elems(i_elem)%cell_height=ezgui_chars_to_int(trim(a_work),istat)
      if(istat/=0) then
        print *,"Invalid screen minCellHeight value("//trim(a_work)//")"
        print *,"  "//trim(card)
      endif
    endif

    if(len_trim(card_upper_case)>0) then
      print *,"The following parameters were not processed"
      print *,"  "//trim(adjustl(card_upper_case))
      print *,"  "//trim(card)
    endif

    do while(xml_beg<=xml_end)
      call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,               &
       card_upper_case, i_end, i_delim1)
      if(i_end==0) exit

      select case (card_upper_case(2:i_delim1-1) )
      case ("/SCREEN")
        i=n_screen
        call ezgui_make_new_keyword("~END_SCREEN",i,a_work)
        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%keyword=a_work
        row_col_elems(i_elem)%type=END_SCREEN_CARD_TYPE
        row_col_elems(i_elem)%y_pos=999
        row_col_elems(i_elem)%length=0
        row_col_elems(i_elem)%link_num=-1
        exit

      case ("POPUPMENU")
        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"PopupMenu entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%keyword=a_work
        row_col_elems(i_elem)%type=POPUP_MENU_CARD_TYPE
        row_col_elems(i_elem)%y_pos=-1
        row_col_elems(i_elem)%link_num=0

        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit

          select case (card_upper_case(2:i_delim1-1) )
          case ("/POPUPMENU")
            i=n_screen
            call ezgui_make_new_keyword("~END_POPUP_MENU",i,a_work)
            n_old=n_elems
            call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,&
             trim(a_work),i_elem)
            if(n_old==n_elems) then
              print *,                                                         &
               "WARNING: keyword("//trim(a_work)//                             &
               ") appears to be used multiple times"
            endif
            row_col_elems(i_elem)%keyword=a_work
            row_col_elems(i_elem)%type=POPUP_MENU_CARD_TYPE
            row_col_elems(i_elem)%y_pos=-1
            row_col_elems(i_elem)%link_num=1
            exit

          case ("BUTTON")
            card_upper_case(1:i_delim1)=" "
            card_upper_case(i_end:i_end)=""
            if(card_upper_case(i_end-1:i_end-1)=="/")                          &
             card_upper_case(i_end-1:i_end-1)=" "

            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "KEYWORD",a_work)
            if(len_trim(a_work)==0) then
              print *,"Button entry does not have a keyword"
              print *,"  "//trim(card)
              cycle
            endif

            n_old=n_elems
            call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,&
             trim(a_work),i_elem)
            if(n_old==n_elems) then
              print *,                                                         &
               "WARNING: keyword("//trim(a_work)//                             &
               ") appears to be used multiple times"
            endif
            row_col_elems(i_elem)%type="P"
            row_col_elems(i_elem)%link_num=3
            row_col_elems(i_elem)%y_pos=-1
            i_delim2=xml_beg

            call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,         &
             card_upper_case, i_end, i_delim1)
            if(i_end==0) exit

            if(card_upper_case(1:i_end)/="</BUTTON>") then
              print *,"<BUTTON...> not followed by </BUTTON>"
              print *,"  "//xml_data(i_delim2:xml_beg-1)
            else
              row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-10)
              row_col_elems(i_elem)%length=xml_beg-10-i_delim2+1
            endif

          case default
            call ezgui_bad_xml_warning(                                        &
             "PopupMenu",card(1:i_end), card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select

        end do

      case ("/POPUPMENU")
        print *,"</POPUPMENU> found without corresponding <POPUPMENU>"

      case ("LABEL")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""
        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"Screen entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%editable="no"

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "ALIGNMENT", a_work)
        if(len_trim(a_work)>0) then
          call ezgui_chars_upper_case(a_work(1:10))
          select case (trim(a_work))
          case ("RIGHT")
            row_col_elems(i_elem)%alignment="right"
          case ("LEFT")
            row_col_elems(i_elem)%alignment="left"
          case ("CENTER")
            row_col_elems(i_elem)%alignment="center"
          case default
            print *,"Invalid alignment code ("//trim(a_work)//") in label card"
            print *,"  "//trim(card)
          end select
        endif

        if(len_trim(card_upper_case)>0) then
          print *,"The following Label parameters were not processed",         &
           len_trim(card_upper_case)
          print *,"  "//trim(adjustl(card_upper_case))
          print *,"  "//trim(card)
        endif

        i_delim2=xml_beg
        call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,             &
         card_upper_case, i_end, i_delim1)
        if(i_end==0) exit

        if(card_upper_case(1:i_end)/="</LABEL>") then
          print *,"<LABEL..> not followed by </LABEL>"
          print *,"  "//xml_data(i_delim2:xml_beg-1)
        else
          row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-9)
          row_col_elems(i_elem)%length=xml_beg-9-i_delim2+1
        endif

      case ("/LABEL")
        print *,"</LABEL> token found without <LABEL>"

      case ("FIELD")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""
        if(card_upper_case(i_end-1:i_end-1)=="/")                              &
         card_upper_case(i_end-1:i_end-1)=" "

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"Screen entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%label=a_work

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end,"MAXLENGTH",a_work)
        if(len_trim(a_work)>0) then
          row_col_elems(i_elem)%length=ezgui_chars_to_int(trim(a_work),istat)
          if(istat/=0) then
            print *,"Invalid MAXLENGTH("//trim(a_work)//") in Field card"
            print *,"  "//trim(card)
          endif
        endif

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "TYPE", a_work)
        if(len_trim(a_work)>0) then
          call ezgui_get_field_type_code(                                      &
           trim(a_work),row_col_elems(i_elem)%type)
          if(row_col_elems(i_elem)%type==" ") then
            print *,"Invalid type in Field card("//trim(a_work)//")"
            print *,"  "//trim(card)
          endif
        endif

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "EDITABLE",a_work)
        if(len_trim(a_work)>0) then
          call ezgui_chars_upper_case(a_work(1:10))
          select case (trim(a_work))
          case ("YES")
            row_col_elems(i_elem)%editable="yes"
          case ("NO")
            row_col_elems(i_elem)%editable="no"
          case default
            print *,"Invalid editable case("//trim(a_work)//" in Field card"
            print *,"  "//trim(card)
          end select
        endif

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end,"SENSITIVE",a_work)
        if(len_trim(a_work)>0) then
          call ezgui_chars_upper_case(a_work(1:10))
          select case (trim(a_work))
          case ("YES")
            row_col_elems(i_elem)%sensitive="yes"
          case ("NO")
            row_col_elems(i_elem)%sensitive="no"
          case default
            print *,"Invalid editable case("//trim(a_work)//" in Field card"
            print *,"  "//trim(card)
          end select
        endif

        if(len_trim(card_upper_case)>0) then
          print *,"The following Field parameters were not processed",         &
           len_trim(card_upper_case)
          print *,"  "//trim(adjustl(card_upper_case))
          print *,"  "//trim(card)
        endif

      case ("BORDER")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""
        if(card_upper_case(i_end-1:i_end-1)=="/")                              &
         card_upper_case(i_end-1:i_end-1)=" "

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"Border entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%type="B"
        call ezgui_get_xml_keyword_info(card, card_upper_case, i_end, "TITLE", &
         row_col_elems(i_elem)%label)

      case ("BUTTON")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""
        if(card_upper_case(i_end-1:i_end-1)=="/")                              &
         card_upper_case(i_end-1:i_end-1)=" "

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"Button entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%type="P"
        row_col_elems(i_elem)%link_num=1
        i_delim2=xml_beg

        call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,             &
         card_upper_case, i_end, i_delim1)
        if(i_end==0) exit

        if(card_upper_case(1:i_end)/="</BUTTON>") then
          print *,"<BUTTON...> not followed by </BUTTON>"
          print *,"  "//xml_data(i_delim2:xml_beg-1)
        else
          row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-10)
          row_col_elems(i_elem)%length=1
        endif

      case ("/BUTTON")
        print *,"</BUTTON> token found without <BUTTON>"
      case ("ARRAY")
        call ezgui_process_xml_array_element(                                  &
         row_col_elems,n_elems,n_screen,card, card_upper_case, i_end, 1)

      case ("ARRAYSET")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"ArraySet entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif
        row_col_elems(i_elem)%link_num=0
        row_col_elems(i_elem)%type="A"
        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "LABEL", a_work)
        if(len_trim(a_work)>0) row_col_elems(i_elem)%label=a_work

        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit
          select case (card_upper_case(2:i_delim1-1) )
          case ("/ARRAYSET")
            exit
          case ("ARRAY")
            call ezgui_process_xml_array_element(                              &
             row_col_elems,n_elems,n_screen,card,card_upper_case,i_end,2)
          case default
            call ezgui_bad_xml_warning("ArraySet",card(1:i_end),               &
             card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select
        end do

      case ("/ARRAYSET")
        print *,"</ARRAYSET> found without corresponding <ARRAYSET>"

      case ("RADIOBUTTONS")
        card_upper_case(1:i_delim1)=" "
        card_upper_case(i_end:i_end)=""

        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "KEYWORD",a_work)
        if(len_trim(a_work)==0) then
          print *,"RadioButtons entry does not have a keyword"
          print *,"  "//trim(card)
          cycle
        endif

        n_old=n_elems
        call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,    &
         trim(a_work),i_elem)
        if(n_old==n_elems) then
          print *,                                                             &
           "WARNING: keyword("//trim(a_work)//                                 &
           ") appears to be used multiple times"
        endif

        row_col_elems(i_elem)%link_num=0
        row_col_elems(i_elem)%type="R"
        row_col_elems(i_elem)%y_pos=-1
        call ezgui_get_xml_keyword_info(                                       &
         card, card_upper_case, i_end, "LABEL", a_work)
        if(len_trim(a_work)>0) row_col_elems(i_elem)%label=a_work

        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit
          select case (card_upper_case(2:i_delim1-1) )
          case ("/RADIOBUTTONS")
            exit
          case ("BUTTON")
            card_upper_case(1:i_delim1)=" "
            card_upper_case(i_end:i_end)=""
            if(card_upper_case(i_end-1:i_end-1)=="/")                          &
             card_upper_case(i_end-1:i_end-1)=" "
            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "KEYWORD",a_work)
            if(len_trim(a_work)==0) then
              print *,"Button entry does not have a keyword"
              print *,"  "//trim(card)
              cycle
            endif

            n_old=n_elems
            call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,&
             trim(a_work),i_elem)
            if(n_old==n_elems) then
              print *,                                                         &
               "WARNING: keyword("//trim(a_work)//                             &
               ") appears to be used multiple times"
            endif
            row_col_elems(i_elem)%type="P"
            row_col_elems(i_elem)%link_num=3
            i_delim2=xml_beg
            call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,         &
             card_upper_case, i_end, i_delim1)
            if(i_end==0) exit

            if(card_upper_case(1:i_end)/="</BUTTON>") then
              print *,"<BUTTON...> not followed by </BUTTON>"
              print *,"  "//xml_data(i_delim2:xml_beg-1)
            else
              row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-10)
              row_col_elems(i_elem)%length=xml_beg-10-i_delim2+1
            endif

          case default
            call ezgui_bad_xml_warning("RadioButtons",card(1:i_end),           &
             card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select
        end do

      case ("/RADIOBUTTONS")
        print *,"</RADIOBUTTONS> found without corresponding <RADIOBUTTONS>"

      case ("LAYOUT")
        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit

          select case(card_upper_case(2:i_delim1-1))
          case ("/LAYOUT")
            exit
          case ("CONSTRAINTS")
            card_upper_case(1:i_delim1)=""
            if(card_upper_case(i_end-1:i_end-1)=="/")                          &
             card_upper_case(i_end-1:i_end-1)=""
            card_upper_case(i_end:i_end)=""
            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "COMPONENT",a_work)
            if(len_trim(a_work)==0) then
              print *,"Constraint entry does not have a keyword"
              print *,"  "//trim(card)
              cycle
            endif

            n_old=n_elems
            call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,&
             trim(a_work),i_elem)
            if(n_elems>n_old) then
              print *,"WARNING: Constraint has keyword("//trim(a_work)//       &
               ") not previously defined"
            endif

            call ezgui_get_xml_keyword_info(                                   &
             card,card_upper_case,i_end,"XSIZE",a_work)
            if(len_trim(a_work)>0) then
              row_col_elems(i_elem)%x_size=                                    &
               ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid XSIZE("//trim(a_work)//") in CONSTRAINT card"
                print *,"  "//trim(card)
              else
                if (row_col_elems(i_elem)%length==0)                           &
                 row_col_elems(i_elem)%length=row_col_elems(i_elem)%x_size
              endif
            endif

            call ezgui_get_xml_keyword_info(                                   &
             card, card_upper_case, i_end,"YSIZE",a_work)
            if(len_trim(a_work)>0) then
              row_col_elems(i_elem)%y_size=                                    &
               ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid YSIZE("//trim(a_work)//") in CONSTRAINT card"
                print *,"  "//trim(card)
              endif
            endif

            call ezgui_get_xml_keyword_info(                                   &
             card, card_upper_case, i_end, "XPOS",a_work)
            if(len_trim(a_work)>0) then
              row_col_elems(i_elem)%x_pos=ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid XPOS("//trim(a_work)//") in CONSTRAINT card"
                print *,"  "//trim(card)
              endif
            endif

            call ezgui_get_xml_keyword_info(                                   &
             card, card_upper_case, i_end, "YPOS",a_work)
            if(len_trim(a_work)>0) then
              row_col_elems(i_elem)%y_pos=ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid YPOS("//trim(a_work)//") in CONSTRAINT card"
                print *,"  "//trim(card)
              endif
            endif

            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "XSTRETCH", a_work)
            if(len_trim(a_work)>0) then
              call ezgui_chars_upper_case(a_work(1:10))
              select case (trim(a_work))
              case ("TRUE","YES")
                row_col_elems(i_elem)%x_stretch="true"
              case ("FALSE","NO")
                row_col_elems(i_elem)%x_stretch="false"
              case default
                print *,"Invalid xStretch case("//trim(a_work)//               &
                 " in CONSTRAINTS card"
                print *,"  "//trim(card)
              end select
            endif

            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "YSTRETCH", a_work)
            if(len_trim(a_work)>0) then
              call ezgui_chars_upper_case(a_work(1:10))
              select case (trim(a_work))
              case ("TRUE","YES")
                row_col_elems(i_elem)%y_stretch="true"
              case ("FALSE","NO")
                row_col_elems(i_elem)%y_stretch="false"
              case default
                print *,"Invalid yStretch case("//trim(a_work)//               &
                 " in CONSTRAINTS card"
                print *,"  "//trim(card)
              end select
            endif

            if(len_trim(card_upper_case)>0) then
              print *,"The following Constraint parameters were not processed",&
               len_trim(card_upper_case)
              i=len_trim(card_upper_case)
              print *,ichar(card_upper_case(i:i))
              print *,"  "//trim(adjustl(card_upper_case))
              print *,"  "//trim(card)
            endif


          case default
            call ezgui_bad_xml_warning("Layout",card(1:i_end),                 &
             card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select
        enddo
      case ("/LAYOUT")
        print *,"</LAYOUT> found without <LAYOUT>"

      case ("HELPSECTION")
        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit

          select case(card_upper_case(2:i_delim1-1))
          case ("/HELPSECTION")
            exit

          case ("HELP")
            call ezgui_process_xml_help_cards(row_col_elems, n_elems, card,    &
             card_upper_case, xml_data,                                        &
             xml_beg, xml_end,n_screen, i_delim1, i_end, ' ', istat)
            if(istat/=0) cycle

          case default
            call ezgui_bad_xml_warning("HelpSection",card(1:i_end),            &
             card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select
        enddo

      case ("/HELPSECTION")
        print *,"</HELPSECTION> found without <HELPSECTION>"

      case default
        call ezgui_bad_xml_warning("Screen",card(1:i_end),                     &
         card_upper_case(2:i_delim1-1), istat)
        if(istat<0) return
      end select
    enddo
    return
  end subroutine ezgui_process_screen_xml_cards

!**************************************************************************
! Process XML Help elements
!
! Written Jan 2000 by Charles C Burch
!**************************************************************************
  subroutine ezgui_process_xml_help_cards(row_col_elems, n_elems, card,        &
     card_upper_case, xml_data, xml_beg, xml_end, n_screen, i_delim1,          &
     i_end, keyword, istat)
    integer, intent(inout)             :: n_elems
    type (row_col_elem)                :: row_col_elems(:)
    character (len=*), intent(inout)   :: card, card_upper_case
    integer                            :: i_delim1, xml_beg, xml_end,i_elem
    integer                            :: i_end, n_screen, istat
    character (len=*),intent(in)       :: xml_data, keyword

    integer               :: i, j, i_delim2, n_old
    character (len=80)    :: a_work

    istat=0
    card_upper_case(1:i_delim1)=""
    if(card_upper_case(i_end-1:i_end-1)=="/")                                  &
     card_upper_case(i_end-1:i_end-1)=""
    card_upper_case(i_end:i_end)=""

    call ezgui_get_xml_keyword_info(                                           &
     card, card_upper_case, i_end, "COMPONENT",a_work)
    if(len_trim(a_work)==0) then
      print *,"Help entry does not have a keyword"
      print *,"  "//trim(card)
      istat=1
      return
    endif

    if(len_trim(keyword)>0) a_work=keyword
    n_old=n_elems
    call ezgui_get_row_col_elems_index(row_col_elems, n_elems,n_screen,        &
     trim(a_work),i_elem)
    if(n_elems>n_old) then
      print *,"WARNING: HELP has keyword("//trim(a_work)//                     &
       ") not previously defined"
    endif

    if(len_trim(card_upper_case)>0) then
      print *,"The following HELP parameters were not processed"
      print *,"  "//trim(adjustl(card_upper_case))
      print *,"  "//trim(card)
    endif

    do while(xml_beg<=xml_end)
      call ezgui_get_xml_token(                                                &
       xml_data, xml_beg, xml_end, card, card_upper_case, i_end, i_delim1)
      if(i_end==0) exit

      select case(card_upper_case(2:i_delim1-1))
      case ("/HELP")
        exit

      case ("TEXT/")
        cycle

      case ("TEXT")
        i_delim2=xml_beg
        if(xml_data(i_delim2:i_delim2)==char(0)) i_delim2=i_delim2+1
        do while(ezgui_find_chars(card_upper_case,1,i_end,"</TEXT>") >i_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case,i_end,i_delim1)
          if(i_end==0) exit
        enddo

        if(row_col_elems(i_elem)%help_size>0) then
          deallocate (row_col_elems(i_elem)%help_info, stat=istat)
        endif

        i_end=xml_beg-i_delim2-7
        allocate ( row_col_elems(i_elem)%help_info(1:i_end), stat=istat )
        if(istat/=0) then
          print *,"Error in allocating help storage(size=",i_end,")"
          print *,"keyword="//trim(row_col_elems(i_elem)%keyword)
          stop
        endif

        do j=1,i_end
          row_col_elems(i_elem)%help_info(j)=xml_data(i_delim2+j-1:i_delim2+j-1)
        enddo
        row_col_elems(i_elem)%help_size=i_end

      case ("TIP")
        i_delim2=xml_beg
        do while(card_upper_case(1:i_end)/="</TIP>")
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit
        enddo
        i_end=xml_beg-7

        row_col_elems(i_elem)%tip_info=' '
        i=1
        do while(i_delim2<=i_end)
          if(xml_data(i_delim2:i_delim2)/=char(0)) then
            row_col_elems(i_elem)%tip_info(i:i)=xml_data(i_delim2:i_delim2)
            i=i+1
            if(i>80) exit
          endif
          i_delim2=i_delim2+1
        enddo

      case default
        call ezgui_bad_xml_warning("Help ",card(1:i_end),                      &
         card_upper_case(2:i_delim1-1), istat)
        if(istat<0) return
      end select
    enddo
    return
  end subroutine ezgui_process_xml_help_cards

!***************************************************************
! Read xml file and store in row_col_elems structure
!     Internal EzGUI usage only
!
! Written November 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_read_xml_file(file_name, n_elems, row_col_elems, istat)
    character (len=*), intent(in)   :: file_name
    integer, intent(inout)          :: n_elems
    integer, intent(out)            :: istat
    type (row_col_elem)             :: row_col_elems(:)

    character (len=256)   :: card, card_upper_case
    integer               :: i_delim1, i_delim2, xml_beg, xml_end,i_elem, i_row
    integer               :: n_screen, io_stat, i, j, i_end, n_old, i_col
    character (len=1)     :: str0=char(0)
    character (len=80)    :: a_work
    character (len=2048)  :: buff
    character (len=XML_DATA_SZ) :: xml_data

    n_elems=1
    window_width=1024
    window_height=768
    window_dialog=0
    call ezgui_init_row_col_elem(row_col_elems(1), -9)
    row_col_elems(1)%keyword="~PROCESS_HELP"
    row_col_elems(1)%type="H"
    row_col_elems(1)%y_pos=-1
    row_col_elems(1)%length=0
    row_col_elems(1)%link_num=0

    n_screen=0
    open(unit=11,file=file_name,status='old',iostat=istat)
    if(istat/=0) return
    read(11,'(a)', iostat=io_stat) buff  !read a line from input file
    xml_end=0

    do while (io_stat>=0)
      call ezgui_clean_up_text(buff)
      i_end=max(1,len_trim(buff))

      if(xml_end+i_end+1>XML_DATA_SZ) then
        print *,' xml data buffer overflow-contact CPS programmer'
        return
      endif

      i=ezgui_find_not_chars(buff,1,i_end," ")
      if(i<i_end) then
        if(buff(i:i+1)/='<!') i=i_end+1
      endif

      if(i>i_end) then                 !skip comment cards
        xml_data(xml_end+1:xml_end+i_end+1)=buff(1:i_end)//str0
        xml_end=xml_end+i_end+1
      endif

      read(11,'(a)', iostat=io_stat) buff  !read a line from input file
    enddo
    close (unit=11)

    xml_beg=1
    do while(xml_beg<=xml_end)
      call ezgui_get_xml_token(                                                &
       xml_data, xml_beg, xml_end, card, card_upper_case,i_end, i_delim1)
      if(i_end==0) exit

      select case (card_upper_case(2:i_delim1-1))
      case ("?XML")

      case ("!DOCTYPE")

      case ("GUI")
        do while(xml_beg<=xml_end)
          call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,           &
           card_upper_case, i_end, i_delim1)
          if(i_end==0) exit

          select case (card_upper_case(2:i_delim1-1))
          case ("/GUI")
            exit

          case ("WINDOW", "DIALOG")
            window_dialog=0                  !window
            if(card_upper_case(2:i_delim1-1)=="DIALOG") then
              window_dialog=1                       !dialog, modal=false
              call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,    &
               "MODAL",a_work)
              call ezgui_chars_upper_case(a_work)
              if(trim(a_work)=="TRUE")window_dialog=2    !dialog modal=true
            endif

            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "HEIGHT",a_work)
            if(len_trim(a_work)>0) then
              window_height=ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid window height value("//trim(a_work)//")"
                print *,"  "//trim(card)
              endif
            endif

            call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,      &
             "WIDTH",a_work)
            if(len_trim(a_work)>0) then
              window_width=ezgui_chars_to_int(trim(a_work),istat)
              if(istat/=0) then
                print *,"Invalid window width value("//trim(a_work)//")"
                print *,"  "//trim(card)
              endif
            endif

            do while(xml_beg<=xml_end)
              call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,       &
               card_upper_case, i_end, i_delim1)
              if(i_end==0) exit

              select case (card_upper_case(2:i_delim1-1))
              case ("/WINDOW")
                if(window_dialog==0) exit
                print *,"</WINDOW> found without corresponding <WINDOW>"

              case ("/DIALOG")
                if(window_dialog>0) exit
                print *,"</DIALOG> found without corresponding <DIALOG>"

              case ("HELP")
                call ezgui_process_xml_help_cards(row_col_elems, n_elems, card,&
                 card_upper_case, xml_data, xml_beg, xml_end,n_screen,         &
                 i_delim1, i_end, '~PROCESS_HELP', istat)
                if(istat/=0) cycle

              case ("TOOLBAR")
                a_work="~TOOLBAR"
                n_old=n_elems
                call ezgui_get_row_col_elems_index(row_col_elems, n_elems,     &
                 n_screen, trim(a_work),i_elem)
                if(n_old==n_elems) then
                  print *,"WARNING: keyword("//trim(a_work)//                  &
                   ") appears to be used multiple times"
                endif
                row_col_elems(i_elem)%keyword=a_work
                row_col_elems(i_elem)%type=TOOLBAR_CARD_TYPE
                row_col_elems(i_elem)%y_pos=-1
                row_col_elems(i_elem)%link_num=0

                do while(xml_beg<=xml_end)
                  call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,   &
                   card_upper_case, i_end, i_delim1)
                  if(i_end==0) exit

                  select case (card_upper_case(2:i_delim1-1) )
                  case ("/TOOLBAR")
                    exit
                  case ("BUTTON")
                    card_upper_case(1:i_delim1)=" "
                    card_upper_case(i_end:i_end)=""
                    if(card_upper_case(i_end-1:i_end-1)=="/")                  &
                     card_upper_case(i_end-1:i_end-1)=" "

                    call ezgui_get_xml_keyword_info(                           &
                     card, card_upper_case, i_end,"KEYWORD",a_work)
                    if(len_trim(a_work)==0) then
                      print *,"Button entry does not have a keyword"
                      print *,"  "//trim(card)
                      cycle
                    endif

                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif
                    row_col_elems(i_elem)%type="P"
                    row_col_elems(i_elem)%link_num=3
                    row_col_elems(i_elem)%y_pos=-1
                    i_delim2=xml_beg
                    call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card, &
                     card_upper_case, i_end, i_delim1)
                    if(i_end==0) exit

                    if(card_upper_case(1:i_end)/="</BUTTON>") then
                      print *,"<BUTTON...> not followed by </BUTTON>"
                      print *,"  "//xml_data(i_delim2:xml_beg-1)
                    else
                      row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-10)
                      row_col_elems(i_elem)%length=xml_beg-10-i_delim2+1
                    endif

                  case default
                    call ezgui_bad_xml_warning("Toolbar",card(1:i_end),        &
                     card_upper_case(2:i_delim1-1), istat)
                    if(istat<0) return
                  end select
                end do

              case ("/TOOLBAR")
                print *,"</TOOLBAR> found without corresponding <TOOLBAR>"

              case ("MENUBAR")
                do while(xml_beg<=xml_end)
                  call ezgui_get_xml_token(xml_data, xml_beg, xml_end,         &
                   card, card_upper_case, i_end, i_delim1)
                  if(i_end==0) exit

                  select case (card_upper_case(2:i_delim1-1) )
                  case ("/MENUBAR")
                    exit
                  case ("/MENU")
                    cycle
                  case ("MENU")
                    i=n_elems+1
                    call ezgui_make_new_keyword("~MENU",i,a_work)
                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif

                    row_col_elems(i_elem)%type=MENUBAR_CARD_TYPE
                    row_col_elems(i_elem)%y_pos=-1
                    row_col_elems(i_elem)%link_num=0

                    i=ezgui_find_chars(card_upper_case,1,i_end,'LABEL="')+7
                    if(i<i_end) then
                      j=ezgui_find_chars(card_upper_case,i,i_end,'"')
                      row_col_elems(i_elem)%label=card(i:j-1)
                    else
                      print *,"Menu entry does not have a label"
                      print *,"  "//trim(card)
                      cycle
                    endif

                  case ("BUTTON")
                    card_upper_case(1:i_delim1)=" "
                    card_upper_case(i_end:i_end)=""
                    if(card_upper_case(i_end-1:i_end-1)=="/")                  &
                     card_upper_case(i_end-1:i_end-1)=" "

                    call ezgui_get_xml_keyword_info(                           &
                     card, card_upper_case, i_end,"KEYWORD",a_work)
                    if(len_trim(a_work)==0) then
                      print *,"Button entry does not have a keyword"
                      print *,"  "//trim(card)
                      cycle
                    endif

                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif
                    row_col_elems(i_elem)%type="P"
                    row_col_elems(i_elem)%link_num=3
                    row_col_elems(i_elem)%y_pos=-1
                    i_delim2=xml_beg

                    call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card, &
                     card_upper_case, i_end, i_delim1)
                    if(i_end==0) exit

                    if(card_upper_case(1:i_end)/="</BUTTON>") then
                      print *,"<BUTTON...> not followed by </BUTTON>"
                      print *,"  "//xml_data(i_delim2:xml_beg-1)
                    else
                      row_col_elems(i_elem)%label=xml_data(i_delim2:xml_beg-10)
                      row_col_elems(i_elem)%length=xml_beg-10-i_delim2+1
                    endif

                  case default
                    call ezgui_bad_xml_warning("Menubar/Menu",card(1:i_end),   &
                     card_upper_case(2:i_delim1-1), istat)
                    if(istat<0) return
                  end select
                end do

              case ("/MENUBAR")
                print *,"</MENUBAR> found without corresponding <MENUBAR>"

              case ("HELPPANEL")
                card_upper_case(1:i_delim1)=" "
                card_upper_case(i_end:i_end)=""
                if(card_upper_case(i_end-1:i_end-1)=="/")                      &
                 card_upper_case(i_end-1:i_end-1)=" "

                call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,  &
                 "KEYWORD",a_work)
                if(len_trim(a_work)==0) then
                  print *,"Button entry does not have a keyword"
                  print *,"  "//trim(card)
                  cycle
                endif

                n_old=n_elems
                call ezgui_get_row_col_elems_index(row_col_elems, n_elems,     &
                 n_screen, trim(a_work),i_elem)
                if(n_old==n_elems) then
                  print *,"WARNING: keyword("//trim(a_work)//                  &
                   ") appears to be used multiple times"
                endif
                row_col_elems(i_elem)%type="H"
                row_col_elems(i_elem)%link_num=0
                row_col_elems(i_elem)%y_pos=-1

                call ezgui_get_xml_keyword_info(card, card_upper_case, i_end,  &
                 "POSITION",a_work)

                call ezgui_chars_upper_case(a_work)
                select case(trim(a_work))
                case ("TOP")
                  a_work="top"
                case ("BOTTOM")
                  a_work="bottom"
                case ("TOPMOST")
                  a_work="topMost"
                case ("BOTTOMMOST")
                  a_work="bottomMost"
                case default
                  a_work="top"
                end select
                row_col_elems(i_elem)%label=a_work

                if(len_trim(card_upper_case)>0) then
                  print *,                                                     &
                 "The following parameters were not processed in HelpPanel card"
                  print *,"  "//trim(adjustl(card_upper_case))
                  print *,"  "//trim(card)
                endif

              case ("COMMANDAREA")
                do while (xml_beg<=xml_end)
                  call ezgui_get_xml_token(xml_data, xml_beg, xml_end, card,   &
                   card_upper_case, i_end, i_delim1)
                  if(i_end==0) exit

                  select case (card_upper_case(2:i_delim1-1) )
                  case ("/COMMANDAREA")
                    exit

                  case ("TOPAREACOMPONENT")
                    card_upper_case(1:i_delim1)=" "
                    card_upper_case(i_end:i_end)=""
                    if(card_upper_case(i_end-1:i_end-1)=="/")                  &
                     card_upper_case(i_end-1:i_end-1)=" "
                    i=n_elems+1
                    call ezgui_make_new_keyword("~TOP_AREA_COMPONENT",i,a_work)
                    call ezgui_get_xml_keyword_info(                           &
                     card, card_upper_case, i_end, "KEYWORD",a_work)

                    n_screen=n_screen+1
                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif
                    row_col_elems(i_elem)%type=TOP_AREA_CARD_TYPE
                    row_col_elems(i_elem)%link_num=0
                    row_col_elems(i_elem)%y_pos=-1

                    do while (xml_beg<=xml_end)
                      call ezgui_get_xml_token(                                &
                       xml_data, xml_beg, xml_end, card, card_upper_case,      &
                       i_end, i_delim1)
                      if(i_end==0) exit

                      select case (card_upper_case(2:i_delim1-1) )
                      case("/TOPAREACOMPONENT")
                        n_screen=n_screen+1
                        i=n_elems+1
                        call ezgui_make_new_keyword(                           &
                         "~END_TOP_AREA_COMPONENT",i,a_work)
                        n_old=n_elems
                        call ezgui_get_row_col_elems_index(                    &
                         row_col_elems, n_elems, n_screen, trim(a_work),i_elem)
                        if(n_old==n_elems) then
                          print *,"WARNING: keyword("//trim(a_work)//          &
                           ") appears to be used multiple times"
                        endif
                        row_col_elems(i_elem)%type=TOP_AREA_CARD_TYPE
                        row_col_elems(i_elem)%link_num=1
                        row_col_elems(i_elem)%y_pos=-1
                        exit

                      case ("SCROLLABLESCREEN")
                        print *,                                               &
                      "ScrollableScreens disallowed in TopAreaComponent-ignored"

                      case ("/SCROLLABLESCREEN")
                        print *,                                               &
                         "/ScrollableScreens disallowed in TopAreaComponent-"//&
                         "ignored"

                      case("SCREEN")
                        call ezgui_process_screen_xml_cards(                   &
                         row_col_elems,n_elems, card,card_upper_case,          &
                         xml_data, xml_beg,xml_end,n_screen,i_delim1,i_end)

                      case default
                        call ezgui_bad_xml_warning("TopAreaComponent",         &
                         card(1:i_end), card_upper_case(2:i_delim1-1), istat)
                        if(istat<0) return
                      end select
                    enddo

                  case ("BOTTOMAREACOMPONENT")
                    card_upper_case(1:i_delim1)=" "
                    card_upper_case(i_end:i_end)=""
                    if(card_upper_case(i_end-1:i_end-1)=="/")                  &
                     card_upper_case(i_end-1:i_end-1)=" "

                    i=n_elems+1
                    call ezgui_make_new_keyword(                               &
                     "~BOTTOM_AREA_COMPONENT",i,a_work)

                    n_screen=n_screen+1
                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif
                    row_col_elems(i_elem)%type=BOTTOM_AREA_CARD_TYPE
                    row_col_elems(i_elem)%link_num=0
                    row_col_elems(i_elem)%y_pos=-1

                    do while (xml_beg<=xml_end)
                      call ezgui_get_xml_token(                                &
                       xml_data, xml_beg, xml_end, card,card_upper_case,       &
                       i_end, i_delim1)
                      if(i_end==0) exit

                      select case (card_upper_case(2:i_delim1-1) )
                      case("/BOTTOMAREACOMPONENT")
                        n_screen=n_screen+1
                        i=n_elems+1
                        call ezgui_make_new_keyword(                           &
                         "~END_BOTTOM_AREA_COMPONENT",i,a_work)
                        n_old=n_elems
                        call ezgui_get_row_col_elems_index(                    &
                         row_col_elems, n_elems, n_screen, trim(a_work),i_elem)
                        if(n_old==n_elems) then
                          print *,"WARNING: keyword("//trim(a_work)//          &
                           ") appears to be used multiple times"
                        endif
                        row_col_elems(i_elem)%type=BOTTOM_AREA_CARD_TYPE
                        row_col_elems(i_elem)%link_num=1
                        row_col_elems(i_elem)%y_pos=-1
                        exit

                      case("SCREEN")
                        call ezgui_process_screen_xml_cards(                   &
                         row_col_elems,n_elems, card,card_upper_case,          &
                         xml_data, xml_beg,xml_end,n_screen,i_delim1,i_end)

                      case ("SCROLLABLESCREEN")
                        print *,                                               &
                       "ScrollableScreens disallowed in BottomAreaComponent"// &
                         "-ignored"

                      case ("/SCROLLABLESCREEN")
                        print *,                                               &
                       "/ScrollableScreens disallowed in BottomAreaComponent"//&
                         "-ignored"

                      case default
                        call ezgui_bad_xml_warning("BottomAreaComponent",      &
                         card(1:i_end), card_upper_case(2:i_delim1-1), istat)
                        if(istat<0) return
                      end select
                    enddo

                  case ("SCREEN")
                    call ezgui_process_screen_xml_cards(                       &
                     row_col_elems,n_elems, card, card_upper_case, xml_data,   &
                     xml_beg,xml_end,n_screen, i_delim1,i_end)

                  case ("/SCREEN")
                    print *,"</SCREEN> token found without <SCREEN>"

                  case ("SCROLLABLESCREEN")
                    n_screen=n_screen+1
                    i=n_elems+1
                    call ezgui_make_new_keyword("~SCROLLABLE_SCREEN",i,a_work)
                    n_old=n_elems
                    call ezgui_get_row_col_elems_index(row_col_elems, n_elems, &
                     n_screen, trim(a_work),i_elem)
                    if(n_old==n_elems) then
                      print *,"WARNING: keyword("//trim(a_work)//              &
                       ") appears to be used multiple times"
                    endif
                    row_col_elems(i_elem)%type=SCROLLABLE_SCREEN_CARD_TYPE
                    row_col_elems(i_elem)%link_num=0
                    row_col_elems(i_elem)%y_pos=-1

                    do while (xml_beg<=xml_end)
                      call ezgui_get_xml_token(                                &
                       xml_data, xml_beg, xml_end, card, card_upper_case,      &
                       i_end, i_delim1)
                      if(i_end==0) exit

                      select case (card_upper_case(2:i_delim1-1) )
                      case ("SCREEN")
                        call ezgui_process_screen_xml_cards(                   &
                         row_col_elems,n_elems, card,card_upper_case,          &
                         xml_data, xml_beg,xml_end,n_screen,i_delim1,i_end)

                      case ("/SCREEN")
                        print *,"</SCREEN> token found without <SCREEN>"

                      case ("/SCROLLABLESCREEN")
                        n_screen=n_screen+1
                        i=n_elems+1
                        call ezgui_make_new_keyword(                           &
                         "~END_SCROLLABLE_SCREEN",i,a_work)
                        n_old=n_elems
                        call ezgui_get_row_col_elems_index(                    &
                         row_col_elems, n_elems, n_screen, trim(a_work),i_elem)
                        if(n_old==n_elems) then
                          print *,"WARNING: keyword("//trim(a_work)//          &
                           ") appears to be used multiple times"
                        endif
                        row_col_elems(i_elem)%type=SCROLLABLE_SCREEN_CARD_TYPE
                        row_col_elems(i_elem)%link_num=1
                        row_col_elems(i_elem)%y_pos=-1
                        exit

                      case default
                        call ezgui_bad_xml_warning(                            &
                         "ScrollableScreen",card(1:i_end),                     &
                         card_upper_case(2:i_delim1-1), istat)
                        if(istat<0) return
                      end select
                    enddo

                  case ("/SCROLLABLESCREEN")
                    print *, &
                     "</ScrollableScreen> found without <ScrollableScreen>"

                  case default
                    call ezgui_bad_xml_warning("Window",card(1:i_end),         &
                     card_upper_case(2:i_delim1-1), istat)
                    if(istat<0) return
                  end select
                enddo

              case ("/COMMANDAREA")
                print *,"</COMMANDAREA> found without <COMMANDAREA>"

              case default
                call ezgui_bad_xml_warning("Window",card(1:i_end),             &
                 card_upper_case(2:i_delim1-1), istat)
                if(istat<0) return
              end select
            enddo

          case ("/WINDOW")
            print *,"</WINDOW token found without <WINDOW>"

          case default
            call ezgui_bad_xml_warning("GUI",card(1:i_end),                    &
             card_upper_case(2:i_delim1-1), istat)
            if(istat<0) return
          end select
        enddo

      case ("/GUI")
        print *, "</GUI> token found without <GUI>"

      case ("WINDOW")
        print *,"<WINDOW> token found without <GUI>"

      case default
        call ezgui_bad_xml_warning("Main",card(1:i_end),                       &
         card_upper_case(2:i_delim1-1), istat)
        if(istat<0) return
      end select
    enddo

! find positions of linked arrays and radio buttons

    do i_elem=1,n_elems
      if(row_col_elems(i_elem)%type=="R") then
        row_col_elems(i_elem)%x_pos=row_col_elems(i_elem+1)%x_pos
        row_col_elems(i_elem)%y_pos=row_col_elems(i_elem+1)%y_pos
      endif

      if(row_col_elems(i_elem)%type=="A") then
        i_row=row_col_elems(i_elem)%y_pos
        i_col=row_col_elems(i_elem)%x_pos
        j=row_col_elems(i_elem)%y_size
        cycle
      endif

      if(row_col_elems(i_elem)%link_num==2) then
        row_col_elems(i_elem)%x_pos=i_col
        row_col_elems(i_elem)%y_pos=i_row
        i_col=i_col+row_col_elems(i_elem)%length+1
        row_col_elems(i_elem)%y_size=j
      endif
    enddo

    call ezgui_sort_row_col_elems(row_col_elems, n_elems)
    return
  end subroutine ezgui_read_xml_file

!***********************************************************
!  print out warning of unrecognized XML entries
!
!  Written December 1999 by Charles C Burch
!***********************************************************
  subroutine ezgui_bad_xml_warning(mode,card, card_upper_case, istat)
    character (len=*), intent(in)  :: mode, card, card_upper_case
    integer, intent(out)          :: istat

    character (len=4)              :: prompt
    integer                        :: i

    print *,"Unrecognized XML element in "//mode//" mode | "//card_upper_case

    do i=1,len(card_upper_case)
      if(ichar(card_upper_case(i:i))<31)                                       &
       print *,"control character(",ichar(card_upper_case(i:i)),               &
       ") in position ",i
    enddo

    print *,"original input="//card
    call ezgui_char_prompt_edit(prompt,"stop","enter go to continue")
    call ezgui_chars_upper_case(prompt)
    istat=0
    if(prompt=="STOP") istat=-1
    return
  end subroutine ezgui_bad_xml_warning

!***********************************************************
!  inquire an xml file_name and save xml contents to it
!
!  Written July 1999 by Charles C Burch
!***********************************************************
  subroutine ezgui_save_xml_file(row_col_elems, n_elems, file_name, geopro)
    character (len=*), intent(out)  :: file_name
    type (row_col_elem), intent(in) :: row_col_elems(:)
    integer, intent(in)             :: n_elems
    logical, optional,intent(in)    :: geopro
    logical                         :: internal_geopro=.false.

    integer             :: istat,f_len
    character (len=240) :: a_work

    if(n_elems==0) then
      file_name=" "
      return
    endif
    if(present(geopro) ) then
      internal_geopro=geopro
    else
      internal_geopro=.false.
    endif
    120 call ezgui_get_filename_out(                                           &
     "Enter xml output file name[blanks to skip]:", file_name,istat)
    if(istat/=0) then
      print *,"Invalid file name specified-try again"
      goto 120
    endif
    f_len=len_trim(file_name)
    if(f_len==0)return

    a_work=file_name
    call ezgui_chars_upper_case(a_work)
    if(ezgui_find_chars(a_work,1,f_len,".LAY") <f_len ) then
      call ezgui_int_prompt_edit(istat,0,                                      &
       "File name has .lay extension, "//                                      &
       "enter 1 to write file, 0 to re-enter filename")
      if(istat/=1) goto 120
      call ezgui_write_layout_file(file_name,n_elems, row_col_elems, istat)
    else if                                                                    &
       (min(ezgui_find_chars(a_work,1,f_len,'.F90'),                           &
       ezgui_find_chars(a_work,1,f_len+1,'.F ')) < f_len) then
      print *,                                                                 &
       "Writing to .f90/F90 or .f/F files are not allowed-please try again"
      goto 120
    else
      if(internal_geopro) then
        call ezgui_write_geopro_xml_file(file_name,n_elems, row_col_elems, istat)
      else
        call ezgui_write_xml_file(file_name,n_elems, row_col_elems, istat)
      endif
    endif

    if(istat/=0) then
      print *,"Error in writing save file"
      file_name=" "
    else
      print *,n_elems, " field elements written to file"
    endif

    return
  end subroutine ezgui_save_xml_file

!*******************************************************************
! write screen layout file described by structure informatiom
!        Internal EzGUI usage
!
! Written DEcember 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_write_layout_file(file_name, n_elems, row_col_elems, istat)
    integer, intent(in)             :: n_elems
    type (row_col_elem), intent(in) :: row_col_elems(:)
    character (len=*), intent(in)   :: file_name
    integer, intent(out)            :: istat

    integer, parameter     :: NUM_LINES=5000
    integer                :: n_screen, n_row, i_row, i_elem, i_col
    integer                :: iline, i, j, n, n1
    character (len=256)    :: lines(NUM_LINES), line
    character (len=4)      :: a_num
    integer                :: n_vis, line_adjs(NUM_LINES), j_col, i_adj, f_len
    integer                :: n_help, i_help,j_help, ibeg, iend
    character              :: button_type*1, a_work*160, p_work*80
    logical                :: scroll_sw

    print *,                                                                   &
     "Warning-This does not necessarily produce an accurate file "//           &
     "for crammed screens"
    open(unit=12,file=file_name,status='unknown',iostat=istat)
    if(istat/=0) return

    lines=" "
    line_adjs=0
    iline=0
    n_screen=1
    n_row=0
    scroll_sw=.false.
    i_elem=0

    if(window_width>0 .or. window_height>0) then
      a_work='<window'
      if(window_width>0) then
        call ezgui_int_to_chars(window_width,a_num)
        a_work=trim(a_work)//' width='//trim(a_num)
      endif
      if(window_height>0) then
        call ezgui_int_to_chars(window_height,a_num)
        a_work=trim(a_work)//' height='//trim(a_num)
      endif
      iline=iline+1
      call ezgui_put_viewline(lines(iline),trim(a_work)//'>',0)
    endif

    do while(i_elem<n_elems)
      i_elem=i_elem+1
      n_vis=0
      i_row=row_col_elems(i_elem)%y_pos+1
      i_col=row_col_elems(i_elem)%x_pos+1
      if(row_col_elems(i_elem)%screen_num==-9) cycle

      select case(row_col_elems(i_elem)%type)
      case (TOP_AREA_CARD_TYPE)                 !TopAreaComponent
        iline=iline+1+n_row
        n_row=0
        if(row_col_elems(i_elem)%link_num==0) then
          call ezgui_put_viewline(lines(iline),"<TA>",i_elem)
        else
          call ezgui_put_viewline(lines(iline),'</TA>',i_elem)
        endif
        cycle

      case (BOTTOM_AREA_CARD_TYPE)              !BottomAreaComponent
        iline=iline+1+n_row
        n_row=0
        if(row_col_elems(i_elem)%link_num==0) then
          call ezgui_put_viewline(lines(iline),"<BA>",i_elem)
        else
          call ezgui_put_viewline(lines(iline),'</BA>',i_elem)
        endif
        cycle

      case ("H")                              !Help Info
        iline=iline+1
        call ezgui_put_viewline(lines(iline),"<HP "//                          &
         trim(row_col_elems(i_elem)%keyword)//" "                              &
         //trim(row_col_elems(i_elem)%label)//">",i_elem)
        cycle

      case (SCROLLABLE_SCREEN_CARD_TYPE)    !Scrollable
        if(row_col_elems(i_elem)%link_num==0) then
          scroll_sw=.true.
        else
          scroll_sw=.false.
        endif
        cycle

      case (MENUBAR_CARD_TYPE)              !Menubar
        do while(i_elem<=n_elems)
          select case(row_col_elems(i_elem)%type)
          case (MENUBAR_CARD_TYPE)
            iline=iline+1
            call ezgui_put_viewline(lines(iline),"<MB "//                      &
             trim(row_col_elems(i_elem)%label)//">",i_elem)
          case ("P")
            iline=iline+1
            call ezgui_form_field_modifiers(                            &
             trim(row_col_elems(i_elem)%label),                                &
             trim(row_col_elems(i_elem)%label),                                &
             "", row_col_elems(i_elem), a_work, n, i_adj)
            call ezgui_put_viewline(lines(iline),a_work(1:n),i_elem)
          case default
            exit
          end select
          i_elem=i_elem+1
        enddo

        iline=iline+1
        call ezgui_put_viewline(lines(iline),"</MB>",i_elem)
        n_row=0
        i_row=0
        i_elem=i_elem-1
        cycle

      case (TOOLBAR_CARD_TYPE)              !Toolbar
        iline=iline+1
        call ezgui_put_viewline(lines(iline),"<TB>",i_elem)
        i_elem=i_elem+1

        do while(i_elem<=n_elems)
          if(row_col_elems(i_elem)%type/="P") exit
          iline=iline+1
          call ezgui_form_field_modifiers(                              &
           trim(row_col_elems(i_elem)%label),                                  &
           trim(row_col_elems(i_elem)%label),                                  &
           "", row_col_elems(i_elem), a_work, n, i_adj)
          call ezgui_put_viewline(lines(iline),a_work(1:n),i_elem)
          i_elem=i_elem+1
        enddo

        iline=iline+1
        call ezgui_put_viewline(lines(iline),"</TB>",i_elem)
        n_row=0
        i_row=0
        i_elem=i_elem-1
        cycle

      case (END_SCREEN_CARD_TYPE)          !End of screen
        iline=iline+n_row
        n_row=0
        cycle

      case (NEW_SCREEN_CARD_TYPE)          !New Screen
        n_screen=row_col_elems(i_elem)%screen_num
        line="<NS "//row_col_elems(i_elem)%label
        if(scroll_sw) line(2:3)="SS"       !scrollable screen

        if(row_col_elems(i_elem)%x_size>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%x_size,a_num)
          line=trim(line)//"/NC="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%y_size>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%y_size,a_num)
          line=trim(line)//"/NR="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%cell_width>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%cell_width,a_num)
          line=trim(line)//"/CW="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%cell_height>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%cell_height,a_num)
          line=trim(line)//"/CH="//adjustl(a_num)
        endif

        iline=iline+n_row+1
        call ezgui_put_viewline(lines(iline),trim(line)//'>', i_elem)

        i=i_elem+1
        do while(i<=n_elems)                           !check for popupmenu
          if(row_col_elems(i)%type==END_SCREEN_CARD_TYPE) exit
          if(row_col_elems(i)%type==POPUP_MENU_CARD_TYPE) then
            iline=iline+1
            call ezgui_put_viewline(lines(iline),"<PM "//                      &
             trim(row_col_elems(i)%keyword)//">",i)
            i=i+1

            do while(i<=n_elems)
              if(row_col_elems(i)%type==POPUP_MENU_CARD_TYPE) exit
              iline=iline+1
              call ezgui_form_field_modifiers(                          &
               trim(row_col_elems(i)%label), trim(row_col_elems(i)%label),     &
               "", row_col_elems(i), a_work, n, i_adj)
              call ezgui_put_viewline(lines(iline),a_work(1:n),i)
              i=i+1
            enddo

            iline=iline+1
            call ezgui_put_viewline(lines(iline),"</PM>",i_elem)
            exit
          else
            i=i+1
          endif
        enddo
        n_row=0
        i_row=0
        cycle

      case ("A")
        cycle

      case ("R")
        cycle

      case (POPUP_MENU_CARD_TYPE)
        i_elem=i_elem+1        !skip definition of pop-up menu-handled with <NS>
        do while(i_elem<n_elems)
          if(row_col_elems(i_elem)%type==POPUP_MENU_CARD_TYPE) exit
          i_elem=i_elem+1
        enddo
        cycle

      case ("B")
        n=row_col_elems(i_elem)%x_size
        i=row_col_elems(i_elem)%y_size

        if(n<1) then
          print *,"Border("//trim(row_col_elems(i_elem)%keyword)//             &
           " has xSize<1--set to 1"
          n=1
        endif

        if(i<1) then
          print *,"Border("//trim(row_col_elems(i_elem)%keyword)//             &
           " has ySize<1--set to 1"
          i=1
        endif

        line(:n)=repeat('-', n)
        line(1:1)="`"
        line(n:n)='+'
        j_col=i_col+line_adjs(iline+i_row)
        call ezgui_put_viewline(                                               &
         lines(iline+i_row)(j_col:j_col+n-1),line(1:n), i_elem)
!         j_col=i_col+line_adjs(iline+i_row+i-1)
        call ezgui_put_viewline(                                               &
         lines(iline+i_row+i-1)(j_col:j_col+n-1),line(1:n),i_elem)

        do j=3,i
!           j_col=i_col+line_adjs(iline+i_row+j-2)
          call ezgui_put_viewline(                                             &
           lines(iline+i_row+j-2)(j_col:j_col),"|", i_elem)
        enddo
        n_vis=i-1

      case ("P","T","K")
        if(i_row<0) cycle
        n=row_col_elems(i_elem)%x_size
        j_col=i_col+line_adjs(iline+i_row)
        if(lines(iline+i_row)(j_col:j_col+n+1)/=' ') then

!  Pushbutton definition overlaps previous information-try to reposition

          do j=1,256
            if(j_col+j+n+1>256) then
              print *,"WARNING: Unable to reposition Pushbutton("              &
               //trim(row_col_elems(i_elem)%keyword)//") to fit"
            endif
            if(lines(iline+i_row)(j_col+j:j_col+j+n+1)==' ') then
              print *,"WARNING: Pushbutton("//                                 &
               trim(row_col_elems(i_elem)%keyword)                             &
               //") was repositioned(",j," space(s)) to fit"
              j_col=j_col+j
              line_adjs(iline+i_row)=line_adjs(iline+i_row)+j
              exit
            endif
          enddo
        endif

        if(row_col_elems(i_elem)%link_num==3) then
          button_type='R'
        else
          button_type='P'
        endif

        ibeg=1
        p_work=row_col_elems(i_elem)%label
        j=len_trim(p_work)
        call ezgui_chars_upper_case(p_work(1:j))

        do i=1,row_col_elems(i_elem)%y_size
          call ezgui_find_unicode_value(p_work,ibeg,j,10,iend,f_len)
          if(i==1) then
            call ezgui_unspace_label(                                          &
             row_col_elems(i_elem)%label(1:iend-1),n, line)
            call ezgui_form_field_modifiers(                            &
             line(1:n)//"`"//button_type,                                      &
             trim(row_col_elems(i_elem)%label), "/XSF/YSF/SY/EY/C/",           &
             row_col_elems(i_elem), a_work, n1, i_adj)
            line_adjs(iline+i_row)=line_adjs(iline+i_row)+i_adj
          else
            a_work=row_col_elems(i_elem)%label(ibeg:iend-1)
            n1=iend-ibeg
          endif

          call ezgui_put_viewline(lines(iline+i_row+i-1)(j_col:j_col+n1-1),    &
           a_work(1:n1), i_elem)
          ibeg=min(j+1,iend+f_len)
        enddo

        n_vis=row_col_elems(i_elem)%y_size-1

      case ("L")
        j_col=i_col+line_adjs(iline+i_row)
        n=row_col_elems(i_elem)%x_size
        call ezgui_unspace_label(row_col_elems(i_elem)%label(1:n),n,line)
        call ezgui_form_field_modifiers(                                &
         line(1:n), "","/L/EN/SY/XSF/YSF/", row_col_elems(i_elem), a_work,     &
         n, i_adj)
        call ezgui_put_viewline(                                               &
         lines(iline+i_row)(j_col:j_col+n-1),a_work(1:n), i_elem)
        line_adjs(iline+i_row)=line_adjs(iline+i_row)+i_adj

      case("S", "F", "I", "X", "O","C")
        j_col=i_col+line_adjs(iline+i_row)
        i_adj=0

!      if(row_col_elems(i_elem)%link_num<0) then
        f_len=row_col_elems(i_elem)%x_size
!      else
!        f_len=row_col_elems(i_elem)%length
!      endif

        line(1:1)=row_col_elems(i_elem)%type
        !if(row_col_elems(i_elem)%editable=="no " .and. line(1:1)=='S')        &
        ! line(1:1)="X"
        if(f_len>0) line(2:f_len)=repeat(line(1:1), f_len-1)
        line(1:1)='`'
        if(row_col_elems(i_elem)%link_num<0) then

!non array element

          j=j_col-2                                       !see if it has a label
          do while(j>=1.and. lines(iline+i_row)(j:j+1)/='  ')
            j=j-1
          enddo

          if(j==0) then
            j=1
          else
            j=j+2
          endif

          if(j<j_col) then
            line(f_len+1:)=lines(iline+i_row)(j:j_col-1)
          endif
          if(len_trim(line(f_len+1:))==0) line(f_len+1:)='~'

          call ezgui_form_field_modifiers(                              &
           line(1:f_len), trim(line(f_len+1:)),                                &
           "/XSF/YSF/SY/EY/C/", row_col_elems(i_elem), a_work, n, i_adj)
          call ezgui_put_viewline(                                             &
           lines(iline+i_row)(j_col:j_col+n-1),a_work(1:n), i_elem)
          line_adjs(iline+i_row)=line_adjs(iline+i_row)+i_adj
        else

! array element

          call ezgui_form_field_modifiers(                              &
           trim(row_col_elems(i_elem)%label),                                  &
           trim(row_col_elems(i_elem)%label),"/XST/YSF/SY/EY/C/",              &
           row_col_elems(i_elem), a_work, n, i_adj)

          if(f_len>=n) then
            n=f_len
            i_adj=0
          else
            i_adj=n-f_len
            line(f_len+1:n)=repeat('-',i_adj)
            f_len=n
          endif

          call ezgui_put_viewline(                                             &
           lines(iline+i_row)(j_col:j_col+n-1),a_work(1:n), i_elem)
          line_adjs(iline+i_row)=line_adjs(iline+i_row)+i_adj

          do i=2,row_col_elems(i_elem)%y_size
            call ezgui_put_viewline(lines(iline+i_row+i-1)(j_col:j_col+n-1),   &
             line(:n), i_elem)
            line_adjs(iline+i_row+i-1)=line_adjs(iline+i_row+i-1)+i_adj
            n_vis=n_vis+1
          enddo
        endif

      case default
      end select

      n_row=max(n_row,i_row+n_vis)
    enddo

! Output help and tip information

    n_row=n_row+iline
    do i_elem=1, n_elems
      n_help=row_col_elems(i_elem)%help_size

      if(n_help>0 .or. len_trim(row_col_elems(i_elem)%tip_info)>0) then
        n_row=n_row+1
        if(row_col_elems(i_elem)%keyword=="~PROCESS_HELP") then
          lines(n_row)="<HELP>"
        else
          lines(n_row)=                                                        &
           "<HELP "//trim(row_col_elems(i_elem)%keyword)//">"//                &
           trim(row_col_elems(i_elem)%tip_info)
        endif

        if(n_help>0) then
          i_help=1

          do while(i_help<=n_help)
            i=0
            n_row=n_row+1
            do j_help=i_help, n_help        !find end-of-line deliminitor
              if(row_col_elems(i_elem)%help_info(j_help)==char(0)) exit
              i=i+1
              lines(n_row)(i:i)=row_col_elems(i_elem)%help_info(j_help)
            enddo
            i_help=j_help+1
          enddo
        endif

      endif
    enddo

    do i=1,n_row
      write(12,'(a)') trim(lines(i))
    enddo
    close(12)
    return
  end subroutine ezgui_write_layout_file

!***********************************************************************
! Convert any spaces greater than two to ~'a
!
! Written December 1999 by Charles C Burch
!***********************************************************************
  subroutine ezgui_unspace_label(lab_in, n, lab_out)
    character (len=*), intent(in)   :: lab_in
    character (len=*), intent(out)  :: lab_out
    integer, intent(in)             :: n

    integer                          :: i,j

    lab_out=lab_in

    i=ezgui_find_chars(lab_out,1,n,'  ')
    do while(i<n)
      j=ezgui_find_not_chars(lab_out,i,n,' ')
      lab_out(i:j-1)=repeat('~',j-i)
      i=ezgui_find_chars(lab_out,j,n,'  ')
    enddo
    return
  end subroutine ezgui_unspace_label

!*************************************************************
! Form Layout field modifiers
!
! Written December 1999 by Charles C Burch
!*************************************************************
  subroutine ezgui_form_field_modifiers(s_in, lab, def, row_col_work,   &
     str, n_str, i_adj)
    type (row_col_elem), intent(in) :: row_col_work
    character (len=*), intent(out)  :: str
    character (len=*), intent(in)   :: s_in, def, lab
    integer, intent(out)            :: n_str, i_adj

    integer                    :: i
    character                  :: a_work*80

    n_str=0
    select case (row_col_work%type)              !check for non-default keyword
    case ('B',NEW_SCREEN_CARD_TYPE,'L')

    case default
      if(len_trim(lab)>0) then
        call ezgui_make_keyword(lab,a_work)
        i=len_trim(row_col_work%keyword)
!         print *,"form..",lab,'|',trim(a_work),'|',row_col_work%keyword(1:i)
        if(trim(a_work)/=row_col_work%keyword(1:i) ) then
          str(n_str+1:n_str+i)=row_col_work%keyword
          n_str=n_str+i
        endif
      endif
    end select

    if(len_trim(def)>0) then
      if(row_col_work%alignment=="left") then   ! see if it has alignment = left
        if(index(def,'/L/')==0) then
          str(n_str+1:n_str+2)='/L'
          n_str=n_str+2
        endif
      endif

      if(row_col_work%alignment=="right") then ! see if it has alignment = right
        if(index(def,'/R/')==0) then
          str(n_str+1:n_str+2)='/R'
          n_str=n_str+2
        endif
      endif

      if(row_col_work%alignment=="center") then! see if it has alignment=center
        if(index(def,'/C/')==0) then
          str(n_str+1:n_str+2)='/C'
          n_str=n_str+2
        endif
      endif

      if(row_col_work%x_stretch=="false") then  ! see if it has xstretch = false
        if(index(def,'/XSF/')==0) then
          str(n_str+1:n_str+4)='/XSF'
          n_str=n_str+4
        endif
      endif

      if(row_col_work%x_stretch=="true") then    ! see if it has xstretch = true
        if(index(def,'/XST/')==0) then
          str(n_str+1:n_str+4)='/XST'
          n_str=n_str+4
        endif
      endif

      if(row_col_work%y_stretch=="true") then    ! see if it has ystretch = true
        if(index(def,'/YST/')==0) then
          str(n_str+1:n_str+4)='/YST'
          n_str=n_str+4
        endif
      endif

      if(row_col_work%y_stretch=="false") then  ! see if it has ystretch = false
        if(index(def,'/YSF/')==0) then
          str(n_str+1:n_str+4)='/YSF'
          n_str=n_str+4
        endif
      endif

      if(row_col_work%editable=="no") then       ! see if it has editable = no
        if(index(def,'/EN/')==0) then
          str(n_str+1:n_str+3)='/EN'
          n_str=n_str+3
        endif
      endif

      if(row_col_work%editable=="yes") then      ! see if it has editable = no
        if(index(def,'/EY/')==0) then
          str(n_str+1:n_str+3)='/EY'
          n_str=n_str+3
        endif
      endif

      if(row_col_work%sensitive=="no") then      ! see if it has sensitive = no
        if(index(def,'/SN/')==0) then
          str(n_str+1:n_str+3)='/SN'
          n_str=n_str+3
        endif
      endif

      if(row_col_work%sensitive=="yes") then     ! see if it has sensitive = no
        if(index(def,'/SY/')==0) then
          str(n_str+1:n_str+3)='/SY'
          n_str=n_str+3
        endif
      endif

      if(row_col_work%y_size==2) then  ! see if it has y_size=2- default 1
        str(n_str+1:n_str+2)='/2'
        n_str=n_str+2
      endif
    endif

    if(n_str>0) then
      str='['//str(1:n_str)//']'//s_in
      i_adj=n_str+2
      n_str=len_trim(str)
    else
      str=s_in
      i_adj=0
      n_str=len_trim(str)
    endif
    return
  end subroutine ezgui_form_field_modifiers

!*****************************************************************
! Compares an array of characters (c_array) starting at index i_beg
!   with the character string str
!   return 0 if equal and -1 if not equal
!
! Written January 2000 by Charles C Burch
!*****************************************************************
  integer function ezgui_charstr_check(c_array, i_beg, str)
    character, intent(in)         :: c_array(:)
    character (len=*), intent(in) :: str
    integer, intent(in)           :: i_beg

    integer   :: n

    n=len(str)
    do while(n>0)
      if(c_array(i_beg+n-1)/=str(n:n)) then
        ezgui_charstr_check=-1
        return
      endif
      n=n-1
    enddo
    ezgui_charstr_check=0
    return
  end function ezgui_charstr_check

!***************************************************************
! write xml file from row_col_elems structure
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_write_xml_file(file_name, n_elems, row_col_elems, istat)
    character (len=*), intent(in)   :: file_name
    integer, intent(in)             :: n_elems
    integer, intent(out)            :: istat
    type (row_col_elem), intent(in) :: row_col_elems(:)

    character (len=3)     :: a_y_pos,a_x_pos,a_field_length,a_x_size,a_y_size
    character (len=4)     :: a_buff
    character (len=20)    :: field_type, colsz
    character (len=32)    :: window_name
    integer               :: n_help, j, nrows, ncols
    integer               :: n_screen, n_screens, i_elem, link_last,ibeg,iend
    integer               :: n_indent, f_beg, f_end, n_u_lab
    character (len=256)   :: a_work, u_label
    logical               :: empty_screen, ta_mode, ba_mode

    open(unit=12,file=file_name,status='unknown',iostat=istat)
    if(istat/=0) return

! Header stuff for XML file
    write (12,'(a)') '<?xml version="1.0"?>'
    write (12,'(a)') '<!DOCTYPE Gui SYSTEM "gui.dtd">'
    write (12,'(a)') '<Gui>'

    f_end=len_trim(file_name)
    f_beg=0
    ba_mode=.false.                !true, if in back area component mode
    ta_mode=.false.                !true, if in top area compionent mode

    do j=1, f_end
      if(file_name(j:j)=='/') f_beg=j
    enddo
    f_end=ezgui_find_chars(file_name,f_beg+1,f_end,'.')
    window_name=file_name(f_beg+1:f_end-1)

    select case(window_dialog)
    case (0)
      a_work=' <Window keyword="'//trim(window_name)//'"'
    case(1)
      a_work=' <Dialog keyword="'//trim(window_name)//'"'
    case(2)
      a_work=' <Dialog keyword="'//trim(window_name)//'"'//' modal="true"'
    end select

    if(window_width>0) then
      call ezgui_int_to_chars(window_width,a_buff)
      a_work=trim(a_work)//' width="'//trim(a_buff)//'"'
    endif

    if(window_height>0) then
      call ezgui_int_to_chars(window_height,a_buff)
      a_work=trim(a_work)//' height="'//trim(a_buff)//'"'
    endif
    write (12,'(a)') trim(a_work)//'>'

    n_screens=row_col_elems(n_elems)%screen_num
    ibeg=1
    do while(ibeg<=n_elems)
      if(row_col_elems(ibeg)%screen_num==-9) then
! --- skip screen -9--has control information
        ibeg=ibeg+1
        cycle
      endif
      if(row_col_elems(ibeg)%screen_num>0) exit

      select case(row_col_elems(ibeg)%type)
      case (TOOLBAR_CARD_TYPE)                !Process toolbar
        write (12,'(a)') '  <Toolbar>'
        ibeg=ibeg+1

        do while(ibeg<=n_elems)
          if(row_col_elems(ibeg)%type/="P") exit

          call ezgui_convert_str_to_unicode(                                   &
           row_col_elems(ibeg)%label,u_label,n_u_lab)
          if(n_u_lab.lt.0) then
            print *,"Warning: label truncated when converted to unicode"
            print *,"  "//trim(row_col_elems(ibeg)%label)
            n_u_lab=len_trim(u_label)
          endif
          write(12,'(a)')                                                      &
           '   <Button keyword="'//trim(row_col_elems(ibeg)%keyword)//         &
           '">'//u_label(1:n_u_lab)//'</Button>'
          ibeg=ibeg+1
        enddo
        write(12,'(a)') '  </Toolbar>'
        cycle

      case (MENUBAR_CARD_TYPE)               !Process menubar
        write (12,'(a)') '  <MenuBar>'
        call ezgui_convert_str_to_unicode(                                     &
         row_col_elems(ibeg)%label,u_label,n_u_lab)
        if(n_u_lab.lt.0) then
          print *,"Warning: label truncated when converted to unicode"
          print *,"  "//trim(row_col_elems(ibeg)%label)
          n_u_lab=len_trim(u_label)
        endif
        write (12,'(a)') '   <Menu label="'//u_label(1:n_u_lab)//'">'

        ibeg=ibeg+1
        do while(ibeg<=n_elems)
          select case(row_col_elems(ibeg)%type)
          case ("P")
            call ezgui_convert_str_to_unicode(                                 &
             row_col_elems(ibeg)%label,u_label,n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(ibeg)%label)
              n_u_lab=len_trim(u_label)
            endif
            write(12,'(a)')                                                    &
             '    <Button keyword="'//trim(row_col_elems(ibeg)%keyword)//      &
             '">'//u_label(1:n_u_lab)//'</Button>'
          case (MENUBAR_CARD_TYPE)
            write (12,'(a)') '   </Menu>'
            call ezgui_convert_str_to_unicode(                                 &
             row_col_elems(ibeg)%label,u_label,n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(ibeg)%label)
              n_u_lab=len_trim(u_label)
            endif
            write (12,'(a)')                                                   &
             '   <Menu label="'//u_label(1:n_u_lab)//'">'
          case default
            exit
          end select
          ibeg=ibeg+1
        enddo

        write(12,'(a)') '   </Menu>'
        write(12,'(a)') '  </MenuBar>'
        cycle

      case ("H")
        write(12,'(a)')                                                        &
         '  <HelpPanel keyword="'//trim(row_col_elems(ibeg)%keyword)//         &
         '" position="'//trim(row_col_elems(ibeg)%label)//'"/>'

      case default
      end select
      ibeg=ibeg+1
    enddo

! Now output the fields
    write (12,'(a)') '  <CommandArea keyword="CommandArea">'
    n_indent=3
    do n_screen=1,n_screens
      nrows=24
      if(ta_mode.or.ba_mode) nrows=1
      ncols=10
      empty_screen=.true.

      do iend=ibeg,n_elems
        if(row_col_elems(iend)%screen_num/=n_screen) exit
        if(row_col_elems(iend)%type==END_SCREEN_CARD_TYPE) cycle
        ncols=max(ncols,row_col_elems(iend)%x_pos+row_col_elems(iend)%x_size-1)
        nrows=max(nrows, row_col_elems(iend)%y_pos+row_col_elems(iend)%y_size)
      enddo
      ncols=ncols+3
      if(iend==ibeg) cycle           !skip empty screens

      iend=iend-1
!   write(12,*) 'new screen..nscreen, ibeg,iend=',n_screen,ibeg,iend
      link_last=-1

      do i_elem =ibeg, iend
        if(link_last>=2) then
          if(row_col_elems(i_elem)%link_num<=1) then
            n_indent=n_indent-1

            select case (link_last)
            case (2)
              write(12,'(a)') repeat(' ',n_indent)//'</ArraySet>'
            case(3)
              write(12,'(a)') repeat(' ',n_indent)//'</RadioButtons>'
            case (6)
              write(12,'(a)') repeat(' ',n_indent)//'</PopupMenu>'
            end select
            link_last=-1
          endif
        endif

        select case(row_col_elems(i_elem)%type)
        case ("0")                              !Null
          empty_screen=.false.
          cycle

        case (TOP_AREA_CARD_TYPE)               !Top Area Component
          if(row_col_elems(i_elem)%link_num==0) then
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<TopAreaComponent>'
            n_indent=n_indent+1
            ta_mode=.true.
          else
            n_indent=n_indent-1
            write(12,'(a)') repeat(' ',n_indent)//'</TopAreaComponent>'
            ta_mode=.false.
          endif
          cycle

        case (BOTTOM_AREA_CARD_TYPE)           !Bottom Area Component
          if(row_col_elems(i_elem)%link_num==0) then
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<BottomAreaComponent>'
            n_indent=n_indent+1
            ba_mode=.true.
          else
            n_indent=n_indent-1
            write(12,'(a)') repeat(' ',n_indent)//'</BottomAreaComponent>'
            ba_mode=.false.
          endif
          cycle

        case (SCROLLABLE_SCREEN_CARD_TYPE)       !Scrollable Screen
          if(row_col_elems(i_elem)%link_num==0) then
            write(12,'(a)') repeat(' ',n_indent)// '<ScrollableScreen>'
            n_indent=n_indent+1
          else
            n_indent=n_indent-1
            write(12,'(a)') repeat(' ',n_indent)//'</ScrollableScreen>'
          endif
          cycle

        case (POPUP_MENU_CARD_TYPE)             !Popup Menu
          if(row_col_elems(i_elem)%link_num==0) then
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<PopupMenu keyword="'//trim(row_col_elems(i_elem)%keyword)//'">'
            n_indent=n_indent+1
          else
            n_indent=n_indent-1
            write(12,'(a)') repeat(' ',n_indent)//'</PopupMenu>'
          endif
          cycle

        case (END_SCREEN_CARD_TYPE)              !end of screen
          cycle

        case (NEW_SCREEN_CARD_TYPE)              !start of new screen
          if(row_col_elems(i_elem)%x_size>0) ncols=row_col_elems(i_elem)%x_size
          if(row_col_elems(i_elem)%y_size>0) nrows=row_col_elems(i_elem)%y_size
          call ezgui_int_to_chars(ncols,a_x_size)
          call ezgui_int_to_chars(nrows,a_y_size)
          call ezgui_convert_str_to_unicode(                                   &
           row_col_elems(i_elem)%label,u_label,n_u_lab)
          if(n_u_lab.lt.0) then
            print *,"Warning: label truncated when converted to unicode"
            print *,"  "//trim(row_col_elems(i_elem)%label)
            n_u_lab=len_trim(u_label)
          endif
          a_work='<Screen keyword="'//trim(row_col_elems(i_elem)%keyword)//    &
           '" title="'//u_label(1:n_u_lab)//                                   &
           '" rows="'//trim(a_y_size)//'" columns="'//trim(a_x_size)//'"'

          if(row_col_elems(i_elem)%cell_width>0) then
            call   ezgui_int_to_chars(row_col_elems(i_elem)%cell_width,a_x_size)
            a_work=trim(a_work)//' minCellWidth="'//trim(a_x_size)//'"'
          endif

          if(row_col_elems(i_elem)%cell_height>0) then
            call ezgui_int_to_chars(row_col_elems(i_elem)%cell_height,a_y_size)
            a_work=trim(a_work)//' minCellHeight="'//trim(a_y_size)//'"'
          endif
          write (12,'(a)') repeat(' ',n_indent)//trim(a_work)//'>'

          n_indent=n_indent+1
          link_last=-1
          empty_screen=.false.
          cycle

        case default
        end select

        !    Edit Area Entries

        call ezgui_int_to_chars(row_col_elems(i_elem)%length,a_field_length)
        call ezgui_convert_field_type_code(                                    &
         row_col_elems(i_elem)%type, field_type)

        select case(row_col_elems(i_elem)%link_num)
        case (-1)
          select case (row_col_elems(i_elem)%type)
          case ("L")
            call ezgui_convert_str_to_unicode(row_col_elems(i_elem)%label,     &
             u_label,n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(i_elem)%label)
              n_u_lab=len_trim(u_label)
            endif
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<Label keyword="'//trim(row_col_elems(i_elem)%keyword)//         &
             '" alignment="'//trim(row_col_elems(i_elem)%alignment)//          &
             '">'//u_label(1:n_u_lab)//'</Label>'

          case("B")
            if(len_trim(row_col_elems(i_elem)%label).eq.0) then
              write(12,'(a)') repeat(' ',n_indent)//                           &
               '<Border keyword="'//trim(row_col_elems(i_elem)%keyword)//'"/>'
            else
              write(12,'(a)') repeat(' ',n_indent)//                           &
               '<Border keyword="'//trim(row_col_elems(i_elem)%keyword)//      &
               '" title="'//trim(row_col_elems(i_elem)%label)//'"/>'
            endif

          case ("I","F","S","X","C")
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<Field keyword="'//trim(row_col_elems(i_elem)%keyword)//         &
             '" type="'//trim(field_type)//                                    &
             '" maxLength="'//trim(a_field_length)//                           &
             '" editable="'//trim(row_col_elems(i_elem)%editable)//            &
             '" sensitive="'//trim(row_col_elems(i_elem)%sensitive)//'"/>'

          case ("T","K")
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<Field keyword="'//trim(row_col_elems(i_elem)%keyword)//         &
             '" type="'//trim(field_type)//                                    &
             '" maxLength="'//trim(a_field_length)//                           &
             '" sensitive="'//trim(row_col_elems(i_elem)%sensitive)//'"/>'

          case default
            print *,"Error(-1)-type not recognized:"//row_col_elems(i_elem)%type
          end select
          link_last=-1

        case (0)
          select case (row_col_elems(i_elem)%type)
          case ("A")
            call ezgui_convert_str_to_unicode(                                 &
             row_col_elems(i_elem)%label,u_label, n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(i_elem)%label)
              n_u_lab=len_trim(u_label)
            endif
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<ArraySet keyword="'//trim(row_col_elems(i_elem)%keyword)        &
             //'" label="'//u_label(1:n_u_lab)//'">'
            link_last=2
            n_indent=n_indent+1

          case ("R")
            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<RadioButtons keyword="'//trim(row_col_elems(i_elem)%keyword)//  &
             '">'
            link_last=3
            n_indent=n_indent+1

          case default
            print *,"Error(00)-type not recognized:"//row_col_elems(i_elem)%type
          end select

        case (1)
          select case (row_col_elems(i_elem)%type)
          case ("I","F","S","X","O")
            if(row_col_elems(i_elem)%x_size.lt.row_col_elems(i_elem)%length)then
              call ezgui_int_to_chars(row_col_elems(i_elem)%x_size,a_x_size)
              colsz=' columnSize="'//trim(a_x_size)//'"'
            else
              colsz=""
            endif

            call ezgui_convert_str_to_unicode(                                 &
             row_col_elems(i_elem)%label,u_label, n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(i_elem)%label)
              n_u_lab=len_trim(u_label)
            endif

            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<Array keyword="'//trim(row_col_elems(i_elem)%keyword)           &
             //'" type="'//trim(field_type)//                                  &
             '" maxLength="'//trim(a_field_length)//                           &
             '" columnName="'//u_label(1:n_u_lab)//                            &
             '" editable="'//trim(row_col_elems(i_elem)%editable)//            &
             '" sensitive="'//trim(row_col_elems(i_elem)%sensitive)//'"'//     &
             trim(colsz)//'/>'

          case ("P")
            call ezgui_convert_str_to_unicode(                                 &
             row_col_elems(i_elem)%label,u_label,n_u_lab)
            if(n_u_lab.lt.0) then
              print *,"Warning: label truncated when converted to unicode"
              print *,"  "//trim(row_col_elems(i_elem)%label)
              n_u_lab=len_trim(u_label)
            endif

            write(12,'(a)') repeat(' ',n_indent)//                             &
             '<Button keyword="'//trim(row_col_elems(i_elem)%keyword)//        &
             '">'//u_label(1:n_u_lab)//'</Button>'

          case default
            print *,"Error(01)-type not recognized:"//row_col_elems(i_elem)%type
          end select

        case (2)
          call ezgui_convert_str_to_unicode(                                   &
           row_col_elems(i_elem)%label,u_label, n_u_lab)
          if(n_u_lab.lt.0) then
            print *,"Warning: label truncated when converted to unicode"
            print *,"  "//trim(row_col_elems(i_elem)%label)
            n_u_lab=len_trim(u_label)
          endif

          call ezgui_int_to_chars(row_col_elems(i_elem)%x_size,colsz)
          write(12,'(a)') repeat(' ',n_indent)//                               &
           '<Array keyword="'//trim(row_col_elems(i_elem)%keyword)             &
           //'" type="'//trim(field_type)//'" maxLength="'//a_field_length//   &
           '" columnName="'//u_label(1:n_u_lab)//                              &
           '" editable="'//trim(row_col_elems(i_elem)%editable)//              &
           '" sensitive="'//trim(row_col_elems(i_elem)%sensitive)//            &
           '" columnSize="'//trim(colsz)//'"/>'

        case (3)
          call ezgui_convert_str_to_unicode(                                   &
           row_col_elems(i_elem)%label,u_label, n_u_lab)
          if(n_u_lab.lt.0) then
            print *,"Warning: label truncated when converted to unicode"
            print *,"  "//trim(row_col_elems(i_elem)%label)
            n_u_lab=len_trim(u_label)
          endif

          n_u_lab=min(len(u_label), n_u_lab+row_col_elems(i_elem)%length-      &
           len_trim(row_col_elems(i_elem)%label))
          write(12,'(a)') repeat(' ',n_indent)//                               &
           '<Button keyword="'//trim(row_col_elems(i_elem)%keyword)//'">'//    &
           u_label(1:n_u_lab)//'</Button>'

        case default
          print *,                                                             &
           "Error(10)-link_num not recognized:",row_col_elems(i_elem)%link_num
        end select
      enddo

      if(.not.empty_screen) then

        !    Write out Constraints

        write (12,'(a)') repeat(' ',n_indent)//'<Layout>'
        n_indent=n_indent+1

        do i_elem=ibeg,iend
          if(row_col_elems(i_elem)%type==NEW_SCREEN_CARD_TYPE .or.             &
           row_col_elems(i_elem)%type=='R' .or.                                &
           row_col_elems(i_elem)%type=='0') cycle
          if(row_col_elems(i_elem)%y_pos<0 .or.                                &
           row_col_elems(i_elem)%type==END_SCREEN_CARD_TYPE) cycle

          if(row_col_elems(i_elem)%type=='B') then
            call ezgui_int_to_chars(row_col_elems(i_elem)%y_pos,a_y_pos)
            call ezgui_int_to_chars(row_col_elems(i_elem)%x_pos,a_x_pos)
            call ezgui_int_to_chars(row_col_elems(i_elem)%x_size,a_x_size)
            call ezgui_int_to_chars(row_col_elems(i_elem)%y_size,a_y_size)
            write (12,'(a)') repeat(' ',n_indent)//                            &
             '<Constraints component="'//trim(row_col_elems(i_elem)%keyword)// &
             '" xPos="'//trim(a_x_pos)//'" yPos="'//trim(a_y_pos)//            &
             '" xSize="'//trim(a_x_size)//'" ySize="'//trim(a_y_size)//'"/>'
            cycle
          endif

          select case (row_col_elems(i_elem)%link_num)
          case (-1,0,1,3)

            call ezgui_int_to_chars(row_col_elems(i_elem)%y_pos,a_y_pos)
            call ezgui_int_to_chars(row_col_elems(i_elem)%x_pos,a_x_pos)
            call ezgui_int_to_chars(row_col_elems(i_elem)%x_size,a_x_size)
            call ezgui_int_to_chars(row_col_elems(i_elem)%y_size,a_y_size)
            write (12,'(a)') repeat(' ',n_indent)//                            &
             '<Constraints component="'//trim(row_col_elems(i_elem)%keyword)// &
             '" xPos="'//trim(a_x_pos)//'" yPos="'//trim(a_y_pos)//            &
             '" xSize="'//trim(a_x_size)//'" ySize="'//trim(a_y_size)//        &
             '" xStretch="'//trim(row_col_elems(i_elem)%x_stretch)//           &
             '" yStretch="'//trim(row_col_elems(i_elem)%y_stretch)//'"/>'

          case default
          end select

        enddo
        n_indent=n_indent-1
        write(12,'(a)') repeat(' ',n_indent)//'</Layout>'

!     Write out help
!
        write (12,'(a)') repeat(' ',n_indent)//'<HelpSection>'
        n_indent=n_indent+1

        do i_elem=ibeg,iend
          if(row_col_elems(i_elem)%type.eq.NEW_SCREEN_CARD_TYPE .or.           &
           row_col_elems(i_elem)%length.eq.0 .or.                              &
           row_col_elems(i_elem)%type.eq."L" .or.                              &
           row_col_elems(i_elem)%type.eq.'0'.or.                               &
           row_col_elems(i_elem)%type.eq.END_SCREEN_CARD_TYPE) cycle
          n_help=row_col_elems(i_elem)%help_size
          if(len_trim(row_col_elems(i_elem)%tip_info)>0 .or. n_help>0)         &
           call ezgui_write_XML_help(row_col_elems(i_elem), n_indent,          &
           row_col_elems(i_elem)%keyword, window_name)
        enddo

        n_indent=n_indent-1
        write(12,'(a)')  repeat(' ',n_indent)//'</HelpSection>'
        n_indent=n_indent-1
        write (12,'(a)') repeat(' ',n_indent)//'</Screen>'
      endif                                              !Process next screen

      ibeg=iend+1
    enddo

    write(12,'(a)') '  </CommandArea>'
    n_indent=n_indent-1

    call ezgui_get_row_col_elems_elem(                                         &
     row_col_elems,n_elems,"~PROCESS_HELP",i_elem,istat)
    if(istat==0) then
      if(row_col_elems(i_elem)%help_size>0)                                    &
       call ezgui_write_XML_help(row_col_elems(i_elem),n_indent, window_name,'')
    endif
    if(window_dialog==0) then
      write(12,'(a)') ' </Window>'
    else
      write(12,'(a)') ' </Dialog>'
    endif

    n_indent=n_indent-1
    write (12,'(a)') '</Gui>'
    close (12)
    return
  end subroutine ezgui_write_xml_file

!***************************************************************
! write XML help for specific element
!
! Written Jan 2000 by Charles C Burch
!***************************************************************
  subroutine ezgui_write_XML_help(row_col_work, n_indent, keyword, window)

    integer, intent(inout)          :: n_indent
    type (row_col_elem), intent(in) :: row_col_work
    character (len=*), intent(in)   :: keyword, window

    integer             :: n_help, i_help, j_buff, j_help, i ,j
    character (len=256) :: buff, work
    logical             :: html_help_mode


    n_help=row_col_work%help_size
    write(12,'(a)') repeat(' ',n_indent)//                                     &
     '<Help component="'//trim(keyword)//'">'

    n_indent=n_indent+1
    if(len_trim(row_col_work%tip_info).gt.0) then      !output tip, if present
      write(12,'(a)') repeat(' ',n_indent)//'<Tip>'//                          &
       trim(adjustl(row_col_work%tip_info))//'</Tip>'
    endif

    if(n_help.le.0 .and. len_trim(row_col_work%tip_info).eq.0) then
      write(12,'(a)') repeat(' ',n_indent)//'<Text/>'  !no tip or help
    else                                   !Have tip or help info, maybe both
      write(12,'(a)') repeat(' ',n_indent)// &
        '<Text><![CDATA[<html><head><base href='//trim(HELPDOCBASE)//'>'

!  Output window name and tip information, if present

      if(len_trim(window).gt.0) then
        if(len_trim(row_col_work%tip_info).gt.0) then
          write(12,'(a)')                                                      &
           '<b><center>'//trim(window)//' - '//trim(keyword)//'<br>'//         &
           trim(row_col_work%tip_info)//'</center></b>'
        else
          write(12,'(a)') '<b><center>'//trim(window)//' - '//                 &
           trim(keyword)//'</center></b>'
        endif
      else
        if(len_trim(row_col_work%tip_info).gt.0)                               &
         write(12,'(a)') '<b><center>'//trim(row_col_work%tip_info)//          &
         '</center></b>'
      endif

      if(n_help.le.0) then             !Checking for help info
        write(12,'(a)') '</html>]]></Text>'
      else                                  !n_help is > 0
        i_help=1
        html_help_mode=                                                        &
         ezgui_charstr_check(row_col_work%help_info, i_help,                   &
                            "<![CDATA[<html>") .eq. 0
        if(html_help_mode) then
          i_help=16              !help already has CDATA in it, bypass it
          if(row_col_work%help_info(i_help).eq.char(0)) i_help=i_help+1
          buff=''
          j_buff=0
        else
          buff='<pre>'
          j_buff=len_trim(buff)
        endif

!  Now output the help information

        call ezgui_init_convert_to_unicode()
        do while(i_help.le.n_help)
          i=0
          do j_help=i_help, n_help        !find end-of-line deliminitor
            if(row_col_work%help_info(j_help).eq.char(0)) exit
            i=i+1
            work(i:i)=row_col_work%help_info(j_help)
          enddo

          if(i.gt.0) then
            call ezgui_convert_to_unicode_html(work(1:i), &
              buff(j_buff+1:), j, .false.)

            if(j.ge.0) then
              j_buff=j_buff+j
            else
              print *, "Warning: help("//work(1:i)// &
                ") for ",trim(keyword)," truncated when converted to unicode"
              j_buff=len(buff)
            endif
          endif

          if(j_buff.eq.0) then
            j_buff=1
            buff(1:1)=" "
          endif

          if(j_help.lt.n_help) then
            write(12,'(a)') buff(1:j_buff)
          else
            if(html_help_mode) then
              write(12,'(a)') buff(1:j_buff)//'</Text>'
            else
              write(12,'(a)') buff(1:j_buff)//'</pre></head></html>]]></Text>'
            endif
          endif

          i_help=j_help+1
          j_buff=0
        enddo

        if(convert_unicode_html_mode.gt.0) then
          print *, "Warning: help for ",trim(keyword), &
          " likley has invalid html commands"
        endif
      endif

    endif

    n_indent=n_indent-1
    write(12,'(a)') repeat(' ',n_indent)//'</Help>'
    return
  end subroutine ezgui_write_XML_help

!***************************************************************
! Call an editor to edit a text file
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_edit_text(file_name)
    character (len=*), intent(in) :: file_name

    print *,"Editing file="//trim(adjustl(file_name))
    call ezgui_sleep(1)
    call ezgui_system(trim(editor)//" "//trim(adjustl(file_name)))
    return
  end subroutine ezgui_edit_text

!***************************************************************
! sort row_col_elems structure
!        Internal ezgui usage
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_sort_row_col_elems(row_col_elems, n_elems)
    integer              :: n_elems, i_elem, last_sw, n_last
    type (row_col_elem)  :: row_col_elems(:), row_col_work

    last_sw=n_elems
    do while (last_sw>1)
      n_last=last_sw-1
      last_sw=0

      do i_elem=1,n_last
        if(row_col_elems(i_elem)%screen_num <                                  &
         row_col_elems(i_elem+1)%screen_num) cycle
        if(row_col_elems(i_elem)%screen_num==                                  &
         row_col_elems(i_elem+1)%screen_num) then
          if(row_col_elems(i_elem)%y_pos<row_col_elems(i_elem+1)%y_pos) cycle
          if(row_col_elems(i_elem)%y_pos==row_col_elems(i_elem+1)%y_pos) then
            if(row_col_elems(i_elem)%x_pos<=row_col_elems(i_elem+1)%x_pos) cycle
          endif
        endif

        last_sw=i_elem
        row_col_work=row_col_elems(i_elem)
        row_col_elems(i_elem)=row_col_elems(i_elem+1)
        row_col_elems(i_elem+1)=row_col_work
      enddo
    enddo
    return
  end subroutine ezgui_sort_row_col_elems

!*******************************************************************
! get entry number by number or keyword in a_check
!  returns status (istat=0 OK) and elemnt number i_elem
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_get_row_col_elems_elem(row_col_elems,n_elems,a_check, &
   i_elem,istat)
    type (row_col_elem)           :: row_col_elems(:)
    character (len=*), intent(in) :: a_check
    integer, intent(in)           :: n_elems
    integer, intent(out)          :: i_elem, istat

    character  :: check1*20, check2*20

    i_elem=ezgui_chars_to_int(a_check,istat)
    if(istat/=0) then
      check1=a_check
      call ezgui_chars_upper_case(check1)
      do i_elem=1,n_elems
        check2=row_col_elems(i_elem)%keyword(:20)
        call ezgui_chars_upper_case(check2)
        if(check1==check2) then
          istat=0
          return
        endif
      enddo
    endif

    if(i_elem<0 .or. i_elem>n_elems) istat=1
    return
  end subroutine ezgui_get_row_col_elems_elem

!*******************************************************************
! align "text" using "alignment" placing results into "line"
!
! Written November 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_align_text(text, line, alignment)
    character (len=*), intent(out)    :: line
    character (len=*), intent(in)     :: text
    character (len=*), intent(in)     :: alignment

    integer                           :: n_spaces, n_length
    character (len=6)                 :: alignment_upper_case
    integer                           :: i,i1

    n_spaces=len(line)
    n_length=len(text)
    i=n_spaces-n_length
    if(i==0) then
      line=text
      return
    endif

    alignment_upper_case=alignment
    call ezgui_chars_upper_case(alignment_upper_case)
    if(i>0) then
      select case (alignment_upper_case)
      case ("LEFT")
        line=text
      case ("RIGHT ")
        line(i+1:n_spaces)=text
        line(:i)=" "
      case ("CENTER")
        i1=i/2
        line(i1+1:n_spaces)=text
        if(i1>0) line(1:i1)=" "
      end select
    else
      select case (alignment_upper_case)
      case ("LEFT")
        line=text
      case ("RIGHT ")
        line=text(-i:n_length)
      case ("CENTER")
        i1=(-i)/2
        line=text(i1:n_length)
      end select
    endif
    return
  end subroutine ezgui_align_text

!*******************************************************************
! view screen layout described by XML
!   mode=0: GUI display only
!        1: GUI help only
!
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_view_row_col_elems(row_col_elems, n_elems, mode)
    integer, intent(in)             :: n_elems, mode
    type (row_col_elem), intent(in) :: row_col_elems(:)

    integer, parameter     :: NUM_LINES=7500
    integer                :: n_screen, n_row, i_row, i_elem, i_col, iline
    integer                :: k, i, j
    character (len=256)    :: lines(NUM_LINES), line, a_work
    character (len=3)      :: a_num
    integer                :: n, n_vis, n_help, i_help,j_help, ibeg,iend, f_len
    logical                :: ss_sw

    lines=" "
    iline=0
    n_screen=1
    n_row=0
    ss_sw=.false.
    i_elem=0

    do while(i_elem<n_elems .and. mode==0)
      i_elem=i_elem+1
!   print *,i_elem,row_col_elems(i_elem)%keyword, row_col_elems(i_elem)%type
      n_vis=0
      i_row=row_col_elems(i_elem)%y_pos+1
      i_col=row_col_elems(i_elem)%x_pos+1
!   print *,"row, col..",i_row,i_col, iline,n_row
      if(row_col_elems(i_elem)%screen_num==-9) cycle

      select case(row_col_elems(i_elem)%type)
      case (TOP_AREA_CARD_TYPE)               !TopAreaComponent
        iline=iline+1+n_row
        n_row=0
        if(row_col_elems(i_elem)%link_num==0) then
          call ezgui_put_viewline(lines(iline),"<TA>",i_elem)
        else
          call ezgui_put_viewline(lines(iline),'</TA>',i_elem)
        endif
        cycle

      case (BOTTOM_AREA_CARD_TYPE)            !BottomAreaComponent
        iline=iline+1+n_row
        n_row=0
        if(row_col_elems(i_elem)%link_num==0) then
          call ezgui_put_viewline(lines(iline),"<BA>",i_elem)
        else
          call ezgui_put_viewline(lines(iline),'</BA>',i_elem)
        endif
        cycle

      case (SCROLLABLE_SCREEN_CARD_TYPE)      !ScrollableScreen
        if(row_col_elems(i_elem)%link_num==0) then
          ss_sw=.true.
        else
          ss_sw=.false.
        endif
        cycle

      case ("H")                              !Help Info
        iline=iline+1
        call ezgui_put_viewline(lines(iline),                                  &
         "<HP "//trim(row_col_elems(i_elem)%keyword)//" "                      &
         //trim(row_col_elems(i_elem)%label)//">",i_elem)
        cycle

      case (MENUBAR_CARD_TYPE)                           !MenuBar
        do while(i_elem<=n_elems)
          select case (row_col_elems(i_elem)%type)
          case (MENUBAR_CARD_TYPE)
            iline=iline+1
            call ezgui_put_viewline(lines(iline),                              &
             "<MB "//trim(row_col_elems(i_elem)%label)//">",i_elem)
          case ("P")
            iline=iline+1
            call ezgui_put_viewline(lines(iline),                              &
             trim(row_col_elems(i_elem)%label),i_elem)
          case default
            exit
          end select
          i_elem=i_elem+1
        enddo

        iline=iline+1
        call ezgui_put_viewline(lines(iline),"</MB>",i_elem)
        n_row=0
        i_row=0
        i_elem=i_elem-1
        cycle

      case (TOOLBAR_CARD_TYPE)                 !Toolbar
        iline=iline+1
        call ezgui_put_viewline(lines(iline),"<TB>",i_elem)
        i_elem=i_elem+1

        do while(i_elem<=n_elems)
          if(row_col_elems(i_elem)%type/="P") exit
          iline=iline+1
          call ezgui_put_viewline(lines(iline),                                &
           trim(row_col_elems(i_elem)%label),i_elem)
          i_elem=i_elem+1
        enddo

        iline=iline+1
        call ezgui_put_viewline(lines(iline),"</TB>",i_elem)
        n_row=0
        i_elem=i_elem-1
        cycle

      case (NEW_SCREEN_CARD_TYPE)           !New Screen
        n_screen=row_col_elems(i_elem)%screen_num
        iline=iline+n_row+1
        n_row=0
        if(ss_sw) then
          line="<SS "//row_col_elems(i_elem)%label
        else
          line="<NS "//row_col_elems(i_elem)%label
        endif

        if(row_col_elems(i_elem)%x_size>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%x_size,a_num)
          line=trim(line)//"/NC="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%y_size>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%y_size,a_num)
          line=trim(line)//"/NR="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%cell_width>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%cell_width,a_num)
          line=trim(line)//"/CW="//adjustl(a_num)
        endif

        if(row_col_elems(i_elem)%cell_height>0) then
          call   ezgui_int_to_chars(row_col_elems(i_elem)%cell_height,a_num)
          line=trim(line)//"/CH="//adjustl(a_num)
        endif

        call ezgui_put_viewline(lines(iline),trim(line)//">", i_elem)
        cycle

      case (END_SCREEN_CARD_TYPE)                        !End of screen
        iline=iline+n_row
        n_row=0
        cycle
        !Popup Menu
      case (POPUP_MENU_CARD_TYPE)
        iline=iline+1
        call ezgui_put_viewline(                                               &
         lines(iline),"<PM "//trim(row_col_elems(i)%keyword)//">",i_elem)

        i_elem=i_elem+1
        do while(i_elem<=n_elems)
          if(row_col_elems(i_elem)%type==POPUP_MENU_CARD_TYPE) exit
          iline=iline+1
          call ezgui_put_viewline(                                             &
           lines(iline),trim(row_col_elems(i_elem)%label),i_elem)
          i_elem=i_elem+1
        enddo

        iline=iline+1
        call ezgui_put_viewline(lines(iline),"</PM>",i_elem)
        cycle

      case ("A","R")                                  !arrayset, radiobuttons
        cycle

      case ("B")                                      !Border
        n=row_col_elems(i_elem)%x_size
        i=row_col_elems(i_elem)%y_size

        if(n<1) then
          print *,                                                             &
           "Border("//trim(row_col_elems(i_elem)%keyword)//                    &
           " has xSize<1--set to 1"
          n=1
        endif

        if(i<1) then
          print *,                                                             &
           "Border("//trim(row_col_elems(i_elem)%keyword)//                    &
           " has ySize<1--set to 1"
          i=1
        endif

        line(:n)=repeat('-', n)
        line(1:1)='+'
        line(n:n)='+'
        j=len_trim(row_col_elems(i_elem)%label)
        if( j.gt.0) then
          k=min(n-2,j)
          line(2:k+1)=row_col_elems(i_elem)%label(1:j)
        endif

        call ezgui_put_viewline(                                               &
         lines(iline+i_row)(i_col:i_col+n-1),line(1:n), i_elem)
        call ezgui_put_viewline(                                               &
         lines(iline+i_row+i-1)(i_col:i_col+n-1),line(1:n), i_elem)

        do j=3,i
          call ezgui_put_viewline(                                             &
           lines(iline+i_row+j-2)(i_col:i_col),"|", i_elem)
          call ezgui_put_viewline(                                             &
           lines(iline+i_row+j-2)(i_col+n-1:i_col+n-1),"|", i_elem)
        enddo
        n_vis=i-1

      case ("P","T","K")                            !Pushbutton
        if(i_row<0) cycle
        n=row_col_elems(i_elem)%x_size
        line(1:n)=' '
        if(row_col_elems(i_elem)%type=="T") then
          line(1:1)='['
          line(n:n)=']'
        else if(row_col_elems(i_elem)%type=="K") then
          line(1:1)='{'
          line(n:n)='}'
        else
          line(1:1)='|'
          line(n:n)='|'
        endif

        ibeg=1
        a_work=row_col_elems(i_elem)%label
        j=len_trim(a_work)
        call ezgui_chars_upper_case(a_work(1:j))

        do i=1,row_col_elems(i_elem)%y_size
          call ezgui_find_unicode_value(a_work,ibeg,j,10, iend,f_len)
          line(2:n-1)=row_col_elems(i_elem)%label(ibeg:iend-1)
          call ezgui_put_viewline(                                             &
           lines(iline+i_row+i-1)(i_col:i_col+n-1),line(:n), i_elem)
          ibeg=min(j+1,iend+f_len)
        enddo
        n_vis=row_col_elems(i_elem)%y_size-1
        !Label
      case ("L")
        n=row_col_elems(i_elem)%x_size
        call ezgui_put_viewline(lines(iline+i_row)(i_col:i_col+n-1),           &
         row_col_elems(i_elem)%label(1:n), i_elem)

      case("S", "F", "I", "X", "O","C")      !Edit fields
!      if(row_col_elems(i_elem)%link_num<0) then
        n=row_col_elems(i_elem)%x_size
!      else
!        n=row_col_elems(i_elem)%length      !length of edit fields
!      endif

        if(n<2.or.n>120) then
          print *,"WARNING field length(",n,") error with element",i_elem
          n=max(2,min(n,120))
        endif

        line(1:1)=row_col_elems(i_elem)%type
        if(row_col_elems(i_elem)%editable=='no ')line(1:1)="X"
        if(n>1) line(2:n)=repeat(line(1:1), n-1)
        line(1:1)='`'

        if(row_col_elems(i_elem)%link_num<0) then
          call ezgui_put_viewline(                                             &
           lines(iline+i_row)(i_col:i_col+n-1),line(:n), i_elem)
        else
          i=min(row_col_elems(i_elem)%x_size,                                  &
           len_trim(row_col_elems(i_elem)%label))
          call ezgui_put_viewline(lines(iline+i_row)(i_col:i_col+i-1),         &
           row_col_elems(i_elem)%label(:i), i_elem)
          do i=2,row_col_elems(i_elem)%y_size
            call ezgui_put_viewline(lines(iline+i_row+i-1)(i_col:i_col+n-1),   &
             line(:n), i_elem)
            n_vis=n_vis+1
          enddo
        endif

      case default
      end select

      n_row=max(n_row,i_row+n_vis)
    enddo

! Output help and tip information

    n_row=n_row+iline
    i_elem=0
    do while(i_elem<n_elems .and. mode==1)
      i_elem=i_elem+1
      n_help=row_col_elems(i_elem)%help_size

      if(n_help>0 .or. len_trim(row_col_elems(i_elem)%tip_info)>0) then
        n_row=n_row+1
        if(row_col_elems(i_elem)%keyword=="~PROCESS_HELP") then
          lines(n_row)="<HELP>"
        else
          lines(n_row)=                                                        &
           "<HELP "//trim(row_col_elems(i_elem)%keyword)//">"//                &
           trim(row_col_elems(i_elem)%tip_info)
        endif

        if(n_help>0) then
          i_help=1
          do while(i_help<=n_help)
            i=0
            n_row=n_row+1
            do j_help=i_help, n_help        !find end-of-line deliminitor
              if(row_col_elems(i_elem)%help_info(j_help)==char(0)) exit
              i=i+1
              lines(n_row)(i:i)=row_col_elems(i_elem)%help_info(j_help)
            enddo
            i_help=j_help+1
          enddo
        endif

      endif
    enddo

    if(mode==1) then
      n_row=n_row+1
      lines(n_row)=' '

      do i_elem=1,n_elems
        if(row_col_elems(i_elem)%type==NEW_SCREEN_CARD_TYPE  .or.              &
         row_col_elems(i_elem)%length==0    .or.                               &
         row_col_elems(i_elem)%type=="L"    .or.                               &
         row_col_elems(i_elem)%type==END_SCREEN_CARD_TYPE) cycle
        n_help=row_col_elems(i_elem)%help_size

        if(len_trim(row_col_elems(i_elem)%tip_info)==0) then
          if(n_help==0) then
            n_row=n_row+1
            lines(n_row)=                                                      &
             'Keyword('//trim(row_col_elems(i_elem)%keyword)//                 &
             ') has no tip nor help'
          else
            n_row=n_row+1
            lines(n_row)=                                                      &
             'Keyword('//trim(row_col_elems(i_elem)%keyword)//') has no tip'
          endif
        else
          if(n_help==0) then
            n_row=n_row+1
            lines(n_row)=                                                      &
             'Keyword('//trim(row_col_elems(i_elem)%keyword)//') has no help'
          endif
        endif
      enddo
    endif

    n=3
    if(n==2.or.n==3) open(unit=13,file="view.ezg",status="unknown")

    do i=1,n_row
      if(n==1.or.n==3) print *,trim(lines(i))
      if(n==2.or.n==3) write(13,'(a)') trim(lines(i))
    enddo

    if(n==1.or.n==3) print *,' '
    if(n==2.or.n==3) close(13)
    return
  end subroutine ezgui_view_row_col_elems

!*******************************************************************
! put a string into view buffer, checking to see if any overlap
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_put_viewline(line, str, i_elem)
    character (len=*), intent(inout)  :: line
    character (len=*), intent(in)     :: str
    integer, intent(in)               :: i_elem

    integer                           :: i, n

    if (len_trim(line)>0) then
      n=len_trim(str)
      do i=1,n
        if(line(i:i)/=' ') then
          if(str(i:i)/=line(i:i)) then
            print *, "Field overlap with element=",i_elem,                     &
             "line=",line, "str=",str
!          call ezgui_char_prompt_edit(line,"","Press return to continue")
            exit
          endif
        endif
      enddo
    endif

    line=str
    return
  end subroutine ezgui_put_viewline

!*******************************************************************
! check validity of XML entries
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_check_row_col_elems(row_col_elems, n_elems, n_stat)
    integer, intent(in)    :: n_elems
    integer, intent(out)   :: n_stat
    type (row_col_elem)    :: row_col_elems(:)

    integer    :: i_elem, istat, imode, istat_save
    character  :: dummy*1

    do imode=1,3,2
      if(imode==1) print *,"Start of validity check"
      if(imode==3) print *,"Start of repair"

      n_stat=0
      do i_elem=1, n_elems
        call ezgui_check_row_col_elem(row_col_elems, i_elem, istat,imode)
        n_stat=n_stat+istat
      enddo
      call ezgui_check_xml_keywords(row_col_elems, n_elems, istat, imode)
      n_stat=n_stat+istat

      if(imode==1)  then
        istat_save=n_stat
        print *,"Number of errors found=",n_stat
        if(n_stat==0) then
          exit
        else
          call ezgui_int_prompt_edit(istat,0,                                  &
           "Enter 1 to try to correct errors, 0 to bypass")
          if(istat==0) exit
        endif
      else
        n_stat=istat_save
      endif
    enddo

    call ezgui_char_prompt_edit(dummy,"","Press return to continue")
    return
  end subroutine ezgui_check_row_col_elems

!*******************************************************************
! check validity of XML keyword entries
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_check_xml_keywords(row_col_elems, n_elems, istat, mode)
    type (row_col_elem)    :: row_col_elems(:)
    integer, intent(in)    :: n_elems, mode
    integer, intent(out)   :: istat

    integer                :: i_elem, j_elem
    character              :: a3*3

    istat=0
    do i_elem=1, n_elems-1
      if(len_trim(row_col_elems(i_elem)%keyword)==0) then
        select case(row_col_elems(i_elem)%type)
        case ("I","S","M","R","C","F","A","T","P","K")
          istat=istat+1
          if(mode<=2) print *,"entry (",i_elem,") has invalid blank keyword"
          if(mode>=2) then
            call ezgui_convert_int_to_chars(i_elem,a3)
            row_col_elems(i_elem)%keyword="var"//a3
          endif
        end select

        if(len_trim(row_col_elems(i_elem)%keyword)==0) cycle
      endif

      do j_elem=i_elem+1, n_elems
        if(row_col_elems(i_elem)%keyword==row_col_elems(j_elem)%keyword)  then
          istat=istat+1
          if(mode<=2)                                                          &
           print *," same keyword("//trim(row_col_elems(i_elem)%keyword)//     &
           ") used in elements ", i_elem," and ",j_elem
          if(mode>=2) then
            call ezgui_convert_int_to_chars(j_elem,a3)
            row_col_elems(j_elem)%keyword=                                     &
             trim(row_col_elems(j_elem)%keyword)//a3
          endif
        endif
      enddo

    enddo
    return
  end subroutine ezgui_check_xml_keywords

!*******************************************************************
! check validity pf single XML entry
!   mode=1: print out errors
!   mode=2: print out errors and try to repair
!    mode=3: repair only
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!*******************************************************************
  subroutine ezgui_check_row_col_elem(row_col_elems,i_elem, istat, mode)
    type (row_col_elem)    :: row_col_elems(:)
    integer, intent(in)    :: i_elem, mode
    integer, intent(out)   :: istat

    character              :: field_type*20
    integer                :: keyword_counter, n

    istat=0
    keyword_counter=0

    if(row_col_elems(i_elem)%screen_num<1 .or.                                 &
     row_col_elems(i_elem)%screen_num>10) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid screen number (",row_col_elems(i_elem)%screen_num,") in entry",&
       i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%screen_num=                                       &
       min(10,max(1,row_col_elems(i_elem)%screen_num))
    endif

    if(row_col_elems(i_elem)%y_pos<1 .or. row_col_elems(i_elem)%y_pos>50) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid row number (",row_col_elems(i_elem)%y_pos,") in entry",i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%y_pos=min(50,max(1,row_col_elems(i_elem)%y_pos))
    endif

    if(row_col_elems(i_elem)%x_pos<0 .or. row_col_elems(i_elem)%x_pos>132) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid column number (",row_col_elems(i_elem)%x_pos,") in entry",i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%x_pos=min(132,max(0,row_col_elems(i_elem)%x_pos))
    endif

    if(row_col_elems(i_elem)%length<0 .or.                                     &
     row_col_elems(i_elem)%screen_num>132) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid length (",row_col_elems(i_elem)%length,") in entry",i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%length=min(132,max(1,row_col_elems(i_elem)%length))
    endif

    call ezgui_convert_field_type_code(row_col_elems(i_elem)%type, field_type)
    if(len_trim(field_type)==0) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,a,a,i3)",                                                  &
       "Invalid field type (",trim(row_col_elems(i_elem)%type),") in entry",   &
       i_elem
      if(mode>=2) row_col_elems(i_elem)%type="L"
    endif

    if(trim(row_col_elems(i_elem)%editable)/="yes" .and.                       &
     trim(row_col_elems(i_elem)%editable)/="YES" .and.                         &
     trim(row_col_elems(i_elem)%editable)/="no"  .and.                         &
     trim(row_col_elems(i_elem)%editable)/="NO") then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,a,a,i3)",                                                  &
       "Invalid editable (",row_col_elems(i_elem)%editable,") in entry",i_elem
      if(mode>=2) row_col_elems(i_elem)%editable="yes"
    endif

    if(row_col_elems(i_elem)%type/="L" .and.                                   &
     len_trim(row_col_elems(i_elem)%keyword)==0) then
      istat=istat+1
      if(mode<=2) print "(1x,a,i3)","Keyword needed for entry",i_elem
      if (mode>=2)  then
        call ezgui_int_to_chars(keyword_counter,field_type(1:3))
        row_col_elems(i_elem)%keyword="keyword"//trim(field_type(1:3))
        keyword_counter=keyword_counter+1
      endif
    endif

    n=5
    if(row_col_elems(i_elem)%type==NEW_SCREEN_CARD_TYPE) n=100

    if(row_col_elems(i_elem)%y_size<0 .or. row_col_elems(i_elem)%y_size>n) then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid ySize (",row_col_elems(i_elem)%y_size,") in entry",i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%y_size=max(0,min(n,row_col_elems(i_elem)%y_size))
    endif

    if(row_col_elems(i_elem)%x_size<0 .or. row_col_elems(i_elem)%x_size>132)then
      istat=istat+1
      if(mode<=2)                                                              &
       print "(1x,a,i3,a,i3)",                                                 &
       "Invalid xSize (",row_col_elems(i_elem)%x_size,") in entry",i_elem
      if(mode>=2)                                                              &
       row_col_elems(i_elem)%x_size=max(0,min(132,row_col_elems(i_elem)%x_size))
    endif
    return
  end subroutine ezgui_check_row_col_elem

!***************************************************************
! list entry i_beg to i_end of row_col_elems structure
!   mode=0: user, 1:developer
!        Internal EzGUI usage
!
! Written July 1999 by Charles C Burch
!***************************************************************
  subroutine ezgui_list_row_col_elems(row_col_elems, i_beg, i_end, mode)
    integer, intent(in)             :: i_beg, i_end, mode
    type (row_col_elem), intent(in) :: row_col_elems(:)

    integer :: i

    open(unit=11,file="list.ezg",status="unknown")
    if(mode==0) then
      print *,        "                    List of GUI elements"
      print *,        "el# scr row col type len link xsz ysz label"//          &
       "                         keyword"
      write(11, '(a)')"                    List of GUI elements"
      write(11, '(a)')"el# scr row col type len link xsz ysz label"//          &
       "                         keyword"
    else
    endif

    do i=i_beg,i_end
      if(mode==0) then
        print '(1x,i3,1x,i3,1x,i3,1x,i3,1x,a,4x,i4,1x,i3,1x,i3,1x,i3,1x,a,a)', &
         i,row_col_elems(i)%screen_num, row_col_elems(i)%y_pos,                &
         row_col_elems(i)%x_pos,row_col_elems(i)%type,row_col_elems(i)%length, &
         row_col_elems(i)%link_num,row_col_elems(i)%x_size,                    &
         row_col_elems(i)%y_size,row_col_elems(i)%label(:25),                  &
         row_col_elems(i)%keyword(:15)
        write(11,'( i3,1x,i3,1x,i3,1x,i3,1x,a,4x,i4,1x,i3,1x,i3,1x,i3,1x,a,a)')&
         i,row_col_elems(i)%screen_num, row_col_elems(i)%y_pos,                &
         row_col_elems(i)%x_pos,row_col_elems(i)%type,row_col_elems(i)%length, &
         row_col_elems(i)%link_num,row_col_elems(i)%x_size,                    &
         row_col_elems(i)%y_size,row_col_elems(i)%label(:25),                  &
         row_col_elems(i)%keyword(:15)
      else
      endif
    enddo
    return
  end subroutine ezgui_list_row_col_elems

  subroutine ezgui_write_geopro_xml_file(file_name, n_elems, row_col_elems, istat)

    character (len=*), intent(in)   :: file_name
    integer, intent(in)             :: n_elems
    integer, intent(out)            :: istat
    type (row_col_elem), intent(in) :: row_col_elems(:)
    type (row_col_elem)             :: A

    character (len=3)     :: a_y_pos,a_x_pos,a_field_length,a_x_size,a_y_size
    character (len=4)     :: a_buff
    character (len=20)    :: field_type, colsz

    character (len=32)    :: window_name
    character (len=32)    :: keyword,lkeyword
    character (len=80)    :: label
    character (len=80)   :: tooltip
    character, dimension(:),pointer   :: help_info
    character (len=1)     :: C
    integer               :: help_size

    integer               :: n_help, j, nrows, ncols
    integer               :: n_screen, n_screens, i_elem, link_last,ibeg,iend
    integer               :: n_indent, f_beg, f_end, n_u_lab
    character (len=256)   :: a_work, u_label
    logical               :: empty_screen, ta_mode, ba_mode
    logical               :: in_table
    integer               :: table_num

    open(unit=10,file='declare.cpp.x',status='unknown',iostat=istat)
    if(istat/=0) return
    open(unit=57,file='new_declare.cpp.x',status='unknown',iostat=istat)
    if(istat/=0) return
    open(unit=13,file='params.cpp.x',status='unknown',iostat=istat)
    if(istat/=0) return
    open(unit=58,file='new_params1.cpp.x',status='unknown',iostat=istat)
    if(istat/=0) return
    open(unit=59,file='new_params2.cpp.x',status='unknown',iostat=istat)
    if(istat/=0) return
    open(unit=12,file=file_name,status='unknown',iostat=istat)
    if(istat/=0) return

! Header stuff for XML file
    write (12,'(a)') '<?xml version="1.0" encoding="UTF-8"?>'
    write (12,'(a)') '<?xml-stylesheet type="text/xsl" href="LagModule.xsl"?>'
    write (12,'(a)') '<!DOCTYPE LagModule SYSTEM "LagModule.dtd">'

    f_end=len_trim(file_name)
    f_beg=0
    ba_mode=.false.                !true, if in back area component mode
    ta_mode=.false.                !true, if in top area compionent mode

    do j=1, f_end
      if(file_name(j:j)=='/') f_beg=j
    enddo
    f_end=ezgui_find_chars(file_name,f_beg+1,f_end,'.')

    window_name=file_name(f_beg+1:f_end-1)
    write (12,'(a)') '<LagModule tab="GeoPRO" xid="a0" mode="serial" name="' //trim(window_name) &
      // '" label="' //trim(window_name)// '" inport="" outport="" version="1.0" wrapper="" category="CPSeis" debugger="no" memcheck="no">'
    write (12,'(a)',ADVANCE='YES')'<doc><pre>'
      !-- geopro documentation here
    call ezgui_get_row_col_elems_elem(row_col_elems,n_elems,"~PROCESS_HELP",i_elem,istat)
    if(istat==0) then
      do j=1, row_col_elems(i_elem)%help_size
        A=row_col_elems(i_elem)
        C=A%help_info(j)
        select case (C)
          case (char(34))
            C=char(39)
          case ("<",">","&","'")
            C=" "
          case (char(0))
            C=char(10)
          case default
        end select 
        write (12,'(a1)',ADVANCE='NO') C
      end do
    endif
    write (12,'(a)')'  </pre></doc>'

! Now output the fields
    write(6,'(A3,A21,A21,A23,2a4)')'SC#','Field Type          ','Label               ','Keyword            Type','Edt','Sen'
    in_table=.false.
    table_num=0
    do i_elem=1,n_elems
      
      A=row_col_elems(i_elem)
      call ezgui_int_to_chars(A%length,a_field_length)
      call ezgui_convert_field_type_code(A%type, field_type)
      keyword=trim(A%keyword)
      lkeyword=string_2_lower(keyword)
      call ezgui_clean_string(A%label,label)
      j=index(label,"=")
      if(j>0)label=trim(label(:j-1))
      if(trim(label) == '' ) cycle
      call ezgui_clean_string(A%tip_info,tooltip)
      write(6,'(I3,A21,A21,A21,a2,2a4)')A%screen_num,field_type,label(:20),keyword,A%type,A%editable,A%sensitive
      select case (A%type)
        case ("A")
          if(in_table) then
            in_table=.false.
            write(12,'(a)') '</LagParameterTable>'
          endif
          table_num=table_num+1
          in_table=.true.
          write(12,'(a,i1,a)') '<LagParameterTable id="ParameterTable',table_num,'" label="'//trim(label)//'" constrained="false">'
        case ("I","F","S","X","C","T","K")
          if(A%link_num == -1 ) then
            if(in_table) then
              in_table=.false.
              write(12,'(a)') '</LagParameterTable>'
            endif
            write(12,'(a)') '<LagParameter id="'//trim(keyword) &
              //'" name="'//trim(keyword)//'" label="'//trim(label) &
              //'" toolTip="'//trim(tooltip)//'" required="REQUIRED">'
          elseif(A%link_num == 2 ) then
            write(12,'(a)') '  <LagParameter id="'//trim(keyword) &
              //'" name="'//trim(keyword)//'" label="'//trim(label) &
              //'" toolTip="'//trim(tooltip)//'" required="NOT_REQUIRED">'
          endif
          write(12,'(a)',ADVANCE='YES') '  <doc><pre>'
            do j=1, A%help_size
              C=A%help_info(j)
              select case (C)
                case (char(34))
                  C=char(39)
                case ("<",">","&","'")
                  C=" "
                case (char(0))
                  C=char(10)
                case default
              end select 
              write (12,'(a1)',ADVANCE='NO') C
            end do
          write(12,'(a)') '  </pre></doc>'
          !  ----------------------------10 = declarations 
          !  ----------------------------13 = parameters
          !  ----------------------------12 = xml
          !  ----------------------------57 = new declarations 
          !  ----------------------------58 = new parameters1
          !  ----------------------------59 = new parameters2
          select case (A%type)
            case ("C","K","R")
                  write(10,'(a)') '    char    '//trim(lkeyword)//'[STRINGLIST_LENGTH];'
                  write(57,'(a)') '    char    '//trim(lkeyword)//'[PCW::LENGTH];'
                  write(13,'(a)') '    strcpy('//trim(lkeyword)//', LagString('//trim(keyword)//').c_str());'
                  write(13,'(a)') '    pc_put_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    pc_get_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    LagMessage_info("'//trim(keyword)//'     =   %s\n",'//trim(lkeyword)//');'
                  write(58,'(a)') '    strcpy('//trim(lkeyword)//', LagString('//trim(keyword)//').c_str());'
                  write(58,'(a)') '    PCW::put ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(59,'(a)') '    PCW::get ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(12,'(a)') '  <LagOption>'
                  write(12,'(a)') '    <LagOptionItem label="A" value="1" selected="selected"/>'
                  write(12,'(a)') '    <LagOptionItem label="B" value="2" selected="not_selected"/>'
                  write(12,'(a)') '    <LagOptionItem label="C" value="3" selected="not_selected"/>'
                  write(12,'(a)') '  </LagOption>'
            case ("I")
              if(A%link_num == -1 ) then ! scalar 
                  write(12,'(a)') '  <int value="1" readOnly="no"/>'
                  write(13,'(a)') '    pc_put_iscalar ("'//trim(keyword)//'", &'//trim(keyword)//');'
                  write(13,'(a)') '    pc_get_iscalar ("'//trim(keyword)//'", &'//trim(keyword)//');'
                  write(13,'(a)') '    LagMessage_info("'//trim(keyword)//'     =   %d\n",'//trim(keyword)//');'
                  write(58,'(a)') '    PCW::put ("'//trim(keyword)//'",  '//trim(keyword)//');'
                  write(59,'(a)') '    PCW::get ("'//trim(keyword)//'", &'//trim(keyword)//');'
              elseif (A%link_num == 2) then ! array
                  write(12,'(a)') '    <LagVector label="'//trim(label)//'" cg_type="template" cg_subtype="">'
                  write(12,'(a)') '      <int value="" readOnly="no"/>'
                  write(12,'(a)') '    </LagVector>'
                  write(10,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(10,'(a)') '    int * '//trim(lkeyword)//' = (int*) LagMem_calloc('//trim(lkeyword)//'_cnt, sizeof(int), "'//trim(lkeyword)//'");'
                  write(10,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) '//trim(lkeyword)//'[i] = '//trim(keyword)//'[i];'
                  write(57,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(57,'(a)') '    int * '//trim(lkeyword)//' = (int*) LagMem_calloc('//trim(lkeyword)//'_cnt, sizeof(int), "'//trim(lkeyword)//'");'
                  write(57,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) '//trim(lkeyword)//'[i] = '//trim(keyword)//'[i];'
                  write(13,'(a)') '    pc_put_iarray ("'//trim(keyword)//'", '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
                  write(13,'(a)') '    pc_get_iarray ("'//trim(keyword)//'", &'//trim(lkeyword)//'_cnt, '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
                  write(13,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) LagMessage_info("'//trim(lkeyword)//'[%d]=%d\n",i,'//trim(lkeyword)//'[i]);' 
                  write(58,'(a)') '    PCW::put ("'//trim(keyword)//'", '//trim(lkeyword)//', '//trim(lkeyword)//'_cnt);'
                  write(59,'(a)') '    PCW::get ("'//trim(keyword)//'", '//trim(lkeyword)//'_cnt, '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
              endif
            case ("F")
              if(A%link_num == -1 ) then ! scalar 
                  write(12,'(a)') '  <float value="1.0" readOnly="no"/>'
                  write(13,'(a)') '    pc_put_fscalar ("'//trim(keyword)//'", &'//trim(keyword)//');'
                  write(13,'(a)') '    pc_get_fscalar ("'//trim(keyword)//'", &'//trim(keyword)//');'
                  write(13,'(a)') '    LagMessage_info("'//trim(keyword)//'     =   %f\n",'//trim(keyword)//');'
                  write(58,'(a)') '    PCW::put ("'//trim(keyword)//'",  '//trim(keyword)//');'
                  write(59,'(a)') '    PCW::get ("'//trim(keyword)//'", &'//trim(keyword)//');'
              elseif (A%link_num == 2) then ! array
                  write(12,'(a)') '    <LagVector label="'//trim(label)//'" cg_type="template" cg_subtype="">'
                  write(12,'(a)') '      <float value="" readOnly="no"/>'
                  write(12,'(a)') '    </LagVector>'
                  write(10,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(10,'(a)') '    float * '//trim(lkeyword)//' = (float*) LagMem_calloc('//trim(lkeyword)//'_cnt, sizeof(float), "'//trim(lkeyword)//'");'
                  write(10,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) '//trim(lkeyword)//'[i] = '//trim(keyword)//'[i];'
                  write(57,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(57,'(a)') '    float * '//trim(lkeyword)//' = (float*) LagMem_calloc('//trim(lkeyword)//'_cnt, sizeof(float), "'//trim(lkeyword)//'");'
                  write(57,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) '//trim(lkeyword)//'[i] = '//trim(keyword)//'[i];'
                  write(13,'(a)') '    pc_put_farray ("'//trim(keyword)//'", '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
                  write(13,'(a)') '    pc_get_farray ("'//trim(keyword)//'", &'//trim(lkeyword)//'_cnt, '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
                  write(13,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) LagMessage_info("'//trim(lkeyword)//'[%d]=%f\n",i,'//trim(lkeyword)//'[i]);' 
                  write(58,'(a)') '    PCW::put ("'//trim(keyword)//'", '//trim(lkeyword)//', '//trim(lkeyword)//'_cnt);'
                  write(59,'(a)') '    PCW::get ("'//trim(keyword)//'", '//trim(lkeyword)//'_cnt, '//trim(lkeyword)//', &'//trim(lkeyword)//'_cnt);'
              endif
            case ("S")
              if(index(label,"PATH") > 0 .or. index(label,"FILE") > 0 ) then
                  write(12,'(a)') '  <LagFileDialog>'
                  write(12,'(a)') '    <LagFileDialogItem label="File Dialog" value="NONE" caption="Select" filters="*.*" savefile="yes" directory="no"/>'
 
                  write(12,'(a)') '  </LagFileDialog>'
                  write(10,'(a)') '    char    '//trim(lkeyword)//'[STRINGLIST_LENGTH];'
                  write(57,'(a)') '    char    '//trim(lkeyword)//'[PCW::LENGTH];'
                  write(13,'(a)') '    strcpy('//trim(lkeyword)//', '//trim(keyword)//'.getFileDialogValue().c_str());'
                  write(13,'(a)') '    pc_put_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    pc_get_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    LagMessage_info("'//trim(keyword)//'     =   %s\n",'//trim(lkeyword)//');'
                  write(58,'(a)') '    strcpy('//trim(lkeyword)//', '//trim(keyword)//'.getFileDialogValue().c_str());'
                  write(58,'(a)') '    PCW::put ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(59,'(a)') '    PCW::get ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
              else
                if(A%link_num == -1 ) then ! scalar
                  if(in_table) then
                    in_table=.false.
                    write(12,'(a)') '</LagParameterTable>'
                  endif
                  write(10,'(a)') '    char    '//trim(lkeyword)//'[STRINGLIST_LENGTH];'
                  write(57,'(a)') '    char    '//trim(lkeyword)//'[PCW::LENGTH];'
                  write(13,'(a)') '    strcpy('//trim(lkeyword)//', LagString('//trim(keyword)//').c_str());'
                  write(13,'(a)') '    pc_put_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    pc_get_cscalar ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(13,'(a)') '    LagMessage_info("'//trim(keyword)//'     =   %s\n",'//trim(lkeyword)//');'
                  write(58,'(a)') '    strcpy('//trim(lkeyword)//', LagString('//trim(keyword)//').c_str());'
                  write(58,'(a)') '    PCW::put ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(59,'(a)') '    PCW::get ("'//trim(lkeyword)//'", '//trim(lkeyword)//');'
                  write(12,'(a)') '  <LagString value="StringDefault" readOnly="no"/>'
                elseif (A%link_num == 2) then ! array
                  write(12,'(a)') '    <LagVector label="'//trim(label)//'" cg_type="template" cg_subtype="">'
                  write(12,'(a)') '      <LagString value="StringDefault" readOnly="no"/>'
                  write(12,'(a)') '    </LagVector>'
                  write(13,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i], LagString('//trim(keyword)//'[i]).c_str());'
                  write(58,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i], LagString('//trim(keyword)//'[i]).c_str());'
                  write(10,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(10,'(a)') '    char    '//trim(lkeyword)//'['//trim(lkeyword)//'_cnt][STRINGLIST_LENGTH];'
                  write(10,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i],(char*) '//trim(keyword)//'[i].c_str());'
                  write(10,'(a)') '    int  nwords_'//trim(lkeyword)//'=STRINGLIST_LENGTH/sizeof(int);'
                  write(10,'(a)') '    int nchars_'//trim(lkeyword)//'=STRINGLIST_LENGTH, nelements_'//trim(lkeyword)//'='//trim(lkeyword)//'_cnt;'
                  write(57,'(a)') '    int '//trim(lkeyword)//'_cnt = '//trim(keyword)//'.size();'
                  write(57,'(a)') '    char    '//trim(lkeyword)//'['//trim(lkeyword)//'_cnt][PCW::LENGTH];'
                  write(57,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i],(char*) '//trim(keyword)//'[i].c_str());'
                  write(57,'(a)') '    int  nwords_'//trim(lkeyword)//'=PCW::LENGTH/sizeof(int);'
                  write(57,'(a)') '    int nchars_'//trim(lkeyword)//'=PCW::LENGTH, nelements_'//trim(lkeyword)//'='//trim(lkeyword)//'_cnt;'
                  write(13,'(a)') '    pc_put_carray ((int *) "'//trim(keyword)//'", (int *) '//trim(lkeyword)//',&nelements_'//trim(lkeyword)//', &nwords_'//trim(lkeyword)//');'
                  write(13,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i],"");'

                  write(13,'(a)') '    pc_get_carray ((int *) "'//trim(keyword)//'", &nelements_'//trim(lkeyword)//', (int *) '//trim(lkeyword)//', &nelements_'//trim(lkeyword)//', &nwords_'//trim(lkeyword)//');'
                  write(13,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) LagMessage_info("'//trim(lkeyword)//'[%d]=%s\n",i,'//trim(lkeyword)//'[i]);' 
                  write(58,'(a)') '    PCW::put ("'//trim(keyword)//'", (char*)'//trim(lkeyword)//',nelements_'//trim(lkeyword)//');'
                  write(58,'(a)') '    for(int i=0;i<'//trim(lkeyword)//'_cnt;i++) strcpy('//trim(lkeyword)//'[i],"");'

                  write(59,'(a)') '    PCW::get ("'//trim(keyword)//'", nelements_'//trim(lkeyword)//', (char*)'//trim(lkeyword)//', &nelements_'//trim(lkeyword)//');'

                endif
              endif
            case default
          end select
          write(12,'(a)')'</LagParameter>'
          if(A%link_num /= 2  .and. in_table ) then
            in_table=.false.
            write(12,'(a)') '</LagParameterTable>'
          endif
        case default
          if(in_table) then
            in_table=.false.
            write(12,'(a)') '</LagParameterTable>'
          endif
      end select
    enddo

    if(in_table ) then
      in_table=.false.
      write(12,'(a)') '</LagParameterTable>'
    endif
    write (12,'(a)') '</LagModule>'
    close (12)
    close (13)
    close (10)
    close (57)
    close (58)
    close (59)
    return
  end subroutine ezgui_write_geopro_xml_file

  subroutine ezgui_clean_string(inp,res)
    character (len=*), intent(in) :: inp
    character (len=*), intent(out):: res
    character (len=1)             :: c
    integer                       :: i,li,lo,n
    li=len(inp)
    lo=len(res)
    n =min(li,lo)
    do i = 1,n
      c=inp(i:i)
      select case (c)
        case (char(34))
          c=char(39)
        case ("<",">","&","'")
          c=" "
        case (char(0))
          c=char(10)
        case default
      end select 
      res(i:i)=c
    end do
    if(i<lo) res(i:)=''
  end subroutine ezgui_clean_string
      
end module ezgui_module


!***********************************************************
!  Main menu for ezgui
!
!  Written July 1999 by Charles C Burch
!***********************************************************
subroutine ezgui_f(check_help, mature, dirs, n_dirs, filein, fileout)
  use ezgui_module
  use getsys_module
  use path_module
  implicit none

  integer,   intent(in) :: check_help
  character, intent(in) :: dirs(80, N_INCLUDE_DIRS_SIZE)
  integer,   intent(in) :: mature
  integer,   intent(in) :: n_dirs
  character, intent(in) :: filein(80)
  character, intent(in) :: fileout(80)

  integer               :: n1, n2
  integer, parameter    :: NUM_CARDS=15000, NUM_ELEMS=3000

  character (len=80)    :: file_name, a_work, file_in, file_out
  integer               :: n_elems, istat, i_elem, i1, i2
  type (row_col_elem)   :: row_col_elems(NUM_ELEMS)
  logical               :: save_sw
  character (len=256)   :: cards(NUM_CARDS)
  character (len=1)     :: card_type(NUM_CARDS)
  character (len=80)    :: srcdir

  print '(1x,a,/)', trim(program_title)

  check_help_sw=check_help
  n_include_dirs=n_dirs
  print *,"Include directories are:"
  do i1=1,n_include_dirs
    do i2=1,80
      include_dirs(i1)(i2:i2)=dirs(i2,i1)
    enddo
    print *,"  "//trim(include_dirs(i1))
  enddo
  print *,""

  n_elems=0
  save_sw=.True.

! Process any command arguments, if present

  do i1=1,80
    file_in(i1:i1)=filein(i1)
    file_out(i1:i1)=fileout(i1)
  enddo
  n1=len_trim(file_in)
  n2=len_trim(file_out)

! Use file_in as basis for setting HELPDOCBASE (trailing slash IS NEEDED)
  HELPDOCBASE = 'unknown'
  MATURITY=mature
  if(MATURITY==0) then
    srcdir = trim(path_get_dir(file_in));
    if(srcdir=='NONE' .or. srcdir=='./') call getsys_env('PWD', srcdir)
    HELPDOCBASE = 'file://' // trim(srcdir) // '/'
  else if(MATURITY==1) then
    HELPDOCBASE = 'file:///usr/app/vendors/int/Conoco/xml/CFEalpha/help/'
  else if(MATURITY==2) then
    HELPDOCBASE = 'file:///usr/app/vendors/int/Conoco/xml/CFEbeta/help/'
  else if(MATURITY==3) then
    HELPDOCBASE = 'file:///usr/app/vendors/int/Conoco/xml/help/'
  else
    write(*,*) 'maturity is not set properly -- this is a bug'
    stop
  end if

  write(*,*) 'Help system docbasepath (for jpg, gif images) is ', HELPDOCBASE

  if(n1.gt.0) then
    i1=ezgui_find_chars(file_in,1,n1,'./')
    if(i1<n1) then
      i1=i1+2
    else
      i1=1
    endif

    i1=ezgui_find_chars(file_in,i1,n1,'.')
    if(n2.eq.0) then                                     !second argument
      file_out=file_in(1:i1-1)
    endif

    if(i1>n1) then                     !extension of first argument
      file_in=trim(file_in)//'.f90'
      n1=n1+4
    else
      i1=i1-1
    endif

    i2=ezgui_find_chars(file_out,1,n2,'./')
    if(i2<n2) then
      i2=i2+2
    else
      i2=1
    endif

    i2=ezgui_find_chars(file_out,i2,n2,'.')   !extension of second argument
    if(i2>n2) then
      file_out=trim(file_out)//'.xml'
      n2=n2+4
    endif

    print *,"Input file name="//trim(file_in)
    if(                                                                        &
     min(ezgui_find_chars(file_in,1,80,".xml"),                                &
     ezgui_find_chars(file_in,1,80,".XML"))< 80)                               &
     then
      call ezgui_read_xml_file(trim(file_in),n_elems, row_col_elems, istat)
    else
      call ezgui_read_layout_file(trim(file_in), cards, card_type, NUM_CARDS,  &
       n_elems, row_col_elems, istat)
    endif

    if(istat/=0) then
      print *,"Error in reading input file"
      return
    endif

    print '(1x,i4,a/)',n_elems, " field elements read from file"
    print *,"Output file name="//trim(file_out)
    if(ezgui_find_chars(file_out,1,80,".LAY") <80 .or.                         &
     ezgui_find_chars(file_out,1,80,".lay")<80) then
      call ezgui_write_layout_file(file_out,n_elems, row_col_elems, istat)
    else
      call ezgui_write_xml_file(file_out,n_elems, row_col_elems, istat)
    endif
    return
  endif

  100 print *,"option  action"
  print     *,"   0 - exit"
  print     *,"   1 - open input file"
  print     *,"   2 - save XML file"
  print     *,"   G - save GeoPRO XML file and other files"
  print     *,"   3 - close input session"
  print     *,"   4 - list field entries"
  print     *,"   5 - view GUI display"
  print     *,"   6 - display GUI help"
  print     *,"   7 - help (list documentation)"
  print     *,"   8 - set check help switch"
! print     *,"   9 - validate file"

  call ezgui_char_prompt_edit(a_work,"0","Enter desired option code")
  call ezgui_chars_upper_case(a_work)

  select case (trim(adjustl(a_work)))

  case ("0", "EXIT")
    if(.not.save_sw) then
      print *,"Active XML session not saved yet"
      call ezgui_save_xml_file(row_col_elems, n_elems, file_name)
    endif
    return

  case ("1", "OPEN")
    if(n_elems > 0) then
      if(.not.save_sw) then
        print *,"Active XML session not saved yet"
        call ezgui_save_xml_file(row_col_elems, n_elems, file_name)
      else
        call ezgui_int_prompt_edit(i_elem,0,                                   &
         "Active XML session underway, "//                                     &
         "enter 1 to close it, 0 to return to main menu")
        if(i_elem/=1) goto 100
      endif

      do i_elem=1,n_elems
        if(row_col_elems(i_elem)%help_size>0)                                  &
         deallocate(row_col_elems(i_elem)%help_info, stat=istat)
      enddo

      n_elems=0
    endif
    save_sw=.True.

    110 call ezgui_get_filename_in(                                            &
     "Enter input xml/layout/f90 file name[blanks to skip]:",file_name,istat)
    if(istat/=0) then
      print *,"Invalid file name specified-try again"
      goto 110
    endif
    if(len_trim(file_name)==0) goto 100

    if(                                                                        &
     min(ezgui_find_chars(file_name,1,80,".lay"),                              &
     ezgui_find_chars(file_name,1,80,".LAY"))<80) then
      i_elem=1
    else if(min(ezgui_find_chars(file_name,1,80,".xml"),                       &
       ezgui_find_chars(file_name,1,80,".XML"))<80) then
      i_elem=2
    else if(min(ezgui_find_chars(file_name,1,80,".f90"),                       &
       ezgui_find_chars(file_name,1,80,".F90"))<80) then
      i_elem=1
    else
      call ezgui_int_prompt_edit(i_elem,0,                                     &
       "Non-normal file name, enter 0 to skip, 1 for lay/f90, 2 for .xml")
    endif

    select case(i_elem)
    case (1)
      call ezgui_read_layout_file(file_name, cards, card_type, NUM_CARDS,      &
       n_elems, row_col_elems, istat)
    case (2)
      call ezgui_read_xml_file(file_name,n_elems, row_col_elems, istat)
    case default
      goto 100
    end select

    if(istat/=0) then
      print *,"Error in reading layout file"
      do i_elem=1,n_elems
        if(row_col_elems(i_elem)%help_size>0)                                  &
         deallocate(row_col_elems(i_elem)%help_info, stat=istat)
      enddo
      n_elems=0
    else
      print *,n_elems, " field elements read from file"
    endif
    goto 100

  case ("2","SAVE")
    if(n_elems==0) then
      call ezgui_char_prompt_edit(a_work,"",                                   &
       "Current XML seesion is empty, press return to return to main menu")
      goto 100
    endif

    call ezgui_save_xml_file(row_col_elems, n_elems, file_name)
    save_sw=.True.
    goto 100

  case ("G","GEOPRO")
    if(n_elems==0) then
      call ezgui_char_prompt_edit(a_work,"",                                   &
       "Current XML session is empty, press return to return to main menu")
      goto 100
    endif

    call ezgui_save_xml_file(row_col_elems, n_elems, file_name,geopro=.true.)
    save_sw=.True.
    goto 100

  case ("3","CLOSE")
    if(.not.save_sw) then
      print *,"Active XML work seesion not save yet"
      call ezgui_save_xml_file(row_col_elems, n_elems, file_name)
    endif

    save_sw=.True.
    do i_elem=1,n_elems
      if(row_col_elems(i_elem)%help_size>0)                                    &
       deallocate(row_col_elems(i_elem)%help_info, stat=istat)
    enddo

    n_elems=0
    call ezgui_char_prompt_edit(a_work,"",                                     &
     "XML session closed, press return to continue")
    goto 100

  case ("4","LIST")
    call ezgui_list_row_col_elems(row_col_elems, 1, n_elems,0)
!    call ezgui_char_prompt_edit(file_name,"","Press return to continue")
    goto 100

  case ("5","VIEW")
    call ezgui_view_row_col_elems(row_col_elems, n_elems,0)
    goto 100

  case ("6","DISPLAY")
    call ezgui_view_row_col_elems(row_col_elems, n_elems,1)
    goto 100

  case ("7","HELP")
    call ezgui_print_help
    goto 100

  case ("8","CHECKHELP")
    call ezgui_int_prompt_edit(check_help_sw,0,                                &
     "Enter 1 to set check help swith, 0 to reset:")
    goto 100

!  case ("9","VALIDATE")
!    call ezgui_check_row_col_elems(row_col_elems, n_elems,istat)
!    if(istat>0) save_sw=.False.
!    goto 100

  case default
    call ezgui_char_prompt_edit(a_work,"",                                     &
     "Invalid option entered, press return to continue")
    goto 100
  end select

  stop
end

subroutine ezgui_print_help
  use ezgui_module
  implicit none

  integer   :: i_pause
!
  i_pause=0
  print *,""
  print *,"********************************************************************"
  print *,"                COPYRIGHT NOTICE"
  print *,"     THIS IS CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC."
  print *,"       AND IS PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK."
  print *,""
  print *,"********************************************************************"
  print *,""
  print *,"      EzGUI Layout-File-To-GUI-XML Converter Documentation        "
  print *,"                Version 1.0- March 21, 2000"
  print *,""
  print *,"                     By Charles C Burch"
  print *,""
  print *,""
  print *,""
  print *,""
  print *,""
  print *,"EzGUI is a utility program to take an ASCII text layout of a GUI"
  print *,"definition and produce the needed XML (a computer standard called"
  print *,"extended markup language) used to produce the desired GUI appearance"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"in CFE and other future application without requiring the code"
  print *,"developer to know anything about XML or Java. In many ways, EzGUI is"
  print *,"similar to EzEdit. The layout files are similar, but there are"
  print *,"differences and additional features are supported. The following"
  print *,"summarizes some of the key features supported by EzGUI:"
  print *,""
  print *,"  * Labels"
  print *,"  * Editable fields for floating point, integer and character data"
  print *,"  * Arrays"
  print *,"  * Tables (linked arrays)"
  print *,"  * Push buttons"
  print *,"  * Radio push button"
  print *,"  * Borders"
  print *,"  * Combo-Boxes"
  print *,"  * Pop-up menus"
  print *,"  * Editable and non-editable fields"
  print *,"  * Left, right and center justification of labels."
  print *,"  * Sensitive and insensitive fields"
  print *,"  * On line help and tip information"
  print *,"  * Menu bar"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"  * Tool bar"
  print *,"  * Include files"
  print *,""
  print *,""
  print *,"                File Names"
  print *,""
  print *,"EzGUI was designed to produce GUI XML files for use with CPS and for"
  print *,"other future applications. Layout information can be included in"
  print*,"either layout files with the extension .lay or in Fortran source code"
  print*,"files with the extension .f90 (or .F90). Information in .f90 files is"
  print *,"automatically extracted and processed as if it was contained in a"
  print*,"layout file. This allows a single Fortran source file to include both"
  print *,"the Fortran code and the GUI layout to minimize getting such"
  print *,"information out of sync. Comment lines between the !<gui_def> and"
  print *,"!</gui_def> in a .f90 file will be extracted and treated as GUI"
  print *,"layout information. Comment lines between the !<HelpSection> and"
  print *,"!</HelpSection> will be read as help information. Fortran programs"
  print *,"with !<CPS_v1 type="//'"'//"PROCESS"//'"'//                         &
   "/> will have certain information inserted"
  print *,"that is consistent with what is required by CFE. Information in the"
  print *,"brief_doc, descript_doc, advice_doc and history_doc sections of a"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,".f90 file will be extracted and included as overview help"
  print *,"information."
  print *,""
  print *,"               Executing EzGUI"
  print *,""
  print *,"To run EzGUI, one enters EzGUI [-Idirectory] [inputfile"
  print *,"[outputfile]]. The -I command is optional and will be discussed in"
  print*,"the Include file Section later. The inputfile and outputfile are also"
  print *,"optional."
  print *,""
  print *,"Unless a file extension is specified, a extension of .f90 is assumed"
  print *,"for the input and .xml for the output. Valid input filename"
  print *,"extensions are .f90, .xml and .lay in either lower or upper case."
  print*,"Valid output filename extensions are .xml and .lay in either lower or"
  print *,"uppercase, but the support for .lay is limited as its usage was"
  print*,"intended for early-on file translations. The first filename specified"
  print*,"is assumed to be the input file name, and the second, if present, the"
  print *,"output file name. If inputfile is specified but outputfile is not,"
  print *,"the a default filename for the output will be the inputfile with .an"
  print *,"extension .xml. Normally one simply enters EzGUI filename, in which"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"case the layout is read from the file filename.f90 and the output is"
  print *,"filename.xml."
  print *,""
  print*,"If no filenames are specified, then EzGUI enters into menu mode where"
  print *,"EzGUI will then display the options available, which are below:"
  print *,""
  print *,"  EXIT - Quits execution of EzGUI"
  print *,"  OPEN - Reads in a .LAY .F90, or .XML file"
  print *,"  SAVE - Output the current GUI definition as a specified XML file"
  print *,"  CLOSE - Clears the current GUI definition and prepares to read a"
  print *,"          new file"
  print *,"  VIEW - Produces a screen dump (& file view.ezg) that contains dump"
  print *,"         of current GUI definition"
  print *,"  DISPLAY - Displays (& file view.ezg) the current help and tip"
  print *,"            contents of current GUI definition"
  print *,"  LIST - Lists a summary (& file list.ezg) of the current EzGUI"
  print *,"         variables"
  print *,"  HELP - Displays the latest EzGUI documentation."
  print *,""
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"The options can be selected by entered the above keywords or the"
  print *,"option number printed out by EzGUI. Normally, one reads on a layout"
  print *,"file with OPEN, writes the XML file using SAVE, and then EXIT. CLOSE"
  print *,"is used to start a new session using a new input file. LIST is a"
  print *,"diagnostic tool to see the internal names used by EzGUI in case of"
  print *,"problems; it displays on the screen as well as in the file"
  print *,""//'"'//"list.ezg"//'"'//                                           &
   ". VIEW produces an ASCII version of what the current layout"
  print *,"looks like, both on the screen as well as in the file "//'"'//      &
   "view.ezg"//'"'//"."
  print *,"DISPLAY produces an ASCII version of what the current help and tip"
  print *,"information looks like both on the screen and in the file "//'"'//  &
   "view.ezg"//'"'//";"
  print*,"it also lists the variables that do not have help or tip information."
  print *,"HELP produces a "//'"'//"man page like"//'"'//                      &
   " display of the latest EzGUI"
  print *,"documentation."
  print *,""
  print *,"           Labels and Prompt-edit fields"
  print *,""
  print *,"Like EzEdit, a text editor is used to produce a layout file with the"
  print *,"proper placement of labels, prompt-edit fields and other elements. A"
  print*,"group of characters starting with a non-blank character with no ` and"
  print *,"terminated by two or more consecutive blanks are considered a label."
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"Labels can have single blanks in them, but if one wants strings of"
  print *,"two or more blanks in a label field, then one should use ~'s. EzGUI"
  print*,"recognizes that multiple ~ are to be replaced with blanks. A single ~"
  print *,"in a label is left alone as one can already have a single blank"
  print *,"within a label."
  print *,""
  print *,"A prompt-edit field consists of an optional label and an edit-field"
  print*,"definition. A prompt-edit label is like a layout label but terminates"
  print *,"with the special character ` which also defines the start of the"
  print *,"associated edit-field definition. The character following the `"
  print *,"defines the type of field being edited. The characters F, I, S and X"
  print*,"represent floating point, integer, character strings and non-editable"
  print *,"character strings, respectively. The length of the edit field is"
  print *,"determine where the first blank character occurs after the `."
  print *,"Examples of prompt-edit fields are"
  print *,""
  print *,"  A=`FFF which is a 4-character floating point number fields "
  print *,"          (the `counts as a character)"
  print *,"  ABC = `II which is a 3-character integer field"
  print *,"  String:`SSSS which is a 5-character string field"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"  Date=`XXX which is a 4-character field that is not editable,"
  print *,"            but can be dynamically changed by CFE"
  print *,""
  print *,""
  print *,"                New Screen"
  print *,""
  print*,"A layout can consist of multiple screens. Each screen is started with"
  print *,"an NS command whose format is <NS title>. An example of simple EzGUI"
  print *,"layout is"
  print *,""
  print *,"<NS First Screen>            Title for this screen"
  print *,""
  print *,"    Variable 1 =`FF variable 2= `III"
  print *,""
  print *,""
  print *,"A screen can be scrollable, in which case, one uses <SS title>"
  print *,"instead of NS. <NS> and <SS> can include the commands /NC=xx or"
  print *,"/NR=xx to define number of rows and columns in the screen. Unless"
  print *,"specified differently, normal screens will have a minimum size of 24"
  print *,"rows and 80 columns and if either is a larger size, the actual value"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"is used."
  print *,""
  print *,"                    Keywords"
  print *,""
  print *,"EzGUI takes layout definitions and generates the needed XML to"
  print *,"produce the GUI screen for entering data. Keywords are used to"
  print *,"associate the input data, and the keywords are consistent with those"
  print *,"used in the parameter cache in CPS. EzGUI defines keywords for"
  print *,"prompt-edit fields in three ways. The default way is to concatenate"
  print *,"the capitalization of up to the first 32 alphanumeric characters of"
  print *,"the prompt field. In the example above, the keywords for the `FF"
  print *,"field is VARIABLE1 and for `III is VARIABLE2."
  print *,""
  print *,"The second way to specify a keyword is to include it within []'s. An"
  print*,"example would be Variable 1[V1] =`FF. In this case, the keyword would"
  print *,"be V1. In the display of the GUI, the [V1] would not appear. The use"
  print *,"of [] can cause misalignment of columns in the layout file of later"
  print *,"edit fields. To compensate for this, one can include - in [ ] or `"
  print *,"fields. These take up space in the layout files but are eliminated"
  print *,"when the GUI is displayed. An example would be"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,""
  print *,"<NS First Screen>           Title for this screen"
  print *,""
  print *,"Variable 1[--] Variable [V1]`FF [V2]`IIII"
  print *,""
  print *,"In most cases, one should probably use the <PARMS> command described"
  print *,"in a later section to specify non-default keywords, which avoids the"
  print *,"misalignment of information in a layout definition."
  print *,""
  print*,"It is possible to have a prompt label with no alphanumeric characters"
  print *,"and no keyword specified with []'s. In this case, EzGUI will print"
  print*,"out a warning and assign a dummy keyword. In most cases, the software"
  print *,"developer will have to modify the layout file and assign a keyword,"
  print *,"so the entered data can be coordinated with the CPS parameter cache."
  print *,""
  print *,"                Field modifiers"
  print *,""
  print *,"XML normally uses a proportional font whose characters take a"
  print *,"variable amount of space. This produces problems in aligning text"
  print *,"information. To help compensate for this, one assign left, right or"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print*,"center alignment field modifiers to a label by including /L, /R or /C"
  print *,"as part of a [] field either in the prompt-edit field definition or"
  print*,"in a later <PARMS command> that is discussed in the next section. The"
  print *,"keyword may be included or missing when using a field modifier. For"
  print *,"the above example, one should use the following"
  print *,""
  print *,"<NS First Screen>                Title for this screen"
  print *,""
  print *,"     Variable 1[/L] Variable 2[/L] "
  print *,"     [V1]`FF        [V2]`IIII"
  print *,""
  print *,""
  print *,""
  print *,"The first edit field has no prompt label, but it has the keyword V1"
  print*,"and the label Variable 1 would be left justified above it. The second"
  print *,"edit field is similar. Since all the field modifiers are of equal"
  print *,"size, everything aligns up fine. If they were of unequal size, one"
  print *,"would need to use -'s or blanks."
  print *,""
  print *,"EzGUI supports all features of CFE XML through field modifiers which"
  print*,"can be included in the optional [ ] field. The /L, /R and /C are such"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print*,"field modifiers. Below are the different field modifiers supported by"
  print *,"EzGUI:"
  print *,""
  print *,""
  print *,""
  print *,"  /C Right justified    /EN Editable no"
  print *,"  /EY Editable yes      /L Left justified"
  print *,"  /ML Max length        /R Right justified"
  print *,"  /SN Sensitive no      /SY Sensitive yes"
  print *,"  /XSF xStretch false   /XST xSretch true"
  print *,"  /YSF yStretch false   /YST ySretch true"
  print *,""
  print *,""
  print *,""
  print *,""
  print *,""
  print *,""
  print *,"The defaults for labels are center, editable-no, sensitive-yes,"
  print *,"xStretch=true, and yStretch=true. Similar defaults are used for"
  print *,"prompt-edit fields, but they are editable and left justified. The"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print*,"defaults are hopefully sufficient for a typical simple screen layout."
  print *,""
  print*,"The /ML=nnn modifier to set the maximum length of variable while only"
  print *,"the specified field edit length will be displayed in a scrollable"
  print *,"fashion."
  print *,""
  print *,"                     PARMS command"
  print *,""
  print*,"An alternate way to specify a keyword and field modifiers is with the"
  print *,"<PARMS> command. The format is <PARMS existing label or keyword"
  print *,"[optional new keyword/field modifiers]>."
  print *,""
  print *,"<PARMS ...> can appear anywhere in the screen layout definitions but"
  print *,"in most cases, it would be best to use them shortly after the"
  print*,"original prompt-edit fields is defined. You can have duplicate labels"
  print *,"in the layout file and the <PARMS > statements change the fields in"
  print *,"the same order as the appear in the layout file. The specified label"
  print *,"needs to compress to the same keyword as the original label"
  print *,"compressed to, so blanks and special characters do not matter."
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"An example would be"
  print *,""
  print *,"Field 1=`III Field 2=`IIII "
  print *,"<PARMS Field 1[KEYWORD1]> "
  print *,"<PARMS Field2[KEYWORD2]>"
  print *,""
  print *,"Array `FFF `FFF "
  print *,"<PARMS Array[array_kw/yst]>"
  print *,""
  print *,"Start time `FF "
  print *,"<PARMS Start time[TBEG/EN]>"
  print *,""
  print *,"Note: one could have also used <PARMS STARTTIME[TBEG/EN]>"
  print *,""
  print *,"                Arrays and Tables"
  print *,""
  print *,"Two types of arrays are supported by EzGUI, which are similar to"
  print *,"arrays in EzEdit. The two types are single column arrays and"
  print *,"linked-column or table arrays. Multiple lines are used to define"
  print *,"arrays within the layout file. The second line of the definition is"
  print *,"key to the definition. The first element must be a `. The next"
  print *,"character must be a valid field definer (F, I, S, or X). The end of"
  print *,"the field is determined by the location of the next double blank or"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"`. If it is a blank, the array is a single column, and if it is a `,"
  print*,"the columns are linked to form a table where the elements of each row"
  print *,"are treated together. The columns are considered linked until a"
  print *,"double blank is found. The labels for each column of the array are"
  print *,"the characters in the first line of the array definition right above"
  print *,"the field definition in the second line, which defines the width of"
  print *,"the field. The number of rows to be displayed at one time (in a"
  print*,"scrollable window) is determined by the number of lines in the layout"
  print *,"definition that are identical to the second line defining the field"
  print *,"definition. Keywords, field modifiers and - can be used as with"
  print *,"scalar prompt edit fields. An extra 3 spaces will be added to the"
  print *,"width of each array by EzGUI to try to compensate for the scroll bar"
  print *,"inserted by CFE. Below is an example with a one-column array and a"
  print *,"3-column table"
  print *,""
  print *,"<NS Array Example Layout>"
  print *,""
  print *,"     simple array[A]  Column 1 Column 2 Column 3"
  print *,"     `SSSSSSSSSS----  `FFFFFFFF`IIIIIIII`FFFFFFF"
  print *,"     `SSSSSSSSSS----  `FFFFFFFF`IIIIIIII`FFFFFFF"
  print *,"     `SSSSSSSSSS----  `FFFFFFFF`IIIIIIII`FFFFFFF"
  print *,"     `SSSSSSSSSS----  `FFFFFFFF`IIIIIIII`FFFFFFF"
  print *,"     `SSSSSSSSSS----"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,""
  print*,"The first array is only one column. It consists of character strings,"
  print*,"each 10 characters long. The label is simple array and the keyword is"
  print *,"A. In the GUI display 5 elements would be displayed at a time. The"
  print *,"second array is a table of 3 columns. The first and second columns"
  print *,"contain floating-point number and the second integers. The column"
  print *,"names are as shown and the key words for the columns would be"
  print *,"COLUMN1, COLUMN2 and COLUMN3. The keyword for the entire table would"
  print *,"be the keyword for the first column concatenated with _ARRAYSET. In"
  print *,"this case, it would be COLUMN1_ARRAYSET."
  print *,""
  print *,"                Borders"
  print *,""
  print *,"Some new features in EzGUI that were not in EzEdit are borders,"
  print *,"pushbuttons, and radio buttons. A border is a rectangular box"
  print *,"enclosing information in the GUI display. Borders are defined in the"
  print*,"layout using the field definer `- to indicate start of the left upper"
  print *,"corner of the box. The right top corner of the box is the character"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"position of the last non-blank character after the `- box definer."
  print*,"The bottom left corner of the box is defined by the first `- found in"
  print *,"lines of the layout that have the same column position as the top"
  print *,"left corner of the box. The characters in the layout between the top"
  print *,"and bottom lines in the column of the left side are replaced by a"
  print *,"blank if they are "//'"'//"|"//'"'//  &
          " characters. It is recommended that the"
  print *,"character | be used to show the look of the box in the layout and to"
  print *,"ensure the corners line up. Boxes may be nested. The box may contain"
  print *,"labels, prompt-edit fields, etc. Below is a layout example with"
  print *,"sample border boxes:"
  print *,""
  print *,"<NS Border Example>"
  print *,"     `-------------------------------------"
  print *,"     |                                        `-------------------"
  print *,"     | `-------------------------             | Hi World"
  print *,"     | | Hello World                          `-------------------"
  print *,"     | `-------------------------"
  print *,"     |"
  print *,"     `-------------------------------------"
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,""
  print *,"           Push Buttons and Radio Buttons"
  print *,""
  print*,"Push buttons are defined to EZGUI by using the field definer `P after"
  print *,"the label of the push button. In the display by CFE the pushbutton"
  print *,"will have the specified label and CFE will be signaled whenever the"
  print*,"pushbuttons pressed. A group of pushbuttons may be linked together to"
  print *,"form a radio pushbutton where only one pushbutton can be active at"
  print *,"one time. All buttons with` R define the radio pushbuttons that will"
  print *,"be linked together. A double blank terminates the definition of a"
  print *,"radio button. The keyword for the push buttons will be the same as"
  print *,"with other EzGUI elements, and field modifiers may be used. If one"
  print *,"wishes to have a pushbutton that has a label that is two lines"
  print *,"instead of one, one uses the field modifier /2 and places the second"
  print *,"line of label text directly below the pushbutton information of the"
  print *,"first line. In multi-line label pushbuttons, the size of the push"
  print *,"button is determined by the size of the label in the first line (~"
  print *,"may be needed if the second line of the label is longer than the"
  print *,"first line). . Push buttons can also be two lines high by specifying"
  print *,"`D instead of `P. An example using push buttons is below:"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,""
  print *,"<NS Button example>"
  print *,""
  print *,"     Do it`P Option 1`Roption 2`R Option 3`R"
  print *,""
  print *,"In this example we have a simple push button and a 3 element radio"
  print *,"pushbutton. The pushbutton has keyword DOIT. The radio button has"
  print *,"individual pushbutton keywords OPTION1, OPTION2 and OPTION3. The"
  print*,"radio button itself has a keyword by the concatenation of the keyword"
  print *,"of its pushbuttons. (in this case OPTION1OPTION2OPTION3)."
  print *,""
  print *,"Examples of two line push buttons would be"
  print *,""
  print *,"    Do`D       Save[\2]]`P"
  print *,"    It         Defaults"
  print *,""
  print *,""
  print *,"Radio buttons can extend past one line. If the last `R in a line for"
  print *,"a radio button is `R&, the radio buttons are assume to continue on"
  print*,"the next line in the column where the first entry of the radio button"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"occurred. You can have as many lines in a radio button definition as"
  print*,"you want. If you want a blank line between the rows of the buttons in"
  print *,"the radio button, simply use `R& with no label at the line where you"
  print *,"want a blank line."
  print *,""
  print *,"                Combo-Boxes"
  print *,""
  print*,"A special type of prompt-edit field is the combo-box. The format is a"
  print *,"label followed by an edit field definition which begins with `C. The"
  print*,"rules for keywords, etc are the same as for other prompt-edit fields."
  print *,"Data is entered into a combo-box through selecting one of the"
  print *,"possible options in a pull down menu. One can click on the desired"
  print *,"option to be chosen or start typing in the option in which case the"
  print *,"desired option is selected as soon as it can be uniquely identified"
  print *,"using the typed-in information. An example of the use of a combo-box"
  print *,"is            Smooth data =`CCC"
  print *,""
  print *,"In this case, the programmer could define the available options as"
  print *,"yes and no, and the user then selects which one is desired."
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"                Pop-up Menus"
  print *,""
  print *,"Each screen can have an optional pop-up menu that is activated using"
  print *,"mouse button 3. The EzGUI commands <PM keyword> defines start of"
  print*,"popup menu and </PM> defines end of popup menu. One can have only one"
  print *,"pop-up menu per screen. Between <PM> and </PM> are lists of"
  print *,"single-line pushbuttons., one per line in the layout. An example"
  print *,"pop-up menu is"
  print *,""
  print *,"<PM popup1> "
  print *,"option 1 "
  print *,"option 2 "
  print *,"option 3 "
  print *,"</PM>"
  print *,""
  print *,"                Layout Overlays"
  print *,""
  print *,"Sometimes, one wants to be able to overlay information in the GUI"
  print *,"definition with other information. This is accomplished with the"
  print *,"<ROW> command, but it is recommended that this command not be used"
  print *,"for normal applications. The formats are:"
  print *,"  <ROW=nn> to set the row number of the next layout card to nn"
  print *,"  <ROW+=nn> to add nn to the current row number"
  print *,"  <ROW-=nn> to subtract nn from the current row number"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,""
  print *,""
  print *,"<ROW-=1> says the row number of the next layout card is the current"
  print *,"row number minus one, which is the row number of the previous card."
  print*,"This allows overlaying the previous line with additional information."
  print *,"This might be useful when the previous line gets too crammed because"
  print *,"of field modifiers, making push buttons lie next to one other, or to"
  print *,"overlay the line with fields that get turned on and off. Overlapping"
  print *,"fields are not supported with the View command or the write layout"
  print *,"command. It works with XML output files, which is what it was"
  print *,"designed for."
  print *,""
  print *,"<ROW> cards are not to be used within the definition of arrays or"
  print *,"borders but can be used to define the row number of the start"
  print *,"position."
  print *,""
  print *,"               Help and Tip Information"
  print *,""
  print *,"CFE supports field sensitive help and user tip information. EzGUI"
  print *,"supplies this information using the <HELP> statements. The format is"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"<HELP keyword> ......tip information ..... ..... Help information"
  print *,"..... .... <Help different keyword>, <NS ...> or end of file"
  print *,""
  print *,"Normally, help is to be placed after the layout of the screen, so it"
  print *,"does not interfere with the look of the screen in the layout file."
  print *,"Help can be inserted in the screen definition but in this case it"
  print *,"must be terminated by the special field terminator <ENDHELP>, so"
  print *,"EzGUI knows that it need to return to screen definition mode inside"
  print*,"of treating successive lines after elements the help. For clarity, it"
  print *,"probably best to terminate the last help statement with a <ENDHELP>,"
  print *,"but EzGUI will supply it if missing when the end of the file is"
  print *,"found."
  print *,""
  print *,"The following is a complete layout for a simple layout screen:"
  print *,""
  print *,"<NS Simple Screen Layout>"
  print *,"                     Example Layout Example"
  print *,""
  print *,"          First variable =`FF   Second variable[var2]=`IIII"
  print *,""
  print *,"<HELP FIRSTVARIABLE>Tip information for First Variable"
  print *,"This is the help for first variable"
  print *,"<HELP VAR2>Tip information for Second variable"
  print *,"This is the help for the second variable"
  print *,"<ENDHELP>"
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"EzGUI also supports help that is in XML format. In addition, EzGUI"
  print *,"will try to compensate for some of the common errors found with some"
  print *,"of the user-written XML help."
  print *,""
  print *,"The Help produced by EzGUI is in HTML, and specified spaces and new"
  print *,"lines are honored. One could imbed HTML with the help, but this is"
  print *,"discouraged for most applications. All <, >, and &'s that are"
  print*,"followed by a space will be converted to Unicode by EzGUI. . All <, >"
  print *,"and &'s in the help text that are not followed by a space should be"
  print *,"replaced by the Unicode "//'"'//"&lt;"//'"'//", "//'"'//"&gt;"//   &
          '"'//", and "//'"'//"&amp;"//'"'//", respectively, as"
  print *,"the CFE Help interpreter may try to treat them as part of a HTML"
  print *,"command otherwise."
  print *,""
  print *,"CFE supports a special type of help called Process Help, which is"
  print *,"intended as overview help for a given CPS process. Process Help is"
  print *,"defined in EzGUI by using <HELP> with no keyword. Process help is"
  print*,"automatically extracted when the input file is .f90 file that follows"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"CPS documentation standards."
  print *,""
  print *,"                Include Statement"
  print *,""
  print *,"EzGUI supports the ability to include layout information from"
  print*,"multiple .lay or .f90 files into a single GUI layout definition. This"
  print *,"allows common entities to be defined once and be included in other"
  print *,"layout files. The format is <INCLUDE filename>. The contents of the"
  print *,"included filename will be read and used as if it was inserted into"
  print *,"the layout file at the specified place the include statement"
  print *,"occurred. Include files can be nested so an Include file can include"
  print *,"another Include files, etc. The process help for the included .f90"
  print *,"files are skipped."
  print *,""
  print *,"The default directory for searching for include files are"
  print *,"/usr/app/vendors/sps/etc/ and then the current directory. One can"
  print *,"specify other directories to first search for include files by using"
  print *,"up to ten "//'"'//"-Idirectory"//'"'//   &
          " commands with the EzGUI command in the order"
  print *,"in which you want the directories to be search. The first directory"
  print *,"that includes the specified include file is the one that will be"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"used."
  print *,""
  print *,"                GUI Components"
  print *,""
  print*,"A GUI display can have multiple components, which are as (1)Menu Bar,"
  print *,"(2)Tool Bar, (3)Top Area Component, (4)Screens, (5)Bottom Area"
  print *,"Component, and (6)Help Panel. In most cases, one only needs to be"
  print *,"concerned with the screen elements in CFE, as the others are"
  print *,"automatically handled by include files (which are discussed later)"
  print *,"inserted by EzGUI when the layout information is extracted from .f90"
  print *,"files."
  print *,""
  print *,"                     Menu Bar"
  print *,""
  print *,"The Menu Bar appears on the top of the screen, if it is present, and"
  print*,"consists of a number of menu items each which will create a drop down"
  print *,"list of pushbuttons when activated by the mouse. As mentioned"
  print *,"earlier, the Menu Bar is not intended for use by most applications."
  print *,"The EzGUI command <MB Menu Item Name> starts the definition of the"
  print*,"toolbar and </MB> terminates an entry of a menu item for the toolbar."
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"Between <TB Menu Item Name> and </TB>are lists of single-line"
  print *,"pushbuttons, one per layout line. An example of a Menu Bar would be"
  print *,"<MB File> "
  print *,"Workfile Builder "
  print *,"Build Jobfile "
  print *,"[SUBMIT_JOB]Submit Job"
  print *,"Session Log Exit"
  print *,"<MB Edit> "
  print *,"[EDITWORKFILE]Text "
  print *,"Edit Workfile"
  print *,"[EDITJOBFILE]Text "
  print *,"Edit Jobfile "
  print *,"[EDITFILE]Text Editor "
  print *,"<MB View>"
  print *,"View"
  print *,"Workfile "
  print *,"View Jobfile "
  print *,"View Reportfile"
  print *,"[VIEWFILE]View any text file"
  print *,"<MB Options>"
  print *,"[CHANGEDIRECTORY]Change Working Directory "
  print *,"Toolbar "
  print *,"Editor"
  print *,"User Preferences "
  print *,"<MB Tools> "
  print *,"[MULTIFILEBUILDER]Multi-Workfile Builder"
  print *,"Utility Launcher "
  print *,"JOBMON "
  print *,"<MB HELP> "
  print *,"Application"
  print *,"Help Application"
  print *,"Version DOC "
  print *,"</MB>"
  print *,""
  print *,"                     Tool Bar"
  print *,""
  print *,"The next item to be displayed on the GUI display is the Tool Bar, if"
  print *,"it is present. The Tool bar consists a single line of pushbutton"
  print *,"entries. As mentioned earlier, the Tool Bar is not intended for use"
  print*,"by most applications. The EzGUI command <TB> starts the definition of"
  print *,"the toolbar and </TB> terminates it. Between <TB> and </TB>are lists"
  print*,"of single-line pushbuttons, one per layout line. An example of a Tool"
  print *,"Bar would be "
  print *,"<TB> "
  print *,"[UTILITYLAUNCHERTB]Utils "
  print *,"[DOCTB]DOC"
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"[VIEWWORKFILETB]VWF "
  print *,"[VIEWJOBFILETB]VJF "
  print *,"[VIEWREPORTFILETB]VRF"
  print *,"[JOBMONTB]JOBMON "
  print *,"</TB>"
  print *,""
  print *,"      Top/Bottom Area Component, Help Panel, and other options"
  print *,""
  print *,"Below the toolbar can be an optional Top Area Component, which is"
  print *,"defined using the EzGUI command <TA> to start Top Area Component and"
  print*,"the command </TA> to define its end . One can have one screen between"
  print *,"<TA.> and </TA> entries defined by <NS> and other GUI prompt edit"
  print *,"fields."
  print *,""
  print *,"After the Top Area Component normally comes the GUI screens, which"
  print *,"have already been discussed using <NS> or <SS> entries."
  print *,""
  print *,"Below the screens can be an optional Bottom Area Component which is"
  print *,"defined using the EzGUI command <BA> to start Bottom Area Component"
  print *,"and the command </BA> to define its end . One can have one screen"
  print *,"between <BA> and </BA> entries defined by <NS> and other GUI prompt"
  print *,"edit fields."
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"One can also have an optional help panel line where tip information"
  print *,"is displayed by using the <HP keyword position> command, where the"
  print *,"available positions are top, bottom, topMost, bottomMost."
  print *,""
  print *,"As mentioned earlier, Top/Bottom Area Components and the Help Panel"
  print *,"are not intended for use by most applications. The <BA>, <TA> and"
  print *,"<HP> commands may be placed anywhere in the layout but are"
  print *,"recommended to be used at the location where they are desired to be"
  print *,"displayed. The default size for Top and Bottom Area Components are"
  print *,"the actual number of rows in their definition."
  print *,""
  print *,"An example using <HP>, <TA> and <BA> is"
  print *,""
  print *,"<HP HPKW bottom> "
  print *,"<TA> "
  print *,"<NS TACS>"
  print *,"                  [/C/XST]Top Area Component"
  print *,""
  print *,"Top Area Push Button`P "
  print *,"</TA>"
  print *,"<BA> "
  print *,"<NS BASC>"
  print *,"                        [/C/XST]Bottom Area Component"
  print *,""
  print *,"Bottom Area Push Button`P "
  print *,"</BA>"
  print *,""
  if(i_pause/=2) then
    call ezgui_int_prompt_edit(i_pause,0,                                      &
     "Enter 0 to continue, 1 to quit, 2 to bypass pauses")
    if(i_pause==1) return
  endif
!
  print *,"The EzGUI command <WINDOW WIDTH=xx HEIGHT=xx> is an option in the"
  print *,"layout file to specify the window size in pixels. In most cases, one"
  print *,"should not use this command and a default of 1024 by 768 is used."
  print *,""
  print*,"EzGUI can be used to create modal or non-modal Dialog XML files which"
  print*,"is intended for special applications. In the layout, one uses <Dialog"
  print *,"....> instead of <NS> with the options Width=nn,"
  print *,"  Height=nn, and Modal (which sets modal to true otherwise modal is"
  print *,"false). Examples are <Dialog> and <Dialog Width=764 Modal>."
  print *,""
!
  return
end

!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

!!! The code below is Fortran driver code to allow ezgui to be used with
!!! Microsoft F90 compiler(with no C code needed) when the !!'s are removed.
!!!
!!  subroutine ezgui_get_arg(arg_num, arg, istat)
!!    integer, intent(in)            :: arg_num
!!    character (len=*), intent(out) :: arg
!!    integer, intent(out)           :: istat
!!
!!    integer                        :: iargc
!!    integer(2)                     :: i_arg_num, i_stat !for MS F90
!!!   integer                        :: i_arg_num         !for sun and pgf90
!!
!!    if(arg_num<0 .or. arg_num>iargc() ) then
!!      istat=-1
!!      arg=" "
!!      return
!!    endif
!!
!!    istat=1
!!    i_arg_num=arg_num
!!    call getarg(i_arg_num, arg, i_stat)  !for MS f90
!!!   call getarg(i_arg_num, arg)          !for sun and pgf90
!!    return
!!  end subroutine ezgui_get_arg
!!
!!  subroutine ezgui_sleep(n)
!!    integer, intent(in) :: n
!!    call sleep(n)
!!    return
!!  end subroutine ezgui_sleep
!!
!!  subroutine ezgui_system(comm)
!!    character(len=*), intent(in) :: comm
!!    call system(comm)
!!    return
!!  end subroutine ezgui_system
!!
!!program ezgui
!!  use ezgui_module
!!  implicit none
!!
!!  character (len=1)   :: file_in(80), file_out(80)
!!  character (len=80)  :: command_arg, command_arg_upper_case
!!  integer             :: istat, i1, jbeg
!!  integer             :: i, n1, n2
!!  character           :: dirs(80, N_INCLUDE_DIRS_SIZE)
!!  integer             :: n_dirs
!!!
!!! get any - commands
!!!
!!  i1=1
!!  n_dirs=0
!!  do while(.true.)
!!    call ezgui_get_arg(i1,command_arg, istat)
!!    if(istat<0) exit
!!    if(command_arg(1:1)/='-') exit
!!
!!    i1=i1+1
!!    command_arg_upper_case=command_arg
!!    call ezgui_chars_upper_case(command_arg_upper_case)
!!
!!! See of -checkhelp specified
!!
!!    if(command_arg_upper_case(1:11).eq."-CHECKHELP ") then
!!      check_help_sw=1
!!      cycle
!!    endif
!!
!!! See  if -nocheckhelp is specified
!!
!!    if(command_arg_upper_case(1:13).eq."-NOCHECKHELP ") then
!!      check_help_sw=0
!!      cycle
!!    endif
!!
!!! See if any includes of form -Idir or -I dir are specified
!!
!!    if(command_arg_upper_case(1:2)=='-I') then
!!      jbeg=3
!!      if(command_arg(3:3).eq." ") then
!!        call ezgui_get_arg(i1,command_arg, istat)
!!        if(istat<0) exit
!!        if(command_arg(1:1)=='-') then
!!          print *,"WARNING -I command found with no directory specified"
!!          cycle
!!        endif
!!        i1=i1+1
!!        jbeg=1
!!      endif
!!
!!      if(n_dirs<N_INCLUDE_DIRS_SIZE-2) then
!!        n_dirs=n_dirs+1
!!        do i=1,80
!!          dirs(i,n_dirs)=command_arg(jbeg+i-1:jbeg+i-1)
!!        enddo
!!      else
!!        print *, 'Too many -I commands:-I'//trim(command_arg)//' skipped'
!!      endif
!!      cycle
!!    endif
!!
!!    print *,'Invalid -command option found:('//trim(command_arg)//') skipped'
!!  enddo
!!
!!  do i=1, n_dirs
!!    do n1=80,1,-1
!!      if(dirs(n1,i).ne.' ') exit
!!    enddo
!!    if(include_dirs(i)(n1:n1)/='/') include_dirs(i)(n1+1:n1+1)='/'
!!  enddo
!!
!!  command_arg='/usr/app/vendors/sps/etc/'
!!  do i=1,80
!!    dirs(i,n_dirs+1)=command_arg(i:i)
!!  enddo
!!  command_arg='./'
!!  do i=1,80
!!    dirs(i,n_dirs+2)=command_arg(i:i)
!!  enddo
!!  n_dirs=n_dirs+2
!!
!!  file_in=' '
!!  file_out=' '
!!  n1=0
!!  n2=0
!!  call ezgui_get_arg(i1,command_arg, istat)
!!  if(istat>=0) then
!!    n1=len_trim(command_arg)
!!    do i=1,n1
!!      file_in(i)=command_arg(i:i)
!!    enddo
!!
!!    i1=i1+1
!!    call ezgui_get_arg(i1,command_arg,istat)
!!    if(istat>0) then
!!      n2=len_trim(command_arg)
!!      do i=1,n2
!!        file_out(i)=command_arg(i:i)
!!      enddo
!!    endif
!!  endif
!!
!!  call ezgui_f(check_help_sw, dirs, n_dirs, file_in, file_out)
!!  stop
!!end program ezgui

