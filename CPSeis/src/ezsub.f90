!<CPS_v1 type="PROGRAM"/>
!!------------------------------ ezsub.f90 --------------------------------!!
!!------------------------------ ezsub.f90 --------------------------------!!
!!------------------------------ ezsub.f90 --------------------------------!!


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
! Name       : EZSUB
! Category   : stand-alone
! Written    : 2001-07-25   by: Charles C. Burch
! Revised    : 2001-07-25   by: Karen Goodger
! Maturity   : beta
! Purpose    : General purpose text substitution for computer code files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! general purpose text substitution for computer code files
!  execute: ezsub command_file_name
!    commands in command_file_name
!      input=input_filename
!      output=output_filename
!      T/n1/n2/n3/
!      s/text1/text2/[[l1][:l2]]
!      S/text1/text2/[[l1][:l2]]
!      c/text1/text2/[[l1][:l2]]
!      C/text1/text2/[[l1][:l2]]
!        T set tab settings which replaces tabs with spaces
!        c change all occurences of text1 (case sensitive) with text2
!        C change all occurences of text1 (case insensitive) with text2
!        s case sensitive replace text1 with text 2, when text1 is a variable
!        S case insensitive replace text1 with text 2, when text1 is a variable
!        l1=beginning line number, l2=ending line number
!        Default value for l1 is 1.
!        Default value for l2 is last line number in the input file.
!
!        l1 and/or l2 can optionally be literals.
!        Literals are specified by starting it with a deliminator,
!        such as ", and ending with same deliminator.
!        For l1, the text of the file is searched and the line number where
!        the first occurrence of the literal occurs is used for l1.
!        For l2, the text of the file is searched and the line  number after
!        l1 with the first occurrence of the literal is used for l2.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!    create a command_file using the instructions found in the general
!    description section.
!    execute: ezsub command_file_name
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author            Description
!     ----        ------            -----------
!  3.
!  2. 2001-07-25  Karen Goodger     Put into template format.
!  1. 2001-07-25  Charles C. Burch  Initial version.
!
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
!  Due to the getarg routine, the pgf90 compiler must be used on linux
!
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

module ezsub_module

contains

!                    General purpose routines
!***********************************************************
!  gets command line parameter arg_num and places results
!    into arg
!    istat=-1 if error, +1 if n error
!
!  This needs modification for differnt compilers
!
!  Written March 2000 by Charles C Burch
!***********************************************************
subroutine ezsub_get_arg(arg_num, arg, istat)
  implicit none
  integer, intent(in)            :: arg_num
  character (len=*), intent(out) :: arg
  integer, intent(out)           :: istat

  integer                        :: iargc
! integer(2)                     :: i_arg_num, i_stat    !for MS F90
  integer                        :: i_arg_num, i_stat   !for sun and pgf90

  if(arg_num<0 .or. arg_num>iargc() ) then
    istat=-1
    arg=" "
    return
  endif

  istat=1
  i_arg_num=arg_num
! call getarg(i_arg_num, arg, i_stat)  !for MS f90
  call getarg(i_arg_num, arg)          !for sun and pgf90
  return
end subroutine ezsub_get_arg

!*************************************************************
!  Searches string str(ibeg:iend) for the string srch
!  and returns the starting index in str where srch occurs
!  Return iend+1 if srch is not found
!
!  Written July 1999 by Charles C Burch
!**************************************************************
integer function ezsub_find_chars(str, ibeg, iend, srch)
  implicit none

  character (len=*), intent(in):: str, srch
  integer, intent(in) :: ibeg, iend

  integer :: i                          !, srch_len

  if(ibeg>iend) then
    ezsub_find_chars=iend+1
    return
  endif

  i=index(str(ibeg:iend),srch)
  if(i==0) then
    ezsub_find_chars=iend+1
  else
    ezsub_find_chars=ibeg+i-1
  endif
  return
end function ezsub_find_chars

!*************************************************************
!  Sees if string str(ibeg:ibeg+len(str1)-1) is str1
!    returns ibeg if there otherwise iend+1
!
!  Written November 1999 by Charles C Burch
!**************************************************************
integer function ezsub_check_chars(str, ibeg, iend, str1)
  implicit none
  character (len=*), intent(in)    :: str, str1
  integer, intent(in)              :: ibeg,iend
  integer                          :: n

  n=len(str1)
  ezsub_check_chars=iend+1
  if( n<=(iend-ibeg+1)) then
    if(str(ibeg:ibeg+n-1)==str1) ezsub_check_chars=ibeg
  endif
  return
end function ezsub_check_chars

!*****************************************************************
!  Searches string str(ibeg:iend) for non occurence of string srch
!  Returns the starting index in str where srch does not occurs
!  Returns iend+1 if srch is not found
!
!  Written July 1999 by Charles C Burch
!*****************************************************************
integer function ezsub_find_not_chars(str, ibeg, iend, srch)
  implicit none

  character (len=*), intent(in):: str, srch
  integer, intent(in) :: ibeg, iend

  integer :: i, srch_len


  srch_len=len(srch)
  if(iend-ibeg+1<srch_len) then
    ezsub_find_not_chars=ibeg
    return
  endif
  do i=ibeg, iend
    if(str(i:i+srch_len-1)/=srch) then
      ezsub_find_not_chars=i
      return
    endif
  end do
  ezsub_find_not_chars=iend+1
  return
end function ezsub_find_not_chars
!***************************************************************
! Convert character string to upper case
!
! Written July 1999 by Charles C Burch
!***************************************************************
subroutine ezsub_chars_to_upper(str)
  implicit none

  character (len=*), intent(inout) :: str

  integer            :: i, str_len, ich
!      The pgf compiler aborts on the following
!!  integer, parameter :: ia=ichar('a'), iz=ichar('z'), &
!!                        i_adj=ichar('A')-ichar('a')
!      So am using the equivalent
  integer, parameter :: ia=97, iz=122,i_adj=-32



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
end subroutine ezsub_chars_to_upper
!***************************************************************
! return integer from decimal character data in str
!   istat=0 if no error, /=0, if error
!
! Written December 1999 by Charles C Burch
!***************************************************************
integer function ezsub_chars_to_int(str, istat)
  implicit none

  character (len=*), intent(in)    :: str
  integer, intent(out)             :: istat

  integer                          :: i, i1, str_len, isign

  ezsub_chars_to_int=0
  str_len=len(str)
  istat=0
  isign=1

  do i1=1, str_len
    if(str(i1:i1)/=' ') exit
  enddo
  if(i1>str_len) return

  if(str(i1:i1)=='-') then
    isign=-1
    i1=i1+1
  else if(str(i1:i1)=='+') then
    i1=i1+1
  endif

  do i=i1,str_len
    select case(str(i:i))
    case ('0':'9')
      ezsub_chars_to_int=10*ezsub_chars_to_int+ichar(str(i:i))-ichar('0')
    case (' ')
    case default
      istat=-1
    end select
  enddo

  ezsub_chars_to_int=isign*ezsub_chars_to_int
  return
end function ezsub_chars_to_int

!***************************************************************
! Convert integer j_in into character string a
!
! Written July 1999 by Charles C Burch
!***************************************************************
subroutine ezsub_int_to_chars(j_in,a_out)
  implicit none

  integer, intent(in)            ::j_in
  character (len=*), intent(out) :: a_out

  character         :: a*20

  write(a,'(i20)') j_in
  a_out=adjustl(a)
  return
end subroutine ezsub_int_to_chars
!***************************************************************
! Simple edit character varable with default value
!
! Written July 1999 by Charles C Burch
!***************************************************************
subroutine ezsub_char_prompt_edit(a_out, a_in, prompt)
  implicit none

  character (len=*), intent(in)  :: a_in, prompt
  character (len=*), intent(out) :: a_out

  character (len=132) :: a_save

  if(len(a_in)>0) then
    print '(1x,a,$)',prompt//'('//trim(a_in)//'):'
  else
    print '(1x,a,$)',prompt//':'
  endif
  a_save=a_in                                    !in case a_in same as a_out
  read (*,'(a)') a_out
  if(len_trim(a_out)==0) a_out=a_save
  return
end subroutine ezsub_char_prompt_edit

!***************************************************************
! Save a string into list of literals
!
! Written July 2001 by Charles C Burch
!***************************************************************
subroutine ezsub_put_lit(lit,n_lits,lits,lit_sizes,lit_values,lit_sw,lit_index)
  character (len=*), intent(in)    :: lit
  integer,           intent(inout) :: n_lits, lit_sizes(:), lit_values(:)
  character (len=*), intent(inout) :: lits(:)
  integer,           intent(out)   :: lit_index
  logical,           intent(inout) :: lit_sw

  integer                          :: i

  do i=1, n_lits
    if(lit.eq.lits(i)(1:lit_sizes(i))) then
      lit_index=i
      return
    endif
  enddo

  if(n_lits.ge.size(lit_values,dim=1)) then
    print *,"Warning: lit overflow-(",trim(lit),") not processed"
    lit_index=0
    return
  endif

  lit_sw=.true.
  n_lits=n_lits+1
  lits(n_lits)=lit
  lit_sizes(n_lits)=len(lit)
  lit_values(n_lits)=-1
  lit_index=n_lits
  return
end subroutine ezsub_put_lit

!***************************************************************
! Check if string contains any of the list of literals
!
! Written July 2001 by Charles C Burch
!***************************************************************
subroutine ezsub_check_lit_values(str,n_lits,lits,lit_sizes, lit_values, isw)
  character (len=*), intent(in)    :: str
  integer,           intent(in)    :: n_lits, lit_sizes(:)
  integer,           intent(inout) :: lit_values(:)
  character (len=*), intent(in)    :: lits(:)
  integer,           intent(out)   :: isw

  integer                          :: n_str, i

  isw=0
  n_str=len(str)

  do i=1, n_lits
    if(lit_values(i).lt.0) then
      if(ezsub_find_chars(str,1,n_str,lits(i)(1:lit_sizes(i))).le.n_str) then
        isw=1
        lit_values(i)=1
      endif
    endif
  enddo
  return
end subroutine ezsub_check_lit_values

end module ezsub_module


!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!


      program ezsub
      use ezsub_module
      implicit none

      character(len=100),save :: EZSUB_IDENT = &
'$Id: ezsub.f90,v 1.2 2001/07/25 21:55:08 Goodger beta sps $'

  integer, parameter   :: N_KEYS_MAX=256
  character (len=256)  :: card_in, card_in_upper_case, file_in, file_out

  character (len=80)   :: keys_in(N_KEYS_MAX), keys_out(N_KEYS_MAX)
  character (len=80)   :: lits(N_KEYS_MAX)
  integer              :: keys_in_len(N_KEYS_MAX), keys_out_len(N_KEYS_MAX)
  integer              :: keys_line_beg(N_KEYS_MAX), keys_line_end(N_KEYS_MAX)
  character (len=1)    :: keys_command(N_KEYS_MAX)

  integer              :: key_match, key_no_match, n_matchs, n_no_matchs
  integer              :: n_tot_matchs,n_tot_no_matchs
  integer              :: i_line, n_in, n_out, n_a_out
  integer              :: lit_sizes(N_KEYS_MAX), n_lits, lit_values(N_KEYS_MAX)
  integer              :: i, n, istat, n_keys, card_len, i_beg, i_end, n_delims
  character (len=1)    :: delim, s_or_blank
  character (len=30)   :: delims, a_out, b_out
  logical              :: match_sw, changed_sw, tab_sw
  integer              :: tabs(256), isw, j
  logical              :: lit_sw

  character (len=80), parameter :: title='Start of EzSub version 1.0'

  n_lits=0
  lit_sw=.false.
  print *,trim(title)
  print *,' '

  call ezsub_get_arg(1,file_in,istat)
  if(istat/=1) then
    call ezsub_char_prompt_edit(file_in,'',&
     'Enter command file name(blanks to exit, help to get help)')
    print *,' '
    if(len_trim(file_in)==0) stop
  endif

  file_out=file_in
  call ezsub_chars_to_upper(file_out)
  if(file_out(1:5).eq."HELP ")then
    call ezsub_print_help
    stop
  endif

  open(unit=1,file=file_in, status='old', iostat=istat)
  if(istat/=0) then
    print *,'Error in opening command file('//trim(file_in)//', job aborted'
    stop
  endif


  print *,'Reading commands from file('//trim(file_in)//')'
  delims=' =&+-*/(),:"./`[]{};%<>'//char(8)//"'"//char(92)!PG has bug with \(92)
  n_delims=len_trim(delims)
  n_keys=0

  file_in=' '
  file_out=' '

  tabs=(/(i+1,i=1,256)/)
  tab_sw=.false.

! get commands

  do while(.true.)
    read(1,'(a256)',iostat=istat) card_in
    if(istat/=0) exit

    card_len=len_trim(card_in)
    print *,'  '//trim(card_in)
    if(card_len==0 .or. card_in(1:1)=='!') cycle

    card_in_upper_case=card_in
    call ezsub_chars_to_upper(card_in_upper_case)

! input command ?

    i_beg=ezsub_find_not_chars(card_in,1,card_len,' ')
    if(ezsub_check_chars(card_in_upper_case,i_beg,i_beg+5,'INPUT=') ==i_beg)then
      if(len_trim(file_in)>0) then
        print *,'multiple input files specified, last one specified used'
      endif
      file_in=card_in(i_beg+6:card_len)
      cycle
    endif

! output command ?

    if(ezsub_check_chars(card_in_upper_case,i_beg,i_beg+6,'OUTPUT=')==i_beg)then
      if(len_trim(file_out)>0) then
        print *,'multiple output files specified, last one specified used'
      endif
      file_out=card_in(i_beg+7:card_len)
      cycle
    endif

! T command ?

    if(card_in(i_beg:i_beg)=='T' .or. card_in(i_beg:i_beg)=='t') then
      i_beg=ezsub_find_not_chars(card_in, i_beg+1,card_len,' ')
      if(i_beg>card_len) then
        print *,'Error: No deliminator found in Tab card'
        stop
      endif

      delim=card_in(i_beg:i_beg)
      if(delim>='0' .and. delim<='9') then
        print *,'Error:Invalid deliminator found in Tab card'
        stop
      endif

      i=1
      do while(i_beg<card_len)
        i_end=ezsub_find_chars(card_in,i_beg+1,card_len,delim)
        n=ezsub_chars_to_int(card_in(i_beg+1:i_end-1),istat)

        if(istat/=0) then
          print *,'Error: Invalid tab stop specified('//                       &
           card_in(i_beg+1:i_end-1)//')--ignored'
          stop
        endif

        if(i>=n) then
          print *,'Error: Tab stops not increasing order'
          stop
        endif

        tabs(i:n-1)=n
        i=n
        i_beg=i_end
      enddo

      tab_sw=.true.
      cycle
    endif

! S,s,C, or c command?

    if(card_in_upper_case(i_beg:i_beg)=='S' .or.                               &
     card_in_upper_case(i_beg:i_beg)=='C') then
      if(n_keys==N_KEYS_MAX) then
        print *,'Error: too many commands specified, reduce commands '//       &
         'or contact CC Burch '
        stop
      endif

      n_keys=n_keys+1
      keys_command(n_keys)=card_in(i_beg:i_beg)
      i_beg=ezsub_find_not_chars(card_in, i_beg+1,card_len,' ')
      if(i_beg>card_len) then
        print *,'No deliminator found-card skipped'
        cycle
      endif
      delim=card_in(i_beg:i_beg)
      i_end=ezsub_find_chars(card_in,i_beg+1,card_len,delim)

      if(keys_command(n_keys)=='S'.or.keys_command(n_keys)=='C') then
        keys_in(n_keys)=card_in_upper_case(i_beg+1:i_end-1)
      else
        keys_in(n_keys)=card_in           (i_beg+1:i_end-1)
      endif

      keys_in_len(n_keys)=i_end-(i_beg+1)
      i_beg=i_end+1
      i_end=ezsub_find_chars(card_in,i_beg,card_len,delim)
      keys_out(n_keys)=card_in(i_beg:i_end-1)
      keys_out_len(n_keys)=i_end-i_beg

! Get line numbers if present

      keys_line_beg(n_keys)=1
      keys_line_end(n_keys)=2147000000
      i_beg=i_end+1

      if(i_beg<=card_len)then
        delim=card_in(i_beg:i_beg)
        if(delim.lt.'0'.or.delim.gt.'9') then
          i=ezsub_find_chars(card_in,i_beg+1,card_len,delim)
          i_end=ezsub_find_chars(card_in,i+1,card_len,':')
          if(i.gt.card_len)then
            print *,"Warning: literal missing matching deliminator in"
            print *,"  "//trim(card_in)
            i_end=ezsub_find_chars(card_in,i_beg,card_len,':')
            i=i_end
          endif

          call ezsub_put_lit(card_in(i_beg+1:i-1),n_lits, lits, lit_sizes, &
            lit_values,lit_sw, n)
          if(n.gt.0) keys_line_beg(n_keys)=-n
        else
          i_end=ezsub_find_chars(card_in,i_beg,card_len,':')
          n=ezsub_chars_to_int(card_in(i_beg:i_end-1),istat)
          if(istat==0) keys_line_beg(n_keys)=n
        endif

        if(i_end>card_len) then
          keys_line_end(n_keys)=keys_line_beg(n_keys)
        else
          i_beg=i_end+1
          if(i_beg<=card_len) then
            delim=card_in(i_beg:i_beg)
            if(delim.lt.'0'.or.delim.gt.'9') then
              i=ezsub_find_chars(card_in,i_beg+1,card_len,delim)
              if(card_in(i:i).eq.delim) i=i-1
              call ezsub_put_lit(card_in(i_beg+1:i), n_lits, lits, lit_sizes, &
               lit_values,lit_sw, n)
              if(n.gt.0) keys_line_end(n_keys)=-n
            else
              n=ezsub_chars_to_int(card_in(i_beg:card_len),istat)
              if(istat==0) keys_line_end(n_keys)=n
            endif

            if(keys_line_beg(n_keys).gt.0 .and. &
               keys_line_end(n_keys).gt.0 .and. &
               keys_line_beg(n_keys).gt.keys_line_end(n_keys)) then
              n=keys_line_beg(n_keys)
              keys_line_beg(n_keys)=keys_line_end(n_keys)
              keys_line_end(n_keys)=n
            endif
          endif
        endif

      endif
      cycle
    endif

    print *,'Error: Invalid command'
    stop
  enddo
  close(unit=1)

  print *,' '
  print *,'input='//trim(file_in)
  print *,'output='//trim(file_out)
!  do i=1,n_keys
!    print *,keys_command(i),keys_in_len(i),trim(keys_in(i))
!    print *,keys_out_len(i),trim(keys_out(i))
!    print *,keys_line_beg(i),keys_line_end(i)
!  enddo

  if(len_trim(file_in)==0) then
    print *,'Error: No input file specified'
    stop
  endif

  if(len_trim(file_out)==0) then
    print *,'warning: No output file specified, EZSUB.LIS used'
    file_out='EZSUB.LIS'
  endif

  open(unit=1,file=file_in,status='old',iostat=istat)
  if(istat/=0) then
    print *,'Error: Unable to open input file('//trim(file_in)//'), job aborted'
    stop
  endif

  open(unit=2,file=file_out,status='unknown',iostat=istat)
  if(istat/=0) then
    print *,'Error: Unable to open output file('//trim(file_out)//             &
     '), job aborted'
    stop
  endif

  if(n_keys==0) then
    print *,'Error: No key substitutions specified, job aborted'
    stop
  endif

!-------------------------------------------------------------------
! Process commands on file
!-------------------------------------------------------------------

  n_tot_matchs=0
  n_tot_no_matchs=0
  i_line=0
  do while(.true.)
    i_line=i_line+1
    read(1,'(a256)', iostat=istat) card_in
    if(istat/=0) exit

! - print *,'card  in:'//trim(card_in)
    changed_sw=.false.
    n_matchs=0
    n_no_matchs=0

!check for tabs

    if(tab_sw) then
      card_len=len_trim(card_in)
      i_beg=ezsub_find_chars(card_in,1,card_len,char(8))
      n=0
      do while(i_beg<=card_len)
        n=tabs(i_beg)
        print *,'Test Mode--tab found at pos ',i_beg
        card_in(i_beg:)=repeat(' ',n-i_beg)//card_in(i_beg+1:card_len)
        card_len=card_len+n-i_beg-1
        i_beg=ezsub_find_chars(card_in,i_beg,card_len,char(8))
      enddo
    endif

! - see if any active literals-if so change literal to line number
! - in appropriate commands--see if any remaining active literals

    if(lit_sw) then
      call ezsub_check_lit_values(card_in,n_lits,lits,lit_sizes, lit_values,isw)
      if(isw.ne.0) then
        lit_sw=.false.

        do j=1,n_lits
          if(lit_values(j).gt.0) then
            lit_values(j)=0    !mark as inactive
            isw=0

            do i=1,n_keys       !change to line line numbers in commands
              if(keys_line_beg(i).eq.(-j) ) then
                keys_line_beg(i)=i_line
                if(isw.eq.0) then
                  print *,""
                  isw=1
                endif

                call ezsub_int_to_chars(i_line,a_out)
                call ezsub_int_to_chars(i,b_out)
                print *,"literal(",lits(j)(1:lit_sizes(j)),&
                 ") converted to begin line# ",trim(a_out)," in command# ",&
                 trim(b_out)
              endif

              if(keys_line_end(i).eq.(-j) ) then
                if(keys_line_beg(i).gt.0 .and. i_line.ge.keys_line_beg(i)) then
                  keys_line_end(i)=i_line  !change if beg resolved
                  if(isw.eq.0) then
                    print *,""
                    isw=1
                  endif

                  call ezsub_int_to_chars(i_line,a_out)
                  call ezsub_int_to_chars(i,b_out)
                  print *,"literal(",lits(j)(1:lit_sizes(j)),&
                   ") converted to end line# ", trim(a_out)," in command# ",&
                   trim(b_out)
                else
                  lit_values(j)=-1      !beg not resolved-literal remains active
                  lit_sw=.true.
                endif
              endif
            enddo

          else if(lit_values(j).lt.0) then
            lit_sw=.true.       !found a remaining active literal
          endif
        enddo

      endif
    endif

!-------------------------------------------------------------------
! --- Perform commands on input information
!-------------------------------------------------------------------

    do i=1, n_keys
      if(keys_line_beg(i).lt.0) cycle      !skip if unresolved beg literal
! --- know keys_line_beg(i) is defined
      if(i_line.lt.keys_line_beg(i)) cycle ! skip if line # smaller line_beg
! --- know line# >=line_beg
! --- if line_end defined, see if line#>line_end
      if(keys_line_end(i).gt.0 .and. i_line.gt.keys_line_end(i)) cycle

      card_len=len_trim(card_in)
      key_no_match=0
      key_match=0
      n_in=keys_in_len(i)
      n_out=keys_out_len(i)
      i_beg=1

      do while(i_beg<=card_len)

! see if case insensitive or case sensitive

        if(keys_command(i)=='S'.or.keys_command(i)=='C') then
          card_in_upper_case=card_in
          call ezsub_chars_to_upper(card_in_upper_case)
          i_beg=ezsub_find_chars(card_in_upper_case,i_beg,card_len,&
                                 keys_in(i)(1:n_in))
        else
          i_beg=ezsub_find_chars(card_in,i_beg,card_len,keys_in(i)(1:n_in))
        endif
        if(i_beg>card_len) exit

! we have found a potential change

        if(.not.changed_sw) then
          print *,' '
          call ezsub_int_to_chars(i_line,a_out)
          n_a_out=len_trim(a_out)
          print *, 'At line #'//trim(a_out)//', input is:'//trim(card_in)
          changed_sw=.true.
        endif

! if S/s, check for valid preceeding and succeeding characters

        match_sw=.true.
        i_end=i_beg+n_in-1
        if(keys_command(i)=='s'.or.keys_command(i)=='S') then
          if(i_beg>1) then
            match_sw=                                                          &
             ezsub_find_chars(delims,1,n_delims,card_in(i_beg-1:i_beg-1))<=&
                              n_delims
          endif
          if(match_sw) then
            if(i_end<card_len) match_sw=                                       &
             ezsub_find_chars(delims,1,n_delims,card_in(i_end+1:i_end+1))<=&
                              n_delims
          endif
        endif

! if a match-make change and update counters

!       print *,'matchsw=',match_sw,i,i_beg, i_end,n_out, card_len
        if(match_sw) then
          key_match=key_match+1
          card_in(i_beg:)=keys_out(i)(1:n_out)//card_in(i_end+1:card_len+1)
          i_beg=i_beg+n_out+1
          card_len=card_len+n_out-n_in
        else
          key_no_match=key_no_match+1
          i_beg=i_beg+1
        endif

      enddo

! Print out # changes

      if(key_match+key_no_match>0) then
        print *,'('//keys_in(i)(1:n_in)//') found in ('//keys_command(i)// &
         ') mode'

        if(key_match>0)    then
          if(i<n_keys) then
            card_len=len_trim(card_in)
            card_in_upper_case=card_in
            call ezsub_chars_to_upper(card_in_upper_case)
          endif

          call ezsub_int_to_chars(key_match,a_out)
          s_or_blank=' '
          if (key_match>1) s_or_blank='s'
          print *,'  Substituted with (' //keys_out(i)(1:n_out)//') ',         &
           trim(a_out)//' time'//s_or_blank
        endif

        if(key_no_match>0) then
          call ezsub_int_to_chars(key_no_match, a_out)
          s_or_blank=' '
          if (key_no_match>1) s_or_blank='s'
          print *,'  Not Substituted with ('//keys_out(i)(1:n_out)//') ',      &
           trim(a_out)//' time'//s_or_blank
        endif

      endif

      n_matchs=n_matchs+key_match
      n_no_matchs=n_no_matchs+key_no_match
    enddo

    if(changed_sw) then
      if(n_matchs>0) then
!           print *,'At line #'//trim(a_out)// ', input is:'//trim(card_in)
        print *,'Output is'//repeat(' ',n_a_out+10)//':'//trim(card_in)
        if(card_len>80) &
          print *,'Warning: Line length is greater than 80 characters'
      else
        print *,'No substitutions made, line remains unchanged'
      endif
    endif

    write(2,'(a)') trim(card_in)
    n_tot_matchs=n_tot_matchs+n_matchs
    n_tot_no_matchs=n_tot_no_matchs+n_no_matchs
  enddo

  if(lit_sw) then
    print *,"WARNING--The following literals were never resolved"
    do i=1,n_lits
      if(lit_values(i).lt.0) then
        print *,"  ",lits(i)(1:lit_sizes(i))
      endif
    enddo
  endif

  print *,' '
  call ezsub_int_to_chars(n_tot_matchs, a_out)
  print *,'Number of changes mades= '//trim(a_out)

  if(n_tot_no_matchs>0) then
    call ezsub_int_to_chars(n_tot_no_matchs,a_out)
    print *,'Number matches found but not changed= '//trim(a_out)
  endif

  stop
  end program ezsub


subroutine ezsub_print_help
  print *,"general purpose text substitution for computer code files"
  print *," execute: ezsub command_file_name"
  print *,"   commands in command_file_name"
  print *,"     input=input_filename"
  print *,"     output=output_filename"
  print *,"     T/n1/n2/n3/"
  print *,"     s/text1/text2/[[l1][:l2]]"
  print *,"     S/text1/text2/[[l1][:l2]]"
  print *,"     c/text1/text2/[[l1][:l2]]"
  print *,"     C/text1/text2/[[l1][:l2]]"
  print *,"       T set tab settings which replaces tabs with spaces"
  print *,"       c change all occurences of text1 (case sensitive) with text2"
  print *,&
   "       C change all occurences of text1 (case insensitive) with text2"
  print *,&
   "       s case sensitive replace text1 with text2, when text1 is a variable"
  print *,&
  "       S case insensitive replace text1 with text2, when text1 is a variable"
  print *,"       l1=beginning line number, l2=ending line number"
  print *,"       Default value for l1 is 1."
  print *,"       Default value for l2 is last line number in the input file."
  print *,""
  print *,"       l1 and/or l2 can optionally be literals."
  print *,"       Literals are specified by starting it with a deliminator,"
  print *,'       such as ", and ending with same deliminator.'
  print *,&
   "       For l1, the text of the file is searched and line number where "
  print *,"       the first occurrence of the literal occurs is used for l1."
  print *,&
   "       For l2, the text of the file is searched and the line  number after "
  print *,"       l1 with the first occurrence of the literal is used for l2."
  return
end subroutine ezsub_print_help



!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!





