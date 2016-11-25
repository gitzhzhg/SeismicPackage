!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ headsave.f90 ------------------------------!!
!!------------------------------ headsave.f90 ------------------------------!!
!!------------------------------ headsave.f90 ------------------------------!!

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
! Name       : HEADSAVE 
! Category   : headers
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : Save and print first, last, min, max header words
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Save and print first, last, min, max header words
!
!------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
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
!------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!
!                  o                i     b      o
!                hello = HEADSAVE     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    HEADSAVE_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    HEADSAVE_ZZZ (bbbb, indx, value)
!
! character(len=*)           aaaa(*) =    --> description 
! character(len=8),pointer   bbbb(:) =    --> description 
! double precision           cvar    =    --> description
! character(len=*)           msg     =    --> description 
! integer                    hello   =    --> description
! integer         ,optional  indx    =    --> description
! double precision,optional  value   =    --> description
!
!
!------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2006-04-25  B. Menger      Removed Unused Variables.
!  6  2004-03-15  Bill Done      In headsave_create(), nullify the hd_sav
!                                member of the headsave_struct prior to calling
!                                memfun_all(). This is needed to avoid a
!                                Portland Group compiler problem.
!  5  2003-07-10  Douglas Hanson Fix title print.
!  4  2003-06-05  Douglas Hanson Add create, delete, nullify.
!  3  2001-07-10  Douglas Hanson cpsfcr
!  2  2000-06-14  Brad Kruse     Review for standards.
!  1  1999-12-01  Douglas Hanson Initial version.
!
!------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!
!------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module headsave_module
  !
  ! - Module references
  !
  use getsys_module
  use memfun_module
  use named_constants_module
  use pc_module
  use string_module
  !
  implicit none
  !
  private
  public :: headsave_create
  public :: headsave_delete
  public :: headsave_nullify
  public :: headsave_store
  public :: headsave_print
  !
  interface headsave_store
    !
    module procedure headsave_store
    module procedure headsave_store_0
    !
  end interface 
  !
  interface headsave_print
    !
    module procedure headsave_print
    module procedure headsave_print_0
    !
  end interface 
  !
  character(len=100),public,save :: HEADSAVE_IDENT = &
    '$Id: headsave.f90,v 1.7 2006/04/25 13:23:47 Menger prod sps $'
  !
  type, public :: headsave_struct
    !
    character(len=filename_length)   :: c_title
    integer                          :: nh_inp
    integer                          :: nc_inp
    double precision,        pointer :: hd_sav ( :, : )
    !
  end type headsave_struct
  !
  contains
  !
  subroutine headsave_create ( o, c_title, nh_inp, i_err )
    !
    ! - Arguments
    !
    type(headsave_struct),   pointer :: o       ! headsave structure
    character(len=*), intent (in   ) :: c_title
    integer,          intent (in   ) :: nh_inp
    integer,          intent (inout) :: i_err
    !
    ! - Local variables
    !
    i_err = 0
    !
    allocate ( o )
    nullify(o%hd_sav)   ! added to avoid portland compiler bug
    !
    o%c_title = c_title
    o%nh_inp  = nh_inp
    o%nc_inp  = 12
    !
    call memfun_all ( o%hd_sav, o%nh_inp, o%nc_inp, o%c_title, i_err )
    !
    return
    !
  end subroutine headsave_create 
  !
  subroutine headsave_delete ( o )
    !
    ! - Arguments
    !
    type(headsave_struct),   pointer :: o       ! headsave structure
    !
    ! - Local variables
    !
    if ( .not. associated ( o ) ) return
    !
    call memfun_del ( o%hd_sav )
    !
    if ( associated ( o ) ) &
         deallocate ( o )
    !
    return
    !
  end subroutine headsave_delete 
  !
  subroutine headsave_nullify ( o )
    !
    ! - Arguments
    !
    type(headsave_struct),   pointer :: o       ! headsave structure
    !
    ! - Local variables
    !
    nullify ( o )
    !
    return
    !
  end subroutine headsave_nullify 
  !
  subroutine headsave_store ( o, i_save, ic1, hd_inp )
    !
    ! - save a copy of header words
    !
    ! - Arguments
    !
    type(headsave_struct),   pointer :: o       ! headsave structure
    integer,          intent (in   ) :: i_save
    integer,          intent (in   ) :: ic1
    double precision, intent (in   ) :: hd_inp (:)           ! (nh_inp)
    !
    ! - Local variables
    !
    ! - Begin headsave_store
    !
    call headsave_store_0  ( i_save, ic1, o%nh_inp, hd_inp, o%hd_sav )
    !
    return
    !
  end subroutine headsave_store 
  !
  subroutine headsave_store_0 ( i_save, ic1, nh_inp, hd_inp, hd_sav )
    !
    ! - save a copy of header words
    !
    ! - Arguments
    !
    integer,          intent (in   ) :: i_save
    integer,          intent (in   ) :: ic1
    integer,          intent (in   ) :: nh_inp
    double precision, intent (in   ) :: hd_inp (:)           ! (nh_inp)
    double precision, intent (inout) :: hd_sav (:, :)        ! (nh_inp, 12)
    !
    ! - Local variables
    !

    integer                          :: i1
    integer                          :: i2
    integer                          :: i3
    integer                          :: i4
    !
    ! - Begin headsave_store
    !
    ! - location 1 is first trace
    !
    i1 = ic1 + 0
    i2 = ic1 + 1
    i3 = ic1 + 2
    i4 = ic1 + 3
    !
    if (i_save .eq. 1) then
      hd_sav(:, i1) = hd_inp       ! First trace
      hd_sav(:, i3) = hd_inp       ! Min word value
      hd_sav(:, i4) = hd_inp       ! Max word value
    end if
    !
    hd_sav(:, i2) = hd_inp         ! Current and last trace
    hd_sav(:, i3) = min (a1 = hd_sav (:, i3), &
                         a2 = hd_inp) ! Min word value
    hd_sav(:, i4) = max (a1 = hd_sav (:, i4), &
                         a2 = hd_inp) ! Max word value
    !
    return
    !
  end subroutine headsave_store_0 
  !
  subroutine headsave_print ( o, c_title, ic1 )
    !
    ! - Arguments
    !
    type(headsave_struct),   pointer :: o       ! headsave structure
    character(len=*), intent (in   ) :: c_title
    integer,          intent (in   ) :: ic1
    !
    ! - Local variables
    !
    character(len=filename_length)   :: c_title_0
    !
    ! - Begin 
    !
    c_title_0 = trim ( o%c_title ) // ' ' // trim ( c_title )
    !
    call headsave_print_0 ( c_title_0, pc_get_lun(), ic1, o%nh_inp, o%hd_sav )
    !
    return
    !
  end subroutine headsave_print
  !
  subroutine headsave_print_0 ( c_title, lu, ic1, nh_inp, hd_sav )
    !
    ! - Arguments
    !
    character(len=*), intent (in   ) :: c_title
    integer,          intent (in   ) :: lu
    integer,          intent (in   ) :: ic1
    integer,          intent (in   ) :: nh_inp
    double precision, intent (in   ) :: hd_sav (nh_inp, 12)
    !
    ! - Local variables
    !
    integer   i, j, ic2
    integer   jc1, jc2
    character c_date*16, c_time*16
    character crd8*8, crd80*80
    !
    ! - Begin 
    !
    ! - print hd_sav data
    !
    call string_date(c_date)   ! current date
    call string_time(c_time)   ! current time
    !
    if (ic1 .eq. 1) then
      !
      ic2 = 8
      write(lu, '( &
     & /, '' headsave trace headers '' &
     & /, '' input trace header values for '', a, &
     & /, '' date='', a16, '' time='', a16, &
     & /, '' header first    last      min     max      '', &
     & ''first    last     min      max'', &
     & /, '' word   input    input    input    input    '', &
     & ''saved    saved    saved    saved'')') &
     & trim(c_title), c_date, c_time
      !
    else    ! if (ic1 .eq. 1) then
      !
      ic2 = 12
      write(lu, '( &
     & /, '' headsave trace headers '' &
     & /, '' output trace header values for '', a, &
     & /, '' date='', a16, '' time='', a16, &
     & /, '' header first    last     min      max'', &
     & /, '' word   output   output   output   output'')') &
     & trim(c_title), c_date, c_time
      !
    end if    ! if (ic1 .eq. 1) then
    !
    do i = 1 , nh_inp
      !
      crd80 = ' '
      !
      do j = ic1 , ic2
        !
        call string_dd2cc (hd_sav(i, j), crd8)
        !
        jc1 = (j - ic1) * 9 + 4
        jc2 = jc1 + 7
        write(crd80(jc1:jc2), '(a8)')trim(crd8)
        !
      end do    ! do j = ic1 , ic2
      !
      write(lu, '(1x, i2, 1x, a)')i, trim(crd80)
      !
    end do    ! do i = 1 , nh_inp
    !
    write(lu, '('' '')')
    !
    return
    !
  end subroutine headsave_print_0
  !
end module headsave_module
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
