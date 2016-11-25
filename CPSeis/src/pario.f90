!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- PARIO.f90 --------------------------------!!
!!------------------------------- PARIO.f90 --------------------------------!!
!!------------------------------- PARIO.f90 --------------------------------!!

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
! Name       : PARIO 
! Category   : IO
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2002-10-25   by: Karen Goodger
! Maturity   : production   2003-01-23
! Purpose    : parrellel io utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! parrellel io utilities.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
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
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                  o                i     b      o
!                hello = PARIO     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    PARIO_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    PARIO_ZZZ (bbbb, indx, value)
!
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
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2003-01-23  Goodger      Add use getlun_module. Fix ending tag on
!                              header_word_doc.  Add ident string.
!  3. 2001-07-10  Douglas Hanson add r,i,c,d types
!  2. 2000-08-25  Douglas Hanson cpsfcr
!  1. 1999-12-01  Douglas Hanson Initial version.
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
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module pario_module
  !
  ! file io routine with some parrellel logic
  !
  use pc_module
  use getlun_module
  use named_constants_module
  use getsys_module
  use string_module
  use sizeof_module
  use cio_module
  use cmpi_module
  !
  implicit  none
  !
  ! set default to all routines public
  !
  public
  !
  ! set default to all routines private
  !private
  !
  ! subroutines
  !
  public :: pario_open_form
  public :: pario_open
  public :: pario_close
  public :: pario_zero
  public :: pario_read
  public :: pario_write
  public :: pario_open_2
  public :: pario_zero_2
  public :: pario_read_2
  public :: pario_write_2
  public :: pario_name_1
  public :: pario_name_2
  !
  ! functions
  !
  ! interfaces
  !
  interface pario_read
    !
    module procedure pario_read_r
    module procedure pario_read_i
    module procedure pario_read_c
    module procedure pario_read_d
    !
  end interface
  !
  interface pario_write
    !
    module procedure pario_write_r
    module procedure pario_write_i
    module procedure pario_write_c
    module procedure pario_write_d
    !
  end interface
      character(len=100),public,save :: PARIO_IDENT = &
       '$Id: pario.f90,v 1.4 2003/01/23 17:09:34 Goodger prod sps $'
  contains

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_open_form( &
     & i_pel, l_bcast, i_file, c_file, c_stat, i_err)
!  open file c_file on pe i_pel
!  return unit number i_file
!  if l_bcast = 0 broadcast from pe 0 to the rest

      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      character c_file*(*)
      character c_stat*(*)
      integer   i_err

      character a_stat*8
      character a_form*16
      character a_access*16

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!      write(pc_get_lun(), '( &
!     & /, " pario_open_form", &
!     & /, " c_file =", a, &
!     & /, " c_stat =", a, &
!     & )') &
!     & trim(c_file), &
!     & trim(c_stat) &

      a_form = 'formatted'
      a_access = 'sequential'
      a_stat = c_stat

! get the unit number
      if (cmpi_i_pel() .eq. i_pel) then

        call getlun(i_file, i_err)
        if (i_err .ne. 0) goto 998

!  try a new open
        call string_to_upper(a_stat)
        open(i_file, file=c_file, status=a_stat, form=a_form, &
     & access=a_access, err=1)

!  suceessful open
        goto 2

!  try an old open
    1   continue
        if (a_stat(1:3) .eq. 'NEW') then

          a_stat = 'OLD'
        open(i_file, file=c_file, status=a_stat, form=a_form, &
     & access=a_access, err=997)

         end if    ! if (a_stat(1:3) .eq. 'NEW') then

!  come to here for sucessful open_form
    2   continue

!      write(pc_get_lun(), '( &
!     & /, " pario_open_form", &
!     & /, " c_file =", a, &
!     & /, " c_stat =", a, &
!     & /, " i_file =", i12 &
!     & )') &
!     & trim(c_file), &
!     & trim(c_stat), &
!     & i_file

       end if    ! if (cmpi_i_pel() .eq. i_pel) then

 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
        call cmpi_bcast(0, i_file)
       end if    ! if (l_bcast) then

      return

  997 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open_form", &
     & /, " during open_form")')

  998 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open_form", &
     & /, " during getlun")')

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open_form", &
     & /, " c_file =", a, &
     & /, " c_stat =", a, &
     & /, " i_file =", i12 &
     & )') &
     & trim(c_file), &
     & trim(c_stat), &
     & i_file
      i_err = -1
      goto 1999

      end subroutine pario_open_form

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_open( &
     & i_pel, l_bcast, i_file, c_file, c_stat, n_data, i_err)
!  open file c_file on pe i_pel with record length n_data, 
!  return unit number i_file
!  if l_bcast = 0 broadcast from pe 0 to the rest

      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      character c_file*(*)
      character c_stat*(*)
      integer   n_data
      integer   i_err

      character a_stat*8
      character a_form*16
      character a_access*16

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!      write(pc_get_lun(), '( &
!     & /, " pario_open", &
!     & /, " c_file =", a, &
!     & /, " c_stat =", a, &
!     & /, " n_data =", i12 &
!     & )') &
!     & trim(c_file), &
!     & trim(c_stat), &
!     & n_data

!      a_form = 'unformatted'
!      a_access = 'direct'
!      a_stat = c_stat

! get the unit number
      if (cmpi_i_pel() .eq. i_pel) then

        i_file = cio_fopen( c_file, c_stat)
        if (i_file .le. 0) goto 997

!        call getlun(i_file, i_err)
!        if (i_err .ne. 0) goto 998

!  try a new open
!        call string_to_upper(a_stat)
!        open(i_file, file=c_file, status=a_stat, form=a_form, &
!     & access=a_access, recl=n_data*4, err=997)
!  suceessful open
!        goto 2

!  try an old open
!    1   continue
!        if (a_stat(1:3) .eq. 'NEW') then

!          a_stat = 'OLD'
!          open(i_file, file=c_file, status=a_stat, form=a_form, &
!     & access=a_access, recl=n_data*4, err=997)

!         end if    ! if (a_stat(1:3) .eq. 'NEW') then

!  come to here for sucessful open
!    2   continue


!      write(pc_get_lun(), '( &
!     & /, " pario_open", &
!     & /, " c_file =", a, &
!     & /, " c_stat =", a, &
!     & /, " i_file =", i12, &
!     & /, " n_data =", i12 &
!     & )') &
!     & trim(c_file), &
!     & trim(c_stat), &
!     & i_file, n_data

       end if    ! if (cmpi_i_pel() .eq. i_pel) then

 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
        call cmpi_bcast(0, i_file)
       end if    ! if (l_bcast) then

      return

  997 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open", &
     & /, " during cio_open" &
     & )')
      goto 999

  998 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open", &
     & /, " during getlun" &
     & )')
      goto 999

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open", &
     & /, " c_file =", a, &
     & /, " c_stat =", a, &
     & /, " i_file =", i12, &
     & /, " n_data =", i12 &
     & )') &
     & trim(c_file), &
     & trim(c_stat), &
     & i_file, n_data
      i_err = -1
      goto 1999

      end subroutine pario_open

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_close( &
     & i_pel, l_bcast, i_file, l_remove, i_err)
!  close file using unit number i_file and pe i_pel
!  if l_bcast = 0 broadcast from pe 0 to the rest
!  if l_remove = .true. remove the file
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      logical   l_remove
      integer   i_err

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

      if (cmpi_i_pel() .eq. i_pel) then

!  close the file
!  remove the file
        i_err = cio_fclose(i_file,l_remove)
        if (i_err .ne. CIO_OK) goto 999
!        close(i_file, err=999)

       end if    ! if (cmpi_i_pel() .eq. i_pel) then

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_close", &
     & /, " i_file =", i12 &
     & )') &
     & i_file
      i_err = -1
      goto 1999

      end subroutine pario_close

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_zero( &
     & i_pel, l_bcast, i_file, n_rec, n_data, i_err)
!  zero a file
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      integer   n_rec, n_data
      integer   i_err

      integer   i_rec, j_rec
      integer   i_data
      real      x_data(n_data,1) ! automatic array

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

      x_data = 0.

! if l_bcast=0 broadcast from pe 0 to the rest
      if (cmpi_i_pel() .eq. i_pel) then

!  write zeros to each record
        do i_rec = 1 ,  n_rec

!          write(i_file, rec=i_rec, err=999) (x_data(i_data), i_data=1, n_data)

           call pario_write ( &
     & i_pel, .false., i_file, &
     & 1, i_rec, 1, &
     & n_data, n_data, x_data, &
     & i_err)
          if (i_err .ne. 0) goto 999
!           call pario_read ( &
!     & i_pel, .false., i_file, &
!     & 1, i_rec, 1, &
!     & n_data, n_data, x_data, &
!     & i_err)
!          if (i_err .ne. 0) goto 999

!          i_err = cio_fseek( &
!     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
!          if (i_err .ne. CIO_OK) goto 999

!          i_err = cio_fwrite( &
!     & x_data, n_data*sizeof(x_data(1,1)), 1, i_file)
!          if (i_err .ne. 1) goto 999

        end do    ! do i_rec = 1 ,  n_rec

       end if    ! if (cmpi_i_pel() .eq. i_pel) then

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_zero", &
     & /, " i_file =", i12, &
     & /, " n_data =", i12, &
     & /, " n_rec  =", i12, &
     & /, " i_rec  =", i12 &
     & )') &
     & i_file, n_data, n_rec, i_rec
      i_err = -1
      goto 1999

      end subroutine pario_zero

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_read_r( &
     & i_pel, l_bcast, i_file, &
     & n_read, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  read n_read records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_read, i_first, n_skip

      integer   m_data, n_data
      real      x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_read
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  read the header and trace
      do i_read = 1 ,  n_read

        i_rec = i_first + (i_read - 1) * n_skip

!      write(pc_get_lun(), '( &
!     &" read i=", i8, " f=", i8, " s=", i8, " r=", i8, " i=", i8, " j=", i8 &
!     & )' ) &
!     & i_file, i_first, n_skip, n_read, i_read, i_rec

      if (cmpi_i_pel() .eq. i_pel) then

!        read (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_read), i_data=1, n_data)

        i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
        if (i_err .ne. CIO_OK) goto 999

        i_err = cio_fread ( &
     & x_data(:, i_read), n_data*sizeof(x_data(1,1)), 1, i_file)
          if (i_err .ne. 1) goto 999

      end if    ! if (cmpi_i_pel() .eq. i_pel) then

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, n_data, x_data(:, i_read) )
       end if    ! if (l_bcast) then

      end do    ! do i_read = 1 ,  n_read

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_read_r pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_read  =", i12, &
     & /, " i_read  =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_read, i_read, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_read_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_read_i( &
     & i_pel, l_bcast, i_file, &
     & n_read, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  read n_read records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_read, i_first, n_skip

      integer   m_data, n_data
      integer   x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_read
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  read the header and trace
      do i_read = 1 ,  n_read

        i_rec = i_first + (i_read - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" read if=", i8, " ns=", i8, " nr=", i8, " j='
!     & i_first, n_skip, n_read, i_read, i_rec

      if (cmpi_i_pel() .eq. i_pel) then

!        read (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_read), i_data=1, n_data)

        i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
        if (i_err .ne. CIO_OK) goto 999

        i_err = cio_fread ( &
     & x_data(:, i_read), n_data*sizeof(x_data(1,1)), 1, i_file)
          if (i_err .ne. 1) goto 999

      end if    ! if (cmpi_i_pel() .eq. i_pel) then

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, n_data, x_data(:, i_read) )
       end if    ! if (l_bcast) then

      end do    ! do i_read = 1 ,  n_read

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_read_i pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_read  =", i12, &
     & /, " i_read  =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_read, i_read, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_read_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_read_c( &
     & i_pel, l_bcast, i_file, &
     & n_read, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  read n_read records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_read, i_first, n_skip

      integer   m_data, n_data
      complex   x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_read
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  read the header and trace
      do i_read = 1 ,  n_read

        i_rec = i_first + (i_read - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" read if=", i8, " ns=", i8, " nr=", i8, " j='
!     & i_first, n_skip, n_read, i_read, i_rec

      if (cmpi_i_pel() .eq. i_pel) then

!        read (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_read), i_data=1, n_data)

        i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
        if (i_err .ne. CIO_OK) goto 999

        i_err = cio_fread ( &
     & x_data(:, i_read), n_data*sizeof(x_data(1,1)), 1, i_file)
          if (i_err .ne. 1) goto 999

      end if    ! if (cmpi_i_pel() .eq. i_pel) then

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, n_data, x_data(:, i_read) )
       end if    ! if (l_bcast) then

      end do    ! do i_read = 1 ,  n_read

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_read_c pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_read  =", i12, &
     & /, " i_read  =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_read, i_read, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_read_c

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_read_d( &
     & i_pel, l_bcast, i_file, &
     & n_read, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  read n_read records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_read, i_first, n_skip

      integer   m_data, n_data
      double precision x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_read
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  read the header and trace
      do i_read = 1 ,  n_read

        i_rec = i_first + (i_read - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" read if=", i8, " ns=", i8, " nr=", i8, " j='
!     & i_first, n_skip, n_read, i_read, i_rec

      if (cmpi_i_pel() .eq. i_pel) then

!        read (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_read), i_data=1, n_data)

        i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
        if (i_err .ne. CIO_OK) goto 999

        i_err = cio_fread ( &
     & x_data(:, i_read), n_data*sizeof(x_data(1,1)), 1, i_file)
          if (i_err .ne. 1) goto 999

      end if    ! if (cmpi_i_pel() .eq. i_pel) then

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, n_data, x_data(:, i_read) )
       end if    ! if (l_bcast) then

      end do    ! do i_read = 1 ,  n_read

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_read_d pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_read  =", i12, &
     & /, " i_read  =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_read, i_read, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_read_d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_write_r( &
     & i_pel, l_bcast, i_file, &
     & n_write, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  write n_write records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_write, i_first, n_skip

      integer   m_data, n_data
      real      x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_write
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  write the header and trace
      do i_write = 1 ,  n_write

        i_rec = i_first + (i_write - 1) * n_skip

!      write(pc_get_lun(), '( &
!     &" writ i=", i8, " f=", i8, " s=", i8, " r=", i8, " i=", i8, " j=", i8 &
!     & )' ) &
!     & i_file, i_first, n_skip, n_write, i_write, i_rec

! if l_bcast=0 broadcast from pe 0 to the rest
        if (cmpi_i_pel() .eq. i_pel) then

!          write (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_write), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data(:, i_write), n_data*sizeof(x_data(1,1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_write = 1 ,  n_write

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_r pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_write =", i12, &
     & /, " i_write =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_write, i_write, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_write_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_write_i( &
     & i_pel, l_bcast, i_file, &
     & n_write, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  write n_write records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_write, i_first, n_skip

      integer   m_data, n_data
      integer   x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_write
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  write the header and trace
      do i_write = 1 ,  n_write

        i_rec = i_first + (i_write - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" write if=", i8, " ns=", i8, " nr=", i8, " j=
!     & i_first, n_skip, n_write, i_write, i_rec

! if l_bcast=0 broadcast from pe 0 to the rest
        if (cmpi_i_pel() .eq. i_pel) then

!          write (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_write), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data(:, i_write), n_data*sizeof(x_data(1,1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_write = 1 ,  n_write

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_i pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_write =", i12, &
     & /, " i_write =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_write, i_write, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_write_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_write_c( &
     & i_pel, l_bcast, i_file, &
     & n_write, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  write n_write records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_write, i_first, n_skip

      integer   m_data, n_data
      complex   x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_write
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  write the header and trace
      do i_write = 1 ,  n_write

        i_rec = i_first + (i_write - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" write if=", i8, " ns=", i8, " nr=", i8, " j=
!     & i_first, n_skip, n_write, i_write, i_rec

! if l_bcast=0 broadcast from pe 0 to the rest
        if (cmpi_i_pel() .eq. i_pel) then

!          write (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_write), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data(:, i_write), n_data*sizeof(x_data(1,1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_write = 1 ,  n_write

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_c pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_write =", i12, &
     & /, " i_write =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_write, i_write, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_write_c

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_write_d( &
     & i_pel, l_bcast, i_file, &
     & n_write, i_first, n_skip, &
     & m_data, n_data, x_data, &
     & i_err)
!  write n_write records of x_data
!  starting at i_first and incrementing by n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_write, i_first, n_skip

      integer   m_data, n_data
      double precision x_data(m_data, *)

      integer   i_err

      integer   i_rec, i_write
      integer   i_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  write the header and trace
      do i_write = 1 ,  n_write

        i_rec = i_first + (i_write - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" write if=", i8, " ns=", i8, " nr=", i8, " j=
!     & i_first, n_skip, n_write, i_write, i_rec

! if l_bcast=0 broadcast from pe 0 to the rest
        if (cmpi_i_pel() .eq. i_pel) then

!          write (i_file, rec=i_rec, err=999) &
!     & (x_data(i_data, i_write), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1,1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data(:, i_write), n_data*sizeof(x_data(1,1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_write = 1 ,  n_write

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_d pe=", i8, &
     & /, " i_file  =", i12, &
     & /, " i_first =", i12, &
     & /, " n_skip  =", i12, &
     & /, " n_write =", i12, &
     & /, " i_write =", i12, &
     & /, " i_rec   =", i12, &
     & /, " m_data  =", i12, &
     & /, " n_data  =", i12 &
     & )') &
     & cmpi_i_pel(), &
     & i_file, i_first, n_skip, n_write, i_write, i_rec, m_data, n_data
      i_err = -1
      goto 1999

      end subroutine pario_write_d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_open_2( &
     & i_pel, l_bcast, i_file, c_file, c_stat, nh_inp, nt_inp, i_err)
!  open file c_file return unit number i_file
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      character c_file*(*)
      character c_stat*(*)
      integer   nh_inp, nt_inp
      integer   i_err

      character a_stat*8
      character a_form*16
      integer   n_data

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  set the record length,  hd_inp is double precision
      n_data = nh_inp + nt_inp

      call pario_open( &
     & i_pel, l_bcast, i_file, c_file, c_stat, n_data, i_err)
      if (i_err .ne. 0) goto 999

!      write(pc_get_lun(), '( &
!     & /, " pario_open_2", &
!     & /, " c_file =", a, &
!     & /, " c_stat =", a, &
!     & /, " i_file =", i12, &
!     & /, " nh_inp =", i12, &
!     & /, " nt_inp =", i12, &
!     & /, " n_data =", i12 &
!     & )') &
!     & trim(c_file), &
!     & trim(c_stat), &
!     & i_file, nh_inp, nt_inp, n_data
! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_open_2", &
     & /, " curing pario_open", &
     & /, " c_file =", a, &
     & /, " c_stat =", a, &
     & /, " i_file =", i12, &
     & /, " nh_inp =", i12, &
     & /, " nt_inp =", i12, &
     & /, " n_data =", i12 &
     & )') &
     & trim(c_file), &
     & trim(c_stat), &
     & i_file, nh_inp, nt_inp, n_data
      i_err = -1
      goto 1999

      end subroutine pario_open_2

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_zero_2( &
     & i_pel, l_bcast, i_file, n_rec, nh_inp, nt_inp, i_err)
!  zero a file
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file
      integer   n_rec, nh_inp, nt_inp
      integer   i_err

      integer   i_rec, j_rec
      integer   n_data, i_data
      integer   ih_1, ih_2
      integer   it_1, it_2
      real      x_data(nh_inp+nt_inp)    ! automatic array

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!  set the record length,  hd_inp is double precision
      n_data = nh_inp + nt_inp
      ih_1 = 1
      ih_2 = ih_1 + nh_inp - 1
      it_1 = ih_2 + 1
      it_2 = it_1 + nt_inp - 1
      x_data = 0.

!  write zeros to each record
      do i_rec = 1 ,  n_rec

        if (cmpi_i_pel() .eq. i_pel) then

!          write(i_file, rec=i_rec, err=999) (x_data(i_data), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data, n_data*sizeof(x_data(1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_rec = 1 ,  n_rec

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_zero_2", &
     & /, " i_file =", i12, &
     & /, " n_data =", i12, &
     & /, " n_rec  =", i12, &
     & /, " i_rec  =", i12 &
     & )') &
     & i_file, n_data, n_rec, i_rec
      i_err = -1
      goto 1999

      end subroutine pario_zero_2

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_read_2( &
     & i_pel, l_bcast, i_file, &
     & n_read, i_first, n_skip, &
     & mh_inp, nh_inp, hd_inp, &
     & mt_inp, nt_inp, tr_inp, &
     & i_err)
!  read n_read records of the header and trace
!  starting at i_first and incrementing bu n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_read, i_first, n_skip

      integer   mh_inp, nh_inp
      double precision hd_inp(mh_inp, *)

      integer   mt_inp, nt_inp
      real      tr_inp(mt_inp, *)

      integer   i_err

      integer   i_rec, i_read, ih_inp, it_inp
      integer   n_data, i_data
      integer   ih_1, ih_2
      integer   it_1, it_2
      real      x_data(nh_inp+nt_inp)    ! automatic array

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

      n_data = nh_inp + nt_inp
      ih_1 = 1
      ih_2 = ih_1 + nh_inp - 1
      it_1 = ih_2 + 1
      it_2 = it_1 + nt_inp - 1

!  read the header and trace
      do i_read = 1 ,  n_read

        i_rec = i_first + (i_read - 1) * n_skip
!      write(pc_get_lun(), '( &
!     &" read if=", i8, " ns=", i8, " nr=", i8, " j='
!     & i_first, n_skip, n_read, i_read, i_rec

        if (cmpi_i_pel() .eq. i_pel) then

!          read (i_file, rec=i_rec, err=999) (x_data(i_data), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fread ( &
     & x_data, n_data*sizeof(x_data(1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

! broadcast the info
        if (l_bcast) then
          call cmpi_bcast(0, n_data, x_data )
         end if    ! if (l_bcast) then

        if (cmpi_i_pel() .eq. i_pel) then
          hd_inp(1:nh_inp, i_read) = x_data(ih_1:ih_2)
          tr_inp(1:nt_inp, i_read) = x_data(it_1:it_2)
         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_read = 1 ,  n_read

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_read_2", &
     & /, " i_file =", i12, &
     & /, " i_read =", i12, &
     & /, " i_rec  =", i12 &
     & )') &
     & i_file, i_read, i_rec
      i_err = -1
      goto 1999

      end subroutine pario_read_2

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_write_2( &
     & i_pel, l_bcast, i_file, &
     & n_write, i_first, n_skip, &
     & mh_inp, nh_inp, hd_inp, &
     & mt_inp, nt_inp, tr_inp, &
     & i_err)
!  write n_write records of the header and trace
!  starting at i_first and incrementing bu n_skip
!  if l_bcast=0 broadcast from pe 0 to the rest
      implicit  none

      integer   i_pel
      logical   l_bcast
      integer   i_file, n_write, i_first, n_skip

      integer   mh_inp, nh_inp
      double precision hd_inp(mh_inp, *)

      integer   mt_inp, nt_inp
      real      tr_inp(mt_inp, *)

      integer   i_err

      integer   i_rec, i_write, ih_inp, it_inp
      integer   n_data, i_data
      integer   ih_1, ih_2
      integer   it_1, it_2
      real      x_data(nh_inp+nt_inp)    ! automatic array

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

      n_data = nh_inp + nt_inp
      ih_1 = 1
      ih_2 = ih_1 + nh_inp - 1
      it_1 = ih_2 + 1
      it_2 = it_1 + nt_inp - 1

!  write the header and trace
      do i_write = 1 ,  n_write
        i_rec = i_first + (i_write - 1) * n_skip

     ! write(pc_get_lun(), '( &
     !& " write if=", i8, " ns=", i8, " nr=", i8, " j=", i8, " i=", i8 &
     !& )') &
     !& i_first, n_skip, n_write, i_write, i_rec

        if (cmpi_i_pel() .eq. i_pel) then

          x_data(ih_1:ih_2) = hd_inp(1:nh_inp, i_write)
          x_data(it_1:it_2) = tr_inp(1:nt_inp, i_write)

!          write(i_file, rec=i_rec, err=999) (x_data(i_data), i_data=1, n_data)

           i_err = cio_fseek( &
     & i_file, 0, n_data*sizeof(x_data(1)), i_rec)
           if (i_err .ne. CIO_OK) goto 999

           i_err = cio_fwrite( &
     & x_data, n_data*sizeof(x_data(1)), 1, i_file)
           if (i_err .ne. 1) goto 999

         end if    ! if (cmpi_i_pel() .eq. i_pel) then

      end do    ! do i_write = 1 ,  n_write

! set the err flag to 0 to indicate all is o.k.
      i_err = 0

! come to here to clean up and return, may be from above or below
 1999 continue

! broadcast the info
      if (l_bcast) then
        call cmpi_bcast(0, i_err)
       end if    ! if (l_bcast) then

      return

  998 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_2", &
     & /, " during read", &
     & /, " i_file =", i12, &
     & /, " i_write=", i12, &
     & /, " i_rec  =", i12 &
     & )') &
     & i_file, i_write, i_rec
      goto 999

  999 continue
      write(pc_get_lun(), '( &
     & /, " error in pario_write_2", &
     & /, " i_file =", i12, &
     & /, " i_write=", i12, &
     & /, " i_rec  =", i12 &
     & )') &
     & i_file, i_write, i_rec
      i_err = -1
      goto 1999

      end subroutine pario_write_2

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_name_1(f_inp, f_out, i_pn)
!  create a unique file name, which has f_inp // i_pn
!  incorporateing the logical unit number, lu_data
      implicit  none

      character f_inp*(*)
      character f_out*(*)
      character f_tmp*80
      integer   i_pn

      integer   i1, i2, j_pn

      i2 = len_trim(f_inp) + 1
      f_tmp = f_inp
      f_out = ' '
      f_out(1 :i2-1) = f_tmp(1:i2-1)
      f_out(i2:i2+4) = '0000'
      j_pn = max(0, min(9999, i_pn))

      if (j_pn .lt. 10) then

        write(f_out(i2+3:i2+3), '(i1)')j_pn

       else if (j_pn .lt. 100) then

        write(f_out(i2+2:i2+3), '(i2)')j_pn

       else if (j_pn .lt. 1000) then

        write(f_out(i2+1:i2+3), '(i3)')j_pn

      else

        write(f_out(i2+0:i2+3), '(i4)')j_pn

       end if

!      if (pc_get_lun() .ge. 0) &
!     & write(pc_get_lun(), '(/, " pario_name_1", &
!     & " i_pn=", i8, " i_pel=", i8, " f_out=", a)') &
!     & i_pn, cmpi_i_pel(), trim(f_out)

      return
      end subroutine pario_name_1

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine pario_name_2(f_inp, f_out, i_pn, i_pel)
!  create a unique file name, which has f_inp // i_pn // i_pel
!  incorporateing the process number i_pn and pe i_pel
      implicit  none

      character f_inp*(*)
      character f_out*(*)
      character f_tmp*80
      integer   i_pn, i_pel

      integer   i1, i2, j_pn

      call pario_name_1(f_inp, f_tmp, i_pn)
      call pario_name_1(f_tmp, f_out, i_pel)

!      if (pc_get_lun() .ge. 0) &
!     & write(pc_get_lun(), '(/, " pario_name_2", &
!     & " i_pn=", i8, " i_pel=", i8, " f_out=", a)') &
!     & i_pn, i_pel, trim(f_out)

      return
      end subroutine pario_name_2

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module pario_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
