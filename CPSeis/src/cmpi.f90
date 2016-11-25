!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- CMPI.f90 --------------------------------!!
!!------------------------------- CMPI.f90 --------------------------------!!
!!------------------------------- CMPI.f90 --------------------------------!!

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
! Name       : CMPI 
! Category   : math
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : various parrellel operations.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! various parrellel operations.
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
!                hello = CMPI     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    CMPI_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    CMPI_ZZZ (bbbb, indx, value)
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
!  5. 2006-06-12  B. Menger      Removed Unused Variables.
!  4. 2001-01-10  Douglas Hanson add cmpi_bcast__c_
!  3. 2000-09-22  Douglas Hanson fix allrealmin, allrealmax bug
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
      module cmpi_module
! This is a set of utility wrappers for mpi calls
! Currently these are all modified to work in a single pe environment.
! The actual mpi calls have been commented out.

      use pc_module
!      use pcps_module

      implicit  none

! set default to all routines public
      public

! subroutines
      public :: cmpi_pe_divide        ! divide the image between pes in the x di
      public :: cmpi_pe_divide_limit  ! limit the value of div_type
      public :: cmpi_flush_file       ! flush a file
      public :: cmpi_stop             ! execute stop with a print
      public :: cmpi_bcast
      public :: cmpi_reduce
      public :: cmpi_allreduce

! functions
      public :: cmpi_init      ! initialize the mpi calls
      public :: cmpi_finalize  ! finalize the mpi calls
      public :: cmpi_i_pel     ! return the pe index 0=first cmpi_n_pel()-1=last
      public :: cmpi_k_pel     ! return the pe index 1=first cmpi_n_pel()=last
      public :: cmpi_n_pel     ! return the number of pes.
      public :: cmpi_barrier   ! pause until all pes reach his point
      public :: cmpi_allintmin ! return min value over all pes
      public :: cmpi_allintmax ! return max value over all pes
      public :: cmpi_allrealmin ! return min value over all pes
      public :: cmpi_allrealmax ! return max value over all pes

      interface cmpi_bcast
        module procedure  cmpi_bcast_r_1d
        module procedure  cmpi_bcast_r_2d
        module procedure  cmpi_bcast_r_3d
        module procedure  cmpi_bcast_i_1d
        module procedure  cmpi_bcast_i_2d
        module procedure  cmpi_bcast_i_3d
        module procedure  cmpi_bcast_c_1d
        module procedure  cmpi_bcast_c_2d
        module procedure  cmpi_bcast_c_3d
        module procedure  cmpi_bcast_d_1d
        module procedure  cmpi_bcast_d_2d
        module procedure  cmpi_bcast_d_3d
        module procedure  cmpi_bcast_1_r
        module procedure  cmpi_bcast_1_i
        module procedure  cmpi_bcast_3
      end interface 

      interface cmpi_reduce
        module procedure  cmpi_reduce_r
        module procedure  cmpi_reduce_i
      end interface 

      interface cmpi_allreduce
        module procedure  cmpi_allreduce_r
        module procedure  cmpi_allreduce_i
        module procedure  cmpi_allreduce_0_r
        module procedure  cmpi_allreduce_0_i
        module procedure  cmpi_allreduce_1_r
        module procedure  cmpi_allreduce_1_i
      end interface 

      character(len=100),public,save :: CMPI_IDENT = &
      '$Id: cmpi.f90,v 1.5 2006/06/12 13:03:49 Menger prod sps $'


      contains

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_pe_divide( &
     & lu_out, c_title, div_type, nx_inp, x0_inp, dx_inp)
!  divide the image between pes in the x direction
! div_type = type of division 0 = round robin, 1 = block, 2 = 0 only 3 = all

!  passed variables
      integer ::   lu_out
      character c_title*(*)
      integer ::   div_type
      integer ::   nx_inp
      real ::      x0_inp, dx_inp

      integer ::   ix_out_pe
      integer ::   nx_out_pe
      integer ::   nx_out
      real ::      x0_out, dx_out
      integer ::   nx_out_0
      real ::      x0_out_0, dx_out_0
      real ::      x1_stk, y1_stk

      integer ::   div_type_0
      integer ::   n_extra, cmpi_j_pel

      div_type_0 = div_type
      call cmpi_pe_divide_limit(div_type_0)

      x1_stk = (nx_inp - 1) * dx_inp + x0_inp

      do cmpi_j_pel = 0 , cmpi_n_pel()-1

!  round robin division
        if (div_type_0 .eq. 0) then


          nx_out_pe = (nx_inp - 1) / cmpi_n_pel() + 1    ! num of xs per pe
          ix_out_pe = cmpi_j_pel                         ! num of xs bef this pe

          nx_out  = nx_out_pe                            ! num of xs on this pe
          x0_out  = x0_inp + dx_inp * cmpi_j_pel         ! first x this pe
          dx_out  = dx_inp * cmpi_n_pel()                ! x increment

          y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe

          if ( &
     & (dx_inp .ge. 0. .and. y1_stk .gt. x1_stk) &
     & .or. &
     & (dx_inp .lt. 0. .and. y1_stk .lt. x1_stk) &
     & ) nx_out = nx_out - 1

!  block division
         else if (div_type_0 .eq. 1) then    ! if (div_type_0 .eq. 0) then

!  all pes get at least nx_out_pe
!  n_extra pes get 1 extra
!  these are pes cmpi_i_pel() = cmpi_n_pel()-1-n_extra+1 - cmpi_n_pel()-1
          nx_out_pe = nx_inp / cmpi_n_pel()                ! min num of xs per p
          n_extra = nx_inp - nx_out_pe * cmpi_n_pel()       ! this many get ny_s
!        print*, ' i_pe=', cmpi_j_pel, ' n_pe=', cmpi_n_pel(), &
!     & ' nx_out_pe=', nx_out_pe, ' n_extra=', n_extra

          ix_out_pe = cmpi_j_pel * nx_out_pe &
     & + max(0, cmpi_j_pel+n_extra-cmpi_n_pel())

          nx_out  = nx_out_pe           ! num of xs on this pe
          if (cmpi_j_pel .ge. cmpi_n_pel()-n_extra) &
     & nx_out = nx_out_pe + 1 ! this many get nx_out_pe + 1
          x0_out  = x0_inp + dx_inp * ix_out_pe            ! first x this pe
          dx_out  = dx_inp                                 ! xincrement

!  only pe 0 gets any
         else if (div_type_0 .eq. 2) then

          if (cmpi_j_pel .eq. 0) then

            nx_out_pe = nx_inp                             ! num of xs per pe
            ix_out_pe = 0                                  ! num of xs bef this

            nx_out  = nx_out_pe                            ! num of xs on this p
            x0_out  = x0_inp                               ! first x this pe
            dx_out  = dx_inp                               ! x increment

            y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe

          else    ! if (cmpi_j_pel .eq. 0) then

            nx_out_pe = 0                                  ! num of xs per pe
            ix_out_pe = 0                                  ! num of xs bef this

            nx_out  = nx_out_pe                            ! num of xs on this p
            x0_out  = x0_inp                               ! first x this pe
            dx_out  = dx_inp                               ! x increment

            y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe

           end if    ! if (cmpi_j_pel .eq. 0) then

!  every pe gets them all
         else if (div_type_0 .eq. 3) then

          nx_out_pe = nx_inp                             ! num of xs per pe
          ix_out_pe = 0                                  ! num of xs bef this pe

          nx_out  = nx_out_pe                            ! num of xs on this pe
          x0_out  = x0_inp                               ! first x this pe
          dx_out  = dx_inp                               ! x increment

          y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe

         end if    ! if (div_type_0 .eq. 0) then

        y1_stk = (nx_out - 1) * dx_out + x0_out

        if (cmpi_j_pel .eq. cmpi_i_pel()) then

          nx_out_0 = nx_out
          x0_out_0 = x0_out
          dx_out_0 = dx_out

         end if    ! if (cmpi_j_pel .eq. cmpi_i_pel()) then

!  write the info
        if (cmpi_i_pel() .eq. 0) then

          if (cmpi_j_pel .eq. 0) then

            write(lu_out, '( &
     & /, '' cmpi_pe_divide pe division'', &
     & /, '' title='', a, &
     & /, '' number of pes           ='', i8, &
     & /, '' division type           ='', i8, '' 0=round robin, 1=block'',&
     & /, '' number of bins / block  ='', i8 &
     & )') &
     & trim(c_title), &
     & cmpi_n_pel(), &
     & div_type, &
     & nx_out_pe

              if (div_type .eq. 1) &
     & write(lu_out, &
     & '(1x, i8, '' pes '', i8, '' - '', i8, '' have '', i8, '' bins'', &
     & /, 1x, i8, '' pes '', i8, '' - '', i8, '' have '', i8, '' bins'' &
     & )') &
     & cmpi_n_pel()-n_extra, 0, &
     & cmpi_n_pel()-1-n_extra, nx_out_pe, &
     &             n_extra, cmpi_n_pel()-n_extra, &
     & cmpi_n_pel()-1       , nx_out_pe+1

              write(lu_out, '( &
     &   ''              pe   Num bef '', &
     & ''Num bins first bin  last bin  increment'', &
     & /, ''  input'', 1x, i8, 1x, i8, 1x, i8, 1x, g12.5, 1x, g12.5, 1x, g12.5&
     & )') &
     & cmpi_n_pel(), 0, nx_inp, x0_inp, x1_stk, dx_inp

           end if    ! if (cmpi_j_pel .eq. 0) then

          write(lu_out, &
     & '( &
     &   '' output'', 1x, i8, 1x, i8, 1x, i8, 1x, g12.5, 1x, g12.5, 1x, g12.5&
     & )') &
     & cmpi_j_pel, ix_out_pe, nx_out, x0_out, y1_stk, dx_out

         end if    ! if (cmpi_i_pel() .eq. 0) then

      end do    ! do cmpi_j_pel = 0  , cmpi_n_pel()-1

!  divide the image between pes in the x direction
      nx_inp = nx_out_0
      x0_inp = x0_out_0
      dx_inp = dx_out_0

      return
      end subroutine cmpi_pe_divide

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_pe_divide_limit(div_type)
!  limit the value of div_type
! div_type = type of division 0 = round robin, 1 = block, 2 = 0 only 3 = all

!  passed variables
      integer ::   div_type
      return
      end subroutine cmpi_pe_divide_limit

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_flush_file(lu)
!  flush current online buffer

      integer ::   lu
      !integer ::   i_err

!      call flush(lu, i_err)

      return

      end subroutine cmpi_flush_file

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_stop(title, i_flag)
! execute stop with a print

      character title*(*)
      integer ::   i_flag
      integer ::   i_err

      write(pc_get_lun(), '( &
     & /, a, /, '' pe='', i8, '' stopping i_flag='', i10)')&
     & trim(title), cmpi_i_pel(), i_flag

      if (pc_get_lun() .ne. 6) &
     & write(6        , '(/, a, /, '' pe='', i8, '' stopping i_flag='', i10,&
     & '' print unit='', i8)') &
     & trim(title), cmpi_i_pel(), i_flag, pc_get_lun()

      i_err = cmpi_finalize()

      stop
      end subroutine cmpi_stop

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_r_1d(i_pel, nx_inp_1, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1
      real ::      x_inp(:)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_r_1d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_r_2d(i_pel, nx_inp_1, nx_inp_2, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2
      real ::      x_inp(:, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_r_2d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_r_3d(i_pel, nx_inp_1, nx_inp_2, nx_inp_3, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2, nx_inp_3
      real ::      x_inp(:, :, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_r_3d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_i_1d(i_pel, nx_inp_1, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1
      integer ::   x_inp(:)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_i_1d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_i_2d(i_pel, nx_inp_1, nx_inp_2, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2
      integer ::   x_inp(:, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_i_2d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_i_3d(i_pel, nx_inp_1, nx_inp_2, nx_inp_3, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2, nx_inp_3
      integer ::   x_inp(:, :, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_i_3d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_c_1d(i_pel, nx_inp_1, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1
      complex ::    x_inp(:)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_c_1d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_c_2d(i_pel, nx_inp_1, nx_inp_2, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2
      complex ::   x_inp(:, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_c_2d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_c_3d(i_pel, nx_inp_1, nx_inp_2, nx_inp_3, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2, nx_inp_3
      complex ::   x_inp(:, :, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_c_3d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_d_1d(i_pel, nx_inp_1, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1
      double precision :: x_inp(:)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_d_1d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_d_2d(i_pel, nx_inp_1, nx_inp_2, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2
      double precision :: x_inp(:, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_d_2d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_d_3d(i_pel, nx_inp_1, nx_inp_2, nx_inp_3, x_inp)
!  broadcast nx_inp_1 elements of real array x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp_1, nx_inp_2, nx_inp_3
      double precision :: x_inp(:, :, :)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp_1, mpi_real, i_pel                     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_d_3d

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_c(i_pel, nx_inp, x_inp)
!  broadcast nx_inp elements of character string x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp
      character x_inp*(*)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, nx_inp, mpi_character, i_pel                &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_c

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_1_r(i_pel, x_inp)
!  broadcast real x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      real ::      x_inp

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, 1, mpi_real, i_pel                         &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_1_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_1_i(i_pel, x_inp)
!  broadcast integer x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   x_inp

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, 1, mpi_integer, i_pel                       
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_1_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_c1(i_pel, x_inp)
!  broadcast character x_inp
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      character x_inp*1

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(x_inp, 1, mpi_character, i_pel                    &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_c1

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_3(i_pel, nx, x0, dx)
!  broadcast nx, x0, dx
!  from pe i_pel to the rest of the Pes.                                 
!     & he MPI_Bcast routine broadcasts a message from the process with a 
!     & pecified rank (called a root) to all other processes of the group.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx
      real ::      x0, dx

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_bcast(nx, 1, mpi_integer, i_pel                         &, &
!     & mpi_comm_world, mpi_return)
!      call mpi_bcast(x0, 1, mpi_real, i_pel                         &, &
!     & mpi_comm_world, mpi_return)
!      call mpi_bcast(dx, 1, mpi_real, i_pel                         &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      return
      end subroutine cmpi_bcast_3

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_reduce_r(i_pel, nx_inp, x_inp, x_out)
!  reduce nx_inp elements of real array x_inp on all pes
!  into real array x_out on pe i_pel.                                    
!     & he MPI_Reduce routine reduces values on all processes to a single value.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp
      real ::      x_inp(nx_inp)
      real ::      x_out(nx_inp)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_reduce(x_inp, x_out, nx_inp, mpi_real, mpi_sum           &, &
!     & i_pel, mpi_comm_world, mpi_return)
!  Top of J90 version
      x_out = x_inp
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_reduce_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_reduce_i(i_pel, nx_inp, x_inp, x_out)
!  reduce nx_inp elements of integer array x_inp on all pes
!  into integer array x_out on pe i_pel.                                 
!     & he MPI_Reduce routine reduces values on all processes to a single value.

!      include   'mpif.h'

      integer ::   i_pel
      integer ::   nx_inp
      integer ::   x_inp(nx_inp)
      integer ::   x_out(nx_inp)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_reduce(x_inp, x_out, nx_inp, mpi_integer, mpi_sum        &, &
!     & i_pel, mpi_comm_world, mpi_return)
!  Top of J90 version
      x_out = x_inp
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_reduce_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_r(nx_inp, x_inp, x_out)
!  reduce nx_inp elements of real array x_inp on all pes
!  into real array x_out and broadcast to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   nx_inp
      real ::      x_inp(nx_inp)
      real ::      x_out(nx_inp)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, nx_inp, mpi_real, mpi_sum        &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version
      x_out = x_inp
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_allreduce_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_i(nx_inp, x_inp, x_out)
!  reduce nx_inp elements of integer array x_inp on all pes
!  into integer array x_out and broadcast to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   nx_inp
      integer ::   x_inp(nx_inp)
      integer ::   x_out(nx_inp)

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, nx_inp, mpi_integer, mpi_sum     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version
      x_out = x_inp
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_allreduce_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_0_r(nx_inp, x_inp)
!  reduce nx_inp elements of real array x_inp on all pes
!  inplace and broadcast to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   nx_inp
      real ::      x_inp(nx_inp)

      !real ::      work(nx_inp)    ! automatic array

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_allreduce(x_inp, work, nx_inp, mpi_real, mpi_sum         &, &
!     & mpi_comm_world, mpi_return)
!      x_inp = work
!  Top of J90 version
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_allreduce_0_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_0_i(nx_inp, x_inp)
!  reduce nx_inp elements of integer array x_inp on all pes
!  into integer array x_inp using work as a work array and broadcast to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   nx_inp
      integer ::   x_inp(nx_inp)

      !integer ::   work(nx_inp)    ! automatic array

      !integer ::   mpi_return

!  Top of T3E version
!      call mpi_allreduce(x_inp, work, nx_inp, mpi_integer, mpi_sum      &, &
!     & mpi_comm_world, mpi_return)
!      x_inp = work
!  Top of J90 version
!  End of T3E,  J90 differences

      return
      end subroutine cmpi_allreduce_0_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_1_r(x_inp)
!  reduce the value of a single real value x_inp from all pes to all pes 
!     & he MPI_Reduce routine reduces values on all processes to a single value.

!      include   'mpif.h'

      integer ::   nx_inp
      real ::      x_inp
      real ::      x_out

      !integer ::   mpi_return

      nx_inp = 1
      x_out = x_inp

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, nx_inp, mpi_real, mpi_sum        &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      x_inp = x_out

      return
      end subroutine cmpi_allreduce_1_r

!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_1_i(x_inp)
!  reduce the value of a single integer value x_inp from all pes to all p&s&
!     & he MPI_Reduce routine reduces values on all processes to a single value.

!      include   'mpif.h'

      integer ::   nx_inp
      integer ::   x_inp
      integer ::   x_out

      !integer ::   mpi_return

      nx_inp = 1
      x_out = x_inp

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, nx_inp, mpi_integer, mpi_sum     &, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E,  J90 differences

      x_inp = x_out

      return
      end subroutine cmpi_allreduce_1_i

!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_init()
! initialize the mpi calls                                               
!     & he MPI_Init routine initializes the MPI execution environment.

!      include   'mpif.h'

      integer ::   i_err

      i_err = 0

!  Top of T3E version
!      call mpi_init(i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_init = i_err

      return
      end function cmpi_init

!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_finalize()
! finalize the mpi calls

!      include   'mpif.h'

      integer ::   i_err

      i_err = 0

!  Top of T3E version
!      call mpi_barrier(mpi_comm_world, i_err)
!      call mpi_finalize(i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_finalize = i_err

      return
      end function cmpi_finalize

!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_i_pel()
! return the pe number 0 = first cmpi_n_pel()-1 = last &
!     & e MPI_Comm_rank routine determines the rank of the calling proces &
!     & the communicator.

!      include   'mpif.h'

      integer :: i_pel
      !integer :: i_err

      i_pel = 0

!  Top of T3E version
!      call mpi_comm_rank(mpi_comm_world, i_pel, i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_i_pel = i_pel
!      cmpi_i_pel = max ( 0, pcps_current_worker_num - 1 )

      return
      end function cmpi_i_pel


!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_k_pel()
! return the pe number 0 = first cmpi_n_pel()-1 = last &
!     & e MPI_Comm_rank routine determines the rank of the calling proces &
!     & the communicator.

!      include   'mpif.h'

      integer :: i_pel
      !integer :: i_err

      i_pel = 0

!  Top of T3E version
!      call mpi_comm_rank(mpi_comm_world, i_pel, i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_k_pel = cmpi_i_pel()
!      cmpi_k_pel = pcps_current_worker_num

      return
      end function cmpi_k_pel

!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_n_pel()
! return the number of pes. &
!     & e MPI_Comm_size routine determines the number of the calling procsses&
!     & the communicator.

!      include   'mpif.h'

      integer :: n_pel
      !integer :: i_err

      n_pel = 1

!  Top of T3E version
!      call mpi_comm_size(mpi_comm_world, n_pel, i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_n_pel = n_pel
!      cmpi_n_pel = max ( 1, pcps_num_workers )

      return
      end function cmpi_n_pel

!23456789012345678901234567890123456789012345678901234567890123456789012
      integer  function cmpi_barrier()
!  set an mpi barrier, wait at this point until all pes reach it &
!     & e MPI_Barrier routine blocks until all processes have reached the &
!     & utine at which the barrier is placed.  It blocks the caller until &
!     & l group members have called it; the call returns at any process oly&
!     & ter all group members have entered the call.

!      include   'mpif.h'
      integer ::   i_err

      i_err = 0

!  Top of T3E version
!      call mpi_barrier(mpi_comm_world, i_err)
!  Top of J90 version

!  End of T3E, J90 differences

      cmpi_barrier = i_err

      return
      end function cmpi_barrier

!123456789012345678901234567890123456789012345678901234567890123456789012
       integer function cmpi_allintmin(x_inp)
!  return the maximum of integer x_inp from all pes to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   x_inp, x_out
!      !integer ::   mpi_return

      x_out = x_inp

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, 1, mpi_integer, mpi_min, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E, J90 differences

       cmpi_allintmin = x_out

       return

       end function cmpi_allintmin

!123456789012345678901234567890123456789012345678901234567890123456789012
       integer function cmpi_allintmax(x_inp)
!  return the maximum of integer x_inp from all pes to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      integer ::   x_inp, x_out

      x_out = x_inp

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, 1, mpi_integer, mpi_max, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E, J90 differences

       cmpi_allintmax = x_out

       return

       end function cmpi_allintmax

!123456789012345678901234567890123456789012345678901234567890123456789012
       real function cmpi_allrealmin(x_inp)
!  return the maximum of real x_inp from all pes to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      real ::      x_inp, x_out

      x_out = x_inp

!  Top of J90 version

!  End of T3E, J90 differences

       cmpi_allrealmin = x_out

       return

       end function cmpi_allrealmin

!123456789012345678901234567890123456789012345678901234567890123456789012
       real function cmpi_allrealmax(x_inp)
!  return the maximum of real x_inp from all pes to all pes.
!  The MPI_Allreduce routine combines values from all processes and
!  distributes the result back to all processes.

!      include   'mpif.h'

      real ::      x_inp, x_out
      !integer ::   mpi_return

      x_out = x_inp

!  Top of T3E version
!      call mpi_allreduce(x_inp, x_out, 1, mpi_real, mpi_max, &
!     & mpi_comm_world, mpi_return)
!  Top of J90 version

!  End of T3E, J90 differences

       cmpi_allrealmax = x_out

       return

       end function cmpi_allrealmax

      end module cmpi_module
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

