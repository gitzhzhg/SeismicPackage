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
!                        C P S   P R I M I T I V E
!
! Name       : pp
! Category   : io
! Written    : 2000-12-20   by: Charles C. Burch
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : Various parallel IO operations.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Various parallel operations.
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
! subroutine pp_assign_x2workers(lu_out, title, mode, nx, x0, dx)
!
! subdivides a x range defined by x0, dx and nx which are assigned to
!    n_workers using mode (0=round robin, 1=block, 2= worker1 only 3=all)
!
! Calling parameters
!   integer, intent(in)            :: lu_out  !lu for prints
!   integer, intent(in)            :: mode
!   character, (len=*), intent(in) :: title   !title for prints
!   integer, intent(inout)         :: nx
!   real   , intent(inout)         :: x0, dx
!-------------------------------------------------------------------------------
! subroutine pp_setup_test_environment(n_procs, i_proc)
!
! For testing use only: sets pcps variable to simulate using n_procs and
!                       being worker number i_proc
!
! Calling parameters
!   integer, intent(in)  :: n_procs, i_proc
!-------------------------------------------------------------------------------
! subroutine pp_create_file_name(base_name, file_name, i_pn, i_worker)
!
! creates a file name using a "base_name" and the process number(i_pn)
!  results are placed in "file_name"
! an optional field "i_worker" adds worker number to the generated
!   name, if present
!
!  Calling parameters
!    character (len=*), intent(in)  :: base_name
!    character (len=*), intent(out) :: file_name
!    integer          , intent(in)  :: i_pn
!    integer, optional, intent(in)  :: i_worker
!-------------------------------------------------------------------------------
! subroutine pp_open_file(i_file, i_worker, file_name, file_stat, i_err, mode,&
!  file_space_commit, file_lock,  file_auto_delete, file_extent_size)
!
! if worker i_worker, open file with file_name
!  file_stat controls how the file is opened as with CIO
!
!  returns handle i_file
!  return i_err=0 if successful or -1 if error
!  mode =0 :no coordination is done with workers opening the file other than
!           only i_worker opens the file with the call though
!       =1 :parallel coordination done--if file is to be created,
!           i_worker firsts creates the file.
!           Then each worker one at a time opens the file
!       =2 :Parallel coordination but only i_worker opens the file
!           This mode is for primarily reads where
!           i_worker reads and broadcast to others
!       =3 :Parallel coordination but only i_worker opens the file
!           This mode is for primarily reads where
!           i_worker reads and sends to worker 0
!
!   For modes 1,2 and 3, all cpus must make similar simultaneous call to
!    this routine
!
!  file_space_commit is optional and can be
!                    PREALLOCATE_FILE_SPACE_ENABLED  preallocate,
!                    PREALLOCATE_FILE_SPACE_DISABLED don't preallocate
!                    PREALLOCATE_FILE_SPACE_DEFAULT  preallocate cpstemp/data
!  file_lock         is optional and can be
!                    FILE_LOCK_DISABLED then do not lock file,
!                    FILE_LOCK_ENABLED lock as needed
!  file_auto_delete is optional and can be
!                    AUTO_DELETE_DISABLED then do not auto delete file,
!                    AUTO_DELETE_ON_EXIT auto delete when program exits
!                    AUTO_DELETE_ON_CLOSE auto delete when file is closed
!  file_extent_size is optional and specifies pfio file extent size
!
!
!  Calling parameters
!   integer, intent(out)          :: i_file, i_err
!   integer, intent(in)           :: i_worker, mode
!   character (len=*), intent(in) :: file_name, file_stat
!   integer, optional, intent(in) :: file_space_comit, file_lock
!   integer, optional, intent(in) :: file_auto_delete, file_extent_size
!-------------------------------------------------------------------------------
! subroutine pp_close_file(i_file, i_worker, l_remove, i_err, mode)
!
! if worker i_worker, close file with handle i_file
!  l_remove(true/false) controls if file is deleted when it is closed
!
!  return i_err=0 if successful or -1 if error
!  mode=0 : not parallel coordination-only i-worker close file
!      =1 parallel coordination: all workers close file without delete
!         except i_worker.  i_worker closes the file last,
!         removes it if requested, and touches it to force NFS invalidation
!      =2,3 Parallel coordination, but only i_worker closes the file
!         error is shared amongst workers
!
!  For modes 1/2/3, all cpus must make similar simultaneous call to this routine
!
!  Calling parameters
!     integer, intent(in)           :: i_file, i_worker, mode
!     logical, intent(in)           :: l_remove
!     integer, intent(out)          :: i_err
!-------------------------------------------------------------------------------
! subroutine pp_zero_byte_file(i_file, i_worker, n_rec, n_data, i_err, mode)
! if worker iworker write n_rec of n_data zero bytes to
!  file with handle i_file
!  mode=0  no parallel coordination
!  mode=1  Parallel coordination-all cpus call the routine, but only i_worker
!          does the writing but all cpus wait until it is done
!  mode =2,3 Currently same as mode=1
!
!  For modes 1/2/3, all cpus must make similar simultaneous call to this routine
!
!  Calling parameters
!   integer, intent(in)  :: i_worker, i_file, n_rec, n_data
!   integer, intent(out) :: i_err
!-------------------------------------------------------------------------------
! subroutine pp_read_file( i_file, i_worker, n_read, i_first, n_stride, &
!  n_data, x_data, i_err, mode)
!
! if worker i_worker, read from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker reads the file
!  mode=1 : only i_worker reads the file, workers are synced up after the read
!  mode=2 : only i_worker reads the file, but results broadcasted to other cpus
!  mode=3 : only i_worker reads the file, but results sent to worker 0 
!
!  For modes 1/2/3, all cpus must make similar simultaneous call to this routine
!
!  Calling parameters
!   integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
!   integer, intent(in)           :: n_data, mode
!   x_data can be integer real, double, complex 2 or 3d
!   integer, intent(out) ::  i_err
!-------------------------------------------------------------------------------
! subroutine pp_write_file( i_file, i_worker, n_write, i_first, n_stride, &
!  n_data, x_data, i_err, mode)
!
! if worker i_worker, writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2,3 : currently same as mode=1 but reads are done differently
!
!  For modes 1/2/3, all cpus must make similar simultaneous call to this routine
!
!  Calling parameters
!   integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
!   integer, intent(in)        :: n_data, mode
!   x_data can be integer real, double, complex 2 or 3d
!   integer, intent(out)       :: i_err
!-------------------------------------------------------------------------------
! subroutine pp_zero_trace_file(i_file, i_worker, n_rec, &
!  hd_len, tr_len, i_err)
!
! if worker i_worker, writes n_rec records of zeroes
!  with length of trace data with header length hd_len and
!  trace length tr_len
!
!  returns i_err: is zero if sucessful and -1 if error
!  mode same as pp_zero_file
!
!  Calling parameters
!   integer, intent(in)  :: i_file, i_worker, hd_len, tr_len
!   integer, intent(out) :: i_err
!-------------------------------------------------------------------------------
! subroutine pp_write_trace_file(i_file, i_worker, &
!  n_write, i_first, n_stride, hd_len, tr_len, hds, trcs, i_err, mode)
!
! if worker i_worker, write to file with handle i_file
!  n_write records with traces (hdrs/trcs) starting at
!  record i_first with a stride of n_stride records between writes.
!  hdrs and trcs contain the traces to be written with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!
!  returns i_err: is zero if sucessful and -1 if error
!  mode as for pp_write_file
!
!  Calling parameters
!   integer, intent(in)           :: i_file, i_worker, mode
!   integer, intent(in)           :: n_write, i_first, n_stride
!   integer, intent(in)           :: hd_len, tr_len
!   integer, intent(out)          :: i_err
!   real, intent(in)              :: trs(tr_len,*)
!   double precision, intent(in)  :: hdrs(hd_len, *)
!-------------------------------------------------------------------------------
! subroutine pp_read_trace_file(i_file, i_worker, &
!  n_read, i_first, n_stride, hd_len, tr_len, hds, trcs, i_err, mode)
!
! if worker i_worker, read from file with handle i_file
!  n_read records with traces (hdrs/trcs) starting at
!  record i_first with a stride of n_stride records between reads.
!  hdrs and trcs contain the traces to be read with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!
!  returns i_err: is zero if sucessful and -1 if error
!  mode same for pp_read_file
!
!  Calling parameters
!   integer, intent(in)           :: i_file, i_worker, mode
!   integer, intnet(in)           :: n_read, i_first, n_stride
!   integer, intent(in)           :: hd_len, tr_len
!   integer, intent(out)          :: i_err
!   real, intent(out)             :: trs(tr_len,*)
!   double precision, intent(out) :: hdrs(hd_len, *)
!-------------------------------------------------------------------------------
! subroutine pp_create_new_file(file_name, i_worker, ext_size,  &
!  num_exts, zero_sw, mode, i_err)
! creates a new file with given file name,  extent size and 
!   number of extents
! If zero_sw is not zero, file is zeroed and file space
!   preallocated, if zero, data only written at last byte
!
! If file is created succesfully, i_err=0, else is -1
!
! file is created on cpu i_worker.
! In mode 0, no parallel coordination.
! In mode 1, cpus are synced up prior to creating the file 
!  and status of file creation is shared with all cpus
!
!  Calling parameters
!   character(len=*), intent(in) :: file_name
!   integer,          intent(in) :: i_worker
!   integer,          intent(in) :: ext_size
!   integer,          intent(in) :: num_exts
!   integer,          intent(in) :: zero_sw 
!   integer,          intent(out):: i_err   
!-------------------------------------------------------------------------------
! subroutine pp_sync_file_updates(file_name, i_worker, mode)
!
! Update a file's modification time so NFS cache is forced to be  
!  updated to have the most recent version of the data written to a file
! Only i_worker will request the time update to be performed.
! mode=0: no parallel synchronization, 
!      1: cpus are synchronized prior to and after the file time update
!
!  Calling parameters
!   character (len=*), intent(in ) :: file_name
!   integer          , intent(in)  :: mode
!   integer,           intent(in)  :: i_worker
!-------------------------------------------------------------------------------
! subroutine pp_dump_stats(title)
! Dump counts and elapse stats 
!
!  Calling parameters
!   character (len=*), intent(in) :: title
!-------------------------------------------------------------------------------
! subroutine pp_reset_stats
!   Reset count and elapse
!-------------------------------------------------------------------------------
! subroutine pp_start_stats()
! set flag to collect stats 
!-------------------------------------------------------------------------------
! subroutine pp_stop_stats()
! set flag to not collect stats 
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
!     Date        Author      Description
!     ----        ------      -----------
!010. 2006-04-25  B. Menger   Removed Unused Variables.
!  9. 2005-12-05  Brian Macy  Added pp_set_direct_io
!  8. 2004-03-15  Bill Done   In pp_create_file_name() change the "*" in two
!                             write statements to "'(i10)'". This is to fix a
!                             problem with the Portland Group compiler.
!  7. 2002-03-26  Burch CC    Fix bug with return error code on 3d read routines
!                             Added pp_create_new_file & pp_sync file_updates
!  6. 2001-12-10  Burch CC    Added mode-3 parallel read:send read data to boss
!                             Made zero file/zero trace file more efficient
!                             Added data overflow checks
!                             Added tracking of elapse time and # times called
!  5. 2001-08-28  Burch CC    Added parallel options for mode flag.
!                             Added space-commit/file_lock/auto-delete options
!                             to pp_open_file.
!  4. 2001-07-10  Burch CC    Various changes for 2d and 3d data types
!                             Many of which made by Doug Hanson.
!  3. 2001-03-27  Burch CC    Various changes for migrations codes
!  2. 2001-01-10  Burch CC    Bug fix in pp_assign_x2workers when no workers
!  1. 2000-12-20  Burch CC    Initial version
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
module pp_module
  use pcps_module
  use pcpsx_module
  use cio_module
  use sizeof_module
  use getlun_module
  use string_module
  use ppio_module
  use cpucount_module
  use getsys_module

  implicit  none

  private

  public :: pp_assign_x2workers       ! divide the image between workers
  public :: pp_close_file
  public :: pp_create_file_name
  public :: pp_create_new_file
  public :: pp_dump_stats
  public :: pp_open_file
  public :: pp_open_form
  public :: pp_read_file
  public :: pp_read_trace_file
  public :: pp_read_trace_file_1
  public :: pp_read_trace_file_2
  public :: pp_reset_stats
  public :: pp_setup_test_environment ! simulates pcps environment
  public :: pp_start_stats
  public :: pp_stop_stats
  public :: pp_sync_file_updates
  public :: pp_write_file
  public :: pp_write_trace_file
  public :: pp_write_trace_file_1
  public :: pp_write_trace_file_2
  public :: pp_zero_file
  public :: pp_zero_trace_file
  public :: pp_set_direct_io

  interface pp_read_file
    module procedure pp_read_file_c1
    module procedure pp_read_file_r1
    module procedure pp_read_file_r2
    module procedure pp_read_file_r3
    module procedure pp_read_file_i1
    module procedure pp_read_file_i2
    module procedure pp_read_file_i3
    module procedure pp_read_file_d1
    module procedure pp_read_file_d2
    module procedure pp_read_file_d3
    module procedure pp_read_file_z1
    module procedure pp_read_file_z2
    module procedure pp_read_file_z3
  end interface
  !
  interface pp_write_file
    module procedure pp_write_file_c1
    module procedure pp_write_file_r1
    module procedure pp_write_file_r2
    module procedure pp_write_file_r3
    module procedure pp_write_file_i1
    module procedure pp_write_file_i2
    module procedure pp_write_file_i3
    module procedure pp_write_file_d1
    module procedure pp_write_file_d2
    module procedure pp_write_file_d3
    module procedure pp_write_file_z1
    module procedure pp_write_file_z2
    module procedure pp_write_file_z3
  end interface

  interface pp_read_trace_file
    module procedure pp_read_trace_file_1
    module procedure pp_read_trace_file_2
  end interface 

  interface pp_write_trace_file
    module procedure pp_write_trace_file_1
    module procedure pp_write_trace_file_2
  end interface 

  interface
    function cio_fread_char_c (ptr, nbytes, unit) result (numread)
      character, intent(out)                  :: ptr
      integer,   intent(in)                    :: nbytes, unit
      integer                                  :: numread
    end function cio_fread_char_c
  end interface

  interface
    function cio_fwrite_char_c (ptr, nbytes, unit) result (numread)
      character, intent(in)                   :: ptr
      integer,  intent(in)                    :: nbytes, unit
      integer                                 :: numread
    end function cio_fwrite_char_c
  end interface

  logical  :: pp_stat_flag=.false.
  integer  :: pp_counts=0
  real     :: pp_elapse=0, pp_t0
  
  character(len=100),save,public :: PP_ident =                               &
   "$Id: pp.f90,v 1.10 2006/04/25 13:24:03 Menger prod sps $"

contains

!--------------------------------------------------------------------
! Dump counts and elapse stats 
!
! Written October 2001 by Charles C Burch
!--------------------------------------------------------------------
  subroutine pp_dump_stats(title)
    character (len=*), intent(in) :: title

    call pcps_print(" ",2)
    write(pcps_message,*) trim(title)//", cpu=",pcps_current_worker_num
    call pcps_print(pcps_message,2)
    
    write(pcps_message,*)  &
     " # times pp called=",pp_counts,", elapse time in pp=",pp_elapse
    call pcps_print(pcps_message,2)
    
    return
  end subroutine pp_dump_stats

!--------------------------------------------------------------------
! Reset count and elapse
!
! Written October 2001 by Charles C Burch
!--------------------------------------------------------------------
  subroutine pp_reset_stats
    pp_counts=0
    pp_elapse=0.
    return
  end subroutine pp_reset_stats

!--------------------------------------------------------------------
! set flag to collect stats 
!
! Written October 2001 by Charles C Burch
!--------------------------------------------------------------------
  subroutine pp_start_stats()
    pp_stat_flag=.true.
    return
  end subroutine pp_start_stats

!--------------------------------------------------------------------
! set flag to not collect stats 
!
! Written October 2001 by Charles C Burch
!--------------------------------------------------------------------
  subroutine pp_stop_stats()
    pp_stat_flag=.false.
    return
  end subroutine pp_stop_stats

!--------------------------------------------------------------------
! subdivides a x ranges defined by x0, dx and nx which are asigned to
!    n_workers using mode (0=round robin, 1=block, 2= boss only 3=all
!
! Modeled after cmpi_pe_divide written by Doug Hanson
!
! Written December 2000 by Charles C Burch
!--------------------------------------------------------------------
  subroutine pp_assign_x2workers(lu_out, title, mode, nx, x0, dx)
    integer, intent(in)            :: lu_out, mode
    character (len=*), intent(in)  :: title
    integer, intent(inout)         :: nx
    real   , intent(inout)         :: x0, dx

    integer   :: nx_inp, nx_out, nx_this_worker, i_worker, n_workers
    real      :: x0_inp, x0_out, x0_this_worker
    real      :: dx_inp, dx_out, dx_this_worker


    integer   :: ix_out_pe, nx_out_pe, n_extra, print_worker
    real      :: x1_stk, y1_stk

    nx_inp=nx
    x0_inp=x0
    dx_inp=dx

    if(pcps_num_procs.eq.1) then
      print_worker=0
      n_workers=1
    else
      if(pcps_current_worker_num.eq.0) return
      print_worker=1
      n_workers=pcps_num_workers
    endif

    x1_stk = (nx_inp - 1) * dx_inp + x0_inp
    n_extra=0

    do i_worker=1, n_workers

      select case(mode)
      case (0)
! --------------------round robin division-------------------------

        nx_out_pe = (nx_inp - 1) / n_workers + 1    ! num of xs per pe
        ix_out_pe = i_worker                        ! num of xs bef this pe

        nx_out  = nx_out_pe                         ! num of xs on this pe
        x0_out  = x0_inp + dx_inp * (i_worker-1)         ! first x this pe
        dx_out  = dx_inp * n_workers                ! x increment

        y1_stk = (nx_out - 1) * dx_out + x0_out     ! last x this pe

        if( (dx_inp .ge. 0. .and. y1_stk .gt. x1_stk) .or.                     &
         (dx_inp .lt. 0. .and. y1_stk .lt. x1_stk) ) nx_out=nx_out-1
      case(1)
! --------------------------- block division-------------------------

!  ---------------all workers get at least nx_out_pe----------------
!  n_extra workers (n_workers-n_extra : n_workers) get one extra

        nx_out_pe = nx_inp / n_workers              ! min num of xs per p
        n_extra = nx_inp - nx_out_pe * n_workers    ! this many get ny_s

        ix_out_pe = (i_worker-1) * nx_out_pe                                   &
         & + max(0, i_worker-1+n_extra-n_workers)

        nx_out  = nx_out_pe           ! num of xs on this pe
        if ((i_worker-1) .ge. n_workers-n_extra)                              &
         nx_out = nx_out_pe + 1 ! this many get nx_out_pe + 1
        x0_out  = x0_inp + dx_inp * ix_out_pe       ! first x this pe
        dx_out  = dx_inp                            ! xincrement

      case(2)
!  ----------------------only pe 1 gets any  ------------------

        if (i_worker .eq. 1) then
          nx_out_pe = nx_inp                             ! num of xs per pe
          ix_out_pe = 0                                  ! num of xs bef this
          nx_out  = nx_out_pe                            ! num of xs on this p
          x0_out  = x0_inp                               ! first x this pe
          dx_out  = dx_inp                               ! x increment
          y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe
        else
          nx_out_pe = 0                                  ! num of xs per pe
          ix_out_pe = 0                                  ! num of xs bef this
          nx_out  = nx_out_pe                            ! num of xs on this p
          x0_out  = x0_inp                               ! first x this pe
          dx_out  = dx_inp                               ! x increment
          y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe
        end if

      case(3)
! ----------------- every pe gets them all ------------------------

        nx_out_pe = nx_inp                             ! num of xs per pe
        ix_out_pe = 0                                  ! num of xs bef this pe
        nx_out  = nx_out_pe                            ! num of xs on this pe
        x0_out  = x0_inp                               ! first x this pe
        dx_out  = dx_inp                               ! x increment
        y1_stk = (nx_out - 1) * dx_out + x0_out        ! last x this pe

      case default
        nx_out=nx_inp
        x0_out=x0_inp
        dx_out=dx_inp
      end select

      y1_stk = (nx_out - 1) * dx_out + x0_out

      if (i_worker .eq. pcps_current_worker_num .or. pcps_num_procs.eq.1) then
        nx_this_worker = nx_out
        x0_this_worker = x0_out
        dx_this_worker = dx_out
      end if

!  ------------------------write the info------------------------
      if ( pcps_current_worker_num.eq.print_worker) then

        if (i_worker .eq. 1) then

          write(lu_out, '( &
          &  '' pp_x2workers_assign'', &
          &  '' title='', a, &
          &  '' number of workers           ='', i8, &
          &  '' division type           ='', i8, '' 0=round robin, 1=block'',&
          &  '' number of bins / block  ='', i8 &
          & )')                                                                &
           trim(title),                                                        &
           n_workers,                                                          &
           mode,                                                               &
           nx_out_pe

          if (mode .eq. 1) then
            write(lu_out,                                                      &
             '(1x,i8,'' workers '',i8,'' - '',i8,'' have '',i8,'' bins'')')    &
             n_workers-n_extra, 1, n_workers-n_extra, nx_out_pe
            if(n_extra.gt.0) write(lu_out,                                     &
             '(1x,i8,'' workers '',i8,'' - '',i8,'' have '',i8,'' bins'')')    &
             n_extra, n_workers-n_extra+1, n_workers , nx_out_pe+1
            write(lu_out,*) " "
          endif

          write(lu_out,                                                        &
           '( ''            wrks   Num bef '', &
          & ''Num bins first bin  last bin  increment'', &
          & ''  input'',1x,i8,1x,i8,1x,i8,1x,g12.5,1x,g12.5,1x,g12.5         &
          & )')                                                                &
           pcps_num_workers, 0, nx_inp, x0_inp, x1_stk, dx_inp

        end if

        write(lu_out,                                                          &
         '('' output'',1x,i8,1x,i8,1x,i8,1x,g12.5,1x,g12.5,1x,g12.5 )')        &
         i_worker, ix_out_pe, nx_out, x0_out, y1_stk, dx_out

      end if

    end do

!  divide the image between pes in the x direction
    nx = nx_this_worker
    x0 = x0_this_worker
    dx = dx_this_worker

    return

  end subroutine pp_assign_x2workers

!-----------------------------------------------------------------
! For testing use only
! sets pcps variable to simulate using n_procs and
!  being processor number i_proc
!
! Written December 2000 by Charles C Burch
!-----------------------------------------------------------------
  subroutine pp_setup_test_environment(n_procs, i_proc)
    integer, intent(in)  :: n_procs, i_proc

    pcps_num_procs=max(1,n_procs)
    pcps_num_workers=max(1, pcps_num_procs-1)
    pcps_current_worker_num=max(0,min(pcps_num_workers, i_proc))

    if(pcps_num_workers.le.1) then
      pcps_worker_mode=.true.
      pcps_boss_mode=.true.
    else
      pcps_boss_mode=pcps_current_worker_num.eq.0
      pcps_worker_mode=.not.pcps_boss_mode
    endif
    return
  end subroutine pp_setup_test_environment

!---------------------------------------------------------------------
! creates a file name using a "base_name" and the process number(i_pn)
!  results are placed in "file_name"
! an optional field "i_worker" adds worker number to the generated
!   name, if present
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------------------------
  subroutine pp_create_file_name(base_name, file_name, i_pn, i_worker)
    character (len=*), intent(in)  :: base_name
    character (len=*), intent(out) :: file_name
    integer          , intent(in)  :: i_pn
    integer, optional, intent(in)  :: i_worker

    integer           :: worker_num
    character(len=10) :: a_work

    if(present(i_worker)) then
      worker_num=i_worker
    else
      worker_num=-1
    endif

    write(a_work,'(i10)') i_pn    ! change * to '(i10)' for Portland
    file_name=trim(base_name)//"_"//adjustl(a_work)
    if(worker_num.ge.0) then
      write(a_work,'(i10)') worker_num    ! change * to '(i10)' for Portland
      file_name=trim(file_name)//"_"//adjustl(a_work)
    endif
    return
  end subroutine pp_create_file_name

!-----------------------------------------------------------------------
! Update a file's modification time so NFS cache is forced to be  
!  updated to have the most recent version of the data written to a file
! Only i_worker will request the time update to be performed.
! mode=0: no parallel synchronization, 
!      1: cpus are synchrnized prior to and after the file time update
!
! Written February 2002 by Charles C Burch
!-----------------------------------------------------------------------
  subroutine pp_sync_file_updates(file_name, i_worker, mode)
    character (len=*), intent(in ) :: file_name
    integer          , intent(in)  :: mode
    integer,           intent(in)  :: i_worker

    integer  :: i_err

    if(mode.lt.0 .or. mode.gt.1) then
      write(pcps_message,*) &
        "Warning: Invalid mode(",mode,") in pp_sync_file_updates"
      call pcps_print(pcps_message,2)
    endif

    if(mode.eq.1) call pcpsx_sync_workers(i_err)   !sync up workers

    if(i_worker.eq.pcps_current_worker_num) &
     call cio_update_file_time(file_name)

    if(mode.eq.1) call pcpsx_sync_workers(i_err)   !sync up workers

    return
  end subroutine pp_sync_file_updates

!----------------------------------------------------------
! creates a new file with given file name,  extent size and 
!   number of extents
! If zero_sw is not zero, file is zeroed and file space
!   preallocated, if zero, data only written at last byte
!
! If file is created succesfully, i_err=0, else is -1
!
! file is created on cpu i_worker.
! In mode 0, no parallel coordination.
! In mode 1, cpus are synced up prior to creating the file 
!  and status of file creation is shared with all cpus
!
! Written February 2002 by Charles C Burch
!----------------------------------------------------------
  subroutine pp_create_new_file(file_name, i_worker, ext_size,  &
   num_exts, zero_sw, mode, i_err)
    character(len=*), intent(in) :: file_name
    integer,          intent(in) :: i_worker
    integer,          intent(in) :: ext_size
    integer,          intent(in) :: num_exts
    integer,          intent(in) :: zero_sw 
    integer,          intent(in) :: mode    
    integer,          intent(out):: i_err   
  
    integer                      :: ifile, zero=0
 
    if(mode.lt.0 .or. mode.gt.1) then
      write(pcps_message,*) &
        "Warning: Invalid mode(",mode,") in pp_create_new_file"
      call pcps_print(pcps_message,2)
    endif

    if(mode.eq.1) call pcpsx_sync_workers(i_err)   !sync up workers
    i_err=0
    if(pcps_current_worker_num.ne.i_worker) goto 999

    if(cio_set_file_ext_size(ext_size).ne.CIO_OK) goto 995 

    if(zero_sw.ne.0) then
      ifile=cio_fopen(file_name, 'w',  &
       file_space_commit=PREALLOCATE_FILE_SPACE_ENABLED, &
       file_lock=FILE_LOCK_ENABLED)
    else
      ifile=cio_fopen(file_name, 'w')
    endif
    if(ifile.le.0) goto 995

    i_err=cio_fseek(ifile,ext_size,num_exts-1,ext_size-1,0)
    if(i_err.ne.CIO_OK) goto 990

    i_err=cio_fwrite(zero,1,1,ifile)
    if(i_err.ne.1) goto 990

    i_err=cio_fclose(ifile)
    goto 999

990 i_err=cio_fclose(ifile)
995 i_err=-1

999 if(mode.eq.1) call pcpsx_check_worker_errors(i_err)
    return
  end subroutine pp_create_new_file

!----------------------------------------------------------
! if worker i_worker open file with file_name
!  returns handle i_file
!  file_stat controls how the file is opened
!  return i_err=0 if successful or -1 if error
!  mode is for possible parallel expansion-needs to zero for now
!
! Written December 2000 by Charles C Burch
!----------------------------------------------------------
  subroutine pp_open_form(i_file, i_worker, file_name, file_stat, i_err, mode)
    integer, intent(out)          :: i_file, i_err
    integer, intent(in)           :: i_worker, mode
    character (len=*), intent(in) :: file_name, file_stat

    character a_file_stat*8
    character a_form*16
    character a_access*16

    i_err=0
    if(mode.ne.0) then
      write(pcps_message,*) "Warning:mode(",mode,") non-zero in pp_open_form"
      call pcps_print(pcps_message,2)
    endif

    if(i_worker.ne.pcps_current_worker_num) then
      i_file=0
      return
    endif

!  initialize the error flag to 0,  no error,  (-1 = error)
      i_err = 0

!     a_form = 'formatted'
      a_access = 'sequential'
      a_file_stat = file_stat

! get the unit number
      call getlun(i_file, i_err)
      if (i_err .ne. 0) goto 998

!  try a new open
      call string_to_upper(a_file_stat)
      open(i_file, file=file_name, status=a_file_stat, form=a_form, &
      access=a_access, err=1)

!  suceessful open
      goto 2

!  try an old open
    1  continue
      if (a_file_stat(1:3) .eq. 'NEW') then

        a_file_stat = 'OLD'
        open(i_file, file=file_name, status=a_file_stat, form=a_form, &
        access=a_access, err=997)

       end if    ! if (a_file_stat(1:3) .eq. 'NEW') then

!  come to here for sucessful open_form
    2 continue

 1999 continue
      return

  997 continue
      write(pcps_message,*) "Error in pp_open_form during open_form"
      call pcps_print(pcps_message,2)

  998 continue
      write(pcps_message,*) "Error in pp_open_form during getlun"
      call pcps_print(pcps_message,2)

  999 continue
      write(pcps_message,*)          &
       "Error in pp_open_form, file_name = ", trim(file_name), &
       ", file_stat = ", trim(file_stat), ", i_file = ",i_file
      call pcps_print(pcps_message,2)
      i_err = -1
      goto 1999

    end subroutine pp_open_form

!----------------------------------------------------------
! if worker i_worker open file with file_name
!  returns handle i_file
!  file_stat controls how the file is opened
!  return i_err=0 if successful or -1 if error
!!
!  mode =0 :no coordination is done with workers opening the file other than
!           only i_worker opens the file with the call though
!       =1 :parallel coordination done--if file is to be created,
!           i_worker firsts creates the file.
!           Then each worker one at a time opens the file
!       =2 :Parallel coordination but only i_worker opens the file
!           This mode is for primarily reads where
!           i_worker reads and broadcast to others
!       =3 :Parallel coordination but only i_worker opens the file
!           This mode is for primarily reads where
!           i_worker reads and sends to boss
!
!  file_space_commit is optional and can be
!                    PREALLOCATE_FILE_SPACE_ENABLED  preallocate,
!                    PREALLOCATE_FILE_SPACE_DISABLED don't preallocate
!                    PREALLOCATE_FILE_SPACE_DEFAULT  preallocate cpstemp/data
!  file_lock         is optional and can be
!                    FILE_LOCK_DISABLED then do not lock file,
!                    FILE_LOCK_ENABLED lock as needed
!  file_auto_delete is optional and can be
!                    AUTO_DELETE_DISABLED then do not auto delete file,
!                    AUTO_DELETE_ON_EXIT auto delete when program exits
!                    AUTO_DELETE_ON_CLOSE auto delete when file is closed
!  file_extent_size is optional and specifies pfio file extent size
!
! Written December 2000 by Charles C Burch
!----------------------------------------------------------
  subroutine pp_open_file(i_file, i_worker, file_name, file_stat, i_err, mode,&
   file_space_commit, file_lock,  file_auto_delete, file_extent_size)
    integer, intent(out)          :: i_file, i_err
    integer, intent(in)           :: i_worker, mode
    character (len=*), intent(in) :: file_name, file_stat
    integer,optional              :: file_space_commit
    integer,optional              :: file_lock
    integer,optional              :: file_auto_delete
    integer,optional              :: file_extent_size

    character (len=2)             :: worker_file_stat
    integer                       :: j_worker, j_err
    logical                       :: opened_by_i_worker, opened_by_this_cpu

    if(pp_stat_flag) pp_t0=getsys_seconds()
    
    i_err=0
    opened_by_i_worker=.false.

    if(mode.lt.0 .or. mode.gt.3) then
      write(pcps_message,*) "Warning: Invalid mode(",mode,") in pp_open_file"
      call pcps_print(pcps_message,2)
    endif

! - determine if this current cpu actually opens the file or not
    opened_by_this_cpu=(i_worker.eq.pcps_current_worker_num).or.(mode.eq.1)

    if(opened_by_this_cpu) then

! --- Now do optionally specified parameters on file to be opened

      if(present(file_extent_size)) then
        if(cio_set_file_ext_size(file_extent_size).ne.CIO_OK) then
          i_err=-1
          if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
          return
        endif
      endif

      if(present(file_space_commit)) then
        call cio_set_file_space_commit(file_space_commit)
      endif

      if(present(file_lock)) then
        if(cio_set_file_lock_control(file_lock).ne.CIO_OK) then
          call cio_set_file_space_commit_c(-2) !restore default
          if(present(file_extent_size)) &
            i_err=cio_set_file_ext_size(cio_extsize())
          i_err=-1
          if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
          return
        endif
      endif

      if(present(file_auto_delete)) then
        if(cio_set_file_auto_delete(file_auto_delete).ne.CIO_OK) then
          call cio_set_file_space_commit_c(-2)      !restore default
          call cio_set_file_lock_control_c(-2)      !restore default
          if(present(file_extent_size)) &
            i_err=cio_set_file_ext_size(cio_extsize())
          i_err=-1
          if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
          return
        endif
      endif

    endif

    if(mode.le.0 .or. mode.gt.3) then

! --- no parallel coordination

      if(i_worker.ne.pcps_current_worker_num) then
        i_file=0
        if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
        return
      endif

      i_file = cio_fopen( file_name, file_stat)
      if (i_file .le. 0) then
        write(pcps_message,*) "Error in pp_open, file=",trim(file_name), &
          ", cpu#=",pcps_current_worker_num
        call pcps_print(pcps_message,2)
        i_err=-1
      endif

    else if(mode.eq.1) then

! --- parallel coordination: i_worker creates file if needed
!     then all cpus open the file one at a time-
!     each wait until all cpus have opened the file

      worker_file_stat=file_stat
      if(worker_file_stat.eq."w") then
        worker_file_stat="r+"
        if(pcps_current_worker_num.eq.i_worker) then
          i_file = cio_fopen( file_name, file_stat)
          opened_by_i_worker=.true.
        endif
      endif

      call pcpsx_sync_workers(j_err)
      do j_worker=0,pcps_num_procs-1
        if(pcps_current_worker_num.eq.j_worker) then

          if(j_worker.eq.i_worker .and. opened_by_i_worker) then
            continue
          else
            i_file = cio_fopen( file_name, worker_file_stat)
          endif

          if (i_file .le. 0) then
            write(pcps_message,*) "Error in pp_open, file=",trim(file_name), &
             ", cpu#=",pcps_current_worker_num
            call pcps_print(pcps_message,2)
            i_err=-1
          endif

        endif
        call pcpsx_sync_workers(j_err)
      enddo
      call pcpsx_check_worker_errors(i_err)

    else if(mode.eq.2.or.mode.eq.3) then

      if(i_worker.eq.pcps_current_worker_num) then
        i_file = cio_fopen( file_name, file_stat)
        if (i_file .le. 0) then
          write(pcps_message,*) "Error in pp_open, file=",trim(file_name), &
            ", cpu#=",pcps_current_worker_num
          call pcps_print(pcps_message,2)
          i_err=-1
        endif
      else
        i_file=0
      endif
      call pcpsx_check_worker_errors(i_err)

    endif

!   write(pcps_message,           &
!    '( '' ppopen'',           &
!    &  '' file_name ='', a,   &
!    &  '' file_stat ='', a,   &
!    &  '' i_file    ='', i12, &
!    &  '' n_data    ='', i12, &
!    &  '' i_err     ='', i2   &
!    & )')                       &
!     trim(file_name), trim(file_stat), i_file, n_data, i_err
!     call pcps_print(pcps_message,2)

    if(i_err.ne.0) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
    end subroutine pp_open_file

!----------------------------------------------------------
! if worker i_worker close file with handle i_file
!  l_remove controls if file is deleted when it is closed
!  return i_err=0 if successful or -1 if error
!  mode=0 is for non-coordinated parallel operation-each pe does its own work
!  mode=1 is for coordinated parallel closing of the same file by all cpus
!          close i_worker cpu last
!          i_err is shared with all cpus and the remove is done only on i_worker
!  mode=2/3  only close file on i_worker, share the status
!
! Written December 2000 by Charles C Burch
!----------------------------------------------------------
    subroutine pp_close_file(i_file, i_worker, l_remove, i_err, mode)
      integer, intent(in)           :: i_file, i_worker, mode
      logical, intent(in)           :: l_remove
      integer, intent(out)          :: i_err

      character (len=260)           :: file_name
      integer                       :: j_worker, j_err

! - print *,"Enter pp_close,cpu, i_worker=",pcps_current_worker_num, i_worker

    if(pp_stat_flag) pp_t0=getsys_seconds()
    i_err=0
    if(mode.lt.0 .or. mode.gt.3) then
      write(pcps_message,*) "Warning: Invalid mode(",mode,") in pp_close_file"
      call pcps_print(pcps_message,2)
    endif

    if(mode.le.0 .or. mode.gt.2) then

! --- no parallel coordination

      if(i_worker.eq.pcps_current_worker_num) then
        j_err = cio_fclose(i_file,l_remove)
        if (j_err .ne. CIO_OK) then
          write(pcps_message,*) "Error in pp_close, i_file =",i_file, &
           ", cpu#=",pcps_current_worker_num
          call pcps_print(pcps_message,2)
          i_err = -1
        endif
      endif

    else if(mode.eq.1) then

! --- parallel coordination--
!     first each cpu other than i_worker closes file with no-delete
!     i_worker closes the file last and removes it if requested
!     all cpus wait until they are all done
!     if the file is kept, i_worker will change file time stamp

      call pcpsx_sync_workers(j_err)

!  all worker except i_worker close the file with no-remove

      do j_worker=0,pcps_num_procs-1
        if(pcps_current_worker_num.eq.j_worker) then
          if(j_worker.ne.i_worker) then
            i_err = cio_fclose(i_file,.false.)
          endif
        endif
        call pcpsx_sync_workers(j_err)
      enddo

! --- i_worker now closes the file and removes file if requested or will
!     modify time stamp to attempt to force cpus to flush any NFS cache
!     information

      if(i_worker.eq.pcps_current_worker_num) then

        if(.not.l_remove) then
          j_err=cio_finquire(i_file,file_name)
          i_err = cio_fclose(i_file,l_remove)
          call cio_update_file_time(trim(file_name))
        else
          i_err = cio_fclose(i_file,l_remove)
        endif
      endif

! --- check error status

      if (i_err .ne. CIO_OK) then
        write(pcps_message,*) "Error in pp_close, i_file =",i_file, &
         ", cpu#=",pcps_current_worker_num
        call pcps_print(pcps_message,2)
        i_err = -1
      else
        i_err=0
      endif

      call pcpsx_check_worker_errors(i_err)

    else if(mode.eq.2.or.mode.eq.3) then

      if(i_worker.eq.pcps_current_worker_num) then
        j_err = cio_fclose(i_file,l_remove)
        if (j_err .ne. CIO_OK) then
          write(pcps_message,*) "Error in pp_close, i_file =",i_file, &
           ", cpu#=",pcps_current_worker_num
          call pcps_print(pcps_message,2)
          i_err = -1
        endif
      endif

      call pcpsx_check_worker_errors(i_err)
    endif

    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
    end subroutine pp_close_file

!----------------------------------------------------
! pp_zero_file:
! if worker, iworker write n_rec of n_data reals to
!  file with handle i_file
!  i_err is zero if sucessful and -1 if error

! mode 0-each worker does it own things (default)
!   only i_worker zeroes the file
! mode 1-only i_worker zeroes the file--the other wait
!   the error from the zeroing is shared with workers
!   the file time stamp is updated to clear nfs cache
! mode 2/3-only i_worker zeroes the file--the others wait
!   the error status is shared withother workers
!
! Written May 2001 by Charles C Burch
!-----------------------------------------------------
    subroutine pp_zero_file(i_file, i_worker, n_rec, n_data, i_err,mode)
    integer, intent(in)          :: i_worker, i_file, n_rec, n_data
    integer, intent(out)         :: i_err
    integer, intent(in),optional :: mode

    integer             ::                      mode1  
    real                :: x_data


    if(present(mode)) then
      mode1=mode
    else
      mode1=0
    endif

    if(mode1.lt.0.or.mode1.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode1,") in pp_zero_file"
      call pcps_print(pcps_message, 2)
      mode1=0
    endif

    call pp_zero_byte_file(i_file, i_worker, n_rec, n_data*sizeof(x_data), &
        i_err,mode1)
    return
  end subroutine pp_zero_file

!---------------------------------------------------
! pp_zero_byte_file
! if worker i_worker, write n_rec of n_data zero bytes to
!  file with handle i_file
!  i_err is zero if sucessful and -1 if error
! mode 0-each worker does it own things (default)
!   only i_worker zeroes the file
! mode 1-only i_worker zeroes the file--the other wait
!   the error from the zeroing is shared with workers
!   the file time stamp is updated to clear nfs cache
! mode 2/3-only i_worker zeroes the file--the others wait
!   the error status is shared withother workers
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
    subroutine pp_zero_byte_file(i_file, i_worker, n_rec, n_data, i_err, &
    mode)
    integer, intent(in)          :: i_worker, i_file, n_rec, n_data
    integer, intent(out)         :: i_err
    integer, intent(in),optional :: mode

    integer             :: i_rec              , mode1  
    character           :: x_data(n_data)        ! automatic array
    character (len=260) :: file_name

    logical          :: error

    if(present(mode)) then
      mode1=mode
    else
      mode1=0
    endif

    error=.false.
    if(mode1.lt.0.or.mode1.gt.3) then
      write(pcps_message,*) &
       "Warning, Invalid mode(",mode1,") in pp_zero_byte_file"
      call pcps_print(pcps_message, 2)
      mode1=0
    endif

!  initialize the error flag to 0,  no error,  (-1 = error)
    i_err = 0
    if(i_worker.eq.pcps_current_worker_num) then

      x_data = char(0)
      i_err = cio_fseek(i_file, 0, n_data, 0)
      if(i_err.ne.CIO_OK) then
        write(pcps_message, '( &
         &  '' error with cio_seek in pp_zero_byte_file'', &
         &  '' i_file ='', i12, &
         &  '' n_data ='', i12, &
         &  '' i_err  ='', i12)')&
         i_file, n_data, i_err
        call pcps_print(pcps_message,2)
        error=.true.
        goto 900
      endif
      
!  write zeros to each record
      do i_rec = 1,  n_rec
! ---   print'(" zero i=",i12," n=",i12," j=",i12)',i_file,n_data,i_rec
        i_err = cio_fwrite_char_c (x_data(1), n_data, i_file)
        if (i_err .ne. n_data) then
          write(pcps_message, &
           '('' error with cio_fwrite in pp_zero_byte_file'', &
           & '' i_file ='', i12, &
           & '' n_data ='', i12, &
           & '' n_rec  ='', i12, &
           & '' i_rec  ='', i12, &
           & '' i_err  ='', i12 )') &
           i_file, n_data, n_rec, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif
      end do 

      if(mode1.eq.1 .and. pcps_num_procs>1) then
        i_err=cio_finquire(i_file,file_name)
        if(i_err.ne.CIO_OK) then
          error=.true.
        endif
        i_err = cio_fflush(i_file)
        call cio_update_file_time(file_name)
      endif
    endif

900 if(mode1.gt.0) call pcpsx_check_worker_errors(error)

    i_err = 0
    if(error) i_err=-1
    return
  end subroutine pp_zero_byte_file

!---------------------------------------------------
! if worker i_worker reads from file with handle i_file
!  n_read records with traces (hdrs/trcs) starting at
!  record i_first with a stride of n_stride records between reads.
!  hdrs and trcs contain the traces to be read with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!  returns i_err: is zero if sucessful and -1 if error
! mode 0-each worker does it own things (default)
!   only i_worker zeroes the file
! mode 1-only i_worker zeroes the file--the other wait
!   the error from the zeroing is shared with workers
!   the file time stamp is updated to clear nfs cache
! mode 2/3-only i_worker zeroes the file--the others wait
!   the error status is shared withother workers
!
! Written December 2000 by Charles C Burch
! New Version October 2001 by Charles C Burch
!---------------------------------------------------
  subroutine pp_zero_trace_file(i_file, i_worker, n_rec, &
   hd_len, tr_len, i_err, mode)
    integer, intent(in)           :: i_file, i_worker, hd_len, tr_len, n_rec
    integer, intent(out)          :: i_err
    integer, intent(in), optional :: mode
    
    double precision              :: hd(1:hd_len)
    real                          :: tr(1:tr_len)
    character,allocatable         :: zeroes(:)
    integer                       :: lrecl, mode1, i_rec, n_max, i
    logical                       :: error
    character (len=260)           :: file_name

    if(present(mode)) then
      mode1=mode
    else
      mode1=0
    endif

    if(mode1.lt.0.or.mode1.gt.3) then
      write(pcps_message,*) &
       "Warning, Invalid mode(",mode1,") in pp_zero_byte_file"
      call pcps_print(pcps_message, 2)
      mode1=0
    endif

    error=.true.
    lrecl=tr_len*sizeof(tr(1))+hd_len*sizeof(hd(1))
    n_max=max(1,65534/lrecl)
    allocate(zeroes(1:lrecl*n_max),stat=i_err)
    if(i_err.ne.0) goto 900
    
    if(i_worker.eq.pcps_current_worker_num) then

! --- write zero hd/tr-read it back as bytes in case zero represented funny

      hd=0.
      tr=0.
      call pp_write_trace_file_1(i_file, i_worker, &
       1, hd_len, tr_len, hd, tr, i_err, 0)
      if(i_err.ne.0) goto 900
      call pp_read_file_c1(i_file, i_worker, 1, 1, 1, &
       lrecl, zeroes, i_err, 0)
      if(i_err.ne.0) goto 900

      do i=2,n_max
         zeroes((i-1)*lrecl+1:i*lrecl)=zeroes(1:lrecl)
      enddo


!  write zeros to rest of records
      i_rec=2
      do while(i_rec.le.n_rec)
! ---   print'(" zero i=",i12," n=",i12," j=",i12)',i_file,n_data,i_rec
         i=min(n_rec-i_rec+1,n_max)
        i_err = cio_fwrite_char_c (zeroes(1), lrecl*i, i_file)
        if (i_err .ne. lrecl*i) then
          write(pcps_message, &
           '('' error with cio_fwrite in pp_zero_byte_file'', &
           & '' i_file ='', i12, &
           & '' n_data ='', i12, &
           & '' n_rec  ='', i12, &
           & '' i_rec  ='', i12, &
           & '' i_err  ='', i12 )') &
           i_file, lrecl*i, n_rec, i_rec, i_err
          call pcps_print(pcps_message,2)
          goto 900
        endif

        i_rec=i_rec+i
      end do 
      
      if(mode1.eq.1 .and. pcps_num_procs>1) then
        i_err=cio_finquire(i_file,file_name)
        if(i_err.ne.CIO_OK) then
          error=.true.
          goto 900
        endif

        i_err = cio_fflush(i_file)
        call cio_update_file_time(file_name)
      endif
    endif
    error=.false.

900 deallocate(zeroes,stat=i_err)
    if(mode1.gt.0) call pcpsx_check_worker_errors(error)

    i_err = 0
    if(error) i_err=-1
    return
  end subroutine pp_zero_trace_file

!---------------------------------------------------
! if worker i_worker reads from file with handle i_file
!  n_read records with traces (hdrs/trcs) starting at
!  record i_first with a stride of n_stride records between reads.
!  hdrs and trcs contain the traces to be read with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!  return i_err: is zero if sucessful and -1 if error
!  mode same as with pp_read_file
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_trace_file_1(i_file, i_worker, &
   i_first,  hd_len, tr_len, hdrs, trcs, i_err, mode)
    integer, intent(in)             :: i_file, i_worker, mode
    integer, intent(in)             :: i_first
    integer, intent(in)             :: hd_len, tr_len
    integer, intent(out)            :: i_err
    real, intent(inout)             :: trcs(:)
    double precision, intent(inout) :: hdrs(:)

    integer  :: hd_recl, tr_recl           
    logical  :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error=.false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)  &
        "Warning: Invalid mode(",mode,") in pp_read_trace_file"
      call pcps_print(pcps_message,2)
    endif

    if(size(hdrs).lt.hd_len .or. size(trcs).lt.tr_len) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_trace_file"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    hd_recl=hd_len*sizeof(hdrs(1))
    tr_recl=tr_len*sizeof(trcs(1))
    if(pcps_current_worker_num.eq.i_worker) then
        i_err = cio_fseek(i_file, 0, hd_recl+tr_recl, i_first)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_seek in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_first, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

        i_err = cio_fread(hdrs(1), hd_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_read-hdrs in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_first, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

        i_err = cio_fread(trcs(1), tr_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_read-trcs in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_first, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          return
        endif
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call ppio_broadcast_d1(i_worker,hd_len,hdrs)
          call ppio_broadcast_r1(i_worker,tr_len,trcs)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call ppio_receive_data_d1(i_worker,hd_len,hdrs,1)
              call ppio_receive_data_r1(i_worker,tr_len,trcs,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call ppio_send_data_d1(0,hd_len,hdrs,1)
              call ppio_send_data_r1(0,tr_len,trcs,1)
            endif
          endif
        endif
      endif
    endif

    i_err = 0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_trace_file_1

!---------------------------------------------------
! if worker i_worker reads from file with handle i_file
!  n_read records with traces (hdrs/trcs) starting at
!  record i_first with a stride of n_stride records between reads.
!  hdrs and trcs contain the traces to be read with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!  return i_err: is zero if sucessful and -1 if error
!  mode same as with pp_read_file
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_trace_file_2(i_file, i_worker, &
   n_read, i_first, n_stride, hd_len, tr_len, hdrs, trcs, i_err, mode)
    integer, intent(in)             :: i_file, i_worker, mode
    integer, intent(in)             :: n_read, i_first, n_stride
    integer, intent(in)             :: hd_len, tr_len
    integer, intent(out)            :: i_err
    real, intent(inout)             :: trcs(:,:)
    double precision, intent(inout) :: hdrs(:,:)

    integer  :: hd_recl, tr_recl, i_read, i_rec
    logical  :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error=.false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)  &
        "Warning: Invalid mode(",mode,") in pp_read_trace_file"
      call pcps_print(pcps_message,2)
    endif

    if(size(hdrs).lt.hd_len*n_read .or. size(trcs).lt.tr_len*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_trace_file"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    hd_recl=hd_len*sizeof(hdrs(1,1))
    tr_recl=tr_len*sizeof(trcs(1,1))
    if(pcps_current_worker_num.eq.i_worker) then
      do i_read = 1 ,  n_read
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, hd_recl+tr_recl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_seek in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_read ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_read, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

        i_err = cio_fread(hdrs(1,i_read), hd_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_read-hdrs in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_read ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_read, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

        i_err = cio_fread(trcs(1,i_read), tr_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_read-trcs in pp_read_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_read ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
            i_file, i_read, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          return
        endif
      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call ppio_broadcast_d2(i_worker,n_read,hdrs)
          call ppio_broadcast_r2(i_worker,n_read,trcs)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call ppio_receive_data_d2(i_worker,n_read,hdrs,1)
              call ppio_receive_data_r2(i_worker,n_read,trcs,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call ppio_send_data_d2(0,n_read,hdrs,1)
              call ppio_send_data_r2(0,n_read,trcs,1)
            endif
          endif
        endif
      endif
    endif

    i_err = 0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_trace_file_2

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_write records with traces (hdrstrcs) starting at
!  record i_first with a stride of n_stride records between writes.
!  hdrs and trcs contain the traces to be written with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!  i_err is zero if sucessful and -1 if error
!  mode same as with pp_write_file

! Written December 2000 by Charles C Burch
!---------------------------------------------------
 subroutine pp_write_trace_file_1(i_file, i_worker, &
  i_first, hd_len, tr_len, hdrs, trcs, i_err, mode)
    integer, intent(in)           :: i_file, i_worker, mode
    integer, intent(in)           :: i_first
    integer, intent(in)           :: hd_len, tr_len
    integer, intent(out)          :: i_err
    real, intent(in)              :: trcs(:)
    double precision, intent(in)  :: hdrs(:)

    integer  :: hd_recl, tr_recl
    logical  :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) &
       "Warning: Invalid mode(",mode,") in pp_write_trace_file"
      call pcps_print(pcps_message,2)
    endif

    if(size(hdrs).lt.hd_len.or. size(trcs).lt.tr_len) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_trace_file"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then
      hd_recl=hd_len*sizeof(hdrs(1))
      tr_recl=tr_len*sizeof(trcs(1))
      i_err = cio_fseek(i_file, 0, hd_recl+tr_recl, i_first)
      if (i_err .ne. CIO_OK) then
        write(pcps_message, '( &
         &  '' error with cio_seek in pp_write_trace_file'', &
         &  '' i_file ='', i12, &
         &  '' i_rec  ='', i12, &
         &  '' i_err  ='', i12)')&
         i_file, i_first, i_err
        call pcps_print(pcps_message,2)
        error=.true.
        goto 900
      endif
      i_err = cio_fwrite(hdrs(1), hd_recl, 1, i_file)
      if (i_err .ne. 1) then
        write(pcps_message, '( &
         &  '' error with cio_write-hdrs in pp_write_trace_file'', &
         &  '' i_file ='', i12, &
         &  '' i_rec  ='', i12, &
         &  '' i_err  ='', i12)')&
         i_file, i_first, i_err
        call pcps_print(pcps_message,2)
        error = .true.
        goto 900
      endif

      i_err = cio_fwrite(trcs(1), tr_recl, 1, i_file)
      if (i_err .ne. 1) then
        write(pcps_message, '( &
         &  '' error with cio_write-trcs in pp_write_trace_file'', &
         &  '' i_file ='', i12, &
         &  '' i_rec  ='', i12, &
         &  '' i_err  ='', i12)')&
         i_file, i_first, i_err
        call pcps_print(pcps_message,2)
        error = .true.
        goto 900
      endif
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err = 0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_trace_file_1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_write records with traces (hdrstrcs) starting at
!  record i_first with a stride of n_stride records between writes.
!  hdrs and trcs contain the traces to be written with
!  hd_len indicating length of hdrs and tr_len the length
!  of trcs
!  i_err is zero if sucessful and -1 if error
!  mode same as with pp_write_file

! Written December 2000 by Charles C Burch
!---------------------------------------------------
 subroutine pp_write_trace_file_2(i_file, i_worker, &
   n_write, i_first, n_stride, hd_len, tr_len, hdrs, trcs, i_err, mode)
    integer, intent(in)           :: i_file, i_worker, mode
    integer, intent(in)           :: n_write, i_first, n_stride
    integer, intent(in)           :: hd_len, tr_len
    integer, intent(out)          :: i_err
    real, intent(in)              :: trcs(:,:)
    double precision, intent(in)  :: hdrs(:,:)

    integer  :: hd_recl, tr_recl, i_write, i_rec
    logical  :: error


    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) &
       "Warning: Invalid mode(",mode,") in pp_write_trace_file"
      call pcps_print(pcps_message,2)
    endif

    if(size(hdrs).lt.hd_len*n_write .or. size(trcs).lt.tr_len*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_trace_file"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then
      hd_recl=hd_len*sizeof(hdrs(1,1))
      tr_recl=tr_len*sizeof(trcs(1,1))
      do i_write = 1 ,  n_write
        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, hd_recl+tr_recl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_seek in pp_write_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_write ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
           i_file, i_write, i_rec, i_err
          call pcps_print(pcps_message,2)
          error=.true.
          goto 900
        endif

        i_err = cio_fwrite(hdrs(1,i_write), hd_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_write-hdrs in pp_write_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_write='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
           i_file, i_write, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

        i_err = cio_fwrite(trcs(1,i_write), tr_recl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_write-trcs in pp_write_trace_file'', &
           &  '' i_file ='', i12, &
           &  '' i_write ='', i12, &
           &  '' i_rec  ='', i12, &
           &  '' i_err  ='', i12)')&
           i_file, i_write, i_rec, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif
      end do    ! do i_write = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err = 0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_trace_file_2

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written October 2001 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_c1(i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer,   intent(in)            :: i_worker,i_file,n_read,i_first,n_stride
    integer,   intent(in)            :: n_data, mode
    character, intent(inout)         :: x_data(:)
    integer,   intent(out)           :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_c1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_c1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_c1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread_char_c(x_data(1+(i_read-1)*n_data), lrecl, i_file)
        if (i_err .ne. lrecl) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_c1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call ppio_broadcast_c1(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call ppio_receive_data_c1(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call ppio_send_data_c1(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_c1
  
!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_r1( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    real   , intent(inout)        :: x_data(:)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_r1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_r1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_r1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1+(i_read-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_r1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_r1

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_r2( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    real   , intent(inout)        :: x_data(:, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_r2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_r2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_r2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_r2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_r2

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_r3( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    real   , intent(out)          :: x_data(:, :, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_r3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_r3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek

        i_rec = i_first + (i_read - 1) * n_stride
!       print *,"pp_read", i_read, i_rec, lrecl, i_file
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_read_file_r3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          call pcps_print(pcps_message,2)
          error=.true.
          goto 900
        endif

! --- now read the record

        i_err = cio_fread (x_data(1, 1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fread in pp_read_file_r3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )') &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          call pcps_print(pcps_message,2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_r3

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_i1( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    integer, intent(inout)        :: x_data(:)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_i1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_i1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_i1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1+(i_read-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_i1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_i1

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker doe sthe read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_i2( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    integer   , intent(inout)     :: x_data(:, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_i2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_i2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_i2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_i2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_i2

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_i3( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    integer, intent(out)          :: x_data(:, :, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_i3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_i3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek

        i_rec = i_first + (i_read - 1) * n_stride
!       print *,"pp_read", i_read, i_rec, lrecl, i_file
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_read_file_i3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          error=.true.
          call pcps_print(pcps_message, 2)
          goto 900
        endif

! --- now read the record

        i_err = cio_fread (x_data(1, 1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fread in pp_read_file_i3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )') &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          error=.true.
          call pcps_print(pcps_message, 2)
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_i3

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_d1( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    double precision, intent(inout) :: x_data(:)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_d1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_d1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_d1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1+(i_read-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_d1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_d1

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker doe sthe read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_d2( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    double precision, intent(inout) :: x_data(:, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_d2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_d2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_d2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_d2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_d2

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_d3( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    double precision, intent(out) :: x_data(:, :, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_d3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_d3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek

        i_rec = i_first + (i_read - 1) * n_stride
!       print *,"pp_read", i_read, i_rec, lrecl, i_file
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_read_file_d3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          call pcps_print(pcps_message,2)
          error=.true.
          goto 900
        endif

! --- now read the record

        i_err = cio_fread (x_data(1, 1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fread in pp_read_file_d3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )') &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          error=.true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_d3

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_z1( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    complex, intent(inout)        :: x_data(:)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_z1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_z1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_z1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1+(i_read-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_z1, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl,  i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_z1

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker doe sthe read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_z2( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    complex, intent(inout)        :: x_data(:, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_z2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_z2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips
      lrecl = n_data*sizeof(x_data(1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek
        i_rec = i_first + (i_read - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           &  '' error with cio_fseek in pp_read_file_z2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )' ) &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

! --- now read the record
        i_err = cio_fread (x_data(1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           &  '' error with cio_fread in pp_read_file_z2, worker='', i8, &
           &  '' i_file  ='', i12, &
           &  '' i_first ='', i12, &
           &  '' n_stride='', i12, &
           &  '' n_read  ='', i12, &
           &  '' i_read  ='', i12, &
           &  '' i_rec   ='', i12, &
           &  '' n_data  ='', i12, &
           &  '' lrecl   ='', i12, &
           &  '' i_err   ='', i12 )') &
            i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
            n_data, lrecl, i_err
          call pcps_print(pcps_message, 2)
          error=.true.
          goto 900
        endif

      end do    ! do i_read = 1 ,  n_read
    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_z2

!---------------------------------------------------
! if worker i_worker, reads from file with handle i_file
!  n_read records with x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode 0 only i_worker does the read-no parallel coordination
!  mode 1 only i_worker does requested read-workers are synced after the read
!  mode 2 i_worker does the read but broadcasts the results to other cpus
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_read_file_z3( i_file, i_worker, n_read, i_first, n_stride, &
   n_data, x_data, i_err, mode)
    integer, intent(in)           :: i_worker, i_file, n_read, i_first, n_stride
    integer, intent(in)           :: n_data, mode
    complex, intent(out)          :: x_data(:, :, :)
    integer, intent(out)          :: i_err

    integer   :: i_rec, i_read, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*) "Warning, Invalid mode(",mode,") in pp_read_file_z3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_read) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_read_file_z3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_read read, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_read = 1 ,  n_read

! --- calculate record # and do a seek

        i_rec = i_first + (i_read - 1) * n_stride
!       print *,"pp_read", i_read, i_rec, lrecl, i_file
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_read_file_z3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12,  &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
          error=.true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now read the record

        i_err = cio_fread (x_data(1, 1, i_read), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fread in pp_read_file_z3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_read  ='', i12, &
           & '' i_read  ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' lrecl   ='', i12, &
           & '' i_err   ='', i12 )') &
           i_worker, i_file, i_first, n_stride, n_read, i_read, i_rec, &
           n_data, lrecl, i_err
           error=.true.
          call pcps_print(pcps_message,2)
           goto 900
        endif

      end do    ! do i_read = 1 ,  n_read

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
      if(.not.error) then
        if(mode.eq.2) then
          call pcpsx_broadcast(i_worker,n_read, x_data)
        else if(mode.eq.3) then
          if(i_worker.ne.0) then
            if(pcps_current_worker_num.eq.0) then
              call pcpsx_receive_data(i_worker,n_read,x_data,1)
            else if(pcps_current_worker_num.eq.i_worker) then
              call pcpsx_send_data(0,n_read,x_data,1)
            endif
          endif
        endif
      endif
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_read_file_z3

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written October 2001 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_c1( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer,   intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer,   intent(in)        :: n_data, mode
    character, intent(in)        :: x_data(:)
    integer,   intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_c1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_c1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_r1, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite_char_c(x_data(1+(i_write-1)*n_data), lrecl, i_file)
        if (i_err .ne. lrecl) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_c1, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_c1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_r1( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    real   , intent(in)        :: x_data(:)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_r1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_r1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_r1, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1+(i_write-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_r1, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_r1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_r2( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    real   , intent(in)        :: x_data(:, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_r2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_r2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_r2, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_r2, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_r2

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between writes.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_r3( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    real   , intent(in)        :: x_data(:, :, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_r3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_r3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_r3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, 1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_r3, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif
      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_r3

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_i1( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    integer, intent(in)        :: x_data(:)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_i1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_i1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_i1, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1+(i_write-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_i1, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_i1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_i2( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    integer, intent(in)        :: x_data(:, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_i2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_i2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_i2, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_i2, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_i2

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between writes.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the write
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_i3( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    integer, intent(in)        :: x_data(:, :, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_i3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_i3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_i3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, 1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_i3, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif
      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_i3

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_d1( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    double precision, intent(in) :: x_data(:)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_d1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_d1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_d1, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1+(i_write-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_d1, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_d1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between writes
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the write
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_d2( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)          :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)          :: n_data, mode
    double precision, intent(in) :: x_data(:, :)
    integer, intent(out)         :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_d2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_d2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_d2, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_d2, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_d2

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between writes.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the write
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_d3( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)          :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)          :: n_data, mode
    double precision, intent(in) :: x_data(:, :, :)
    integer, intent(out)         :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_d3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_d3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_d3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, 1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_d3, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif
      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_d3

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the read
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_z1( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    complex, intent(in)        :: x_data(:)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_z1"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_z1"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_z1, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1+(i_write-1)*n_data), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_z1, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_z1

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between reads.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the write
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_z2( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    complex, intent(in)        :: x_data(:, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_z2"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_z2"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_z2, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_z2, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif

      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_z2

!---------------------------------------------------
! if worker i_worker writes to file with handle i_file
!  n_read records from x_data starting at
!  record i_first with a stride of n_stride records between writes.
!  returns i_err: is zero if sucessful and -1 if error
!  mode=0 : only i_worker writes the file
!  mode=1 : only i_worker writes the file, workers are synced up after the write
!  mode=2 : currently same as mode=1 but reads are done differently
!
! Written December 2000 by Charles C Burch
!---------------------------------------------------
  subroutine pp_write_file_z3( i_file, i_worker, n_write, i_first, n_stride, &
   n_data, x_data, i_err,  mode)
    integer, intent(in)        :: i_worker, i_file, n_write, i_first, n_stride
    integer, intent(in)        :: n_data, mode
    complex, intent(in)        :: x_data(:, :, :)
    integer, intent(out)       :: i_err

    integer   :: i_rec, i_write, lrecl
    logical   :: error

    if(pp_stat_flag) pp_t0=getsys_seconds()
    error = .false.
    if(mode.lt.0.or.mode.gt.3) then
      write(pcps_message,*)"Warning, Invalid mode(",mode,") in pp_write_file_z3"
      call pcps_print(pcps_message, 2)
    endif

    if(size(x_data).lt.n_data*n_write) then
      write(pcps_message,*) &
       "Error: Apparent array out of bounds in pp_write_file_z3"
      call pcps_print(pcps_message, 2)
      error=.true.
      goto 900
    endif

    if(i_worker.eq.pcps_current_worker_num) then

! --- perform the n_write write, skips

      lrecl = n_data*sizeof(x_data(1,1,1))
      do i_write = 1 ,  n_write

! --- calculate record # and do a seek

        i_rec = i_first + (i_write - 1) * n_stride
        i_err = cio_fseek(i_file, 0, lrecl, i_rec)
        if (i_err .ne. CIO_OK) then
          write(pcps_message, '( &
           & '' error with cio_fseek in pp_write_file_z3, worker='', i8, &
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           &  '' i_err   ='', i12 )' )&
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          call pcps_print(pcps_message,2)
          error = .true.
          goto 900
        endif

! --- now write the record

        i_err = cio_fwrite (x_data(1, 1, i_write), lrecl, 1, i_file)
        if (i_err .ne. 1) then
          write(pcps_message, '( &
           & '' error with cio_fwrite in pp_write_file_z3, worker='', i8,&
           & '' i_file  ='', i12, &
           & '' i_first ='', i12, &
           & '' n_stride='', i12, &
           & '' n_write ='', i12, &
           & '' i_write ='', i12, &
           & '' i_rec   ='', i12, &
           & '' n_data  ='', i12, &
           & '' i_err   ='', i12 )' ) &
           i_worker, i_file, i_first, n_stride, n_write, i_write, i_rec, &
           n_data, i_err
          error = .true.
          call pcps_print(pcps_message,2)
          goto 900
        endif
      end do

    endif

900 if(mode.gt.0) then
      call pcpsx_check_worker_errors(error)
    endif

    i_err=0
    if(error) i_err=-1
    if(pp_stat_flag) call cpucount(pp_counts,pp_elapse, pp_t0)
    return
  end subroutine pp_write_file_z3

!---------------------------------------------------
! set file to use direct I/O
!
! Written October 2005 by Brian Macy
!---------------------------------------------------
  subroutine pp_set_direct_io(i_file, i_err)
    integer, intent(in)           :: i_file
    integer, intent(out)          :: i_err

    i_err = cio_set_direct_io(i_file)

    return
  end subroutine pp_set_direct_io

end module pp_module

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
