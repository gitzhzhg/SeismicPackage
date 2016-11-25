!<CPS_v1 type="PRIMITIVE"/>
! see also trciof77.f90 and trcio.f90
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
! Name       : trciof77wrapper
! Category   : io
! Written    : 2000-07-20  by: Michael L. Sherrill
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : F77 style wrapper that C functions can call to get trcio 
!              file globals and data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! This F77 primitive subroutine is called by C language in the SPWS
! trace file I/O library. It enables C to access F90 I/O structure 
! members via F77 in order to get file global information and trcio trot data
!-------------------------------------------------------------------------------
!</descript_doc>
!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
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
!                              i      o     o   o     o      o      
! trciof77wrapper_get_globals(ifil, wtype, dt, ndpt, nwih, nbits,
!                                o        o            o              o
!                             nbits_hd, tmin, data_start_position, xorigin, 
!                                o       o     o     o     o      o
!                             yorigin, dx11, dx12, dx21, dx22, num_traces,  
!                                o     o    o
!                             trmaxg, lun, stat)
!
! character        :: ifile               ! File to be opened      
! character        :: wtype               ! Word type WIBM etc.    
! real             :: dt                  ! Trace sample rate      
! integer          :: ndpt                ! Number of samples      
! integer          :: nwih                ! Number of header words 
! integer          :: nbits               ! Bits per sample        
! integer          :: nbits_hd            ! Bits in a header word  
! real             :: tmin                ! File starting time     
! integer          :: data_start_position ! Header offset to data  
! double precision :: xorigin             ! Globals x origin       
! double precision :: yorigin             ! Globals y origin       
! double precision :: dx11,dx12,dx21,dx22 ! Globals coordinates    
! integer          :: num_traces          ! Number of file traces  
! double precision :: trmaxg              ! Max value in file
! integer          :: lun                 ! Logical unit number    
! integer          :: stat                ! Returned status        
!-------------------------------------------------------------------------------
!
!                                      i        i                o        o
! trciof77wrapper_get_keyword(ifil, find_keyword, returned_keyword_value, stat)
!
! character        :: ifile                  ! File to be opened      
! integer          :: find_keyword           ! Keyword being searched for
! character        :: returned_keyword_value ! Value of keyword found
! integer          :: stat                   ! Successful or not
!
!-------------------------------------------------------------------------------
!                             i        i       o   o   i     i    o
! trciof77wrapper_get_trace(ifile, open_file, hd, tr, tnum, lun, stat,
!                             i        i
!                           hdsize, trsize)
!
! character :: ifile     ! File to be opened      
! integer   :: open_file ! Flag to open file (must be done once only)      
! double    :: hd        ! Array for headers      
! float     :: tr        ! Array for trace values 
! integer   :: tnum      ! Trace to read          
! integer   :: lun       ! Logical unit number    
! integer   :: stat      ! Status of file read    
! integer   :: hdsize    ! Size of header array   
! integer   :: trsize    ! Size of trace array    
!-------------------------------------------------------------------------------
!
!                             i    o
! trciof77wrapper_close_file(lun, stat)
! integer   :: lun       ! Logical unit number    
! integer   :: stat      ! Returned status
!-------------------------------------------------------------------------------
!                              i      i       i      i     i
! trciof77wrapper_create_file(ifil, imode, scratch, nwih, ndpt, 
!                               i              i          i   i 
!                             nbits_trace, nbits_header, dt, tmin,
!                               i     i      o    o  
!                              tmax, trmaxg, lun, stat )
!
!  integer          :: ifil(128)   ! File to be opened
!  integer          :: imode(2)    ! Mode of open (See trcio.f90)
!  integer          :: scratch     ! Is a temporary file or not
!  integer          :: nwih        ! Number words in header
!  integer          :: ndpt        ! Number of samples
!  integer          :: nbits_trace ! Number bits in trace values 
!  integer          :: nbits_header! Number bits in header values 
!  real             :: dt          ! Sample rate
!  real             :: tmin        ! Time start of data
!  real             :: tmax        ! Time end of data
!  real             :: trmaxg      ! Lav of data
!  integer          :: lun         ! Logical unit of file
!  integer          :: stat        ! Status of operation
!-------------------------------------------------------------------------------
!                              i   i    i   i        i      o
! trciof77wrapper_write_trace(lun, hd, tr, hdsize, trsize, stat)
!
! integer          :: lun          ! Logical unit associated with file
! integer          :: hdsize       ! Size of header array
! integer          :: trsize       ! Size of trace array
! double precision :: hd(hdsize)   ! Header array
! real             :: tr(trsize)   ! Trace array
! integer          :: stat         ! Status of operation
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! 1. trciof77wrapper_get_globals and trciof77wrapper_get_keyword
!    will open and close the file.
! 2. trciof77wrapper_get_trace must open the file by setting the open_file 
!    > 0 on the first read. Subsequent reads should not reopen the file. 
! 3. trciof77wrapper_close_file should be called after all traces have
!    been read in with trciof77wrapper_get_trace.
! 4. All of these subroutines gain access to the trcio_struct via the lun.
! 5. Currently windowing the traces and patterned trace reading is done
!    in the methods that call these subroutines, but it could be added
!    here if needed at a later date.
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!     Date        Author               Description
!     ----        ------               -----------
!  7. 2006-06-20 B. Menger             Removed Unused Variables.
!  6. 2005-09-12 Goodger               Added arg calledFrom to create routine.
!  5. 2005-03-08 Goodger               Add routine set_ext_size.  Needed by
!                                      promax WriteTrceFile program.
!  4. 2001-05-02 Michael L. Sherrill   Fixed header word doc.
!  3. 2001-02-01 Michael L. Sherrill   Added capability to create a file
!                                      and capability to get specified
!                                      keyword values.
!  2. 2000-09-06 Michael L. Sherrill   Added capability to get max value in 
!                                      the file (trmaxg)
!  1. 2000-07-20 Michael L. Sherrill   Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
! None to date
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!-------------------------------------------------------------------------------
!</programming_doc>
module trciof77wrapper_module
  implicit none
  character(len=100),public,save :: XXXX_IDENT = &
'$Id: trciof77wrapper.f90,v 1.7 2006/06/20 13:12:11 Menger prod sps $'
end module trciof77wrapper_module

!!-------------------------- start of subroutines -------------------------!!
!!-------------------------- start of subroutines -------------------------!!
!!-------------------------- start of subroutines -------------------------!!

subroutine trciof77wrapper_get_globals(ifil, wtype, dt, ndpt, nwih, nbits,  &
                             nbits_hd, tmin, data_start_position, xorigin,  &
                             yorigin, dx11, dx12, dx21, dx22, num_traces,   &
                             trmaxg, lun, stat)
  use trciof77_module
  use string_module
  implicit none
  integer          :: ifil(128)
  character*10     :: wtype
  real             :: dt
  integer          :: ndpt, nwih, nbits, nbits_hd 
  real             :: tmin
  integer          :: data_start_position
  double precision :: xorigin, yorigin, dx11, dx12, dx21, dx22
  double precision :: trmaxg
  integer          :: num_traces, lun, stat
  character*512    :: filename
  integer          :: close_stat


    ! Convert C strings
    call string_hh2cc(ifil, filename)

    stat = trciof77_globals(filename, wtype, dt, ndpt, nwih, nbits, nbits_hd, &
                            tmin, data_start_position, xorigin, yorigin, dx11,&
                            dx12, dx21, dx22, num_traces, trmaxg, lun)


    ! Close the file before returning
    if(stat.eq.0) then
      close_stat = trciof77_close(lun)
      stat = close_stat
    endif
    
    return
 
end



subroutine trciof77wrapper_get_keyword(ifil, find_keyword, &
                                       returned_keyword_value, stat)
  use trciof77_module
  use string_module
  implicit none
  integer          :: ifil(128)
  integer          :: find_keyword(16)
  character*512    :: returned_keyword_value
  integer          :: stat

  ! Local
  character*512    :: filename
  character*64     :: keyword_searched
  integer          :: close_stat
  integer          :: lun


    ! Convert C strings
    call string_hh2cc(ifil, filename)
    call string_hh2cc(find_keyword, keyword_searched)


    stat = trciof77_get_keyword(filename, keyword_searched, &
                                returned_keyword_value, lun   )

    

    ! Close the file before returning
    close_stat = trciof77_close(lun)
    
    return 
 
end 




subroutine trciof77wrapper_get_trace(ifil, open_file, hd, tr, tnum, lun, &
                                     stat, hdsize, trsize)
  use trciof77_module
  use string_module
  implicit none
  integer          :: ifil(30)
  integer          :: open_file
  integer          :: hdsize, trsize
  double precision :: hd(hdsize)
  real             :: tr(trsize)
  integer          :: tnum, lun
  integer          :: stat
  character*120    :: filename

    ! Open the file if requested (should be done on the first trace only)
    if(open_file .ne. 0) then
      call string_hh2cc(ifil, filename)
      stat = trciof77_open(filename, lun)
      if(stat .ne. 0) then
        return
      endif
    endif 

    stat = trciof77_read_trace(lun,hd,tr,tnum)
 
    return
 
end

subroutine trciof77wrapper_set_ext_size(extsize)

  use cio_module
  integer :: extsize,istat

  istat=cio_set_file_ext_size(extsize)


end



subroutine trciof77wrapper_close_file(lun, close_stat)
  use trciof77_module
  implicit none
  integer :: lun
  integer :: close_stat

    close_stat = trciof77_close(lun)

    return
 
end


subroutine trciof77wrapper_create_file(ifil, imode, scratch, nwih, ndpt,    &
                                       nbits_trace, nbits_header, dt, tmin, &
                                       tmax, trmaxg, lun, stat,calledFrom_arg )
  use trciof77_module
  use string_module
  implicit none
  integer,optional :: calledFrom_arg(2)
  integer          :: ifil(128)
  integer          :: imode(2)
  integer          :: scratch, nwih, ndpt, nbits_trace, nbits_header 
  real             :: dt
  real             :: tmin
  real             :: tmax
  real             :: trmaxg
  integer          :: lun, stat

  ! Local
  character*512    :: filename
  character*8      :: mode,calledFrom



    if(present(calledFrom_arg))then
      call string_hh2cc(calledFrom_arg,calledFrom)
    else
      calledFrom=' '
    endif

    ! Convert C strings
    call string_hh2cc(ifil, filename)
    call string_hh2cc(imode, mode)

    stat = trciof77_create_file(filename, mode, scratch, nwih, ndpt, &
                                nbits_trace, nbits_header, dt, tmin, &
                                tmax, trmaxg, lun,calledFrom          )

    return
 
end



subroutine trciof77wrapper_write_trace(lun, hd, tr, hdsize, trsize, stat)
  use trciof77_module
  use string_module
  implicit none
  integer          :: lun
  integer          :: hdsize, trsize
  double precision :: hd(hdsize)
  real             :: tr(trsize)
  integer          :: stat

  ! Local
  integer          :: close_stat


    stat = trciof77_write_trace(lun, hd, tr)

    if(stat .ne. 0) then
      close_stat = trciof77_close(lun)
      stat = close_stat
      return
    endif


    return 
 
end
