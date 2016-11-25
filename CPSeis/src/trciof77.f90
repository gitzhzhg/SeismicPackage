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
! see also trcio.f90

!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E        
!
! Name       : trciof77
! Category   : io
! Written    : 2000-07-11   by: Bill Menger
! Revised    : 2008-08-05   by: Bill Menger
! Maturity   : beta
! Purpose    : INTERNAL Trace file I/O wrapper around trcio module for f77.
!              Use SeismicIO module instead of this one. (SeismicIO will call
!              this for you.)
! Portability: Fortran 9X still doesn't pass INTEGER(KIND=8) correctly.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! These primimitive functions are f77 callable in order to get trace data
! and file global information.  This module is called by SeismicIO functions
! and SeismicIO functions are intended to be used by others.  Please do not
! write your code to call these functions, but use SeismicIO instead.
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
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
! ----------------> DEPRECATED -- see trciof77_create <-----------------
!                   i       i     o                    
! trciof77_open (filename, lun, stat)
!
! character :: filename            ! File to be opened 
! integer   :: lun                 ! Logical unit number
! integer   :: stat                ! Returned result of open
!-------------------------------------------------------------------------------
! ------------>DEPRECATED see globals_by_unit below. <----------------
! WHY? Because this function opens the file AND returns global keyword information,
! serving two purposes, causing difficulty in code maintenance.  Instead, use :
! SeismicIO_open followed by SeismicIO_get_globals.
!                      i       o     o    o    o      o      o     
! trciof77_globals(filename, wtype, dt, ndpt, nwih, nbits, nbits_hd, 
!                    o            o              o        o       o   
!                  tmin, data_start_position, xorigin, yorigin, dx11,
!                    o     o     o       o        o      o
!                  dx12, dx21, dx22, num_traces, trmaxg, lun)
!
! character        :: filename            ! File to be opened    
! character        :: wtype               ! Word type WIBM etc.  
! real             :: dt                  ! Trace sample rate    
! integer          :: ndpt                ! Number of samples    
! integer          :: nwih                ! Number of header words 
! integer          :: nbits               ! Bits per sample        
! integer          :: nbits_hd            ! Bits in a header word  
! real             :: tmin                ! File starting time     
! integer          :: data_start_position ! Header offset to data  
! real             :: xorigin             ! Globals x origin       
! real             :: yorigin             ! Globals y origin       
! double precision :: dx11,dx12,dx21,dx22 ! Globals coordinates   
! integer          :: num_traces          ! Number of file traces 
! real             :: trmaxg              ! Max val in file
! integer          :: lun                 ! Logical unit number   
! integer          :: stat                ! Returned status       
!-------------------------------------------------------------------------------
!                                   i     o      o   o     o     o      o     
! status = trciof77_globals_by_unit(unit, wtype, dt, ndpt, nwih, nbits, nbits_hd, 
!                    o            o              o        o       o   
!                  tmin, data_start_position, xorigin, yorigin, dx11,
!                    o     o     o       o        o   
!                  dx12, dx21, dx22, num_traces, trmaxg)
!
! integer          :: unit                ! Logical unit number   
! character        :: wtype               ! Word type WIBM etc.  
! real             :: dt                  ! Trace sample rate    
! integer          :: ndpt                ! Number of samples    
! integer          :: nwih                ! Number of header words 
! integer          :: nbits               ! Bits per sample        
! integer          :: nbits_hd            ! Bits in a header word  
! real             :: tmin                ! File starting time     
! integer          :: data_start_position ! Header offset to data  
! real             :: xorigin             ! Globals x origin       
! real             :: yorigin             ! Globals y origin       
! double precision :: dx11,dx12,dx21,dx22 ! Globals coordinates   
! integer          :: num_traces          ! Number of file traces 
! real             :: trmaxg              ! Max val in file
! integer          :: status              ! Returned status       
!-------------------------------------------------------------------------------
! --------------------> DEPRECATED see keyword_by_unit below <-------------
! WHY? Because this function opens the file AND returns global keyword information,
! serving two purposes, causing difficulty in code maintenance.  Instead, use :
! SeismicIO_open followed by SeismicIO_get_keyword.
!                         i              i               o          o
! trciof77_get_keyword(filename, desired_keyword,  returned_value, lun) 
!
! character        :: filename            ! File to be opened    
! character        :: desired_keyword     ! Look for this keyword
! character        :: returned_value      ! Value of keyword if found
! integer          :: lun                 ! Logical unit number
!-------------------------------------------------------------------------------
!                                       i     i       o             
! status = trciof77_get_keyword_by_unit(unit, keyword,value) 
!
! integer,intent(in)                  :: unit     ! unit number of already opened file
! character(len=*),intent(in)         :: keyword  ! Look for this keyword
! character(len=*),intent(out)        :: value    ! Value of keyword if found
! integer                             :: status   ! return status from lookup.    
!-------------------------------------------------------------------------------
!                  i    o
! trciof77_close( lun, stat)
!
! integer   :: lun                 ! Logical unit 
! integer   :: stat                ! Returned status
!-------------------------------------------------------------------------------
!                      i   o    o   i      o
! trciof77_read_trace(lun, hd, tr, tnum, status)
!
! integer   :: lun                 ! Logical unit     
! double    :: hd                  ! Cps header array 
! real      :: tr                  ! Trace data array  
! integer   :: tnum                ! Trace number to get
! integer   :: status              ! Returned status    
!-------------------------------------------------------------------------------
!                       i   o    o   i      o
! trciof77_write_trace(lun, hd, tr, tnum, status)
! integer   :: lun                 ! Logical unit           (input)
! double    :: hd                  ! Cps header array       (output)
! real      :: tr                  ! Trace data array       (output)
! integer   :: tnum                ! Trace number to get    (input)
! integer   :: status              ! Returned status        (output)
!-------------------------------------------------------------------------------
! ---------------< REPLACES trciof77_open if mode = r or r+) >--------------
! DON'T USE THIS!  Instead, use SeismicIO_open.  It will call this function.
! To write these values to the file's header, call SeismicIO_WriteHeader after
! opening the file with SeismicIO_open.
!                      i         i       i      i     i
!trciof77_create_file(filename, mode, scratch, nwih, ndpt, 
!                      i              i          i   i 
!                    nbits_trace, nbits_header, dt, tmin,
!                      i     i      o    o  
!                     tmax, trmaxg, lun, stat )
!
!  character        :: filename    ! File to be opened and created
!  character        :: mode        ! Mode of open (See trcio.f90)
!  integer          :: scratch     ! Is a temporary file or not
!  integer          :: nwih        ! Number words in header
!  integer          :: ndpt        ! Number of samples
!  integer          :: nbits_trace ! Number bits in trace values 
!  integer          :: nbits_header! Number bits in header values 
!  real             :: dt          ! Sample reate
!  real             :: tmin        ! Time start of data
!  real             :: tmax        ! Time end of data
!  real             :: trmaxg      ! Lav of data
!  integer          :: lun         ! Logical unit of file
!  integer          :: stat        ! Status of operation
!-------------------------------------------------------------------------------
!o                             i          i    i    i           i 
!status = trciof77_writeheader(UnitNumber,nwih,ndpt,nbits_trace,nbits_hd,&
!                              i  i    i       i       i    i    i    i
!                              dt,tmin,xorigin,yorigin,dx11,dx12,dx21,dx22&
!                              i     i
!                              ftype,history)
!
!    integer          ,intent(in)  :: UnitNumber
!    integer          ,intent(in)  :: nwih,ndpt,nbits_trace,nbits_hd
!    real             ,intent(in)  :: dt,tmin
!    double precision ,intent(in)  :: xorigin,yorigin,dx11,dx12,dx21,dx22
!    character(len=*) ,intent(in)  :: ftype   (LBO TRCIO LBO2 SU SEGY...)
!    character(len=*) ,intent(in)  :: history (ALL BRIEF NONE CURRENT)
!    integer                       :: status
!
!-------------------------------------------------------------------------------
!   o                                      i                i
! status = function trciof77_seek_trace(UnitNumber,SequentialTraceNumber)
!   integer,intent(in)                      :: UnitNumber
!   integer,intent(in)                      :: SequentialTraceNumber
!   integer                                 :: status
! Positions the file at the start of trace "SequentialTraceNumber".
!
!-------------------------------------------------------------------------------
!   o                                                    i
! SequentialTraceNumber = function trciof77_tell_trace(UnitNumber)
!   integer,intent(in)                      :: UnitNumber
!   integer                                 :: SequentialTraceNumber
! Tells you what trace number the file is positioned at.
! -1 indicates that the file is not positioned at the start of a trace.
!
!-------------------------------------------------------------------------------
!
!
!   o                        i         i       i
!  status = trciof77_seek(UnitNumber,offset, whence)
!    integer,intent(in)                      :: UnitNumber
!    integer,intent(in),dimension(:)         :: offset(2)
!    integer,intent(in)                      :: whence
!    integer                                 :: status
!  see man pages for fseek and you will know how to call this.
!
!-------------------------------------------------------------------------------
!
!   o                       i
!  offset = trciof77_tell(UnitNumber)
!    integer,intent(in)                      :: UnitNumber
!    integer,           dimension(:)         :: offset(2)
!
!  see man pages for ftell and you will know how to call this.
!
!
!-------------------------------------------------------------------------------
!  o                                        i
! extent_size = trciof77_get_extentsize(UnitNumber)
!   integer,intent(in)                      :: UnitNumber
!   integer                                 :: extent_size ! in bytes
!
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author                 Description
!     ----        ------                 -----------
!  9. 2008-08-05  Bill Menger            Added get_keyword_by_unit,globals_by_unit,
!                                        seek, tell, seek_trace, tell_trace.
!  8. 2005-09-12  Goodger                Added arg calledFrom.
!  7. 2002-04-18  Michael L. Sherrill    The trciof77_close subroutine
!                                        was nullifying a pointer to the
!                                        file instead of the file itself.
!                                        Accommodate changes in trcio_struct.
!  6. 2001-06-18  Michael L. Sherrill    Added code to create_file in order
!                                        to create 8 bit trcio files.
!                                        PRODUCTION.
!  5. 2001-03-21  Michael L. Sherrill    Fixed header word doc.
!  4. 2001-02-01  Michael L. Sherrill    Added capability to create a file
!                                        and ability to get global keywords
!  3. 2000-09-06  Michael L. Sherrill    Added ability to get the max value
!                                        of the file (trmaxg)
!  2. 2000-07-20  Michael L. Sherrill    Added calling parameters needed
!                                        by SPWS I/O and other work to
!                                        complete original template
!  1. 2000-07-11  Bill Menger            Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module trciof77_module
  use trcio_module
  use cpsio_module
  use cio_module
  implicit none

  character(len=100),save :: trciof77_ident = &
  '$Id: trciof77.f90,v 1.47 2008/08/05 15:12:04 mengewm Exp $'
 
  type :: trcio_struct_holder
    type(trcio_struct),pointer :: file
  end type

  type(trcio_struct_holder)       :: hidfil(110:200)
  type(trcio_struct),pointer      :: file

  contains 

  function trciof77_open(filename, lun) result(stat)
    character(len=*) ,intent(in)  :: filename
    integer          ,intent(out) :: lun
    !----------- result  from function ------------------ 
    integer                       :: stat

    stat = 0

    file => trcio_open(filename,'r')
    if(.not. associated(file) ) then
      stat = trcio_error
      return
    endif
    if(file%lun < 110 .or. file%lun > 200) then
      stat = trcio_error
      return
    endif

    hidfil(file%lun)%file => file
    lun                   = file%lun

  end function trciof77_open


  function trciof77_globals(filename, wtype, dt, ndpt, nwih, nbits, nbits_hd, &
                            tmin, data_start_position, xorigin, yorigin, dx11,&
                            dx12, dx21, dx22, num_traces, trmaxg, lun)        &
                            result(stat)
    character(len=*) ,intent(in)  :: filename
    character(len=*) ,intent(out) :: wtype
    real             ,intent(out) :: dt
    integer          ,intent(out) :: ndpt
    integer          ,intent(out) :: nwih
    integer          ,intent(out) :: nbits
    integer          ,intent(out) :: nbits_hd
    real             ,intent(out) :: tmin
    integer          ,intent(out) :: data_start_position
    double precision ,intent(out) :: xorigin
    double precision ,intent(out) :: yorigin
    double precision ,intent(out) :: dx11
    double precision ,intent(out) :: dx12
    double precision ,intent(out) :: dx21
    double precision ,intent(out) :: dx22
    integer          ,intent(out) :: num_traces
    double precision ,intent(out) :: trmaxg
    integer          ,intent(out) :: lun
    !----------- results from function ------------------ 
    integer                       :: stat
    !----------- local variables -------------------

    file => trcio_open(filename,'r')
    if(.not. associated(file) ) then
      stat = trcio_error
      return
    endif
    if(file%lun < 110 .or. file%lun > 200) then
      stat = trcio_error
      return
    endif
 

    hidfil(file%lun)%file => file 
    wtype                 = file%wtype
    dt                    = file%dt
    ndpt                  = file%num_values
    nwih                  = file%nwih 
    nbits                 = file%nbits 
    nbits_hd              = file%nbits_hd
    tmin                  = file%tmin
    data_start_position   = file%data_start_pos(2)
    xorigin               = file%common%xorigin
    yorigin               = file%common%yorigin
    dx11                  = file%common%dx11
    dx12                  = file%common%dx12
    dx21                  = file%common%dx21
    dx22                  = file%common%dx22
    num_traces            = file%num_traces
    trmaxg                = file%trmaxg
    lun                   = file%lun
    stat                  = trcio_ok

  end function trciof77_globals



  function trciof77_globals_by_unit(unit, wtype, dt, ndpt, nwih, nbits, nbits_hd, &
                            tmin, data_start_position, xorigin, yorigin, dx11,&
                            dx12, dx21, dx22, num_traces, trmaxg) result(status)
    integer          ,intent(in ) :: unit
    character(len=*) ,intent(out) :: wtype
    real             ,intent(out) :: dt
    integer          ,intent(out) :: ndpt
    integer          ,intent(out) :: nwih
    integer          ,intent(out) :: nbits
    integer          ,intent(out) :: nbits_hd
    real             ,intent(out) :: tmin
    integer          ,intent(out) :: data_start_position
    double precision ,intent(out) :: xorigin
    double precision ,intent(out) :: yorigin
    double precision ,intent(out) :: dx11
    double precision ,intent(out) :: dx12
    double precision ,intent(out) :: dx21
    double precision ,intent(out) :: dx22
    integer          ,intent(out) :: num_traces
    double precision ,intent(out) :: trmaxg
    !----------- results from function ------------------ 
    integer                       :: status
    !----------- local variables -------------------

    wtype                 = hidfil(unit)%file%wtype
    dt                    = hidfil(unit)%file%dt
    ndpt                  = hidfil(unit)%file%num_values
    nwih                  = hidfil(unit)%file%nwih 
    nbits                 = hidfil(unit)%file%nbits 
    nbits_hd              = hidfil(unit)%file%nbits_hd
    tmin                  = hidfil(unit)%file%tmin
    data_start_position   = hidfil(unit)%file%data_start_pos(2)
    xorigin               = hidfil(unit)%file%common%xorigin
    yorigin               = hidfil(unit)%file%common%yorigin
    dx11                  = hidfil(unit)%file%common%dx11
    dx12                  = hidfil(unit)%file%common%dx12
    dx21                  = hidfil(unit)%file%common%dx21
    dx22                  = hidfil(unit)%file%common%dx22
    num_traces            = hidfil(unit)%file%num_traces
    trmaxg                = hidfil(unit)%file%trmaxg
    status                = trcio_ok

  end function trciof77_globals_by_unit





  function trciof77_get_keyword(filename, desired_keyword, &
                                returned_value, lun) result(stat)
    character(len=*) ,intent(in)  :: filename
    character(len=*) ,intent(in)  :: desired_keyword
    character(len=*) ,intent(out) :: returned_value
    integer          ,intent(out) :: lun

    !----------- results from function ------------------ 
    integer                       :: stat




    file => trcio_open(filename,'r')
    if(.not. associated(file) ) then
      stat = trcio_error
      return
    endif
    if(file%lun < 110 .or. file%lun > 200) then
      stat = trcio_error
      return
    endif
 

    hidfil(file%lun)%file => file 
    lun                   = file%lun


    if( cpsio_number_sections(file%lun) >= 1) then 
      stat = cpsio_get_keyword(file%lun, desired_keyword, returned_value, &
                               section_name='jobglobals')
    else
      stat = trcio_error
    endif


  end function trciof77_get_keyword

  function trciof77_get_keyword_by_unit(unit,keyword,value) result(status)
    integer          ,intent(in)  :: unit
    character(len=*) ,intent(in)  :: keyword
    character(len=*) ,intent(out) :: value
    integer                       :: status

    if( cpsio_number_sections(unit) >= 1) then 
      status = cpsio_get_keyword(unit, keyword, value,section_name='jobglobals')
      if(status /= 0 ) then
        status = cpsio_get_keyword(unit, keyword, value,section_name='seismic')
        if(status /= 0) then
          status = cpsio_get_keyword(unit, keyword, value,section_name='history')
        endif
      endif
    else
      status = trcio_error
    endif
  end function trciof77_get_keyword_by_unit


  function trciof77_close (lun) result (status)
    integer,intent(in) :: lun
    integer            :: status

    file => hidfil(lun)%file

    if(.not. associated(file) ) then
      status = 1;
      return
    endif

    status = trcio_close(file)
    nullify(hidfil(lun)%file)
  end function trciof77_close





  integer function trciof77_read_trace(lun,hd,tr,tnum) result (status)
    integer,intent(in)                         :: lun
    double precision ,intent(out),dimension(:) :: hd
    real             ,intent(out),dimension(:) :: tr
    integer, optional,intent(in)               :: tnum

    file => hidfil(lun)%file

    if(present(tnum) ) then
      status = trcio_read_trace(file,hd,tr,tnum)
    else
      status = trcio_read_trace(file,hd,tr)
    endif

  end function trciof77_read_trace
  





  integer function trciof77_write_trace(lun,hd,tr,tnum) result (status)
    integer,intent(in)                         :: lun
    double precision ,intent(in ),dimension(:) :: hd
    real             ,intent(in ),dimension(:) :: tr
    integer, optional,intent(in)               :: tnum

    file => hidfil(lun)%file

    if(present(tnum) ) then
      status = trcio_write_trace(file,hd,tr,tnum)
    else
      status = trcio_write_trace(file,hd,tr)
    endif

  end function trciof77_write_trace
  

function trciof77_create_file(filename, mode, scratch, nwih, ndpt,   &
                              nbits_trace, nbits_hd, dt, tmin, tmax, &
                              trmaxg, lun ,calledFrom_arg) result(stat)
    character(len=*) ,intent(in)  :: filename
    character(len=*) ,intent(in)  :: mode
    integer          ,intent(in)  :: scratch
    integer          ,intent(in)  :: nwih
    integer          ,intent(in)  :: ndpt
    integer          ,intent(in)  :: nbits_trace
    integer          ,intent(in)  :: nbits_hd
    real             ,intent(in)  :: dt
    real             ,intent(in)  :: tmin
    real             ,intent(in)  :: tmax
    real             ,intent(in)  :: trmaxg
    integer          ,intent(out) :: lun
    character(len=*) ,intent(in),optional  :: calledFrom_arg
    !----------- results from function ------------------ 
    integer                       :: stat
    !----------- local variables -------------------
    logical                       :: temp_file
    character(len=8)              :: calledFrom


    if(mode(1:1) == 'r') then 
      stat = trciof77_open(filename,lun)
      return
    endif


    if(present(calledFrom_arg))then
      calledFrom=calledFrom_arg
    else
      calledFrom=' '
    endif

    if(scratch .ne. 0) then
      temp_file = .true.
    else
      temp_file = .false.
    endif
    file => trcio_open(filename, mode, temp_file, nwih, ndpt, &
                       nbits_trace, nbits_hd ,calledFrom=calledFrom  )

    if(.not. associated(file) ) then
      stat = trcio_error
      return
    endif
    if(file%lun < 110 .or. file%lun > 200) then
      stat = trcio_error
      return
    endif
 
    file%dt               = dt
    file%tmin             = tmin
    file%tmax             = tmax
    file%trmaxg           = trmaxg
    hidfil(file%lun)%file => file 
    lun                   = file%lun


    !--The next line is temporary until we add wtype and a file type
    !--to the calling parameters. The following creates an eight bit file.
    !--We will need the file type parameter if and when we want to
    !--write out a segy type file.
    if(nbits_trace .lt. 32) file%wtype = 'INT'

    stat = trcio_update_header(file)
    stat = trcio_writeheader(file)

  end function trciof77_create_file


function trciof77_writeheader(UnitNumber,nwih,ndpt,nbits_trace,nbits_hd,     &
                              dt,tmin,xorigin,yorigin,dx11,dx12,dx21,dx22,   &
                              ftype,history) result (status)

    integer          ,intent(in)  :: UnitNumber
    integer          ,intent(in)  :: nwih,ndpt,nbits_trace,nbits_hd
    real             ,intent(in)  :: dt,tmin
    double precision ,intent(in)  :: xorigin,yorigin,dx11,dx12,dx21,dx22
    character(len=*) ,intent(in)  :: ftype
    character(len=*) ,intent(in)  :: history
    !----------- results from function ------------------ 
    integer                       :: status
    !----------- local to the function ------------------ 

    file => hidfil(UnitNumber)%file

    file%dt                      = dt
    file%tmin                    = tmin
    file%tmax                    = tmin + (ndpt-1)* dt
    file%nwih                    = nwih
    file%num_values              = ndpt
    file%nbits                   = nbits_trace
    file%nbits_hd                = nbits_hd
    file%common%xorigin          = xorigin
    file%common%yorigin          = yorigin
    file%common%dx11             = dx11
    file%common%dx12             = dx12
    file%common%dx21             = dx21
    file%common%dx22             = dx22
    file%ftype                   = trim(ftype(:min(5,len(ftype))))
    file%common%history          = trim(history(:min(8,len(history))))
    if (file%ftype == 'LBO')  file%lbo_version = 1
    if (file%ftype == 'LBO2') file%lbo_version = 2

    if(nbits_trace < 32) file%wtype = 'INT'
    status = trcio_update_header(file)
    call cio_frewind(file%lun)
    status = trcio_writeheader(file)
    

  end function trciof77_writeheader

  function trciof77_seek_trace(UnitNumber,SequentialTraceNumber) result (status)
    integer,intent(in)                      :: UnitNumber
    integer,intent(in)                      :: SequentialTraceNumber
    integer                                 :: status

    file => hidfil(UnitNumber)%file
    status = trcio_seek_trace(file,SequentialTraceNumber)

  end function trciof77_seek_trace

  function trciof77_tell_trace(UnitNumber) result (SequentialTraceNumber)
    integer,intent(in)                      :: UnitNumber
    integer                                 :: SequentialTraceNumber

    file => hidfil(UnitNumber)%file
    SequentialTraceNumber = trcio_tell_trace(file)

  end function trciof77_tell_trace

  function trciof77_seek(UnitNumber,offset, whence) result (status)
    integer,intent(in)                      :: UnitNumber
    integer,intent(in)                      :: offset(2)
    integer,intent(in)                      :: whence
    integer                                 :: status

    file => hidfil(UnitNumber)%file
    status = cio_fseek(file%lun,offset,whence)

  end function trciof77_seek

  function trciof77_tell(UnitNumber,offset) result (status)
    integer,intent(in)                      :: UnitNumber
    integer,intent(out)                     :: offset(2)
    integer                                 :: status

    file                      => hidfil(UnitNumber)%file
    status                    =  cio_ftell(file%lun,offset)

  end function trciof77_tell

  function trciof77_get_extentsize(UnitNumber) result (extent_size)
   integer,intent(in)                      :: UnitNumber
   integer                                 :: extent_size ! in bytes


   file => hidfil(UnitNumber)%file
   extent_size = cio_get_file_ext_size(file%lun)

  end function trciof77_get_extentsize

end module trciof77_module
