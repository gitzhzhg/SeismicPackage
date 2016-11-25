!<CPS_v1 type="PROCESS"/>

!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2009 Fusiongeo LLC
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
!                         C P S   P R O C E S S
!
! Name       : WSEP    (Write SEP file)
! Category   : io
! Written    : 2009-10-12   by: Bill Menger
! Revised    : 2009-10-12   by: Bill Menger
! Maturity   : beta
! Purpose    : Write seismic traces to SEP file
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!This process will allow traces to pass through the flow untouched, but will 
!write all traces that come past it into an SEP formatted file.  The file name
!you provide should end in .H and will represent the ascii text file for the
!SEP data.  An associated file of the same name but with an @ added to the end
!will be created with your binary data.  Example:  filename = test.H will create
!two files, test.H and test.H@.  The file test.H is editable and contains your
!metadata about the dataset.  The data is always written in native_float format.
!By default, the first dimension is set to the sample rate.  If the sample rate
!is less than 1.0 the label is set to Time, if greater, it is set to Depth.  You
!can override any values in the ascii header file (test.H above).
!
!This process will not sort your data and will not fill in zeroed traces for 
!any missing traces in one dimension.  SEP format requires that your data be
!regular in all dimensions, so it is up to you to fill in missing traces in a
!previous process within this flow AND to sort your data in the order in which
!the SEP header is told that the data will be loaded.  No checks exist to 
!guarantee that you follow these rules.

!You are limited to 15 dimensions, look for parameter MSD to increase this limit.
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! Input traces pass through the process unchanged.
! File(s) are written in the format specified.
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     grid transformation structure         used but not changed
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
! Hwd#    Description                Action taken
! ----    -----------                ------------
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2010-07-19  Bill Menger  Fixed file close to truncate to exact size needed.
!  2. 2010-07-09  Bill Menger  Increased to 15 dimensions and fixed bug that
!                              allowed bfio to split the file into chunks across
!                              servers, like other cpseis files.
!  1. 2009-10-12  Bill Menger  Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    Upon receiving NTR=NO_MORE_TRACES, the output binary file is closed.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
! File Format is SEP.  Fortran I/O will not work for the binary portion, so the
! cio_fread/write/open/close routines are used.  The maximum extent size is set
! prior to opening with cio_fopen so that the file will not fragment across a
! set of servers like other cpseis files are allowed to do.
!-------------------------------------------------------------------------------
!</algorithm_doc>
!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!-------------------------------------------------------------------------------
!</programming_doc>
!-------------------------------------------------------------------------------
!<gui_def>
!<NS WSEP/NC=80>
!     FILENAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                               SEP Dimensions
!     `-----------------------------------------------------------------
!     N       D          O               LABEL                             
!     `IIIIIII`FFFFFFFFFF`FFFFFFFFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!     `IIIIIII`FFFFFFFFFF`FFFFFFFFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!     `IIIIIII`FFFFFFFFFF`FFFFFFFFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!     `IIIIIII`FFFFFFFFFF`FFFFFFFFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!     `IIIIIII`FFFFFFFFFF`FFFFFFFFFFFFFFF`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!     `-----------------------------------------------------------------
!     OVERRIDE=`CC
!<PARMS N_ARRAYSET[XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="FILENAME">
! <Tip>Full file name for the output SEP file. </Tip>
! Default = NONE
! Allowed = any valid file name
! Your file name for the SEP output file.
!</Help>

!<Help KEYWORD="N">
!<Tip>Number of samples in this dimension.  First entry in this column is the fastest dimension.</Tip>
! Default = 
! Allowed >=0 integer
! Fastest dimension first, (typically the number of samples per trace), then next slowest, on down.
!</Help>

!<Help KEYWORD="OVERRIDE">
!<Tip>Override the settings for N1, D1, O1, L1, and N2?</Tip>
! Default = NO
! Allowed = YES or NO
! The settings for the first dimension and the second N2 value are automatically generated unless you say YES.
!</Help>
!
!<Help KEYWORD="D">
!<Tip>Sample rate or increment between samples associated with this dimension.</Tip>
! Default = 
! Allowed = floating point number
! Sample rate for this dimension or "increment" for this dimension.  Distance or time/depth between
! samples.
!</Help>

!<Help KEYWORD="O">
!<Tip>Origin for this dimension.  (could be starting time for a trace, first inline, etc.)</Tip>
! Default = 
! Allowed = floating point number
! Origin or starting point for this dimension.
!</Help>

!<Help KEYWORD="LABEL">
!<Tip>Label associated with this dimension.</Tip>
! Default = 
! Allowed = character string
! Your label for this dimension.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module wsep_module

  use pc_module
  use named_constants_module
  use string_module
  use getlun_module
  use cio_module

  implicit none

  private

  !--- PROCEDURES
  public  :: wsep_create     ! uses the parameter cache.
  public  :: wsep_initialize
  public  :: wsep_update     ! uses the parameter cache.
  public  :: wsep_delete
  public  :: wsep_struct
  public  :: wsep            ! main execution (trace processing) routine.
  public  :: wsep_wrapup

  character(len=100),save,public :: wsep_ident = &
  '$Id: wsep.f90,v 1.84 2008/10/24 21:03:11 mengewm Exp $'
  integer :: MSD
  parameter (MSD=15)
  type :: wsep_struct
    private
    !--Globals--
    integer                    :: numtr
    integer                    :: nwih
    integer                    :: ndpt
    real                       :: tstrt
    real                       :: dt
    type(grid_struct)          :: grid

    !--PARAMETERS --
    logical                    :: gathered ! is the data gathered?
    !--Local saved vars --
    integer                    :: stdout
    integer                    :: ipn
    logical                    :: skip_wrapup
    logical                    :: first
    logical                    :: override

    integer                          :: ndim
    integer, dimension(MSD)          :: n
    double precision, dimension(MSD) :: d
    double precision, dimension(MSD) :: o
    character(len=80),dimension(MSD) :: label
    integer                          :: unit
    integer                          :: ntraces
    integer                          :: traces_written
    character(len=FILENAME_LENGTH)   :: filename
  end type wsep_struct

contains

  subroutine wsep_create (obj)
    type(wsep_struct),pointer :: obj       ! arguments

    nullify  (obj)
    allocate (obj)
    call wsep_initialize   (obj)
    !--- this data comes from this process itself

  end subroutine wsep_create

  subroutine wsep_delete (obj)
    type(wsep_struct),pointer :: obj       ! arguments
    integer                   :: ierr

    if(associated(obj) ) call wsep_wrapup (obj)
    if(associated(obj) ) deallocate(obj)

  end subroutine wsep_delete

  subroutine wsep_initialize (obj)
  type(wsep_struct),pointer :: obj       ! arguments
  integer :: i

  if(.not. associated(obj) ) return
    !----------Here we initialize all data that will be subsequently updated.

    !--- this data comes from the parm-cache globals
    call pc_get_global ('numtr', obj%numtr) ! maximum number of traces.
    call pc_get_global ('ndpt' , obj%ndpt)  ! number of trace samples.
    call pc_get_global ('nwih' , obj%nwih)  ! number of header words.
    call pc_get_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
    call pc_get_global ('dt'   , obj%dt)    ! trace sample interval (sec).
    !call pc_get_global ('grid' , obj%grid)  ! grid transform data structure.
    !--- this data comes from the parm-cache data cards for this process

    obj%ipn = pc_get_ipn()

    obj%first    = .true.
    obj%override = .true.
    obj%n(1)  = obj%ndpt
    obj%d(1)  = obj%dt
    obj%o(1)  = obj%tstrt
    if(obj%dt < 1.0 ) then
      obj%label(1) = 'Time'
    else
      obj%label(1) = 'Depth'
    endif
    do i=2,MSD
      obj%n(i)     = 1
      obj%d(i)     = 0.0
      obj%o(i)     = 0.0
      obj%label(i) = ''
    end do
    if(obj%numtr > 1 ) then
      obj%n(2) = obj%numtr
    endif
    obj%filename=''
    call getlun(obj%unit)
    obj%traces_written=0
    obj%ntraces=0

    call pc_put_options_field('override',(/"YES"," NO"/),2)

    call wsep_update (obj)

  end subroutine wsep_initialize

  subroutine wsep_update (obj)
    type(wsep_struct),target              :: obj
    integer                               :: i,istat,nc,status

    !--- add skip_wrapup flag.
    obj%skip_wrapup = .true.

    !--- get stdout unit number from parm cache.
    obj%stdout = pc_get_lun()         ! if needed.
    !--- get globals from parm cache.
    call pc_get_global ('numtr', obj%numtr) ! maximum number of traces.
    call pc_get_global ('gathered', obj%gathered) ! is data gathered?
    !--- get parameters for this instance of my process from parm cache.
    call pc_put_global  ('numtr'       , obj%numtr)     ! if changed.
    call pc_put_control ('twosets'     , .false.)       ! default false
    call pc_put_control ('iftd'        , .false.)       ! default false
    call pc_put_control ('ndisk'       , 0 )
    call pc_put_control ('setup_only'  , .false.)       ! default .false.

    obj%ndim=MSD
    call pc_get ('n'     ,     obj%n(:obj%ndim), obj%ndim)
    call pc_get ('d'     ,     obj%d(:obj%ndim), obj%ndim)
    call pc_get ('o'     ,     obj%o(:obj%ndim), obj%ndim)
    call pc_get ('label' , obj%label(:obj%ndim), obj%ndim)
    call pc_get ('filename', obj%filename)
    call pc_get ('override', obj%override)
    if(.not. obj%override) then
      obj%n(1)  = obj%ndpt
      obj%d(1)  = obj%dt
      obj%o(1)  = obj%tstrt
      if(obj%dt < 1.0) then
         obj%label(1) = 'Time'
      else
         obj%label(1) = 'Depth'
      endif
      obj%n(2)  = obj%numtr
    endif
    do i = 1,MSD 
       if(obj%n(i) <= 1 ) exit
    end do
    obj%ndim=max(1,i-1)
    
    do i=obj%ndim+1,MSD
      obj%n(i)     = 1
      obj%d(i)     = 1.0
      obj%o(i)     = 0.0
      obj%label(i) = 'Unknown'
    end do

    call pc_put ('n'     ,     obj%n(:obj%ndim), obj%ndim)
    call pc_put ('d'     ,     obj%d(:obj%ndim), obj%ndim)
    call pc_put ('o'     ,     obj%o(:obj%ndim), obj%ndim)
    call pc_put ('label' , obj%label(:obj%ndim), obj%ndim)
    call pc_put ('filename', trim(obj%filename))
    call pc_put ('override', obj%override)


    write(obj%stdout,'(a)')'# SEP-Format File created by CPSeis WSEP process'
    write(obj%stdout,'(a)')'#'
    write(obj%stdout,'(a)')'#------------------------------------------------------'
    do i=1,obj%ndim
      !                        123456789012  1  1234567890123456
      write(obj%stdout,'(a,i1,a)'    )'# Dimension ',i,' characteristics'
      write(obj%stdout,'(a1,i1,a1,i9)'   )'n',i,'=',obj%n(i)
      write(obj%stdout,'(a1,i1,a1,f14.7)')'d',i,'=',obj%d(i)
      write(obj%stdout,'(a1,i1,a1,f14.7)')'o',i,'=',obj%o(i)
      write(obj%stdout,'(a5,i1,a2,A)'    )'label',i,'="',trim(obj%label(i))//'"'
    end do
    write(obj%stdout,'(a)')'esize=4'
    write(obj%stdout,'(a)')'data_format="native_float"'
    write(obj%stdout,'(a)')'in="'//trim(obj%filename)//'@"'
    write(obj%stdout,'(a)')'#------------------------------------------------------'

    !<execute_only>

    if (pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

  end subroutine wsep_update

  !<execute_only>

subroutine wsep (obj,ntr,hd,tr)
  !----------- PASSED PARAMETERS ----------------------------------------
  type(wsep_struct),pointer       :: obj                    ! parm block
  integer         ,intent(inout)  :: ntr                    ! num trc
  double precision,intent(inout)  :: hd(:,:)                ! headers
  real            ,intent(inout)  :: tr(:,:)                ! traces
  !----------- LOCAL VARIABLES ------------------------------------------
  double precision,dimension(:)   :: hd_local(obj%nwih)     ! temp stash
  real            ,dimension(:)   :: tr_local(obj%ndpt)     ! temp stash
  integer                         :: status,i,ierr,ntraces,nbr_written
  integer(KIND=8)                 :: nsamples,nextents
  integer         ,dimension(:)   :: ext_size(2)      
  integer         ,dimension(:)   :: file_size(2)
  character(len=80)               :: card  ! use this for sep header file line.
  !--- executable code starts -------------------------------------------

  !--- go away if error coming in.
  if (ntr == FATAL_ERROR)  then
    call cio_flsz(trim(obj%filename),file_size)
    status=cio_fclose(obj%unit)
    status=cio_truncate(trim(obj%filename),file_size)
    if(obj%ntraces < obj%traces_written ) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%traces_written-obj%ntraces,' more traces than described in header.'
    elseif(obj%ntraces > obj%traces_written) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%ntraces-obj%traces_written,' fewer traces than described in header.'
    endif
    return
  endif
  if (ntr == NEED_TRACES) return

  if (ntr <  0 ) return

  if(ntr == NO_MORE_TRACES) then
    ! close data file
    call cio_flsz(trim(obj%filename),file_size)
    status=cio_fclose(obj%unit)
    status=cio_truncate(trim(obj%filename),file_size)
    if(obj%ntraces < obj%traces_written ) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%traces_written-obj%ntraces,' more traces than described in header.'
    elseif(obj%ntraces > obj%traces_written) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%ntraces-obj%traces_written,' fewer traces than described in header.'
    endif
    return
  endif

  if(obj%first) then
    obj%first = .false.
    ! open header file
    open(unit=obj%unit,file=trim(obj%filename),form='FORMATTED',iostat=ierr)
    if(ierr /= 0 ) then
       call pc_error("Unable to open file for header data")
    endif
    ! write header file
    write(obj%unit,'(a)')'# SEP-Format File created by CPSeis WSEP process'
    write(obj%unit,'(a)')'#'
    write(obj%unit,'(a)')'#------------------------------------------------------'
    ntraces=1
    nsamples=1
    do i=1,obj%ndim
      obj%n(i)=max(obj%n(i),1) ! keeps a zero from being entered by mistake
      nsamples = obj%n(i)*nsamples
      if(i>1) ntraces =ntraces * obj%n(i)
      !                              123456789012  1  1234567890123456
      write(obj%unit,'(a,i1,a)'    )'# Dimension ',i,' characteristics'
      !                                  1  1  1  123456789
      write(card    ,'(a1,i1,a1,i9)'   )'n',i,'=',obj%n(i)
      call string_squeeze_blanks(card)
      write(obj%unit,'(a)')trim(card)
      !                                  1  1  1  12345678901234
      write(card    ,'(a1,i1,a1,f14.7)')'d',i,'=',obj%d(i)
      call string_squeeze_blanks(card)
      write(obj%unit,'(a)')trim(card)
      !                                  1  1  1  12345678901234
      write(card    ,'(a1,i1,a1,f14.7)')'o',i,'=',obj%o(i)
      call string_squeeze_blanks(card)
      write(obj%unit,'(a)')trim(card)
      !                                  12345  1  12  12345678901234567890....
      write(card    ,'(a5,i1,a2,A)'    )'label',i,'="',trim(obj%label(i))//'"'
      call string_squeeze_blanks(card)
      write(obj%unit,'(a)')trim(card)
    end do
    obj%ntraces = ntraces 
    write(obj%unit,'(a,i11,a)')'# There should be ',obj%ntraces,' traces in this file.'
    write(obj%unit,'(a,i11,a)')'# The file should be ',4*obj%ntraces*obj%n(1),' bytes in size.'
    write(obj%unit,'(a)')'esize=4'
    write(obj%unit,'(a)')'data_format="native_float"'
    write(obj%unit,'(a)')'in="'//trim(obj%filename)//'@"'
    write(obj%unit,'(a)')'#------------------------------------------------------'
    close(obj%unit)
    write(6,'(a)')'SEP Header file '//trim(obj%filename)//' written.'
    ! open data file
    nextents=(4*nsamples)/cio_get_file_ext_size(0) 
    ext_size(1) = 1 + nextents   ! Make it bigger
    ext_size(2) = 0
    ! Now set the extent size of this file to something large enough so that the file won't
    ! be fragmented with the cio system across more than one location.
    status=cio_set_file_ext_size(ext_size)
    ! Now that we have set the extent size, we can open the file.
    obj%unit=cio_fopen(trim(obj%filename)//'@','w+') ! open the file, overwrite if it exists.
    !open(unit=obj%unit,file=trim(obj%filename)//'@',form='UNFORMATTED',IOSTAT=ierr)
    if(obj%unit < 0) then
       call pc_error("Unable to open file for binary data")
       ntr=FATAL_ERROR
       return
    endif
  endif

  ! write seismic data to the binary file, one 2-d slice at a time.  make sure
  ! that you provide data to this routine in the exact order you want the cube
  ! to be written, and fill in all missing traces with zeroed traces to make sure
  ! all dimensions are completely regular.
  ! Also, you can only output the exact number of samples specified in the job.

  obj%traces_written = obj%traces_written + ntr
  
  !write(obj%unit)tr(1:obj%ndpt,1:ntr)
  !-----------------------(object being written, size(bytes), number_objects, file_unit)
  nbr_written = cio_fwrite(tr(1:obj%ndpt,1:ntr), 4          , obj%ndpt*ntr  , obj%unit )
  !print*,'ntr=',ntr,' ndpt=',obj%ndpt,' nbr_written=',nbr_written,' trcw=',obj%traces_written

  if(nbr_written /= obj%ndpt*ntr ) then
    call pc_error("Error writing traces to file")
    ntr=FATAL_ERROR
    ! close data file
    call cio_flsz(trim(obj%filename),file_size)
    status=cio_fclose(obj%unit)
    status=cio_truncate(trim(obj%filename),file_size)
    if(obj%ntraces < obj%traces_written ) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%traces_written-obj%ntraces,' more traces than described in header.'
    elseif(obj%ntraces > obj%traces_written) then
      write(obj%stdout,'(a,I11,a)')'WSEP-ERROR: Wrote ',obj%ntraces-obj%traces_written,' fewer traces than described in header.'
    endif
    return
  endif
  
end subroutine wsep

subroutine wsep_wrapup (obj)
  type(wsep_struct),pointer  :: obj
  !--- local vars.
  integer                    :: status
  integer                    :: i

  if(.not. associated(obj) ) return

  if(obj%skip_wrapup) return
  obj%skip_wrapup = .true.

  if (pc_get_update_state() /= PC_FRONTEND .and. &
      pc_get_update_state() /= PC_GUI ) then
    close(obj%unit)
  endif

end subroutine wsep_wrapup

end module wsep_module
