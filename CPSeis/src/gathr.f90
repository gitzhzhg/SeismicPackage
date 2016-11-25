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
! Name       : GATHR
! Category   : sorts
! Written    : 2001-07-06   by: Charles C Burch
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : A simple primitive to gather traces using dynamic memory.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! The GATHR primitive takes input traces and outputs them in bunches
! as defined by a specified header word or a specified number of traces.
! A new bunch will begin each time the specified number of traces are received
! or the value of the specified header word, rounded to a whole number, changes.
! Input traces may be either gathered or ungathered.
! Arrays can be specified to be quite large and only the space needed is 
!   actually allocated.
!
! If gathering by a specified header word, the traces must be sorted to the
! desired order prior to being input to GATHR.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
! If gathering by a specified header word, the traces must be sorted to the
! desired order prior to being input to GATHR.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                            Action taken
! ----       -----------                            ------------
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! No header words are changed by these routines.
! The header word specified in gathr_create will be used to test for
! gather breaks if check_groups is set to true in gathr_create
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
! call gathr_create(obj, n_max, n_bunch, check_grps, gather_hw)
! This creates the gathr structure obj and sets variables in obj
!
! Arguments are:
!   type (gathr_struct), pointer  :: obj       !object to be created
!   integer, intent(in), optional :: n_max     !maximum size arrays are to grow
!     n_max can be quite large and only space needed will be allocated
!   integer, intent(in), optional :: n_bunch   !maximum size to bunch at a time
!   integer, intent(in), optional :: gather_hw !header word to check for breaks
!   logical, intent(in), optional :: check_grps!true if check for group breaks
!
! Defaults: n_max=9999,n_bunch=n_max, gather_hw=3,check_groups=.true.
!-------------------------------------------------------------------------------
! call gathr_delete(obj)
! This deletes the gathr structure obj and free any allocated work space
!
! Arguments are:
!   type (gathr_struct), pointer  :: obj      !object to be created
!-------------------------------------------------------------------------------
! call gathr(obj, ntr, hdrs, trcs)
! This gathers input data according to parameters in gathr object
!  input will be ntr=NEED_TRACES, NO_MORE_TRACES or >0
!  if gather break occurs, ntr, trcs, hdrs will be set
!  hdrs, trcs (pointers) are resized if output data will not fit in it.
!  gather break can be group change or specified max size hit
!
! Arguments are:
!    type (gathr_struct)             :: obj        !obj for gathr parameters
!    integer,          intent(inout) :: ntr        !input and output ntr
!    double precision, intent(inout) :: hdrs(:,:)  !input and output header data
!    real            , intent(inout) :: trcs(:,:)  !input and output trace data
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Note this is intended to be used by software developers
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author           Description
!     ----        ------           -----------
! 3. 2006-06-20  B. Menger         Removed Unused Variables.
! 2.  2002-01-28  Charles C Burch  Changed hdrs,trcs in gathr to non pointers
! 1.  2001-07-06  Charles C Burch  Initial version
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
! None
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
!  n_max can be quite large in gathr_create call-only space needed will be used
!
! Examples for possible uses:
!
! call gathr_create(9999,20,.false.,3)
!   sets up gathr to output the input data in groups of 20 traces at a time
!   in this case no breaks in any header word are honored.
!
! call gathr_create(9999,20,.true.,3)
!   sets up gathr to output the input data in groups of at most 20 traces at 
!   a time, all traces in a group will have common header word 3.
!
! call gathr_create(9999,9999,.true.,3)
!   sets up gathr to output the input data in gathers of common header word 3
!   it is assumed that no gather will be larger than 9999 in this case

!-------------------------------------------------------------------------------
!</programming_doc>


!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!


module gathr_module
  use named_constants_module

  implicit none
  private

  public :: gathr_create
  public :: gathr_delete
  public :: gathr

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  type, public :: gathr_struct
    integer                   :: n_curr            !current amount data buffered
    integer                   :: n_max             !max buffer size
    integer                   :: n_size            !current buffer size
    integer                   :: hd_len            !length of header data
    integer                   :: tr_len            !length of trace data
    integer                   :: gather_header_word!header word for any gather
    integer                   :: n_bunch           !output bunch size
    logical                   :: eof_sw            !true if NO_MORE_TRACES rcvd
    logical                   :: check_groups_sw   !true, if check group break
    double precision          :: group_id          !value of currrent group
    real            , pointer :: trcs(:,:)         !trace buffer area
    double precision, pointer :: hdrs(:,:)         !header buffer area
  end type gathr_struct


!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!

  character(len=100),save,public :: GATHR_ident =                              &
   "$Id: gathr.f90,v 1.3 2006/06/20 13:11:53 Menger prod sps $"

!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!

  contains

!==================================================================
! Create gather structure and initialize
!   obj          gathr structure(pointer) to be created
!   n_max        maximum size arrays are to grow (optional, default 9999)
!     it can be quite large and only space needed will be allocated
!   n_bunch      maximum size to bunch at a time (optional, default n_max)
!   gather_hw    header word to check for breaks (optional default 3)
!   check_groups true if check for group breaks  (optional, default true)
!
! Written July 2001 by Charles C Burch
!==================================================================
  subroutine gathr_create(obj, n_max, n_bunch, check_groups, gather_header_word)
    type (gathr_struct), pointer  :: obj
    integer, intent(in), optional :: n_max, n_bunch, gather_header_word
    logical, intent(in), optional :: check_groups

    integer                       :: istat

    if(associated(obj)) then
      if(associated(obj%hdrs)) deallocate(obj%hdrs,stat=istat)
      if(associated(obj%trcs)) deallocate(obj%trcs,stat=istat)
      deallocate(obj,stat=istat)
    endif

    allocate (obj, stat=istat)
    nullify(obj%hdrs)
    nullify(obj%trcs)

    obj%n_curr=0
    obj%n_size=0
    obj%hd_len=0
    obj%tr_len=0
    obj%eof_sw=.false.
    obj%group_id=0

    if(present(n_max)) then
      obj%n_max=n_max
    else
      obj%n_max=9999
    endif

    if(present(n_bunch)) then
      obj%n_bunch=n_bunch
    else
      obj%n_bunch=obj%n_max
    endif
    obj%n_max=max(obj%n_max,obj%n_bunch)

    if(present(check_groups)) then
      obj%check_groups_sw=check_groups
    else
      obj%check_groups_sw=.true.
    endif

    if(present(gather_header_word)) then
      obj%gather_header_word=gather_header_word
    else
      obj%gather_header_word=3
    endif

    return
  end subroutine gathr_create

!====================================================
! Delete gathr structure and free up and allocated space
!
! Written July 2001 by Charles C Burch
!====================================================
  subroutine gathr_delete(obj)
    type (gathr_struct), pointer  :: obj

    integer                       :: istat

    if(.not.associated(obj)) return

    if(associated(obj%hdrs)) deallocate(obj%hdrs,stat=istat)
    if(associated(obj%trcs)) deallocate(obj%trcs,stat=istat)

    deallocate(obj,stat=istat)
    nullify(obj)
    return
  end subroutine gathr_delete

!==============================================================
! gather input data according to paramters in gathr structure 
!  input will be ntr=NEED_TRACES, NO_MORE_TRACES or >0
!  if gather break occurs, ntr, trcs, hdrs will be set
!  hdrs, trcs will be resized if output data wil not fit it in
!  gather break can be group change or specified max size hit
!
! Written July 2001 by Charles C Burch
!==============================================================
  subroutine gathr(obj, ntr, hdrs, trcs)
    type (gathr_struct)       :: obj
    integer, intent(inout)    :: ntr
    double precision          :: hdrs(:,:)
    real                      :: trcs(:,:)

    double precision, pointer :: hdrs_tmp(:,:)
    real            , pointer :: trcs_tmp(:,:)
    integer                   :: istat, i, n


    if(obj%hd_len.le.0) then              !get array sizes first time through
      obj%hd_len=size(hdrs,dim=1)
      obj%tr_len=size(trcs,dim=1)
      n=size(hdrs,dim=2)
      if(2*n.gt.obj%n_max) obj%n_max=2*n
      if(n.lt.obj%n_bunch) obj%n_bunch=n
    endif

    if (ntr.gt.0) then
! --- get group_id if, not set
      if(obj%n_curr.eq.0) obj%group_id=hdrs(obj%gather_header_word,1)

      if((obj%n_curr+ntr).gt.obj%n_size) then   !expand obj%hdrs/trcs, if needed
        if(obj%n_curr.gt.0) then                !save old data
          allocate(hdrs_tmp(1:obj%hd_len,1:obj%n_curr),stat=istat)
          allocate(trcs_tmp(1:obj%tr_len,1:obj%n_curr),stat=istat)
          hdrs_tmp=obj%hdrs(:,1:obj%n_curr)
          trcs_tmp=obj%trcs(:,1:obj%n_curr)
        endif

        if(obj%n_size.gt.0) then                !free old obj%hdrs/trcs
          deallocate(obj%hdrs,stat=istat)
          deallocate(obj%trcs,stat=istat)
        endif

! ---   allocate new obj%hdrs/trcs
        obj%n_size=min(obj%n_max,max(obj%n_curr+ntr+1,obj%n_size+obj%n_size/8))
        allocate(obj%hdrs(1:obj%hd_len,1:obj%n_size),stat=istat)
        allocate(obj%trcs(1:obj%tr_len,1:obj%n_size),stat=istat)

        if(obj%n_curr.gt.0) then                !move any old data
          obj%hdrs(:,1:obj%n_curr)=hdrs_tmp
          obj%trcs(:,1:obj%n_curr)=trcs_tmp
          deallocate(hdrs_tmp,stat=istat)       !free work area to save data
          deallocate(trcs_tmp,stat=istat)
        endif
      endif

! -- move data to buffer and set NTR=NEED_TRACES to see if output data ready

      obj%hdrs(:,obj%n_curr+1:obj%n_curr+ntr)=hdrs(:,1:ntr)
      obj%trcs(:,obj%n_curr+1:obj%n_curr+ntr)=trcs(:,1:ntr)
      obj%n_curr=obj%n_curr+ntr
      ntr=NEED_TRACES
    endif

    if(ntr.eq.NO_MORE_TRACES) then  !EOF-set switch and set ntr to NEED_TRACES
      obj%eof_sw=.true.
      ntr=NEED_TRACES
    endif

    if(ntr.ne.NEED_TRACES) then    !ntr should be NEED_TRACES
      ntr=FATAL_ERROR
      return
    endif

! --- ntr=NEED_TRACES at this point

    if(obj%n_curr.eq.0) then   !handle no data buffered
      if(obj%eof_sw) then
        ntr=NO_MORE_TRACES     !eof
      else
        ntr=NEED_TRACES        !not eof
      endif
      return
    endif

! --- know n_curr>0, so have data in buffer

    if(obj%eof_sw) then
      if(obj%check_groups_sw)then
! --- eof and check groups-see where group break occurs
        do i=1,obj%n_curr
          if(obj%hdrs(obj%gather_header_word,1) .ne.                           &
           obj%hdrs(obj%gather_header_word,i)) exit
        enddo
        ntr=min(i-1,obj%n_bunch)
      else

! --- eof and no check groups-output a bunch or what is available
        ntr=min(obj%n_curr,obj%n_bunch)!eof, no group check-return buffered data
      endif

    else
      if(obj%check_groups_sw) then
! --- not eof and check group-see if group break 
        if(obj%hdrs(obj%gather_header_word,obj%n_curr).eq.obj%group_id .and. &
         obj%n_curr.lt.obj%n_bunch) then
          ntr=NEED_TRACES       !not at eof and no group break-request more data
          return
        endif

        obj%group_id=obj%hdrs(obj%gather_header_word,1)
        do i=1,obj%n_curr         !see where group break occurs
          if(obj%hdrs(obj%gather_header_word,i).ne.obj%group_id) then
            obj%group_id=obj%hdrs(obj%gather_header_word,i)
            exit
          endif
        enddo
        ntr=min(i-1,obj%n_bunch)

      else
! --- not eof and not check groups-see if bunch available
        if(obj%n_curr.lt.obj%n_bunch) then
          ntr=NEED_TRACES
          return
        endif
        ntr=obj%n_bunch
      endif

    endif

! -- extract ntr traces from buffer to output

    hdrs(:,1:ntr)=obj%hdrs(:,1:ntr)   !move buffered data to output
    trcs(:,1:ntr)=obj%trcs(:,1:ntr)

    n=obj%n_curr-ntr                  !shift buffered data, if needed
    if(n.gt.0) then
      obj%hdrs(:,1:n)=obj%hdrs(:,ntr+1:obj%n_curr)
      obj%trcs(:,1:n)=obj%trcs(:,ntr+1:obj%n_curr)
    endif
    obj%n_curr=n

    return
  end subroutine gathr

!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!

end module gathr_module

! ------------Crude test program -------------------
!program main
! 
!  use gathr_module
!  use named_constants_module
! 
!  type (gathr_struct), pointer :: obj
!  double precision, pointer    :: hdrs(:,:)
!  real            , pointer    :: trcs(:,:)
!  integer                      :: i, j,n
! 
!! --- pick one of the following three cases:
!      call gathr_create(obj, 20, 5,.false.,3)  
!      call gathr_create(obj, 20, 5,.true.,3)  
!      call gathr_create(obj, 20, 20,.true.,3)
!
!  allocate(hdrs(10,20))
!  allocate(trcs(10,20))
! 
!  n=0
!  do i=1, 20
!    do j=1,i
!      n=n+1
!      hdrs(1,1)=n
!      if(j.le.10) then
!        hdrs(3,j)=2*i-1
!      else
!        hdrs(3,j)=2*i
!      endif
! 
!      hdrs(4,j)=j
!    enddo !j
! 
!    ntr=i
!    if(i.eq.20) ntr=NO_MORE_TRACES
!    j=ntr
! 
!  100 continue
!    print *,"sending, i, ntr=",i, ntr
!    if(ntr.gt.0) print '(" in:",25i3)',(nint(hdrs(3,k)),k=1,ntr)
!    call gathr(obj,ntr,hdrs,trcs)
!    print *,"ntr=",ntr
!
!    if(ntr.gt.0) then
!      print '(" Out",25i3)',(nint(hdrs(3,k)),k=1,ntr)
!      ntr=NEED_TRACES
!      goto 100
!    else if(ntr.eq.NO_MORE_TRACES) then
!      print *,"NO_MORE_TRACES"
!      exit
!    else if(ntr.eq.NEED_TRACES) then
!      print *,"NEED_TRACES"
!      cycle
!    else
!      print *,"Invalid ntr",ntr
!    endif
!  enddo !i
! 
!  call gathr_delete(obj)
!  stop
!end program main

!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!
!!--------------------------------- end ---------------------------------!!

