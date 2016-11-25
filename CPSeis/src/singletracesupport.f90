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
!                         C P S   P R I M I T I V E              
!
! Name       : SINGLETRACESUPPORT
! Category   : miscellaneous
! Written    : 2010-08-02   by: Tom Stoeckley
! Revised    : 2010-08-02   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Enhance modules which require single trace input and output.
! Portability: No known limitations.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! Use this primitive to enhance processing modules which require single trace
! input and output only.  This primitive allows such processing modules to input
! and output multiple trace gathers.
!
! Such processing modules have these requirements:
!
! (a) The module must have a oneset subroutine but not a twosets subroutine.
! (b) The input and output values of nwih and ndpt must be the same.
! (c) The module must output the same traces it receives, in the same order.
! (d) As a result, the input and output gather sizes will be the same.
!
!                              ------------
!
! To convert the module to allow multiple trace gathers, follow these steps:
!
! (1) Add the following line to the processing module's use statements:
!        use singletracesupport_module
!
! (2) Add the following line to the processing module's data structure:
!        type(singletracesupport_struct),pointer :: sts
!
! (3) Add the following line to the processing module's create subroutine:
!        nullify (obj%sts)
!
! (4) Call the singletracesupport_create subroutine from the processing module's
!     update subroutine when preparing to process traces.
!
! (5) Call the singletracesupport_delete subroutine from the processing module's
!     wrapup subroutine.
!
! (6) Call pc_put_control ("twosets", .true.) from the processing module's
!     update subroutine.
!
! (7) Remove code which restricts the global NUMTR to 1, and do not use or reset
!     the globals NUMTR and GATHERED.
!
! (8) Change the name of the processing module's oneset subroutine to something
!     else, such as xxxx_single_trace (where xxxx is the processing module name),
!     but with no other changes.
!
! (9) Write a new twosets subroutine for the processing module which looks like
!     this (where xxxx is the processing module name):
!
!                              ------------
!
!       subroutine xxxx (obj, ntr, hdi, tri, hdo, tro)
!       implicit none
!       type(xxxx_struct),intent(inout) :: obj
!       integer          ,intent(inout) :: ntr
!       double precision ,intent(inout) :: hdi(:,:)
!       real             ,intent(inout) :: tri(:,:)
!       double precision ,intent(out)   :: hdo(:,:)
!       real             ,intent(out)   :: tro(:,:)
!
!       if (singletracesupport_input(obj%sts,ntr,hdi,tri)) return
!
!   22  call xxxx_single_trace (obj, ntr, hdi, tri)
!
!       if (singletracesupport_output(obj%sts,ntr,hdi,tri,hdo,tro)) go to 22
!
!       end subroutine xxxx
!-------------------------------------------------------------------------------
!</descript_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
! 8.  2010-08-02  Stoeckley   Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module singletracesupport_module

      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: singletracesupport_IDENT = &
       '$Id: singletracesupport.f90,v 1.5 2006/01/03 12:36:22 S.Chiu prod sps $'

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type,public :: singletracesupport_struct              

          private
          integer   :: nwih
          integer   :: ndpt
          integer   :: itr_in
          integer   :: itr_out
          integer   :: ntr_inout
          logical   :: ended

      end type singletracesupport_struct

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine singletracesupport_create (obj, nwih, ndpt)
      implicit none
      type(singletracesupport_struct),pointer    :: obj    ! argument
      integer                        ,intent(in) :: nwih   ! argument
      integer                        ,intent(in) :: ndpt   ! argument

      allocate (obj)

      obj%nwih      = nwih
      obj%ndpt      = ndpt
      obj%itr_in    = 0
      obj%itr_out   = 0
      obj%ntr_inout = 0
      obj%ended     = .false.

      return
      end subroutine singletracesupport_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine singletracesupport_delete (obj)
      implicit none
      type(singletracesupport_struct),pointer :: obj      ! argument

      if (associated(obj)) deallocate(obj)
      return
      end subroutine singletracesupport_delete

!!------------------------------ input ------------------------------------!!
!!------------------------------ input ------------------------------------!!
!!------------------------------ input ------------------------------------!!

! This twosets subroutine receives a gather and outputs a gather.
! This twosets subroutine is the glue between the outside world and
! the corresponding singletracesupport_single_trace subroutine which accepts only single trace input.
! The assumptions are:
!  (1) This process outputs the same traces input, and in the same order.
!  (2) The output gather will be the same size as the input gather.
!  (3) The trace and header lengths NWIH and NDPT are not changed.


      function singletracesupport_input (obj, ntr, hdi, tri) result (finished)
      implicit none
      type(singletracesupport_struct),intent(inout) :: obj        ! argument
      integer                        ,intent(inout) :: ntr        ! argument
      double precision               ,intent(in)    :: hdi(:,:)   ! argument
      real                           ,intent(in)    :: tri(:,:)   ! argument
      logical                                       :: finished   ! result

      finished = .false.
      if (ntr == NO_MORE_TRACES) then
          continue   ! do nothing here.
      else if (ntr == NEED_TRACES) then
          if (obj%ended) then    ! the last gather has already been output.
              ntr = NO_MORE_TRACES
              finished = .true.
              return
          endif
          obj%itr_out = 0   ! need to start refilling the output gather.
      else if (ntr > 0) then
          obj%ntr_inout = ntr    ! size of input and output gather.
          obj%itr_in = 1
          ntr = 1     ! the first trace in the new input gather will be processed.
      else
          print *, 'naughty naughty input ntr should not be ',ntr
          ntr = FATAL_ERROR
          finished = .true.
      endif
      end function singletracesupport_input

!!------------------------------ output ------------------------------------!!
!!------------------------------ output ------------------------------------!!
!!------------------------------ output ------------------------------------!!

      function singletracesupport_output (obj, ntr, hdi, tri, hdo, tro) result (repeat)
      implicit none
      type(singletracesupport_struct),intent(inout) :: obj        ! argument
      integer                        ,intent(inout) :: ntr        ! argument
      double precision               ,intent(inout) :: hdi(:,:)   ! argument
      real                           ,intent(inout) :: tri(:,:)   ! argument
      double precision               ,intent(out)   :: hdo(:,:)   ! argument
      real                           ,intent(out)   :: tro(:,:)   ! argument
      logical                                       :: repeat     ! result

      repeat = .false.
      if (ntr == 1) then
          ! put the processed trace into the output gather.
          obj%itr_out = obj%itr_out + 1
          hdo(1:obj%nwih,obj%itr_out) = hdi(1:obj%nwih,1)
          tro(1:obj%ndpt,obj%itr_out) = tri(1:obj%ndpt,1)
          if (obj%itr_out == obj%ntr_inout) then
              ntr = obj%ntr_inout    ! return a full output gather.
              obj%itr_out = 0      ! will begin to refill the output gather.
              return
          endif
          ! go back to the processing module to try to get another processed trace.
          ntr = NEED_TRACES
          repeat = .true.
      else if (ntr == FATAL_ERROR) then
          continue   ! do nothing here.
      else if (ntr == NO_MORE_TRACES) then  ! return what remains of the output gather, or 0.
          if (obj%itr_out == 0) return   ! no more traces to output.
          obj%ended = .true.
          ntr = obj%itr_out    ! outputting whatever is in the last output gather ever.
      else if (ntr == NEED_TRACES) then
          if (obj%itr_in == obj%ntr_inout) return  ! go get more traces since the input gather is used up.
          ! put the next trace to process at the beginning of the input gather.
          obj%itr_in = obj%itr_in + 1
          hdi(1:obj%nwih,1) = hdi(1:obj%nwih,obj%itr_in)
          tri(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%itr_in)
          ! go back to the processing module to process this trace.
          ntr = 1
          repeat = .true.
      else
          print *, 'naughty naughty output ntr should not be ',ntr
          ntr = FATAL_ERROR
      endif
      end function singletracesupport_output

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module singletracesupport_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
