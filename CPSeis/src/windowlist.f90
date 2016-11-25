!<CPS_v1 type="AUXILIARY_FILE"/>
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
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : windowlist 
! Category   : stand-alone
! Written    : 2002-04-23   by: Donna K. Vunderink
! Revised    : 2002-04-23   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : List of Windows Object Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2002-04-23  Vunderink    Initial version.
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

      module windowlist_module

      use window_module

      implicit none

      private
      public :: windowlist_create
      public :: windowlist_delete
      public :: windowlist_delete_window
      public :: windowlist_add_window
      public :: windowlist_set_first_window
      public :: windowlist_set_last_window
      public :: windowlist_get_first_window
      public :: windowlist_get_last_window

      character(len=100),public,save :: windowlist_ident = &
       '$Id: windowlist.f90,v 1.1 2002/04/23 20:20:52 Vunderink prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: windowlist_struct              
        private
        type(window_struct),pointer                  :: first_window
        type(window_struct),pointer                  :: last_window
      end type windowlist_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine windowlist_create (obj)
      implicit none
      type(windowlist_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify(obj%first_window)
      nullify(obj%last_window)

      return
      end subroutine windowlist_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine windowlist_delete (obj)
      implicit none
      type(windowlist_struct),pointer         :: obj       ! arguments

      type(window_struct),pointer             :: previous  ! local

      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_window)) exit
        call window_get_previous (obj%last_window,previous)
        call window_delete (obj%last_window)
        obj%last_window => previous
      enddo

      deallocate(obj)

      return
      end subroutine windowlist_delete


!!---------------------------- delete_window ------------------------------!!
!!---------------------------- delete_window ------------------------------!!
!!---------------------------- delete_window ------------------------------!!


      subroutine windowlist_delete_window (obj,window_obj)
      implicit none
      type(windowlist_struct),pointer              :: obj  ! argument
      type(window_struct),pointer                  :: window_obj      ! argument

      type(window_struct),pointer                  :: previous        ! local
      type(window_struct),pointer                  :: next            ! local

      nullify(previous)
      nullify(next)

      call window_get_previous (window_obj,previous)
      call window_get_next     (window_obj,next)

      if (.not. associated(previous)) then
        if (associated(next)) then
          obj%first_window =>  next
        else
          nullify(obj%first_window)
        endif
      else if (.not. associated(next)) then
        if (associated(previous)) then
          obj%last_window => previous
        else
          nullify(obj%last_window)
        endif
      endif

      call window_delete (window_obj)

      return
      end subroutine windowlist_delete_window


!!----------------------------- add_window --------------------------------!!
!!----------------------------- add_window --------------------------------!!
!!----------------------------- add_window --------------------------------!!


      subroutine windowlist_add_window (obj,new)
      implicit none
      type(windowlist_struct),pointer              :: obj             ! argument
      type(window_struct),pointer                  :: new             ! argument

      if (associated(obj%first_window)) then
        call window_set_next     (obj%last_window,new)
        call window_set_previous (new,obj%last_window)
      else
        obj%first_window => new
      endif

      obj%last_window => new

      return
      end subroutine windowlist_add_window


!!-------------------------- set_first_window -----------------------------!!
!!-------------------------- set_first_window -----------------------------!!
!!-------------------------- set_first_window -----------------------------!!


      subroutine windowlist_set_first_window (obj,first_window)
      implicit none
      type(windowlist_struct),pointer               :: obj            ! argument
      type(window_struct),pointer                   :: first_window   ! argument

      if (.not. associated(obj)) return

      if (associated(first_window)) then
        obj%first_window => first_window
      else
        nullify(obj%first_window)
      endif
 
      return
      end subroutine windowlist_set_first_window


!!--------------------------- set_last_window -----------------------------!!
!!--------------------------- set_last_window -----------------------------!!
!!--------------------------- set_last_window -----------------------------!!


      subroutine windowlist_set_last_window (obj,last_window)
      implicit none
      type(windowlist_struct),pointer               :: obj            ! argument
      type(window_struct),pointer                   :: last_window    ! argument

      if (.not. associated(obj)) return

      if (associated(last_window)) then
        obj%last_window => last_window
      else
        nullify(obj%last_window)
      endif
 
      return
      end subroutine windowlist_set_last_window


!!-------------------------- get_first_window -----------------------------!!
!!-------------------------- get_first_window -----------------------------!!
!!-------------------------- get_first_window -----------------------------!!


      subroutine windowlist_get_first_window (obj,first_window)
      implicit none
      type(windowlist_struct),pointer               :: obj            ! argument
      type(window_struct),pointer                   :: first_window   ! argument

      if (associated(obj)) then
        if (associated(obj%first_window)) then
          first_window => obj%first_window
        else
          nullify(first_window)
        endif
      else
        nullify(first_window)
      endif
 
      return
      end subroutine windowlist_get_first_window


!!--------------------------- get_last_window -----------------------------!!
!!--------------------------- get_last_window -----------------------------!!
!!--------------------------- get_last_window -----------------------------!!


      subroutine windowlist_get_last_window (obj,last_window)
      implicit none
      type(windowlist_struct),pointer               :: obj            ! argument
      type(window_struct),pointer                   :: last_window    ! argument

      if (associated(obj)) then
        if (associated(obj%last_window)) then
          last_window => obj%last_window
        else
          nullify(last_window)
        endif
      else
        nullify(last_window)
      endif
 
      return
      end subroutine windowlist_get_last_window


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module windowlist_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

