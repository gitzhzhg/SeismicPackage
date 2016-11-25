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
! Name       : awindowlist 
! Category   : cfe
! Written    : 1999-08-03   by: Donna K. Vunderink
! Revised    : 2003-08-12   by: Tom Stoeckley
! Maturity   : production   2003-09-15
! Purpose    : CFE List of Windows Object Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2003-09-15  Stoeckley    Changed name from windowlist to awindowlist;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
!  2. 2000-08-15  Vunderink    Removed use of cfe_constants
!  1. 1999-08-03  Vunderink    Initial version.
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

      module awindowlist_module

      use awindow_module

      implicit none

      private
      public :: awindowlist_create
      public :: awindowlist_delete
      public :: awindowlist_delete_window
      public :: awindowlist_add_window
      public :: awindowlist_set_first_window
      public :: awindowlist_set_last_window
      public :: awindowlist_get_first_window
      public :: awindowlist_get_last_window

      character(len=100),public,save :: awindowlist_ident = &
       '$Id: awindowlist.f90,v 1.3 2003/09/12 19:14:15 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: awindowlist_struct              
        private
        type(awindow_struct),pointer                  :: first_window
        type(awindow_struct),pointer                  :: last_window
      end type awindowlist_struct


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


      subroutine awindowlist_create (obj)
      implicit none
      type(awindowlist_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify(obj%first_window)
      nullify(obj%last_window)

      return
      end subroutine awindowlist_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine awindowlist_delete (obj)
      implicit none
      type(awindowlist_struct),pointer         :: obj       ! arguments

      type(awindow_struct),pointer             :: previous  ! local

      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_window)) exit
        call awindow_get_previous (obj%last_window,previous)
        call awindow_delete (obj%last_window)
        obj%last_window => previous
      enddo

      deallocate(obj)

      return
      end subroutine awindowlist_delete


!!---------------------------- delete_window ------------------------------!!
!!---------------------------- delete_window ------------------------------!!
!!---------------------------- delete_window ------------------------------!!


      subroutine awindowlist_delete_window (obj,window_obj)
      implicit none
      type(awindowlist_struct),pointer           :: obj  ! argument
      type(awindow_struct),pointer               :: window_obj      ! argument

      type(awindow_struct),pointer               :: previous        ! local
      type(awindow_struct),pointer               :: next            ! local

      nullify(previous)
      nullify(next)

      call awindow_get_previous (window_obj,previous)
      call awindow_get_next     (window_obj,next)

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

      call awindow_delete (window_obj)

      return
      end subroutine awindowlist_delete_window


!!----------------------------- add_window --------------------------------!!
!!----------------------------- add_window --------------------------------!!
!!----------------------------- add_window --------------------------------!!


      subroutine awindowlist_add_window (obj,new)
      implicit none
      type(awindowlist_struct),pointer           :: obj             ! argument
      type(awindow_struct),pointer               :: new             ! argument

      if (associated(obj%first_window)) then
        call awindow_set_next     (obj%last_window,new)
        call awindow_set_previous (new,obj%last_window)
      else
        obj%first_window => new
      endif

      obj%last_window => new

      return
      end subroutine awindowlist_add_window


!!-------------------------- set_first_window -----------------------------!!
!!-------------------------- set_first_window -----------------------------!!
!!-------------------------- set_first_window -----------------------------!!


      subroutine awindowlist_set_first_window (obj,first_window)
      implicit none
      type(awindowlist_struct),pointer            :: obj            ! argument
      type(awindow_struct),pointer                :: first_window   ! argument

      if (.not. associated(obj)) return

      if (associated(first_window)) then
        obj%first_window => first_window
      else
        nullify(obj%first_window)
      endif
 
      return
      end subroutine awindowlist_set_first_window


!!--------------------------- set_last_window -----------------------------!!
!!--------------------------- set_last_window -----------------------------!!
!!--------------------------- set_last_window -----------------------------!!


      subroutine awindowlist_set_last_window (obj,last_window)
      implicit none
      type(awindowlist_struct),pointer            :: obj            ! argument
      type(awindow_struct),pointer                :: last_window    ! argument

      if (.not. associated(obj)) return

      if (associated(last_window)) then
        obj%last_window => last_window
      else
        nullify(obj%last_window)
      endif
 
      return
      end subroutine awindowlist_set_last_window


!!-------------------------- get_first_window -----------------------------!!
!!-------------------------- get_first_window -----------------------------!!
!!-------------------------- get_first_window -----------------------------!!


      subroutine awindowlist_get_first_window (obj,first_window)
      implicit none
      type(awindowlist_struct),pointer            :: obj            ! argument
      type(awindow_struct),pointer                :: first_window   ! argument

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
      end subroutine awindowlist_get_first_window


!!--------------------------- get_last_window -----------------------------!!
!!--------------------------- get_last_window -----------------------------!!
!!--------------------------- get_last_window -----------------------------!!


      subroutine awindowlist_get_last_window (obj,last_window)
      implicit none
      type(awindowlist_struct),pointer            :: obj            ! argument
      type(awindow_struct),pointer                :: last_window    ! argument

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
      end subroutine awindowlist_get_last_window


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module awindowlist_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

