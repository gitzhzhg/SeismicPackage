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
! Name       : worklist 
! Category   : cfe
! Written    : 1999-08-03   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE List of Workfiles Object Module.
! Portability: No known limitations, but see note below.
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
!005. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!  4. 2003-11-18  Stoeckley    Provide workaround for Portland Group compiler.
!  3. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe.
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
! The Portland Group compiler incorrectly executes code such as the following:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       next => aaa%next
!       end subroutine
!
!       call glotch_get_next (ccc,ccc)
!
! To work around the bug, calls such as the above (in this file and several
! others) have been changed to:
!
!       call glotch_get_next (ccc,next)
!       ccc => next
!
! Changing the subroutine like this does not fix the problem:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       type(xxxx_struct),pointer :: temp
!       temp => aaa%next
!       next => temp
!       end subroutine
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


      module worklist_module

      use workfile_module

      implicit none

      private
      public :: worklist_create
      public :: worklist_delete
      public :: worklist_delete_workfile
      public :: worklist_replace_workfile
      public :: worklist_insert_workfile
      public :: worklist_set_first_workfile
      public :: worklist_set_last_workfile
      public :: worklist_set_num_workfiles
      public :: worklist_set_previous
      public :: worklist_set_next
      public :: worklist_get_first_workfile
      public :: worklist_get_last_workfile
      public :: worklist_get_num_workfiles
      public :: worklist_get_previous
      public :: worklist_get_next
      public :: worklist_get_list

      character(len=100),public,save :: worklist_ident = &
       '$Id: worklist.f90,v 1.5 2006/09/18 13:32:52 Glover prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: worklist_struct              
        private
        integer                                :: num_workfiles
        type(workfile_struct),pointer          :: first_workfile
        type(workfile_struct),pointer          :: last_workfile
        type(worklist_struct),pointer          :: previous
        type(worklist_struct),pointer          :: next
      end type worklist_struct


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


      subroutine worklist_create (obj)
      implicit none
      type(worklist_struct),pointer :: obj       ! arguments

      allocate (obj)
      obj%num_workfiles = 0
      nullify(obj%first_workfile)
      nullify(obj%last_workfile)
      nullify(obj%previous)
      nullify(obj%next)

      return
      end subroutine worklist_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine worklist_delete (obj)
      implicit none
      type(worklist_struct),pointer     :: obj       ! arguments

      type(workfile_struct),pointer     :: previous  ! local

      nullify (previous) ! jpa
      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_workfile)) exit
        call workfile_get_previous (obj%last_workfile,previous)
        call workfile_delete (obj%last_workfile)
        obj%last_workfile => previous
      enddo
      deallocate(obj)

      return
      end subroutine worklist_delete


!!--------------------------- delete_workfile ------------------------------!!
!!--------------------------- delete_workfile ------------------------------!!
!!--------------------------- delete_workfile ------------------------------!!


      subroutine worklist_delete_workfile (worklist_obj,workfile_obj)
      implicit none
      type(worklist_struct),pointer                  :: worklist_obj  ! argument
      type(workfile_struct),pointer                  :: workfile_obj  ! argument

      type(workfile_struct),pointer                   :: previous      ! local
      type(workfile_struct),pointer                   :: next          ! local

      nullify(previous)
      nullify(next)

      call workfile_get_previous (workfile_obj,previous)
      call workfile_get_next     (workfile_obj,next)

      if (.not. associated(previous)) then
        if (associated(next)) then
          worklist_obj%first_workfile =>  next
        else
          nullify(worklist_obj%first_workfile)
        endif
      else if (.not. associated(next)) then
        if (associated(previous)) then
          worklist_obj%last_workfile => previous
        else
          nullify(worklist_obj%last_workfile)
        endif
      endif

      call workfile_delete (workfile_obj)

      return
      end subroutine worklist_delete_workfile


!!--------------------------- replace_workfile -----------------------------!!
!!--------------------------- replace_workfile -----------------------------!!
!!--------------------------- replace_workfile -----------------------------!!


      subroutine worklist_replace_workfile (worklist_obj,new_obj,old_obj)
      implicit none
      type(worklist_struct),pointer                  :: worklist_obj  ! argument
      type(workfile_struct),pointer                  :: new_obj       ! argument
      type(workfile_struct),pointer                  :: old_obj       ! argument

      type(workfile_struct),pointer                  :: previous      ! local
      type(workfile_struct),pointer                  :: next          ! local

      nullify(previous)
      nullify(next)

      call workfile_get_previous (old_obj,previous)
      call workfile_get_next     (old_obj,next)

      call workfile_set_previous (new_obj,previous)
      call workfile_set_next     (new_obj,next)

      call workfile_delete (old_obj)

      if (.not. associated(worklist_obj%first_workfile)) then
        worklist_obj%first_workfile => new_obj
      endif
      if (.not. associated(worklist_obj%last_workfile)) then
        worklist_obj%last_workfile => new_obj
      endif

      return
      end subroutine worklist_replace_workfile


!!---------------------------- insert_workfile -----------------------------!!
!!---------------------------- insert_workfile -----------------------------!!
!!---------------------------- insert_workfile -----------------------------!!


      subroutine worklist_insert_workfile (worklist_obj,new_obj,after_obj)
      implicit none
      type(worklist_struct),pointer                  :: worklist_obj  ! argument
      type(workfile_struct),pointer                  :: new_obj       ! argument
      type(workfile_struct),pointer                  :: after_obj     ! argument

      type(workfile_struct),pointer                  :: previous      ! local
      type(workfile_struct),pointer                  :: next          ! local

      nullify(previous)
      nullify(next)

      call workfile_get_previous (after_obj,previous)
      call workfile_get_next     (after_obj,next)


      call workfile_set_next     (after_obj,new_obj)
      call workfile_set_previous (new_obj,after_obj)
      call workfile_set_next     (new_obj,next)
      call workfile_set_previous (next,new_obj)

      if (.not. associated(next)) worklist_obj%last_workfile => new_obj

      return
      end subroutine worklist_insert_workfile


!!-------------------------- set_first_workfile ----------------------------!!
!!-------------------------- set_first_workfile ----------------------------!!
!!-------------------------- set_first_workfile ----------------------------!!


      subroutine worklist_set_first_workfile (obj,first_workfile)
      implicit none
      type(worklist_struct),pointer                 :: obj            ! argument
      type(workfile_struct),pointer                 :: first_workfile ! argument

      if (.not. associated(obj)) return

      if (associated(first_workfile)) then
        obj%first_workfile => first_workfile
      else
        nullify(obj%first_workfile)
      endif
 
      return
      end subroutine worklist_set_first_workfile


!!--------------------------- set_last_workfile ----------------------------!!
!!--------------------------- set_last_workfile ----------------------------!!
!!--------------------------- set_last_workfile ----------------------------!!


      subroutine worklist_set_last_workfile (obj,last_workfile)
      implicit none
      type(worklist_struct),pointer                 :: obj            ! argument
      type(workfile_struct),pointer                 :: last_workfile  ! argument

      if (.not. associated(obj)) return

      if (associated(last_workfile)) then
        obj%last_workfile => last_workfile
      else
        nullify(obj%last_workfile)
      endif
 
      return
      end subroutine worklist_set_last_workfile


!!--------------------------- set_num_workfiles ----------------------------!!
!!--------------------------- set_num_workfiles ----------------------------!!
!!--------------------------- set_num_workfiles ----------------------------!!


      subroutine worklist_set_num_workfiles (obj,num_workfiles)
      implicit none
      type(worklist_struct),pointer                 :: obj            ! argument
      integer,intent(in)                            :: num_workfiles  ! argument

      if (.not. associated(obj)) return

      obj%num_workfiles = num_workfiles
 
      return
      end subroutine worklist_set_num_workfiles


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine worklist_set_previous (obj,previous)
      implicit none
      type(worklist_struct),pointer                  :: obj           ! argument
      type(worklist_struct),pointer                  :: previous      ! argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif
 
      return
      end subroutine worklist_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine worklist_set_next (obj,next)
      implicit none
      type(worklist_struct),pointer                  :: obj           ! argument
      type(worklist_struct),pointer                  :: next          ! argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif
 
      return
      end subroutine worklist_set_next


!!-------------------------- get_first_workfile ----------------------------!!
!!-------------------------- get_first_workfile ----------------------------!!
!!-------------------------- get_first_workfile ----------------------------!!


      subroutine worklist_get_first_workfile (obj,first_workfile)
      implicit none
      type(worklist_struct),pointer                 :: obj            ! argument
      type(workfile_struct),pointer                 :: first_workfile ! argument

      if (associated(obj)) then
        if (associated(obj%first_workfile)) then
          first_workfile => obj%first_workfile
        else
          nullify(first_workfile)
        endif
      else
        nullify(first_workfile)
      endif
 
      return
      end subroutine worklist_get_first_workfile


!!--------------------------- get_last_workfile ----------------------------!!
!!--------------------------- get_last_workfile ----------------------------!!
!!--------------------------- get_last_workfile ----------------------------!!


      subroutine worklist_get_last_workfile (obj,last_workfile)
      implicit none
      type(worklist_struct),pointer                 :: obj            ! argument
      type(workfile_struct),pointer                 :: last_workfile  ! argument

      if (associated(obj)) then
        if (associated(obj%last_workfile)) then
          last_workfile => obj%last_workfile
        else
          nullify(last_workfile)
        endif
      else
        nullify(last_workfile)
      endif
 
      return
      end subroutine worklist_get_last_workfile


!!--------------------------- get_num_workfiles ----------------------------!!
!!--------------------------- get_num_workfiles ----------------------------!!
!!--------------------------- get_num_workfiles ----------------------------!!


      subroutine worklist_get_num_workfiles (obj,num_workfiles)
      implicit none
      type(worklist_struct),pointer                  :: obj           ! argument
      integer,intent(out)                            :: num_workfiles ! argument

      if (associated(obj)) then
        num_workfiles = obj%num_workfiles
      else
        num_workfiles = 0
      endif
 
      return
      end subroutine worklist_get_num_workfiles


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine worklist_get_previous (obj,previous)
      implicit none
      type(worklist_struct),pointer                  :: obj           ! argument
      type(worklist_struct),pointer                  :: previous      ! argument

      if (associated(obj)) then
        if (associated(obj%previous)) then
          previous => obj%previous
        else
          nullify(previous)
        endif
      else
        nullify(previous)
      endif
 
      return
      end subroutine worklist_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine worklist_get_next (obj,next)
      implicit none
      type(worklist_struct),pointer                  :: obj           ! argument
      type(worklist_struct),pointer                  :: next          ! argument

      if (associated(obj)) then
        if (associated(obj%next)) then
          next => obj%next
        else
          nullify(next)
        endif
      else
        nullify(next)
      endif
 
      return
      end subroutine worklist_get_next


!!------------------------------ get_list ----------------------------------!!
!!------------------------------ get_list ----------------------------------!!
!!------------------------------ get_list ----------------------------------!!


      subroutine worklist_get_list (obj,list,nlist)
      implicit none
      type(worklist_struct),pointer                     :: obj       ! argument
      character(len=WORKFILE_NAME_LEN),pointer          :: list(:)   ! argument
      integer                                           :: nlist     ! argument

      type(workfile_struct),pointer                     :: current   ! local
      type(workfile_struct),pointer                     :: next      ! local
      integer                                           :: i         ! local

      nullify (next) ! jpa
      if (associated(list)) deallocate(list)
      nlist = obj%num_workfiles
      allocate(list(nlist))

      current => obj%first_workfile
      do i = 1, obj%num_workfiles
        if (.not. associated(current)) exit
        call workfile_get_name (current, list(i))
!!!     call workfile_get_next (current,current)
        call workfile_get_next (current,next)
        current => next
      enddo

      return
      end subroutine worklist_get_list


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module worklist_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

