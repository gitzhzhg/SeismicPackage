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
! Name       : cfelistbuffer 
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : List Buffer Object Module.
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
!008. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!  7. 2003-11-18  Stoeckley    Provide workaround for Portland Group compiler.
!  6. 2003-09-15  Stoeckley    Changed name from listbuffer to cfelistbuffer;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
!  5. 2002-01-04  Vunderink    Changed name of this module from mwb_buffer to
!                                listbuffer and made mwb_list to list name
!                                changes.
!  4. 2001-01-07  Vunderink    Set the number of list to zero in clear.
!  3. 2000-12-19  Vunderink    Enhanced get_names to check if obj is associated,
!                                removed unused routine replace_list and
!                                modified get_last_list to insert list
!                                alphabetically.
!  2. 2000-12-05  Vunderink    Added the routine clear, and fixed bugs in
!                                delete_list, insert_first_list, set_first_list 
!                                and set_last_list
!  1. 2000-08-15  Vunderink    Initial version.
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

      module cfelistbuffer_module

      use pc_module
      use cfelist_module

      implicit none

      private
      public :: cfelistbuffer_create
      public :: cfelistbuffer_clear
      public :: cfelistbuffer_delete
      public :: cfelistbuffer_delete_list
      public :: cfelistbuffer_insert_list
      public :: cfelistbuffer_insert_first_list
      public :: cfelistbuffer_set_first_list
      public :: cfelistbuffer_set_last_list
      public :: cfelistbuffer_set_num_lists
      public :: cfelistbuffer_get_first_list
      public :: cfelistbuffer_get_last_list
      public :: cfelistbuffer_get_num_lists
      public :: cfelistbuffer_get_names

      character(len=100),public,save :: cfelistbuffer_ident = &
       '$Id: cfelistbuffer.f90,v 1.8 2006/09/18 13:32:41 Glover prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: cfelistbuffer_struct              
        private
        integer                                :: num_lists
        type(cfelist_struct),pointer          :: first_list
        type(cfelist_struct),pointer          :: last_list
      end type cfelistbuffer_struct


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine cfelistbuffer_create (obj)
      implicit none
      type(cfelistbuffer_struct),pointer :: obj       ! arguments

      allocate (obj)
      obj%num_lists = 0
      nullify(obj%first_list)
      nullify(obj%last_list)

      return
      end subroutine cfelistbuffer_create


!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!


      subroutine cfelistbuffer_clear (obj)
      implicit none
      type(cfelistbuffer_struct),pointer :: obj       ! arguments

      type(cfelist_struct),pointer       :: previous  ! local

      nullify (previous) ! jpa

      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_list)) exit
        call cfelist_get_previous (obj%last_list,previous)
        call cfelist_delete (obj%last_list)
        obj%last_list => previous
      enddo
      obj%num_lists = 0
      nullify(obj%first_list)
      nullify(obj%last_list)

      return
      end subroutine cfelistbuffer_clear


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine cfelistbuffer_delete (obj)
      implicit none
      type(cfelistbuffer_struct),pointer :: obj       ! arguments

      type(cfelist_struct),pointer       :: previous  ! local

      nullify (previous) ! jpa

      if (.not. associated(obj)) return

      do
        if (.not. associated(obj%last_list)) exit
        call cfelist_get_previous (obj%last_list,previous)
        call cfelist_delete (obj%last_list)
        obj%last_list => previous
      enddo

      deallocate(obj)

      return
      end subroutine cfelistbuffer_delete


!!----------------------------- delete_list -------------------------------!!
!!----------------------------- delete_list -------------------------------!!
!!----------------------------- delete_list -------------------------------!!


      subroutine cfelistbuffer_delete_list (obj,list_obj)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: list_obj      ! argument

      type(cfelist_struct),pointer                  :: previous      ! local
      type(cfelist_struct),pointer                  :: next          ! local

      nullify(previous)
      nullify(next)

      call cfelist_get_previous (list_obj,previous)
      call cfelist_get_next     (list_obj,next)

      call cfelist_set_next     (previous,next)
      call cfelist_set_previous (next    ,previous)

      if (.not. associated(previous)) then
        if (associated(next)) then
          obj%first_list =>  next
        else
          nullify(obj%first_list)
        endif
      endif
      if (.not. associated(next)) then
        if (associated(previous)) then
          obj%last_list => previous
        else
          nullify(obj%last_list)
        endif
      endif

      call cfelist_delete (list_obj)

      obj%num_lists = obj%num_lists - 1

      return
      end subroutine cfelistbuffer_delete_list


!!------------------------------ insert_list ------------------------------!!
!!------------------------------ insert_list ------------------------------!!
!!------------------------------ insert_list ------------------------------!!


      subroutine cfelistbuffer_insert_list (obj,new_obj)
      implicit none
      type(cfelistbuffer_struct),pointer         :: obj              ! argument
      type(cfelist_struct),pointer               :: new_obj          ! argument

      character(len=PC_LENGTH)                    :: new_listname     ! local
      character(len=PC_LENGTH)                    :: current_listname ! local
      type(cfelist_struct),pointer               :: current          ! local
      type(cfelist_struct),pointer               :: next             ! local
      type(cfelist_struct),pointer               :: previous         ! local

      nullify (previous) ! jpa
      nullify (next)

      call cfelist_get_name (new_obj,new_listname)

      current => obj%first_list
      do
        if (.not. associated(current)) exit
        call cfelist_get_name (current,current_listname)
        if (trim(new_listname) .lt. trim(current_listname)) exit
!!!     call cfelist_get_next (current,current)
        call cfelist_get_next (current,next)
        current => next
      enddo
      if (associated(current)) then
         call cfelist_get_previous (current,previous)
         if (associated(previous)) then
           call cfelist_set_next     (previous,new_obj)
           call cfelist_set_next     (new_obj ,current)
           call cfelist_set_previous (new_obj ,previous)
           call cfelist_set_previous (current ,new_obj)
         else
          call cfelist_set_previous (obj%first_list, new_obj)
          nullify(previous)
          call cfelist_set_previous (new_obj       ,previous)
          call cfelist_set_next     (new_obj       ,obj%first_list)
          obj%first_list => new_obj
         endif
      else
        call cfelist_set_next     (obj%last_list,new_obj)
        nullify(next)
        call cfelist_set_previous (new_obj      ,obj%last_list)
        call cfelist_set_next     (new_obj      ,next)
        obj%last_list => new_obj
      endif

      obj%num_lists = obj%num_lists + 1

      return
      end subroutine cfelistbuffer_insert_list


!!--------------------------- insert_first_list ---------------------------!!
!!--------------------------- insert_first_list ---------------------------!!
!!--------------------------- insert_first_list ---------------------------!!


      subroutine cfelistbuffer_insert_first_list (obj,new_obj)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: new_obj       ! argument

      type(cfelist_struct),pointer                  :: old_first     ! local

      nullify (old_first) ! jpa

      if (.not. associated(obj)) return

      call cfelistbuffer_get_first_list (obj,old_first)
      if (associated(old_first)) then
        call cfelist_set_previous (old_first,new_obj )
        call cfelist_set_next     (new_obj  ,old_first)
        obj%num_lists = obj%num_lists + 1
      endif
      call cfelistbuffer_set_first_list (obj,new_obj)
      obj%num_lists = obj%num_lists + 1

      return
      end subroutine cfelistbuffer_insert_first_list


!!---------------------------- set_first_list -----------------------------!!
!!---------------------------- set_first_list -----------------------------!!
!!---------------------------- set_first_list -----------------------------!!


      subroutine cfelistbuffer_set_first_list (obj,first_list)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: first_list    ! argument

      type(cfelist_struct),pointer                 :: current,next   ! local

      nullify (next) ! jpa

      if (.not. associated(obj)) return

      if (associated(first_list)) then
        obj%first_list => first_list
      else
        nullify(obj%first_list)
      endif

      obj%num_lists = 0
      current => obj%first_list
      do
        if (.not. associated(current)) exit
        obj%num_lists = obj%num_lists + 1
!!!     call cfelist_get_next (current,current)
        call cfelist_get_next (current,next)
        current => next
      enddo
 
      return
      end subroutine cfelistbuffer_set_first_list


!!----------------------------- set_last_list -----------------------------!!
!!----------------------------- set_last_list -----------------------------!!
!!----------------------------- set_last_list -----------------------------!!


      subroutine cfelistbuffer_set_last_list (obj,last_list)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: last_list     ! argument

      type(cfelist_struct),pointer             :: current,previous   ! local

      nullify (previous) ! jpa

      if (.not. associated(obj)) return

      if (associated(last_list)) then
        obj%last_list => last_list
      else
        nullify(obj%last_list)
      endif
 

      obj%num_lists = 0
      current => obj%last_list
      do
        if (.not. associated(current)) exit
        obj%num_lists = obj%num_lists + 1
!!!     call cfelist_get_previous (current,current)
        call cfelist_get_previous (current,previous)
        current => previous
      enddo

      return
      end subroutine cfelistbuffer_set_last_list


!!----------------------------- set_num_lists -----------------------------!!
!!----------------------------- set_num_lists -----------------------------!!
!!----------------------------- set_num_lists -----------------------------!!


      subroutine cfelistbuffer_set_num_lists (obj,num_lists)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      integer,intent(in)                             :: num_lists     ! argument

      if (.not. associated(obj)) return

      obj%num_lists = num_lists
 
      return
      end subroutine cfelistbuffer_set_num_lists


!!---------------------------- get_first_list -----------------------------!!
!!---------------------------- get_first_list -----------------------------!!
!!---------------------------- get_first_list -----------------------------!!


      subroutine cfelistbuffer_get_first_list (obj,first_list)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: first_list    ! argument

      if (associated(obj)) then
        if (associated(obj%first_list)) then
          first_list => obj%first_list
        else
          nullify(first_list)
        endif
      else
        nullify(first_list)
      endif
 
      return
      end subroutine cfelistbuffer_get_first_list


!!----------------------------- get_last_list -----------------------------!!
!!----------------------------- get_last_list -----------------------------!!
!!----------------------------- get_last_list -----------------------------!!


      subroutine cfelistbuffer_get_last_list (obj,last_list)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      type(cfelist_struct),pointer                  :: last_list     ! argument

      if (associated(obj)) then
        if (associated(obj%last_list)) then
          last_list => obj%last_list
        else
          nullify(last_list)
        endif
      else
        nullify(last_list)
      endif
 
      return
      end subroutine cfelistbuffer_get_last_list


!!----------------------------- get_num_lists -----------------------------!!
!!----------------------------- get_num_lists -----------------------------!!
!!----------------------------- get_num_lists -----------------------------!!


      subroutine cfelistbuffer_get_num_lists (obj,num_lists)
      implicit none
      type(cfelistbuffer_struct),pointer            :: obj           ! argument
      integer,intent(out)                            :: num_lists     ! argument

      if (associated(obj)) then
        num_lists = obj%num_lists
      else
        num_lists = 0
      endif
 
      return
      end subroutine cfelistbuffer_get_num_lists


!!------------------------------ get_names --------------------------------!!
!!------------------------------ get_names --------------------------------!!
!!------------------------------ get_names --------------------------------!!


      subroutine cfelistbuffer_get_names (obj,list,nlist)
      implicit none
      type(cfelistbuffer_struct),pointer               :: obj       ! argument
      character(len=PC_LENGTH),pointer                 :: list(:)   ! argument
      integer                                          :: nlist     ! argument

      type(cfelist_struct),pointer                :: current,next   ! local
      integer                                          :: i         ! local

      nullify (next) ! jpa

      if (.not. associated(obj)) then
        nlist = 0
        return
      endif

      nlist = obj%num_lists
      if (associated(list)) deallocate(list)

      if (nlist .gt. 0) then
        allocate(list(nlist))

        current => obj%first_list
        do i = 1, obj%num_lists
          if (.not. associated(current)) exit
          call cfelist_get_name (current,list(i))
!!!       call cfelist_get_next (current,current)
          call cfelist_get_next (current,next)
          current => next
        enddo
      endif

      return
      end subroutine cfelistbuffer_get_names


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfelistbuffer_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

