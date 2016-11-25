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
! Name       : ciu_window 
! Category   : stand-alone
! Written    : 2002-04-23   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : CIU Windows Module.
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
!  2. 2002-10-09  Vunderink    Modified to use new prog_module layer.
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


      module ciu_window_module

      use pc_module
      use cardset_module
      use filebox_module
      use prog_module
      use window_module
      use windowlist_module

      implicit none

      private
      public :: ciu_window_clear
      public :: ciu_window_create
      public :: ciu_window_delete
      public :: ciu_window_create_child
      public :: ciu_window_get_window_id
      public :: ciu_window_get_window_type
      public :: ciu_window_get_keyword
      public :: ciu_window_get_value
      public :: ciu_window_get_index
      public :: ciu_window_get_prog
      public :: ciu_window_get_filebox
      public :: ciu_window_get_cardset
      public :: ciu_window_get_previous
      public :: ciu_window_get_next
      public :: ciu_window_get_pointer
      public :: ciu_window_get_current
      public :: ciu_window_get_parent
      public :: ciu_window_alloc_keys
      public :: ciu_window_alloc_gui_cards
      public :: ciu_window_set_keys
      public :: ciu_window_set_window_id
      public :: ciu_window_set_window_type
      public :: ciu_window_set_keyword
      public :: ciu_window_set_value
      public :: ciu_window_set_index
      public :: ciu_window_set_prog
      public :: ciu_window_set_filebox
      public :: ciu_window_set_cardset
      public :: ciu_window_set_previous
      public :: ciu_window_set_next
      public :: ciu_window_set_current
      public :: ciu_window_match
      public :: ciu_window_get_all_of_type

      character(len=100),public,save :: ciu_window_ident = &
       '$Id: ciu_window.f90,v 1.2 2002/10/09 19:02:33 Vunderink prod sps $'

      type,public :: ciu_window_struct
        private
        type(window_struct),pointer                  :: window
        type(windowlist_struct),pointer              :: children
        type(ciu_window_struct),pointer              :: parent
        type(ciu_window_struct),pointer              :: previous
        type(ciu_window_struct),pointer              :: next
      end type ciu_window_struct

      integer,parameter                              :: WINDOW_UESED_INC   = 100
      integer,public,parameter                       :: HELP_WINDOW        = 101
      integer,public,parameter                       :: VERSION_WINDOW     = 102

      integer                        ,save           :: nwindows_used
      integer                ,pointer,save           :: windows_used(:)
      type(ciu_window_struct),pointer,save           :: ciu_window_list_first
      type(ciu_window_struct),pointer,save           :: ciu_window_list_last
      type(ciu_window_struct),pointer,save           :: ciu_window_current

      contains


!!-------------------------------- clear ----------------------------------!!
!!-------------------------------- clear ----------------------------------!!
!!-------------------------------- clear ----------------------------------!!


      subroutine ciu_window_clear

      nwindows_used = WINDOW_UESED_INC
      allocate(windows_used(nwindows_used))
      windows_used = -1
      nullify(ciu_window_list_first)
      nullify(ciu_window_list_last)
      nullify(ciu_window_current)

      return
      end subroutine ciu_window_clear


!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!


      subroutine ciu_window_create (obj)

      type(ciu_window_struct),pointer                 :: obj           !argument

      integer                                         :: i             !local
      integer                                         :: window_id     !local
      integer,allocatable                             :: temp(:)       !local

      do i=1,nwindows_used
        if (windows_used(i) .eq. -1) exit
      enddo
      window_id = i
      if (i .gt. nwindows_used) then
        allocate(temp(nwindows_used))
        do i=1,nwindows_used
          temp(i) = windows_used(i)
        enddo
        deallocate(windows_used)
        allocate(windows_used(nwindows_used+WINDOW_UESED_INC))
        windows_used = -1
        do i=1,nwindows_used
          windows_used(i) = temp(i)
        enddo
        nwindows_used = nwindows_used+WINDOW_UESED_INC
        deallocate(temp)
      endif
      windows_used(i) = 1

      allocate(obj)

      call window_create (obj%window)
      call window_set_window_id (obj%window,window_id)
      if (associated(ciu_window_current)) then
        obj%parent => ciu_window_current
      else
        nullify(obj%parent)
      endif
      nullify(obj%children)
      nullify(obj%previous)
      nullify(obj%next)

      if (.not. associated(ciu_window_list_first)) then
        ciu_window_list_first => obj
        ciu_window_list_last  => obj
      else
        ciu_window_list_last%next => obj
        obj%previous   => ciu_window_list_last
        ciu_window_list_last      => obj
      endif

      return
      end subroutine ciu_window_create


!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!


      subroutine ciu_window_delete (obj)

      type(ciu_window_struct),pointer                 :: obj           !argument

      integer                                         :: window_id     !local
      integer                                         :: current_id    !local
      type(ciu_window_struct),pointer                 :: wtemp         !local

      if (.not. associated(obj)) return

      if (associated(ciu_window_current)) then
        call window_get_window_id (ciu_window_current%window,current_id)
        call window_get_window_id (obj%window,window_id)
        if (window_id .eq. current_id) then
          ciu_window_current => obj%parent
        else
!         nullify(ciu_window_current)
        endif
      endif

      call window_delete (obj%window)

      if (associated(obj%previous).and.associated(obj%next)) then
        obj%previous%next => obj%next
        obj%next%previous => obj%previous
      else if (.not. associated(obj%previous)) then
        if (associated(obj%next)) then
          ciu_window_list_first =>  obj%next
          nullify(obj%next%previous)
        else
          nullify(ciu_window_list_first)
        endif
      else if (.not. associated(obj%next)) then
        if (associated(obj%previous)) then
          ciu_window_list_last => obj%previous
          nullify(obj%previous%next)
        else
          nullify(ciu_window_list_last)
        endif
      endif

      deallocate(obj)

      return
      end subroutine ciu_window_delete


!!--------------------------- create_child --------------------------------!!
!!--------------------------- create_child --------------------------------!!
!!--------------------------- create_child --------------------------------!!


      subroutine ciu_window_create_child (obj,child)

      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: child         !argument

      integer                                         :: i             !local
      integer                                         :: window_id     !local
      integer,allocatable                             :: temp(:)       !local

      if (.not. associated(obj)) then
        nullify(child)
        return
      endif

      do i=1,nwindows_used
        if (windows_used(i) .eq. -1) exit
      enddo
      window_id = i
      if (i .gt. nwindows_used) then
        allocate(temp(nwindows_used))
        do i=1,nwindows_used
          temp(i) = windows_used(i)
        enddo
        deallocate(windows_used)
        allocate(windows_used(nwindows_used+WINDOW_UESED_INC))
        windows_used = -1
        do i=1,nwindows_used
          windows_used(i) = temp(i)
        enddo
        nwindows_used = nwindows_used+WINDOW_UESED_INC
        deallocate(temp)
      endif
      windows_used(i) = 1

      allocate(child)

      call window_duplicate (obj%window,child%window)
      call window_set_window_id (child%window,window_id)
      child%parent => obj
      if (.not. associated(child%parent%children)) then
        call windowlist_create     (child%parent%children)
        call windowlist_add_window (child%parent%children,child%window)
      endif
      nullify(child%children)
      nullify(child%previous)
      nullify(child%next)

      if (.not. associated(ciu_window_list_first)) then
        ciu_window_list_first => child
        ciu_window_list_last  => child
      else
        ciu_window_list_last%next => child
        child%previous            => ciu_window_list_last
        ciu_window_list_last      => child
      endif

      return
      end subroutine ciu_window_create_child


!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!


      subroutine ciu_window_get_window_id (obj,window_id)

      type(ciu_window_struct),pointer                 :: obj           !argument
      integer,intent(out)                             :: window_id     !argument

      if (associated(obj)) then
        call window_get_window_id (obj%window,window_id)
      else
        window_id = 0
      endif

      return
      end subroutine ciu_window_get_window_id


!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!


      subroutine ciu_window_get_window_type (obj,window_type)

      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=*),intent(out)                    :: window_type   !argument

      if (associated(obj)) then
        call window_get_window_type (obj%window,window_type)
      else
        window_type = ' '
      endif

      return
      end subroutine ciu_window_get_window_type


!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!


      subroutine ciu_window_get_keyword (obj,keyword)

      type(ciu_window_struct),pointer                :: obj            !argument
      character(len=*),intent(out)                   :: keyword        !argument

      if (associated(obj)) then
        call window_get_keyword (obj%window,keyword)
      else
        keyword = ' '
      endif

      return
      end subroutine ciu_window_get_keyword


!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!


      subroutine ciu_window_get_value (obj,value)

      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=*),intent(out)                    :: value         !argument

      if (associated(obj)) then
        call window_get_value (obj%window,value)
      else
        value = ' '
      endif

      return
      end subroutine ciu_window_get_value


!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!


      subroutine ciu_window_get_index (obj,index)

      type(ciu_window_struct),pointer                 :: obj           !argument
      integer,intent(out)                             :: index         !argument

      if (associated(obj)) then
        call window_get_index (obj%window,index)
      else
        index = 0
      endif

      return
      end subroutine ciu_window_get_index


!!----------------------------- get_pointer -------------------------------!!
!!----------------------------- get_pointer -------------------------------!!
!!----------------------------- get_pointer -------------------------------!!


      subroutine ciu_window_get_pointer (window_id,obj)

      integer,intent(in)                              :: window_id     !argument
      type(ciu_window_struct),pointer                 :: obj           !argument

      integer                                         :: temp_id       !local

      obj => ciu_window_list_first
      do
        if (.not. associated(obj)) exit
        call window_get_window_id (obj%window,temp_id)
        if (window_id .eq. temp_id) exit
        obj => obj%next
      enddo

      return
      end subroutine ciu_window_get_pointer


!!----------------------------- get_prog -------------------------------!!
!!----------------------------- get_prog -------------------------------!!
!!----------------------------- get_prog -------------------------------!!


      subroutine ciu_window_get_prog (obj, prog)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(prog_struct)   ,pointer                  :: prog        !argument

      if (associated(obj)) then
        call window_get_prog (obj%window,prog)
      else
        nullify(prog)
      endif

      return
      end subroutine ciu_window_get_prog


!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!


      subroutine ciu_window_get_filebox (obj, filebox)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(filebox_struct)   ,pointer                 :: filebox       !argument

      if (associated(obj)) then
        call window_get_filebox (obj%window,filebox)
      else
        nullify(filebox)
      endif

      return
      end subroutine ciu_window_get_filebox


!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!


      subroutine ciu_window_alloc_gui_cards (obj, cards, ncards)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=*)       ,pointer                 :: cards(:)      !argument
      integer                ,intent(out)             :: ncards        !argument

      type(prog_struct)   ,pointer                  :: prog        !local

      if (associated(obj)) then
        call window_get_prog (obj%window,prog)
        call prog_alloc_gui_cards (prog,cards,ncards)
      else
        ncards = 0
        if (associated(cards)) deallocate(cards)
      endif

      return
      end subroutine ciu_window_alloc_gui_cards


!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!


      subroutine ciu_window_get_cardset (obj, cardset)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (associated(obj)) then
        call window_get_cardset (obj%window,cardset)
      else
        nullify(cardset)
      endif

      return
      end subroutine ciu_window_get_cardset


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine ciu_window_get_previous (obj, previous)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: previous      !argument

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
      end subroutine ciu_window_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine ciu_window_get_next (obj, next)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: next          !argument

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
      end subroutine ciu_window_get_next


!!----------------------------- get_current -------------------------------!!
!!----------------------------- get_current -------------------------------!!
!!----------------------------- get_current -------------------------------!!


      subroutine ciu_window_get_current (current)
      implicit none
      type(ciu_window_struct),pointer                 :: current       !argument

      if (associated(ciu_window_current)) then
        current => ciu_window_current
      else
        nullify(current)
      endif

      return
      end subroutine ciu_window_get_current


!!----------------------------- get_parent --------------------------------!!
!!----------------------------- get_parent --------------------------------!!
!!----------------------------- get_parent --------------------------------!!


      subroutine ciu_window_get_parent (obj,parent)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: parent        !argument

      if (associated(obj)) then
        if (associated(obj%parent)) then
          parent => obj%parent
        else
          nullify(parent)
        endif
      else
        nullify(parent)
      endif

      return
      end subroutine ciu_window_get_parent


!!----------------------------- alloc_keys --------------------------------!!
!!----------------------------- alloc_keys --------------------------------!!
!!----------------------------- alloc_keys --------------------------------!!


      subroutine ciu_window_alloc_keys (obj,keys,nkeys)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=PC_LENGTH) ,pointer               :: keys(:)       !argument
      integer,intent(out)                             :: nkeys         !argument

      if (associated(obj)) then
        call window_alloc_keys (obj%window,keys,nkeys)
      else
        if (associated(keys)) deallocate(keys)
        nkeys = 0
      endif

      return
      end subroutine ciu_window_alloc_keys


!!------------------------------ set_keys ---------------------------------!!
!!------------------------------ set_keys ---------------------------------!!
!!------------------------------ set_keys ---------------------------------!!


      subroutine ciu_window_set_keys (obj,keys,nkeys)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=PC_LENGTH) ,pointer               :: keys(:)       !argument
      integer,intent(in)                              :: nkeys         !argument

      if (.not. associated(obj)) return

      call window_set_keys (obj%window,keys,nkeys)

      return
      end subroutine ciu_window_set_keys


!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!


      subroutine ciu_window_set_window_id (obj,window_id)

      type(ciu_window_struct),pointer                 :: obj           !argument
      integer,intent(in)                              :: window_id     !argument

      if (.not. associated(obj)) return

      call window_set_window_id (obj%window,window_id)

      return
      end subroutine ciu_window_set_window_id


!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!


      subroutine ciu_window_set_window_type (obj,window_type)

      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=*),intent(in)                     :: window_type   !argument

      if (.not. associated(obj)) return

      call window_set_window_type (obj%window,window_type)

      return
      end subroutine ciu_window_set_window_type


!!------------------------------ set_keyword ------------------------------!!
!!------------------------------ set_keyword ------------------------------!!
!!------------------------------ set_keyword ------------------------------!!


      subroutine ciu_window_set_keyword (obj,keyword)

      type(ciu_window_struct),pointer                :: obj            !argument
      character(len=*),intent(in)                    :: keyword        !argument

      if (.not. associated(obj)) return

      call window_set_keyword (obj%window,keyword)

      return
      end subroutine ciu_window_set_keyword


!!------------------------------ set_value --------------------------------!!
!!------------------------------ set_value --------------------------------!!
!!------------------------------ set_value --------------------------------!!


      subroutine ciu_window_set_value (obj,value)

      type(ciu_window_struct),pointer                 :: obj           !argument
      character(len=*),intent(in)                     :: value         !argument

      if (.not. associated(obj)) return

      call window_set_value (obj%window,value)

      return
      end subroutine ciu_window_set_value


!!------------------------------ set_index --------------------------------!!
!!------------------------------ set_index --------------------------------!!
!!------------------------------ set_index --------------------------------!!


      subroutine ciu_window_set_index (obj,index)

      type(ciu_window_struct),pointer                 :: obj           !argument
      integer,intent(in)                              :: index         !argument

      if (.not. associated(obj)) return

      call window_set_index (obj%window,index)

      return
      end subroutine ciu_window_set_index


!!----------------------------- set_prog -------------------------------!!
!!----------------------------- set_prog -------------------------------!!
!!----------------------------- set_prog -------------------------------!!


      subroutine ciu_window_set_prog (obj, prog)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(prog_struct)   ,pointer                  :: prog        !argument

      if (.not. associated(obj)) return

      call window_set_prog (obj%window,prog)

      return
      end subroutine ciu_window_set_prog


!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!


      subroutine ciu_window_set_filebox (obj, filebox)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(filebox_struct)   ,pointer                 :: filebox       !argument

      if (.not. associated(obj)) return

      call window_set_filebox (obj%window,filebox)

      return
      end subroutine ciu_window_set_filebox


!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!


      subroutine ciu_window_set_cardset (obj, cardset)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (.not. associated(obj)) return

      call window_set_cardset (obj%window,cardset)

      return
      end subroutine ciu_window_set_cardset


!!------------------------------ set_parent -------------------------------!!
!!------------------------------ set_parent -------------------------------!!
!!------------------------------ set_parent -------------------------------!!


      subroutine ciu_window_set_parent (obj,parent)

      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: parent        !argument

      if (.not. associated(obj)) return

      obj%parent => parent

      return
      end subroutine ciu_window_set_parent


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine ciu_window_set_previous (obj, previous)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: previous      !argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine ciu_window_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine ciu_window_set_next (obj, next)
      implicit none
      type(ciu_window_struct),pointer                 :: obj           !argument
      type(ciu_window_struct),pointer                 :: next          !argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif

      return
      end subroutine ciu_window_set_next


!!----------------------------- set_current -------------------------------!!
!!----------------------------- set_current -------------------------------!!
!!----------------------------- set_current -------------------------------!!


      subroutine ciu_window_set_current (current)
      implicit none
      type(ciu_window_struct),pointer                 :: current       !argument

      if (associated(current)) then
        ciu_window_current => current
      else
        nullify(ciu_window_current)
      endif

      return
      end subroutine ciu_window_set_current


!!-------------------------------- match ----------------------------------!!
!!-------------------------------- match ----------------------------------!!
!!-------------------------------- match ----------------------------------!!


      function ciu_window_match (window_type,keyword,value,index)   &
                                                             result (window_id)
      implicit none
      character(len=*),intent(in)                     :: window_type   !argument
      character(len=*),intent(in)                     :: keyword       !argument
      character(len=*),intent(in)                     :: value         !argument
      integer         ,intent(in)                     :: index         !argument
      integer                                         :: window_id     !result

      type(ciu_window_struct),pointer                 :: obj           !local
      character(len=PC_LENGTH)                        :: temp_type     !local
      character(len=PC_LENGTH)                        :: temp_keyword  !local
      character(len=PC_LENGTH)                        :: temp_value    !local
      integer                                         :: temp_index    !local

      window_id = -1

      obj => ciu_window_list_first
      do
        if (.not. associated(obj)) exit
        call window_get_window_type (obj%window,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) then
          call window_get_keyword (obj%window,temp_keyword)
          if (trim(keyword) .eq. trim(temp_keyword)) then 
            call window_get_value (obj%window,temp_value)
            if (trim(value) .eq. trim(temp_value)) then
              call window_get_index (obj%window,temp_index)
              if (index .eq. temp_index) then
                call window_get_window_id   (obj%window,window_id)
                exit
              endif
            endif
          endif
        endif
        obj => obj%next
      enddo

      return
      end function ciu_window_match


!!--------------------------- get_all_of_type -----------------------------!!
!!--------------------------- get_all_of_type -----------------------------!!
!!--------------------------- get_all_of_type -----------------------------!!


      subroutine ciu_window_get_all_of_type (window_type,window_ids)
      implicit none
      character(len=*),intent(in)                     :: window_type   !argument
      integer,pointer                                 :: window_ids(:) !local

      type(ciu_window_struct),pointer                 :: obj           !local
      character(len=PC_LENGTH)                        :: temp_type     !local
      integer                                         :: count         !local

      count = 0
      if (associated(window_ids)) deallocate(window_ids)

      obj => ciu_window_list_first
      do
        if (.not. associated(obj)) exit
        call window_get_window_type (obj%window,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) count = count + 1
        obj => obj%next
      enddo

      if (count .eq. 0) return

      allocate(window_ids(count))

      count = 0
      obj => ciu_window_list_first
      do
        if (.not. associated(obj)) exit
        call window_get_window_type (obj%window,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) then
          count = count + 1
          call window_get_window_id (obj%window,window_ids(count))
        endif
        obj => obj%next
      enddo

      return
      end subroutine ciu_window_get_all_of_type


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module ciu_window_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

