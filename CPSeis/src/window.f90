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
! Name       : window 
! Category   : stand-alone
! Written    : 2002-04-23   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : CIU Window Object Module.
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

      module window_module
      use pc_module
      use cardset_module
      use prog_module
      use filebox_module
      use string_module

      implicit none

      private
      public :: window_create
      public :: window_delete
      public :: window_duplicate
      public :: window_get_window_id
      public :: window_get_window_type
      public :: window_get_keyword
      public :: window_get_value
      public :: window_get_index
      public :: window_get_prog
      public :: window_get_filebox
      public :: window_get_cardset
      public :: window_get_previous
      public :: window_get_next
      public :: window_alloc_keys
      public :: window_set_keys
      public :: window_set_window_id
      public :: window_set_window_type
      public :: window_set_keyword
      public :: window_set_value
      public :: window_set_index
      public :: window_set_prog
      public :: window_set_filebox
      public :: window_set_cardset
      public :: window_set_previous
      public :: window_set_next

      character(len=100),public,save :: window_ident = &
       '$Id: window.f90,v 1.2 2002/10/09 19:04:19 Vunderink prod sps $'

      type,public :: window_struct
        private
        integer                                      :: window_id
        character(len=PC_LENGTH)                     :: window_type
        character(len=PC_LENGTH)                     :: keyword
        character(len=PC_LENGTH)                     :: value
        integer                                      :: index
        integer                                      :: nkeys
        character(len=PC_LENGTH),pointer             :: keys(:)
        type(prog_struct),pointer                  :: prog
        type(filebox_struct),pointer                 :: filebox
        type(cardset_struct),pointer                 :: cardset
        type(window_struct),pointer                  :: previous
        type(window_struct),pointer                  :: next
      end type window_struct

      contains


!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!


      subroutine window_create (obj)

      type(window_struct),pointer                     :: obj           !argument


      allocate(obj)

      obj%window_id      = 0
      obj%window_type    = ' '
      obj%keyword        = ' '
      obj%value          = ' '
      obj%index          = 0
      obj%nkeys          = 0
      nullify(obj%keys)
      nullify(obj%prog)
      nullify(obj%filebox)
      call cardset_create (obj%cardset)
      nullify(obj%previous)
      nullify(obj%next)

      return
      end subroutine window_create


!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!


      subroutine window_delete (obj)

      type(window_struct),pointer                     :: obj           !argument

      type(window_struct),pointer                     :: wtemp         !local

      call cardset_delete (obj%cardset)
      if (associated(obj%previous).and.associated(obj%next)) then
        obj%previous%next => obj%next
        obj%next%previous => obj%previous
      else if (.not. associated(obj%previous)) then
        if (associated(obj%next)) nullify(obj%next%previous)
      else if (.not. associated(obj%next)) then
        if (associated(obj%previous)) nullify(obj%previous%next)
      endif
      if (associated(obj%keys)) deallocate(obj%keys)

      deallocate(obj)

      return
      end subroutine window_delete


!!----------------------------- duplicate ---------------------------------!!
!!----------------------------- duplicate ---------------------------------!!
!!----------------------------- duplicate ---------------------------------!!


      subroutine window_duplicate (obj,new)

      type(window_struct),pointer                     :: obj           !argument
      type(window_struct),pointer                     :: new           !argument

      allocate(new)

      new%window_id      =  obj%window_id
      new%window_type    =  obj%window_type
      new%keyword        =  obj%keyword
      new%value          =  obj%value
      new%index          =  obj%index
      new%prog        => obj%prog
      new%filebox        => obj%filebox  
      call cardset_create (new%cardset)
      if (associated(obj%keys)) then
        if (associated(new%keys)) deallocate(new%keys)
        new%nkeys = obj%nkeys
        allocate(new%keys(new%nkeys))
        new%keys = obj%keys
      else
        new%nkeys = 0
        if (associated(new%keys)) deallocate(new%keys)
      endif
        
      nullify(new%previous)
      nullify(new%next)

      return
      end subroutine window_duplicate


!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!


      subroutine window_get_window_id (obj,window_id)

      type(window_struct),pointer                     :: obj           !argument
      integer,intent(out)                             :: window_id     !argument

      if (associated(obj)) then
        window_id = obj%window_id
      else
        window_id = 0
      endif

      return
      end subroutine window_get_window_id


!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!


      subroutine window_get_window_type (obj,window_type)

      type(window_struct),pointer                     :: obj           !argument
      character(len=*),intent(out)                    :: window_type   !argument

      if (associated(obj)) then
        window_type = obj%window_type
      else
        window_type = ' '
      endif

      return
      end subroutine window_get_window_type


!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!


      subroutine window_get_keyword (obj,keyword)

      type(window_struct),pointer                    :: obj            !argument
      character(len=*),intent(out)                   :: keyword        !argument

      if (associated(obj)) then
        keyword = obj%keyword
      else
        keyword = ' '
      endif

      return
      end subroutine window_get_keyword


!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!


      subroutine window_get_value (obj,value)

      type(window_struct),pointer                     :: obj           !argument
      character(len=*),intent(out)                    :: value         !argument

      if (associated(obj)) then
        value = obj%value
      else
        value = ' '
      endif

      return
      end subroutine window_get_value


!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!


      subroutine window_get_index (obj,index)

      type(window_struct),pointer                     :: obj           !argument
      integer,intent(out)                             :: index         !argument

      if (associated(obj)) then
        index = obj%index
      else
        index = 0
      endif

      return
      end subroutine window_get_index


!!----------------------------- get_prog -------------------------------!!
!!----------------------------- get_prog -------------------------------!!
!!----------------------------- get_prog -------------------------------!!


      subroutine window_get_prog (obj, prog)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(prog_struct)   ,pointer                 :: prog       !argument

      if (associated(obj)) then
        if (associated(obj%prog)) then
          prog => obj%prog
        else
          nullify(prog)
        endif
      else
        nullify(prog)
      endif

      return
      end subroutine window_get_prog


!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!


      subroutine window_get_filebox (obj, filebox)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(filebox_struct),pointer                    :: filebox       !argument

      if (associated(obj)) then
        if (associated(obj%filebox)) then
          filebox => obj%filebox
        else
          nullify(filebox)
        endif
      else
        nullify(filebox)
      endif

      return
      end subroutine window_get_filebox


!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!


      subroutine window_get_cardset (obj, cardset)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (associated(obj)) then
        if (associated(obj%cardset)) then
          cardset => obj%cardset
        else
          nullify(cardset)
        endif
      else
        nullify(cardset)
      endif

      return
      end subroutine window_get_cardset


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine window_get_previous (obj, previous)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(window_struct),pointer                     :: previous      !argument

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
      end subroutine window_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine window_get_next (obj, next)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(window_struct),pointer                     :: next          !argument

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
      end subroutine window_get_next


!!---------------------------- alloc_keys ---------------------------------!!
!!---------------------------- alloc_keys ---------------------------------!!
!!---------------------------- alloc_keys ---------------------------------!!


      subroutine window_alloc_keys (obj,keys,nkeys)

      type(window_struct),pointer                     :: obj           !argument
      character(len=PC_LENGTH),pointer                :: keys(:)       !argument
      integer,intent(out)                             :: nkeys         !argument

      if (associated(obj)) then
        if (associated(keys)) deallocate(keys)
        nkeys = obj%nkeys
        if (nkeys .gt. 0) then
          allocate(keys(nkeys))
          keys = obj%keys
        endif
      else
        if (associated(keys)) deallocate(keys)
        nkeys = 0
      endif

      return
      end subroutine window_alloc_keys


!!----------------------------- set_keys ----------------------------------!!
!!----------------------------- set_keys ----------------------------------!!
!!----------------------------- set_keys ----------------------------------!!


      subroutine window_set_keys (obj,keys,nkeys)

      type(window_struct),pointer                     :: obj           !argument
      character(len=PC_LENGTH),pointer                :: keys(:)       !argument
      integer,intent(in)                              :: nkeys         !argument

      if (.not. associated(obj)) return

      if (associated(obj%keys)) deallocate(obj%keys)
      obj%nkeys = nkeys
      if (obj%nkeys .gt. 0) then
        allocate(obj%keys(obj%nkeys))
        obj%keys(1:obj%nkeys) = keys(1:nkeys)
      endif

      return
      end subroutine window_set_keys


!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!


      subroutine window_set_window_id (obj,window_id)

      type(window_struct),pointer                     :: obj           !argument
      integer,intent(in)                              :: window_id     !argument

      if (.not. associated(obj)) return

      obj%window_id = window_id

      return
      end subroutine window_set_window_id


!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!


      subroutine window_set_window_type (obj,window_type)

      type(window_struct),pointer                     :: obj           !argument
      character(len=*),intent(in)                     :: window_type   !argument

      if (.not. associated(obj)) return

      obj%window_type = trim(window_type)
      call string_to_upper(obj%window_type)

      return
      end subroutine window_set_window_type


!!----------------------------- set_keyword -------------------------------!!
!!----------------------------- set_keyword -------------------------------!!
!!----------------------------- set_keyword -------------------------------!!


      subroutine window_set_keyword (obj,keyword)

      type(window_struct),pointer                    :: obj            !argument
      character(len=*),intent(in)                    :: keyword        !argument

      if (.not. associated(obj)) return

      obj%keyword = keyword

      return
      end subroutine window_set_keyword


!!----------------------------- set_value ---------------------------------!!
!!----------------------------- set_value ---------------------------------!!
!!----------------------------- set_value ---------------------------------!!


      subroutine window_set_value (obj,value)

      type(window_struct),pointer                     :: obj           !argument
      character(len=*),intent(in)                     :: value         !argument

      if (.not. associated(obj)) return

      obj%value = value

      return
      end subroutine window_set_value


!!----------------------------- set_index ---------------------------------!!
!!----------------------------- set_index ---------------------------------!!
!!----------------------------- set_index ---------------------------------!!


      subroutine window_set_index (obj,index)

      type(window_struct),pointer                     :: obj           !argument
      integer,intent(in)                              :: index         !argument

      if (.not. associated(obj)) return

      obj%index = index

      return
      end subroutine window_set_index


!!----------------------------- set_prog -------------------------------!!
!!----------------------------- set_prog -------------------------------!!
!!----------------------------- set_prog -------------------------------!!


      subroutine window_set_prog (obj, prog)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(prog_struct)   ,pointer                 :: prog       !argument

      if (.not. associated(obj)) return

      if (associated(prog)) then
        obj%prog => prog
      else
        nullify(obj%prog)
      endif

      return
      end subroutine window_set_prog


!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!


      subroutine window_set_filebox (obj, filebox)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(filebox_struct),pointer                    :: filebox       !argument

      if (.not. associated(obj)) return

      if (associated(filebox)) then
        obj%filebox => filebox
      else
        nullify(obj%filebox)
      endif

      return
      end subroutine window_set_filebox


!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!


      subroutine window_set_cardset (obj, cardset)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (.not. associated(obj)) return

      if (associated(cardset)) then
        obj%cardset => cardset
      else
        nullify(obj%cardset)
      endif

      return
      end subroutine window_set_cardset


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine window_set_previous (obj, previous)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(window_struct),pointer                     :: previous      !argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine window_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine window_set_next (obj, next)
      implicit none
      type(window_struct),pointer                     :: obj           !argument
      type(window_struct),pointer                     :: next          !argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif

      return
      end subroutine window_set_next


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module window_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

