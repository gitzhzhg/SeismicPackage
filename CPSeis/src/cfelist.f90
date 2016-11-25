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
! Name       : cfelist
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2003-10-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : List Object Module.
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
!  4. 2003-11-18  Stoeckley    Provide warning regarding workaround for
!                               Portland Group compiler.
!  3. 2003-09-15  Stoeckley    Changed name from list to cfelist; changed type
!                               to primitive; changed category to cfe.
!  2. 2002-01-04  Vunderink    Changed name of this module from mwb_list to
!                                list.
!  1. 2000-08-15  Vunderink    Initial version.
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
! To work around the bug, calls such as the above must be changed to:
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


      module cfelist_module

      use pc_module

      implicit none

      private
      public :: cfelist_create
      public :: cfelist_delete
      public :: cfelist_clear
      public :: cfelist_copy
      public :: cfelist_set_name
      public :: cfelist_get_name
      public :: cfelist_set_comment
      public :: cfelist_get_comment
      public :: cfelist_name_matches 
      public :: cfelist_num_strings
      public :: cfelist_alloc_strings
      public :: cfelist_get_string
      public :: cfelist_put_strings
      public :: cfelist_put_string
      public :: cfelist_put_empty_strings
      public :: cfelist_insert_string
      public :: cfelist_remove_string
      public :: cfelist_replace_string
      public :: cfelist_add_string
      public :: cfelist_get_previous
      public :: cfelist_get_next
      public :: cfelist_set_previous
      public :: cfelist_set_next


      character(len=100),public,save :: cfelist_ident = &
       '$Id: cfelist.f90,v 1.4 2003/11/17 16:38:06 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      integer,parameter,public  :: MWB_LIST_LENGTH = PC_LENGTH

      type,public :: cfelist_struct
        private
        character(len=MWB_LIST_LENGTH)           :: name
        character(len=MWB_LIST_LENGTH)           :: comment
        character(len=MWB_LIST_LENGTH),pointer   :: strings(:)
        integer                                  :: nstrings
        type(cfelist_struct),pointer            :: previous
        type(cfelist_struct),pointer            :: next
      end type cfelist_struct


!!------------------------- end of data -----------------------------------!!
!!------------------------- end of data -----------------------------------!!
!!------------------------- end of data -----------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine cfelist_create (obj)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument

      allocate(obj)
      nullify(obj%strings)
      nullify(obj%previous)
      nullify(obj%next)
      obj%name     = ' '
      obj%comment  = ' '
      obj%nstrings = 0
      return
      end subroutine cfelist_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine cfelist_delete (obj)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument

      call cfelist_clear (obj)
      deallocate(obj)
      nullify(obj)
      return
      end subroutine cfelist_delete


!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!
!!------------------------------- clear -----------------------------------!!


      subroutine cfelist_clear (obj)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument

      if(associated(obj%strings)) deallocate(obj%strings)
      obj%nstrings = 0
      return
      end subroutine cfelist_clear


!!------------------------------- copy ------------------------------------!!
!!------------------------------- copy ------------------------------------!!
!!------------------------------- copy ------------------------------------!!


      subroutine cfelist_copy (obj1, obj2)
      implicit none
      type(cfelist_struct),pointer :: obj1            ! argument
      type(cfelist_struct),pointer :: obj2            ! argument

      if (obj1%nstrings > 0) then
           call cfelist_put_strings (obj2, obj1%strings, obj1%nstrings)
      else
           call cfelist_clear       (obj2)
      end if
      obj2%name = obj1%name
      return
      end subroutine cfelist_copy


!!----------------------------- set_name ----------------------------------!!
!!----------------------------- set_name ----------------------------------!!
!!----------------------------- set_name ----------------------------------!!


      subroutine cfelist_set_name (obj, name)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument
      character(len=*),intent(in)   :: name            ! argument

      obj%name = name
      call string_to_upper (obj%name)
      return
      end subroutine cfelist_set_name


!!----------------------------- get_name ----------------------------------!!
!!----------------------------- get_name ----------------------------------!!
!!----------------------------- get_name ----------------------------------!!


      subroutine cfelist_get_name (obj, name)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument
      character(len=*),intent(out)  :: name            ! argument

      name = obj%name
      return
      end subroutine cfelist_get_name


!!--------------------------- set_comment ---------------------------------!!
!!--------------------------- set_comment ---------------------------------!!
!!--------------------------- set_comment ---------------------------------!!


      subroutine cfelist_set_comment (obj, comment)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument
      character(len=*),intent(in)   :: comment            ! argument

      obj%comment = comment
      call string_to_upper (obj%comment)
      return
      end subroutine cfelist_set_comment


!!--------------------------- get_comment ---------------------------------!!
!!--------------------------- get_comment ---------------------------------!!
!!--------------------------- get_comment ---------------------------------!!


      subroutine cfelist_get_comment (obj, comment)
      implicit none
      type(cfelist_struct),pointer :: obj             ! argument
      character(len=*),intent(out)  :: comment            ! argument

      comment = obj%comment
      return
      end subroutine cfelist_get_comment


!!--------------------------- name_matches --------------------------------!!
!!--------------------------- name_matches --------------------------------!!
!!--------------------------- name_matches --------------------------------!!


      function cfelist_name_matches (obj, name) result (matches)
      implicit none
      type(cfelist_struct),pointer  :: obj            ! argument
      character(len=*),intent(in)    :: name           ! argument
      logical                        :: matches        ! result
      character(len=MWB_LIST_LENGTH) :: temp           ! local

      temp = name
      call string_to_upper (temp)
      matches = (temp == obj%name)
      return
      end function cfelist_name_matches


!!---------------------------- num_strings --------------------------------!!
!!---------------------------- num_strings --------------------------------!!
!!---------------------------- num_strings --------------------------------!!


      function cfelist_num_strings (obj) result (nstrings)
      implicit none
      type(cfelist_struct),pointer  :: obj            ! argument
      integer                        :: nstrings       ! result

      nstrings = obj%nstrings
      return
      end function cfelist_num_strings


!!-------------------------- alloc_strings --------------------------------!!
!!-------------------------- alloc_strings --------------------------------!!
!!-------------------------- alloc_strings --------------------------------!!


      subroutine cfelist_alloc_strings (obj, pstrings, nstrings)
      implicit none
      type(cfelist_struct),pointer  :: obj            ! argument
      character(len=*),pointer       :: pstrings(:)    ! argument
      integer         ,intent(out)   :: nstrings       ! argument

      if (associated(pstrings)) deallocate(pstrings)
      nstrings = obj%nstrings
      if (nstrings == 0) then
           allocate(pstrings(1))              ! added 2000-01-24
      else
           allocate(pstrings(nstrings))
           pstrings(1:nstrings) = obj%strings(1:nstrings)
      end if
      return
      end subroutine cfelist_alloc_strings


!!--------------------------- get_string ----------------------------------!!
!!--------------------------- get_string ----------------------------------!!
!!--------------------------- get_string ----------------------------------!!


      subroutine cfelist_get_string (obj, indx, string)
      implicit none
      type(cfelist_struct),pointer       :: obj           ! argument
      integer                ,intent(in)  :: indx          ! argument
      character(len=*)       ,intent(out) :: string        ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) then
           string = obj%strings(indx)
      else
           string = ' '
      end if
      return
      end subroutine cfelist_get_string



!!--------------------------- put_strings ---------------------------------!!
!!--------------------------- put_strings ---------------------------------!!
!!--------------------------- put_strings ---------------------------------!!


      subroutine cfelist_put_strings (obj, strings, nstrings)
      implicit none
      type(cfelist_struct),pointer         :: obj                ! argument
      integer                ,intent(in)    :: nstrings           ! argument
      character(len=*)       ,intent(in)    :: strings(:)         ! argument


      if (associated(obj%strings)) deallocate (obj%strings)
      if (nstrings > 0) then
           allocate(obj%strings(nstrings))
           obj%strings(1:nstrings) = strings(1:nstrings)
      end if
      obj%nstrings = nstrings
      return
      end subroutine cfelist_put_strings


!!---------------------------- put_string ---------------------------------!!
!!---------------------------- put_string ---------------------------------!!
!!---------------------------- put_string ---------------------------------!!


      subroutine cfelist_put_string (obj, string)
      implicit none
      type(cfelist_struct),pointer         :: obj             ! argument
      character(len=*)     ,intent(in)      :: string          ! argument

      if (associated(obj%strings)) deallocate (obj%strings)
      allocate(obj%strings(1))
      obj%nstrings = 1
      obj%strings(1) = string
      return
      end subroutine cfelist_put_string


!!-------------------------- empty_strings --------------------------------!!
!!-------------------------- empty_strings --------------------------------!!
!!-------------------------- empty_strings --------------------------------!!


      subroutine cfelist_put_empty_strings (obj, nstrings)
      implicit none
      type(cfelist_struct),pointer         :: obj             ! argument
      integer              ,intent(in)      :: nstrings        ! argument

      if (associated(obj%strings)) deallocate (obj%strings)
      allocate(obj%strings(nstrings))
      obj%nstrings = nstrings
      obj%strings(1:nstrings) = ' '
      return
      end subroutine cfelist_put_empty_strings


!!-------------------------- insert_string --------------------------------!!
!!-------------------------- insert_string --------------------------------!!
!!-------------------------- insert_string --------------------------------!!


      subroutine cfelist_insert_string (obj, indx, string)
      implicit none
      type(cfelist_struct),pointer         :: obj                 ! argument
      integer              ,intent(in)      :: indx                ! argument
      character(len=*)     ,intent(in)      :: string              ! argument

      character(len=MWB_LIST_LENGTH)        :: temp(obj%nstrings)  ! local

      if (indx >= 1 .and. indx <= obj%nstrings+1) then
           temp(1:obj%nstrings) = obj%strings(1:obj%nstrings)
           if (associated(obj%strings)) deallocate (obj%strings)
           allocate(obj%strings(obj%nstrings + 1))
           obj%strings(1:indx-1)              = temp(1:indx-1)
           obj%strings(indx)                  = string
           obj%strings(indx+1:obj%nstrings+1) = temp(indx:obj%nstrings)
           obj%nstrings = obj%nstrings + 1
      end if
      return
      end subroutine cfelist_insert_string


!!-------------------------- remove_string --------------------------------!!
!!-------------------------- remove_string --------------------------------!!
!!-------------------------- remove_string --------------------------------!!


      subroutine cfelist_remove_string (obj, indx)
      implicit none
      type(cfelist_struct),pointer         :: obj             ! argument
      integer              ,intent(in)      :: indx            ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) then
           obj%strings(1:indx-1)            = obj%strings(1:indx-1)
           obj%strings(indx:obj%nstrings-1) = obj%strings(indx+1:obj%nstrings)
           obj%nstrings = obj%nstrings - 1
      end if
      return
      end subroutine cfelist_remove_string


!!-------------------------- replace_string -------------------------------!!
!!-------------------------- replace_string -------------------------------!!
!!-------------------------- replace_string -------------------------------!!


      subroutine cfelist_replace_string (obj, indx, string)
      implicit none
      type(cfelist_struct),pointer         :: obj             ! argument
      integer              ,intent(in)      :: indx            ! argument
      character(len=*)     ,intent(in)      :: string          ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) then
           obj%strings(indx) = string
      end if
      return
      end subroutine cfelist_replace_string


!!--------------------------- add_string ----------------------------------!!
!!--------------------------- add_string ----------------------------------!!
!!--------------------------- add_string ----------------------------------!!


      subroutine cfelist_add_string (obj, string)
      implicit none
      type(cfelist_struct),pointer         :: obj                 ! argument
      character(len=*)     ,intent(in)      :: string              ! argument
      character(len=MWB_LIST_LENGTH)        :: temp(obj%nstrings)  ! local

      temp(1:obj%nstrings) = obj%strings(1:obj%nstrings)
      if (associated(obj%strings)) deallocate (obj%strings)
      allocate(obj%strings(obj%nstrings + 1))
      obj%strings(1:obj%nstrings) = temp(1:obj%nstrings)
      obj%nstrings = obj%nstrings + 1
      obj%strings(obj%nstrings) = string
      return
      end subroutine cfelist_add_string


!!---------------------------- get_previous -------------------------------!!
!!---------------------------- get_previous -------------------------------!!
!!---------------------------- get_previous -------------------------------!!


      subroutine cfelist_get_previous (obj, previous)
      implicit none
      type(cfelist_struct),pointer              :: obj                !argument
      type(cfelist_struct),pointer              :: previous           !argument

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
      end subroutine cfelist_get_previous


!!------------------------------ get_next ---------------------------------!!
!!------------------------------ get_next ---------------------------------!!
!!------------------------------ get_next ---------------------------------!!


      subroutine cfelist_get_next (obj, next)
      implicit none
      type(cfelist_struct),pointer              :: obj                !argument
      type(cfelist_struct),pointer              :: next               !argument

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
      end subroutine cfelist_get_next


!!---------------------------- set_previous -------------------------------!!
!!---------------------------- set_previous -------------------------------!!
!!---------------------------- set_previous -------------------------------!!


      subroutine cfelist_set_previous (obj, previous)
      implicit none
      type(cfelist_struct),pointer              :: obj                !argument
      type(cfelist_struct),pointer              :: previous           !argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine cfelist_set_previous


!!------------------------------ set_next ---------------------------------!!
!!------------------------------ set_next ---------------------------------!!
!!------------------------------ set_next ---------------------------------!!


      subroutine cfelist_set_next (obj, next)
      implicit none
      type(cfelist_struct),pointer              :: obj                !argument
      type(cfelist_struct),pointer              :: next               !argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif

      return
      end subroutine cfelist_set_next


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module cfelist_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

