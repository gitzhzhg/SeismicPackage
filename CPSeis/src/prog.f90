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
! Name       : prog
! Category   : stand-alone
! Written    : 2002-10-09   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : Program Object Module.
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
!  1. 2002-10-09  Vunderink    Initial version.
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


      module prog_module

      use cio_module
      use master_module
      use named_constants_module
      use pc_module
      use string_module

      implicit none
      private
      public :: prog_create
      public :: prog_initialize
      public :: prog_update
      public :: prog_delete
      public :: prog_execute
      public :: prog_list
      public :: prog_get_name
      public :: prog_alloc_gui_cards

      character(len=100),public,save :: prog_ident = &
       '$Id: prog.f90,v 1.1 2002/10/09 19:03:50 Vunderink prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: prog_struct
      private
        character(len=PC_LENGTH)                   :: name
        type(master_struct),pointer                :: master
        type(cardset_struct),pointer               :: gui
      end type prog_struct

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine prog_create (obj,prog_name)
      implicit none
      type(prog_struct),pointer                  :: obj                !argument
      character(len=*),intent(in)                :: prog_name          !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      allocate (obj)

      nullify(obj%master)
      nullify(obj%gui)

      obj%name = prog_name
      call string_to_upper(obj%name)

      call master_create  (obj%master,obj%name)

      call cardset_create (obj%gui)
      call cardset_set_packing_option (obj%gui,CARDSET_PACKED)
      call pc_alloc_gui_cards         (cards, ncards)
      call cardset_put_cards          (obj%gui, cards, ncards)

      return
      end subroutine prog_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine prog_delete (obj)
      implicit none
      type(prog_struct),pointer       :: obj                !argument

      if (.not. associated(obj)) return

      if (associated(obj%master)) call master_delete  (obj%master)
      if (associated(obj%gui))    call cardset_delete (obj%gui)

      deallocate(obj)

      return
      end subroutine prog_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine prog_initialize (obj)
      implicit none
      type(prog_struct),pointer                 :: obj                !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      if (.not. associated(obj)) return

      if (associated(obj%master)) then
        call master_initialize (obj%master)

        call pc_alloc_gui_cards         (cards, ncards)
        call cardset_clear              (obj%gui)
        call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
        call cardset_put_cards          (obj%gui, cards, ncards)
      endif

      return
      end subroutine prog_initialize


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine prog_update (obj)
      implicit none
      type(prog_struct),pointer                  :: obj                !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      if (.not. associated(obj)) return

      if (associated(obj%master)) then
        call master_update (obj%master)

        call pc_alloc_gui_cards         (cards, ncards)
        call cardset_clear              (obj%gui)
        call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
        call cardset_put_cards          (obj%gui, cards, ncards)
      endif

      return
      end subroutine prog_update


!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!


      subroutine prog_execute (obj)
      implicit none
      type(prog_struct),pointer        :: obj                !argument


      if (.not. associated(obj)) return

      if (associated(obj%master)) call master_execute (obj%master)

      return
      end subroutine prog_execute


!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!


      subroutine prog_list (plist,nplist,catagory_name)
      implicit none
      character(len=*),pointer         :: plist(:)            !argument
      integer         ,intent(out)     :: nplist              !argument
      character(len=*),optional        :: catagory_name       !argument

      if (present(catagory_name)) then
        call master_list (plist,nplist,catagory_name)
      else
        call master_list (plist,nplist)
      endif

      return
      end subroutine prog_list


!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!


      subroutine prog_get_name (obj, name)
      implicit none
      type(prog_struct),intent(in)   :: obj                !argument
      character(len=*) ,intent(out)  :: name               !argument

      name = obj%name

      return
      end subroutine prog_get_name


      subroutine prog_alloc_gui_cards (obj,cards,ncards)
      implicit none
      type(prog_struct),pointer                       :: obj           !argument
      character(len=*) ,pointer                       :: cards(:)      !argument
      integer          ,intent(out)                   :: ncards        !argument

      if (.not. associated(obj)) return

      call cardset_alloc_cards (obj%gui, cards, ncards)

      end subroutine prog_alloc_gui_cards


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module prog_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

