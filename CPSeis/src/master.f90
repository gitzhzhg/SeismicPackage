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
! Name       : master
! Category   : stand-alone
! Written    : 2002-04-23   by: Donna K. Vunderink
! Revised    : 2002-11-05   by: Donna K. Vunderink
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
!  3. 2002-11-05  Vunderink    Removed BTIE.
!  2. 2002-10-09  Vunderink    Modified master_update and master_execute to
!                                check if object is associated.
!  1. 2002-04-23  Vunderink    Initial version.
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


!<program_list>
!PROGRAM_COUNT=1
!RPTSTATS             MISCELLANEOUS        Report Statistics
!</program_list>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module master_module
      use pc_module
      use named_constants_module
      use string_module
      use cio_module

      use RPTSTATS_module

      implicit none
      private
      public :: master_create
      public :: master_initialize
      public :: master_update
      public :: master_delete
      public :: master_execute
      public :: master_list
      public :: master_get_name
      public :: master_alloc_gui_cards

      character(len=100),public,save :: master_ident = &
       '$Id: master.f90,v 1.3 2002/11/05 14:18:11 Vunderink prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: master_struct
      private
        character(len=PC_LENGTH)                   :: name
        type(RPTSTATS_struct),pointer              :: RPTSTATSobj
      end type master_struct

      type :: plist_struct
        character(len=PC_LENGTH)                   :: program
        character(len=20)                          :: catagory
        character(len=PC_LENGTH)                   :: purpose
      end type plist_struct

      integer,save                                 :: len_all
      type(plist_struct),pointer                   :: all(:)
      logical,save                                 :: first_time = .TRUE.


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine master_create (obj,prog_name)
      implicit none
      type(master_struct),pointer       :: obj                !argument
      character(len=*),intent(in)       :: prog_name          !argument

      allocate (obj)

      nullify(obj%RPTSTATSobj)

      obj%name = prog_name
      call string_to_upper(obj%name)

      select case (trim(obj%name))
        case ('RPTSTATS')             ;call RPTSTATS_create(obj%RPTSTATSobj)
        case default                  ;call pc_error("Invalid program "//  &
                                                     trim(obj%name))
      end select

      return
      end subroutine master_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine master_delete (obj)
      implicit none
      type(master_struct),pointer       :: obj                !argument


      if (.not. associated(obj)) return

      select case (trim(obj%name))
        case ('RPTSTATS')             ;call RPTSTATS_delete(obj%RPTSTATSobj)
        case default                  ;call pc_error("Invalid program "//  &
                                                     trim(obj%name))
      end select

      deallocate(obj)

      return
      end subroutine master_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine master_initialize (obj)
      implicit none
      type(master_struct),pointer       :: obj                !argument


      select case (trim(obj%name))
        case ('RPTSTATS')             ;call RPTSTATS_initialize(obj%RPTSTATSobj)
        case default                  ;call pc_error("Invalid program "//  &
                                                     trim(obj%name))
      end select

      return
      end subroutine master_initialize


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine master_update (obj)
      implicit none
      type(master_struct),pointer        :: obj                !argument


      if (.not. associated(obj)) return

      select case (trim(obj%name))
        case ('RPTSTATS')             ;call RPTSTATS_update(obj%RPTSTATSobj)
        case default                  ;call pc_error("Invalid program "//  &
                                                     trim(obj%name))
      end select

      return
      end subroutine master_update


!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!


      subroutine master_execute (obj)
      implicit none
      type(master_struct),pointer        :: obj                !argument


      if (.not. associated(obj)) return

      select case (trim(obj%name))
        case ('RPTSTATS')             ;call RPTSTATS(obj%RPTSTATSobj)
        case default                  ;call pc_error("Invalid program "//  &
                                                     trim(obj%name))
      end select

      return
      end subroutine master_execute


!!------------------------------ load_list ---------------------------------!!
!!------------------------------ load_list ---------------------------------!!
!!------------------------------ load_list ---------------------------------!!


      subroutine master_load_list

      len_all = 1
      allocate(all(len_all))

      all(1)%program  = 'RPTSTATS'
      all(1)%catagory = 'MISCELLANEOUS'
      all(1)%purpose  = 'Report Statistics'

      first_time = .FALSE.

      return
      end subroutine master_load_list


!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!


      subroutine master_list (prog_list,nprog_list,catagory_name)
      implicit none
      character(len=*),pointer         :: prog_list(:)       !argument
      integer         ,intent(out)     :: nprog_list         !argument
      character(len=*),optional        :: catagory_name      !argument

      character(len=PC_LENGTH)         :: catagory           !local
      integer                          :: i                  !local
      integer                          :: istat              !local

      if (first_time) call master_load_list

      if (present(catagory_name)) then
        catagory = catagory_name
        call string_to_upper(catagory)
      else
        catagory = 'ALL_PROGRAMS'
      endif

      nprog_list = 0
      if (associated(prog_list)) then
        deallocate(prog_list,stat=istat)
        if (istat .ne. 0) then
          print *,'master_list: deallocate error istat=',istat
        endif
      endif

      select case (trim(catagory))
        case ('ALL_PROGRAMS')
          nprog_list = LEN_ALL
          allocate(prog_list(nprog_list))
          do i=1,LEN_ALL
            prog_list(i) = all(i)%program
          enddo

        case default
          nprog_list = 0
          do i=1,LEN_ALL
            if (trim(all(i)%catagory).eq.trim(catagory))  &
                                                     nprog_list = nprog_list + 1
          enddo
          if (nprog_list .gt. 0) then
            allocate(prog_list(nprog_list))
            nprog_list = 0
            do i=1,LEN_ALL
              if (trim(all(i)%catagory).eq.trim(catagory)) then
                nprog_list = nprog_list + 1
                prog_list(nprog_list) = all(i)%program
              endif
            enddo
          endif
      end select

      if (.not. associated(prog_list)) then
        allocate(prog_list(1))
        prog_list(1) = ' '
      endif

      return
      end subroutine master_list


!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!


      subroutine master_get_name (obj, name)
      implicit none
      type(master_struct),intent(in)   :: obj                !argument
      character(len=*)  ,intent(out)   :: name               !argument

      name = obj%name

      return
      end subroutine master_get_name


      subroutine master_alloc_gui_cards (obj,cards,ncards)
      implicit none
      type(master_struct),intent(in)                  :: obj           !argument
      character(len=*)       ,pointer                 :: cards(:)      !argument
      integer                ,intent(out)             :: ncards        !argument

      ncards = 0

      end subroutine master_alloc_gui_cards


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module master_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

