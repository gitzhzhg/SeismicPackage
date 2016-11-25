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
! Name       : cfewindow 
! Category   : cfe
! Written    : 1999-09-07   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE Windows Module.
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
!010. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!009. 2006-01-10  B. Menger    Removed Unused Variables.
!  8. 2003-09-15  Stoeckley    Changed name from cfe_window to cfewindow;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
!  7. 2002-06-04  Vunderink    Make cfewindow_update return if pointer
!                                argument not associated. 
!  6. 2002-01-04  Vunderink    Made changes needed for buildlist tool.
!  5. 2000-08-15  Vunderink    Enhanced for multi-workfile builder and changed
!                                character variables to use PC_LENGTH
!  4. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH
!  3. 2000-02-29  Vunderink    Added cfewindow_alloc_gui_cards and constants
!                                for the version and help windows.
!  2. 2000-02-27  Vunderink    cfewindow_delete should not nullify the current
!                                window pointer
!  1. 1999-09-07  Vunderink    Initial version.
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


      module cfewindow_module
      use pc_module
      use cardset_module
      use filebox_module
      use buildlist_module
      use process_module
      use awindow_module
      use awindowlist_module

      implicit none

      private
      public :: cfewindow_clear
      public :: cfewindow_create
      public :: cfewindow_delete
      public :: cfewindow_update
      public :: cfewindow_create_child
      public :: cfewindow_get_window_id
      public :: cfewindow_get_window_type
      public :: cfewindow_get_keyword
      public :: cfewindow_get_value
      public :: cfewindow_get_index
      public :: cfewindow_get_process
      public :: cfewindow_get_parameter
      public :: cfewindow_get_filebox
      public :: cfewindow_get_cardset
      public :: cfewindow_get_previous
      public :: cfewindow_get_next
      public :: cfewindow_get_pointer
      public :: cfewindow_get_current
      public :: cfewindow_get_parent
      public :: cfewindow_alloc_keys
      public :: cfewindow_alloc_gui_cards
      public :: cfewindow_set_keys
      public :: cfewindow_set_window_id
      public :: cfewindow_set_window_type
      public :: cfewindow_set_keyword
      public :: cfewindow_set_value
      public :: cfewindow_set_index
      public :: cfewindow_set_process
      public :: cfewindow_set_parameter
      public :: cfewindow_set_filebox
      public :: cfewindow_set_cardset
      public :: cfewindow_set_previous
      public :: cfewindow_set_next
      public :: cfewindow_set_current
      public :: cfewindow_match
      public :: cfewindow_get_all_of_type

      character(len=100),public,save :: cfewindow_ident = &
       '$Id: cfewindow.f90,v 1.10 2006/09/18 13:32:43 Glover prod sps $'

      type,public :: cfewindow_struct
        private
        type(awindow_struct),pointer                  :: awindow
        type(awindowlist_struct),pointer              :: children
        type(cfewindow_struct),pointer              :: parent
        type(cfewindow_struct),pointer              :: previous
        type(cfewindow_struct),pointer              :: next
      end type cfewindow_struct

      integer,parameter                              :: WINDOW_UESED_INC   = 100
      integer,public,parameter                       :: HELP_WINDOW        = 101
      integer,public,parameter                       :: VERSION_WINDOW     = 102

      integer                        ,save           :: nwindows_used
      integer                ,pointer,save           :: windows_used(:)
      type(cfewindow_struct),pointer,save           :: window_list_first
      type(cfewindow_struct),pointer,save           :: window_list_last
      type(cfewindow_struct),pointer,save           :: window_current

      contains


!!-------------------------------- clear ----------------------------------!!
!!-------------------------------- clear ----------------------------------!!
!!-------------------------------- clear ----------------------------------!!


      subroutine cfewindow_clear

      nwindows_used = WINDOW_UESED_INC
      allocate(windows_used(nwindows_used))
      windows_used = -1
      nullify(window_list_first)
      nullify(window_list_last)
      nullify(window_current)

      return
      end subroutine cfewindow_clear


!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!


      subroutine cfewindow_create (obj)

      type(cfewindow_struct),pointer                 :: obj           !argument

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
      nullify (obj%awindow) ! jpa

      call awindow_create (obj%awindow)
      call awindow_set_window_id (obj%awindow,window_id)
      if (associated(window_current)) then
        obj%parent => window_current
      else
        nullify(obj%parent)
      endif
      nullify(obj%children)
      nullify(obj%previous)
      nullify(obj%next)

      if (.not. associated(window_list_first)) then
        window_list_first => obj
        window_list_last  => obj
      else
        window_list_last%next => obj
        obj%previous   => window_list_last
        window_list_last      => obj
      endif

      return
      end subroutine cfewindow_create


!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!


      subroutine cfewindow_delete (obj)

      type(cfewindow_struct),pointer                 :: obj           !argument

      integer                                         :: window_id     !local
      integer                                         :: current_id    !local


      if (.not. associated(obj)) return

      if (associated(window_current)) then
        call awindow_get_window_id (window_current%awindow,current_id)
        call awindow_get_window_id (obj%awindow,window_id)
        if (window_id .eq. current_id) then
          window_current => obj%parent
        else
!         nullify(window_current)
        endif
      endif

      call awindow_delete (obj%awindow)

      if (associated(obj%previous).and.associated(obj%next)) then
        obj%previous%next => obj%next
        obj%next%previous => obj%previous
      else if (.not. associated(obj%previous)) then
        if (associated(obj%next)) then
          window_list_first =>  obj%next
          nullify(obj%next%previous)
        else
          nullify(window_list_first)
        endif
      else if (.not. associated(obj%next)) then
        if (associated(obj%previous)) then
          window_list_last => obj%previous
          nullify(obj%previous%next)
        else
          nullify(window_list_last)
        endif
      endif

      deallocate(obj)

      return
      end subroutine cfewindow_delete


!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!
!!------------------------------- update ----------------------------------!!


      subroutine cfewindow_update (obj)

      type(cfewindow_struct),pointer                 :: obj           !argument

      integer                                         :: ncards        !local
      character(len=PC_LENGTH),pointer                :: cards(:)      !local
      character(len=PC_LENGTH)                        :: window_type   !local

      type(buildlist_struct)  ,pointer                :: parameter     !local

      if (.not. associated(obj)) return

      nullify(cards)
      nullify (parameter) ! jpa

      call awindow_get_window_type (obj%awindow,window_type)
      if (trim(window_type) .eq. 'PROCESS') then
        call cfewindow_alloc_gui_cards (obj,cards,ncards)
        if (ncards .gt. 0) then
          call pc_clear_gui_cards
          call pc_put_gui_cards (cards,ncards)
        endif
      else if (trim(window_type) .eq. 'PARAMETER') then
        call awindow_get_parameter     (obj%awindow,parameter)
        call pc_clear_gui_cards
        call pc_clear_process_cards
        call buildlist_update         (parameter)
      endif

      if (associated(cards)) deallocate(cards)

      return
      end subroutine cfewindow_update


!!--------------------------- create_child --------------------------------!!
!!--------------------------- create_child --------------------------------!!
!!--------------------------- create_child --------------------------------!!


      subroutine cfewindow_create_child (obj,child)

      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: child         !argument

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

      call awindow_duplicate (obj%awindow,child%awindow)
      call awindow_set_window_id (child%awindow,window_id)
      child%parent => obj
      if (.not. associated(child%parent%children)) then
        call awindowlist_create     (child%parent%children)
        call awindowlist_add_window (child%parent%children,child%awindow)
      endif
      nullify(child%children)
      nullify(child%previous)
      nullify(child%next)

      if (.not. associated(window_list_first)) then
        window_list_first => child
        window_list_last  => child
      else
        window_list_last%next => child
        child%previous            => window_list_last
        window_list_last      => child
      endif

      return
      end subroutine cfewindow_create_child


!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!


      subroutine cfewindow_get_window_id (obj,window_id)

      type(cfewindow_struct),pointer                 :: obj           !argument
      integer,intent(out)                             :: window_id     !argument

      if (associated(obj)) then
        call awindow_get_window_id (obj%awindow,window_id)
      else
        window_id = 0
      endif

      return
      end subroutine cfewindow_get_window_id


!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!


      subroutine cfewindow_get_window_type (obj,window_type)

      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=*),intent(out)                    :: window_type   !argument

      if (associated(obj)) then
        call awindow_get_window_type (obj%awindow,window_type)
      else
        window_type = ' '
      endif

      return
      end subroutine cfewindow_get_window_type


!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!


      subroutine cfewindow_get_keyword (obj,keyword)

      type(cfewindow_struct),pointer                :: obj            !argument
      character(len=*),intent(out)                   :: keyword        !argument

      if (associated(obj)) then
        call awindow_get_keyword (obj%awindow,keyword)
      else
        keyword = ' '
      endif

      return
      end subroutine cfewindow_get_keyword


!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!


      subroutine cfewindow_get_value (obj,value)

      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=*),intent(out)                    :: value         !argument

      if (associated(obj)) then
        call awindow_get_value (obj%awindow,value)
      else
        value = ' '
      endif

      return
      end subroutine cfewindow_get_value


!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!


      subroutine cfewindow_get_index (obj,index)

      type(cfewindow_struct),pointer                 :: obj           !argument
      integer,intent(out)                             :: index         !argument

      if (associated(obj)) then
        call awindow_get_index (obj%awindow,index)
      else
        index = 0
      endif

      return
      end subroutine cfewindow_get_index


!!----------------------------- get_pointer -------------------------------!!
!!----------------------------- get_pointer -------------------------------!!
!!----------------------------- get_pointer -------------------------------!!


      subroutine cfewindow_get_pointer (window_id,obj)

      integer,intent(in)                              :: window_id     !argument
      type(cfewindow_struct),pointer                 :: obj           !argument

      integer                                         :: temp_id       !local

      obj => window_list_first
      do
        if (.not. associated(obj)) exit
        call awindow_get_window_id (obj%awindow,temp_id)
        if (window_id .eq. temp_id) exit
        obj => obj%next
      enddo

      return
      end subroutine cfewindow_get_pointer


!!----------------------------- get_process -------------------------------!!
!!----------------------------- get_process -------------------------------!!
!!----------------------------- get_process -------------------------------!!


      subroutine cfewindow_get_process (obj, process)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(process_struct)   ,pointer                 :: process       !argument

      if (associated(obj)) then
        call awindow_get_process (obj%awindow,process)
      else
        nullify(process)
      endif

      return
      end subroutine cfewindow_get_process


!!---------------------------- get_parameter ------------------------------!!
!!---------------------------- get_parameter ------------------------------!!
!!---------------------------- get_parameter ------------------------------!!


      subroutine cfewindow_get_parameter (obj, parameter)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(buildlist_struct) ,pointer                 :: parameter     !argument

      if (associated(obj)) then
        call awindow_get_parameter (obj%awindow,parameter)
      else
        nullify(parameter)
      endif

      return
      end subroutine cfewindow_get_parameter


!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!


      subroutine cfewindow_get_filebox (obj, filebox)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(filebox_struct)   ,pointer                 :: filebox       !argument

      if (associated(obj)) then
        call awindow_get_filebox (obj%awindow,filebox)
      else
        nullify(filebox)
      endif

      return
      end subroutine cfewindow_get_filebox


!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!


      subroutine cfewindow_alloc_gui_cards (obj, cards, ncards)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=*)       ,pointer                 :: cards(:)      !argument
      integer                ,intent(out)             :: ncards        !argument

      character(len=PC_LENGTH)                        :: window_type   !local
      type(process_struct)   ,pointer                 :: process       !local

      nullify (process) ! jpa

      if (associated(obj)) then
        call awindow_get_window_type (obj%awindow,window_type)
        if (trim(window_type) .eq. 'PROCESS') then
          call awindow_get_process (obj%awindow,process)
          call process_alloc_gui_cards (process,cards,ncards)
        else
          ncards = 0
          if (associated(cards)) deallocate(cards)
        endif
      else
        ncards = 0
        if (associated(cards)) deallocate(cards)
      endif

      return
      end subroutine cfewindow_alloc_gui_cards


!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!


      subroutine cfewindow_get_cardset (obj, cardset)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (associated(obj)) then
        call awindow_get_cardset (obj%awindow,cardset)
      else
        nullify(cardset)
      endif

      return
      end subroutine cfewindow_get_cardset


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine cfewindow_get_previous (obj, previous)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: previous      !argument

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
      end subroutine cfewindow_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine cfewindow_get_next (obj, next)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: next          !argument

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
      end subroutine cfewindow_get_next


!!----------------------------- get_current -------------------------------!!
!!----------------------------- get_current -------------------------------!!
!!----------------------------- get_current -------------------------------!!


      subroutine cfewindow_get_current (current)
      implicit none
      type(cfewindow_struct),pointer                 :: current       !argument

      if (associated(window_current)) then
        current => window_current
      else
        nullify(current)
      endif

      return
      end subroutine cfewindow_get_current


!!----------------------------- get_parent --------------------------------!!
!!----------------------------- get_parent --------------------------------!!
!!----------------------------- get_parent --------------------------------!!


      subroutine cfewindow_get_parent (obj,parent)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: parent        !argument

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
      end subroutine cfewindow_get_parent


!!----------------------------- alloc_keys --------------------------------!!
!!----------------------------- alloc_keys --------------------------------!!
!!----------------------------- alloc_keys --------------------------------!!


      subroutine cfewindow_alloc_keys (obj,keys,nkeys)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=PC_LENGTH) ,pointer               :: keys(:)       !argument
      integer,intent(out)                             :: nkeys         !argument

      if (associated(obj)) then
        call awindow_alloc_keys (obj%awindow,keys,nkeys)
      else
        if (associated(keys)) deallocate(keys)
        nkeys = 0
      endif

      return
      end subroutine cfewindow_alloc_keys


!!------------------------------ set_keys ---------------------------------!!
!!------------------------------ set_keys ---------------------------------!!
!!------------------------------ set_keys ---------------------------------!!


      subroutine cfewindow_set_keys (obj,keys,nkeys)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=PC_LENGTH) ,pointer               :: keys(:)       !argument
      integer,intent(in)                              :: nkeys         !argument

      if (.not. associated(obj)) return

      call awindow_set_keys (obj%awindow,keys,nkeys)

      return
      end subroutine cfewindow_set_keys


!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!


      subroutine cfewindow_set_window_id (obj,window_id)

      type(cfewindow_struct),pointer                 :: obj           !argument
      integer,intent(in)                              :: window_id     !argument

      if (.not. associated(obj)) return

      call awindow_set_window_id (obj%awindow,window_id)

      return
      end subroutine cfewindow_set_window_id


!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!


      subroutine cfewindow_set_window_type (obj,window_type)

      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=*),intent(in)                     :: window_type   !argument

      if (.not. associated(obj)) return

      call awindow_set_window_type (obj%awindow,window_type)

      return
      end subroutine cfewindow_set_window_type


!!------------------------------ set_keyword ------------------------------!!
!!------------------------------ set_keyword ------------------------------!!
!!------------------------------ set_keyword ------------------------------!!


      subroutine cfewindow_set_keyword (obj,keyword)

      type(cfewindow_struct),pointer                :: obj            !argument
      character(len=*),intent(in)                    :: keyword        !argument

      if (.not. associated(obj)) return

      call awindow_set_keyword (obj%awindow,keyword)

      return
      end subroutine cfewindow_set_keyword


!!------------------------------ set_value --------------------------------!!
!!------------------------------ set_value --------------------------------!!
!!------------------------------ set_value --------------------------------!!


      subroutine cfewindow_set_value (obj,value)

      type(cfewindow_struct),pointer                 :: obj           !argument
      character(len=*),intent(in)                     :: value         !argument

      if (.not. associated(obj)) return

      call awindow_set_value (obj%awindow,value)

      return
      end subroutine cfewindow_set_value


!!------------------------------ set_index --------------------------------!!
!!------------------------------ set_index --------------------------------!!
!!------------------------------ set_index --------------------------------!!


      subroutine cfewindow_set_index (obj,index)

      type(cfewindow_struct),pointer                 :: obj           !argument
      integer,intent(in)                              :: index         !argument

      if (.not. associated(obj)) return

      call awindow_set_index (obj%awindow,index)

      return
      end subroutine cfewindow_set_index


!!----------------------------- set_process -------------------------------!!
!!----------------------------- set_process -------------------------------!!
!!----------------------------- set_process -------------------------------!!


      subroutine cfewindow_set_process (obj, process)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(process_struct)   ,pointer                 :: process       !argument

      if (.not. associated(obj)) return

      call awindow_set_process (obj%awindow,process)

      return
      end subroutine cfewindow_set_process


!!---------------------------- set_parameter ------------------------------!!
!!---------------------------- set_parameter ------------------------------!!
!!---------------------------- set_parameter ------------------------------!!


      subroutine cfewindow_set_parameter (obj, parameter)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(buildlist_struct) ,pointer                 :: parameter     !argument

      if (.not. associated(obj)) return

      call awindow_set_parameter (obj%awindow,parameter)

      return
      end subroutine cfewindow_set_parameter


!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!


      subroutine cfewindow_set_filebox (obj, filebox)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(filebox_struct)   ,pointer                 :: filebox       !argument

      if (.not. associated(obj)) return

      call awindow_set_filebox (obj%awindow,filebox)

      return
      end subroutine cfewindow_set_filebox


!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!


      subroutine cfewindow_set_cardset (obj, cardset)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cardset_struct)   ,pointer                 :: cardset       !argument

      if (.not. associated(obj)) return

      call awindow_set_cardset (obj%awindow,cardset)

      return
      end subroutine cfewindow_set_cardset


!!------------------------------ set_parent -------------------------------!!
!!------------------------------ set_parent -------------------------------!!
!!------------------------------ set_parent -------------------------------!!


      subroutine cfewindow_set_parent (obj,parent)

      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: parent        !argument

      if (.not. associated(obj)) return

      obj%parent => parent

      return
      end subroutine cfewindow_set_parent


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine cfewindow_set_previous (obj, previous)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: previous      !argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine cfewindow_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine cfewindow_set_next (obj, next)
      implicit none
      type(cfewindow_struct),pointer                 :: obj           !argument
      type(cfewindow_struct),pointer                 :: next          !argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif

      return
      end subroutine cfewindow_set_next


!!----------------------------- set_current -------------------------------!!
!!----------------------------- set_current -------------------------------!!
!!----------------------------- set_current -------------------------------!!


      subroutine cfewindow_set_current (current)
      implicit none
      type(cfewindow_struct),pointer                 :: current       !argument

      if (associated(current)) then
        window_current => current
      else
        nullify(window_current)
      endif

      return
      end subroutine cfewindow_set_current


!!-------------------------------- match ----------------------------------!!
!!-------------------------------- match ----------------------------------!!
!!-------------------------------- match ----------------------------------!!


      function cfewindow_match (window_type,keyword,value,index)   &
                                                             result (window_id)
      implicit none
      character(len=*),intent(in)                     :: window_type   !argument
      character(len=*),intent(in)                     :: keyword       !argument
      character(len=*),intent(in)                     :: value         !argument
      integer         ,intent(in)                     :: index         !argument
      integer                                         :: window_id     !result

      type(cfewindow_struct),pointer                 :: obj           !local
      character(len=PC_LENGTH)                        :: temp_type     !local
      character(len=PC_LENGTH)                        :: temp_keyword  !local
      character(len=PC_LENGTH)                        :: temp_value    !local
      integer                                         :: temp_index    !local

      window_id = -1

      obj => window_list_first
      do
        if (.not. associated(obj)) exit
        call awindow_get_window_type (obj%awindow,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) then
          call awindow_get_keyword (obj%awindow,temp_keyword)
          if (trim(keyword) .eq. trim(temp_keyword)) then 
            call awindow_get_value (obj%awindow,temp_value)
            if (trim(value) .eq. trim(temp_value)) then
              call awindow_get_index (obj%awindow,temp_index)
              if (index .eq. temp_index) then
                call awindow_get_window_id   (obj%awindow,window_id)
                exit
              endif
            endif
          endif
        endif
        obj => obj%next
      enddo

      return
      end function cfewindow_match


!!--------------------------- get_all_of_type -----------------------------!!
!!--------------------------- get_all_of_type -----------------------------!!
!!--------------------------- get_all_of_type -----------------------------!!


      subroutine cfewindow_get_all_of_type (window_type,window_ids)
      implicit none
      character(len=*),intent(in)                     :: window_type   !argument
      integer,pointer                                 :: window_ids(:) !local

      type(cfewindow_struct),pointer                 :: obj           !local
      character(len=PC_LENGTH)                        :: temp_type     !local
      integer                                         :: count         !local

      count = 0
      if (associated(window_ids)) deallocate(window_ids)

      obj => window_list_first
      do
        if (.not. associated(obj)) exit
        call awindow_get_window_type (obj%awindow,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) count = count + 1
        obj => obj%next
      enddo

      if (count .eq. 0) return

      allocate(window_ids(count))

      count = 0
      obj => window_list_first
      do
        if (.not. associated(obj)) exit
        call awindow_get_window_type (obj%awindow,temp_type)
        if (trim(window_type) .eq. trim(temp_type)) then
          count = count + 1
          call awindow_get_window_id (obj%awindow,window_ids(count))
        endif
        obj => obj%next
      enddo

      return
      end subroutine cfewindow_get_all_of_type


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfewindow_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

