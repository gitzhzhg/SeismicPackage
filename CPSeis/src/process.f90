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
!                        C P S  P R I M I T I V E
!
! Name       : process 
! Category   : cfe
! Written    : 1999-08-03   by: Donna K. Vunderink
! Revised    : 2006-10-31   by: D. Glover
! Maturity   : production
! Purpose    : CFE Process Object Module.
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
! 22. 2006-10-31  D. Glover    Added NULLIFY statements for Intel compiler.
! 21. 2006-01-10  B. Menger    Remove Unused Variables.
! 20. 2003-11-18  Stoeckley    Add warning regarding workaround for Portland
!                               Group compiler; call superproc instead of
!                               super_wrapper.
! 19. 2003-09-15  Stoeckley    Call super_wrapper instead of super; changed
!                               type to primitive; changed category to cfe.
! 18. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 17. 2002-08-28  Vunderink    Added routine get_keyword_nature
! 16. 2001-12-03  Vunderink    Remove error, warning, and info messages before
!                                copying process
! 15. 2001-08-29  Vunderink    Added routine process_put_gui_iscalar
! 14. 2001-08-08  Vunderink    Set packing option after clearing cardsets and
!                                remove setting/changing packing option after
!                                data is in cardset
! 13. 2001-06-21  Vunderink    Fixed bug in process_copy_????_cards
! 12. 2001-01-16  Vunderink    Report warnings if frontend in process_update 
! 11. 2000-12-05  Vunderink    Added routine alloc_keywords
! 10. 2000-09-04  Vunderink    Added routines to compare cardsets of two
!                                processes
!  9. 2000-08-15  Vunderink    Removed use of cfe_constants, added use of pclun
!                                module for parameter cache lun, and enhanced
!                                get_by_keyword to get value of an array element
!  8. 2000-05-08  Vunderink    Changed process_list argument list to include
!                                size of list array.
!  7. 2000-04-24  Vunderink    Added process_show_messages, fixed
!                                process_creae_super to do a frontend_update and
!                                to clear cards in cardset before put, and fixed
!                                process_super_update_only and process_copy to 
!                                check for pointer association.
!  6. 2000-03-17  Vunderink    Added process_super_create_only, process_list,
!                                process_super_update_only, process_validate,
!                                and process_set_view to isolate super_module
!                                from the rest of CFE.
!  5. 2000-02-29  Vunderink    Added process_clear_mate, process_get_mate,
!                                process_set_mate, and process_run_traps to
!                                support updating all process popups.
!  4. 2000-02-16  Vunderink    Changed process_copy to call pc_frontend_update
!  3. 2000-01-05  Vunderink    Fixed to work with parameter cache change of
!                                checking ipn on put of jdata and pdata cards.
!  2. 2000-01-27  Vunderink    Changed process_copy to call super_update when
!                                the obj2%super pointer is already associated
!  1. 1999-08-03  Vunderink    Initial version.
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


      module process_module

      use pc_module
      use superproc_module
      use cardset_module
      use string_module
      use pclun_module

      implicit none

      private

      public :: process_create
      public :: process_create_super
      public :: process_super_create_only
      public :: process_super_update_only
      public :: process_delete
      public :: process_update
      public :: process_initialize
      public :: process_copy
      public :: process_copy_pdata_cards
      public :: process_copy_jdata_cards
      public :: process_copy_global_cards
      public :: process_copy_process_cards
      public :: process_copy_control_cards
      public :: process_copy_gui_cards
      public :: process_clear_pdata_cards
      public :: process_clear_jdata_cards
      public :: process_clear_global_cards
      public :: process_clear_process_cards
      public :: process_clear_control_cards
      public :: process_clear_gui_cards
      public :: process_clear_mate
      public :: process_clear_previous
      public :: process_clear_next
      public :: process_compare_pdata
      public :: process_compare_jdata
      public :: process_compare_global
      public :: process_compare_process
      public :: process_compare_control
      public :: process_compare_gui
      public :: process_put_pdata_cards
      public :: process_put_jdata_cards
      public :: process_put_global_cards
      public :: process_put_process_cards
      public :: process_put_control_cards
      public :: process_put_gui_cards
      public :: process_alloc_pdata_cards
      public :: process_alloc_jdata_cards
      public :: process_alloc_global_cards
      public :: process_alloc_process_cards
      public :: process_alloc_control_cards
      public :: process_alloc_gui_cards
      public :: process_alloc_keywords
      public :: process_alloc_by_keyword
      public :: process_put_process_cscalar
      public :: process_put_gui_cscalar
      public :: process_put_gui_iscalar
      public :: process_put_process_carray
      public :: process_put_gui_carray
      public :: process_get_name
      public :: process_get_super
      public :: process_get_ipn
      public :: process_get_mate
      public :: process_get_previous
      public :: process_get_next
      public :: process_set_super
      public :: process_set_ipn
      public :: process_set_mate
      public :: process_set_previous
      public :: process_set_next
      public :: process_get_by_keyword
      public :: process_get_keyword_nature
      public :: process_remove_keyword
      public :: process_run_traps
      public :: process_get_errors
      public :: process_get_warnings
      public :: process_get_infos
      public :: process_show_messages
      public :: process_list
      public :: process_set_view
      public :: process_validate

      character(len=100),public,save :: process_ident = &
       '$Id: process.f90,v 1.22 2006/10/30 14:01:44 Glover prod sps $'

      integer,parameter,private   :: STDOUT               = 6
      integer,parameter,public    :: PROCESS_NAME_LEN     = 12
      integer,save,private        :: pc_lun               = 6


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: process_struct              
        private
        character(len=PROCESS_NAME_LEN)        :: name
        integer                                :: ipn
        type(superproc_struct),pointer         :: super
        type(cardset_struct),pointer           :: pdata
        type(cardset_struct),pointer           :: jdata
        type(cardset_struct),pointer           :: global
        type(cardset_struct),pointer           :: process
        type(cardset_struct),pointer           :: control
        type(cardset_struct),pointer           :: gui
        type(process_struct),pointer           :: mate
        type(process_struct),pointer           :: previous
        type(process_struct),pointer           :: next
      end type process_struct

      integer,parameter,public :: PROCESS_PRODUCTION = SUPERPROC_PRODUCTION
      integer,parameter,public :: PROCESS_BETA       = SUPERPROC_BETA
      integer,parameter,public :: PROCESS_ALPHA      = SUPERPROC_ALPHA
      integer,parameter,public :: PROCESS_RAW        = SUPERPROC_RAW


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface process_get_by_keyword
        module procedure process_get_by_scalar
        module procedure process_get_by_element
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine process_create (obj,name,ipn)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*),intent(in)                :: name               !argument
      integer,intent(in)                         :: ipn                !argument

      allocate (obj)
      obj%name = name
      call string_to_upper (obj%name)
      obj%ipn  = ipn
      nullify(obj%super)
      nullify(obj%pdata)
      nullify(obj%jdata)
      nullify(obj%global)
      nullify(obj%process)
      nullify(obj%control)
      nullify(obj%gui)
      call cardset_create (obj%pdata)
      call cardset_set_packing_option (obj%pdata  , CARDSET_PACKED)
      call cardset_create (obj%jdata)
      call cardset_set_packing_option (obj%jdata  , CARDSET_PACKED)
      call cardset_create (obj%global)
      call cardset_set_packing_option (obj%global , CARDSET_PACKED)
      call cardset_create (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_create (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_create (obj%gui)
      call cardset_set_packing_option (obj%gui    , CARDSET_PACKED)
      nullify(obj%mate)
      nullify(obj%previous)
      nullify(obj%next)

      return
      end subroutine process_create


!!-------------------------- create_super ---------------------------------!!
!!-------------------------- create_super ---------------------------------!!
!!-------------------------- create_super ---------------------------------!!


      subroutine process_create_super (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      if (.not. associated(obj)) return

      nullify(cards)
      pc_lun = pclun_get()
      call pc_frontend_update         (pc_lun)
      call pc_set_ipn                 (1)
      call cardset_alloc_cards        (obj%pdata ,cards ,ncards)
      call pc_put_pdata_cards         (cards ,ncards)
      call pc_set_ipn                 (2)
      call cardset_alloc_cards        (obj%jdata ,cards ,ncards)
      call pc_put_jdata_cards         (cards ,ncards)
      call pc_set_ipn                 (obj%ipn)
      call cardset_alloc_cards        (obj%global ,cards ,ncards)
      call pc_put_global_cards        (cards ,ncards)
      call cardset_alloc_cards        (obj%process ,cards ,ncards)
      call pc_put_process_cards       (cards ,ncards)
      call cardset_alloc_cards        (obj%gui ,cards ,ncards)
      call pc_put_gui_cards           (cards ,ncards)
      call superproc_create           (obj%super,obj%name)
      call pc_alloc_pdata_cards       (cards, ncards)
      call cardset_clear              (obj%pdata)
      call cardset_set_packing_option (obj%pdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%pdata, cards, ncards)
      call pc_alloc_jdata_cards       (cards, ncards)
      call cardset_clear              (obj%jdata)
      call cardset_set_packing_option (obj%jdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%jdata, cards, ncards)
      call pc_alloc_global_cards      (cards, ncards)
      call cardset_clear              (obj%global)
      call cardset_set_packing_option (obj%global, CARDSET_PACKED)
      call cardset_put_cards          (obj%global, cards, ncards)
      call pc_alloc_process_cards     (cards, ncards)
      call cardset_clear              (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_put_cards          (obj%process, cards, ncards)
      call pc_alloc_control_cards     (cards, ncards)
      call cardset_clear              (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_put_cards          (obj%control, cards, ncards)
      call pc_alloc_gui_cards         (cards, ncards)
      call cardset_clear              (obj%gui)
      call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
      call cardset_put_cards          (obj%gui, cards, ncards)
      call pc_restore
      if (associated(cards)) deallocate(cards)

      return
      end subroutine process_create_super


!!------------------------ super_create_only ------------------------------!!
!!------------------------ super_create_only ------------------------------!!
!!------------------------ super_create_only ------------------------------!!


      subroutine process_super_create_only (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not. associated(obj)) return

      call superproc_create (obj%super,obj%name)

      return
      end subroutine process_super_create_only


!!------------------------ super_update_only ------------------------------!!
!!------------------------ super_update_only ------------------------------!!
!!------------------------ super_update_only ------------------------------!!


      subroutine process_super_update_only (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not. associated(obj)) return

      if (associated(obj%super)) call superproc_update (obj%super)

      return
      end subroutine process_super_update_only


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine process_delete (obj)
      implicit none
      type(process_struct),pointer :: obj       ! arguments



      if (.not. associated(obj)) return

      pc_lun = pclun_get()
      call pc_frontend_update (pc_lun)
      if (associated(obj%super))   call superproc_delete(obj%super)
      call pc_restore
      if (associated(obj%pdata))   call cardset_delete (obj%pdata)
      if (associated(obj%jdata))   call cardset_delete (obj%jdata)
      if (associated(obj%global))  call cardset_delete (obj%global)
      if (associated(obj%process)) call cardset_delete (obj%process)
      if (associated(obj%control)) call cardset_delete (obj%control)
      if (associated(obj%gui))     call cardset_delete (obj%gui)
      if (associated(obj%mate))    call process_clear_mate (obj%mate)
      deallocate(obj)

      return
      end subroutine process_delete


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine process_update (obj,from_cards,frontend,found_errors)
      implicit none
      type(process_struct),target                :: obj                !argument
      logical,optional                           :: from_cards         !argument
      logical,optional                           :: frontend           !argument
      logical,optional                           :: found_errors       !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local
      character(len=CARDSET_LENGTH),pointer      :: errors(:)          !local
      integer                                    :: nerrors            !local
      character(len=CARDSET_LENGTH),pointer      :: warnings(:)        !local
      integer                                    :: nwarnings          !local

      nullify(cards)
      nullify(errors)
      nullify(warnings)
      nerrors   = 0
      nwarnings = 0

      pc_lun = pclun_get()
      if (present(frontend)) then
        if (frontend) then
          call pc_frontend_update (pc_lun)
        else
          if (present(from_cards)) then
            if (from_cards) call pc_gui_update (pc_lun)
          endif
        endif
      else
        if (present(from_cards)) then
          if (from_cards) call pc_gui_update (pc_lun)
        endif
      endif
      if (present(found_errors)) found_errors = .false.

      call pc_set_ipn                 (1)
      call cardset_alloc_cards        (obj%pdata ,cards ,ncards)
      call pc_put_pdata_cards         (cards ,ncards)
      call pc_set_ipn                 (2)
      call cardset_alloc_cards        (obj%jdata ,cards ,ncards)
      call pc_put_jdata_cards         (cards ,ncards)
      call pc_set_ipn                 (obj%ipn)
      call cardset_alloc_cards        (obj%global ,cards ,ncards)
      call pc_put_global_cards        (cards ,ncards)
      if (present(from_cards)) then
        if (from_cards) then
          call cardset_alloc_cards    (obj%process ,cards ,ncards)
          call pc_put_process_cards   (cards ,ncards)
          call cardset_alloc_cards    (obj%gui ,cards ,ncards)
          call pc_put_gui_cards       (cards ,ncards)
        endif
      endif
      call superproc_update           (obj%super)
      if (present(frontend)) then
        if (frontend) call pc_alloc_gui ('ERROR','ERROR',errors,nerrors)
        if (frontend) call pc_alloc_gui ('WARNING','WARNING',warnings,nwarnings)
      endif
      call pc_alloc_pdata_cards       (cards, ncards)
      call cardset_clear              (obj%pdata)
      call cardset_set_packing_option (obj%pdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%pdata, cards, ncards)
      call pc_alloc_jdata_cards       (cards, ncards)
      call cardset_clear              (obj%jdata)
      call cardset_set_packing_option (obj%jdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%jdata, cards, ncards)
      call pc_alloc_global_cards      (cards, ncards)
      call cardset_clear              (obj%global)
      call cardset_set_packing_option (obj%global, CARDSET_PACKED)
      call cardset_put_cards          (obj%global, cards, ncards)
      call pc_alloc_process_cards     (cards, ncards)
      call cardset_clear              (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_put_cards          (obj%process, cards, ncards)
      call pc_alloc_control_cards     (cards, ncards)
      call cardset_clear              (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_put_cards          (obj%control, cards, ncards)
      call pc_alloc_gui_cards         (cards, ncards)
      call cardset_clear              (obj%gui)
      call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
      call cardset_put_cards          (obj%gui, cards, ncards)
      if (present(from_cards)) then
        if (from_cards) call pc_restore
      endif
      if (present(frontend)) then
        if (frontend) call pc_restore
      endif
      if (nerrors .gt. 0) then
        call pc_put_gui ('ERROR','ERROR',errors,nerrors)
        if (present(found_errors)) found_errors = .true.
      endif
      if (nwarnings .gt. 0) then
        call pc_put_gui ('WARNING','WARNING',warnings,nwarnings)
      endif

      if (associated(errors))   deallocate(errors)
      if (associated(warnings)) deallocate(warnings)
      if (associated(cards))    deallocate(cards)

      return
      end subroutine process_update


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine process_initialize (obj)
      implicit none
      type(process_struct),target                :: obj                !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      nullify(cards)
      call pc_set_ipn                 (1)
      call cardset_alloc_cards        (obj%pdata ,cards ,ncards)
      call pc_put_pdata_cards         (cards ,ncards)
      call pc_set_ipn                 (2)
      call cardset_alloc_cards        (obj%jdata ,cards ,ncards)
      call pc_put_jdata_cards         (cards ,ncards)
      call pc_set_ipn                 (obj%ipn)
      call cardset_alloc_cards        (obj%global ,cards ,ncards)
      call pc_put_global_cards        (cards ,ncards)
      call superproc_initialize       (obj%super)
      call pc_alloc_pdata_cards       (cards, ncards)
      call cardset_clear              (obj%pdata)
      call cardset_set_packing_option (obj%pdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%pdata, cards, ncards)
      call pc_alloc_jdata_cards       (cards, ncards)
      call cardset_clear              (obj%jdata)
      call cardset_set_packing_option (obj%jdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%jdata, cards, ncards)
      call pc_alloc_global_cards      (cards, ncards)
      call cardset_clear              (obj%global)
      call cardset_set_packing_option (obj%global, CARDSET_PACKED)
      call cardset_put_cards          (obj%global, cards, ncards)
      call pc_alloc_process_cards     (cards, ncards)
      call cardset_clear              (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_put_cards          (obj%process, cards, ncards)
      call pc_alloc_control_cards     (cards, ncards)
      call cardset_clear              (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_put_cards          (obj%control, cards, ncards)
      call pc_alloc_gui_cards         (cards, ncards)
      call cardset_clear              (obj%gui)
      call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
      call cardset_put_cards          (obj%gui, cards, ncards)
      if (associated(cards)) deallocate(cards)

      return
      end subroutine process_initialize


!!------------------------------ copy -------------------------------------!!
!!------------------------------ copy -------------------------------------!!
!!------------------------------ copy -------------------------------------!!


      subroutine process_copy (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      character(len=CARDSET_LENGTH),pointer      :: cards(:)           !local
      integer                                    :: ncards             !local

      if (.not. associated(obj1)) return
      if (.not. associated(obj2)) return

      nullify(cards)
      if (associated(obj1%super)) then
        pc_lun = pclun_get()
        call pc_frontend_update         (pc_lun)
        call pc_set_ipn                 (1)
        call cardset_alloc_cards        (obj1%pdata ,cards ,ncards)
        call pc_put_pdata_cards         (cards ,ncards)
        call pc_set_ipn                 (2)
        call cardset_alloc_cards        (obj1%jdata ,cards ,ncards)
        call pc_put_jdata_cards         (cards ,ncards)
        call pc_set_ipn                 (obj2%ipn)
        call cardset_alloc_cards        (obj2%global ,cards ,ncards)
        call pc_put_global_cards        (cards ,ncards)
        call cardset_alloc_cards        (obj1%process ,cards ,ncards)
        call pc_put_process_cards       (cards ,ncards)
        call cardset_alloc_cards        (obj1%gui ,cards ,ncards)
        call pc_put_gui_cards           (cards ,ncards)
        call pc_remove_gui_action       ('ERROR'  ,'ERROR')
        call pc_remove_gui_action       ('WARNING','WARNING')
        call pc_remove_gui_action       ('INFO'   ,'INFO')
        if (associated(obj2%super)) then
          call superproc_update         (obj2%super)
        else
          call superproc_create         (obj2%super ,obj2%name)
        endif
        call pc_alloc_pdata_cards       (cards, ncards)
        call cardset_clear              (obj2%pdata)
        call cardset_set_packing_option (obj2%pdata, CARDSET_PACKED)
        call cardset_put_cards          (obj2%pdata, cards, ncards)
        call pc_alloc_jdata_cards       (cards, ncards)
        call cardset_clear              (obj2%jdata)
        call cardset_set_packing_option (obj2%jdata, CARDSET_PACKED)
        call cardset_put_cards          (obj2%jdata, cards, ncards)
        call pc_alloc_global_cards      (cards, ncards)
        call cardset_clear              (obj2%global)
        call cardset_set_packing_option (obj2%global, CARDSET_PACKED)
        call cardset_put_cards          (obj2%global, cards, ncards)
        call pc_alloc_process_cards     (cards, ncards)
        call cardset_clear              (obj2%process)
        call cardset_set_packing_option (obj2%process, CARDSET_PACKED)
        call cardset_put_cards          (obj2%process, cards, ncards)
        call pc_alloc_control_cards     (cards, ncards)
        call cardset_clear              (obj2%control)
        call cardset_set_packing_option (obj2%control, CARDSET_PACKED)
        call cardset_put_cards          (obj2%control, cards, ncards)
        call pc_alloc_gui_cards         (cards, ncards)
        call cardset_clear              (obj2%gui)
        call cardset_set_packing_option (obj2%gui, CARDSET_PACKED)
        call cardset_put_cards          (obj2%gui, cards, ncards)
        call pc_restore
      else
        call cardset_copy (obj1%pdata   ,obj2%pdata  )
        call cardset_copy (obj1%jdata   ,obj2%jdata  )
        call cardset_copy (obj1%global  ,obj2%global )
        call cardset_copy (obj1%process ,obj2%process)
        call cardset_copy (obj1%control ,obj2%control)
        call cardset_copy (obj1%gui     ,obj2%gui    )
      endif
      if (associated(cards)) deallocate(cards)

      return
      end subroutine process_copy


!!--------------------------- copy_pdata_cards ----------------------------!!
!!--------------------------- copy_pdata_cards ----------------------------!!
!!--------------------------- copy_pdata_cards ----------------------------!!


      subroutine process_copy_pdata_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%pdata ,obj2%pdata)
      else
        call cardset_clear              (obj2%pdata)
        call cardset_set_packing_option (obj2%pdata, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_pdata_cards


!!--------------------------- copy_jdata_cards ----------------------------!!
!!--------------------------- copy_jdata_cards ----------------------------!!
!!--------------------------- copy_jdata_cards ----------------------------!!


      subroutine process_copy_jdata_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%jdata ,obj2%jdata)
      else
        call cardset_clear              (obj2%jdata)
        call cardset_set_packing_option (obj2%jdata, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_jdata_cards


!!-------------------------- copy_global_cards ----------------------------!!
!!-------------------------- copy_global_cards ----------------------------!!
!!-------------------------- copy_global_cards ----------------------------!!


      subroutine process_copy_global_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%global ,obj2%global)
      else
        call cardset_clear              (obj2%global)
        call cardset_set_packing_option (obj2%global, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_global_cards


!!-------------------------- copy_process_cards ---------------------------!!
!!-------------------------- copy_process_cards ---------------------------!!
!!-------------------------- copy_process_cards ---------------------------!!


      subroutine process_copy_process_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%process ,obj2%process)
      else
        call cardset_clear              (obj2%process)
        call cardset_set_packing_option (obj2%process, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_process_cards


!!-------------------------- copy_control_cards ---------------------------!!
!!-------------------------- copy_control_cards ---------------------------!!
!!-------------------------- copy_control_cards ---------------------------!!


      subroutine process_copy_control_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%control ,obj2%control)
      else
        call cardset_clear              (obj2%control)
        call cardset_set_packing_option (obj2%control, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_control_cards


!!---------------------------- copy_gui_cards -----------------------------!!
!!---------------------------- copy_gui_cards -----------------------------!!
!!---------------------------- copy_gui_cards -----------------------------!!


      subroutine process_copy_gui_cards (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument

      if (.not.associated(obj2)) return

      if (associated(obj1)) then
        call cardset_copy               (obj1%gui ,obj2%gui)
      else
        call cardset_clear              (obj2%gui)
        call cardset_set_packing_option (obj2%gui, CARDSET_PACKED)
      endif

      return
      end subroutine process_copy_gui_cards


!!-------------------------- clear_pdata_cards ----------------------------!!
!!-------------------------- clear_pdata_cards ----------------------------!!
!!-------------------------- clear_pdata_cards ----------------------------!!


      subroutine process_clear_pdata_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%pdata)
      call cardset_set_packing_option (obj%pdata, CARDSET_PACKED)
       
      return
      end subroutine process_clear_pdata_cards


!!-------------------------- clear_jdata_cards ----------------------------!!
!!-------------------------- clear_jdata_cards ----------------------------!!
!!-------------------------- clear_jdata_cards ----------------------------!!


      subroutine process_clear_jdata_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%jdata)
      call cardset_set_packing_option (obj%jdata, CARDSET_PACKED)
       
      return
      end subroutine process_clear_jdata_cards


!!------------------------- clear_global_cards ----------------------------!!
!!------------------------- clear_global_cards ----------------------------!!
!!------------------------- clear_global_cards ----------------------------!!


      subroutine process_clear_global_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%global)
      call cardset_set_packing_option (obj%global, CARDSET_PACKED)
       
      return
      end subroutine process_clear_global_cards


!!------------------------- clear_process_cards ---------------------------!!
!!------------------------- clear_process_cards ---------------------------!!
!!------------------------- clear_process_cards ---------------------------!!


      subroutine process_clear_process_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
       
      return
      end subroutine process_clear_process_cards


!!------------------------- clear_control_cards ---------------------------!!
!!------------------------- clear_control_cards ---------------------------!!
!!------------------------- clear_control_cards ---------------------------!!


      subroutine process_clear_control_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
       
      return
      end subroutine process_clear_control_cards


!!--------------------------- clear_gui_cards -----------------------------!!
!!--------------------------- clear_gui_cards -----------------------------!!
!!--------------------------- clear_gui_cards -----------------------------!!


      subroutine process_clear_gui_cards (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      call cardset_clear              (obj%gui)
      call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
       
      return
      end subroutine process_clear_gui_cards


!!----------------------------- clear_mate --------------------------------!!
!!----------------------------- clear_mate --------------------------------!!
!!----------------------------- clear_mate --------------------------------!!


      subroutine process_clear_mate (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      nullify(obj%mate)
       
      return
      end subroutine process_clear_mate


!!--------------------------- clear_previous ------------------------------!!
!!--------------------------- clear_previous ------------------------------!!
!!--------------------------- clear_previous ------------------------------!!


      subroutine process_clear_previous (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      nullify(obj%previous)
       
      return
      end subroutine process_clear_previous


!!----------------------------- clear_next --------------------------------!!
!!----------------------------- clear_next --------------------------------!!
!!----------------------------- clear_next --------------------------------!!


      subroutine process_clear_next (obj)
      implicit none
      type(process_struct),pointer               :: obj                !argument

      if (.not.associated(obj)) return

      nullify(obj%next)
       
      return
      end subroutine process_clear_next


!!--------------------------- compare_pdata -------------------------------!!
!!--------------------------- compare_pdata -------------------------------!!
!!--------------------------- compare_pdata -------------------------------!!


      function process_compare_pdata (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%pdata)
      nkeys2 = cardset_num_keywords (obj2%pdata)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%pdata,i)
        if (cardset_keyword_present (obj2%pdata, keyword)) then
          nature1 = cardset_nature (obj1%pdata, keyword)
          nature2 = cardset_nature (obj2%pdata, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%pdata,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%pdata,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array (obj1%pdata,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%pdata,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_pdata


!!--------------------------- compare_jdata -------------------------------!!
!!--------------------------- compare_jdata -------------------------------!!
!!--------------------------- compare_jdata -------------------------------!!


      function process_compare_jdata (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%jdata)
      nkeys2 = cardset_num_keywords (obj2%jdata)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%jdata,i)
        if (cardset_keyword_present (obj2%jdata, keyword)) then
          nature1 = cardset_nature (obj1%jdata, keyword)
          nature2 = cardset_nature (obj2%jdata, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%jdata,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%jdata,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array (obj1%jdata,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%jdata,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_jdata




!!--------------------------- compare_global ------------------------------!!
!!--------------------------- compare_global ------------------------------!!
!!--------------------------- compare_global ------------------------------!!


      function process_compare_global (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%global)
      nkeys2 = cardset_num_keywords (obj2%global)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%global,i)
        if (cardset_keyword_present (obj2%global, keyword)) then
          nature1 = cardset_nature (obj1%global, keyword)
          nature2 = cardset_nature (obj2%global, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%global,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%global,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array (obj1%global,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%global,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_global




!!--------------------------- compare_process -----------------------------!!
!!--------------------------- compare_process -----------------------------!!
!!--------------------------- compare_process -----------------------------!!


      function process_compare_process (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%process)
      nkeys2 = cardset_num_keywords (obj2%process)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%process,i)
        if (cardset_keyword_present (obj2%process, keyword)) then
          nature1 = cardset_nature (obj1%process, keyword)
          nature2 = cardset_nature (obj2%process, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%process,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%process,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array(obj1%process,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%process,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_process


!!--------------------------- compare_control -----------------------------!!
!!--------------------------- compare_control -----------------------------!!
!!--------------------------- compare_control -----------------------------!!


      function process_compare_control (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%control)
      nkeys2 = cardset_num_keywords (obj2%control)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%control,i)
        if (cardset_keyword_present (obj2%control, keyword)) then
          nature1 = cardset_nature (obj1%control, keyword)
          nature2 = cardset_nature (obj2%control, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%control,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%control,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array(obj1%control,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%control,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_control


!!----------------------------- compare_gui -------------------------------!!
!!----------------------------- compare_gui -------------------------------!!
!!----------------------------- compare_gui -------------------------------!!

      function process_compare_gui (obj1, obj2) result (same)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
      logical                                    :: same               !result

      character(len=PC_LENGTH)                   :: errmsg             !local
      character(len=PC_LENGTH)                   :: keyword            !local
      character(len=PC_LENGTH)                   :: value              !local
      character(len=PC_LENGTH),pointer           :: array(:)           !local
      integer                                    :: i                  !local
      integer                                    :: nkeys1             !local
      integer                                    :: nkeys2             !local
      integer                                    :: nature1            !local
      integer                                    :: nature2            !local
      integer                                    :: narray             !local

      nullify (array) ! jpa
      same = .true.
      nkeys1 = cardset_num_keywords (obj1%gui)
      nkeys2 = cardset_num_keywords (obj2%gui)
      if (nkeys1 .ne. nkeys2) then
        same = .false.
        return
      endif
      do i=1,nkeys1
        keyword = cardset_get_keyword (obj1%gui,i)
        if (cardset_keyword_present (obj2%gui, keyword)) then
          nature1 = cardset_nature (obj1%gui, keyword)
          nature2 = cardset_nature (obj2%gui, keyword)
          if (nature1 .eq. nature2) then
            if (nature1 .eq. CARDSET_SCALAR) then
              call cardset_get_scalar (obj1%gui,keyword,value,errmsg)
              if (cardset_scalar_matches (obj2%gui,keyword,value)) then
                cycle
              else
                same = .false.
                exit
              endif
            else if (nature1 .eq. CARDSET_ARRAY) then
              call cardset_alloc_array(obj1%gui,keyword,array,narray,errmsg)
              if (cardset_array_matches(obj2%gui,keyword,array,narray)) then
                cycle
              else
                same = .false.
                exit
              endif
            endif
          else
            same = .false.
            exit
          endif
        else
          same = .false.
          exit
        endif
      enddo
       
      return
      end function process_compare_gui


!!--------------------------- put_pdata_cards -----------------------------!!
!!--------------------------- put_pdata_cards -----------------------------!!
!!--------------------------- put_pdata_cards -----------------------------!!


      subroutine process_put_pdata_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%pdata)
      call cardset_set_packing_option (obj%pdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%pdata, cards, ncards)
       
      return
      end subroutine process_put_pdata_cards


!!--------------------------- put_jdata_cards -----------------------------!!
!!--------------------------- put_jdata_cards -----------------------------!!
!!--------------------------- put_jdata_cards -----------------------------!!


      subroutine process_put_jdata_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%jdata)
      call cardset_set_packing_option (obj%jdata, CARDSET_PACKED)
      call cardset_put_cards          (obj%jdata, cards, ncards)
       
      return
      end subroutine process_put_jdata_cards


!!-------------------------- put_global_cards -----------------------------!!
!!-------------------------- put_global_cards -----------------------------!!
!!-------------------------- put_global_cards -----------------------------!!


      subroutine process_put_global_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%global)
      call cardset_set_packing_option (obj%global, CARDSET_PACKED)
      call cardset_put_cards          (obj%global, cards, ncards)
       
      return
      end subroutine process_put_global_cards


!!-------------------------- put_process_cards ----------------------------!!
!!-------------------------- put_process_cards ----------------------------!!
!!-------------------------- put_process_cards ----------------------------!!


      subroutine process_put_process_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%process)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_put_cards          (obj%process, cards, ncards)
       
      return
      end subroutine process_put_process_cards


!!-------------------------- put_control_cards ----------------------------!!
!!-------------------------- put_control_cards ----------------------------!!
!!-------------------------- put_control_cards ----------------------------!!


      subroutine process_put_control_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%control)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_put_cards          (obj%control, cards, ncards)
       
      return
      end subroutine process_put_control_cards


!!---------------------------- put_gui_cards ------------------------------!!
!!---------------------------- put_gui_cards ------------------------------!!
!!---------------------------- put_gui_cards ------------------------------!!


      subroutine process_put_gui_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: cards(:)           !argument
      integer             ,intent(in)            :: ncards             !argument

      call cardset_clear              (obj%gui)
      call cardset_set_packing_option (obj%gui, CARDSET_PACKED)
      call cardset_put_cards          (obj%gui, cards, ncards)
       
      return
      end subroutine process_put_gui_cards


!!-------------------------- alloc_pdata_cards ----------------------------!!
!!-------------------------- alloc_pdata_cards ----------------------------!!
!!-------------------------- alloc_pdata_cards ----------------------------!!


      subroutine process_alloc_pdata_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%pdata, cards, ncards)
       
      return
      end subroutine process_alloc_pdata_cards


!!-------------------------- alloc_jdata_cards ----------------------------!!
!!-------------------------- alloc_jdata_cards ----------------------------!!
!!-------------------------- alloc_jdata_cards ----------------------------!!


      subroutine process_alloc_jdata_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%jdata, cards, ncards)
       
      return
      end subroutine process_alloc_jdata_cards


!!------------------------- alloc_global_cards ----------------------------!!
!!------------------------- alloc_global_cards ----------------------------!!
!!------------------------- alloc_global_cards ----------------------------!!


      subroutine process_alloc_global_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%global, cards, ncards)
       
      return
      end subroutine process_alloc_global_cards


!!------------------------- alloc_process_cards ---------------------------!!
!!------------------------- alloc_process_cards ---------------------------!!
!!------------------------- alloc_process_cards ---------------------------!!


      subroutine process_alloc_process_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%process, cards, ncards)
       
      return
      end subroutine process_alloc_process_cards


!!------------------------- alloc_control_cards ---------------------------!!
!!------------------------- alloc_control_cards ---------------------------!!
!!------------------------- alloc_control_cards ---------------------------!!


      subroutine process_alloc_control_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%control, cards, ncards)
       
      return
      end subroutine process_alloc_control_cards


!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!
!!--------------------------- alloc_gui_cards -----------------------------!!


      subroutine process_alloc_gui_cards (obj, cards, ncards)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,pointer               :: cards(:)           !argument
      integer             ,intent(out)           :: ncards             !argument

      call cardset_alloc_cards        (obj%gui, cards, ncards)
       
      return
      end subroutine process_alloc_gui_cards


!!------------------------- put_process_cscalar ---------------------------!!
!!------------------------- put_process_cscalar ---------------------------!!
!!------------------------- put_process_cscalar ---------------------------!!


      subroutine process_put_process_cscalar (obj, keyword, scalar)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,intent(in)            :: scalar             !argument

      call cardset_put_scalar (obj%process, keyword, scalar)
       
      return
      end subroutine process_put_process_cscalar


!!--------------------------- put_gui_cscalar -----------------------------!!
!!--------------------------- put_gui_cscalar -----------------------------!!
!!--------------------------- put_gui_cscalar -----------------------------!!


      subroutine process_put_gui_cscalar (obj, keyword, scalar)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,intent(in)            :: scalar             !argument

      call cardset_put_scalar (obj%gui, keyword, scalar)
       
      return
      end subroutine process_put_gui_cscalar


!!--------------------------- put_gui_iscalar -----------------------------!!
!!--------------------------- put_gui_iscalar -----------------------------!!
!!--------------------------- put_gui_iscalar -----------------------------!!


      subroutine process_put_gui_iscalar (obj, keyword, scalar)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      integer             ,intent(in)            :: scalar             !argument

      call cardset_put_scalar (obj%gui, keyword, scalar)
       
      return
      end subroutine process_put_gui_iscalar


!!------------------------- put_process_carray ----------------------------!!
!!------------------------- put_process_carray ----------------------------!!
!!------------------------- put_process_carray ----------------------------!!


      subroutine process_put_process_carray (obj, keyword, array, narray)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,intent(in)            :: array(:)           !argument
      integer             ,intent(in)            :: narray             !argument

      call cardset_put_array (obj%process, keyword, array, narray)
       
      return
      end subroutine process_put_process_carray


!!--------------------------- put_gui_carray ------------------------------!!
!!--------------------------- put_gui_carray ------------------------------!!
!!--------------------------- put_gui_carray ------------------------------!!


      subroutine process_put_gui_carray (obj, keyword, array, narray)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,intent(in)            :: array(:)           !argument
      integer             ,intent(in)            :: narray             !argument

      call cardset_put_array (obj%gui, keyword, array, narray)
       
      return
      end subroutine process_put_gui_carray


!!------------------------------ get_name ---------------------------------!!
!!------------------------------ get_name ---------------------------------!!
!!------------------------------ get_name ---------------------------------!!


      subroutine process_get_name (obj, name)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(out)           :: name               !argument
       
      if (associated(obj)) then
        name = obj%name
      else
        name = ' '
      endif

      return
      end subroutine process_get_name


!!------------------------------ get_super --------------------------------!!
!!------------------------------ get_super --------------------------------!!
!!------------------------------ get_super --------------------------------!!


      subroutine process_get_super (obj, super)
      implicit none
      type(process_struct)  ,pointer         :: obj                !argument
      type(superproc_struct),pointer         :: super              !argument
       
      if (associated(obj)) then
        if (associated(obj%super)) then
          super => obj%super
        else
          nullify(super)
        endif
      else
        nullify(super)
      endif

      return
      end subroutine process_get_super


!!------------------------------- get_ipn ---------------------------------!!
!!------------------------------- get_ipn ---------------------------------!!
!!------------------------------- get_ipn ---------------------------------!!


      subroutine process_get_ipn (obj, ipn)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      integer             ,intent(out)           :: ipn                !argument
       
      if (associated(obj)) then
        ipn = obj%ipn
      else
        ipn = 0
      endif

      return
      end subroutine process_get_ipn


!!------------------------------ get_mate ---------------------------------!!
!!------------------------------ get_mate ---------------------------------!!
!!------------------------------ get_mate ---------------------------------!!


      subroutine process_get_mate (obj, mate)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      type(process_struct),pointer               :: mate               !argument
       
      if (associated(obj)) then
        if (associated(obj%mate)) then
          mate => obj%mate
        else
          nullify(mate)
        endif
      else
        nullify(mate)
      endif

      return
      end subroutine process_get_mate


!!---------------------------- get_previous -------------------------------!!
!!---------------------------- get_previous -------------------------------!!
!!---------------------------- get_previous -------------------------------!!


      subroutine process_get_previous (obj, previous)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      type(process_struct),pointer               :: previous           !argument
       
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
      end subroutine process_get_previous


!!------------------------------ get_next ---------------------------------!!
!!------------------------------ get_next ---------------------------------!!
!!------------------------------ get_next ---------------------------------!!


      subroutine process_get_next (obj, next)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      type(process_struct),pointer               :: next               !argument
       
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
      end subroutine process_get_next


!!------------------------------ set_super --------------------------------!!
!!------------------------------ set_super --------------------------------!!
!!------------------------------ set_super --------------------------------!!


      subroutine process_set_super (obj, super)
      implicit none
      type(process_struct)  ,pointer         :: obj                !argument
      type(superproc_struct),pointer         :: super              !argument
       
      if (.not. associated(obj)) return

      if (associated(super)) then
        obj%super => super
      else
        nullify(obj%super)
      endif

      return
      end subroutine process_set_super


!!------------------------------- set_ipn ---------------------------------!!
!!------------------------------- set_ipn ---------------------------------!!
!!------------------------------- set_ipn ---------------------------------!!


      subroutine process_set_ipn (obj, ipn)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      integer             ,intent(in)            :: ipn                !argument
       
      if (.not. associated(obj)) return

      obj%ipn = ipn

      return
      end subroutine process_set_ipn


!!------------------------------ set_mate ---------------------------------!!
!!------------------------------ set_mate ---------------------------------!!
!!------------------------------ set_mate ---------------------------------!!


      subroutine process_set_mate (obj1, obj2)
      implicit none
      type(process_struct),pointer               :: obj1               !argument
      type(process_struct),pointer               :: obj2               !argument
       
      if (.not. associated(obj1)) return
      if (.not. associated(obj2)) return

      obj1%mate => obj2
      obj2%mate => obj1

      return
      end subroutine process_set_mate


!!---------------------------- set_previous -------------------------------!!
!!---------------------------- set_previous -------------------------------!!
!!---------------------------- set_previous -------------------------------!!


      subroutine process_set_previous (obj, previous)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      type(process_struct),pointer               :: previous           !argument
       
      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine process_set_previous


!!------------------------------ set_next ---------------------------------!!
!!------------------------------ set_next ---------------------------------!!
!!------------------------------ set_next ---------------------------------!!


      subroutine process_set_next (obj, next)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      type(process_struct),pointer               :: next               !argument
       
      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif
       
      return
      end subroutine process_set_next


!!----------------------------- get_by_scalar -----------------------------!!
!!----------------------------- get_by_scalar -----------------------------!!
!!----------------------------- get_by_scalar -----------------------------!!


      subroutine process_get_by_scalar (obj, class, keyword, value)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,intent(out)           :: value              !argument

      character(len=PC_LENGTH)                   :: local_class        !local
      character(len=PC_LENGTH)                   :: errmsg             !local
      type(cardset_struct),pointer               :: ptemp              !local
       
      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      value = 'NONE'
      call cardset_get_scalar (ptemp, keyword, value, errmsg)

      return
      end subroutine process_get_by_scalar


!!---------------------------- get_by_element -----------------------------!!
!!---------------------------- get_by_element -----------------------------!!
!!---------------------------- get_by_element -----------------------------!!


      subroutine process_get_by_element (obj, class, keyword, indx, value)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      integer             ,intent(in)            :: indx               !argument
      character(len=*)    ,intent(out)           :: value              !argument

      character(len=PC_LENGTH)                   :: local_class        !local
      character(len=PC_LENGTH)                   :: errmsg             !local
      type(cardset_struct),pointer               :: ptemp              !local
       
      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      value = 'NONE'
      call cardset_get_element (ptemp, keyword, indx, value, errmsg)

      return
      end subroutine process_get_by_element


!!--------------------------- alloc_by_keyword ----------------------------!!
!!--------------------------- alloc_by_keyword ----------------------------!!
!!--------------------------- alloc_by_keyword ----------------------------!!


      subroutine process_alloc_by_keyword (obj, class, keyword, array, narray)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      character(len=*)    ,pointer               :: array(:)           !argument
      integer             ,intent(inout)         :: narray             !argument

      character(len=PC_LENGTH)                   :: local_class        !local
      character(len=PC_LENGTH)                   :: errmsg             !local
      type(cardset_struct),pointer               :: ptemp              !local
       
      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      call cardset_alloc_array (ptemp, keyword, array, narray, errmsg)

      return
      end subroutine process_alloc_by_keyword


!!-------------------------- get_keyword_nature ---------------------------!!
!!-------------------------- get_keyword_nature ---------------------------!!
!!-------------------------- get_keyword_nature ---------------------------!!


      function process_get_keyword_nature (obj, class, keyword) result (nature)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,intent(in)            :: keyword            !argument
      integer                                    :: nature             !result  

      character(len=PC_LENGTH)                   :: local_class        !local
      type(cardset_struct),pointer               :: ptemp              !local
       
      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      nature = cardset_nature (ptemp, keyword)

      return
      end function process_get_keyword_nature


!!---------------------------- alloc_keywords -----------------------------!!
!!---------------------------- alloc_keywords -----------------------------!!
!!---------------------------- alloc_keywords -----------------------------!!


      subroutine process_alloc_keywords (obj, class, array, narray)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,pointer               :: array(:)           !argument
      integer             ,intent(inout)         :: narray             !argument

      integer                                    :: i                  !local
      character(len=PC_LENGTH)                   :: local_class        !local
      type(cardset_struct),pointer               :: ptemp              !local

      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      narray = cardset_num_keywords(ptemp)
      if (associated(array)) deallocate(array)
      if (narray .gt. 0) then
        allocate(array(narray))
        do i=1,narray
          array(i) = cardset_get_keyword(ptemp,i)
        enddo
      else
        allocate(array(1))
        array(1) = ' '
      endif

      return
      end subroutine process_alloc_keywords


!!---------------------------- remove_keyword -----------------------------!!
!!---------------------------- remove_keyword -----------------------------!!
!!---------------------------- remove_keyword -----------------------------!!


      subroutine process_remove_keyword (obj, class, keyword)
      implicit none
      type(process_struct),pointer               :: obj                !argument
      character(len=*)    ,intent(in)            :: class              !argument
      character(len=*)    ,intent(in)            :: keyword            !argument

      character(len=PC_LENGTH)                   :: local_class        !local
      type(cardset_struct),pointer               :: ptemp              !local
       
      local_class = class
      call string_to_upper (local_class)
      select case (trim(local_class))
        case ('PDATA')     ; ptemp => obj%pdata
        case ('JDATA')     ; ptemp => obj%jdata
        case ('GLOBAL')    ; ptemp => obj%global
        case ('PROCESS')   ; ptemp => obj%process
        case ('CONTROL')   ; ptemp => obj%control
        case ('GUI')       ; ptemp => obj%gui
      end select
      call cardset_remove_keyword (ptemp, keyword)

      return
      end subroutine process_remove_keyword


!!------------------------------ run_traps --------------------------------!!
!!------------------------------ run_traps --------------------------------!!
!!------------------------------ run_traps --------------------------------!!


      subroutine process_run_traps (obj,errors,warnings,infos,from_cards)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=CARDSET_LENGTH),pointer         :: errors(:)      ! argument
      character(len=CARDSET_LENGTH),pointer         :: warnings(:)    ! argument
      character(len=CARDSET_LENGTH),pointer         :: infos(:)       ! argument
      logical,optional                              :: from_cards     ! argument

      character(len=CARDSET_LENGTH),pointer         :: cards(:)       ! local
      integer                                       :: ncards         ! local
      integer                                       :: nerrors        ! local
      integer                                       :: nwarnings      ! local
      integer                                       :: ninfos         ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local
      type(process_struct)        ,pointer          :: previous       ! local
      type(superproc_struct)      ,pointer          :: super          ! local

      nullify (previous) ! jpa
      nullify (super) ! jpa
      if (associated(errors)) then
        nerrors = size(errors)
      else
        nerrors   = 0
      endif
      if (associated(warnings)) then
        nwarnings = size(warnings)
      else
        nwarnings = 0
      endif
      if (associated(infos)) then
        ninfos = size(infos)
      else
        ninfos    = 0
      endif

      pc_lun = pclun_get()
      call pc_frontend_update (pc_lun)

      if (present(from_cards)) then
        if (from_cards) then
          call process_alloc_process_cards (obj ,cards ,ncards)
          call pc_put_process_cards        (cards ,ncards)
        endif
      endif

      call process_get_previous (obj,previous)
      call process_copy_global_cards  (previous,obj)
      call pc_set_ipn             (1)
      call cardset_alloc_cards    (obj%pdata ,cards ,ncards)
      call pc_put_pdata_cards     (cards ,ncards)
      call pc_set_ipn             (2)
      call cardset_alloc_cards    (obj%jdata ,cards ,ncards)
      call pc_put_jdata_cards     (cards ,ncards)
      call pc_set_ipn             (obj%ipn)
      call cardset_alloc_cards    (obj%global ,cards ,ncards)
      call pc_put_global_cards    (cards ,ncards)
      call process_get_super      (obj,super)
      call superproc_update       (super)

      ncards = 0
      call pc_alloc_gui ('ERROR','ERROR',cards,ncards)
      if (ncards .gt. 0) then
        if (nerrors .eq. 0) then
          if (associated(errors)) deallocate(errors)
          nerrors = ncards + 1
          allocate(errors(nerrors))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          errors(2:nerrors) = cards
        else
          allocate(temp_cards(nerrors))
          temp_cards = errors
          deallocate(errors)
          allocate(errors(nerrors+ncards+1))
          errors(1:nerrors) = temp_cards(1:nerrors)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(nerrors+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          errors(nerrors+2:nerrors+ncards+1) = cards
          nerrors = nerrors + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      ncards = 0
      call pc_alloc_gui ('WARNING','WARNING',cards,ncards)
      if (ncards .gt. 0) then
        if (nwarnings .eq. 0) then
          if (associated(warnings)) deallocate(warnings)
          nwarnings = ncards + 1
          allocate(warnings(nwarnings))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          warnings(2:nwarnings) = cards
        else
          allocate(temp_cards(nwarnings))
          temp_cards = warnings
          deallocate(warnings)
          allocate(warnings(nwarnings+ncards+1))

          warnings(1:nwarnings) = temp_cards(1:nwarnings)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(nwarnings+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          warnings(nwarnings+2:nwarnings+ncards+1) = cards
          nwarnings = nwarnings + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      ncards = 0
      call pc_alloc_gui ('INFO','INFO',cards,ncards)
      if (ncards .gt. 0) then
        if (ninfos .eq. 0) then
          if (associated(infos)) deallocate(infos)
          ninfos = ncards + 1
          allocate(infos(ninfos))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          infos(2:ninfos) = cards
        else
          allocate(temp_cards(ninfos))
          temp_cards = infos
          deallocate(infos)
          allocate(infos(ninfos+ncards+1))
          infos(1:ninfos) = temp_cards(1:ninfos)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(ninfos+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          infos(ninfos+2:ninfos+ncards+1) = cards
          ninfos = ninfos + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      call pc_alloc_pdata_cards      (cards, ncards)
      call process_put_pdata_cards   (obj, cards, ncards)
      call pc_alloc_jdata_cards      (cards, ncards)
      call process_put_jdata_cards   (obj, cards, ncards)
      call pc_alloc_global_cards     (cards, ncards)
      call process_put_global_cards  (obj, cards, ncards)
      call pc_alloc_process_cards    (cards, ncards)
      call process_put_process_cards (obj, cards, ncards)
      call pc_alloc_control_cards    (cards, ncards)
      call process_put_control_cards (obj, cards, ncards)
      call pc_alloc_gui_cards        (cards, ncards)
      call process_put_gui_cards     (obj, cards, ncards)

      if (associated(cards)) deallocate(cards)
      call pc_restore

      end subroutine process_run_traps


!!----------------------------- get_errors ---------------------------------!!
!!----------------------------- get_errors ---------------------------------!!
!!----------------------------- get_errors ---------------------------------!!


      subroutine process_get_errors (obj,errors,nerrors)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=*),pointer                      :: errors(:)      ! argument
      integer                                       :: nerrors        ! argument

      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),pointer         :: cards(:)       ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local
      character(len=CARDSET_LENGTH)                 :: errmsg         ! local

      nullify(cards)
      allocate(cards(1))

      ncards = 0
      call cardset_alloc_array (obj%gui,'ERROR#ERROR',cards,ncards,errmsg)
      if (ncards .gt. 0) then
        if (nerrors .eq. 0) then
          if (associated(errors)) deallocate(errors)
          nerrors = ncards + 1
          allocate(errors(nerrors))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          errors(2:nerrors) = cards
        else
          allocate(temp_cards(nerrors))
          temp_cards = errors
          deallocate(errors)
          allocate(errors(nerrors+ncards+1))
          errors(1:nerrors) = temp_cards(1:nerrors)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          errors(nerrors+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          errors(nerrors+2:nerrors+ncards+1) = cards
          nerrors = nerrors + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      deallocate(cards)

      return
      end subroutine process_get_errors


!!---------------------------- get_warnings --------------------------------!!
!!---------------------------- get_warnings --------------------------------!!
!!---------------------------- get_warnings --------------------------------!!


      subroutine process_get_warnings (obj,warnings,nwarnings)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=*),pointer                      :: warnings(:)    ! argument
      integer                                       :: nwarnings      ! argument

      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),pointer         :: cards(:)       ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local
      character(len=CARDSET_LENGTH)                 :: errmsg         ! local

      nullify(cards)
      allocate(cards(1))

      ncards = 0
      call cardset_alloc_array (obj%gui,'WARNING#WARNING',cards,ncards,errmsg)
      if (ncards .gt. 0) then
        if (nwarnings .eq. 0) then
          if (associated(warnings)) deallocate(warnings)
          nwarnings = ncards + 1
          allocate(warnings(nwarnings))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          warnings(2:nwarnings) = cards
        else
          allocate(temp_cards(nwarnings))
          temp_cards = warnings
          deallocate(warnings)
          allocate(warnings(nwarnings+ncards+1))
          warnings(1:nwarnings) = temp_cards(1:nwarnings)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          warnings(nwarnings+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          warnings(nwarnings+2:nwarnings+ncards+1) = cards
          nwarnings = nwarnings + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      deallocate(cards)

      return
      end subroutine process_get_warnings


!!------------------------------ get_infos ---------------------------------!!
!!------------------------------ get_infos ---------------------------------!!
!!------------------------------ get_infos ---------------------------------!!


      subroutine process_get_infos (obj,infos,ninfos)
      implicit none
      type(process_struct),pointer                  :: obj            ! argument
      character(len=*),pointer                      :: infos(:)       ! argument
      integer                                       :: ninfos         ! argument

      integer                                       :: ncards         ! local
      character(len=CARDSET_LENGTH),pointer         :: cards(:)       ! local
      character(len=CARDSET_LENGTH),allocatable     :: temp_cards(:)  ! local

      character(len=CARDSET_LENGTH)                 :: process_name   ! local
      integer                                       :: ipn            ! local
      character(len=10)                             :: cipn           ! local
      character(len=CARDSET_LENGTH)                 :: errmsg         ! local

      nullify(cards)
      allocate(cards(1))

      ncards = 0
      call cardset_alloc_array (obj%gui,'INFO#INFO',cards,ncards,errmsg)
      if (ncards .gt. 0) then
        if (ninfos .eq. 0) then
          if (associated(infos)) deallocate(infos)
          ninfos = ncards + 1
          allocate(infos(ninfos))
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(1) = '---- Process '//trim(cipn)//'  '//  &
                      trim(process_name)//'----'
          infos(2:ninfos) = cards
        else
          allocate(temp_cards(ninfos))
          temp_cards = infos
          deallocate(infos)
          allocate(infos(ninfos+ncards+1))
          infos(1:ninfos) = temp_cards(1:ninfos)
          call process_get_name (obj,process_name)
          call process_get_ipn  (obj,ipn)
          call string_ii2cc     (ipn,cipn)
          infos(ninfos+1) = '---- Process '//trim(cipn)//'  '//  &
                              trim(process_name)//'----'
          infos(ninfos+2:ninfos+ncards+1) = cards
          ninfos = ninfos + ncards + 1
          deallocate(temp_cards)
        endif
      endif

      deallocate(cards)

      return
      end subroutine process_get_infos


!!---------------------------- show_messages -------------------------------!!
!!---------------------------- show_messages -------------------------------!!
!!---------------------------- show_messages -------------------------------!!


      subroutine process_show_messages (process)
      implicit none
      type(process_struct),pointer                  :: process        ! argument

      integer                                       :: i              ! local
      integer                                       :: narray         ! local
      character(len=CARDSET_LENGTH),pointer         :: array(:)       ! local
      character(len=CARDSET_LENGTH)                 :: errmsg         ! local

      nullify(array)
      allocate(array(1))

      narray = 0
      call cardset_alloc_array (process%gui,'ERROR#ERROR',array,narray,errmsg)
      if (narray .gt. 0) then
        call pc_put_gui ('ERROR','ERROR',array,narray)
        do i=1,narray
          write(STDOUT,*) 'ERROR -  ',array(i)
        enddo
      endif

      narray = 0
      call cardset_alloc_array (process%gui,'WARNING#WARNING',  &
                                                            array,narray,errmsg)
      if (narray .gt. 0) then
        call pc_put_gui ('WARNING','WARNING',array,narray)
        do i=1,narray
          write(STDOUT,*) 'WARNING -  ',array(i)
        enddo
      endif

      narray = 0
      call cardset_alloc_array (process%gui,'INFO#INFO',array,narray,errmsg)
      if (narray .gt. 0) then
        call pc_put_gui ('INFO','INFO',array,narray)
        do i=1,narray
          write(STDOUT,*) 'INFO -  ',array(i)
        enddo
      endif

      if (associated(array)) deallocate(array)

      return
      end subroutine process_show_messages


!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!


      subroutine process_list (list,nlist,catagory)
      implicit none
      character(len=*),pointer                   :: list(:)            !argument
      integer         ,intent(out)               :: nlist              !argument
      character(len=*),optional                  :: catagory           !argument


      if (present(catagory)) then
        call superproc_list (list,nlist,catagory)
      else
        call superproc_list (list,nlist)
      endif

      return
      end subroutine process_list


!!------------------------------- set_view ---------------------------------!!
!!------------------------------- set_view ---------------------------------!!
!!------------------------------- set_view ---------------------------------!!


      subroutine process_set_view (i)
      implicit none
      integer,intent(in)                         :: i                  !argument

      call superproc_set_view (i)

      return
      end subroutine process_set_view


!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!


      function process_validate (process_name) result (valid)
      implicit none
      character(len=*),intent(in)                :: process_name       !argument
      logical                                    :: valid              !result

      valid = superproc_validate (process_name)

      return
      end function process_validate


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module process_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

