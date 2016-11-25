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
! Name       : awindow 
! Category   : cfe
! Written    : 1999-09-07   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE Window Object Module.
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
!007. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!  6. 2006-01-10  B. Menger    Removed Unused Variables.
!  5. 2003-00-15  Stoeckley    Changed name from window to awindow;
!                               changed type to primitive; changed category to
!                               cfe.
!  4. 2002-01-04  Vunderink    Made mwb_param to buildlist name changes.
!  3. 2000-09-04  Vunderink    Added use string_module
!  2. 2000-08-15  Vunderink    Made enhancements for multi-workfile builder
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

      module awindow_module
      use pc_module
      use cardset_module
      use process_module
      use buildlist_module
      use filebox_module
      use string_module

      implicit none

      private
      public :: awindow_create
      public :: awindow_delete
      public :: awindow_duplicate
      public :: awindow_get_window_id
      public :: awindow_get_window_type
      public :: awindow_get_keyword
      public :: awindow_get_value
      public :: awindow_get_index
      public :: awindow_get_process
      public :: awindow_get_parameter
      public :: awindow_get_filebox
      public :: awindow_get_cardset
      public :: awindow_get_previous
      public :: awindow_get_next
      public :: awindow_alloc_keys
      public :: awindow_set_keys
      public :: awindow_set_window_id
      public :: awindow_set_window_type
      public :: awindow_set_keyword
      public :: awindow_set_value
      public :: awindow_set_index
      public :: awindow_set_process
      public :: awindow_set_parameter
      public :: awindow_set_filebox
      public :: awindow_set_cardset
      public :: awindow_set_previous
      public :: awindow_set_next

      character(len=100),public,save :: awindow_ident = &
       '$Id: awindow.f90,v 1.7 2006/09/18 13:32:38 Glover prod sps $'

      type,public :: awindow_struct
        private
        integer                                      :: window_id
        character(len=PC_LENGTH)                     :: window_type
        character(len=PC_LENGTH)                     :: keyword
        character(len=PC_LENGTH)                     :: value
        integer                                      :: index
        integer                                      :: nkeys
        character(len=PC_LENGTH),pointer             :: keys(:)
        type(process_struct),pointer                 :: process
        type(buildlist_struct),pointer               :: parameter
        type(filebox_struct),pointer                 :: filebox
        type(cardset_struct),pointer                 :: cardset
        type(awindow_struct),pointer                  :: previous
        type(awindow_struct),pointer                  :: next
      end type awindow_struct

      contains


!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!
!!------------------------------- create ----------------------------------!!


      subroutine awindow_create (obj)

      type(awindow_struct),pointer                  :: obj           !argument


      allocate(obj)

      obj%window_id      = 0
      obj%window_type    = ' '
      obj%keyword        = ' '
      obj%value          = ' '
      obj%index          = 0
      obj%nkeys          = 0
      nullify (obj%keys)
      nullify (obj%process)
      nullify (obj%parameter)
      nullify (obj%filebox)
      nullify (obj%cardset) ! jpa
      call cardset_create (obj%cardset)
      nullify (obj%previous)
      nullify (obj%next)

      return
      end subroutine awindow_create


!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!
!!------------------------------- delete ----------------------------------!!


      subroutine awindow_delete (obj)

      type(awindow_struct),pointer                  :: obj           !argument



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
      end subroutine awindow_delete


!!----------------------------- duplicate ---------------------------------!!
!!----------------------------- duplicate ---------------------------------!!
!!----------------------------- duplicate ---------------------------------!!


      subroutine awindow_duplicate (obj,new)

      type(awindow_struct),pointer                  :: obj           !argument
      type(awindow_struct),pointer                  :: new           !argument

      allocate(new)

      new%window_id      =  obj%window_id
      new%window_type    =  obj%window_type
      new%keyword        =  obj%keyword
      new%value          =  obj%value
      new%index          =  obj%index
      new%process        => obj%process
      new%parameter      => obj%parameter
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
      end subroutine awindow_duplicate


!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!
!!---------------------------- get_window_id ------------------------------!!


      subroutine awindow_get_window_id (obj,window_id)

      type(awindow_struct),pointer                  :: obj           !argument
      integer,intent(out)                          :: window_id     !argument

      if (associated(obj)) then
        window_id = obj%window_id
      else
        window_id = 0
      endif

      return
      end subroutine awindow_get_window_id


!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!
!!--------------------------- get_window_type -----------------------------!!


      subroutine awindow_get_window_type (obj,window_type)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=*),intent(out)                 :: window_type   !argument

      if (associated(obj)) then
        window_type = obj%window_type
      else
        window_type = ' '
      endif

      return
      end subroutine awindow_get_window_type


!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!
!!----------------------------- get_keyword -------------------------------!!


      subroutine awindow_get_keyword (obj,keyword)

      type(awindow_struct),pointer                 :: obj            !argument
      character(len=*),intent(out)                :: keyword        !argument

      if (associated(obj)) then
        keyword = obj%keyword
      else
        keyword = ' '
      endif

      return
      end subroutine awindow_get_keyword


!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!
!!----------------------------- get_value ---------------------------------!!


      subroutine awindow_get_value (obj,value)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=*),intent(out)                 :: value         !argument

      if (associated(obj)) then
        value = obj%value
      else
        value = ' '
      endif

      return
      end subroutine awindow_get_value


!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!
!!----------------------------- get_index ---------------------------------!!


      subroutine awindow_get_index (obj,index)

      type(awindow_struct),pointer                  :: obj           !argument
      integer,intent(out)                          :: index         !argument

      if (associated(obj)) then
        index = obj%index
      else
        index = 0
      endif

      return
      end subroutine awindow_get_index


!!----------------------------- get_process -------------------------------!!
!!----------------------------- get_process -------------------------------!!
!!----------------------------- get_process -------------------------------!!


      subroutine awindow_get_process (obj, process)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(process_struct)   ,pointer              :: process       !argument

      if (associated(obj)) then
        if (associated(obj%process)) then
          process => obj%process
        else
          nullify(process)
        endif
      else
        nullify(process)
      endif

      return
      end subroutine awindow_get_process


!!---------------------------- get_parameter ------------------------------!!
!!---------------------------- get_parameter ------------------------------!!
!!---------------------------- get_parameter ------------------------------!!


      subroutine awindow_get_parameter (obj, parameter)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(buildlist_struct),pointer               :: parameter     !argument

      if (associated(obj)) then
        if (associated(obj%parameter)) then
          parameter => obj%parameter
        else
          nullify(parameter)
        endif
      else
        nullify(parameter)
      endif

      return
      end subroutine awindow_get_parameter


!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!
!!----------------------------- get_filebox -------------------------------!!


      subroutine awindow_get_filebox (obj, filebox)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(filebox_struct),pointer                 :: filebox       !argument

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
      end subroutine awindow_get_filebox


!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!
!!----------------------------- get_cardset -------------------------------!!


      subroutine awindow_get_cardset (obj, cardset)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(cardset_struct)   ,pointer              :: cardset       !argument

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
      end subroutine awindow_get_cardset


!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!
!!----------------------------- get_previous ------------------------------!!


      subroutine awindow_get_previous (obj, previous)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(awindow_struct),pointer                  :: previous      !argument

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
      end subroutine awindow_get_previous


!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!
!!------------------------------- get_next --------------------------------!!


      subroutine awindow_get_next (obj, next)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(awindow_struct),pointer                  :: next          !argument

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
      end subroutine awindow_get_next


!!---------------------------- alloc_keys ---------------------------------!!
!!---------------------------- alloc_keys ---------------------------------!!
!!---------------------------- alloc_keys ---------------------------------!!


      subroutine awindow_alloc_keys (obj,keys,nkeys)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=PC_LENGTH),pointer             :: keys(:)       !argument
      integer,intent(out)                          :: nkeys         !argument

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
      end subroutine awindow_alloc_keys


!!----------------------------- set_keys ----------------------------------!!
!!----------------------------- set_keys ----------------------------------!!
!!----------------------------- set_keys ----------------------------------!!


      subroutine awindow_set_keys (obj,keys,nkeys)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=PC_LENGTH),pointer             :: keys(:)       !argument
      integer,intent(in)                           :: nkeys         !argument

      if (.not. associated(obj)) return

      if (associated(obj%keys)) deallocate(obj%keys)
      obj%nkeys = nkeys
      if (obj%nkeys .gt. 0) then
        allocate(obj%keys(obj%nkeys))
        obj%keys(1:obj%nkeys) = keys(1:nkeys)
      endif

      return
      end subroutine awindow_set_keys


!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!
!!---------------------------- set_window_id ------------------------------!!


      subroutine awindow_set_window_id (obj,window_id)

      type(awindow_struct),pointer                  :: obj           !argument
      integer,intent(in)                           :: window_id     !argument

      if (.not. associated(obj)) return

      obj%window_id = window_id

      return
      end subroutine awindow_set_window_id


!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!
!!--------------------------- set_window_type -----------------------------!!


      subroutine awindow_set_window_type (obj,window_type)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=*),intent(in)                  :: window_type   !argument

      if (.not. associated(obj)) return

      obj%window_type = trim(window_type)
      call string_to_upper(obj%window_type)

      return
      end subroutine awindow_set_window_type


!!----------------------------- set_keyword -------------------------------!!
!!----------------------------- set_keyword -------------------------------!!
!!----------------------------- set_keyword -------------------------------!!


      subroutine awindow_set_keyword (obj,keyword)

      type(awindow_struct),pointer                 :: obj            !argument
      character(len=*),intent(in)                 :: keyword        !argument

      if (.not. associated(obj)) return

      obj%keyword = keyword

      return
      end subroutine awindow_set_keyword


!!----------------------------- set_value ---------------------------------!!
!!----------------------------- set_value ---------------------------------!!
!!----------------------------- set_value ---------------------------------!!


      subroutine awindow_set_value (obj,value)

      type(awindow_struct),pointer                  :: obj           !argument
      character(len=*),intent(in)                  :: value         !argument

      if (.not. associated(obj)) return

      obj%value = value

      return
      end subroutine awindow_set_value


!!----------------------------- set_index ---------------------------------!!
!!----------------------------- set_index ---------------------------------!!
!!----------------------------- set_index ---------------------------------!!


      subroutine awindow_set_index (obj,index)

      type(awindow_struct),pointer                  :: obj           !argument
      integer,intent(in)                           :: index         !argument

      if (.not. associated(obj)) return

      obj%index = index

      return
      end subroutine awindow_set_index


!!----------------------------- set_process -------------------------------!!
!!----------------------------- set_process -------------------------------!!
!!----------------------------- set_process -------------------------------!!


      subroutine awindow_set_process (obj, process)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(process_struct)   ,pointer              :: process       !argument

      if (.not. associated(obj)) return

      if (associated(process)) then
        obj%process => process
      else
        nullify(obj%process)
      endif

      return
      end subroutine awindow_set_process


!!---------------------------- set_parameter ------------------------------!!
!!---------------------------- set_parameter ------------------------------!!
!!---------------------------- set_parameter ------------------------------!!


      subroutine awindow_set_parameter (obj, parameter)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(buildlist_struct),pointer               :: parameter     !argument

      if (.not. associated(obj)) return

      if (associated(parameter)) then
        obj%parameter => parameter
      else
        nullify(obj%parameter)
      endif

      return
      end subroutine awindow_set_parameter


!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!
!!----------------------------- set_filebox -------------------------------!!


      subroutine awindow_set_filebox (obj, filebox)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(filebox_struct),pointer                 :: filebox       !argument

      if (.not. associated(obj)) return

      if (associated(filebox)) then
        obj%filebox => filebox
      else
        nullify(obj%filebox)
      endif

      return
      end subroutine awindow_set_filebox


!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!
!!----------------------------- set_cardset -------------------------------!!


      subroutine awindow_set_cardset (obj, cardset)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(cardset_struct)   ,pointer              :: cardset       !argument

      if (.not. associated(obj)) return

      if (associated(cardset)) then
        obj%cardset => cardset
      else
        nullify(obj%cardset)
      endif

      return
      end subroutine awindow_set_cardset


!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!
!!----------------------------- set_previous ------------------------------!!


      subroutine awindow_set_previous (obj, previous)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(awindow_struct),pointer                  :: previous      !argument

      if (.not. associated(obj)) return

      if (associated(previous)) then
        obj%previous => previous
      else
        nullify(obj%previous)
      endif

      return
      end subroutine awindow_set_previous


!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!
!!------------------------------- set_next --------------------------------!!


      subroutine awindow_set_next (obj, next)
      implicit none
      type(awindow_struct),pointer                  :: obj           !argument
      type(awindow_struct),pointer                  :: next          !argument

      if (.not. associated(obj)) return

      if (associated(next)) then
        obj%next => next
      else
        nullify(obj%next)
      endif

      return
      end subroutine awindow_set_next


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module awindow_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

