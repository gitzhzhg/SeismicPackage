
!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- cardsetlist.f90 ------------------------------!!
!!--------------------------- cardsetlist.f90 ------------------------------!!
!!--------------------------- cardsetlist.f90 ------------------------------!!


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
!                      C P S   P R I M I T I V E
!
! Name       : CARDSETLIST
! Category   : character
! Written    : 1999-06-22   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Module for storing a list of several cardset objects.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module is a repository for a list of cardset objects.
! Each cardset object is identified by a name.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented here, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!              TO CREATE AND CLEAR AND DELETE THE CARDSETLIST
!
!                    call cardsetlist_create (obj)
!                    call cardsetlist_clear  (obj)
!                    call cardsetlist_delete (obj)
!                    call cardsetlist_copy   (obj1,obj2)
!                    call cardsetlist_append (obj1,obj2)
!                                              i    b
!
! type(cardsetlist_struct)      obj = the cardsetlist structure.
!
! CARDSETLIST_CREATE:
!   (1) allocates obj (passed as a pointer).
!   (2) initializes the contants to contain no cardsets.
!
! CARDSETLIST_CLEAR:
!   (1) deletes all cardsets.
!
! CARDSETLIST_DELETE:
!   (1) deletes all cardsets.
!   (2) deallocates obj (passed as a pointer).
!
! CARDSETLIST_COPY:
!   (1) copies the contents of OBJ1 to OBJ2.
!   (2) the previous contents of OBJ2 are deleted first.
!
! CARDSETLIST_APPEND:
!   (1) appends the contents of OBJ1 to OBJ2.
!   (2) replaces existing cardsets with the same name and adds new cardsets.
!   (3) the previous contents of OBJ2 are deleted first.
!
!-------------------------------------------------------------------------------
!                  TO ACCESS INDIVIDUAL CARDSET OBJECTS
!
!          o                                               i
!        nsets   =  cardsetlist_num_cardsets        (obj)
!        cardset => cardsetlist_get_cardset         (obj, index)
!        cardset => cardsetlist_find_cardset        (obj, name)
!        cardset => cardsetlist_create_new_cardset  (obj, name)
!        cardset => cardsetlist_find_or_add_cardset (obj, name)
!
!              call cardsetlist_get_cardset_names   (obj, names, nsets)
!                                                           o      o
!
! type(cardsetlist_struct)             obj = the cardsetlist structure.
! type(cardset_struct),pointer     cardset = the requested cardset structure.
! integer                            nsets = number of cardsets.
! integer                            index = index of desired cardset.
! character(len=*)                    name = name of the desired cardset.
! character(len=*)                names(:) = names of all of the cardsets.
!
! The name is converted to upper case when input.
! The name matching is done in upper case.
!
! CARDSETLIST_NUM_CARDSETS:
!   (1) returns the number of cardsets (0 or more) in the cardsetlist object.
!
! CARDSETLIST_GET_CARDSET:
!   (1) gets the requested cardset in the cardsetlist object.
!   (2) returns a nullified pointer if INDEX is out of range.
!
! CARDSETLIST_FIND_CARDSET:
!   (1) gets the requested cardset in the cardsetlist object.
!   (2) returns a nullified pointer if NAME is not found.
!   (3) returns the first matching cardset if two or more have the same name.
!
! CARDSETLIST_CREATE_NEW_CARDSET:
!   (1) creates a new cardset with specified name in the cardsetlist object.
!   (2) never returns a nullified pointer.
!
! CARDSETLIST_FIND_OR_ADD_CARDSET:
!   (1) calls CARDSETLIST_FIND_CARDSET.
!   (2) calls CARDSETLIST_CREATE_NEW_CARDSET if matching cardset is not found.
!   (3) never returns a nullified pointer.
!
! Cardsets can be added to the cardset list one at a time by calling
! CARDSETLIST_CREATE_NEW_CARDSET or CARDSETLIST_FIND_OR_ADD_CARDSET.
! The name of the new cardset must be specified at this time and should
! not subsequently be changed.
!
! Cardsets cannot be deleted individually from the cardsetlist, although
! all of them can be deleted simultaneously by calling CARDSETLIST_CLEAR.
! Individual cardsets must not be deleted when their pointers are obtained.
! Individual cardsets can however be cleared of their contents.
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!011. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!010. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2004-05-03  R Selzler  Correct one compiler error, Absoft 8.0 with
!                              latest quick fix: Dummy arguments with the
!                              INTENT(OUT) attribute must be defined before use.
!  8. 2004-01-21  Stoeckley  Fix bug in CARDSETLIST_CREATE_NEW_CARDSET
!                             associated with copying a zero-length array.
!  7. 2002-02-04  Stoeckley  Add CARDSETLIST_COPY, CARDSETLIST_APPEND, and
!                             CARDSETLIST_GET_CARDSET_NAMES, all to help
!                             support workstation program I/O.
!  6. 2001-06-11  Stoeckley  Add subroutine CARDSETLIST_FIND_OR_ADD_CARDSET.
!  5. 2001-04-10  Stoeckley  Change name of private fix_struct to get around
!                             a new sun compiler bug.
!  4. 2000-10-20  Stoeckley  Add required missing documentation section.
!  3. 1999-11-17  Stoeckley  Add ident string for RCS.
!  2. 1999-09-10  Stoeckley  Minor documentation changes.
!  1. 1999-06-22  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>



!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
! A bug was discovered in the new sun compiler (on poepsn25 in April 2001):
! The structure FIX_STRUCT in this module cannot have the same name as
! structures in other modules used by this module.  Since the name of this
! structure was not important, it was changed in order to get around this
! bug.  This turns out to be a known bug but there is no indication about
! whether it will be fixed.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!


module cardsetlist_module

  use cardset_module
  implicit none

  public

      character(len=100),public,save :: CARDSETLIST_IDENT = &
       '$Id: cardsetlist.f90,v 1.11 2006/09/18 13:32:39 Glover prod sps $'


!!---------------------- fix for array of pointers ------------------------!!
!!---------------------- fix for array of pointers ------------------------!!
!!---------------------- fix for array of pointers ------------------------!!


  type,private :: cardsetlist_fix_struct

    type(cardset_struct),pointer :: cardset

  end type cardsetlist_fix_struct


!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!


  type,public :: cardsetlist_struct

    private
    type(cardsetlist_fix_struct),pointer :: cardsets(:)   ! list of cardsets.
    integer                              :: nsets         ! number of cardsets.

  end type cardsetlist_struct


!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!


contains


!!-------------------- create delete clear copy append ---------------------!!
!!-------------------- create delete clear copy append ---------------------!!
!!-------------------- create delete clear copy append ---------------------!!


      subroutine cardsetlist_create (obj)
      implicit none
      type(cardsetlist_struct),pointer :: obj         ! argument

      allocate(obj)
      nullify(obj%cardsets)
      obj%nsets = 0
      return
      end subroutine cardsetlist_create



      subroutine cardsetlist_delete (obj)
      implicit none
      type(cardsetlist_struct),pointer :: obj         ! argument

      call cardsetlist_clear (obj)
      deallocate(obj)
      return
      end subroutine cardsetlist_delete



      subroutine cardsetlist_clear (obj)
      implicit none
      type(cardsetlist_struct),intent(inout) :: obj        ! argument
      integer                                :: index      ! local
      type(cardset_struct)    ,pointer       :: cardset    ! local

      do index = 1,obj%nsets
          cardset => obj%cardsets(index)%cardset
          call cardset_delete (cardset)
      end do
      if(associated(obj%cardsets)) deallocate(obj%cardsets)
      obj%nsets = 0
      return
      end subroutine cardsetlist_clear



      subroutine cardsetlist_copy (obj1,obj2)
      implicit none
      type(cardsetlist_struct),intent(in)    :: obj1        ! argument
      type(cardsetlist_struct),intent(inout) :: obj2        ! argument
      integer                                :: index       ! local

      call cardsetlist_clear (obj2)
      obj2%nsets = obj1%nsets
      allocate(obj2%cardsets(obj2%nsets))
      do index = 1,obj2%nsets
          call cardset_create (obj2%cardsets(index)%cardset)
          call cardset_copy   (obj1%cardsets(index)%cardset, &
                               obj2%cardsets(index)%cardset)
      end do
      return
      end subroutine cardsetlist_copy



      subroutine cardsetlist_append (obj1,obj2)
      implicit none
      type(cardsetlist_struct),intent(in)    :: obj1        ! argument
      type(cardsetlist_struct),intent(inout) :: obj2        ! argument
      integer                                :: index       ! local
      character(len=80)                      :: name        ! local
      type(cardset_struct),pointer           :: cardset     ! local

      do index = 1,obj1%nsets
          call cardset_get_name (obj1%cardsets(index)%cardset, name)
          cardset => cardsetlist_find_or_add_cardset (obj2, name)
          call cardset_copy     (obj1%cardsets(index)%cardset, cardset)
      end do
      return
      end subroutine cardsetlist_append


!!-------------------------- num cardsets ----------------------------------!!
!!-------------------------- num cardsets ----------------------------------!!
!!-------------------------- num cardsets ----------------------------------!!


      function cardsetlist_num_cardsets (obj) result (nsets)
      implicit none
      type(cardsetlist_struct),intent(in) :: obj         ! argument
      integer                             :: nsets       ! result

      nsets = obj%nsets
      return
      end function cardsetlist_num_cardsets


!!-------------------------- get cardset ---------------------------------!!
!!-------------------------- get cardset ---------------------------------!!
!!-------------------------- get cardset ---------------------------------!!



      function cardsetlist_get_cardset (obj, index) result (cardset)
      implicit none
      type(cardsetlist_struct),intent(in)   :: obj         ! argument
      integer                 ,intent(in)   :: index       ! argument
      type(cardset_struct)    ,pointer      :: cardset     ! result

      if (index < 1 .or. index > obj%nsets) then
           nullify (cardset)
      else
           cardset => obj%cardsets(index)%cardset
      end if
      return
      end function cardsetlist_get_cardset


!!-------------------------- find cardset ---------------------------------!!
!!-------------------------- find cardset ---------------------------------!!
!!-------------------------- find cardset ---------------------------------!!


      function cardsetlist_find_cardset (obj, name) result (cardset)
      implicit none
      type(cardsetlist_struct),intent(in)   :: obj       ! argument
      character(len=*)        ,intent(in)   :: name      ! argument
      type(cardset_struct)    ,pointer      :: cardset   ! result
      integer                               :: index     ! local

      character(len=CARDSET_LENGTH)         :: name2     ! local
      character(len=CARDSET_LENGTH)         :: name3     ! local

      name2 = name
      call string_to_upper (name2)
      do index = 1,obj%nsets
          cardset => obj%cardsets(index)%cardset
          call cardset_get_name (cardset, name3)
          if (name3 == name2) return
      end do
      nullify(cardset)
      return
      end function cardsetlist_find_cardset


!!--------------------- create new cardset --------------------------------!!
!!--------------------- create new cardset --------------------------------!!
!!--------------------- create new cardset --------------------------------!!


      function cardsetlist_create_new_cardset (obj, name) result (cardset)
      implicit none
      type(cardsetlist_struct),intent(inout) :: obj                ! argument
      character(len=*)        ,intent(in)    :: name               ! argument
      type(cardset_struct)    ,pointer       :: cardset            ! result
      type(cardsetlist_fix_struct)           :: temp(obj%nsets)    ! local

      nullify (cardset) ! jpa
      call cardset_create   (cardset)
      call cardset_set_name (cardset, name)
      if (obj%nsets > 0) temp = obj%cardsets(1:obj%nsets)
      if(associated(obj%cardsets)) deallocate(obj%cardsets)
      allocate(obj%cardsets(obj%nsets + 1))
      if (obj%nsets > 0) obj%cardsets(1:obj%nsets) = temp
      obj%nsets = obj%nsets + 1
      obj%cardsets(obj%nsets)%cardset => cardset
      return
      end function cardsetlist_create_new_cardset


!!-------------------------- find or add cardset ----------------------------!!
!!-------------------------- find or add cardset ----------------------------!!
!!-------------------------- find or add cardset ----------------------------!!


      function cardsetlist_find_or_add_cardset (obj, name) result (cardset)
      implicit none
      type(cardsetlist_struct),intent(inout) :: obj         ! argument
      character(len=*)        ,intent(in)    :: name        ! argument
      type(cardset_struct)    ,pointer       :: cardset     ! result

      cardset => cardsetlist_find_cardset(obj,name)
      if(.not.associated(cardset)) &
                     cardset => cardsetlist_create_new_cardset(obj,name)
      return
      end function cardsetlist_find_or_add_cardset


!!-------------------------- get cardset names ------------------------------!!
!!-------------------------- get cardset names ------------------------------!!
!!-------------------------- get cardset names ------------------------------!!


      subroutine cardsetlist_get_cardset_names (obj, names, nsets)
      implicit none
      type(cardsetlist_struct),intent(in)    :: obj         ! argument
      character(len=*)        ,intent(out)   :: names(:)    ! argument
      integer                 ,intent(out)   :: nsets       ! argument
      type(cardset_struct)    ,pointer       :: cardset     ! local
      integer                                :: index       ! local

      do index = 1,obj%nsets
          cardset => obj%cardsets(index)%cardset
          names(index) = ' '
          call cardset_get_name (cardset, names(index))
      end do
      nsets = obj%nsets
      return
      end subroutine cardsetlist_get_cardset_names


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module cardsetlist_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

