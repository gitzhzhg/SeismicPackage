
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- hashlist.f90 ------------------------------!!
!!---------------------------- hashlist.f90 ------------------------------!!
!!---------------------------- hashlist.f90 ------------------------------!!


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
! Name       : HASHLIST
! Category   : character
! Written    : 2000-08-11   by: Tom Stoeckley
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : Hash table for quick finding of keywords in an array.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module contains a hash table for use in quickly finding the array
! index associated with a given keyword.  Whenever the calling program
! adds a keyword to its own array, it notifies this primitive of the value
! of this keyword and the associated array index.  Then when the calling
! program wants to find the array index associated with a specific keyword,
! it simply asks this primitive for that index.
!
! This primitive uses a hash table to find the associated index without
! searching through an array.  A table address is calculated for each
! keyword.  For each table address, this primitive keeps a linked list
! of keywords and associated indices.  If no keywords hash to a given table
! address, the linked list for that address will be empty.  If one keyword
! hashes to a given address, the linked list will contain just one item.
! If two or more keywords hash to the same address, the linked list will be
! searched to return the index for the correct keyword.  In this way,
! this module will never fail to return the correct index for any keyword,
! whether or not its hash table address clashes with another keyword.
!
! If any keyword is not unique, this module will return the index for the
! first of all matching keywords provided to this module.
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
!                           CALLING SEQUENCE
!
!                                           o
!              call hashlist_create       (obj)
!
!                                           b
!              call hashlist_delete       (obj)
!
!                                           b
!              call hashlist_clear        (obj)
!
!                                           b      i      i
!              call hashlist_add_keyword  (obj, keyword, ikey)
!
!                                           i      i      o
!              call hashlist_find_keyword (obj, keyword, ikey)
!
!                                           i     b
!              call hashlist_copy         (obj1, obj2)
!
!                                           i   i
!              call hashlist_print        (obj,lun)
!
!
! type(hashlist_struct)      obj = pointer to the hashlist structure.
! character(len=*)       keyword = keyword identifying an array element.
! integer                   ikey = index of the array element of this keyword.
! integer                    lun = logical unit number to print to (>=1).
!
! HASHLIST_CLEAR:
!  (1) Must be called to initialize the hash table to be empty.
!  (2) Must be called whenever the name or index of any keyword is changed,
!       such as when a keyword (and associated array element) is removed,
!       or inserted within the array (except for appending to the end of the
!       array), or when a keyword for an array element is renamed, or the
!       array is cleared or sorted.  Then HASHLIST_ADD_KEYWORD must be called
!       for every keyword after any changes to the array have been made.
!
! HASHLIST_ADD_KEYWORD:
!  (1) Must be called for each keyword in the calling program array.
!  (2) Must be called when a keyword (and associated array element) is appended.
!  (3) Must be called (after calling HASHLIST_CLEAR) for every array element
!       when any changes are made to the array other than simply appending
!       a new array element to the end of the array.
!
! HASHLIST_FIND_KEYWORD:
!  (1) Can be called at any time to get the index of the array element for
!       the specified keyword.
!  (2) If the keyword is not in the array, IKEY will be set to zero.
!  (3) If the same keyword is in the array more than once, IKEY will be set
!       to the first index provided for this keyword.
!
! HASHLIST_COPY:
!  (1) Both objects in the argument list are assumed to be already created.
!
! HASHLIST_PRINT:
!  (1) To print the contents of this module (for debug purposes).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2008-12-11  Bill Menger Nullified pointers.
!  2. 2000-10-20  Stoeckley  Add required missing documentation section.
!  1. 2000-08-21  Stoeckley  Initial version.
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



!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!


module hashlist_module

  implicit none

  public
  private :: hashlist_private_get_address

      character(len=100),public,save :: HASHLIST_IDENT = &
       '$Id: hashlist.f90,v 1.2 2000/10/20 16:17:55 sps prod sps $'


  integer,parameter,private :: HASHLIST_LENGTH   =  80
  integer,parameter,private :: HASHLIST_HASHSIZE = 401
                                               ! number of table addresses.

  type,private :: hashlist_item
    private
    character(len=HASHLIST_LENGTH) :: keyword  ! keyword.
    integer                        :: ikey     ! index.
    type(hashlist_item),pointer    :: next     ! pointer to next keyword with
                                               !  this table address (null if
                                               !  no additional keywords).
  end type hashlist_item


  type,private :: hashlist_wrappers
    private
    type(hashlist_item),pointer :: first       ! pointer to first keyword with
                                               !  this table address (null if
                                               !  no such keywords).
  end type hashlist_wrappers


  type,public :: hashlist_struct
    private
    type(hashlist_wrappers) :: list(HASHLIST_HASHSIZE)
  end type hashlist_struct


contains


!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!


      subroutine hashlist_create (obj)
      implicit none
      type(hashlist_struct),pointer :: obj             ! argument
      integer                       :: address         ! local

      nullify(obj)
      allocate (obj)
      do address = 1, HASHLIST_HASHSIZE
           nullify (obj%list(address)%first)
      end do
      return
      end subroutine hashlist_create


!!------------------------------ delete ---------------------------------!!
!!------------------------------ delete ---------------------------------!!
!!------------------------------ delete ---------------------------------!!


      subroutine hashlist_delete (obj)
      implicit none
      type(hashlist_struct),pointer :: obj             ! argument

      call hashlist_clear (obj)
      deallocate (obj)
      return
      end subroutine hashlist_delete


!!------------------------------- clear ---------------------------------!!
!!------------------------------- clear ---------------------------------!!
!!------------------------------- clear ---------------------------------!!


      subroutine hashlist_clear (obj)
      implicit none
      type(hashlist_struct),intent(inout) :: obj             ! argument
      integer                             :: address         ! local
      type(hashlist_item),pointer         :: link,link2      ! local

      nullify(link)
      nullify(link2)
      do address = 1, HASHLIST_HASHSIZE
           link => obj%list(address)%first
           if (.not.associated(link)) cycle
           link2 => link%next
           deallocate (link)
           obj%list(address)%first => link
           do 
                if (.not.associated(link2)) exit
                link => link2%next
                deallocate (link2)
                link2 => link
           end do
      end do
      return
      end subroutine hashlist_clear


!!------------------------------- print ---------------------------------!!
!!------------------------------- print ---------------------------------!!
!!------------------------------- print ---------------------------------!!


      subroutine hashlist_print (obj,lun)
      implicit none
      type(hashlist_struct),intent(inout) :: obj             ! argument
      integer              ,intent(in)    :: lun             ! argument
      integer                             :: address         ! local
      type(hashlist_item),pointer         :: link            ! local

      write(lun,*) ' '
      write(lun,*) 'hashlist contains ',HASHLIST_HASHSIZE,' table addresses.'
      write(lun,*) 'live table addresses are listed here:'
      write(lun,*) ' '
      nullify(link)
      do address = 1, HASHLIST_HASHSIZE
           link => obj%list(address)%first
           do 
                if (.not.associated(link)) exit
                write(lun,*) 'table address = ',address,'  keyword = ',  &
                         trim(link%keyword),'  index = ',link%ikey
                link => link%next
           end do
      end do
      write(lun,*) ' '
      write(lun,*) 'end of hashlist printout.'
      write(lun,*) ' '
      return
      end subroutine hashlist_print


!!---------------------------- add keyword -------------------------------!!
!!---------------------------- add keyword -------------------------------!!
!!---------------------------- add keyword -------------------------------!!


      subroutine hashlist_add_keyword (obj,keyword,ikey)
      implicit none
      type(hashlist_struct),intent(inout) :: obj             ! argument
      character(len=*)     ,intent(in)    :: keyword         ! argument
      integer              ,intent(in)    :: ikey            ! argument
      integer                             :: address         ! local
      type(hashlist_item),pointer         :: link,link2      ! local

      address = hashlist_private_get_address (keyword)
      nullify(link)
      nullify(link2)
      link => obj%list(address)%first
      if (.not.associated(link)) then
           allocate (link2)
           obj%list(address)%first => link2
      else
           do 
                link2 => link%next
                if (.not.associated(link2)) exit
                link => link2
           end do
           allocate (link2)
           link%next => link2
      end if
      link2%keyword = keyword
      link2%ikey    = ikey
      nullify (link2%next)
      return
      end subroutine hashlist_add_keyword


!!---------------------------- find keyword -------------------------------!!
!!---------------------------- find keyword -------------------------------!!
!!---------------------------- find keyword -------------------------------!!


      subroutine hashlist_find_keyword (obj,keyword,ikey)
      implicit none
      type(hashlist_struct),intent(in)    :: obj             ! argument
      character(len=*)     ,intent(in)    :: keyword         ! argument
      integer              ,intent(out)   :: ikey            ! argument
      integer                             :: address         ! local
      type(hashlist_item),pointer         :: link            ! local

      address = hashlist_private_get_address (keyword)
      nullify(link)
      link => obj%list(address)%first
      do 
           if (.not.associated(link)) then
                ikey = 0
                return
           else if (link%keyword == keyword) then
                ikey = link%ikey
                return
           end if
           link => link%next
      end do
      end subroutine hashlist_find_keyword


!!------------------------------ copy --------------------------------------!!
!!------------------------------ copy --------------------------------------!!
!!------------------------------ copy --------------------------------------!!


      subroutine hashlist_copy (obj1,obj2)
      implicit none
      type(hashlist_struct),intent(in)    :: obj1            ! argument
      type(hashlist_struct),intent(inout) :: obj2            ! argument
      integer                             :: address         ! local
      type(hashlist_item),pointer         :: link            ! local

      call hashlist_clear (obj2)
      nullify(link)
      do address = 1, HASHLIST_HASHSIZE
           link => obj1%list(address)%first
           do 
                if (.not.associated(link)) exit
                call hashlist_add_keyword (obj2,link%keyword,link%ikey)
                link => link%next
           end do
      end do
      return
      end subroutine hashlist_copy


!!--------------------- private get address --------------------------------!!
!!--------------------- private get address --------------------------------!!
!!--------------------- private get address --------------------------------!!


      function hashlist_private_get_address (keyword) result (address)
      implicit none
      character(len=*)     ,intent(in)    :: keyword         ! argument
      integer                             :: address         ! result
      integer                             :: length,i,sum    ! local

      length = len_trim(keyword)
      sum = 0
      do i = 1,length
        sum = sum + ichar(keyword(i:i))
      end do
      address = mod(sum,HASHLIST_HASHSIZE) + 1
      return
      end function hashlist_private_get_address


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module hashlist_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

