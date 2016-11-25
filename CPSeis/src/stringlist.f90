
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- stringlist.f90 ------------------------------!!
!!---------------------------- stringlist.f90 ------------------------------!!
!!---------------------------- stringlist.f90 ------------------------------!!


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
! Name       : STRINGLIST
! Category   : character
! Written    : 1999-06-25   by: Tom Stoeckley
! Revised    : 2008-12-11   by: B. Menger
! Maturity   : production
! Purpose    : Module for storing and retrieving character strings in a list.
! Portability: No known limitations - but see the portability section below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module is a repository for a list of character strings.  This module
! is ignorant of the nature of the character strings it contains.  These
! character strings can be inserted into, or removed from, this module all
! at once or one at a time.  They can contain any miscellaneous information.
!
! The character strings in this module can be a list of data cards used by
! the parameter cache or by generic file I/O routines.  Alternatively, the
! character strings can be a representation of a parameter which might be
! a scalar value or an array of values.
!
! Each instance of this module can have a name.  This can be the name of
! the class of data cards in this module, the name of the delineated section
! on a file from which the data cards in this module have been obtained,
! or a keyword associated with the parameter represented in this module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS
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
!     TO CREATE AND CLEAR AND DELETE AND COPY AND PRINT THE STRINGLIST
!
!                   call stringlist_create (obj)
!                   call stringlist_clear  (obj)
!                   call stringlist_delete (obj)
!
!                   call stringlist_copy   (obj1, obj2)
!                                            i     b
!
!                   call stringlist_print  (obj, lunprint)
!                                            i     i
!
! type(stringlist_struct)    obj = the stringlist structure.
! integer               lunprint = logical unit number for printing (or zero).
!
! STRINGLIST_CREATE:
!   (1) allocates obj (PASSED AS A POINTER).
!   (2) sets the number of strings to zero.
!   (3) sets the name of the stringlist to blank.
!
! STRINGLIST_CLEAR:
!   (1) deletes all strings.
!   (2) sets the number of strings to zero.
!   (3) does not change the name of the stringlist.
!
! STRINGLIST_DELETE:
!   (1) deletes all strings.
!   (2) deallocates obj (PASSED AS A POINTER).
!
! STRINGLIST_COPY:
!   (1) copies the contents of OBJ1 to OBJ2.
!   (2) the previous contents of OBJ2 are deleted first.
!
! STRINGLIST_PRINT:
!   (1) prints the contents of the object.
!
!-------------------------------------------------------------------------------
!                  TO ACCESS THE NAME OF THE STRINGLIST
!
!                                                          o
!                    call stringlist_get_name       (obj, name)
!
!                                                          i 
!                    call stringlist_set_name       (obj, name)
!                    call stringlist_set_name_exact (obj, name)
!
!              o                                           i
!           matches = stringlist_name_matches       (obj, name)
!           matches = stringlist_name_matches_exact (obj, name)
!
! type(stringlist_struct)       obj = the stringlist structure.
! character(len=*)             name = name of the stringlist.
! logical                   matches = true if the name matches.
!
! The name can be blank.
! The name is converted to upper case when input.
! The name matching is done in a case-insensitive manner.
!
! The routines with the _EXACT suffix use NAME exactly as it is, and do
! not convert it to upper case.  These routines can be used if the name is
! already upper case when input (for efficiency reasons).
!
!-------------------------------------------------------------------------------
!                   TO ACCESS STRINGS IN THE STRINGLIST
!
!                 o                                 
!              nstrings = stringlist_num_strings (obj)
!
!                                            o         o        o
!     call stringlist_get_strings   (obj, strings,  nstrings, errmsg)
!     call stringlist_alloc_strings (obj, pstrings, nstrings)
!
!                                                  i      o       o
!     call stringlist_get_string            (obj, indx, string, errmsg)
!     call stringlist_get_last_string       (obj,       string, errmsg)
!
!                                          i        i     
!     call stringlist_put_strings (obj, strings, nstrings)
!     call stringlist_add_strings (obj, strings, nstrings)
!
!                                                          i
!     call stringlist_put_empty_strings     (obj,       nstrings)
!     call stringlist_add_empty_strings     (obj,       nstrings)
!
!                                                         i
!     call stringlist_put_string            (obj,       string)
!     call stringlist_add_string            (obj,       string)
!
!     call stringlist_put_empty_string      (obj)
!     call stringlist_add_empty_string      (obj)
!
!                                                      i      i   
!     call stringlist_insert_string             (obj, indx, string)
!     call stringlist_insert_string             (obj, indx        )
!     call stringlist_remove_string             (obj, indx        )
!     call stringlist_replace_string            (obj, indx, string)
!     call stringlist_replace_add_string        (obj, indx, string)
!     call stringlist_replace_last_string       (obj,       string)
!     call stringlist_clear_buffer              (obj)
!
!
! type(stringlist_struct)            obj = the stringlist structure.
! integer                       nstrings = number of strings.
! integer                           indx = index of desired string.
! character(len=*)                string = single string.
! character(len=*)            strings(:) = array of strings.
! character(len=*),pointer   pstrings(:) = pointer to array of strings.
! character(len=*)                errmsg = error message (blank if no error).
!
! Upon input, strings can have any length, but strings over STRINGLIST_LENGTH
! characters will be truncated.  STRINGLIST_LENGTH is a named constant set
! to the value 160.
!
! STRINGLIST_NUM_STRINGS:
!   (1) returns the number of strings (0 or more) in the stringlist object.
!
! STRINGLIST_GET_STRINGS:
!   (1) gets all of the strings in the stringlist object.
!   (2) an error occurs if STRINGS is dimensioned too small for all strings.
!   (3) does not reset STRINGS or NSTRINGS if an error occurs.
!
! STRINGLIST_ALLOC_STRINGS:
!   (1) gets all of the strings in the stringlist object.
!   (2) PSTRINGS is deallocated and reallocated to contain all strings.
!          (PSTRINGS must be nullified or allocated before first use.)
!          (PSTRINGS should be conditionally deallocated after last use.)
!   (3) PSTRINGS is always reallocated to at least one array element, even if
!        there are no array elements and NELEMENTS is set or reset to zero.
!
! STRINGLIST_GET_STRING:
!   (1) gets the requested string in the stringlist object.
!   (2) an error occurs if INDEX is out of range.
!   (3) does not reset STRING if an error occurs.
!
! STRINGLIST_GET_LAST_STRING:
!   (1) gets the last string in the stringlist object.
!   (2) an error occurs if there are no strings.
!   (3) does not reset STRING if an error occurs.
!   (4) this is a special case alternative for STRINGLIST_GET_STRING.
!
! STRINGLIST_PUT_STRINGS:
!   (1) replaces the previous contents with an array with 0 or more strings.
!
! STRINGLIST_ADD_STRINGS:
!   (1) appends the strings to the previous contents.
!
! STRINGLIST_PUT_EMPTY_STRINGS:
!   (1) replaces the previous contents with an array of blank strings.
!   (2) this is a special case alternative for STRINGLIST_PUT_STRINGS.
!
! STRINGLIST_ADD_EMPTY_STRINGS:
!   (1) appends empty strings to the previous contents.
!   (2) this is a special case alternative for STRINGLIST_ADD_STRINGS.
!
! STRINGLIST_PUT_STRING:
!   (1) replaces the previous contents with a single string.
!
! STRINGLIST_ADD_STRING:
!   (1) appends one string to the previous contents.
!
! STRINGLIST_PUT_EMPTY_STRING:
!   (1) replaces the previous contents with a single empty string.
!   (2) this is a special case alternative for STRINGLIST_PUT_STRING.
!
! STRINGLIST_ADD_EMPTY_STRING:
!   (1) appends one empty string to the previous contents.
!   (2) this is a special case alternative for STRINGLIST_ADD_STRING.
!
! STRINGLIST_INSERT_STRING:
!   (1) inserts the specified string at the specified INDEX.
!   (2) inserts string from buffer if argument STRING is missing.
!   (3) does nothing if INDEX is out of range.
!
! STRINGLIST_REMOVE_STRING:
!   (1) removes the string at the specified INDEX.
!   (2) puts removed string into a buffer for possible insertion later.
!   (3) does nothing if INDEX is out of range.
!
! STRINGLIST_REPLACE_STRING:
!   (1) replaces a previous string with the specified string.
!   (2) does nothing if INDEX is out of range.
!
! STRINGLIST_REPLACE_ADD_STRING:
!   (1) replaces a previous string with the specified string.
!   (2) if INDEX is too large, increases the number of strings, setting
!        any intermediate strings to blank.
!   (3) does nothing if INDEX is too small.
!
! STRINGLIST_REPLACE_LAST_STRING:
!   (1) replaces the last string with the specified string.
!   (2) does nothing if there are no strings.
!   (3) this is a special case alternative for STRINGLIST_REPLACE_STRING.
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 14. 2008-12-11  B. Menger  Nullified pointer
!013. 2006-06-20  B. Menger   Removed Unused Variables.
! 12. 2003-01-23  Stoeckley  Increase STRINGLIST_LENGTH to 200 characters.
! 11. 2002-02-04  Stoeckley  Add STRINGLIST_ADD_STRINGS, STRINGLIST_PRINT,
!                             STRINGLIST_PUT_EMPTY_STRING, 
!                             STRINGLIST_ADD_EMPTY_STRINGS, 
!                             STRINGLIST_INSERT_STRING(from buffer),
!                             STRINGLIST_CLEAR_BUFFER, and
!                             STRINGLIST_REPLACE_ADD_STRING, all to help
!                             support workstation program I/O; make a few
!                             slight improvements in variable names for
!                             consistency.
! 10. 2001-05-17  Stoeckley  Replace some code with calls to the new ARRAY
!                             primitive to reduce code duplication.
!  9. 2001-05-02  Stoeckley  Change to allocate in chunks.
!  8. 2000-09-15  Stoeckley  Improve the efficiency of stringlist_name_matches
!                             and add new routines with the suffix _EXACT.
!  7. 2000-01-28  Stoeckley  Increase length of character variables to 160.
!  6. 2000-01-24  Stoeckley  Change stringlist_alloc routines to always
!                             allocate at least one array element; removed
!                             portability limitation.
!  5. 1999-11-17  Stoeckley  Add ident string for RCS.
!  4. 1999-09-10  Stoeckley  Minor documentation changes.
!  3. 1999-08-26  Stoeckley  Modified STRINGLIST_PUT_STRINGS to get around
!                             Absoft compiler bugs.
!  2. 1999-07-23  Stoeckley  Added portability limitation documentation.
!                             Also made slight change in stringlist_put_string.
!  1. 1999-06-25  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS             
!
! As of testing on 2000-01-21, the Portland Group compiler has been fixed,
! so that the following information is no longer relevant, but is retained
! for historical interest.
!
!    However, two limitations (possibly not actual bugs) have been found
!    with the Portland Group compiler (but not with the Absoft or Sun
!    compilers):
!
!      (1) To get the length of a character variable in a character array,
!          the construct LEN(ARRAY(N)) (where N is an array index) must be
!          used because LEN(ARRAY) returns garbage.  (I do not know whether
!          N must lie within the array bounds in this case, or whether this
!          works if the array is unallocated or allocated with zero length.)
!
!      (2) When passing an unallocated character pointer array to a
!          subroutine whose formal argument is not declared with the
!          pointer attribute, the program appears to abort on the
!          declaration statement when (LEN=*).  (Of course, even without
!          this limitation, ways must be provided - such as an argument
!          specifying the usable number of array elements - to keep from
!          using this argument when not allocated.)
!
!                   ++++++++++++++++++++++++++++++++++
!
! Older information (prior to 2000-01-21):
!
! The LINUX Portland Group compiler suffers from an internal compiler error
! when a formal subroutine argument has the following type:
!
!       character(len=*),pointer :: array(:)
!
! The length of each element of the array must be explicitly specified:
!
!       character(len=value),pointer :: array(:)
!
! In this primitive, the internal compiler error occurs in stringlist_alloc.
! A way to fix the problem would be to add a length argument to the subroutine
! as follows:
!
! Instead of this:
!
!     subroutine stringlist_alloc_strings (obj, pstrings, nstrings)
!     implicit none
!     type(stringlist_struct),intent(in)  :: obj           ! argument
!     character(len=*)       ,pointer     :: pstrings(:)   ! argument
!     integer                ,intent(out) :: nstrings      ! argument
!
! One could do this:
!
!     subroutine stringlist_alloc_strings (obj, pstrings, nstrings, length)
!     implicit none
!     type(stringlist_struct),intent(in)  :: obj           ! argument
!     character(len=length)  ,pointer     :: pstrings(:)   ! argument
!     integer                ,intent(out) :: nstrings      ! argument
!     integer                ,intent(in)  :: length        ! argument
!
! This problem is to be fixed in a later revision of the compiler.
!
!                   ++++++++++++++++++++++++++++++++++
!
! In addition, bugs were found in the LINUX Absoft compiler, but it was
! possible to program around those bugs in the code in this primitive.
! The details are described in comments in the code next to the subroutine
! stringlist_put_strings.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!


module stringlist_module

  use named_constants_module
  use string_module
  use array_module
  implicit none
  public

      character(len=100),public,save :: STRINGLIST_IDENT = &
       '$Id: stringlist.f90,v 1.13 2006/06/20 13:12:10 Menger prod sps $'


!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!


! integer,parameter,public  :: STRINGLIST_LENGTH = 320  ! bad with fktr & pgf90
! integer,parameter,public  :: STRINGLIST_LENGTH = 160  ! ok with fktr & pgf90
! integer,parameter,public  :: STRINGLIST_LENGTH =  80  ! ok with fktr & pgf90
  integer,parameter,public  :: STRINGLIST_LENGTH = 200  ! ok with fktr & pgf90
! integer,parameter,public  :: STRINGLIST_LENGTH = 240  ! bad
! integer,parameter,public  :: STRINGLIST_LENGTH = 260  ! bad
! integer,parameter,public  :: STRINGLIST_LENGTH = 152  ! bad

  type,public :: stringlist_struct

    private
    character(len=STRINGLIST_LENGTH)         :: name
    character(len=STRINGLIST_LENGTH),pointer :: strings(:)
    integer                                  :: nstrings
    integer                                  :: nalloc
    character(len=STRINGLIST_LENGTH)         :: buffer

  end type stringlist_struct


!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!


contains


!!--------------------- create clear delete copy print ---------------------!!
!!--------------------- create clear delete copy print ---------------------!!
!!--------------------- create clear delete copy print ---------------------!!


      subroutine stringlist_create (obj)
      implicit none
      type(stringlist_struct),pointer :: obj             ! argument

      nullify(obj)
      allocate(obj)
      nullify(obj%strings)
      obj%name     = ' '
      obj%nstrings = 0
      obj%nalloc   = 0
      obj%buffer   = ' '
      return
      end subroutine stringlist_create



      subroutine stringlist_delete (obj)
      implicit none
      type(stringlist_struct),pointer :: obj             ! argument

      call stringlist_clear (obj)
      deallocate(obj)
      return
      end subroutine stringlist_delete



      subroutine stringlist_clear (obj)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument

      if(associated(obj%strings)) then
           deallocate(obj%strings)
           obj%nstrings = 0
           obj%nalloc   = 0
      end if
      return
      end subroutine stringlist_clear


! The conditional test in stringlist_copy is necessary with the portland
! group compiler because (1) obj1%strings is disassociated when
! obj1%nstrings is zero, and (2) stringlist_put_strings aborts apparently
! on the declaration of the strings argument in this case (probably because
! the length of the string cannot be obtained).


      subroutine stringlist_copy (obj1, obj2)
      implicit none
      type(stringlist_struct),intent(in)    :: obj1            ! argument
      type(stringlist_struct),intent(inout) :: obj2            ! argument

      if (obj1%nstrings > 0) then
           call stringlist_put_strings (obj2, obj1%strings, obj1%nstrings)
      else
           call stringlist_clear       (obj2)
      end if
      obj2%name = obj1%name
      return
      end subroutine stringlist_copy



      subroutine stringlist_print (obj,lunprint)
      implicit none
      type(stringlist_struct),intent(in) :: obj            ! arguments
      integer                ,intent(in) :: lunprint       ! arguments
      integer                            :: indx           ! local

      if (lunprint <= 0) return
      write (lunprint,5000)
      if (obj%name /= ' ') write (lunprint,6000) trim(obj%name)
      do indx = 1,obj%nstrings
           write (lunprint,5000) indx,trim(obj%strings(indx))
      end do
      write (lunprint,5000)
5000  format (1x,i3,1x,a)
6000  format (' name ',a)
      return
      end subroutine stringlist_print


!!----------------- to access the name of the stringlist ------------------!!
!!----------------- to access the name of the stringlist ------------------!!
!!----------------- to access the name of the stringlist ------------------!!


      subroutine stringlist_get_name (obj, name)
      implicit none
      type(stringlist_struct),intent(in)  :: obj             ! argument
      character(len=*)       ,intent(out) :: name            ! argument

      name = obj%name
      return
      end subroutine stringlist_get_name


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_set_name (obj, name)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      character(len=*)       ,intent(in)    :: name            ! argument

      call string_to_upper (name, obj%name)
      return
      end subroutine stringlist_set_name



      subroutine stringlist_set_name_exact (obj, name)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      character(len=*)       ,intent(in)    :: name            ! argument

      obj%name = name
      return
      end subroutine stringlist_set_name_exact


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function stringlist_name_matches (obj, name) result (matches)
      implicit none
      type(stringlist_struct),intent(in) :: obj            ! argument
      character(len=*)       ,intent(in) :: name           ! argument
      logical                            :: matches        ! result

      matches = string_upper_compare (name, obj%name)
      return
      end function stringlist_name_matches



      function stringlist_name_matches_exact (obj, name) result (matches)
      implicit none
      type(stringlist_struct),intent(in) :: obj            ! argument
      character(len=*)       ,intent(in) :: name           ! argument
      logical                            :: matches        ! result

      matches = (name == obj%name)
      return
      end function stringlist_name_matches_exact


!!-------------------- to get strings from the stringlist -------------------!!
!!-------------------- to get strings from the stringlist -------------------!!
!!-------------------- to get strings from the stringlist -------------------!!


      function stringlist_num_strings (obj) result (nstrings)
      implicit none
      type(stringlist_struct),intent(in) :: obj             ! argument
      integer                            :: nstrings        ! result

      nstrings = obj%nstrings
      return
      end function stringlist_num_strings


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_get_strings (obj, strings, nstrings, errmsg)
      implicit none
      type(stringlist_struct)  ,intent(in)  :: obj           ! argument
      character(len=*)         ,intent(out) :: strings(:)    ! argument
      integer                  ,intent(out) :: nstrings      ! argument
      character(len=*)         ,intent(out) :: errmsg        ! argument

      if (size(strings) >= obj%nstrings) then
           nstrings = obj%nstrings
           strings(1:obj%nstrings) = obj%strings(1:obj%nstrings)
           errmsg = ' '
      else
           errmsg = 'array too small to contain all elements'
      end if
      return
      end subroutine stringlist_get_strings



      subroutine stringlist_alloc_strings (obj, pstrings, nstrings)
      implicit none
      type(stringlist_struct)  ,intent(in)  :: obj            ! argument
      character(len=*)         ,pointer     :: pstrings(:)    ! argument
      integer                  ,intent(out) :: nstrings       ! argument

      if (associated(pstrings)) deallocate(pstrings)
      nullify(pstrings)
      if (obj%nstrings == 0) then
           allocate(pstrings(1))              ! added 2000-01-24
      else
           allocate(pstrings(obj%nstrings))
           pstrings(1:obj%nstrings) = obj%strings(1:obj%nstrings)
      end if
      nstrings = obj%nstrings
      return
      end subroutine stringlist_alloc_strings


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_get_string (obj, indx, string, errmsg)
      implicit none
      type(stringlist_struct),intent(in)  :: obj           ! argument
      integer                ,intent(in)  :: indx          ! argument
      character(len=*)       ,intent(out) :: string        ! argument
      character(len=*)       ,intent(out) :: errmsg        ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) then
           string = obj%strings(indx)
           errmsg = ' '
      else
           errmsg = 'index out of range'
      end if
      return
      end subroutine stringlist_get_string



      subroutine stringlist_get_last_string (obj, string, errmsg)
      implicit none
      type(stringlist_struct),intent(in)  :: obj           ! argument
      character(len=*)       ,intent(out) :: string        ! argument
      character(len=*)       ,intent(out) :: errmsg        ! argument

      if (obj%nstrings > 0) then
           string = obj%strings(obj%nstrings)
           errmsg = ' '
      else
           errmsg = 'trying to get last string when there are no strings'
      end if
      return
      end subroutine stringlist_get_last_string


!!-------------------- to put strings into the stringlist -------------------!!
!!-------------------- to put strings into the stringlist -------------------!!
!!-------------------- to put strings into the stringlist -------------------!!


! The lines below labelled OLD were replaced with the lines labelled NEW in
! order to get around a bug in the absoft compiler.  The code aborted when
! NSTRINGS was zero, even though the Fortran-90 standard allows allocating
! and copying zero-length arrays.  Interestingly, replacing (LEN=*) with a
! fixed length caused the code to work correctly; this of course would not
! be an acceptable change to the code.

! The line below labelled BAD was replaced with the line labelled GOOD
! in order to get around another bug in the absoft compiler.  When
! STRINGLIST_PUT_STRINGS was called from STRINGLIST_COPY, the length of
! the STRINGS argument was garbage, as attested by the commented-out print
! statement.  Again, replacing (LEN=*) with a fixed length caused the code
! to work correctly; this of course would also not be an acceptable change
! to the code.  It turns out that replacing the argument declaration
! STRINGS(NSTRINGS) with STRINGS(:) fixes the problem.  It also turns out
! that the problem only occurs when the STRINGS argument is a member of a
! data structure in the calling program, and that is the reason the problem
! manifested itself when called from STRINGLIST_COPY.

! A problem with the portland group compiler restricts this call to cases
! where the second argument is associated (if it is a pointer), even though
! that argument is never referenced in such cases (because nstrings is zero).
! Apparently it aborts on the declaration statement when trying to get the
! length of the character variables in the second argument.  Also, curiously,
! the portland group compiler (even when nstrings > 0) can get the length
! only from a specific array element and not from the array without an index.


      subroutine stringlist_put_strings (obj, strings, nstrings)
      implicit none
      type(stringlist_struct)  ,intent(inout) :: obj                ! argument
      integer                  ,intent(in)    :: nstrings           ! argument
!!!!  character(len=*)         ,intent(in)    :: strings(nstrings)  ! BAD
      character(len=*)         ,intent(in)    :: strings(:)         ! GOOD

!print *, 'entering stringlist_put_strings'
!if (nstrings > 0) then
!print *, '    nstrings = ',nstrings,'  length(strings)    = ',len(strings)
!print *, '    nstrings = ',nstrings,'  length(strings(1)) = ',len(strings(1))
!else
!print *, '    nstrings = ',nstrings
!end if

      if (associated(obj%strings)) deallocate (obj%strings)
      if (nstrings > 0) then                                   ! NEW
           allocate(obj%strings(nstrings))                     ! NEW
           obj%strings(1:nstrings) = strings(1:nstrings)       ! NEW
      end if                                                   ! NEW
      obj%nstrings = nstrings
      obj%nalloc   = nstrings
!!!!  allocate(obj%strings(nstrings))                          ! OLD
!!!!  obj%strings(1:nstrings) = strings(1:nstrings)            ! OLD
      return
      end subroutine stringlist_put_strings



      subroutine stringlist_add_strings (obj, strings, nstrings)
      implicit none
      type(stringlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)         ,intent(in)    :: strings(:)      ! argument
      integer                  ,intent(in)    :: nstrings        ! argument
      integer                                 :: indx            ! local

      do indx = 1,nstrings
           call stringlist_add_string (obj, strings(indx))
      end do
      return
      end subroutine stringlist_add_strings


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_put_empty_strings (obj, nstrings)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      integer                ,intent(in)    :: nstrings        ! argument

      if (associated(obj%strings)) deallocate (obj%strings)
      allocate(obj%strings(nstrings))
      obj%nstrings = nstrings
      obj%nalloc   = nstrings
      obj%strings(1:nstrings) = ' '
      return
      end subroutine stringlist_put_empty_strings



      subroutine stringlist_add_empty_strings (obj, nstrings)
      implicit none
      type(stringlist_struct)  ,intent(inout) :: obj             ! argument
      integer                  ,intent(in)    :: nstrings        ! argument
      integer                                 :: indx            ! local

      do indx = 1,nstrings
           call stringlist_add_empty_string (obj)
      end do
      return
      end subroutine stringlist_add_empty_strings


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_put_string (obj, string)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      character(len=*)       ,intent(in)    :: string          ! argument

      if (associated(obj%strings)) deallocate (obj%strings)
      allocate(obj%strings(1))
      obj%nstrings = 1
      obj%nalloc   = 1
      obj%strings(1) = string
      return
      end subroutine stringlist_put_string



      subroutine stringlist_add_string (obj, string)
      implicit none
      type(stringlist_struct),intent(inout) :: obj                 ! argument
      character(len=*)       ,intent(in)    :: string              ! argument

      call stringlist_insert_string (obj, obj%nstrings+1, string)
      return
      end subroutine stringlist_add_string


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_put_empty_string (obj)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument

      call stringlist_put_string (obj, ' ')
      return
      end subroutine stringlist_put_empty_string



      subroutine stringlist_add_empty_string (obj)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument

      call stringlist_add_string (obj, ' ')
      return
      end subroutine stringlist_add_empty_string


                       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine stringlist_insert_string (obj, indx, string)
      implicit none
      type(stringlist_struct)  ,intent(inout) :: obj              ! argument
      integer                  ,intent(in)    :: indx             ! argument
      character(len=*),optional,intent(in)    :: string           ! argument

      if (present(string)) then
        call array_insert_element (obj%strings, obj%nstrings, indx, string)
      else
        call array_insert_element (obj%strings, obj%nstrings, indx, obj%buffer)
      end if
      return
      end subroutine stringlist_insert_string



      subroutine stringlist_remove_string (obj, indx)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      integer                ,intent(in)    :: indx            ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) obj%buffer = obj%strings(indx)
      call array_remove_element (obj%strings, obj%nstrings, indx)
      return
      end subroutine stringlist_remove_string



      subroutine stringlist_clear_buffer (obj)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument

      obj%buffer = ' '
      return
      end subroutine stringlist_clear_buffer



      subroutine stringlist_replace_string (obj, indx, string)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      integer                ,intent(in)    :: indx            ! argument
      character(len=*)       ,intent(in)    :: string          ! argument

      if (indx >= 1 .and. indx <= obj%nstrings) then
           obj%strings(indx) = string
      end if
      return
      end subroutine stringlist_replace_string



      subroutine stringlist_replace_add_string (obj, indx, string)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      integer                ,intent(in)    :: indx            ! argument
      character(len=*)       ,intent(in)    :: string          ! argument


      if (indx <= obj%nstrings) then
           call stringlist_replace_string    (obj, indx, string)
      else if (indx == obj%nstrings + 1) then
           call stringlist_add_string        (obj, string)
      else
           call stringlist_add_empty_strings (obj, indx - obj%nstrings - 1)
           call stringlist_add_string        (obj, string)
      end if
      return
      end subroutine stringlist_replace_add_string



      subroutine stringlist_replace_last_string (obj, string)
      implicit none
      type(stringlist_struct),intent(inout) :: obj             ! argument
      character(len=*)       ,intent(in)    :: string          ! argument

      call stringlist_replace_string (obj, obj%nstrings, string)
      return
      end subroutine stringlist_replace_last_string


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module stringlist_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

