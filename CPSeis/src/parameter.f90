
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- parameter.f90 ------------------------------!!
!!---------------------------- parameter.f90 ------------------------------!!
!!---------------------------- parameter.f90 ------------------------------!!


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
! Name       : PARAMETER
! Category   : character
! Written    : 1999-06-22   by: Tom Stoeckley
! Revised    : 2009-09-23   by: Bill Menger
! Maturity   : beta
! Purpose    : Module for storing and retrieving a parameter.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module contains a single parameter which might be a scalar data value
! (a single value) or an array of data values (zero or more values).  These
! data values can be integer, real, double precision, logical, or character
! variables.  Scalar data values can also be type(grid_struct).  The parameter
! is retained as a string or a list of strings, and therefore can be provided
! or retrieved either as strings or as variables of any compatible type.
!
! Each instance of this module has a keyword associated with the parameter
! in this module.
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
!          TO CREATE AND CLEAR AND DELETE AND COPY THE PARAMETER
!
!                    call parameter_create (obj)
!                    call parameter_clear  (obj)
!                    call parameter_delete (obj)
!                    call parameter_copy   (obj1, obj2)
!                                            i     b
!
! type(parameter_struct)     obj = the parameter structure.
!
! PARAMETER_CREATE:
!   (1) allocates obj (PASSED AS A POINTER).
!   (2) sets the nature of the parameter to be EMPTY.
!   (3) sets the vartype of the parameter to be STRING.
!   (4) sets the keyword to blank.
!
! PARAMETER_CLEAR:
!   (1) deletes all parameter values.
!   (2) sets the nature of the parameter to be EMPTY.
!   (3) sets the vartype of the parameter to be STRING.
!   (4) does not change the keyword.
!
! PARAMETER_DELETE:
!   (1) deletes all parameter values.
!   (2) deallocates obj (PASSED AS A POINTER).
!
! PARAMETER_COPY:
!   (1) copies the contents of OBJ1 to OBJ2.
!   (2) the previous contents of OBJ2 are deleted first.
!
! The nature of the parameter can be one of three possibilities:
!   (1) the parameter can be EMPTY (no parameter).
!   (2) the parameter can be a SCALAR (a single scalar parameter).
!   (3) the parameter can be an ARRAY (zero or more array elements).
!
! Note that an ARRAY with no elements is different from an EMPTY parameter.
! Note that an ARRAY with one element is different from a SCALAR.
!
!-------------------------------------------------------------------------------
!                        TO ACCESS THE KEYWORD
!
!                                                             o
!                   call parameter_get_keyword       (obj, keyword)
!
!                                                             i 
!                   call parameter_set_keyword       (obj, keyword)
!                   call parameter_set_keyword_exact (obj, keyword)
!
!             o                                               i
!          matches = parameter_keyword_matches       (obj, keyword)
!          matches = parameter_keyword_matches_exact (obj, keyword)
!
! type(parameter_struct)      obj = the parameter structure.
! character(len=*)        keyword = keyword of the parameter.
! logical                 matches = true if the keyword matches.
!
! The keyword can be blank.
! The keyword is converted to upper case when input.
! The keyword matching is done in a case-insensitive manner.
!
! The routines with the _EXACT suffix use KEYWORD exactly as it is, and do
! not convert it to upper case.  These routines can be used if the keyword is
! already upper case when input (for efficiency reasons).
!
!-------------------------------------------------------------------------------
!                      TO ACCESS PARAMETER VALUES
!
!                    o
!                 nelements = parameter_num_elements (obj)
!                 nature    = parameter_nature       (obj)
!                 vartype   = parameter_vartype      (obj)
!
!                                             o        o         o
!      call parameter_alloc_array     (obj, parray, nelements, errmsg)
!      call parameter_get_array       (obj, array,  nelements, errmsg)
!
!                                             i        o         o
!      call parameter_get_scalar      (obj,         scalar,    errmsg)
!      call parameter_get_element     (obj, indx,   element,   errmsg)
!
!                                                              opt   opt
!                                             i       i         i     i
!      call parameter_put_array       (obj, array, nelements, nchar, ndec)
!
!                                                               opt   opt
!                                                 i     i        i     i
!      call parameter_put_scalar          (obj,       scalar,  nchar, ndec)
!      call parameter_add_element         (obj,       element, nchar, ndec)
!      call parameter_insert_element      (obj, indx, element, nchar, ndec)
!      call parameter_insert_element      (obj, indx                      )
!      call parameter_remove_element      (obj, indx                      )
!      call parameter_replace_element     (obj, indx, element, nchar, ndec)
!      call parameter_replace_add_element (obj, indx, element, nchar, ndec)
!      call parameter_clear_buffer        (obj)
!
!          o                                            i        i
!       matches = parameter_array_matches       (obj, array, nelements)
!       matches = parameter_scalar_matches      (obj, scalar)
!       matches = parameter_element_matches     (obj, element, indx)
!
!          indx = parameter_find_element        (obj, element)
!          indx = parameter_find_or_add_element (obj, element, nchar, ndec)
!            o                                          i        i     i
!                                                               opt   opt
!
! type(parameter_struct)          obj = the parameter structure.
! integer                   nelements = number of array elements.
! integer                      nature = nature of the parameter.
! integer                     vartype = variable type of the parameter.
! integer                        indx = index of desired array element.
! (any type)                   scalar = single scalar parameter value.
! (any type)                  element = individual array element.
! (any type)                 array(:) = array of parameter values.
! (any type),pointer        parray(:) = pointer to array of parameter values.
! character(len=*)             errmsg = error message (blank if no error).
! integer,       optional       nchar = maximum number of characters to encode.
! integer,       optional        ndec = maximum number of decimals to encode.
! logical                     matches = true if argument matches the parameter.
!
! The type of ARRAY and SCALAR and ELEMENT can be real, integer, double
! precision, logical, or character(len=*).  The type of SCALAR can also
! be type(grid_struct).  Character variables exceeding length PARAMETER_LENGTH
! will be truncated.
!
!      | PARAMETER_LENGTH is a named constant with the same value |
!      | as STRINGLIST_LENGTH (currently 160 as of 2000-01-28)    |
!      | in the STRINGLIST primitive.                             |
!
! Since the parameter values are stored internally as character strings,
! the type does not have to match from one subroutine call to another.
! Logical values are stored internally as the strings 'YES' or 'NO'.
! Nil values are stored internally as a blank string.
! Nil values are defined in the NAMED_CONSTANTS module.
!
! The NCHAR argument is for integer, real, and double precision variables only. 
! The NDEC argument is for real and double precision variables only.
! The NCHAR and NDEC arguments are both used for type(grid_struct).
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
! PARAMETER_NUM_ELEMENTS:
!   (1) returns number of elements (0 or more) if the parameter is an ARRAY.
!   (2) returns              1                 if the parameter is a SCALAR.
!   (3) returns              0                 if the parameter is EMPTY.
!
! PARAMETER_NATURE:
!   (1) returns named constant PARAMETER_ARRAY  if the parameter is an ARRAY.
!   (2) returns named constant PARAMETER_SCALAR if the parameter is a SCALAR.
!   (3) returns named constant PARAMETER_EMPTY  if the parameter is EMPTY.
!
! PARAMETER_VARTYPE:
!   (1) returns named constant PARAMETER_INTEGER if the parameter is an INTEGER.
!   (2) returns named constant PARAMETER_FLOAT   if the parameter is a FLOAT.
!   (3) returns named constant PARAMETER_DOUBLE  if the parameter is DOUBLE.
!   (4) returns named constant PARAMETER_STRING  if the parameter is STRING.
!   (5) returns named constant PARAMETER_LOGICAL if the parameter is LOGICAL.
!   (6) returns named constant PARAMETER_GRID    if the parameter is GRID.
!
! PARAMETER_ALLOC_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) PARRAY is deallocated and reallocated to contain all elements.
!          (PARRAY must be nullified or allocated before first use.)
!          (PARRAY should be deallocated after last use.)
!   (3) PARRAY is always reallocated to at least one array element, even if
!        NELEMENTS is set to zero or an error occurs.
!   (4) an error occurs if the parameter is a scalar.
!   (5) an error occurs if the parameter is empty.
!   (6) an error occurs if any element cannot be decoded into the desired type.
!   (7) does not reset PARRAY or NELEMENTS if an error occurs.
!
! PARAMETER_GET_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) an error occurs if ARRAY is dimensioned too small for all elements.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if the parameter is empty.
!   (5) an error occurs if any element cannot be decoded into the desired type.
!   (6) does not reset ARRAY or NELEMENTS if an error occurs.
!
! PARAMETER_GET_SCALAR:
!   (1) gets the requested scalar value in the parameter object.
!   (2) an error occurs if the parameter is an array.
!   (3) an error occurs if the parameter is empty.
!   (4) an error occurs if the scalar cannot be decoded into the desired type.
!   (5) does not reset SCALAR if an error occurs.
!
! PARAMETER_GET_ELEMENT:
!   (1) gets the requested array element in the parameter object.
!   (2) an error occurs if INDEX is out of range.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if the parameter is empty.
!   (5) an error occurs if the element cannot be decoded into the desired type.
!   (6) does not reset ELEMENT if an error occurs.
!
! PARAMETER_PUT_ARRAY:
!   (1) replaces the previous contents with an array with 0 or more elements.
!   (2) sets the nature of the parameter to be an ARRAY.
!
! PARAMETER_PUT_SCALAR:
!   (1) replaces the previous contents with a scalar.
!   (2) sets the nature of the parameter to be a SCALAR.
!
! PARAMETER_ADD_ELEMENT:
!   (1) appends one element to the previous contents regardless of its nature.
!   (2) sets the nature of the parameter to be an ARRAY.
!
! PARAMETER_INSERT_ELEMENT:
!   (1) inserts the specified element at the specified INDEX.
!   (2) inserts element from buffer if argument ELEMENT is missing.
!   (3) does nothing if INDEX is out of range.
!   (4) does nothing if the nature of the parameter is not an ARRAY.
!
! PARAMETER_REMOVE_ELEMENT:
!   (1) removes the element at the specified INDEX.
!   (2) puts removed element into a buffer for possible insertion later.
!   (3) does nothing if INDEX is out of range.
!   (4) does nothing if the nature of the parameter is not an ARRAY.
!
! PARAMETER_REPLACE_ELEMENT:
!   (1) replaces a previous element with the specified element.
!   (2) does nothing if INDEX is out of range.
!   (3) does nothing if the nature of the parameter is not an ARRAY.
!
! PARAMETER_REPLACE_ADD_ELEMENT:
!   (1) replaces or adds one element to previous contents regardless of nature.
!   (2) does nothing if INDEX is too small.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) increases the length of the array if necessary, filling any
!        intermediate array elements with nil.
!
! PARAMETER_ARRAY_MATCHES:
!   (1) matches all of the array elements (0 or more) with the parameter object.
!   (2) returns false if the parameter is a scalar.
!   (3) returns false if the parameter is empty.
!   (4) returns false if any element cannot be decoded into the desired type.
!   (5) returns false if the number of elements does not match.
!   (6) returns false if any element value does not match.
!   (7) returns true  if all element values match.
!
! PARAMETER_SCALAR_MATCHES:
!   (1) matches the requested scalar value with the parameter object.
!   (2) returns false if the parameter is an array.
!   (3) returns false if the parameter is empty.
!   (4) returns false if the scalar cannot be decoded into the desired type.
!   (5) returns false if the scalar value does not match.
!   (6) returns true  if the scalar value matches.
!
! PARAMETER_ELEMENT_MATCHES:
!   (1) matches the specified array element with the parameter object.
!   (2) returns false if the parameter is a scalar.
!   (3) returns false if the parameter is empty.
!   (4) returns false if the element cannot be decoded into the desired type.
!   (5) returns false if the index is out of range.
!   (6) returns false if the element value does not match.
!   (7) returns true  if the element value matches.
!
! PARAMETER_FIND_ELEMENT:
!   (1) returns the index of the matching array element.
!   (2) returns 0 if there is no match.
!   (3) returns 0 if the parameter is a scalar.
!   (4) returns 0 if the parameter is empty.
!   (5) returns 0 if none of the elements can be decoded into the desired type.
!
! PARAMETER_FIND_OR_ADD_ELEMENT:
!   (1) finds the matching element regardless of the nature of the contents.
!   (2) adds the element to the array if there is no match.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) returns the index of the matching (or added) array element.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 14. 2009-09-23  Bill Menger Modified logic for the cases where nelements == 0 in an
!                             array.  Logic looked faulty as sometimes it would try to
!                             deallocate memory when the pointer may not have been
!                             initialized.
! 13. 2008-12-11  Bill Menger Nullified more pointers.
! 12. 2007-09-18  Stoeckley  Add ability to get variable types.
!011. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!010. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2002-02-04  Stoeckley  Add PARAMETER_INSERT_FROM_BUFFER,
!                             PARAMETER_CLEAR_BUFFER,
!                             PARAMETER_REPLACE_ADD_ELEMENT,
!                             PARAMETER_ELEMENT_MATCHES, PARAMETER_FIND_ELEMENT,
!                             and PARAMETER_FIND_OR_ADD_ELEMENT, all to help
!                             support workstation program I/O; make a few
!                             slight improvements in variable names for
!                             consistency.
!  8. 2000-09-15  Stoeckley  Add routines with the _EXACT suffix.
!  7. 2000-04-04  Stoeckley  Modify logic of PARAMETER_ALLOC_ARRAY calls
!                             to enforce intent(out) for NELEMENTS.
!  6. 2000-01-28  Stoeckley  Increase length of character variables.
!  5. 2000-01-24  Stoeckley  Change PARAMETER_ALLOC routines to always
!                             allocate at least one array element.
!  4. 1999-11-17  Stoeckley  Add ident string for RCS.
!  3. 1999-11-03  Stoeckley  Add call to grid_initialize.
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
!-------------------------------------------------------------------------------
!</portability_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! (1) The type(grid_struct) is stored internally as a 6-element array even
!     though it is referred to as a scalar (a single data structure).  This
!     6-element array has the values XORG, YORG, DX(1), DX(2), DX(3), and
!     DX(4), whose meanings are clear in the GRID documentation.
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!


module parameter_module

  use named_constants_module
  use stringlist_module
  use string_module
  use grid_module
  implicit none

  public
  private :: parameter_fix_errmsg
  private :: parameter_get_array_helper
  private :: parameter_get_scalar_helper
  private :: parameter_get_element_helper
  private :: parameter_cc2ii
  private :: parameter_cc2ff
  private :: parameter_cc2dd
  private :: parameter_cc2ll

      character(len=100),public,save :: PARAMETER_IDENT = &
       '$Id: parameter.f90,v 1.12 2007/09/19 14:02:24 Stoeckley beta sps $'


!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!


  integer,parameter,public :: PARAMETER_LENGTH  = STRINGLIST_LENGTH
  integer,parameter,public :: PARAMETER_EMPTY   = 1    ! nature.
  integer,parameter,public :: PARAMETER_SCALAR  = 2    ! nature.
  integer,parameter,public :: PARAMETER_ARRAY   = 3    ! nature.
  integer,parameter,public :: PARAMETER_INTEGER = 1    ! vartype.
  integer,parameter,public :: PARAMETER_FLOAT   = 2    ! vartype.
  integer,parameter,public :: PARAMETER_DOUBLE  = 3    ! vartype.
  integer,parameter,public :: PARAMETER_STRING  = 4    ! vartype.
  integer,parameter,public :: PARAMETER_LOGICAL = 5    ! vartype.
  integer,parameter,public :: PARAMETER_GRID    = 6    ! vartype.

  type,public :: parameter_struct

    private
    type(stringlist_struct),pointer :: str
    integer                         :: nature
    integer                         :: vartype

  end type parameter_struct


!!------------------------- interfaces ----------------------------------!!
!!------------------------- interfaces ----------------------------------!!
!!------------------------- interfaces ----------------------------------!!


  interface parameter_alloc_array
    module procedure parameter_alloc_iarray
    module procedure parameter_alloc_farray
    module procedure parameter_alloc_darray
    module procedure parameter_alloc_carray
    module procedure parameter_alloc_larray
  end interface

  interface parameter_get_array
    module procedure parameter_get_iarray
    module procedure parameter_get_farray
    module procedure parameter_get_darray
    module procedure parameter_get_carray
    module procedure parameter_get_larray
  end interface

  interface parameter_get_scalar
    module procedure parameter_get_iscalar
    module procedure parameter_get_fscalar
    module procedure parameter_get_dscalar
    module procedure parameter_get_cscalar
    module procedure parameter_get_lscalar
    module procedure parameter_get_gscalar
  end interface

  interface parameter_get_element
    module procedure parameter_get_ielement
    module procedure parameter_get_felement
    module procedure parameter_get_delement
    module procedure parameter_get_celement
    module procedure parameter_get_lelement
  end interface

  interface parameter_put_array
    module procedure parameter_put_iarray
    module procedure parameter_put_farray
    module procedure parameter_put_darray
    module procedure parameter_put_carray
    module procedure parameter_put_larray
  end interface

  interface parameter_put_scalar
    module procedure parameter_put_iscalar
    module procedure parameter_put_fscalar
    module procedure parameter_put_dscalar
    module procedure parameter_put_cscalar
    module procedure parameter_put_lscalar
    module procedure parameter_put_gscalar
  end interface

  interface parameter_add_element
    module procedure parameter_add_ielement
    module procedure parameter_add_felement
    module procedure parameter_add_delement
    module procedure parameter_add_celement
    module procedure parameter_add_lelement
  end interface

  interface parameter_insert_element
    module procedure parameter_insert_ielement
    module procedure parameter_insert_felement
    module procedure parameter_insert_delement
    module procedure parameter_insert_celement
    module procedure parameter_insert_lelement
    module procedure parameter_insert_from_buffer
  end interface

  interface parameter_replace_element
    module procedure parameter_replace_ielement
    module procedure parameter_replace_felement
    module procedure parameter_replace_delement
    module procedure parameter_replace_celement
    module procedure parameter_replace_lelement
  end interface

  interface parameter_replace_add_element
    module procedure parameter_replace_add_ielement
    module procedure parameter_replace_add_felement
    module procedure parameter_replace_add_delement
    module procedure parameter_replace_add_celement
    module procedure parameter_replace_add_lelement
  end interface

  interface parameter_scalar_matches
    module procedure parameter_iscalar_matches
    module procedure parameter_fscalar_matches
    module procedure parameter_dscalar_matches
    module procedure parameter_cscalar_matches
    module procedure parameter_lscalar_matches
    module procedure parameter_gscalar_matches
  end interface

  interface parameter_array_matches
    module procedure parameter_iarray_matches
    module procedure parameter_farray_matches
    module procedure parameter_darray_matches
    module procedure parameter_carray_matches
    module procedure parameter_larray_matches
  end interface

  interface parameter_element_matches
    module procedure parameter_ielement_matches
    module procedure parameter_felement_matches
    module procedure parameter_delement_matches
    module procedure parameter_celement_matches
    module procedure parameter_lelement_matches
  end interface

  interface parameter_find_element
    module procedure parameter_find_ielement
    module procedure parameter_find_felement
    module procedure parameter_find_delement
    module procedure parameter_find_celement
    module procedure parameter_find_lelement
  end interface

  interface parameter_find_or_add_element
    module procedure parameter_find_or_add_ielement
    module procedure parameter_find_or_add_felement
    module procedure parameter_find_or_add_delement
    module procedure parameter_find_or_add_celement
    module procedure parameter_find_or_add_lelement
  end interface


!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!


contains


!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!


      subroutine parameter_create (obj)
      implicit none
      type(parameter_struct),pointer :: obj             ! argument

      nullify(obj)
      allocate(obj)
      nullify (obj%str) ! jpa
      call stringlist_create (obj%str)
      obj%nature  = PARAMETER_EMPTY
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_create



      subroutine parameter_delete (obj)
      implicit none
      type(parameter_struct),pointer :: obj             ! argument

      call stringlist_delete (obj%str)
      deallocate(obj)
      return
      end subroutine parameter_delete



      subroutine parameter_clear (obj)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument

      call stringlist_clear (obj%str)
      obj%nature  = PARAMETER_EMPTY
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_clear



      subroutine parameter_copy (obj1, obj2)
      implicit none
      type(parameter_struct),intent(in)    :: obj1            ! argument
      type(parameter_struct),intent(inout) :: obj2            ! argument

      call stringlist_copy (obj1%str, obj2%str)
      obj2%nature  = obj1%nature
      obj2%vartype = obj1%vartype
      return
      end subroutine parameter_copy


!!------------------------ to access the keyword ----------------------------!!
!!------------------------ to access the keyword ----------------------------!!
!!------------------------ to access the keyword ----------------------------!!


      subroutine parameter_set_keyword (obj, keyword)
      implicit none
      type(parameter_struct),intent(inout) :: obj            ! argument
      character(len=*)      ,intent(in)    :: keyword        ! argument

      call stringlist_set_name (obj%str, keyword)
      return
      end subroutine parameter_set_keyword



      subroutine parameter_set_keyword_exact (obj, keyword)
      implicit none
      type(parameter_struct),intent(inout) :: obj            ! argument
      character(len=*)      ,intent(in)    :: keyword        ! argument

      call stringlist_set_name_exact (obj%str, keyword)
      return
      end subroutine parameter_set_keyword_exact


                          !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_get_keyword (obj, keyword)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(out) :: keyword         ! argument

      call stringlist_get_name (obj%str, keyword)
      return
      end subroutine parameter_get_keyword


                          !!!!!!!!!!!!!!!!!!!!!!!!!


      function parameter_keyword_matches (obj, keyword) result (matches)
      implicit none
      type(parameter_struct),intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      logical                           :: matches         ! result

      matches = stringlist_name_matches (obj%str, keyword)
      return
      end function parameter_keyword_matches



      function parameter_keyword_matches_exact (obj, keyword) result (matches)
      implicit none
      type(parameter_struct),intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      logical                           :: matches         ! result

      matches = stringlist_name_matches_exact (obj%str, keyword)
      return
      end function parameter_keyword_matches_exact


!!----------------------- private helper routines ------------------------!!
!!----------------------- private helper routines ------------------------!!
!!----------------------- private helper routines ------------------------!!


      function parameter_fix_errmsg (obj, errmsg1) result (errmsg2)
      implicit none
      type(parameter_struct),intent(in) :: obj              ! argument
      character(len=*)      ,intent(in) :: errmsg1          ! argument
      character(len=PARAMETER_LENGTH)   :: errmsg2          ! result
      character(len=PARAMETER_LENGTH)   :: keyword          ! local

      if (errmsg1 == ' ') then
           errmsg2 = ' '
      else
           call stringlist_get_name (obj%str, keyword)
           errmsg2 = trim(keyword)//': '//errmsg1
      end if
      return
      end function parameter_fix_errmsg


                          !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_get_array_helper (obj, nsize, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: nsize           ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      nelements = parameter_num_elements (obj)
      if (obj%nature == PARAMETER_EMPTY) then
           errmsg = 'attempt to get array from empty parameter'
      else if (obj%nature == PARAMETER_SCALAR) then
           errmsg = 'attempt to get array from scalar'
      else if (nelements > nsize) then
           errmsg = 'attempt to get array when dimensioned too small'
      else
           errmsg = ' '
      end if
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_array_helper



      subroutine parameter_get_scalar_helper (obj, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      if (obj%nature == PARAMETER_EMPTY) then
           errmsg = 'attempt to get scalar from empty parameter'
      else if (obj%nature == PARAMETER_ARRAY) then
           errmsg = 'attempt to get scalar from array'
      else
           errmsg = ' '
      end if
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_scalar_helper



      subroutine parameter_get_element_helper (obj, indx, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: nelements       ! local

      nelements = parameter_num_elements (obj)
      if (obj%nature == PARAMETER_EMPTY) then
           errmsg = 'attempt to get array element from empty parameter'
      else if (obj%nature == PARAMETER_SCALAR) then
           errmsg = 'attempt to get array element from scalar'
      else if (indx <= 0 .or. indx > nelements) then
           errmsg = 'index out of range when getting array element'
      else
           errmsg = ' '
      end if
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_element_helper


                          !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_cc2ii (obj, indx, value, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      integer               ,intent(out) :: value           ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: value2          ! local
      character(len=PARAMETER_LENGTH)    :: string          ! local
      integer                            :: istat           ! local

      call stringlist_get_string (obj%str, indx, string, errmsg)
      if (errmsg == ' ') call string_cc2ii (string, value2, istat, errmsg)
      if (errmsg == ' ') value = value2
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_cc2ii



      subroutine parameter_cc2ff (obj, indx, value, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      real                  ,intent(out) :: value           ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      real                               :: value2          ! local
      character(len=PARAMETER_LENGTH)    :: string          ! local
      integer                            :: istat           ! local

      call stringlist_get_string (obj%str, indx, string, errmsg)
      if (errmsg == ' ') call string_cc2ff (string, value2, istat, errmsg)
      if (errmsg == ' ') value = value2
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_cc2ff



      subroutine parameter_cc2dd (obj, indx, value, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      double precision      ,intent(out) :: value           ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      double precision                   :: value2          ! local
      character(len=PARAMETER_LENGTH)    :: string          ! local
      integer                            :: istat           ! local

      call stringlist_get_string (obj%str, indx, string, errmsg)
      if (errmsg == ' ') call string_cc2dd (string, value2, istat, errmsg)
      if (errmsg == ' ') value = value2
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_cc2dd



      subroutine parameter_cc2ll (obj, indx, value, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical               ,intent(out) :: value           ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      logical                            :: value2          ! local
      character(len=PARAMETER_LENGTH)    :: string          ! local
      integer                            :: istat           ! local

      call stringlist_get_string (obj%str, indx, string, errmsg)
      if (errmsg == ' ') call string_cc2ll (string, value2, istat, errmsg)
      if (errmsg == ' ') value = value2
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_cc2ll


!!--------------------- to access parameter values ------------------------!!
!!--------------------- to access parameter values ------------------------!!
!!--------------------- to access parameter values ------------------------!!


      function parameter_num_elements (obj) result (nelements)
      implicit none
      type(parameter_struct),intent(in) :: obj             ! argument
      integer                           :: nelements       ! result

      nelements = stringlist_num_strings (obj%str)
      return
      end function parameter_num_elements



      function parameter_nature (obj) result (nature)
      implicit none
      type(parameter_struct),intent(in) :: obj          ! argument
      integer                           :: nature       ! result

      nature = obj%nature
      return
      end function parameter_nature


      function parameter_vartype (obj) result (vartype)
      implicit none
      type(parameter_struct),intent(in) :: obj          ! argument
      integer                           :: vartype      ! result

      vartype = obj%vartype
      return
      end function parameter_vartype


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_alloc_iarray (obj, parray, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj              ! argument
      integer               ,pointer     :: parray(:)        ! argument
      integer               ,intent(out) :: nelements        ! argument
      character(len=*)      ,intent(out) :: errmsg           ! argument
      integer               ,pointer     :: parray2(:)       ! local
      integer                            :: nelements2       ! local

      nelements2 = parameter_num_elements (obj)
      errmsg = ' '
      nullify(parray2)
      allocate(parray2(nelements2))
      if(nelements2 > 0 ) parray2=0
      call parameter_get_iarray (obj, parray2, nelements, errmsg)
      ! if error from the above call, take action
      if(errmsg == ' ') then
         if(associated(parray)) deallocate (parray)
         allocate (parray(max(nelements2,1)))
         parray(1:nelements2) = parray2(1:nelements2)
      endif
      if (.not. associated(parray)) then
           allocate(parray(1))   ! added max(1) 2000-04-04
           parray = 0
      endif
      !--- parray is allocated and size 1 unless there was an error in get_iarray.
      !-- truth table
      !-- errmsg  = ' '
      !               parray is associated and >= 1 in size
      !-- errmsg /= ' '
      !--             parray is associated and is any in size
      if (size(parray) == 0 ) then
           deallocate(parray)
           allocate(parray(1))                  ! added 2000-04-04
           parray = 0
      endif 
      deallocate(parray2)
      return
      end subroutine parameter_alloc_iarray



      subroutine parameter_alloc_farray (obj, parray, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj              ! argument
      real                  ,pointer     :: parray(:)        ! argument
      integer               ,intent(out) :: nelements        ! argument
      character(len=*)      ,intent(out) :: errmsg           ! argument
      real                  ,pointer     :: parray2(:)       ! local
      integer                            :: nelements2       ! local

      nelements2 = parameter_num_elements (obj)
      nullify(parray2)
      allocate(parray2(nelements2))
      if(nelements2 > 0 ) parray2=0.0
      errmsg=' '
      call parameter_get_farray (obj, parray2, nelements, errmsg)
      !if error from the above call, take action
      if (errmsg == ' ') then
           if (associated(parray)) deallocate(parray)
           allocate(parray(max(nelements2,1)))   ! added max(1) 2000-04-04
           parray(1:nelements2) = parray2(1:nelements2)
      endif
      if (.not.associated(parray)) then    ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = 0.0
      endif
      if (size(parray) == 0) then          ! added 2000-04-04
           deallocate(parray)                   ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = 0.0
      end if
      deallocate(parray2)
      return
      end subroutine parameter_alloc_farray



      subroutine parameter_alloc_darray (obj, parray, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj              ! argument
      double precision      ,pointer     :: parray(:)        ! argument
      integer               ,intent(out) :: nelements        ! argument
      character(len=*)      ,intent(out) :: errmsg           ! argument
      double precision      ,pointer     :: parray2(:)       ! local
      integer                            :: nelements2       ! local

      nelements2 = parameter_num_elements (obj)
      nullify(parray2)
      allocate(parray2(nelements2))
      if(nelements2 > 0 ) parray2=0.0
      errmsg = ' '
      call parameter_get_darray (obj, parray2, nelements, errmsg)
      if (errmsg == ' ') then
           if (associated(parray)) deallocate(parray)
           allocate(parray(max(nelements2,1)))   ! added max(1) 2000-04-04
           parray(1:nelements2) = parray2(1:nelements2)
      endif
      if (.not.associated(parray)) then    ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = 0.0
      endif
      if (size(parray) == 0) then          ! added 2000-04-04
           deallocate(parray)                   ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = 0.0
      end if
      deallocate(parray2)
      return
      end subroutine parameter_alloc_darray



      subroutine parameter_alloc_carray (obj, parray, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)       :: obj              ! argument
      character(len=*)      ,pointer          :: parray(:)        ! argument
      integer               ,intent(out)      :: nelements        ! argument
      character(len=*)      ,intent(out)      :: errmsg           ! argument
      character(len=PARAMETER_LENGTH),pointer :: parray2(:)       ! local
      integer                                 :: nelements2       ! local

      nelements2 = parameter_num_elements (obj)
      nullify(parray2)
      allocate(parray2(nelements2))
      if(nelements2 > 0 ) parray2=''
      errmsg = ' '
      call parameter_get_carray (obj, parray2, nelements, errmsg)
      if (errmsg == ' ') then
           if (associated(parray)) deallocate(parray)
           allocate(parray(max(nelements2,1)))   ! added max(1) 2000-04-04
           parray(1:nelements2) = parray2(1:nelements2)
      endif
      if (.not.associated(parray)) then    ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = ''
      endif
      if (size(parray) == 0) then          ! added 2000-04-04
           deallocate(parray)                   ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = ''
      end if
      deallocate(parray2)
      return
      end subroutine parameter_alloc_carray



      subroutine parameter_alloc_larray (obj, parray, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj              ! argument
      logical               ,pointer     :: parray(:)        ! argument
      integer               ,intent(out) :: nelements        ! argument
      character(len=*)      ,intent(out) :: errmsg           ! argument
      logical               ,pointer     :: parray2(:)       ! local
      integer                            :: nelements2       ! local

      nelements2 = parameter_num_elements (obj)
      nullify(parray2)
      allocate(parray2(nelements2))
      if(nelements2 > 0 ) parray2=.false.
      errmsg = ' '
      call parameter_get_larray (obj, parray2, nelements, errmsg)
      if (errmsg == ' ') then
           if (associated(parray)) deallocate(parray)
           allocate(parray(max(nelements2,1)))   ! added max(1) 2000-04-04
           parray(1:nelements2) = parray2(1:nelements2)
      endif
      if (.not.associated(parray)) then    ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = .false.
      endif
      if (size(parray) == 0) then          ! added 2000-04-04
           deallocate(parray)                   ! added 2000-04-04
           allocate(parray(1))                  ! added 2000-04-04
           parray = .false.
      end if
      deallocate(parray2)
      return
      end subroutine parameter_alloc_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_get_iarray (obj, array, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(out) :: array(:)        ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: indx            ! local
      integer               ,pointer     :: array2(:)       ! local
      integer                            :: nelements2      ! local
      integer                            :: temp

      temp = size(array)
      call parameter_get_array_helper (obj, temp, nelements2, errmsg)
      if (errmsg /= ' ') return
      if (nelements2 == 0) then
           nelements = nelements2
           return
      end if
      nullify(array2)
      allocate(array2(nelements2))
      do indx = 1,nelements2
           call parameter_cc2ii (obj, indx, array2(indx), errmsg)
           if (errmsg /= ' ') exit
      end do
      if (errmsg == ' ') then
           nelements = nelements2
           array(1:nelements2) = array2(1:nelements2)
      end if
      deallocate(array2)
      return
      end subroutine parameter_get_iarray



      subroutine parameter_get_farray (obj, array, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(out) :: array(:)        ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: indx            ! local
      real                  ,pointer     :: array2(:)       ! local
      integer                            :: nelements2      ! local
      integer                            :: temp

      temp = size(array)

      call parameter_get_array_helper (obj, temp, nelements2, errmsg)
      if (errmsg /= ' ') return
      if (nelements2 == 0) then
           nelements = nelements2
           return
      end if
      nullify(array2)
      allocate(array2(nelements2))
      do indx = 1,nelements2
           call parameter_cc2ff (obj, indx, array2(indx), errmsg)
           if (errmsg /= ' ') exit
      end do
      if (errmsg == ' ') then
           nelements = nelements2
           array(1:nelements2) = array2(1:nelements2)
      end if
      deallocate(array2)
      return
      end subroutine parameter_get_farray



      subroutine parameter_get_darray (obj, array, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(out) :: array(:)        ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: indx            ! local
      double precision      ,pointer     :: array2(:)       ! local
      integer                            :: nelements2      ! local
      integer                            :: temp

      temp = size(array)
      call parameter_get_array_helper (obj, temp, nelements2, errmsg)
      if (errmsg /= ' ') return
      if (nelements2 == 0) then
           nelements = nelements2
           return
      end if
      nullify(array2)
      allocate(array2(nelements2))
      do indx = 1,nelements2
           call parameter_cc2dd (obj, indx, array2(indx), errmsg)
           if (errmsg /= ' ') exit
      end do
      if (errmsg == ' ') then
           nelements = nelements2
           array(1:nelements2) = array2(1:nelements2)
      end if
      deallocate(array2)
      return
      end subroutine parameter_get_darray



      subroutine parameter_get_carray (obj, array, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(out) :: array(:)        ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      integer                            :: nelements2      ! local
      integer                            :: temp

      temp = size(array)

      call parameter_get_array_helper (obj, temp, nelements2, errmsg)
      if (errmsg /= ' ') return
      if (nelements2 == 0) then
           nelements = nelements2
           return
      end if
      call stringlist_get_strings (obj%str, array, nelements, errmsg)
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_carray



      subroutine parameter_get_larray (obj, array, nelements, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(out) :: array(:)        ! argument
      integer               ,intent(out) :: nelements       ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: indx            ! local
      logical               ,pointer     :: array2(:)       ! local
      integer                            :: nelements2      ! local
      integer                            :: temp

      temp = size(array)
      call parameter_get_array_helper (obj, temp, nelements2, errmsg)
      if (errmsg /= ' ') return
      if (nelements2 == 0) then
           nelements = nelements2
           return
      end if
      nullify(array2)
      allocate(array2(nelements2))
      do indx = 1,nelements2
           call parameter_cc2ll (obj, indx, array2(indx), errmsg)
           if (errmsg /= ' ') exit
      end do
      if (errmsg == ' ') then
           nelements = nelements2
           array(1:nelements2) = array2(1:nelements2)
      end if
      deallocate(array2)
      return
      end subroutine parameter_get_larray



                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_get_iscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_scalar_helper (obj, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ii (obj, 1, scalar, errmsg)
      return
      end subroutine parameter_get_iscalar


      subroutine parameter_get_fscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_scalar_helper (obj, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ff (obj, 1, scalar, errmsg)
      return
      end subroutine parameter_get_fscalar


      subroutine parameter_get_dscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_scalar_helper (obj, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2dd (obj, 1, scalar, errmsg)
      return
      end subroutine parameter_get_dscalar


      subroutine parameter_get_cscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_scalar_helper (obj, errmsg)
      if (errmsg /= ' ') return
      call stringlist_get_string (obj%str, 1, scalar, errmsg)
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_cscalar


      subroutine parameter_get_lscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_scalar_helper (obj, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ll (obj, 1, scalar, errmsg)
      return
      end subroutine parameter_get_lscalar


      subroutine parameter_get_gscalar (obj, scalar, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      type(grid_struct)     ,intent(out) :: scalar          ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument
      integer                            :: nelements       ! local
      double precision                   :: array(6)        ! local

      call parameter_get_darray (obj, array, nelements, errmsg)
      if (errmsg /= ' ') return
      if (nelements /= 6) then
           errmsg = 'trying to get grid_struct when #elements not six'
           errmsg = parameter_fix_errmsg (obj, errmsg)
      else
           call grid_initialize (scalar)
           call grid_set_origin (scalar, array(1),array(2))
           call grid_set_dx     (scalar, array(3),array(4),array(5),array(6))
      end if
      return
      end subroutine parameter_get_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_get_ielement (obj, indx, element, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      integer               ,intent(out) :: element         ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_element_helper (obj, indx, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ii (obj, indx, element, errmsg)
      return
      end subroutine parameter_get_ielement


      subroutine parameter_get_felement (obj, indx, element, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      real                  ,intent(out) :: element         ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_element_helper (obj, indx, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ff (obj, indx, element, errmsg)
      return
      end subroutine parameter_get_felement


      subroutine parameter_get_delement (obj, indx, element, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      double precision      ,intent(out) :: element         ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_element_helper (obj, indx, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2dd (obj, indx, element, errmsg)
      return
      end subroutine parameter_get_delement


      subroutine parameter_get_celement (obj, indx, element, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      character(len=*)      ,intent(out) :: element         ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_element_helper (obj, indx, errmsg)
      if (errmsg /= ' ') return
      call stringlist_get_string (obj%str, indx, element, errmsg)
      errmsg = parameter_fix_errmsg (obj, errmsg)
      return
      end subroutine parameter_get_celement


      subroutine parameter_get_lelement (obj, indx, element, errmsg)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical               ,intent(out) :: element         ! argument
      character(len=*)      ,intent(out) :: errmsg          ! argument

      call parameter_get_element_helper (obj, indx, errmsg)
      if (errmsg /= ' ') return
      call parameter_cc2ll (obj, indx, element, errmsg)
      return
      end subroutine parameter_get_lelement


                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_put_iarray (obj, array, nelements, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj                ! argument
      integer               ,intent(in)    :: nelements          ! argument
      integer               ,intent(in)    :: array(:)           ! argument
      integer,optional      ,intent(in)    :: nchar              ! argument
      integer                              :: indx               ! local
      character(len=PARAMETER_LENGTH)      :: string             ! local

      call stringlist_put_empty_strings (obj%str, nelements)
      do indx = 1,nelements
           call string_ii2cc              (array(indx), string, nchar)
           call stringlist_replace_string (obj%str, indx, string)
      end do
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_put_iarray


      subroutine parameter_put_farray (obj, array, nelements, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj                ! argument
      integer               ,intent(in)    :: nelements          ! argument
      real                  ,intent(in)    :: array(:)           ! argument
      integer,optional      ,intent(in)    :: nchar              ! argument
      integer,optional      ,intent(in)    :: ndec               ! argument
      integer                              :: indx               ! local
      character(len=PARAMETER_LENGTH)      :: string             ! local

      call stringlist_put_empty_strings (obj%str, nelements)
      do indx = 1,nelements
           call string_ff2cc              (array(indx), string, nchar, ndec)
           call stringlist_replace_string (obj%str, indx, string)
      end do
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_put_farray


      subroutine parameter_put_darray (obj, array, nelements, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj                ! argument
      integer               ,intent(in)    :: nelements          ! argument
      double precision      ,intent(in)    :: array(:)           ! argument
      integer,optional      ,intent(in)    :: nchar              ! argument
      integer,optional      ,intent(in)    :: ndec               ! argument
      integer                              :: indx               ! local
      character(len=PARAMETER_LENGTH)      :: string             ! local

      call stringlist_put_empty_strings (obj%str, nelements)
      do indx = 1,nelements
           call string_dd2cc              (array(indx), string, nchar, ndec)
           call stringlist_replace_string (obj%str, indx, string)
      end do
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_put_darray


      subroutine parameter_put_carray (obj, array, nelements)
      implicit none
      type(parameter_struct),intent(inout) :: obj                ! argument
      integer               ,intent(in)    :: nelements          ! argument
      character(len=*)      ,intent(in)    :: array(:)           ! argument
      integer                              :: indx               ! local
      character(len=PARAMETER_LENGTH)      :: string             ! local

      call stringlist_put_empty_strings (obj%str, nelements)
      do indx = 1,nelements
           string = array(indx)
           call stringlist_replace_string (obj%str, indx, string)
      end do
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_put_carray


      subroutine parameter_put_larray (obj, array, nelements)
      implicit none
      type(parameter_struct),intent(inout) :: obj                ! argument
      integer               ,intent(in)    :: nelements          ! argument
      logical               ,intent(in)    :: array(:)           ! argument
      integer                              :: indx               ! local
      character(len=PARAMETER_LENGTH)      :: string             ! local

      call stringlist_put_empty_strings (obj%str, nelements)
      do indx = 1,nelements
           call string_ll2cc              (array(indx), string)
           call stringlist_replace_string (obj%str, indx, string)
      end do
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_put_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_put_iscalar (obj, scalar, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: scalar          ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ii2cc          (scalar, string, nchar)
      call stringlist_put_string (obj%str, string)
      obj%nature  = PARAMETER_SCALAR
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_put_iscalar


      subroutine parameter_put_fscalar (obj, scalar, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      real                  ,intent(in)    :: scalar          ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ff2cc          (scalar, string, nchar, ndec)
      call stringlist_put_string (obj%str, string)
      obj%nature  = PARAMETER_SCALAR
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_put_fscalar


      subroutine parameter_put_dscalar (obj, scalar, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      double precision      ,intent(in)    :: scalar          ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_dd2cc          (scalar, string, nchar, ndec)
      call stringlist_put_string (obj%str, string)
      obj%nature  = PARAMETER_SCALAR
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_put_dscalar


      subroutine parameter_put_cscalar (obj, scalar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: scalar          ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      string = scalar
      call stringlist_put_string (obj%str, string)
      obj%nature  = PARAMETER_SCALAR
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_put_cscalar


      subroutine parameter_put_lscalar (obj, scalar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      logical               ,intent(in)    :: scalar          ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ll2cc          (scalar, string)
      call stringlist_put_string (obj%str, string)
      obj%nature  = PARAMETER_SCALAR
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_put_lscalar


      subroutine parameter_put_gscalar (obj, scalar, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      type(grid_struct)      ,intent(in)   :: scalar          ! argument
      integer,optional       ,intent(in)   :: nchar           ! argument
      integer,optional       ,intent(in)   :: ndec            ! argument
      double precision                     :: array(6)        ! local

      call grid_get_origin (scalar, array(1),array(2))
      call grid_get_dx     (scalar, array(3),array(4),array(5),array(6))
      call parameter_put_darray (obj, array, 6, nchar, ndec)
      obj%vartype = PARAMETER_GRID
      return
      end subroutine parameter_put_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_add_ielement (obj, element, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ii2cc          (element, string, nchar)
      call stringlist_add_string (obj%str, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_add_ielement


      subroutine parameter_add_felement (obj, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ff2cc          (element, string, nchar, ndec)
      call stringlist_add_string (obj%str, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_add_felement


      subroutine parameter_add_delement (obj, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_dd2cc          (element, string, nchar, ndec)
      call stringlist_add_string (obj%str, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_add_delement


      subroutine parameter_add_celement (obj, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: element         ! argument

      call stringlist_add_string (obj%str, element)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_add_celement


      subroutine parameter_add_lelement (obj, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      logical               ,intent(in)    :: element         ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ll2cc          (element, string)
      call stringlist_add_string (obj%str, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_add_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_insert_ielement (obj, indx, element, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ii2cc             (element, string, nchar)
      call stringlist_insert_string (obj%str, indx, string)
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_insert_ielement


      subroutine parameter_insert_felement (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ff2cc             (element, string, nchar, ndec)
      call stringlist_insert_string (obj%str, indx, string)
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_insert_felement


      subroutine parameter_insert_delement (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_dd2cc             (element, string, nchar, ndec)
      call stringlist_insert_string (obj%str, indx, string)
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_insert_delement


      subroutine parameter_insert_celement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      string = element
      call stringlist_insert_string (obj%str, indx, string)
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_insert_celement


      subroutine parameter_insert_lelement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical               ,intent(in)    :: element         ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ll2cc             (element, string)
      call stringlist_insert_string (obj%str, indx, string)
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_insert_lelement


      subroutine parameter_insert_from_buffer (obj, indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument

      if (obj%nature /= PARAMETER_ARRAY) return
      call stringlist_insert_string (obj%str, indx)
      return
      end subroutine parameter_insert_from_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_remove_element (obj, indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument

      if (obj%nature /= PARAMETER_ARRAY) return
      call stringlist_remove_string (obj%str, indx)
      return
      end subroutine parameter_remove_element


      subroutine parameter_clear_buffer (obj)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument

      if (obj%nature /= PARAMETER_ARRAY) return
      call stringlist_clear_buffer (obj%str)
      return
      end subroutine parameter_clear_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_replace_ielement (obj, indx, element, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ii2cc              (element, string, nchar)
      call stringlist_replace_string (obj%str, indx, string)
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_replace_ielement


      subroutine parameter_replace_felement (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ff2cc              (element, string, nchar, ndec)
      call stringlist_replace_string (obj%str, indx, string)
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_replace_felement


      subroutine parameter_replace_delement (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_dd2cc              (element, string, nchar, ndec)
      call stringlist_replace_string (obj%str, indx, string)
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_replace_delement


      subroutine parameter_replace_celement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      character(len=*)      ,intent(in)    :: element         ! argument

      if (obj%nature /= PARAMETER_ARRAY) return
      call stringlist_replace_string (obj%str, indx, element)
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_replace_celement


      subroutine parameter_replace_lelement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical               ,intent(in)    :: element         ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      if (obj%nature /= PARAMETER_ARRAY) return
      call string_ll2cc              (element, string)
      call stringlist_replace_string (obj%str, indx, string)
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_replace_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parameter_replace_add_ielement (obj, indx, element, nchar)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ii2cc                  (element, string, nchar)
      call stringlist_replace_add_string (obj%str, indx, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_INTEGER
      return
      end subroutine parameter_replace_add_ielement


      subroutine parameter_replace_add_felement &
                                         (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ff2cc                  (element, string, nchar, ndec)
      call stringlist_replace_add_string (obj%str, indx, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_FLOAT
      return
      end subroutine parameter_replace_add_felement


      subroutine parameter_replace_add_delement &
                                         (obj, indx, element, nchar, ndec)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_dd2cc                  (element, string, nchar, ndec)
      call stringlist_replace_add_string (obj%str, indx, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_DOUBLE
      return
      end subroutine parameter_replace_add_delement


      subroutine parameter_replace_add_celement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      character(len=*)      ,intent(in)    :: element         ! argument

      call stringlist_replace_add_string (obj%str, indx, element)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_STRING
      return
      end subroutine parameter_replace_add_celement


      subroutine parameter_replace_add_lelement (obj, indx, element)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical               ,intent(in)    :: element         ! argument
      character(len=PARAMETER_LENGTH)      :: string          ! local

      call string_ll2cc                  (element, string)
      call stringlist_replace_add_string (obj%str, indx, string)
      obj%nature  = PARAMETER_ARRAY
      obj%vartype = PARAMETER_LOGICAL
      return
      end subroutine parameter_replace_add_lelement


!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!


      function parameter_iscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      integer                            :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_iscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element /= scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_iscalar_matches



      function parameter_fscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      real                               :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_fscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element /= scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_fscalar_matches



      function parameter_dscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      double precision                   :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_dscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element /= scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_dscalar_matches



      function parameter_cscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      character(len=PARAMETER_LENGTH)    :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_cscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element /= scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_cscalar_matches



      function parameter_lscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      logical                            :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_lscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element .neqv. scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_lscalar_matches



      function parameter_gscalar_matches (obj, scalar) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      type(grid_struct)     ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(grid_struct)                  :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      call parameter_get_gscalar (obj, element, errmsg)
      if (errmsg /= ' ') then
           matches = .false.
      else if (element /= scalar) then
           matches = .false.
      else
           matches = .true.
      end if
      return
      end function parameter_gscalar_matches


!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!


      function parameter_iarray_matches (obj, array, nelements) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      integer                            :: indx            ! local
      integer                            :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (nelements /= parameter_num_elements(obj)) then
           matches = .false.
      else
           matches = .true.
           do indx = 1,nelements
                call parameter_cc2ii (obj, indx, element, errmsg)
                if (errmsg /= ' ') then
                     matches = .false.
                     exit
                else if (element /= array(indx)) then
                     matches = .false.
                     exit
                end if
           end do
      end if
      return
      end function parameter_iarray_matches



      function parameter_farray_matches (obj, array, nelements) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      integer                            :: indx            ! local
      real                               :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (nelements /= parameter_num_elements(obj)) then
           matches = .false.
      else
           matches = .true.
           do indx = 1,nelements
                call parameter_cc2ff (obj, indx, element, errmsg)
                if (errmsg /= ' ') then
                     matches = .false.
                     exit
                else if (element /= array(indx)) then
                     matches = .false.
                     exit
                end if
           end do
      end if
      return
      end function parameter_farray_matches



      function parameter_darray_matches (obj, array, nelements) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      integer                            :: indx            ! local
      double precision                   :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (nelements /= parameter_num_elements(obj)) then
           matches = .false.
      else
           matches = .true.
           do indx = 1,nelements
                call parameter_cc2dd (obj, indx, element, errmsg)
                if (errmsg /= ' ') then
                     matches = .false.
                     exit
                else if (element /= array(indx)) then
                     matches = .false.
                     exit
                end if
           end do
      end if
      return
      end function parameter_darray_matches



      function parameter_carray_matches (obj, array, nelements) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      integer                            :: indx            ! local
      character(len=PARAMETER_LENGTH)    :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (nelements /= parameter_num_elements(obj)) then
           matches = .false.
      else
           matches = .true.
           do indx = 1,nelements
                call stringlist_get_string (obj%str, indx, element, errmsg)
!!!!!!          errmsg = parameter_fix_errmsg (obj, errmsg)      ! not needed.
                if (errmsg /= ' ') then
                     matches = .false.
                     exit
                else if (element /= array(indx)) then
                     matches = .false.
                     exit
                end if
           end do
      end if
      return
      end function parameter_carray_matches



      function parameter_larray_matches (obj, array, nelements) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      integer                            :: indx            ! local
      logical                            :: element         ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (nelements /= parameter_num_elements(obj)) then
           matches = .false.
      else
           matches = .true.
           do indx = 1,nelements
                call parameter_cc2ll (obj, indx, element, errmsg)
                if (errmsg /= ' ') then
                     matches = .false.
                     exit
                else if (element .neqv. array(indx)) then
                     matches = .false.
                     exit
                end if
           end do
      end if
      return
      end function parameter_larray_matches


!!------------------ to test for matching array element ---------------------!!
!!------------------ to test for matching array element ---------------------!!
!!------------------ to test for matching array element ---------------------!!


      function parameter_ielement_matches &
                                    (obj, element, indx) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      integer                            :: nelements       ! local
      integer                            :: element2        ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      nelements = parameter_num_elements(obj)
      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (indx < 1 .or. indx > nelements) then
           matches = .false.
      else
           call parameter_cc2ii (obj, indx, element2, errmsg)
           if (errmsg /= ' ') then
                matches = .false.
           else if (element2 /= element) then
                matches = .false.
           else
                matches = .true.
           end if
      end if
      return
      end function parameter_ielement_matches



      function parameter_felement_matches &
                                    (obj, element, indx) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      integer                            :: nelements       ! local
      real                               :: element2        ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      nelements = parameter_num_elements(obj)
      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (indx < 1 .or. indx > nelements) then
           matches = .false.
      else
           call parameter_cc2ff (obj, indx, element2, errmsg)
           if (errmsg /= ' ') then
                matches = .false.
           else if (element2 /= element) then
                matches = .false.
           else
                matches = .true.
           end if
      end if
      return
      end function parameter_felement_matches



      function parameter_delement_matches &
                                    (obj, element, indx) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      integer                            :: nelements       ! local
      double precision                   :: element2        ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      nelements = parameter_num_elements(obj)
      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (indx < 1 .or. indx > nelements) then
           matches = .false.
      else
           call parameter_cc2dd (obj, indx, element2, errmsg)
           if (errmsg /= ' ') then
                matches = .false.
           else if (element2 /= element) then
                matches = .false.
           else
                matches = .true.
           end if
      end if
      return
      end function parameter_delement_matches



      function parameter_celement_matches &
                                    (obj, element, indx) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      integer                            :: nelements       ! local
      character(len=PARAMETER_LENGTH)    :: element2        ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      nelements = parameter_num_elements(obj)
      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (indx < 1 .or. indx > nelements) then
           matches = .false.
      else
           call stringlist_get_string (obj%str, indx, element2, errmsg)
!!!!!!     errmsg = parameter_fix_errmsg (obj, errmsg)      ! not needed.
           if (errmsg /= ' ') then
                matches = .false.
           else if (element2 /= element) then
                matches = .false.
           else
                matches = .true.
           end if
      end if
      return
      end function parameter_celement_matches



      function parameter_lelement_matches &
                                    (obj, element, indx) result (matches)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      integer                            :: nelements       ! local
      logical                            :: element2        ! local
      character(len=PARAMETER_LENGTH)    :: errmsg          ! local

      nelements = parameter_num_elements(obj)
      if (obj%nature /= PARAMETER_ARRAY) then
           matches = .false.
      else if (indx < 1 .or. indx > nelements) then
           matches = .false.
      else
           call parameter_cc2ll (obj, indx, element2, errmsg)
           if (errmsg /= ' ') then
                matches = .false.
           else if (element2 .neqv. element) then
                matches = .false.
           else
                matches = .true.
           end if
      end if
      return
      end function parameter_lelement_matches


!!----------------------------- find element --------------------------------!!
!!----------------------------- find element --------------------------------!!
!!----------------------------- find element --------------------------------!!


      function parameter_find_ielement (obj, element) result (indx)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      integer               ,intent(in)  :: element         ! argument
      integer                            :: indx            ! result
      integer                            :: nelements       ! local
      logical                            :: matches         ! local

      nelements = parameter_num_elements(obj)
      do indx = 1,nelements
           matches = parameter_element_matches (obj,element,indx)
           if (matches) return
      end do
      indx = 0
      return
      end function parameter_find_ielement



      function parameter_find_felement (obj, element) result (indx)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      real                  ,intent(in)  :: element         ! argument
      integer                            :: indx            ! result
      integer                            :: nelements       ! local
      logical                            :: matches         ! local

      nelements = parameter_num_elements(obj)
      do indx = 1,nelements
           matches = parameter_element_matches (obj,element,indx)
           if (matches) return
      end do
      indx = 0
      return
      end function parameter_find_felement



      function parameter_find_delement (obj, element) result (indx)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      double precision      ,intent(in)  :: element         ! argument
      integer                            :: indx            ! result
      integer                            :: nelements       ! local
      logical                            :: matches         ! local

      nelements = parameter_num_elements(obj)
      do indx = 1,nelements
           matches = parameter_element_matches (obj,element,indx)
           if (matches) return
      end do
      indx = 0
      return
      end function parameter_find_delement



      function parameter_find_celement (obj, element) result (indx)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: element         ! argument
      integer                            :: indx            ! result
      integer                            :: nelements       ! local
      logical                            :: matches         ! local

      nelements = parameter_num_elements(obj)
      do indx = 1,nelements
           matches = parameter_element_matches (obj,element,indx)
           if (matches) return
      end do
      indx = 0
      return
      end function parameter_find_celement



      function parameter_find_lelement (obj, element) result (indx)
      implicit none
      type(parameter_struct),intent(in)  :: obj             ! argument
      logical               ,intent(in)  :: element         ! argument
      integer                            :: indx            ! result
      integer                            :: nelements       ! local
      logical                            :: matches         ! local

      nelements = parameter_num_elements(obj)
      do indx = 1,nelements
           matches = parameter_element_matches (obj,element,indx)
           if (matches) return
      end do
      indx = 0
      return
      end function parameter_find_lelement


!!------------------------- find or add element ----------------------------!!
!!------------------------- find or add element ----------------------------!!
!!------------------------- find or add element ----------------------------!!


      function parameter_find_or_add_ielement &
                                      (obj,element,nchar) result (indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer                              :: indx            ! result

      indx = parameter_find_element (obj, element)
      if (indx == 0) then
           call parameter_add_element (obj,element,nchar)
           indx = parameter_num_elements(obj)
      end if
      return
      end function parameter_find_or_add_ielement



      function parameter_find_or_add_felement &
                                      (obj,element,nchar,ndec) result (indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result

      indx = parameter_find_element (obj, element)
      if (indx == 0) then
           call parameter_add_element (obj,element,nchar,ndec)
           indx = parameter_num_elements(obj)
      end if
      return
      end function parameter_find_or_add_felement



      function parameter_find_or_add_delement &
                                      (obj,element,nchar,ndec) result (indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result

      indx = parameter_find_element (obj, element)
      if (indx == 0) then
           call parameter_add_element (obj,element,nchar,ndec)
           indx = parameter_num_elements(obj)
      end if
      return
      end function parameter_find_or_add_delement



      function parameter_find_or_add_celement (obj,element) result (indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      indx = parameter_find_element (obj, element)
      if (indx == 0) then
           call parameter_add_element (obj,element)
           indx = parameter_num_elements(obj)
      end if
      return
      end function parameter_find_or_add_celement



      function parameter_find_or_add_lelement (obj,element) result (indx)
      implicit none
      type(parameter_struct),intent(inout) :: obj             ! argument
      logical               ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      indx = parameter_find_element (obj, element)
      if (indx == 0) then
           call parameter_add_element (obj,element)
           indx = parameter_num_elements(obj)
      end if
      return
      end function parameter_find_or_add_lelement


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module parameter_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

