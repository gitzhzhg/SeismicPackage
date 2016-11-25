
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- parlist.f90 ------------------------------!!
!!---------------------------- parlist.f90 ------------------------------!!
!!---------------------------- parlist.f90 ------------------------------!!


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
! Name       : PARLIST
! Category   : character
! Written    : 1999-06-22   by: Tom Stoeckley
! Revised    : 2009-09-23   by: B. Menger
! Maturity   : beta
! Purpose    : Module for storing and retrieving parameters in a list.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module is a repository for a list of parameters.  Each parameter can
! be a scalar data value (a single value) or an array of data values (zero or
! more values).  The data values can be integer, real, double precision,
! logical, or character variables.  Scalar data values can also be type
! (grid_struct).  Each parameter is retained as a string or a list of strings,
! and therefore can be provided or retrieved either as strings or as variables
! of any compatible type. 
!
! Each parameter is identified by a unique keyword which is case-insensitive.
! Any calling program can create any parameter simply by calling an appropriate
! subroutine with a unique keyword.
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
!           TO CREATE AND CLEAR AND DELETE AND COPY THE PARLIST
!
!                    call parlist_create (obj)
!                    call parlist_clear  (obj)
!                    call parlist_delete (obj)
!                    call parlist_copy   (obj1, obj2)
!                                          i     b
!
! type(parlist_struct)      obj = the parlist structure.
!
! PARLIST_CREATE:
!   (1) allocates obj (PASSED AS A POINTER).
!   (2) initializes the contants to contain no parameters.
!
! PARLIST_CLEAR:
!   (1) deletes all parameters.
!
! PARLIST_DELETE:
!   (1) deletes all parameters.
!   (2) deallocates obj (PASSED AS A POINTER).
!
! PARLIST_COPY:
!   (1) copies the contents of OBJ1 to OBJ2.
!   (2) the previous contents of OBJ2 are deleted first.
!
! The nature of each parameter can be one of two possibilities:
!   (1) the parameter can be a SCALAR (a single scalar parameter).
!   (2) the parameter can be an ARRAY (zero or more array elements).
!
! Note that an ARRAY with one element is different from a SCALAR.
!
!-------------------------------------------------------------------------------
!                     TO GET KEYWORD INFORMATION
!
!      nkeys   = parlist_num_keywords          (obj)
!      keyword = parlist_get_keyword           (obj, ikey)
!        o                                             i
!
!        o                                             i
!      ikey    = parlist_find_keyword          (obj, keyword)
!      ikey    = parlist_find_keyword_exact    (obj, keyword)
!
!        o                                             i
!      present = parlist_keyword_present       (obj, keyword)
!      present = parlist_keyword_present_exact (obj, keyword)
!
!                                                      i        i
!           call parlist_reset_keyword         (obj, keyword, newword)
!           call parlist_reset_keyword_exact   (obj, keyword, newword)
!
!                                                      i
!           call parlist_remove_keyword        (obj, keyword)
!           call parlist_remove_keyword_exact  (obj, keyword)
!
! type(parlist_struct)              obj = the parlist structure.
! integer                         nkeys = number of keywords.
! integer                          ikey = index of desired keyword.
! character(len=*)              keyword = keyword of the desired parameter.
! logical                       present = whether the keyword is present.
! character(len=*)              newword = new keyword to replace old keyword.
!
! The keyword is converted to upper case when input.
! The keyword matching is done in upper case.
! KEYWORD is always returned in upper case.
! KEYWORD is returned as blank if IKEY is out of range.
! IKEY    is returned as zero  if KEYWORD is not found.
! NEWWORD is not used if KEYWORD is not present.
!
! The routines with the _EXACT suffix use KEYWORD exactly as it is, and do
! not convert it to upper case.  These routines can be used if the keyword is
! already upper case when input (for efficiency reasons).
!
!-------------------------------------------------------------------------------
!                    TO ACCESS PARAMETER VALUES
!
!                o                                    i
!            nelements = parlist_num_elements (obj, keyword)
!            nelements = parlist_num_elements (obj,  ikey  )
!            nature    = parlist_nature       (obj, keyword)
!            nature    = parlist_nature       (obj,  ikey  )
!            vartype   = parlist_vartype      (obj, keyword)
!            vartype   = parlist_vartype      (obj,  ikey  )
!
!                                        i        o          o         o
!   call parlist_alloc_array     (obj, keyword, parray,   nelements, errmsg)
!   call parlist_alloc_array (@) (obj,  ikey  , parray,   nelements, errmsg)
!   call parlist_get_array       (obj, keyword, array,    nelements, errmsg)
!   call parlist_get_array   (@) (obj,  ikey  , array,    nelements, errmsg)
!
!                                        i       i       o         o
!   call parlist_get_scalar      (obj, keyword,       scalar,    errmsg)
!   call parlist_get_scalar  (@) (obj,  ikey  ,       scalar,    errmsg)
!   call parlist_get_element     (obj, keyword, indx, element,   errmsg)
!   call parlist_get_element (@) (obj,  ikey  , indx, element,   errmsg)
!
!                                                                  opt   opt
!                                        i        i       i         i     i
!   call parlist_put_array       (obj, keyword, array, nelements, nchar, ndec)
!
!                                                                  opt  opt
!                                              i       i    i       i    i
!   call parlist_put_scalar             (obj,keyword,     scalar, nchar,ndec)
!   call parlist_add_element            (obj,keyword,     element,nchar,ndec)
!   call parlist_insert_element         (obj,keyword,indx,element,nchar,ndec)
!   call parlist_insert_element         (obj,keyword,indx                   )
!   call parlist_remove_element         (obj,keyword,indx                   )
!   call parlist_replace_element        (obj,keyword,indx,element,nchar,ndec)
!   call parlist_replace_or_add_element (obj,keyword,indx,element,nchar,ndec)
!   call parlist_clear_buffer           (obj,keyword)
!
!          o                                      i       i         i
!      matches = parlist_array_matches   (obj, keyword, array,  nelements)
!      matches = parlist_scalar_matches  (obj, keyword, scalar)
!      matches = parlist_element_matches (obj, keyword, element, indx)
!
!         indx = parlist_find_element        (obj,keyword,element)
!         indx = parlist_find_or_add_element (obj,keyword,element,nchar,ndec)
!           o                                        i       i      i    i
!                                                                  opt  opt
!
! type(parlist_struct)        obj = the parlist structure.
! character(len=*)        keyword = keyword of the desired parameter.
! integer                    ikey = index of desired keyword.
! integer               nelements = number of array elements.
! integer                  nature = nature of the parameter.
! integer                 vartype = variable type of the parameter.
! integer                    indx = index of desired array element.
! (any type)               scalar = single scalar parameter value.
! (any type)              element = individual array element.
! (any type)             array(:) = array of parameter values.
! (any type),pointer    parray(:) = pointer array of parameter values.
! character(len=*)         errmsg = non-blank if an error occurs.
! integer,      optional    nchar = maximum number of characters to encode.
! integer,      optional     ndec = maximum number of decimals to encode.
! logical                 matches = true if argument matches the parameter.
!
! The type of ARRAY and SCALAR and ELEMENT can be real, integer, double
! precision, logical, or character(len=*).  The type of SCALAR can also
! be type(grid_struct).  Character variables exceeding length PARLIST_LENGTH
! will be truncated.
!
! NOTE (@): The versions of the following routines which take the IKEY argument
! require PARRAY, ARRAY, and SCALAR to be type character only:
!        parlist_alloc_array 
!        parlist_get_array 
!        parlist_get_scalar
!        parlist_get_element
!
!      | PARLIST_LENGTH is a named constant with the same value |
!      | as STRINGLIST_LENGTH (currently 160 as of 2000-01-28)  |
!      | in the STRINGLIST primitive.                           |
!
! Since the parameter values are stored internally as character strings,
! the type does not have to match from one subroutine call to another.
! Logical values are stored internally as the strings 'YES' or 'NO'.
! Nil values are stored internally as a blank string.
! Nil values are defined in the NAMED_CONSTANTS module.
!
! The keyword is converted to upper case when input.
! The keyword matching is done in upper case.
!
! The NCHAR argument is for integer, real, and double precision variables only.
! The NDEC argument is for real and double precision variables only.
! The NCHAR and NDEC arguments are both used for type(grid_struct).
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
! If an error occurs because the keyword is not found, the error message
! will begin with the characters 'keyword '.
!
! PARLIST_NUM_ELEMENTS:
!   (1) returns number of elements (0 or more) if the parameter is an ARRAY.
!   (2) returns              1                 if the parameter is a SCALAR.
!   (3) returns              0                 if KEYWORD is not found.
!
! PARLIST_NATURE:
!   (1) returns named constant PARLIST_ARRAY   if the parameter is an ARRAY.
!   (2) returns named constant PARLIST_SCALAR  if the parameter is a SCALAR.
!   (3) returns named constant PARLIST_MISSING if KEYWORD is not found.
!
! PARLIST_ALLOC_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) PARRAY is deallocated and reallocated to contain all elements.
!          (PARRAY must be nullified or allocated before first use.)
!          (PARRAY should be deallocated after last use.)
!   (3) PARRAY is always reallocated to at least one array element, even if
!        NELEMENTS is set to zero or an error occurs.
!   (4) an error occurs if the parameter is a scalar.
!   (5) an error occurs if any element cannot be decoded into the desired type.
!   (6) an error occurs if KEYWORD is not found.
!   (7) does not reset PARRAY or NELEMENTS if an error occurs.
!
! PARLIST_GET_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) an error occurs if ARRAY is dimensioned too small for all elements.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if any element cannot be decoded into the desired type.
!   (5) an error occurs if KEYWORD is not found.
!   (6) does not reset ARRAY or NELEMENTS if an error occurs.
!
! PARLIST_GET_SCALAR:
!   (1) gets the requested scalar value in the parameter object.
!   (2) an error occurs if the parameter is an array.
!   (3) an error occurs if the scalar cannot be decoded into the desired type.
!   (4) an error occurs if KEYWORD is not found.
!   (5) does not reset SCALAR if an error occurs.
!
! PARLIST_GET_ELEMENT:
!   (1) gets the requested array element in the parameter object.
!   (2) an error occurs if IELEMENT is out of range.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if the element cannot be decoded into the desired type.
!   (5) an error occurs if KEYWORD is not found.
!   (6) does not reset ELEMENT if an error occurs.
!
! PARLIST_PUT_ARRAY:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces the previous contents with an array with 0 or more elements.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) sets the variable type of the parameter.
!
! PARLIST_PUT_SCALAR:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces the previous contents with a scalar.
!   (3) sets the nature of the parameter to be a SCALAR.
!   (4) sets the variable type of the parameter.
!
! PARLIST_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) appends one element to the previous contents regardless of its nature.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) sets the variable type of the parameter.
!
! PARLIST_INSERT_ELEMENT:
!   (1) inserts the specified element at the specified INDEX.
!   (2) inserts element from buffer if argument ELEMENT is missing.
!   (3) does nothing if KEYWORD is not found.
!   (4) does nothing if INDEX is out of range.
!   (5) does nothing if the nature of the parameter is not an ARRAY.
!   (6) sets the variable type of the parameter.
!
! PARLIST_REMOVE_ELEMENT:
!   (1) removes the element at the specified INDEX.
!   (2) puts removed element into a buffer for possible insertion later.
!   (3) does nothing if KEYWORD is not found.
!   (4) does nothing if INDEX is out of range.
!   (5) does nothing if the nature of the parameter is not an ARRAY.
!
! PARLIST_REPLACE_ELEMENT:
!   (1) replaces a previous element with the specified element.
!   (2) does nothing if KEYWORD is not found.
!   (3) does nothing if INDEX is out of range.
!   (4) does nothing if the nature of the parameter is not an ARRAY.
!   (5) sets the variable type of the parameter.
!
! PARLIST_REPLACE_OR_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces or adds one element to previous contents regardless of nature.
!   (3) does nothing if INDEX is too small.
!   (4) sets the nature of the parameter to be an ARRAY.
!   (5) increases the length of the array if necessary, filling any
!        intermediate array elements with nil.
!   (6) sets the variable type of the parameter.
!
! PARLIST_ARRAY_MATCHES:
!   (1) matches all of the array elements (0 or more) with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is a scalar.
!   (4) returns false if any element cannot be decoded into the desired type.
!   (5) returns false if the number of elements does not match.
!   (6) returns false if any element value does not match.
!   (7) returns true  if all element values match.
!
! PARLIST_SCALAR_MATCHES:
!   (1) matches the requested scalar value with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is an array.
!   (4) returns false if the scalar cannot be decoded into the desired type.
!   (5) returns false if the scalar value does not match.
!   (6) returns true  if the scalar value matches.
!
! PARLIST_ELEMENT_MATCHES:
!   (1) matches the specified array element with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is a scalar.
!   (4) returns false if the element cannot be decoded into the desired type.
!   (5) returns false if the index is out of range.
!   (6) returns false if the element value does not match.
!   (7) returns true  if the element value matches.
!
! PARLIST_FIND_ELEMENT:
!   (1) returns the index of the matching array element.
!   (2) returns zero if KEYWORD is not found or there is no match.
!   (3) returns zero if the parameter is a scalar or an error occurs.
!
! PARLIST_FIND_OR_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) finds the matching element regardless of the nature of the contents.
!   (3) adds the element to the array if there is no match.
!   (4) sets the nature of the parameter to be an ARRAY.
!   (5) returns the index of the matching (or added) array element.
!   (6) sets the variable type of the parameter (if adding).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 18. 2009-09-23  B. Menger  Modified logic for arrays of parameters so that 
!                            in the case of a non-allocated array that was of
!                            0 size (from random memory answer on the size
!                            function), then array was not deallocated before
!                            being allocated.
! 17. 2008-12-11  B. Menger  Nullified variables.
! 16. 2007-09-18  Stoeckley  Add ability to get variable type.
!015. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 14. 2004-03-15  Stoeckley  Fix bug in PARLIST_APPEND_PARAM associated with
!                             copying a zero-length array.
! 13. 2002-02-04  Stoeckley  Add PARLIST_INSERT_FROM_BUFFER,
!                             PARLIST_CLEAR_BUFFER,
!                             PARLIST_REPLACE_OR_ADD_ELEMENT,
!                             PARLIST_ELEMENT_MATCHES, PARLIST_FIND_ELEMENT,
!                             and PARLIST_FIND_OR_ADD_ELEMENT, all to help
!                             support workstation program I/O; make a few
!                             slight improvements in variable names for
!                             consistency.
! 12. 2001-06-11  Stoeckley  Add functions PARLIST_FIND_KEYWORD and
!                             PARLIST_FIND_KEYWORD_EXACT and more routines
!                             which accept the IKEY argument instead of the
!                             KEYWORD argument.
! 11. 2001-04-10  Stoeckley  Change name of private fix_struct to get around
!                             a new sun compiler bug.
! 10. 2000-09-15  Stoeckley  Change PARLIST_FIND_OR_ADD_PARAM and
!                             PARLIST_FIND_PARAM to eliminate second call
!                             to STRING_TO_UPPER; add new routines to accept
!                             the IKEY argument instead of the KEYWORD argument;
!                             add new routines with the suffix _EXACT.
!  9. 2000-08-21  Stoeckley  Change PARLIST_FIND_PARAM to be more efficient
!                             when searching for matching keyword by using
!                             the new primitive HASHLIST.  This speeds up
!                             the SPLT_UPDATE code (with > 100 parameters)
!                             by a factor of 10 (linux) and 6 (solaris).
!  8. 2000-04-04  Stoeckley  Modify logic of PARAMETER_ALLOC_ARRAY calls
!                             to enforce intent(out) for NELEMENTS.
!  7. 2000-01-28  Stoeckley  Increase length of character variables.
!  6. 2000-01-24  Stoeckley  Change PARLIST_ALLOC routines to always
!                             allocate at least one array element.
!  5. 1999-12-29  Stoeckley  Add PARLIST_REMOVE_KEYWORD.
!  4. 1999-11-17  Stoeckley  Add ident string for RCS.
!  3. 1999-09-10  Stoeckley  Minor documentation changes.
!  2. 1999-06-30  Vunderink  Fixed bug in parlist_copy
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
! structures in other modules which use this module.  Since the name of this
! structure was not important, it was changed in order to get around this
! bug.  This turns out to be a known bug but there is no indication about
! whether it will be fixed.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!
!!-------------------------- start of module -----------------------------!!


module parlist_module

  use parameter_module
  use grid_module
  use string_module
  use hashlist_module
  implicit none

  public
  private :: parlist_get_param
  private :: parlist_find_param
  private :: parlist_find_param_exact
  private :: parlist_append_param
  private :: parlist_find_or_add_param
  private :: parlist_find_or_add_param_exact
  private :: parlist_redo_hashlist

      character(len=100),public,save :: PARLIST_IDENT = &
'$Id: parlist.f90,v 1.16 2007/09/19 14:02:24 Stoeckley beta sps $'


!!---------------------- fix for array of pointers ------------------------!!
!!---------------------- fix for array of pointers ------------------------!!
!!---------------------- fix for array of pointers ------------------------!!


  type,private :: parlist_fix_struct

    type(parameter_struct),pointer :: param

  end type parlist_fix_struct


!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!


  integer,parameter,public  :: PARLIST_LENGTH   = PARAMETER_LENGTH
  integer,parameter,public  :: PARLIST_MISSING  = PARAMETER_EMPTY   ! nature.
  integer,parameter,public  :: PARLIST_SCALAR   = PARAMETER_SCALAR  ! nature.
  integer,parameter,public  :: PARLIST_ARRAY    = PARAMETER_ARRAY   ! nature.
  integer,parameter,public  :: PARLIST_INTEGER  = PARAMETER_INTEGER ! vartype.
  integer,parameter,public  :: PARLIST_FLOAT    = PARAMETER_FLOAT   ! vartype.
  integer,parameter,public  :: PARLIST_DOUBLE   = PARAMETER_DOUBLE  ! vartype.
  integer,parameter,public  :: PARLIST_STRING   = PARAMETER_STRING  ! vartype.
  integer,parameter,public  :: PARLIST_LOGICAL  = PARAMETER_LOGICAL ! vartype.
  integer,parameter,public  :: PARLIST_GRID     = PARAMETER_GRID    ! vartype.


  type,public :: parlist_struct

    private
    type(parlist_fix_struct),pointer :: params(:)     ! list of parameters.
    integer                          :: nkeys         ! number of parameters.
    type(hashlist_struct)   ,pointer :: hashlist      ! hash table primitive.

  end type parlist_struct


!!------------------------- interfaces ----------------------------------!!
!!------------------------- interfaces ----------------------------------!!
!!------------------------- interfaces ----------------------------------!!


  interface parlist_num_elements
    module procedure parlist_num_elements1
    module procedure parlist_num_elements2
  end interface

  interface parlist_nature
    module procedure parlist_nature1
    module procedure parlist_nature2
  end interface

  interface parlist_vartype
    module procedure parlist_vartype1
    module procedure parlist_vartype2
  end interface

  interface parlist_alloc_array
    module procedure parlist_alloc_iarray
    module procedure parlist_alloc_farray
    module procedure parlist_alloc_darray
    module procedure parlist_alloc_carray
    module procedure parlist_alloc_carray2
    module procedure parlist_alloc_larray
  end interface

  interface parlist_get_array
    module procedure parlist_get_iarray
    module procedure parlist_get_farray
    module procedure parlist_get_darray
    module procedure parlist_get_carray
    module procedure parlist_get_carray2
    module procedure parlist_get_larray
  end interface

  interface parlist_get_scalar
    module procedure parlist_get_iscalar
    module procedure parlist_get_fscalar
    module procedure parlist_get_dscalar
    module procedure parlist_get_cscalar
    module procedure parlist_get_cscalar2
    module procedure parlist_get_lscalar
    module procedure parlist_get_gscalar
  end interface

  interface parlist_get_element
    module procedure parlist_get_ielement
    module procedure parlist_get_felement
    module procedure parlist_get_delement
    module procedure parlist_get_celement
    module procedure parlist_get_celement2
    module procedure parlist_get_lelement
  end interface

  interface parlist_put_array
    module procedure parlist_put_iarray
    module procedure parlist_put_farray
    module procedure parlist_put_darray
    module procedure parlist_put_carray
    module procedure parlist_put_larray
  end interface

  interface parlist_put_scalar
    module procedure parlist_put_iscalar
    module procedure parlist_put_fscalar
    module procedure parlist_put_dscalar
    module procedure parlist_put_cscalar
    module procedure parlist_put_lscalar
    module procedure parlist_put_gscalar
  end interface

  interface parlist_add_element
    module procedure parlist_add_ielement
    module procedure parlist_add_felement
    module procedure parlist_add_delement
    module procedure parlist_add_celement
    module procedure parlist_add_lelement
  end interface

  interface parlist_insert_element
    module procedure parlist_insert_ielement
    module procedure parlist_insert_felement
    module procedure parlist_insert_delement
    module procedure parlist_insert_celement
    module procedure parlist_insert_lelement
    module procedure parlist_insert_from_buffer
  end interface

  interface parlist_replace_element
    module procedure parlist_replace_ielement
    module procedure parlist_replace_felement
    module procedure parlist_replace_delement
    module procedure parlist_replace_celement
    module procedure parlist_replace_lelement
  end interface

  interface parlist_replace_or_add_element
    module procedure parlist_replace_or_add_ielement
    module procedure parlist_replace_or_add_felement
    module procedure parlist_replace_or_add_delement
    module procedure parlist_replace_or_add_celement
    module procedure parlist_replace_or_add_lelement
  end interface

  interface parlist_scalar_matches
    module procedure parlist_iscalar_matches
    module procedure parlist_fscalar_matches
    module procedure parlist_dscalar_matches
    module procedure parlist_cscalar_matches
    module procedure parlist_lscalar_matches
    module procedure parlist_gscalar_matches
  end interface

  interface parlist_array_matches
    module procedure parlist_iarray_matches
    module procedure parlist_farray_matches
    module procedure parlist_darray_matches
    module procedure parlist_carray_matches
    module procedure parlist_larray_matches
  end interface

  interface parlist_element_matches
    module procedure parlist_ielement_matches
    module procedure parlist_felement_matches
    module procedure parlist_delement_matches
    module procedure parlist_celement_matches
    module procedure parlist_lelement_matches
  end interface

  interface parlist_find_element
    module procedure parlist_find_ielement
    module procedure parlist_find_felement
    module procedure parlist_find_delement
    module procedure parlist_find_celement
    module procedure parlist_find_lelement
  end interface

  interface parlist_find_or_add_element
    module procedure parlist_find_or_add_ielement
    module procedure parlist_find_or_add_felement
    module procedure parlist_find_or_add_delement
    module procedure parlist_find_or_add_celement
    module procedure parlist_find_or_add_lelement
  end interface


!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!
!!------------------------ end of data -----------------------------------!!


contains


!!------------------------- private routines ------------------------------!!
!!------------------------- private routines ------------------------------!!
!!------------------------- private routines ------------------------------!!


      function parlist_get_param (obj, ikey, errmsg) result (param)
      implicit none
      type(parlist_struct)     ,intent(in)  :: obj       ! argument
      integer                  ,intent(in)  :: ikey      ! argument
      character(len=*),optional,intent(out) :: errmsg    ! argument
      type(parameter_struct),pointer        :: param     ! result

      if (ikey >= 1 .and. ikey <= obj%nkeys) then
         param => obj%params(ikey)%param
         if (present(errmsg)) errmsg = ' '
      else
         nullify(param)
         if (present(errmsg)) errmsg = 'index of keyword out of range'
      end if
      return
      end function parlist_get_param


                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function parlist_find_param &
                         (obj, keyword, errmsg, ikey, upper) result (param)
      implicit none
      type(parlist_struct)     ,intent(in)  :: obj       ! argument
      character(len=*)         ,intent(in)  :: keyword   ! argument
      character(len=*),optional,intent(out) :: errmsg    ! argument
      integer         ,optional,intent(out) :: ikey      ! argument
      character(len=*),optional,intent(out) :: upper     ! argument
      type(parameter_struct),pointer        :: param     ! result
      character(len=PARLIST_LENGTH)         :: upper2    ! local

      call string_to_upper (keyword, upper2)
      param => parlist_find_param_exact (obj, upper2, errmsg, ikey)
      if (present(upper)) upper = upper2
      return
      end function parlist_find_param



      function parlist_find_param_exact &
                            (obj, keyword, errmsg, ikey) result (param)
      implicit none
      type(parlist_struct)     ,intent(in)  :: obj       ! argument
      character(len=*)         ,intent(in)  :: keyword   ! argument
      character(len=*),optional,intent(out) :: errmsg    ! argument
      integer         ,optional,intent(out) :: ikey      ! argument
      type(parameter_struct),pointer        :: param     ! result
      integer                               :: ikey2     ! local

      call hashlist_find_keyword (obj%hashlist, keyword, ikey2)
      if (ikey2 >= 1 .and. ikey2 <= obj%nkeys) then
         param => obj%params(ikey2)%param
         if (present(errmsg)) errmsg = ' '
         if (present(ikey)) ikey = ikey2
      else
         nullify(param)
         if (present(errmsg)) errmsg = 'keyword '//trim(keyword)//' not found'
         if (present(ikey)) ikey = 0
      end if
      return
      end function parlist_find_param_exact


                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function parlist_append_param (obj) result (param)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      type(parameter_struct),pointer     :: param              ! result
      type(parlist_fix_struct)           :: temp(obj%nkeys)    ! local

      nullify(param)
      call parameter_create (param)
      if (obj%nkeys > 0) temp = obj%params(1:obj%nkeys)
      if(associated(obj%params)) deallocate (obj%params)
      allocate (obj%params(obj%nkeys + 1))
      if (obj%nkeys > 0) obj%params(1:obj%nkeys) = temp
      obj%nkeys = obj%nkeys + 1
      obj%params(obj%nkeys)%param => param
      return
      end function parlist_append_param


                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function parlist_find_or_add_param (obj, keyword) result (param)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      type(parameter_struct),pointer     :: param           ! result
      character(len=PARLIST_LENGTH)      :: upper           ! local

      call string_to_upper (keyword, upper)
      nullify(param)
      param => parlist_find_or_add_param_exact (obj, upper)
      return
      end function parlist_find_or_add_param



      function parlist_find_or_add_param_exact (obj, keyword) result (param)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      type(parameter_struct),pointer     :: param           ! result

      nullify(param)
      param => parlist_find_param_exact (obj, keyword)
      if (associated(param)) return
      param => parlist_append_param     (obj)
      call parameter_set_keyword_exact  (param, keyword)
      call hashlist_add_keyword         (obj%hashlist, keyword, obj%nkeys)
      return
      end function parlist_find_or_add_param_exact


!!------------------------- private redo hashlist -----------------------!!
!!------------------------- private redo hashlist -----------------------!!
!!------------------------- private redo hashlist -----------------------!!


      subroutine parlist_redo_hashlist (obj)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      integer                            :: ikey            ! local
      type(parameter_struct),pointer     :: param           ! local
      character(len=PARLIST_LENGTH)      :: keyword         ! local

      nullify(param)
      call hashlist_clear (obj%hashlist)
      do ikey = 1,obj%nkeys
           param => obj%params(ikey)%param
           call parameter_get_keyword (param, keyword)
           call hashlist_add_keyword  (obj%hashlist, keyword, ikey)
      end do
      return
      end subroutine parlist_redo_hashlist


!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!


      subroutine parlist_create (obj)
      implicit none
      type(parlist_struct),pointer :: obj         ! argument

      nullify(obj)
      allocate(obj)
      nullify (obj%params)
      nullify (obj%hashlist) ! jpa
      obj%nkeys = 0
      call hashlist_create (obj%hashlist)
      return
      end subroutine parlist_create



      subroutine parlist_delete (obj)
      implicit none
      type(parlist_struct),pointer :: obj         ! argument

      call parlist_clear   (obj)
      call hashlist_delete (obj%hashlist)
      deallocate(obj)
      return
      end subroutine parlist_delete



      subroutine parlist_clear (obj)
      implicit none
      type(parlist_struct),intent(inout) :: obj       ! argument
      integer                            :: ikey      ! local

      do ikey = 1,obj%nkeys
          call parameter_delete (obj%params(ikey)%param)
      end do
      if(associated(obj%params)) deallocate(obj%params)
      obj%nkeys = 0
      call hashlist_clear (obj%hashlist)
      return
      end subroutine parlist_clear



      subroutine parlist_copy (obj1, obj2)
      implicit none
      type(parlist_struct),intent(in)    :: obj1      ! argument
      type(parlist_struct),intent(inout) :: obj2      ! argument
      integer                            :: ikey      ! local

      call parlist_clear (obj2)
      obj2%nkeys = obj1%nkeys
      allocate(obj2%params(obj2%nkeys))
      do ikey = 1,obj2%nkeys
          call parameter_create (obj2%params(ikey)%param)
          call parameter_copy (obj1%params(ikey)%param, obj2%params(ikey)%param)
      end do
      call hashlist_copy (obj1%hashlist, obj2%hashlist)
      return
      end subroutine parlist_copy



!!--------------------- to get keyword information ------------------------!!
!!--------------------- to get keyword information ------------------------!!
!!--------------------- to get keyword information ------------------------!!


      function parlist_num_keywords (obj) result (nkeys)
      implicit none
      type(parlist_struct),intent(in) :: obj               ! argument
      integer                         :: nkeys             ! result

      nkeys = obj%nkeys
      return
      end function parlist_num_keywords



      function parlist_get_keyword (obj, ikey) result (keyword)
      implicit none
      type(parlist_struct),intent(in) :: obj          ! argument
      integer             ,intent(in) :: ikey         ! argument
      character(len=PARLIST_LENGTH)   :: keyword      ! result

      if (ikey < 1 .or. ikey > obj%nkeys) then
           keyword = ' '
      else
           call parameter_get_keyword (obj%params(ikey)%param, keyword)
      end if
      return
      end function parlist_get_keyword


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function parlist_find_keyword (obj, keyword) result (ikey)
      implicit none
      type(parlist_struct),intent(in) :: obj          ! argument
      character(len=*)    ,intent(in) :: keyword      ! argument
      integer                         :: ikey         ! result
      type(parameter_struct),pointer  :: param        ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, ikey = ikey)
      return
      end function parlist_find_keyword



      function parlist_find_keyword_exact (obj, keyword) result (ikey)
      implicit none
      type(parlist_struct),intent(in) :: obj          ! argument
      character(len=*)    ,intent(in) :: keyword      ! argument
      integer                         :: ikey         ! result
      type(parameter_struct),pointer  :: param        ! local


      nullify(param)
      param => parlist_find_param_exact (obj, keyword, ikey = ikey)
      return
      end function parlist_find_keyword_exact


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      function parlist_keyword_present (obj, keyword) result (present)
      implicit none
      type(parlist_struct),intent(in) :: obj          ! argument
      character(len=*)    ,intent(in) :: keyword      ! argument
      logical                         :: present      ! result
      type(parameter_struct),pointer  :: param        ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      present = associated(param)
      return
      end function parlist_keyword_present



      function parlist_keyword_present_exact (obj, keyword) result (present)
      implicit none
      type(parlist_struct),intent(in) :: obj          ! argument
      character(len=*)    ,intent(in) :: keyword      ! argument
      logical                         :: present      ! result
      type(parameter_struct),pointer  :: param        ! local

      nullify(param)
      param => parlist_find_param_exact (obj, keyword)
      present = associated(param)
      return
      end function parlist_keyword_present_exact


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_reset_keyword (obj, keyword, newword)
      implicit none
      type(parlist_struct),intent(inout) :: obj          ! argument
      character(len=*)    ,intent(in)    :: keyword      ! argument
      character(len=*)    ,intent(in)    :: newword      ! argument
      character(len=PARLIST_LENGTH)      :: upper        ! local

      call string_to_upper             (keyword, upper)
      call parlist_reset_keyword_exact (obj, upper, newword)
      return
      end subroutine parlist_reset_keyword



      subroutine parlist_reset_keyword_exact (obj, keyword, newword)
      implicit none
      type(parlist_struct),intent(inout) :: obj          ! argument
      character(len=*)    ,intent(in)    :: keyword      ! argument
      character(len=*)    ,intent(in)    :: newword      ! argument
      type(parameter_struct),pointer     :: param        ! local

      nullify(param)
      param => parlist_find_param_exact (obj, keyword)
      if (.not.associated(param)) return
      call parameter_set_keyword (param, newword)
      call parlist_redo_hashlist (obj)
      return
      end subroutine parlist_reset_keyword_exact


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_remove_keyword (obj, keyword)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      character(len=PARLIST_LENGTH)      :: upper              ! local

      call string_to_upper              (keyword, upper)
      call parlist_remove_keyword_exact (obj, upper)
      return
      end subroutine parlist_remove_keyword



      subroutine parlist_remove_keyword_exact (obj, keyword)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      type(parameter_struct),pointer     :: param              ! local
      integer                            :: ikey               ! local
      type(parlist_fix_struct)           :: temp(obj%nkeys)    ! local

      nullify(param)
      param => parlist_find_param_exact (obj, keyword, ikey=ikey)
      if (.not.associated(param)) return
      temp = obj%params
      if(associated(obj%params)) deallocate (obj%params)
      allocate (obj%params(obj%nkeys - 1))
      if(ikey >         1) obj%params(1:ikey-1)        = temp(1:ikey-1)
      if(ikey < obj%nkeys) obj%params(ikey:obj%nkeys-1)= temp(ikey+1:obj%nkeys)
      obj%nkeys = obj%nkeys - 1
      call parameter_delete (param)
      call parlist_redo_hashlist (obj)
      return
      end subroutine parlist_remove_keyword_exact


!!-------------------- to access parameter values -----------------------!!
!!-------------------- to access parameter values -----------------------!!
!!-------------------- to access parameter values -----------------------!!


      function parlist_num_elements1 (obj, keyword) result (nelements)
      implicit none
      type(parlist_struct),intent(in) :: obj             ! argument
      character(len=*)    ,intent(in) :: keyword         ! argument
      integer                         :: nelements       ! result
      type(parameter_struct),pointer  :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           nelements = parameter_num_elements (param)
      else
           nelements = 0
      end if
      return
      end function parlist_num_elements1



      function parlist_num_elements2 (obj, ikey) result (nelements)
      implicit none
      type(parlist_struct),intent(in) :: obj             ! argument
      integer             ,intent(in) :: ikey            ! argument
      integer                         :: nelements       ! result
      type(parameter_struct),pointer  :: param           ! local

      nullify(param)
      param => parlist_get_param (obj, ikey)
      if (associated(param)) then
           nelements = parameter_num_elements (param)
      else
           nelements = 0
      end if
      return
      end function parlist_num_elements2



      function parlist_nature1 (obj, keyword) result (nature)
      implicit none
      type(parlist_struct),intent(in) :: obj            ! argument
      character(len=*)    ,intent(in) :: keyword        ! argument
      integer                         :: nature         ! result
      type(parameter_struct),pointer  :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           nature = parameter_nature (param)
      else
           nature = PARLIST_MISSING
      end if
      return
      end function parlist_nature1



      function parlist_nature2 (obj, ikey) result (nature)
      implicit none
      type(parlist_struct),intent(in) :: obj            ! argument
      integer             ,intent(in) :: ikey           ! argument
      integer                         :: nature         ! result
      type(parameter_struct),pointer  :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey)
      if (associated(param)) then
           nature = parameter_nature (param)
      else
           nature = PARLIST_MISSING
      end if
      return
      end function parlist_nature2


      function parlist_vartype1 (obj, keyword) result (vartype)
      implicit none
      type(parlist_struct),intent(in) :: obj            ! argument
      character(len=*)    ,intent(in) :: keyword        ! argument
      integer                         :: vartype        ! result
      type(parameter_struct),pointer  :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           vartype = parameter_vartype (param)
      else
           vartype = PARLIST_STRING
      end if
      return
      end function parlist_vartype1



      function parlist_vartype2 (obj, ikey) result (vartype)
      implicit none
      type(parlist_struct),intent(in) :: obj            ! argument
      integer             ,intent(in) :: ikey           ! argument
      integer                         :: vartype        ! result
      type(parameter_struct),pointer  :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey)
      if (associated(param)) then
           vartype = parameter_vartype (param)
      else
           vartype = PARLIST_STRING
      end if
      return
      end function parlist_vartype2


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_alloc_iarray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      integer             ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_alloc_iarray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_iarray



      subroutine parlist_alloc_farray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      real                ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_alloc_farray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_farray



      subroutine parlist_alloc_darray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      double precision    ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_alloc_darray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_darray



      subroutine parlist_alloc_carray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      character(len=*)    ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_alloc_carray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_carray


      subroutine parlist_alloc_carray2 (obj, ikey, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      integer             ,intent(in)           :: ikey           ! argument
      character(len=*)    ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey, errmsg)
      if (associated(param)) then
           call parameter_alloc_carray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_carray2



      subroutine parlist_alloc_larray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      logical             ,pointer              :: parray(:)      ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_alloc_larray (param, parray, nelements, errmsg)
           if (size(parray) == 0) then            
             deallocate(parray)                     
             allocate(parray(1))                    
           end if
      else if (.not.associated(parray)) then      
           allocate(parray(1))                    
      endif
      return
      end subroutine parlist_alloc_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_get_iarray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      integer             ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_iarray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_iarray



      subroutine parlist_get_farray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      real                ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_farray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_farray



      subroutine parlist_get_darray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      double precision    ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_darray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_darray



      subroutine parlist_get_carray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      character(len=*)    ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_carray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_carray



      subroutine parlist_get_carray2 (obj, ikey, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      integer             ,intent(in)           :: ikey           ! argument
      character(len=*)    ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey, errmsg)
      if (associated(param)) then
           call parameter_get_carray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_carray2



      subroutine parlist_get_larray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(parlist_struct),intent(in)           :: obj            ! argument
      character(len=*)    ,intent(in)           :: keyword        ! argument
      logical             ,intent(out)          :: array(:)       ! argument
      integer             ,intent(out)          :: nelements      ! argument
      character(len=*)    ,intent(out)          :: errmsg         ! argument
      type(parameter_struct),pointer            :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_larray (param, array, nelements, errmsg)
      end if
      return
      end subroutine parlist_get_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_get_iscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_iscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_iscalar



      subroutine parlist_get_fscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      real                ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_fscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_fscalar



      subroutine parlist_get_dscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      double precision    ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_dscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_dscalar



      subroutine parlist_get_cscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      character(len=*)    ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_cscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_cscalar



      subroutine parlist_get_cscalar2 (obj, ikey, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      integer             ,intent(in)  :: ikey           ! argument
      character(len=*)    ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey, errmsg)
      if (associated(param)) then
           call parameter_get_cscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_cscalar2



      subroutine parlist_get_lscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      logical             ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_lscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_lscalar



      subroutine parlist_get_gscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      type(grid_struct)   ,intent(out) :: scalar         ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_gscalar (param, scalar, errmsg)
      end if
      return
      end subroutine parlist_get_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_get_ielement (obj, keyword, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(in)  :: indx           ! argument
      integer             ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_ielement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_ielement



      subroutine parlist_get_felement (obj, keyword, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(in)  :: indx           ! argument
      real                ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_felement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_felement



      subroutine parlist_get_delement (obj, keyword, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(in)  :: indx           ! argument
      double precision    ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_delement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_delement



      subroutine parlist_get_celement (obj, keyword, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(in)  :: indx           ! argument
      character(len=*)    ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_celement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_celement



      subroutine parlist_get_celement2 (obj, ikey, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      integer             ,intent(in)  :: ikey           ! argument
      integer             ,intent(in)  :: indx           ! argument
      character(len=*)    ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_get_param (obj, ikey, errmsg)
      if (associated(param)) then
           call parameter_get_celement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_celement2



      subroutine parlist_get_lelement (obj, keyword, indx, element, errmsg)
      implicit none
      type(parlist_struct),intent(in)  :: obj            ! argument
      character(len=*)    ,intent(in)  :: keyword        ! argument
      integer             ,intent(in)  :: indx           ! argument
      logical             ,intent(out) :: element        ! argument
      character(len=*)    ,intent(out) :: errmsg         ! argument
      type(parameter_struct),pointer   :: param          ! local

      nullify(param)
      param => parlist_find_param (obj, keyword, errmsg)
      if (associated(param)) then
           call parameter_get_lelement (param, indx, element, errmsg)
      end if
      return
      end subroutine parlist_get_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_put_iarray (obj, keyword, array, nelements, nchar)
      implicit none
      type(parlist_struct),intent(inout)       :: obj             ! argument
      character(len=*)    ,intent(in)          :: keyword         ! argument
      integer             ,intent(in)          :: nelements       ! argument
      integer             ,intent(in)          :: array(:)        ! argument
      integer             ,intent(in),optional :: nchar           ! argument
      type(parameter_struct),pointer           :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_iarray (param, array, nelements, nchar)
      return
      end subroutine parlist_put_iarray



      subroutine parlist_put_farray (obj, keyword, array,nelements,nchar,ndec)
      implicit none
      type(parlist_struct),intent(inout)       :: obj          ! argument
      character(len=*)    ,intent(in)          :: keyword      ! argument
      integer             ,intent(in)          :: nelements    ! argument
      real                ,intent(in)          :: array(:)     ! argument
      integer             ,intent(in),optional :: nchar        ! argument
      integer             ,intent(in),optional :: ndec         ! argument
      type(parameter_struct),pointer           :: param        ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_farray (param, array, nelements, nchar, ndec)
      return
      end subroutine parlist_put_farray



      subroutine parlist_put_darray (obj, keyword, array,nelements,nchar,ndec)
      implicit none
      type(parlist_struct),intent(inout)       :: obj          ! argument
      character(len=*)    ,intent(in)          :: keyword      ! argument
      integer             ,intent(in)          :: nelements    ! argument
      double precision    ,intent(in)          :: array(:)     ! argument
      integer             ,intent(in),optional :: nchar        ! argument
      integer             ,intent(in),optional :: ndec         ! argument
      type(parameter_struct),pointer           :: param        ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_darray (param, array, nelements, nchar, ndec)
      return
      end subroutine parlist_put_darray



      subroutine parlist_put_carray (obj, keyword, array, nelements)
      implicit none
      type(parlist_struct),intent(inout)       :: obj          ! argument
      character(len=*)    ,intent(in)          :: keyword      ! argument
      integer             ,intent(in)          :: nelements    ! argument
      character(len=*)    ,intent(in)          :: array(:)     ! argument
      type(parameter_struct),pointer           :: param        ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_carray (param, array, nelements)
      return
      end subroutine parlist_put_carray



      subroutine parlist_put_larray (obj, keyword, array, nelements)
      implicit none
      type(parlist_struct),intent(inout)       :: obj          ! argument
      character(len=*)    ,intent(in)          :: keyword      ! argument
      integer             ,intent(in)          :: nelements    ! argument
      logical             ,intent(in)          :: array(:)     ! argument
      type(parameter_struct),pointer           :: param        ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_larray (param, array, nelements)
      return
      end subroutine parlist_put_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_put_iscalar (obj, keyword, scalar, nchar)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      integer             ,intent(in)    :: scalar             ! argument
      integer,optional    ,intent(in)    :: nchar              ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_iscalar (param, scalar, nchar)
      return
      end subroutine parlist_put_iscalar



      subroutine parlist_put_fscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      real                ,intent(in)    :: scalar             ! argument
      integer,optional    ,intent(in)    :: nchar              ! argument
      integer,optional    ,intent(in)    :: ndec               ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_fscalar (param, scalar, nchar, ndec)
      return
      end subroutine parlist_put_fscalar



      subroutine parlist_put_dscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      double precision    ,intent(in)    :: scalar             ! argument
      integer,optional    ,intent(in)    :: nchar              ! argument
      integer,optional    ,intent(in)    :: ndec               ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_dscalar (param, scalar, nchar, ndec)
      return
      end subroutine parlist_put_dscalar



      subroutine parlist_put_cscalar (obj, keyword, scalar)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      character(len=*)    ,intent(in)    :: scalar             ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_cscalar (param, scalar)
      return
      end subroutine parlist_put_cscalar



      subroutine parlist_put_lscalar (obj, keyword, scalar)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      logical             ,intent(in)    :: scalar             ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_lscalar (param, scalar)
      return
      end subroutine parlist_put_lscalar



      subroutine parlist_put_gscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj                ! argument
      character(len=*)    ,intent(in)    :: keyword            ! argument
      type(grid_struct)   ,intent(in)    :: scalar             ! argument
      integer,optional    ,intent(in)    :: nchar              ! argument
      integer,optional    ,intent(in)    :: ndec               ! argument
      type(parameter_struct),pointer     :: param              ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_put_gscalar (param, scalar, nchar, ndec)
      return
      end subroutine parlist_put_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_add_ielement (obj, keyword, element, nchar)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_add_ielement (param, element, nchar)
      return
      end subroutine parlist_add_ielement



      subroutine parlist_add_felement (obj, keyword, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      real                ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_add_felement (param, element, nchar, ndec)
      return
      end subroutine parlist_add_felement



      subroutine parlist_add_delement (obj, keyword, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      double precision    ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_add_delement (param, element, nchar, ndec)
      return
      end subroutine parlist_add_delement



      subroutine parlist_add_celement (obj, keyword, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      character(len=*)    ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_add_celement (param, element)
      return
      end subroutine parlist_add_celement



      subroutine parlist_add_lelement (obj, keyword, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      logical             ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param (obj, keyword)
      call parameter_add_lelement (param, element)
      return
      end subroutine parlist_add_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_insert_ielement (obj, keyword, indx, element, nchar)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      integer             ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_ielement (param, indx, element, nchar)
      end if
      return
      end subroutine parlist_insert_ielement



      subroutine parlist_insert_felement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      real                ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_felement (param, indx, element, nchar, ndec)
      end if
      return
      end subroutine parlist_insert_felement



      subroutine parlist_insert_delement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      double precision    ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_delement (param, indx, element, nchar, ndec)
      end if
      return
      end subroutine parlist_insert_delement



      subroutine parlist_insert_celement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      character(len=*)    ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_celement (param, indx, element)
      end if
      return
      end subroutine parlist_insert_celement



      subroutine parlist_insert_lelement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      logical             ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_lelement (param, indx, element)
      end if
      return
      end subroutine parlist_insert_lelement



      subroutine parlist_insert_from_buffer (obj, keyword, indx)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_insert_from_buffer (param, indx)
      end if
      return
      end subroutine parlist_insert_from_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_remove_element (obj, keyword, indx)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_remove_element (param, indx)
      end if
      return
      end subroutine parlist_remove_element



      subroutine parlist_clear_buffer (obj, keyword)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_clear_buffer (param)
      end if
      return
      end subroutine parlist_clear_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_replace_ielement (obj, keyword, indx, element, nchar)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      integer             ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_replace_ielement (param, indx, element, nchar)
      end if
      return
      end subroutine parlist_replace_ielement



      subroutine parlist_replace_felement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      real                ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_replace_felement (param, indx, element, nchar, ndec)
      end if
      return
      end subroutine parlist_replace_felement



      subroutine parlist_replace_delement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      double precision    ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_replace_delement (param, indx, element, nchar, ndec)
      end if
      return
      end subroutine parlist_replace_delement



      subroutine parlist_replace_celement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      character(len=*)    ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_replace_celement (param, indx, element)
      end if
      return
      end subroutine parlist_replace_celement



      subroutine parlist_replace_lelement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      logical             ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           call parameter_replace_lelement (param, indx, element)
      end if
      return
      end subroutine parlist_replace_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine parlist_replace_or_add_ielement &
                                       (obj, keyword, indx, element, nchar)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      integer             ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param  (obj, keyword)
      call parameter_replace_add_ielement (param, indx, element, nchar)
      return
      end subroutine parlist_replace_or_add_ielement



      subroutine parlist_replace_or_add_felement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      real                ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param  (obj, keyword)
      call parameter_replace_add_felement (param, indx, element, nchar, ndec)
      return
      end subroutine parlist_replace_or_add_felement



      subroutine parlist_replace_or_add_delement  &
                                 (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      double precision    ,intent(in)    :: element         ! argument
      integer,optional    ,intent(in)    :: nchar           ! argument
      integer,optional    ,intent(in)    :: ndec            ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param  (obj, keyword)
      call parameter_replace_add_delement (param, indx, element, nchar, ndec)
      return
      end subroutine parlist_replace_or_add_delement



      subroutine parlist_replace_or_add_celement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      character(len=*)    ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param  (obj, keyword)
      call parameter_replace_add_celement (param, indx, element)
      return
      end subroutine parlist_replace_or_add_celement



      subroutine parlist_replace_or_add_lelement (obj, keyword, indx, element)
      implicit none
      type(parlist_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer             ,intent(in)    :: indx            ! argument
      logical             ,intent(in)    :: element         ! argument
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param  (obj, keyword)
      call parameter_replace_add_lelement (param, indx, element)
      return
      end subroutine parlist_replace_or_add_lelement


!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!


      function parlist_iscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      integer               ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_iscalar_matches



      function parlist_fscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      real                  ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_fscalar_matches



      function parlist_dscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      double precision      ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_dscalar_matches



      function parlist_cscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      character(len=*)      ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_cscalar_matches



      function parlist_lscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      logical               ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_lscalar_matches



      function parlist_gscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      type(grid_struct)     ,intent(in)  :: scalar          ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_scalar_matches (param, scalar)
      else
           matches = .false.
      end if
      return
      end function parlist_gscalar_matches


!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!


      function parlist_iarray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      integer               ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_array_matches (param, array, nelements)
      else
           matches = .false.
      end if
      return
      end function parlist_iarray_matches



      function parlist_farray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      real                  ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_array_matches (param, array, nelements)
      else
           matches = .false.
      end if
      return
      end function parlist_farray_matches



      function parlist_darray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      double precision      ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_array_matches (param, array, nelements)
      else
           matches = .false.
      end if
      return
      end function parlist_darray_matches



      function parlist_carray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      character(len=*)      ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_array_matches (param, array, nelements)
      else
           matches = .false.
      end if
      return
      end function parlist_carray_matches



      function parlist_larray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      logical               ,intent(in)  :: array(:)        ! argument
      integer               ,intent(in)  :: nelements       ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_array_matches (param, array, nelements)
      else
           matches = .false.
      end if
      return
      end function parlist_larray_matches


!!------------------ to test for matching array element ------------------!!
!!------------------ to test for matching array element ------------------!!
!!------------------ to test for matching array element ------------------!!


      function parlist_ielement_matches  &
                       (obj, keyword, element, indx) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      integer               ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_element_matches (param, element, indx)
      else
           matches = .false.
      end if
      return
      end function parlist_ielement_matches



      function parlist_felement_matches  &
                       (obj, keyword, element, indx) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      real                  ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_element_matches (param, element, indx)
      else
           matches = .false.
      end if
      return
      end function parlist_felement_matches



      function parlist_delement_matches  &
                       (obj, keyword, element, indx) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      double precision      ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_element_matches (param, element, indx)
      else
           matches = .false.
      end if
      return
      end function parlist_delement_matches



      function parlist_celement_matches  &
                       (obj, keyword, element, indx) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      character(len=*)      ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_element_matches (param, element, indx)
      else
           matches = .false.
      end if
      return
      end function parlist_celement_matches



      function parlist_lelement_matches  &
                       (obj, keyword, element, indx) result (matches)
      implicit none
      type(parlist_struct)  ,intent(in)  :: obj             ! argument
      character(len=*)      ,intent(in)  :: keyword         ! argument
      logical               ,intent(in)  :: element         ! argument
      integer               ,intent(in)  :: indx            ! argument
      logical                            :: matches         ! result
      type(parameter_struct),pointer     :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           matches = parameter_element_matches (param, element, indx)
      else
           matches = .false.
      end if
      return
      end function parlist_lelement_matches


!!-------------------------------- find element ----------------------------!!
!!-------------------------------- find element ----------------------------!!
!!-------------------------------- find element ----------------------------!!


      function parlist_find_ielement (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      integer               ,intent(in) :: element         ! argument
      integer                           :: indx            ! result
      type(parameter_struct),pointer    :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           indx = parameter_find_element (param, element)
      else
           indx = 0
      end if
      return
      end function parlist_find_ielement



      function parlist_find_felement (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      real                  ,intent(in) :: element         ! argument
      integer                           :: indx            ! result
      type(parameter_struct),pointer    :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           indx = parameter_find_element (param, element)
      else
           indx = 0
      end if
      return
      end function parlist_find_felement



      function parlist_find_delement (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      double precision      ,intent(in) :: element         ! argument
      integer                           :: indx            ! result
      type(parameter_struct),pointer    :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           indx = parameter_find_element (param, element)
      else
           indx = 0
      end if
      return
      end function parlist_find_delement



      function parlist_find_celement (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      character(len=*)      ,intent(in) :: element         ! argument
      integer                           :: indx            ! result
      type(parameter_struct),pointer    :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           indx = parameter_find_element (param, element)
      else
           indx = 0
      end if
      return
      end function parlist_find_celement



      function parlist_find_lelement (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(in) :: obj             ! argument
      character(len=*)      ,intent(in) :: keyword         ! argument
      logical               ,intent(in) :: element         ! argument
      integer                           :: indx            ! result
      type(parameter_struct),pointer    :: param           ! local

      nullify(param)
      param => parlist_find_param (obj, keyword)
      if (associated(param)) then
           indx = parameter_find_element (param, element)
      else
           indx = 0
      end if
      return
      end function parlist_find_lelement


!!------------------------- find or add element ----------------------------!!
!!------------------------- find or add element ----------------------------!!
!!------------------------- find or add element ----------------------------!!


      function parlist_find_or_add_ielement &
                                   (obj, keyword, element, nchar) result (indx)
      implicit none
      type(parlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer                              :: indx            ! result
      type(parameter_struct),pointer       :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param   (obj, keyword)
      indx = parameter_find_or_add_element (param, element, nchar)
      return
      end function parlist_find_or_add_ielement



      function parlist_find_or_add_felement &
                            (obj, keyword, element, nchar, ndec) result (indx)
      implicit none
      type(parlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result
      type(parameter_struct),pointer       :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param   (obj, keyword)
      indx = parameter_find_or_add_element (param, element, nchar, ndec)
      return
      end function parlist_find_or_add_felement



      function parlist_find_or_add_delement &
                            (obj, keyword, element, nchar, ndec) result (indx)
      implicit none
      type(parlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result
      type(parameter_struct),pointer       :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param   (obj, keyword)
      indx = parameter_find_or_add_element (param, element, nchar, ndec)
      return
      end function parlist_find_or_add_delement



      function parlist_find_or_add_celement &
                                   (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result
      type(parameter_struct),pointer       :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param   (obj, keyword)
      indx = parameter_find_or_add_element (param, element)
      return
      end function parlist_find_or_add_celement



      function parlist_find_or_add_lelement &
                                   (obj, keyword, element) result (indx)
      implicit none
      type(parlist_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result
      type(parameter_struct),pointer       :: param           ! local

      nullify(param)
      param => parlist_find_or_add_param   (obj, keyword)
      indx = parameter_find_or_add_element (param, element)
      return
      end function parlist_find_or_add_lelement


!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!
!!-------------------------- end of module -------------------------------!!


end module parlist_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

