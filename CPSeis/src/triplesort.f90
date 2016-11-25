!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- triplesort.f90 --------------------------------!!
!!--------------------------- triplesort.f90 --------------------------------!!
!!--------------------------- triplesort.f90 --------------------------------!!


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
! Name       : TRIPLESORT 
! Category   : sorts
! Written    : 2000-05-12   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Sort an array of structures containing three integers.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive sorts an array of structures containing three integers.
! This primitive also does binary searches within a sorted array of these
! integer structures.
!
! For convenience (but not needed by this module), structures containing
! three floats and three doubles and three logicals are also provided.
!
! The structures are defined as follows:
!
!     type,public :: triplesort_ints
!       integer :: primary            ! primary sort key  (highest priority).
!       integer :: secondary          ! secondary sort key.
!       integer :: tertiary           ! tertiary sort key  (lowest priority).
!     end type triplesort_ints
!
!     type,public :: triplesort_floats
!       real    :: primary
!       real    :: secondary 
!       real    :: tertiary 
!     end type triplesort_floats
!
!     type,public :: triplesort_doubles
!       double precision :: primary
!       double precision :: secondary 
!       double precision :: tertiary 
!     end type triplesort_doubles
!
!     type,public :: triplesort_bools
!       logical :: primary
!       logical :: secondary 
!       logical :: tertiary 
!     end type triplesort_bools
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
! To sort an array of keys into ascending order:
!
!                                     b     i 
!      call triplesort_sort         (keys,nkeys)
!      call triplesort_sort (indices,keys,nkeys)
!                              b      i     i  
!
! To search for a range of matching keys within an array of keys:
!
!      call triplesort_search (nkeys,keys,keywant,level,   index1,index2)
!                                i    i     i       i        o      o
!
! To search for bracketing keys within an array of keys:
!
!      call triplesort_bracket (nkeys,keys,keywant,   indx1,indx2)
!                                 i    i     i          o     o
!
! To compare two keys:
!
!      compare = triplesort_compare (key1,key2,level)
!      compare = triplesort_compare (val1,val2,level)
!      compare = triplesort_compare (dbl1,dbl2,level)
!         o                           i    i     i
!
! Overloaded operators and intrinsics (all using implied level = 3):
!
!              key = key1         ! no special overloaded functions required.
!              val = val1         ! no special overloaded functions required.
!              dbl = dbl1         ! no special overloaded functions required.
!              boo = boo1         ! no special overloaded functions required.
!
!              key = iscalar      ! sets all components to same scalar value.
!              val = fscalar      ! sets all components to same scalar value.
!              dbl = dscalar      ! sets all components to same scalar value.
!              boo = lscalar      ! sets all components to same scalar value.
!
!              key = iarray       ! sets 3 components to 3 array values.
!              val = farray       ! sets 3 components to 3 array values.
!              dbl = darray       ! sets 3 components to 3 array values.
!              boo = larray       ! sets 3 components to 3 array values.
!
!   (key1 == key2)   (key1 /= key2)   ! equality means all components match.
!   (val1 == val2)   (val1 /= val2)   ! equality means all components match.
!   (dbl1 == dbl2)   (dbl1 /= dbl2)   ! equality means all components match.
!   (boo1 == boo2)   (boo1 /= boo2)   ! equality means all components match.
!
!   (key1 <  key2)   (key1 >  key2)   ! compares components in priority order.
!   (key1 <= key2)   (key1 >= key2)   ! compares components in priority order.
!   (val1 <  val2)   (val1 >  val2)   ! compares components in priority order.
!   (val1 <= val2)   (val1 >= val2)   ! compares components in priority order.
!   (dbl1 <  dbl2)   (dbl1 >  dbl2)   ! compares components in priority order.
!   (dbl1 <= dbl2)   (dbl1 >= dbl2)   ! compares components in priority order.
!
!   (key == iscalar) (key /= iscalar) ! equality means all components match.
!   (val == fscalar) (val /= fscalar) ! equality means all components match.
!   (dbl == dscalar) (dbl /= dscalar) ! equality means all components match.
!   (boo == lscalar) (boo /= lscalar) ! equality means all components match.
!
!   (key <  iscalar) (key >  iscalar) ! compares components in priority order.
!   (key <= iscalar) (key >= iscalar) ! compares components in priority order.
!   (val <  fscalar) (val >  fscalar) ! compares components in priority order.
!   (val <= fscalar) (val >= fscalar) ! compares components in priority order.
!   (dbl <  dscalar) (dbl >  dscalar) ! compares components in priority order.
!   (dbl <= dscalar) (dbl >= dscalar) ! compares components in priority order.
!
!          keymin = min (key1,key2)
!          keymax = max (key1,key2)
!            o            i    i
!
!          iloc   = minloc (keys,nkeys,mask)
!          keymin = minval (keys,nkeys,mask)
!            o               i     i    i
!                                      opt
!
! To get minimum and maximum values of each structure component separately:
!
!          keymin = triplesort_minima (key1,key2)
!          keymax = triplesort_maxima (key1,key2)
!          valmin = triplesort_minima (val1,val2)
!          valmax = triplesort_maxima (val1,val2)
!          dblmin = triplesort_minima (dbl1,dbl2)
!          dblmax = triplesort_maxima (dbl1,dbl2)
!            o                          i    i
!
! To convert to and from bin numbers for each structure component:
!
!          bin = triplesort_binning   (val,vinit,vinc)
!          bin = triplesort_binning   (dbl,dinit,dinc)
!          val = triplesort_unbinning (bin,vinit,vinc)
!          dbl = triplesort_unbinning (bin,dinit,dinc)
!           o                           i    i    i
!
! integer               indices(nkeys)   = indices into array of keys to sort.
! type(triplesort_ints)    keys(nkeys)   = array of keys to sort or search.
! integer                  nkeys         = number of keys in the array.
! integer                  level         = level of comparison (1 or 2 or 3).
! type(triplesort_ints)    keywant       = key to search for.
! type(triplesort_ints)    key,key1,key2 = structure with 3 components.
! type(triplesort_floats)  val,val1,val2 = structure with 3 components.
! type(triplesort_doubles) dbl,dbl1,dbl2 = structure with 3 components.
! type(triplesort_bools)   boo,boo1,boo2 = structure with 3 components.
! integer                  index1,index2 = first and last index of matching key.
! integer                  indx1,indx2   = indices of bracketing keys.
! integer                  compare       = result of comparison.
! logical                  mask(nkeys)   = mask to use for search within KEYS.
! integer                  iloc          = index of minimum value in KEYS.
! type(triplesort_ints)    keymin,keymax = minimum and maximum value(s).
! type(triplesort_floats)  valmin,valmax = and maximum minimum value(s).
! type(triplesort_doubles) dblmin,dblmax = and maximum minimum value(s).
! type(triplesort_ints)    bin           = bin numbers = nint((val-vinit)/vinc)
! type(triplesort_ints)    bin           = bin numbers = nint((dbl-dinit)/dinc)
! type(triplesort_floats)  vinit         = initial coordinate values.
! type(triplesort_floats)  vinc          = coordinate bin widths and increments.
! type(triplesort_doubles) dinit         = initial coordinate values.
! type(triplesort_doubles) dinc          = coordinate bin widths and increments.
! integer                  iscalar       = scalar value.
! real                     fscalar       = scalar value.
! double precision         dscalar       = scalar value.
! logical                  lscalar       = scalar value.
! integer                  iarray(3)     = three values.
! real                     farray(3)     = three values.
! double precision         darray(3)     = three values.
! logical                  larray(3)     = three values.
!
! LEVEL <= 1 uses only the primary sort key for comparing keys.
! LEVEL == 2 uses the primary and secondary sort keys.
! LEVEL >= 3 uses all three sort keys (default).
! Two keys are considered to match if they match down to the specified level.
!
! TRIPLESORT_SORT:
!
!  Does an n-log-n sort of the keys in place.
!  Uses all three sort keys.
!  The routine without the INDICES argument sorts the KEYS.  The sorted
!   order of KEYS then is in the order of KEYS(i) where i=1,nkeys.
!  The routine with the INDICES argument rearranges the INDICES instead of
!   sorting the KEYS.  The sorted order of KEYS then is in the order of
!   KEYS(INDICES(i)) where i=1,nkeys.  INDICES must be initialized to the
!   values 1,...,nkeys.
!
! TRIPLESORT_SEARCH:
!
!  Does a binary search of the sort keys.
!  Given array of keys, and the desired key KEYWANT, returns first and last
!    index of matching keys: 1 <= INDEX1 <= INDEX2 <= NKEYS.
!  The array of keys must be in ascending order.
!  If two or more adjacent matching keys are found, INDEX1 will be
!    the index of the first match and INDEX2 will be the index of the
!    last match.
!  If there is only one matching key, returns INDEX1 = INDEX2 = matching index.
!  If there is no matching key, returns INDEX1 = INDEX2 = 0.
!
! TRIPLESORT_BRACKET:
!
!  Does a binary search of the sort keys.
!  Given array of keys, and the desired key KEYWANT, returns indices which
!    bracket KEYWANT: 1 <= INDX1 <= INDX2 <= NKEYS.
!  The array of keys must be in ascending order.
!  INDX1 = INDX2 = 0     are returned if NKEYS is zero.
!  INDX1 = INDX2 = 1     are returned if KEYWANT <= KEYS(1).
!  INDX1 = INDX2 = NKEYS are returned if KEYWANT >= KEYS(NKEYS).
!  INDX1 = INDX2 = i     are returned if KEYWANT exactly matches KEYS(i).
!  otherwise INDX2 = INDX1 + 1 and both are in range (1:NKEYS).
!
! TRIPLESORT_COMPARE:
!
!  Returns COMPARE = -1  if  KEY1 <  KEY2.
!  Returns COMPARE =  0  if  KEY1 == KEY2.
!  Returns COMPARE =  1  if  KEY1 >  KEY2.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  8. 2006-06-20  B. Menger  Removed Unused Variables.
!  7. 2003-12-09  Stoeckley  Add double precision option.
!  6. 2002-08-01  Stoeckley  Change triplesort_binning and triplesort_unbinning
!                             to call the MTH module.
!  5. 2002-02-04  Stoeckley  Add TRIPLESORT_BRACKET.
!  4. 2001-04-04  Stoeckley  Fix bug in triplesort_eq_bools.
!  3. 2001-03-20  Stoeckley  Add overloaded operators and intrinsics; add
!                             the LEVEL subroutine argument.
!  2. 2000-10-06  Stoeckley  Add missing required documentation section.
!  1. 2000-05-12  Stoeckley  Initial version.
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
! Some of the overloaded operators and intrinsics in this primitive are
! adapted from Mike O'Brien's work in the version of the TSORT process which
! he converted from the old CPS.
!
! The TRIPLESORT_SORT_INDICES routine is adapted from Mike O'Brien's
! TSORT_SHELLISORT_THREE_INTS in the version of the TSORT process which
! he converted from the old CPS.  Mike's work was in turn adapted from
! Hale, D., 1989, SeisUnix seismic processing software library.  Hale's
! work was adapted from Sedgewick, R., 1983, Algorithms, Addison Wesley,
! p. 96.
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module triplesort_module
      use mth_module
      implicit none
      public

      character(len=100),public,save :: TRIPLESORT_IDENT = &
'$Id: triplesort.f90,v 1.8 2006/06/20 13:12:12 Menger prod sps $'

      integer,private,parameter :: LEVEL3 = 3

      type,public :: triplesort_ints
        integer :: primary
        integer :: secondary
        integer :: tertiary
      end type triplesort_ints

      type,public :: triplesort_floats
        real    :: primary
        real    :: secondary
        real    :: tertiary
      end type triplesort_floats

      type,public :: triplesort_doubles
        double precision :: primary
        double precision :: secondary
        double precision :: tertiary
      end type triplesort_doubles

      type,public :: triplesort_bools
        logical :: primary
        logical :: secondary
        logical :: tertiary
      end type triplesort_bools


      interface triplesort_compare
        module procedure triplesort_compare_ints
        module procedure triplesort_compare_floats
        module procedure triplesort_compare_doubles
      end interface

      interface triplesort_binning
        module procedure triplesort_binning_floats
        module procedure triplesort_binning_doubles
      end interface

      interface triplesort_unbinning
        module procedure triplesort_unbinning_floats
        module procedure triplesort_unbinning_doubles
      end interface

      interface assignment (=)
        module procedure triplesort_assign_ints
        module procedure triplesort_assign_floats
        module procedure triplesort_assign_doubles
        module procedure triplesort_assign_bools
        module procedure triplesort_assign_array_ints
        module procedure triplesort_assign_array_floats
        module procedure triplesort_assign_array_doubles
        module procedure triplesort_assign_array_bools
      end interface

      interface operator (==)
        module procedure triplesort_eq_ints
        module procedure triplesort_eq_floats
        module procedure triplesort_eq_doubles
        module procedure triplesort_eq_bools
        module procedure triplesort_eq_scalar_ints
        module procedure triplesort_eq_scalar_floats
        module procedure triplesort_eq_scalar_doubles
        module procedure triplesort_eq_scalar_bools
      end interface

      interface operator (/=)
        module procedure triplesort_ne_ints
        module procedure triplesort_ne_floats
        module procedure triplesort_ne_doubles
        module procedure triplesort_ne_bools
        module procedure triplesort_ne_scalar_ints
        module procedure triplesort_ne_scalar_floats
        module procedure triplesort_ne_scalar_doubles
        module procedure triplesort_ne_scalar_bools
      end interface

      interface operator (<)
        module procedure triplesort_lt_ints
        module procedure triplesort_lt_floats
        module procedure triplesort_lt_doubles
        module procedure triplesort_lt_scalar_ints
        module procedure triplesort_lt_scalar_floats
        module procedure triplesort_lt_scalar_doubles
      end interface

      interface operator (>)
        module procedure triplesort_gt_ints
        module procedure triplesort_gt_floats
        module procedure triplesort_gt_doubles
        module procedure triplesort_gt_scalar_ints
        module procedure triplesort_gt_scalar_floats
        module procedure triplesort_gt_scalar_doubles
      end interface

      interface operator (<=)
        module procedure triplesort_le_ints
        module procedure triplesort_le_floats
        module procedure triplesort_le_doubles
        module procedure triplesort_le_scalar_ints
        module procedure triplesort_le_scalar_floats
        module procedure triplesort_le_scalar_doubles
      end interface

      interface operator (>=)
        module procedure triplesort_ge_ints
        module procedure triplesort_ge_floats
        module procedure triplesort_ge_doubles
        module procedure triplesort_ge_scalar_ints
        module procedure triplesort_ge_scalar_floats
        module procedure triplesort_ge_scalar_doubles
      end interface

      interface min
        module procedure triplesort_min
      end interface

      interface max
        module procedure triplesort_max
      end interface

      interface triplesort_minima
        module procedure triplesort_minima_ints
        module procedure triplesort_minima_floats
        module procedure triplesort_minima_doubles
      end interface

      interface triplesort_maxima
        module procedure triplesort_maxima_ints
        module procedure triplesort_maxima_floats
        module procedure triplesort_maxima_doubles
      end interface

      interface minval
        module procedure triplesort_minval
      end interface

      interface minloc
        module procedure triplesort_minloc
      end interface

      interface triplesort_sort
        module procedure triplesort_sort_keys
        module procedure triplesort_sort_indices
      end interface


      contains


!!------------------------- triplesort sort keys ---------------------------!!
!!------------------------- triplesort sort keys ---------------------------!!
!!------------------------- triplesort sort keys ---------------------------!!

 
      subroutine triplesort_sort_keys (keys,nkeys)
      implicit none
      integer              ,intent(in)    :: nkeys           ! arguments
      type(triplesort_ints),intent(inout) :: keys(nkeys)     ! arguments
      type(triplesort_ints)               :: key             ! local
      integer                             :: istride,k,j     ! local
      integer                             :: indx1,indx2     ! local

      istride = nkeys
      do
           istride = istride/2
           if (istride < 1) exit
           k = nkeys - istride
           do j = 1, k
                indx1 = j
                do
                     indx2 = indx1 + istride
                     if (keys(indx1) <= keys(indx2)) exit

                     key         = keys(indx1)        ! switch
                     keys(indx1) = keys(indx2)        ! switch
                     keys(indx2) = key                ! switch

                     if (indx1 <= istride) exit
                     indx1 = indx1 - istride
                end do
           end do
      end do
      return
      end subroutine triplesort_sort_keys


!!------------------------- triplesort sort indices ------------------------!!
!!------------------------- triplesort sort indices ------------------------!!
!!------------------------- triplesort sort indices ------------------------!!

 
      subroutine triplesort_sort_indices (indices,keys,nkeys)
      implicit none
      integer              ,intent(in)    :: nkeys           ! arguments
      integer              ,intent(inout) :: indices(nkeys)  ! arguments
      type(triplesort_ints),intent(in)    :: keys(nkeys)     ! arguments

      integer                             :: istride,k,j     ! local
      integer                             :: indx1,indx2     ! local

      istride = nkeys
      do
           istride = istride/2
           if (istride < 1) exit
           do j = istride+1,nkeys
                indx2 = indices(j)
                k     = j
                do
                     if (k-istride < 1) exit
                     indx1 = indices(k-istride)
                     if (keys(indx1) <= keys(indx2)) exit
                     indices(k) = indices(k-istride)
                     k          = k - istride
                end do
                indices(k) = indx2
           end do
      end do
      return
      end subroutine triplesort_sort_indices


!!----------------------- triplesort compare ----------------------------!!
!!----------------------- triplesort compare ----------------------------!!
!!----------------------- triplesort compare ----------------------------!!


      function triplesort_compare_ints (key1,key2,level) result (compare)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2         ! arguments
      integer              ,intent(in) :: level             ! arguments
      integer                          :: compare           ! result

      if      (key1%primary   < key2%primary  ) then ; compare = -1
      else if (key1%primary   > key2%primary  ) then ; compare =  1
      else if (                  level <= 1   ) then ; compare =  0
      else if (key1%secondary < key2%secondary) then ; compare = -1
      else if (key1%secondary > key2%secondary) then ; compare =  1
      else if (                  level == 2   ) then ; compare =  0
      else if (key1%tertiary  < key2%tertiary ) then ; compare = -1
      else if (key1%tertiary  > key2%tertiary ) then ; compare =  1
      else                                           ; compare =  0
      end if
      return
      end function triplesort_compare_ints



      function triplesort_compare_floats (key1,key2,level) result (compare)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2         ! arguments
      integer                ,intent(in) :: level             ! arguments
      integer                            :: compare           ! result

      if      (key1%primary   < key2%primary  ) then ; compare = -1
      else if (key1%primary   > key2%primary  ) then ; compare =  1
      else if (                  level <= 1   ) then ; compare =  0
      else if (key1%secondary < key2%secondary) then ; compare = -1
      else if (key1%secondary > key2%secondary) then ; compare =  1
      else if (                  level == 2   ) then ; compare =  0
      else if (key1%tertiary  < key2%tertiary ) then ; compare = -1
      else if (key1%tertiary  > key2%tertiary ) then ; compare =  1
      else                                           ; compare =  0
      end if
      return
      end function triplesort_compare_floats



      function triplesort_compare_doubles (key1,key2,level) result (compare)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2         ! arguments
      integer                 ,intent(in) :: level             ! arguments
      integer                             :: compare           ! result

      if      (key1%primary   < key2%primary  ) then ; compare = -1
      else if (key1%primary   > key2%primary  ) then ; compare =  1
      else if (                  level <= 1   ) then ; compare =  0
      else if (key1%secondary < key2%secondary) then ; compare = -1
      else if (key1%secondary > key2%secondary) then ; compare =  1
      else if (                  level == 2   ) then ; compare =  0
      else if (key1%tertiary  < key2%tertiary ) then ; compare = -1
      else if (key1%tertiary  > key2%tertiary ) then ; compare =  1
      else                                           ; compare =  0
      end if
      return
      end function triplesort_compare_doubles


!!------------------------ triplesort search ------------------------------!!
!!------------------------ triplesort search ------------------------------!!
!!------------------------ triplesort search ------------------------------!!


      subroutine triplesort_search (nkeys,keys,keywant,level,  index1,index2)
      implicit none
      integer              ,intent(in)  :: nkeys                 ! arguments
      integer              ,intent(in)  :: level                 ! arguments
      type(triplesort_ints),intent(in)  :: keys(:),keywant       ! arguments
      integer              ,intent(out) :: index1,index2         ! arguments
      integer            :: index3,compare1,compare2,compare3    ! local

      if (nkeys == 0) then
           index1 = 0
           index2 = 0
           return
      end if

      index1 = 1
      index2 = nkeys

      compare1 = triplesort_compare (keywant,keys(index1),level)
      compare2 = triplesort_compare (keywant,keys(index2),level)

      if (compare1 < 0 .or. compare2 > 0) then
           index1 = 0
           index2 = 0
           return
      end if

      do
           if (compare1 == 0) then
                index2   = index1
                compare2 = compare1
                exit
           else if (compare2 == 0) then
                index1   = index2
                compare1 = compare2
                exit
           else if (index2 - index1 <= 1) then
                index1 = 0
                index2 = 0
                return
           end if

           index3 = (index1 + index2) / 2

           compare3 = triplesort_compare (keywant,keys(index3),level)
           if (compare3 > 0) then
                index1   = index3
                compare1 = compare3
                cycle
           else if (compare3 < 0) then
                index2   = index3
                compare2 = compare3
                cycle
           end if

           index1   = index3
           index2   = index3
           compare1 = compare3
           compare2 = compare3
           exit
      end do

      do
           if (index1 == 1) exit
           compare1 = triplesort_compare (keywant,keys(index1-1),level)
           if (compare1 /= 0) exit
           index1 = index1 - 1
      end do

      do
           if (index2 == nkeys) exit
           compare2 = triplesort_compare (keywant,keys(index2+1),level)
           if (compare2 /= 0) exit
           index2 = index2 + 1
      end do
      return
      end subroutine triplesort_search


!!------------------------ triplesort bracket ------------------------------!!
!!------------------------ triplesort bracket ------------------------------!!
!!------------------------ triplesort bracket ------------------------------!!


      subroutine triplesort_bracket (keys,nkeys,keywant,indx1,indx2)
      implicit none
      integer              ,intent(in)  :: nkeys                 ! arguments
      type(triplesort_ints),intent(in)  :: keys(:),keywant       ! arguments
      integer              ,intent(out) :: indx1,indx2           ! arguments
      integer                           :: indx3                 ! local

! Test for no points:

      if (nkeys == 0) then
        indx1 = 0
        indx2 = 0
        return
      end if

! Test for extrapolation:

      if (keywant <= keys(1)) then
        indx1 = 1
        indx2 = 1
        return
      end if

      if (keywant >= keys(nkeys)) then
        indx1 = nkeys
        indx2 = nkeys
        return
      end if

! Do a binary search in keys(:) to find indices for values bracketing keywant:

      indx1 = 1
      indx2 = nkeys
      do
        if (indx2 - indx1 <= 1) exit
        indx3 = (indx1 + indx2) / 2
        if(keywant > keys(indx3)) then
          indx1 = indx3
        else
          indx2 = indx3
        end if
      end do
      return
      end subroutine triplesort_bracket


!!----------------------- minima and maxima -------------------------------!!
!!----------------------- minima and maxima -------------------------------!!
!!----------------------- minima and maxima -------------------------------!!


      type(triplesort_ints) function triplesort_minima_ints &
                                            (key1,key2) result (keymin)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      keymin%primary   = min(key1%primary  ,key2%primary  )
      keymin%secondary = min(key1%secondary,key2%secondary)
      keymin%tertiary  = min(key1%tertiary ,key2%tertiary )
      return
      end function triplesort_minima_ints



      type(triplesort_ints) function triplesort_maxima_ints &
                                            (key1,key2) result (keymax)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      keymax%primary   = max(key1%primary  ,key2%primary  )
      keymax%secondary = max(key1%secondary,key2%secondary)
      keymax%tertiary  = max(key1%tertiary ,key2%tertiary )
      return
      end function triplesort_maxima_ints



      type(triplesort_floats) function triplesort_minima_floats &
                                            (val1,val2) result (valmin)
      implicit none
      type(triplesort_floats),intent(in) :: val1,val2

      valmin%primary   = min(val1%primary  ,val2%primary  )
      valmin%secondary = min(val1%secondary,val2%secondary)
      valmin%tertiary  = min(val1%tertiary ,val2%tertiary )
      return
      end function triplesort_minima_floats



      type(triplesort_floats) function triplesort_maxima_floats &
                                            (val1,val2) result (valmax)
      implicit none
      type(triplesort_floats),intent(in) :: val1,val2

      valmax%primary   = max(val1%primary  ,val2%primary  )
      valmax%secondary = max(val1%secondary,val2%secondary)
      valmax%tertiary  = max(val1%tertiary ,val2%tertiary )
      return
      end function triplesort_maxima_floats



      type(triplesort_doubles) function triplesort_minima_doubles &
                                            (dbl1,dbl2) result (dblmin)
      implicit none
      type(triplesort_doubles),intent(in) :: dbl1,dbl2

      dblmin%primary   = min(dbl1%primary  ,dbl2%primary  )
      dblmin%secondary = min(dbl1%secondary,dbl2%secondary)
      dblmin%tertiary  = min(dbl1%tertiary ,dbl2%tertiary )
      return
      end function triplesort_minima_doubles



      type(triplesort_doubles) function triplesort_maxima_doubles &
                                            (dbl1,dbl2) result (dblmax)
      implicit none
      type(triplesort_doubles),intent(in) :: dbl1,dbl2

      dblmax%primary   = max(dbl1%primary  ,dbl2%primary  )
      dblmax%secondary = max(dbl1%secondary,dbl2%secondary)
      dblmax%tertiary  = max(dbl1%tertiary ,dbl2%tertiary )
      return
      end function triplesort_maxima_doubles


!!------------------------ binning and unbinning ----------------------------!!
!!------------------------ binning and unbinning ----------------------------!!
!!------------------------ binning and unbinning ----------------------------!!


      type(triplesort_ints) function triplesort_binning_floats &
                                            (val,vinit,vinc) result (bin)
      implicit none
      type(triplesort_floats),intent(in) :: val,vinit,vinc

   bin%primary   = mth_bin_number(vinit%primary  ,vinc%primary  ,val%primary  )
   bin%secondary = mth_bin_number(vinit%secondary,vinc%secondary,val%secondary)
   bin%tertiary  = mth_bin_number(vinit%tertiary ,vinc%tertiary ,val%tertiary )
      return
      end function triplesort_binning_floats



      type(triplesort_ints) function triplesort_binning_doubles &
                                            (dbl,dinit,dinc) result (bin)
      implicit none
      type(triplesort_doubles),intent(in) :: dbl,dinit,dinc

   bin%primary   = mth_bin_number(dinit%primary  ,dinc%primary  ,dbl%primary  )
   bin%secondary = mth_bin_number(dinit%secondary,dinc%secondary,dbl%secondary)
   bin%tertiary  = mth_bin_number(dinit%tertiary ,dinc%tertiary ,dbl%tertiary )
      return
      end function triplesort_binning_doubles



      type(triplesort_floats) function triplesort_unbinning_floats &
                                            (bin,vinit,vinc) result (val)
      implicit none
      type(triplesort_ints  ),intent(in) :: bin
      type(triplesort_floats),intent(in) :: vinit,vinc

   val%primary   = mth_bin_center(vinit%primary  ,vinc%primary  ,bin%primary  )
   val%secondary = mth_bin_center(vinit%secondary,vinc%secondary,bin%secondary)
   val%tertiary  = mth_bin_center(vinit%tertiary ,vinc%tertiary ,bin%tertiary )
      return
      end function triplesort_unbinning_floats



      type(triplesort_doubles) function triplesort_unbinning_doubles &
                                            (bin,dinit,dinc) result (dbl)
      implicit none
      type(triplesort_ints   ),intent(in) :: bin
      type(triplesort_doubles),intent(in) :: dinit,dinc

   dbl%primary   = mth_bin_center(dinit%primary  ,dinc%primary  ,bin%primary  )
   dbl%secondary = mth_bin_center(dinit%secondary,dinc%secondary,bin%secondary)
   dbl%tertiary  = mth_bin_center(dinit%tertiary ,dinc%tertiary ,bin%tertiary )
      return
      end function triplesort_unbinning_doubles


!!----------------------- overloaded assignments ----------------------------!!
!!----------------------- overloaded assignments ----------------------------!!
!!----------------------- overloaded assignments ----------------------------!!


      subroutine triplesort_assign_ints (key,scalar)
      implicit none
      type(triplesort_ints),intent(inout) :: key
      integer              ,intent(in)    :: scalar

      key%primary   = scalar
      key%secondary = scalar
      key%tertiary  = scalar
      return
      end subroutine triplesort_assign_ints



      subroutine triplesort_assign_floats (key,scalar)
      implicit none
      type(triplesort_floats),intent(inout) :: key
      real                   ,intent(in)    :: scalar

      key%primary   = scalar
      key%secondary = scalar
      key%tertiary  = scalar
      return
      end subroutine triplesort_assign_floats



      subroutine triplesort_assign_doubles (key,scalar)
      implicit none
      type(triplesort_doubles),intent(inout) :: key
      double precision        ,intent(in)    :: scalar

      key%primary   = scalar
      key%secondary = scalar
      key%tertiary  = scalar
      return
      end subroutine triplesort_assign_doubles



      subroutine triplesort_assign_bools (key,scalar)
      implicit none
      type(triplesort_bools),intent(inout) :: key
      logical               ,intent(in)    :: scalar

      key%primary   = scalar
      key%secondary = scalar
      key%tertiary  = scalar
      return
      end subroutine triplesort_assign_bools


                             !!!!!!!!!!!!!!!!!!!!


      subroutine triplesort_assign_array_ints (key,array)
      implicit none
      type(triplesort_ints),intent(inout) :: key
      integer              ,intent(in)    :: array(3)

      key%primary   = array(1)
      key%secondary = array(2)
      key%tertiary  = array(3)
      return
      end subroutine triplesort_assign_array_ints



      subroutine triplesort_assign_array_floats (key,array)
      implicit none
      type(triplesort_floats),intent(inout) :: key
      real                   ,intent(in)    :: array(3)

      key%primary   = array(1)
      key%secondary = array(2)
      key%tertiary  = array(3)
      return
      end subroutine triplesort_assign_array_floats



      subroutine triplesort_assign_array_doubles (key,array)
      implicit none
      type(triplesort_doubles),intent(inout) :: key
      double precision        ,intent(in)    :: array(3)

      key%primary   = array(1)
      key%secondary = array(2)
      key%tertiary  = array(3)
      return
      end subroutine triplesort_assign_array_doubles



      subroutine triplesort_assign_array_bools (key,array)
      implicit none
      type(triplesort_bools),intent(inout) :: key
      logical               ,intent(in)    :: array(3)

      key%primary   = array(1)
      key%secondary = array(2)
      key%tertiary  = array(3)
      return
      end subroutine triplesort_assign_array_bools


!!----------------------- overloaded operators ------------------------------!!
!!----------------------- overloaded operators ------------------------------!!
!!----------------------- overloaded operators ------------------------------!!


      logical function triplesort_eq_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_eq_ints = (triplesort_compare(key1,key2,LEVEL3) == 0)
      return
      end function triplesort_eq_ints



      logical function triplesort_ne_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_ne_ints = (triplesort_compare(key1,key2,LEVEL3) /= 0)
      return
      end function triplesort_ne_ints



      logical function triplesort_lt_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_lt_ints = (triplesort_compare(key1,key2,LEVEL3) < 0)
      return
      end function triplesort_lt_ints



      logical function triplesort_gt_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_gt_ints = (triplesort_compare(key1,key2,LEVEL3) > 0)
      return
      end function triplesort_gt_ints



      logical function triplesort_le_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_le_ints = (triplesort_compare(key1,key2,LEVEL3) <= 0)
      return
      end function triplesort_le_ints



      logical function triplesort_ge_ints (key1,key2)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      triplesort_ge_ints = (triplesort_compare(key1,key2,LEVEL3) >= 0)
      return
      end function triplesort_ge_ints


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_eq_floats = (triplesort_compare(key1,key2,LEVEL3) == 0)
      return
      end function triplesort_eq_floats



      logical function triplesort_ne_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_ne_floats = (triplesort_compare(key1,key2,LEVEL3) /= 0)
      return
      end function triplesort_ne_floats



      logical function triplesort_lt_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_lt_floats = (triplesort_compare(key1,key2,LEVEL3) < 0)
      return
      end function triplesort_lt_floats



      logical function triplesort_gt_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_gt_floats = (triplesort_compare(key1,key2,LEVEL3) > 0)
      return
      end function triplesort_gt_floats



      logical function triplesort_le_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_le_floats = (triplesort_compare(key1,key2,LEVEL3) <= 0)
      return
      end function triplesort_le_floats



      logical function triplesort_ge_floats (key1,key2)
      implicit none
      type(triplesort_floats),intent(in) :: key1,key2

      triplesort_ge_floats = (triplesort_compare(key1,key2,LEVEL3) >= 0)
      return
      end function triplesort_ge_floats


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_eq_doubles = (triplesort_compare(key1,key2,LEVEL3) == 0)
      return
      end function triplesort_eq_doubles



      logical function triplesort_ne_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_ne_doubles = (triplesort_compare(key1,key2,LEVEL3) /= 0)
      return
      end function triplesort_ne_doubles



      logical function triplesort_lt_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_lt_doubles = (triplesort_compare(key1,key2,LEVEL3) < 0)
      return
      end function triplesort_lt_doubles



      logical function triplesort_gt_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_gt_doubles = (triplesort_compare(key1,key2,LEVEL3) > 0)
      return
      end function triplesort_gt_doubles



      logical function triplesort_le_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_le_doubles = (triplesort_compare(key1,key2,LEVEL3) <= 0)
      return
      end function triplesort_le_doubles



      logical function triplesort_ge_doubles (key1,key2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1,key2

      triplesort_ge_doubles = (triplesort_compare(key1,key2,LEVEL3) >= 0)
      return
      end function triplesort_ge_doubles


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_bools (key1,key2)
      implicit none
      type(triplesort_bools),intent(in) :: key1,key2

      triplesort_eq_bools = ( (key1%primary   .eqv. key2%primary  )  .and. &
                              (key1%secondary .eqv. key2%secondary)  .and. &
                              (key1%tertiary  .eqv. key2%tertiary ) )
      return
      end function triplesort_eq_bools



      logical function triplesort_ne_bools (key1,key2)
      implicit none
      type(triplesort_bools),intent(in) :: key1,key2

      triplesort_ne_bools = (.not.triplesort_eq_bools(key1,key2))
      return
      end function triplesort_ne_bools


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_eq_scalar_ints = (key1 == key2)
      return
      end function triplesort_eq_scalar_ints



      logical function triplesort_ne_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_ne_scalar_ints = (key1 /= key2)
      return
      end function triplesort_ne_scalar_ints



      logical function triplesort_lt_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_lt_scalar_ints = (key1 < key2)
      return
      end function triplesort_lt_scalar_ints



      logical function triplesort_gt_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_gt_scalar_ints = (key1 > key2)
      return
      end function triplesort_gt_scalar_ints



      logical function triplesort_le_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_le_scalar_ints = (key1 <= key2)
      return
      end function triplesort_le_scalar_ints



      logical function triplesort_ge_scalar_ints (key1,scalar2)
      implicit none
      type(triplesort_ints),intent(in) :: key1
      integer              ,intent(in) :: scalar2
      type(triplesort_ints)            :: key2

      key2 = scalar2
      triplesort_ge_scalar_ints = (key1 >= key2)
      return
      end function triplesort_ge_scalar_ints


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_eq_scalar_floats = (key1 == key2)
      return
      end function triplesort_eq_scalar_floats



      logical function triplesort_ne_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_ne_scalar_floats = (key1 /= key2)
      return
      end function triplesort_ne_scalar_floats



      logical function triplesort_lt_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_lt_scalar_floats = (key1 < key2)
      return
      end function triplesort_lt_scalar_floats



      logical function triplesort_gt_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_gt_scalar_floats = (key1 > key2)
      return
      end function triplesort_gt_scalar_floats



      logical function triplesort_le_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_le_scalar_floats = (key1 <= key2)
      return
      end function triplesort_le_scalar_floats



      logical function triplesort_ge_scalar_floats (key1,scalar2)
      implicit none
      type(triplesort_floats),intent(in) :: key1
      real                   ,intent(in) :: scalar2
      type(triplesort_floats)            :: key2

      key2 = scalar2
      triplesort_ge_scalar_floats = (key1 >= key2)
      return
      end function triplesort_ge_scalar_floats


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_eq_scalar_doubles = (key1 == key2)
      return
      end function triplesort_eq_scalar_doubles



      logical function triplesort_ne_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_ne_scalar_doubles = (key1 /= key2)
      return
      end function triplesort_ne_scalar_doubles



      logical function triplesort_lt_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_lt_scalar_doubles = (key1 < key2)
      return
      end function triplesort_lt_scalar_doubles



      logical function triplesort_gt_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_gt_scalar_doubles = (key1 > key2)
      return
      end function triplesort_gt_scalar_doubles



      logical function triplesort_le_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_le_scalar_doubles = (key1 <= key2)
      return
      end function triplesort_le_scalar_doubles



      logical function triplesort_ge_scalar_doubles (key1,scalar2)
      implicit none
      type(triplesort_doubles),intent(in) :: key1
      double precision        ,intent(in) :: scalar2
      type(triplesort_doubles)            :: key2

      key2 = scalar2
      triplesort_ge_scalar_doubles = (key1 >= key2)
      return
      end function triplesort_ge_scalar_doubles


                             !!!!!!!!!!!!!!!!!


      logical function triplesort_eq_scalar_bools (key1,scalar2)
      implicit none
      type(triplesort_bools),intent(in) :: key1
      logical               ,intent(in) :: scalar2
      type(triplesort_bools)            :: key2

      key2 = scalar2
      triplesort_eq_scalar_bools = (key1 == key2)
      return
      end function triplesort_eq_scalar_bools



      logical function triplesort_ne_scalar_bools (key1,scalar2)
      implicit none
      type(triplesort_bools),intent(in) :: key1
      logical               ,intent(in) :: scalar2
      type(triplesort_bools)            :: key2

      key2 = scalar2
      triplesort_ne_scalar_bools = (key1 /= key2)
      return
      end function triplesort_ne_scalar_bools


!!------------------------ overloaded min and max ---------------------------!!
!!------------------------ overloaded min and max ---------------------------!!
!!------------------------ overloaded min and max ---------------------------!!


      type(triplesort_ints) function triplesort_min &
                                            (key1,key2) result (keymin)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      if (key1 < key2) then
           keymin = key1
      else
           keymin = key2
      end if
      return
      end function triplesort_min



      type(triplesort_ints) function triplesort_max &
                                            (key1,key2) result (keymax)
      implicit none
      type(triplesort_ints),intent(in) :: key1,key2

      if (key1 > key2) then
           keymax = key1
      else
           keymax = key2
      end if
      return
      end function triplesort_max



!!------------------------ overloaded minloc --------------------------------!!
!!------------------------ overloaded minloc --------------------------------!!
!!------------------------ overloaded minloc --------------------------------!!

      integer function triplesort_minloc (keys,nkeys,mask) result (iloc)
      implicit none
      type(triplesort_ints),intent(in) :: keys(:)
      integer              ,intent(in) :: nkeys
      logical     ,optional,intent(in) :: mask(:)
  !!  integer                          :: min_primary, min_secondary
  !!  integer                          :: iloc2(1)
      integer                          :: ikey
      type(triplesort_ints)            :: keymin

! One way of doing it:

      iloc = 0
      if (nkeys <= 0) return
      do ikey = 1,nkeys
          if (present(mask)) then
               if (.not.mask(ikey)) cycle
          end if
          if (iloc == 0) then
               keymin = keys(ikey)
               iloc   = ikey
          else if (keys(ikey) < keymin) then
               keymin = keys(ikey)
               iloc   = ikey
          end if
      end do
      return

! Another way of doing it:

  !!  min_primary     = minval    (keys(1:nkeys)%primary, MASK = mask)
  !!  if (present(mask)) then
  !!    min_secondary = minval    (keys(1:nkeys)%secondary,                  &
  !!                       MASK = (keys(1:nkeys)%primary   == min_primary    &
  !!                         .and. mask(:)))
  !!    iloc2         = minloc    (keys(1:nkeys)%tertiary,                   &
  !!                       MASK = (keys(1:nkeys)%primary   == min_primary    &
  !!                         .and. keys(1:nkeys)%secondary == min_secondary  &
  !!                         .and. mask(:)))
  !!  else
  !!    min_secondary = minval    (keys(1:nkeys)%secondary,                  &
  !!                       MASK = (keys(1:nkeys)%primary   == min_primary))
  !!    iloc2         = minloc    (keys(1:nkeys)%tertiary,                   &
  !!                       MASK = (keys(1:nkeys)%primary   == min_primary    &
  !!                         .and. keys(1:nkeys)%secondary == min_secondary))
  !!  end if
  !!  iloc = iloc2(1)
      return
      end function triplesort_minloc


!!------------------------ overloaded minval --------------------------------!!
!!------------------------ overloaded minval --------------------------------!!
!!------------------------ overloaded minval --------------------------------!!


      type(triplesort_ints) function triplesort_minval &
                                         (keys,nkeys,mask) result (keymin)
      implicit none
      type(triplesort_ints),intent(in) :: keys(:)
      integer              ,intent(in) :: nkeys
      logical     ,optional,intent(in) :: mask(:)
      integer                          :: iloc

      iloc = minloc(keys,nkeys,mask)
      if (iloc > 0) then
           keymin = keys(iloc)
      else
           keymin = 0
      end if
      return
      end function triplesort_minval


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module triplesort_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

