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
! Name       : MEMMAN 
! Category   : MEMORY
! Written    : 2002-09-24   by: Charles C Burch
! Revised    : 2007-12-04   by: Bill Menger
! Maturity   : beta
! Purpose    : MEMory MANager Routines
! Portability: No known limitations
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! memman performs the typical pointer variable operations and keeps tracks
! of the state of all pointer variables operated by memman.  Warning and
! error messages are produced when memman detects possible error conditions
! that might create a memory leak or misuse of a pointer variable.
!
! The tracking uses the address of the pointer variable as the key.  
! The memman_nullify routine provide a v_name argument where the programmer 
! can associate a literal name with the pointer variable to be used on messages 
! as the pointer address is typically not very meaningful to the F90 programmer.
!
! Messages with pointer variable information will use the format
!  PPPP:DDDD-SSSS-NNNN, where
!    PPPP is the address of the pointer variable
!    DDDD is the address of the data space assigned to the pointer 
!         -0 means nullified
!    SSSS is the size in bytes of the data space assigned to the pointer data
!    NNNN is the literal name the programmer assigned to the pointer variable 
!
! The default definition of a v_name is "-".  Once v_name is specified by 
! memman_nullify, then that definition is used with all successive memman calls
! for that specific pointer variable.
!
! The initial state in memman for pointer variables is undefined. The first call
! to memman with any pointer variable should be memman_nullify which nullifies
! the variable.  One will get an error message if a pointer variable is used by
! a memman call without first calling memman_nullify.
! example use: call memman_nullify(v,"programer_name_for v")
! 
! After a pointer variable is nullified, then space is allocated for it by using
! memman_allocate.  One can resize a pointer variable using memman_allocate
! or memman_reallocate.  memman_reallocate will copy any old data to the new
! memory space where memman_allocate will not.  memman supports 1,2,3,4, or 5
! dimension pointer variables that can be integer, real, double, complex,
! logical or character type.  In  memman_allocate and memman_reallocate, one
! needs to supply the correct number of dimension sizes consistent with the
! dimension of the argument pointer variable.  For debugging purposes, one
! can also optionally attach the v_name as the last argument of memman_allocate
! or memman_reallocate, but this is not required nor suggested if one uses
! v_name with memman_nullify.
! example uses: call memman_allocate(v,100,ierr)
!               call memman_reallocate(v,200,ierr)
!               call memman_allocate(v,100,ierr,"v")
!               call memman_reallocate(v,200,ierr,"v")
!
! When one is done with the memory space for a pointer variable, one should
! call memmman_deallocate to free up the space for other uses.  
! memman_deallocate will do an implicit nullify of the pointer variable.
! When one is completely done with a pointer variable, one should use
! memman_free.  This is especially important when using a local pointer 
! variable used with a subroutine.  Once a pointer variable is freed, it must 
! be nullified before it is reused.
! example uses: call memman_deallocate(v)
!               call memman_free(v)
!
! --------------------------- typical use example ------------------------------
! The following illustrates typical use of key memman routines:
!
!  integer, pointer   :: v(:) !could be other types and 2,3,4,5 dimension
!
!  call memman_nullify(v, "v_name")   !must be first call using v
!
!  call memman_allocate(v, intial_size, ierr) !set up for first use
!                   do some work using v
!  call memman_reallocate(v, new_dim, ierr) ! resize v amd copy old data
!            do work with resized v
!  call memman_deallocate(v) !   all done with current v info
!   
!  call memman_allocate(v, another_new_dim, ierr)  !setup for new use
!           do some work using v
!  call memman_free(v)  !all done with v
!
! ------------------------------ debugging aids --------------------------------
! To see actions performed by memman,   call memman_set_print_mode(2)
! when you are done with the diagnostics, call memman_set_print_mode(1)
!  
! To see the current status of all pointer variables handled by memman,
!  call memman_dump_tracking("any descriptive string to be used as a header")
! This list all variables in the format 
!  pointer_address|address_allocated_to_pointer bytes_allocated v_name
! pointers that are nullified will have address and size of zero.
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!-------------------------------------------------------------------------------
!</global_doc>
 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! function memman_loc(v)
!   integer/real/double precision/complex/character(len=*)/logical, pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
! Purpose: returns the address of v similar to %loc(v) in F99 and &v in C.
!-------------------------------------------------------------------------------
! function memman_pointer_loc(v)
!   integer/real/double precision/complex/character(len=*)/logical, pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
! Purpose: returns the pointer address of v.
!-------------------------------------------------------------------------------
! function memman_sizeof(v)
!   integer/real/double precision/complex/character(len=*)/logical  :: 
!                            v, v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
! Purpose: returns the size in bytes of v.
!-------------------------------------------------------------------------------
! subroutine memman_allocate(v, n1,[n2,[n3,[n4,[n5]]]], ierr, [v_name])
!   integer/real/double precision/complex/character(len=*), pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
!   integer, intent(in)                    :: n1 [,n2,[n3 [,n4 [,n5]]]]
!   integer, intent(out)                   :: ierr
!   character(len=*), intent(in), optional :: v_name
! Purpose: allocate v with specified dimensions
!  return ierr =0 if no err, else returns allocate status if error
!-------------------------------------------------------------------------------
! subroutine memman_reallocate(v, n1,[n2,[n3,[n4,[n5]]]], ierr, [v_name])
!   integer/real/double precision/complex/character(len=*), pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
!   integer, intent(in)                    :: n1 [n2,[,n3 [,n4 [,n5]]]]
!   integer, intent(out)                   :: ierr
!   character(len=*), intent(in), optional :: v_name
! Purpose: reallocate v with specified dimensions
!  return ierr =0 if no err, else returns reallocate status if error
!
! Note memman_allocate will reallocate a variable also if the variable
!  is already allocated.  memfun_reallocates copies exisiting data from the
!  previous allocation where memfun_allocate does not copy the old data.   
!-------------------------------------------------------------------------------
! subroutine memman_deallocate(v, [free])
!   integer/real/double precision/complex/character(len=*), pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
!   logical, intent(in),optional         :: free
! Purpose: deallocate v and nullify v
! if "free is present and true, remove tracking information for v
!    In this case if v is reused, it must be renullified
! if "free" is not present or false, retain tracking information for v
!    In this case v is nullified and need not be nullified before being reused,
!    but nothing will be hurt if it is renullified
!
! Note: For most use CPS process usage, "free: should not be present
! "free" = .true. is primarily used for work pointer variables within
! a subroutine when use of the variable is completed-this removes any tracking
! information which is important as the location for v might be later reused in
! another subroutines or other pointer variable.
!-------------------------------------------------------------------------------
! subroutine memman_free(v)
! This is the same as memman_deallocate(v,free=.true.)
!-------------------------------------------------------------------------------
! subroutine memman_nullify(v, [v_name])
!   integer/real/double precision/complex/character(len=*), pointer  :: 
!                                v(:), v(:,:),v(:,:,:), v(:,:,:,:), v(:,:,:,:,:)
!   character(len=*),intent(in),optional :: v_name
! Purpose: nullify v and assign v_name to v
! One should usually supply v_name so error messages have names that a
! programmer can recognize.
!-------------------------------------------------------------------------------
! subroutine memman_dump_tracking(title)
!   character(len=*),intent(in) :: title
! Purpose:Produce a list the current allocated variables with associated title
!-------------------------------------------------------------------------------
! subroutine memman_exit_tracking(isw)
!   integer,intent(in) :: isw
! Purpose:Produce a lis of allocated variables and empty the list
!         isw=0 bypasses messages if there are no allocated variable (default)
!               prints only variable for which data space is allocated to them
!             1 prints all variables used even if they have been deallocated 
!-------------------------------------------------------------------------------
! function memman_get_memory_allocated() result(mem)
!   integer        :: mem
! Purpose: Get amount of memory allocated in 4-byte words
!-------------------------------------------------------------------------------
! subroutine memman_set_print_mode(sw)
!   integer, intent(in)  :: sw
! Purpose: sw=0 no printing, 1 errors only(default), 2 diagnostics also 
!-------------------------------------------------------------------------------
! subroutine memman_save_print_mode(sw)
!   integer, intent(in)  :: sw
! Purpose: save print_mode on a stack and set to sw 
! Note: The stack size is currently 20
!      memman_restore_print_mode needs to be called when one is done with the
!      print_sw set by memman_save_print_mode
!-------------------------------------------------------------------------------
! subroutine memman_restore_print_mode
! Purpose: restore print_mode saved print_mode from a stack
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  2  2007-12-04  Bill Menger  Initialized two variables for ifort compiler.
!  1  2003-06-17  C C Burch    Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known restrictions
! Modification may be needed for machines supporting >2GB data space
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

module memman_module                   
  use sizeof_module
  implicit  none

!!---------------------------- interfaces ---------------------------------!!

  interface                             ! memman_crou.c routines
    function  memman_check_variable_c(p_loc, v_loc, v_size, v_name) result(stat)
      integer,    intent(in) :: p_loc
      integer,    intent(in) :: v_loc
      integer,    intent(in) :: v_size
      character,  intent(in) :: v_name
      integer                :: stat
    end function  memman_check_variable_c

    function  memman_tracking_all_c(p_loc, v_loc, v_size, v_name) result(stat)
      integer,    intent(in) :: p_loc
      integer,    intent(in) :: v_loc
      integer,    intent(in) :: v_size
      character,  intent(in) :: v_name
      integer                :: stat
    end function  memman_tracking_all_c

    function  memman_tracking_del_c(p_loc, v_loc, v_size, v_name) result(stat)
      integer,    intent(in) :: p_loc
      integer,    intent(in) :: v_loc
      integer,    intent(in) :: v_size
      character,  intent(in) :: v_name
      integer                :: stat
    end function  memman_tracking_del_c

    subroutine  memman_tracking_free_c(p_loc)
      integer,    intent(in) :: p_loc
    end subroutine  memman_tracking_free_c

    subroutine memman_dump_tracking_c(title)
     character, intent(in) :: title
    end subroutine memman_dump_tracking_c

    subroutine memman_exit_tracking_c(isw)
      integer, intent(in)   :: isw
    end subroutine memman_exit_tracking_c

    function memman_get_memory_allocated_c() result(mem)
      integer               :: mem
    end function memman_get_memory_allocated_c

    subroutine memman_get_info_c(p_loc, v_loc, v_size, v_name)
      integer,    intent(in) :: p_loc
      integer,    intent(in) :: v_loc
      integer,    intent(in) :: v_size
      character,  intent(in) :: v_name
    end subroutine memman_get_info_c
    
    subroutine memman_change_info_c(p_loc, v_loc, v_size, v_name)
      integer,    intent(in) :: p_loc
      integer,    intent(in) :: v_loc
      integer,    intent(in) :: v_size
      character,  intent(in) :: v_name
    end subroutine memman_change_info_c
   
    function memman_loc_char_c(v) result(add)
      character                           :: v
      integer                             :: add
    end function memman_loc_char_c
    
    function memman_loc_integer_c(v) result(add)
      integer                             :: v
      integer                             :: add
    end function memman_loc_integer_c
    
    function memman_loc_real_c(v) result(add)
      real                                :: v
      integer                             :: add
    end function memman_loc_real_c
    
    function memman_loc_double_c(v) result(add)
      double precision                    :: v
      integer                             :: add
    end function memman_loc_double_c
    
    function memman_loc_complex_c(v) result(add)
      complex                             :: v
      integer                             :: add
    end function memman_loc_complex_c
    
    function memman_loc_logical_c(v) result(add)
      logical                             :: v
      integer                             :: add
    end function memman_loc_logical_c
    
    function memman_ptrloc_c1_c(v) result(add)
      character(len=*),pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_c1_c
    
    function memman_ptrloc_c2_c(v) result(add)
      character(len=*),pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_c2_c
    
    function memman_ptrloc_c3_c(v) result(add)
      character(len=*),pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_c3_c
    
    function memman_ptrloc_c4_c(v) result(add)
      character(len=*),pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_c4_c
    
    function memman_ptrloc_c5_c(v) result(add)
      character(len=*),pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_c5_c
    
    function memman_ptrloc_i1_c(v) result(add)
      integer         ,pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_i1_c
    
    function memman_ptrloc_i2_c(v) result(add)
      integer         ,pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_i2_c
    
    function memman_ptrloc_i3_c(v) result(add)
      integer         ,pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_i3_c
    
    function memman_ptrloc_i4_c(v) result(add)
      integer         ,pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_i4_c
    
    function memman_ptrloc_i5_c(v) result(add)
      integer         ,pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_i5_c
    
    function memman_ptrloc_r1_c(v) result(add)
      real            ,pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_r1_c
    
    function memman_ptrloc_r2_c(v) result(add)
      real            ,pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_r2_c
    
    function memman_ptrloc_r3_c(v) result(add)
      real            ,pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_r3_c
    
    function memman_ptrloc_r4_c(v) result(add)
      real            ,pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_r4_c
    
    function memman_ptrloc_r5_c(v) result(add)
      real            ,pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_r5_c
    
    function memman_ptrloc_d1_c(v) result(add)
      double precision,pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_d1_c
    
    function memman_ptrloc_d2_c(v) result(add)
      double precision,pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_d2_c
    
    function memman_ptrloc_d3_c(v) result(add)
      double precision,pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_d3_c
    
    function memman_ptrloc_d4_c(v) result(add)
      double precision,pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_d4_c
    
    function memman_ptrloc_d5_c(v) result(add)
      double precision,pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_d5_c
    
    function memman_ptrloc_z1_c(v) result(add)
      complex         ,pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_z1_c
    
    function memman_ptrloc_z2_c(v) result(add)
      complex         ,pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_z2_c
    
    function memman_ptrloc_z3_c(v) result(add)
      complex         ,pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_z3_c
    
    function memman_ptrloc_z4_c(v) result(add)
      complex         ,pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_z4_c
    
    function memman_ptrloc_z5_c(v) result(add)
      complex         ,pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_z5_c
    
    function memman_ptrloc_l1_c(v) result(add)
      logical         ,pointer            :: v(:)
      integer                             :: add
    end function memman_ptrloc_l1_c
    
    function memman_ptrloc_l2_c(v) result(add)
      logical         ,pointer            :: v(:,:)
      integer                             :: add
    end function memman_ptrloc_l2_c
    
    function memman_ptrloc_l3_c(v) result(add)
      logical         ,pointer            :: v(:,:,:)
      integer                             :: add
    end function memman_ptrloc_l3_c
    
    function memman_ptrloc_l4_c(v) result(add)
      logical         ,pointer            :: v(:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_l4_c
    
    function memman_ptrloc_l5_c(v) result(add)
      logical         ,pointer            :: v(:,:,:,:,:)
      integer                             :: add
    end function memman_ptrloc_l5_c
   
    subroutine memman_set_print_mode_c(sw)
      integer                             :: sw
    end subroutine memman_set_print_mode_c
 
  end interface

! ---------------------- Overloaded functions --------------------------

  interface memman_get_info   
    module procedure  memman_get_inf               !input actual address

    module procedure  memman_get_inf_c1
    module procedure  memman_get_inf_r1
    module procedure  memman_get_inf_i1
    module procedure  memman_get_inf_z1
    module procedure  memman_get_inf_d1
    module procedure  memman_get_inf_l1

    module procedure  memman_get_inf_r2
    module procedure  memman_get_inf_i2

    module procedure  memman_get_inf_z2
    module procedure  memman_get_inf_d2
    module procedure  memman_get_inf_l2
    module procedure  memman_get_inf_c2

    module procedure  memman_get_inf_r3
    module procedure  memman_get_inf_i3
    module procedure  memman_get_inf_z3
    module procedure  memman_get_inf_d3
    module procedure  memman_get_inf_l3
    module procedure  memman_get_inf_c3

    module procedure  memman_get_inf_r4
    module procedure  memman_get_inf_i4
    module procedure  memman_get_inf_z4
    module procedure  memman_get_inf_d4
    module procedure  memman_get_inf_l4
    module procedure  memman_get_inf_c4

    module procedure  memman_get_inf_r5
    module procedure  memman_get_inf_i5
    module procedure  memman_get_inf_z5
    module procedure  memman_get_inf_d5
    module procedure  memman_get_inf_l5
    module procedure  memman_get_inf_c5
  end interface

  interface memman_check_variable
    module procedure  memman_check_c1
    module procedure  memman_check_r1
    module procedure  memman_check_i1
    module procedure  memman_check_z1
    module procedure  memman_check_d1
    module procedure  memman_check_l1

    module procedure  memman_check_r2
    module procedure  memman_check_i2
    module procedure  memman_check_z2
    module procedure  memman_check_d2
    module procedure  memman_check_l2
    module procedure  memman_check_c2

    module procedure  memman_check_r3
    module procedure  memman_check_i3
    module procedure  memman_check_z3
    module procedure  memman_check_d3
    module procedure  memman_check_l3
    module procedure  memman_check_c3

    module procedure  memman_check_r4
    module procedure  memman_check_i4
    module procedure  memman_check_z4
    module procedure  memman_check_d4
    module procedure  memman_check_l4
    module procedure  memman_check_c4

    module procedure  memman_check_r5
    module procedure  memman_check_i5
    module procedure  memman_check_z5
    module procedure  memman_check_d5
    module procedure  memman_check_l5
    module procedure  memman_check_c5
  end interface

  interface memman_loc
    module procedure  memman_loc_char
    module procedure  memman_loc_real
    module procedure  memman_loc_integer
    module procedure  memman_loc_complex
    module procedure  memman_loc_double
    module procedure  memman_loc_logical

    module procedure  memman_loc_c1
    module procedure  memman_loc_r1
    module procedure  memman_loc_i1
    module procedure  memman_loc_z1
    module procedure  memman_loc_d1
    module procedure  memman_loc_l1

    module procedure  memman_loc_r2
    module procedure  memman_loc_i2
    module procedure  memman_loc_z2
    module procedure  memman_loc_d2
    module procedure  memman_loc_l2
    module procedure  memman_loc_c2

    module procedure  memman_loc_r3
    module procedure  memman_loc_i3
    module procedure  memman_loc_z3
    module procedure  memman_loc_d3
    module procedure  memman_loc_l3
    module procedure  memman_loc_c3

    module procedure  memman_loc_r4
    module procedure  memman_loc_i4
    module procedure  memman_loc_z4
    module procedure  memman_loc_d4
    module procedure  memman_loc_l4
    module procedure  memman_loc_c4

    module procedure  memman_loc_r5
    module procedure  memman_loc_i5
    module procedure  memman_loc_z5
    module procedure  memman_loc_d5
    module procedure  memman_loc_l5
    module procedure  memman_loc_c5
  end interface

  interface memman_pointer_loc
    module procedure  memman_ptrloc_c1
    module procedure  memman_ptrloc_c2
    module procedure  memman_ptrloc_c3
    module procedure  memman_ptrloc_c4
    module procedure  memman_ptrloc_c5

    module procedure  memman_ptrloc_i1
    module procedure  memman_ptrloc_i2
    module procedure  memman_ptrloc_i3
    module procedure  memman_ptrloc_i4
    module procedure  memman_ptrloc_i5

    module procedure  memman_ptrloc_r1
    module procedure  memman_ptrloc_r2
    module procedure  memman_ptrloc_r3
    module procedure  memman_ptrloc_r4
    module procedure  memman_ptrloc_r5

    module procedure  memman_ptrloc_d1
    module procedure  memman_ptrloc_d2
    module procedure  memman_ptrloc_d3
    module procedure  memman_ptrloc_d4
    module procedure  memman_ptrloc_d5

    module procedure  memman_ptrloc_z1
    module procedure  memman_ptrloc_z2
    module procedure  memman_ptrloc_z3
    module procedure  memman_ptrloc_z4
    module procedure  memman_ptrloc_z5

    module procedure  memman_ptrloc_l1
    module procedure  memman_ptrloc_l2
    module procedure  memman_ptrloc_l3
    module procedure  memman_ptrloc_l4
    module procedure  memman_ptrloc_l5
  end interface

  interface memman_allocate
    module procedure  memman_all_c1
    module procedure  memman_all_c2
    module procedure  memman_all_c3
    module procedure  memman_all_c4
    module procedure  memman_all_c5

    module procedure  memman_all_r1
    module procedure  memman_all_r2
    module procedure  memman_all_r3
    module procedure  memman_all_r4
    module procedure  memman_all_r5

    module procedure  memman_all_i1
    module procedure  memman_all_i2
    module procedure  memman_all_i3
    module procedure  memman_all_i4
    module procedure  memman_all_i5

    module procedure  memman_all_z1
    module procedure  memman_all_z2
    module procedure  memman_all_z3
    module procedure  memman_all_z4
    module procedure  memman_all_z5

    module procedure  memman_all_d1
    module procedure  memman_all_d2
    module procedure  memman_all_d3
    module procedure  memman_all_d4
    module procedure  memman_all_d5

    module procedure  memman_all_l1
    module procedure  memman_all_l2
    module procedure  memman_all_l3
    module procedure  memman_all_l4
    module procedure  memman_all_l5
  end interface

  interface memman_reallocate
    module procedure  memman_reall_c1
    module procedure  memman_reall_c2
    module procedure  memman_reall_c3
    module procedure  memman_reall_c4
    module procedure  memman_reall_c5

    module procedure  memman_reall_r1
    module procedure  memman_reall_r2
    module procedure  memman_reall_r3
    module procedure  memman_reall_r4
    module procedure  memman_reall_r5

    module procedure  memman_reall_i1
    module procedure  memman_reall_i2
    module procedure  memman_reall_i3
    module procedure  memman_reall_i4
    module procedure  memman_reall_i5

    module procedure  memman_reall_z1
    module procedure  memman_reall_z2
    module procedure  memman_reall_z3
    module procedure  memman_reall_z4
    module procedure  memman_reall_z5

    module procedure  memman_reall_d1
    module procedure  memman_reall_d2
    module procedure  memman_reall_d3
    module procedure  memman_reall_d4
    module procedure  memman_reall_d5

    module procedure  memman_reall_l1
    module procedure  memman_reall_l2
    module procedure  memman_reall_l3
    module procedure  memman_reall_l4
    module procedure  memman_reall_l5
  end interface

  interface memman_deallocate
    module procedure  memman_del_c1
    module procedure  memman_del_c2
    module procedure  memman_del_c3
    module procedure  memman_del_c4
    module procedure  memman_del_c5

    module procedure  memman_del_r1
    module procedure  memman_del_r2
    module procedure  memman_del_r3
    module procedure  memman_del_r4
    module procedure  memman_del_r5

    module procedure  memman_del_i1
    module procedure  memman_del_i2
    module procedure  memman_del_i3
    module procedure  memman_del_i4
    module procedure  memman_del_i5

    module procedure  memman_del_z1
    module procedure  memman_del_z2
    module procedure  memman_del_z3
    module procedure  memman_del_z4
    module procedure  memman_del_z5

    module procedure  memman_del_d1
    module procedure  memman_del_d2
    module procedure  memman_del_d3
    module procedure  memman_del_d4
    module procedure  memman_del_d5

    module procedure  memman_del_l1
    module procedure  memman_del_l2
    module procedure  memman_del_l3
    module procedure  memman_del_l4
    module procedure  memman_del_l5
  end interface

  interface memman_free
    module procedure  memman_free_c1
    module procedure  memman_free_r1
    module procedure  memman_free_i1
    module procedure  memman_free_z1
    module procedure  memman_free_d1
    module procedure  memman_free_l1

    module procedure  memman_free_r2
    module procedure  memman_free_i2
    module procedure  memman_free_z2
    module procedure  memman_free_d2
    module procedure  memman_free_l2
    module procedure  memman_free_c2

    module procedure  memman_free_r3
    module procedure  memman_free_i3
    module procedure  memman_free_z3
    module procedure  memman_free_d3
    module procedure  memman_free_l3
    module procedure  memman_free_c3

    module procedure  memman_free_r4
    module procedure  memman_free_i4
    module procedure  memman_free_z4
    module procedure  memman_free_d4
    module procedure  memman_free_l4
    module procedure  memman_free_c4

    module procedure  memman_free_r5
    module procedure  memman_free_i5
    module procedure  memman_free_z5
    module procedure  memman_free_d5
    module procedure  memman_free_l5
    module procedure  memman_free_c5
  end interface

  interface memman_nullify
    module procedure  memman_nul_c1
    module procedure  memman_nul_c2
    module procedure  memman_nul_c3
    module procedure  memman_nul_c4
    module procedure  memman_nul_c5

    module procedure  memman_nul_r1
    module procedure  memman_nul_r2
    module procedure  memman_nul_r3
    module procedure  memman_nul_r4
    module procedure  memman_nul_r5

    module procedure  memman_nul_i1
    module procedure  memman_nul_i2
    module procedure  memman_nul_i3
    module procedure  memman_nul_i4
    module procedure  memman_nul_i5

    module procedure  memman_nul_z1
    module procedure  memman_nul_z2
    module procedure  memman_nul_z3
    module procedure  memman_nul_z4
    module procedure  memman_nul_z5

    module procedure  memman_nul_d1
    module procedure  memman_nul_d2
    module procedure  memman_nul_d3
    module procedure  memman_nul_d4
    module procedure  memman_nul_d5

    module procedure  memman_nul_l1
    module procedure  memman_nul_l2
    module procedure  memman_nul_l3
    module procedure  memman_nul_l4
    module procedure  memman_nul_l5
  end interface

  interface memman_sizeof
    module procedure memman_sizeof_c0
    module procedure memman_sizeof_c1
    module procedure memman_sizeof_c2
    module procedure memman_sizeof_c3
    module procedure memman_sizeof_c4
    module procedure memman_sizeof_c5

    module procedure memman_sizeof_i0
    module procedure memman_sizeof_i1
    module procedure memman_sizeof_i2
    module procedure memman_sizeof_i3
    module procedure memman_sizeof_i4
    module procedure memman_sizeof_i5

    module procedure memman_sizeof_r0
    module procedure memman_sizeof_r1
    module procedure memman_sizeof_r2
    module procedure memman_sizeof_r3
    module procedure memman_sizeof_r4
    module procedure memman_sizeof_r5

    module procedure memman_sizeof_d0
    module procedure memman_sizeof_d1
    module procedure memman_sizeof_d2
    module procedure memman_sizeof_d3
    module procedure memman_sizeof_d4
    module procedure memman_sizeof_d5

    module procedure memman_sizeof_z0
    module procedure memman_sizeof_z1
    module procedure memman_sizeof_z2
    module procedure memman_sizeof_z3
    module procedure memman_sizeof_z4
    module procedure memman_sizeof_z5

    module procedure memman_sizeof_l0
    module procedure memman_sizeof_l1
    module procedure memman_sizeof_l2
    module procedure memman_sizeof_l3
    module procedure memman_sizeof_l4
    module procedure memman_sizeof_l5

  end interface

  private                           ! set default to all routines as private

  public :: memman_allocate         ! allocate a variable
  public :: memman_change_info      ! change tracking information
  public :: memman_check_variable   ! check if data on varaiable is consistent
  public :: memman_deallocate       ! deallocate a variable
  public :: memman_dump_tracking    ! list the currently allocated variables
  public :: memman_exit_tracking    ! check if any exisiting allocated var
  public :: memman_free             ! deallocate and remove tracking info
  public :: memman_get_info         ! get information for allocated variable
  public :: memman_get_memory_allocated !gets amount current memory allocated
  public :: memman_is_string_blank  ! tests if a string blank or not
  public :: memman_loc              ! get location of a variable
  public :: memman_nullify          ! nullify  a variable
  public :: memman_pointer_loc      ! get location of a pointer variable
  public :: memman_reallocate       ! realocate a variable
  public :: memman_restore_print_mode ! pop print_mode on a stack
  public :: memman_save_print_mode  ! push print_mode on a stack
  public :: memman_set_print_mode   ! activates/deactives printing
  public :: memman_sizeof           ! size in bytes of a variable

  integer, private, save :: print_sw=1              !default errors only
  integer, save          :: n_print_saves=0         !for push/pop print_mode
  integer, parameter     :: n_print_saves_max=20
  integer, save          :: print_sw_saves(n_print_saves_max)

! need to get elsewhere other than pc_module-will use pcps later
  integer, parameter :: print_lun=6

  character (len=100), public, save :: MEMMAN_IDENT = &
       '$Id: memman.f90,v 1.2 2007/12/05 15:05:53 Menger beta sps $'

  contains

!---------------------------------------------------------
! simple function to see if a string is all blanks
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  function memman_is_string_blank(s) result(stat)
    character(len=*), intent(in) :: s
    logical                      :: stat

    integer                      :: i,n

    n=len(s)
    if(n.eq.0) then
      stat=.true.
      return
    endif

    stat=.false.
    do i=1,n
      if(s(i:i).ne.' ') return
    enddo
    stat=.true.
    return
  end  function memman_is_string_blank
    
!---------------------------------------------------------------
! Convert a Fortran character(len=*) variable to c-type variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------------
  subroutine memman_form_c_vname(v_name, c_name)
    character(len=*), intent(in), optional :: v_name
    character, intent(out)                 :: c_name(:)

    integer                                :: i, n, n1, n2

    n1=0
    if(present(v_name)) then
      n=len(v_name)
      n2=size(c_name)-1
      do i=1,n
        if(v_name(i:i).ne.' ') then
          if(n1.ge.n2) exit
          n1=n1+1
          c_name(n1)=v_name(i:i)
        endif
      enddo
    endif
    c_name(n1+1)=char(0)
    return
  end subroutine memman_form_c_vname
    
!------------------------------------------------------------------
! Controls print level:0-no priniting, 1 errors only, 2 diagnostics
!
! Written July 2002 by Charles C Burch
!------------------------------------------------------------------
  subroutine memman_set_print_mode(sw)
    integer, intent(in)  :: sw

    print_sw=sw
    if(print_sw.lt.0) print_sw=0
    if(print_sw.gt.2) print_sw=2
    call memman_set_print_mode_c(print_sw)
    return
  end subroutine memman_set_print_mode

!---------------------------------------------------------
! Save print_mode and set to sw 
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_save_print_mode(sw)
    integer, intent(in)  :: sw

    n_print_saves=n_print_saves+1
    if(n_print_saves.le.n_print_saves_max.and.n_print_saves.gt.0) then
      print_sw_saves(n_print_saves)=print_sw
    else
      write(print_lun,*) &
       "Warning: memman_save_print_mode stack overflow, size=", &
       n_print_saves, ", max=",n_print_saves_max
    endif

    call memman_set_print_mode(sw)
    return
  end subroutine memman_save_print_mode

!---------------------------------------------------------
! Restore print_mode saved print_mode 
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_restore_print_mode

    if(n_print_saves.le.n_print_saves_max.and.n_print_saves.gt.0) then
      call memman_set_print_mode(print_sw_saves(n_print_saves))
    else
      write(print_lun,*) &
       "Warning: memman_restore_print_mode stack overflow, size=", &
       n_print_saves, ", max=",n_print_saves_max
    endif

    n_print_saves=n_print_saves-1
    return
  end subroutine memman_restore_print_mode
    

!-------------------------------------------- -------------
! List any tracked allocated variables not deleted
! Clear out the tracking buffer
! isw if present and zero means to bypass the 
!     no allocated variable message
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_exit_tracking(isw)
    integer, optional, intent(in) :: isw

    integer                       :: c_isw

    c_isw=0
    if(present(isw)) c_isw=isw

    call memman_exit_tracking_c(c_isw)
    return
  end  subroutine memman_exit_tracking
  
!---------------------------------------------------------
! Dump the tracked allocated variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_dump_tracking(title)
    character(len=*), intent(in) :: title

    character                   :: c_title(1:len(title)+1)
    integer                     :: i, n
    n=len_trim(title)
    do i=1, n
      c_title(i)=title(i:i)
    enddo
    c_title(n+1)=char(0)

    call memman_dump_tracking_c(c_title(1))
    return
  end  subroutine memman_dump_tracking

!---------------------------------------------------------
! Track an allocated variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_tracking_allocate(p_loc, v_loc, v_size, v_name)
    integer, intent(in)                    :: p_loc
    integer, intent(in)                    :: v_loc
    integer, intent(in)                    :: v_size
    character(len=*), intent(in), optional :: v_name

    integer                                :: stat
    character                              :: c_name(240)

    call memman_form_c_vname(v_name, c_name)
    stat=memman_tracking_all_c(p_loc, v_loc, v_size, c_name(1))
    return
  end  subroutine memman_tracking_allocate
  
!---------------------------------------------------------
! Track a deallocated variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_tracking_deallocate(p_loc, v_loc, v_size, v_name)
    integer, intent(in)                    :: p_loc
    integer, intent(in)                    :: v_loc
    integer, intent(in)                    :: v_size
    character(len=*), intent(in), optional :: v_name

    integer                                :: stat
    character                              :: c_name(240)

    call memman_form_c_vname(v_name, c_name)
    stat=memman_tracking_del_c(p_loc, v_loc, v_size, c_name(1))
    return
  end  subroutine memman_tracking_deallocate

!---------------------------------------------------------
! Free a track variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_tracking_free(p_loc)
    integer, intent(in)                    :: p_loc

    call memman_tracking_free_c(p_loc)
    return
  end  subroutine memman_tracking_free

!---------------------------------------------------------
! Mark as nullified variable
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_nullify_variable(p_loc, v_name)
    integer, intent(in)                    :: p_loc
    character(len=*), intent(in), optional :: v_name

    call memman_tracking_allocate(p_loc, 0, 0, v_name)
    return
  end  subroutine memman_nullify_variable
  
!---------------------------------------------------------
! Get amount of memory allocated in bytes
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  function memman_get_memory_allocated() result(mem)
    integer    :: mem

    mem=memman_get_memory_allocated_c()
    return
  end function memman_get_memory_allocated

!---------------------------------------------------------
! Call c routine to change info
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_change_info(p_loc, v_loc, v_size, v_name)
    integer, intent(in)          :: p_loc  
    integer, intent(in)          :: v_loc  
    integer, intent(in)          :: v_size 
    character(len=*), intent(in) :: v_name 

    character                    :: c_name(240)

    call memman_form_c_vname(v_name, c_name)
    call memman_change_info_c(p_loc, v_loc, v_size, c_name(1))
    return
  end subroutine memman_change_info

!---------------------------------------------------------
! Print memman_del error messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_deallocate_error(routine, stat, p_loc, v_loc, &
      size, v_name)
    integer,         intent(in)           :: stat
    character(len=*),intent(in)           :: routine
    integer,         intent(in)           :: p_loc
    integer,         intent(in)           :: v_loc
    integer,         intent(in)           :: size
    character(len=*),intent(in), optional :: v_name

    character(len=20)                     :: a_vloc, a_ploc, a_size
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.1) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc
    write(a_vloc,'(i20)') v_loc
    write(a_size,'(i20)') size

    vname=trim(adjustl(a_ploc))//":"//trim(adjustl(a_vloc))//"-"// &
          trim(adjustl(a_size))//"-"//trim(a_name)
    write(print_lun, *) &
        "Error in memman_del_"//routine//"("//trim(vname)// &
        ") status=", stat
    return
  end subroutine memman_deallocate_error

!---------------------------------------------------------
! Print memman_all error messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_allocate_error(p_loc, stat, routine, v_name, size)
    integer,         intent(in)           :: p_loc
    integer,         intent(in)           :: stat
    character(len=*),intent(in)           :: routine
    character(len=*),intent(in), optional :: v_name
    integer, intent(in)                   :: size

    character(len=20)                     :: a_ploc
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.1) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc
    vname=trim(adjustl(a_ploc))//":"//trim(a_name)
    write(print_lun, *) &
        "Error in memman_all_"//routine//"("//trim(vname)// &
        ") status=", stat, ", bytes requested=",size
    return
  end subroutine memman_allocate_error

!-------------------------------------------------------------
! Print memman_nullify error messages
! if deallocate is present, no error message as it is assumed
!   that the developer knows what he/she is doing
!
! Written July 2002 by Charles C Burch
!-------------------------------------------------------------
  subroutine memman_nullify_error(routine, p_loc,  &
     v_loc, size, v_name)
    character(len=*),intent(in)           :: routine
    integer,         intent(in)           :: p_loc
    integer,         intent(in)           :: v_loc
    integer,         intent(in)           :: size
    character(len=*),intent(in), optional :: v_name

    character(len=20)                     :: a_vloc, a_ploc, a_size
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.1) return

    if(size.eq.0) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc
    write(a_vloc,'(i20)') v_loc
    write(a_size,'(i20)') size

    vname=trim(adjustl(a_ploc))//":"//trim(adjustl(a_vloc))//"-"// &
          trim(adjustl(a_size))//"-"//trim(a_name)

    write(print_lun, *) &
      "Error: memman_nul_"//routine//"("//trim(vname)// &
      ") variable has associated data, possible memory leak" 

    return
  end subroutine memman_nullify_error

!-------------------------------------------------------------
! Print memman no nullify error messages
!
! Written July 2002 by Charles C Burch
!-------------------------------------------------------------
  subroutine memman_nonullify_error(p_loc, routine, v_name)
    character(len=*),intent(in) :: routine
    integer,         intent(in) :: p_loc
    character(len=*),intent(in) :: v_name

    character(len=20)           :: a_ploc
    character(len=100)          :: vname

    if(print_sw.lt.1) return

    write(a_ploc,'(i20)') p_loc

    vname=trim(adjustl(a_ploc))//":0-0-"//trim(v_name)

    write(print_lun, *) &
      "Error: memman_"//routine//"("//trim(vname)// &
      ") variable has not been previously nullified by memman" 

    return
  end subroutine memman_nonullify_error

!---------------------------------------------------------
! Print memman_deallocate messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_deallocate_msg(free, routine, p_loc, v_loc, del, v_name)
    integer, intent(in)                   :: del
    character(len=*),intent(in)           :: routine
    character(len=*),intent(in), optional :: v_name
    integer, intent(in)                   :: p_loc 
    integer, intent(in)                   :: v_loc 
    logical, intent(in), optional         :: free

    character(len=20)                     :: a_vloc, a_ploc, a_del, a_mem
    character(len=100)                    :: vname
    character(len=40)                     :: a_name
    character(len=5)                      :: free_del

    if(print_sw.lt.2) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    free_del="del_"
    if(present(free)) then
      if(free) free_del="free_"
    endif

    write(a_ploc,'(i20)') p_loc
    write(a_vloc,'(i20)') v_loc
    write(a_del,'(i20)') del
    write(a_mem,'(i20)') memman_get_memory_allocated()

    vname=trim(adjustl(a_ploc))//":"//trim(adjustl(a_vloc))//"-"// &
          trim(adjustl(a_del))//"-"//trim(a_name)
    write(print_lun, *) &
        "memman_"//trim(free_del)//routine//"("//trim(vname)// &
        ") freed = "//trim(adjustl(a_del))// &
        " total = "// trim(adjustl(a_mem))
    return
  end subroutine memman_deallocate_msg

!---------------------------------------------------------
! Print memman_allocate messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_allocate_msg(routine, p_loc, v_loc, all, v_name)
    integer, intent(in)                   :: all
    character(len=*),intent(in)           :: routine
    character(len=*),intent(in), optional :: v_name
    integer, intent(in)                   :: v_loc 
    integer, intent(in)                   :: p_loc 

    character(len=20)                     :: a_vloc, a_ploc, a_all, a_mem
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.2) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc
    write(a_vloc,'(i20)') v_loc
    write(a_all,'(i20)') all
    write(a_mem,'(i20)') memman_get_memory_allocated()

    vname=trim(adjustl(a_ploc))//":"//trim(adjustl(a_vloc))//"-"// &
          trim(adjustl(a_all))//"-"//trim(a_name)
    write(print_lun, *) &
        "memman_all_"//routine//"("//trim(vname)// &
        ") alloc = "//trim(adjustl(a_all))// &
        " total = "// trim(adjustl(a_mem))
    return
  end subroutine memman_allocate_msg

!---------------------------------------------------------
! Print memman_reallocate messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_reallocate_msg(routine, p_loc, v_loc, all, v_name)
    integer, intent(in)                   :: all
    character(len=*),intent(in)           :: routine
    character(len=*),intent(in), optional :: v_name
    integer, intent(in)                   :: v_loc 
    integer, intent(in)                   :: p_loc 

    character(len=20)                     :: a_vloc, a_ploc, a_all, a_mem
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.2) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc
    write(a_vloc,'(i20)') v_loc
    write(a_all,'(i20)') all
    write(a_mem,'(i20)') memman_get_memory_allocated()

    vname=trim(adjustl(a_ploc))//":"//trim(adjustl(a_vloc))//"-"// &
          trim(adjustl(a_all))//"-"//trim(a_name)
    write(print_lun, *) &
        "memman_realloc_"//routine//"("//trim(vname)// &
        ") alloc = "//trim(adjustl(a_all))// &
        " total = "// trim(adjustl(a_mem))
    return
  end subroutine memman_reallocate_msg

!---------------------------------------------------------
! Print memman_nullify messages
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  subroutine memman_nullify_msg(p_loc, routine, v_name)
    integer, intent(in)                   :: p_loc
    character(len=*),intent(in)           :: routine
    character(len=*),intent(in), optional :: v_name

    character(len=20)                     :: a_ploc
    character(len=100)                    :: vname
    character(len=40)                     :: a_name

    if(print_sw.lt.2) return

    if(present(v_name)) then
      a_name=v_name
    else
      a_name=' '
    endif

    write(a_ploc,'(i20)') p_loc

    vname=trim(adjustl(a_ploc))//":"//trim(a_name)
    write(print_lun, *) "memman_nul_"//routine//"("//trim(vname)//")"
    return
  end subroutine memman_nullify_msg

!-----------------------------------------------------------------------------
! Get vname from either specified v_name or from previously allocated variable
!
! Written July 2002 by Charles C Burch
!-----------------------------------------------------------------------------
  subroutine memman_get_vname(v_name, p_loc, vname, ploc_present)
    character(len=*), intent(in),optional   :: v_name
    integer, intent(in)                     :: p_loc
    character(len=*), intent(out)           :: vname
    logical, intent(out)                    :: ploc_present
   
    integer                                 :: v_loc, v_size

    call memman_get_info(p_loc,v_loc,v_size,vname)
    ploc_present=v_size.ge.0
    
    if(present(v_name)) then
      if(.not.memman_is_string_blank(v_name)) vname=v_name
    endif

    return
  end  subroutine memman_get_vname

!-----------------------------------------------------------------------------
! Get vname from either specified v_name or from previously allocated variable
!
! Written July 2002 by Charles C Burch
!-----------------------------------------------------------------------------
  function memman_tracking_present(p_loc) result(stat)
    integer, intent(in)                     :: p_loc
    logical                                 :: stat
   
    integer                                 :: v_loc, v_size
    character(len=40)                       :: vname

    call memman_get_info(p_loc,v_loc,v_size,vname)
    stat=v_size.ge.0
    return
  end  function memman_tracking_present

!-----------------------------------------------------
! Allocate/deallocate/reallocate nullify routines
!
! written August 2002 by Charles C Burch
!-----------------------------------------------------

!------------------------------- Character array -------------------------
  subroutine memman_all_c1(v, n1, i_err, v_name)
    character(len=*), pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)
    
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_c1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)),stat=i_err)
    n_mem = max(1,n1) * len(v(1)) *sizeof(' ')

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"c1",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("c1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_c1

  subroutine memman_reall_c1(v, n1, i_err, v_name)
    character(len=*), pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: m1, lv, k1, l1, p_loc
    character,pointer                      :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_c1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v), &
          memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    lv=len(v(1))
    m1=min(m1,n1)
    allocate(temp(1:m1,1:lv),stat=i_err)
    if(i_err.ne.0) goto 900
 
    do k1=1,m1
      do l1=1,lv
        temp(k1,l1)=v(k1)(l1:l1)
      enddo
    enddo

    call memman_allocate(v,n1,i_err)
    if(i_err.eq.0) then
      do k1=1,m1
        do l1=1,lv
          v(k1)(l1:l1)=temp(k1,l1)
        enddo
      enddo
    endif  
    deallocate(temp,stat=k1)

900 call memman_restore_print_mode
    call memman_reallocate_msg("c1", p_loc, memman_loc(v), &
      memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_c1
    
  subroutine memman_del_c1(v, free)
    character(len=*), pointer              :: v(:)
    logical, intent(in), optional          :: free

    integer                                :: p_loc, v_loc, n_mem, istat
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_c1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("c1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif

    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif

    call memman_restore_print_mode
    call memman_deallocate_msg(free, "c1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_c1

  subroutine memman_nul_c1(v, v_name)
    character(len=*), pointer              :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("c1", p_loc, memman_loc(v), & 
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "c1", vname)
    return
  end subroutine memman_nul_c1

!------------------------------- Real array -------------------------
  subroutine memman_all_r1(v, n1, i_err, v_name)
    real, pointer                          :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_r1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)), stat=i_err)
    n_mem = max(1,n1) * sizeof(0.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"r1",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("r1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_r1

  subroutine memman_reall_r1(v, n1, i_err, v_name)
    real            , pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: m1, p_loc
    real            , pointer              :: temp(:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_r1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900     
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)
       i_err=0
       goto 900
    endif
   
    nullify(temp) 
    allocate(temp(n1), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    if(m1.gt.0) temp(1:m1)=v(1:m1)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("r1", p_loc, memman_loc(v), memman_sizeof(v), &
     vname)
    return
  end  subroutine memman_reall_r1
  
  subroutine memman_del_r1(v, free)
    real, pointer                         :: v(:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_r1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("r1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "r1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_r1

  subroutine memman_nul_r1(v, v_name)
    real, pointer                          :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("r1", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "r1", vname)
    return
  end subroutine memman_nul_r1

!--------------------------- Integer array --------------------
  subroutine memman_all_i1(v, n1, i_err, v_name)
    integer, pointer                       :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_i1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)), stat=i_err)
    n_mem = max(1,n1) * sizeof(0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else  
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"i1",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("i1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_i1

  subroutine memman_reall_i1(v, n1, i_err, v_name)
    integer         , pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: m1, p_loc
    integer         , pointer              :: temp(:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_i1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v),  &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    if(m1.gt.0) temp(1:m1)=v(1:m1)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i1", p_loc, memman_loc(v), &
      memman_sizeof(v),  vname)
    return
  end  subroutine memman_reall_i1
  
  subroutine memman_del_i1(v, free)
    integer, pointer                      :: v(:)
    logical, intent(in), optional         :: free
   
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_i1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("i1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "i1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_i1

  subroutine memman_nul_i1(v, v_name)
    integer, pointer                       :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("i1", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "i1", vname)
    return
  end subroutine memman_nul_i1

!------------------------------- complex array -------------------
  subroutine memman_all_z1(v, n1, i_err, v_name)
    complex, pointer                       :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_z1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)), stat=i_err)
    n_mem = max(1,n1) * sizeof(cmplx(0.,0.))

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"z1",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("z1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_z1

  subroutine memman_reall_z1(v, n1, i_err, v_name)
    complex         , pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: m1, p_loc
    complex         , pointer              :: temp(:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_z1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    if(m1.gt.0) temp(1:m1)=v(1:m1)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v), &
     memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("z1", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_z1
  
  subroutine memman_del_z1(v, free)
    complex, pointer                      :: v(:)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_z1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("z1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "z1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_z1

  subroutine memman_nul_z1(v, v_name)
    complex, pointer                       :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("z1", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "z1", vname)
    return
  end subroutine memman_nul_z1

!---------------------------- double precision array -------------------
  subroutine memman_all_d1(v, n1, i_err, v_name)
    double precision, pointer              :: v(:)
    integer                                :: n1
    integer                                :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_d1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)), stat=i_err)
    n_mem = max(1,n1) * sizeof(0.0d0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else  
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"d1",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("d1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_d1

  subroutine memman_reall_d1(v, n1, i_err, v_name)
    double precision, pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: m1, p_loc
    double precision, pointer              :: temp(:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_d1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    if(m1.gt.0) temp(1:m1)=v(1:m1)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("d1", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_d1
  
  subroutine memman_del_d1(v, free)
    double precision, pointer             :: v(:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_d1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("d1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "d1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_d1

  subroutine memman_nul_d1(v, v_name)
    double precision, pointer              :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("d1", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "d1", vname)
    return
  end subroutine memman_nul_d1

!------------------------ logical array ---------------------------
  subroutine memman_all_l1(v, n1, i_err, v_name)
    logical, pointer                       :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_l1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1)), stat=i_err)
    n_mem = max(1,n1) * sizeof(.true.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"l1",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("l1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_l1

  subroutine memman_reall_l1(v, n1, i_err, v_name)
    logical         , pointer              :: v(:)
    integer, intent(in)                    :: n1
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: m1, p_loc
    logical         , pointer              :: temp(:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_l1",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,i_err)
      goto 900
    endif
   
    m1=size(v)
    if(m1.eq.n1) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    if(m1.gt.0) temp(1:m1)=v(1:m1)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("l1", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_l1
  
  subroutine memman_del_l1(v, free)
    logical, pointer                      :: v(:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_l1",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("l1",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "l1", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_l1

  subroutine memman_nul_l1(v, v_name)
    logical, pointer                       :: v(:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("l1", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "l1", vname)
    return
  end subroutine memman_nul_l1

!-------------------------- real 2d ---------------------------
  subroutine memman_all_r2(v, n1, n2, i_err, v_name)
    real, pointer                          :: v(:, :)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_r2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * sizeof(0.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"r2",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("r2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_r2

  subroutine memman_reall_r2(v, n1, n2, i_err, v_name)
    real            , pointer              :: v(:,:)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2
    real            , pointer              :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_r2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1 .and. m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    if(m1.gt.0.and.m2.gt.0) temp(1:m1,1:m2)=v(1:m1,1:m2)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("r2", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_r2
  
  subroutine memman_del_r2(v, free)
    real, pointer                         :: v(:, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_r2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("r2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "r2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_r2

  subroutine memman_nul_r2(v, v_name)
    real, pointer                          :: v(:, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("r2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "r2", vname)
    return
  end subroutine memman_nul_r2

!---------------------- integer 2d ----------------------------
  subroutine memman_all_i2(v, n1, n2, i_err, v_name)
    integer, pointer                       :: v(:, :)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_i2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * sizeof(0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"i2",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("i2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_i2

  subroutine memman_reall_i2(v, n1, n2, i_err, v_name)
    integer         , pointer              :: v(:,:)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2
    integer         , pointer              :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_i2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1 .and. m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v), &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    if(m1.gt.0.and.m2.gt.0) temp(1:m1,1:m2)=v(1:m1,1:m2)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i2", p_loc, memman_loc(v), &
      memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_i2
  
  subroutine memman_del_i2(v, free)
    integer, pointer                      :: v(:, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_i2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("i2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "i2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_i2

  subroutine memman_nul_i2(v, v_name)
    integer, pointer                       :: v(:, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("i2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "i2", vname)
    return
  end subroutine memman_nul_i2

!------------------------- complex 2d -----------------------
  subroutine memman_all_z2(v, n1, n2, i_err, v_name)
    complex, pointer                       :: v(:, :)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_z2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * sizeof(cmplx(0.,0.))

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else   
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"z2",vname, n_mem)
      n_mem = 0
    end if  

    call memman_restore_print_mode
    call memman_allocate_msg("z2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_z2

  subroutine memman_reall_z2(v, n1, n2, i_err, v_name)
    complex         , pointer              :: v(:,:)
    integer, intent(in)                    :: n1,n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2
    complex         , pointer              :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_z2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1 .and. m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v),  &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    if(m1.gt.0.and.m2.gt.0) temp(1:m1,1:m2)=v(1:m1,1:m2)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v), &
      memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("z2", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_z2
  
  subroutine memman_del_z2(v, free)
    complex, pointer                      :: v(:, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_z2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("z2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "z2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_z2

  subroutine memman_nul_z2(v, v_name)
    complex, pointer                       :: v(:, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("z2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "z2", vname)
    return
  end subroutine memman_nul_z2

!-------------------------------- double 2d --------------------
  subroutine memman_all_d2(v, n1, n2, i_err, v_name)
    double precision, pointer              :: v(:, :)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_d2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * sizeof(0.0d0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else  
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"d2",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("d2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_d2

  subroutine memman_reall_d2(v, n1, n2, i_err, v_name)
    double precision, pointer              :: v(:,:)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2
    double precision, pointer              :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_d2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1 .and. m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    if(m1.gt.0.and.m2.gt.0) temp(1:m1,1:m2)=v(1:m1,1:m2)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("d2", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_d2
  
  subroutine memman_del_d2(v, free)
    double precision, pointer             :: v(:, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_d2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("d2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "d2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_d2

  subroutine memman_nul_d2(v, v_name)
    double precision, pointer              :: v(:, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("d2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "d2", vname)
    return
  end subroutine memman_nul_d2

!-------------------------------- logical2d --------------------
  subroutine memman_all_l2(v, n1, n2, i_err, v_name)
    logical         , pointer              :: v(:, :)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_l2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * sizeof(.true.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"l2",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("l2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_l2

  subroutine memman_reall_l2(v, n1, n2, i_err, v_name)
    logical         , pointer              :: v(:,:)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2
    logical         , pointer              :: temp(:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_l2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1 .and. m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v), &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    if(m1.gt.0.and.m2.gt.0) temp(1:m1,1:m2)=v(1:m1,1:m2)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("l2", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_l2
  
  subroutine memman_del_l2(v, free)
    logical         , pointer             :: v(:, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_l2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("l2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "l2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_l2

  subroutine memman_nul_l2(v, v_name)
    logical         , pointer              :: v(:, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("l2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "l2", vname)
    return
  end subroutine memman_nul_l2

!------------------------ character 2d -------------------------
  subroutine memman_all_c2(v, n1, n2, i_err, v_name)

    character(len=*), pointer              :: v(:,:)
    integer, intent(in)                    :: n1,n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_c2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1),max(1,n2)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * len(v(1,1)) * sizeof(' ')

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"c2",vname, n_mem)
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("c2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_c2

  subroutine memman_reall_c2(v, n1, n2, i_err, v_name)
    character(len=*), pointer              :: v(:,:)
    integer, intent(in)                    :: n1, n2
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: p_loc, m1, m2, lv, k1, k2, l1
    character,pointer                      :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_c2",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    if(m1.eq.n1.and.m2.eq.n2) then
       call memman_change_info(p_loc, memman_loc(v), & 
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    m1=min(m1,n1)
    m2=min(m2,n2)
    lv=len(v(1,1))
    allocate(temp(1:m1,1:m2,1:lv),stat=i_err)
    if(i_err.ne.0) goto 900
      
    do k1=1,m1
      do k2=1,m2
        do l1=1,lv
          temp(k1,k2,l1)=v(k1,k2)(l1:l1)
        enddo  
      enddo
    enddo
    
    call memman_allocate(v,n1,n2,i_err)
    if(i_err.eq.0) then
      do k1=1,m1
        do k2=1,m2
          do l1=1,lv
            v(k1,k2)(l1:l1)=temp(k1,k2,l1)
          enddo  
        enddo
      enddo
    endif  
    deallocate(temp,stat=k1)
    
900 call memman_restore_print_mode
    call memman_reallocate_msg("c2", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_c2
  
  subroutine memman_del_c2(v, free)
    character (len=*), pointer            :: v(:,:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_c2",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("c2",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "c2", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_c2

  subroutine memman_nul_c2(v, v_name)
    character (len=*), pointer             :: v(:,:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("c2", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "c2", vname)
    return
  end subroutine memman_nul_c2
 
!---------------------------- real 3d -------------------------
  subroutine memman_all_r3(v, n1, n2, n3, i_err, v_name)
    real, pointer                          :: v(:, :, :)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_r3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * sizeof(0.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"r3",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("r3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_r3

  subroutine memman_reall_r3(v, n1, n2, n3, i_err, v_name)
    real            , pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3
    real            , pointer              :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_r3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v), &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0) temp(1:m1,1:m2,1:m3)=v(1:m1,1:m2,1:m3)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("r3", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_r3
  
  subroutine memman_del_r3(v, free)
    real, pointer                         :: v(:, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_r3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("r3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "r3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_r3

  subroutine memman_nul_r3(v, v_name)
    real, pointer                          :: v(:, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("r3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "r3", vname)
    return
  end subroutine memman_nul_r3

!--------------------------- integer 3d ---------------------------
  subroutine memman_all_i3(v, n1, n2, n3, i_err, v_name)
    integer, pointer                       :: v(:, :, :)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_i3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * sizeof(0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"i3",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("i3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_i3

  subroutine memman_reall_i3(v, n1, n2, n3, i_err, v_name)
    integer         , pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3
    integer         , pointer              :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_i3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v), &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0) temp(1:m1,1:m2,1:m3)=v(1:m1,1:m2,1:m3)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i3", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_i3
  
  subroutine memman_del_i3(v, free)
    integer, pointer                      :: v(:, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_i3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("i3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "i3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_i3

  subroutine memman_nul_i3(v, v_name)
    integer, pointer                       :: v(:, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("i3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "i3", vname)
    return
  end subroutine memman_nul_i3

!------------------------- complex 3d --------------------------
  subroutine memman_all_z3(v, n1, n2, n3, i_err, v_name)
    complex, pointer                       :: v(:, :, :)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_z3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * sizeof(cmplx(0.,0.))

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"z3",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("z3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_z3

  subroutine memman_reall_z3(v, n1, n2, n3, i_err, v_name)
    complex         , pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3
    complex         , pointer              :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_z3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v),  &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0) temp(1:m1,1:m2,1:m3)=v(1:m1,1:m2,1:m3)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),  &
     memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("z3", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_z3
  
  subroutine memman_del_z3(v, free)
    complex, pointer                      :: v(:, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_z3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("z3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "z3", p_loc, v_loc, n_mem, vname)
    return
  
  end subroutine memman_del_z3

  subroutine memman_nul_z3(v, v_name)
    complex, pointer                       :: v(:, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("z3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "z3", vname)
    return
  end subroutine memman_nul_z3

!--------------------------- double 3d ---------------------------
  subroutine memman_all_d3(v, n1, n2, n3, i_err, v_name)
    double precision, pointer              :: v(:, :, :)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_d3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * sizeof(0.0d0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"d3",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("d3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_d3

  subroutine memman_reall_d3(v, n1, n2, n3, i_err, v_name)
    double precision, pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3
    double precision, pointer              :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_d3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0) temp(1:m1,1:m2,1:m3)=v(1:m1,1:m2,1:m3)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("d3", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_d3
  
  subroutine memman_del_d3(v, free)
    double precision, pointer             :: v(:, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_d3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("d3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "d3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_d3

  subroutine memman_nul_d3(v, v_name)
    double precision, pointer              :: v(:, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("d3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "d3", vname)
    return
  end subroutine memman_nul_d3

!-------------------------------- logical 3d --------------------
  subroutine memman_all_l3(v, n1, n2, n3, i_err, v_name)
    logical         , pointer              :: v(:, :, :)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_l3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * sizeof(.true.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"l3",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("l3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_l3

  subroutine memman_reall_l3(v, n1, n2, n3, i_err, v_name)
    logical         , pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3
    logical         , pointer              :: temp(:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_l3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0) temp(1:m1,1:m2,1:m3)=v(1:m1,1:m2,1:m3)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("l3", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_l3
  
  subroutine memman_del_l3(v, free)
    logical         , pointer             :: v(:, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_l3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("l3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "l3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_l3

  subroutine memman_nul_l3(v, v_name)
    logical         , pointer              :: v(:, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("l3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "l3", vname)
    return
  end subroutine memman_nul_l3

!------------------------ character 3d -------------------------
  subroutine memman_all_c3(v, n1, n2, n3, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1,n2,n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_c3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1),max(1,n2),max(1,n3)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * len(v(1,1,1)) * sizeof(' ')

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"c3",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("c3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_c3

  subroutine memman_reall_c3(v, n1, n2, n3, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:)
    integer, intent(in)                    :: n1, n2, n3
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: p_loc, m1,m2,m3, lv, k1,k2,k3, l1
    character,pointer                      :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_c3",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    if(m1.eq.n1.and.m2.eq.n2.and.m3.eq.n3) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    lv=len(v(1,1,1))
    allocate(temp(1:m1,1:m2,1:m3,1:lv),stat=i_err)
    if(i_err.ne.0) goto 900
      
    do k1=1,m1
      do k2=1,m2
        do k3=1,m3
          do l1=1,lv
            temp(k1,k2,k3,l1)=v(k1,k2,k3)(l1:l1)
          enddo  
        enddo  
      enddo
    enddo
    
    call memman_allocate(v,n1,n2,n3,i_err)
    if(i_err.eq.0) then
      do k1=1,m1
        do k2=1,m2
          do k3=1,m3
            do l1=1,lv
              v(k1,k2,k3)(l1:l1)=temp(k1,k2,k3,l1)
            enddo  
          enddo  
        enddo
      enddo
    endif  
    deallocate(temp,stat=k1)
    
900 call memman_restore_print_mode
    call memman_reallocate_msg("c3", p_loc, memman_loc(v), &
      memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_c3
  
  subroutine memman_del_c3(v, free)
    character (len=*), pointer            :: v(:,:,:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_c3",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("c3",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "c3", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_c3

  subroutine memman_nul_c3(v, v_name)
    character (len=*), pointer             :: v(:,:,:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("c3", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "c3", vname)
    return
  end subroutine memman_nul_c3
 
!----------------------------- real 4d --------------------------
  subroutine memman_all_r4(v, n1, n2, n3, n4, i_err, v_name)
    real, pointer                          :: v(:, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_r4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * sizeof(0.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"r4",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("r4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_r4

  subroutine memman_reall_r4(v, n1, n2, n3, n4, i_err, v_name)
    real            , pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3,m4
    real            , pointer              :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_r4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4) then
      call memman_change_info(p_loc, memman_loc(v), &
       memman_sizeof(v), vname)
      i_err=0
      goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4)=v(1:m1,1:m2,1:m3,1:m4)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("r4", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_r4
  
  subroutine memman_del_r4(v, free)
    real, pointer                         :: v(:, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_r4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("r4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "r4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_r4

  subroutine memman_nul_r4(v, v_name)
    real, pointer                          :: v(:, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("r4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "r4", vname)
    return
  end subroutine memman_nul_r4

!------------------------ integer 4d ----------------------------
  subroutine memman_all_i4(v, n1, n2, n3, n4, i_err, v_name)
    integer, pointer                       :: v(:, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_i4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * sizeof(0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"i4",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("i4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_i4

  subroutine memman_reall_i4(v, n1, n2, n3, n4, i_err, v_name)
    integer         , pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3,m4
    integer         , pointer              :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)
    
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_i4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4) then
      call memman_change_info(p_loc, memman_loc(v), &
       memman_sizeof(v), vname)
      i_err=0
      goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4)=v(1:m1,1:m2,1:m3,1:m4)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i4", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_i4
  
  subroutine memman_del_i4(v, free)
    integer, pointer                      :: v(:, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_i4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("i4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "i4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_i4

  subroutine memman_nul_i4(v, v_name)
    integer, pointer                       :: v(:, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("i4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "i4", vname)
    return
  end subroutine memman_nul_i4

!------------------------- complex 4d ---------------------------
  subroutine memman_all_z4(v, n1, n2, n3, n4, i_err, v_name)
    complex, pointer                       :: v(:, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_z4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * sizeof(cmplx(0.,0.))

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"z4",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("z4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_z4

  subroutine memman_reall_z4(v, n1, n2, n3, n4, i_err, v_name)
    complex         , pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3,m4
    complex         , pointer              :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_z4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4) then
      call memman_change_info(p_loc, memman_loc(v), &
          memman_sizeof(v), vname)
      i_err=0
      goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4)=v(1:m1,1:m2,1:m3,1:m4)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),  &
     memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("z4", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_z4
  
  subroutine memman_del_z4(v, free)
    complex, pointer                      :: v(:, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_z4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("z4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "z4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_z4

  subroutine memman_nul_z4(v, v_name)
    complex, pointer                       :: v(:, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("z4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "z4", vname)
    return
  end subroutine memman_nul_z4

!------------------------------- double 4d ------------------------
  subroutine memman_all_d4(v, n1, n2, n3, n4, i_err, v_name)
    double precision, pointer              :: v(:, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_d4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * sizeof(0.0d0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"d4",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("d4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_d4

  subroutine memman_reall_d4(v, n1, n2, n3, n4, i_err, v_name)
    double precision, pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3,m4
    double precision, pointer              :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_d4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4)=v(1:m1,1:m2,1:m3,1:m4)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("d4", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_d4
  
  subroutine memman_del_d4(v, free)
    double precision, pointer             :: v(:, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_d4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("d4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "d4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_d4

  subroutine memman_nul_d4(v, v_name)
    double precision, pointer              :: v(:, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("d4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "d4", vname)
    return
  end subroutine memman_nul_d4

!-------------------------------- logical 4d --------------------
  subroutine memman_all_l4(v, n1, n2, n3, n4, i_err, v_name)
    logical         , pointer              :: v(:, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_l4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * sizeof(.true.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"l4",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("l4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_l4

  subroutine memman_reall_l4(v, n1, n2, n3, n4, i_err, v_name)
    logical         , pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3, m4
    logical         , pointer              :: temp(:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_l4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4)=v(1:m1,1:m2,1:m3,1:m4)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("l4", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_l4
  
  subroutine memman_del_l4(v, free)
    logical         , pointer             :: v(:, :, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_l4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("l4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "l4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_l4

  subroutine memman_nul_l4(v, v_name)
    logical         , pointer              :: v(:, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("l4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "l4", vname)
    return
  end subroutine memman_nul_l4

!------------------------ character 4d -------------------------
  subroutine memman_all_c4(v, n1, n2, n3, n4, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_c4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1),max(1,n2),max(1,n3),max(1,n4)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * &
            len(v(1,1,1,1)) * sizeof(' ')

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"c4",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("c4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_c4

  subroutine memman_reall_c4(v, n1, n2, n3, n4, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:,:)
    integer, intent(in)                    :: n1, n2, n3, n4
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: p_loc, m1,m2,m3,m4 
    integer                                :: lv, k1,k2,k3,k4, l1
    character,pointer                      :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_c4",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    if(m1.eq.n1.and.m2.eq.n2.and.m3.eq.n3.and.m4.eq.n4) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    lv=len(v(1,1,1,1))
    allocate(temp(1:m1,1:m2,1:m3,1:m4,1:lv),stat=i_err)
    if(i_err.ne.0) goto 900
      
    do k1=1,m1
      do k2=1,m2
        do k3=1,m3
          do k4=1,m4
            do l1=1,lv
              temp(k1,k2,k3,k4,l1)=v(k1,k2,k3,k4)(l1:l1)
            enddo
          enddo  
        enddo  
      enddo
    enddo
    
    call memman_allocate(v,n1,n2,n3,n4,i_err)
    if(i_err.eq.0) then
      do k1=1,m1
        do k2=1,m2
          do k3=1,m3
            do k4=1,m4
              do l1=1,lv
                v(k1,k2,k3,k4)(l1:l1)=temp(k1,k2,k3,k4,l1)
              enddo
            enddo  
          enddo  
        enddo
      enddo
    endif  
    deallocate(temp,stat=k1)
    
900 call memman_restore_print_mode
    call memman_reallocate_msg("c4", p_loc, memman_loc(v), &
      memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_c4
  
  subroutine memman_del_c4(v, free)
    character (len=*), pointer            :: v(:,:,:,:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_c4",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("c4",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "c4", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_c4

  subroutine memman_nul_c4(v, v_name)
    character (len=*), pointer             :: v(:,:,:,:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("c4", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "c4", vname)
    return
  end subroutine memman_nul_c4
 
!----------------------------- real 5d ---------------------------
  subroutine memman_all_r5(v, n1, n2, n3, n4, n5, i_err, v_name)
    real, pointer                          :: v(:, :, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_r5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4), max(1,n5)), &
      stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * max(1,n5) * &
            sizeof(0.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"r5",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("r5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_r5

  subroutine memman_reall_r5(v, n1, n2, n3, n4, n5, i_err, v_name)
    real            , pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc,m1,m2,m3,m4,m5
    real            , pointer              :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_r5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4.and.m5.eq.n5) then
       call memman_change_info(p_loc, memman_loc(v), &
        memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4, n5), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0.and.m5.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4,1:m5)=v(1:m1,1:m2,1:m3,1:m4,1:m5)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("r5", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_r5
  
  subroutine memman_del_r5(v, free)
    real, pointer                         :: v(:, :, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_r5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem =  memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("r5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "r5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_r5

  subroutine memman_nul_r5(v, v_name)
    real, pointer                          :: v(:, :, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("r5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "r5", vname)
    return
  end subroutine memman_nul_r5

!-------------------------- integer 5d ----------------------
  subroutine memman_all_i5(v, n1, n2, n3, n4, n5, i_err, v_name)
    integer, pointer                       :: v(:, :, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_i5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4), max(1,n5)), &
     stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * max(1,n5) *&
            sizeof(0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"i5",vname, n_mem)
      n_mem = 0
    end if

    call memman_restore_print_mode
    call memman_allocate_msg("i5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_i5

  subroutine memman_reall_i5(v, n1, n2, n3, n4, n5, i_err, v_name)
    integer         , pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc,m1,m2,m3,m4,m5
    integer         , pointer              :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_i5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4.and.m5.eq.n5) then
      call memman_change_info(p_loc, memman_loc(v), &
       memman_sizeof(v), vname)
      i_err=0
      goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4, n5), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0.and.m5.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4,1:m5)=v(1:m1,1:m2,1:m3,1:m4,1:m5)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i5", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_i5
  
  subroutine memman_del_i5(v, free)
    integer, pointer                      :: v(:, :, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_i5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem =  memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("i5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "i5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_i5

  subroutine memman_nul_i5(v, v_name)
    integer, pointer                       :: v(:, :, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("i5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "i5", vname)
    return
  end subroutine memman_nul_i5

!--------------------------- complex 5d --------------------
  subroutine memman_all_z5(v, n1, n2, n3, n4, n5, i_err, v_name)
    complex, pointer                       :: v(:, :, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_z5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4), max(1,n5)), &
       stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * max(1,n5) * &
            sizeof(cmplx(0.,0.))

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"z5",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("z5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_z5

  subroutine memman_reall_z5(v, n1, n2, n3, n4, n5, i_err, v_name)
    complex         , pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc,m1,m2,m3,m4,m5
    complex         , pointer              :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_z5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
    call memman_save_print_mode(min(1,print_sw))
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4.and.m5.eq.n5) then
       call memman_change_info(p_loc, memman_loc(v),   &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4, n5), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0.and.m5.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4,1:m5)=v(1:m1,1:m2,1:m3,1:m4,1:m5)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v), &
     memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("i5", p_loc, memman_loc(v), &
     memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_z5
  
  subroutine memman_del_z5(v, free)
    complex, pointer                      :: v(:, :, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_z5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem =  memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("z5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "z5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_z5

  subroutine memman_nul_z5(v, v_name)
    complex, pointer                       :: v(:, :, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("z5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "z5", vname)
    return
  end subroutine memman_nul_z5

!------------------------- double 5d --------------------
  subroutine memman_all_d5(v, n1, n2, n3, n4, n5, i_err, v_name)
    double precision, pointer              :: v(:, :, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_d5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1), max(1,n2), max(1,n3), max(1,n4), max(1,n5)), &
      stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * max(1,n5) * &
            sizeof(0.0d0)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"d5",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("d5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_d5

  subroutine memman_reall_d5(v, n1, n2, n3, n4, n5, i_err, v_name)
    double precision, pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc,m1,m2,m3,m4,m5
    double precision, pointer              :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_d5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4.and.m5.eq.n5) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4, n5), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0.and.m5.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4,1:m5)=v(1:m1,1:m2,1:m3,1:m4,1:m5)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("d5", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_d5
  
  subroutine memman_del_d5(v, free)
    double precision, pointer             :: v(:, :, :, :, :)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_d5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem =  memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("d5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "d5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_d5

  subroutine memman_nul_d5(v, v_name)
    double precision, pointer              :: v(:, :, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("d5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "d5", vname)
    return
  end subroutine memman_nul_d5

!-------------------------------- logical 5d --------------------
  subroutine memman_all_l5(v, n1, n2, n3, n4, n5, i_err, v_name)
    logical         , pointer              :: v(:, :, :, :, :)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_l5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1),max(1,n2),max(1,n3),max(1,n4),max(1,n5)), stat=i_err)
    n_mem = max(1,n1)*max(1,n2)*max(1,n3)*max(1,n4)*max(1,n5)*sizeof(.true.)

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"l5",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("l5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_l5

  subroutine memman_reall_l5(v, n1, n2, n3, n4, n5, i_err, v_name)
    logical         , pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name
    
    integer                                :: p_loc, m1, m2, m3, m4, m5
    logical         , pointer              :: temp(:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_l5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1 .and. m2.eq.n2 .and. m3.eq.n3 .and. m4.eq.n4 .and. m5.eq.n5)then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    nullify(temp) 
    allocate(temp(n1, n2, n3, n4, n5), stat=i_err)
    if(i_err.ne.0) goto 900 

    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    if(m1.gt.0.and.m2.gt.0.and.m3.gt.0.and.m4.gt.0.and.m5.gt.0) &
      temp(1:m1,1:m2,1:m3,1:m4,1:m5)=v(1:m1,1:m2,1:m3,1:m4,1:m5)
    deallocate(v, stat=i_err)
    v=>temp
    call memman_change_info(p_loc, memman_loc(v),memman_sizeof(v),vname)

900 call memman_restore_print_mode
    call memman_reallocate_msg("l5", p_loc, memman_loc(v), &
     memman_sizeof(v),vname)
    return
  end  subroutine memman_reall_l5
  
  subroutine memman_del_l5(v, free)
    logical         , pointer             :: v(:, :, :, :, :)
    logical, intent(in), optional         :: free
    
    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_l5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("l5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "l5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_l5

  subroutine memman_nul_l5(v, v_name)
    logical         , pointer              :: v(:, :, :, :, :)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("l5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "l5", vname)
    return
  end subroutine memman_nul_l5

!------------------------ character 5d -------------------------
  subroutine memman_all_c5(v, n1, n2, n3, n4, n5, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1,n2,n3,n4,n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: n_mem, p_loc, v_loc
    character(len=40)                      :: vname
    logical                                :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"all_c5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) call memman_deallocate(v)

    allocate(v(max(1,n1),max(1,n2),max(1,n3),max(1,n4),max(1,n5)), stat=i_err)
    n_mem = max(1,n1) * max(1,n2) * max(1,n3) * max(1,n4) * max(1,n5) * &
            len(v(1,1,1,1,1)) * sizeof(' ')

    if (i_err .eq. 0) then
      v_loc=memman_loc(v(1,1,1,1,1))
      call memman_tracking_allocate(p_loc, v_loc, n_mem, vname)
    else 
      v_loc = 0
      call memman_allocate_error(p_loc,i_err,"c5",vname, n_mem)
      n_mem = 0
    end if 

    call memman_restore_print_mode
    call memman_allocate_msg("c5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_all_c5

  subroutine memman_reall_c5(v, n1, n2, n3, n4, n5, i_err, v_name)
    character(len=*), pointer              :: v(:,:,:,:,:)
    integer, intent(in)                    :: n1, n2, n3, n4, n5
    integer, intent(out)                   :: i_err
    character(len=*), intent(in), optional :: v_name

    integer                                :: p_loc, m1,m2,m3,m4,m5 
    integer                                :: lv, k1,k2,k3,k4,k5, l1
    character,pointer                      :: temp(:,:,:,:,:,:)
    character(len=40)                      :: vname
    logical                                :: ploc_present
    
    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"reall_c5",vname)
      call memman_nullify(v)
    endif
    
    if(.not.associated(v)) then
      call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
      goto 900
    endif
   
    m1=size(v,1)
    m2=size(v,2)
    m3=size(v,3)
    m4=size(v,4)
    m5=size(v,5)
    if(m1.eq.n1.and.m2.eq.n2.and.m3.eq.n3.and.m4.eq.n4.and.m5.eq.n5) then
       call memman_change_info(p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
       i_err=0
       goto 900
    endif
    
    m1=min(m1,n1)
    m2=min(m2,n2)
    m3=min(m3,n3)
    m4=min(m4,n4)
    m5=min(m5,n5)
    lv=len(v(1,1,1,1,1))
    allocate(temp(1:m1,1:m2,1:m3,1:m4,1:m5,1:lv),stat=i_err)
    if(i_err.ne.0) goto 900
      
    do k1=1,m1
      do k2=1,m2
        do k3=1,m3
          do k4=1,m4
            do k5=1,m5
              do l1=1,lv
                temp(k1,k2,k3,k4,k5,l1)=v(k1,k2,k3,k4,k5)(l1:l1)
              enddo
            enddo
          enddo  
        enddo  
      enddo
    enddo
    
    call memman_allocate(v,n1,n2,n3,n4,n5,i_err)
    if(i_err.eq.0) then
      do k1=1,m1
        do k2=1,m2
          do k3=1,m3
            do k4=1,m4
              do k5=1,m5
                do l1=1,lv
                  v(k1,k2,k3,k4,k5)(l1:l1)=temp(k1,k2,k3,k4,k5,l1)
                enddo
              enddo
            enddo  
          enddo  
        enddo
      enddo
    endif  
    deallocate(temp,stat=k1)
    
900 call memman_restore_print_mode
    call memman_reallocate_msg("c5", p_loc, memman_loc(v), &
      memman_sizeof(v), vname)
    return
  end  subroutine memman_reall_c5
  
  subroutine memman_del_c5(v, free)
    character (len=*), pointer            :: v(:,:,:,:,:)
    logical, intent(in), optional         :: free

    integer                               :: p_loc, v_loc, n_mem, istat
    character(len=40)                     :: vname
    logical                               :: ploc_present

    call memman_save_print_mode(min(1,print_sw))
    p_loc=memman_pointer_loc(v)
    call memman_get_vname("", p_loc, vname, ploc_present)
  
    if(.not.ploc_present) then
      call memman_nonullify_error(p_loc,"del_c5",vname)
      call memman_nullify(v)
    endif
    
    if(associated(v)) then
      v_loc=memman_loc(v(1,1,1,1,1))
      n_mem = memman_sizeof(v)
      call memman_tracking_deallocate(p_loc, v_loc, n_mem, vname)
      deallocate(v, stat=istat)
      if(istat.ne.0) &
       call memman_deallocate_error("c5",istat,p_loc,v_loc,n_mem,vname)
      call memman_nullify(v, vname)
    else
      v_loc = 0
      n_mem = 0
    endif
  
    if(present(free)) then
      if(free) call memman_tracking_free(p_loc)
    endif
    
    call memman_restore_print_mode
    call memman_deallocate_msg(free, "c5", p_loc, v_loc, n_mem, vname)
    return
  end subroutine memman_del_c5

  subroutine memman_nul_c5(v, v_name)
    character (len=*), pointer             :: v(:,:,:,:,:)
    character(len=*), intent(in), optional :: v_name

    character(len=40)                      :: vname
    integer                                :: p_loc
    logical                                :: ploc_present

    p_loc=memman_pointer_loc(v)
    call memman_get_vname(v_name, p_loc, vname, ploc_present)

    if(ploc_present) then
      if(associated(v)) then
        call memman_nullify_error("c5", p_loc, memman_loc(v),  &
         memman_sizeof(v), vname)
        call memman_save_print_mode(min(1,print_sw))
        call memman_deallocate(v)
        call memman_restore_print_mode
      endif
    endif

    call memman_nullify_variable(p_loc, vname)
    nullify(v)
    call memman_nullify_msg(p_loc, "c5", vname)
    return
  end subroutine memman_nul_c5

!-------------------------------------------------------------------
! memman_free routines: deallocate and remove from memman database
!
! Written September 2002 by Charles C Burch
!-------------------------------------------------------------------

  subroutine memman_free_c1(v) 
    character(len=*),pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_c1
  
  subroutine memman_free_c2(v) 
    character(len=*),pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_c2
  
  subroutine memman_free_c3(v) 
    character(len=*),pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_c3
  
  subroutine memman_free_c4(v) 
    character(len=*),pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_c4
  
  subroutine memman_free_c5(v) 
    character(len=*),pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_c5
  
  subroutine memman_free_i1(v) 
    integer         ,pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_i1
  
  subroutine memman_free_i2(v) 
    integer         ,pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_i2
  
  subroutine memman_free_i3(v) 
    integer         ,pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_i3
  
  subroutine memman_free_i4(v) 
    integer         ,pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_i4
  
  subroutine memman_free_i5(v) 
    integer         ,pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_i5
  
  subroutine memman_free_r1(v) 
    real            ,pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_r1
  
  subroutine memman_free_r2(v) 
    real            ,pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_r2
  
  subroutine memman_free_r3(v) 
    real            ,pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_r3
  
  subroutine memman_free_r4(v) 
    real            ,pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_r4
  
  subroutine memman_free_r5(v) 
    real            ,pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_r5
  
  subroutine memman_free_d1(v) 
    double precision,pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_d1
  
  subroutine memman_free_d2(v) 
    double precision,pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_d2
  
  subroutine memman_free_d3(v) 
    double precision,pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_d3
  
  subroutine memman_free_d4(v) 
    double precision,pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_d4
  
  subroutine memman_free_d5(v) 
    double precision,pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_d5
  
  subroutine memman_free_z1(v) 
    complex         ,pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_z1
  
  subroutine memman_free_z2(v) 
    complex         ,pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_z2
  
  subroutine memman_free_z3(v) 
    complex         ,pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_z3
  
  subroutine memman_free_z4(v) 
    complex         ,pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_z4
  
  subroutine memman_free_z5(v) 
    complex         ,pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_z5
  
  subroutine memman_free_l1(v) 
    logical         ,pointer            :: v(:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_l1
  
  subroutine memman_free_l2(v) 
    logical         ,pointer            :: v(:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_l2
  
  subroutine memman_free_l3(v) 
    logical         ,pointer            :: v(:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_l3
  
  subroutine memman_free_l4(v) 
    logical         ,pointer            :: v(:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_l4
  
  subroutine memman_free_l5(v) 
    logical         ,pointer            :: v(:,:,:,:,:)

    call memman_deallocate(v, free=.true.)
    return
  end subroutine memman_free_l5

!---------------------------------------------------------
! Check if if variable allocated and consistent
!
! Written July 2002 by Charles C Burch
!---------------------------------------------------------
  function memman_check_var(p_loc, v_loc, v_size, v_name) result(stat)
    integer, intent(in)          :: p_loc
    integer, intent(in)          :: v_loc
    integer, intent(in)          :: v_size
    character(len=*), intent(in) :: v_name
    
    integer                      :: stat
    character                    :: c_name(240)
   
    call memman_form_c_vname(v_name, c_name)
    stat=memman_check_variable_c(p_loc, v_loc, v_size, c_name(1))
    return
  end function memman_check_var

  function memman_check_c1(v, v_name) result(stat)      !character 1d
    character(len=*), pointer    :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_c1     

  function memman_check_c2(v, v_name) result(stat)      !character 2d
    character(len=*), pointer    :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_c2     

  function memman_check_c3(v, v_name) result(stat)      !character 3d
    character(len=*), pointer    :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_c3     

  function memman_check_c4(v, v_name) result(stat)      !character 4d
    character(len=*), pointer    :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_c4     

  function memman_check_c5(v, v_name) result(stat)      !character 5d
    character(len=*), pointer    :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_c5     

  function memman_check_i1(v, v_name) result(stat)      !integer 1d
    integer, pointer             :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_i1     

  function memman_check_i2(v, v_name) result(stat)      !integer 2d
    integer, pointer             :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
      memman_sizeof(v), v_name)
    return
  end function memman_check_i2     

  function memman_check_i3(v, v_name) result(stat)      !integer 3d
    integer, pointer             :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
      memman_sizeof(v), v_name)
    return
  end function memman_check_i3     

  function memman_check_i4(v, v_name) result(stat)      !integer 4d
    integer, pointer             :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_i4     

  function memman_check_i5(v, v_name) result(stat)      !integer 5d
    integer, pointer             :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_i5     

  function memman_check_r1(v, v_name) result(stat)      !real    1d
    real, pointer                :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
      memman_sizeof(v), v_name)
    return
  end function memman_check_r1     

  function memman_check_r2(v, v_name) result(stat)      !real 2d
    real, pointer                :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
      memman_sizeof(v), v_name)
    return
  end function memman_check_r2     

  function memman_check_r3(v, v_name) result(stat)      !real 3d
    real, pointer                :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_r3     

  function memman_check_r4(v, v_name) result(stat)      !real 4d
    real, pointer                :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_r4     

  function memman_check_r5(v, v_name) result(stat)      !real 5d
    real, pointer                :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_r5     

  function memman_check_d1(v, v_name) result(stat)      !double 1d
    double precision, pointer    :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_d1     

  function memman_check_d2(v, v_name) result(stat)      !double 2d
    double precision, pointer    :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_d2     

  function memman_check_d3(v, v_name) result(stat)      !double 3d
    double precision, pointer    :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_d3     

  function memman_check_d4(v, v_name) result(stat)      !double 4d
    double precision, pointer    :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_d4     

  function memman_check_d5(v, v_name) result(stat)      !double 5d
    double precision, pointer    :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_d5     


  function memman_check_z1(v, v_name) result(stat)      !complex    1d
    complex, pointer             :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_z1     

  function memman_check_z2(v, v_name) result(stat)      !complex 2d
    complex, pointer             :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v),  &
     memman_sizeof(v), v_name)
    return
  end function memman_check_z2     

  function memman_check_z3(v, v_name) result(stat)      !complex 3d
    complex, pointer             :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_z3     

  function memman_check_z4(v, v_name) result(stat)      !complex 4d
    complex, pointer             :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_z4     

  function memman_check_z5(v, v_name) result(stat)      !complex 5d
    complex, pointer             :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_z5     


  function memman_check_l1(v, v_name) result(stat)      !logical    1d
    logical, pointer             :: v(:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_l1     

  function memman_check_l2(v, v_name) result(stat)      !logical    2d
    logical, pointer             :: v(:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
     memman_sizeof(v), v_name)
    return
  end function memman_check_l2     

  function memman_check_l3(v, v_name) result(stat)      !logical    3d
    logical, pointer             :: v(:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_l3     

  function memman_check_l4(v, v_name) result(stat)      !logical    4d
    logical, pointer             :: v(:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_l4     

  function memman_check_l5(v, v_name) result(stat)      !logical    5d
    logical, pointer             :: v(:,:,:,:,:)
    character(len=*), intent(in) :: v_name
    integer                      :: stat

    stat=memman_check_var(memman_pointer_loc(v), memman_loc(v), &
      memman_sizeof(v), v_name)
    return
  end function memman_check_l5     

!---------------------------------------------------------------
! memman_loc functions return address of the argument
! 
! Written July 2002 by Charles C Burch
!---------------------------------------------------------------
  function memman_loc_char(v) result(v_loc)     !character
    character(len=*) :: v

    integer          :: v_loc

    v_loc=memman_loc_char_c(v(1:1))
    return
  end function memman_loc_char      

  function memman_loc_c1(v) result(v_loc)      !character 1d
    character(len=*) :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_char(v(1))
    return
  end function memman_loc_c1     

  function memman_loc_c2(v) result(v_loc)      !character 2d
    character(len=*) :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_char(v(1,1))
    return
  end function memman_loc_c2     

  function memman_loc_c3(v) result(v_loc)      !character 3d
    character(len=*) :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_char(v(1,1,1))
    return
  end function memman_loc_c3     

  function memman_loc_c4(v) result(v_loc)      !character 4d
    character(len=*) :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_char(v(1,1,1,1))
    return
  end function memman_loc_c4     

  function memman_loc_c5(v) result(v_loc)      !character 5d
    character(len=*) :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_char(v(1,1,1,1,1))
    return
  end function memman_loc_c5     

  function memman_loc_integer(v) result(v_loc)  !integer
    integer          ::  v

    integer          :: v_loc

    v_loc=memman_loc_integer_c(v)
    return
  end function memman_loc_integer     

  function memman_loc_i1(v) result(v_loc)      !integer 1d
    integer          :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_integer(v(1))
    return
  end function memman_loc_i1     

  function memman_loc_i2(v) result(v_loc)      !integer 2d
    integer          :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_integer(v(1,1))
    return
  end function memman_loc_i2     

  function memman_loc_i3(v) result(v_loc)      !integer 3d
    integer          :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_integer(v(1,1,1))
    return
  end function memman_loc_i3     

  function memman_loc_i4(v) result(v_loc)      !integer 4d
    integer          :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_integer(v(1,1,1,1))
    return
  end function memman_loc_i4     

  function memman_loc_i5(v) result(v_loc)      !integer 5d
    integer          :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_integer(v(1,1,1,1,1))
    return
  end function memman_loc_i5     

  function memman_loc_real(v) result(v_loc)    !real
    real             :: v

    integer          :: v_loc

    v_loc=memman_loc_real_c(v)
    return
  end function memman_loc_real      

  function memman_loc_r1(v) result(v_loc)      !real    1d
    real             :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_real(v(1))
    return
  end function memman_loc_r1     

  function memman_loc_r2(v) result(v_loc)      !real 2d
    real             :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_real(v(1,1))
    return
  end function memman_loc_r2     

  function memman_loc_r3(v) result(v_loc)      !real 3d
    real             :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_real(v(1,1,1))
    return
  end function memman_loc_r3     

  function memman_loc_r4(v) result(v_loc)      !real 4d
    real             :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_real(v(1,1,1,1))
    return
  end function memman_loc_r4     

  function memman_loc_r5(v) result(v_loc)      !real 5d
    real             :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_real(v(1,1,1,1,1))
    return
  end function memman_loc_r5     

  function memman_loc_double(v) result(v_loc)  !double
    double precision :: v

    integer          :: v_loc

    v_loc=memman_loc_double_c(v)
    return
  end function memman_loc_double      

  function memman_loc_d1(v) result(v_loc)      !double 1d
    double precision :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_double(v(1))
    return
  end function memman_loc_d1     

  function memman_loc_d2(v) result(v_loc)      !double 2d
    double precision :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_double(v(1,1))
    return
  end function memman_loc_d2     

  function memman_loc_d3(v) result(v_loc)      !double 3d
    double precision :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_double(v(1,1,1))
    return
  end function memman_loc_d3     

  function memman_loc_d4(v) result(v_loc)      !double 4d
    double precision :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_double(v(1,1,1,1))
    return
  end function memman_loc_d4     

  function memman_loc_d5(v) result(v_loc)      !double 5d
    double precision :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_double(v(1,1,1,1,1))
    return
  end function memman_loc_d5     


  function memman_loc_complex(v) result(v_loc) !complex
    complex          :: v

    integer          :: v_loc

    v_loc=memman_loc_complex_c(v)
    return
  end function memman_loc_complex      

  function memman_loc_z1(v) result(v_loc)      !complex    1d
    complex          :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_complex(v(1))
    return
  end function memman_loc_z1     

  function memman_loc_z2(v) result(v_loc)      !complex 2d
    complex          :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_complex(v(1,1))
    return
  end function memman_loc_z2     

  function memman_loc_z3(v) result(v_loc)      !complex 3d
    complex          :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_complex(v(1,1,1))
    return
  end function memman_loc_z3     

  function memman_loc_z4(v) result(v_loc)      !complex 4d
    complex          :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_complex(v(1,1,1,1))
    return
  end function memman_loc_z4     

  function memman_loc_z5(v) result(v_loc)      !complex 5d
    complex          :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_complex(v(1,1,1,1,1))
    return
  end function memman_loc_z5     


  function memman_loc_logical(v) result(v_loc) !logical
    logical          :: v

    integer          :: v_loc

    v_loc=memman_loc_logical_c(v)
    return
  end function memman_loc_logical      

  function memman_loc_l1(v) result(v_loc)      !logical    1d
    logical          :: v(:)

    integer          :: v_loc

    v_loc=memman_loc_logical(v(1))
    return
  end function memman_loc_l1     

  function memman_loc_l2(v) result(v_loc)      !logical    2d
    logical          :: v(:,:)

    integer          :: v_loc

    v_loc=memman_loc_logical(v(1,1))
    return
  end function memman_loc_l2     

  function memman_loc_l3(v) result(v_loc)      !logical    3d
    logical          :: v(:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_logical(v(1,1,1))
    return
  end function memman_loc_l3     

  function memman_loc_l4(v) result(v_loc)      !logical    4d
    logical          :: v(:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_logical(v(1,1,1,1))
    return
  end function memman_loc_l4     

  function memman_loc_l5(v) result(v_loc)      !logical    5d
    logical          :: v(:,:,:,:,:)

    integer          :: v_loc

    v_loc=memman_loc_logical(v(1,1,1,1,1))
    return
  end function memman_loc_l5     

!------------------------------------------------------------
! get info associated with an allocated variable
!
! Written July 2002 by Charles C Burch
!------------------------------------------------------------
  subroutine memman_get_inf(p_loc, v_loc, v_size, v_name)
    integer, intent(in)           :: p_loc
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name

    character                     :: c_name(240)
    integer                       :: i,n

    v_name=' '
    if(p_loc.le.0) return
      v_loc=0  !-- to make ifort happy that the var is set.
      v_size=0 !-- to make ifort happy that the var is set.

    call memman_get_info_c(p_loc, v_loc, v_size, c_name(1))
    n=min(len(v_name),size(c_name))
    do i=1, n
      if(c_name(i).eq.char(0)) exit
      v_name(i:i)=c_name(i)
    enddo

    return
  end subroutine memman_get_inf

  subroutine memman_get_inf_c1(v, p_loc, v_loc, v_size, v_name)
    character(len=*),pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_c1

  subroutine memman_get_inf_c2(v, p_loc, v_loc, v_size, v_name)
    character(len=*),pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_c2

  subroutine memman_get_inf_c3(v, p_loc, v_loc, v_size, v_name)
    character(len=*),pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_c3

  subroutine memman_get_inf_c4(v, p_loc, v_loc, v_size, v_name)
    character(len=*),pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_c4

  subroutine memman_get_inf_c5(v, p_loc, v_loc, v_size, v_name)
    character(len=*),pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_c5

  subroutine memman_get_inf_l1(v, p_loc, v_loc, v_size, v_name)
    logical         ,pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_l1

  subroutine memman_get_inf_l2(v, p_loc, v_loc, v_size, v_name)
    logical         ,pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_l2

  subroutine memman_get_inf_l3(v, p_loc, v_loc, v_size, v_name)
    logical         ,pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_l3

  subroutine memman_get_inf_l4(v, p_loc, v_loc, v_size, v_name)
    logical         ,pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_l4

  subroutine memman_get_inf_l5(v, p_loc, v_loc, v_size, v_name)
    logical         ,pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_l5

  subroutine memman_get_inf_i1(v, p_loc, v_loc, v_size, v_name)
    integer         ,pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_i1

  subroutine memman_get_inf_i2(v, p_loc, v_loc, v_size, v_name)
    integer         ,pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_i2

  subroutine memman_get_inf_i3(v, p_loc, v_loc, v_size, v_name)
    integer         ,pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_i3

  subroutine memman_get_inf_i4(v, p_loc, v_loc, v_size, v_name)
    integer         ,pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_i4

  subroutine memman_get_inf_i5(v, p_loc, v_loc, v_size, v_name)
    integer         ,pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_i5

  subroutine memman_get_inf_r1(v, p_loc, v_loc, v_size, v_name)
    real            ,pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_r1

  subroutine memman_get_inf_r2(v, p_loc, v_loc, v_size, v_name)
    real            ,pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_r2

  subroutine memman_get_inf_r3(v, p_loc, v_loc, v_size, v_name)
    real            ,pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_r3

  subroutine memman_get_inf_r4(v, p_loc, v_loc, v_size, v_name)
    real            ,pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_r4

  subroutine memman_get_inf_r5(v, p_loc, v_loc, v_size, v_name)
    real            ,pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_r5

  subroutine memman_get_inf_d1(v, p_loc, v_loc, v_size, v_name)
    double precision,pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_d1

  subroutine memman_get_inf_d2(v, p_loc, v_loc, v_size, v_name)
    double precision,pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_d2

  subroutine memman_get_inf_d3(v, p_loc, v_loc, v_size, v_name)
    double precision,pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_d3

  subroutine memman_get_inf_d4(v, p_loc, v_loc, v_size, v_name)
    double precision,pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_d4

  subroutine memman_get_inf_d5(v, p_loc, v_loc, v_size, v_name)
    double precision,pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_d5

  subroutine memman_get_inf_z1(v, p_loc, v_loc, v_size, v_name)
    complex         ,pointer      :: v(:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_z1

  subroutine memman_get_inf_z2(v, p_loc, v_loc, v_size, v_name)
    complex         ,pointer      :: v(:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_z2

  subroutine memman_get_inf_z3(v, p_loc, v_loc, v_size, v_name)
    complex         ,pointer      :: v(:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_z3

  subroutine memman_get_inf_z4(v, p_loc, v_loc, v_size, v_name)
    complex         ,pointer      :: v(:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_z4

  subroutine memman_get_inf_z5(v, p_loc, v_loc, v_size, v_name)
    complex         ,pointer      :: v(:,:,:,:,:)
    integer, intent(out)          :: v_loc
    integer, intent(out)          :: v_size
    character(len=*), intent(out) :: v_name
    integer, intent(out)          :: p_loc

    p_loc=memman_pointer_loc(v)
    call memman_get_inf(p_loc, v_loc, v_size, v_name)
    return
  end subroutine memman_get_inf_z5

!-----------------------------------------------------
! pointer location routines
!
! written August 2002 by Charles C Burch
!-----------------------------------------------------

  function memman_ptrloc_c1(v) result(add)
    character(len=*),pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_c1_c(v)
    return
  end function memman_ptrloc_c1
  
  function memman_ptrloc_c2(v) result(add)
    character(len=*),pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_c2_c(v)
    return
  end function memman_ptrloc_c2
  
  function memman_ptrloc_c3(v) result(add)
    character(len=*),pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_c3_c(v)
    return
  end function memman_ptrloc_c3
  
  function memman_ptrloc_c4(v) result(add)
    character(len=*),pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_c4_c(v)
    return
  end function memman_ptrloc_c4
  
  function memman_ptrloc_c5(v) result(add)
    character(len=*),pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_c5_c(v)
    return
  end function memman_ptrloc_c5
  
  function memman_ptrloc_i1(v) result(add)
    integer         ,pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_i1_c(v)
    return
  end function memman_ptrloc_i1
  
  function memman_ptrloc_i2(v) result(add)
    integer         ,pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_i2_c(v)
    return
  end function memman_ptrloc_i2
  
  function memman_ptrloc_i3(v) result(add)
    integer         ,pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_i3_c(v)
    return
  end function memman_ptrloc_i3
  
  function memman_ptrloc_i4(v) result(add)
    integer         ,pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_i4_c(v)
    return
  end function memman_ptrloc_i4
  
  function memman_ptrloc_i5(v) result(add)
    integer         ,pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_i5_c(v)
    return
  end function memman_ptrloc_i5
  
  function memman_ptrloc_r1(v) result(add)
    real            ,pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_r1_c(v)
    return
  end function memman_ptrloc_r1
  
  function memman_ptrloc_r2(v) result(add)
    real            ,pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_r2_c(v)
    return
  end function memman_ptrloc_r2
  
  function memman_ptrloc_r3(v) result(add)
    real            ,pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_r3_c(v)
    return
  end function memman_ptrloc_r3
  
  function memman_ptrloc_r4(v) result(add)
    real            ,pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_r4_c(v)
    return
  end function memman_ptrloc_r4
  
  function memman_ptrloc_r5(v) result(add)
    real            ,pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_r5_c(v)
    return
  end function memman_ptrloc_r5
  
  function memman_ptrloc_d1(v) result(add)
    double precision,pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_d1_c(v)
    return
  end function memman_ptrloc_d1
  
  function memman_ptrloc_d2(v) result(add)
    double precision,pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_d2_c(v)
    return
  end function memman_ptrloc_d2
  
  function memman_ptrloc_d3(v) result(add)
    double precision,pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_d3_c(v)
    return
  end function memman_ptrloc_d3
  
  function memman_ptrloc_d4(v) result(add)
    double precision,pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_d4_c(v)
    return
  end function memman_ptrloc_d4
  
  function memman_ptrloc_d5(v) result(add)
    double precision,pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_d5_c(v)
    return
  end function memman_ptrloc_d5
  
  function memman_ptrloc_z1(v) result(add)
    complex         ,pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_z1_c(v)
    return
  end function memman_ptrloc_z1
  
  function memman_ptrloc_z2(v) result(add)
    complex         ,pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_z2_c(v)
    return
  end function memman_ptrloc_z2
  
  function memman_ptrloc_z3(v) result(add)
    complex         ,pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_z3_c(v)
    return
  end function memman_ptrloc_z3
  
  function memman_ptrloc_z4(v) result(add)
    complex         ,pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_z4_c(v)
    return
  end function memman_ptrloc_z4
  
  function memman_ptrloc_z5(v) result(add)
    complex         ,pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_z5_c(v)
    return
  end function memman_ptrloc_z5
  
  function memman_ptrloc_l1(v) result(add)
    logical         ,pointer            :: v(:)
    integer                             :: add

    add=memman_ptrloc_l1_c(v)
    return
  end function memman_ptrloc_l1
  
  function memman_ptrloc_l2(v) result(add)
    logical         ,pointer            :: v(:,:)
    integer                             :: add

    add=memman_ptrloc_l2_c(v)
    return
  end function memman_ptrloc_l2
  
  function memman_ptrloc_l3(v) result(add)
    logical         ,pointer            :: v(:,:,:)
    integer                             :: add

    add=memman_ptrloc_l3_c(v)
    return
  end function memman_ptrloc_l3
  
  function memman_ptrloc_l4(v) result(add)
    logical         ,pointer            :: v(:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_l4_c(v)
    return
  end function memman_ptrloc_l4
  
  function memman_ptrloc_l5(v) result(add)
    logical         ,pointer            :: v(:,:,:,:,:)
    integer                             :: add

    add=memman_ptrloc_l5_c(v)
    return
  end function memman_ptrloc_l5

!-----------------------------------------------------
! sizeof in bytes of non-single-element arguments
!
! written August 2002 by Charles C Burch
!-----------------------------------------------------

  function memman_sizeof_i0(d) result(size_of)
    integer, intent(in) :: d
    integer             :: size_of

    size_of = sizeof(1)
    return
  end function memman_sizeof_i0

  function memman_sizeof_i1(d) result(size_of)
    integer, intent(in) :: d(:)
    integer             :: size_of

    size_of = size(d) * sizeof(1)
    return
  end function memman_sizeof_i1

  function memman_sizeof_i2(d) result(size_of)
    integer, intent(in) :: d(:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(1)
    return
  end function memman_sizeof_i2

  function memman_sizeof_i3(d) result(size_of)
    integer, intent(in) :: d(:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(1)
    return
  end function memman_sizeof_i3

  function memman_sizeof_i4(d) result(size_of)
    integer, intent(in) :: d(:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(1)
    return
  end function memman_sizeof_i4

  function memman_sizeof_i5(d) result(size_of)
    integer, intent(in) :: d(:,:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(1)
    return
  end function memman_sizeof_i5

  function memman_sizeof_r0(d) result(size_of)
    real, intent(in) :: d
    integer          :: size_of

    size_of = sizeof(1.0)
    return
  end function memman_sizeof_r0

  function memman_sizeof_r1(d) result(size_of)
    real, intent(in) :: d(:)
    integer          :: size_of

    size_of = size(d) * sizeof(1.0)
    return
  end function memman_sizeof_r1

  function memman_sizeof_r2(d) result(size_of)
    real, intent(in) :: d(:,:)
    integer          :: size_of

    size_of = size(d) * sizeof(1.0)
    return
  end function memman_sizeof_r2

  function memman_sizeof_r3(d) result(size_of)
    real, intent(in) :: d(:,:,:)
    integer          :: size_of

    size_of = size(d) * sizeof(1.0)
    return
  end function memman_sizeof_r3

  function memman_sizeof_r4(d) result(size_of)
    real, intent(in) :: d(:,:,:,:)
    integer          :: size_of

    size_of = size(d) * sizeof(1.0)
    return
  end function memman_sizeof_r4

  function memman_sizeof_r5(d) result(size_of)
    real, intent(in) :: d(:,:,:,:,:)
    integer          :: size_of

    size_of = size(d) * sizeof(1.0)
    return
  end function memman_sizeof_r5

  function memman_sizeof_d0(d) result(size_of)
    double precision, intent(in) :: d
    integer                      :: size_of

    size_of = sizeof(1.0d0)
    return
  end function memman_sizeof_d0

  function memman_sizeof_d1(d) result(size_of)
    double precision, intent(in) :: d(:)
    integer                      :: size_of

    size_of = size(d) * sizeof(1.0d0)
    return
  end function memman_sizeof_d1

  function memman_sizeof_d2(d) result(size_of)
    double precision, intent(in) :: d(:,:)
    integer                      :: size_of

    size_of = size(d) * sizeof(1.0d0)
    return
  end function memman_sizeof_d2

  function memman_sizeof_d3(d) result(size_of)
    double precision, intent(in) :: d(:,:,:)
    integer                      :: size_of

    size_of = size(d) * sizeof(1.0d0)
    return
  end function memman_sizeof_d3

  function memman_sizeof_d4(d) result(size_of)
    double precision, intent(in) :: d(:,:,:,:)
    integer                      :: size_of

    size_of = size(d) * sizeof(1.0d0)
    return
  end function memman_sizeof_d4

  function memman_sizeof_d5(d) result(size_of)
    double precision, intent(in) :: d(:,:,:,:,:)
    integer                      :: size_of

    size_of = size(d) * sizeof(1.0d0)
    return
  end function memman_sizeof_d5

  function memman_sizeof_z0(d) result(size_of)
    complex, intent(in) :: d
    integer             :: size_of

    size_of = sizeof(cmplx(1.))
    return
  end function memman_sizeof_z0

  function memman_sizeof_z1(d) result(size_of)
    complex, intent(in) :: d(:)
    integer             :: size_of

    size_of = size(d) * sizeof(cmplx(1.))
    return
  end function memman_sizeof_z1

  function memman_sizeof_z2(d) result(size_of)
    complex, intent(in) :: d(:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(cmplx(1.))
    return
  end function memman_sizeof_z2

  function memman_sizeof_z3(d) result(size_of)
    complex, intent(in) :: d(:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(cmplx(1.))
    return
  end function memman_sizeof_z3

  function memman_sizeof_z4(d) result(size_of)
    complex, intent(in) :: d(:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(cmplx(1.))
    return
  end function memman_sizeof_z4

  function memman_sizeof_z5(d) result(size_of)
    complex, intent(in) :: d(:,:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(cmplx(1.))
    return
  end function memman_sizeof_z5

  function memman_sizeof_l0(d) result(size_of)
    logical, intent(in) :: d
    integer             :: size_of

    size_of = sizeof(.true.)
    return
  end function memman_sizeof_l0

  function memman_sizeof_l1(d) result(size_of)
    logical, intent(in) :: d(:)
    integer             :: size_of

    size_of = size(d) * sizeof(.true.)
    return
  end function memman_sizeof_l1

  function memman_sizeof_l2(d) result(size_of)
    logical, intent(in) :: d(:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(.true.)
    return
  end function memman_sizeof_l2

  function memman_sizeof_l3(d) result(size_of)
    logical, intent(in) :: d(:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(.true.)
    return
  end function memman_sizeof_l3

  function memman_sizeof_l4(d) result(size_of)
    logical, intent(in) :: d(:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(.true.)
    return
  end function memman_sizeof_l4

  function memman_sizeof_l5(d) result(size_of)
    logical, intent(in) :: d(:,:,:,:,:)
    integer             :: size_of

    size_of = size(d) * sizeof(.true.)
    return
  end function memman_sizeof_l5

  function memman_sizeof_c0  (d) result (size_of)
    character(len=*),intent(in)   :: d
    integer                       :: size_of

    size_of = len(d) * sizeof(' ')
  end function memman_sizeof_c0

  function memman_sizeof_c1(d) result(size_of)
    character(len=*), intent(in) :: d(:)
    integer                      :: size_of

    size_of = size(d) * len(d(1)) * sizeof(' ')
    return
  end function memman_sizeof_c1

  function memman_sizeof_c2(d) result(size_of)
    character(len=*), intent(in) :: d(:,:)
    integer                      :: size_of

    size_of = size(d) * len(d(1,1)) * sizeof(' ')
    return
  end function memman_sizeof_c2

  function memman_sizeof_c3(d) result(size_of)
    character(len=*), intent(in) :: d(:,:,:)
    integer                      :: size_of

    size_of = size(d) * len(d(1,1,1)) * sizeof(' ')
    return
  end function memman_sizeof_c3

  function memman_sizeof_c4(d) result(size_of)
    character(len=*), intent(in) :: d(:,:,:,:)
    integer                      :: size_of

    size_of = size(d) * len(d(1,1,1,1)) * sizeof(' ')
    return
  end function memman_sizeof_c4

  function memman_sizeof_c5(d) result(size_of)
    character(len=*), intent(in) :: d(:,:,:,:,:)
    integer                      :: size_of

    size_of = size(d) * len(d(1,1,1,1,1)) * sizeof(' ')
    return
  end function memman_sizeof_c5


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

end module memman_module

! -------------------------------- Test Driver ------------------------------

! Basic test driver for memman.f90 written September 2002 by CCB
!program main
!  use memman_module
!  implicit none
!
!  character(len=24)        :: v_name
!  integer                  :: ierr, i, v_size, v_loc, p_loc
!
!  character(len=5),pointer :: c1(:),c2(:,:),c3(:,:,:),c4(:,:,:,:),c5(:,:,:,:,:)
!  integer, pointer         :: i1(:),i2(:,:),i3(:,:,:),i4(:,:,:,:),i5(:,:,:,:,:)
!  real, pointer            :: r1(:),r2(:,:),r3(:,:,:),r4(:,:,:,:),r5(:,:,:,:,:)
!  double precision,pointer :: d1(:),d2(:,:),d3(:,:,:),d4(:,:,:,:),d5(:,:,:,:,:)
!  complex, pointer         :: z1(:),z2(:,:),z3(:,:,:),z4(:,:,:,:),z5(:,:,:,:,:)
!  logical, pointer         :: l1(:),l2(:,:),l3(:,:,:),l4(:,:,:,:),l5(:,:,:,:,:)
!
!  real, pointer            :: r_test(:)
!  
!  character(len=5)         :: c0
!  integer                  :: i0
!  real                     :: r0
!  double precision         :: d0
!  complex                  :: z0
!  logical                  :: l0
!
!  print *,"memman tests"
!  call memman_set_print_mode(2)
!
!  print *,""
!  print *,"basic r-test tests"
!  call memman_allocate(r_test,10,ierr, "r_test")
!  r_test=123
!  call memman_reallocate(r_test,1000,ierr)
!  call memman_reallocate(r_test,1000,ierr)
!  call memman_allocate(r_test,1000000,ierr)
!  call memman_nullify(r_test,"r_test")
!
!  print *," "
!  print *,"Null names tests"
!  call memman_reallocate(r_test,10,ierr)
!  call memman_allocate(r_test,1000000,ierr)
!  call memman_deallocate(r_test)
!  call memman_exit_tracking(1)
!
!  call memman_nullify(c1,"c1")
!  call memman_nullify(c2,"c2")
!  call memman_nullify(c3,"c3")
!  call memman_nullify(c4,"c4")
!  call memman_nullify(c5,"c5")
!
!  call memman_nullify(i1,"i1")
!  call memman_nullify(i2,"i2")
!  call memman_nullify(i3,"i3")
!  call memman_nullify(i4,"i4")
!  call memman_nullify(i5,"i5")
!
!  call memman_nullify(r1,"r1")
!  call memman_nullify(r2,"r2")
!  call memman_nullify(r3,"r3")
!  call memman_nullify(r4,"r4")
!  call memman_nullify(r5,"r5")
!
!  call memman_nullify(d1,"d1")
!  call memman_nullify(d2,"d2")
!  call memman_nullify(d3,"d3")
!  call memman_nullify(d4,"d4")
!  call memman_nullify(d5,"d5")
!
!  call memman_nullify(z1,"z1")
!  call memman_nullify(z2,"z2")
!  call memman_nullify(z3,"z3")
!  call memman_nullify(z4,"z4")
!  call memman_nullify(z5,"z5")
!
!  call memman_nullify(l1,"l1")
!  call memman_nullify(l2,"l2")
!  call memman_nullify(l3,"l3")
!  call memman_nullify(l4,"l4")
!  call memman_nullify(l5,"l5")
!
!  print *," "
!  print *,"Character tests"
!  print *," "
!  call memman_allocate(c1,10,ierr)
!  print *,"memman_loc=",memman_loc(c1)
!  call memman_deallocate(c1)
!  call memman_allocate(c1,0,ierr)
!  c1='dummy'
!  call memman_reallocate(c1,10,ierr)
!  if(c1(1).ne."dummy") print *,"realloc_c1 error"
!  call memman_deallocate(c1)
!
!  print *," "
!  call memman_allocate(c2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(c2)
!  call memman_deallocate(c2)
!  call memman_allocate(c2,0,0,ierr)
!  c2='dummy'
!  call memman_reallocate(c2,10,10,ierr)
!  if(c2(1,1).ne."dummy") print *,"realloc_c2 error"
!  call memman_deallocate(c2)
!
!  print *," "
!  call memman_allocate(c3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(c3)
!  call memman_deallocate(c3)
!  call memman_allocate(c3,0,0,0,ierr)
!  c3='dummy'
!  call memman_reallocate(c3,10,10,10,ierr)
!  if(c3(1,1,1).ne."dummy") print *,"realloc_c3 error"
!  call memman_deallocate(c3)
!
!  print *," "
!  call memman_allocate(c4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(c4)
!  call memman_deallocate(c4)
!  call memman_allocate(c4,0,0,0,0,ierr)
!  c4='dummy'
!  call memman_reallocate(c4,10,10,10,10,ierr)
!  if(c4(1,1,1,1).ne."dummy") print *,"realloc_c4 error"
!  call memman_deallocate(c4)
!
!  print *," "
!  call memman_allocate(c5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(c5)
!  call memman_deallocate(c5)
!  call memman_allocate(c5,0,0,0,0,0,ierr)
!  c5='dummy'
!  call memman_reallocate(c5,10,10,10,10,10,ierr)
!  if(c5(1,1,1,1,1).ne."dummy") print *,"realloc_c5 error"
!  call memman_deallocate(c5)
!
!  print *," "
!  print *,"Logical tests"
!  print *," "
!  call memman_allocate(l1,10,ierr)
!  print *,"memman_loc=",memman_loc(l1)
!  call memman_deallocate(l1)
!  call memman_allocate(l1,0,ierr)
!  l1=.true.
!  call memman_reallocate(l1,10,ierr)
!  if(.not.l1(1)) print *,"realloc_l1 error"
!  call memman_deallocate(l1)
!
!  print *," "
!  call memman_allocate(l2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(l2)
!  call memman_deallocate(l2)
!  call memman_allocate(l2,0,0,ierr)
!  l2=.true.
!  call memman_reallocate(l2,10,10,ierr)
!  if(.not.l2(1,1)) print *,"realloc_l2 error"
!  call memman_deallocate(l2)
!
!  print *," "
!  call memman_allocate(l3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(l3)
!  call memman_deallocate(l3)
!  call memman_allocate(l3,0,0,0,ierr)
!  l3=.true.
!  call memman_reallocate(l3,10,10,10,ierr)
!  if(.not.l3(1,1,1)) print *,"realloc_l3 error"
!  call memman_deallocate(l3)
!
!  print *," "
!  call memman_allocate(l4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(l4)
!  call memman_deallocate(l4)
!  call memman_allocate(l4,0,0,0,0,ierr)
!  l4=.true.
!  call memman_reallocate(l4,10,10,10,10,ierr)
!  if(.not.l4(1,1,1,1)) print *,"realloc_l4 error"
!  call memman_deallocate(l4)
!
!  print *," "
!  call memman_allocate(l5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(l5)
!  call memman_deallocate(l5)
!  call memman_allocate(l5,0,0,0,0,0,ierr)
!  l5=.true.
!  call memman_reallocate(l5,10,10,10,10,10,ierr)
!  if(.not.l5(1,1,1,1,1)) print *,"realloc_l5 error"
!  call memman_deallocate(l5)
!
!  print *," "
!  print *,"Integer tests"
!  print *," "
!  call memman_allocate(i1,10,ierr)
!  print *,"memman_loc=",memman_loc(i1)
!  call memman_deallocate(i1)
!  call memman_allocate(i1,0,ierr)
!  i1=123
!  call memman_reallocate(i1,10,ierr)
!  if(i1(1).ne. 123) print *,"realloc_i1 error"
!  call memman_deallocate(i1)
!
!  print *," "
!  call memman_allocate(i2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(i2)
!  call memman_deallocate(i2)
!  call memman_allocate(i2,0,0,ierr)
!  i2=123
!  call memman_reallocate(i2,10,10,ierr)
!  if(i2(1,1).ne. 123) print *,"realloc_i2 error"
!  call memman_deallocate(i2)
!
!  print *," "
!  call memman_allocate(i3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(i3)
!  call memman_deallocate(i3)
!  call memman_allocate(i3,0,0,0,ierr)
!  i3=123
!  call memman_reallocate(i3,10,10,10,ierr)
!  if(i3(1,1,1).ne. 123) print *,"realloc_i3 error"
!  call memman_deallocate(i3)
!
!  print *," "
!  call memman_allocate(i4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(i4)
!  call memman_deallocate(i4)
!  call memman_allocate(i4,0,0,0,0,ierr)
!  i4=123
!  call memman_reallocate(i4,10,10,10,10,ierr)
!  if(i4(1,1,1,1).ne. 123) print *,"realloc_i4 error"
!  call memman_deallocate(i4)
!
!  print *," "
!  call memman_allocate(i5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(i5)
!  call memman_deallocate(i5)
!  call memman_allocate(i5,0,0,0,0,0,ierr)
!  i5=123
!  call memman_reallocate(i5,10,10,10,10,10,ierr)
!  if(i5(1,1,1,1,1).ne. 123) print *,"realloc_i5 error"
!  call memman_deallocate(i5)
!
!  print *," "
!  print *,"Real tests"
!  print *," "
!  call memman_allocate(r1,10,ierr)
!  print *,"memman_loc=",memman_loc(r1)
!  call memman_deallocate(r1)
!  call memman_allocate(r1,0,ierr)
!  r1=123
!  call memman_reallocate(r1,10,ierr)
!  if(r1(1).ne. 123) print *,"realloc_r1 error"
!  call memman_deallocate(r1)
!
!  print *," "
!  call memman_allocate(r2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(r2)
!  call memman_deallocate(r2)
!  call memman_allocate(r2,0,0,ierr)
!  r2=123
!  call memman_reallocate(r2,10,10,ierr)
!  if(r2(1,1).ne. 123) print *,"realloc_r2 error"
!  call memman_deallocate(r2)
!
!  print *," "
!  call memman_allocate(r3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(r3)
!  call memman_deallocate(r3)
!  call memman_allocate(r3,0,0,0,ierr)
!  r3=123
!  call memman_reallocate(r3,10,10,10,ierr)
!  if(r3(1,1,1).ne. 123) print *,"realloc_r3 error"
!  call memman_deallocate(r3)
!
!  print *," "
!  call memman_allocate(r4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(r4)
!  call memman_deallocate(r4)
!  call memman_allocate(r4,0,0,0,0,ierr)
!  r4=123
!  call memman_reallocate(r4,10,10,10,10,ierr)
!  if(r4(1,1,1,1).ne. 123) print *,"realloc_r4 error"
!  call memman_deallocate(r4)
!
!  print *," "
!  call memman_allocate(r5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(r5)
!  call memman_deallocate(r5)
!  call memman_allocate(r5,0,0,0,0,0,ierr)
!  r5=123
!  call memman_reallocate(r5,10,10,10,10,10,ierr)
!  if(r5(1,1,1,1,1).ne. 123) print *,"realloc_r5 error"
!  call memman_deallocate(r5)
!
!  print *," "
!  print *,"Double tests"
!  print *," "
!  call memman_allocate(d1,10,ierr)
!  print *,"memman_loc=",memman_loc(d1)
!  call memman_deallocate(d1)
!  call memman_allocate(d1,0,ierr)
!  d1=123
!  call memman_reallocate(d1,10,ierr)
!  if(d1(1).ne. 123) print *,"realloc_d1 error"
!  call memman_deallocate(d1)
!
!  print *," "
!  call memman_allocate(d2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(d2)
!  call memman_deallocate(d2)
!  call memman_allocate(d2,0,0,ierr)
!  d2=123
!  call memman_reallocate(d2,10,10,ierr)
!  if(d2(1,1).ne. 123) print *,"realloc_d2 error"
!  call memman_deallocate(d2)
!
!  print *," "
!  call memman_allocate(d3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(d3)
!  call memman_deallocate(d3)
!  call memman_allocate(d3,0,0,0,ierr)
!  d3=123
!  call memman_reallocate(d3,10,10,10,ierr)
!  if(d3(1,1,1).ne. 123) print *,"realloc_d3 error"
!  call memman_deallocate(d3)
!
!  print *," "
!  call memman_allocate(d4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(d4)
!  call memman_deallocate(d4)
!  call memman_allocate(d4,0,0,0,0,ierr)
!  d4=123
!  call memman_reallocate(d4,10,10,10,10,ierr)
!  if(d4(1,1,1,1).ne. 123) print *,"realloc_d4 error"
!  call memman_deallocate(d4)
!
!  print *," "
!  call memman_allocate(d5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(d5)
!  call memman_deallocate(d5)
!  call memman_allocate(d5,0,0,0,0,0,ierr)
!  d5=123
!  call memman_reallocate(d5,10,10,10,10,10,ierr)
!  if(d5(1,1,1,1,1).ne. 123) print *,"realloc_d5 error"
!  call memman_deallocate(d5)
!
!  print *," "
!  print *,"Complex tests"
!  print *," "
!  call memman_allocate(z1,10,ierr)
!  print *,"memman_loc=",memman_loc(z1)
!  call memman_deallocate(z1)
!  call memman_allocate(z1,0,ierr)
!  z1=123
!  call memman_reallocate(z1,10,ierr)
!  if(z1(1).ne. 123) print *,"realloc_z1 error"
!  call memman_deallocate(z1)
!
!  print *," "
!  call memman_allocate(z2,10,10,ierr)
!  print *,"memman_loc=",memman_loc(z2)
!  call memman_deallocate(z2)
!  call memman_allocate(z2,0,0,ierr)
!  z2=123
!  call memman_reallocate(z2,10,10,ierr)
!  if(z2(1,1).ne. 123) print *,"realloc_z2 error"
!  call memman_deallocate(z2)
!
!  print *," "
!  call memman_allocate(z3,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(z3)
!  call memman_deallocate(z3)
!  call memman_allocate(z3,0,0,0,ierr)
!  z3=123
!  call memman_reallocate(z3,10,10,10,ierr)
!  if(z3(1,1,1).ne. 123) print *,"realloc_z3 error"
!  call memman_deallocate(z3)
!
!  print *," "
!  call memman_allocate(z4,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(z4)
!  call memman_deallocate(z4)
!  call memman_allocate(z4,0,0,0,0,ierr)
!  z4=123
!  call memman_reallocate(z4,10,10,10,10,ierr)
!  if(z4(1,1,1,1).ne. 123) print *,"realloc_z4 error"
!  call memman_deallocate(z4)
!
!  print *," "
!  call memman_allocate(z5,10,10,10,10,10,ierr)
!  print *,"memman_loc=",memman_loc(z5)
!  call memman_deallocate(z5)
!  call memman_allocate(z5,0,0,0,0,0,ierr)
!  z5=123
!  call memman_reallocate(z5,10,10,10,10,10,ierr)
!  if(z5(1,1,1,1,1).ne. 123) print *,"realloc_z5 error"
!  call memman_deallocate(z5)
!
!  print *," "
!  print *,"Accumulation test"
!  call memman_allocate(c1,10,ierr)
!  call memman_allocate(c2,10,10,ierr)
!  call memman_allocate(c3,10,10,10,ierr)
!  call memman_allocate(c4,10,10,10,10,ierr)
!  call memman_allocate(c5,10,10,10,10,10,ierr)
!  call memman_allocate(l1,10,ierr)
!  call memman_allocate(l2,10,10,ierr)
!  call memman_allocate(l3,10,10,10,ierr)
!  call memman_allocate(l4,10,10,10,10,ierr)
!  call memman_allocate(l5,10,10,10,10,10,ierr)
!  call memman_allocate(i1,10,ierr)
!  call memman_allocate(i2,10,10,ierr)
!  call memman_allocate(i3,10,10,10,ierr)
!  call memman_allocate(i4,10,10,10,10,ierr)
!  call memman_allocate(i5,10,10,10,10,10,ierr)
!  call memman_allocate(r1,10,ierr)
!  call memman_allocate(r2,10,10,ierr)
!  call memman_allocate(r3,10,10,10,ierr)
!  call memman_allocate(r4,10,10,10,10,ierr)
!  call memman_allocate(r5,10,10,10,10,10,ierr)
!  call memman_allocate(d3,10,10,10,ierr)
!  call memman_deallocate(d3)
!  call memman_allocate(d1,10,ierr)
!  call memman_allocate(d2,10,10,ierr)
!  call memman_allocate(d3,10,10,10,ierr)
!  call memman_allocate(d4,10,10,10,10,ierr)
!  call memman_allocate(d5,10,10,10,10,10,ierr)
!  call memman_allocate(z1,10,ierr)
!  call memman_allocate(z2,10,10,ierr)
!  call memman_allocate(z3,10,10,10,ierr)
!  call memman_allocate(z4,10,10,10,10,ierr)
!  call memman_allocate(z5,10,10,10,10,10,ierr)
!
!  call memman_dump_tracking("see allocated variables")
!  print *,"Amount memeory allocated=",memman_get_memory_allocated()
!
!  print *," "
!  print *,"testing memman_check_variable" 
!  if(memman_check_variable(c1,"c1").ne.0) print *,"check error c1"
!  if(memman_check_variable(c2,"c2").ne.0) print *,"check error c2"
!  if(memman_check_variable(c3,"c3").ne.0) print *,"check error c3"
!  if(memman_check_variable(c4,"c4").ne.0) print *,"check error c4"
!  if(memman_check_variable(c5,"c5").ne.0) print *,"check error c5"
!  if(memman_check_variable(l1,"l1").ne.0) print *,"check error l1"
!  if(memman_check_variable(l2,"l2").ne.0) print *,"check error l2"
!  if(memman_check_variable(l3,"l3").ne.0) print *,"check error l3"
!  if(memman_check_variable(l4,"l4").ne.0) print *,"check error l4"
!  if(memman_check_variable(l5,"l5").ne.0) print *,"check error l5"
!  if(memman_check_variable(i1,"i1").ne.0) print *,"check error i1"
!  if(memman_check_variable(i2,"i2").ne.0) print *,"check error i2"
!  if(memman_check_variable(i3,"i3").ne.0) print *,"check error i3"
!  if(memman_check_variable(i4,"i4").ne.0) print *,"check error i4"
!  if(memman_check_variable(i5,"i5").ne.0) print *,"check error i5"
!  if(memman_check_variable(r1,"r1").ne.0) print *,"check error r1"
!  if(memman_check_variable(r2,"r2").ne.0) print *,"check error r2"
!  if(memman_check_variable(r3,"r3").ne.0) print *,"check error r3"
!  if(memman_check_variable(r4,"r4").ne.0) print *,"check error r4"
!  if(memman_check_variable(r5,"r5").ne.0) print *,"check error r5"
!  if(memman_check_variable(d1,"d1").ne.0) print *,"check error d1"
!  if(memman_check_variable(d2,"d2").ne.0) print *,"check error d2"
!  if(memman_check_variable(d3,"d3").ne.0) print *,"check error d3"
!  if(memman_check_variable(d4,"d4").ne.0) print *,"check error d4"
!  if(memman_check_variable(d5,"d5").ne.0) print *,"check error d5"
!  if(memman_check_variable(z1,"z1").ne.0) print *,"check error z1"
!  if(memman_check_variable(z2,"z2").ne.0) print *,"check error z2"
!  if(memman_check_variable(z3,"z3").ne.0) print *,"check error z3"
!  if(memman_check_variable(z4,"z4").ne.0) print *,"check error z4"
!  if(memman_check_variable(z5,"z5").ne.0) print *,"check error z5"
! 
!  call memman_get_info(c1, p_loc, v_loc, v_size, v_name)
!  print *,"info_c1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(c2, p_loc, v_loc, v_size, v_name)
!  print *,"info_c2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(c3, p_loc, v_loc, v_size, v_name)
!  print *,"info_c3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(c4, p_loc, v_loc, v_size, v_name)
!  print *,"info_c4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(c5, p_loc, v_loc, v_size, v_name)
!  print *,"info_c5=", p_loc, v_loc, v_size, trim(v_name)
!
!  call memman_get_info(l1, p_loc, v_loc, v_size, v_name)
!  print *,"info_l1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(l2, p_loc, v_loc, v_size, v_name)
!  print *,"info_l2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(l3, p_loc, v_loc, v_size, v_name)
!  print *,"info_l3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(l4, p_loc, v_loc, v_size, v_name)
!  print *,"info_l4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(l5, p_loc, v_loc, v_size, v_name)
!  print *,"info_l5=", p_loc, v_loc, v_size, trim(v_name)
!
!  call memman_get_info(i1, p_loc, v_loc, v_size, v_name)
!  print *,"info_i1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(i2, p_loc, v_loc, v_size, v_name)
!  print *,"info_i2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(i3, p_loc, v_loc, v_size, v_name)
!  print *,"info_i3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(i4, p_loc, v_loc, v_size, v_name)
!  print *,"info_i4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(i5, p_loc, v_loc, v_size, v_name)
!  print *,"info_i5=", p_loc, v_loc, v_size, trim(v_name)
!
!  call memman_get_info(r1, p_loc, v_loc, v_size, v_name)
!  print *,"info_r1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(r2, p_loc, v_loc, v_size, v_name)
!  print *,"info_r2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(r3, p_loc, v_loc, v_size, v_name)
!  print *,"info_r3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(r4, p_loc, v_loc, v_size, v_name)
!  print *,"info_r4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(r5, p_loc, v_loc, v_size, v_name)
!  print *,"info_r5=", p_loc, v_loc, v_size, trim(v_name)
!
!  call memman_get_info(d1, p_loc, v_loc, v_size, v_name)
!  print *,"info_d1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(d2, p_loc, v_loc, v_size, v_name)
!  print *,"info_d2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(d3, p_loc, v_loc, v_size, v_name)
!  print *,"info_d3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(d4, p_loc, v_loc, v_size, v_name)
!  print *,"info_d4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(d5, p_loc, v_loc, v_size, v_name)
!  print *,"info_d5=", p_loc, v_loc, v_size, trim(v_name)
!
!  call memman_get_info(z1, p_loc, v_loc, v_size, v_name)
!  print *,"info_z1=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(z2, p_loc, v_loc, v_size, v_name)
!  print *,"info_z2=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(z3, p_loc, v_loc, v_size, v_name)
!  print *,"info_z3=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(z4, p_loc, v_loc, v_size, v_name)
!  print *,"info_z4=", p_loc, v_loc, v_size, trim(v_name)
!  call memman_get_info(z5, p_loc, v_loc, v_size, v_name)
!  print *,"info_z5=", p_loc, v_loc, v_size, trim(v_name)
!
!  print *," "
!  print *,"sizeof tests"
!  print *, "sizeof(i0)=", memman_sizeof(i0)
!  print *, "sizeof(i1)=", memman_sizeof(i1)
!  print *, "sizeof(i2)=", memman_sizeof(i2)
!  print *, "sizeof(i3)=", memman_sizeof(i3)
!  print *, "sizeof(i4)=", memman_sizeof(i4)
!  print *, "sizeof(i5)=", memman_sizeof(i5)
!
!  print *, "sizeof(r0)=", memman_sizeof(r0)
!  print *, "sizeof(r1)=", memman_sizeof(r1)
!  print *, "sizeof(r2)=", memman_sizeof(r2)
!  print *, "sizeof(r3)=", memman_sizeof(r3)
!  print *, "sizeof(r4)=", memman_sizeof(r4)
!  print *, "sizeof(r5)=", memman_sizeof(r5)
!
!  print *, "sizeof(d0)=", memman_sizeof(d0)
!  print *, "sizeof(d1)=", memman_sizeof(d1)
!  print *, "sizeof(d2)=", memman_sizeof(d2)
!  print *, "sizeof(d3)=", memman_sizeof(d3)
!  print *, "sizeof(d4)=", memman_sizeof(d4)
!  print *, "sizeof(d5)=", memman_sizeof(d5)
!
!  print *, "sizeof(z0)=", memman_sizeof(z0)
!  print *, "sizeof(z1)=", memman_sizeof(z1)
!  print *, "sizeof(z2)=", memman_sizeof(z2)
!  print *, "sizeof(z3)=", memman_sizeof(z3)
!  print *, "sizeof(z4)=", memman_sizeof(z4)
!  print *, "sizeof(z5)=", memman_sizeof(z5)
!
!  print *, "sizeof(l0)=", memman_sizeof(l0)
!  print *, "sizeof(l1)=", memman_sizeof(l1)
!  print *, "sizeof(l2)=", memman_sizeof(l2)
!  print *, "sizeof(l3)=", memman_sizeof(l3)
!  print *, "sizeof(l4)=", memman_sizeof(l4)
!  print *, "sizeof(l5)=", memman_sizeof(l5)
!
!  print *, "sizeof(c0)=", memman_sizeof(c0)
!  print *, "sizeof(c1)=", memman_sizeof(c1)
!  print *, "sizeof(c2)=", memman_sizeof(c2)
!  print *, "sizeof(c3)=", memman_sizeof(c3)
!  print *, "sizeof(c4)=", memman_sizeof(c4)
!  print *, "sizeof(c5)=", memman_sizeof(c5)
!
!  print *," "
!  print *,"testing memman_deallocate" 
!  call memman_deallocate(z5)
!  call memman_deallocate(z4)
!  call memman_deallocate(z3)
!  call memman_deallocate(z2)
!  call memman_deallocate(z1)
!  call memman_deallocate(d5)
!  call memman_deallocate(d4)
!  call memman_deallocate(d3)
!  call memman_deallocate(d2)
!  call memman_deallocate(d1)
!  call memman_deallocate(r5)
!  call memman_deallocate(r4)
!  call memman_deallocate(r3)
!  call memman_deallocate(r2)
!  call memman_deallocate(r1)
!  call memman_deallocate(i5)
!  call memman_deallocate(i4)
!  call memman_deallocate(i3)
!  call memman_deallocate(i2)
!  call memman_deallocate(i1)
!  call memman_deallocate(l5)
!  call memman_deallocate(l4)
!  call memman_deallocate(l3)
!  call memman_deallocate(l2)
!  call memman_deallocate(l1)
!  call memman_deallocate(c5)
!  call memman_deallocate(c4)
!  call memman_deallocate(c3)
!  call memman_deallocate(c2)
!  call memman_deallocate(c1)
!
!  call memman_dump_tracking("after accumulation test 1")
!
!
!  print *," "
!  print *,"Accumulation test2"
!  call memman_allocate(c1,10,ierr)
!  call memman_allocate(c2,10,10,ierr)
!  call memman_allocate(c3,10,10,10,ierr)
!  call memman_allocate(c4,10,10,10,10,ierr)
!  call memman_allocate(c5,10,10,10,10,10,ierr)
!  call memman_allocate(l1,10,ierr)
!  call memman_allocate(l2,10,10,ierr)
!  call memman_allocate(l3,10,10,10,ierr)
!  call memman_allocate(l4,10,10,10,10,ierr)
!  call memman_allocate(l5,10,10,10,10,10,ierr)
!  call memman_allocate(i1,10,ierr)
!  call memman_allocate(i2,10,10,ierr)
!  call memman_allocate(i3,10,10,10,ierr)
!  call memman_allocate(i4,10,10,10,10,ierr)
!  call memman_allocate(i5,10,10,10,10,10,ierr)
!  call memman_allocate(r1,10,ierr)
!  call memman_allocate(r2,10,10,ierr)
!  call memman_allocate(r3,10,10,10,ierr)
!  call memman_allocate(r4,10,10,10,10,ierr)
!  call memman_allocate(r5,10,10,10,10,10,ierr)
!  call memman_allocate(d3,10,10,10,ierr)
!  call memman_deallocate(d3)
!  call memman_allocate(d1,10,ierr)
!  call memman_allocate(d2,10,10,ierr)
!  call memman_allocate(d3,10,10,10,ierr)
!  call memman_allocate(d4,10,10,10,10,ierr)
!  call memman_allocate(d5,10,10,10,10,10,ierr)
!  call memman_allocate(z1,10,ierr)
!  call memman_allocate(z2,10,10,ierr)
!  call memman_allocate(z3,10,10,10,ierr)
!  call memman_allocate(z4,10,10,10,10,ierr)
!  call memman_allocate(z5,10,10,10,10,10,ierr)
!
!  print *," "
!  print *,"testing memman_free"
!  call memman_free(c5)
!  call memman_free(c4)
!  call memman_free(c3)
!  call memman_free(c2)
!  call memman_free(c1)
!  call memman_free(l5)
!  call memman_free(l4)
!  call memman_free(l3)
!  call memman_free(l2)
!  call memman_free(l1)
!  call memman_free(i5)
!  call memman_free(i4)
!  call memman_free(i3)
!  call memman_free(i2)
!  call memman_free(i1)
!  call memman_free(r5)
!  call memman_free(r4)
!  call memman_free(r3)
!  call memman_free(r2)
!  call memman_free(r1)
!  call memman_free(d5)
!  call memman_free(d4)
!  call memman_free(d3)
!  call memman_free(d2)
!  call memman_free(d1)
!  call memman_free(z5)
!  call memman_free(z4)
!  call memman_free(z3)
!  call memman_free(z2)
!  call memman_free(z1)
!
!  call memman_dump_tracking("after free")
!  call memman_exit_tracking(0)
!
!end program main

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
