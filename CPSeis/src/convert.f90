!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- convert.f90 --------------------------------!!
!!---------------------------- convert.f90 --------------------------------!!
!!---------------------------- convert.f90 --------------------------------!!


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
! Name       : CONVERT
! Category   : miscellaneous
! Written    : 2001-10-04   by: Tom Stoeckley
! Revised    : 2001-10-04   by: Tom Stoeckley
! Maturity   : production   2002-03-19
! Purpose    : Utilities for converting variable types between C and Fortran.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains utilities to convert between C and Fortran
! variable types to assist in writing interfaces between C (or C++) and
! Fortran routines.
!
! Some of the tools in the STRING primitive and in the NAMED_CONSTANTS
! primitive are also useful for the same purpose.  Their capabilities are
! not duplicated in this primitive, but those tools deemed useful for this
! purpose are briefly referred to below.
!
! See the c2f_interface.h header file for additional documentation.
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
!             TO CONVERT BETWEEN CHARACTER AND HOLLERITH VARIABLES
!
! These subroutines are intended to facilitate passing character variables
! between C and Fortran.  They are to be called from Fortran.
!
! Use the following routines in the STRING primitive for this purpose:
!
!                       call string_cc2hh
!                       call string_cc2hh_alloc
!                       call string_hh2cc
!                       call string_cc2hh_array
!                       call string_hh2cc_array
!
!-------------------------------------------------------------------------------
!             TO GET INFORMATION ABOUT CHARACTER/INTEGER WORD SIZES
!
! Use the following routines in the STRING primitive for this purpose:
!
!                     string_chars_per_integer
!                     string_num_integers       
!                     string_num_trimmed_integers
!
!-------------------------------------------------------------------------------
!               TO CONVERT BETWEEN INTEGER AND LOGICAL VARIABLES
!
! These subroutines are intended to facilitate passing logical variables
! between C and Fortran.  They are to be called from Fortran.  Although
! they are quite trivial, they exist here because they may be used quite
! often.
!
!                                          i     o
!                     call convert_ii2ll (ivar, lvar)
!                     call convert_ll2ii (lvar, ivar)
!                                          i     o
!
!                                           i       o         i  
!              call convert_ii2ll_array (iarray, larray, nelements)
!              call convert_ll2ii_array (larray, iarray, nelements)
!                                           i       o         i   
!
! integer    ivar = integer variable passed to or from C.
! logical    lvar = equivalent logical variable.
!
! integer    iarray(nelements) = integer array passed to or from C.
! logical    larray(nelements) = equivalent logical array.
! integer    nelements         = number of elements in array.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                         C-LANGUAGE POINTERS
!
! C or C++ pointers passed to and from Fortran90 should have the CPOINTER
! Fortran derived type when stored within Fortran code.  This derived type
! is defined in the NAMED_CONSTANTS module.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2002-03-19  Stoeckley  Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module convert_module
      implicit none
      public

      character(len=100),public,save :: CONVERT_IDENT = &
'$Id: convert.f90,v 1.1 2002/03/19 21:04:19 Stoeckley prod sps $'

      contains


!!------------- convert between integer and logical scalars --------------!!
!!------------- convert between integer and logical scalars --------------!!
!!------------- convert between integer and logical scalars --------------!!


      subroutine convert_ii2ll (ivar,lvar)
      implicit none
      integer,intent(in)  :: ivar    ! argument
      logical,intent(out) :: lvar    ! argument

      lvar = (ivar /= 0)
      return
      end subroutine convert_ii2ll



      subroutine convert_ll2ii (lvar,ivar)
      implicit none
      logical,intent(in)  :: lvar    ! argument
      integer,intent(out) :: ivar    ! argument

      if (lvar) then 
           ivar = 1 
      else 
           ivar = 0 
      end if
      return
      end subroutine convert_ll2ii


!!------------- convert between integer and logical arrays --------------!!
!!------------- convert between integer and logical arrays --------------!!
!!------------- convert between integer and logical arrays --------------!!


      subroutine convert_ii2ll_array (ivar,lvar,nvar)
      implicit none
      integer,intent(in)  :: ivar(*)   ! argument
      logical,intent(out) :: lvar(*)   ! argument
      integer,intent(in)  :: nvar      ! argument
      integer             :: indx      ! local

      do indx = 1,nvar 
           call convert_ii2ll (ivar(indx),lvar(indx)) 
      end do
      return
      end subroutine convert_ii2ll_array



      subroutine convert_ll2ii_array (lvar,ivar,nvar)
      implicit none
      logical,intent(in)  :: lvar(*)   ! argument
      integer,intent(out) :: ivar(*)   ! argument
      integer,intent(in)  :: nvar      ! argument
      integer             :: indx      ! local

      do indx = 1,nvar 
           call convert_ll2ii (lvar(indx),ivar(indx)) 
      end do
      return
      end subroutine convert_ll2ii_array


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module convert_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

