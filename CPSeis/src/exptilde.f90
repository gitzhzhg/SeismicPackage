!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- exptilde.f90 --------------------------------!!
!!---------------------------- exptilde.f90 --------------------------------!!
!!---------------------------- exptilde.f90 --------------------------------!!

           ! other files are:  exptilde_crou.c  exptilde_crou.h


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
! Name       : exptilde
! Category   : io
! Written    : 2000-01-03   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-19
! Purpose    : Expand the tilde (~) in a filename to an absolute path.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                 
!
! The purpose of this primitive is to change the name of a file by expanding
! the tilde (~) to an absolute path.  If the filename begins with a tilde,
! it is expanded to an absolute path.  Otherwise the filename is unchanged.
!
! This primitive is callable from both C and Fortran.
! Only the files exptilde_crou.c and exptilde_crou.h are needed from C.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS             
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
! For C-language pointers in C code, the flag (i,o,b) refers to the contents
! pointed to by the pointer, not to the value of the pointer itself.  The
! pointer value (the address) is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                       FORTRAN CALLING SEQUENCE
!
!                                     b
!                   call exptilde (filename)
!                   call exptilde (filename1, filename2)
!                                     i          o
!
! character(len=*) filename  = input and output filename.
! character(len=*) filename1 = input  filename (may contain a tilde).
! character(len=*) filename2 = output filename (no tilde).
!
! For the second routine, note that the INPUT filename is the first argument,
! consistent with often-used Fortran conventions.
!
!-------------------------------------------------------------------------------
!                       C LANGUAGE CALLING SEQUENCE
!
!                                        b
!                void exptilde_crou1 (filename)
!                void exptilde_crou2 (filename2, filename1)
!                                        o          i
!
! char       *filename  = input and output filename.
! const char *filename1 = input  filename (may contain a tilde).
! char       *filename2 = output filename (no tilde).
!
! For the second function, note that the OUTPUT filename is the first argument,
! consistent with the usual C language convention for strings.
!
! The header file exptilde_crou.h must be included in the calling program.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2000-10-19  Stoeckley    Replace all of the code with calls to the
!                               C functions in the new file exptilde_crou.c
!                               which was created from the workstation file
!                               exp_tilde.c (in ~spws/util/cprim).  This
!                               change vastly simplifies the code, removes
!                               dependencies on the TEMPNAME and FINQUIRE
!                               primitives, and unifies the CPS and workstation
!                               code.
!  3. 2000-02-24  Stoeckley    Remove dependency on the CIO primitive to
!                               eliminate circular dependencies when called
!                               from CIO.
!  2. 2000-01-17  Stoeckley    Change to use bash on linux and call cio_remove.
!  1. 2000-01-07  Stoeckley    Initial version.
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


      module exptilde_module
      use string_module
      implicit none
      public

      interface exptilde
           module procedure exptilde_1
           module procedure exptilde_2
      end interface

      contains


!!------------------------------ exptilde ---------------------------------!!
!!------------------------------ exptilde ---------------------------------!!
!!------------------------------ exptilde ---------------------------------!!


      subroutine exptilde_1 (filename)
      implicit none
      character(len=*),intent(inout) :: filename                  ! argument

      call exptilde_2 (filename,filename)
      return
      end subroutine exptilde_1



      subroutine exptilde_2 (filename1,filename2)
      implicit none
      character(len=*),intent(in)    :: filename1                 ! argument
      character(len=*),intent(out)   :: filename2                 ! argument
      integer                        :: holler1(50),holler2(50)   ! local

      call string_cc2hh   (filename1, holler1  )
      call exptilde_crou2 (holler2  , holler1  )
      call string_hh2cc   (holler2  , filename2)
      return
      end subroutine exptilde_2


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module exptilde_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

