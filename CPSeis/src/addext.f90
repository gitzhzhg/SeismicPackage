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
! Name       : ADDEXT
! Category   : io
! Written    : 1988-07-13   by: Bob Baumel
! Revised    : 2001-03-23   by: Bob Baumel
! Maturity   : production   2001-05-14
! Purpose    : Add a file extension to a file name.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!  The ADDEXT primitive conditionally adds an extension to a filename.
!  ADDEXT detects the presence of a preexisting extention and changes it
!  only if the calling argument 'lreplace' is .true. When 'lreplace' is
!  absent from the calling list, it will be set to .false.
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
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                                 opt
!                     b     i      i
!      call addext (fname, ext, lreplace)
!
! character(len=*)  fname    = filename (including path) needing extension.
! character(len=*)  ext      = extension (without '.') to append to filename.
! logical           lreplace = if .true., existing extension is replaced;
!                              if omitted or .false., existing extension is
!                              retained.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 10. 2001-05-14  Baumel     Fixed history_doc XML tag.
!  9. 2000-05-08  Baumel     Fixed bug involving extraneous characters when
!                             replacing a longer extension with shorter one;
!                             also removed code based on VAX filenames.
!  8. 2000-01-17  Stoeckley  Fixed bug when optional argument was absent.
!                             Also moved from the CHARACTER category to IO.
!  7. 1999-12-29  O'Brien    Brought xml tags up to date
!                            Add RCS character ID variable
!  6. 1999-08-16  O'Brien    Full f90 conversion.
!                             Subroutine naming convention changes
!                             * interface has been altered.
!                             From: addext         ==>  To: addext  *
!                             From: addextrp       ==>  To: addext  *
!                             From: addext_replace ==>  To: addext  *
!  5. 1999-01-06  Goodger    Begin using the fortran90 compiler.
!  4. 1996-01-22  D Hanson   Add the ADDEEXT_REPLACE = ADDEXTRP to be
!                            Consistent with the VAX version.
!  3. 1989-09-05  D Hanson   Convert to CRAY, ADDEEXT_REPLACE = ADDEXTRP
!  2. 1988-07-22  B Baumel   Add the ADDEXTRP entry point; recognize
!                            the '>' character, as well as ']', as a possible
!                            right-hand delimeter of a directory name.
!  1. 1988-07-13  B Baumel   Separate routine from other front-end code
!                            where it had been hidden.
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
!                           PROGRAMMING NOTES
!
!
!    ADDEXT is very specialized. It expects filenames will follow certain
!    naming conventions when the pathname is included as part of the filename.
!
!    The 'replace' option caused the replacement of an existing extension
!    if one is presentin the input filename. The extension will always be
!    appended when the input filename has no extension.
!
!    The delimiter between filename and extension is '.'
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module addext_module
      implicit none

      private
      public :: addext

      character(len=100),public,save :: ADDEXT_IDENT = &
'$Id: addext.f90,v 1.10 2001/05/10 19:04:20 sps prod sps $'

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      subroutine addext (fname, ext, lreplace)
      implicit none

      character(len=*) , intent(inout) :: fname            ! arguments
      character(len=*) , intent(in)    :: ext              ! arguments
      logical, optional, intent(in)    :: lreplace         ! arguments

      integer   :: lastslash ,idot                         ! local

!-----------------------------------------------
!  Find end of directory path as indicated by final '/'
      lastslash = index (fname, '/', back=.true.)

!  Find final '.' after the last slash
      idot = index (fname(lastslash+1:), '.', back=.true.)

!  If already an extension and lreplace is missing or false, simply return.
      if (idot > 0) then
        if (.not.present(lreplace)) return
        if (.not.lreplace) return
      end if

!  Otherwise build a name with the extension.
      if (idot > 0) then
        fname = fname(:lastslash+idot) // ext
      else
        fname = trim(fname) // '.' // ext
      endif
      return
      end subroutine addext

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module addext_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
