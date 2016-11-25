!<CPS_v1 type="PRIMITIVE"/>
!!---------------------- fftf77.f90 ---------------------------------!!
!!---------------------- fftf77.f90 ---------------------------------!!
!!---------------------- fftf77.f90 ---------------------------------!!

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
! Name       : fftf77
! Category   : math
! Written    : 2000-10-02  by: Michael L. Sherrill
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : F77 style wrapper that C functions can call in order to get 
!              the fft f90 module
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! FFTF77 provides a wrapper to the fft f90 module
! See fft.f90
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
!                             
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>



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
!                           i                 i
!        fftf77(          type,            fftlen           
!                           i                 b
!                         sign,              buf1
!                           b
!                         buf2
!
! integer                    :: type(10)       !'ctoc' = complex to complex fft
!                                              !'rtoc' = real to complex fft
!                                              !'ctor' = complex to real fft
! integer                    :: fftlen         ! A power of 2
! integer                    :: sign           ! 1 or -1
! real                       :: buf1(:)        ! Real array result
! complex                    :: buf2(:)        ! Complex array result
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! See fft.f90
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author                   Description
!     ----        ------                   -----------
!  2. 2006-04-25  B. Menger                Removed Unused Variables.
!  1. 2001-02-15  Michael L. Sherrill      Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

module fftf77_module
  implicit none
  character(len=100),public,save :: fftf77_ident = &
  '$Id: fftf77.f90,v 1.2 2006/04/25 12:00:54 Menger prod sps $'
end module fftf77_module


!!---------------------- start of wrappers  ------------------------------!!
!!---------------------- start of wrappers  ------------------------------!!
!!---------------------- start of wrappers  ------------------------------!!

subroutine fftf77(type, fftlen, sign, buf1, buf2, bufsize ) 

  use fft_module
  use string_module
  implicit none

  integer                    :: type(10)             !'ctoc' complex to complex
                                                     !'rtoc'real to complex fft
                                                     !'ctor' complex to real fft
  integer                    :: fftlen               ! A power of 2
  integer                    :: sign                 ! 1 or -1
  integer                    :: bufsize              ! buf array sizes
  real                       :: buf1(bufsize)        ! real array result
  complex                    :: buf2(bufsize)        ! complex array result

  ! Local
  character*40               :: type_opt

  integer                    :: fft_id

  ! Convert strings
  call string_hh2cc(type, type_opt)


  ! Initialize the obj
  fft_id = fft_new(sign, fftlen, type_opt)

  if(type_opt(1:4).eq.'ctoc') then
    call fft_cct(fft_id, buf2)
  else if(type_opt(1:4).eq.'rtoc') then
    call fft_rct(fft_id, buf1, buf2)
  else
    call fft_crt(fft_id, buf2, buf1)
  endif

  call fft_del(fft_id)

  return 

  end subroutine fftf77
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
