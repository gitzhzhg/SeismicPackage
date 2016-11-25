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
! Name       : mutehw
! Category   : math
! Written    : 1992-02-18   by: Bill Troutt
! Revised    : 2001-10-29   by: Tom Stoeckley
! Maturity   : production   2001-11-01
! Purpose    : Mute header word checker.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 
!    MUTEHW checks that the head and tail mute header words are compatible
!    with each other and NDPT. Options include adjusting HDR_TOP_MUTE and
!    HDR_BOTTOM_MUTE by a constant and applying one or both mutes.
!
!    Gives users of this module access to named constants MUTEHW_SET,
!    MUTEHW_HEAD, MUTEHW_TAIL, and MUTEHW_BOTH. These are used for calling
!    argument ikill and have the following effects:
!
!      MUTEHW_SET  - apply a shift to HDR_TOP_MUTE and HDR_BOTTOM_MUTE
!                    and ensure that HDR_TOP_MUTE and HDR_BOTTOM_MUTE
!                    are in the range 1 to NDPT.
!      MUTEHW_HEAD - apply MUTEHW_SET and perform a head mute based
!                    on HDR_TOP_MUTE value
!      MUTEHW_TAIL - apply MUTEHW_SET and perform a tail mute based
!                    on HDR_BOTTOM_MUTE value
!      MUTEHW_BOTH - apply MUTEHW_HEAD, and MUTEHW_TAIL
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of data samples per trace      used but not changed
!
!
!-------------------------------------------------------------------------------
!</global_doc>
 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
!
! Header Word      Action taken
! -----------      -----------------------
! HDR_TOP_MUTE     SHIFT is added, limitted to be from 1 to NDPT
! HDR_BOTTOM_MUTE  SHIFT is added, limmited to be from HDR_TOP_MUTE to NDPT
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
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                              b   b    i     i      i
!                call mutehw (hd, tr, ndpt, shift, ikill)
!
! double precision hd(:) =   trace headers
! real             tr(:) =   trace samples
! integer          ndpt  =   number of samples in a trace
! real             shift =   number added to mute header words
! integer          ikill =   Flag for operation
!                    ikill = MUTEHW_SET  - apply a shift to HDR_TOP_MUTE and
!                                          HDR_BOTTOM_MUTE and ensure that
!                                          HDR_TOP_MUTE and HDR_BOTTOM_MUTE
!                                          are in the range 1 to NDPT.
!                    ikill = MUTEHW_HEAD - apply MUTEHW_SET and perform a head
!                                          mute based on HDR_TOP_MUTE value
!                    ikill = MUTEHW_TAIL - apply MUTEHW_SET and perform a tail
!                                          mute based on HDR_BOTTOM_MUTE value
!                    ikill = MUTEHW_BOTH - apply MUTEHW_HEAD, and MUTEHW_TAIL
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
! 14. 2001-11-01  Stoeckley     Added missing ending tag which caused some
!                                of the documentation not to show up in CPSDOC;
!                                add missing documentation section; correct the
!                                spelling of history_doc.
! 13. 2000-04-28  O'Brien       Changed tr(:) and hd(:) in declarations
!                                to tr(*) and hd(*) to accomodate a bug in
!                                the Portland Group compiler.
! 12. 2000-01-28  O'Brien       Renamed MUTEHW_NONE to MUTEHW_SET,
!                                  (MUTEHW_NONE is depricated)
!                                Insert 'Written by' at top of document,
!                                Adjust documentation for clarity.
! 11. 2000-01-28  O'Brien       Post new date in revision history.
! 10. 2000-01-27  O'Brien       Documentation fix. Named constants made public.
!  9. 2000-01-26  O'Brien       Retooled to include named constants for ikill.
!                                Removed all dead trace and LAV functions.
!  8. 2000-01-26  O'Brien       Brought to latest header word conformance rules
!  7. 1999-12-29  O'Brien       Added RCS character ID variable
!                                Brought xml tags up to date
!  6. 1999-08-23  O'Brien       Change header array type to double precision
!  5. 1999-08-06  O'Brien       Full f90 conversion
!  4. 1999-01-11  Goodger       Begin using the fortran90 compiler.
!  3. 1992-03-19  Troutt        Fix minor indexing problem when honoring the
!                                tail mute (IKILL=-2,-3).
!  2. 1992-03-18  Troutt        Add options to honor mutes (IKILL=-1,-2,-3).
!  1. 1992-02-18  Bill Troutt   Initial version
!
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
!  1. This routine 1st checks to see if HDR_BOTTOM_MUTE has
!     been set.  If HDR_BOTTOM_MUTE < 1, it is 1st set to NDPT.
!
!  2. The value SHIFT is then algebraically added to HDR_TOP_MUTE
!     and HDR_BOTTOM_MUTE
!
!  3. This routine puts constraints on the mute header words
!
!                0      <  HDR_TOP_MUTE    <= NDPT
!          HDR_TOP_MUTE <= HDR_BOTTOM_MUTE <= NDPT
!
!  4. The IKILL option is processed. ikill < 0 applies top and/or
!     bottom mutes to traces.
! 
!
!  Note that one of the old functions of the ikill argument has been removed.
!  Programmers who are converting calls to mutehw where ikill was set to 1
!  can acheive the same results with the following technique:
!
!     call mutehw (hd(:,k),tr(:,k),obj%ndpt,shift,MUTEHW_SET)
!     if ( hd(HDR_TOP_MUTE,k) >= hd(HDR_BOTTOM_MUTE,k) ) then
!       tr(:,k) = 0.0
!     endif
!
!     where  k          is an index counter in a loop over NTR traces
!            obj%ndpt   is the number of trace samples the calling
!                         routine knows about
!            shift      is a time shift to apply to the mute headers
!                         given in number of samples
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module mutehw_module
      use named_constants_module 
      implicit none

      private
      public :: mutehw

      character(len=100),public,save :: MUTEHW_IDENT = &
             '$Id: mutehw.f90,v 1.14 2001/10/31 21:49:17 Stoeckley prod sps $'

      integer, public, parameter :: MUTEHW_SET  =  0
      integer, public, parameter :: MUTEHW_HEAD = -1
      integer, public, parameter :: MUTEHW_TAIL = -2
      integer, public, parameter :: MUTEHW_BOTH = -3
      integer, public, parameter :: MUTEHW_NONE =  0  

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


      subroutine mutehw (hd,tr,ndpt,shift,ikill)

      implicit none

      real,intent(inout)             :: tr(*)                  !arguments
      double precision,intent(inout) :: hd(*)                  !arguments
      real,intent(in)                :: shift                  !arguments
      integer,intent(in)             :: ndpt,ikill             !arguments

      integer                        :: top_mute,bot_mute      !local

! Extract header mute words
      top_mute = nint(hd(HDR_TOP_MUTE))
      bot_mute = nint(hd(HDR_BOTTOM_MUTE))

! Make sure tail mute is set.
      if (bot_mute <= 0) bot_mute = ndpt

! Shift input header values
      top_mute = top_mute + nint(shift)
      bot_mute = bot_mute + nint(shift)

! Clip values for head and tail.
      top_mute = min( ndpt, max( 1,top_mute ) )
      bot_mute = min( ndpt, max(top_mute,bot_mute) )

! Select the zeroing operation based on ikill
      select case (ikill)

        case ( MUTEHW_SET )
!         ! Nothing to do here

        case ( MUTEHW_HEAD )
!         Apply top mute.
          tr(1:top_mute-1) = 0.0

        case ( MUTEHW_TAIL )
!         Apply bottom mute.
          tr(bot_mute+1:ndpt) = 0.0

        case ( MUTEHW_BOTH )
!         Apply both mutes.
          tr(1:top_mute-1)    = 0.0
          tr(bot_mute+1:ndpt) = 0.0

      end select

! Update the headers and return
      hd(HDR_TOP_MUTE)    = top_mute
      hd(HDR_BOTTOM_MUTE) = bot_mute

      return
      end  subroutine mutehw


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module mutehw_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

