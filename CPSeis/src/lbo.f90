!<CPS_v1 type="PRIMITIVE"/>
!
! other files are:  lbo_crou.c  lbo_crou.h
!
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
! Name       : lbo
! Category   : io
! Written    : 2004-04-06   by: SMCook
! Revised    : 2006-08-29   by: SMCook
! Maturity   : production
! Purpose    : Compress seismic data using 'Localized Bit Optimization'.
! Portability: Has not been tested on 64-bit architecture.
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! LBO format is short for 'Localized Bit Optimization'.  It is a form of
!  single-trace compression wherein short trace segments are stored as integers
!  scaled such that they maximally utilize the pre-defined precision (i.e. the
!  largest absolute amp fills the bits exactly to the 'clip point').  Each
!  segment also has an associated floating point scale factor that allows the
!  original amplitudes to be recovered, within roundoff error, which is of
!  course a function of the chosen precision.
!
! The algorithm works for precisions ranging from 1-32 bits. Typical sensible
!  real-world choices, however, might be precisions 6 thru 20.
!
! A value of about 20 samples is usually a good segment length -- one that is
!  unlikely to experience significant amplitude decay and also yields a decent
!  compression ratio.
!
!
! Here's the formula for record length for VERSION 1, which reveals a lot about
!  the internals of the format:
!
!   recl =
!       1 +             /* 1 byte for version flag                 */
!       1 +             /* 1 byte for byte order flag              */
!       1 +             /* 1 byte for precision                    */
!       1 +             /* 1 byte currently not used (set to zero) */
!       4 +             /* 4 bytes for nsamps                      */
!       4 +             /* 4 bytes for samps_per_pack              */
!       4*npackets +    /* 4 bytes per packet for scale factors    */
!       ndatabytes;     /* space for the bitstream itself          */
!
!   recl = ((recl + 7) / 4) * 4;   /* pad & round up to multiple of 4 */
!
! VERSION 2, which supports FNILs, has an additional bitstream that records the
!  positions of the FNILs.
!
! "Graphically" an LBO trace will look something like this:
!
!  B      - version number
!  B      - byte order
!  B      - precision
!  B      - nonzero only if version 2 (byte is set to 1 if this trace has FNILs)
!  IIII   - number of samples
!  IIII   - number of samples per "pack", each pack having a float scale factor
!  FFFF   - scale factor for first pack/segment
!  FFFF   - scale factor for second pack/segment
!  FFFF   - etc. for scale factors
!  PPPP |
!  PPPP |
!  PPPP |
!  PPPP |
!  PPPP |
!  PPPP | - big-endian bit stream, as per precision specified
!  PPPP |
!  PPPP |
!  PPPP |
!  PPPP |
!  PPPP |
!  SLOP   - some minor roundup
!  PPPP |
!  PPPP |
!  PPPP | - separate bitstream where 1's flag positions of FNILs
!  PPPP |
!  PPP? |
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
!                         io  i      i      i    i(opt) (default is FALSE)
!
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2006-08-29  SMCook       Version 2 introduction -- provides FNIL support
!                               via an additional bitstream marking positions
!                               of FNIL samples.
!  3. 2006-01-10  B. Menger    Removed unused variables.
!  2. 2004-08-23  SMCook       Added FNIL error trap.
!  1. 2004-04-06  SMCook       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! Has not been tested on 64-bit architectures.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! Please see lbo_crou.c.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module lbo_module

  use named_constants_module

  implicit none

  private

  public :: lbo_get_recl_excluding_header
  public :: lbo_compress_trace
  public :: lbo_uncompress_trace

  character(len=100),public,save :: lbo_ident = &
  '$Id: lbo.f90,v 1.4 2006/08/30 13:15:15 SMCook prod sps $'

!-------------------------------------------------------------------------------

  interface
    subroutine lbo_compress_trace_c(                                   &
      version,arr,nsamps,hbuf,hlen,precision,samps_per_pack,status)

      integer,intent(in)                    :: version
      real   ,intent(in)                    :: arr
      integer,intent(in)                    :: nsamps
      integer,intent(in)                    :: hbuf
      integer,intent(in)                    :: hlen
      integer,intent(in)                    :: precision
      integer,intent(in)                    :: samps_per_pack
      integer,intent(out)                   :: status
    end subroutine lbo_compress_trace_c
  end interface


  interface
    subroutine lbo_uncompress_trace_c(                            &
      hbuf,hlen,arr,nsamps,version,precision,samps_per_pack,status)

      integer,intent(in)                    :: hbuf
      integer,intent(in)                    :: hlen
      real   ,intent(in)                    :: arr
      integer,intent(in)                    :: nsamps
      integer,intent(out)                   :: version
      integer,intent(out)                   :: precision
      integer,intent(out)                   :: samps_per_pack
      integer,intent(out)                   :: status
    end subroutine lbo_uncompress_trace_c
  end interface

!-------------------------------------------------------------------------------

  integer,parameter,public :: LBO_FNIL_VIOLATION = -999

  contains

!-------------------------------------------------------------------------------

  subroutine lbo_get_recl_excluding_header(     &
    version,nsamps,samps_per_pack,precision,recl)
    integer,intent(in)      :: version,nsamps,samps_per_pack,precision
    integer,intent(out)     :: recl

    call lbo_get_recl_excluding_header_c( &
      version,nsamps,samps_per_pack,precision,recl)
  end subroutine lbo_get_recl_excluding_header

!-------------------------------------------------------------------------------

  subroutine lbo_compress_trace(                         &
    version,arr,hbuf,precision,samps_per_pack,status)

    integer,intent(in)                        :: version
    real   ,intent(inout),dimension(:)        :: arr
    integer,intent(inout),dimension(:)        :: hbuf
    integer,intent(in)                        :: precision
    integer,intent(in)                        :: samps_per_pack
    integer,intent(inout)                     :: status

    integer                           :: i, imax, nsamps, hlen

    nsamps = size(arr)
    hlen = 4*size(hbuf)

    ! 3 consecutive FNILs is an error in LBO version 1
    if(version == 1) then
      imax = nsamps - 2
      do i=1,nsamps
        if(arr(i) == FNIL) then
          if(arr(i+1) == FNIL) then
            if(arr(i+2) == FNIL) then
              status = LBO_FNIL_VIOLATION
              return
            endif
          endif
        endif
      enddo
    endif

    !
    ! FNIL insertion (for testing/debugging lbo format version 2).
    !
    !itarget = 1
    !do i=1,nsamps
    !  if(i == itarget) then
    !    arr(i) = FNIL
    !    itarget = itarget + 3
    !  else
    !    arr(i) = i - 1
    !  endif
    !enddo

    call lbo_compress_trace_c(                     &
           version, arr(1), nsamps, hbuf(1), hlen, &
           precision, samps_per_pack, status)

  end subroutine lbo_compress_trace

!-------------------------------------------------------------------------------

  subroutine lbo_uncompress_trace( &
    hbuf,arr,version,precision,samps_per_pack,status)

    integer,intent(inout),dimension(:) :: hbuf
    real   ,intent(inout),dimension(:) :: arr
    integer,intent(inout)             :: version,precision,samps_per_pack,status

    integer        :: nsamps, hlen

    hlen = 4*size(hbuf)
    nsamps = size(arr)

    version = -1
    precision = -1
    status = -1
    arr = 0
    call lbo_uncompress_trace_c( &
           hbuf(1),hlen,arr(1),nsamps,version,precision,samps_per_pack,status)

  end subroutine lbo_uncompress_trace

!-------------------------------------------------------------------------------

end module lbo_module
