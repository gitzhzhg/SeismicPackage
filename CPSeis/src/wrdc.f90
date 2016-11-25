!<CPS_v1 type="PRIMITIVE"/>
! other files are:  wrdc_crou.c  wrdc.h
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
! Name       : wrdc 
! Category   : io
! Written    : 1999-10-06   by: Bill Menger
! Revised    : 2004-05-03   by: Randy Selzler
! Maturity   : production
! Purpose    : Convert from one word format to another.
! Portability: possibly 4-byte word length only.
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! This function set does primitive word conversion of scalars or vectors
! from native IEEE to IEEE for other machines and/or packs/unpacks data as
! follows:
!  double precision --> 
!                      real OR real non-native
!                      integer*2 OR integer*2 non-native
!                      integer*1 OR integer*1 non-native
!  real             -->
!                      double precision OR double precision non-native
!                      integer*2 OR integer*2 non-native
!                      integer*1 OR integer*1 non-native
!  integer(kind=2)  --> 
!                      double precision OR double precision non-native
!                      real OR real non-native
!                      integer*1 OR integer*1 non-native
!  integer(kind=1)  -->
!                      double precision OR double precision non-native
!                      real OR real non-native
!                      integer*2 OR integer*2 non-native
!  real             -->ibm floating point
!  ibm floating     -->real
!  ascii            -->ebcdic
!  ebcdic           -->ascii
!  integer(kind=1)  -->unsigned integer(kind=1) and vice versa.
!  
!  It also allows you to scale a real or double precision vector to fit within
!  the range allowed by a signed integer of nbits long (typically 16 or 8).
!  *** A dynamic range compression/decompression is also available, 
!      which can be applied before scaling(compress) and after 
!      unscaling(decompress).  
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
! status  =  wrdc_pack  (vec,lgth,fromsiz,tosiz,swap)
! status  =  wrdc_unpack(vec,lgth,fromsiz,tosiz,swap)
!
! NOTE: ***** packing precedes swapping, and swapping precedes unpacking.
!
!                      io  i(opt)
! call wrdc_compress  (vec,power)
! call wrdc_uncompress(vec,power)
!  OR
!                       i    o   i(opt)
! call wrdc_compress  (ivec,ovec,power)
! call wrdc_uncompress(ivec,ovec,power)
!
! Purpose: To compress the dynamic range of a vector by raising it to a 
!          fractional power (like 1/2, 1/4...)
!   real | dble    :: vec(:) The in/out vector to be scaled
!   integer        :: power  Optional power to use( default is 4)
!   Ex: To use 1/3, call wrdc_compress(vec,3); call wrdc_uncompress(vec,3)
!
!                    io   o     i      i(opt)
! call wrdc_scale  (vec,scale,nbits, set_max_abs)
!                    io   i
! call wrdc_unscale(vec,scale)
! OR
!                    i    o     o     i      i(opt)
! call wrdc_scale  (ivec,ovec,scale,nbits, set_max_abs)
!                    i    o     i
! call wrdc_unscale(ivec,ovec,scale)
!
! Purpose: To scale a REAL vector to fit within signed integer word of nbits.
!          in size.
!   real    :: vec(:)      The in/out vector to be scaled
!   real    :: scale       The scale factor used in scaling the vector
!   integer :: nbits       The number of bits the scaled vector will fit within.
!   real    :: set_max_abs Optional desired maximum absolute value.
!   NOTE: vec is not PACKED.  You must subsequently call a pack routine to
!         squish it into an integer vector of nbits per word.
!
!                         io  i   i(opt)
! call wrdc_ibm_to_float(vec,lgth,swap)(swap is used!!)
! call wrdc_float_to_ibm(vec,lgth,swap)(swap is no longer used--can be ignored)
!          OR
!                         i    o    i   i(opt)
! call wrdc_ibm_to_float(ivec,ovec,lgth,swap)
! call wrdc_float_to_ibm(ivec,ovec,lgth,swap)
! Where ivec is untouched, all output is put into ovec.
! NOTE::: ovec MUST be size and shape of ivec
!
!
! NOTE: ***** swapping precedes conversion from ibm (if swap=true)
! NOTE: ***** swapping precedes conversion to ibm   (if machine-endian=0)
! NOTE: ***** IBM is always written "big-endian".
!
!                    io 
! call wrdc_ebc_asc(chars)
! call wrdc_asc_ebc(chars)
!     OR              i       o
! call wrdc_ebc_asc(inchars,outchars)
! call wrdc_asc_ebc(inchars,outchars)
! Where inchars are not touched, output is put into outchars.
! NOTE::: outchars MUST be size and shape of inchars
!
! real | double precision | integer*4 | integer*2 | integer*1 :: vec(:)
! real | double precision | integer*4 | integer*2 | integer*1 :: ivec(:)
! real | double precision | integer*4 | integer*2 | integer*1 :: ovec(:)
!                  OR
! real | double precision | integer*4 | integer*2 | integer*1 :: vec,ivec,ovec
!                                        buffer with data. (operation done
!                                        in place).
! 
! integer                 :: status    ! 0 = ok, -1 = problem.
! integer                 :: lgth      ! length in type(vec) words of vec.
! integer                 :: fromsiz   ! number of bytes in vec originally
! integer                 :: tosiz     ! number of bytes to put vec into.
! logical                 :: swap      ! logical to say whether to swap bytes.
! character(len=*),dimension(:) :: chars,inchars,outchars
!  OR
! character(len=*)              :: chars,inchars,outchars
!
!                   i     i        o
! call wrdc_sb_to_usb(n, signed, unsigned)
! call wrdc_usb_to_sb(n, unsigned, signed)
!
! Purpose: Convert signed to unsigned (8-bit) data or vice versa.
!
! integer                        :: n         ! number of bytes to convert.
! real/integer/char,dimension(:) :: signed    !array to convert
! real/integer/char,dimension(:) :: unsigned  !array to put results in.
! If real/integer then each byte is converted, assuming you have packed data
! inside the real/integer array.
!
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
! 17. 2004-05-03  R Selzler    Correct two compiler errors, Absoft 8.0 with
!                              latest quick fix: Dummy arguments with the
!                              INTENT(OUT) attribute must be defined before use.
! 16. 2001-05-14  Ed Schmauch  Added set_max_abs optional argument to
!                              wrdc_scale.  set_max_abs enables AMPL_MAX
!                              parameter in trot.
! 15. 2001-03-23  Bill Menger  Added signed/unsigned conversion codes.
! 14. 2000-08-14  Bill Menger  Modified to only copy input to output up to
!                              lgth items when appropriate.
! 13. 2000-07-11  Bill Menger  Added swap_endian() to ibm float converter.
! 12. 2000-06-27  Bill Menger  changed ilen to jlen for intrinsic conflict.
! 11. 2000-04-10  Bill Menger  Allowed for extra calls using in-buf,out-buf.
! 10. 2000-03-08  Bill Menger  Modified documentation.
!  9. 2000-01-27  Bill Menger  Overloaded compress/scale for dble prec.
!  8. 2000-01-21  Bill Menger  Added compress/scale routines.
!  7. 2000-01-11  Bill Menger  Fixed the ibm-float endian problem and tested
!                              output from Solaris and Pentium using su codes.
!  6. 2000-01-07  Bill Menger  Modified ibm-float endian again.
!  5. 1999-12-09  Bill Menger  Fixed bug where ibm-float endian was backwards.
!  4. 1999-12-08  Bill Menger  Added ident string
!  3. 1999-11-10  Bill Menger  Fleshed out the stubs into real routines.
!  2. 1999-10-06  Bill Menger  Put dummy stubs in place.
!  1. 1999-10-06  Bill Menger  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
! No known limitations.
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!-------------------------------------------------------------------------------
!</compile_doc>
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!-------------------------------------------------------------------------------
!</algorithm_doc>
!  The compress routine (and its conjugate, the uncompress routine)
!  use dynamic range compression. Typical usage:
!  call wrdc_compress(vec) (defaults to power= 4 (raising to 1/4 power)
!  call wrdc_scale(vec,scale,nbits)
!  i = wrdc_pack(vec,LEN,4,nbits/8,swap)
!  ...
!  i = wrdc_unpack(vec,LEN,nbits/8,4,swap)
!  call wrdc_unscale(vec,scale)
!  call wrdc_uncompress(vec) (uses 4th power by default)
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module wrdc_module
  use swap_module
  use sizeof_module
  use named_constants_module

  implicit none

  private

  public :: wrdc_pack, wrdc_unpack
  public :: wrdc_ibm_to_float,wrdc_float_to_ibm
  public :: wrdc_ebc_asc, wrdc_asc_ebc
  public :: wrdc_compress, wrdc_uncompress
  public :: wrdc_scale,wrdc_unscale
  public :: wrdc_sb_to_usb, wrdc_usb_to_sb

  character(len=100),public,save :: wrdc_ident = &
  '$Id: wrdc.f90,v 1.17 2004/05/03 11:29:43 Selzler prod sps $'

  interface
    subroutine wrdc_sbtousb_ca(n, sb,usb)
      integer         ,intent(in)                :: n
      character(len=*),intent(in)                :: sb
      character(len=*),intent(out)               :: usb
    end subroutine wrdc_sbtousb_ca 
  end interface

  interface
    subroutine wrdc_usbtosb_ca(n, usb,sb)
      integer         ,intent(in)                :: n
      character(len=*),intent(in)                :: usb
      character(len=*),intent(out)               :: sb
    end subroutine wrdc_usbtosb_ca 
  end interface

  interface
    subroutine wrdc_sbtousb_ci(n, sb,usb)
      integer,intent(in)                :: n
      integer,intent(in)                :: sb
      integer,intent(out)               :: usb
    end subroutine wrdc_sbtousb_ci 
  end interface

  interface
    subroutine wrdc_usbtosb_ci(n, usb,sb)
      integer,intent(in)                :: n
      integer,intent(in)                :: usb
      integer,intent(out)               :: sb
    end subroutine wrdc_usbtosb_ci 
  end interface

  interface
    subroutine wrdc_sbtousb_cr(n, sb,usb)
      integer,intent(in)                :: n
      real,   intent(in)                :: sb
      real,   intent(out)               :: usb
    end subroutine wrdc_sbtousb_cr 
  end interface

  interface
    subroutine wrdc_usbtosb_cr(n, usb,sb)
      integer,intent(in)                :: n
      real,   intent(in)                :: usb
      real,   intent(out)               :: sb
    end subroutine wrdc_usbtosb_cr 
  end interface

  interface wrdc_sb_to_usb
    module procedure wrdc_sb_to_usb_char
    module procedure wrdc_sb_to_usb_int
    module procedure wrdc_sb_to_usb_real
  end interface

  interface wrdc_usb_to_sb
    module procedure wrdc_usb_to_sb_char
    module procedure wrdc_usb_to_sb_int
    module procedure wrdc_usb_to_sb_real
  end interface


  interface wrdc_pack_only
    integer function wrdc_packf_dble(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      double precision, intent(inout)    :: vec
    end function wrdc_packf_dble

    integer function wrdc_packf_real(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      real            , intent(inout)    :: vec
    end function wrdc_packf_real

!    integer function wrdc_packi_dble(vec,lgth,from,to) result (status)
!      integer,intent(in)                 :: lgth,from,to
!      integer(kind=8) , intent(inout)    :: vec
!    end function wrdc_packi_dble

    integer function wrdc_packi_real(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      integer         , intent(inout)    :: vec
    end function wrdc_packi_real

    integer function wrdc_packi_short(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      integer(kind=2) , intent(inout)    :: vec
    end function wrdc_packi_short

  end interface

  interface wrdc_unpack_only
    integer function wrdc_unpackf_dble(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      double precision, intent(inout)    :: vec
    end function wrdc_unpackf_dble

    integer function wrdc_unpackf_real(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      real            , intent(inout)    :: vec
    end function wrdc_unpackf_real

!    integer function wrdc_unpacki_dble(vec,lgth,from,to) result (status)
!      integer,intent(in)                 :: lgth,from,to
!      integer(kind=8) , intent(inout)    :: vec
!    end function wrdc_unpacki_dble

    integer function wrdc_unpacki_real(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      integer         , intent(inout)    :: vec
    end function wrdc_unpacki_real

    integer function wrdc_unpacki_short(vec,lgth,from,to) result (status)
      integer,intent(in)                 :: lgth,from,to
      integer(kind=2) , intent(inout)    :: vec
    end function wrdc_unpacki_short

  end interface

  interface wrdc_pack
    module procedure wrdc_packd
    module procedure wrdc_packr
    module procedure wrdc_packi
    module procedure wrdc_packs
    module procedure wrdc_packdvec
    module procedure wrdc_packrvec
    module procedure wrdc_packivec
    module procedure wrdc_packsvec
  end interface

  interface wrdc_unpack
    module procedure wrdc_unpackd
    module procedure wrdc_unpackr
    module procedure wrdc_unpacki
    module procedure wrdc_unpacks
    module procedure wrdc_unpackdvec
    module procedure wrdc_unpackrvec
    module procedure wrdc_unpackivec
    module procedure wrdc_unpacksvec
  end interface
  
  interface 
    subroutine wrdc_asc_ebc_c(string,lgth)
      character(len=*) ,intent(inout) :: string
      integer                         :: lgth
    end subroutine wrdc_asc_ebc_c
  end interface

  interface wrdc_asc_ebc
    module procedure wrdc_asc_ebc_1
    module procedure wrdc_asc_ebc_s
    module procedure wrdc_asc_ebc_1_2a
    module procedure wrdc_asc_ebc_s_2a
  end interface
  
  interface
    subroutine wrdc_ebc_asc_c(string,lgth)
      character(len=*) ,intent(inout) :: string
      integer                         :: lgth
    end subroutine wrdc_ebc_asc_c
  end interface

  interface wrdc_ebc_asc
    module procedure wrdc_ebc_asc_1
    module procedure wrdc_ebc_asc_s
    module procedure wrdc_ebc_asc_1_2a
    module procedure wrdc_ebc_asc_s_2a
  end interface

  interface
    subroutine wrdc_ibm_to_float_c(vec,lgth,endian)
      real,intent(inout)          :: vec
      integer,intent(in)          :: lgth
      integer,intent(in)          :: endian
    end subroutine wrdc_ibm_to_float_c
  end interface

  interface
    subroutine wrdc_float_to_ibm_c(vec,lgth,endian)
      real,intent(inout)          :: vec
      integer,intent(in)          :: lgth
      integer,intent(in)          :: endian
    end subroutine wrdc_float_to_ibm_c
  end interface

  interface wrdc_ibm_to_float
    module procedure wrdc_ibm_to_float_1 
    module procedure wrdc_ibm_to_float_s
    module procedure wrdc_ibm_to_float_1_2a
    module procedure wrdc_ibm_to_float_s_2a
  end interface

  interface wrdc_float_to_ibm
    module procedure wrdc_float_to_ibm_1 
    module procedure wrdc_float_to_ibm_s
    module procedure wrdc_float_to_ibm_1_2a 
    module procedure wrdc_float_to_ibm_s_2a
  end interface

  interface wrdc_scale
    module procedure wrdc_scale_real
    module procedure wrdc_scale_dble
    module procedure wrdc_scale_real_2a
    module procedure wrdc_scale_dble_2a
  end interface
  
  interface wrdc_unscale
    module procedure wrdc_unscale_real
    module procedure wrdc_unscale_dble
    module procedure wrdc_unscale_real_2a
    module procedure wrdc_unscale_dble_2a
  end interface

  interface wrdc_compress
    module procedure wrdc_compress_real
    module procedure wrdc_compress_dble
    module procedure wrdc_compress_real_2a
    module procedure wrdc_compress_dble_2a
  end interface

  interface wrdc_uncompress
    module procedure wrdc_uncompress_real
    module procedure wrdc_uncompress_dble
    module procedure wrdc_uncompress_real_2a
    module procedure wrdc_uncompress_dble_2a
  end interface

  contains

  subroutine wrdc_ibm_to_float_s(vec,lgth,swap)
    real,intent(inout)                        :: vec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    integer                                   :: endian,to,status
    !-------- Set the "endian" to the opposite of this machine's if we
    !-------- are told to swap bytes.
    if(present(swap) ) then
      if(swap) then
        endian = 1 - swap_endian()
      else
        endian = swap_endian()
      endif
    else
      endian = swap_endian()
    endif

    !-------- Now we can convert from 4 byte IBM with the endian of choice.
    call wrdc_ibm_to_float_c(vec,lgth,endian)
    !-------- Test for size of a real.  If not 4 bytes, fix that now.
    to = sizeof(vec)
    if(to > 4) then
      status = wrdc_pack(vec,lgth,4,to)
    elseif (to < 4 ) then
      status = wrdc_unpack(vec,lgth,4,to)
    endif
  end subroutine wrdc_ibm_to_float_s

  subroutine wrdc_ibm_to_float_1(vec,lgth,swap)
    real,intent(inout),dimension(:)           :: vec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    if(present(swap)) then
      call wrdc_ibm_to_float_s(vec(1),lgth,swap)
    else
      call wrdc_ibm_to_float_s(vec(1),lgth)
    endif
  end subroutine wrdc_ibm_to_float_1

  subroutine wrdc_float_to_ibm_s(vec,lgth,swap)
    real,intent(inout)                        :: vec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    integer                                   :: endian,from,status
    !--------IGNORE THE SWAP PARAMETER-------------

    !-------- Test for size of a real.  If not 4 bytes, fix that now.
    from = sizeof(vec)
    if(from > 4) then
      status = wrdc_pack(vec,lgth,from,4)
    elseif (from < 4 ) then
      status = wrdc_unpack(vec,lgth,from,4)
    endif

    !-------- Now we can convert to 4 byte IBM.

    endian = swap_endian() !wmm added 7/10/2000

    call wrdc_float_to_ibm_c(vec,lgth,endian)

  end subroutine wrdc_float_to_ibm_s

  subroutine wrdc_float_to_ibm_1(vec,lgth,swap)
    real,intent(inout),dimension(:)           :: vec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    if(present(swap)) then
      call wrdc_float_to_ibm_s(vec(1),lgth,swap)
    else
      call wrdc_float_to_ibm_s(vec(1),lgth)
    endif
  end subroutine wrdc_float_to_ibm_1

!--------- double buffered versions of the above

  subroutine wrdc_ibm_to_float_s_2a(ivec,ovec,lgth,swap)
    real,intent(   in)                        :: ivec
    real,intent(  out)                        :: ovec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    integer                                   :: endian,to,status
    !-------- Set the "endian" to the opposite of this machine's if we
    !-------- are told to swap bytes.
    ovec        = ivec        
    if(present(swap) ) then
      if(swap) then
        endian = 1 - swap_endian()
      else
        endian = swap_endian()
      endif
    else
      endian = swap_endian()
    endif

    !-------- Now we can convert from 4 byte IBM with the endian of choice.
    call wrdc_ibm_to_float_c(ovec,lgth,endian)
    !-------- Test for size of a real.  If not 4 bytes, fix that now.
    to = sizeof(ovec)
    if(to > 4) then
      status = wrdc_pack(ovec,lgth,4,to)
    elseif (to < 4 ) then
      status = wrdc_unpack(ovec,lgth,4,to)
    endif
  end subroutine wrdc_ibm_to_float_s_2a

  subroutine wrdc_ibm_to_float_1_2a(ivec,ovec,lgth,swap)
    real,intent(   in),dimension(:)           :: ivec
    real,intent(  out),dimension(:)           :: ovec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    ovec(:lgth) = ivec(:lgth)
    if(present(swap)) then
      call wrdc_ibm_to_float_s(ovec(1),lgth,swap)
    else
      call wrdc_ibm_to_float_s(ovec(1),lgth)
    endif
  end subroutine wrdc_ibm_to_float_1_2a

  subroutine wrdc_float_to_ibm_s_2a(ivec,ovec,lgth,swap)
    real,intent(   in)                        :: ivec
    real,intent(  out)                        :: ovec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    integer                                   :: endian,from,status
    ovec        = ivec
    !--------IGNORE THE SWAP PARAMETER-------------

    !-------- Test for size of a real.  If not 4 bytes, fix that now.
    from = sizeof(ovec)
    if(from > 4) then
      status = wrdc_pack(ovec,lgth,from,4)
    elseif (from < 4 ) then
      status = wrdc_unpack(ovec,lgth,from,4)
    endif

    !-------- Now we can convert to 4 byte IBM.

    endian = swap_endian() !wmm added 7/10/2000

    call wrdc_float_to_ibm_c(ovec,lgth,endian)

  end subroutine wrdc_float_to_ibm_s_2a

  subroutine wrdc_float_to_ibm_1_2a(ivec,ovec,lgth,swap)
    real,intent(   in),dimension(:)           :: ivec
    real,intent(  out),dimension(:)           :: ovec
    integer,intent(in)                        :: lgth
    logical,intent(in),optional               :: swap
    ovec(:lgth) = ivec(:lgth)
    if(present(swap)) then
      call wrdc_float_to_ibm_s(ovec(1),lgth,swap)
    else
      call wrdc_float_to_ibm_s(ovec(1),lgth)
    endif
  end subroutine wrdc_float_to_ibm_1_2a
!----------------------------------------------------------------------

  integer function wrdc_packdvec(vec,lgth,from,to,swap) result (status)
    double precision,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_pack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_pack(vec(1),lgth,from,to)
    endif
  end function wrdc_packdvec
  integer function wrdc_packd(vec,lgth,from,to,swap) result (status)
    double precision,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    status =  wrdc_pack_only(vec,lgth,from,to)
    if(present(swap)) then
      if (swap) call swap_bytes_unk(vec,to,lgth)
    endif
  end function wrdc_packd

  integer function wrdc_packrvec(vec,lgth,from,to,swap) result (status)
    real            ,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_pack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_pack(vec(1),lgth,from,to)
    endif
  end function wrdc_packrvec
  integer function wrdc_packr(vec,lgth,from,to,swap) result (status)
    real            ,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    status =  wrdc_pack_only(vec,lgth,from,to)
    if(present(swap)) then
      if (swap) call swap_bytes_unk(vec,to,lgth)
    endif
  end function wrdc_packr

  integer function wrdc_packivec(vec,lgth,from,to,swap) result (status)
    integer ,        intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_pack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_pack(vec(1),lgth,from,to)
    endif
  end function wrdc_packivec

  integer function wrdc_packi(vec,lgth,from,to,swap) result (status)
    integer ,        intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    status =  wrdc_pack_only(vec,lgth,from,to)
    if(present(swap)) then
      if (swap) call swap_bytes_unk(vec,to,lgth)
    endif
  end function wrdc_packi

  integer function wrdc_packsvec(vec,lgth,from,to,swap) result (status)
    integer(kind=2) ,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_pack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_pack(vec(1),lgth,from,to)
    endif
  end function wrdc_packsvec
  integer function wrdc_packs(vec,lgth,from,to,swap) result (status)
    integer(kind=2) ,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    status =  wrdc_pack_only(vec,lgth,from,to)
    if(present(swap)) then
      if (swap) call swap_bytes_unk(vec,to,lgth)
    endif
  end function wrdc_packs

  integer function wrdc_unpackdvec(vec,lgth,from,to,swap)result (status)
    double precision,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_unpack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_unpack(vec(1),lgth,from,to)
    endif
  end function wrdc_unpackdvec
  integer function wrdc_unpackd(vec,lgth,from,to,swap) result (status)
    double precision,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap) ) then
      if(swap ) call swap_bytes_unk(vec,from,lgth)
    endif
    status =  wrdc_unpack_only(vec,lgth,from,to)
  end function wrdc_unpackd

  integer function wrdc_unpackrvec(vec,lgth,from,to,swap)result (status)
    real            ,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_unpack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_unpack(vec(1),lgth,from,to)
    endif
  end function wrdc_unpackrvec
  integer function wrdc_unpackr(vec,lgth,from,to,swap) result (status)
    real            ,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap) ) then
      if(swap ) call swap_bytes_unk(vec,from,lgth)
    endif
    status =  wrdc_unpack_only(vec,lgth,from,to)
  end function wrdc_unpackr

  integer function wrdc_unpackivec(vec,lgth,from,to,swap)result (status)
    integer ,        intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_unpack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_unpack(vec(1),lgth,from,to)
    endif
  end function wrdc_unpackivec

  integer function wrdc_unpacki(vec,lgth,from,to,swap) result (status)
    integer ,        intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap) ) then
      if(swap ) call swap_bytes_unk(vec,from,lgth)
    endif
    status =  wrdc_unpack_only(vec,lgth,from,to)
  end function wrdc_unpacki

  integer function wrdc_unpacksvec(vec,lgth,from,to,swap) result(status)
    integer(kind=2) ,intent(inout)        :: vec(:)
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap)) then
      status = wrdc_unpack(vec(1),lgth,from,to,swap)
    else
      status = wrdc_unpack(vec(1),lgth,from,to)
    endif
  end function wrdc_unpacksvec
  integer function wrdc_unpacks(vec,lgth,from,to,swap) result (status)
    integer(kind=2) ,intent(inout)        :: vec
    integer,intent(in)                    :: lgth,from,to
    logical,intent(in),optional           :: swap
    if(present(swap) ) then
      if(swap ) call swap_bytes_unk(vec,from,lgth)
    endif
    status =  wrdc_unpack_only(vec,lgth,from,to)
  end function wrdc_unpacks

!---------------------------------------------------------------------

  subroutine wrdc_asc_ebc_1(string)
    character(len=*), dimension (:),intent(inout) :: string
    integer                                       :: i,jlen
    jlen  = len(string(1))
    do i = 1, size(string)
      call wrdc_asc_ebc_c(string(i),jlen)
    end do
  end subroutine wrdc_asc_ebc_1

  subroutine wrdc_asc_ebc_s(string)
    character(len=*),               intent(inout) :: string
    integer                                       :: temp
    temp = len(string)
    call wrdc_asc_ebc_c(string,temp)
  end subroutine wrdc_asc_ebc_s

  subroutine wrdc_ebc_asc_1(string)
    character(len=*), dimension (:),intent(inout) :: string
    integer                                       :: i,jlen
    jlen  = len(string(1))
    do i = 1, size(string)
      call wrdc_ebc_asc_c(string(i),jlen)
    end do
  end subroutine wrdc_ebc_asc_1

  subroutine wrdc_ebc_asc_s(string)
    character(len=*),               intent(inout) :: string
    integer                                       :: temp
    temp  = len(string)
    call wrdc_ebc_asc_c(string,temp)
  end subroutine wrdc_ebc_asc_s

! --- added these to allow in-buf and out-buf for the above 4 routines --

  subroutine wrdc_asc_ebc_1_2a(instring,outstring)
    character(len=*), dimension (:),intent( in) ::  instring
    character(len=*), dimension (:),intent(out) :: outstring
    integer                                       :: i,jlen
    outstring = instring
    jlen  = len(outstring(1))
    do i = 1, size(outstring)
      call wrdc_asc_ebc_c(outstring(i),jlen)
    end do
  end subroutine wrdc_asc_ebc_1_2a

  subroutine wrdc_asc_ebc_s_2a(instring,outstring)
    character(len=*),               intent( in) ::  instring
    character(len=*),               intent(out) :: outstring
    integer                                     :: temp
    outstring = instring
    temp = len(outstring)
    call wrdc_asc_ebc_c(outstring,temp)
  end subroutine wrdc_asc_ebc_s_2a

  subroutine wrdc_ebc_asc_1_2a(instring,outstring)
    character(len=*), dimension (:),intent( in) ::  instring
    character(len=*), dimension (:),intent(out) :: outstring
    integer                                       :: i,jlen
    outstring = instring
    jlen  = len(outstring(1))
    do i = 1, size(outstring)
      call wrdc_ebc_asc_c(outstring(i),jlen)
    end do
  end subroutine wrdc_ebc_asc_1_2a

  subroutine wrdc_ebc_asc_s_2a(instring,outstring)
    character(len=*),               intent( in) ::  instring
    character(len=*),               intent(out) :: outstring
    integer                                     :: temp
    outstring = instring
    temp = len(outstring)
    call wrdc_ebc_asc_c(outstring,temp)
  end subroutine wrdc_ebc_asc_s_2a
!-----------------------------------------------------------

  subroutine wrdc_compress_real(vec,power)
    real,intent(inout),dimension(:)               :: vec
    integer,optional,intent(in)                   :: power
    if(present(power) ) then
      select case (power)
        case (0)
          return
        case (2) 
          vec = sign(1.0,vec)*sqrt(abs(vec))
        case (4)
          vec = sign(1.0,vec)*sqrt(sqrt(abs(vec)))
        case (6)
          vec = sign(1.0,vec)*sqrt(sqrt(sqrt(abs(vec))))
        case default
          vec = sign(1.0,vec) * abs(vec)**(1/power)
      end select
    else
      vec = sign(1.0,vec)*sqrt(sqrt(abs(vec)))
    endif    
  end subroutine wrdc_compress_real

  subroutine wrdc_uncompress_real(vec,power)
    real,intent(inout),dimension(:)               :: vec
    integer,optional,intent(in)                   :: power
    if(present(power) ) then
      vec = sign(1.0,vec) * vec**power
    else
      vec = sign(1.0,vec) * vec**4
    endif    
  end subroutine wrdc_uncompress_real

  subroutine wrdc_compress_dble(vec,power)
    double precision,intent(inout),dimension(:)   :: vec
    integer,optional,intent(in)                   :: power
    if(present(power) ) then
      select case (power)
        case (0)
          return
        case (2) 
          vec = sign(1d0,vec)*sqrt(abs(vec))
        case (4)
          vec = sign(1d0,vec)*sqrt(sqrt(abs(vec)))
        case (6)
          vec = sign(1d0,vec)*sqrt(sqrt(sqrt(abs(vec))))
        case default
          vec = sign(1d0,vec) * abs(vec)**(1/power)
      end select
    else
      vec = sign(1d0,vec)*sqrt(sqrt(abs(vec)))
    endif    
  end subroutine wrdc_compress_dble

  subroutine wrdc_uncompress_dble(vec,power)
    double precision,intent(inout),dimension(:)   :: vec
    integer,optional,intent(in)                   :: power
    if(present(power) ) then
      vec = sign(1d0,vec) * vec**power
    else
      vec = sign(1d0,vec) * vec**4
    endif    
  end subroutine wrdc_uncompress_dble

!----------- double buffered versions of the above

  subroutine wrdc_compress_real_2a(ivec,ovec,power)
    real,intent(   in),dimension(:)               :: ivec
    real,intent(  out),dimension(:)               :: ovec
    integer,optional,intent(in)                   :: power

    ovec = ivec

    if(present(power) ) then
      select case (power)
        case (0)
          return
        case (2) 
          ovec = sign(1.0,ovec)*sqrt(abs(ovec))
        case (4)
          ovec = sign(1.0,ovec)*sqrt(sqrt(abs(ovec)))
        case (6)
          ovec = sign(1.0,ovec)*sqrt(sqrt(sqrt(abs(ovec))))
        case default
          ovec = sign(1.0,ovec) * abs(ovec)**(1/power)
      end select
    else
      ovec = sign(1.0,ovec)*sqrt(sqrt(abs(ovec)))
    endif    
  end subroutine wrdc_compress_real_2a

  subroutine wrdc_uncompress_real_2a(ivec,ovec,power)
    real,intent(   in),dimension(:)               :: ivec
    real,intent(  out),dimension(:)               :: ovec
    integer,optional,intent(in)                   :: power

    ovec = ivec

    if(present(power) ) then
      ovec = sign(1.0,ovec) * ovec**power
    else
      ovec = sign(1.0,ovec) * ovec**4
    endif    
  end subroutine wrdc_uncompress_real_2a

  subroutine wrdc_compress_dble_2a(ivec,ovec,power)
    double precision,intent(   in),dimension(:)   :: ivec
    double precision,intent(  out),dimension(:)   :: ovec
    integer,optional,intent(in)                   :: power

    ovec = ivec 

    if(present(power) ) then
      select case (power)
        case (0)
          return
        case (2) 
          ovec = sign(1d0,ovec)*sqrt(abs(ovec))
        case (4)
          ovec = sign(1d0,ovec)*sqrt(sqrt(abs(ovec)))
        case (6)
          ovec = sign(1d0,ovec)*sqrt(sqrt(sqrt(abs(ovec))))
        case default
          ovec = sign(1d0,ovec) * abs(ovec)**(1/power)
      end select
    else
      ovec = sign(1d0,ovec)*sqrt(sqrt(abs(ovec)))
    endif    
  end subroutine wrdc_compress_dble_2a

  subroutine wrdc_uncompress_dble_2a(ivec,ovec,power)
    double precision,intent(   in),dimension(:)   :: ivec
    double precision,intent(  out),dimension(:)   :: ovec
    integer,optional,intent(in)                   :: power

    ovec = ivec

    if(present(power) ) then
      ovec = sign(1d0,ovec) * ovec**power
    else
      ovec = sign(1d0,ovec) * ovec**4
    endif    
  end subroutine wrdc_uncompress_dble_2a
!--------------------------------------------------------------------

  subroutine wrdc_scale_real(vec,scale,nbits,set_max_abs)
    real,intent(inout),dimension(:)               :: vec
    real,intent(out)                              :: scale
    integer,intent(in)                            :: nbits
    real, intent(in), optional                    :: set_max_abs
    !--- Local variables 
    real                                          :: vabsmax,local_scale
    real                                          :: max_for_nbits

    !--- First, look to see if the vector is all zeros.  if so, just quit.
    !--- While we're at it, we can save the max value for later use.
    vabsmax = maxval(abs(vec))
    if(vabsmax == 0.0 ) then
      scale = 1.0
      return
    endif
    !-------------------------------------------------------------------
    !--- If you wanted to ensure your scale was always a power of 2 and was
    !--- no greater than the dynamic range afforded by a signed integer of
    !--- nbits long, you could use a more complicated scale factor below
    !--- but it won't guarantee to use all of the available range of bits
    !--- afforded by your integer words.
    !    scale = &
    !    exp(log(2.0)*floor(log((2**(nbits-1) - 1)/maxval(abs(vec)))/log(2.0)))
    !-------------------------------------------------------------------
    !--- This scale will always use the maximum of your available dynamic
    !--- range:
    !--- scale to fit within +/- nbits-1 word size (signed integer)
    max_for_nbits = 2**(nbits-1) - 1

    if (present(set_max_abs)) then
        local_scale = max_for_nbits / set_max_abs
    else
        local_scale = max_for_nbits / vabsmax
    endif

    !--- The "nint" function rounds off your scaled vector to the best integer
    !--- approximation of the actual value.  This is the step where your
    !--- accuracy is diminished by the number of bits you chose for output.
    vec = 1.0*nint(vec*local_scale)

    ! Clip array at +- max_for_nbits, if necessary.
    !
    if (present(set_max_abs)) then
        if (set_max_abs < vabsmax) then
            vec = min(vec,  max_for_nbits)
            vec = max(vec, -max_for_nbits)
        endif
    endif

    scale = local_scale
  end subroutine wrdc_scale_real

  subroutine wrdc_scale_dble(vec,scale,nbits,set_max_abs)
    double precision,intent(inout),dimension(:)   :: vec
    real,intent(out)                              :: scale
    integer,intent(in)                            :: nbits
    real, intent(in), optional                    :: set_max_abs
    !--- Local variables 
    double precision                              :: vabsmax,local_scale
    double precision                              :: max_for_nbits

    !--- First, look to see if the vector is all zeros.  if so, just quit.
    !--- While we're at it, we can save the max value for later use.
    vabsmax = maxval(abs(vec))
    if(vabsmax == 0d0 ) then
      scale = 1.0
      return
    endif
    !-------------------------------------------------------------------
    !--- If you wanted to ensure your scale was always a power of 2 and was
    !--- no greater than the dynamic range afforded by a signed integer of
    !--- nbits long, you could use a more complicated scale factor below
    !--- but it won't guarantee to use all of the available range of bits
    !--- afforded by your integer words.
    !    scale = &
    !    exp(log(2.0)*floor(log((2**(nbits-1) - 1)/maxval(abs(vec)))/log(2.0)))
    !-------------------------------------------------------------------
    !--- This scale will always use the maximum of your available dynamic
    !--- range:
    !--- scale to fit within +/- nbits-1 word size (signed integer)
    max_for_nbits = 2**(nbits-1) - 1

    if (present(set_max_abs)) then
        local_scale = max_for_nbits / dble(set_max_abs)
    else
        local_scale = max_for_nbits / vabsmax
    endif

    !--- The "nint" function rounds off your scaled vector to the best integer
    !--- approximation of the actual value.  This is the step where your
    !--- accuracy is diminished by the number of bits you chose for output.
    vec = 1d0*nint(vec*local_scale)

    ! Clip array at +- max_for_nbits, if necessary.
    !
    if (present(set_max_abs)) then
        if (dble(set_max_abs) < vabsmax) then
            vec = min(vec,  max_for_nbits)
            vec = max(vec, -max_for_nbits)
        endif
    endif

    scale = real(local_scale)
  end subroutine wrdc_scale_dble

  subroutine wrdc_unscale_real(vec,scale)
    real,intent(inout),dimension(:)               :: vec
    real,intent(in )                              :: scale
    !--- local variables ---
    double precision                              :: uscale

    !--- flip the scale for recovery.
    if(scale == 0.0 ) return
    uscale = 1d0/scale
    !--- unscale the input vector
    vec = vec*uscale
  end subroutine wrdc_unscale_real

  subroutine wrdc_unscale_dble(vec,scale)
    double precision,intent(inout),dimension(:)   :: vec
    real,intent(in )                              :: scale
    !--- local variables ---
    double precision                              :: uscale

    !--- flip the scale for recovery.
    if(scale == 0.0 ) return
    uscale = 1d0/scale
    !--- unscale the input vector
    vec = vec*uscale
  end subroutine wrdc_unscale_dble
!----------- double buffered of the above -----

  subroutine wrdc_scale_real_2a(ivec,ovec,scale,nbits,set_max_abs)
    real,intent(   in),dimension(:)               :: ivec
    real,intent(  out),dimension(:)               :: ovec
    real,intent(out)                              :: scale
    integer,intent(in)                            :: nbits
    real, intent(in), optional                    :: set_max_abs
    !--- Local variables 
    real                                          :: vabsmax,local_scale
    real                                          :: max_for_nbits

    ovec = ivec

    !--- First, look to see if the vector is all zeros.  if so, just quit.
    !--- While we're at it, we can save the max value for later use.
    vabsmax = maxval(abs(ovec))
    if(vabsmax == 0.0 ) then
      scale = 1.0
      return
    endif
    !-------------------------------------------------------------------
    !--- If you wanted to ensure your scale was always a power of 2 and was
    !--- no greater than the dynamic range afforded by a signed integer of
    !--- nbits long, you could use a more complicated scale factor below
    !--- but it won't guarantee to use all of the available range of bits
    !--- afforded by your integer words.
    !    scale = &
    !    exp(log(2.0)*floor(log((2**(nbits-1) - 1)/maxval(abs(vec)))/log(2.0)))
    !-------------------------------------------------------------------
    !--- This scale will always use the maximum of your available dynamic
    !--- range:
    !--- scale to fit within +/- nbits-1 word size (signed integer)
    max_for_nbits = 2**(nbits-1) - 1

    if (present(set_max_abs)) then
        local_scale = max_for_nbits / set_max_abs
    else
        local_scale = max_for_nbits / vabsmax
    endif

    !--- The "nint" function rounds off your scaled vector to the best integer
    !--- approximation of the actual value.  This is the step where your
    !--- accuracy is diminished by the number of bits you chose for output.
    ovec = 1.0*nint(ovec*local_scale)

    ! Clip array at +- max_for_nbits, if necessary.
    !
    if (present(set_max_abs)) then
        if (set_max_abs < vabsmax) then
            ovec = min(ovec,  max_for_nbits)
            ovec = max(ovec, -max_for_nbits)
        endif
    endif

    scale = local_scale
  end subroutine wrdc_scale_real_2a

  subroutine wrdc_scale_dble_2a(ivec,ovec,scale,nbits,set_max_abs)
    double precision,intent(   in),dimension(:)   :: ivec
    double precision,intent(  out),dimension(:)   :: ovec
    real,intent(out)                              :: scale
    integer,intent(in)                            :: nbits
    real, intent(in), optional                    :: set_max_abs
    !--- Local variables 
    double precision                              :: vabsmax,local_scale
    double precision                              :: max_for_nbits

    ovec = ivec

    !--- First, look to see if the vector is all zeros.  if so, just quit.
    !--- While we're at it, we can save the max value for later use.
    vabsmax = maxval(abs(ovec))
    if(vabsmax == 0d0 ) then
      scale = 1.0
      return
    endif
    !-------------------------------------------------------------------
    !--- If you wanted to ensure your scale was always a power of 2 and was
    !--- no greater than the dynamic range afforded by a signed integer of
    !--- nbits long, you could use a more complicated scale factor below
    !--- but it won't guarantee to use all of the available range of bits
    !--- afforded by your integer words.
    !    scale = &
    !    exp(log(2.0)*floor(log((2**(nbits-1) - 1)/maxval(abs(vec)))/log(2.0)))
    !-------------------------------------------------------------------
    !--- This scale will always use the maximum of your available dynamic
    !--- range:
    !--- scale to fit within +/- nbits-1 word size (signed integer)
    max_for_nbits = 2**(nbits-1) - 1

    if (present(set_max_abs)) then
        local_scale = max_for_nbits / dble(set_max_abs)
    else
        local_scale = max_for_nbits / vabsmax
    endif

    !--- The "nint" function rounds off your scaled vector to the best integer
    !--- approximation of the actual value.  This is the step where your
    !--- accuracy is diminished by the number of bits you chose for output.
    ovec = 1d0*nint(ovec*local_scale)

    ! Clip array at +- max_for_nbits, if necessary.
    !
    if (present(set_max_abs)) then
        if (dble(set_max_abs) < vabsmax) then
            ovec = min(ovec,  max_for_nbits)
            ovec = max(ovec, -max_for_nbits)
        endif
    endif

    scale = real(local_scale)
  end subroutine wrdc_scale_dble_2a

  subroutine wrdc_unscale_real_2a(ivec,ovec,scale)
    real,intent(   in),dimension(:)               :: ivec
    real,intent(  out),dimension(:)               :: ovec
    real,intent(in )                              :: scale
    !--- local variables ---
    double precision                              :: uscale

    ovec = ivec

    !--- flip the scale for recovery.
    if(scale == 0.0 ) return
    uscale = 1d0/scale
    !--- unscale the input vector
    ovec = ovec*uscale
  end subroutine wrdc_unscale_real_2a

  subroutine wrdc_unscale_dble_2a(ivec,ovec,scale)
    double precision,intent(   in),dimension(:)   :: ivec
    double precision,intent(  out),dimension(:)   :: ovec
    real,intent(in )                              :: scale
    !--- local variables ---
    double precision                              :: uscale

    ovec = ivec

    !--- flip the scale for recovery.
    if(scale == 0.0 ) return
    uscale = 1d0/scale
    !--- unscale the input vector
    ovec = ivec*uscale
  end subroutine wrdc_unscale_dble_2a


  subroutine wrdc_usb_to_sb_char(n,c_usb,c_sb)
    integer,intent(in)                        :: n
    character(len=*),dimension(:),intent(in)  :: c_usb
    character(len=*),dimension(:),intent(out) :: c_sb
    c_sb(1) = ' '
    call wrdc_usbtosb_ca(n,c_usb(1),c_sb(1))
  end subroutine wrdc_usb_to_sb_char

  subroutine wrdc_usb_to_sb_int(n,i_usb,i_sb)
    integer,intent(in)                        :: n
    integer,intent(in),dimension(:)           :: i_usb
    integer,intent(out),dimension(:)          :: i_sb
    call wrdc_usbtosb_ci(n,i_usb(1),i_sb(1))
  end subroutine wrdc_usb_to_sb_int

  subroutine wrdc_usb_to_sb_real(n,r_usb,r_sb)
    integer,intent(in)                       :: n
    real  ,intent(in),dimension(:)           :: r_usb
    real  ,intent(out),dimension(:)          :: r_sb

    call wrdc_usbtosb_cr(n,r_usb(1),r_sb(1))
  end subroutine wrdc_usb_to_sb_real

  subroutine wrdc_sb_to_usb_char(n,c_sb,c_usb)
    integer,intent(in)                        :: n
    character(len=*),dimension(:),intent(in)  :: c_sb
    character(len=*),dimension(:),intent(out) :: c_usb
    c_usb(1) = ' '
    call wrdc_sbtousb_ca(n,c_sb(1),c_usb(1))
  end subroutine wrdc_sb_to_usb_char

  subroutine wrdc_sb_to_usb_int(n,i_sb,i_usb)
    integer,intent(in)                        :: n
    integer,intent(in),dimension(:)           :: i_sb
    integer,intent(out),dimension(:)          :: i_usb
    call wrdc_sbtousb_ci(n,i_sb(1),i_usb(1))
  end subroutine wrdc_sb_to_usb_int

  subroutine wrdc_sb_to_usb_real(n,r_sb,r_usb)
    integer,intent(in)                       :: n
    real  ,intent(in),dimension(:)           :: r_sb
    real  ,intent(out),dimension(:)          :: r_usb

    call wrdc_sbtousb_cr(n,r_sb(1),r_usb(1))
  end subroutine wrdc_sb_to_usb_real

end module wrdc_module
