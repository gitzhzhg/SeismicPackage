
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- pkutil.f90 -------------------------------!!
!!------------------------------- pkutil.f90 -------------------------------!!
!!------------------------------- pkutil.f90 -------------------------------!!


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
! Name       : PKUTIL 
! Category   : packs
! Written    : 1996-10-30   by: Tom Stoeckley
! Revised    : 2001-04-11   by: Tom Stoeckley
! Maturity   : production   2001-04-30
! Purpose    : Packing and unpacking utility.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!         Pack arrays of real (floating point) data values into
!         shorter integer arrays, with several values in each
!         computer word.
!
!         Also unpack these integer arrays, restoring the original
!         real values (but with less precision of course).
!
!         These routines work on 4-byte and 8-byte platforms.
!         Data values are packed from the native word size
!         (32 or 64 bits) into 1 or 8 or 16 or 32 bits.
!
!         The following packing occurs on each type platform:
!
!    Packing to these many bits   On 4-byte platforms   On 8-byte platforms
!    --------------------------   -------------------   -------------------
!               1                       32:1                  64:1
!               8                        4:1                   8:1
!              16                        2:1                   4:1
!              32                        1:1                   2:1
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
! To pack or unpack an array to 1 or 8 or 16 or 32 bits:
!
!                        o                       i   i
!                      npack = pkutil_npack   (nbits,n)
!                      npack = pkutil_npack1        (n)
!                      npack = pkutil_npack8        (n)
!                      npack = pkutil_npack16       (n)
!                      npack = pkutil_npack32       (n)
!
!                                                                opt
!                                       i   i i     o     i       i
!               CALL PKUTIL_PACK     (nbits,X,N,   PACK,NPACK,   LAV)
!               CALL PKUTIL_PACK1          (X,N,   PACK,NPACK)
!               CALL PKUTIL_PACK8          (X,N,   PACK,NPACK,   LAV)
!               CALL PKUTIL_PACK16         (X,N,   PACK,NPACK,   LAV)
!               CALL PKUTIL_PACK32         (X,N,   PACK,NPACK,   LAV)
!
!                                       i    i     i      o i
!               CALL PKUTIL_UNPACK   (nbits,PACK,NPACK,   X,N)
!               CALL PKUTIL_UNPACK1        (PACK,NPACK,   X,N)
!               CALL PKUTIL_UNPACK8        (PACK,NPACK,   X,N)
!               CALL PKUTIL_UNPACK16       (PACK,NPACK,   X,N)
!               CALL PKUTIL_UNPACK32       (PACK,NPACK,   X,N)
!
! integer       NBITS = number of bits in a single packed value (1,8,16,32).
! integer           N = number of floating point data values to pack or unpack.
! integer       NPACK = number of integers required to store the packed values.
! real           X(N) = array of data values to be packed or restored.
! integer PACK(NPACK) = array containing the packed values.
! real            LAV = largest absolute value for scaling.
!
! The functions which return NPACK can be used to determine the packed array
! length required to store N unpacked values.  This length is sufficient to
! store a scale factor also (except when packing to 1 bit), so that the scale
! of the original values can be restored.
!
!   Packing to these many bits   On 4-byte platforms   On 8-byte platforms
!   --------------------------   -------------------   -------------------
!              1                 npack = (n+31)/32     npack = (n+63)/64
!              8                 npack = (n+3)/4 + 1   npack = (n+7)/8 + 1
!             16                 npack = (n+1)/2 + 1   npack = (n+3)/4 + 1
!             32                 npack = n             npack = (n+1)/2 + 1
!
! Note that on 4-byte platforms, packing to 32 bits is a do-nothing operation.
! A simple copy is performed between the X and PACK arrays with no type
! conversion.  Packing followed by unpacking returns the exact same values.
!
! If the entire array PACK(NPACK) is set to zeroes, and then the
! unpacking routine is called, all of the unpacked values will be zero.
!
! The first routine in each group above (with the argument NBITS) simply
! calls one of the other routines in a SELECT CASE statement, and therefore
! is not as efficient as the other routines.
!
! If NBITS is set to anything other than 1 or 8 or 16 or 32, the routine
! simply stops with an error message.  Although this is actually a run-time
! error, it is considered to be a programmer error, not a run-time error.
!
! If the LAV argument is present, it is used for scaling the values.
! Otherwise the largest absolute value in the X(N) array is used.
! If the LAV argument is negative, its absolute value is used.
!
! If you wish to save the packed array PACK(NPACK) without the scale factor
! (e.g. to a trace file), then you must subtract 1 from NPACK when saving
! the array for cases where "+1" is included in the calculation of NPACK
! in the above table.
!
! If you wish to know what the scale factor is for the packed array, it will
! be the last element in PACK(NPACK) for cases where "+1" is included in the
! calculation of NPACK in the above table.  This will be a real value stored
! in an integer word.
!
!-------------------------------------------------------------------------------
!!!                      OBSOLETE DOCUMENTATION
!!!
!!! This obsolete documentation from the old CPS system is retained here
!!! as a record of the previous way this primitive worked on the old system.
!!! On the old system, a 64-bit word size was assumed.  Note that the
!!! integers appended to the subroutine names in the old system were the
!!! packing ratio, whereas the integers appended to the subroutine names
!!! in the new system are the number of bits in a packed value.
!!!
!!! To pack or unpack an array 64-to-1 or 8-to-1 or 4-to-1 or 2-to-1:
!!!
!!!                             i i     o     i
!!!       CALL PKUTIL_PACK64   (X,N,   PACK,NPACK)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_PACK8    (X,N,   PACK,NPACK)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_PACK4    (X,N,   PACK,NPACK)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_PACK2    (X,N,   PACK,NPACK)      <-- OBSOLETE !!!
!!!
!!!                              i     i      o i
!!!       CALL PKUTIL_UNPACK64 (PACK,NPACK,   X,N)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_UNPACK8  (PACK,NPACK,   X,N)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_UNPACK4  (PACK,NPACK,   X,N)      <-- OBSOLETE !!!
!!!       CALL PKUTIL_UNPACK2  (PACK,NPACK,   X,N)      <-- OBSOLETE !!!
!!!
!!! real    X(N)        = array to be packed or restored (integer for 64-40-1).
!!! integer PACK(NPACK) = array containing the packed values.
!!!
!!! For 64-to-1 packing, PACK(NPACK) must be dimensioned (N+63)/64.
!!! For  8-to-1 packing, PACK(NPACK) must be dimensioned (N+ 7)/ 8 + 1.
!!! For  4-to-1 packing, PACK(NPACK) must be dimensioned (N+ 3)/ 4 + 1.
!!! For  2-to-1 packing, PACK(NPACK) must be dimensioned (N+ 1)/ 2.
!!!
!!! If the entire array PACK(NPACK) is set to zeroes, and then the
!!! unpacking routine is called, all of the unpacked values will be zero.
!!!
!!! In the old CPS system, packing 2:1 (to 32 bits) simply chopped off the
!!! least significant portion of the CRAY 64-bit word.  Since floating point
!!! numbers may be represented differently on other 64-bit machines, this
!!! method might not always work.  Therefore, to be portable in the new system,
!!! packing to 32 bits uses integers in the same manner as packing to 8 or
!!! 16 bits.
!!!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!  1.  This utility consists of a set of vectorized routines for packing
!      floating point arrays into smaller arrays to save central memory.
!      There are no restrictions on the values in the arrays.
!
!  2.  For the routines which pack and unpack to 32 bits and 16 bits and
!      8 bits, a scale factor is saved with the packed values so that the
!      original amplitudes (but with less precision) are recovered when the
!      array is unpacked.  This scale factor is the largest absolute value
!      (LAV) in the array (or a separate value in an optional argument).
!      Therefore, if the array to be packed contains values which differ
!      by several orders of magnitude, the smaller values will be recovered
!      as zero.
!
!  3.  For the routines which pack and unpack to 32 bits, no packing
!      or unpacking actually occurs on 32-bit systems, and the original
!      exact values are recovered when unpacking.
!
!  4.  When packing to 8 bits, values are converted to 8-bit integers
!      as follows:
!              128 + 127*nint(VALUE/LAV)  (between 1 and 255).
!      A packed value of 128 or 0 is interpreted as zero.
!      Any values with absolute value less than about 0.5 * LAV / 127
!      will become zero after packing and then unpacking.
!
!  5.  When packing to 16 bits, values are converted to 16-bit integers
!      as follows:
!              32768 + 32767*nint(VALUE/LAV)  (between 1 and 65535).
!      A packed value of 32768 or 0 is interpreted as zero.
!      Any values with absolute value less than about 0.5 * LAV / 32767
!      will become zero after packing and then unpacking.
!
!  6.  When packing to 32 bits, values are converted to 32-bit integers
!      as follows:
!          2147483648 + 2147483647*nint(VALUE/LAV)  (between 1 and 4294967296).
!      A packed value of 2147483648 or 0 is interpreted as zero.
!      Any values with absolute value less than about 0.5 * LAV / 2147483647
!      will become zero after packing and then unpacking.
!
!  7.  When packing to one bit, values are converted to 1-bit integers
!      as follows:
!              All non-zero values are stored as 1 and retrieved as 1.
!              All   zero   values are stored as 0 and retrieved as 0.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2001-04-30  Stoeckley    Change value of BIAS32 to get rid of compiler
!                               warning message.
!  6. 2000-10-19  Stoeckley    Add required missing documentation section.
!  5. 2000-03-09  Stoeckley    Add optional argument LAV to several routines;
!                               add overloaded functions for integer type
!                               unpacked arrays.
!  4. 1999-12-21  Stoeckley    Converted from old system with a nearly-
!                               complete rewrite.
!  3. 1999-01-18  Goodger      Begin using the fortran90 compiler.
!  2. 1997-12-02  Stoeckley    Added routines to pack/unpack 64-to-1.
!  1. 1996-10-30  Stoeckley    Initial version.
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
! The following Fortran-77 constructs were replaced to comply with the
! Fortran-90 standard.  The old constructs work on the sun fortran compiler
! and the linux absoft compiler, but with warning messages.
!
!   (1) Using hexadecimal constants should be limited to data statements:
!
!         Not standard:  integer,parameter :: MASK = Z'FFFF'
!         Standard:      integer,save      :: MASK
!                        data MASK / Z'FFFF' /
!
!   (2) The functions SHIFTR and SHIFTL should be replaced with ISHFT:
!
!         Not standard:  J = SHIFTL(J,ISHIFT)    J = SHIFTR(J, ISHIFT)
!         Standard:      J = ISHFT (J,ISHIFT)    J = ISHFT (J,-ISHIFT)
!
!   (3) Masks using .AND. and .OR. should be replaced with the functions
!        IAND and IOR:
!
!         Not standard:  J = J .OR. MASK       J = J .AND. MASK
!         Standard:      J = IOR(J,MASK)       J = IAND(J,MASK)
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module pkutil_module
      implicit none
      public

      private :: pkutil_private_npack
      private :: pkutil_private_pack
      private :: pkutil_private_unpack

      character(len=100),public,save :: PKUTIL_IDENT = &
       '$Id: pkutil.f90,v 1.7 2001/04/26 18:46:37 sps prod sps $'

      integer,parameter,private :: BIAS8  = 2**8   / 2
      integer,parameter,private :: BIAS16 = 2**16  / 2
  !!  integer,parameter,private :: BIAS32 = 2**32  / 2
      integer,parameter,private :: BIAS32 = 2**(bit_size(0)/2) / 2

      integer,parameter,private :: lunstop = 6

      integer,save,private :: MASK1
      integer,save,private :: MASK8
      integer,save,private :: MASK16
      integer,save,private :: MASK32

      data MASK1  / Z'1'        /
      data MASK8  / Z'FF'       /
      data MASK16 / Z'FFFF'     /
      data MASK32 / Z'FFFFFFFF' /

          !!!! BIAS8  =           128 = half of           256
          !!!! BIAS16 =        32,768 = half of        65,536
          !!!! BIAS32 = 2,147,483,648 = half of 4,294,967,296

          !!!! On 32-bit machines, BIAS32 might lose precision.
          !!!! But on 32-bit machines, BIAS32 is not used.

  !!!! Some compilers on 32-bit machines generate a compiler
  !!!! warning that the value of BIAS32 causes an integer overflow.
  !!!! Therefore, the value for BIAS32 above was changed from 2**32 / 2
  !!!! to  2**(bit_size(0)/2) / 2 to get rid of the warning.
  !!!! On 64-bit machines, BIAS32 will have the correct value 2**32 / 2.
  !!!! On 32-bit machines, BIAS32 will have the incorrect value 2**16 / 2,
  !!!! which does not matter since BIAS32 is not actually used on 32-bit
  !!!! machines.


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface pkutil_pack
           module procedure pkutil_pack_real
           module procedure pkutil_pack_integer
      end interface

      interface pkutil_unpack
           module procedure pkutil_unpack_real
           module procedure pkutil_unpack_integer
      end interface


      interface pkutil_pack1
           module procedure pkutil_pack1_real
           module procedure pkutil_pack1_integer
      end interface

      interface pkutil_unpack1
           module procedure pkutil_unpack1_real
           module procedure pkutil_unpack1_integer
      end interface


      interface pkutil_pack8
           module procedure pkutil_pack8_real
           module procedure pkutil_pack8_integer
      end interface

      interface pkutil_unpack8
           module procedure pkutil_unpack8_real
           module procedure pkutil_unpack8_integer
      end interface


      interface pkutil_pack16
           module procedure pkutil_pack16_real
           module procedure pkutil_pack16_integer
      end interface

      interface pkutil_unpack16
           module procedure pkutil_unpack16_real
           module procedure pkutil_unpack16_integer
      end interface


      interface pkutil_pack32
           module procedure pkutil_pack32_real
           module procedure pkutil_pack32_integer
      end interface

      interface pkutil_unpack32
           module procedure pkutil_unpack32_real
           module procedure pkutil_unpack32_integer
      end interface


      contains


!!----------------------- packing to NBITS bits ----------------------------!!
!!----------------------- packing to NBITS bits ----------------------------!!
!!----------------------- packing to NBITS bits ----------------------------!!


      function pkutil_npack (nbits,  n) result (npack)
      implicit none
      integer,intent(in)  :: nbits,n              ! arguments
      integer             :: npack                ! result

      select case (nbits)
           case ( 1); npack = pkutil_npack1  (n)
           case ( 8); npack = pkutil_npack8  (n)
           case (16); npack = pkutil_npack16 (n)
           case (32); npack = pkutil_npack32 (n)
           case default
               write (lunstop,*) 'PKUTIL: illegal number of NBITS = ',nbits
               write (lunstop,*) 'PKUTIL: programming error in calling program'
               stop
      end select
      return
      end function pkutil_npack


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK_real (nbits,  X,N,   PACK,npack,  lav)
      implicit none
      integer,intent(in)           :: nbits,N,npack            ! arguments
      real   ,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      real   ,intent(in),optional  :: lav                      ! arguments

      select case (nbits)
           case ( 1); call pkutil_pack1  (x,n,  pack,npack)
           case ( 8); call pkutil_pack8  (x,n,  pack,npack,  lav)
           case (16); call pkutil_pack16 (x,n,  pack,npack,  lav)
           case (32); call pkutil_pack32 (x,n,  pack,npack,  lav)
           case default
               write (lunstop,*) 'PKUTIL: illegal number of NBITS = ',nbits
               write (lunstop,*) 'PKUTIL: programming error in calling program'
               stop
      end select
      end subroutine pkutil_pack_real



      SUBROUTINE PKUTIL_UNPACK_real (nbits,  PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: nbits,npack,n                    ! arguments
      integer,intent(in)  :: PACK(npack)                      ! arguments
      real   ,intent(out) :: X(n)                             ! arguments

      select case (nbits)
           case ( 1); call pkutil_unpack1  (pack,npack,  x,n)
           case ( 8); call pkutil_unpack8  (pack,npack,  x,n)
           case (16); call pkutil_unpack16 (pack,npack,  x,n)
           case (32); call pkutil_unpack32 (pack,npack,  x,n)
           case default
               write (lunstop,*) 'PKUTIL: illegal number of NBITS = ',nbits
               write (lunstop,*) 'PKUTIL: programming error in calling program'
               stop
      end select
      end subroutine pkutil_unpack_real


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK_integer (nbits,  X,N,   PACK,npack,  lav)
      implicit none
      integer,intent(in)           :: nbits,N,npack            ! arguments
      integer,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      integer,intent(in),optional  :: lav                      ! arguments

      select case (nbits)
           case ( 1); call pkutil_pack1  (x,n,  pack,npack)
           case ( 8); call pkutil_pack8  (x,n,  pack,npack,  lav)
           case (16); call pkutil_pack16 (x,n,  pack,npack,  lav)
           case (32); call pkutil_pack32 (x,n,  pack,npack,  lav)
           case default
               write (lunstop,*) 'PKUTIL: illegal number of NBITS = ',nbits
               write (lunstop,*) 'PKUTIL: programming error in calling program'
               stop
      end select
      end subroutine pkutil_pack_integer



      SUBROUTINE PKUTIL_UNPACK_integer (nbits,  PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: nbits,npack,n                    ! arguments
      integer,intent(in)  :: PACK(npack)                      ! arguments
      integer,intent(out) :: X(n)                             ! arguments

      select case (nbits)
           case ( 1); call pkutil_unpack1  (pack,npack,  x,n)
           case ( 8); call pkutil_unpack8  (pack,npack,  x,n)
           case (16); call pkutil_unpack16 (pack,npack,  x,n)
           case (32); call pkutil_unpack32 (pack,npack,  x,n)
           case default
               write (lunstop,*) 'PKUTIL: illegal number of NBITS = ',nbits
               write (lunstop,*) 'PKUTIL: programming error in calling program'
               stop
      end select
      end subroutine pkutil_unpack_integer


!!------------------------ packing to 1 bit ------------------------------!!
!!------------------------ packing to 1 bit ------------------------------!!
!!------------------------ packing to 1 bit ------------------------------!!


      function pkutil_npack1 (n) result (npack)
      implicit none
      integer,intent(in)  :: n                    ! arguments
      integer             :: npack                ! result

      npack = pkutil_private_npack (1,n)
      return
      end function pkutil_npack1


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK1_real (X,N,   PACK,npack)
      implicit none
      integer,intent(in)  :: N,npack                           ! arguments
      real   ,intent(in)  :: X(n)                              ! arguments
      integer,intent(out) :: PACK(npack)                       ! arguments
      integer             :: I,L,J,IPACK,ISHIFT,NPACK2         ! local
      integer             :: nsections                         ! local

      nsections = bit_size(i)

      pack(1:npack) = 0

      DO L = 1,nsections                       ! was 1,64 in old code
          ISHIFT = nsections-L                 ! was 64-L in old code
          NPACK2 = (N+nsections-L)/nsections   ! was (N+64-L)/64 in old code
          DO IPACK = 1,NPACK2                  ! vectorizes
              I = nsections*(IPACK-1) + L      ! was 64*(IPACK-1)+L in old code
              J = X(I)                         ! zero or non-zero
              IF (J /= 0) then
                  J = 1                        ! one
                  J = ISHFT(J,ISHIFT)          ! shift left
                  PACK(IPACK) = IOR(PACK(IPACK),J)
              END IF
          END DO
      END DO
      return
      end subroutine pkutil_pack1_real



      SUBROUTINE PKUTIL_UNPACK1_real (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                          ! arguments
      integer,intent(in)  :: PACK(npack)                      ! arguments
      real   ,intent(out) :: X(n)                             ! arguments
      integer             :: I,L,J,IPACK,nsections            ! local

      nsections = bit_size(i)

      DO I = 1,N                                 ! vectorizes
          IPACK = (I+nsections-1)/nsections      ! was (I+63)/64 in old code
          L = nsections*IPACK - I                ! was 64*IPACK-I in old code
          J = ISHFT(PACK(IPACK),-L)              ! shift right
          J = IAND(J,MASK1)                      ! either 0 or 1
          X(I) = J                               ! either 0 or 1
      END DO
      return
      end subroutine pkutil_unpack1_real


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK1_integer (X,N,   PACK,npack)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      integer,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      real                         :: xxx(n)                   ! local

      xxx(1:n) = x(1:n)
      call pkutil_pack1_real (xxx,N,   pack,npack)
      return
      end subroutine pkutil_pack1_integer



      SUBROUTINE PKUTIL_UNPACK1_integer (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      integer,intent(out) :: X(n)                              ! arguments
      real                :: xxx(n)                            ! local

      call pkutil_unpack1_real (PACK,npack,   xxx,N)
      x(1:n) = nint(xxx(1:n))
      return
      end subroutine pkutil_unpack1_integer


!!------------------------- packing to 8 bits --------------------------!!
!!------------------------- packing to 8 bits --------------------------!!
!!------------------------- packing to 8 bits --------------------------!!


      function pkutil_npack8 (n) result (npack)
      implicit none
      integer,intent(in)  :: n                    ! arguments
      integer             :: npack                ! result

      npack = pkutil_private_npack (8,n)
      return
      end function pkutil_npack8


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK8_real (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      real   ,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      real   ,intent(in),optional  :: lav                      ! arguments

      call pkutil_private_pack (8,BIAS8,   X,N,   pack,npack,   lav)
      return
      end subroutine pkutil_pack8_real



      SUBROUTINE PKUTIL_UNPACK8_real (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      real   ,intent(out) :: X(n)                              ! arguments

      call pkutil_private_unpack (8,BIAS8,MASK8,   PACK,npack,   X,N)
      return
      end subroutine pkutil_unpack8_real


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK8_integer (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      integer,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      integer,intent(in),optional  :: lav                      ! arguments
      real                         :: xxx(n)                   ! local

      xxx(1:n) = x(1:n)
      if (present(lav)) then
           call pkutil_private_pack (8,BIAS8, xxx,N, pack,npack, real(lav))
      else
           call pkutil_private_pack (8,BIAS8, xxx,N, pack,npack)
      end if
      return
      end subroutine pkutil_pack8_integer



      SUBROUTINE PKUTIL_UNPACK8_integer (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      integer,intent(out) :: X(n)                              ! arguments
      real                :: xxx(n)                            ! local

      call pkutil_private_unpack (8,BIAS8,MASK8,   PACK,npack,   xxx,N)
      x(1:n) = nint(xxx(1:n))
      return
      end subroutine pkutil_unpack8_integer


!!------------------------- packing to 16 bits --------------------------!!
!!------------------------- packing to 16 bits --------------------------!!
!!------------------------- packing to 16 bits --------------------------!!


      function pkutil_npack16 (n) result (npack)
      implicit none
      integer,intent(in)  :: n                    ! arguments
      integer             :: npack                ! result

      npack = pkutil_private_npack (16,n)
      return
      end function pkutil_npack16


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK16_real (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      real   ,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      real   ,intent(in),optional  :: lav                      ! arguments

      call pkutil_private_pack (16,BIAS16,   X,N,   pack,npack,   lav)
      return
      end subroutine pkutil_pack16_real



      SUBROUTINE PKUTIL_UNPACK16_real (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      real   ,intent(out) :: X(n)                              ! arguments

      call pkutil_private_unpack (16,BIAS16,MASK16,   PACK,npack,   X,N)
      return
      end subroutine pkutil_unpack16_real


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK16_integer (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      integer,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      integer,intent(in),optional  :: lav                      ! arguments
      real                         :: xxx(n)                   ! local

      xxx(1:n) = x(1:n)
      if (present(lav)) then
           call pkutil_private_pack (16,BIAS16, xxx,N, pack,npack, real(lav))
      else
           call pkutil_private_pack (16,BIAS16, xxx,N, pack,npack)
      end if
      return
      end subroutine pkutil_pack16_integer



      SUBROUTINE PKUTIL_UNPACK16_integer (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      integer,intent(out) :: X(n)                              ! arguments
      real                :: xxx(n)                            ! local

      call pkutil_private_unpack (16,BIAS16,MASK16,   PACK,npack,   xxx,N)
      x(1:n) = nint(xxx(1:n))
      return
      end subroutine pkutil_unpack16_integer


!!------------------------- packing to 32 bits --------------------------!!
!!------------------------- packing to 32 bits --------------------------!!
!!------------------------- packing to 32 bits --------------------------!!


      function pkutil_npack32 (n) result (npack)
      implicit none
      integer,intent(in)  :: n                    ! arguments
      integer             :: npack                ! result

      npack = pkutil_private_npack (32,n)
      return
      end function pkutil_npack32


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK32_real (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      real   ,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      real   ,intent(in),optional  :: lav                      ! arguments

      call pkutil_private_pack (32,BIAS32,   X,N,   pack,npack,   lav)
      return
      end subroutine pkutil_pack32_real



      SUBROUTINE PKUTIL_UNPACK32_real (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      real   ,intent(out) :: X(n)                              ! arguments

      call pkutil_private_unpack (32,BIAS32,MASK32,   PACK,npack,   X,N)
      return
      end subroutine pkutil_unpack32_real


                               !!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_PACK32_integer (X,N,   PACK,npack,   lav)
      implicit none
      integer,intent(in)           :: N,npack                  ! arguments
      integer,intent(in)           :: X(n)                     ! arguments
      integer,intent(out)          :: PACK(npack)              ! arguments
      integer,intent(in),optional  :: lav                      ! arguments
      real                         :: xxx(n)                   ! local

      xxx(1:n) = x(1:n)
      if (present(lav)) then
           call pkutil_private_pack (32,BIAS32, xxx,N, pack,npack, real(lav))
      else
           call pkutil_private_pack (32,BIAS32, xxx,N, pack,npack)
      end if
      return
      end subroutine pkutil_pack32_integer



      SUBROUTINE PKUTIL_UNPACK32_integer (PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: npack,n                           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      integer,intent(out) :: X(n)                              ! arguments
      real                :: xxx(n)                            ! local

      call pkutil_private_unpack (32,BIAS32,MASK32,   PACK,npack,   xxx,N)
      x(1:n) = nint(xxx(1:n))
      return
      end subroutine pkutil_unpack32_integer


!!-------------------------- private routines ------------------------------!!
!!-------------------------- private routines ------------------------------!!
!!-------------------------- private routines ------------------------------!!


!!! nbits      = desired number of bits in a packed value.
!!! nsections  = number of packed values which will fit into an unpacked word.
!!! n          = dimension of real unpacked array.
!!! npack      = dimension of integer packed array.
!!! bit_size() = number of bits in an unpacked value (Fortran-90 intrinsic).


                              !!!!!!!!!!!!!!!!!!!


      function pkutil_private_npack (nbits,n) result (npack)
      implicit none
      integer,intent(in)  :: nbits,n              ! arguments
      integer             :: npack                ! result
      integer             :: nsections            ! local

      nsections = bit_size(n) / nbits
      if (nsections == 1) then
           npack = n
      else if (nbits == 1) then
           npack = (n+nsections-1)/nsections
      else
           npack = (n+nsections-1)/nsections + 1
                                         ! includes room for scale factor.
      end if
      return
      end function pkutil_private_npack


                              !!!!!!!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_private_PACK (nbits,bias,   X,N,   PACK,npack,  LAV)
      implicit none
      integer,intent(in)          :: nbits,bias,n,npack             ! arguments
      real   ,intent(in)          :: X(n)                           ! arguments
      integer,intent(out)         :: PACK(npack)                    ! arguments
      real   ,intent(in),optional :: lav                            ! arguments
      real                        :: XLAV,VALUE,FACTOR              ! local
      integer                     :: ILAV,I,L,J,IPACK,ISHIFT,NPACK2 ! local
      integer                     :: nsections                      ! local
      equivalence (XLAV,ILAV)                                       ! local

      nsections = bit_size(i) / nbits

      if (nsections == 1) then                       ! no packing necessary.
           pack(1:n) = transfer(x(1:n),pack(1:n),n)  ! this might work instead.
  !        do i = 1,n
  !             xlav = x(i)
  !             pack(i) = ilav
  !        end do
           return
      end if

      XLAV = 0.0
      DO I = 1,N
          XLAV = MAX(ABS(X(I)),XLAV)
      END DO                                ! X(I) is between -XLAV and XLAV

      pack(1:npack) = 0

      IF (XLAV == 0.0) return
      if (present(lav)) XLAV = LAV

      PACK(NPACK) = ILAV
      FACTOR      = (bias - 1.0) / XLAV

      DO L = 1,nsections                    
          ISHIFT = (nsections-L)*nbits     
          NPACK2 = (N+nsections-L)/nsections
          DO IPACK = 1,NPACK2                 ! vectorizes
              I = nsections*(IPACK-1) + L  
              VALUE = FACTOR * min(X(I),XLAV) ! between -(bias-1) and +(bias-1)
              J = bias + NINT(VALUE)          ! between 1 and (2*bias-1)
              J = ISHFT(J,ISHIFT)             ! (shift left)
              PACK(IPACK) = IOR(PACK(IPACK),J)
          END DO
      END DO
      return
      end subroutine pkutil_private_pack


                              !!!!!!!!!!!!!!!!!!!


      SUBROUTINE PKUTIL_private_UNPACK (nbits,bias,mask,   PACK,npack,   X,N)
      implicit none
      integer,intent(in)  :: nbits,bias,mask,npack,n           ! arguments
      integer,intent(in)  :: PACK(npack)                       ! arguments
      real   ,intent(out) :: X(n)                              ! arguments
      real                :: XLAV,VALUE,FACTOR                 ! local
      integer             :: LAV,I,L,J,IPACK                   ! local
      integer             :: nsections                         ! local
      equivalence (XLAV,LAV)                                   ! local

      nsections = bit_size(i) / nbits

      if (nsections == 1) then                    ! no unpacking necessary.
           x(1:n) = transfer(pack(1:n),x(1:n),n)  ! this might work instead.
  !        do i = 1,n
  !             lav = pack(i)
  !             x(i) = xlav
  !        end do
           return
      end if

      LAV    = PACK(NPACK)
      FACTOR = XLAV / (bias - 1.0)          

      DO I = 1,N                              ! vectorizes
          IPACK = (I+nsections-1)/nsections
          L = nsections*IPACK - I        
          J = ISHFT(PACK(IPACK),-nbits*L)            ! shift right
          J = IAND(J,mask)                           ! between 1 and (2*bias-1)
          IF (J == 0) J = bias
          VALUE = J - bias                    ! between -(bias-1) and +(bias-1)
          X(I) = VALUE * FACTOR               ! between -XLAV  and XLAV
      END DO
      return
      end subroutine pkutil_private_unpack


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module pkutil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

