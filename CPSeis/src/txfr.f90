!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- txfr.f90 --------------------------------!!
!!------------------------------- txfr.f90 --------------------------------!!
!!------------------------------- txfr.f90 --------------------------------!!

! other files are:  txfr_crou.c
 
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
! Name       : TXFR 
! Category   : memory
! Written    : 2001-08-28   by: Bill Menger
! Revised    : 2006-08-29   by: Bill Menger
! Maturity   : production
! Purpose    : Implementation of transfer function for a limited set of types.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! TXFR performs the transfer function for a limited set of data types, by 
! calling "c" routines to do the data moves instead of using the F90 
! transfer function.  These do not EVER allocate memory. They perform bounds
! checking by running an assert which will abort the program if enough
! memory was not previously allocated for the memory copy.
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
!                 i    o  i(opt)
!  call txfr_c2i(ca,  ia)
!  call txfr_i2c(ia,  ca,lenca)
!  call txfr_s2i(st,  ia)
!  call txfr_i2s(ia,  st,lenst)
!  call txfr_cv2i(cva,ia)
!  call txfr_i2cv(ia,cva,lenca)
!  call txfr_r2i(ia,  ra, lenia)
!  call txfr_i2r(ra,  ia, lenra)
!  call txfr_r2r(ra,  ra, lenra)
!  call txfr_d2d(da,  da, lenra)
!
!  character(len=1),dimension(:) :: ca (character array)
!  character(len=*),dimension(:) :: cva (character array)
!  character(len=*)              :: st (string)
!  integer,dimension(:)          :: ia (integer array)
!  integer                       :: lenca,lenst (length of ca and st)
!  integer                       :: lenia, lenra(length of ia, ra arrays.
!  real                          :: ra (real array)
!
!  Purpose:  Transfer first argument into the second argument, with optional
!            length when transferring to character variables.
!
!  Usage:    Provide an integer array large enough to hold either the ca or
!            st character variables, then call txfr_c2i or txfr_s2i to move
!            the characters, byte-for-byte, into the ia array.
!  
!            Provide a character array ca or string st into which to move 
!            the data, byte-for-byte, from integer array ia.  If the number
!            of characters to transfer is not on an even word boundary, you
!            may use the lenca or lenst variable to determine precisely how
!            many bytes are transferred into the string or character variables.
!            Remaining bytes are nullified, as are any remaining words in the
!            ia array.
!
!            Provide transfer of data from real to integer and back.
!
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
!                             REVISION HISTORY              
!
!     Date        Author      Description
!     ----        ------      -----------
!  4. 2006-08-29  Bill Menger Added real/real and dbl/dbl transfers because of
!                             an error we found in absoft f90 when it did an
!                             operation A(:,:) = B(:,:) where data within A,B
!                             was packed integers but A and B were declared as
!                             reals.
!  3. 2002-02-04  Bill Menger Added Real/Integer transfers.
!  2. 2001-08-29  Bill Menger Added variable 1-D string option.
!  1. 2001-08-28  Bill Menger Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module txfr_module
  implicit none
  private

  public  :: txfr_c2i, txfr_i2c,txfr_r2r, txfr_d2d
  public  :: txfr_s2i, txfr_i2s,txfr_cv2i, txfr_i2cv
  public  :: txfr_r2i, txfr_i2r

  character(len=100),public,save :: TXFR_IDENT = &
  '$Id: txfr.f90,v 1.4 2006/08/30 13:15:16 Menger prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

  interface
    subroutine txfr_ch_to_i4_c(ca,lca,lia,ia)
      character(len=1),intent(in)   :: ca
      integer,intent(in)            :: lca
      integer,intent(in)            :: lia
      integer,intent(out)           :: ia
    end subroutine txfr_ch_to_i4_c
  end interface

  interface
    subroutine txfr_st_to_i4_c(st,lca,lia,ia)
      character(len=*),intent(in)   :: st
      integer,intent(in)            :: lca
      integer,intent(in)            :: lia
      integer,intent(out)           :: ia
    end subroutine txfr_st_to_i4_c
  end interface

  interface
    subroutine txfr_i4_to_ch_c(ia,lia,lca,ca)
      integer,intent(in)            :: ia
      integer,intent(in)            :: lia
      integer,intent(in)            :: lca
      character(len=1),intent(out)  :: ca
    end subroutine txfr_i4_to_ch_c
  end interface

  interface
    subroutine txfr_i4_to_st_c(ia,lia,lca,st)
      integer,intent(in)            :: ia
      integer,intent(in)            :: lia
      integer,intent(in)            :: lca
      character(len=*),intent(out)  :: st
    end subroutine txfr_i4_to_st_c
  end interface

  interface txfr_c2i
    module procedure txfr_ch_to_i4
    module procedure txfr_ch_to_i41d
  end interface

  interface txfr_cv2i
    module procedure txfr_chv_to_i4
    module procedure txfr_chv_to_i41d
  end interface

  interface txfr_i2c
    module procedure txfr_i4_to_ch
    module procedure txfr_i41d_to_ch
  end interface

  interface txfr_i2cv
    module procedure txfr_i4_to_chv
    module procedure txfr_i41d_to_chv
  end interface

  interface txfr_s2i
    module procedure txfr_st_to_i4
    module procedure txfr_st_to_i41d
  end interface

  interface txfr_i2s
    module procedure txfr_i4_to_st
    module procedure txfr_i41d_to_st
  end interface

  interface
    subroutine txfr_r4_to_i4_c(ra,ia,lenia)
      real      ,intent(in)        :: ra
      integer   ,intent(out)       :: ia
      integer   ,intent(in)        :: lenia
    end subroutine txfr_r4_to_i4_c
  end interface

  interface
    subroutine txfr_i4_to_r4_c(ia,ra,lenra)
      integer   ,intent(in)        :: ia
      real      ,intent(out)       :: ra
      integer   ,intent(in)        :: lenra
    end subroutine txfr_i4_to_r4_c
  end interface

  interface
    subroutine txfr_r4_to_r4_c(rai,rao,lenra)
      real      ,intent(in)        :: rai
      real      ,intent(out)       :: rao
      integer   ,intent(in)        :: lenra
    end subroutine txfr_r4_to_r4_c
  end interface

  interface
    subroutine txfr_d8_to_d8_c(dai,dao,lenda)
      double precision,intent(in)  :: dai
      double precision,intent(out) :: dao
      integer   ,intent(in)        :: lenda
    end subroutine txfr_d8_to_d8_c
  end interface

  interface txfr_r2r
    module procedure txfr_r4_to_r4_2d
    module procedure txfr_r4_to_r4_1d
    module procedure txfr_r4_to_r4_scalar
  end interface

  interface txfr_d2d
    module procedure txfr_d8_to_d8_2d
    module procedure txfr_d8_to_d8_1d
    module procedure txfr_d8_to_d8_scalar
  end interface

  interface txfr_r2i
    module procedure txfr_r4_to_i4_1d
    module procedure txfr_r4_to_i4_scalar
  end interface

  interface txfr_i2r
    module procedure txfr_i4_to_r4_1d
    module procedure txfr_i4_to_r4_scalar
  end interface

  contains

  subroutine txfr_ch_to_i4(ca,ia)
    character(len=1),dimension(:),intent(in) :: ca
    integer,intent(out)                      :: ia

    integer                                  :: lca,lia

    lca        = size(ca)
    lia        = 1
    ia         = 0

    call txfr_ch_to_i4_c(ca(1),lca,lia,ia)

  end subroutine txfr_ch_to_i4

  subroutine txfr_ch_to_i41d(ca,ia)
    character(len=1),dimension(:),intent(in) :: ca
    integer,intent(out), dimension(:)        :: ia

    integer                                  :: lca,lia

    lca        = size(ca)
    lia        = size(ia)
    ia(lia)    = 0

    call txfr_ch_to_i4_c(ca(1),lca,lia,ia(1))

  end subroutine txfr_ch_to_i41d


  subroutine txfr_i4_to_ch(ia,ca,nbytes)
    integer,intent(in)                        :: ia
    character(len=1),dimension(:),intent(out) :: ca
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = size(ca)
    lia        = 1

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_ch_c(ia,lia,lca,ca(1))

  end subroutine txfr_i4_to_ch

  subroutine txfr_i41d_to_ch(ia,ca,nbytes)
    integer,intent(in),dimension(:)           :: ia
    character(len=1),dimension(:),intent(out) :: ca
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = size(ca)
    lia        = size(ia)

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_ch_c(ia(1),lia,lca,ca(1))

  end subroutine txfr_i41d_to_ch


!----------------------------------------------------------

  subroutine txfr_chv_to_i4(ca,ia)
    character(len=*),dimension(:),intent(in) :: ca
    integer,intent(out)                      :: ia

    integer                                  :: lca,lia

    lca        = size(ca)*len(ca(1))
    lia        = 1
    ia         = 0

    call txfr_ch_to_i4_c(ca(1)(1:1),lca,lia,ia)

  end subroutine txfr_chv_to_i4

  subroutine txfr_chv_to_i41d(ca,ia)
    character(len=*),dimension(:),intent(in) :: ca
    integer,intent(out), dimension(:)        :: ia

    integer                                  :: lca,lia

    lca        = size(ca)*len(ca(1))
    lia        = size(ia)
    ia(lia)    = 0

    call txfr_ch_to_i4_c(ca(1)(1:1),lca,lia,ia(1))

  end subroutine txfr_chv_to_i41d


  subroutine txfr_i4_to_chv(ia,ca,nbytes)
    integer,intent(in)                        :: ia
    character(len=*),dimension(:),intent(out) :: ca
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = size(ca)*len(ca(1))
    lia        = 1

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_ch_c(ia,lia,lca,ca(1)(1:1))

  end subroutine txfr_i4_to_chv

  subroutine txfr_i41d_to_chv(ia,ca,nbytes)
    integer,intent(in),dimension(:)           :: ia
    character(len=*),dimension(:),intent(out) :: ca
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = size(ca)*len(ca(1))
    lia        = size(ia)

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_ch_c(ia(1),lia,lca,ca(1)(1:1))

  end subroutine txfr_i41d_to_chv


!----------------------------------------------------------


  subroutine txfr_st_to_i4(st,ia)
    character(len=*),intent(in)               :: st
    integer,intent(out)                       :: ia

    integer                                   :: lca,lia

    lca        = len(st)
    lia        = 1
    ia         = 0

    call txfr_st_to_i4_c(st,lca,lia,ia)

  end subroutine txfr_st_to_i4

  subroutine txfr_st_to_i41d(st,ia)
    character(len=*),intent(in)               :: st
    integer,intent(out), dimension(:)         :: ia

    integer                                   :: lca,lia

    lca        = len(st)
    lia        = size(ia)
    ia(lia)    = 0

    call txfr_st_to_i4_c(st,lca,lia,ia(1))

  end subroutine txfr_st_to_i41d

  subroutine txfr_i4_to_st(ia,st,nbytes)
    integer,intent(in)                        :: ia
    character(len=*),intent(out)              :: st
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = len(st)
    lia        = 1

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_st_c(ia,lia,lca,st)

  end subroutine txfr_i4_to_st

  subroutine txfr_i41d_to_st(ia,st,nbytes)
    integer,intent(in),dimension(:)           :: ia
    character(len=*),intent(out)              :: st
    integer,intent(in),optional               :: nbytes

    integer                                   :: lca,lia

    lca        = len(st)
    lia        = size(ia)

    if(present(nbytes) ) then
      lca = min(lca,nbytes)
    endif

    call txfr_i4_to_st_c(ia(1),lia,lca,st)

  end subroutine txfr_i41d_to_st

  subroutine txfr_i4_to_r4_scalar(ia,ra,lenra)
    integer,intent(in )           :: ia
    real,intent(out)              :: ra
    integer,intent(in)            :: lenra
    call txfr_i4_to_r4_c(ia,ra,lenra)
  end subroutine txfr_i4_to_r4_scalar


  subroutine txfr_r4_to_i4_scalar(ra,ia,lenia)
    real,intent(in)               :: ra
    integer,intent(out)           :: ia
    integer,intent(in)            :: lenia
    call txfr_r4_to_i4_c(ra,ia,lenia)
  end subroutine txfr_r4_to_i4_scalar

  subroutine txfr_r4_to_r4_scalar(rai,rao,lenra)
    real,intent(in)               :: rai
    real,intent(out)              :: rao
    integer,intent(in)            :: lenra
    call txfr_r4_to_r4_c(rai,rao,lenra)
  end subroutine txfr_r4_to_r4_scalar

  subroutine txfr_d8_to_d8_scalar(dai,dao,lenda)
    double precision,intent(in)   :: dai
    double precision,intent(out)  :: dao
    integer,intent(in)            :: lenda
    call txfr_d8_to_d8_c(dai,dao,lenda)
  end subroutine txfr_d8_to_d8_scalar

  subroutine txfr_i4_to_r4_1d(ia,ra,lenra)
    integer,intent(in ),dimension(:) :: ia
    real,intent(out)   ,dimension(:) :: ra
    integer,intent(in)               :: lenra
    call txfr_i4_to_r4_c(ia(1),ra(1),lenra)
  end subroutine txfr_i4_to_r4_1d

  subroutine txfr_r4_to_i4_1d(ra,ia,lenia)
    real,intent(in)    ,dimension(:) :: ra
    integer,intent(out),dimension(:) :: ia
    integer,intent(in)               :: lenia
    call txfr_r4_to_i4_c(ra(1),ia(1),lenia)
  end subroutine txfr_r4_to_i4_1d

  subroutine txfr_r4_to_r4_1d(rai,rao,lenra)
    real,intent(in)    ,dimension(:) :: rai
    real,intent(out)   ,dimension(:) :: rao
    integer,intent(in)               :: lenra
    call txfr_r4_to_r4_c(rai(1),rao(1),lenra)
  end subroutine txfr_r4_to_r4_1d

  subroutine txfr_r4_to_r4_2d(rai,rao,lenra)
    real,intent(in)    ,dimension(:,:) :: rai
    real,intent(out)   ,dimension(:,:) :: rao
    integer,intent(in)               :: lenra
    call txfr_r4_to_r4_c(rai(1,1),rao(1,1),lenra)
  end subroutine txfr_r4_to_r4_2d

  subroutine txfr_d8_to_d8_1d(dai,dao,lenda)
    double precision,intent(in) ,dimension(:) :: dai
    double precision,intent(out),dimension(:) :: dao
    integer,intent(in)               :: lenda
    call txfr_d8_to_d8_c(dai(1),dao(1),lenda)
  end subroutine txfr_d8_to_d8_1d

  subroutine txfr_d8_to_d8_2d(dai,dao,lenda)
    double precision,intent(in) ,dimension(:,:) :: dai
    double precision,intent(out),dimension(:,:) :: dao
    integer,intent(in)               :: lenda
    call txfr_d8_to_d8_c(dai(1,1),dao(1,1),lenda)
  end subroutine txfr_d8_to_d8_2d

end module txfr_module
