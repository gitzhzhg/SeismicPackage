!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- fft.f90 --------------------------------!!
!!------------------------------- fft.f90 --------------------------------!!
!!------------------------------- fft.f90 --------------------------------!!

!! +++ PREPROCESSOR DIRECTIVES FOR SYSTEM DEPENDENCIES
!! LIBCHOICE 1 = FFTW library
!! LIBCHOICE 2 = CRAY SCILIB ROUTINES
!! LIBCHOICE 3 = RHS MIXED RADIX ROUTINES ( AS A FALLBACK )
!! to preprocess
!!     gcc -c -E -DLIBCHOICE=1 -x c fft.F90 > fft.f90
!!  or 
!!     gcc -c -E  -x c fft.F90 > fft.f90
#define LIBCHOICE 1
#ifdef CRAY
#undef  LIBCHOICE
#define LIBCHOICE 2
#endif
#ifdef STANDALONE
#undef  LIBCHOICE
#define LIBCHOICE 3
#endif

!<copyright>
!*******************************************************************************
!***********                   COPYRIGHT NOTICE                      ***********
!*********** CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC. ***********
!***********  PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK  ***********
!*******************************************************************************
!</copyright>


!<brief_doc>
!-------------------------------------------------------------------------------
!                     C P S       P R I M I T I V E
!
! Name           : FFT_
! Category   : math
! Written    : 1999-09-07   by: RSDay
! Revised    : 2002-06-21   by: RSDay
! Maturity   : beta
! Purpose    : Fast Fourier Transform routines.
! Portability: Code should be preprocessed prior to compile and linking
!              Relies on  cray scilib or fftw library routines
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                      GENERAL DESCRIPTION
!
! These routines will perform real to complex, complex to real, and
! complex to complex fast fourier transforms. They are meant to be
! portable across platforms. The fft length does not have to be a
! power of 2. Functions exist to support ffts of 1, 2, or 3 dimensional data.
!
! The fft_create call will allocate all necessary work buffers that are
! needed to perform the ffts. The function fft_mem_usage() returns an
! estimate of the memory that will be allocated by the fft_create call.
!
!
!              Mixed radix code of RH Stolt is also included
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                     INPUT AND OUTPUT ARGUMENTS
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
!                       CALLING SEQUENCE
!
! To create and delete an 1-D FFT object:
!                    o         i     i     i      opt       opt
!    i_err =  fft_create (obj, sign, size, ctype, opt_scale,opt_stdo)
!            - OR for F77 access -
!     ui   =  fft_new( sign, size, ctype )
!
!    ui  .... is a unique integer returned by the fft_new call
!    obj .... is a pointer to type(fft_struct)
!    ctype .. is one of the following strings
!            'ctoc' = complex to complex fft
!            'rtoc' = real to complex fft
!            'ctor' = complex to real fft
!    sign ... is a negative or positive integer. This is the sign
!             used in the complex exponential for the fourier
!             transform.
!    size ... can be a mixed radix number (a factor of small primes)
!            size >= 2
!    opt_scale. Optional argument to apply a user defined scaling to
!             the output buffer when the fft is applied.
!    opt_stdo.Set unit number for messages to standard output
!                   i
!    call fft_delete (obj)
!            - OR for F77 access -
!    call fft_del (ui)
!
!
! To perform an fft transformation
!
!                          i    i     o     opt
!    call fft_cc_transform(obj, bufi ,bufo, opt_scale)   complex to complex
!      ( call with first 2 args to do an in place transform )
!                          i    i     o     opt
!    call fft_rc_transform(obj, bufi ,bufo, opt_scale)   real to complex
!      bufo needs n/2+1 complex values
!                          i    i     o     opt
!    call fft_cr_transform(obj, bufi, bufo, opt_scale)   complex to real
!      bufi needs n/2+1 complex values
!            - OR for F77 access -
!    call fft_cct(ui, bufi ,bufo  )   complex to complex
!      ( call with first 2 args to do an in place transform )
!    call fft_rct(ui, bufi ,bufo  )   real to complex
!    call fft_crt(ui, bufi, bufo  )   complex to real
!
! To print out basic information about the fft object
!                i          i
!    call fft_print(obj,lun)
!            - OR for F77 access -
!    call fft_prn(ui,lun)
!
! To normalize the transform
!                       i    b
!    call fft_normalize(obj, bufi  )
!    or use the the optional scaling argument, opt_scale in
!    the trannsform routines, or the create routine
!
!
! To obtain information from/about the fft object
!                   i
!    size  = fft_size (obj      )
!    call fft_type (obj, ctype  )  returns type string
!    words = fft_mem_usage(size,ctype )
!     or words = fft_mem_usage(size,type ) where type is 0,1,2
!
! type(fft_struct),pointer   obj       = pointer to the FFT structure.
!
! To find a reasonable fft length
!     nln = fft_nfctr( n )
!     fft_nfctr =  smallest even product of 2s, 3s, and 5s which
!     contains n
!
!------------------------- MULTI D FFTS ----------------------------------------
! Multi dimensional FFTs may be performed with the following functions.
!
!                          b    i     i     i     i         opt
!    i_err = fft_create_nd(obj, dims, sign, ctype,in_place, opt_stdo)
!        - to create the fft object
!
!    obj  .....is a pointer or object of type(fft_nd_struct)
!    dims(:)...Integer array that specifies the dimensions of the input.
!              rank = size(dims), dimension 1 = dims(1), etc.
!              rank is currently constrained to the range 1 to 3
!    sign......Integer to define the sign of the transform (+ or -).
!    ctype.....string indicating transform type
!              accepted values are 'ctoc','ctor','rtoc'
!    in_place..logical flag for performing in place transforms
!              The dimension of arrays for in place transforms is
!              R(s1,s2) for the real arrays
!              C(s1/2+1,s2) for the complex arrays in the rtoc
!              and ctor transforms, and
!              CC(s1,s2) for the arrays in c to c transforms
!    bufi......Input array of type appropriate to the transform
!              specified to the create function. Some function
!              expect bufi to be passed with no f90 size or shape
!              information, while others(e.g. fft_allpy_nd) do
!              expect f90 size and shape information.
!    bufo......Ouput array of type appropriate to the transform
!              specified to the create function. Some function
!              expect bufo to be passed with no f90 size or shape
!              information, while others(e.g. fft_allpy_nd) do
!              expect f90 size and shape information.
!    opt_stdo..Set unit number for messages to standard output
!    opt_scale.Scale parameter(to normalize set to 1/(s1*s2)).
!
!                      i    i     opt   opt
!    i_err = fft_cc_nd(obj, bufi ,bufo, opt_scale )
!        - to perform a complex to complex transform
!     type(fft_nd_struct),intent(in) :: obj         ! arguments
!     complex,intent(inout)        :: bufi(*)
!     complex,intent(out),optional :: bufo(*)
!
!                         i    i     opt
!    i_err = fft_apply_nd(obj, bufi ,bufo)
!        - to perform a complex to complex transform
!     type(fft_nd_struct),intent(in):: obj         ! arguments
!     complex,        :: bufi(:) or bufi(:,:) or bufi(:,:,:)
!     complex,optional:: bufo(:) or bufo(:,:) or bufo(:,:,:)
!
!                      i    i     opt   opt
!    i_err = fft_cr_nd(obj, bufi, bufo, opt_scale )
!        - to perform a complex to real transform
!     type(fft_nd_struct),intent(in) :: obj         ! arguments
!     complex,intent(inout)          :: bufi(*)
!     real,intent(out),optional      :: bufo(*)
!
!    i_err = fft_rc_nd(obj, bufi ,bufo, opt_scale   )
!        - to perform a real to complex transform
!     type(fft_nd_struct),intent(in) :: obj         ! arguments
!     real   ,intent(inout)        :: bufi(*)
!     complex,intent(out),optional :: bufo(*)
!
!    call fft_delete_nd (obj)
!        - to delete the fft object
!     type(fft_struct),pointer :: obj          ! arguments
!
! To create a 2D FFT object: (SUPERCEDED by nd routines - legacy)
!                          o    i     i     i     i        opt
!    i_err = fft_create_2d(obj, size, sign, ctype,in_place,opt_stdo)
!    obj .... is a pointer to type(fft_2d_struct)
!    size(2).... Integer array that specifies the size s1 x s2
!                where s1=size(1) is the fast storage dimension
!    sign   .... Integer to define the sign of the transform (+ or -).
!                This is the sign used in the complex exponential 
!                for the fourier transform.
!    ctype  .... string indicating transform type(ctoc,ctor,rtoc)
!    in_place... logical flag for performing in place transforms
!                The dimension of arrays for in place transforms is
!                R(s1,s2) for the real arrays
!                C(s1/2+1,s2) for the complex arrays in the rtoc
!                and ctor transforms, and
!                CC(s1,s2) for the arrays in c to c transforms
!    opt_stdo.Set unit number for messages to standard output
!    return value i_err is -1 if there is an error
!
! To delete a 2D FFT object:
!                          b
!     call  fft_delete_2d (obj)
!    obj .... is a pointer to type(fft_2d_struct)
!
! To perform a 2D real to complex transform:
!                       i    b     opt   opt
!     i_err = fft_rc_2d(obj, bufi ,bufo, opt_scale   )
!    obj .... is a type(fft_2d_struct)
!    bufi.... A real array of rank 2. Its size is bufi(s1,s2)
!             for out of place transforms, and bufi(2*(s1/2+1),s2) for
!             in place transforms. The bufi array is not altered during
!             an out of place trransform.
!    bufo ....A complex array of rank two(ignored for in place transforms).
!             The size of bufo is bufo(s1/2+1,s2). You must set up for an
!             in place transform with the fft_create_2d function!! You can
!             not set up for out of place, and then try to do it in place.
!    opt_scale.. optional scale parameter(to normalize set to 1/(s1*s2)).
!    return value i_err is -1 if there is an error
!
! To perform a 2D complex to real transform:
!                         i    b     opt   opt
!      i_err = fft_cr_2d (obj, bufi, bufo, opt_scale )
!    obj .... is a type(fft_2d_struct)
!    bufi.... A complex array of rank 2. Its size is bufi(s1/2+1,s2)
!             NOTE THAT BUFI IS ALTERED WHEN fft_cr_2d RETURNS!
!    bufo ....A real array of rank two(ignored for in place transforms).
!             The size of bufo is bufo(s1,s2). You must set up for an
!             in place transform with the fft_create_2d function!! You can
!             not set up for out of place, and then try to do it in place.
!    opt_scale.. optional scale parameter(to normalize set to 1/(s1*s2)).
!
! To perform a 2D complex to complex transform:
!                       i    b     opt   opt
!     i_err = fft_cc_2d(obj, bufi ,bufo, opt_scale )
!    obj .... is a type(fft_2d_struct)
!    bufi.... A complex array of rank 2. Its size is bufi(s1,s2). The bufi
!             array is not altered during an out of place transform.
!    bufo ....A complex array of rank two(ignored for in place transforms).
!             The size of bufo is bufo(s1,s2). You must set up for an
!             in place transform with the fft_create_2d function!! You can
!             not set up for out of place, and then try to do it in place.
!    opt_scale.. optional scale parameter(to normalize set to 1/(s1*s2)).
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                        REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 12  2002-06-21  RSDay        Added Multi-D fft functions. fft_create_nd,
!                              fft_delete_nd, fft_cc_nd,fft_rc_nd,fft_cr_nd
!                              fft_apply_nd. Use in place of 2D functions
! 11. 2001-08-27  Bill Menger  Fixed bug on line 2314 where "n" was used before
!                              defined. (found by IRIX64 f90 compiler)
! 10. 2001-04-30  RSDay        removed some xml tags
!  9. 2001-03-22  RSDay        Updated doc for fft_rc_2d
!  8. 2000-11-17  RSDay        Added functions for 2D FFTs. Added an
!                              optional argument for stdout unit to the create
!                              calls
!  7. 2000-02-22  RSDay        Added optional last argument opt_scale
!                              to transform routines to allow user scaling
!  6. 2000-01-27  RSDay        Declared fft_nfctr interface public.
!                              Added doc notes for complex fft length.
!  5. 2000-01-17  RSDay        added interface definition for fft_nfctr
!  4. 99-12-02    RSDay        changed doc from fft_obj to obj to be
!                              consistent with name in code.
!  3. 99-10-20    RSDay        altered argument type of fft_mem_usage
!                              and provided interface to hide differences
!  2. 99-09-30    RSDay        replaced get_machine by cgetsys_ostype
!  1. 1999-09-07  RSDay        Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                      PORTABILITY LIMITATIONS
!
! Needs to link to the FFTW routines or the cray SCILIB routines. The FFTW
! package can be obtained from http://www.fftw.org. The preprocessor
! variable LIBCHOICE can be set to select the appropriate fft calls. Without
! the preprocess filter, the code will need to link to both the FFTW and SCILIB
! libraries.
!
!! PREPROCESSOR DIRECTIVES FOR SYSTEM DEPENDENCIES
!! LIBCHOICE 1 = FFTW library (the default)
!! LIBCHOICE 2 = CRAY SCILIB ROUTINES
!! LIBCHOICE 3 = RHS MIXED RADIX ROUTINES ( AS A FALLBACK )
!! to preprocess this file prior to compilation
!!     gcc -c -E -DLIBCHOICE=1 -x c fft.F90 > fft.f90
!!  or 
!!     gcc -c -E  -x c fft.F90 > fft.f90
!
! Also calls the CPS external function cgetsys_ostype()
!
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! The file fftw_f77.inc has been included in this module. It would be
! more portable with respect to fftw upgrades if we used an include statement
! to point to this file.( Normally kept in .../fftw/fftw_f77.inc)
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                        PROGRAMMING NOTES
!
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module fft_module
      implicit none
      private
      public :: fft_create
      public :: fft_delete
      public :: fft_cc_transform
      public :: fft_cc_tr1buf
      public :: fft_cc_tr2buf
      public :: fft_rc_transform
      public :: fft_cr_transform
      public :: fft_normalize
      public :: fft_create_2d
      public :: fft_delete_2d
      public :: fft_cc_2d
      public :: fft_rc_2d
      public :: fft_cr_2d
      public :: fft_create_nd
      public :: fft_delete_nd
      public :: fft_apply_nd
      public :: fft_cc_nd
      public :: fft_rc_nd
      public :: fft_cr_nd
      public :: fft_cnorm
      public :: fft_rnorm
      public :: fft_cc
      public :: fft_rc
      public :: fft_cr
      public :: fft_size
      public :: fft_type !returns internal flag for type
      public :: fft_ctype
      public :: fft_set_title
      public :: fft_get_title
      public :: fft_print
      public :: fft_mem_usage
      public :: fft_mem_use
      public :: fft_mem

! F77 wrappers
      public :: fft_new
      public :: fft_del
      public :: fft_cct
      public :: fft_cct1buf
      public :: fft_cct2buf
      public :: fft_rct
      public :: fft_crt
      public :: fft_prn
      public :: fft_title
      public  :: fft_nfctr

! private methods to support F77 wrappers
      private :: fft_ll_delete
      private :: fft_ll_first
      private :: fft_ll_new_index
      private :: fft_ll_set_first
      private :: fft_ll_find
      private :: fft_ll_add
      private :: fft_ll_rm
      private :: fft_ll_print
      private :: fft_node_new
      private :: fft_node_delete
      private :: fft_node_print
      private :: fft_node_index
      private :: fft_node_data
      private :: fft_node_next
      private :: fft_node_set_next

! Note from R.S. Day:
! The following is the include file fftw_f77.inc
! (See http://www.fftw.org/doc)
!-FFTW_F77_INCLUDE>
      integer,private :: FFTW_FORWARD,FFTW_BACKWARD
      parameter (FFTW_FORWARD=-1,FFTW_BACKWARD=1)

      integer,private :: FFTW_REAL_TO_COMPLEX,FFTW_COMPLEX_TO_REAL
      parameter (FFTW_REAL_TO_COMPLEX=-1,FFTW_COMPLEX_TO_REAL=1)

      integer,private :: FFTW_ESTIMATE,FFTW_MEASURE
      parameter (FFTW_ESTIMATE=0,FFTW_MEASURE=1)

      integer,private :: FFTW_OUT_OF_PLACE,FFTW_IN_PLACE,FFTW_USE_WISDOM
      parameter (FFTW_OUT_OF_PLACE=0)
      parameter (FFTW_IN_PLACE=8,FFTW_USE_WISDOM=16)

      integer,private :: FFTW_THREADSAFE
      parameter (FFTW_THREADSAFE=128)
!-/FFTW_F77_INCLUDE>

  character(len=100),save,public :: fft_ident = &
  "$Id: fft.f90,v 1.11 2001/08/24 16:10:09 Menger prod sps $"



!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: fft_struct
        integer plan(2)
        character title*80
        integer type
        integer size
        integer sign
        integer wsiz
        real    scale
        real,pointer:: work(:)
        integer tsiz
        real,pointer:: table(:)
        integer :: stdo
      end type fft_struct

      type,public :: fft_2d_struct
        integer plan(2)
        character title*80
        integer type
        integer size(2)
        integer sign
        real    scale
        logical in_place
        integer wsiz
        real,pointer :: work(:)
        integer :: stdo
      end type fft_2d_struct

      type,public:: fft_nd_struct
        integer  :: plan(2)
        character:: title*80
        integer  :: type
        integer  :: rank
        integer  :: size(3)
        integer  :: sign
        real     :: scale
        logical  :: in_place
        integer  :: stdo
      end type fft_nd_struct


!! linked list
      type, private :: node
      integer :: ui
      type(fft_struct),pointer :: fft_obj
      type (node), pointer :: next
      end type node

      type, private :: list
      private
      integer        count
      type (node), pointer :: first
      end type list

!! provide static instance of list to hold fft objects
     type(list), private :: static_fft_list
     integer , private :: first_time = 1



!!---------------------------- interfaces ---------------------------------!!

 interface fft_mem_usage
   module procedure  fft_mem_use
   module procedure  fft_mem
 end interface
 interface fft_cct
   module procedure  fft_cct1buf
   module procedure  fft_cct2buf
 end interface
 interface fft_normalize
   module procedure  fft_cnorm
   module procedure  fft_rnorm
 end interface
 interface fft_cc_transform
   module procedure  fft_cc_tr1buf
   module procedure  fft_cc_tr2buf
 end interface
 interface
   integer function fft_nfctr (n)
     integer, intent(in) :: n
   end function fft_nfctr
 end interface
 interface fft_apply_nd
   module procedure  fft_apply_3d
   module procedure  fft_apply_2d
  !module procedure  fft_apply_1d
 end interface


! ! If any interface blocks are needed, they can be placed here.
! ! OTHERWISE OMIT THIS SECTION.


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

! ! If any data is needed, it can be placed here, and made public or private.
! ! OTHERWISE OMIT THIS SECTION (except for the "contains" line).

      contains

!!--------------------------- create ---------------------------------!!

      integer function fft_create (obj, sign, size, ctype,opt_scale,&
       opt_stdo )
      implicit none
! Note by R.S.Day: The include file was placed in the module
!     include 'fftw_f77.inc'
      type(fft_struct),pointer :: obj          ! arguments
      character(len=*),intent(in) ::  ctype
      integer,intent(in)          :: sign,size
      real,intent(in),optional    :: opt_scale
      integer,intent(in),optional :: opt_stdo
      integer  i_err,type
      integer  cray,cgetsys_ostype
      integer  isys,dir
      real     bufi,bufo

      fft_create = -1
      type = -1
      nullify (obj)
      if(size< 1) return
      if(ctype(1:4).eq.'rtoc') type=fft_rc()
      if(ctype(1:4).eq.'RTOC') type=fft_rc()
      if(ctype(1:4).eq.'ctor') type=fft_cr()
      if(ctype(1:4).eq.'CTOR') type=fft_cr()
      if(ctype(1:4).eq.'ctoc') type=fft_cc()
      if(ctype(1:4).eq.'CTOC') type=fft_cc()
      if(type< 0) return
      if(type> 2) return
      allocate (obj,stat=i_err)
      if(i_err /= 0) then
      return
      endif
      obj%stdo = 6
      if(present(opt_stdo)) obj%stdo=opt_stdo
      obj%title= ' '
      obj%size = size
      obj%scale= 1.0
      if(present(opt_scale)) obj%scale=opt_scale
      if(sign<0 ) obj%sign = -1
      if(sign>=0) obj%sign = 1
      obj%type = type
      obj%wsiz = 0
      obj%plan(1) = -1       !initialize to call cray routines
      nullify (obj%work)
      obj%tsiz = 0
      nullify (obj%table)

      cray = 0
      if(cgetsys_ostype() == 1 ) cray=1
      if(cgetsys_ostype() == 8 ) cray=1
      if(cray == 1 ) then
      obj%tsiz = 4*size+100  !for trig tables
      if(obj%type==2) obj%tsiz = 8*size+100  !safe size
      allocate (obj%table(obj%tsiz),stat=i_err)
      if(i_err /= 0) then
        return
      endif
      obj%wsiz = 4*size + 4  !for work arrays
      if(obj%type==2) obj%wsiz = 8*size      !safe size
      allocate (obj%work(obj%wsiz),stat=i_err)
      if(i_err /= 0) then
        return
      endif
      endif

      dir =0
      isys=0
      if(obj%type == 0)       then

#if LIBCHOICE == 1
        call rfftw_f77_create_plan(obj%plan,size, &
     &        fftw_real_to_complex, &
     &        fftw_estimate)
#elif LIBCHOICE == 2
        call scfft (dir, size, obj%scale, bufi, bufo, &
     &        obj%table, obj%work, isys)
#endif

      else if(obj%type == 1) then

#if LIBCHOICE == 1
        call rfftw_f77_create_plan(obj%plan,size, &
     &        fftw_complex_to_real, &
     &        fftw_estimate)
#elif LIBCHOICE == 2
        call csfft (dir, size, obj%scale, bufi, bufo, &
     &        obj%table, obj%work, isys)
#endif

      else if(obj%type == 2) then

#if LIBCHOICE == 1
        call fftw_f77_create_plan(obj%plan,size, &
     &        sign, fftw_estimate)
#elif LIBCHOICE == 2
        call ccfft (dir, size, obj%scale, bufi, bufo, &
     &        obj%table, obj%work, isys)
#endif

      else
      deallocate(obj%work)
      deallocate(obj%table)
      deallocate(obj)
      return
      endif

      fft_create=0
      return
      end function fft_create
!!
!!----------------------------- delete -------------------------------!!

      subroutine fft_delete (obj)
      implicit none
      type(fft_struct),pointer :: obj          ! arguments

      if (.not.associated(obj))       return

#if LIBCHOICE == 1
      if(obj%type <= 1) then
      call rfftw_f77_destroy_plan(obj%plan)
      else
      call  fftw_f77_destroy_plan(obj%plan)
      endif
#endif

      if (associated(obj%work))       deallocate (obj%work)
      if (associated(obj%table)) deallocate (obj%table)
      nullify(obj%work)
      nullify(obj%table)
      obj%tsiz =0
      obj%wsiz =0
      deallocate(obj)
      nullify(obj)

      return
      end subroutine fft_delete
!

!!--------------------------- cc_transform ---------------------------!!

      subroutine fft_cc_tr2buf(obj, bufi ,bufo,opt_scale )
      implicit none
      type(fft_struct):: obj         ! arguments
      complex bufi(obj%size)
      complex bufo(obj%size)
      real,intent(in),optional :: opt_scale
      integer isys ,i,j             ! local variables
      real    scale

      if(obj%type /= 2 ) then
        write(obj%stdo,*) 'fft_cc_transform: error, type=', &
     &      fft_type(obj),' is wrong'
        write(obj%stdo,*) 'fft_cc_transform: length=',fft_size(obj)
        stop
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
#if LIBCHOICE == 2
       isys =0
       call ccfft (obj%sign, obj%size, scale, bufi, bufo, &
    &  obj%table, obj%work, isys)
#elif LIBCHOICE == 1
       call fftw_f77_one(obj%plan,bufi,bufo)
       if(scale /= 1.0) then
         bufo = scale*bufo
       else
         bufo = bufo
       endif
#else
       call fft_cmfft(bufi, bufo, obj%size, obj%sign)
       if(scale /= 1.0) then
         bufo = scale*bufo
       else
         bufo = bufo
       endif
#endif

      return
      end subroutine fft_cc_tr2buf
! !
      subroutine fft_cc_tr1buf(obj, bufi, opt_scale)
      implicit none
      type(fft_struct):: obj         ! arguments
      complex bufi(obj%size)
      real,intent(in),optional :: opt_scale
      complex bufo(obj%size)
      complex bufx(obj%size)
      integer isys ,i,j             ! local variables
      real    scale

      if(obj%type /= 2 ) then
        write(obj%stdo,*) 'fft_cc_transform: error, type=', &
     &      fft_type(obj),' is wrong'
        write(obj%stdo,*) 'fft_cc_transform: length=',fft_size(obj)
      stop
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
#if LIBCHOICE == 2
       isys =0
       call ccfft (obj%sign, obj%size, scale, bufi, bufi, &
       obj%table, obj%work, isys)
#elif LIBCHOICE == 1
       call fftw_f77_one(obj%plan,bufi,bufx)
       if(scale /= 1.0) then
        bufi = scale*bufx
       else
        bufi = bufx
       endif
#else
       call fft_cmfft(bufi, bufx, obj%size, obj%sign)
       if(scale /= 1.0) then
        bufi = scale*bufx
       else
        bufi = bufx
       endif
#endif

      return
      end subroutine fft_cc_tr1buf


      subroutine fft_rc_transform(obj, bufi ,bufo, opt_scale   )
      implicit none
      type(fft_struct):: obj         ! arguments
      real    bufi(obj%size)
      complex bufo(obj%size/2+1)
      real,intent(in),optional :: opt_scale
      complex bufx(obj%size/2+1)   !local variables
      real    scale
      integer isys,i

      if(obj%type /= 0) then         !real to complex
        write(obj%stdo,*) 'fft_rc_transform: error, type=', &
     &      fft_type(obj),' is wrong'
        stop
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
#if LIBCHOICE == 2
      isys =0
      call scfft (obj%sign, obj%size, scale, bufi, bufo, &
     &      obj%table, obj%work, isys)
#elif LIBCHOICE == 1
      call rfftw_f77_one(obj%plan,bufi,bufx)
      call fft_reorder(obj,bufx,bufo)
      if(scale /= 1.0) then
        bufo = scale*bufo
      endif
      if(obj%sign >= 0) then
        do i=1,obj%size/2 + 1
          bufo(i)= conjg(bufo(i))
        enddo
      endif
#else
      call fft_rmfft ( bufi, bufx, obj%size, -1 )
      if(obj%sign >= 0) then
        do i=1,obj%size/2 + 1
          bufo(i)= scale*conjg(bufx(i))
        enddo
      else
        bufo = scale*bufx
!        do i=1,obj%size/2 + 1
!          bufo(i)= bufx(i)
!        enddo
      endif
#endif

      return
      end subroutine fft_rc_transform


!!----------------------------- inverse ------------------------------!!

      subroutine fft_cr_transform (obj, bufi, bufo, opt_scale )
      implicit none
      type(fft_struct):: obj         ! arguments
      complex bufi(obj%size/2+1)
      real    bufo(obj%size)
      real,intent(in),optional :: opt_scale
      complex bufx(obj%size/2+1)   ! local variables
      complex bufy(obj%size/2+1)
      integer isys,i
      real    scale

      if(obj%type /=1 ) then  !complex to real
        write(obj%stdo,*) 'fft_cr_transform: error, type=', &
     &      fft_type(obj),' is wrong'
        stop
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
#if LIBCHOICE == 2
      isys =0
      call csfft (obj%sign, obj%size, scale, bufi, bufo, &
     &      obj%table, obj%work, isys)
#elif LIBCHOICE == 1
      if(obj%sign < 0) then
        do i=1,obj%size/2 + 1
          bufx(i)= conjg(bufi(i))
        enddo
        call fft_reorder(obj,bufx, bufy)
      else
        call fft_reorder(obj,bufi, bufy)
      endif
      call rfftw_f77_one(obj%plan,bufy,bufo)
      if(scale /= 1.0) bufo = scale*bufo
#else
      if(obj%sign < 0) then
        do i=1,obj%size/2 + 1
          bufx(i)= scale*conjg(bufi(i))
        enddo
      else
        bufx=scale*bufi
      endif
      call fft_rmfft ( bufx, bufo, obj%size, 1 )
#endif

      return
      end subroutine fft_cr_transform

!!---------------------------- normalize ----------------------------!!

      subroutine fft_cnorm(obj, bufi)
      implicit none
      type(fft_struct):: obj         ! arguments
      complex bufi(*)
      real anorm
      integer n
      anorm = 1.0/float(obj%size)
      n = obj%size
      if(obj%type==2) n =2*n
      call fft_norm(n,anorm,bufi)
      return
      end subroutine fft_cnorm

      subroutine fft_rnorm(obj, bufi)
      implicit none
      type(fft_struct):: obj         ! arguments
      real  bufi(*)
      real anorm
      integer n
      anorm = 1.0/float(obj%size)
      n = obj%size
      if(obj%type==2) n =2*n
      call fft_norm(n,anorm,bufi)
      return
      end subroutine fft_rnorm

!    type = fft_rc() = 0 for real to complex
!    type = fft_cr() = 1 for complex to real
!    type = fft_cc() = 2 for complex to complex
      integer function fft_rc ()
      implicit none
      fft_rc=0
      return
      end function fft_rc

      integer function fft_cr ()
      implicit none
      fft_cr=1
      return
      end function fft_cr

      integer function fft_cc ()
      implicit none
      fft_cc=2
      return
      end function fft_cc

      integer function fft_mem(size,ctype)
      implicit none
      character(len=*) :: ctype
      integer size,type
      integer cray,cgetsys_ostype
      type = fft_rc(); 
      if(ctype == 'ctoc') then
       type = fft_cc(); 
      endif
      fft_mem = fft_mem_use(size,type)
      return
      end function fft_mem

      integer function fft_mem_use(size,type)
      implicit none
      integer type
      integer size
      integer cray,cgetsys_ostype
      cray = 0
      if(cgetsys_ostype() == 1 ) cray=1
      if(cgetsys_ostype() == 8 ) cray=1
      if(cray == 1) then
      fft_mem_use = 16*size+100  !safe overestimate
      else
      fft_mem_use = 4*size
      endif
      return
      end function fft_mem_use


!!---------------------------- print  -------------------------------!!

      subroutine fft_print(obj,lun)
      implicit none
      integer lun
      type(fft_struct):: obj         ! arguments
      character ctype*8
      write(lun,*) ' +++ FFT PARAMETERS +++'
      write(lun,*) ' FFT TITLE:',obj%title
      write(lun,*) ' FFT SIZE :',obj%size
      write(lun,*) ' FFT SIGN :',obj%sign
      call fft_ctype(obj,ctype)
      write(lun,*) ' FFT TYPE :',ctype
      end subroutine fft_print

!!---------------------------- title  -------------------------------!!

      subroutine fft_set_title(obj,title)
      implicit none
      type(fft_struct):: obj         ! arguments
      character(len=*) title
      obj%title = title
      end subroutine fft_set_title

      subroutine fft_get_title(obj,title)
      implicit none
      type(fft_struct):: obj         ! arguments
      character(len=*) title
      title = obj%title
      end subroutine fft_get_title

!!---------------------------- size   -------------------------------!!

      integer function fft_size (obj )
      implicit none
      type(fft_struct):: obj         ! arguments

      fft_size = -1
      fft_size = obj%size
      end function fft_size

!!---------------------------- type   -------------------------------!!

      integer function fft_type (obj )
      implicit none
      type(fft_struct):: obj         ! arguments

      fft_type = -1
      fft_type = obj%type
      end function fft_type

      subroutine fft_ctype (obj, ctype )
      implicit none
      character(len=*) :: ctype
      type(fft_struct):: obj         ! arguments

      ctype=' '
      if(obj%type == fft_rc()) ctype='rtoc'
      if(obj%type == fft_cr()) ctype='ctor'
      if(obj%type == fft_cc()) ctype='ctoc'

      end subroutine fft_ctype
!!
!! Wrappers to hide type(fft_struct) by providing an integer
!!F77_WRAPPER!
      integer function fft_new( sign, size, ctype )
      character(len=*) :: ctype
      integer          sign,size
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      integer          i_err

      if(first_time==1) then
      static_fft_list%count = 0
      nullify(static_fft_list%first)
      first_time=0
      endif
      fft_new = -1
      i_err =  fft_create (fft_obj, sign, size, ctype )
      if(associated(fft_obj)) then
      call fft_node_new(node_obj)
      node_obj%fft_obj => fft_obj
      call fft_ll_add(static_fft_list, node_obj)
      fft_new = fft_node_index(node_obj)
      endif
      return
      end function fft_new

      subroutine fft_del(ui)
      integer ui
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_ll_rm(static_fft_list,ui )
      call fft_delete(fft_obj)
      return
      end subroutine fft_del

      subroutine fft_cct1buf(ui, bufi)
      integer ui
      complex bufi(*)
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_cc_tr1buf(fft_obj,bufi)
      return
      end subroutine fft_cct1buf

      subroutine fft_cct2buf(ui, bufi ,bufo )
      integer ui
      complex bufi(*),bufo(*)
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_cc_tr2buf(fft_obj,bufi,bufo)
      return
      end subroutine fft_cct2buf

      subroutine fft_rct(ui, bufi ,bufo       )
      integer ui
      real    bufi(*)
      complex bufo(*)
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_rc_transform(fft_obj,bufi,bufo)
      return
      end subroutine fft_rct

      subroutine fft_crt(ui, bufi, bufo       )
      integer ui
      real    bufo(*)
      complex bufi(*)
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_cr_transform(fft_obj,bufi,bufo)
      return
      end subroutine fft_crt

      subroutine fft_prn(ui,lun )
      integer ui,lun
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_print(fft_obj,lun)
      return
      end subroutine fft_prn

      subroutine fft_title(ui ,title)
      integer ui
      character(len=*) title
      type(fft_struct),pointer :: fft_obj
      type(node),pointer :: node_obj
      node_obj => fft_ll_find(static_fft_list,ui)
      fft_obj  => node_obj%fft_obj
      call fft_set_title(fft_obj,title)
      return
      end subroutine fft_title

!!/F77_WRAPPER!
!!
!!NODE_METHODS!
      subroutine fft_node_new(obj)
      type(node),pointer:: obj
      integer val
      allocate(obj)
      obj%ui = -1
      return
      end subroutine fft_node_new

      subroutine fft_node_delete(obj)
      type(node),pointer:: obj
      obj%ui = -1
      if(associated(obj)) then
      deallocate(obj)
      nullify(obj)
      endif
      return
      end subroutine fft_node_delete

      subroutine fft_node_print(obj)
      type(node),pointer:: obj
!     write(obj%stdo,'(" node: index=",i3)')  obj %ui
      if(associated(obj%fft_obj)) then
!      write(obj%stdo,'(" node: fft_obj is associated")')
      else
!      write(obj%stdo,'(" node: fft_obj is not associated")')
      endif
      return
      end subroutine fft_node_print

      integer function fft_node_index(obj)
      type(node),pointer:: obj
      fft_node_index=-1
      if(associated(obj)) fft_node_index= obj%ui
      return
      end function fft_node_index

      function fft_node_data(obj) result(fft_obj)
      type(node) , pointer :: obj
      type(fft_struct),pointer :: fft_obj
      if(.not.associated(obj)) then
       return
      else
       fft_obj => obj%fft_obj
      endif
      return
      end function fft_node_data

      function fft_node_next(obj) result(next)
      type(node) , pointer :: obj,next
      if(.not.associated(obj)) then
       return
      else
       next => obj%next
      endif
      return
      end function fft_node_next

      subroutine fft_node_set_next(obj,next)
      type(node) , pointer :: obj,next
      if(.not.associated(obj)) return
      obj%next => next
      return
      end subroutine fft_node_set_next
!!/NODE_METHODS!
!
!!LIST_METHODS!
      subroutine fft_ll_delete(fft_list)
      type(list):: fft_list
      type(node) , pointer :: cur,last
      cur => fft_ll_first(fft_list)
      do
       if(.not.associated(cur)) then
      fft_list%count=0
      return
       endif
       last => cur
       cur => fft_node_next(cur)
       call fft_node_delete(last)
      enddo
      return
      end subroutine fft_ll_delete

      function      fft_ll_first(fft_list) result(obj)
      type(list):: fft_list
      type(node) , pointer :: obj
      obj => fft_list%first
      end function fft_ll_first

      subroutine fft_ll_set_first(fft_list,obj)
      type(list):: fft_list
      type(node) , pointer :: obj
      if(associated(fft_list%first)) then
      if(associated(obj)) obj%next => fft_list%first%next
      nullify(fft_list%first%next)
      fft_list%first => obj;
      else
      fft_list % first => obj;
      if(associated(obj)) nullify(obj%next)
      fft_list%count = 1
      endif
      return
      end subroutine fft_ll_set_first

      subroutine fft_ll_add(fft_list, obj)
      type(list):: fft_list
      type(node) , pointer :: obj
      type(node) , pointer :: cur
      if(fft_list %count == 0) then
      obj%ui = fft_ll_new_index(fft_list)
      fft_list%first => obj
      nullify(obj % next)
      fft_list%count = 1
      return
      endif
      cur => fft_ll_first(fft_list)
      do
      if(cur%ui == obj%ui) then
!      write(6,'(" fft_ll_add: obj with id=",i3," already in list")') &
!    &       obj%ui
       return
      endif
      if(.not. associated(cur % next) ) then
        obj%ui = fft_ll_new_index(fft_list)
        cur % next  =>  obj
        nullify(obj % next)
        fft_list%count = fft_list%count + 1
        return
      endif
      cur => fft_node_next(cur)
      enddo
      return
      end subroutine fft_ll_add

      subroutine fft_ll_rm(fft_list, val)
      type(list):: fft_list
      integer val
      type(node) , pointer :: cur,prev,next
      if(fft_list %count == 0) return
      nullify (prev)
      cur => fft_ll_first(fft_list)
      do
      if(.not. associated(cur)) return
      if(cur%ui == val) then
        next      => fft_node_next(cur)
!        call fft_node_set_next(prev,next)
        if(associated(prev)) then
          call fft_node_set_next(prev,next)
        else
          call fft_ll_set_first(fft_list,next)
        endif
        fft_list%count = fft_list%count -1
        if(fft_list%count < 0) fft_list%count=0
        call fft_node_delete(cur)
        return
      endif
      prev => cur
      cur  => fft_node_next(cur)
      enddo
      return
      end subroutine fft_ll_rm

      function fft_ll_new_index(fft_list) result(ui)
      integer ui
      type(list):: fft_list
      type(node) , pointer :: obj
      type(node) , pointer :: cur
      integer old,beg,i,match
      cur => fft_ll_first(fft_list)
      if(.not.associated(cur)) then
       ui=1
       return
      endif
      beg = fft_node_index(cur) + 1
      do i =beg,256
      cur => fft_ll_first(fft_list)
      match = 0
      do
        if(.not.associated(cur)) exit
        old = fft_node_index(cur)
        if(i==old) match = 1
        cur => fft_node_next(cur)
      enddo
      if(match==0) then
        ui=i
        return
      endif
      enddo
      ui = -1
      return
      end function fft_ll_new_index

      function fft_ll_find(fft_list, val) result(obj)
      type(list):: fft_list
      integer val
      type(node) , pointer :: obj
      type(node) , pointer :: cur
      cur => fft_ll_first(fft_list)
      if(.not. associated(cur)) then
       return
      endif
      do
      if(.not. associated(cur)) return
      if(cur % ui == val) then
        obj =>  cur
        return
      endif
      cur  => fft_node_next(cur)
      enddo
      return
      end  function fft_ll_find

      subroutine fft_ll_print(fft_list)
      type(list):: fft_list
      type(node), pointer:: cur
      integer i
      if(fft_list%count==0) return
      cur => fft_ll_first(fft_list)
      do i=1,fft_list%count
       call fft_node_print(cur)
       cur  => fft_node_next(cur)
      enddo
      return
      end subroutine fft_ll_print
!
      integer function fft_create_2d(obj, size, sign, ctype,&
       in_place, opt_stdo)
      type(fft_2d_struct),pointer :: obj          ! arguments
      integer,intent(in) :: size(:)
      integer,intent(in) :: sign
      character(len=*),intent(in) :: ctype
      logical,intent(in) :: in_place
      integer,intent(in),optional :: opt_stdo
! Local variables
      integer  :: type,i_err
      integer  flags

      fft_create_2d = -1
      type = -1
      nullify (obj)
      if(size(1)< 1 .or. size(2)<1) return
      if(size(1)*size(2)<4 .or. size(1)*size(2)> 10000000) return 
      if(ctype(1:4).eq.'rtoc') type=fft_rc()
      if(ctype(1:4).eq.'RTOC') type=fft_rc()
      if(ctype(1:4).eq.'ctor') type=fft_cr()
      if(ctype(1:4).eq.'CTOR') type=fft_cr()
      if(ctype(1:4).eq.'ctoc') type=fft_cc()
      if(ctype(1:4).eq.'CTOC') type=fft_cc()
      if(type< 0) return
      if(type> 2) return
      allocate (obj,stat=i_err)
      if(i_err /= 0) then
        return
      endif
      obj%stdo = 6
      if(present(opt_stdo)) obj%stdo=opt_stdo
      obj%title= ' '
      obj%size(1:2) = size(1:2)
      obj%scale= 1.0
      if(sign<0 ) obj%sign = -1
      if(sign>=0) obj%sign = 1
      obj%type = type
      obj%in_place = in_place
      obj%plan(1) = -1

      flags = fftw_estimate
      if(in_place) flags = fftw_estimate+fftw_in_place
      if(obj%type == fft_cc())  then
        call fftw2d_f77_create_plan(obj%plan,size(1),size(2), &
        obj%sign, flags)
      endif
      if(obj%type == fft_rc()) then
        call rfftw2d_f77_create_plan(obj%plan,size(1),size(2), &
        fftw_real_to_complex, flags)
      endif
      if(obj%type == fft_cr()) then
        call rfftw2d_f77_create_plan(obj%plan,size(1),size(2), &
        fftw_complex_to_real, flags)
      endif
      fft_create_2d = 0
      return
      end function fft_create_2d
!
      subroutine fft_delete_2d (obj)
      type(fft_2d_struct),pointer :: obj          ! arguments

      if (.not.associated(obj))       return
      if(obj%type == fft_rc() .or. obj%type == fft_cr() ) then
        call rfftwnd_f77_destroy_plan(obj%plan)
      else
        call  fftwnd_f77_destroy_plan(obj%plan)
      endif
      if (associated(obj%work))       deallocate (obj%work)
      nullify(obj%work)
      obj%wsiz =0
      deallocate(obj)
      nullify(obj)
      return
      end subroutine fft_delete_2d

      integer function fft_cc_2d(obj, bufi ,bufo,opt_scale )
      type(fft_2d_struct),intent(in):: obj         ! arguments
      complex,intent(inout)        :: bufi(*)
      complex,intent(out),optional :: bufo(*)
      real,intent(in),optional     :: opt_scale
      integer i,j             ! local variables
      integer nv
      real    scale

      fft_cc_2d = -1
      if(obj%type /= fft_cc() ) then
       write(obj%stdo,*) 'fft_cc_2d: error, type=', &
     &      obj%type,' is wrong'
       write(obj%stdo,*) 'fft_cc_2d: length=',obj%size(1),obj%size(2)
       return
      endif
      if(.not. present(bufo)) then
        if(.not. obj%in_place) then
          write(obj%stdo,*) 'fft_cc_2d: bufo missing for out of place fft'
          return
        endif
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
      if(obj%in_place) then
        call fftwnd_f77_one(obj%plan,bufi,0)
      else
        call fftwnd_f77_one(obj%plan,bufi,bufo)
      endif
      if(scale /= 1.0) then
        nv = obj%size(1)*obj%size(2)
        if(obj%in_place) then
          bufi(1:nv) = scale*bufi(1:nv)
        else
          bufo(1:nv) = scale*bufo(1:nv)
        endif
      endif
      fft_cc_2d = 0

      return
      end function fft_cc_2d

      integer function fft_rc_2d(obj, bufi ,bufo, opt_scale   )
      type(fft_2d_struct)          :: obj         ! arguments
      real,intent(inout)           :: bufi(*)
      complex,intent(out),optional :: bufo(*)
      real,intent(in),optional     :: opt_scale
      real    scale
      integer i,nv

      fft_rc_2d = -1
      if(obj%type /= fft_rc()) then         !real to complex
        write(obj%stdo,*) 'fft_rc_2d: error, type=', &
     &      obj%type,' is wrong'
        return
      endif
      if(.not. present(bufo)) then
        if(.not. obj%in_place) then
          write(obj%stdo,*) 'fft_rc_2d: bufo missing for out of place fft'
          return
        endif
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
      if(obj%in_place) then
        call rfftwnd_f77_one_real_to_complex(obj%plan,bufi,0)
      else
        call rfftwnd_f77_one_real_to_complex(obj%plan,bufi,bufo)
      endif
      if(scale /= 1.0) then
        nv = (obj%size(1)/2+1) *obj%size(2)
        if(obj%in_place) then
          bufi(1:2*nv) = scale*bufi(1:2*nv)
        else
          bufo(1:nv) = scale*bufo(1:nv)
        endif
      endif
      if(obj%sign >= 0) then
        nv = (obj%size(1)/2+1) *obj%size(2)
        if(obj%in_place) then
          do i=2,2*nv,2
            bufi(i)= -bufi(i)
          enddo
        else
          do i=1,nv
            bufo(i)= conjg(bufo(i))
          enddo
        endif
      endif
      fft_rc_2d = 0
      return
      end function fft_rc_2d

!
! note that bufi is overwritten by rfftwnd_complex_to_real
      integer function fft_cr_2d (obj, bufi, bufo, opt_scale )
      type(fft_2d_struct),intent(in) :: obj         ! arguments
      complex,intent(inout)          :: bufi(*)
      real,intent(out),optional      :: bufo(*)
      real,intent(in),optional       :: opt_scale
      integer i,nv
      real    scale

      fft_cr_2d = -1
      if(obj%type /= fft_cr() ) then  !complex to real
        write(obj%stdo,*) 'fft_cr_2d: error, type=', &
     &      obj%type,' is wrong'
        return
      endif
      if(.not. present(bufo)) then
        if(.not. obj%in_place) then
          write(obj%stdo,*) 'fft_cr_2d: bufo missing for out of place fft'
          return
        endif
      endif

      scale=obj%scale
      if(present(opt_scale)) scale = opt_scale
      if(obj%sign < 0) then  ! conjugate the input?
        nv = (obj%size(1)/2+1) *obj%size(2)
        do i=1,nv
          bufi(i)= conjg(bufi(i))
        enddo
      endif
      if(obj%in_place) then
        call rfftwnd_f77_one_complex_to_real(obj%plan,bufi,0)
      else
        call rfftwnd_f77_one_complex_to_real(obj%plan,bufi,bufo)
      endif
      if(obj%sign < 0 ) then    !unconjugate the input?
        if(.not. obj%in_place) then !only when we need to preserve bufi
          nv = (obj%size(1)/2+1) *obj%size(2)
          do i=1,nv
            bufi(i)= conjg(bufi(i))
          enddo
        endif
      endif
      if(scale /= 1.0) then
        if(obj%in_place) then
          nv = (obj%size(1)/2+1) *obj%size(2)
          bufi(1:nv) = scale*bufi(1:nv)
        else
          nv = obj%size(1)*obj%size(2)
          bufo(1:nv) = scale*bufo(1:nv)
        endif
      endif
      fft_cr_2d = 0
      return
      end function fft_cr_2d

!
       integer function fft_create_nd(obj, dims, sign, ctype,&
        in_place, opt_stdo) result(status)
       type(fft_nd_struct),pointer :: obj          ! arguments
       integer,intent(in)          :: dims(:)
       integer,intent(in)          :: sign
       character(len=*),intent(in) :: ctype
       logical,intent(in)          :: in_place
       integer,intent(in),optional :: opt_stdo
! Local variables
       integer  :: type,i_err
       integer  :: flags,rank
       integer  :: i,nel

       status = -1
       type   = -1
       rank   = size(dims)
       if(rank<1 .or. rank>3) return
       nullify (obj)
       nel = 1
       do i = 1,rank
         nel = nel*dims(i)
       enddo
       if(dims(1)< 1) return
       if(nel<4 .or. nel> 64000000) return 
       if(ctype(1:4).eq.'rtoc') type=fft_rc()
       if(ctype(1:4).eq.'RTOC') type=fft_rc()
       if(ctype(1:4).eq.'ctor') type=fft_cr()
       if(ctype(1:4).eq.'CTOR') type=fft_cr()
       if(ctype(1:4).eq.'ctoc') type=fft_cc()
       if(ctype(1:4).eq.'CTOC') type=fft_cc()
       if(type< 0 .or. type > 2) return
       allocate (obj,stat=i_err)
       if(i_err /= 0) then
         return
       endif
       obj%stdo = 6
       if(present(opt_stdo)) obj%stdo=opt_stdo
       obj%title= ' '
       obj%rank = rank
       obj%size(1:rank) = dims(1:rank)
       obj%scale= 1.0
       if(sign<0 ) obj%sign = -1
       if(sign>=0) obj%sign = 1
       obj%type = type
       obj%in_place = in_place
       obj%plan(1) = -1

       flags = fftw_estimate
       if(in_place) flags = fftw_estimate+fftw_in_place
       if(obj%type == fft_cc())  then
         call fftwnd_f77_create_plan(obj%plan,rank,dims(1:rank), &
         obj%sign, flags)
       endif
       if(obj%type == fft_rc()) then
         call rfftwnd_f77_create_plan(obj%plan,rank,dims(1:rank), &
         fftw_real_to_complex, flags)
       endif
       if(obj%type == fft_cr()) then
         call rfftwnd_f77_create_plan(obj%plan,rank,dims(1:rank), &
         fftw_complex_to_real, flags)
       endif
       status = 0
       return
       end function fft_create_nd
!
       subroutine fft_delete_nd (obj)
       type(fft_nd_struct),pointer :: obj          ! arguments

       if (.not.associated(obj))       return
       if(obj%type == fft_rc() .or. obj%type == fft_cr() ) then
         call rfftwnd_f77_destroy_plan(obj%plan)
       else
         call fftwnd_f77_destroy_plan(obj%plan)
       endif
       deallocate(obj)
       nullify(obj)
       return
       end subroutine fft_delete_nd

       integer function fft_apply_3d(obj, bufi ,bufo) result(status)
       type(fft_nd_struct),intent(in):: obj         ! arguments
       complex,intent(inout)        :: bufi(:,:,:)
       complex,intent(out),optional :: bufo(:,:,:)
       status = -1
       if(obj%rank /=3) return
       if(present(bufo)) then
         status = fft_cc_nd(obj, bufi ,bufo)
       else
         status = fft_cc_nd(obj, bufi)
       endif
       return
       end function fft_apply_3d

       integer function fft_apply_2d(obj, bufi ,bufo) result(status)
       type(fft_nd_struct),intent(in):: obj         ! arguments
       complex,intent(inout)        :: bufi(:,:)
       complex,intent(out),optional :: bufo(:,:)
       status = -1
       if(obj%rank /=2) return
       if(present(bufo)) then
         status = fft_cc_nd(obj, bufi ,bufo)
       else
         status = fft_cc_nd(obj, bufi)
       endif
       return
       end function fft_apply_2d

      !integer function fft_apply_1d(obj, bufi ,bufo) result(status)
      !type(fft_nd_struct),intent(in):: obj         ! arguments
      !complex,intent(inout)        :: bufi(:)
      !complex,intent(out),optional :: bufo(:)
      !status = -1
      !if(obj%rank /=1) return
      !if(present(bufo)) then
      !  status = fft_cc_nd(obj, bufi ,bufo)
      !else
      !  status = fft_cc_nd(obj, bufi)
      !endif
      !return
      !end function fft_apply_1d

       integer function fft_cc_nd(obj, bufi ,bufo,opt_scale ) result(status)
       type(fft_nd_struct),intent(in):: obj         ! arguments
       complex,intent(inout)        :: bufi(*)
       complex,intent(out),optional :: bufo(*)
       real,intent(in),optional     :: opt_scale
       integer i,j             ! local variables
       integer nv
       real    scale

       status = -1
       if(obj%type /= fft_cc() ) then
        write(obj%stdo,*) 'fft_cc_nd: error, type=', &
      &      obj%type,' is wrong'
        write(obj%stdo,*) 'fft_cc_nd: length=',obj%size(1),obj%size(2)
        return
       endif
       if(.not. present(bufo)) then
         if(.not. obj%in_place) then
           write(obj%stdo,*) 'fft_cc_nd: bufo missing for out of place fft'
           return
         endif
       endif

       scale=obj%scale
       if(present(opt_scale)) scale = opt_scale
       if(obj%in_place) then
         call fftwnd_f77_one(obj%plan,bufi,0)
       else
         call fftwnd_f77_one(obj%plan,bufi,bufo)
       endif
       if(scale /= 1.0) then
         nv = 1
         do i = 1,obj%rank
           nv = nv*obj%size(i)
         enddo
         if(obj%in_place) then
           bufi(1:nv) = scale*bufi(1:nv)
         else
           bufo(1:nv) = scale*bufo(1:nv)
         endif
       endif
       status = 0

       return
       end function fft_cc_nd

       integer function fft_rc_nd(obj, bufi ,bufo, opt_scale   ) result(status)
       type(fft_nd_struct)          :: obj         ! arguments
       real,intent(inout)           :: bufi(*)
       complex,intent(out),optional :: bufo(*)
       real,intent(in),optional     :: opt_scale
       real    scale
       integer i,nv

       status = -1
       if(obj%type /= fft_rc()) then         !real to complex
         write(obj%stdo,*) 'fft_rc_nd: error, type=', &
      &      obj%type,' is wrong'
         return
       endif
       if(.not. present(bufo)) then
         if(.not. obj%in_place) then
           write(obj%stdo,*) 'fft_rc_nd: bufo missing for out of place fft'
           return
         endif
       endif

       scale=obj%scale
       if(present(opt_scale)) scale = opt_scale
       if(obj%in_place) then
         call rfftwnd_f77_one_real_to_complex(obj%plan,bufi,0)
       else
         call rfftwnd_f77_one_real_to_complex(obj%plan,bufi,bufo)
       endif
       nv = obj%size(1)/2  + 1
       do i = 2,obj%rank
         nv = nv*obj%size(i)
       enddo
       if(scale /= 1.0) then
         if(obj%in_place) then
           bufi(1:2*nv) = scale*bufi(1:2*nv)
         else
           bufo(1:nv) = scale*bufo(1:nv)
         endif
       endif
       if(obj%sign >= 0) then
         nv = (obj%size(1)/2+1) *obj%size(2)
         if(obj%in_place) then
           do i=2,2*nv,2
             bufi(i)= -bufi(i)
           enddo
         else
           do i=1,nv
             bufo(i)= conjg(bufo(i))
           enddo
         endif
       endif
       status = 0
       return
       end function fft_rc_nd

!
! note that bufi is overwritten by rfftwnd_complex_to_real
       integer function fft_cr_nd (obj, bufi, bufo, opt_scale ) result(status)
       type(fft_nd_struct),intent(in) :: obj         ! arguments
       complex,intent(inout)          :: bufi(*)
       real,intent(out),optional      :: bufo(*)
       real,intent(in),optional       :: opt_scale
       integer i,nv
       real    scale

       status = -1
       if(obj%type /= fft_cr() ) then  !complex to real
         write(obj%stdo,*) 'fft_cr_nd: error, type=', &
      &      obj%type,' is wrong'
         return
       endif
       if(.not. present(bufo)) then
         if(.not. obj%in_place) then
           write(obj%stdo,*) 'fft_cr_nd: bufo missing for out of place fft'
           return
         endif
       endif

       scale=obj%scale
       if(present(opt_scale)) scale = opt_scale
       nv = obj%size(1)/2  + 1
       do i = 2,obj%rank
         nv = nv*obj%size(i)
       enddo
       if(obj%sign < 0) then  ! conjugate the input?
         do i=1,nv
           bufi(i)= conjg(bufi(i))
         enddo
       endif
       if(obj%in_place) then
         call rfftwnd_f77_one_complex_to_real(obj%plan,bufi,0)
       else
         call rfftwnd_f77_one_complex_to_real(obj%plan,bufi,bufo)
       endif
       if(obj%sign < 0 ) then    !unconjugate the input?
         if(.not. obj%in_place) then !only when we need to preserve bufi
           do i=1,nv
             bufi(i)= conjg(bufi(i))
           enddo
         endif
       endif
       if(scale /= 1.0) then
         if(obj%in_place) then
           bufi(1:nv) = scale*bufi(1:nv)
         else
           nv = 1
           do i = 1,obj%rank
             nv = nv*obj%size(i)
           enddo
           bufo(1:nv) = scale*bufo(1:nv)
         endif
       endif
       status = 0
       return
       end function fft_cr_nd

!!/LIST_METHODS!
!!
! Notes on real-complex transforms:
! Depending upon the direction of the plan, either the input or the output
! array is halfcomplex, and is stored in the following format:
!
!      r0, r1, r2, ..., rn/2, i(n+1)/2-1, ..., i2, i1
!
! Here, rk is the real part of the kth output, and ik is the imaginary part.
! (We follow here the C convention that integer division is rounded down, e.g.
!  7 / 2 = 3.) For a halfcomplex array hc[], the kth component has its real
! part in hc[k] and its imaginary part in hc[n-k], with the exception of k == 0
! or n/2 (the latter only if n is even)---in these two cases, the imaginary part
! is zero due to symmetries of the real-complex transform, and is not stored.
! Thus, the transform of n real values is a halfcomplex array of length n, and
! vice versa. (1) This is actually only half of the DFT spectrum of the data.
! Although the other half can be obtained by complex conjugation.
!
! The inverse transform (halfcomplex to real) has the side-effect of
! destroying its input array.
!
! n, is the size of the transform you are trying to compute. The size n can
! be any positive integer, but sizes that are products of small
! factors are transformed most efficiently.
! The second argument, dir, can be either FFTW_FORWARD or FFTW_BACKWARD,
! and indicates the direction of the transform you are interested in.
! Alternatively, you can use the sign of the exponent in the transform,
! -1 or +1, which corresponds to FFTW_FORWARD or FFTW_BACKWARD respectively.
! The flags argument is either FFTW_MEASURE or FFTW_ESTIMATE.
! FFTW_MEASURE means that FFTW actually runs and measures the execution
! time of several FFTs in order to find the best way to compute the
! transform of size n. This may take some time, depending on your
! installation and on the precision of the timer in your machine.
! FFTW_ESTIMATE, on the contrary, does not run any computation, and just
! builds a reasonable plan, which may be sub-optimal. In other words, if
! your program performs many transforms of the same size and initialization
! time is not important, use FFTW_MEASURE; otherwise use the estimate.
! (A compromise between these two extremes exists.
! SeeSection Words of Wisdom.)
!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
      end module fft_module

! mixed radix code from R.H.Stolt converted to fortran 90

!!FFT_RHS!
! ***************************************************************************
      SUBROUTINE FFT_CMFFT(BI, BO, NLN, ISIGN)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
!...Translated by Pacific-Sierra Research 77to90  4.4E      09:07:05   8/10/99
!...Switches: olen=80 INDAL=2 -x6r
!-----------------------------------------------
!   I n t e r f a c e      B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y      A r g u m e n t s
!-----------------------------------------------
      INTEGER  :: NLN
      INTEGER , INTENT(IN) :: ISIGN
      REAL , INTENT(IN) :: BI(*)
      REAL , INTENT(INOUT) :: BO(*)

      INTEGER NFAC, M2, M3, M4, M5, MFAC
      COMMON /NFCTRPA/ NFAC, M2, M3, M4, M5, MFAC( 12 )
      INTEGER  FFT_NFCTR

!-----------------------------------------------
!   L o c a l      V a r i a b l e s
!-----------------------------------------------
      INTEGER :: N, ND, NH, IREV, IFWD, MPC, MPF, JFAC, NA, IPASS, M, NAD, NAN&
      , K, IND0, IND1, IND2, IND3, IND4
      REAL :: SGN, CS3, SN3, CS51, SN51, CS52, SN52, TMPR, TMPI, TR1, TI1, TR2&
      , TI2, TTR, TSR, TR3, TI3, TR4, TI4, TRBP, TIBM
      DOUBLE  PRECISION XPR, XPI, DXPR, DXPI, XPR2, XPI2, XPR3,&
       XPI3, XPR4, XPI4 , DARG, DTMP, TWOPI
!     INTEGER :: VISIT=0
!     SAVE VISIT
!-----------------------------------------------
!***************************** COPYRIGHT NOTICE *****************************
!*              *
!*       CONFIDENTIAL AND PROPRIETARY INFORMATION      *
!*             OF CONOCO INC.           *
!*            PROTECTED BY THE COPYRIGHT LAW         *
!*         AS AN UNPUBLISHED WORK      *
!*              *
!****************************************************************************
!\USER DOC
!-----------------------------------------------------------------------
!
!      EXPLORATION RESEARCH AND SERVICES DIVISION
!             CONOCO, INC.
!
!  Process Name:  FFT_CMFFT, FFT_RMFFT, FFT_NFCTR
!      Author:      R. H. Stolt
!  Last Revised:  2000/01/17
!
!  Purpose:  FFT_CMFFT performs a mixed radix FFT on complex data. Its
!         companion FFT_RMFFT performs a mixed radix FFT on real data.
!         The routines are written in generic FORTRAN so as to be
!         usable on any host computer, including CRAYs, VAXes, PCs,
!         and workstations, without recourse to software packages
!         which may not be available on all machines.      FFT_NFCTR is a
!         supporting function which finds the next multiple of 2s,
!         3s, and 5s larger than a given number.
!
!-----------------------------------------------------------------------
!         INPUT PARAMETERS
!   See Program Documentation
!-----------------------------------------------------------------------
!   These subroutines are re-enterable
!-----------------------------------------------------------------------
!            NOTES
!
!  1. These routines differ from "standard" FFTs in that they do not
!     require data length to be a power of two.       They do require that
!     data length NLN be a product of 2s, 3s, and 5s; i.e., that
!          NLN = 2^M2 * 3^M3 * 5^M5.
!     Not all the exponents need be nonzero; however, for FFT_CMFFT, M2
!     must be nonzero (that is, NLN must be even) and for FFT_RMFFT, M2
!     must be larger than one (that is, NLN must be divisible by four).
!
!  2. If, on entry to FFT_CMFFT or FFT_RMFFT, NLN does not meet the above
!     requirements, it is replaced by the smallest bigger number which
!     does, and the input data extended to the new NLN by appending
!     zeroes.
!
!  3. The smallest even multiple of 2s, 3s, and 5s can be found
!     prior to calling the FFTs by calling the function FFT_NFCTR:
!         NLN = FFT_NFCTR( N ),
!     where N is the minimum required FFT length.  For FFT_RMFFT, the
!     number to find is
!         NLN = 2 * FFT_NFCTR ( (N+1)/2 ).
!     This makes NLN a multiple of 4, which is required because FFT_RMFFT
!     calls FFT_CMFFT with NLN = NLN/2.
!
!  4. Sizes of BI and BO are as follows:
!     _____________________________________
!     |          | CMFFT | RMFFT |      RMFFT |
!     |          |          | ISIGN=1 | ISIGN=-1|
!     |___________________________________|
!     | BI  | 2*NLN |  NLN+2 |       NLN   |
!     |          |          |  |   |
!     | BO  | 2*NLN |      NLN |  NLN+2  |
!     |___________________________________|
!
!     For FFT_RMFFT, the frequency-domain array contains only positive
!     frequencies 0 through NLN/2; FFT_CMFFT contains positive and negative
!     in the order 0,1,...,NLN/2-1,+-NLN/2,-NLN/2+1,...,-1.
!
!  5. Scaling. Both FFT_CMFFT and FFT_RMFFT are scaled so that a forward FFT
!     followed by an inverse FFT multiplies the data by NLN.  That is,
!     to recover the datas original scale factor, multiply each
!     element by 1./FLOAT( NLN ).  I did not provide a routine to do
!     this for you, so you will have to do it yourself.
!
!  6. Why do mixed radix FFTs?
!     a) You can use shorter arrays.  A 1025 length array can be
!      accomodated by a mixed radix length of 1080; the nearest
!      power of two is 2048.
!     b) Its faster.  It turns out that for a given length, the mixed
!      radix FFT, if equivalently optimized, will be a little faster.
!      This is mainly because radix-four FFTs go like gangbusters.
!      Threes and fives are a little slower, but not too bad, so the
!      net result for most numbers is some improvement.
!
!  7. Is there a reason not to?
!      FFT_CMFFT has a lot more code to it than a simple power-of-two
!      FFT, hence hogs more memory.  It also requires separate
!      arrays for input and output data.
!
!  8. Where is the SINE-COSINE table?
!      These routines generate their exponents as they need them.  A
!      SINE-COSINE table would decrease execution times by 5 to 8
!      percent, which was not in my judgment a sufficient incentive
!      to use them.  They are a pain for the user to generate and
!      keep track of, especially when doing multi-dimensional FFTs.
!
!  9. Why didnt I use a single array for input and output?
!      Thats      harder to do for mixed radix transforms.  The bit-
!      reversal step required in a power-of-two FFT becomes a factor-
!      order reversal in the mixed-radix case.      The former is a
!      simple transposition of pairs of data elements; the latter
!      mapping is not a transposition, and I did not see an easy
!      and messless way to perform it in place.       Sorry.
!-----------------------------------------------------------------------
! \END DOC
!\PROG DOC
!-----------------------------------------------------------------------
!        REVISION HISTORY
! 99/08/10     Converted to fortran 90
! 91/07/30     Reduced number of scratch variables in FFT_CMFFT, added zero
!           padding to inverse FFT_RMFFT.
! 91/07/30     Touched up double precision to satisfy CRAY compiler.
! 91/07/31     Removed some inessential integer variables.
!-----------------------------------------------------------------------
!        CALLING SEQUENCE
!
!   SUBROUTINE FFT_CMFFT ( BI, BO, NLN, ISIGN )
!   SUBROUTINE FFT_RMFFT ( BI, BO, NLN, ISIGN )
!   INTEGER FUNCTION FFT_NFCTR ( NLN )
!
!     BI    =  INPUT ARRAY (Complex for FFT_CMFFT and FFT_RMFFT with ISIGN
!           set to 1; real for FFT_RMFFT with ISIGN set to -1)
!     BO    =  OUTPUT ARRAY (Complex for FFT_CMFFT and for FFT_RMFFT if
!           ISIGN = -1; real for FFT_RMFFT if ISIGN = 1)
!     NLN   =  ARRAY LENGTH
!     ISIGN =  SIGN OF EXPONENT IN TRANSFORM.  FOR A FORWARD FFT,
!           ISIGN = - 1.  FOR INVERSE, ISIGN = 1.
!     FFT_NFCTR =  SMALLEST EVEN PRODUCT OF 2S, 3S, AND 5S WHICH
!           CONTAINS NLN
!
!-----------------------------------------------------------------------
!            NOTES
!
!  1. NFCTR hides more parameters in common block /NFCTRPA/.  They are
!      NFAC  -- Number of factors in NFCTR
!      M2, M3, M4, M5 --  Number of 2s, 3s, 4s, and 5s in NFCTR,
!        respectively.       (M4 is always chosen so that M2 < 2.)
!      MFAC(12) -- MFAC(J) is the Jth factor in NFCTR. Thus,
!        each factor is a 5, 4, 3, or 2.  They are ordered so that
!        5s      come before 4s, etc.  Note that MFAC allows for only
!        twelve factors, so the largest FFT these routines can
!        reliably perform without modification is 354,294.
!
!-----------------------------------------------------------------------
!     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
!           IN THIS MODULE
! Subroutines:
!      FFT_RMFFT,      FFT_CMFFT
! Functions:
!      FFT_NFCTR
!
!-----------------------------------------------------------------------
!        LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
!
!     DCOS      DSIN  DASIN
!-----------------------------------------------------------------------
!        MEMORY REQUIREMENTS
! Storage   -  none
! Scratch   -  none
! Parms          -  none
!-----------------------------------------------------------------------
!\END DOC
!

!
!   FFT OF SINGLE COMPLEX TRACE
!   DB       = INPUT DATA ARRAY
!   DA       = OUTPUT DATA ARRAY
!   NLN        = ARRAY LENGTH
!   ISIGN = SENSE OF FFT
!     LENGTH N OF FFT IS PRODUCT OF FACTORS:
!      N = MFAC(1) * MFAC(2) * ... * MFAC(NFAC)
!     THE FACTORS MFAC(J) CAN BE 2S, 3S, 4S, AND 5S, AND MUST INCLUDE
!     AT LEAST ONE TWO OR FOUR.       THE PROGRAM WILL PICK THE SMALLEST SUCH
!     N NOT SMALLER THAN NLN.  IF NLN < N, DATA WILL BE PADDED WITH 0S.
!   ISIGN = SENSE OF FFT.  ALGORITHM IS
!     FFT (K) = SUM OVER J EXP { i ISIGN TWOPI/N J*K }
!     FOR USUAL CONVENTION,
!     FOR FORWARD FFT, ISIGN = - 1;
!     FOR INVERSE FFT, ISIGN = + 1.
!   NOTE THAT AFTER FORWARD AND INVERSE FFT, DATA IS EFFECTIVELY
!   MULTIPLIED BY N.

!   FIND THE SMALLEST POSSIBLE N AND ITS FACTORS.
      N = FFT_NFCTR(NLN)
!     VISIT = VISIT + 1
!     IF(VISIT < 10) THEN
!      WRITE(6,*) 'N=',n,' m2=',m2,' m3=',m3,' m4=',m4
!     ENDIF
!  THE NUMBER NFAC OF FACTORS AND THE SEQUENCE MFAC(J) OF FACTORS ARE
!  RETURNED IN THE COMMON BLOCK /NFCTRPA/

!   FACTOR REVERSAL STEP
      ND = 2*N
      NH = N/2
      IREV = 1

      DO IFWD = 1, ND, 2

!   IFWD IS DECOMPOSABLE AS
! IFWD = 1 + 2*[L(1)+MFAC(1)*[L(2)+MFAC(2)*[L(3)+...MFAC(NFAC-1)*L(NFAC)]..]]]
!   WHERE O <= L(J) < MFAC(J). THE SET OF DIGITS [ L(1), L(2), ..., L(NFAC) ]
!   THUS DESCRIBES THE NUMBER IFWD.
!   IREV IS THE NUMBER FORMED BY REVERSING THE ORDER OF THE DIGITS L:
! IREV = 1 + 2*[L(NFAC)+MFAC(NFAC)*[L(NFAC-1)+...+MFAC(2)*L(1)]..]].
!   THIS IS A GENERALIZATION OF RADIX TWO "BIT REVERSAL".  THE DATA MUST BE
!   MAPPED WITH ITS FACTORS REVERSED IN ORDER FOR THE FFT TO OUTPUT RESULTS
!   IN PROPER ORDER.
      IF (IREV < NLN*2) THEN
        BO(IFWD) = BI(IREV)
        BO(IFWD+1) = BI(IREV+1)
      ELSE
        BO(IFWD) = 0.
        BO(IFWD+1) = 0.
      ENDIF

!   FIND IREV FOR NEXT IFWD.  ADDING 2 TO IFWD BASICALLY MEANS ADDING 1
!   TO L(1) (WHICH ADDS 2*N/MFAC(1) TO IREV) UNLESS L(1) = MFAC(1) - 1,
!   IN WHICH CASE L(1) BECOMES 0 AND L(2) BECOMES L(2) + 1 ( WHICH ADDS
!   - 2*N/MFAC(1) * ( MFAC(1) - 1 - 1/MFAC(2) ) TO IREV ), UNLESS ETC.
      MPC = ND/MFAC(1)
      MPF = ND - MPC

      IF (IREV > MPF) THEN
        JFAC = 2
        IREV = IREV - MPF
        MPF = MPC
        MPC = MPC/MFAC(JFAC)
        MPF = MPF - MPC
        JFAC = JFAC + 1
        DO WHILE(JFAC<=NFAC .AND. IREV>MPF)
          IREV = IREV - MPF
          MPF = MPC
          MPC = MPC/MFAC(JFAC)
          MPF = MPF - MPC
          JFAC = JFAC + 1
        END DO
      ENDIF
      IREV = IREV + MPC
      END DO

!   BEGIN FFT
      NA = 1
!  CALCULATE EXPONENTS FOR RADIX 3 AND 5 FFTS
      TWOPI = DASIN(1.D0)*4.
      SGN = ISIGN
      IF (M3 > 0) THEN
      DARG = TWOPI/3.D0
      CS3 = DCOS(DARG)
      SN3 = ISIGN*DSIN(DARG)
      ENDIF
      IF (M5 > 0) THEN
      DARG = TWOPI/5.D0
      CS51 = DCOS(DARG)
      SN51 = ISIGN*DSIN(DARG)
      DARG = DARG + DARG
      CS52 = DCOS(DARG)
      SN52 = ISIGN*DSIN(DARG)
      ENDIF

!   ONE PASS IS REQUIRED THRU THE DATA FOR EACH FACTOR IN N
      DO IPASS = 1, NFAC
      M = MFAC(IPASS)
      NAD = 2*NA
      NAN = NA*M
      DARG = TWOPI/(NAN*ISIGN)
      DXPR = DCOS(DARG)
      DXPI = DSIN(DARG)
      XPR = 1.D0
      XPI = 0.D0

!   LOOP OVER K VALUES
      DO K = 1, NA

!   IF FACTOR FOR THIS PASS IS A TWO
        SELECT CASE (M)
        CASE (2)
          DO IND0 = K + K - 1, ND, NAD*2

            IND1 = IND0 + NAD
            TMPR = BO(IND1)*XPR - BO(IND1+1)*XPI
            TMPI = BO(IND1)*XPI + BO(IND1+1)*XPR

            BO(IND1) = BO(IND0) - TMPR
            BO(IND0) = BO(IND0) + TMPR

            BO(IND1+1) = BO(IND0+1) - TMPI
            BO(IND0+1) = BO(IND0+1) + TMPI
          END DO

!   IF FACTOR FOR THIS PASS IS A THREE
        CASE (3)
          XPR2 = XPR*XPR - XPI*XPI
          XPI2 = 2.D0*XPR*XPI
          DO IND0 = K + K - 1, ND, NAD*3

            IND1 = IND0 + NAD
            TR1 = BO(IND1)*XPR - BO(IND1+1)*XPI
            TI1 = BO(IND1)*XPI + BO(IND1+1)*XPR

            IND2 = IND1 + NAD
            TR2 = BO(IND2)*XPR2 - BO(IND2+1)*XPI2
            TI2 = BO(IND2)*XPI2 + BO(IND2+1)*XPR2

            TMPR = TR1 + TR2
            TMPI = TI1 - TI2
            TTR = BO(IND0) + TMPR*CS3
            TSR = TMPI*SN3
            BO(IND2) = TTR + TSR
            BO(IND1) = TTR - TSR
            BO(IND0) = BO(IND0) + TMPR

            TMPR = TR1 - TR2
            TMPI = TI1 + TI2
            TTR = BO(IND0+1) + TMPI*CS3
            TSR = TMPR*SN3
            BO(IND2+1) = TTR - TSR
            BO(IND1+1) = TTR + TSR
            BO(IND0+1) = BO(IND0+1) + TMPI
          END DO

!   IF FACTOR FOR THIS PASS IS FOUR
        CASE (4)
          XPR2 = XPR*XPR - XPI*XPI
          XPI2 = 2.D0*XPR*XPI
          XPR3 = XPR2*XPR - XPI2*XPI
          XPI3 = XPR2*XPI + XPI2*XPR

          DO IND0 = K + K - 1, ND, NAD*4

            IND1 = IND0 + NAD
            TR1 = BO(IND1)*XPR - BO(IND1+1)*XPI
            TI1 = BO(IND1)*XPI + BO(IND1+1)*XPR

            IND2 = IND1 + NAD
            TR2 = BO(IND2)*XPR2 - BO(IND2+1)*XPI2
            TI2 = BO(IND2)*XPI2 + BO(IND2+1)*XPR2

            IND3 = IND2 + NAD
            TR3 = BO(IND3)*XPR3 - BO(IND3+1)*XPI3
            TI3 = BO(IND3)*XPI3 + BO(IND3+1)*XPR3

            TTR = BO(IND0) - TR2
            TSR = SGN*(TI1 - TI3)
            BO(IND3) = TTR + TSR
            BO(IND1) = TTR - TSR

            TTR = BO(IND0+1) - TI2
            TSR = SGN*(TR1 - TR3)
            BO(IND3+1) = TTR - TSR
            BO(IND1+1) = TTR + TSR

            TTR = BO(IND0) + TR2
            TSR = TR1 + TR3
            BO(IND2) = TTR - TSR
            BO(IND0) = TTR + TSR

            TTR = BO(IND0+1) + TI2
            TSR = TI1 + TI3
            BO(IND2+1) = TTR - TSR
            BO(IND0+1) = TTR + TSR

          END DO

!   IF FACTOR FOR THIS PASS IS FIVE
        CASE (5)
          XPR2 = XPR*XPR - XPI*XPI
          XPI2 = 2.D0*XPR*XPI
          XPR3 = XPR2*XPR - XPI2*XPI
          XPI3 = XPR2*XPI + XPI2*XPR
          XPR4 = XPR2*XPR2 - XPI2*XPI2
          XPI4 = 2.D0*XPR2*XPI2

          DO IND0 = K + K - 1, ND, NAD*5

            IND1 = IND0 + NAD
            TR1 = BO(IND1)*XPR - BO(IND1+1)*XPI
            TI1 = BO(IND1)*XPI + BO(IND1+1)*XPR

            IND2 = IND1 + NAD
            TR2 = BO(IND2)*XPR2 - BO(IND2+1)*XPI2
            TI2 = BO(IND2)*XPI2 + BO(IND2+1)*XPR2

            IND3 = IND2 + NAD
            TR3 = BO(IND3)*XPR3 - BO(IND3+1)*XPI3
            TI3 = BO(IND3)*XPI3 + BO(IND3+1)*XPR3

            IND4 = IND3 + NAD
            TR4 = BO(IND4)*XPR4 - BO(IND4+1)*XPI4
            TI4 = BO(IND4)*XPI4 + BO(IND4+1)*XPR4

            TMPR = TR1 + TR4
            TMPI = TI1 - TI4
            TRBP = TR2 + TR3
            TIBM = TI2 - TI3

            TTR = BO(IND0) + TMPR*CS51 + TRBP*CS52
            TSR = TMPI*SN51 + TIBM*SN52
            BO(IND4) = TTR + TSR
            BO(IND1) = TTR - TSR

            TTR = BO(IND0) + TMPR*CS52 + TRBP*CS51
            TSR = TMPI*SN52 - TIBM*SN51
            BO(IND3) = TTR + TSR
            BO(IND2) = TTR - TSR

            BO(IND0) = BO(IND0) + TMPR + TRBP

            TMPR = TR1 - TR4
            TMPI = TI1 + TI4
            TRBP = TR2 - TR3
            TIBM = TI2 + TI3

            TTR = BO(IND0+1) + TMPI*CS51 + TIBM*CS52
            TSR = TMPR*SN51 + TRBP*SN52
            BO(IND4+1) = TTR - TSR
            BO(IND1+1) = TTR + TSR

            TTR = BO(IND0+1) + TMPI*CS52 + TIBM*CS51
            TSR = TMPR*SN52 - TRBP*SN51
            BO(IND3+1) = TTR - TSR
            BO(IND2+1) = TTR + TSR

            BO(IND0+1) = BO(IND0+1) + TMPI + TIBM

          END DO

!   ALL POSSIBLE FACTORS PROCESSED
        END SELECT
!   UPDATE EXPONENTIAL
        DTMP = XPR*DXPR - XPI*DXPI
        XPI = XPR*DXPI + XPI*DXPR
        XPR = DTMP

!   NEXT K VALUE
      END DO

!   NEXT PASS
      NA = NAN
      END DO

      NLN = N
      RETURN
      END SUBROUTINE FFT_CMFFT



!
! *********************************************************************
      SUBROUTINE FFT_RMFFT(BI, BO, N, ISIGN)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
!...Translated by Pacific-Sierra Research 77to90  4.4E      09:07:05   8/10/99
!...Switches: olen=80 INDAL=2 -x6r
!-----------------------------------------------
!   I n t e r f a c e      B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y      A r g u m e n t s
!-----------------------------------------------
      INTEGER , INTENT(INOUT) :: N
      INTEGER  :: ISIGN
      REAL  :: BI(*)
      REAL  :: BO(*)
!-----------------------------------------------
!   L o c a l      V a r i a b l e s
!-----------------------------------------------
      INTEGER NFAC, M2, M3, M4, M5, MFAC
      COMMON /NFCTRPA/ NFAC, M2, M3, M4, M5, MFAC( 12 )
      INTEGER  FFT_NFCTR

      INTEGER :: I
      INTEGER :: NH, K, NN
      REAL :: OR, OI, ER, FR
      DOUBLE PRECISION      DXPR, DXPI, XPR, XPI, TWOPI, DARG, DTMP
!-----------------------------------------------
!
!   SUBROUTINE TO DO A REAL FFT OF LENGTH N.
!   IT WORKS BY CONVERTING THE DATA TO A COMPLEX ARRAY OF LENGTH N/2,
!   THEN CALLS FFT_CMFFT.
!
!   BI         =  INPUT ARRAY , LENGTH N  FOR FORWARD FFT, N+2 FOR INVERSE
!   BO         =  OUTPUT ARRAY, LENGTH N + 2 FOR FORWARD FFT, N FOR INVERSE
!   N        =  DESIRED LENGTH. IF NOT FACTORIZABLE BY 2S, 3S, 4S,
!           AND 5S,      WITH AT LEAST ONE FACTOR OF FOUR, N WILL BE
!           REPLACED BY THE SMALLEST SUCH NUMBER CONTAINING N AND
!           THE DATA WILL BE PADDED WITH ZEROS.
!   ISIGN   =  SIGN OF EXPONENT IN FFT.
!           ISIGN = - 1 FOR A FORWARD REAL TO COMPLEX FFT.
!           ISIGN = + 1 FOR AN INVERSE COMPLEX TO REAL FFT.

!  BASIC CONSTANTS
      TWOPI = DASIN(1.D0)*4.D0
      NH = (N + 1)/2


!  FORWARD FFT
      IF (ISIGN == (-1)) THEN
      CALL FFT_CMFFT (BI, BO, NH, ISIGN)
      N = NH*2
      DARG = -TWOPI/N
      BO(N+1) = BO(1) - BO(2)
      BO(N+2) = 0.
      BO(1) = BO(1) + BO(2)
      BO(2) = 0.
      BO(NH+2) = -BO(NH+2)
      DXPR = DCOS(DARG)
      DXPI = DSIN(DARG)
      XPR = DXPR*.5D0
      XPI = DXPI*.5D0
      DO K = 3, NH - 1, 2

        OR = BO(K+1) + BO(N+3-K)
        OI = BO(N+2-K) - BO(K)

        ER = (BO(K)+BO(N+2-K))*0.5
        FR = XPR*OR - XPI*OI
        BO(K) = ER + FR
        BO(N+2-K) = ER - FR

        ER = (BO(K+1)-BO(N+3-K))*0.5
        FR = XPI*OR + XPR*OI
        BO(K+1) = ER + FR
        BO(N+3-K) = (-ER) + FR

        DTMP = XPR*DXPR - XPI*DXPI
        XPI = XPR*DXPI + XPI*DXPR
        XPR = DTMP
      END DO
!  INVERSE COMPLEX TO REAL FFT
      ELSE IF (ISIGN == 1) THEN
      NN = 2*FFT_NFCTR(NH)
      IF (NN > N) THEN
        BI(N+3:NN+2) = 0.
        N = NN
        NH = N/2
      ENDIF
      DARG = TWOPI/N
      ER = BI(1) + BI(N+1)
      BI(2) = BI(1) - BI(N+1)
      BI(1) = ER
      BI(NH+1) = BI(NH+1)*2.
      BI(NH+2) = -BI(NH+2)*2.
      DXPR = DCOS(DARG)
      DXPI = DSIN(DARG)
      XPR = -DXPR
      XPI = -DXPI
      DO K = 3, NH - 1, 2

        OR = BI(K+1) + BI(N+3-K)
        OI = BI(N+2-K) - BI(K)

        ER = BI(K) + BI(N+2-K)
        FR = XPR*OR - XPI*OI
        BI(K) = ER + FR
        BI(N+2-K) = ER - FR

        ER = BI(K+1) - BI(N+3-K)
        FR = XPI*OR + XPR*OI
        BI(K+1) = ER + FR
        BI(N+3-K) = (-ER) + FR

        DTMP = XPR*DXPR - XPI*DXPI
        XPI = XPR*DXPI + XPI*DXPR
        XPR = DTMP
      END DO
      CALL FFT_CMFFT (BI, BO, NH, ISIGN)
      ELSE
      WRITE (*, *) ' ISIGN MUST BE PLUS OR MINUS ONE '
      ENDIF

      RETURN
      END SUBROUTINE FFT_RMFFT

!**********************************************************************
      INTEGER FUNCTION FFT_NFCTR( N)
      INTEGER N
      INTEGER N2,N23,N235,M2R,M3R,M5R,J

      INTEGER NFAC, M2, M3, M4, M5, MFAC
      COMMON /NFCTRPA/ NFAC, M2, M3, M4, M5, MFAC( 12 )
!
!  FUNCTION TO FIND THE SMALLEST EVEN PRODUCT OF 2S, 3S, 4S AND 5S
!  WHICH CONTAINS N.
!
!   CHECK FOR N SMALLER THAN 5
      IF(N.LT.5) THEN
       FFT_NFCTR = 4
       NFAC = 2
       M2 = 2
       M3 = 0
       M4 = 0
       M5 = 0
       MFAC(1) = 2
       MFAC(2) = 2
       RETURN
      END IF
!   STRATEGY:  EXAMINE ALL POSSIBLE COMBINATIONS, PICK THE SMALLEST
!
!  EXAMINE ALL FACTORS OF TWO, BEGINNING WITH 2
      FFT_NFCTR = N*2
      N2 = 1
      M2 = 0
      M4 = 0
  200 N2 = N2 * 2
      M2 = M2 + 1
!  FOR A GIVEN NUMBER OF TWOS, EXAMINE ALL POWERS OF THREE,
!  STARTING WITH 3^0.
       N23 = N2
       M3 = 0
       IF (N23.GE.N) GOTO 600

!  FOR A GIVEN NUMBER OF TWOS AND THREES, EXAMINE ALL POWERS OF FIVE,
!  STARTING WITH 5^0.
  300          N235 = N23
             M5 = 0
  500             N235 = N235 * 5
             M5 = M5 + 1
             IF(N235.LT.N) GOTO 500
          IF(N235.LT.FFT_NFCTR) THEN
             FFT_NFCTR = N235
             M2R = M2
             M3R = M3
             M5R = M5
          END IF
          N23 = N23 * 3
          M3 = M3 + 1
          M5 = 0
       IF (N23.LT.N) GOTO 300
  600 IF (N23.LT.FFT_NFCTR)      THEN
       FFT_NFCTR = N23
       M2R = M2
       M3R = M3
       M5R = M5
      END IF
      IF(N2.LT.N) GOTO 200
      M4 = M2R/2
      M2 = M2R      - M4*2
      M3 = M3R
      M5 = M5R
      NFAC = M2 + M3 + M4 + M5
      DO 680 J = 1, NFAC
      IF ( M5 .GE. J ) THEN
       MFAC(J) = 5
      ELSEIF ( M4 + M5 .GE. J ) THEN
       MFAC(J) = 4
      ELSEIF ( M3 + M4 + M5 .GE. J ) THEN
       MFAC(J) = 3
      ELSEIF ( M2 + M3 + M4 + M5 .GE. J ) THEN
       MFAC(J) = 2
      ELSE
       MFAC(J) = 0
      END IF
  680 CONTINUE
      RETURN
      END FUNCTION FFT_NFCTR
!!/FFT_RHS!


!!--------------------------   reorder      ------------------------------!!

      subroutine fft_reorder(obj, bufi, bufo)
      use fft_module
      implicit none
      type(fft_struct) :: obj          ! arguments
      real     bufi(*),bufo(*)
      integer  i,j                !local variables
      integer  nhalf,np1,np2,nyq
!
!  do nothing for complex to complex transforms
      if(obj%type >= 2) return

      np1  = obj%size+1
      np2  = np1+1
      nhalf= obj%size/2
      nyq  = obj%size/2 + 1
!  go from bufi=normal to bufo=fftw order
!  needed before complex to real transform
      if(obj%type == 1) then  !normal order to FFTW order
      bufo(1)        = bufi(1)
      bufo(nyq) = bufi(np1)
      j=3
      do i=2,nhalf
        bufo(i) = bufi(j)
        bufo(np2-i) = bufi(j+1)
        j = j+2
      enddo
      else if(obj%type == 0) then
!  go from bufi=fftw order to bufo=normal order
!  needed after call for real to complex transform
      ! temp save of the 1st complex point
      bufo(1)        = bufi(1)
      bufo(2)        = 0.0
      ! reorder the n/2+1 st complex value
      bufo(np1) = bufi(nyq)
      bufo(np2) = 0.0
      j=3
      do i=2,nhalf
       bufo(j)   = bufi(i)
       bufo(j+1) = bufi(np2-i)
       j = j+2
      enddo
      endif
      return
      end subroutine fft_reorder

!!------------------------------   copy       -----------------------------!!

      subroutine fft_copy(n,bufi,bufo)
      implicit none
      integer       n
      real       bufi(n),bufo(n)
      integer       i
      bufo = bufi
      return
      end subroutine fft_copy

!!------------------------------   norm       -----------------------------!!

      subroutine fft_norm(n,anorm, bufi)
      implicit none
      real       anorm,bufi(*)
      integer       i,n

      if(n <=0 ) return
      do i = 1,n
      bufi(i) = bufi(i) * anorm
      enddo

      return
      end subroutine fft_norm

!#ifdef CRAY | NOFFTW
!!STUBS-FFTW!
!!---------------------------      fftw stubs ---------------------------!!
!     subroutine rfftw_f77_create_plan(plan,size, &
!    &        fftw_real_to_complex, &
!    &        fftw_estimate)
!     write(6,*) 'fftw stub 0: this should not be called'
!     if(size.ne.0) stop
!     return
!     end
!     subroutine fftw_f77_create_plan(plan,size, &
!    &          fftw_backward, &
!    &          fftw_estimate)
!     write(6,*) 'fftw stub 1: this should not be called'
!     if(size.ne.0) stop
!     return
!     end
!     subroutine rfftw_f77_destroy_plan(plan)
!     write(6,*) 'fftw stub 2: this should not be called'
!     if(size.ne.0) stop
!     return
!     end
!     subroutine fftw_f77_destroy_plan(plan)
!     write(6,*) 'fftw stub 3: this should not be called'
!     if(size.ne.0) stop
!     return
!     end
!     subroutine fftw_f77_one(plan,bufi,bufo)
!     write(6,*) 'fftw stub 4: this should not be called'
!     if(size.ne.0) stop
!     return
!     end
!     subroutine rfftw_f77_one(plan,bufi,bufo)
!     write(6,*) 'fftw stub 5: this should not be called'
!     if(size.ne.0) stop
!     return
!     end

!!/STUBS-FFTW!
!#endif

!#ifndef CRAY
!!STUBS-CRAY!
!     subroutine ccfft (isign, size, scale, bufi, bufo, &
!    &        table, work, isys)
!     implicit none
!     integer isign,size,isys
!     real    scale
!     complex bufi(*),bufo(*)
!     real    table(*),work(*)
!     write(6,*) 'ccfft stub: this should not be called'
!     if(size.ne.0) stop
!     return
!     end subroutine ccfft
!     subroutine scfft (isign, size, scale, bufi, bufo, &
!    &        table, work, isys)
!     implicit none
!     integer isign,size,isys
!     real    scale
!     complex bufo(*)
!     real    bufi(*),table(*),work(*)
!     write(6,*) 'scfft stub: this should not be called'
!     if(size.ne.0) stop
!     return
!     end subroutine scfft
!     subroutine csfft (isign, size, scale, bufi, bufo, &
!    &        table, work, isys)
!     implicit none
!     integer isign,size,isys
!     real    scale
!     complex bufi(*)
!     real    bufo(*),table(*),work(*)
!     write(6,*) 'csfft stub: this should not be called'
!     if(size.ne.0) stop
!     return
!     end subroutine csfft
!!/STUBS-CRAY!
!#endif

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

#ifdef VERIFY
!!VERIFICATION_CODE!
!! The following code is provided for testing the fft routines.
!! A test of the R-C and C-C FFT routines is performed
!! Link to the following libs:
!!   the CPS library, libsrfftw.a and libsfftw.a
!       PROGRAM FFT_TEST
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ,N
!       CHARACTER CARD*80
!       FFT_LEN=1440
!       FFT_LEN=1024
!       LOOPSIZ=120
!       WRITE(6,*) 'ENTER FFT_LEN & LOOPSIZ'
!       READ(5,'(A)') CARD
!       READ(CARD,*) FFT_LEN,LOOPSIZ
!       WRITE(6,*) 'FFT_LEN:',FFT_LEN
!       N = FFT_NFCTR(FFT_LEN)
!       WRITE(6,*) 'FFT_LEN from FFT_NFCTR:',N
!       WRITE(6,*) '2D REAL-COMPLEX TEST'
!       CALL FFT_2D_TEST(FFT_LEN,LOOPSIZ)
!       WRITE(6,*) 'REAL-COMPLEX TEST'
!       CALL FFTUTIL_RC_TEST(FFT_LEN,LOOPSIZ)
!       WRITE(6,*) 'REAL-COMPLEX TEST RHS MixRad'
!       CALL FFTUTIL_RC_TEST2(FFT_LEN,LOOPSIZ)
!       WRITE(6,*) 'REAL-COMPLEX TEST - F77'
!       CALL FFTUTIL_RC_TEST_F77(FFT_LEN,LOOPSIZ)
!       WRITE(6,*) 'COMPLEX-COMPLEX TEST'
!       CALL FFTUTIL_CC_TEST(FFT_LEN,LOOPSIZ)
!       WRITE(6,*) 'COMPLEX-COMPLEX F77 TEST'
!       CALL FFTUTIL_CC_TEST_F77(FFT_LEN,LOOPSIZ)
!       STOP
!       END PROGRAM

!       SUBROUTINE FFT_2D_TEST(FFT_LEN, LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER    :: FFT_LEN,LOOPSIZ
!       INTEGER    :: I,J,SIZE(2),SIGN,I_ERR
!       REAL       :: CPU1,CPU2,EPS
!       COMPLEX   ::  BUFO(20000),cbuf(10000) !cbuf(65,64)
!       REAL      ::  BUFI(128,64), BUFS(128,64),SCALE,OPT_SCALE
!       REAL      ::  IBUFI(130,64)
!       REAL      ::  IBUFS(130,64)
!       REAL    CGETSYS_SECONDS
!       TYPE(FFT_2D_STRUCT),POINTER :: OBJF,OBJI
!       character :: ctype*8
!       integer   n1,n2,k,nv
!       N1 = 128
!       N2 = 64
!       WRITE(6,*) 'FFT_2D_TEST: N1=',N1,' N2=',N2
!       SIZE(1) = N1
!       SIZE(2) = N2
!       BUFI= 0.0
!       DO I=1,N1
!       DO J=1,N2
!         IF(I==J) BUFI(I,J)=1.0
!         IF(I==2*J) BUFI(I,J)=1.0
!         IF(I==3*J) BUFI(I,J)=1.0
!       ENDDO
!       ENDDO
!       CTYPE='rtoc'
!       SIGN = -1
!       I_ERR = fft_create_2d(objf, size, sign, ctype,.false.)
!       CTYPE='ctor'
!       SIGN =  1
!       I_ERR = fft_create_2d(obji, size, sign, ctype,.false.)
!       CPU1 = CGETSYS_SECONDS()
!       DO I = 1,LOOPSIZ
!         I_ERR = FFT_RC_2D(OBJF, BUFI, BUFO) 
!         SCALE = 1.0/(SIZE(1)*SIZE(2))
!         I_ERR = FFT_CR_2D(OBJI, BUFO, BUFS,SCALE) 
!       ENDDO
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_RC_2D:' &
!      &              ,CPU2-CPU1
!       CALL FFT_DELETE_2D(OBJF)
!       CALL FFT_DELETE_2D(OBJI)
!       DO I=1,N1
!       DO J=1,N2
!         if(abs(bufi(i,j)-bufs(i,j)) > 0.001*bufi(i,j) &
!           .and. bufi(i,j)/=0) then
!          write(6,*) 'accuracy problem i=',I,' j=',J
!          write(6,*) 'bufi(i,j)=',bufi(i,j),' bufs(i,j)=',bufs(i,j)
!          stop
!         endif
!       ENDDO
!       ENDDO
!
! repeat as in place
!       IBUFI= 0.0
!       BUFO= (0.0,0.0)
!       DO I=1,N1
!       DO J=1,N2
!         IF(I==J) IBUFI(I,J)=1.0
!       ENDDO
!       ENDDO
!       IBUFS = IBUFI
!       CTYPE='rtoc'
!       SIGN = -1
!       I_ERR = FFT_CREATE_2D(OBJF, SIZE, SIGN, CTYPE,.TRUE.)
!       CTYPE='ctor'
!       SIGN =  1
!       I_ERR = FFT_CREATE_2D(OBJI, SIZE, SIGN, CTYPE,.TRUE.)
!       SCALE = 1.0/(SIZE(1)*SIZE(2))
!       I_ERR = FFT_RC_2D(OBJF, IBUFI) 
!       K=1
!       DO I=1,N1/2 + 1
!       DO J=1,N2
!         cbuf(k) = cmplx(ibufi(2*i-1,j),ibufi(2*i,j))
!         k = K+1
!       ENDDO
!       ENDDO
!          nv = (size(1)/2+1)* size(2)
!          open(unit=22,file='fftdat',access='sequential',form='unformatted')
!          write(22) (cabs(cbuf(k)),k=1,nv)
!          close(22)
!       I_ERR = FFT_CR_2D(OBJI, CBUF, OPT_SCALE=SCALE) 
!       CALL FFT_DELETE_2D(OBJF)
!       CALL FFT_DELETE_2D(OBJI)
!       DO I=1,N1/2 + 1
!       DO J=1,N2
!         ibufi(2*i-1,j) = real(cbuf(i,j))
!         ibufi(2*i,j) = aimag(cbuf(i,j))
!       ENDDO
!       ENDDO
!       DO I=1,N1
!       DO J=1,N2
!         if(abs(ibufi(i,j)-ibufs(i,j)) > 0.001*ibufi(i,j) &
!           .and. ibufs(i,j)/=0) then
!          write(6,*) 'accuracy problem i=',I,' j=',J
!          write(6,*) 'ibufi(i,j)=',ibufi(i,j),' ibufs(i,j)=',ibufs(i,j)
!          stop
!         endif
!       ENDDO
!       ENDDO
!       RETURN
!       END SUBROUTINE

!       SUBROUTINE FFTUTIL_RC_TEST(FFT_LEN,LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ
!       INTEGER      I,OK
!       TYPE(FFT_STRUCT),POINTER :: OBJ
!       INTEGER      TYPE,SIGN,I_ERR,UI,MEM
!       CHARACTER CTYPE*8
!       REAL      CGETSYS_SECONDS
!       REAL      BUFI(2048),CPU1,CPU2,EPS
!       COMPLEX      BUFO(2048)
!       REAL      BUFS(2048),SCALE
!
!       MEM = FFT_MEM_USAGE(FFT_LEN,I)
!       WRITE(6,*) 'FFT_MEM_USAGE=',MEM
!       IF(FFT_LEN.GT.2048) THEN
!        WRITE(6,*) 'FFT_LEN>2048'
!        STOP
!       ENDIF
!       DO I=1,FFT_LEN
!       BUFI(I)=0.0
!       BUFS(I)=BUFI(I)
!       BUFO(I)=0.0
!       ENDDO
!       BUFI(2)=1.0
!       BUFS(2)=BUFI(2)

!       SIGN  = -1
!       CTYPE = 'rtoc'
!       SCALE=1.0/FFT_LEN
!       I_ERR = FFT_CREATE(OBJ, SIGN, FFT_LEN, CTYPE,SCALE )
!       IF(I_ERR /= 0) THEN
!        WRITE(6,*)' CREATE FAILED'
!        STOP
!       ENDIF
!       CPU1 = CGETSYS_SECONDS()
!       DO I =1,LOOPSIZ
!        CALL FFT_RC_TRANSFORM (OBJ, BUFI ,BUFO        )
!       ENDDO
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_RC_FORWARD:' &
!      &              ,CPU2-CPU1
!       CALL FFT_DELETE(OBJ)

!!      WRITE(6,*) (BUFO(I),I=1,FFT_LEN)

!       SIGN  = 1
!       CTYPE = 'ctor'
!       I_ERR = FFT_CREATE(OBJ, SIGN, FFT_LEN, CTYPE )
!       CALL FFT_CR_TRANSFORM(OBJ, BUFO ,BUFI   )
!!      CALL FFT_CR_TRANSFORM(OBJ, BUFO ,BUFI,SCALE)
!!      CALL FFT_NORMALIZE(OBJ,BUFI)
!       CALL FFT_DELETE(OBJ)
!
!       OK=1
!       EPS=.01
!       DO I=1,FFT_LEN
!       IF(ABS(BUFI(I)-BUFS(I)).GT. 0.0001*ABS(BUFS(I)+EPS)) THEN
!         OK=0
!       ENDIF
!       ENDDO
!       IF(OK.EQ.1) THEN
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS < 0.0001'
!       ELSE
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS > 0.0001'
!        WRITE(6,*) 'INPUT DATA'
!        WRITE(6,*) (BUFS(I),I=1,FFT_LEN)
!        WRITE(6,*) 'IROUND TRIP DATA'
!        WRITE(6,*) (BUFI(I),I=1,FFT_LEN)
!       ENDIF
!       RETURN
!       END
!
!       SUBROUTINE FFTUTIL_RC_TEST_F77(FFT_LEN,LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ
!       INTEGER      I,OK
!       TYPE(FFT_STRUCT),POINTER :: OBJ
!       INTEGER      TYPE,SIGN,I_ERR,UI
!       CHARACTER CTYPE*8
!       REAL      CGETSYS_SECONDS
!       REAL      BUFI(2048),CPU1,CPU2,EPS
!       COMPLEX      BUFO(2048)
!       REAL      BUFS(2048)
!
!       IF(FFT_LEN.GT.2048) THEN
!        WRITE(6,*) 'FFT_LEN>2048'
!        STOP
!       ENDIF
!       DO I=1,FFT_LEN
!       BUFI(I)=0.0
!       BUFS(I)=BUFI(I)
!       BUFO(I)=0.0
!       ENDDO
!       BUFI(2)=1.0
!       BUFS(2)=BUFI(2)

!       SIGN  = 1
!       CTYPE = 'rtoc'
!       UI = FFT_NEW( SIGN, FFT_LEN, CTYPE )
!       IF(I_ERR /= 0) THEN
!        WRITE(6,*)' CREATE FAILED'
!        STOP
!       ENDIF
!       CPU1 = CGETSYS_SECONDS()
!       DO I =1,LOOPSIZ
!        CALL FFT_RCT(UI, BUFI ,BUFO   )
!       ENDDO
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_RC_FORWARD:' &
!      &              ,CPU2-CPU1
!       CALL FFT_DEL(UI)

!!    WRITE(6,*) (BUFO(I),I=1,FFT_LEN)

!       SIGN  = -1
!       CTYPE = 'ctor'
!       UI = FFT_NEW(SIGN, FFT_LEN, CTYPE )
!       CALL FFT_CRT(UI, BUFO ,BUFI   )
!       CALL FFT_NORM(FFT_LEN, 1.0/FFT_LEN, BUFI)
!       CALL FFT_DEL(UI)
!
!       OK=1
!       EPS=.01
!       DO I=1,FFT_LEN
!       IF(ABS(BUFI(I)-BUFS(I)).GT. 0.0001*ABS(BUFS(I)+EPS)) THEN
!         OK=0
!       ENDIF
!       ENDDO
!       IF(OK.EQ.1) THEN
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS < 0.0001'
!       ELSE
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS > 0.0001'
!        WRITE(6,*) 'INPUT DATA'
!        WRITE(6,*) (BUFS(I),I=1,FFT_LEN)
!        WRITE(6,*) 'IROUND TRIP DATA'
!        WRITE(6,*) (BUFI(I),I=1,FFT_LEN)
!       ENDIF
!       RETURN
!       END
!ccc
!       SUBROUTINE FFTUTIL_RC_TEST2(FFT_LEN,LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ
!       INTEGER      I,OK
!       TYPE(FFT_STRUCT),POINTER :: OBJ
!       INTEGER      TYPE,SIGN,I_ERR
!       CHARACTER CTYPE*8
!       REAL      CGETSYS_SECONDS
!       REAL      BUFI(2048),CPU1,CPU2,EPS
!       COMPLEX      BUFO(2048)
!       REAL      RBUFO(2048)
!       REAL      BUFS(2048)
!       equivalence (BUFO,RBUFO)
!
!       IF(FFT_LEN.GT.2048) THEN
!        WRITE(6,*) 'FFT_LEN>2048'
!        STOP
!       ENDIF
!       DO I=1,FFT_LEN
!       BUFI(I)=0.0
!       BUFS(I)=BUFI(I)
!       BUFO(I)=0.0
!       ENDDO
!       BUFI(2)=1.0
!       BUFS(2)=BUFI(2)

!       SIGN  = -1
!       CPU1 = CGETSYS_SECONDS()
!       DO I =1,LOOPSIZ
!        CALL FFT_RMFFT ( BUFI, RBUFO, FFT_LEN, SIGN )
!       ENDDO
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_RMFFT:' &
!      &              ,CPU2-CPU1

!!      WRITE(6,*) (BUFO(I),I=1,FFT_LEN)

!       SIGN  = 1
!       TYPE  = FFT_CR() ! c to r
!       CTYPE = 'ctor'
!       I_ERR = FFT_CREATE(OBJ, SIGN, FFT_LEN, CTYPE )
!       CALL FFT_CR_TRANSFORM(OBJ, BUFO ,BUFI   )
!       CALL FFT_NORMALIZE(OBJ, BUFI)
!       CALL FFT_DELETE(OBJ)
!
!       OK=1
!       EPS=.01
!       DO I=1,FFT_LEN
!       IF(ABS(BUFI(I)-BUFS(I)).GT. 0.0001*ABS(BUFS(I)+EPS)) THEN
!         OK=0
!       ENDIF
!       ENDDO
!       IF(OK.EQ.1) THEN
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS < 0.0001'
!       ELSE
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS > 0.0001'
!        WRITE(6,*) 'INPUT DATA'
!        WRITE(6,*) (BUFS(I),I=1,FFT_LEN)
!        WRITE(6,*) 'IROUND TRIP DATA'
!        WRITE(6,*) (BUFI(I),I=1,FFT_LEN)
!       ENDIF
!       RETURN
!       END
!ccc
!       SUBROUTINE FFTUTIL_CC_TEST(FFT_LEN,LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ
!       INTEGER      I,SIGN,OK
!       INTEGER      TYPE,I_ERR,UI
!       CHARACTER CTYPE*8
!       REAL      CGETSYS_SECONDS
!       REAL      CPU1,CPU2,EPS,SCALE
!       TYPE(FFT_STRUCT),POINTER :: OBJ
!       COMPLEX      BUFI(2048),BUFO(2048),BUFS(2048),BUF1(2048)
!       IF(FFT_LEN.GT.2048) THEN
!        WRITE(6,*) 'FFT_LEN>2048'
!        STOP
!       ENDIF
!       DO I=1,FFT_LEN
!       BUFI(I)=0.0
!       BUFS(I)=BUFI(I)
!       BUFO(I)=0.0
!       ENDDO
!       BUFI(2)=1.0
!       BUFS(2)=1.0

!       TYPE = FFT_CC() ! c to c
!       CTYPE = 'ctoc'
!       SIGN=1
!       I_ERR =FFT_CREATE(OBJ, SIGN, FFT_LEN, CTYPE )
!       IF(I_ERR /= 0) THEN
!        WRITE(6,*)' CREATE FAILED'
!        STOP
!       ENDIF
!       SCALE = 1.0/FFT_LEN
!       CPU1 = CGETSYS_SECONDS()
!       DO I=1,LOOPSIZ
!        BUFI=BUFS
!!       CALL FFT_CC_TRANSFORM(OBJ,BUFI,BUFO)
!!       CALL FFT_CC_TRANSFORM(OBJ,BUFI,SCALE)
!        CALL FFT_CC_TRANSFORM(OBJ,BUFI)
!       ENDDO
!       CALL FFT_DELETE(OBJ)
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_CC_TRANSFORM:' &
!      &              ,CPU2-CPU1

!!      WRITE(6,*) 'BUFO FROM FFT_CC_TRANSFORM', FFT_LEN
!!      WRITE(6,*) (BUFO(I),I=1,FFT_LEN)

!       SIGN=-1
!       I_ERR =FFT_CREATE(OBJ, SIGN, FFT_LEN, CTYPE )
!!      CALL FFT_CC_TRANSFORM(OBJ,BUFO,BUFI)
!       CALL FFT_CC_TRANSFORM(OBJ,BUFI)
!       CALL FFT_NORMALIZE(OBJ,BUFI)
!       CALL FFT_DELETE(OBJ)

!       OK=1
!       EPS=.01
!       DO I=1,FFT_LEN
!       IF(CABS(BUFI(I)-BUFS(I)).GT. 0.0001*CABS(BUFS(I)+EPS)) THEN
!         OK=0
!       ENDIF
!       ENDDO
!       IF(OK.EQ.1) THEN
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS < 0.0001'
!       ELSE
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS > 0.0001'
!       ENDIF
!       RETURN
!       END

!       SUBROUTINE FFTUTIL_CC_TEST_F77(FFT_LEN,LOOPSIZ)
!       USE FFT_MODULE
!       IMPLICIT NONE
!       INTEGER      FFT_LEN,LOOPSIZ
!       INTEGER      I,SIGN,OK
!       INTEGER      TYPE,I_ERR,UI
!       CHARACTER CTYPE*8
!       REAL      CGETSYS_SECONDS
!       REAL      CPU1,CPU2,EPS
!       TYPE(FFT_STRUCT),POINTER :: OBJ
!       COMPLEX      BUFI(2048),BUFO(2048),BUFS(2048),BUF1(2048)
!       IF(FFT_LEN.GT.2048) THEN
!        WRITE(6,*) 'FFT_LEN>2048'
!        STOP
!       ENDIF
!       DO I=1,FFT_LEN
!       BUFI(I)=0.0
!       BUFS(I)=BUFI(I)
!       BUFO(I)=0.0
!       ENDDO
!       BUFI(2)=1.0
!       BUFS(2)=1.0

!       TYPE = FFT_CC() ! c to c
!       CTYPE = 'ctoc'
!       SIGN=1
!       UI = FFT_NEW( SIGN, FFT_LEN, CTYPE )
!       IF(UI < 0) THEN
!        WRITE(6,*)' CREATE FAILED'
!        STOP
!       ENDIF
!       CPU1 = CGETSYS_SECONDS()
!       DO I=1,LOOPSIZ
!       BUFI=BUFS
!        CALL FFT_CCT(UI,BUFI,BUFO)
!       ENDDO
!       CALL FFT_DEL(UI)
!       CPU2 = CGETSYS_SECONDS()
!       WRITE(6,*) 'TIME FOR ',LOOPSIZ,' CALLS TO FFT_CCT:' &
!      &              ,CPU2-CPU1

!!      WRITE(6,*) 'BUFO FROM FFT_CCT', FFT_LEN
!!      WRITE(6,*) (BUFO(I),I=1,FFT_LEN)

!       SIGN=-1
!       UI =FFT_NEW( SIGN, FFT_LEN, CTYPE )
!       CALL FFT_CCT(UI,BUFO,BUFI)
!       CALL FFT_NORM(2*FFT_LEN,1.0/FFT_LEN,BUFI)
!       CALL FFT_DEL(UI)

!       OK=1
!       EPS=.01
!       DO I=1,FFT_LEN
!       IF(CABS(BUFI(I)-BUFS(I)).GT. 0.0001*CABS(BUFS(I)+EPS)) THEN
!         OK=0
!       ENDIF
!       ENDDO
!       IF(OK.EQ.1) THEN
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS < 0.0001'
!       ELSE
!        WRITE(6,*) 'DATA ERROR AFTER ROUND TRIP IS > 0.0001'
!       ENDIF
!       RETURN
!       END
!!/VERIFICATION_CODE!
#endif
