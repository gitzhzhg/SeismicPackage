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
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : DENSIFY            (densify a trace)
! Category   : math
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2004-05-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : To densify a trace (resample it to a finer sample rate).
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive is designed to densify an array, which is commonly a seismic
! trace.  This primitive resamples the array to a finer sample interval
! using the FFT primitive.  This primitive also performs the reverse
! operation to restore the original sample interval.
!
! This primitive does not change the starting and ending times on the trace.
! This primitive does not deal with trace header words.
!
! The purpose of this primitive is to prepare the trace for any process
! which requires finer sampling to perform well.
!
! This primitive was made from a subset of the NMO process, which was
! converted to the new system by Randy Selzler.  The purpose of splitting
! this primitive out of the NMO process was to simplify the NMO code and
! to make the function of this primitive reusable.
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
!                             SUBROUTINES
!
!                           o    i        i        o      o     o
!   call densify_create   (obj, ndpt, fineratio, nfine, error, msg)
!   call densify_delete   (obj)
!                           b
!
!                                            opt     opt
!                           i    i       o    b       b
!   call densify_forward  (obj, tri, trfine, mtop, mbottom)
!   call densify_reverse  (obj, trfine, tro, mtop, mbottom)
!                           i    i       o    b       b
!                                            opt     opt
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(densify_struct)  obj = pointer to the densify object.
! integer              ndpt = number of input and output trace values.
! integer         fineratio = trace densification ratio (1 or 2 or 4 or 8).
! integer             nfine = number of trace values in resampled trace TRFINE.
! logical             error = true if a parameter error is discovered.
! character(len=*)      msg = message for possible printing.
! real            tri(ndpt) = input trace (or any array).
! real            tro(ndpt) = output trace (or any array).
! real        trfine(nfine) = trace with finer sample interval.
! integer              mtop = head mute index of trace (changed).
! integer           mbottom = tail mute index of trace (changed).
!
! If FINERATIO is one, no resampling is done.
!
! MTOP and MBOTTOM are adjusted so that any changes to the timings on the
! trace (due to resampling) are also applied to these mute indices.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!  3. 2004-05-03 Stoeckley  Apply scale to TRFINE instead of TRO.
!  2. 2003-10-16 Stoeckley  Add optional arguments MTOP and MBOTTOM.
!  1. 2003-10-02 Stoeckley  Initial version.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module densify_module
      use fft_module
      implicit none
      public

      type,public :: densify_struct
        private

        integer                  :: ndpt
        integer                  :: fineratio
        integer                  :: nfine
        integer                  :: nfft 
        integer                  :: nbuffer
        integer                  :: ntemp
        integer                  :: nhalf
        real                     :: scale
        type(fft_struct),pointer :: fft_inverse
        type(fft_struct),pointer :: fft_forward

      end type densify_struct

      character(len=100),public :: densify_ident = &
        "$Id: densify.f90,v 1.3 2004/05/03 11:29:26 Stoeckley prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine densify_create (obj,ndpt,fineratio,nfine,error,msg)
      implicit none
      type(densify_struct),pointer     :: obj                  ! arguments
      integer             ,intent(in)  :: ndpt                 ! arguments
      integer             ,intent(in)  :: fineratio            ! arguments
      integer             ,intent(out) :: nfine                ! arguments
      logical             ,intent(out) :: error                ! arguments
      character(len=*)    ,intent(out) :: msg                  ! arguments
      integer                          :: ier1,ier2            ! local

!----------allocate and initialize the data structure:

      allocate (obj)

      obj%ndpt      = ndpt
      obj%fineratio = fineratio
      obj%nfine     = 1 + obj%fineratio * (obj%ndpt - 1)
      obj%nfft      = 8
      obj%nbuffer   = 1    
      obj%ntemp     = 1     
      obj%nhalf     = 1      
      obj%scale     = 1.0     

      nullify (obj%fft_inverse)
      nullify (obj%fft_forward)

!----------initialize the output parameters:

      nfine = obj%nfine      
      error = .false.
      msg   = 'DENSIFY module successfully created and initialized'

!----------return if there are any input parameter errors:

      if (obj%fineratio /= 1 .and. obj%fineratio /= 2 .and.  &
          obj%fineratio /= 4 .and. obj%fineratio /= 8) then
             error = .true.
             msg   = 'illegal value for RATIO - must be 1 or 2 or 4 or 8'
             return
      end if

!----------reset some values if resampling is to be performed:

      if (obj%fineratio == 1) return

      do while (obj%nfft < obj%ndpt)
           obj%nfft = 2*obj%nfft
      end do

      obj%nbuffer = obj%nfft*obj%fineratio/2 + 1
      obj%ntemp   = obj%nfft*obj%fineratio + 2
      obj%nhalf   = 1 + obj%nfft / 2
      obj%scale   = 1.0/float(obj%nfft)

      ier1 = fft_create (obj%fft_inverse, -1, obj%nfft              , 'rtoc')
      ier2 = fft_create (obj%fft_forward, +1, obj%nfft*obj%fineratio, 'ctor')

      if (ier1 /= 0 .or. ier2 /= 0) then
           error = .true.
           msg   = 'DENSIFY error creating FFT object'
           return
      end if

      return
      end subroutine densify_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine densify_delete (obj)
      implicit none
      type(densify_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%fft_inverse)) call fft_delete (obj%fft_inverse)
      if (associated(obj%fft_forward)) call fft_delete (obj%fft_forward)

      deallocate(obj)
      return
      end subroutine densify_delete


!!--------------------------- forward ------------------------------------!!
!!--------------------------- forward ------------------------------------!!
!!--------------------------- forward ------------------------------------!!


      subroutine densify_forward (obj, tri, trfine, mtop, mbottom)
      implicit none
      type(densify_struct),intent(in)    :: obj                  ! arguments
      real                ,intent(in)    :: tri(:)               ! arguments
      real                ,intent(out)   :: trfine(:)            ! arguments
      integer    ,optional,intent(inout) :: mtop,mbottom         ! arguments
      complex                            :: buffer(obj%nbuffer)  ! local
      real                               :: temp  (obj%ntemp)    ! local

      if (obj%fineratio == 1) then
           trfine(1:obj%ndpt) = tri(1:obj%ndpt)
           return
      end if

      temp  (1:obj%ndpt)  = tri(1:obj%ndpt)
      temp  (obj%ndpt+1:) = 0.0
      buffer(1:obj%nhalf) = 0.0

      call fft_rc_transform (obj%fft_inverse, temp, buffer)

      buffer(obj%nhalf+1:) = 0.0

      call fft_cr_transform (obj%fft_forward, buffer, temp)

      trfine(1:obj%nfine) = obj%scale * temp(1:obj%nfine)

      if (present(mtop   )) mtop    = 1 + obj%fineratio * (mtop    - 1)
      if (present(mbottom)) mbottom = 1 + obj%fineratio * (mbottom - 1)
      return
      end subroutine densify_forward


!!--------------------------- reverse ------------------------------------!!
!!--------------------------- reverse ------------------------------------!!
!!--------------------------- reverse ------------------------------------!!


      subroutine densify_reverse (obj, trfine, tro, mtop, mbottom)
      implicit none
      type(densify_struct),intent(in)    :: obj                ! arguments
      real                ,intent(in)    :: trfine(:)          ! arguments
      real                ,intent(out)   :: tro(:)             ! arguments
      integer    ,optional,intent(inout) :: mtop,mbottom       ! arguments
      integer                            :: isamp,indx         ! local

      if (obj%fineratio == 1) then
           tro(1:obj%ndpt) = trfine(1:obj%ndpt)
           return
      end if

      do indx = 1,obj%ndpt
           isamp = 1 + obj%fineratio * (indx - 1)
           tro(indx) = trfine(isamp)
      end do

      if (present(mtop   )) mtop    = 1 + (mtop    - 1) / obj%fineratio
      if (present(mbottom)) mbottom = 1 + (mbottom - 1) / obj%fineratio
      return
      end subroutine densify_reverse


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module densify_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

