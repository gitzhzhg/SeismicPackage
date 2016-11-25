!<CPS_v1 type="PRIMITIVE"/>
!!------------------------- rollingbins.f90 --------------------------------!!
!!------------------------- rollingbins.f90 --------------------------------!!
!!------------------------- rollingbins.f90 --------------------------------!!

 
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
! Name       : ROLLINGBINS 
! Category   : sorts
! Written    : 2004-06-15   by: Tom Stoeckley
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Maintain a rolling cache of bins.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This class maintains a rolling cache of bins with the following dimensions:
!
!   dimension 1:   NFOLD  = maximum number of items in a single bin.
!   dimension 2:   NXBINS = number of bins in X direction.
!   dimension 3:   NYROLL = number of bins in Y direction.
!
! Each item consists of the following two values:
!
!                  AAA = any value associated with the bin.
!                  BBB = any value associated with the bin.
!
! The X bin numbers residing in the cache at all times are from 1 thru NXBINS.
!
! The maximum number of Y bins residing in the cache at any one time is NYROLL.
! There is no restriction on the Y bin numbers placed into the cache.
! The Y bins in the cache at any particular time are the last NYROLL ybins
! which were input.
!
! X bins need not be put into the cache in any particular order.
!
! Y bins need not be put into the cache in any particular order, although
! the calling program needs to be aware that older Y bins will be lost as
! newer Y bins are input.
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
!                                 o    i     i      i
!      call rollingbins_create  (obj,nfold,nxbins,nyroll)
!      call rollingbins_delete  (obj)
!                                 b
!
!                                                               opt
!                                 b    b     i     i    i   i    o
!      call rollingbins_put     (obj,ifold,ixbin,iybin,aaa,bbb,irec)
!      call rollingbins_get     (obj,ifold,ixbin,iybin,aaa,bbb,irec)
!                                 i    i     i     i    o   o    o
!                                                               opt
!
!
! type(rollingbins_struct)  obj = pointer to the ROLLINGBINS data structure.
! integer          nfold  = maximum number of items in a single bin.
! integer          nxbins = number of X bins in the cache at all times.
! integer          nyroll = number of Y bins in the cache at any one time.
! integer          ifold  = number of items in the bin (1 thru NFOLD).
! integer          ixbin  = bin number in X direction (1 thru NXBINS).
! integer          iybin  = bin number in Y direction (any value).
! real             aaa    = any value associated with the bin.
! real             bbb    = any value associated with the bin.
! integer          irec   = record number of specified location.
!
! ROLLINGBINS_PUT:
!   Any previous item in the specified location is overwritten.
!   If IFOLD is 0: IFOLD is reset to an empty spot in the bin if possible.
!   If IFOLD is 0: nothing is done if there are no empty spots.
!   Nothing is done if IFOLD or IXBIN is out of range.
!   The oldest Y bin is lost if IYBIN is not already in the cache.
!
! ROLLINGBINS_GET:
!   AAA,BBB will be set to FNIL if IFOLD or IXBIN is out of range.
!   AAA,BBB will be set to FNIL if IYBIN is not in the cache.
!   AAA,BBB will be set to FNIL if the item does not exist in the cache.
!
! IREC will vary from 1 thru NFOLD*NXBINS*NYROLL.
!
! IREC will be set to IFOLD + (IXBIN-1)*NFOLD + (IYROLL-1)*NFOLD*NXBINS,
!                               where IYROLL is between 1 and NYROLL.
!
! IREC will be set to 0 if IFOLD or IXBIN is out of range.
! IREC will be set to 0 if the requested bin cannot be saved or found.
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
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-01-31  Stoeckley  Initial version.
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


      module rollingbins_module
      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: ROLLINGBINS_IDENT = &
'$Id: rollingbins.f90,v 1.1 2005/01/31 14:09:17 Stoeckley prod sps $'

      type,public :: rollingbins_struct              

        private
        integer         :: nfold,nxbins,nyroll
        integer,pointer :: iybins(:)
        real   ,pointer :: aaa(:,:,:)
        real   ,pointer :: bbb(:,:,:)

      end type rollingbins_struct

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine rollingbins_create (obj,nfold,nxbins,nyroll)

      type(rollingbins_struct),pointer    :: obj              ! arguments
      integer                 ,intent(in) :: nfold            ! arguments
      integer                 ,intent(in) :: nxbins           ! arguments
      integer                 ,intent(in) :: nyroll           ! arguments

      allocate (obj)

      obj%nfold  = max(nfold ,1)
      obj%nxbins = max(nxbins,1)
      obj%nyroll = max(nyroll,1)

      allocate (obj%iybins                        (obj%nyroll))
      allocate (obj%aaa    (obj%nfold, obj%nxbins, obj%nyroll))
      allocate (obj%bbb    (obj%nfold, obj%nxbins, obj%nyroll))

      obj%iybins(:)  = 0
      obj%aaa(:,:,:) = FNIL
      obj%bbb(:,:,:) = FNIL

      end subroutine rollingbins_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine rollingbins_delete (obj)

      type(rollingbins_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      deallocate (obj%iybins)
      deallocate (obj%aaa)
      deallocate (obj%bbb)

      deallocate (obj)

      end subroutine rollingbins_delete


!!----------------------------- put ---------------------------------------!!
!!----------------------------- put ---------------------------------------!!
!!----------------------------- put ---------------------------------------!!


      subroutine rollingbins_put (obj,ifold,ixbin,iybin,aaa,bbb,irec)

      type(rollingbins_struct),intent(inout) :: obj               ! arguments
      integer                 ,intent(inout) :: ifold             ! arguments
      integer                 ,intent(in)    :: ixbin,iybin       ! arguments
      real                    ,intent(in)    :: aaa,bbb           ! arguments
      integer        ,optional,intent(out)   :: irec              ! arguments
      integer                                :: iyroll,indx       ! local

!----------verify input parameters and return if out of range:

      if (present(irec)) irec = 0

      if (ifold < 0 .or. ifold > obj%nfold ) return        ! zero allowed.
      if (ixbin < 1 .or. ixbin > obj%nxbins) return

!----------find matching ybin:

      iyroll = 0
      do indx = 1,obj%nyroll
           if (iybin == obj%iybins(indx)) then
                iyroll = indx
                exit
           endif
      enddo

!----------replace lowest ybin if match not found:

      if (iyroll == 0) then
           iyroll = 1
           do indx = 2,obj%nyroll
                if (obj%iybins(indx) < obj%iybins(iyroll)) iyroll = indx
           enddo
           obj%aaa(:,:,iyroll) = FNIL
           obj%bbb(:,:,iyroll) = FNIL
           obj%iybins (iyroll) = iybin
      endif

!----------find empty bin if requested:

      if (ifold == 0) then
           do indx = 1,obj%nfold
                if (obj%aaa(indx,ixbin,iyroll) == FNIL) then
                     ifold = indx
                     exit
                end if
           end do
           if (ifold == 0) return                   ! no empty bins found.
      endif

!----------put bin values:

      obj%aaa(ifold,ixbin,iyroll) = aaa
      obj%bbb(ifold,ixbin,iyroll) = bbb

!----------return optional arguments:

      if (present(irec)) irec = ifold + (ixbin  - 1) * obj%nfold  &
                                      + (iyroll - 1) * obj%nfold * obj%nxbins

      end subroutine rollingbins_put


!!----------------------------- get ---------------------------------------!!
!!----------------------------- get ---------------------------------------!!
!!----------------------------- get ---------------------------------------!!


      subroutine rollingbins_get (obj,ifold,ixbin,iybin,aaa,bbb,irec)

      type(rollingbins_struct),intent(in)  :: obj                 ! arguments
      integer                 ,intent(in)  :: ifold,ixbin,iybin   ! arguments
      real                    ,intent(out) :: aaa,bbb             ! arguments
      integer        ,optional,intent(out) :: irec                ! arguments
      integer                              :: iyroll,indx         ! local

!----------verify input parameters and return if out of range:

      if (present(irec)) irec = 0

      aaa = FNIL
      bbb = FNIL

      if (ifold < 1 .or. ifold > obj%nfold ) return        ! 0 not allowed.
      if (ixbin < 1 .or. ixbin > obj%nxbins) return

!----------find matching ybin:

      iyroll = 0
      do indx = 1,obj%nyroll
           if (iybin == obj%iybins(indx)) then
                iyroll = indx
                exit
           endif
      enddo
      if (iyroll == 0) return                   ! there is no matching bin.

!----------get bin values:

      aaa = obj%aaa(ifold,ixbin,iyroll)
      bbb = obj%bbb(ifold,ixbin,iyroll)

!----------return optional arguments:

      if (present(irec)) irec = ifold + (ixbin  - 1) * obj%nfold  &
                                      + (iyroll - 1) * obj%nfold * obj%nxbins

      end subroutine rollingbins_get


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module rollingbins_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

