
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- kadabra.f90 ------------------------------!!
!!---------------------------- kadabra.f90 ------------------------------!!
!!---------------------------- kadabra.f90 ------------------------------!!


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
! Name       : KADABRA
! Category   : migrations
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Contains utilities used by processes KA and DABRA.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module contains various utilities needed by processes KA and DABRA.
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
!
!
!
!
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-06-20  B. Menger   Removed Unused Variables.
!  3. 2005-01-10  Stoeckley  Add routine to get tapered weights.
!  2. 2003-08-20  Stoeckley  Change defaults for threshholds.
!  1. 2003-06-19  Stoeckley  Initial version.
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
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


   module kadabra_module
   use named_constants_module
   use pc_module
   use tweights_module
   implicit none
   public

   character(len=100),public,save :: KADABRA_IDENT = &
    '$Id: kadabra.f90,v 1.4 2006/06/20 13:11:57 Menger prod sps $'


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


   integer,parameter,public :: KADABRA_NUM_TR_XY       = 3
   real   ,parameter,public :: KADABRA_STDEV           = 3.0
   real   ,parameter,public :: KADABRA_MIN_PERCENT     = 75.0
   real   ,parameter,public :: KADABRA_MAX_PERCENT     = 125.0
   real   ,parameter,public :: KADABRA_RMS_AMPL_THRESH = 0.8
   real   ,parameter,public :: KADABRA_MAX_AMPL_THRESH = 1.2
   real   ,parameter,public :: KADABRA_SEMB_THRESH     = 0.4
   real   ,parameter,public :: KADABRA_PERCENT_VALID   = 60.0
   real   ,parameter,public :: KADABRA_CENTER_PERCENT  = 20.0
   real   ,parameter,public :: KADABRA_TAPERED_PERCENT = 20.0

   interface kadabra_get
      module procedure kadabra_get_iarray
      module procedure kadabra_get_farray
   end interface

   contains


!!------------------------------- get --------------------------------------!!
!!------------------------------- get --------------------------------------!!
!!------------------------------- get --------------------------------------!!


      subroutine kadabra_get_iarray (keyword,parray,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument

      if (pc_num_elements_process(keyword) <= 0 .and. &
          pc_num_elements_gui(keyword,'ReplaceElements') <= 0) return

      call pc_alloc (keyword,parray,nelements)
      end subroutine kadabra_get_iarray



      subroutine kadabra_get_farray (keyword,parray,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      real             ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument

      if (pc_num_elements_process(keyword) <= 0 .and. &
          pc_num_elements_gui(keyword,'ReplaceElements') <= 0) return

      call pc_alloc (keyword,parray,nelements)
      end subroutine kadabra_get_farray


!!--------------------------- verify thresh ---------------------------------!!
!!--------------------------- verify thresh ---------------------------------!!
!!--------------------------- verify thresh ---------------------------------!!


      subroutine kadabra_verify_thresh (num,thresh,default_thresh)
      integer         ,intent(in)    :: num                 ! arguments
      real            ,intent(inout) :: thresh(:)           ! arguments
      real            ,intent(in)    :: default_thresh      ! arguments
      integer                        :: indx                ! local

      do indx = 1,num
        if (thresh(indx) == FNIL) thresh(indx) = default_thresh
        thresh(indx) = max(thresh(indx), 0.0)
      end do
      end subroutine kadabra_verify_thresh


!!--------------------------- verify minmax ---------------------------------!!
!!--------------------------- verify minmax ---------------------------------!!
!!--------------------------- verify minmax ---------------------------------!!


      subroutine kadabra_verify_minmax (num,min_percent,max_percent)
      integer         ,intent(in)    :: num                      ! arguments
      real            ,intent(inout) :: min_percent(:)           ! arguments
      real            ,intent(inout) :: max_percent(:)           ! arguments
      integer                        :: indx                     ! local

      do indx = 1,num
        if (min_percent(indx) == FNIL) min_percent(indx) = KADABRA_MIN_PERCENT
        if (max_percent(indx) == FNIL) max_percent(indx) = KADABRA_MAX_PERCENT
        call mth_constrain (min_percent(indx), 0.0, 100.0)
        max_percent(indx) = max(max_percent(indx), 100.0)
      end do
      end subroutine kadabra_verify_minmax


!!--------------------------- verify stdev ---------------------------------!!
!!--------------------------- verify stdev ---------------------------------!!
!!--------------------------- verify stdev ---------------------------------!!


      subroutine kadabra_verify_stdev (num,stdev)
      integer         ,intent(in)    :: num                ! arguments
      real            ,intent(inout) :: stdev(:)           ! arguments
      integer                        :: indx               ! local

      do indx = 1,num
        if (stdev(indx) == FNIL) stdev(indx) = KADABRA_STDEV
        stdev(indx) = max(stdev(indx), 0.0)
      end do
      end subroutine kadabra_verify_stdev


!!-------------------------- get put options -------------------------------!!
!!-------------------------- get put options -------------------------------!!
!!-------------------------- get put options -------------------------------!!


      subroutine kadabra_get_put_options (keyword,value,options,noptions)
      character(len=*),intent(in)    :: keyword              ! argument
      character(len=*),intent(inout) :: value                ! argument
      character(len=*),intent(in)    :: options(:)           ! argument
      integer,optional,intent(in)    :: noptions             ! argument
      integer                        :: noptions2,indx       ! local
      logical                        :: whoops               ! local

      call pc_get (keyword,value)

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      if (noptions2 > 0 .and. value == ' ') value = options(1)

      whoops = .true.

      do indx = 1,noptions2
           if (value == options(indx)) then
                whoops = .false.
                exit
           end if
      end do

      if (whoops) then
           call pc_error ('invalid',keyword,'choice:',value)
           do indx = 1,noptions2
                call pc_error ('a valid choice for',keyword,'is:',options(indx))
           end do
      end if

      call pc_put_options_field (keyword,options,noptions)
      call pc_put               (keyword,value)
      end subroutine kadabra_get_put_options


!!------------------------ get distance weights ----------------------------!!
!!------------------------ get distance weights ----------------------------!!
!!------------------------ get distance weights ----------------------------!!


      subroutine kadabra_get_distance_weights (use_distance_smoothing,  &
                                               center_weight_percent,   &
                                               kounto,dweights,nx,ny)
      logical           ,intent(in)    :: use_distance_smoothing   ! arguments
      real              ,intent(in)    :: center_weight_percent    ! arguments
      integer           ,intent(in)    :: kounto,nx,ny             ! arguments
      real              ,intent(out)   :: dweights(nx,ny)          ! arguments
      integer                          :: kx,ky      ,midx,midy ! local
      real                             ::      wsum,weight ! local
      real                             :: xdist,ydist ! local
      real                             :: fraction                 ! local
      integer                          :: lunprint                 ! local

!!!!!!!!!!!!!!! get distance weights:

      if (use_distance_smoothing) then
        fraction = 0.01 * center_weight_percent
        midx = (nx + 1) / 2
        midy = (ny + 1) / 2
        if (fraction >= 1.0) then
           dweights(:,:)       = 0.0
           dweights(midx,midy) = 1.0
        else
           do ky = 1,ny
           do kx = 1,nx
             xdist = kx - midx
             ydist = ky - midy
             dweights(kx,ky) = sqrt(xdist**2 + ydist**2)
             if (dweights(kx,ky) > 0.0) dweights(kx,ky) = 1.0 / dweights(kx,ky)
           end do
           end do
           wsum = sum(dweights)
           weight = fraction * wsum / (1.0 - fraction)
           where (dweights == 0.0)
                dweights = weight
           end where
        end if
      else
           dweights(:,:) = 1.0
      end if

      wsum = sum(dweights)
      if (wsum > 0.0) dweights = dweights / wsum

!!!!!!!!!!!!!!! print distance weights:

      if (kounto == 0) then
           call pc_print (' ')
           lunprint = pc_get_lun()
           do ky = 1,ny
                write (lunprint,1000) &
                            (nint(100.0*dweights(ky,kx)),kx=1,nx)
1000            format (' ----------------KADABRA: distance weights ',23i4)
           end do
           call pc_print (' ')
      end if

      end subroutine kadabra_get_distance_weights


!!------------------------ get tapered weights ----------------------------!!
!!------------------------ get tapered weights ----------------------------!!
!!------------------------ get tapered weights ----------------------------!!


      subroutine kadabra_get_tapered_weights (use_tapered_smoothing,   &
                                              tapered_weight_percent,  &
                                              kounto,tweights,nx,ny)
      logical           ,intent(in)    :: use_tapered_smoothing    ! arguments
      real              ,intent(in)    :: tapered_weight_percent   ! arguments
      integer           ,intent(in)    :: kounto,nx,ny             ! arguments
      real              ,intent(out)   :: tweights(nx,ny)          ! arguments
      real                             :: xweights(nx)             ! local
      real                             :: yweights(ny)             ! local
      integer                          :: nxflat,nyflat,kx,ky      ! local
      real                             :: fraction,wsum            ! local
      integer                          :: lunprint                 ! local

!!!!!!!!!!!!!!! get tapered weights:

      if (use_tapered_smoothing) then
           fraction = 0.01 * tapered_weight_percent
           nxflat = nint(nx * (1.0 - fraction))
           nyflat = nint(ny * (1.0 - fraction))
           call mth_constrain_odd (nxflat, 1, nx)
           call mth_constrain_odd (nyflat, 1, ny)
           call tweights_calculate ('HANN',xweights,nx,nx,nxflat)
           call tweights_calculate ('HANN',yweights,ny,ny,nyflat)
           do ky = 1,ny
           do kx = 1,nx
             tweights(kx,ky) = xweights(kx) * yweights(ky)
           end do
           end do
      else
           tweights = 1.0
      end if

      wsum = sum(tweights)
      if (wsum > 0.0) tweights = tweights / wsum

!!!!!!!!!!!!!!! print tapered weights:

      if (kounto == 0) then
           call pc_print (' ')
           lunprint = pc_get_lun()
           do ky = 1,ny
                write (lunprint,1000) &
                            (nint(100.0*tweights(ky,kx)),kx=1,nx)
1000            format (' ----------------KADABRA: tapered weights ',23i4)
           end do
           call pc_print (' ')
      end if

      end subroutine kadabra_get_tapered_weights


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


   end module kadabra_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

