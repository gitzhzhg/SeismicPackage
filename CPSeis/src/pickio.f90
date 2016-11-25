
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- pickio.f90 --------------------------------!!
!!------------------------------- pickio.f90 --------------------------------!!
!!------------------------------- pickio.f90 --------------------------------!!


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
! Name       : PICKIO
! Category   : io
! Written    : 2004-09-14   by: Tom Stoeckley
! Revised    : 2005-03-07   by: Tom Stoeckley
! Maturity   : production
! Purpose    : To read and write refraction statics pick files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing a refraction statics
! pick file (.cst file).
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
! Open the pick file and read or write initial information:
!
!                             o     i        i      o   o
!    call pickio_open_read  (obj,filename,lunprint,err,msg,
!
!             o     o     o     o     o     o    o    o    o     o    o    o
!            ngrp,itmax,mxxgp,mxygp,mnxgp,mnygp,mxo,incgp,rinc,nchan,trsh,izc,
!            nxpw,nypw,npkwn,npwoff,ipkwhx,ipkwhy,xpw,ypw,pkwn)
!             o    o     o     o      o      o     o   o   o
!
!
!                             b     i        i      o   o
!    call pickio_open_write (obj,filename,lunprint,err,msg,
!
!             i     i     i     i     i     i    i    i    i     i    i    i
!            ngrp,itmax,mxxgp,mxygp,mnxgp,mnygp,mxo,incgp,rinc,nchan,trsh,izc,
!            nxpw,nypw,npkwn,npwoff,ipkwhx,ipkwhy,xpw,ypw,pkwn)
!             i    i     i     i      i      i     i   i   i
!
!
! Close the pick file:
!                            
!                        b
!    call pickio_close (obj)
!
!
! Read or write picks for a single shot profile:
!
!                              b   o   o   o     o     o    o    o     o
!    call pickio_read_group  (obj,err,msg,shot,iarriv,ixgp,iygp,ioff,ielev)
!    call pickio_write_group (obj,err,msg,shot,iarriv,ixgp,iygp,ioff,ielev)
!                              b   i   i   i     i     i    i    i     i
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(pickio_struct)      obj = pointer to the PICKIO data structure.
! character(*)        filename = name of the pick file for input or output.
! integer             lunprint = logical unit number for printing (>0).
! integer                  err = error flag (returned).
! character(*)             msg = message for possible printing (returned).
!
! integer                 ngrp = number of groups (shot profiles).
! integer                itmax = maximum pick time on shot profile (millisec).
! integer                mxxgp = maximum X ground position.
! integer                mxygp = maximum Y ground position.
! integer                mnxgp = minimum X ground position.
! integer                mnygp = minimum Y ground position.
! integer                  mxo = maximum offset (ground position units).
! integer                incgp = ground position increment.
! real                    rinc = receiver interval (feet or meters).
! integer                nchan = number of channels in each shot profile.
! real                    trsh = threshhold amplitude for picking.
! character(*)             izc = flag for zero-crossing search ("YES" or "NO").
! integer                 nxpw = number of xpw values  (CANNOT EXCEED 100).
! integer                 nypw = number of ypw values  (CANNOT EXCEED 100).
! integer                npkwn = number of pkwn values (CANNOT EXCEED 1800).
! integer               npwoff = number of pick window offsets (2 thru 6).
! integer               ipkwhx = hwd # for pick window X locations.
! integer               ipkwhy = hwd # for pick window Y locations.
! real               xpw(nxpw) = pick window X locations.
! real               ypw(nypw) = pick window Y locations.
! real             pkwn(npkwn) = number of pkwn values.
!
! real                shot(10) = shot profile information (see below).
! integer        iarriv(nchan) = pick time on trace (milliseconds).
! integer          ixgp(nchan) = receiver X ground position (hwd 35 or 47).
! integer          iygp(nchan) = receiver Y ground position (hwd 36 or  0).
! integer          ioff(nchan) = offset of trace (hwd 6) (feet or meters).
! integer         ielev(nchan) = receiver elevation (hwd 16).
!
!
! The NPKWN argument has the following characteristics:
!
!      NPKWN = NPWOFF * 3 * NXPW *NYPW
!      NPKWN is not actually in the header record on disk.
!
! The following integer named constants are defined:
!
!      PICKIO_MAX_NXPW  (=  100) specifies the minimum required size of XPW.
!      PICKIO_MAX_NYPW  (=  100) specifies the minimum required size of YPW.
!      PICKIO_MAX_NPKWN (= 1800) specifies the minimum required size of PKWN.
!
! The SHOT array contains the following values:
!
!      shot( 1) = source X ground position (hwd 33 or 46).
!      shot( 2) = hwd 20 (source depth).         
!      shot( 3) = hwd 44 (source uphole time).  
!      shot( 4) = hwd 13 (source elev) minus hwd 20.
!      shot( 5) = apparently not used.         
!      shot( 6) = apparently not used.        
!      shot( 7) = apparently not used.       
!      shot( 8) = source Y ground position (hwd 34 or 0).
!      shot( 9) = hwd 9 (shot profile number).  
!      shot(10) = apparently not used.         
!
! The returned error will have one of these integer named constant values:
!
!      PICKIO_OK    means the operation was successful.
!      PICKIO_ERROR means an open or read/write error occurred.
!      PICKIO_EOF   means an end-of-file was encountered.
!
! An error is generated if an array argument is too small.
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
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-03-07  Stoeckley  Initial version.
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
 
 
      module pickio_module
      use dio_module
      use string_module
      use named_constants_module
      implicit none
      public
      private :: pickio_private_read1
      private :: pickio_private_read2
      private :: pickio_private_write

      character(len=100),public,save :: PICKIO_IDENT = &
'$Id: pickio.f90,v 1.1 2005/03/07 13:49:13 Stoeckley prod sps $'

      integer,public,parameter  :: PICKIO_OK    = DIO_OK  
      integer,public,parameter  :: PICKIO_ERROR = DIO_ERROR
      integer,public,parameter  :: PICKIO_EOF   = DIO_EOF

      type,public :: pickio_struct

           private
           type(dio_struct),pointer :: dio
           integer                  :: lunprint
           integer                  :: kount_cards
           integer                  :: kount_groups
           integer                  :: nchan

      end type pickio_struct

      integer,parameter,public :: PICKIO_MAX_NXPW  =  100
      integer,parameter,public :: PICKIO_MAX_NYPW  =  100
      integer,parameter,public :: PICKIO_MAX_NPKWN = 1800

      character(len=*),parameter,private :: f5001 = '(8I6)'
      character(len=*),parameter,private :: f5002 = '(F6.1,I6,F6.3,3X,A3,5I4)'
      character(len=*),parameter,private :: f5005 = '(5(1X,F9.0))'
      character(len=*),parameter,private :: f5006 = '(3(1X,F9.3))'
      character(len=*),parameter,private :: f6002 = '(5F10.3)'
      character(len=*),parameter,private :: f6003 = '(5I6)'

      contains


!!------------------------- pickio open read -------------------------------!!
!!------------------------- pickio open read -------------------------------!!
!!------------------------- pickio open read -------------------------------!!
 
 
      subroutine pickio_open_read (obj,filename,lunprint,err,msg,     &
                                   ngrp,itmax,                        &
                                   mxxgp,mxygp,mnxgp,mnygp,           &
                                   mxo,incgp,                         &
                                   rinc,nchan,trsh,izc,               &
                                   nxpw,nypw,npkwn,                   &
                                   npwoff,ipkwhx,ipkwhy,              &
                                   xpw,ypw,pkwn)

      type(pickio_struct),pointer     :: obj                      ! arguments
      character(len=*)   ,intent(in)  :: filename                 ! arguments
      integer            ,intent(in)  :: lunprint                 ! arguments
      integer            ,intent(out) :: err                      ! arguments
      character(len=*)   ,intent(out) :: msg                      ! arguments
      integer            ,intent(out) :: ngrp,itmax               ! arguments
      integer            ,intent(out) :: mxxgp,mxygp,mnxgp,mnygp  ! arguments
      integer            ,intent(out) :: mxo,incgp                ! arguments
      real               ,intent(out) :: rinc                     ! arguments
      integer            ,intent(out) :: nchan                    ! arguments
      real               ,intent(out) :: trsh                     ! arguments
      character(len=*)   ,intent(out) :: izc                      ! arguments
      integer            ,intent(out) :: nxpw,nypw,npkwn          ! arguments
      integer            ,intent(out) :: npwoff,ipkwhx,ipkwhy     ! arguments
      real               ,intent(out) :: xpw(:),ypw(:),pkwn(:)    ! arguments
      character(len=80)               :: card                     ! local
      integer                         :: i,istat                  ! local
 
!----------get started:

      write (lunprint,*) ' '
      write (lunprint,*) 'PICKIO: OPENING INPUT PICKFILE'
      write (lunprint,*) 'PICKIO: filename = ',trim(filename)
      write (lunprint,*) ' '

      allocate (obj)
      nullify  (obj%dio)

      obj%lunprint     = lunprint
      obj%kount_cards  = 0
      obj%kount_groups = 0
      obj%nchan        = 0

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= PICKIO_OK) then
           write(lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif
 
!----------read first card:

      call pickio_private_read1 (obj,card,err,msg)
      if (err /= PICKIO_OK) return

      read (card,f5001,iostat = istat) ngrp,itmax,               &
                                       mxxgp,mxygp,mnxgp,mnygp,  &
                                       mxo,incgp

      call pickio_private_read2 (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------read second card:

      call pickio_private_read1 (obj,card,err,msg)
      if (err /= PICKIO_OK) return

      read (card,f5002,iostat = istat) rinc,nchan,trsh,izc,      &
                                       nxpw,nypw,                &
                                       npwoff,ipkwhx,ipkwhy

      call pickio_private_read2 (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------check for array size errors:

      npkwn = npwoff * 3 * nxpw * nypw

      obj%nchan = nchan

      if (size(xpw) < nxpw) then
           err = PICKIO_ERROR
           msg = 'XPW ARRAY TOO SMALL'
           write (lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(ypw) < nypw) then
           err = PICKIO_ERROR
           msg = 'YPW ARRAY TOO SMALL'
           write (lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(pkwn) < npkwn) then
           err = PICKIO_ERROR
           msg = 'PKWN ARRAY TOO SMALL'
           write (lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

!----------read xpw cards:

      do i = 1,nxpw,5

           call pickio_private_read1 (obj,card,err,msg)
           if (err /= PICKIO_OK) return

           read (card,f5005,iostat = istat) xpw(i:min(i+4,nxpw))

           call pickio_private_read2 (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

!----------read ypw cards:

      do i = 1,nypw,5

           call pickio_private_read1 (obj,card,err,msg)
           if (err /= PICKIO_OK) return

           read (card,f5005,iostat = istat) ypw(i:min(i+4,nypw))

           call pickio_private_read2 (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

!----------read pkwn cards:

      do i = 1,npkwn,3

           call pickio_private_read1 (obj,card,err,msg)
           if (err /= PICKIO_OK) return

           read (card,f5006,iostat = istat) pkwn(i:min(i+2,npkwn))

           call pickio_private_read2 (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

      end subroutine pickio_open_read
 

!!-------------------------- pickio open write -----------------------------!!
!!-------------------------- pickio open write -----------------------------!!
!!-------------------------- pickio open write -----------------------------!!
 
 
      subroutine pickio_open_write (obj,filename,lunprint,err,msg,     &
                                    ngrp,itmax,                        &
                                    mxxgp,mxygp,mnxgp,mnygp,           &
                                    mxo,incgp,                         &
                                    rinc,nchan,trsh,izc,               &
                                    nxpw,nypw,npkwn,                   &
                                    npwoff,ipkwhx,ipkwhy,              &
                                    xpw,ypw,pkwn)

      type(pickio_struct),pointer     :: obj                      ! arguments
      character(len=*)   ,intent(in)  :: filename                 ! arguments
      integer            ,intent(in)  :: lunprint                 ! arguments
      integer            ,intent(out) :: err                      ! arguments
      character(len=*)   ,intent(out) :: msg                      ! arguments
      integer            ,intent(in)  :: ngrp,itmax               ! arguments
      integer            ,intent(in)  :: mxxgp,mxygp,mnxgp,mnygp  ! arguments
      integer            ,intent(in)  :: mxo,incgp                ! arguments
      real               ,intent(in)  :: rinc                     ! arguments
      integer            ,intent(in)  :: nchan                    ! arguments
      real               ,intent(in)  :: trsh                     ! arguments
      character(len=*)   ,intent(in)  :: izc                      ! arguments
      integer            ,intent(in)  :: nxpw,nypw,npkwn          ! arguments
      integer            ,intent(in)  :: npwoff,ipkwhx,ipkwhy     ! arguments
      real               ,intent(in)  :: xpw(:),ypw(:),pkwn(:)    ! arguments
      character(len=80)               :: card                     ! local
      integer                         :: i,istat                  ! local
 
!----------get started:

      write (lunprint,*) ' '
      write (lunprint,*) 'PICKIO: OPENING OUTPUT PICKFILE'
      write (lunprint,*) 'PICKIO: filename = ',trim(filename)
      write (lunprint,*) 'PICKIO: number of groups = ',ngrp
      write (lunprint,*) 'PICKIO: number of channels = ',nchan
      write (lunprint,*) ' '

      allocate (obj)
      nullify  (obj%dio)

      obj%lunprint     = lunprint
      obj%kount_cards  = 0
      obj%kount_groups = 0
      obj%nchan        = nchan

      if (npkwn /= npwoff * 3 * nxpw * nypw) then
           err = PICKIO_ERROR
           msg = 'NPKWN must be equal to NPWOFF * 3 * NXPW * NYPW'
           write (lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      call dio_open_write (obj%dio,filename,err,msg)
      if (err /= PICKIO_OK) then
           write(lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

!----------write first card:

      write (card,f5001,iostat = istat) ngrp,itmax,               &
                                        mxxgp,mxygp,mnxgp,mnygp,  &
                                        mxo,incgp

      call pickio_private_write (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------write second card:

      write (card,f5002,iostat = istat) rinc,nchan,trsh,izc,      &
                                        nxpw,nypw,                &
                                        npwoff,ipkwhx,ipkwhy

      call pickio_private_write (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------write xpw cards:

      do i = 1,nxpw,5

           write (card,f5005,iostat = istat) xpw(i:min(i+4,nxpw))

           call pickio_private_write (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

!----------write ypw cards:

      do i = 1,nypw,5

           write (card,f5005,iostat = istat) ypw(i:min(i+4,nypw))

           call pickio_private_write (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

!----------write pkwn cards:

      do i = 1,npkwn,3

           write (card,f5006,iostat = istat) pkwn(i:min(i+2,npkwn))

           call pickio_private_write (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

      end subroutine pickio_open_write
 

!!--------------------------- pickio close --------------------------------!!
!!--------------------------- pickio close --------------------------------!!
!!--------------------------- pickio close --------------------------------!!


      subroutine pickio_close (obj)

      type(pickio_struct),pointer :: obj             ! arguments

      if (.not.associated(obj)) return

      write (obj%lunprint,*) ' '
      write (obj%lunprint,*) 'PICKIO: CLOSING PICKFILE'
      write (obj%lunprint,*) 'PICKIO: number of cards ',obj%kount_cards
      write (obj%lunprint,*) 'PICKIO: number of groups ',obj%kount_groups
      write (obj%lunprint,*) ' '

      call dio_close (obj%dio)

      deallocate(obj)

      end subroutine pickio_close


!!--------------------------- pickio read group -------------------------!!
!!--------------------------- pickio read group -------------------------!!
!!--------------------------- pickio read group -------------------------!!


      subroutine pickio_read_group &
                          (obj,err,msg,shot,iarriv,ixgp,iygp,ioff,ielev)

      type(pickio_struct),intent(inout) :: obj                 ! arguments
      integer            ,intent(out)   :: err                 ! arguments
      character(len=*)   ,intent(out)   :: msg                 ! arguments
      real               ,intent(out)   :: shot(:)             ! arguments
      integer            ,intent(out)   :: iarriv(:)           ! arguments
      integer            ,intent(out)   :: ixgp(:)             ! arguments
      integer            ,intent(out)   :: iygp(:)             ! arguments
      integer            ,intent(out)   :: ioff(:)             ! arguments
      integer            ,intent(out)   :: ielev(:)            ! arguments
      character(len=80)                 :: card                ! local
      integer                           :: istat,ic            ! local

!----------check for array size errors:

      obj%kount_groups = obj%kount_groups + 1

      if (size(shot) < 10) then
           err = PICKIO_ERROR
           msg = 'SHOT ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(iarriv) < obj%nchan) then
           err = PICKIO_ERROR
           msg = 'IARRIV ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(ixgp) < obj%nchan) then
           err = PICKIO_ERROR
           msg = 'IXGP ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(iygp) < obj%nchan) then
           err = PICKIO_ERROR
           msg = 'IYGP ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(ioff) < obj%nchan) then
           err = PICKIO_ERROR
           msg = 'IOFF ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

      if (size(ielev) < obj%nchan) then
           err = PICKIO_ERROR
           msg = 'IELEV ARRAY TOO SMALL'
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           return
      endif

!----------read first shot card:

      call pickio_private_read1 (obj,card,err,msg)
      if (err /= PICKIO_OK) return

      read (card,f6002,iostat = istat) shot(1:5)

      call pickio_private_read2 (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------read second shot card:

      call pickio_private_read1 (obj,card,err,msg)
      if (err /= PICKIO_OK) return

      read (card,f6002,iostat = istat) shot(6:10)

      call pickio_private_read2 (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------read receiver cards:

      do ic = 1,obj%nchan

           call pickio_private_read1 (obj,card,err,msg)
           if (err /= PICKIO_OK) return

           read (card,f6003,iostat = istat) &
                         iarriv(ic),ixgp(ic),iygp(ic),ioff(ic),ielev(ic)

           call pickio_private_read2 (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

      end subroutine pickio_read_group


!!--------------------------- pickio write group -------------------------!!
!!--------------------------- pickio write group -------------------------!!
!!--------------------------- pickio write group -------------------------!!


      subroutine pickio_write_group &
                          (obj,err,msg,shot,iarriv,ixgp,iygp,ioff,ielev)

      type(pickio_struct),intent(inout) :: obj                 ! arguments
      integer            ,intent(out)   :: err                 ! arguments
      character(len=*)   ,intent(out)   :: msg                 ! arguments
      real               ,intent(in)    :: shot(:)             ! arguments
      integer            ,intent(in)    :: iarriv(:)           ! arguments
      integer            ,intent(in)    :: ixgp(:)             ! arguments
      integer            ,intent(in)    :: iygp(:)             ! arguments
      integer            ,intent(in)    :: ioff(:)             ! arguments
      integer            ,intent(in)    :: ielev(:)            ! arguments
      character(len=80)                 :: card                ! local
      integer                           :: istat,ic            ! local

!----------write first shot card:

      obj%kount_groups = obj%kount_groups + 1

      write (card,f6002,iostat = istat) shot(1:5)

      call pickio_private_write (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------write second shot card:

      write (card,f6002,iostat = istat) shot(6:10)

      call pickio_private_write (obj,card,istat,err,msg)
      if (err /= PICKIO_OK) return

!----------write receiver cards:

      do ic = 1,obj%nchan

           write (card,f6003,iostat = istat) &
                         iarriv(ic),ixgp(ic),iygp(ic),ioff(ic),ielev(ic)

           call pickio_private_write (obj,card,istat,err,msg)
           if (err /= PICKIO_OK) return

      enddo

      end subroutine pickio_write_group


!!-------------------------- private read1 ----------------------------!!
!!-------------------------- private read1 ----------------------------!!
!!-------------------------- private read1 ----------------------------!!


      subroutine pickio_private_read1 (obj,card,err,msg)

      type(pickio_struct),intent(inout) :: obj                 ! arguments
      character(len=*)   ,intent(out)   :: card                ! arguments
      integer            ,intent(out)   :: err                 ! arguments
      character(len=*)   ,intent(out)   :: msg                 ! arguments

      obj%kount_cards = obj%kount_cards + 1

      call dio_read_card (obj%dio,card)
      call dio_status    (obj%dio,err,msg)

      if (err /= PICKIO_OK) then
           write (obj%lunprint,*) ' '
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           write (obj%lunprint,*) 'PICKIO: ',trim(card)
           write (obj%lunprint,*) 'PICKIO: card number ',obj%kount_cards
           write (obj%lunprint,*) 'PICKIO: group number ',obj%kount_groups
           write (obj%lunprint,*) ' '
           return
      endif

      end subroutine pickio_private_read1


!!-------------------------- private read2 ----------------------------!!
!!-------------------------- private read2 ----------------------------!!
!!-------------------------- private read2 ----------------------------!!


      subroutine pickio_private_read2 (obj,card,istat,err,msg)

      type(pickio_struct),intent(inout) :: obj                 ! arguments
      character(len=*)   ,intent(in)    :: card                ! arguments
      integer            ,intent(in)    :: istat               ! arguments
      integer            ,intent(out)   :: err                 ! arguments
      character(len=*)   ,intent(out)   :: msg                 ! arguments

      if (istat /= 0) then
           err = PICKIO_ERROR
           msg = 'ERROR DECODING CARD IMAGE'
           write (obj%lunprint,*) ' '
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           write (obj%lunprint,*) 'PICKIO: ',trim(card)
           write (obj%lunprint,*) 'PICKIO: card number ',obj%kount_cards
           write (obj%lunprint,*) 'PICKIO: group number ',obj%kount_groups
           write (obj%lunprint,*) ' '
           return
      endif

      err = PICKIO_OK
      msg = 'OK'

      end subroutine pickio_private_read2


!!-------------------------- private write ----------------------------!!
!!-------------------------- private write ----------------------------!!
!!-------------------------- private write ----------------------------!!


      subroutine pickio_private_write (obj,card,istat,err,msg)

      type(pickio_struct),intent(inout) :: obj                 ! arguments
      character(len=*)   ,intent(in)    :: card                ! arguments
      integer            ,intent(in)    :: istat               ! arguments
      integer            ,intent(out)   :: err                 ! arguments
      character(len=*)   ,intent(out)   :: msg                 ! arguments

      obj%kount_cards = obj%kount_cards + 1

      if (istat /= 0) then
           err = PICKIO_ERROR
           msg = 'ERROR ENCODING CARD IMAGE'
           write (obj%lunprint,*) ' '
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           write (obj%lunprint,*) 'PICKIO: ',trim(card)
           write (obj%lunprint,*) 'PICKIO: card number ',obj%kount_cards
           write (obj%lunprint,*) 'PICKIO: group number ',obj%kount_groups
           write (obj%lunprint,*) ' '
           return
      endif

      call dio_write_card (obj%dio,card)
      call dio_status     (obj%dio,err,msg)

      if (err /= PICKIO_OK) then
           write (obj%lunprint,*) ' '
           write (obj%lunprint,*) 'PICKIO: ',trim(msg)
           write (obj%lunprint,*) 'PICKIO: ',trim(card)
           write (obj%lunprint,*) 'PICKIO: card number ',obj%kount_cards
           write (obj%lunprint,*) 'PICKIO: group number ',obj%kount_groups
           write (obj%lunprint,*) ' '
           return
      endif

      end subroutine pickio_private_write


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module pickio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
