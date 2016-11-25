!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- mgather3d.f90 ---------------------------------!!
!!--------------------------- mgather3d.f90 ---------------------------------!!
!!--------------------------- mgather3d.f90 ---------------------------------!!


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
!                         C P S   P R I M I T I V E         
!
! Name       : MGATHER3D   (3D Moving Gather)
! Category   : math
! Written    : 2001-08-17   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : 3D moving gather of traces.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive allows a 3D process module to get a 3D moving gather of
! traces which rolls along one trace at a time.  It is for use by time-domain
! multi-channel operations such as trace mixing or base trace building.
! SDIP3D is an example of a process module which uses this primitive.
!
! This primitive is functionally similar to the MGATHER primitive, which
! is only 2D.
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
!    o                            i     i       i         i        i
!  NSTORE   = MGATHER3D_STORE   (nwih, ndpt, nxgather, nygather, nxbins)
!  NSCRATCH = MGATHER3D_SCRATCH (nwih, ndpt, nxgather, nygather)
!
!                           o    i    i     i     i   i    i   i    i     i
!  CALL MGATHER3D_CREATE  (OBJ, LUN, NWIH, NDPT, NHX, NHY, X1, Y1, XINC, YINC,
!                               NXGATHER, NYGATHER, XWEIGHTS, YWEIGHTS,
!                                  i         i         i         i
!                               NSX, NSY, NSW, NSF, NXBINS, PROGNAME)
!                                i    i    i    i     i        i
!
!                           b    b    i    i    o    o
!  CALL MGATHER3D         (OBJ, NTR, HDI, TRI, HDO, TRO)
!
!  CALL MGATHER3D_DELETE  (OBJ)
!                           b
!
! integer               NSTORE = permanent storage used by MGATHER3D.
! integer             NSCRATCH = scratch storage used by MGATHER3D.
! type(mgather_struct)     OBJ = pointer to the MGATHER3D data structure.
! integer                  NTR = number of input and output traces.
! double         HDI(NWIH,NTR) = input headers.
! real           TRI(NDPT,NTR) = input traces.
! integer                  LUN = unit number for printing (or 0 not to print)
! integer                  NHX = header word containing X coordinate.
! integer                  NHY = header word containing Y coordinate.
! real                      X1 = center of any bin in header word NHX.
! real                      Y1 = center of any bin in header word NHY.
! real                    XINC = bin width (increment) of coords in hwd NHX.
! real                    YINC = bin width (increment) of coords in hwd NHY.
! integer             NXGATHER = # traces in X dir in moving gather (odd).
! integer             NYGATHER = # traces in Y dir in moving gather (odd).
! real      XWEIGHTS(NXGATHER) = weights in X dir to place into header word NSW.
! real      YWEIGHTS(NYGATHER) = weights in Y dir to place into header word NSW.
! integer                  NSX = scratch header word to receive the X bin coord.
! integer                  NSY = scratch header word to receive the Y bin coord.
! integer                  NSW = scratch header word to receive the weight.
! integer                  NSF = scratch header word for private flag.
! integer               NXBINS = maximum number of traces (bins) in a line.
! character(len=*)    PROGNAME = program or process name for printing.
! double    HDO(NWIH,NXGATHER*NYGATHER) = headers of moving gather (output).
! real      TRO(NDPT,NXGATHER*NYGATHER) = traces of moving gather (output).
!
! The first and second dimensions of the arrays must not be smaller than
! the sizes shown, but may exceed the sizes shown.
!
! The WEIGHTS can be any numbers >= zero.  They are set into the specified
! header word NSW in each trace in the moving gather.
!
! The NSX header words in the moving gather will all be set to the bin number
! calculated from NHX and X1 and XINC.  XINC must be > zero.
!
! The NSY header words in the moving gather will all be set to the bin number
! calculated from NHY and Y1 and YINC.  YINC must be > zero.
!
! The input value of NTR must be the number of traces in HDI and TRI
!  (unless it is set to NEED_TRACES or NO_MORE_TRACES).
!
! The output value of NTR will always be set to NXGATHER * NYGATHER
!  (unless it is set to NEED_TRACES or NO_MORE_TRACES or FATAL_ERROR).
!
! Since HDO and TRO are copies of traces maintained internally, they can be
! changed without affecting the traces returned in the next call to MGATHER3D.
!
! MGATHER3D_CREATE will return a nullified pointer if an error occurs.
! MGATHER3D_DELETE does nothing if OBJ is not associated.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!  1. Each call to this routine outputs a moving gather such that
!     each gather corresponds to one input trace.  The gather will
!     always contain exactly NXGATHER * NYGATHER traces arranged in a
!     regularized XY grid.  The input trace associated with the gather
!     will always be in the center of this grid (and in the center
!     of the gather).  Any missing input traces will be replaced by
!     dead traces.  If two or more input traces fall into the same
!     bin, only one of the traces in that bin will be used.
!
!  2. This primitive uses a temporary disk file to store traces.
!
!-------------------------------------------------------------------------------
!                            EXAMPLE OF USE
!
!  In the DATA STRUCTURE of a process module:
!
!        type(mgather3d_struct),pointer :: mgather3d
!
!  In the CREATE ROUTINE of a process module:
!
!        nullify (obj%mgather3d)
!
!  In the UPDATE ROUTINE of a process module:
!
!        call mgather3d_delete (obj%mgather3d)     ! in case already exists.
!
!        call mgather3d_create (obj%mgather3d,lun,nwih,ndpt,nhx,nhy,  &
!                                 x1,y1,xinc,yinc,nxgather,nygather,  &
!                                 xweights,yweights,nsx,nsy,nsw,nsf,  &
!                                 nxbins,progname)
!
!  In the WRAPUP ROUTINE of a process module:
!
!        call mgather3d_delete (obj%mgather3d)
!
!  In the TRACE PROCESSING ROUTINE of a process module, where NTR, HDI,
!  and TRI are the input arguments in the process module routine:
!
!        call mgather3d (obj%mgather3d,ntr,hdi,tri,hdo,tro)
!
!        if (ntr == NEED_TRACES) return       ! go back for more traces.
!        if (ntr == NO_MORE_TRACES .or. &     ! we are finished.
!            ntr == FATAL_ERROR) then         ! a fatal I/O error has occurred.
!             call xxxx_wrapup (obj)
!             return
!        end if
!
!     !!! Now NTR will be equal to NXGATHER * NYGATHER.
!     !!! Use the moving gather HDO(NTR) and TRO(NTR) to calculate one
!     !!! or more output traces.
!
!        ntr = 1                    ! this is the number of output traces.
!        return
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!008. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!007. 2006-01-10  B. Menger  Removed Unused Variables.
!  6. 2004-06-08  Stoeckley  Fix bug whereby message(1:5) was referenced when
!                             message had a length < 5.
!  5. 2003-05-13  Stoeckley  Change to allow LUN to be zero to repress printing.
!  4. 2002-10-10  Stoeckley  Change to use the MTH module for binning.
!  3. 2001-12-13  Stoeckley  Set XY coordinate header words for inserted dead
!                             traces; add additional error messages; remove
!                             an error message for a case where there really
!                             is no error.
!  2. 2001-11-14  Stoeckley  Reduce excessive printouts.
!  1. 2001-08-17  Stoeckley  Initial version, created from code originally
!                             residing in the SDIP3D process.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY ISSUES           
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module mgather3d_module
      use named_constants_module
      use temptfile_module
      use mth_module
      implicit none
      private
      public :: mgather3d_store
      public :: mgather3d_scratch
      public :: mgather3d_create
      public :: mgather3d_delete
      public :: mgather3d

      character(len=100),public,save :: MGATHER3D_IDENT = &
'$Id: mgather3d.f90,v 1.8 2006/09/18 13:32:50 Glover prod sps $'


      type,public :: mgather3d_struct

        private
        integer                  :: lun                  ! input parameters
        integer                  :: nwih,ndpt            ! input parameters
        integer                  :: nhx,nhy              ! input parameters
        double precision         :: x1,y1                ! input parameters
        double precision         :: xinc,yinc            ! input parameters
        integer                  :: nxgather,nygather    ! input parameters
        integer                  :: nsx,nsy,nsw,nsf      ! input parameters
        integer                  :: nxbins               ! input parameters
        character(len=32)        :: prefix               ! input parameters

        integer                  :: mntod                ! dependent
        integer                  :: nkeep                ! dependent
        integer                  :: ikeep                ! dependent
        integer                  :: nrec                 ! dependent
        integer                  :: last_input           ! dependent
        integer                  :: last_output          ! dependent
        integer                  :: istat(10)            ! dependent
        real                     :: fstat(10)            ! dependent
        integer                  :: midx,midy            ! dependent

        real            ,pointer :: xweights(:)          ! input parameters
        real            ,pointer :: yweights(:)          ! input parameters
        double precision,pointer :: hdgather(:,:,:)      ! dependent
        real            ,pointer :: trgather(:,:,:)      ! dependent
        integer         ,pointer :: kxbin   (:)          ! dependent
        integer         ,pointer :: kybin   (:)          ! dependent

        type(temptfile_struct),pointer :: temptfile

      end type mgather3d_struct


      contains


!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!


      function mgather3d_store &
                      (nwih,ndpt,nxgather,nygather,nxbins) result (nstore)
      implicit none
      integer,intent(in)  :: nwih,ndpt,nxgather,nygather,nxbins   ! arguments
      integer             :: nstore                               ! result
      integer             :: mntod                                ! local

      mntod  = nxbins * (nygather + 1) + 1
      nstore = nxgather + nygather &
                  + (nwih + ndpt) * nxgather * nygather + 2 * mntod
      return
      end function mgather3d_store



      function mgather3d_scratch &
                      (nwih,ndpt,nxgather,nygather) result (nscratch)
      implicit none
      integer,intent(in)  :: nwih,ndpt,nxgather,nygather          ! arguments
      integer             :: nscratch                             ! result

      nscratch = (nwih + ndpt) * nxgather * nygather
      return
      end function mgather3d_scratch


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine mgather3d_create (obj,lun,nwih,ndpt,nhx,nhy,          &
                                   x1,y1,xinc,yinc,nxgather,nygather,  &
                                   xweights,yweights,nsx,nsy,nsw,      &
                                   nsf,nxbins,progname)
      implicit none
      type(mgather3d_struct),pointer     :: obj                     ! arguments
      integer               ,intent(in)  :: lun,nwih,ndpt,nhx,nhy   ! arguments
      real                  ,intent(in)  :: x1,y1,xinc,yinc         ! arguments
      integer               ,intent(in)  :: nxgather,nygather       ! arguments
      real                  ,intent(in)  :: xweights(:)             ! arguments
      real                  ,intent(in)  :: yweights(:)             ! arguments
      integer               ,intent(in)  :: nsx,nsy,nsw,nsf,nxbins  ! arguments
      character(len=*)      ,intent(in)  :: progname                ! arguments
      integer                            :: ier                     ! local

      allocate (obj)

      nullify (obj%temptfile) ! jpa
      obj%lun      = lun 
      obj%nwih     = nwih
      obj%ndpt     = ndpt
      obj%nhx      = nhx
      obj%nhy      = nhy
      obj%x1       = x1
      obj%y1       = y1
      obj%xinc     = xinc
      obj%yinc     = yinc
      obj%nxgather = nxgather
      obj%nygather = nygather
      obj%nsx      = nsx
      obj%nsy      = nsy
      obj%nsw      = nsw
      obj%nsf      = nsf
      obj%nxbins   = nxbins
      obj%prefix   = trim(progname)//':'

      obj%mntod       = nxbins * (nygather + 1) + 1
      obj%nkeep       = NEED_TRACES
      obj%ikeep       = 0
      obj%nrec        = 0
      obj%last_input  = 0
      obj%last_output = 0
      obj%istat(:)    = 0
      obj%fstat(:)    = 0.0
      obj%midx        = (obj%nxgather + 1) / 2
      obj%midy        = (obj%nygather + 1) / 2

      if (lun > 0) then
        write (lun,*) trim(obj%prefix),  &
                        ' maximum number of traces on disk will be ',obj%mntod
      end if

      allocate (obj%xweights (nxgather)               )
      allocate (obj%yweights (nygather)               )
      allocate (obj%hdgather (nwih,nxgather,nygather) )
      allocate (obj%trgather (ndpt,nxgather,nygather) )
      allocate (obj%kxbin    (obj%mntod)              )
      allocate (obj%kybin    (obj%mntod)              )

      obj%xweights(:) = xweights(1:nxgather)
      obj%yweights(:) = yweights(1:nygather)

      obj%hdgather(  :,:,:) =  0.0
      obj%hdgather(nsw,:,:) = -1.0
      obj%trgather(  :,:,:) =  0.0

      call temptfile_open (obj%temptfile, progname, nwih, ndpt, lun, ier, &
                                     maxrecords=obj%mntod, vartypes='DR')

      if (ier /= TEMPTFILE_OK) then
        if (lun > 0) then
           write (lun,*) trim(obj%prefix),' MGATHER3D error opening TEMPTFILE'
           write (lun,*) trim(obj%prefix),' MGATHER3D deleted'
        end if
        call mgather3d_delete (obj)
      end if
      return
      end subroutine mgather3d_create


!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!


      subroutine mgather3d_delete (obj)
      implicit none
      type(mgather3d_struct),pointer :: obj                  ! arguments

      if (.not.associated(obj)) return

      call mgather3d_statistics (obj)

      deallocate (obj%xweights)
      deallocate (obj%yweights)
      deallocate (obj%hdgather)
      deallocate (obj%trgather)
      deallocate (obj%kxbin)
      deallocate (obj%kybin)

      call temptfile_close (obj%temptfile)

      deallocate (obj)
      return
      end subroutine mgather3d_delete


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine mgather3d (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(mgather3d_struct),intent(inout) :: obj            ! arguments
      integer               ,intent(inout) :: ntr            ! arguments
      double precision      ,intent(inout) :: hdi(:,:)       ! arguments
      real                  ,intent(in)    :: tri(:,:)       ! arguments
      double precision      ,intent(out)   :: hdo(:,:)       ! arguments
      real                  ,intent(out)   :: tro(:,:)       ! arguments
      integer                              :: i,ix,iy        ! local

      call mgather3d_private_execute (obj,ntr,hdi,tri)

      if (ntr == NEED_TRACES .or.  &
          ntr == FATAL_ERROR .or.  &
          ntr == NO_MORE_TRACES) return

      !!!! now ntr == obj%nxgather * obj%nygather.

      i = 0
      do iy = 1,obj%nygather
      do ix = 1,obj%nxgather
           i = i + 1
           hdo(1:obj%nwih,i) = obj%hdgather(:,ix,iy)
           tro(1:obj%ndpt,i) = obj%trgather(:,ix,iy)
      end do
      end do
      return
      end subroutine mgather3d


!!--------------------------- private execute -----------------------------!!
!!--------------------------- private execute -----------------------------!!
!!--------------------------- private execute -----------------------------!!


      subroutine mgather3d_private_execute (obj,ntr,hdi,tri)
      implicit none
      type(mgather3d_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(inout) :: ntr                 ! arguments
      double precision      ,intent(inout) :: hdi(:,:)            ! arguments
      real                  ,intent(in)    :: tri(:,:)            ! arguments
      logical                              :: error               ! local
      integer                              ::   ix,iy,irec ! local
      double precision :: hdprev(obj%nwih,obj%nxgather,obj%nygather)  ! local
      real             :: trprev(obj%ndpt,obj%nxgather,obj%nygather)  ! local

!----------GET STARTED.

      if (obj%nkeep == NEED_TRACES) then  ! we are receiving traces from above.
           obj%nkeep = ntr                ! ntr will be positive or zero.
           obj%ikeep = 0
      else                         ! someone from below wants another trace.
           continue                ! ntr is NEED_TRACES.
      end if

!----------ADD THE NEXT INPUT TRACE.

      if (obj%nkeep > 0) then
333        if (obj%ikeep == obj%nkeep) then
                obj%nkeep = NEED_TRACES
                ntr       = NEED_TRACES
                return
           end if

           call mgather3d_next_irec (obj,  irec,error)
           if (error) then
                ntr = FATAL_ERROR
                return
           end if

           if (irec > 0) then
                obj%ikeep = obj%ikeep + 1
                call mgather3d_add &
                           (obj,irec,hdi(:,obj%ikeep),tri(:,obj%ikeep),error)
                if (error) then
                     ntr = FATAL_ERROR
                     return
                end if

                obj%last_input = irec
                if (irec > obj%nrec) obj%nrec = irec
                go to 333
           end if
      end if

!----------DECIDE WHETHER WE ARE FINISHED.

      if (obj%nkeep == 0 .and. obj%last_output == obj%last_input) then
           ntr = NO_MORE_TRACES
           return
      end if

!----------copy previous gather to scratch space.

      hdprev(:,:,:) = obj%hdgather(:,:,:)
      trprev(:,:,:) = obj%trgather(:,:,:)

!----------initialize new gather to dead traces (and weights to -1).

      obj%hdgather(      :,:,:) =  0.0
      obj%hdgather(obj%nsw,:,:) = -1.0
      obj%trgather(      :,:,:) =  0.0
      obj%hdgather(      2,:,:) = 1
      obj%hdgather(     64,:,:) = obj%ndpt

!----------set header word 1 for middle trace.

      obj%hdgather(1,obj%midx,obj%midy) = obj%istat(1) + 1.0

!----------GET THE NEW GATHER.

      irec = obj%last_output + 1                   ! index of middle trace.
      if (irec > obj%nrec) irec = 1
      obj%last_output = irec

      call mgather3d_gather (obj,irec,hdprev,trprev,error)
      if (error) then
           ntr = FATAL_ERROR
           return
      end if

!----------add weights to the moving gather.

      DO IY = 1,obj%nygather
      DO IX = 1,obj%nxgather
           if (obj%hdgather(obj%nsw,ix,iy) /= -1.0) &
               obj%hdgather(obj%nsw,ix,iy) = obj%xweights(ix) * obj%yweights(iy)
      end do
      end do

      call mgather3d_full_print (obj,'after adding weights:')

!----------debugging.

      call mgather3d_debug (obj,error)
      if (error) then
           ntr = FATAL_ERROR
           return
      end if

      ntr = obj%nxgather * obj%nygather
      return
      end subroutine mgather3d_private_execute


!!------------------------------ add ---------------------------------------!!
!!------------------------------ add ---------------------------------------!!
!!------------------------------ add ---------------------------------------!!

!     add trace to disk at record irec.
!     header word obj%nsw is used for weights (1=live, -1=dead).
!     header word obj%nsx is used for X bin number.
!     header word obj%nsy is used for Y bin number.


      subroutine mgather3d_add (obj,irec,hdi,tri,error)
      implicit none
      type(mgather3d_struct),intent(inout) :: obj               ! arguments
      integer               ,intent(in)    :: irec              ! arguments
      double precision      ,intent(inout) :: hdi(:)            ! arguments
      real                  ,intent(in)    :: tri(:)            ! arguments
      logical               ,intent(out)   :: error             ! arguments
      integer                              :: ier               ! local

!----------update the directory.

      obj%kxbin(irec) = mth_bin_number (obj%x1, obj%xinc, hdi(obj%nhx))
      obj%kybin(irec) = mth_bin_number (obj%y1, obj%yinc, hdi(obj%nhy))

!----------set scratch header words.

      hdi(obj%nsw) = 1.0
      hdi(obj%nsx) = obj%kxbin(irec)
      hdi(obj%nsy) = obj%kybin(irec)

      if (hdi(25) == 0.0) hdi(obj%nsw) = -1.0

!----------save trace to disk.

      call temptfile_write (obj%temptfile,irec,hdi,tri,ier)

      if (ier /= TEMPTFILE_OK) then
        if (obj%lun > 0) then
          write(obj%lun,*) &
                  trim(obj%prefix),' Error writing tmp file at trace ',irec
          call mgather3d_full_print (obj,'FATAL ERROR IN MGATHER3D_ADD')
        endif
        error = .true.
        return
      endif

      error = .false.
      return
      end subroutine mgather3d_add


!!--------------------------- next irec ------------------------------------!!
!!--------------------------- next irec ------------------------------------!!
!!--------------------------- next irec ------------------------------------!!

! returns irec = next available record number to receive a trace.
! returns zero if there is no available space for a trace.


      subroutine mgather3d_next_irec (obj,  irec,error)
      implicit none
      type(mgather3d_struct),intent(in)  :: obj               ! arguments
      integer               ,intent(out) :: irec              ! arguments
      logical               ,intent(out) :: error             ! arguments

      if (obj%nrec > obj%mntod) then
           if (obj%lun > 0) then
             write(obj%lun,*) &
                   trim(obj%prefix),' MGATHER3D programming error 1:'
             write(obj%lun,*) &
                   trim(obj%prefix),' NREC = ',obj%nrec,'  MNTOD = ',obj%mntod
             call mgather3d_full_print &
                   (obj,'FATAL ERROR IN MGATHER3D_NEXT_IREC')
           endif
           error = .true.
           return
      else if (obj%nrec < obj%mntod) then
            if (obj%last_input /= obj%nrec) then
              if (obj%lun > 0) then
                 write(obj%lun,*) &
                    trim(obj%prefix),' MGATHER3D programming error 2:'
                 write(obj%lun,*) &
                    trim(obj%prefix),' NREC = ',obj%nrec,'  MNTOD = ',obj%mntod
                 write(obj%lun,*) &
                    trim(obj%prefix),' LAST_INPUT = ',obj%last_input
                 call mgather3d_full_print &
                    (obj,'FATAL ERROR IN MGATHER3D_NEXT_IREC')
              end if
              error = .true.
              return
            end if
            irec = obj%last_input + 1
      else if (obj%last_output > obj%last_input + obj%nrec/2) then
            irec = obj%last_input + 1
      else if (obj%last_output < obj%last_input .and.    &
               obj%last_output > obj%last_input - obj%nrec/2) then
            irec = obj%last_input + 1
            if (irec > obj%mntod) irec = 1
      else
            irec = 0
      end if
      error = .false.
      return
      end subroutine mgather3d_next_irec


!!--------------------------- gather ---------------------------------------!!
!!--------------------------- gather ---------------------------------------!!
!!--------------------------- gather ---------------------------------------!!

! irec = index of middle trace.
! hdprev and trprev contain the previous gather (input).
! hdgather and trgather will contain the next gather (output).
! unfilled portions of the gather are set to dead traces.
! the original trace is in the middle of the output gather.


      subroutine mgather3d_gather (obj,irec,hdprev,trprev,error)
      implicit none
      type(mgather3d_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(in)    :: hdprev(:,:,:)      ! arguments
      real                  ,intent(in)    :: trprev(:,:,:)      ! arguments
      logical               ,intent(out)   :: error              ! arguments
      integer             :: ix,iy,ix1,iy1,ix2,iy2,ier,irec2     ! local
      integer             :: kxmin,kxmax,kymin,kymax,itrace      ! local
      integer             :: xbin,ybin                           ! local

!----------get started.

      itrace = nint(obj%hdgather(1,obj%midx,obj%midy))

      call mgather3d_full_print (obj,'before setting bin numbers:')

!----------set bin numbers and coordinates of traces in new gather.

      do iy = 1,obj%nygather
      do ix = 1,obj%nxgather
        xbin = obj%kxbin(irec) + ix - obj%midx
        ybin = obj%kybin(irec) + iy - obj%midy
!!!     obj%hdgather(obj%nsw,ix,iy) = -1.0                    ! already done
        obj%hdgather(obj%nsx,ix,iy) = xbin
        obj%hdgather(obj%nsy,ix,iy) = ybin
        obj%hdgather(obj%nhx,ix,iy) = mth_bin_center (obj%x1, obj%xinc, xbin)
        obj%hdgather(obj%nhy,ix,iy) = mth_bin_center (obj%y1, obj%yinc, ybin)
      end do
      end do

      call mgather3d_full_print (obj,'after setting bin numbers:')

!----------copy reusable traces from previous gather to new gather.

      do iy1 = 1,obj%nygather
      do ix1 = 1,obj%nxgather
       do iy2 = 1,obj%nygather
       do ix2 = 1,obj%nxgather

        if(hdprev      (obj%nsw,ix1,iy1) /= -1.0                         .and.&
           obj%hdgather(obj%nsw,ix2,iy2) == -1.0                         .and.&
           hdprev      (obj%nsx,ix1,iy1) == obj%hdgather(obj%nsx,ix2,iy2).and.&
           hdprev      (obj%nsy,ix1,iy1) == obj%hdgather(obj%nsy,ix2,iy2)) then

            obj%hdgather(      :,ix2,iy2) = hdprev(:,ix1,iy1)
            obj%trgather(      :,ix2,iy2) = trprev(:,ix1,iy1)
            obj%hdgather(obj%nsw,ix2,iy2) = 1.0
            obj%hdgather(obj%nsf,ix2,iy2) = 1.0
        end if

       end do
       end do
      end do
      end do

      call mgather3d_full_print (obj,'after filling in from previous gather:')

!----------fill in new gather from disk buffer.

      kxmin = obj%kxbin(irec) - obj%nxgather/2
      kxmax = obj%kxbin(irec) + obj%nxgather/2
      kymin = obj%kybin(irec) - obj%nygather/2
      kymax = obj%kybin(irec) + obj%nygather/2

      do irec2 = 1,obj%nrec
           if (obj%kxbin(irec2) >= kxmin .and. obj%kxbin(irec2) <= kxmax .and. &
               obj%kybin(irec2) >= kymin .and. obj%kybin(irec2) <= kymax) then

                  ix = obj%kxbin(irec2) - kxmin + 1
                  iy = obj%kybin(irec2) - kymin + 1

                  if (obj%hdgather(obj%nsw,ix,iy) == -1.0) then
                    call temptfile_read (obj%temptfile,irec2,  &
                           obj%hdgather(:,ix,iy),obj%trgather(:,ix,iy),ier)

                    if (ier /= TEMPTFILE_OK) then
                      if (obj%lun > 0) then
                        write(obj%lun,*) trim(obj%prefix), &
                                  ' Error writing tmp file at trace ',irec2
                        call mgather3d_full_print &
                                      (obj,'FATAL ERROR IN MGATHER_GATHER')
                      endif
                      error = .true.
                      return
                    endif

                    obj%hdgather(obj%nsw,ix,iy) = 1.0
                    obj%hdgather(obj%nsf,ix,iy) = 2.0
                  end if
           end if
      end do

      call mgather3d_full_print (obj,'after filling in from disk buffer:')

 ! NOTE: If there are two or more traces on disk in the same bin, it is
 ! possible that the middle trace read off the disk will have a different
 ! header word 1 than expected.  Therefore, the following error test has
 ! been commented out on 2001-12-04 because it is not really an error.
 ! Instead, the new statement below the commented code was added to reset
 ! the sequential trace number to the correct value, so that the middle
 ! trace in the moving gather always has a correct incrementing sequence
 ! number.

   !  if (obj%hdgather(1,obj%midx,obj%midy) /= itrace) then
   !       write(obj%lun,*) trim(obj%prefix),                       &
   !                        ' incorrect trace number at trace# ',   &
   !                        itrace,'  (header word 1 is ',          &
   !                        obj%hdgather(1,obj%midx,obj%midy),')'
   !       call mgather3d_full_print (obj,'FATAL ERROR IN MGATHER3D_GATHER')
   !       error = .true.
   !       return
   !  end if

      obj%hdgather(1,obj%midx,obj%midy) = itrace      ! added 2001-12-04

      error = .false.
      return
      end subroutine mgather3d_gather


!!--------------------------- debug ----------------------------------------!!
!!--------------------------- debug ----------------------------------------!!
!!--------------------------- debug ----------------------------------------!!

! header 1       = sequential trace number.
! header obj%nhx = X coordinate.
! header obj%nhy = Y coordinate.
! header obj%nsx = X bin number.
! header obj%nsy = Y bin number.
! header obj%nsw = weight (-1 do not use, 0 search only, or >0).
! header obj%nsf = 1 if from prev gather, or 2 if from disk, or 0.

! the values of istat and fstat have been initialized to 0.

! istat( 1) = #traces.              fstat( 1) = unused.
! istat( 2) = min(#used).           fstat( 2) = global min min(X-XMID).
! istat( 3) = max(#used).           fstat( 3) = global max max(X-XMID).
! istat( 4) = av (#used).           fstat( 4) = global min min(Y-YMID).
! istat( 5) = unused.               fstat( 5) = global max max(Y-YMID).
! istat( 6) = unused.               fstat( 6) = global av  min(X-XMID).
! istat( 7) = unused.               fstat( 7) = global av  max(X-XMID).
! istat( 8) = unused.               fstat( 8) = global av  min(Y-YMID).
! istat( 9) = unused.               fstat( 9) = global av  max(Y-YMID).
! istat(10) = unused.               fstat(10) = unused.


      subroutine mgather3d_debug (obj,error)
      implicit none
      type(mgather3d_struct),intent(inout) :: obj                 ! arguments
      logical               ,intent(out)   :: error               ! arguments
      real                                 :: xcoord,ycoord       ! local
      real                                 :: dxcoord,dycoord     ! local
      real                                 :: dxmin,dymin         ! local
      real                                 :: dxmax,dymax         ! local
      integer                              :: itrace,ix,iy,used   ! local
      integer                              :: itrace2             ! local

!----------verify trace sequence number.

      obj%istat(1) = obj%istat(1) + 1
      itrace       = obj%istat(1)
      itrace2      = obj%hdgather(1,obj%midx,obj%midy)

      if (itrace2 /= itrace) then
        if (obj%lun > 0) then
           write(obj%lun,*) trim(obj%prefix),                        &
                            ' trace sequence error at trace# ',      &
                            itrace,'  (header word 1 is ',itrace2,')'
           call mgather3d_full_print (obj,'FATAL ERROR IN MGATHER3D_DEBUG')
        end if
        error = .true.
        return
      end if

!----------get min and max coordinates in gather.

      xcoord = obj%hdgather(obj%nhx,obj%midx,obj%midy)
      ycoord = obj%hdgather(obj%nhy,obj%midx,obj%midy)
      dxmin  = 0.0
      dxmax  = 0.0
      dymin  = 0.0
      dymax  = 0.0
      used   = 0

      do iy = 1,obj%nygather
      do ix = 1,obj%nxgather
        if (obj%hdgather(obj%nsw,ix,iy) > 0.0) then
             dxcoord = obj%hdgather(obj%nhx,ix,iy) - xcoord
             dycoord = obj%hdgather(obj%nhy,ix,iy) - ycoord
             if (used == 0) then
                  dxmin = dxcoord
                  dxmax = dxcoord
                  dymin = dycoord
                  dymax = dycoord
             else
                  dxmin = min(dxcoord,dxmin)
                  dxmax = max(dxcoord,dxmax)
                  dymin = min(dycoord,dymin)
                  dymax = max(dycoord,dymax)
             end if
             used = used + 1
        end if
      end do
      end do

!----------optional printout.

      call mgather3d_full_print (obj,' ')
      call mgather3d_part_print (obj,dxmin,dxmax,dymin,dymax,used)

!----------save min and max values.

      if (itrace == 1) then
           obj%fstat(2) = dxmin
           obj%fstat(3) = dxmax
           obj%fstat(4) = dymin
           obj%fstat(5) = dymax
           obj%istat(2) = used
           obj%istat(3) = used
      else
           obj%fstat(2) = min(obj%fstat(2),dxmin)
           obj%fstat(3) = max(obj%fstat(3),dxmax)
           obj%fstat(4) = min(obj%fstat(4),dymin)
           obj%fstat(5) = max(obj%fstat(5),dymax)
           obj%istat(2) = min(obj%istat(2),used)
           obj%istat(3) = max(obj%istat(3),used)
      end if

!----------save sums of values.

      obj%fstat(6) = obj%fstat(6) + dxmin
      obj%fstat(7) = obj%fstat(7) + dxmax
      obj%fstat(8) = obj%fstat(8) + dymin
      obj%fstat(9) = obj%fstat(9) + dymax
      obj%istat(4) = obj%istat(4) + used
      error        = .false.
      return
      end subroutine mgather3d_debug


!!--------------------------- statistics -----------------------------------!!
!!--------------------------- statistics -----------------------------------!!
!!--------------------------- statistics -----------------------------------!!


      subroutine mgather3d_statistics (obj)
      implicit none
      type(mgather3d_struct),intent(in) :: obj               ! arguments
      integer                           :: n,i               ! local
      real                              :: sum               ! local

      if (obj%lun <= 0) return

      n = obj%istat(1)
      if (n == 0) return
      sum = obj%istat(4)

      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                           ' ',obj%istat(1),' moving gathers created.'
      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                    ' minimum #traces used in moving gather: ',obj%istat(2)
      write (obj%lun,*) trim(obj%prefix), &
                    ' maximum #traces used in moving gather: ',obj%istat(3)
      write (obj%lun,*) trim(obj%prefix), &
                           ' average #traces used in moving gather: ',sum/n
      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                  ' minimum X-difference of all moving gathers: ',obj%fstat(2)
      write (obj%lun,*) trim(obj%prefix), &
                  ' maximum X-difference of all moving gathers: ',obj%fstat(3)
      write (obj%lun,*) trim(obj%prefix), &
                           ' average minimum X-difference: ',obj%fstat(6)/n
      write (obj%lun,*) trim(obj%prefix), &
                           ' average maximum X-difference: ',obj%fstat(7)/n
      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                  ' minimum Y-difference of all moving gathers: ',obj%fstat(4)
      write (obj%lun,*) trim(obj%prefix), &
                  ' maximum Y-difference of all moving gathers: ',obj%fstat(5)
      write (obj%lun,*) trim(obj%prefix), &
                           ' average minimum Y-difference: ',obj%fstat(8)/n
      write (obj%lun,*) trim(obj%prefix), &
                           ' average maximum Y-difference: ',obj%fstat(9)/n
      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                           ' maximum number of traces put to disk: ',obj%nrec
      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                           ' bin numbers of traces last residing',  &
                           ' at beginning of disk file:'

      do i = 1,min(obj%nrec,10)
           write (obj%lun,*) trim(obj%prefix),' record number: ',i,  &
                           '   xbin: ',obj%kxbin(i),'   ybin: ',obj%kybin(i)
      end do

      write (obj%lun,*) ' '
      write (obj%lun,*) trim(obj%prefix), &
                          ' bin numbers of traces last residing',  &
                          ' at end of disk file:'

      do i = max(obj%nrec-9,1),obj%nrec
           write (obj%lun,*) trim(obj%prefix),' record number: ',i,  &
                           '   xbin: ',obj%kxbin(i),'   ybin: ',obj%kybin(i)
      end do

      write (obj%lun,*) ' '
      return
      end subroutine mgather3d_statistics


!!--------------------------- part print -----------------------------------!!
!!--------------------------- part print -----------------------------------!!
!!--------------------------- part print -----------------------------------!!

! one-line summary printout of selected gathers.


      subroutine mgather3d_part_print (obj,dxmin,dxmax,dymin,dymax,used)
      implicit none
      type(mgather3d_struct),intent(in) :: obj                      ! arguments
      real                  ,intent(in) :: dxmin,dxmax,dymin,dymax  ! arguments
      integer               ,intent(in) :: used                     ! arguments
      integer                           :: itrace                   ! local
      real                              :: xcoord,ycoord            ! local

      if (obj%lun <= 0) return

      itrace = nint(obj%hdgather(1,obj%midx,obj%midy))

      if ( itrace < 5                                            .or.    &
          (itrace < 1000   .and. itrace == 200  *(itrace/200  )) .or.    &
          (itrace < 10000  .and. itrace == 2000 *(itrace/2000 )) .or.    &
          (itrace < 100000 .and. itrace == 20000*(itrace/20000)) ) then

             xcoord = obj%hdgather(obj%nhx,obj%midx,obj%midy)
             ycoord = obj%hdgather(obj%nhy,obj%midx,obj%midy)

             write (obj%lun,2000) trim(obj%prefix), &
                           itrace,dxmin,xcoord,dxmax,dymin,ycoord,dymax,used
2000         format (' ',a,' trace=',i7,                                &
                     '  dxmin=',f9.1,'  xcoord=',f9.1,'  dxmax=',f9.1,  &
                     '  dymin=',f9.1,'  ycoord=',f9.1,'  dymax=',f9.1,  &
                     '  used=',i4)
      end if
      return
      end subroutine mgather3d_part_print


!!--------------------------- full print -----------------------------------!!
!!--------------------------- full print -----------------------------------!!
!!--------------------------- full print -----------------------------------!!

! full printout of selected gathers.
! if message is 'FATAL...': always prints out the entire gather.
! if message is blank     : prints selected gathers regardless of DEBUG.
! if message is not blank : prints selected gathers only if DEBUG is true.
! if DEBUG is false, only a small subset of the gather is printed.


      subroutine mgather3d_full_print (obj,message)
      implicit none
      type(mgather3d_struct),intent(in) :: obj                    ! arguments
      character(len=*)      ,intent(in) :: message                ! arguments
      integer                           :: ix,iy,itrace           ! local
      real                              :: xcoord,ycoord          ! local
      real                              :: dxcoord,dycoord        ! local
      integer                           :: xbin,ybin              ! local
      integer                           :: dxbin,dybin            ! local
      character(len=4)                  :: from                   ! local
      character(len=6)                  :: arrow                  ! local
      logical,parameter                 :: DEBUG = .false.        ! local
!!!   logical,parameter                 :: DEBUG = .true.         ! local
      logical                           :: whoops                 ! local

      if (obj%lun <= 0) return

      if (len(message) < 5) then
           whoops = .false.
      else
           whoops = (message(1:5) == 'FATAL')
      end if

      if (message /= ' ' .and. .not.whoops .and. .not.DEBUG) return

      itrace = nint(obj%hdgather(1,obj%midx,obj%midy))

      if (itrace /= 1 .and. itrace /= 451 .and. itrace /= 851 .and. &
          .not.whoops) return

      xcoord =      obj%hdgather(obj%nhx,obj%midx,obj%midy)
      ycoord =      obj%hdgather(obj%nhy,obj%midx,obj%midy)
      xbin   = nint(obj%hdgather(obj%nsx,obj%midx,obj%midy))
      ybin   = nint(obj%hdgather(obj%nsy,obj%midx,obj%midy))

      write (obj%lun,*) ' '

      if (message /= ' ') write (obj%lun,*) trim(obj%prefix),' ',message

      write (obj%lun,*) trim(obj%prefix), &
                          ' moving gather centered on trace number ',itrace,':'
      write (obj%lun,1000) trim(obj%prefix)

      do iy = 1,obj%nygather
      do ix = 1,obj%nxgather
           if (.not.whoops .and. .not.DEBUG) then
             if (iy > 1 .and. iy < obj%nygather .and. iy /= obj%midy) cycle
             if (ix > 1 .and. ix < obj%nxgather .and. ix /= obj%midx) cycle
           end if

           dxcoord =      obj%hdgather(obj%nhx,ix,iy)  - xcoord
           dycoord =      obj%hdgather(obj%nhy,ix,iy)  - ycoord
           dxbin   = nint(obj%hdgather(obj%nsx,ix,iy)) - xbin
           dybin   = nint(obj%hdgather(obj%nsy,ix,iy)) - ybin
           arrow   = ' '
           from    = '----'

           if (ix == obj%midx .and. iy == obj%midy) arrow = '<--mid'

           if (obj%hdgather(obj%nsf,ix,iy) == 1.0) from = 'prev'
           if (obj%hdgather(obj%nsf,ix,iy) == 2.0) from = 'DISK'

           write (obj%lun,2000)                                        &
                      trim(obj%prefix),ix,iy,                          &
                 nint(obj%hdgather(      1,ix,iy)),                    &
                      obj%hdgather(obj%nsw,ix,iy) ,from,               &
                      obj%hdgather(obj%nhx,ix,iy) ,                    &
                      obj%hdgather(obj%nhy,ix,iy) ,dxcoord,dycoord,    &
                 nint(obj%hdgather(obj%nsx,ix,iy)),                    &
                 nint(obj%hdgather(obj%nsy,ix,iy)),dxbin,dybin,arrow
      end do
      end do

      write (obj%lun,*) ' '
1000  format (' ',a,'   ix  iy  trace weight from',           &
                   '   xcoord   ycoord  dxcoord  dycoord',    &
                   '     xbin     ybin    dxbin    dybin')
2000  format (' ',a,' ',2i4,i7,1x,f6.1,1x,a4,4f9.1,4i9,1x,a6)
      return
      end subroutine mgather3d_full_print


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>

      end module mgather3d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

