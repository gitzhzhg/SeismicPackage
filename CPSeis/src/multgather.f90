!<CPS_v1 type="PRIMITIVE"/>
!!------------------------- multgather.f90 -------------------------------!!
!!------------------------- multgather.f90 -------------------------------!!
!!------------------------- multgather.f90 -------------------------------!!


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
!                         C P S   P R O C E S S             
!
! Name       : MULTGATHER   (a set of Multiple Moving Gathers)
! Category   : math
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2005-01-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : a set of several similar 3D moving gathers of traces.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive allows a 3D process module to get a set of 3D moving gathers
! of traces which roll along one trace at a time.  It is for use by time-domain
! multi-channel operations such as trace mixing or base trace building.
!
! This primitive is simply a collection of several instances of the MGATHER3D
! primitive.
!
! In this primitive, each input trace is passed to a separate instance of
! MGATHER3D.  When the MGATHER3D objects return trace gathers, all of these
! trace gathers are returned by MULTGATHER in a 3-dimensional array.
!
! The number of input traces must be the same on every call to MULTGATHER.
!
! The following MGATHER3D arguments are set by MULTGATHER instead of being
! passed to MULTGATHER by the calling program:
!
!                  XWEIGHTS(NXGATHER)   (assumed to be all set to one)
!                  YWEIGHTS(NYGATHER)   (assumed to be all set to one)
!                  NSX                  (set to scratch header 58)
!                  NSY                  (set to scratch header 59)
!                  NSW                  (set to scratch header 60)
!                  NSF                  (set to scratch header 61)
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
!     o                              i    i    i      i        i       i
!   NSTORE   = MULTGATHER_STORE   (nsets,nwih,ndpt,nxgather,nygather,nxbins)
!   NSCRATCH = MULTGATHER_SCRATCH (nsets,nwih,ndpt,nxgather,nygather)
!
!                            o    i     i     i     i     i    i   i   i 
!   CALL MULTGATHER_CREATE (OBJ, LUN, nsets, NWIH, NDPT, NHX, NHY, X1, Y1,
!                           XINC,YINC,NXGATHER,NYGATHER,NXBINS,PROGNAME,whoops)
!                            i    i      i        i       i       i       o
!
!                            b    b    i    i    o    o
!   CALL MULTGATHER        (OBJ, NTR, HDI, TRI, HDO, TRO)
!
!   CALL MULTGATHER_DELETE (OBJ)
!                            b
!
! integer               NSTORE = permanent storage used by MULTGATHER.
! integer             NSCRATCH = scratch storage used by MULTGATHER.
! type(multgather_struct)  OBJ = pointer to the MULTGATHER data structure.
! integer                nsets = number of input traces (> 0).
! integer                  NTR = number of input and output traces.
! double       HDI(NWIH,nsets) = input headers.
! real         TRI(NDPT,nsets) = input traces.
! integer                  LUN = logical unit number for printing (> zero).
! integer                  NHX = header word containing X coordinate.
! integer                  NHY = header word containing Y coordinate.
! real                      X1 = center of any bin in header word NHX.
! real                      Y1 = center of any bin in header word NHY.
! real                    XINC = bin width (increment) of coords in hwd NHX.
! real                    YINC = bin width (increment) of coords in hwd NHY.
! integer             NXGATHER = # traces in X dir in moving gather (odd).
! integer             NYGATHER = # traces in Y dir in moving gather (odd).
! integer               NXBINS = maximum number of traces (bins) in a line.
! character(len=*)    PROGNAME = program or process name for printing.
! logical               WHOOPS = true if an error occurred.
! double HDO(NWIH,NXGATHER*NYGATHER,nsets) = headers of moving gather (output).
! real   TRO(NDPT,NXGATHER*NYGATHER,nsets) = traces of moving gather (output).
!
! The first, second, and third dimensions of the arrays must not be smaller
! than the sizes shown, but may exceed the sizes shown.
!
! NSETS is the number of MGATHER3D objects which will be used.
! XINC and YINC must be > zero.
!
! The input value of NTR must be the number of traces in HDI and TRI and
! must be equal to NSETS (unless it is set to NEED_TRACES or NO_MORE_TRACES).
!
! The output value of NTR will always be set to NSETS (unless it is set
! to NEED_TRACES or NO_MORE_TRACES or FATAL_ERROR).
!
! Since HDO and TRO are copies of traces maintained internally, they can be
! changed without affecting the traces returned in the next call to MULTGATHER.
!
! MULTGATHER_DELETE does nothing if OBJ is not associated.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!  1. Each call to this routine outputs a set of moving gathers such that
!     each gather corresponds to one input trace.  Each gather will
!     always contain exactly NXGATHER * NYGATHER traces arranged in a
!     regularized XY grid.  The input trace associated with each gather
!     will always be in the center of this grid (and in the center
!     of the gather).  Any missing input traces will be replaced by
!     dead traces.  If two or more input traces fall into the same
!     bin, only one of the traces in that bin will be used.
!
!  2. This primitive uses temporary disk files to store traces.
!
!  3. If NXGATHER and NYGATHER are both 1, the MGATHER3D primitive is not
!     used and MULTGATHER simply passes the input traces to the output traces.
!
!-------------------------------------------------------------------------------
!                            EXAMPLE OF USE
!
!  In the DATA STRUCTURE of a process module:
!
!        type(multgather_struct),pointer :: multgather
!
!  In the CREATE ROUTINE of a process module:
!
!        nullify (obj%multgather)
!
!  In the UPDATE ROUTINE of a process module:
!
!        call multgather_delete (obj%multgather)     ! in case already exists.
!
!        call multgather_create (obj%multgather,lun,nwih,ndpt,nhx,nhy,  &
!                                x1,y1,xinc,yinc,nxgather,nygather,     &
!                                nxbins,progname,whoops)
!
!  In the WRAPUP ROUTINE of a process module:
!
!        call multgather_delete (obj%multgather)
!
!  In the TRACE PROCESSING ROUTINE of a process module, where NTR, HDI,
!  and TRI are the input arguments in the process module routine:
!
!        call multgather (obj%multgather,ntr,hdi,tri,hdo,tro)
!
!        if (ntr == NEED_TRACES) return       ! go back for more traces.
!        if (ntr == NO_MORE_TRACES .or. &     ! we are finished.
!            ntr == FATAL_ERROR) then         ! a fatal I/O error has occurred.
!             call xxxx_wrapup (obj)
!             return
!        end if
!
!     !!! Now NTR will be equal to NSETS.
!     !!! Use the moving gathers HDO(NWIH,nxgather*nygather,NSETS)
!     !!!                    and TRO(NDPT,nxgather*nygather,NSETS)
!     !!!                    to do your calculations.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-01-10  Stoeckley  Initial version.
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


      module multgather_module
      use named_constants_module
      use mgather3d_module
      use string_module
      implicit none
      private
      public :: multgather_store
      public :: multgather_scratch
      public :: multgather_create
      public :: multgather_delete
      public :: multgather

      character(len=100),public,save :: MULTGATHER_IDENT = &
'$Id: multgather.f90,v 1.1 2005/01/10 14:12:17 Stoeckley prod sps $'


!!------------------------------ data ------------------------------------!!
!!------------------------------ data ------------------------------------!!
!!------------------------------ data ------------------------------------!!


      type,private :: mgather3d_holder
        private
        type(mgather3d_struct),pointer :: mgather3d
      end type mgather3d_holder


      type,public :: multgather_struct

        private
        integer                        :: lun
        integer                        :: nsets
        character(len=30)              :: progname
        logical                        :: bypass
        type(mgather3d_holder),pointer :: holder(:)

      end type multgather_struct


      integer,parameter,private :: SCRATCH_NSX = HDR_SCRATCH_58
      integer,parameter,private :: SCRATCH_NSY = HDR_SCRATCH_59
      integer,parameter,private :: SCRATCH_NSW = HDR_SCRATCH_60
      integer,parameter,private :: SCRATCH_NSF = HDR_SCRATCH_61


      contains


!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!


      function multgather_store &
                    (nsets,nwih,ndpt,nxgather,nygather,nxbins) result (nstore)
      implicit none
      integer,intent(in)  :: nsets,nwih,ndpt,nxgather,nygather    ! arguments
      integer,intent(in)  :: nxbins                               ! arguments
      integer             :: nstore                               ! result

      if (nxgather == 1 .and. nygather == 1) then
           nstore = 0
           return
      end if

      nstore = nsets * mgather3d_store (nwih,ndpt,nxgather,nygather,nxbins)
      return
      end function multgather_store



      function multgather_scratch &
                    (nsets,nwih,ndpt,nxgather,nygather) result (nscratch)
      implicit none
      integer,intent(in)  :: nsets,nwih,ndpt,nxgather,nygather    ! arguments
      integer             :: nscratch                             ! result

      if (nxgather == 1 .and. nygather == 1) then
           nscratch = 0
           return
      end if

      nscratch = nsets * mgather3d_scratch (nwih,ndpt,nxgather,nygather)
      return
      end function multgather_scratch


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine multgather_create (obj,lun,nsets,nwih,ndpt,nhx,nhy,   &
                                   x1,y1,xinc,yinc,nxgather,nygather,  &
                                   nxbins,progname,whoops)
      implicit none
      type(multgather_struct),pointer    :: obj                     ! arguments
      integer               ,intent(in)  :: lun,nsets,nwih,ndpt     ! arguments
      integer               ,intent(in)  :: nhx,nhy                 ! arguments
      real                  ,intent(in)  :: x1,y1,xinc,yinc         ! arguments
      integer               ,intent(in)  :: nxgather,nygather       ! arguments
      integer               ,intent(in)  :: nxbins                  ! arguments
      character(len=*)      ,intent(in)  :: progname                ! arguments
      logical               ,intent(out) :: whoops                  ! arguments
      integer                            :: iset,lun2               ! local
      real                               :: xweights(nxgather)      ! local
      real                               :: yweights(nygather)      ! local
      character(len=50)                  :: progname2               ! local

      allocate (obj)

      obj%lun      = lun
      obj%nsets    = nsets
      obj%progname = progname
      obj%bypass   = (nxgather == 1 .and. nygather == 1)

      nullify (obj%holder)

      if (obj%nsets <= 0) then
           write(obj%lun,*) 'error creating multgather with NSETS <= 0'
           whoops = .true.
           return
      end if

      if (obj%bypass) then
           whoops = .false.
           return
      end if

      allocate (obj%holder(obj%nsets))

      xweights(:) = 1.0
      yweights(:) = 1.0

      do iset = 1,obj%nsets
           if (iset == obj%nsets) then
                lun2 = lun
           else
                lun2 = 0
           end if
           progname2 = trim(progname)//string_ii2ss(iset)
           call mgather3d_create (obj%holder(iset)%mgather3d,         &
                                  lun2,nwih,ndpt,nhx,nhy,             &
                                  x1,y1,xinc,yinc,nxgather,nygather,  &
                                  xweights, yweights,                 &
                                  SCRATCH_NSX, SCRATCH_NSY,           &
                                  SCRATCH_NSW, SCRATCH_NSF,           &
                                  nxbins,progname2)
           if (.not.associated(obj%holder(iset)%mgather3d)) then
                write(obj%lun,*) 'error creating mgather3d object ',iset
                whoops = .true.
                return
           end if
      end do

      whoops = .false.
      end subroutine multgather_create


!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!


      subroutine multgather_delete (obj)
      type(multgather_struct),pointer :: obj                  ! arguments
      integer                         :: iset                 ! local

      if (.not.associated(obj)) return
      if (.not.associated(obj%holder)) return

      do iset = 1,obj%nsets
           call mgather3d_delete (obj%holder(iset)%mgather3d)
      end do

      deallocate (obj%holder)
      deallocate (obj)
      end subroutine multgather_delete


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine multgather (obj,ntr,hdi,tri,hdo,tro)
      type(multgather_struct),intent(inout) :: obj               ! arguments
      integer                ,intent(inout) :: ntr               ! arguments
      double precision       ,intent(inout) :: hdi(:,:)          ! arguments
      real                   ,intent(in)    :: tri(:,:)          ! arguments
      double precision       ,intent(out)   :: hdo(:,:,:)        ! arguments
      real                   ,intent(out)   :: tro(:,:,:)        ! arguments
      integer                               :: iset              ! local
      integer                               :: nmove(obj%nsets)  ! local

!!!!!!!!!!!!!!! initialize NMOVE and check for input errors:

      if (ntr == obj%nsets) then
           nmove(:) = 1
      else if (ntr > 0) then
           write(obj%lun,*) trim(obj%progname), &
                             ': NUMBER OF INPUT TRACES IS ',ntr
           write(obj%lun,*) trim(obj%progname), &
                             ': NUMBER OF INPUT TRACES SHOULD BE ',obj%nsets
           ntr = FATAL_ERROR
           return
      else
           nmove(:) = ntr
      end if

!!!!!!!!!!!!!!! shortcut if nxgather and nygather are one:

      if (obj%bypass) then
           if (ntr == obj%nsets) then
                hdo(:,1,1:ntr) = hdi(:,1:ntr)
                tro(:,1,1:ntr) = tri(:,1:ntr)
           end if
           return
      end if

!!!!!!!!!!!!!!! process each input trace:

      do iset = 1,obj%nsets
           call mgather3d (obj%holder(iset)%mgather3d,nmove(iset),  &
                           hdi(:,iset:iset),tri(:,iset:iset),       &
                           hdo(:,:,iset),tro(:,:,iset))

      end do

!!!!!!!!!!!!!!! check for output inconsistencies:

      do iset = 2,obj%nsets
           if (nmove(iset) /= nmove(1)) then
                write(obj%lun,*) trim(obj%progname), &
                               ': INCONSISTENT OUTPUTS FROM MGATHER3D'
                write(obj%lun,*) trim(obj%progname), &
                               ': NMOVE for index', 1  ,'is',nmove(1)
                write(obj%lun,*) trim(obj%progname), &
                               ': NMOVE for index',iset,'is',nmove(iset)
                ntr = FATAL_ERROR
                return
           end if
      end do

!!!!!!!!!!!!!!! finish up and return:

      if (nmove(1) > 0) then
           ntr = obj%nsets
      else
           ntr = nmove(1)
      end if
      end subroutine multgather


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module multgather_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

