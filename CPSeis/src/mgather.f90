!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- mgather.f90 --------------------------------!!
!!----------------------------- mgather.f90 --------------------------------!!
!!----------------------------- mgather.f90 --------------------------------!!


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
! Name       : MGATHER    (Moving Gather)
! Category   : math
! Written    : 1988-10-18   by: Tom Stoeckley
! Revised    : 2002-09-03   by: Tom Stoeckley
! Maturity   : production   2002-10-10
! Purpose    : Moving gather of traces which rolls along one trace at a time.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive allows a process module to get a moving gather of traces
! which rolls along one trace at a time.  It is for use by time-domain
! multi-channel operations such as trace mixing or base trace building.
! SDIP is an example of a process module which uses this primitive.
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
!    o                       i     i       i  
!  NSTORE = MGATHER_STORE  (nwih, ndpt, ngather)
!
!                            o    i     i       i      i   i    i
!  CALL MGATHER_CREATE     (OBJ, NWIH, NDPT, NGATHER, NHY, Y1, YINC,
!                                WEIGHTS, NSX, NSY, NSW)
!                                   i      i    i    i
!
!                            b    b    i    i    o    o
!  CALL MGATHER            (OBJ, NTR, HDI, TRI, HDO, TRO)
!
!  CALL MGATHER_DELETE     (OBJ)
!                            b
!
! integer               NSTORE = permanent storage used by MGATHER.
! type(mgather_struct)     OBJ = pointer to the MGATHER data structure.
! integer                  NTR = number of input and output traces.
! double         HDI(NWIH,NTR) = input headers.
! real           TRI(NDPT,NTR) = input traces.
! integer              NGATHER = number of traces in moving gather (odd).
! integer                  NHY = header word containing line identifier.
! real                      Y1 = bin center of any line in header word NHY.
! real                    YINC = bin width (increment) of lines in hwd NHY.
! real        WEIGHTS(NGATHER) = weights to place into header word NSW.
! integer                  NSX = scratch header word to receive the X bin coord.
! integer                  NSY = scratch header word to receive the Y bin coord.
! integer                  NSW = scratch header word to receive the weight.
! double     HDO(NWIH,NGATHER) = headers of moving gather (output).
! real       TRO(NDPT,NGATHER) = traces of moving gather (output).
!
! The first and second dimensions of the arrays must not be smaller than
! the sizes shown, but may exceed the sizes shown.
!
! The WEIGHTS can be any numbers >= zero.  They are set into the specified
! header word NSW in each trace in the moving gather.
!
! The NSX header words in the moving gather will be set to consecutive
! integers (from negative to positive) such that the middle trace will
! receive a value of zero.
!
! The NSY header words in the moving gather will all be set to the bin number
! calculated from NHY and Y1 and YINC.  YINC must be > zero.
!
! The input value of NTR must be the number of traces in HDI and TRI
!  (unless it is set to NEED_TRACES or NO_MORE_TRACES).
!
! The output value of NTR will always be set to NGATHER
!  (unless it is set to NEED_TRACES or NO_MORE_TRACES).
!
! Since HDO and TRO are copies of traces maintained internally, they can be
! changed without affecting the traces returned in the next call to MGATHER.
!
! MGATHER_DELETE does nothing if OBJ is not associated.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!  1. Each call to this routine outputs a moving gather such that
!     each gather corresponds to one input trace.  The gather will
!     always contain exactly NGATHER traces.  The input trace associated
!     with the gather will always be in the center of the gather.
!     The most recent trace is always at the end of the gather.
!     The number of input traces in the gather grows, two traces at
!     a time (always odd), until it reaches size NGATHER (also odd).
!     Then it stays at that size (adding one new trace and dropping
!     the oldest trace each time) until no input traces are left.
!     Then it shrinks two input traces at a time down to zero.
!     The output gather will begin and end with dead traces if the
!     number of input traces in the gather is less than NGATHER; this
!     occurs only at the beginning and end of a line.
!
!  2. If there are two or more lines, each line is treated as an
!     independent data set, so that no gathers contain traces from
!     more than one line.
!
!-------------------------------------------------------------------------------
!                            EXAMPLE OF USE
!             
!  In the DATA STRUCTURE of a process module:
!
!        type(mgather_struct),pointer :: mgather
!
!  In the CREATE ROUTINE of a process module:
!
!        nullify (obj%mgather)
!
!  In the UPDATE ROUTINE of a process module:
!
!        call mgather_delete (obj%mgather)     ! in case already exists.
!
!        call mgather_create (obj%mgather,nwih,ndpt,nhy,y1,yinc,ngather, &
!                                   weights,nsx,nsy,nsw)
!
!  In the WRAPUP ROUTINE of a process module:
!
!        call mgather_delete (obj%mgather)
!
!  In the TRACE PROCESSING ROUTINE of a process module, where NTR, HDI,
!  and TRI are the input arguments in the process module routine:
!
!        call mgather (obj%mgather,ntr,hdi,tri,hdo,tro)
!
!        if (ntr == NEED_TRACES) return         ! go back for more traces.
!        if (ntr == NO_MORE_TRACES) then        ! we are finished.
!             call xxxx_wrapup (obj)
!             return
!        end if
!
!     !!! Now NTR will be equal to NGATHER.
!     !!! Use the moving gather to calculate one or more output traces
!     !!! from NTR traces stored in HDO and TRO.
!
!        ntr = 1                    ! this is the number of output traces.
!        return
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  7. 2002-10-10  Stoeckley    Change to use the MTH module for binning.
!  6. 2001-12-11  Stoeckley    Convert to a creatable/deletable object which
!                               internally stores the moving gather; this
!                               change was made to allow the output gathers
!                               to be changed by the calling process without
!                               adverse effect, and to make the output gathers
!                               all the same length and the traces in the same
!                               order as the input traces.
!  5. 2000-10-06  Stoeckley    Add missing required documentation section.
!  4. 2000-04-25  Stoeckley    Converted from old system; fixed a minor
!                               12-year-old bug.
!  3. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  2. 1989-05-01  Stoeckley    Fix bug which occurred when GETG is called
!                               and NDPT had been changed.
!  1. 1988-10-18  Stoeckley    Initial version.
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


      module mgather_module
      use named_constants_module
      use mth_module
      implicit none
      public
      private :: mgather_private_execute

      character(len=100),public,save :: MGATHER_IDENT = &
       '$Id: mgather.f90,v 1.7 2002/10/09 13:19:54 Stoeckley prod sps $'


      type,public :: mgather_struct              

         private
         integer                  :: nwih,ndpt            ! input parameters
         integer                  :: ngather,nhy          ! input parameters
         double precision         :: y1,yinc              ! input parameters
         integer                  :: nsx,nsy,nsw          ! input parameters
         integer                  :: number,ikeep,nkeep   ! dependent
         integer                  :: mid                  ! dependent

         real            ,pointer :: weights(:)           ! input parameters
         double precision,pointer :: hdgather(:,:)        ! dependent
         real            ,pointer :: trgather(:,:)        ! dependent

      end type mgather_struct


      contains


!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!
!!----------------------------- storage ------------------------------------!!


      function mgather_store (nwih,ndpt,ngather) result (nstore)
      implicit none
      integer,intent(in)  :: nwih,ndpt,ngather                    ! arguments
      integer             :: nstore                               ! result

      nstore = (nwih + ndpt + 1) * ngather
      return
      end function mgather_store



      function mgather_scratch () result (nscratch)
      implicit none
      integer             :: nscratch                             ! result

      nscratch = 0
      return
      end function mgather_scratch


!!--------------------------------- create --------------------------------!!
!!--------------------------------- create --------------------------------!!
!!--------------------------------- create --------------------------------!!


      subroutine mgather_create (obj,nwih,ndpt,ngather,nhy,y1,yinc,  &
                                 weights,nsx,nsy,nsw)
      implicit none
      type(mgather_struct),pointer     :: obj                  ! arguments
      integer             ,intent(in)  :: nwih,ndpt            ! arguments
      integer             ,intent(in)  :: ngather,nhy          ! arguments
      real                ,intent(in)  :: y1,yinc              ! arguments
      real                ,intent(in)  :: weights(:)           ! arguments
      integer             ,intent(in)  :: nsx,nsy,nsw          ! arguments

      allocate (obj)

      obj%nwih    = nwih
      obj%ndpt    = ndpt
      obj%ngather = ngather
      obj%nhy     = nhy
      obj%y1      = y1
      obj%yinc    = yinc
      obj%nsx     = nsx
      obj%nsy     = nsy
      obj%nsw     = nsw
      obj%number  = 0
      obj%ikeep   = 2
      obj%nkeep   = 1
      obj%mid     = (obj%ngather + 1) / 2

      allocate (obj%weights      (ngather))
      allocate (obj%hdgather(nwih,ngather))
      allocate (obj%trgather(ndpt,ngather))

      obj%weights(:) = weights(1:ngather)
      return
      end subroutine mgather_create


!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!
!!--------------------------------- delete --------------------------------!!


      subroutine mgather_delete (obj)
      implicit none
      type(mgather_struct),pointer     :: obj                  ! arguments

      if (.not.associated(obj)) return

      deallocate (obj%weights )
      deallocate (obj%hdgather)
      deallocate (obj%trgather)

      deallocate (obj)
      return
      end subroutine mgather_delete


!!------------------------- mgather private execute -----------------------!!
!!------------------------- mgather private execute -----------------------!!
!!------------------------- mgather private execute -----------------------!!

! Note that some code (commented below) was changed from old CPS.
! Old CPS always decremented by 2, which meant that NTR was sometimes
! returned as an even number when there were no more traces (or the line
! number changed) before the output arrays were filled.  This probably
! happened only when the number of traces (or the number in one line)
! was less than NGATHER.  But in such a case the total number of traces
! processed could be one less than the total number input.


      subroutine mgather_private_execute (obj,ntr,  hdi,tri)
      implicit none
      type(mgather_struct),intent(inout) :: obj               ! arguments
      integer             ,intent(inout) :: ntr               ! arguments
      double precision    ,intent(in)    :: hdi(:,:)          ! arguments
      real                ,intent(in)    :: tri(:,:)          ! arguments
      integer                            :: ibin,jbin,i,j     ! local

!----------WE ARE RECEIVING TRACES FROM ABOVE.

      if (obj%ikeep > obj%nkeep) then
           obj%nkeep = ntr
           obj%ikeep = 0
      end if

!----------THERE ARE NO MORE TRACES FROM ABOVE.

      if (obj%nkeep == 0) then
           if (obj%number == 2*(obj%number/2)) then    ! changed from oldcps
                obj%number = obj%number - 1            ! changed from oldcps
           else                                        ! changed from oldcps
                obj%number = obj%number - 2            ! changed from oldcps
           end if                                      ! changed from oldcps
           ntr = max(obj%number,NO_MORE_TRACES)
           return
      end if

!----------LOOK AT THE NEXT INPUT TRACE.

3333  obj%ikeep = obj%ikeep + 1
      if (obj%ikeep > obj%nkeep) then
           ntr = NEED_TRACES
           return
      end if

!----------CHECK LINE NUMBER OF NEXT TRACE.

      if (obj%nhy == 0 .or. obj%number == 0) go to 6666

      ibin = mth_bin_number (obj%y1, obj%yinc, hdi(obj%nhy,obj%ikeep))
      jbin = mth_bin_number (obj%y1, obj%yinc, obj%hdgather(obj%nhy,1))

      if (ibin == jbin) go to 6666

!----------THE NEXT TRACE BELONGS TO A DIFFERENT LINE.

      obj%ikeep  = obj%ikeep  - 1
      if (obj%number == 2*(obj%number/2)) then    ! changed from oldcps
           obj%number = obj%number - 1            ! changed from oldcps
      else                                        ! changed from oldcps
           obj%number = obj%number - 2            ! changed from oldcps
      end if                                      ! changed from oldcps
      if (obj%number > 0) then
            ntr = obj%number
            return
      end if
      obj%ikeep  = obj%ikeep + 1
      obj%number = 0

!----------MOVE ALL THE TRACES DOWN.

6666  if (obj%number == obj%ngather) obj%number = obj%number - 1
      if (obj%number > 0) then
           do i = obj%number,1,-1
                do j = 1,obj%nwih
                     obj%hdgather(j,i+1) = obj%hdgather(j,i)
                end do
                do j = 1,obj%ndpt
                     obj%trgather(j,i+1) = obj%trgather(j,i)
                end do
           end do
      end if

!----------ADD THE NEW TRACE TO THE MOVING GATHER.

      obj%number = obj%number + 1
      obj%hdgather(1:obj%nwih,1) = hdi(1:obj%nwih,obj%ikeep)
      obj%trgather(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%ikeep)
      if (obj%number == 2*(obj%number/2)) go to 3333
      ntr = obj%number
      return
      end subroutine mgather_private_execute


!!----------------------------- mgather -------------------------------!!
!!----------------------------- mgather -------------------------------!!
!!----------------------------- mgather -------------------------------!!


      subroutine mgather (obj,ntr,  hdi,tri,  hdo,tro)
      implicit none
      type(mgather_struct),intent(inout) :: obj                ! arguments
      integer             ,intent(inout) :: ntr                ! arguments
      double precision    ,intent(in)    :: hdi(:,:)           ! arguments
      real                ,intent(in)    :: tri(:,:)           ! arguments
      double precision    ,intent(out)   :: hdo(:,:)           ! arguments
      real                ,intent(out)   :: tro(:,:)           ! arguments
      integer                            :: little,i,ibin      ! local

!----------UPDATE THE INTERNAL MOVING GATHER.

      call mgather_private_execute (obj,ntr,  hdi,tri)

      if (ntr == NEED_TRACES .or. ntr == NO_MORE_TRACES) return

!----------INITIALIZE THE OUTPUT GATHER.

      if (ntr < obj%ngather) then
           hdo(1:obj%nwih,1:obj%ngather) = 0.0
           tro(1:obj%ndpt,1:obj%ngather) = 0.0
           hdo(         2,1:obj%ngather) = 1
           hdo(        64,1:obj%ngather) = obj%ndpt
      end if

!----------COPY THE INTERNAL MOVING GATHER TO THE OUTPUT GATHER.
!----------reverse the trace order since the internal gather is backward.

      little = (obj%ngather - ntr) / 2

      hdo(1:obj%nwih,1+little:ntr+little) = obj%hdgather(:,ntr:1:-1)
      tro(1:obj%ndpt,1+little:ntr+little) = obj%trgather(:,ntr:1:-1)

      ntr = obj%ngather

!----------ADD COORDINATES AND WEIGHTS TO THE OUTPUT GATHER.

      ibin = mth_bin_number (obj%y1, obj%yinc, hdo(obj%nhy,obj%mid))

      do i = 1,obj%ngather
           hdo(obj%nsx,i) = i - obj%mid
           hdo(obj%nsy,i) = ibin
           hdo(obj%nsw,i) = obj%weights(i)
      end do
      return
      end subroutine mgather


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module mgather_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

