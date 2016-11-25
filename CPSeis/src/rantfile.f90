
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- rantfile.f90 ------------------------------!!
!!---------------------------- rantfile.f90 ------------------------------!!
!!---------------------------- rantfile.f90 ------------------------------!!

 
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
! Name       : RANTFILE
! Category   : io
! Written    : 2004-05-11   by: Tom Stoeckley
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Randomly read and interpolate TRCIO files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to randomly read seismic traces from a TRCIO
! file by requesting traces by (X,Y) coordinates.
!
! Currently the following restrictions apply:
!
!  (1) Traces must be requested in the order they reside on the file.
!  (2) Traces matching the requested coordinates must reside on the file.
!  (3) Coordinates match if their rounded values match.
!
! Eventually, the above restrictions may be removed, allowing traces to be
! requested in any order and allowing interpolation to provide traces at
! locations not in the file.
!
! This primitive uses the PERMTFILE primitive.
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
!    call rantfile_create
!        (obj, pathname, nwih, ndpt, tstrt, dt, nhx, nhy, lunprint, error, msg)
!          o      i       i     i      i    i    i    i      i        o     o
!
!                            b
!    call rantfile_delete  (obj)
!                                                                      opt
!                            b     i       i     o   o     o     o      o
!    call rantfile_find    (obj, xcoord, ycoord, hd, tr, error, msg, changed)
!
!
! type(rantfile_struct)    obj = pointer to RANTFILE structure.
! character(len=*)    pathname = file name to read.
! integer                 nwih = number of words of HD.
! integer                 ndpt = number of words of TR.
! real                   tstrt = starting time of TR.
! real                      dt = sample interval of TR.
! integer                  nhx = header word number containing X coordinate.
! integer                  nhy = header word number containing Y coordinate.
! integer             lunprint = logical unit number for printing.
! logical                error = error flag (true if an error occurred).
! character(len=*)         msg = message for possible printing.
! logical              changed = true if the returned trace has changed.
! integer               xcoord = X coordinate of trace.
! integer               ycoord = Y coordinate of trace.
! double precision    hd(nwih) = header word array.
! real                tr(ndpt) = trace array.
!
! If an error occurs on a read, the HD and TR arrays will be filled with zeroes.
!
! When reading a file, NWIH, NDPT, and TSTRT need not match the traces on
! the file.  The returned trace and header will be adjusted appropriately by
! extending with zeroes or truncating.  But if DT does not match, an error
! is generated.
!
! If OBJ is not associated when calling RANTFILE_DELETE, nothing is done.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
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


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module rantfile_module
      use named_constants_module
      use permtfile_module
      implicit none
      public

      character(len=100),public,save :: rantfile_IDENT = &
'$Id: rantfile.f90,v 1.1 2005/01/31 14:09:12 Stoeckley prod sps $'

      type,public :: rantfile_struct
           private
           type(permtfile_struct),pointer :: permtfile
           integer                        :: nwih,ndpt
           integer                        :: nhx,nhy
           integer                        :: irec
           double precision      ,pointer :: hd(:)
           real                  ,pointer :: tr(:)
      end type rantfile_struct

      integer,public,parameter :: RANTFILE_OK    = PERMTFILE_OK
      integer,public,parameter :: RANTFILE_ERROR = PERMTFILE_ERROR

      contains


!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!


      subroutine rantfile_create &
                   (obj,pathname,nwih,ndpt,tstrt,dt,nhx,nhy,lunprint,error,msg)

      type(rantfile_struct),pointer     :: obj                   ! arguments
      character(len=*)     ,intent(in)  :: pathname              ! arguments
      integer              ,intent(in)  :: nwih,ndpt             ! arguments
      real                 ,intent(in)  :: tstrt,dt              ! arguments
      integer              ,intent(in)  :: nhx,nhy               ! arguments
      integer              ,intent(in)  :: lunprint              ! arguments
      logical              ,intent(out) :: error                 ! arguments
      character(len=*)     ,intent(out) :: msg                   ! arguments
      integer                           :: err                   ! local

      allocate (obj)
      nullify  (obj%permtfile)
      nullify  (obj%hd)
      nullify  (obj%tr)
      obj%nhx      = nhx
      obj%nhy      = nhy
      obj%nwih     = nwih
      obj%ndpt     = ndpt
      obj%irec     = 0

      call permtfile_open_read &
                    (obj%permtfile,pathname,nwih,ndpt,tstrt,dt,lunprint,err)

      if (err /= PERMTFILE_OK) then
           error = .true.
           msg = 'error opening trace file'
           return
      end if

      allocate (obj%hd(obj%nwih))
      allocate (obj%tr(obj%ndpt))

      obj%hd(:) = 0.0
      obj%tr(:) = 0.0

      error = .false.
      msg = 'OK'

      end subroutine rantfile_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine rantfile_delete (obj)

      type(rantfile_struct),pointer :: obj                 ! arguments

      if (.not.associated(obj)) return

      call permtfile_close (obj%permtfile)

      if (associated(obj%hd)) deallocate (obj%hd)
      if (associated(obj%tr)) deallocate (obj%tr)

      deallocate (obj)

      end subroutine rantfile_delete


!!------------------------------ find -----------------------------------!!
!!------------------------------ find -----------------------------------!!
!!------------------------------ find -----------------------------------!!


      subroutine rantfile_find (obj,xcoord,ycoord,hd,tr,error,msg,changed)

      type(rantfile_struct),intent(inout) :: obj                   ! arguments
      real                 ,intent(in)    :: xcoord,ycoord         ! arguments
      double precision     ,intent(out)   :: hd(:)                 ! arguments
      real                 ,intent(out)   :: tr(:)                 ! arguments
      logical              ,intent(out)   :: error                 ! arguments
      character(len=*)     ,intent(out)   :: msg                   ! arguments
      logical    ,optional ,intent(out)   :: changed               ! arguments
      integer                             :: err                   ! local

      if (present(changed)) changed = .false.

      do

        if (obj%irec > 0                          .and. &
            nint(xcoord) == nint(obj%hd(obj%nhx)) .and. &
            nint(ycoord) == nint(obj%hd(obj%nhy))) then
             error = .false.
             msg = 'OK'
             exit
        end if

        if (present(changed)) changed = .true.

        obj%irec = obj%irec + 1
        call permtfile_read (obj%permtfile,obj%hd,obj%tr,err,obj%irec)

        if (err /= PERMTFILE_OK) then
             error = .true.
             if (err == PERMTFILE_EOF) then
               write (msg,*) 'EOF reading record ',obj%irec, &
                             ' xcoord = ',xcoord,' ycoord = ',ycoord
             else
               write (msg,*) 'ERROR reading record ',obj%irec, &
                             ' xcoord = ',xcoord,' ycoord = ',ycoord
             end if
             exit
        end if

      end do

      hd(1:obj%nwih) = obj%hd(1:obj%nwih)
      tr(1:obj%ndpt) = obj%tr(1:obj%ndpt)

      end subroutine rantfile_find


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module rantfile_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

