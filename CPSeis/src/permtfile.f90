
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- permtfile.f90 ------------------------------!!
!!---------------------------- permtfile.f90 ------------------------------!!
!!---------------------------- permtfile.f90 ------------------------------!!

 
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
! Name       : PERMTFILE
! Category   : io
! Written    : 2000-05-22   by: Tom Stoeckley
! Revised    : 2006-11-14   by: Bill Menger
! Maturity   : production
! Purpose    : Read and write TRCIO files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to open, read, write, and close a sequential
! TRCIO file which stores seismic traces, or to read such a file randomly.
! This primitive also facilitates adjusting an input trace to the desired
! starting time and trace length when reading TRCIO files.
!
! This primitive is a wrapper around the TRCIO primitive.
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
! call permtfile_open_read
!       (obj,path,nwih,ndpt,tstrt,dt,lunprint,err,rhistory,ntraces)
!         o   i    i    i     i   i     i      o     i        o
!                                                   opt      opt
!
! call permtfile_open_write
!       (obj,path,nwih,ndpt,tstrt,dt,lunprint,err,ipn,nbits,maxrecords,whistory)
!         o   i    i    i     i   i     i      o   i    i       i         i
!                                                 opt  opt     opt       opt
!
!                             b
! call permtfile_close      (obj)
!
!                             b   b   i    o 
! call permtfile_write      (obj, hd, tr, err)
! call permtfile_read       (obj, hd, tr, err, irec)
!                             b   o   o    o    i
!                                              opt
!
!
! type(permtfile_struct)   obj = pointer to PERMTFILE structure.
! character(len=*)        path = file name to read or write.
! integer                 nwih = number of words of HD to read or write.
! integer                 ndpt = number of words of TR to read or write.
! real                   tstrt = starting time of TR to read or write.
! real                      dt = sample interval of TR to read or write.
! integer             lunprint = unit number for printing (or 0).
! integer                  err = PERMTFILE_ERROR (if error) or PERMTFILE_OK
!                                 or PERMTFILE_EOF (if endfile has been read).
! integer              ntraces = number of traces on input file.
! integer                  ipn = process number for history (default 0).
! integer                nbits = number of bits per trace sample to write.
! integer           maxrecords = estimated maximum number of traces to write.
! logical             rhistory = whether to read history cards (default true).
! character(len=*)    whistory = which history cards to write (see below).
! double precision    hd(nwih) = header word array.
! real                tr(ndpt) = trace array.
! integer                 irec = record number of trace to read.
!                                 (set irec to zero for sequential access)
!
! NBITS must be 8 or 16 or 32.
! If NBITS is omitted, 32 bits are written.
! Trace header words always have 64 bits written.
!
! If MAXRECORDS is specified, this number is used to calculate an extent
! size for the file.  This is important for very large files because there
! is a limited number of extents available.  Otherwise the default extent
! size is used.
!
! RHISTORY must be true or false (default true).
! WHISTORY must be ALL or NONE or CURRENT or MODEL.
! If NBITS == 8 or 16, the default for WHISTORY is CURRENT.
! If NBITS == 32 (or is omitted), the default for WHISTORY is MODEL.
!
! Header words 1 and 25 are appropriately set when writing the trace.
! This is the reason why HD is intent(inout) in PERMTFILE_WRITE.
!
! If an error occurs or an endfile is read, the file is closed, but the data
! structure is not deallocated until permtfile_close is called.  If an error
! occurs on a read, or an endfile is read, the HD and TR arrays will be
! filled with zeroes.
!
! When reading a file, NWIH, NDPT, and TSTRT need not match the traces on
! the file.  The returned trace and header will be adjusted appropriately by
! extending with zeroes or truncating.  But if DT does not match, an error
! is generated.
!
! If OBJ is not associated when calling PERMTFILE_CLOSE, nothing is done.
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
! 10. 2006-11-14  Bill Menger Modified p.._read so that on EOF the call to
!                             trcio won't trigger a read off of the end of
!                             the file, which prints a cio_pfio_.. error 
!                             message, confusing to the end user.  Instead of
!                             trying to read past EOF it looks at the trcio
!                             object num_traces and sees if the trace to be
!                             read is within the file boundaries.
!  9. 2005-10-24  Stoeckley  Add optional argument NTRACES.
!  8. 2002-02-04  Stoeckley  Add ability to read traces randomly.
!  7. 2001-08-27  Stoeckley  Add calls to trcio_read_history_cards and
!                             trcio_write_history_cards, and add optional
!                             arguments RHISTORY and WHISTORY.
!  6. 2001-06-29  Stoeckley  Add optional MAXRECORDS argument.
!  5. 2001-03-21  Stoeckley  Remove the use of the BYTE process which is
!                             being retired.
!  4. 2000-10-19  Stoeckley  Add missing required documentation section.
!  3. 2000-08-23  Stoeckley  Make arguments IPN and NBITS optional; make the
!                             temporary change (below) permanent.
!  2. 2000-06-20  Stoeckley  Temporarily change BYTE not to write to
!                             /dev/null until an absoft problem on poeplx03
!                             and 04 gets fixed.
!  1. 2000-05-22  Stoeckley  Initial version.
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


      module permtfile_module
      use named_constants_module
      use trcio_module
      use pc_module
      use string_module
      use lav_module
      use sizeof_module
      use extsize_module
      use string_module
      implicit none
      private
      public :: permtfile_open_read
      public :: permtfile_open_write
      public :: permtfile_close
      public :: permtfile_read
      public :: permtfile_write

      character(len=100),public,save :: permtfile_IDENT = &
       '$Id: permtfile.f90,v 1.10 2006/11/14 14:32:55 Menger prod sps $'

      type,public :: permtfile_struct
           private
           type(trcio_struct),pointer     :: trcio
           character(len=FILENAME_LENGTH) :: path
           integer                        :: nwih,ndpt
           integer                        :: lunprint
           integer                        :: nwih_input,ndpt_input
           integer                        :: nwih_minimum
           integer                        :: index1,index2
           integer                        :: index1_input,index2_input
           integer                        :: irec,errcount
      end type permtfile_struct

      integer,public,parameter :: PERMTFILE_OK    = TRCIO_OK
      integer,public,parameter :: PERMTFILE_ERROR = TRCIO_ERROR
      integer,public,parameter :: PERMTFILE_EOF   = TRCIO_EOF  

      contains


!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!

! Prints messages unless the error count is too large.
! If ERR indicates an error:
!   the error count is incremented.
!   if the file is open, it is closed.


      subroutine permtfile_msg (obj,err,msg)                   ! private

      type(permtfile_struct),intent(inout) :: obj              ! arguments
      integer               ,intent(in)    :: err              ! arguments
      character(len=*)      ,intent(in)    :: msg              ! arguments

      if (obj%lunprint > 0) then
           write (obj%lunprint,*) ' '
           write (obj%lunprint,*) 'PERMTFILE: ',trim(msg)
           if (err == PERMTFILE_ERROR) then
              if (obj%irec > 0) then
                 write (obj%lunprint,*) &
                          'PERMTFILE: FATAL ERROR ON TRACE NUMBER = ',obj%irec
              else
                 write (obj%lunprint,*) 'PERMTFILE: FATAL ERROR'
              end if
           else if (err == PERMTFILE_EOF) then
              write (obj%lunprint,*) &
                  'PERMTFILE: END OF FILE READ AFTER TRACE NUMBER = ',obj%irec-1
           else
           end if
           write (obj%lunprint,*) 'PERMTFILE: PATH = ',trim(obj%path)
           write (obj%lunprint,*) ' '
      end if

      if (err /= PERMTFILE_OK) then
           call permtfile_private_close (obj)
      end if

      if (err == PERMTFILE_ERROR) then
           obj%errcount = obj%errcount + 1
           if (obj%errcount > 20) obj%lunprint = 0
      end if
      if (obj%lunprint > 0) write (obj%lunprint,*) ' '

      end subroutine permtfile_msg


!!---------------------------- private init -------------------------------!!
!!---------------------------- private init -------------------------------!!
!!---------------------------- private init -------------------------------!!


      subroutine permtfile_private_init (obj,path,nwih,ndpt,lunprint)

      type(permtfile_struct),pointer     :: obj                 ! arguments
      character(len=*)      ,intent(in)  :: path                ! arguments
      integer               ,intent(in)  :: nwih,ndpt           ! arguments
      integer               ,intent(in)  :: lunprint            ! arguments

      allocate (obj)

      nullify (obj%trcio)

      obj%path         = path
      obj%nwih         = nwih   
      obj%ndpt         = ndpt   
      obj%lunprint     = lunprint
      obj%nwih_input   = nwih   
      obj%ndpt_input   = ndpt   
      obj%nwih_minimum = nwih   
      obj%index1       = 1
      obj%index2       = obj%ndpt
      obj%index1_input = 1
      obj%index2_input = obj%ndpt
      obj%irec         = 0
      obj%errcount     = 0

      end subroutine permtfile_private_init


!!------------------------------ open read --------------------------------!!
!!------------------------------ open read --------------------------------!!
!!------------------------------ open read --------------------------------!!


      subroutine permtfile_open_read (obj,path,nwih,ndpt,tstrt,dt,lunprint, &
                                      err,rhistory,ntraces)

      type(permtfile_struct),pointer     :: obj                   ! arguments
      character(len=*)      ,intent(in)  :: path                  ! arguments
      integer               ,intent(in)  :: nwih,ndpt             ! arguments
      real                  ,intent(in)  :: tstrt,dt              ! arguments
      integer               ,intent(in)  :: lunprint              ! arguments
      integer               ,intent(out) :: err                   ! arguments
      logical ,optional     ,intent(in)  :: rhistory              ! arguments
      integer ,optional     ,intent(out) :: ntraces               ! arguments
      real                               :: tstrt_input,dt_input  ! local
      real                               :: tstop_input,tstop     ! local
      real                               :: tmin,tmax             ! local
      character(len=80)                  :: msg                   ! local
      integer                            :: istat                 ! local

!----------get started.

      call permtfile_private_init (obj,path,nwih,ndpt,lunprint)

!----------open input trcio file.

      obj%trcio => trcio_open(obj%path, 'r')
      if (.not.associated(obj%trcio)) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'error opening input TRCIO trace file')
           return
      end if

      err = PERMTFILE_OK
      call permtfile_msg (obj,err,'successfully opened input TRCIO trace file')
      obj%nwih_input  = obj%trcio%nwih
      obj%ndpt_input  = obj%trcio%num_values
          dt_input    = obj%trcio%dt
          tstrt_input = obj%trcio%tmin

      if (present(ntraces)) ntraces = obj%trcio%num_traces

!----------check sample rate.

      if (dt_input /= dt) then
           err = PERMTFILE_ERROR
           call string_encode (msg,'mismatching DT',dt_input, &
                                         'on file - should be',dt)
           call permtfile_msg (obj,err,msg)
           return
      end if

!----------get first and last trace index to return.

      tstop       = tstrt       + (obj%ndpt       - 1) * dt
      tstop_input = tstrt_input + (obj%ndpt_input - 1) * dt

      tmin = max(tstrt, tstrt_input)
      tmax = min(tstop, tstop_input)

      obj%index1       = 1 + nint((tmin - tstrt      ) / dt)
      obj%index2       = 1 + nint((tmax - tstrt      ) / dt)

      obj%index1_input = 1 + nint((tmin - tstrt_input) / dt)
      obj%index2_input = 1 + nint((tmax - tstrt_input) / dt)

      if (obj%index1 > obj%index2) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'non-overlapping times on file')
           return
      end if

      if (obj%index1_input > obj%index2_input) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'non-overlapping times on file')
           return
      end if

      if (obj%index2_input - obj%index1_input /= obj%index2 - obj%index1) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'trace length mismatch')
           return
      end if

      if (obj%index1 < 1 .or. obj%index2 > obj%ndpt) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'trace range exceeded')
           return
      end if

      if (obj%index1_input < 1 .or. obj%index2_input > obj%ndpt_input) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'trace range exceeded')
           return
      end if

      obj%nwih_minimum = min(obj%nwih,obj%nwih_input)

!----------read history cards.

      if (.not.present(rhistory)) then
           call trcio_read_history_cards (obj%trcio,istat)
      else if (rhistory) then
           call trcio_read_history_cards (obj%trcio,istat)
      else
           istat = 0
      end if

      if (istat /= 0) then
           err = PERMTFILE_ERROR
           call permtfile_msg &
                 (obj,err,'error reading history cards from TRCIO trace file')
      end if

      end subroutine permtfile_open_read


!!------------------------------ open write -------------------------------!!
!!------------------------------ open write -------------------------------!!
!!------------------------------ open write -------------------------------!!


      subroutine permtfile_open_write (obj,path,nwih,ndpt,tstrt,dt,lunprint, &
                                       err,ipn,nbits,maxrecords,whistory)

      type(permtfile_struct)   ,pointer     :: obj                  ! arguments
      character(len=*)         ,intent(in)  :: path                 ! arguments
      integer                  ,intent(in)  :: nwih,ndpt            ! arguments
      real                     ,intent(in)  :: tstrt,dt             ! arguments
      integer                  ,intent(in)  :: lunprint             ! arguments
      integer                  ,intent(out) :: err                  ! arguments
      integer         ,optional,intent(in)  :: ipn,nbits,maxrecords ! arguments
      character(len=*),optional,intent(in)  :: whistory             ! arguments
      character(len=80)                     :: msg                  ! local
      integer                               :: istat,ipn2,nbits2    ! local
      integer                               :: recsize              ! local

!----------get started.

      call permtfile_private_init (obj,path,nwih,ndpt,lunprint)

      if (present(ipn)) then
           ipn2 = ipn
      else
           ipn2 = 0
      end if

      if (present(nbits)) then
           if (nbits /= 8 .and. nbits /= 16 .and. nbits /= 32) then
                err = PERMTFILE_ERROR
                call string_encode (msg,'illegal value of NBITS =',nbits)
                call permtfile_msg (obj,err,msg)
                return
           end if
           nbits2 = nbits
      else
           nbits2 = 32
      end if

!----------set the file extent size.

      if (present(maxrecords)) then
           recsize = nwih * sizeof(1.0d0) + ndpt * (nbits2 / 8)
           call extsize (lunprint,maxrecords,recsize,err)
           if (err == PERMTFILE_ERROR) then
                call permtfile_msg (obj,err,'error setting extent size')
                return
           end if
      end if

!----------open output trcio file.

      obj%trcio => trcio_open(obj%path, 'w')
      if (.not.associated(obj%trcio)) then
           err = PERMTFILE_ERROR
           call permtfile_msg &
                 (obj,err,'error opening output TRCIO trace file')
      else
           err = PERMTFILE_OK
           call permtfile_msg &
                 (obj,err,'successfully opened output TRCIO trace file')
           obj%trcio%nwih       = obj%nwih
           obj%trcio%num_values = obj%ndpt
           obj%trcio%nbits      = nbits2
           obj%trcio%nbits_hd   = 64
           obj%trcio%dt         = dt
           obj%trcio%tmin       = tstrt
           if (nbits2 == 32) then
                obj%trcio%wtype = 'IEEE'
           else
                obj%trcio%wtype = 'INT'
           end if
           istat = trcio_writeheader (obj%trcio)
           if (istat /= TRCIO_OK) then
                err = PERMTFILE_ERROR
                call permtfile_msg &
                        (obj,err,'error writing TRCIO trace file header')
           end if
      end if

!----------write history cards.

      if (present(whistory)) then
           call trcio_write_history_cards (obj%trcio,whistory)
      else if (nbits2 <= 16) then
           call trcio_write_history_cards (obj%trcio,'CURRENT')
      else
           call trcio_write_history_cards (obj%trcio,'MODEL')
      end if

      end subroutine permtfile_open_write


!!---------------------------- private close ------------------------------!!
!!---------------------------- private close ------------------------------!!
!!---------------------------- private close ------------------------------!!


      subroutine permtfile_private_close (obj)

      type(permtfile_struct),intent(inout) :: obj              ! arguments
      integer                              :: istat            ! local

      if (associated(obj%trcio)) then
           istat = trcio_close (obj%trcio)
           if (obj%lunprint > 0) then
              write (obj%lunprint,*) 'PERMTFILE: closed TRCIO trace file'
              write (obj%lunprint,*) 'PERMTFILE: PATH = ',trim(obj%path)
           end if
      end if

      end subroutine permtfile_private_close


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine permtfile_close (obj)

      type(permtfile_struct),pointer :: obj                 ! arguments

      if (.not.associated(obj)) return

      if (obj%lunprint > 0) write (obj%lunprint,*) ' '
      call permtfile_private_close (obj)
      if (obj%lunprint > 0) write (obj%lunprint,*) ' '

      deallocate (obj)

      end subroutine permtfile_close


!!------------------------------ read -----------------------------------!!
!!------------------------------ read -----------------------------------!!
!!------------------------------ read -----------------------------------!!


      subroutine permtfile_read (obj,hd,tr,err,irec)

      type(permtfile_struct),intent(inout) :: obj                   ! arguments
      double precision      ,intent(out)   :: hd(:)                 ! arguments
      real                  ,intent(out)   :: tr(:)                 ! arguments
      integer               ,intent(out)   :: err                   ! arguments
      integer   ,optional   ,intent(in)    :: irec                  ! arguments
      double precision                     :: hdum(obj%nwih_input)  ! local
      real                                 :: tdum(obj%ndpt_input)  ! local
      integer                              :: istat,irec2           ! local

!----------get started.

      obj%irec = obj%irec + 1
      err = PERMTFILE_OK

      if (size(hd) < obj%nwih) then
           hd(:) = 0.0
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'HD array too small')
      end if
      if (size(tr) < obj%ndpt) then
           tr(:) = 0.0
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'TR array too small')
      end if
      if (err == PERMTFILE_ERROR) return

      hd(1:obj%nwih) = 0.0
      tr(1:obj%ndpt) = 0.0

      if (present(irec)) then
           irec2 = irec
      else
           irec2 = 0
      end if

!----------read trcio file.

      if (associated(obj%trcio)) then
           if (irec2 > 0) then
                if (obj%trcio%num_traces .lt. irec2 ) then 
                  istat=TRCIO_EOF
                else
                  istat = trcio_read_trace (obj%trcio,hdum,tdum,irec2)
                endif
           else
                if (obj%trcio%num_traces .lt. obj%irec ) then
                  istat=TRCIO_EOF
                else
                  istat = trcio_read_trace (obj%trcio,hdum,tdum)
                endif           
           end if

           if (istat == TRCIO_EOF) then
                err = PERMTFILE_EOF
                call permtfile_msg &
                       (obj,err,'end of file read on TRCIO trace file')
           else if (istat == TRCIO_ERROR) then
                err = PERMTFILE_ERROR
                if (irec2 > 0) then
                     call permtfile_msg &
                       (obj,err,'error reading trace '//  &
                        trim(string_ii2ss(irec2))//' from TRCIO trace file')
                else
                     call permtfile_msg &
                       (obj,err,'error reading trace from TRCIO trace file')
                end if
           else
                err = PERMTFILE_OK
                hd(1:obj%nwih_minimum)    = hdum(1:obj%nwih_minimum)
                tr(obj%index1:obj%index2) = tdum(obj%index1_input: &
                                                 obj%index2_input)
           end if

!----------previous error.

      else
           err = PERMTFILE_ERROR
           call permtfile_msg &
                       (obj,err,'attempt to read from closed trace file')
      end if

      end subroutine permtfile_read


!!---------------------------- write -------------------------------------!!
!!---------------------------- write -------------------------------------!!
!!---------------------------- write -------------------------------------!!


      subroutine permtfile_write (obj,hd,tr,err)

      type(permtfile_struct),intent(inout) :: obj                 ! arguments
      double precision      ,intent(inout) :: hd(:)               ! arguments
      real                  ,intent(in)    :: tr(:)               ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      integer                              :: istat               ! local

!----------get started.

      err      = PERMTFILE_OK
      obj%irec = obj%irec + 1
      hd(1)    = obj%irec
      hd(25)   = lav(tr,obj%ndpt)

      if (size(hd) < obj%nwih) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'HD array too small')
      end if
      if (size(tr) < obj%ndpt) then
           err = PERMTFILE_ERROR
           call permtfile_msg (obj,err,'TR array too small')
      end if
      if (err == PERMTFILE_ERROR) return

!----------write trcio file.

      if (associated(obj%trcio)) then

           istat = trcio_write_trace (obj%trcio,hd,tr)
           if (istat /= TRCIO_OK) then
                err = PERMTFILE_ERROR
                call permtfile_msg &
                       (obj,err,'error writing trace to TRCIO trace file')
           else
                err = PERMTFILE_OK
           end if

!----------previous error.

      else
           err = PERMTFILE_ERROR
           call permtfile_msg &
                       (obj,err,'attempt to write to closed trace file')
      end if

      end subroutine permtfile_write


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module permtfile_module

!!------------------- UNIT  TESTS --- UNCOMMENT TO RUN THESE:
!!-------------------- in vi, remove !UT from start of these lines, then
!!---------------------recompile.
!

!UTprogram permtfile_test
!UT  
!UT  !Unit tests for permtfile: 
!UT  
!UT  !ab80_f90 -o permtfile_test permtfile.f90 \
!UT  !-p /home/sps/production/modules/linuxab80 \
!UT  !/home/sps/lib/linuxab80/prodlib.a -lm -lpthread
!UT  
!UT  use permtfile_module
!UT  use pc_module
!UT  use unix_module
!UT  implicit none
!UT  
!UT  real             :: tr(4)
!UT  double precision :: hd(25)
!UT  integer          :: ntrc,i
!UT  real             :: dt, tstrt
!UT  integer          :: nwih,ndpt,lunprint,err,lprt
!UT  type(permtfile_struct),pointer :: p
!UT  
!UT  nwih = 25
!UT  lprt = 6
!UT  ndpt = 4
!UT  dt   = 1.0
!UT  tstrt= 0.0
!UT  ntrc=6
!UT  
!UT  call pc_frontend_update(lprt)
!UT  
!UT  call pc_put_global ('tstrt',tstrt)
!UT  call pc_put_global ('dt',dt)
!UT  call pc_put_global ('ndpt',ndpt)
!UT  call pc_put_global ('nwih',nwih)
!UT  
!UT  call permtfile_open_write(p,"permtfile_test.trc",& 
!UT       nwih,ndpt,tstrt,dt,lprt,err)
!UT  if(err /= PERMTFILE_OK ) then
!UT    stop "FAIL:  error opening perm file for write"
!UT  endif
!UT  
!UT  
!UT  do i = 1, ntrc
!UT    tr(:) = (/1,2,3,4/)
!UT    tr(:) = tr(:)*1.0*i
!UT    call permtfile_write(p,hd,tr,err)
!UT    if(err /= PERMTFILE_OK) then
!UT      stop "FAIL: error writing tfile trace"
!UT    else
!UT      !print*,'err=',err,' hd(1)=',hd(1),' hd(25)=',hd(25)
!UT    endif
!UT  end do
!UT  
!UT  call permtfile_close(p)
!UT  
!UT  call permtfile_open_read(p,"permtfile_test.trc", &
!UT       nwih,ndpt,tstrt,dt,lprt,err, &
!UT                           ntraces=ntrc)
!UT  
!UT  if ( ntrc /= 6 ) then
!UT    print*,'FAIL: ntraces = ',ntrc, ' should be 6'
!UT    stop "FAIL: opened for read but wrong number of traces"
!UT  endif
!UT  
!UT  do i = 1, ntrc + 1
!UT    call permtfile_read(p,hd,tr,err)
!UT    !print*,'hdr=',hd(1),hd(25),' trace(',i,')=',tr
!UT    if( (nint(hd(1)) .ne. i) .and. err .eq. PERMTFILE_OK .and. &
!UT       nint(hd(25)) .ne. 4*i ) then
!UT      print*,'FAIL: Error reading what should have been written.'
!UT      STOP
!UT    endif
!UT    !print*,'i=',i,' err=',err
!UT    if(i > ntrc .and. err .ne. PERMTFILE_EOF ) then
!UT      stop "FAIL:  end of file not correctly handled"
!UT    endif
!UT  end do
!UT  
!UT  
!UT  call permtfile_close(p)
!UT  
!UT  err = unix_system("rm -f permtfile_test.trc")
!UT  
!UT  print*,'PASS:  SUCCESSFULLY COMPLETED UNIT TEST!'
!UT  
!UTend program permtfile_test


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

