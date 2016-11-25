
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- permtset.f90 ------------------------------!!
!!---------------------------- permtset.f90 ------------------------------!!
!!---------------------------- permtset.f90 ------------------------------!!

 
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
! Name       : PERMTSET
! Category   : io
! Written    : 2005-08-23   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Read and write a set of TRCIO files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to open, read, write, and close a set of
! TRCIO files which store seismic traces, and to access these files
! sequentially or randomly.
!
! Through the use of this primitive, the set of TRCIO files appears to
! be a single file which would be a concatenation of the set of files.
!
! This primitive uses a set of PERMTFILE objects.
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
!                           o    i     i     i    i     i   i     i
! call permtset_open_read (obj,paths,npaths,nwih,ndpt,tstrt,dt,lunprint,
!                          err,rhistory,ntraces)
!                           o     i        o
!                                opt      opt
!
!                            o    i     i     i    i     i   i     i
! call permtset_open_write (obj,paths,npaths,nwih,ndpt,tstrt,dt,lunprint,
!                           err,maxrecords,ipn,nbits,whistory)
!                            o      i       i    i      i
!                                          opt  opt    opt
!
!                            b
! call permtset_close      (obj)
!
!                            b   b   i    o 
! call permtset_write      (obj, hd, tr, err)
! call permtset_read       (obj, hd, tr, err, irec)
!                            b   o   o    o    i
!                                             opt
!
!
! type(permtset_struct)    obj = pointer to PERMTSET structure.
! character(len=*)    paths(:) = file names to read or write.
! integer               npaths = number of file names to read or write.
! integer                 nwih = number of words of HD to read or write.
! integer                 ndpt = number of words of TR to read or write.
! real                   tstrt = starting time of TR to read or write.
! real                      dt = sample interval of TR to read or write.
! integer             lunprint = unit number for printing (or 0).
! integer                  err = PERMTSET_ERROR (if error) or PERMTSET_OK
!                                 or PERMTSET_EOF (if endfile has been read).
! integer              ntraces = total number of traces on all input files.
! integer           maxrecords = number of traces to put on each output file.
! integer                  ipn = process number for history (default 0).
! integer                nbits = number of bits per trace sample to write.
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
! MAXRECORDS traces will be put onto each output file.
! If there are too many traces, the excess will be put onto the last file.
! If there are not enough traces, some of the last files may be empty, and
! the last file with traces may contain less than MAXRECORDS traces.
!
! RHISTORY must be true or false (default true).
! WHISTORY must be ALL or NONE or CURRENT or MODEL.
! If NBITS == 8 or 16, the default for WHISTORY is CURRENT.
! If NBITS == 32 (or is omitted), the default for WHISTORY is MODEL.
!
! Header words 1 and 25 are appropriately set when writing the trace.
! This is the reason why HD is intent(inout) in PERMTSET_WRITE.
!
! If an error occurs or an endfile is read, the file is closed, but the data
! structure is not deallocated until permtset_close is called.  If an error
! occurs on a read, or an endfile is read, the HD and TR arrays will be
! filled with zeroes.
!
! When reading a file, NWIH, NDPT, and TSTRT need not match the traces on
! the file.  The returned trace and header will be adjusted appropriately by
! extending with zeroes or truncating.  But if DT does not match, an error
! is generated.
!
! If OBJ is not associated when calling PERMTSET_CLOSE, nothing is done.
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
!002. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  1. 2005-10-24  Stoeckley  Initial version.
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


      module permtset_module
      use permtfile_module
      use pc_module
      implicit none
      private
      public :: permtset_open_read
      public :: permtset_open_write
      public :: permtset_close
      public :: permtset_read
      public :: permtset_write

character(len=100),public,save :: permtset_IDENT = &
'$Id: permtset.f90,v 1.2 2006/10/17 13:45:46 Glover prod sps $'

      type,private :: permtfile_holder
           private
           type(permtfile_struct),pointer :: permtfile
      end type permtfile_holder

      type,public :: permtset_struct
           private
           type(permtfile_holder),pointer :: holders(:)
           integer               ,pointer :: istart(:)
           integer               ,pointer :: istop(:)
           integer                        :: nfiles
           integer                        :: irec
           integer                        :: maxrecords
           integer                        :: ifile
      end type permtset_struct

      integer,public,parameter :: PERMTSET_OK    = PERMTFILE_OK
      integer,public,parameter :: PERMTSET_ERROR = PERMTFILE_ERROR
      integer,public,parameter :: PERMTSET_EOF   = PERMTFILE_EOF  

      contains


!!------------------------------ open read --------------------------------!!
!!------------------------------ open read --------------------------------!!
!!------------------------------ open read --------------------------------!!


      subroutine permtset_open_read (obj,paths,npaths,                 &
                                     nwih,ndpt,tstrt,dt,lunprint,err,  &
                                     rhistory,ntraces)

      type(permtset_struct) ,pointer     :: obj                  ! arguments
      character(len=*)      ,intent(in)  :: paths(:)             ! arguments
      integer               ,intent(in)  :: npaths               ! arguments
      integer               ,intent(in)  :: nwih,ndpt            ! arguments
      real                  ,intent(in)  :: tstrt,dt             ! arguments
      integer               ,intent(in)  :: lunprint             ! arguments
      integer               ,intent(out) :: err                  ! arguments
      logical      ,optional,intent(in)  :: rhistory             ! arguments
      integer      ,optional,intent(out) :: ntraces              ! arguments
      integer                            :: ifile,ntraces2,last  ! local
      type(permtfile_struct),pointer     :: permtfile            ! local

      nullify (permtfile) ! jpa

      allocate (obj)
      nullify (obj%holders)
      nullify (obj%istart)
      nullify (obj%istop)

      obj%nfiles     = npaths
      obj%irec       = 0
      obj%maxrecords = 0
      obj%ifile      = 1

      if (npaths <= 0) then
           err = PERMTSET_ERROR
           call pc_error ('PERMTSET: no trace files provided.')
           return
      endif

      allocate (obj%holders(obj%nfiles))
      allocate (obj%istart (obj%nfiles))
      allocate (obj%istop  (obj%nfiles))

      last = 0
      do ifile = 1,obj%nfiles
           call permtfile_open_read (permtfile,paths(ifile),          &
                                     nwih,ndpt,tstrt,dt,lunprint,err, &
                                     rhistory,ntraces2)

           if (err /= PERMTSET_OK) return

           obj%holders(ifile)%permtfile => permtfile

           obj%istart(ifile) = last + 1
           obj%istop (ifile) = last + ntraces2
           last              = obj%istop(ifile)
           call pc_print ('PERMTSET:',ntraces2,' traces on file '//paths(ifile))
      enddo

      if (present(ntraces)) ntraces = last

      end subroutine permtset_open_read


!!------------------------------ open write -------------------------------!!
!!------------------------------ open write -------------------------------!!
!!------------------------------ open write -------------------------------!!


      subroutine permtset_open_write (obj,paths,npaths,                 &
                                      nwih,ndpt,tstrt,dt,lunprint,err,  &
                                      maxrecords,ipn,nbits,whistory)

      type(permtset_struct)    ,pointer     :: obj              ! arguments
      character(len=*)         ,intent(in)  :: paths(:)         ! arguments
      integer                  ,intent(in)  :: npaths           ! arguments
      integer                  ,intent(in)  :: nwih,ndpt        ! arguments
      real                     ,intent(in)  :: tstrt,dt         ! arguments
      integer                  ,intent(in)  :: lunprint         ! arguments
      integer                  ,intent(in)  :: maxrecords       ! arguments
      integer                  ,intent(out) :: err              ! arguments
      integer         ,optional,intent(in)  :: ipn,nbits        ! arguments
      character(len=*),optional,intent(in)  :: whistory         ! arguments
      integer                               :: ifile            ! local
      type(permtfile_struct)   ,pointer     :: permtfile        ! local

      nullify (permtfile) ! jpa

      allocate (obj)
      nullify (obj%holders)
      nullify (obj%istart)
      nullify (obj%istop)

      obj%nfiles     = npaths
      obj%irec       = 0
      obj%maxrecords = maxrecords
      obj%ifile      = 1

      if (npaths <= 0) then
           err = PERMTSET_ERROR
           call pc_error ('PERMTSET: no trace files provided.')
           return
      endif

      allocate (obj%holders(obj%nfiles))

      do ifile = 1,obj%nfiles
           call permtfile_open_write (permtfile,paths(ifile),           &
                                      nwih,ndpt,tstrt,dt,lunprint,err,  &
                                      ipn,nbits,maxrecords,whistory)

           if (err /= PERMTSET_OK) return

           obj%holders(ifile)%permtfile => permtfile
      enddo

      end subroutine permtset_open_write


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine permtset_close (obj)

      type(permtset_struct),pointer  :: obj                 ! arguments
      integer                        :: ifile               ! local
      type(permtfile_struct),pointer :: permtfile           ! local

      if (.not.associated(obj)) return

      do ifile = 1,obj%nfiles
           permtfile => obj%holders(ifile)%permtfile
           call permtfile_close (permtfile)
      enddo

      if(associated(obj%holders)) deallocate (obj%holders)
      if(associated(obj%istart))  deallocate (obj%istart)
      if(associated(obj%istop))   deallocate (obj%istop)

      deallocate (obj)

      end subroutine permtset_close


!!------------------------------ read -----------------------------------!!
!!------------------------------ read -----------------------------------!!
!!------------------------------ read -----------------------------------!!


      subroutine permtset_read (obj,hd,tr,err,irec)

      type(permtset_struct) ,intent(inout) :: obj               ! arguments
      double precision      ,intent(out)   :: hd(:)             ! arguments
      real                  ,intent(out)   :: tr(:)             ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer      ,optional,intent(in)    :: irec              ! arguments
      type(permtfile_struct),pointer       :: permtfile         ! local
      integer                              :: irec2,irec3,ifile ! local

      obj%irec = obj%irec + 1

      if (present(irec)) then
           irec2 = irec
      else
           irec2 = 0
      end if

      if (irec2 == 0) irec2 = obj%irec

      if (irec2 < obj%istart(obj%ifile) .or.  &
          irec2 > obj%istop (obj%ifile)) then
           obj%ifile = 0
           do ifile = 1,obj%nfiles
                if (irec2 >= obj%istart(ifile) .and.  &
                    irec2 <= obj%istop (ifile)) then
                     obj%ifile = ifile
                     exit
                endif
           enddo
      endif

      if (obj%ifile == 0) then
  !        call pc_error ('PERMTSET: trace record',irec2,'out of range')
           err = PERMTSET_EOF
           return
      endif

      irec3 = irec2 - obj%istart(obj%ifile) + 1

      permtfile => obj%holders(obj%ifile)%permtfile

      call permtfile_read (permtfile,hd,tr,err,irec3)
           
      end subroutine permtset_read


!!---------------------------- write -------------------------------------!!
!!---------------------------- write -------------------------------------!!
!!---------------------------- write -------------------------------------!!


      subroutine permtset_write (obj,hd,tr,err)

      type(permtset_struct) ,intent(inout) :: obj                 ! arguments
      double precision      ,intent(inout) :: hd(:)               ! arguments
      real                  ,intent(in)    :: tr(:)               ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      type(permtfile_struct),pointer       :: permtfile           ! local
      integer                              :: ifile               ! local

      obj%irec = obj%irec + 1

      ifile = 1 + (obj%irec - 1) / obj%maxrecords
      ifile = max(ifile,1)
      ifile = min(ifile,obj%nfiles)

      if(ifile > 1) then
           permtfile => obj%holders(ifile-1)%permtfile
           call permtfile_close (permtfile)
      endif

      permtfile => obj%holders(ifile)%permtfile

      call permtfile_write (permtfile,hd,tr,err)

      end subroutine permtset_write


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module permtset_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

