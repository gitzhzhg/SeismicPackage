!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- temptcache.f90 ------------------------------!!
!!---------------------------- temptcache.f90 ------------------------------!!
!!---------------------------- temptcache.f90 ------------------------------!!

 
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
! Name       : TEMPTCACHE
! Category   : io
! Written    : 2004-12-02   by: Tom Stoeckley
! Revised    : 2006-10-31   by: D. Glover
! Maturity   : production
! Purpose    : Very simple temporary random-access seismic trace buffer.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to open, read, write, and close a temporary
! random-access file which stores seismic traces.  It is designed for
! efficient use by a process module which needs to store traces or similar
! data temporarily.  It is useful for storing entire traces and headers, or
! for storing small trace windows which might be packed several trace values
! per word.  The traces can be optionally packed and unpacked by this primitive
! when writing and reading.
!
! This primitive uses the TEMPTFILE primitive to store traces on disk.  This
! primitive keeps a trace buffer in memory containing the last traces read,
! in order to reduce the I/O time when the same traces are requested several
! times.
!
! This primitive has nearly the same public interface as TEMPTFILE and
! TEMPTBUFFER to make it easy to swap this primitive in place of the TEMPTFILE
! or TEMPTBUFFER primitive.  However, like TEMPTBUFFER, this primitive can
! be used only for random-access (i.e. by specifying a record number), whereas
! TEMPTFILE can be used for both random-access and sequential I/O.  Like
! TEMPTBUFFER, this primitive also must use both a trace and header array,
! whereas TEMPTFILE can be used with both arrays or either array.
!
! This primitive can actually read and write any data consisting of fixed
! length records.  Each record consists of a header word array and/or a trace
! array.  Normally, for seismic traces, the header word array is type double
! precision and the trace array is type real.  However, both arrays can
! instead be type integer, or the header array can be type integer and the
! trace array can be type real.  When the arrays are integers, they can
! optionally be packed several per word outside this primitive.
!
! This primitive behaves like TEMPTBUFFER but is more efficient because it
! returns pointers to the trace and header arrays instead of copying the
! arrays.  This has the effect of cutting processing time in half when
! stacking traces, for example.
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
!                                i     i       i         i        i
!   nstore = temptcache_nstore (nwih, ndpt, nrecords, nbuffer, vartypes)
!
!                            o    i     i     i       i        o 
!   call temptcache_open   (obj, seed, nwih, ndpt, lunprint, whoops,
!                           nrecords, nbuffer, vartypes, directory)
!                              i         i        i          i
!                                                           opt
!                            b
!   call temptcache_close  (obj)
!                                                           PAIR HD TR VARTYPES
!                            b    i     o    o          o
!   call temptcache_fetch  (obj, irec, hdd, trr,      whoops) 1 dbl real  'DR'
!   call temptcache_fetch  (obj, irec, hdi, tri,      whoops) 2 int int   'II'
!   call temptcache_fetch  (obj, irec, hdi, trr,      whoops) 3 int real  'IR'
!   call temptcache_fetch8 (obj, irec, hdd, trr,      whoops) 4 dbl real  'DR8'
!   call temptcache_fetch  (obj, irec, hdd, trr, aur, whoops) 5 dbl rl rl 'DRR'
!                            b    i     o    o    o     o
!
! type(temptcache_struct)  obj = pointer to the TEMPTCACHE data structure.
! integer               nstore = allocated memory required (in 4-byte words).
! character(len=*)        seed = base name for creating non-existent filename.
! integer                 nwih = number of words of HDD/HDI (>= 1).
! integer                 ndpt = number of words of TRR/TRI (>= 1).
! integer             lunprint = unit number for printing (or 0 not to print).
! logical               whoops = true if an error occurred.
! integer             nrecords = number of records to read or write (>= 1).
! integer              nbuffer = number of traces in work buffer (>= 1).
! character(len=*)    vartypes = which pair of read/write routines to be used.
! character(len=*)   directory = directory in which to put the file.
! integer                 irec = record number (trace number) to fetch (>= 1).
! double precision   hdd(nwih) = pointer to full-precision header word array.
! real               trr(ndpt) = pointer to full-precision trace array.
! integer            hdi(nwih) = pointer to integerized/packed hdr word array.
! integer            tri(ndpt) = pointer to integerized or packed trace array.
! real               aur(ndpt) = pointer to optional auxiliary array.
!
!
! TEMPTCACHE_OPEN:
!
!   This routine allocates the data structure and opens the file.
!   Allowed values for VARTYPES are shown above.
!
!   If DIRECTORY is not specified or blank, the cpstemp directory will be used.
!   This is a special directory on the local node.
!   The cpstemp directory does not have to pre-exist.
!
!   If DIRECTORY is specified and not blank, the filename will be prepended
!   by the specified directory, which can be an absolute or relative path.
!   For example:
!   Setting DIRECTORY to '/tmp' will place the file into the /tmp directory.
!   Setting DIRECTORY to '.' will place the file into the local directory.
!
! TEMPTCACHE_CLOSE:
!
!   This routine closes the file and deallocates the data structure.
!   If OBJ is not associated, nothing is done.
!
! BEHAVIOR OF THE FETCH ROUTINES:
!
!   If the requested record exists in the cache, pointers to that record
!   in the cache are returned.
!
!   If the requested record does not exist in the cache but does exist on
!   disk, then the oldest record in the cache is written to disk and the
!   requested record is read from disk into the vacated slot in the cache,
!   and then pointers to that record in the cache are returned.
!
!   If the requested record does not exist in the cache or on disk, pointers
!   to a zeroed record in the cache are returned.  If there are no zeroed
!   records in the cache, the oldest record in the cache is first written to
!   disk to make room for the zeroed record in the cache.
!
!   The calling program can update the returned record.  This automatically
!   updates that record in the cache because pointers had been returned.
!
! COMMENTS REGARDING THE FETCH ROUTINES:
!
!   Five fetch subroutines are available.  All fetches to any one instance
!   of this primitive may use only one of these five subroutines.  They
!   should not be mixed.
!
!   HDD and TRR are type double precision and real, respectively.  This is
!   the normal case for storing seismic traces in their full precision.
!
!   HDI and TRI are both type integer.  This could be the case where the values
!   are not really seismic traces, or are packed (or integerized) traces and
!   headers.  Note that when HDI and TRI contain packed values, the values of
!   NWIH and NDPT correspond to the number of words of memory containing the
!   packed values, and not the number of values themselves.
!
! TEMPTCACHE_FETCH8:
!
!   This routine converts the header array to type real before storing
!   on disk, and back to double precision when read from disk.  The trace
!   array is packed into byte values for storing on disk, and unpacked to
!   its original real values (but with less precision of course) when read
!   from disk.
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
!002. 2006-10-31  D. Glover  Added NULLIFY statements for Intel compiler.
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


      module temptcache_module
      use named_constants_module
      use temptfile_module
      implicit none
      private
      public :: temptcache_nstore
      public :: temptcache_open
      public :: temptcache_close
      public :: temptcache_fetch
      public :: temptcache_fetch8

      character(len=100),public,save :: temptcache_IDENT = &
'$Id: temptcache.f90,v 1.2 2006/10/30 14:01:45 Glover prod sps $'

      type,public :: temptcache_struct
           private
           integer                         :: nwih
           integer                         :: ndpt
           integer                         :: lunprint
           integer                         :: nrecords 
           integer                         :: nbuffer 
           character(len=4)                :: vartypes
           integer                         :: priority
           integer                         :: kount_cache
           integer                         :: kount_rfile
           integer                         :: kount_wfile
           double precision       ,pointer :: hdd(:,:)   ! (nwih,nbuffer)
           integer                ,pointer :: hdi(:,:)   ! (nwih,nbuffer)
           real                   ,pointer :: trr(:,:)   ! (ndpt,nbuffer)
           integer                ,pointer :: tri(:,:)   ! (ndpt,nbuffer)
           real                   ,pointer :: aur(:,:)   ! (ndpt,nbuffer)
           integer                ,pointer :: indx(:)    ! (nrecords)
           integer                ,pointer :: flag(:)    ! (nbuffer)
           integer                ,pointer :: irec(:)    ! (nbuffer)
           type(temptfile_struct) ,pointer :: temptfile
      end type temptcache_struct

      interface temptcache_fetch
           module procedure temptcache_fetch_ii
           module procedure temptcache_fetch_dr
           module procedure temptcache_fetch_ir
           module procedure temptcache_fetch_drr
      end interface

      contains


!!------------------------------ nstore ---------------------------------!!
!!------------------------------ nstore ---------------------------------!!
!!------------------------------ nstore ---------------------------------!!


      function temptcache_nstore &
                  (nwih,ndpt,nrecords,nbuffer,vartypes) result (nstore)

      integer                  ,intent(in)  :: nwih,ndpt        ! arguments
      integer                  ,intent(in)  :: nrecords         ! arguments
      integer                  ,intent(in)  :: nbuffer          ! arguments
      character(len=*)         ,intent(in)  :: vartypes         ! arguments
      integer                               :: nstore           ! result

      nstore = nrecords + 2 * nbuffer
      if (vartypes(1:1) == 'D') nstore = nstore + nwih * nbuffer * 2
      if (vartypes(1:1) == 'I') nstore = nstore + nwih * nbuffer
      if (vartypes(2:2) == 'R') nstore = nstore + ndpt * nbuffer
      if (vartypes(2:2) == 'I') nstore = nstore + ndpt * nbuffer
      if (vartypes    == 'DRR') nstore = nstore + ndpt * nbuffer

      end function temptcache_nstore


!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!


      subroutine temptcache_open (obj,seed,nwih,ndpt,lunprint,whoops, &
                                   nrecords,nbuffer,vartypes,directory)

      type(temptcache_struct)  ,pointer     :: obj              ! arguments
      character(len=*)         ,intent(in)  :: seed             ! arguments
      integer                  ,intent(in)  :: nwih,ndpt        ! arguments
      integer                  ,intent(in)  :: lunprint         ! arguments
      logical                  ,intent(out) :: whoops           ! arguments
      integer                  ,intent(in)  :: nrecords         ! arguments
      integer                  ,intent(in)  :: nbuffer          ! arguments
      character(len=*)         ,intent(in)  :: vartypes         ! arguments
      character(len=*),optional,intent(in)  :: directory        ! arguments
      integer                               :: err,ier          ! local

      allocate (obj)

      if (lunprint > 0) then
           write(lunprint,*) 'TEMPTCACHE: opened.'
           write(lunprint,*) 'TEMPTCACHE: nwih     = ',nwih
           write(lunprint,*) 'TEMPTCACHE: ndpt     = ',ndpt
           write(lunprint,*) 'TEMPTCACHE: nrecords = ',nrecords
           write(lunprint,*) 'TEMPTCACHE: nbuffer  = ',nbuffer
           write(lunprint,*) 'TEMPTCACHE: vartypes = ',trim(vartypes)
      endif

      obj%nwih         = nwih
      obj%ndpt         = ndpt
      obj%lunprint     = lunprint
      obj%nrecords     = nrecords
      obj%nbuffer      = nbuffer
      obj%vartypes     = vartypes
      obj%priority     = 0
      obj%kount_cache  = 0
      obj%kount_rfile  = 0
      obj%kount_wfile  = 0

      nullify (obj%hdd)     ! (nwih,nbuffer)
      nullify (obj%hdi)     ! (nwih,nbuffer)
      nullify (obj%trr)     ! (ndpt,nbuffer)
      nullify (obj%tri)     ! (ndpt,nbuffer)
      nullify (obj%aur)     ! (ndpt,nbuffer)
      nullify (obj%indx)    ! (nrecords)
      nullify (obj%flag)    ! (nbuffer)
      nullify (obj%irec)    ! (nbuffer)
      nullify (obj%temptfile) ! jpa

      if (nwih <= 0 .or. ndpt <= 0 .or. nrecords <= 0 .or. nbuffer <= 0 &
              .or. (vartypes /= 'DR' .and. vartypes /= 'II'  .and.      &
                    vartypes /= 'IR' .and. vartypes /= 'DR8' .and.      &
                    vartypes /= 'DRR') ) then
           if (lunprint > 0) then
                write(lunprint,*) 'TEMPTCACHE: input parameter errors.'
           endif
           whoops = .true.
           return
      endif

      if (vartypes(1:1) == 'D') then
           allocate (obj%hdd(nwih,nbuffer),stat=ier)
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TEMPTCACHE: HDD allocation error'
                endif
                whoops = .true.
                return
           endif
           obj%hdd(:,:) = 0
      endif

      if (vartypes(1:1) == 'I') then
           allocate (obj%hdi(nwih,nbuffer),stat=ier)
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TEMPTCACHE: HDI allocation error'
                endif
                whoops = .true.
                return
           endif
           obj%hdi(:,:) = 0.0
      endif

      if (vartypes(2:2) == 'R') then
           allocate (obj%trr(ndpt,nbuffer),stat=ier)
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TEMPTCACHE: TRR allocation error'
                endif
                whoops = .true.
                return
           endif
           obj%trr(:,:) = 0.0
      endif

      if (vartypes(2:2) == 'I') then
           allocate (obj%tri(ndpt,nbuffer),stat=ier)
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TEMPTCACHE: TRI allocation error'
                endif
                whoops = .true.
                return
           endif
           obj%tri(:,:) = 0
      endif

      if (vartypes == 'DRR') then
           allocate (obj%aur(ndpt,nbuffer),stat=ier)
           if (ier /= 0) then
                if (lunprint > 0) then
                     write(lunprint,*) 'TEMPTCACHE: AUR allocation error'
                endif
                whoops = .true.
                return
           endif
           obj%aur(:,:) = 0.0
      endif

      allocate (obj%indx(nrecords),stat=ier)
      if (ier /= 0) then
           if (lunprint > 0) then
                write(lunprint,*) 'TEMPTCACHE: INDX allocation error'
           endif
           whoops = .true.
           return
      endif

      allocate (obj%flag(nbuffer) ,stat=ier)
      if (ier /= 0) then
           if (lunprint > 0) then
                write(lunprint,*) 'TEMPTCACHE: FLAG allocation error'
           endif
           whoops = .true.
           return
      endif

      allocate (obj%irec(nbuffer) ,stat=ier)
      if (ier /= 0) then
           if (lunprint > 0) then
                write(lunprint,*) 'TEMPTCACHE: IREC allocation error'
           endif
           whoops = .true.
           return
      endif

      obj%indx(:) = -1   ! indx(irec) = index into cache (1-nbuffer).
                         ! indx(irec) = 0 if on disk but not in cache.
                         ! indx(irec) = -1 if not on disk or in cache.

      obj%flag(:) = 0    ! flag(indx) = priority of trace in cache (>= 0).

      obj%irec(:) = 0    ! irec(indx) = record number in cache (1-nrecords).
                         ! irec(indx) = 0 if this location in cache is empty.

      call temptfile_open (obj%temptfile,seed,nwih,ndpt,lunprint,err, &
                                       directory,nrecords,vartypes)
      whoops = (err /= TEMPTFILE_OK)

      end subroutine temptcache_open


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine temptcache_close (obj)

      type(temptcache_struct),pointer :: obj                 ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%hdd))  deallocate (obj%hdd)     ! (nwih,nbuffer)
      if (associated(obj%hdi))  deallocate (obj%hdi)     ! (nwih,nbuffer)
      if (associated(obj%trr))  deallocate (obj%trr)     ! (ndpt,nbuffer)
      if (associated(obj%tri))  deallocate (obj%tri)     ! (ndpt,nbuffer)
      if (associated(obj%aur))  deallocate (obj%aur)     ! (ndpt,nbuffer)
      if (associated(obj%indx)) deallocate (obj%indx)    ! (nrecords)
      if (associated(obj%flag)) deallocate (obj%flag)    ! (nbuffer)
      if (associated(obj%irec)) deallocate (obj%irec)    ! (nbuffer)

      call temptfile_close   (obj%temptfile)

      if (obj%lunprint > 0) then
write(obj%lunprint,*) 'TEMPTCACHE: number of trace records = ',obj%nrecords
write(obj%lunprint,*) 'TEMPTCACHE: trace buffer size       = ',obj%nbuffer
write(obj%lunprint,*) 'TEMPTCACHE: traces found in cache   = ',obj%kount_cache
write(obj%lunprint,*) 'TEMPTCACHE: traces read from file   = ',obj%kount_rfile
write(obj%lunprint,*) 'TEMPTCACHE: traces written to file  = ',obj%kount_wfile
write(obj%lunprint,*) 'TEMPTCACHE: closed.'
      endif

      deallocate (obj)

      end subroutine temptcache_close


!!------------------------ private fetch helper --------------------------!!
!!------------------------ private fetch helper --------------------------!!
!!------------------------ private fetch helper --------------------------!!


      subroutine temptcache_fetch_helper &
                              (obj,vt,irec,indx,recwrite,recread,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      character(len=*)       ,intent(in)    :: vt                 ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      integer                ,intent(out)   :: indx               ! arguments
      integer                ,intent(out)   :: recwrite           ! arguments
      integer                ,intent(out)   :: recread            ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indxa(1)           ! local

!----------return error if variable types are wrong:

      if (vt /= obj%vartypes) then
           if (obj%lunprint > 0) then
                write (obj%lunprint,*) 'TEMPTCACHE: illegal vartypes ',trim(vt)
           endif
           whoops = .true.
           return
      endif

!----------return error if record number is out of range:

      if (irec < 1 .or. irec > obj%nrecords) then
           if (obj%lunprint > 0) then
                write (obj%lunprint,*) 'TEMPTCACHE: illegal record number ',irec
           endif
           whoops = .true.
           return
      endif

!----------check to see if the record is in the cache:

      ! indx(irec = 1:nrecords) = index into cache (1-nbuffer).
      ! indx(irec = 1:nrecords) = 0 if on disk but not in cache.
      ! indx(irec = 1:nrecords) = -1 if not on disk or in cache.

      ! flag(indx = 1:nbuffer) = priority of trace in cache (>= 0).

      ! irec(indx = 1:nbuffer) = record number in cache (1-nrecords).
      ! irec(indx = 1:nbuffer) = 0 if this location in cache is empty.

      indx = obj%indx(irec)          ! index of record in the cache.
      if (indx > 0) then             ! the record is in the cache.
           if (obj%irec(indx) /= irec) then
                if (obj%lunprint > 0) then
                     write (obj%lunprint,*) 'TEMPTCACHE: programming error ', &
                                                irec, obj%irec(indx)
                endif
                whoops = .true.
                return
           endif
           obj%priority    = obj%priority + 1
           obj%flag(indx)  = obj%priority
           obj%kount_cache = obj%kount_cache + 1
           recwrite        = 0       ! no need to write record to disk.
           recread         = 0       ! no need to read record from disk.
           whoops          = .false.
           return
      endif

!----------find the lowest priority spot in the cache:

      indxa = minloc(obj%flag)
      indx  = indxa(1)         ! index of lowest priority spot in the cache.

!----------plan to write low priority record to disk:

      recwrite = obj%irec(indx)    ! record to write from cache to disk (or 0).
      if (recwrite > 0) then       ! indicates record is in the cache.
           obj%indx(recwrite) = 0  ! indicates record on disk but not in cache.
           obj%kount_wfile    = obj%kount_wfile + 1
      endif

!----------then plan to read requested record from disk:

      if (obj%indx(irec) == 0) then     ! indicates record is on disk.
           recread         = irec       ! record to read from disk to cache.
           obj%kount_rfile = obj%kount_rfile + 1
      else  ! if (obj%indx(irec) < 0) then
           recread         = 0          ! no need to read record from disk.
      endif

!----------update priority flag and record number in cache:

      obj%priority   = obj%priority + 1
      obj%flag(indx) = obj%priority
      obj%irec(indx) = irec
      obj%indx(irec) = indx       ! indicates record in cache but not on disk.
      whoops         = .false.

      end subroutine temptcache_fetch_helper


!!------------------------- public fetch routines -------------------------!!
!!------------------------- public fetch routines -------------------------!!
!!------------------------- public fetch routines -------------------------!!


      subroutine temptcache_fetch_dr (obj,irec,hdd,trr,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      double precision       ,pointer       :: hdd(:)             ! arguments
      real                   ,pointer       :: trr(:)             ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indx,err           ! local
      integer                               :: recwrite,recread   ! local

      call temptcache_fetch_helper (obj,'DR',irec,indx,recwrite,recread,whoops)
      if (whoops) return

      hdd => obj%hdd(:,indx)
      trr => obj%trr(:,indx)

      if (recwrite > 0) then        ! indicates old record is in the cache.
           call temptfile_write (obj%temptfile,recwrite,hdd,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      if (recread > 0) then        ! indicates requested record is on disk.
           call temptfile_read (obj%temptfile,recread,hdd,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      end subroutine temptcache_fetch_dr



      subroutine temptcache_fetch_ii (obj,irec,hdi,tri,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      integer                ,pointer       :: hdi(:)             ! arguments
      integer                ,pointer       :: tri(:)             ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indx,err           ! local
      integer                               :: recwrite,recread   ! local

      call temptcache_fetch_helper (obj,'II',irec,indx,recwrite,recread,whoops)
      if (whoops) return

      hdi => obj%hdi(:,indx)
      tri => obj%tri(:,indx)

      if (recwrite > 0) then        ! indicates old record is in the cache.
           call temptfile_write (obj%temptfile,recwrite,hdi,tri,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      if (recread > 0) then        ! indicates requested record is on disk.
           call temptfile_read (obj%temptfile,recread,hdi,tri,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      end subroutine temptcache_fetch_ii



      subroutine temptcache_fetch_ir (obj,irec,hdi,trr,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      integer                ,pointer       :: hdi(:)             ! arguments
      real                   ,pointer       :: trr(:)             ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indx,err           ! local
      integer                               :: recwrite,recread   ! local

      call temptcache_fetch_helper (obj,'IR',irec,indx,recwrite,recread,whoops)
      if (whoops) return

      hdi => obj%hdi(:,indx)
      trr => obj%trr(:,indx)

      if (recwrite > 0) then        ! indicates old record is in the cache.
           call temptfile_write (obj%temptfile,recwrite,hdi,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      if (recread > 0) then        ! indicates requested record is on disk.
           call temptfile_read (obj%temptfile,recread,hdi,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      end subroutine temptcache_fetch_ir



      subroutine temptcache_fetch8 (obj,irec,hdd,trr,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      double precision       ,pointer       :: hdd(:)             ! arguments
      real                   ,pointer       :: trr(:)             ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indx,err           ! local
      integer                               :: recwrite,recread   ! local

      call temptcache_fetch_helper (obj,'DR8',irec,indx,recwrite,recread,whoops)
      if (whoops) return

      hdd => obj%hdd(:,indx)
      trr => obj%trr(:,indx)

      if (recwrite > 0) then        ! indicates old record is in the cache.
           call temptfile_write (obj%temptfile,recwrite,hdd,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      if (recread > 0) then        ! indicates requested record is on disk.
           call temptfile_read (obj%temptfile,recread,hdd,trr,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      end subroutine temptcache_fetch8



      subroutine temptcache_fetch_drr (obj,irec,hdd,trr,aur,whoops)

      type(temptcache_struct),intent(inout) :: obj                ! arguments
      integer                ,intent(in)    :: irec               ! arguments
      double precision       ,pointer       :: hdd(:)             ! arguments
      real                   ,pointer       :: trr(:)             ! arguments
      real                   ,pointer       :: aur(:)             ! arguments
      logical                ,intent(out)   :: whoops             ! arguments
      integer                               :: indx,err           ! local
      integer                               :: recwrite,recread   ! local

      call temptcache_fetch_helper (obj,'DRR',irec,indx,recwrite,recread,whoops)
      if (whoops) return

      hdd => obj%hdd(:,indx)
      trr => obj%trr(:,indx)
      aur => obj%aur(:,indx)

      if (recwrite > 0) then        ! indicates old record is in the cache.
           call temptfile_write (obj%temptfile,recwrite,hdd,trr,aur,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      if (recread > 0) then        ! indicates requested record is on disk.
           call temptfile_read (obj%temptfile,recread,hdd,trr,aur,err)
           if (err /= TEMPTFILE_OK) then
                whoops = .true.
                return
           endif
      endif

      end subroutine temptcache_fetch_drr


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module temptcache_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

