
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- statshift.f90 ------------------------------!!
!!---------------------------- statshift.f90 ------------------------------!!
!!---------------------------- statshift.f90 ------------------------------!!

 
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
! Name       : STATSHIFT
! Category   : math
! Written    : 2000-05-22   by: Tom Stoeckley
! Revised    : 2000-09-07   by: Tom Stoeckley
! Maturity   : production   2000-09-27
! Purpose    : Save traces for static shift and output.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used by surface-consistent statics programs to save
! traces when received, and then to shift and output the traces after the
! statics solution is finished.
!
! This primitive uses the TEMPTFILE primitive to temporarily store traces.
! This primitive uses the SHFT process to shift traces.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2000-09-27  Stoeckley  Fix typo which did not output entire trace length.
!  1. 2000-08-23  Stoeckley  Initial version.
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
!                            o    i     i     i    i     i      i     i    o
!    call statshift_open   (obj, seed, nwih, ndpt, dt, path1, path2, lun, err)
!
!                            b   i   i    o
!    call statshift_write  (obj, hd, tr, err)
!    call statshift_read   (obj, hd, tr, err)
!                            b   o   o    o
!
!                            b
!    call statshift_close  (obj)
!    call statshift_rewind (obj)
!
!
! type(statshift_struct)  obj = pointer to the STATSHIFT structure.
! character(len=*)       seed = base name for creating temporary filename.
! integer                nwih = number of words in each trace header in HD.
! integer                ndpt = number of samples in each trace in TR.
! real                     dt = trace sample interval in seconds.
! character(len=*)      path1 = first static file to use for shifting traces.
! character(len=*)      path2 = second static file to use for shifting traces.
! integer                 lun = unit number for printing (or 0).
! integer                 err = STATSHIFT_ERROR (if error) or STATSHIFT_OK or
!                                STATSHIFT_EOF (if endfile is read).
! double precision      hd(:) = header word array.
! real                  tr(:) = trace array.
!
! STATSHIFT_OPEN:
! Open a temporary file to save traces.
!
! STATSHIFT_WRITE:
! Write a trace to the temporary file.
!
! STATSHIFT_REWIND:
! Rewind the temporary file.
!
! STATSHIFT_READ:
! Read a trace from the temporary file and shift it.
! Sets trace and header arrays to zero if error or endfile is encountered.
!
! STATSHIFT_CLOSE:
! Close the temporary file.
!
! If an error occurs, the structure is deallocated.
!
! If a deallocated structure is passed to any routine except STATSHIFT_OPEN,
! nothing occurs except ERR (if present) is set to STATSHIFT_ERROR.
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


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module statshift_module
      use named_constants_module
      use pc_module
      use temptfile_module
      use shft_module
      implicit none
      private
      public :: statshift_open
      public :: statshift_write
      public :: statshift_read
      public :: statshift_close
      public :: statshift_rewind

      character(len=100),public,save :: statshift_IDENT = &
       '$Id: statshift.f90,v 1.2 2000/09/27 15:17:41 sps prod sps $'

      type,public :: statshift_struct
           private
           type(temptfile_struct),pointer :: temptfile
           type(shft_struct)     ,pointer :: shft
           integer                        :: nwih,ndpt,lun
      end type statshift_struct

      integer,public,parameter :: STATSHIFT_OK    = TEMPTFILE_OK
      integer,public,parameter :: STATSHIFT_ERROR = TEMPTFILE_ERROR
      integer,public,parameter :: STATSHIFT_EOF   = TEMPTFILE_EOF


      contains


!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!


      subroutine statshift_open (obj,seed,nwih,ndpt,dt,path1,path2,lun,err)
      implicit none
      type(statshift_struct),pointer  :: obj                 ! arguments
      character(len=*),intent(in)     :: seed                ! arguments
      integer         ,intent(in)     :: nwih,ndpt           ! arguments
      real            ,intent(in)     :: dt                  ! arguments
      character(len=*),intent(in)     :: path1,path2         ! arguments
      integer         ,intent(in)     :: lun                 ! arguments
      integer         ,intent(out)    :: err                 ! arguments

      allocate (obj)
      nullify  (obj%temptfile)
      nullify  (obj%shft)

      obj%nwih = nwih
      obj%ndpt = ndpt
      obj%lun  = lun 

      call temptfile_open (obj%temptfile,seed,nwih,ndpt,lun,err)
      if (err == TEMPTFILE_OK) then
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: SUCCESSFULLY OPENED TEMPORARY TRACE FILE'
      else
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: FATAL ERROR OPENING TEMPORARY TRACE FILE'
           call statshift_close (obj)
           err = STATSHIFT_ERROR
           return
      end if

      call pc_backend_update (obj%lun)
      call pc_put_global     ('nwih'      , nwih)
      call pc_put_global     ('ndpt'      , ndpt)
      call pc_put_global     ('dt'        , dt)
      call pc_put_process    ('pathname_1', path1)
      call pc_put_process    ('pathname_2', path2)
      call shft_create       (obj%shft)
      if (pc_update_error()) then
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: FATAL ERROR CREATING INTERNAL SHFT PROCESS'
           call statshift_close (obj)
           err = STATSHIFT_ERROR
      else
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: SUCCESSFULLY CREATED INTERNAL SHFT PROCESS'
           err = STATSHIFT_OK
      end if
      call pc_restore
      return
      end subroutine statshift_open


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine statshift_close (obj)
      implicit none
      type(statshift_struct),pointer :: obj                 ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%temptfile)) then
           call temptfile_close    (obj%temptfile)
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: CLOSED TEMPORARY TRACE FILE'
      end if

      if (associated(obj%shft)) then
           call pc_backend_update  (obj%lun)
           call pc_backend_execute
           call shft_delete        (obj%shft)
           call pc_restore
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: DELETED INTERNAL SHFT PROCESS'
      end if

      deallocate (obj)
      return
      end subroutine statshift_close


!!------------------------------- rewind ----------------------------------!!
!!------------------------------- rewind ----------------------------------!!
!!------------------------------- rewind ----------------------------------!!


      subroutine statshift_rewind (obj)
      implicit none
      type(statshift_struct),pointer       :: obj               ! arguments

      if (.not.associated(obj)) return

      call temptfile_rewind (obj%temptfile)
      if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: REWOUND TEMPORARY TRACE FILE'
      return
      end subroutine statshift_rewind


!!----------------------------- write ------------------------------------!!
!!----------------------------- write ------------------------------------!!
!!----------------------------- write ------------------------------------!!


      subroutine statshift_write (obj,hd,tr,err)
      implicit none
      type(statshift_struct),pointer       :: obj                 ! arguments
      double precision      ,intent(in)    :: hd(:)               ! arguments
      real                  ,intent(in)    :: tr(:)               ! arguments
      integer               ,intent(out)   :: err                 ! arguments

      if (.not.associated(obj)) then
           err = STATSHIFT_ERROR
           return
      end if

      call temptfile_write (obj%temptfile,0,hd,tr,err)
      if (err == TEMPTFILE_OK) then
           err = STATSHIFT_OK
      else
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: FATAL ERROR WRITING TO TEMPORARY TRACE FILE'
           call statshift_close (obj)
           err = STATSHIFT_ERROR
      end if
      return
      end subroutine statshift_write


!!----------------------------- read ------------------------------------!!
!!----------------------------- read ------------------------------------!!
!!----------------------------- read ------------------------------------!!


      subroutine statshift_read (obj,hd,tr,err)
      implicit none
      type(statshift_struct),pointer       :: obj                 ! arguments
      double precision      ,intent(out)   :: hd(:)               ! arguments
      real                  ,intent(out)   :: tr(:)               ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      double precision                     :: hdum(obj%nwih,1)    ! local
      real                                 :: tdum(obj%ndpt,1)    ! local
      integer                              :: ntr                 ! local

      if (.not.associated(obj)) then
           hd(:) = 0.0
           tr(:) = 0.0
           err = STATSHIFT_ERROR
           return
      end if

      call temptfile_read (obj%temptfile,0,hdum(1:,1),tdum(1:,1),err)
      if (err == TEMPTFILE_EOF) then
           hd(1:obj%nwih) = 0.0
           tr(1:obj%ndpt) = 0.0
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: ENDFILE ENCOUNTERED ON TEMPORARY TRACE FILE'
           err = STATSHIFT_EOF
           return
      else if (err == TEMPTFILE_ERROR) then
           hd(1:obj%nwih) = 0.0
           tr(1:obj%ndpt) = 0.0
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: FATAL ERROR READING TEMPORARY TRACE FILE'
           call statshift_close (obj)
           err = STATSHIFT_ERROR
           return
      end if

      ntr = 1
      call shft (obj%shft,ntr,hdum,tdum)
      if (ntr == FATAL_ERROR) then
           hd(1:obj%nwih) = 0.0
           tr(1:obj%ndpt) = 0.0
           if (obj%lun > 0) write (obj%lun,*) &
                     'STATSHIFT: FATAL ERROR CALLING INTERNAL SHFT PROCESS'
           call statshift_close (obj)
           err = STATSHIFT_ERROR
      else
           hd(1:obj%nwih) = hdum(1:obj%nwih,1)
           tr(1:obj%ndpt) = tdum(1:obj%ndpt,1)
           err = STATSHIFT_OK
      end if
      return
      end subroutine statshift_read


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module statshift_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

