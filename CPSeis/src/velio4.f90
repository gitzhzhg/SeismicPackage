
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velio4.f90 --------------------------------!!
!!------------------------------- velio4.f90 --------------------------------!!
!!------------------------------- velio4.f90 --------------------------------!!


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
! Name       : VELIO4
! Category   : io
! Written    : 2005-11-03   by: Tom Stoeckley
! Revised    : 2010-06-07   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To read velocity functions from TRCIO trace files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading velocity functions from TRCIO trace files.
! The velocity type is not specified in TRCIO trace files.
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
! See if this is a TRCIO trace file:
!
!    valid = velio4_is_trcio (pathname)
!      o                         i    
!
! Open the file:
!                                                     opt opt   opt    opt
!                            o     i      o    o   o   o   o     o      o
!    call velio4_open_read (obj,pathname,nfun,err,msg,nhx,nhy,nmosign,nmoexp,
!                           npicks,delta)
!                             o      o  
!                            opt    opt
!
!                            o     i      o      i     o   o
!    call velio4_open_read (obj,pathname,pjar,secname,err,msg)
!
! Close the file:
!
!    call velio4_close (obj)
!                        b
!
! Get a velocity function:
!                                  opt    opt    opt    opt    opt 
!                              b    o      o      o      o      o   
!    call velio4_read_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!           err,msg,velname,veltype,project,line,rdate,pdate,userid,comment)
!            o   o     o       o       o     o     o     o     o       o   
!                     opt     opt     opt   opt   opt   opt   opt     opt  
!
!                                        opt    opt    opt    opt    opt
!                               b    i    o      o      o      o      o   
!    call velio4_fetch_velfun (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,
!           err,msg,velname,veltype,project,line,rdate,pdate,userid,comment)
!            o   o     o       o       o     o     o     o     o       o   
!                     opt     opt     opt   opt   opt   opt   opt     opt  
!
!-------------------------------------------------------------------------------
!                       TWO WAYS TO OPEN THE FILE
!
! The first VELIO4_OPEN_READ returns all output parameters in the argument
! list.  The second VELIO4_OPEN_READ returns the following output parameters
! in the pickle jar:
!
!                 nfun nhx nhy nmosign nmoexp npicks delta
!
! The pickle jar must be created prior to calling the second VELIO4_OPEN_READ.
!
!-------------------------------------------------------------------------------
!                  TWO WAYS TO GET A VELOCITY FUNCTION
!
! VELIO4_READ_VELFUN returns the next velocity function in the file, and
! and should be called once for each velocity function in the file.
!
! VELIO4_FETCH_VELFUN is an alternate subroutine which can be called for
! any existing location to get any velocity function in the file in any
! random order.
!
! The velocity function returned by either subroutine will have NPICKS
! uniformly sampled picks beginning at zero time (or depth) and incrementing
! by the sample interval DELTA.
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velio4_struct)  obj      = pointer to the VELIO4 data structure.
! char(*)              pathname = name of TRCIO trace file.
! type(pjar_struct)    pjar     = reference to the PJAR data structure.
! char(*)              secname  = PJAR section containing the parameters.
! integer              err      = error flag (returned).
! char(*)              msg      = message for possible printing (returned).
! logical              valid    = whether this is a valid TRCIO trace file.
!
! integer npicks  = number of time/vel picks in the velocity function.
! real    delta   = sample interval (seconds or depth) in the velocity function.
!
! integer nfun      = number of velocity functions in the file = nx * ny.
! integer nhx       = CPS trace header word containing X coordinate (always 7).
! integer nhy       = CPS trace header word containing Y coordinate (always 8).
! real    nmosign   = the   sign   used for the moveout (always set to 1.0).
! real    nmoexp    = the exponent used for the moveout (always set to 2.0).
!
! integer ifun           = desired velocity function number (1 thru nfun).
! real    xcoord         = X coordinate of the velocity function.
! real    ycoord         = Y coordinate of the velocity function.
! real    tpicks(npicks) = array that holds the abscissae (TIME/DEPTH picks).
! real    vpicks(npicks) = array that holds the ordinates (VELOCITY picks).
! char(*) velname        = name of velocity function  (8 characters).
! char(*) veltype        = type of velocity function  (4 characters).
! char(*) project        = project name              (10 characters).
! char(*) line           = line name                 (10 characters).
! char(*) rdate          = recording date             (5 characters).
! char(*) pdate          = processing date            (5 characters).
! char(*) userid         = user ID                    (3 characters).
! char(*) comment        = comment                   (15 characters).
!
! The following arguments are not available on a TRCIO trace file and
! therefore are always returned as shown here:
!
!         nhx       = 7
!         nhy       = 8
!         nmosign   = 1.0
!         nmoexp    = 2.0
!         velname   = 'none'
!         veltype   = ''
!         project   = 'velio'
!         line      = 'none'
!         rdate     = 'none'
!         pdate     = 'none'
!         userid    = 'none'
!         comment   = 'none'
!
! ERR will be set to VELIO4_OK or VELIO4_EOF or VELIO4_ERROR.
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
!  4. 2010-06-07  Stoeckley  Add status='OLD' to open statement in velio4_is_trcio.
!003. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2005-12-22  Stoeckley  Put additional checks into velio4_is_trcio to
!                             keep from calling trcio to check for valid
!                             trcio file because trcio sometimes gets
!                             confused and hangs on a velocity file.
!  1. 2005-11-03  Stoeckley  Initial version.
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
 
 
      module velio4_module
      use pjar_module
      use trcio_module
      use getlun_module
      implicit none
      public

      character(len=100),public,save :: VELIO4_IDENT = &
'$Id: velio4.f90,v 1.3 2006/10/17 13:45:49 Glover prod sps $'

      type,public :: velio4_struct

           private
           type(trcio_struct),pointer :: trcio
           real              ,pointer :: tpicks(:)
           integer                    :: nfun,ifun,nwih,ndpt

      end type velio4_struct

      integer,public,parameter :: VELIO4_OK    = TRCIO_OK
      integer,public,parameter :: VELIO4_EOF   = TRCIO_EOF
      integer,public,parameter :: VELIO4_ERROR = TRCIO_ERROR

      interface velio4_open_read
         module procedure velio4_open_read_args
         module procedure velio4_open_read_pjar
      end interface

      contains


!!-------------------------- velio4 is trcio ------------------------------!!
!!-------------------------- velio4 is trcio ------------------------------!!
!!-------------------------- velio4 is trcio ------------------------------!!


      function velio4_is_trcio (pathname) result (valid)

      character(len=*),intent(in)           :: pathname          ! arguments
      logical                               :: valid             ! result
      type(trcio_struct),pointer            :: trcio             ! local
      integer                               :: err,lun,istat     ! local
      character(len=80)                     :: line              ! local

      valid = .false.
      call getlun (lun)
      open (lun,file=pathname,status='OLD',iostat=istat)
      if (istat /= 0) return
      read (lun, '(A)',iostat=istat) line
      close (lun)
      if (istat /= 0) return
      if (line(1:9) == '*GLOBALS:') return
      if (line(1:24) == '#<CPS_v1 TYPE=VELOCITY/>') return

      trcio => trcio_open (pathname, 'r')

      valid = associated(trcio)

      if (valid) err = trcio_close(trcio)

      end function velio4_is_trcio


!!------------------- velio4 open read (using arguments) --------------------!!
!!------------------- velio4 open read (using arguments) --------------------!!
!!------------------- velio4 open read (using arguments) --------------------!!


      subroutine velio4_open_read_args                                 &
                   (obj,pathname,nfun,err,msg,nhx,nhy,nmosign,nmoexp,  &
                    npicks,delta)

      type(velio4_struct),pointer           :: obj               ! arguments
      character(len=*),intent(in)           :: pathname          ! arguments
      integer         ,intent(out)          :: nfun              ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      integer         ,intent(out),optional :: nhx,nhy           ! arguments
      real            ,intent(out),optional :: nmosign,nmoexp    ! arguments
      integer         ,intent(out),optional :: npicks            ! arguments
      real            ,intent(out),optional :: delta             ! arguments
      type(pjar_struct),pointer             :: pjar              ! local
      character(len=*),parameter :: DEFAULT_SECNAME = 'trcio'    ! local

      nullify (pjar) ! jpa
      call pjar_create (pjar)

      call velio4_open_read (obj,pathname,pjar,DEFAULT_SECNAME,err,msg)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)

                            call pjar_get (pjar, 'nfun'   , nfun   )
      if (present(nhx    )) call pjar_get (pjar, 'nhx'    , nhx    )
      if (present(nhy    )) call pjar_get (pjar, 'nhy'    , nhy    )
      if (present(nmosign)) call pjar_get (pjar, 'nmosign', nmosign)
      if (present(nmoexp )) call pjar_get (pjar, 'nmoexp' , nmoexp )
      if (present(npicks )) call pjar_get (pjar, 'npicks' , npicks )
      if (present(delta  )) call pjar_get (pjar, 'delta'  , delta  )

      call pjar_delete (pjar)

      end subroutine velio4_open_read_args


!!-------------------- velio4 open read (using pickle jar) ------------------!!
!!-------------------- velio4 open read (using pickle jar) ------------------!!
!!-------------------- velio4 open read (using pickle jar) ------------------!!

 
      subroutine velio4_open_read_pjar (obj,pathname,pjar,secname,err,msg)

      type(velio4_struct)      ,pointer       :: obj                ! arguments
      character(len=*)         ,intent(in)    :: pathname           ! arguments
      type(pjar_struct)        ,intent(inout) :: pjar               ! arguments
      character(len=*)         ,intent(in)    :: secname            ! arguments
      integer                  ,intent(out)   :: err                ! arguments
      character(len=*)         ,intent(out)   :: msg                ! arguments
      real                                    :: delta,tstrt        ! local
      integer                                 :: ipick              ! local

!----------initialize data structure:

      allocate (obj)
      nullify (obj%trcio)
      nullify (obj%tpicks)
      obj%nfun = 0
      obj%ifun = 0
      obj%nwih = 0
      obj%ndpt = 0

!----------see if this appears to be a TRCIO trace file:

      if (.not.velio4_is_trcio(pathname)) then
           err = VELIO4_ERROR
           msg = 'this is not a TRCIO trace file'
           return
      end if

!----------open the trcio file:

      obj%trcio => trcio_open (pathname, 'r')

      if (.not.associated(obj%trcio)) then
           err = VELIO4_ERROR
           msg = 'error opening the TRCIO trace file'
           return
      end if

!----------get information from trcio file:

      obj%nwih  = obj%trcio%nwih
      obj%ndpt  = obj%trcio%num_values
      obj%nfun  = obj%trcio%num_traces
      delta     = obj%trcio%dt
      tstrt     = obj%trcio%tmin

      if (tstrt /= 0.0) then
           call velio4_close (obj)
           err = VELIO4_ERROR
           msg = 'TRCIO trace file does not start at zero time'
           return
      endif

      allocate (obj%tpicks(obj%ndpt))

      do ipick = 1,obj%ndpt
           obj%tpicks(ipick) = (ipick - 1) * delta
      enddo

!----------load pickle jar:
 
      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar,'nfun'     ,obj%nfun)
      call pjar_put            (pjar,'nhx'      ,7)
      call pjar_put            (pjar,'nhy'      ,8)
      call pjar_put            (pjar,'nmosign'  ,1.0)
      call pjar_put            (pjar,'nmoexp'   ,2.0)
      call pjar_put            (pjar,'npicks'   ,obj%ndpt)
      call pjar_put            (pjar,'delta'    ,delta)

!----------finish up and return:

      err = VELIO4_OK
      msg = 'TRCIO trace file successfully opened'

      end subroutine velio4_open_read_pjar
 

!!------------------------- velio4 close ----------------------------------!!
!!------------------------- velio4 close ----------------------------------!!
!!------------------------- velio4 close ----------------------------------!!
 
 
      subroutine velio4_close (obj)

      type(velio4_struct),pointer :: obj                     ! arguments
      integer                     :: err                     ! local
 
      if (.not.associated(obj)) return

      if (associated(obj%trcio)) err = trcio_close (obj%trcio)

      deallocate (obj)

      end subroutine velio4_close
 

!!--------------------------- velio4 read velfun -------------------------!!
!!--------------------------- velio4 read velfun -------------------------!!
!!--------------------------- velio4 read velfun -------------------------!!


      subroutine velio4_read_velfun                                   &
                           (obj,xcoord,ycoord,npicks,tpicks,vpicks,   &
                            err,msg,velname,veltype,                  &
                            project,line,rdate,pdate,userid,comment)

      type(velio4_struct),intent(inout)        :: obj              ! arguments
      real               ,intent(out),optional :: xcoord,ycoord    ! arguments
      integer            ,intent(out),optional :: npicks           ! arguments
      real               ,intent(out),optional :: tpicks(:)        ! arguments
      real               ,intent(out),optional :: vpicks(:)        ! arguments
      integer            ,intent(out)          :: err              ! arguments
      character(len=*)   ,intent(out)          :: msg              ! arguments
      character(len=*)   ,intent(out),optional :: velname,veltype  ! arguments
      character(len=*)   ,intent(out),optional :: project,line     ! arguments
      character(len=*)   ,intent(out),optional :: rdate,pdate      ! arguments
      character(len=*)   ,intent(out),optional :: userid           ! arguments
      character(len=*)   ,intent(out),optional :: comment          ! arguments

      obj%ifun = obj%ifun + 1

      call velio4_fetch_velfun                                            &
                      (obj,obj%ifun,xcoord,ycoord,npicks,tpicks,vpicks,   &
                       err,msg,velname,veltype,                           &
                       project,line,rdate,pdate,userid,comment)

      end subroutine velio4_read_velfun


!!------------------------- velio4 fetch velfun --------------------------!!
!!------------------------- velio4 fetch velfun --------------------------!!
!!------------------------- velio4 fetch velfun --------------------------!!


      subroutine velio4_fetch_velfun                                       &
                           (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,   &
                            err,msg,velname,veltype,                       &
                            project,line,rdate,pdate,userid,comment)

      type(velio4_struct),intent(inout)        :: obj              ! arguments
      integer            ,intent(in)           :: ifun             ! arguments
      real               ,intent(out),optional :: xcoord,ycoord    ! arguments
      integer            ,intent(out),optional :: npicks           ! arguments
      real               ,intent(out),optional :: tpicks(:)        ! arguments
      real               ,intent(out),optional :: vpicks(:)        ! arguments
      integer            ,intent(out)          :: err              ! arguments
      character(len=*)   ,intent(out)          :: msg              ! arguments
      character(len=*)   ,intent(out),optional :: velname,veltype  ! arguments
      character(len=*)   ,intent(out),optional :: project,line     ! arguments
      character(len=*)   ,intent(out),optional :: rdate,pdate      ! arguments
      character(len=*)   ,intent(out),optional :: userid           ! arguments
      character(len=*)   ,intent(out),optional :: comment          ! arguments
      double precision                         :: hd(obj%nwih)     ! local
      real                                     :: tr(obj%ndpt)     ! local

      if (present(xcoord )) xcoord  = 0.0
      if (present(ycoord )) ycoord  = 0.0
      if (present(npicks )) npicks  = 0
      if (present(velname)) velname = 'none'
      if (present(veltype)) veltype = ''
      if (present(project)) project = 'velio'
      if (present(line   )) line    = 'none'
      if (present(rdate  )) rdate   = 'none'
      if (present(pdate  )) pdate   = 'none'
      if (present(userid )) userid  = 'none'
      if (present(comment)) comment = 'none'

      if (ifun < 1 .or. ifun > obj%nfun) then
           err = VELIO4_EOF
           msg = 'EOF encountered on TRCIO trace file'
           return
      end if

      err = trcio_read_trace (obj%trcio, hd, tr, ifun)

      if (err /= VELIO4_OK) then
           err = VELIO4_ERROR
           msg = 'error reading velocity function from TRCIO trace file'
           return
      end if

      if (present(xcoord)) xcoord             = hd(7)
      if (present(ycoord)) ycoord             = hd(8)
      if (present(npicks)) npicks             = obj%ndpt
      if (present(tpicks)) tpicks(1:obj%ndpt) = obj%tpicks(:)
      if (present(vpicks)) vpicks(1:obj%ndpt) = tr(:)

      err = VELIO4_OK
      msg = 'velocity function successfully read from TRCIO trace file'

      end subroutine velio4_fetch_velfun


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velio4_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
