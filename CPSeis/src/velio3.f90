
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velio3.f90 --------------------------------!!
!!------------------------------- velio3.f90 --------------------------------!!
!!------------------------------- velio3.f90 --------------------------------!!


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
! Name       : VELIO3
! Category   : io
! Written    : 2004-11-02   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : To read velocity functions from modspec files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading velocity functions from modspec files.
! These are always interval velocities, but may be in either time or depth.
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
! See if this is a modspec file:
!
!    valid = velio3_is_modspec (pathname)
!      o                           i    
!
! Open the file:
!                                                     opt opt   opt    opt
!                            o     i      o    o   o   o   o     o      o
!    call velio3_open_read (obj,pathname,nfun,err,msg,nhx,nhy,nmosign,nmoexp,
!                           npicks,delta,veltype, x1,y1, xinc,yinc, nx,ny,nlay)
!                             i      i      i     o  o     o   o    o  o    o
!                            opt    opt    opt   opt opt  opt opt  opt opt opt
!
!                            o     i      o      i     o   o
!    call velio3_open_read (obj,pathname,pjar,secname,err,msg,
!                           npicks,delta,veltype)
!                             i      i      i
!                            opt    opt    opt
!
! Close the file:
!
!    call velio3_close (obj)
!                        b
!
! Get a velocity function:
!                                  opt    opt    opt    opt    opt 
!                              b    o      o      o      o      o   
!    call velio3_read_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!           err,msg,velname,veltype,project,line,rdate,pdate,userid,comment,
!            o   o     o       o       o     o     o     o     o       o   
!                     opt     opt     opt   opt   opt   opt   opt     opt  
!
!           nlay,zlay,vlay,glay)
!            o    o    o    o
!           opt  opt  opt  opt
!
!                                        opt    opt    opt    opt    opt
!                               b    i    o      o      o      o      o   
!    call velio3_fetch_velfun (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,
!           err,msg,velname,veltype,project,line,rdate,pdate,userid,comment,
!            o   o     o       o       o     o     o     o     o       o   
!                     opt     opt     opt   opt   opt   opt   opt     opt  
!
!           nlay,zlay,vlay,glay)
!            o    o    o    o
!           opt  opt  opt  opt
!
!-------------------------------------------------------------------------------
!                       TWO WAYS TO OPEN THE FILE
!
! The first VELIO3_OPEN_READ returns all output parameters in the argument
! list.  The second VELIO3_OPEN_READ returns the following output parameters
! in the pickle jar:
!
!              nfun nhx nhy nmosign nmoexp x1 y1 xinc yinc nx ny nlay
!
! The pickle jar must be created prior to calling the second VELIO3_OPEN_READ.
!
!-------------------------------------------------------------------------------
!                  TWO WAYS TO GET A VELOCITY FUNCTION
!
! VELIO3_READ_VELFUN returns the next velocity function in the grid (xcoord
! changing fastest), and should be called once for each velocity function
! in the grid.
!
! VELIO3_FETCH_VELFUN is an alternate subroutine which can be called for
! any existing location to get any velocity function in the grid in any
! random order.
!
! The velocity function returned by either subroutine will have NPICKS
! uniformly sampled picks beginning at zero time (or depth) and incrementing
! by the sample interval DELTA.  If DELTA is zero, it will be reset so that
! the last pick will be at the deepest time or depth of all the velocities
! on the file.
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velio3_struct)  obj      = pointer to the VELIO3 data structure.
! char(*)              pathname = name of modspec file.
! type(pjar_struct)    pjar     = reference to the PJAR data structure.
! char(*)              secname  = PJAR section containing the parameters.
! integer              err      = error flag (returned).
! char(*)              msg      = message for possible printing (returned).
! logical              valid    = whether this is a valid modspec file.
!
! integer npicks  = desired number of time/vel picks in the velocity function.
! real    delta   = sample interval (seconds or depth) in the velocity function.
! char(*) veltype = desired velocity function type to return.
!
! integer nfun      = number of velocity functions in the file = nx * ny.
! integer nhx       = CPS trace header word containing X coordinate.
! integer nhy       = CPS trace header word containing Y coordinate.
! real    nmosign   = the   sign   used for the moveout (always set to 1.0).
! real    nmoexp    = the exponent used for the moveout (always set to 2.0).
! real    x1,y1     = first X and Y coordinates.
! real    xinc,yinc = increments in X and Y coordinates.
! real    nx,ny     = number of X and Y coordinates.
! real    nlay      = number of layers.
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
! real    zlay(nlay)     = array that holds the layer time/depths.
! real    vlay(nlay)     = array that holds the layer velocities.
! real    glay(nlay)     = array that holds the layer gradients.
!
! Default value of NPICKS  is 100.
! Default value of DELTA   is 0.
! Default value of VELTYPE is a blank string.
!
! If DELTA is zero, the velocity function will be sampled uniformly down
! to the deepest time or depth of all the velocities on the file.
!
! If VELTYPE is specified to VELIO3_OPEN_READ, and is not blank, the returned
! velocity functions will be converted to the specified type.  Otherwise,
! the velocity functions will be returned as they exist on the modspec file
! (veltype VZIN or VTIN).
!
! ERR will be set to VELIO3_OK or VELIO3_EOF or VELIO3_ERROR.
!
! XCOORD = X1 + (IX-1) * XINC  where IX goes from 1 thru NX.
! YCOORD = Y1 + (IY-1) * YINC  where IY goes from 1 thru NY.
!
! ZLAY, VLAY, and GLAY are the un-resampled values from the modspec file.
! If VELTYPE is specified, ZLAY will be converted from the modspec values
! to time or depth if VELTYPE specifies an abscissa in time and the
! modspec value is in depth, or vice versa.  However, VLAY will be the
! unconverted modspec value (interval velocity) whether or not VELTYPE
! specifies a different type of velocity.
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
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!003. 2006-01-10  B. Menger  Removed Unused Variables.
!  2. 2005-02-21  Stoeckley  Add optional arguments NLAY,ZLAY,VLAY,GLAY.
!  1. 2004-11-02  Stoeckley  Initial version.
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
 
 
      module velio3_module
      use string_module
      use pjar_module
      use modspec_module
      use mth_module
      use dio_module
      use velutil_module
      implicit none
      public

      character(len=100),public,save :: VELIO3_IDENT = &
'$Id: velio3.f90,v 1.4 2006/10/17 13:45:49 Glover prod sps $'

      type,public :: velio3_struct

           private
           type(modspec_struct),pointer :: modspec
           character(len=4)             :: veltype_in,veltype_out
           integer                      :: nfun,npicks,nx,ny,nlay,kount
           real                         :: x1,y1,xinc,yinc,delta,sec2ms

      end type velio3_struct

      integer,public,parameter :: VELIO3_OK    = DIO_OK
      integer,public,parameter :: VELIO3_EOF   = DIO_EOF
      integer,public,parameter :: VELIO3_ERROR = DIO_ERROR

      interface velio3_open_read
         module procedure velio3_open_read_args
         module procedure velio3_open_read_pjar
      end interface

      contains


!!-------------------------- velio3 is modspec ------------------------------!!
!!-------------------------- velio3 is modspec ------------------------------!!
!!-------------------------- velio3 is modspec ------------------------------!!


      function velio3_is_modspec (pathname) result (valid)


      character(len=*),intent(in)           :: pathname          ! arguments
      logical                               :: valid             ! result
      type(dio_struct),pointer              :: dio               ! local
      integer                               :: err               ! local
      character(len=80)                     :: msg,card          ! local

      nullify (dio) ! jpa
      call dio_open_read (dio,pathname,err,msg)

      if (err /= DIO_OK) then
           call dio_close (dio)
           valid = .false.
           return
      end if

      do
           call dio_read_card (dio,card)
           call dio_status    (dio,err,msg)

           if (err /= DIO_OK) then
                call dio_close (dio)
                valid = .false.
                return
           end if

           if (card(1:2) == '* ') then
                call dio_close (dio)
                exit
           else if (card == ' ') then
                cycle
           else
                call dio_close (dio)
                valid = .false.
                return
           end if

      end do

      valid = .true.

      end function velio3_is_modspec


!!------------------- velio3 open read (using arguments) --------------------!!
!!------------------- velio3 open read (using arguments) --------------------!!
!!------------------- velio3 open read (using arguments) --------------------!!


      subroutine velio3_open_read_args                                 &
                   (obj,pathname,nfun,err,msg,nhx,nhy,nmosign,nmoexp,  &
                    npicks,delta,veltype,x1,y1,xinc,yinc,nx,ny,nlay)

      type(velio3_struct),pointer           :: obj               ! arguments
      character(len=*),intent(in)           :: pathname          ! arguments
      integer         ,intent(out)          :: nfun              ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      integer         ,intent(out),optional :: nhx,nhy           ! arguments
      real            ,intent(out),optional :: nmosign,nmoexp    ! arguments
      integer         ,intent(in) ,optional :: npicks            ! arguments
      real            ,intent(in) ,optional :: delta             ! arguments
      character(len=*),intent(in) ,optional :: veltype           ! arguments
      real            ,intent(out),optional :: x1,y1,xinc,yinc   ! arguments
      integer         ,intent(out),optional :: nx,ny,nlay        ! arguments
      type(pjar_struct),pointer             :: pjar              ! local
      character(len=*),parameter :: DEFAULT_SECNAME = 'modspec'  ! local

      nullify (pjar) ! jpa
      call pjar_create (pjar)

      call velio3_open_read (obj,pathname,pjar,DEFAULT_SECNAME,err,msg, &
                             npicks,delta,veltype)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)

                            call pjar_get (pjar, 'nfun'   , nfun   )
      if (present(nhx    )) call pjar_get (pjar, 'nhx'    , nhx    )
      if (present(nhy    )) call pjar_get (pjar, 'nhy'    , nhy    )
      if (present(nmosign)) call pjar_get (pjar, 'nmosign', nmosign)
      if (present(nmoexp )) call pjar_get (pjar, 'nmoexp' , nmoexp )
      if (present(x1     )) call pjar_get (pjar, 'x1'     , x1     )
      if (present(y1     )) call pjar_get (pjar, 'y1'     , y1     )
      if (present(xinc   )) call pjar_get (pjar, 'xinc'   , xinc   )
      if (present(yinc   )) call pjar_get (pjar, 'yinc'   , yinc   )
      if (present(nx     )) call pjar_get (pjar, 'nx'     , nx     )
      if (present(ny     )) call pjar_get (pjar, 'ny'     , ny     )
      if (present(ny     )) call pjar_get (pjar, 'nlay'   , nlay   )

      call pjar_delete (pjar)

      end subroutine velio3_open_read_args


!!-------------------- velio3 open read (using pickle jar) ------------------!!
!!-------------------- velio3 open read (using pickle jar) ------------------!!
!!-------------------- velio3 open read (using pickle jar) ------------------!!

 
      subroutine velio3_open_read_pjar (obj,pathname,pjar,secname,err,msg, &
                                        npicks,delta,veltype)

      type(velio3_struct)      ,pointer       :: obj                ! arguments
      character(len=*)         ,intent(in)    :: pathname           ! arguments
      type(pjar_struct)        ,intent(inout) :: pjar               ! arguments
      character(len=*)         ,intent(in)    :: secname            ! arguments
      integer                  ,intent(out)   :: err                ! arguments
      character(len=*)         ,intent(out)   :: msg                ! arguments
      integer         ,optional,intent(in)    :: npicks             ! arguments
      real            ,optional,intent(in)    :: delta              ! arguments
      character(len=*),optional,intent(in)    :: veltype            ! arguments

      character(len=8)                        :: units              ! local
      real                                    :: angle              ! local
      integer                                 :: nhx,nhy,nhz,nz     ! local
      double precision                        :: ox,oy              ! local
      real                                    :: oz,dx,dy,dz        ! local
      real                                    :: xo,yo              ! local
      real                                    :: xgo,ygo,dx2,dy2    ! local
      real                                    :: a(3),b(3)          ! local
      logical                                 :: has_coef           ! local
      integer                                 :: ix,iy      ,i ! local
      integer                                 :: ix1,ix2,ix3,ix4    ! local
      integer                                 :: iy1,iy2,iy3,iy4    ! local
      integer                                 :: nfun,i1,i2,i3,i4   ! local
      integer                                 :: starting_domain    ! local
      integer                                 :: new_domain         ! local
      real                                    :: xlast,ylast,znon   ! local
      character(len=5)                        :: timedepth,velocity ! local
      integer                              :: kountz,kountv,kountg  ! local
      integer,parameter                       :: LUNPRINT = 6       ! local
      real   ,parameter                       :: NMOSIGN  = 1.0     ! local
      real   ,parameter                       :: NMOEXP   = 2.0     ! local
      real                           ,pointer :: z(:,:,:)           ! local
      real                           ,pointer :: v(:,:,:)           ! local
      real                           ,pointer :: g(:,:,:)           ! local

!----------get started:

      allocate (obj)
      nullify (obj%modspec) ! jpa

      obj%nfun        = 0
      obj%npicks      = 100
      obj%delta       = 0.0 
      obj%sec2ms      = 1.0 
      obj%nx          = 0
      obj%ny          = 0
      obj%nlay        = 0
      obj%x1          = 0.0
      obj%y1          = 0.0
      obj%xinc        = 1.0
      obj%yinc        = 1.0
      obj%kount       = 0
      obj%veltype_in  = ' '
      obj%veltype_out = ' '

      if (present(npicks )) obj%npicks      = npicks
      if (present(delta  )) obj%delta       = delta 
      if (present(veltype)) obj%veltype_out = veltype 

      nullify (z)
      nullify (v)
      nullify (g)

!----------see if this appears to be a modspec file:

      if (.not.velio3_is_modspec(pathname)) then
           err = VELIO3_ERROR
           msg = 'this is not a modspec file'
           return
      end if

!----------create the modspec object:

      call modspec_create (obj%modspec,LUNPRINT,pathname,'GRID')

      if (.not.associated(obj%modspec)) then
           err = VELIO3_ERROR
           msg = 'error creating modspec object'
           return
      end if

!----------get info from modspec file:

      call modspec_getdesc (obj%modspec,obj%nlay,angle,units, &
                            obj%nx,ox,dx,obj%ny,oy,dy,nz,oz,dz)

      if (obj%nlay == 0) then
           err = VELIO3_ERROR
           msg = 'no horizons on modspec file'
           return
      end if

      call modspec_get_hdrs (obj%modspec,nhx,nhy,nhz)

      obj%nfun     = obj%nx * obj%ny
      obj%x1       = ox
      obj%y1       = oy
      obj%xinc     = dx
      obj%yinc     = dy

      has_coef = modspec_get_coef(obj%modspec,a,b,xo,yo,dx2,dy2,xgo,ygo)

      if (has_coef) then
           obj%x1   = xo
           obj%y1   = yo
           obj%xinc = dx2
           obj%yinc = dy2
      end if

      starting_domain = modspec_get_domain (obj%modspec)
      znon            = modspec_get_znon   (obj%modspec)

      call modspec_get_zvg (obj%modspec,z,v,g)

!----------count the nils in the file:

      kountz = 0
      kountv = 0
      kountg = 0

      do i = 1,obj%nlay
      do ix = 1,obj%nx
      do iy = 1,obj%ny
           if (z(i,ix,iy) == znon) kountz = kountz + 1
           if (v(i,ix,iy) == znon) kountv = kountv + 1
           if (g(i,ix,iy) == znon) kountg = kountg + 1
      end do
      end do
      end do

!----------print information:

      xlast = obj%x1 + (obj%nx - 1) * obj%xinc
      ylast = obj%y1 + (obj%ny - 1) * obj%yinc

      write (LUNPRINT,*) ' '
      write (LUNPRINT,*) 'modspec file = ',trim(pathname)
      write (LUNPRINT,*) 'x and y header words            = ',nhx,nhy
      write (LUNPRINT,*) 'first x and y coordinates       = ',obj%x1,obj%y1
      write (LUNPRINT,*) 'x and y coord increments        = ',obj%xinc,obj%yinc
      write (LUNPRINT,*) 'last x and y coordinates        = ',xlast,ylast
      write (LUNPRINT,*) 'number of x and y locations     = ',obj%nx,obj%ny
      write (LUNPRINT,*) 'number of layers                = ',obj%nlay
      write (LUNPRINT,*) 'coefficients a1,a2,a3           = ',a
      write (LUNPRINT,*) 'coefficients b1,b2,b3           = ',b
      write (LUNPRINT,*) 'nil value on file (znon)        = ',znon
      write (LUNPRINT,*) 'number of nil time/depth values = ',kountz
      write (LUNPRINT,*) 'number of nil velocity values   = ',kountv
      write (LUNPRINT,*) 'number of nil gradient values   = ',kountg
      write (LUNPRINT,*) ' '

      if (starting_domain == MODSPEC_DEPTH) then
           write (LUNPRINT,*) 'modspec file is in depth domain (VZIN).'
      else if (starting_domain == MODSPEC_TIME) then
           write (LUNPRINT,*) 'modspec file is in time domain (VTIN).'
      else
           write (LUNPRINT,*) 'illegal modspec file domain.'
      end if
      write (LUNPRINT,*) ' '

!----------return if there are any nil values in the file:

      if (kountz > 0 .or. kountv > 0 .or. kountg > 0) then
           err = VELIO3_ERROR
           msg = 'modspec file must not contain any nil values'
           return
      end if

!----------convert between time and depth if requested:

      if (obj%veltype_out(1:2) == 'VZ') then
           new_domain = MODSPEC_DEPTH
      else if (obj%veltype_out(1:2) == 'VT') then
           new_domain = MODSPEC_TIME
      else
           new_domain = starting_domain
      end if

      call modspec_tdconv  (obj%modspec,new_domain)
      call modspec_get_zvg (obj%modspec,z,v,g)

      if (new_domain == MODSPEC_DEPTH) then
           obj%veltype_in = 'VZIN'
           obj%sec2ms = 1.0
      else
           obj%veltype_in = 'VTIN'
           obj%sec2ms = 1000.0
      end if

      if (obj%veltype_out == ' ') obj%veltype_out = obj%veltype_in

!----------get sample interval if necessary:

      if (obj%delta <= 0.0) then
           obj%delta = maxval(z) / max(obj%npicks-1, 1) / obj%sec2ms
      end if

!----------print more information:

      write (LUNPRINT,*) ' '
      if (new_domain == starting_domain) then
           continue
      else if (new_domain == MODSPEC_DEPTH) then
           write (LUNPRINT,*) 'modspec file converted to depth domain (VZIN).'
      else if (new_domain == MODSPEC_TIME) then
           write (LUNPRINT,*) 'modspec file converted to time domain (VTIN).'
      end if

   write (LUNPRINT,*) 'input velocity function type =  ',trim(obj%veltype_in)
   write (LUNPRINT,*) 'output velocity function type = ',trim(obj%veltype_out)
   write (LUNPRINT,*) 'outputting ',obj%npicks,' picks at sample interval ', &
                                                            obj%delta

      nfun = obj%nx * obj%ny
      i1   = 1
      i2   = obj%nx
      i3   = nfun - obj%nx + 1
      i4   = nfun

      ix1 = 1
      ix2 = obj%nx
      ix3 = 1
      ix4 = obj%nx

      iy1 = 1
      iy2 = 1
      iy3 = obj%ny
      iy4 = obj%ny

      if (obj%veltype_in(1:2) == 'VT') then
           timedepth = ' time'
      else
           timedepth = 'depth'
      end if

      if (obj%veltype_in(3:4) == 'IN') then
           velocity = 'VINT-'
      else if (veltype(3:4) == 'AV') then
           velocity = 'VAV--'
      else if (veltype(3:4) == 'RM') then
           velocity = 'VRMS-'
      else if (veltype(3:4) == 'NM') then
           velocity = 'VNMO-'
      else
           velocity = 'VEL--'
      end if

      write (LUNPRINT,*) ' '
      write (LUNPRINT,*) 'There are ',nfun,' velocity functions:'
      write (LUNPRINT,*) ' '
      write (LUNPRINT,1001) i1,i2,i3,i4
      write (LUNPRINT,1002) ix1,iy1,ix2,iy2,ix3,iy3,ix4,iy4
      write (LUNPRINT,1003) obj%x1+(ix1-1)*obj%xinc,obj%y1+(iy1-1)*obj%yinc, &
                            obj%x1+(ix2-1)*obj%xinc,obj%y1+(iy2-1)*obj%yinc, &
                            obj%x1+(ix3-1)*obj%xinc,obj%y1+(iy3-1)*obj%yinc, &
                            obj%x1+(ix4-1)*obj%xinc,obj%y1+(iy4-1)*obj%yinc
      write (LUNPRINT,*) ' '
      write (LUNPRINT,1004) timedepth,velocity, &
                            timedepth,velocity, &
                            timedepth,velocity, &
                            timedepth,velocity
      do i = 1,obj%nlay
           write (LUNPRINT,4000) i,z(i,ix1,iy1),v(i,ix1,iy1),g(i,ix1,iy1), &
                                   z(i,ix2,iy2),v(i,ix2,iy2),g(i,ix2,iy2), &
                                   z(i,ix3,iy3),v(i,ix3,iy3),g(i,ix3,iy3), &
                                   z(i,ix4,iy4),v(i,ix4,iy4),g(i,ix4,iy4)
      end do
      write (LUNPRINT,*) ' '

1001  format ('   velfun#    =  ',i13,3i28)
1002  format ('   XY indices =  ',4(i8,i11,9x))
1003  format ('   XY coords  =  ',4(f8.1,f11.1,9x))
1004  format ('   layer',4x,4(4x,a5,'----',a5,'--gradient'))
4000  format (1x,i5,5x,4(3x,f8.0,f8.0,f9.3))

!----------load pickle jar:
 
      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar,'nfun'     ,obj%nfun)
      call pjar_put            (pjar,'nhx'      ,nhx)
      call pjar_put            (pjar,'nhy'      ,nhy)
      call pjar_put            (pjar,'nmosign'  ,NMOSIGN)
      call pjar_put            (pjar,'nmoexp'   ,NMOEXP)
      call pjar_put            (pjar,'x1'       ,obj%x1)
      call pjar_put            (pjar,'y1'       ,obj%y1)
      call pjar_put            (pjar,'xinc'     ,obj%xinc)
      call pjar_put            (pjar,'yinc'     ,obj%yinc)
      call pjar_put            (pjar,'nx'       ,obj%nx)
      call pjar_put            (pjar,'ny'       ,obj%ny)
      call pjar_put            (pjar,'nlay'     ,obj%nlay)

!----------finish up and return:

      err = VELIO3_OK
      msg = 'modspec file successfully opened'

      end subroutine velio3_open_read_pjar
 

!!------------------------- velio3 close ----------------------------------!!
!!------------------------- velio3 close ----------------------------------!!
!!------------------------- velio3 close ----------------------------------!!
 
 
      subroutine velio3_close (obj)

      type(velio3_struct),pointer :: obj                     ! arguments
 
      if (.not.associated(obj)) return

      if (associated(obj%modspec)) call modspec_delete (obj%modspec)

      deallocate (obj)

      end subroutine velio3_close
 

!!--------------------------- velio3 read velfun -------------------------!!
!!--------------------------- velio3 read velfun -------------------------!!
!!--------------------------- velio3 read velfun -------------------------!!


      subroutine velio3_read_velfun                                   &
                           (obj,xcoord,ycoord,npicks,tpicks,vpicks,   &
                            err,msg,velname,veltype,                  &
                            project,line,rdate,pdate,userid,comment,  &
                            nlay,zlay,vlay,glay)

      type(velio3_struct),intent(inout)        :: obj              ! arguments
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
      integer            ,intent(out),optional :: nlay             ! arguments
      real               ,intent(out),optional :: zlay(:)          ! arguments
      real               ,intent(out),optional :: vlay(:)          ! arguments
      real               ,intent(out),optional :: glay(:)          ! arguments

      obj%kount = obj%kount + 1

      call velio3_fetch_velfun                                             &
                      (obj,obj%kount,xcoord,ycoord,npicks,tpicks,vpicks,   &
                       err,msg,velname,veltype,                            &
                       project,line,rdate,pdate,userid,comment,            &
                       nlay,zlay,vlay,glay)

      end subroutine velio3_read_velfun


!!------------------------- velio3 fetch velfun --------------------------!!
!!------------------------- velio3 fetch velfun --------------------------!!
!!------------------------- velio3 fetch velfun --------------------------!!


      subroutine velio3_fetch_velfun                                       &
                           (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,   &
                            err,msg,velname,veltype,                       &
                            project,line,rdate,pdate,userid,comment,       &
                            nlay,zlay,vlay,glay)

      type(velio3_struct),intent(inout)        :: obj              ! arguments
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
      integer            ,intent(out),optional :: nlay             ! arguments
      real               ,intent(out),optional :: zlay(:)          ! arguments
      real               ,intent(out),optional :: vlay(:)          ! arguments
      real               ,intent(out),optional :: glay(:)          ! arguments
      real                                     :: ttt(obj%npicks)  ! local
      real                                     :: vvv(obj%npicks)  ! local
      integer                                  :: ix,iy,ipick      ! local
      integer                                  :: layer,ierr       ! local
      real             ,parameter              :: DMAX = 0.0       ! dummy
      integer          ,parameter              :: IPSDM = 1        ! local
      real                            ,pointer :: z(:,:,:)         ! local
      real                            ,pointer :: v(:,:,:)         ! local
      real                            ,pointer :: g(:,:,:)         ! local

      nullify (z)
      nullify (v)
      nullify (g)

      if (present(velname)) velname = 'none'
      if (present(veltype)) veltype = obj%veltype_out
      if (present(project)) project = 'modspec'
      if (present(line   )) line    = 'none'
      if (present(rdate  )) rdate   = 'none'
      if (present(pdate  )) pdate   = 'none'
      if (present(userid )) userid  = 'none'
      if (present(comment)) comment = 'none'

      if (ifun < 1 .or. ifun > obj%nfun) then
           if (present(xcoord)) xcoord = 0.0
           if (present(ycoord)) ycoord = 0.0
           if (present(npicks)) npicks = 0
           err = VELIO3_EOF
           msg = 'EOF encountered'
           return
      end if

      call mth_split_index (ifun,obj%nx,  ix,iy)

      if (present(xcoord )) xcoord  = obj%x1 + (ix - 1) * obj%xinc
      if (present(ycoord )) ycoord  = obj%y1 + (iy - 1) * obj%yinc
      if (present(npicks )) npicks  = obj%npicks

      err = VELIO3_OK
      msg = 'velocity function successfully read from modspec file'

      call modspec_get_zvg (obj%modspec,z,v,g)

      if (present(nlay)) nlay = obj%nlay;
      if (present(zlay)) zlay(1:obj%nlay) = z(1:obj%nlay,ix,iy);
      if (present(vlay)) vlay(1:obj%nlay) = v(1:obj%nlay,ix,iy);
      if (present(glay)) glay(1:obj%nlay) = g(1:obj%nlay,ix,iy);

      if (.not.present(tpicks) .and. .not.present(vpicks)) return

      do ipick = 1,obj%npicks

           ttt(ipick) = (ipick - 1) * obj%delta
           ierr  = modspec_getv (obj%modspec,IPSDM,DMAX,  &
                                 ttt(ipick)*obj%sec2ms,vvv(ipick),layer,ix,iy)

           if (ierr /= 0) then
                if (present(npicks)) npicks = 0
                err = VELIO3_ERROR
                msg = 'error in modspec_getv'
                return
           end if

           !!!!! The modspec_getv function returns an instantaneous
           !!!!! velocity at the specified depth or time adjusted
           !!!!! for any possible gradients.

if (mod(ipick,50) == 1) then
if (ifun == 1 .or. ifun == 112 .or. ifun == 11537 .or. ifun == 11648) then
    print *, 'ifun = ',ifun,'  ipick = ',ipick, '  time = ',ttt(ipick), &
                   '  velocity = ',vvv(ipick)
end if
end if

      end do

      if (obj%veltype_out /= obj%veltype_in) then
           call velutil_convert (obj%veltype_in,obj%npicks,ttt,vvv, &
                                 obj%veltype_out,ttt,vvv,ierr)
      end if

      if (present(tpicks)) tpicks(1:obj%npicks) = ttt(:)
      if (present(vpicks)) vpicks(1:obj%npicks) = vvv(:)

      end subroutine velio3_fetch_velfun


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velio3_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
