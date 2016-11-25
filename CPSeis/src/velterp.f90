!<CPS_v1 type="PRIMITIVE"/>


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
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : VELTERP      (interpolate among a set of velocity functions)
! Category   : velocity
! Written    : 2002-10-24   by: Tom Stoeckley
! Revised    : 2010-08-04   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Maintain and interpolate a set of velocity functions.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive reads in a velocity function file and provides an
! interpolated velocity function at any desired location when asked.
! The input velocity functions must reside on a grid, but do not have
! to be sorted in any particular order.  The returned velocity function
! will be linearly interpolated from the grid and will be resampled at
! a specified sample interval.
!
! This primitive was made from a subset of the NMO process, which was
! converted to the new system by Randy Selzler.  The purpose of splitting
! this primitive out of the NMO process was to simplify the NMO code and
! to make the function of this primitive reusable.
!
! There is no limit to the maximum number of time/depth/velocity picks on
! any velocity function.
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
!                             SUBROUTINES
!
!                         o       i      i    i      i        i         i
!   call velterp_create (obj, pathname, ndpt, dt, velbias, velscale, sampmode,
!                        nhx, nhy, error, msg, order, veltype, maxorig)
!                         o    o     o     o     i       i        o
!                                               opt     opt      opt
!
!                                                           opt     opt    opt
!                         b     i        i        o          o       o      o
!   call velterp_find   (obj, xcoord, ycoord, velocities, changed, norig, torig)
!
!                         b
!   call velterp_delete (obj)
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(velterp_struct)    obj = pointer to the velterp object.
! character(len=*)   pathname = name of velocity file to read.
! integer                ndpt = number of returned velocity samples.
! real                     dt = velocity time/depth sample interval (seconds).
! real                velbias = bias to add to velocities (per second).
! real               velscale = factor to multiply velocities by.
! integer            sampmode = velocity uniform sampling option.
! integer                 nhx = trace header word containing X coordinate.
! integer                 nhy = trace header word containing Y coordinate.
! logical               error = error flag (true if error occurred).
! character(len=*)        msg = message for possible printing (returned).
! integer               order = the required order used for moveout.
! character(len=*)    veltype = required velocity function type.
! integer             maxorig = maximum number of picks before resampling.
! real                 xcoord = X coordinate of desired velocity function.
! real                 ycoord = Y coordinate of desired velocity function.
! real       velocities(ndpt) = interpolated velocities at each sample point.
! logical             changed = true if VELOCITIES have changed.
! integer               norig = original number of picks before resampling.
! real           torig(norig) = original pick times (or depths).
!
! The returned velocities start at zero time.
!
! SAMPMODE = VELTERP_LSAMP means linear velocity resampling to DT step.
! SAMPMODE = VELTERP_SSAMP means spline velocity resampling to DT step.
!
! If ORDER   is present, it must match the value on the file.
! If VELTYPE is present, it must match the value on all functions on the file.
!
! VELTYPE must be one of the following (see the VELIO primitive for details):
!       'VTRM'  'VTNM'  'VTIN'  'VTAV'  'VZRM'  'VZIN'  'VZAV'  'VTDP'
!
! Velocity function types 'VTNM' and 'VTRM' are considered equivalent.
!
! Velocities read from the file will be adjusted as follows:
!       velocity = VELBIAS + VELSCALE * velocity
!
! If PATHNAME is 'NONE' or blank, velocities will be a constant set to VELBIAS.
!
! CHANGED will always be set to true the first time VELTERP_FIND is called.
!
! If the velocity file is a modspec file:
!   If ORDER is present, it will be ignored.
!   If VELTYPE is present, the velocity function will be converted to VELTYPE.
!
! NORIG will be the smallest number of picks of the four velocity functions
! used to calculate the interpolated velocity function.  TORIG(i) will be the
! weighted interpolation of the i-th pick from the same four velocity functions.
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
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 11. 2010-08-04 Stoeckley  Remove some velocity function print statements.
! 10. 2007-11-27 Stoeckley  Eliminate use of the memman primitve.
!009. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
!  8. 2005-10-10 Goodger    Change allocation method from memman to array for
!                           arrays xcoords and ycoords.  memman routines are
!                           getting confused.
!  7. 2005-02-21 Stoeckley  Add optional arguments MAXORIG and NORIG and TORIG.
!  6. 2004-12-09 Stoeckley  Sort the velocity functions so that NMO and other
!                            processes which use this primitive will no longer
!                            be required to have the input velocity file
!                            sorted in any particular order.
!  5. 2004-11-30 Stoeckley  Fix bug whereby abort occurred if optional argument
!                            ORDER was missing.
!  4. 2004-11-02 Stoeckley  Add ability to read modspec files.
!  3. 2004-05-03 Stoeckley  Fix bug in which nhx and nhy were not being
!                            returned in constant velocity case.
!  2. 2003-10-16 Stoeckley  Fix bug in the interpolation between velocity
!                            functions; remove limit on maximum number of picks.
!  1. 2002-10-25 Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module velterp_module
      use mth_module
      use named_constants_module
      use terputil_module
      use velio3_module
      use velio_module
      use velset_module
      use gridcheck_module
      use intpvelf_module
      implicit none
      public
      private :: velterp_private_print
      private :: velterp_private_locate
      private :: velterp_private_interpolate
      private :: velterp_private_resample

      type,public :: velterp_struct
        private
        integer                         :: ndpt
        real                            :: dt 
        integer                         :: sampmode
        integer                         :: nxbins,nybins
        real                            :: xcoord,ycoord
        real                            :: xa,xb,ya,yb
        type(velio3_struct)    ,pointer :: velio3
        type(velset_struct)    ,pointer :: velset
        real                   ,pointer :: xbins(:),ybins(:)
        real                   ,pointer :: vaa(:),vab(:),vba(:),vbb(:)
        real                   ,pointer :: vel(:)
        real                            :: velocity
        integer                         :: maxpicks,norig
        integer                         :: naa,nab,nba,nbb
        real                   ,pointer :: taa(:),tab(:),tba(:),tbb(:)
        real                   ,pointer :: torig(:)
      end type velterp_struct


      integer,public,parameter :: VELTERP_LSAMP = 1
      integer,public,parameter :: VELTERP_SSAMP = 2

      character(len=100),public :: velterp_ident = &
"$Id: velterp.f90,v 1.10 2007/11/28 14:56:19 Stoeckley beta sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine velterp_create (obj,pathname,ndpt,dt,velbias,velscale,  &
                                 sampmode,nhx,nhy,error,msg,order,veltype)
      implicit none
      type(velterp_struct)     ,pointer     :: obj               ! arguments
      character(len=*)         ,intent(in)  :: pathname          ! arguments
      integer                  ,intent(in)  :: ndpt              ! arguments
      real                     ,intent(in)  :: dt                ! arguments
      real                     ,intent(in)  :: velbias           ! arguments
      real                     ,intent(in)  :: velscale          ! arguments
      integer                  ,intent(in)  :: sampmode          ! arguments
      integer                  ,intent(out) :: nhx               ! arguments
      integer                  ,intent(out) :: nhy               ! arguments
      logical                  ,intent(out) :: error             ! arguments
      character(len=*)         ,intent(out) :: msg               ! arguments
      integer         ,optional,intent(in)  :: order             ! arguments
      character(len=*),optional,intent(in)  :: veltype           ! arguments
      type(velio_struct)       ,pointer     :: velio             ! local
      integer                               :: ifun,err,npicks   ! local
      real                     ,pointer     :: tpicks(:)         ! local
      real                     ,pointer     :: vpicks(:)         ! local
      real                     ,pointer     :: xcoords(:)        ! local
      real                     ,pointer     :: ycoords(:)        ! local
      real                                  :: nmosign,nmoexp    ! local
      real                                  :: nmosign2,nmoexp2  ! local
      character(len=4)                      :: veltype2          ! local
      integer                               :: nfun,ierr,i,ipick ! local
      real                                  :: x1,y1,xinc,yinc   ! local

      nullify (velio) ! jpa

!----------allocate and initialize data structure:

      allocate (obj)

      obj%ndpt     = ndpt
      obj%dt       = dt  
      obj%sampmode = sampmode
      obj%nxbins   = 0  
      obj%nybins   = 0  
      obj%xcoord   = FNIL
      obj%ycoord   = FNIL
      obj%xa       = 0.0 
      obj%xb       = 0.0 
      obj%ya       = 0.0 
      obj%yb       = 0.0 
      obj%velocity = 0.0 
      obj%maxpicks = 0  
      obj%norig    = 0  
      obj%naa      = 0  
      obj%nab      = 0  
      obj%nba      = 0  
      obj%nbb      = 0  

      nullify (obj%velio3)
      nullify (obj%velset)
      nullify (obj%xbins) ! jpa
      nullify (obj%ybins) ! jpa
      nullify (obj%vaa) ! jpa
      nullify (obj%vab) ! jpa
      nullify (obj%vba) ! jpa
      nullify (obj%vbb) ! jpa
      nullify (obj%vel) ! jpa
      nullify (obj%taa) ! jpa
      nullify (obj%tab) ! jpa
      nullify (obj%tba) ! jpa
      nullify (obj%tbb) ! jpa
      nullify (obj%torig) ! jpa
      nullify (xcoords)
      nullify (ycoords)

!----------use constant velocity if pathname not specified:

      if (pathname == 'NONE' .or. pathname == ' ') then
           if (velbias <= 0.0) then
                error = .true.
                msg = 'VELBIAS cannot be zero when PATHNAME not specified'
                return
           end if
           obj%velocity = velbias
           nhx = 7
           nhy = 8
           msg = 'velterp module successfully created'
           error = .false.
           return
      end if

!----------allocate velocity arrays:

      allocate (obj%vaa (obj%ndpt), stat=ierr)
      allocate (obj%vab (obj%ndpt), stat=ierr)
      allocate (obj%vba (obj%ndpt), stat=ierr)
      allocate (obj%vbb (obj%ndpt), stat=ierr)
      allocate (obj%vel (obj%ndpt), stat=ierr)

      obj%vaa(:) = 0.0  
      obj%vab(:) = 0.0  
      obj%vba(:) = 0.0  
      obj%vbb(:) = 0.0  
      obj%vel(:) = 0.0  

!----------maybe read modspec file:

      if (velio3_is_modspec(pathname)) then

           call velio3_open_read (obj%velio3, pathname, nfun,           &
                                  err, msg, nhx, nhy, nmosign, nmoexp,  &
                                  ndpt, dt, veltype,                    &
                                  x1,y1,xinc,yinc,                      &
                                  obj%nxbins, obj%nybins, obj%maxpicks)

           if (err /= VELIO_OK) then
                error = .true.
                return
           end if

           allocate (obj%xbins (obj%nxbins), stat=ierr)
           allocate (obj%ybins (obj%nybins), stat=ierr)

           obj%xbins(:) = (/(x1+(i-1)*xinc,i=1,obj%nxbins)/)
           obj%ybins(:) = (/(y1+(i-1)*yinc,i=1,obj%nybins)/)

           call velterp_private_print (obj,veltype)

!----------scan velocity file to obtain maxpicks:

      else
           call velio_scan_alloc (pathname,nfun,err,msg,maxpicks=obj%maxpicks)

           if (err /= VELIO_OK) then
                error = .true.
                return
           end if

      end if

!----------allocate pick time arrays:

      allocate (obj%taa   (obj%maxpicks), stat=ierr)
      allocate (obj%tab   (obj%maxpicks), stat=ierr)
      allocate (obj%tba   (obj%maxpicks), stat=ierr)
      allocate (obj%tbb   (obj%maxpicks), stat=ierr)
      allocate (obj%torig (obj%maxpicks), stat=ierr)

      obj%taa  (:) = 0.0  
      obj%tab  (:) = 0.0  
      obj%tba  (:) = 0.0  
      obj%tbb  (:) = 0.0  
      obj%torig(:) = 0.0  

!----------return if modspec file:

      if (associated(obj%velio3)) then
           msg = 'velterp module successfully created'
           error = .false.
           return
      end if

!----------read velocity file:

      allocate (tpicks(obj%maxpicks))
      allocate (vpicks(obj%maxpicks))

      call velio_open_read (velio, pathname, nfun,  &
                            err, msg, nhx, nhy, nmosign, nmoexp)
      print*,' velterp: file=',trim(pathname),' nfun=',nfun,' nhx=',nhx,' nhy=',nhy,' nmosign=',nmosign,' nmoexp=',nmoexp

      if (err /= VELIO_OK) then
           error = .true.
           return
      end if

      if (nfun == 0) then
           error = .true.
           msg = 'no velocity functions on file'
           return
      end if

      if (present(order)) then
           print*,' velterp: order=',order
           if (order == 2) then
                nmosign2 =  1.0
                nmoexp2  =  2.0
           else
                nmosign2 = -1.0
                nmoexp2  =  4.0
           end if
           if (nmosign /= nmosign2) then
                error = .true.
                msg = 'mismatching NMOSIGN on velocity file'
                return
           end if
           if (nmoexp /= nmoexp2) then
                error = .true.
                msg = 'mismatching NMOEXP on velocity file'
                return
           end if
      else
           nmosign2 =  nmosign
           nmoexp2  =  nmoexp
      end if

      call velset_create (obj%velset,nfun,nhx,nhy,nmosign2,nmoexp2)

      allocate (xcoords(nfun), stat=ierr)
      allocate (ycoords(nfun), stat=ierr)

      do ifun = 1,nfun
           call velio_read_velfun (velio, xcoords(ifun), ycoords(ifun),  &
                                   npicks, tpicks, vpicks, err, msg,     &
                                   veltype=veltype2)

           if (err /= VELIO_OK) then
                if (associated (xcoords)) deallocate (xcoords)
                if (associated (ycoords)) deallocate (ycoords)
                error = .true.
                return
           end if

           if (npicks <= 0) then
                if (associated (xcoords)) deallocate (xcoords)
                if (associated (ycoords)) deallocate (ycoords)
                msg = 'encountered a velocity function with no picks'
                error = .true.
                return
           end if

           if (present(veltype)) then
!!!!!!!!        print*,' velterp: veltype=',veltype
                if (veltype2 == veltype) then
                     continue
                else if (veltype == 'VTNM' .and. veltype2 == 'VTRM') then
                     continue
                else if (veltype == 'VTRM' .and. veltype2 == 'VTNM') then
                     continue
                else
                     error = .true.
                     msg = 'wrong velocity function type on velocity file'
                     return
                end if
           end if

           if (velbias /= 0.0 .or. velscale /= 1.0) then
                vpicks(1:npicks) = velbias + velscale * vpicks(1:npicks)
           end if
!!!!!!!!   print*,' velterp: Velocity Function ',ifun,' at x=',xcoords(ifun),' y=',ycoords(ifun)
!!!!!!!!   do ipick=1,npicks,npicks-1
!!!!!!!!     print*,' velterp: ipick = ',ipick,' tpick(ipick) = ',tpicks(ipick),' vpick(ipick)=',vpicks(ipick)
!!!!!!!!   end do
           call velset_add_velfun (obj%velset,xcoords(ifun),ycoords(ifun), &
                                   npicks,tpicks,vpicks)
      end do

      call velio_close (velio)

!----------sort the velocity functions:

      call velset_sort (obj%velset)

      do ifun = 1,nfun
           call velset_get_velfun (obj%velset,ifun,xcoords(ifun),  &
                                   ycoords(ifun),npicks,tpicks,vpicks)
      end do

      deallocate (tpicks)
      deallocate (vpicks)

!----------get velocity grid information:

      call gridcheck (nfun, xcoords, ycoords, obj%nxbins, obj%nybins, msg)

      if (msg(1:1) /= ' ') then
           if (associated (xcoords)) deallocate (xcoords)
           if (associated (ycoords)) deallocate (ycoords)
           error = .true.
           return
      end if

      allocate (obj%xbins (obj%nxbins), stat=ierr)
      allocate (obj%ybins (obj%nybins), stat=ierr)

      obj%xbins(:) = xcoords(1:obj%nxbins)
      obj%ybins(:) = ycoords(1:nfun:obj%nxbins)

      if (associated (xcoords)) deallocate (xcoords)
      if (associated (ycoords)) deallocate (ycoords)

      call velterp_private_print (obj,veltype)

      msg = 'velterp module successfully created'
      error = .false.
      return
      end subroutine velterp_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine velterp_delete (obj)
      implicit none
      type(velterp_struct),pointer :: obj               ! arguments


      if (.not.associated(obj)) return

      call velio3_close  (obj%velio3)
      call velset_delete (obj%velset)

      if (associated (obj%xbins)) deallocate (obj%xbins)
      if (associated (obj%ybins)) deallocate (obj%ybins)

      if (associated (obj%vaa  )) deallocate (obj%vaa)
      if (associated (obj%vab  )) deallocate (obj%vab)
      if (associated (obj%vba  )) deallocate (obj%vba)
      if (associated (obj%vbb  )) deallocate (obj%vbb)
      if (associated (obj%vel  )) deallocate (obj%vel)

      if (associated (obj%taa  )) deallocate (obj%taa)
      if (associated (obj%tab  )) deallocate (obj%tab)
      if (associated (obj%tba  )) deallocate (obj%tba)
      if (associated (obj%tbb  )) deallocate (obj%tbb)
      if (associated (obj%torig)) deallocate (obj%torig)

      deallocate(obj)
      return
      end subroutine velterp_delete


!!----------------------------- find ----------------------------------!!
!!----------------------------- find ----------------------------------!!
!!----------------------------- find ----------------------------------!!


      subroutine velterp_find (obj, xcoord, ycoord, velocities,  &
                               changed, norig, torig)
      implicit none
      type(velterp_struct),intent(inout) :: obj              ! arguments
      real                ,intent(in)    :: xcoord,ycoord    ! arguments
      real                ,intent(out)   :: velocities(:)    ! arguments
      logical    ,optional,intent(out)   :: changed          ! arguments
      integer    ,optional,intent(out)   :: norig            ! arguments
      real       ,optional,intent(out)   :: torig(:)         ! arguments
      real                               :: xcoord2,ycoord2  ! local

!----------return constant velocity:

      if (obj%velocity > 0.0) then
           velocities(1:obj%ndpt) = obj%velocity
           if (present(changed)) then
                changed = (obj%xcoord == FNIL .or. obj%ycoord == FNIL)
                obj%xcoord = 1.0
                obj%ycoord = 1.0
           end if
           if (present(norig)) norig = 0
           return
      end if

!----------return interpolated velocity function:

      xcoord2 = nint(xcoord)
      ycoord2 = nint(ycoord)

      call mth_constrain (xcoord2, obj%xbins(1), obj%xbins(obj%nxbins))
      call mth_constrain (ycoord2, obj%ybins(1), obj%ybins(obj%nybins))

      if (obj%xcoord == FNIL .or. obj%ycoord == FNIL) then

           call velterp_private_locate      (obj, xcoord2, ycoord2)
           call velterp_private_interpolate (obj, xcoord2, ycoord2)
           if (present(changed)) changed = .true.

      else if (xcoord2 == obj%xcoord .and. ycoord2 == obj%ycoord) then

           if (present(changed)) changed = .false.

      else if (xcoord2 >= obj%xa .and. xcoord2 <= obj%xb .and.  &
               ycoord2 >= obj%ya .and. ycoord2 <= obj%yb) then

           call velterp_private_interpolate (obj, xcoord2, ycoord2)
           if (present(changed)) changed = .true.

      else

           call velterp_private_locate      (obj, xcoord2, ycoord2)
           call velterp_private_interpolate (obj, xcoord2, ycoord2)
           if (present(changed)) changed = .true.

      end if

      obj%xcoord = xcoord2
      obj%ycoord = ycoord2
      velocities(1:obj%ndpt) = obj%vel(1:obj%ndpt)

      if (present(norig)) norig          = obj%norig
      if (present(torig)) torig(1:norig) = obj%torig(1:norig)
      return
      end subroutine velterp_find


!!-------------------------- private print ---------------------------!!
!!-------------------------- private print ---------------------------!!
!!-------------------------- private print ---------------------------!!


      subroutine velterp_private_print (obj,veltype)
      implicit none
      type(velterp_struct),intent(inout) :: obj                  ! arguments
      character(len=*)    ,intent(in)    :: veltype              ! arguments
      real                               :: vel(obj%ndpt,4)      ! local
      integer                            :: i,nfun               ! local
      integer                            :: i1,i2,i3,i4          ! local
      integer                            :: ix1,ix2,ix3,ix4      ! local
      integer                            :: iy1,iy2,iy3,iy4      ! local
      character(len=5)                   :: timedepth,velocity   ! local
      integer                            :: npicks               ! local
      real                               :: tpicks(obj%maxpicks) ! local

      nfun = obj%nxbins * obj%nybins
      i1   = 1
      i2   = obj%nxbins
      i3   = nfun - obj%nxbins + 1
      i4   = nfun

      ix1 = 1
      ix2 = obj%nxbins
      ix3 = 1
      ix4 = obj%nxbins

      iy1 = 1
      iy2 = 1
      iy3 = obj%nybins
      iy4 = obj%nybins

      call velterp_private_resample (obj, i1, vel(:,1), npicks, tpicks)
      call velterp_private_resample (obj, i2, vel(:,2), npicks, tpicks)
      call velterp_private_resample (obj, i3, vel(:,3), npicks, tpicks)
      call velterp_private_resample (obj, i4, vel(:,4), npicks, tpicks)

      if (veltype(1:2) == 'VT') then
           timedepth = ' time'
      else
           timedepth = 'depth'
      end if

      if (veltype(3:4) == 'IN') then
           velocity = 'VINT'
      else if (veltype(3:4) == 'AV') then
           velocity = 'VAV'
      else if (veltype(3:4) == 'RM') then
           velocity = 'VRMS'
      else if (veltype(3:4) == 'NM') then
           velocity = 'VNMO'
      else
           velocity = 'VEL'
      end if

      print *, ' '
      print *, 'There are ',nfun,' velocity functions:'
      print *, ' '
      print 1000, 'velfun# =',i1,i2,i3,i4
      print *, ' '
      print 1000, 'X index =',ix1,ix2,ix3,ix4
      print 1000, 'Y index =',iy1,iy2,iy3,iy4
      print *, ' '
      print 2000, 'X coord =',obj%xbins(ix1),obj%xbins(ix2), &
                              obj%xbins(ix3),obj%xbins(ix4)
      print 2000, 'Y coord =',obj%ybins(iy1),obj%ybins(iy2), &
                              obj%ybins(iy3),obj%ybins(iy4)
      print *, ' '
      print 3000, timedepth,velocity,velocity,velocity,velocity
      do i = 1,obj%ndpt,50
          if (veltype(1:2) == 'VT') then
            print 4000, i,(i-1)*obj%dt,vel(i,:)
          else
            print 5000, i,(i-1)*obj%dt,vel(i,:)
          end if
1000      format (7x,a9       ,3x,i8  ,2x,i8  ,2x,i8  ,2x,i8)
2000      format (7x,a9       ,3x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1)
3000      format ('  sample     ',a5,4(5x,a5))
4000      format (1x,i7,2x,f8.3,2x,f8.0,2x,f8.0,2x,f8.0,2x,f8.0)
5000      format (1x,i7,2x,f8.0,2x,f8.0,2x,f8.0,2x,f8.0,2x,f8.0)
      end do 
      print *, ' '
      return
      end subroutine velterp_private_print


!!-------------------------- private locate ---------------------------!!
!!-------------------------- private locate ---------------------------!!
!!-------------------------- private locate ---------------------------!!

! locates vaa,vab,vba,vbb and xa,xb,ya,yb.
! locates naa,nab,nba,nbb and taa,tab,tba,tbb.

      subroutine velterp_private_locate (obj, xcoord, ycoord)
      implicit none
      type(velterp_struct),intent(inout) :: obj                ! arguments
      real                ,intent(in)    :: xcoord,ycoord      ! arguments
      integer                            :: ixa,ixb,iya,iyb    ! local
      integer                            :: iaa,iab,iba,ibb    ! local

      call terputil_binary_search (xcoord, obj%xbins, obj%nxbins, ixa, ixb)
      call terputil_binary_search (ycoord, obj%ybins, obj%nybins, iya, iyb)

      iaa = ixa + (iya - 1) * obj%nxbins
      iab = ixa + (iyb - 1) * obj%nxbins
      iba = ixb + (iya - 1) * obj%nxbins
      ibb = ixb + (iyb - 1) * obj%nxbins

      call velterp_private_resample (obj, iaa, obj%vaa, obj%naa, obj%taa)
      call velterp_private_resample (obj, iab, obj%vab, obj%nab, obj%tab)
      call velterp_private_resample (obj, iba, obj%vba, obj%nba, obj%tba)
      call velterp_private_resample (obj, ibb, obj%vbb, obj%nbb, obj%tbb)

      obj%xa = obj%xbins(ixa)
      obj%xb = obj%xbins(ixb)
      obj%ya = obj%ybins(iya)
      obj%yb = obj%ybins(iyb)

      return
      end subroutine velterp_private_locate


!!-------------------------- private resample ---------------------------!!
!!-------------------------- private resample ---------------------------!!
!!-------------------------- private resample ---------------------------!!


      subroutine velterp_private_resample (obj, ifun, vvv, npicks, tpicks)
      implicit none
      type(velterp_struct),intent(inout) :: obj                   ! arguments
      integer             ,intent(in)    :: ifun                  ! arguments
      real                ,intent(out)   :: vvv(:)                ! arguments
      integer             ,intent(out)   :: npicks                ! arguments
      real                ,intent(out)   :: tpicks(:)             ! arguments
      real                               :: xcoord,ycoord         ! local
      real                               :: tpicks2(obj%ndpt)     ! local
      integer                            :: npicks2,ier,err       ! local
      real                               :: vpicks(obj%maxpicks)  ! local
      real                               :: work(3*obj%maxpicks)  ! local
      character(len=80)                  :: msg                   ! local

      if (associated(obj%velio3)) then
           call velio3_fetch_velfun (obj%velio3, ifun, xcoord, ycoord,  &
                                     npicks2, tpicks2, vvv, err, msg,   &
                                     nlay=npicks, zlay=tpicks)
           return
      end if

      call velset_get_velfun &
                (obj%velset, ifun, xcoord, ycoord, npicks, tpicks, vpicks)

      if (obj%sampmode == VELTERP_SSAMP) then
           call intpvelf          (npicks, tpicks, vpicks, work, &
                                   0, 0.0, obj%dt, obj%ndpt, vvv, ier)
      else
           call terputil_fastsamp (tpicks, npicks, vpicks, &
                                   0.0, obj%dt, obj%ndpt, vvv)
      end if
      return
      end subroutine velterp_private_resample


!!-------------------------- private interpolate ---------------------------!!
!!-------------------------- private interpolate ---------------------------!!
!!-------------------------- private interpolate ---------------------------!!

! uses vaa,vab,vba,vbb and xa,xb,ya,yb to interpolate vel.

      subroutine velterp_private_interpolate (obj, xcoord, ycoord)
      implicit none
      type(velterp_struct),intent(inout) :: obj                ! arguments
      real                ,intent(in)    :: xcoord,ycoord      ! arguments
      real                               :: waa,wab,wba,wbb    ! local
      integer                            :: indx               ! local

      call terputil_2d_weights (xcoord, obj%xa, obj%xb,  &
                                ycoord, obj%ya, obj%yb, waa, wab, wba, wbb)

      do indx = 1,obj%ndpt
           obj%vel(indx) = waa * obj%vaa(indx) + wab * obj%vab(indx) + &
                           wba * obj%vba(indx) + wbb * obj%vbb(indx)
      end do

      obj%norig = min(obj%naa, obj%nab, obj%nba, obj%nbb)
      do indx = 1,obj%norig
           obj%torig(indx) = waa * obj%taa(indx) + wab * obj%tab(indx) + &
                             wba * obj%tba(indx) + wbb * obj%tbb(indx)
      end do
      return
      end subroutine velterp_private_interpolate


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module velterp_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

