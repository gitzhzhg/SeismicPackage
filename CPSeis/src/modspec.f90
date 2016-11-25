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
!-------------------------------------------------------------------------------
! C P S   P R I M I T I V E
! Name       : MODSPEC
! Category   : velocity
! Written    : 2001-11-21   by: RSDay - conversion of gridlib.f
! Revised    : 2008-01-29   by: RSDay
! Maturity   : beta
! Purpose    : Support Heritage Phillips MODSPEC format
! Portability: No known problems
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! The modspec module supports the gridlib functions that were a part of heritage
! Phillips Petroleum technology. This module provides routines to read and
! manipulate modspec format layered velocity models. Some functionality has been
! slightly enhanced, and arguments of some functions are not the same as the
! heritage code.
!
! The original gridlib routines were written by Kay Wyatt and others at
! Phillips Petroleum Co.
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
!                         o   i    i    i
!     call modspec_create(obj,stdo,file,mode)
!     - reads the modspec file and creates a modspec object. Set mode='GRID'
!       to create an instance of the model on the default grid.
!     type(modspec_struct),pointer :: obj
!     integer,intent(in)           :: stdo !stdout unit number
!     character(len=*),intent(in)  :: file !input modspec file name
!     character(len=*),intent(in)  :: mode !optional behavior flag
!
!                         o   i    i    i     i
!     call modspec_create(obj,stdo,nlay,angle,units,&
!     i  i  i  i  i  i  i  i  i
!     nx,ox,dx,ny,oy,dy,nz,oz,dz)
!     - create new modspec from passed argument. No input file.
!
!                         b
!     call modspec_delete(obj)
!     - destroy the modspec object and delete all allocated memory
!     type(modspec_struct),pointer :: obj
!
!                                   i     i         o
!     status = modspec_getdefparms(stdo, filespec, nlaydef, &
!      o        o        o      o      o      o
!      xorgdef, yorgdef, dxdef, dydef, nxdef, nydef,&
!      o         o
!      angledef, unitsdef)
!     - return values of default parameter settings to the caller. The
!       default parameters in the modspec file are in the format
!       KeyWord=value where the recognized KeyWords are:
!       NLAYDEF  XORGDEF YORGDEF DXDEF DYDEF NXDEF NYDEF ANGLEDEF
!       UNITSDEF NZDEF   DZDEF. Note that this an overloaded function.
!       The form above returns the traditional set of default parameters.
!       The keywords NZDEF and DZDEF are an extension to gridlib.
!
!                                   i
!     domain = modspec_get_domain (obj)
!     integer :: domain
!     domain = 1 = MODSPEC_DEPTH for depth.
!     domain = 2 = MODSPEC_TIME for time.
!
!                          i   o    o     o
!     call modspec_getdesc(obj,nlay,angle,units,&
!     o  o  o  o  o  o  o  o  o
!     nx,ox,dx,ny,oy,dy,nz,oz,dz)
!     - return the grid description for the modspec object. Note that
!       nz,oz,dz were not in the heritage Phillips modspec definition.
!     type(modspec_struct),intent(in) :: obj
!     character(len =*),intent(out) :: units!M or F units flag
!     real, intent(out)             :: angle!modspec rotation angle
!     integer, intent(out)          :: nlay !number of layers
!     integer, intent(out)          :: nx   !number of x grid points
!     integer, intent(out)          :: ny   !number of y grid points
!     integer, intent(out)          :: nz   !number of z grid points
!     double precision, intent(out) :: ox   !the x grid origin
!     double precision, intent(out) :: oy   !the y grid origin
!     real, intent(out)             :: oz   !the z grid origin
!     real, intent(out)             :: dx   !the x grid bin size
!     real, intent(out)             :: dy   !the y grid bin size
!     real, intent(out)             :: dz   !the z grid bin size
!
!                           i   o
!     call modspec_get_file(obj,file)
!     - return name of the modspec file that the object is based upon.
!     type(modspec_struct),intent(inout) :: obj
!     character(len=*),intent(out)       :: file
!
!                          i   o o o
!     call modspec_get_zvg(obj,z,v,g)
!     - give caller access to grid information for the modspec model
!     type(modspec_struct),intent(in) :: obj
!     real,pointer    :: z(:,:,:) !depth grids nlay*nx*ny
!     real,pointer    :: v(:,:,:) !velocity grids nlay*nx*ny
!     real,pointer    :: g(:,:,:) !gradient grids nlay*nx*ny
!
!
!                             b   i  i  i
!     call modspec_set_z_desc(obj,nz,oz,dz)
!     - set the z grid parameters
!
!                               b
!     status = modspec_rddefmod(obj)
!     - reads in default parameters from the modspec file and computes the model
!       parameters on the default grid definition.
!     type(modspec_struct),intent(inout) :: obj
!
!                              i      o    i
!     subroutine modspec_pcopy(fname, obj, stdo)
!     - parallel copy of modspec object
!       root cpu reads from fname to create obj on root
!       obj data is broadcast to non-root cpus so obj can be
!       created on non-root cpus
!                                 b
!     status = modspec_grid2model(obj,&
!       i      i     i   i   i     i     i   i   i     opt
!       units, nlay, nx, ny, xorg, yorg, dx, dy, angle,znon)
!     - build modspec model on the specified target grid. Note that
!       the model is read from the file provided to the create call.
!     type(modspec_struct),intent(inout) :: obj
!     integer, intent(in)    :: nlay
!     integer, intent(in)    :: nx
!     integer, intent(in)    :: ny
!     real, intent(in)       :: dx
!     real, intent(in)       :: dy
!     real, intent(in)       :: angle
!     double precision,intent(in) :: xorg
!     double precision,intent(in) :: yorg
!     character(len=*),intent(in) :: units
!
!
!     status =  modspec_warpgrid2model(obj, &
!       units, nlay, nx, ny, xorg, yorg, dx, dy, angle, &
!       gridx, gridy,znon)
!     - build modspec model on the specified warped target grid. Note that
!       the model is read from the file provided to the create call.
!     type(modspec_struct),intent(inout) :: obj
!     integer,intent(in)  :: nlay
!     integer,intent(in)  :: nx
!     integer,intent(in)  :: ny
!     real,intent(in)     :: dx
!     real,intent(in)     :: dy
!     real,intent(in)     :: gridx(nx,ny)
!     real,intent(in)     :: gridy(nx,ny)
!     real,intent(in)     :: angle
!     double precision,intent(in) :: xorg
!     double precision,intent(in) :: yorg
!     character(len=*),intent(in) :: units
!
!                           i    i
!     status = modspec_getv(obj, domain,&
!       i     i      o         o      i   i   opt
!       dmax, depth, velocity, layer, ix, iy, znon)
!     - compute velocity at x-y grid point(ix,iy) and at z=depth
!     ipsdm - Use 1=MODSPEC_POSTSTACK for Poststack DM velocity (no clipping)
!             Use 2=MODSPEC_PRESTACK for Prestack DM velocity (do use clipping)
!     domain - Use 1=MODSPEC_DEPTH for depth domain output (same as ipsdm=1)
!              Use 2=MODSPEC_TIME for time domain output
!
!                          b     i
!     call modspec_tdconv(obj, domain)
!     domain - Use 1=MODSPEC_DEPTH to convert from time to depth.
!              Use 2=MODSPEC_TIME to convert from depth to time.
!     This subroutine converts the z array from time to depth or vice versa.
!     This subroutine also changes the internal domain variable appropriately.
!     This subroutine does nothing if domain is invalid or matches the
!      current domain.
!     This subroutine was taken from the mathlib.f file.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 33. 2008-01-29  RSDay      Added some convenience functions.
! 32. 2006-10-24  RSDay      Added CPS_HDRA,CPS_HDRB information to print 
!                            functions.
!                            Initialize default xyz storage order parameter.
! 31. 2006-09-14  RSDay      Added modspec_layer_to_gocad
! 30. 2006-08-24  D. Glover  Added NULLIFY statements for Intel compiler.
! 29  2006-04-25  RSDay      Added modspec_lt2ixy for coordinate conversion
!                            from line-trace to 2D array index.
! 28. 2005-12-22  RSDay      prototype chage for modspec_print_obj
! 27. 2005-10-06  RSDay      Method modspec_pcopy_obj added. More error
!                            checks in modspec_pcopy and modspec_regrid.
! 26. 2005-10-04  RSDay      Methods modspec_pcopy, modspec_regrid added.
!                            Eliminate dead code.
! 25. 2005-05-31  RSDay      Added modspec_print_to_cards
! 24. 2005-04-12  RSDay      Added modspec_transform_abs, modspec_grid2world.
!                            Fixed rddefmod_reset line trace settings.
! 23. 2004-12-09  RSDay      Added ability to reset the default grid. Added
!                            modspec_create_reset, modspec_rddefmod_reset.
! 22. 2004-10-26  Stoeckley  Add function modspec_get_domain and subroutine
!                            modspec_tdconv; add named constants for the
!                            domain and ipsdm constants 1 and 2. Modified
!                            modspec_getv.
! 21. 2004-06-17  R.S.Day    Reduce gridin array size in modspec_rddefmod.
! 20. 2004-05-25  R.S.Day    Replace modspec_grid2model with call to
!                            gridplib_model_from grid. Force upper case
!                            domain flag.
! 19. 2004-05-06  R.S.Day    Changes for compatablity with gridlib update
! 18. 2004-04-29  R.S.Day    Use gridlib_read_grid_header.
!                            Use gridlib_compute_velocity_torz.
! 17. 2004-04-22  R.S.Day    Make use of gridlib module where appropriate.
! 16. 2004-03-29  R.S.Day    Modified modspec_grid2grid for consistency with
!                            gridlib changes. Split modspec_getv into 3:
!                            modspec_getv,modspec_getv_torz,modspec_getv_classic
!                            Add DOMAIN variable for getv.
!                            Added modspec_writegrid_fmt. Remove modspec_getv_xy
! 15. 2004-03-09  R.S.Day    fixed modspec_grid2model logic for optional znon
!                            argument.
! 14. 2004-03-08  R.S.Day    Added functions modspec_set_znon, modspec_get_znon,
!                            modspec_dogridheadersmatch. Added znon argument to
!                            modspec_readgrid, modspec_print, modspec_grid2model
! 13. 2004-01-06  R.S.Day    Modified grid rounding as per PAVs recomendation.
! 12. 2003-10-22  R.S.Day    Added modspec_to_gocad_vs
! 11. 2003-10-09  R.S.Day    Fix xndx,yndx arithmetic as per Zhaobo advice
! 10. 2003-08-15  R.S.Day    Fixed a problem with grid sizes in modspec_getv.
!                            Changes for Modspec-CPS coordinate system xfer.
!  9. 2003-08-11  R.S.Day    NX and NY flipped if modpsec and cps x-y are
!                            transposed as specified by hdra and hdrb.
!  8. 2003-08-07  R.S.Day    Added search for CPS_HDRA, CPS_HDRB keywords of
!                            modspec file to enable reliable modspec model
!                            registration to CPS seismic data. Added
!                            modspec_get_coef_full for user convenience.
!  7. 2003-06-30  R.S.Day    Added support for modspec grid coefficients.
!                            Added modspec_get_coef, modspec_get_order
!                            functions.
!  6. 2003-06-18  R.S.Day    Header support for CPS compatibility. Added
!                            modpsec_get_hdrs, modspec_get_angle. HXDEF,HYDEF
!                            added to parameter choices.
!  5. 2003-05-27  R.S.Day    modspec_getdefparm_i bug corrected. look for
!                            ANGLEDEF and UNITSDEF
!  4. 2003-05-08  R.S.Day    made modspec_getdefparm ignore leading "*" on
!                            input cards. Using named constants.
!  3. 2003-02-27  R.S.Day    modspec_readgrid logic fix to set error status on
!                            open errors.
!  2. 2002-11-27  R.S.Day    Fixed some compiler warnings. Added the function
!                            modspec_get_file.
!  1. 2002-11-21  R.S.Day    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!  The modspec_create function should be called first to create an instance of
!  a modspec object from an input file(old) or from passed arguments(new).
!  After creation, other calls can be made to manipulate the model.
!  When finished with an object call modspec_delete to destroy it and free up
!  resources.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! The original gridlib routines were f77 or c language  routines. This module is
! in fortran 90 and is more object oriented in its style.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!
      module modspec_module
      use pcpsx_module
      use gridlib_module
      use cardset_module
      use named_constants_module
      use getlun_module
      use cio_module
      implicit none

       private

! gridlib methods - The API to the outside world
       public  :: modspec_create
       public  :: modspec_delete
       public  :: modspec_initialize
       ! methods to give access to internal variables and data
       public  :: modspec_get_file
       public  :: modspec_get_nlay
       public  :: modspec_get_orig    !get modspec xorg,yorg
       public  :: modspec_getdesc
       public  :: modspec_get_coef
       public  :: modspec_get_coef_full
       public  :: modspec_setdesc
       public  :: modspec_get_domain
       public  :: modspec_set_z_desc
       public  :: modspec_get_z_desc
       public  :: modspec_get_zvg
       public  :: modspec_get_zlimits
       public  :: modspec_get_hdrs    !get CPS hx,hy,hz
       public  :: modspec_set_hdrs    !set CPS hx,hy,hz
       public  :: modspec_get_order   !return order string
       public  :: modspec_reversed    !return logical xy_reversed
       public  :: modspec_get_z
       public  :: modspec_get_v
       public  :: modspec_get_g
       public  :: modspec_get_angle   !get modspec angle
       public  :: modspec_get_vclips
       public  :: modspec_set_verbose
       public  :: modspec_set_znon
       public  :: modspec_get_znon
       public  :: modspec_grid2world !convert line,trace to world x,y,z
       public  :: modspec_lt2ixy     !convert line,trace to grid gx,gy
       public  :: modspec_has_coef   !are the a,b coeficients set ?
       public  :: modspec_transform_abs

       ! traditional gridlib functionality
       public  :: modspec_getdefparms
       public  :: modspec_readgrid
       public  :: modspec_writegrid
       public  :: modspec_writegrid_fmt
       public  :: modspec_getv         !object oriented getv
       public  :: modspec_getv_classic !classic getv function
       public  :: modspec_getv_torz    !time | depth domain getv
       public  :: modspec_grid2grid
       public  :: modspec_warpgrid
       public  :: modspec_fixmodel
       public  :: modspec_checkclip
       public  :: modspec_print
       public  :: modspec_print_obj
       public  :: modspec_print_to_cards
       public  :: modspec_to_gocad_vs
       public  :: modspec_layer_to_gocad
       private :: modspec_abort
       public  :: modspec_tdconv
       public  :: modspec_pcopy
       public  :: modspec_pcopy_obj
       public  :: modspec_regrid

       character(len=100),save,public :: modspec_ident = &
       "$Id: modspec.f90,v 1.33 2008/01/30 15:06:06 RSDay beta sps $"

! A generic description of a grid object
       type,public :: modspec_struct
        private
        integer              :: stdo
        real                 :: znon         !the "ZNON" value
        character(len=12 )   :: domain       !DEPTH | TIME
        character(len=120)   :: file
        integer              :: nlay_file
        integer              :: nlay
        integer              :: nx           !NX from file, or set
        integer              :: ny           !NY from file, or set
        integer              :: nz           !modspec extension
        double precision     :: xorg         !x-origin in world coord
        double precision     :: yorg         !y-origin in world coord
        real                 :: zorg         !modspec extension
        real                 :: dx           !physical dx in rotated sytem
        real                 :: dy           !physical dy in rotated sytem
        real                 :: dz           !modspec extension
        real                 :: angle        !angle between wc and local coords
        character(len=4)     :: units
        integer,pointer      :: dominance(:)
        real,pointer         :: z(:,:,:)     !depth grid (nlay,nx,ny)
        real,pointer         :: v(:,:,:)     !velocity reference grid
        real,pointer         :: g(:,:,:)     !velocity gradient grid
        real,pointer         :: vclipmin(:)  !clip (nlay)
        real,pointer         :: vclipmax(:)  !clip (nlay)
        integer              :: hx           !modspec x axis cps x-header
        integer              :: hy           !modspec y axis cps y-header
        integer              :: hz           !z axis cps header
        integer              :: hdra         !a axis cps header (line)
        integer              :: hdrb         !b axis cps header (trace)
        real                 :: a1,a2,a3     !defines line limits
        real                 :: b1,b2,b3     !defines trace limits
        logical              :: has_coef     !are the a,b coeficients set
        logical              :: xy_reversed  !cps and modspec X-Y reversed?
        character(len=4)     :: xyz_order    !CPS x,y,z storage order
                                             !modgrid x,y interpretation
        logical              :: modspec_verbose
       end type modspec_struct

       interface modspec_getdefparms
        module procedure modspec_getdefparm_o  !place in object
        module procedure modspec_getdefparm_a  !place in arguments
        module procedure modspec_getdefparm_i  !passed an open unit
       end interface
       interface modspec_create
        module procedure modspec_create_old   !from file
        module procedure modspec_create_new   !from arguments
        module procedure modspec_create_reset !from file, override defaults
       end interface

       real,parameter,public :: mspec_gnull = -999.0
       logical,save     :: stat_verbose = .false.

       integer,parameter,public :: MODSPEC_DEPTH = 1    ! domain
       integer,parameter,public :: MODSPEC_TIME  = 2    ! domain

       integer,parameter,public :: MODSPEC_POSTSTACK = 1    ! ipsdm
       integer,parameter,public :: MODSPEC_PRESTACK  = 2    ! ipsdm

      contains

!
!
!                     ********************
!                     *                  *
!                     *     GRIDLIB      *
!                     *                  *
!                     ********************
!
!
!     Kay!! Do NOT forget to update "gridlib.cray.f"
!     as you add new utilities to "gridlib.f"
!     Update copy on babbage too.
!
!
!

! Allocate object, set defaults for variables and
! save the file name
      subroutine modspec_create_old(obj,stdo,file,mode)
      type(modspec_struct),pointer :: obj
      integer,intent(in)           :: stdo !stdout unit number
      character(len=*),intent(in)  :: file !input modspec file name
      character(len=*),intent(in)  :: mode !optional behavior flag
      integer    ::  i_err       !local variables
      allocate (obj, stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modpsec_create: object allocation tanked'
        nullify(obj)
        return
      endif
      nullify(obj%z)
      nullify(obj%v)
      nullify(obj%g)
      nullify(obj%dominance)
      nullify(obj%vclipmin)
      nullify(obj%vclipmax)

      call modspec_initialize(obj)
      obj%file = file
      obj%stdo = stdo
      i_err = modspec_getdefparm_o(obj)
      if(i_err /=0) then
        write(stdo,*) 'modspec_create: modspec_getdefparm_o failed'
        call modspec_delete(obj)
        return
      endif
      if(mode == 'GRID') then
        i_err = modspec_rddefmod(obj)
        if(i_err /=0) then
          write(stdo,*) 'modspec_create: modspec_rddefmod failed'
          call modspec_delete(obj)
          return
        endif
      endif
      return
      end subroutine modspec_create_old


      subroutine modspec_create_new(obj,stdo,nlay,angle,units,&
      nx,ox,dx,ny,oy,dy,nz,oz,dz)
      type(modspec_struct),pointer :: obj
      integer,intent(in)           :: stdo !stdout unit number
      character(len =*),intent(in):: units!M or F units flag
      real, intent(in)             :: angle!modspec rotation angle
      integer, intent(in)          :: nlay !number of layers
      integer, intent(in)          :: nx   !number of x grid points
      integer, intent(in)          :: ny   !number of y grid points
      integer, intent(in)          :: nz   !number of z grid points
      double precision, intent(in) :: ox   !the x grid origin
      double precision, intent(in) :: oy   !the y grid origin
      real, intent(in)             :: oz   !the z grid origin
      real, intent(in)             :: dx   !the x grid bin size
      real, intent(in)             :: dy   !the y grid bin size
      real, intent(in)             :: dz   !the z grid bin size
      integer    ::  i_err       !local variables
      nullify(obj)
      if(nlay < 1 .or. nx<1 .or. ny<1) return
      allocate (obj, stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modpsec_create: object allocation tanked'
        nullify(obj)
        return
      endif
      nullify(obj%z)
      nullify(obj%v)
      nullify(obj%g)
      nullify(obj%dominance)
      nullify(obj%vclipmin)
      nullify(obj%vclipmax)

      call modspec_initialize(obj)
      obj%nlay  = nlay
      obj%stdo  = stdo
      obj%units = units
      obj%angle = angle
      obj%nx = nx
      obj%ny = ny
      obj%nz = nz
      obj%dx = dx
      obj%dy = dy
      obj%dz = dz
      obj%xorg = ox
      obj%yorg = oy
      obj%zorg = oz
      i_err =  modspec_allocate(obj)
      if(i_err /=0) call modspec_delete(obj)
      return
      end subroutine modspec_create_new

      !input in world coordinate units
      subroutine modspec_create_reset(obj,stdo,file,mode,&
      nx,ox,dx,ny,oy,dy,nz,oz,dz)
      type(modspec_struct),pointer :: obj
      integer,intent(in)           :: stdo !stdout unit number
      character(len=*),intent(in)  :: file !input modspec file name
      character(len=*),intent(in)  :: mode !optional behavior flag
      integer, intent(in)          :: nx   !number of x grid points
      integer, intent(in)          :: ny   !number of y grid points
      integer, intent(in)          :: nz   !number of z grid points
      double precision, intent(in) :: ox   !the x grid origin
      double precision, intent(in) :: oy   !the y grid origin
      real, intent(in)             :: oz   !the z grid origin
      real, intent(in)             :: dx   !the x grid bin size
      real, intent(in)             :: dy   !the y grid bin size
      real, intent(in)             :: dz   !the z grid bin size
      integer    ::  i_err       !local variables
      allocate (obj, stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modpsec_create: object allocation tanked'
        nullify(obj)
        return
      endif
      nullify(obj%z)
      nullify(obj%v)
      nullify(obj%g)
      nullify(obj%dominance)
      nullify(obj%vclipmin)
      nullify(obj%vclipmax)

      call modspec_initialize(obj)
      obj%file = file
      obj%stdo = stdo

      !get default parameters from input modspec file
      i_err = modspec_getdefparm_o(obj)
      if(i_err /=0) then
        write(stdo,*) 'modspec_create: modspec_getdefparm_o failed'
        call modspec_delete(obj)
        return
      endif

      !override the default settings in the file

      if(mode == 'GRID') then
        i_err = modspec_rddefmod_reset(obj,nx,ox,dx,&
                ny,oy,dy, nz,oz,dz)
        if(i_err /=0) then
          write(stdo,*) 'modspec_create: modspec_rddefmod failed'
          call modspec_delete(obj)
          return
        endif
      endif
      return
      end subroutine modspec_create_reset

      integer function modspec_allocate(obj) result(status)
      type(modspec_struct),intent(inout) :: obj
      integer  :: i_err
      status = -1
      !free any old memory
      if(associated(obj%z)) deallocate(obj%z)
      if(associated(obj%v)) deallocate(obj%v)
      if(associated(obj%g)) deallocate(obj%g)
      if(associated(obj%vclipmin)) deallocate(obj%vclipmin)
      if(associated(obj%vclipmax)) deallocate(obj%vclipmax)
      if(associated(obj%dominance)) deallocate(obj%dominance)
      nullify(obj%z)
      nullify(obj%v)
      nullify(obj%g)
      nullify(obj%vclipmin)
      nullify(obj%vclipmax)
      nullify(obj%dominance)
      allocate(obj%z(obj%nlay,obj%nx,obj%ny),stat=i_err)
      if(i_err /=0) return
      allocate(obj%v(obj%nlay,obj%nx,obj%ny),stat=i_err)
      if(i_err /=0) return
      allocate(obj%g(obj%nlay,obj%nx,obj%ny),stat=i_err)
      if(i_err /=0) return
      allocate(obj%vclipmin(obj%nlay),stat=i_err)
      if(i_err /=0) return
      allocate(obj%vclipmax(obj%nlay),stat=i_err)
      if(i_err /=0) return
      allocate(obj%dominance(obj%nlay),stat=i_err)
      if(i_err /=0) return
      status = 0
      return
      end function modspec_allocate
!
      subroutine modspec_delete(obj)
      type(modspec_struct),pointer :: obj
      integer    ::  i_err       !local variables
      if(associated(obj%z)) deallocate (obj%z, stat=i_err)
      if(associated(obj%v)) deallocate (obj%v, stat=i_err)
      if(associated(obj%g)) deallocate (obj%g, stat=i_err)
      if(associated(obj%vclipmin)) deallocate (obj%vclipmin, stat=i_err)
      if(associated(obj%vclipmax)) deallocate (obj%vclipmax, stat=i_err)
      if(associated(obj%dominance)) deallocate (obj%dominance, stat=i_err)
      deallocate (obj)
      nullify(obj)
      return
      end subroutine modspec_delete
!
      subroutine modspec_initialize(obj)
      type(modspec_struct),intent(inout) :: obj
      obj%file = ' '
      obj%domain = 'DEPTH'
      obj%nlay = 1
      obj%nlay = 0
      obj%stdo = 6
      obj%nx   = 1
      obj%ny   = 1
      obj%nz   = 1
      obj%xorg = 0.0
      obj%yorg = 0.0
      obj%zorg = 0.0
      obj%dx   = 10.0
      obj%dy   = 10.0
      obj%dz   = 10.0
      obj%angle= 0.0
      obj%units= 'M'
      obj%hx   = 17
      obj%hy   = 18
      obj%hz   = -3 !interpreted by modgrid as DEPTH
      obj%hdra = 0 !8
      obj%hdrb = 0 !7
      obj%has_coef = .false.
      obj%a1   = 0
      obj%a2   = 0
      obj%a3   = 0
      obj%b1   = 0
      obj%b2   = 0
      obj%b3   = 0
      obj%xyz_order = 'ZXY'
      obj%xy_reversed = .false.
      obj%znon= mspec_gnull
      obj%modspec_verbose = .false.
      return
      end subroutine modspec_initialize


      function modspec_get_domain (obj) result (domain)
      type(modspec_struct),intent(in) :: obj
      integer                         :: domain  ! result
      if (obj%domain == 'DEPTH') then
           domain = MODSPEC_DEPTH
      else
           domain = MODSPEC_TIME
      end if
      end function modspec_get_domain


      subroutine modspec_getdesc(obj,nlay,angle,units,&
      nx,ox,dx,ny,oy,dy,nz,oz,dz)
      type(modspec_struct),intent(in) :: obj
      character(len =*),intent(out) :: units!M or F units flag
      real, intent(out)             :: angle!modspec rotation angle
      integer, intent(out)          :: nlay !number of layers
      integer, intent(out)          :: nx   !number of x points
      integer, intent(out)          :: ny   !number of y points
      integer, intent(out)          :: nz   !number of z points
      double precision, intent(out) :: ox   !the x grid origin - wc
      double precision, intent(out) :: oy   !the y grid origin - wc
      real, intent(out)             :: oz   !the z grid origin - wc
      real, intent(out)             :: dx   !the x grid bin size - wc
      real, intent(out)             :: dy   !the y grid bin size - wc
      real, intent(out)             :: dz   !the z grid bin size - wc
      nlay = obj%nlay
      units= obj%units
      nx = obj%nx
      ny = obj%ny
      nz = obj%nz
      dx = obj%dx
      dy = obj%dy
      dz = obj%dz
      ox = obj%xorg
      oy = obj%yorg
      oz = obj%zorg
      if(obj%xy_reversed) then
        ny = obj%nx
        nx = obj%ny
        dy = obj%dx
        dx = obj%dy
        oy = obj%xorg
        ox = obj%yorg
      endif
      angle = obj%angle
      return
      end subroutine modspec_getdesc

      ! xo,yo   = model origin (xorgdef,yorgdef) in grid units
      ! xgo,ygo = grid origin in grid units (min values)
      ! dx, dy  = grid bin sizes which can be < 0
      ! cps header for x,y axis
      logical function modspec_get_coef_full(obj,a,b, xo, yo, dx, dy, xgo,ygo,&
      hx,hy,xyz_order) &
      result(status)
      type(modspec_struct),intent(in) :: obj
      real,intent(out)   :: a(*)
      real,intent(out)   :: b(*)
      real,intent(inout) :: xo,yo
      real,intent(inout) :: dx,dy
      real,intent(inout) :: xgo,ygo
      integer,intent(inout) :: hx,hy
      character(len=*),intent(inout) :: xyz_order
      a(1:3) = 0
      b(1:3) = 0
      xo = obj%xorg  !default to model values
      yo = obj%yorg
      xgo= xo
      ygo= yo
      dx = obj%dx
      dy = obj%dy
      hx = obj%hx
      hy = obj%hy
      xyz_order=obj%xyz_order
      status = obj%has_coef
      if(.not. obj%has_coef) return
      ! line = a1*ix + a2*iy + a3
      ! trace= b1*ix + b2*iy + b3   (2 of the 6 coef should be zero)
      !  assume (line,trace) ==> CPS (7,8) or (xgloc,ygloc)
      !  line(iy=1) = a2 + a3  = ygloc(1) = ygo
      !  trace(ix=1) = b1 + b3 = xgloc(1) = xgo
      a(1) = obj%a1
      a(2) = obj%a2
      a(3) = obj%a3
      b(1) = obj%b1
      b(2) = obj%b2
      b(3) = obj%b3
      if(obj%a1*obj%b2==0 .and. obj%b1*obj%a2==0) then
        write(obj%stdo,*) 'modspec_get_coef: error bad coeficients'
        write(obj%stdo,*) 'modspec_get_coef: a1=',obj%a1,' b2=',obj%b2
        write(obj%stdo,*) 'modspec_get_coef: b1=',obj%b1,' a2=',obj%a2
        return
      endif

      !implicit rules for x and y
      if(obj%a1/=0) then  !line is x, trace is y
          xo = obj%a1 + obj%a3
          yo = obj%b2 + obj%b3
          dx = obj%a1
          dy = obj%b2
      endif
      if(obj%b1/=0) then  !trace is x, line is y
          xo = obj%b1 + obj%b3
          yo = obj%a2 + obj%a3
          dx = obj%b1
          dy = obj%a2
      endif
      !explicit rules for x and y when hdra and hdrb are set
      if(obj%hdra/=0 .and. obj%hdrb/=0) then
        if(obj%xyz_order=='ZXY') then
          if(obj%b1==0) then !b axis is modspec y and cps x
            xo = obj%b2 + obj%b3
            yo = obj%a1 + obj%a3
            dx = obj%b2
            dy = obj%a1
          else               !b axis is modspec x and cps x
            xo = obj%b1 + obj%b3
            yo = obj%a2 + obj%a3
            dx = obj%b1
            dy = obj%a2
          endif
        else
          if(obj%b1==0) then  !b axis is modspec y and cps y
            xo = obj%a1 + obj%a3
            yo = obj%b2 + obj%b3
            dx = obj%a1
            dy = obj%b2
          else                !b axis is modspec x but cps y
            xo = obj%a2 + obj%a3
            yo = obj%b1 + obj%b3
            dx = obj%a2
            dy = obj%b1
          endif
        endif
      endif
      xgo = xo
      ygo = yo
      ! return a minimum origin
      if(dx< 0) then
        xgo = xo - (obj%nx-1)*abs(dx)
        if(obj%xy_reversed) then
          xgo = xo - (obj%ny-1)*abs(dx)
        endif
      endif
      if(dy< 0) then
        ygo = yo - (obj%ny-1)*abs(dy)
        if(obj%xy_reversed) then
          ygo = yo - (obj%nx-1)*abs(dy)
        endif
      endif
      return
      end function modspec_get_coef_full

      logical function modspec_get_coef(obj,a,b, xo, yo, dx, dy, xgo,ygo) &
      result(status)
      type(modspec_struct),intent(in) :: obj
      real,intent(out)   :: a(*)
      real,intent(out)   :: b(*)
      real,intent(inout) :: xo,yo
      real,intent(inout) :: dx,dy
      real,intent(inout) :: xgo,ygo
      integer  :: hx,hy
      character(len=4):: xyz_order
      status = modspec_get_coef_full(obj,a,b, xo, yo, dx, dy, xgo,ygo,&
      hx,hy,xyz_order)
      return
      end function modspec_get_coef

      subroutine modspec_get_file(obj,file)
      type(modspec_struct),intent(in)    :: obj
      character(len=*),intent(out)       :: file
      file = obj%file
      return
      end subroutine modspec_get_file

      subroutine modspec_get_nlay(obj,nlay)
      type(modspec_struct),intent(in) :: obj
      integer,intent(out)       :: nlay
      nlay = obj%nlay
      return
      end subroutine modspec_get_nlay

      logical function modspec_has_coef(obj) result(hc)
      type(modspec_struct),intent(in) :: obj
      hc = obj%has_coef
      return
      end function modspec_has_coef

      logical function modspec_reversed(obj) result(reversed)
      type(modspec_struct),intent(in) :: obj
      reversed = obj%xy_reversed
      return
      end function modspec_reversed

      subroutine modspec_get_orig(obj,xorg,yorg)
      type(modspec_struct),intent(in) :: obj
      double precision,intent(out)       :: xorg,yorg
      xorg = obj%xorg
      yorg = obj%yorg
      return
      end subroutine modspec_get_orig

      real function modspec_get_angle(obj) result(angle)
      type(modspec_struct),intent(inout) :: obj
      angle = obj%angle
      return
      end function modspec_get_angle

      subroutine modspec_get_z_desc(obj,nz,oz,dz)
      type(modspec_struct),intent(inout) :: obj
      integer, intent(out)  :: nz
      real, intent(out)     :: oz
      real, intent(out)     :: dz
      nz = obj%nz
      oz = obj%zorg
      dz = obj%dz
      return
      end subroutine modspec_get_z_desc

      subroutine modspec_set_z_desc(obj,nz,oz,dz)
      type(modspec_struct),intent(inout) :: obj
      integer, intent(in)  :: nz
      real, intent(in)     :: oz
      real, intent(in)     :: dz
      obj%nz = nz
      obj%zorg = oz
      obj%dz = dz
      return
      end subroutine modspec_set_z_desc

      subroutine modspec_setdesc(obj,nlay,angle,units,nx,ox,dx,ny,oy,dy)
      type(modspec_struct),intent(inout) :: obj
      character(len = *), intent(in) :: units
      integer, intent(in)          :: nlay
      integer, intent(in)          :: nx
      integer, intent(in)          :: ny
      double precision, intent(in) :: ox
      double precision, intent(in) :: oy
      real, intent(in)             :: dx
      real, intent(in)             :: dy
      real, intent(in)             :: angle
      integer          :: nx1
      integer          :: ny1
      double precision :: ox1
      double precision :: oy1
      real             :: dx1
      real             :: dy1
      integer          :: i

      nx1 = obj%nx
      ny1 = obj%ny
      if(units /= obj%units) then
        do i = 1,obj%nlay
          dx1 = obj%dx
          dy1 = obj%dy
          ox1 = obj%xorg
          oy1 = obj%yorg
          call gridlib_convert_units(1,obj%znon,&
           obj%units(1:1),units(1:1),&
           obj%z(i,:,:),nx1,ny1,dx1,dy1,ox1,oy1,angle=0.0)
          dx1 = obj%dx
          dy1 = obj%dy
          ox1 = obj%xorg
          oy1 = obj%yorg
          call gridlib_convert_units(1,obj%znon,&
           obj%units(1:1),units(1:1),&
           obj%v(i,:,:),nx1,ny1,dx1,dy1,ox1,oy1,angle=0.0)
        enddo

        obj%units = units
        obj%dx = dx1
        obj%dy = dy1
        obj%xorg = ox1
        obj%yorg = oy1
      endif
      return
      end subroutine modspec_setdesc

      subroutine modspec_get_zvg(obj,z,v,g)
      type(modspec_struct),intent(in) :: obj
      real,pointer    :: z(:,:,:) !depth grids nlay*nx*ny
      real,pointer    :: v(:,:,:) !velocity grids nlay*nx*ny
      real,pointer    :: g(:,:,:) !gradient grids nlay*nx*ny
      z=>obj%z
      v=>obj%v
      g=>obj%g
      return
      end subroutine modspec_get_zvg

      subroutine modspec_get_zlimits(obj,zmin,zmax)
      type(modspec_struct),intent(in) :: obj
      real,intent(out)                :: zmin
      real,intent(out)                :: zmax
      real,pointer    :: z(:,:,:)
      integer         :: i,ix,iy
      zmin=0.0
      zmax=0.0
      if(.not. associated(obj%z)) return
      z=>obj%z
      zmin = z(1,1,1)
      zmax = z(1,1,1)
      do i = 1,size(z,1)
        do ix = 1,size(z,2)
        do iy = 1,size(z,3)
          if(z(i,ix,iy)/= obj%znon) then
           zmin = min(zmin,z(i,ix,iy))
           zmax = max(zmax,z(i,ix,iy))
          endif
        enddo
        enddo
      enddo
      return
      end subroutine modspec_get_zlimits

      subroutine modspec_get_hdrs(obj,hx,hy,hz)
      type(modspec_struct),intent(in) :: obj
      integer,intent(out)                :: hx
      integer,intent(out)                :: hy
      integer,intent(out)                :: hz
      hx=obj%hx
      hy=obj%hy
      hz=obj%hz
      return
      end subroutine modspec_get_hdrs

      subroutine modspec_set_hdrs(obj,hx,hy,hz)
      type(modspec_struct),intent(inout) :: obj
      integer,intent(in)                :: hx
      integer,intent(in)                :: hy
      integer,intent(in)                :: hz
      obj%hx = hx
      obj%hy = hy
      obj%hz = hz
      return
      end subroutine modspec_set_hdrs

      subroutine modspec_get_order(obj,xyz_order)
      type(modspec_struct),intent(in) :: obj
      character(len=*)                :: xyz_order
      xyz_order = ' '
      xyz_order=obj%xyz_order
      return
      end subroutine modspec_get_order

      subroutine modspec_get_z(obj,z)
      type(modspec_struct),intent(in) :: obj
      real,pointer    :: z(:,:,:)
      z=>obj%z
      return
      end subroutine modspec_get_z

      subroutine modspec_get_v(obj,v)
      type(modspec_struct),intent(in) :: obj
      real,pointer    :: v(:,:,:)
      v=>obj%v
      return
      end subroutine modspec_get_v

      subroutine modspec_get_g(obj,g)
      type(modspec_struct),intent(in) :: obj
      real,pointer    :: g(:,:,:)
      g=>obj%g
      return
      end subroutine modspec_get_g

      subroutine modspec_get_vclips(obj,vclipmin,vclipmax)
      type(modspec_struct),intent(in) :: obj
      real,pointer    :: vclipmin(:)
      real,pointer    :: vclipmax(:)
      vclipmin=>obj%vclipmin
      vclipmax=>obj%vclipmax
      return
      end subroutine modspec_get_vclips

      integer function modspec_readgrid(stdo, filein,&
        grid, nx, ny, xorg, yorg, dx, dy, angle, znon) result(i_err)
      integer,intent(in)    :: stdo
      character (len = *), intent(in) :: filein
      real,pointer          :: grid (:)
      integer,intent(inout) :: nx
      integer,intent(inout) :: ny
      double precision,intent(inout) :: xorg
      double precision,intent(inout) :: yorg
      real,intent(inout)    :: dx
      real,intent(inout)    :: dy
      real,intent(inout)    :: angle
      real,intent(inout)    :: znon
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: iunit
      integer :: err
      integer :: iflag, ip
      real    :: gridmin, gridmax
      integer :: ngridmax
      integer :: errRetVal
      logical :: reset_znon
      real,pointer :: gwrk(:)
      character(len=32) :: description, attribute
      character(len=32) :: cunits, punits

!-----------------------------------------------
!
     reset_znon = .true.
     nullify(grid)
     nullify(gwrk)
     call getlun (iunit,i_err)
     if(i_err /= 0) then
       write (stdo, *) 'modspec_readgrid: ERROR in getlun'
       i_err = -1
       return
     endif
     ngridmax=10000000
     call gridlib_read_grid_header(stdo,iunit,filein,ngridmax,  &
          nx,ny,xorg,yorg,dx,dy,angle,znon,description, attribute, &
          cunits, punits, errRetVal)
     if(errRetVal /= 0) then
        goto 99
     endif
     ngridmax = nx*ny
     allocate(gwrk(ngridmax),stat=err)
     if(err /=0) then
       write (stdo, *) 'modspec_readgrid: ERROR in gwrk allocate'
       i_err = -1
       return
     endif
     i_err = -1
      call gridlib_read_grid(stdo,iunit,filein,gwrk,ngridmax, &
            nx,ny,xorg,yorg,dx,dy,angle,znon,reset_znon,&
            description, attribute, &
            cunits, punits,errRetVal)
      if(errRetVal /= 0) then
        goto 99
      endif
      allocate(grid(nx*ny),stat=err)
      if(err /=0) then
        write (stdo, *) 'modspec_readgrid: ERROR in grid allocate'
        goto 99
      endif
      grid(1:nx*ny) = gwrk(1:nx*ny)
      if(associated(gwrk)) deallocate(gwrk)

!---Check to see what the min and max values are.
!---Ignore ZNON's
      iflag = 0
      do ip = 1, nx*ny
       !if (grid(ip) == 0.0) cycle
        if (grid(ip) == znon) cycle
        if (iflag == 0) then
          gridmin = grid(ip)
          gridmax = grid(ip)
          iflag = 1
        else
          gridmin = amin1(grid(ip),gridmin)
          gridmax = amax1(grid(ip),gridmax)
        endif
      end do

!---Print out important grid information
      if(stat_verbose) then
        write (stdo, *) ' '
        write (stdo, *) 'modspec_readgrid: FILE    = ', trim(filein)
        write (stdo, *) 'modspec_readgrid: XORG    = ', xorg,&
         '  YORG    = ', yorg
        write (stdo, *) 'modspec_readgrid: DX      = ', dx, '  DY      = ', dy
        write (stdo, *) 'modspec_readgrid: NX      = ', nx, '  NY      = ', ny
        write (stdo, *) 'modspec_readgrid: ANGLE   = ', angle
        write (stdo, *) 'modspec_readgrid: GRIDMIN = ', gridmin,&
         '    GRIDMAX = ', gridmax
        write (stdo, *) ' '
      endif

      i_err = 0
 99   continue
      if(associated(gwrk)) deallocate(gwrk)
      return
      end function modspec_readgrid


!********************************************************
      subroutine modspec_writegrid(iunit, fileout, grid, nxdim, nydim,&
        nx, ny, xorg, yorg, deltax, deltay, &
        angle, description, attribute,cunits,punits, znon, ier)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit
      integer , intent(in) :: nxdim
      integer , intent(in) :: nydim
      integer , intent(in) :: nx
      integer , intent(in) :: ny
      integer , intent(out) :: ier
      real , intent(in) :: deltax
      real , intent(in) :: deltay
      real , intent(in) :: angle
      real , intent(in) :: znon
      character(len=*), intent(in)  :: description
      character(len=*), intent(in)  :: attribute
      character(len=*), intent(in)  :: punits
      character(len=*), intent(in)  :: cunits
      double precision , intent(in) :: xorg
      double precision , intent(in) :: yorg
      character(len=*) , intent(in) :: fileout
      real , intent(in) :: grid(nxdim,nydim)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      character(len=120)  :: msg
!-----------------------------------------------
!
      ier = 0

      call gridlib_write_grid(fileout, xorg, yorg, nx, ny, deltax, deltay, &
                                     angle, znon, nxdim, nydim, grid,        &
                                     description, attribute, cunits, punits, &
                                     ier, msg)
      return
      end subroutine modspec_writegrid


      subroutine modspec_writegrid_fmt(iunit,fileout,grid,nxdim,nydim,  &
                                  nx,ny,xorg,yorg,deltax,deltay,angle,znon,ier)
      implicit none

      character(len=*),intent(in)   :: fileout
      integer,intent(in)    :: iunit
      integer,intent(in)    :: nxdim
      integer,intent(in)    :: nydim
      integer,intent(in)    :: nx
      integer,intent(in)    :: ny
      real,intent(in)       :: grid(nxdim,nydim)
      double precision,intent(in) :: xorg
      double precision,intent(in) :: yorg
      real,intent(in)       :: deltax
      real,intent(in)       :: deltay
      real,intent(in)       :: angle
      real,intent(in)        :: znon
      integer,intent(inout) :: ier

!---Local declarations
         integer            ::  iost

         ier=0
!---Open grid file
         open(iunit,file=fileout,status='unknown',iostat=iost)
         if (iost/=0) then
           ier=-1
           return
         end if

         call gridlib_write_grid_fmt(iunit,fileout,grid,nxdim,nydim,  &
              nx,ny,xorg,yorg,deltax,deltay,   &
              angle,znon,ier)
         close(iunit)

         return
         end  subroutine modspec_writegrid_fmt

      subroutine modspec_grid2grid( stdo,&
        iflag, znon, &
        grid1, nx1, ny1, dx1, dy1, xorg1, yorg1, angle1, units1, &
        grid2, nx2, ny2, dx2, dy2, xorg2, yorg2, angle2, units2,&
        no_extrap)

      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,intent(in)   :: stdo
      integer,intent(in)   :: iflag
      real, intent(in)     :: znon
      integer,intent(in)   :: nx1
      integer,intent(in)   :: ny1
      integer,intent(in)   :: nx2
      integer,intent(in)   :: ny2
      real, intent(inout)  :: dx1
      real, intent(inout)  :: dy1
      real, intent(in)     :: dx2
      real, intent(in)     :: dy2
      real, intent(inout)  :: grid1 (nx1,ny1)
      real, intent(out)    :: grid2 (nx2,ny2)
      double precision, intent(inout) :: xorg1
      double precision, intent(inout) :: yorg1
      double precision, intent(in) :: xorg2
      double precision, intent(in) :: yorg2
      real, intent(in)     :: angle1
      character(len = *),intent(in):: units1
      real, intent(in)     :: angle2
      character(len = *),intent(in):: units2
      integer,intent(in)   :: no_extrap

      call gridlib_convert_grid(stdo,iflag,znon,            &
           grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1(1:1), &
           grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2(1:1), &
           no_extrap)

      return
      end subroutine modspec_grid2grid

      integer function modspec_getdefparm_a(stdo, filespec, nlaydef, &
       xorgdef, yorgdef, dxdef, dydef, nxdef, nydef,&
       angledef, unitsdef) result(status)
      integer,intent(in)             :: stdo
      character(len = *), intent(in) :: filespec
      integer, intent(out)           :: nlaydef
      double precision,intent(out)   :: xorgdef
      double precision,intent(out)   :: yorgdef
      real,   intent(out)            :: dxdef
      real,   intent(out)            :: dydef
      integer,intent(out)            :: nxdef
      integer,intent(out)            :: nydef
      real,   intent(out)            :: angledef
      character(len=*),intent(out)   :: unitsdef
      type(modspec_struct),pointer :: obj

      nullify (obj) ! jpa

      status = -1
      ! create call reads in the default parameters
      call modspec_create(obj,stdo,filespec,'PARMS')
      if(.not. associated(obj)) return
      nlaydef = obj%nlay
      xorgdef = obj%xorg
      yorgdef = obj%yorg
      dxdef   = obj%dx
      dydef   = obj%dy
      nxdef   = obj%nx
      nydef   = obj%ny
      angledef= obj%angle
      unitsdef= obj%units
      call modspec_delete(obj)
      status = 0
      return
      end function modspec_getdefparm_a

      integer function modspec_getdefparm_o(obj) result(status)
      type(modspec_struct),intent(inout) :: obj
      integer  :: lun
      integer  :: i_err
      status = -1
      if(obj%file==' ') return
      if(obj%file=='NONE') return
      call getlun (lun,i_err)
      if(i_err /= 0) then
        return
      endif
      open(unit=lun, file=obj%file, status='old',err=91, action='read')
      go to 95
 91   continue
      write(obj%stdo,*) 'modspec_getdefparms: open error ',trim(obj%file)
      return
 95   continue
      status = modspec_getdefparm_i(obj, lun)
      close(unit=lun)
      return
      end function modspec_getdefparm_o

      integer function modspec_getdefparm_i(obj, lun) result(status)
      implicit none
      type(modspec_struct),intent(inout) :: obj
      integer,intent(in)                 :: lun

      logical    :: found
      type(cardset_struct),pointer :: cobj
      character(len=80) :: card
      character(len=80) :: cards(100)
      character(len=80) :: msg

      integer  :: nc
      integer  :: hx,hy
      character(len=4):: xyz_order
      logical  :: hasc
      real     :: a(3), b(3)
      real     :: xo,yo
      real     :: dx,dy
      real     :: xgo,ygo

!-----------------------------------------------

      nullify (cobj) ! jpa

      status = -1
      if(lun <= 0) return

     !call gridlib_read_def_model_parms(lun,obj%stdo,obj%file,obj%nlay, &
     !             obj%xorg,obj%yorg,obj%dx,obj%dy,   &
     !             obj%nx,obj%ny,obj%angle,obj%units, &
     !             obj%nz,obj%dz,obj%znon,&
     !             obj%a1,obj%a2,obj%a3,obj%b1,obj%b2,obj%b3,&
     !             obj%hdra,obj%hdrb,    &
     !             obj%domain,status)
      nc=0
       card = ' '
      found = .false.
      do while (nc< size(cards))

        read (lun, '(A)', end=150) card
        if(card(1:1) /= '*') goto 150
        if(.not. found) then
          if(index(card,'NLAYDEF=') > 0) found = .true.
          if(index(card,'A3=') > 0 .or. index(card,'B3=') > 0 ) then
            found = .true.
            obj%has_coef = .true.
          endif
          call cardset_create(cobj)
          nc = nc+1
          card(1:1)=' '
          cards(nc) = card
        else
          if(card(1:1)== '#') cycle
          if(card== ' ') cycle
          if(index(card,' LAYER ') > 0) exit
          if(index(card,'=') == 0) then
            cycle
          else
           nc = nc+1
           card(1:1)=' '
           cards(nc) = card
          endif
        endif
      enddo
 150  continue
      if(nc > 0) then
        call cardset_put_cards(cobj,cards(1:nc),nc)
      else
        write(obj%stdo,*) 'modspec_getdefparms: no input cards?'
        return
      endif
      status = 0
! Get modspec parameter values from the cardset
      call cardset_get_scalar(cobj,'NLAYDEF', obj%nlay,  msg)
      obj%nz = obj%nlay
      call cardset_get_scalar(cobj,'NXDEF',   obj%nx,    msg)
      call cardset_get_scalar(cobj,'NYDEF',   obj%ny,    msg)
      call cardset_get_scalar(cobj,'NZDEF',   obj%nz,    msg)
      call cardset_get_scalar(cobj,'XORGDEF', obj%xorg,  msg)
      call cardset_get_scalar(cobj,'YORGDEF', obj%yorg,  msg)
      call cardset_get_scalar(cobj,'ZORGDEF', obj%zorg,  msg)
      call cardset_get_scalar(cobj,'DXDEF',   obj%dx,    msg)
      call cardset_get_scalar(cobj,'DYDEF',   obj%dy,    msg)
      call cardset_get_scalar(cobj,'DZDEF',   obj%dz,    msg)
      call cardset_get_scalar(cobj,'ANGLEDEF',obj%angle, msg)
      call cardset_get_scalar(cobj,'UNITSDEF',obj%units, msg)
      call cardset_get_scalar(cobj,'ZNON'    ,obj%znon,  msg)
      call cardset_get_scalar(cobj,'DOMAIN'  ,obj%domain,msg)

      call cardset_get_scalar(cobj,'HXDEF'   ,obj%hx,    msg)
      call cardset_get_scalar(cobj,'HYDEF'   ,obj%hy,    msg)
      call cardset_get_scalar(cobj,'HZDEF'   ,obj%hz,    msg)
      call cardset_get_scalar(cobj,'CPS_HDRA',obj%hdra,  msg)
      call cardset_get_scalar(cobj,'CPS_HDRB',obj%hdrb,  msg)
      if(obj%domain(1:1)=='d') obj%domain='DEPTH'
      !
      !  modspec local coordinate
      !   xloc(i) = (i-1)*dxdef
      !   yloc(j) = (j-1)*dydef
      !  modspec world coordinate
      !   xwc(i,j) = xloc(i)*cos(theta) - yloc(j)*sin(theta) + xorgdef
      !   ywc(i,j) = xloc(i)*sin(theta) + yloc(j)*cos(theta) + yorgdef
      !  grid local coordinate
      !   xgloc(i) = xgo + (i-1)*dxgdef
      !   ygloc(j) = ygo + (j-1)*dygdef
      !  grid local to modspec local
      !   xloc(i)  = (xgloc(i) - xgo)*dxdef/dxgdef + xl0
      !   yloc(j)  = (ygloc(j) - ygo)*dydef/dygdef + yl0
      !  where (xl0,yl0) is the point (xg0,yg0) in the modspec local c.s.
      obj%xyz_order = 'ZXY'
      if(obj%has_coef) then
        call cardset_get_scalar(cobj,'A1'   ,obj%a1,    msg)
        call cardset_get_scalar(cobj,'A2'   ,obj%a2,    msg)
        call cardset_get_scalar(cobj,'A3'   ,obj%a3,    msg)
        call cardset_get_scalar(cobj,'B1'   ,obj%b1,    msg)
        call cardset_get_scalar(cobj,'B2'   ,obj%b2,    msg)
        call cardset_get_scalar(cobj,'B3'   ,obj%b3,    msg)
        if(obj%a1*obj%b2==0 .and. obj%b1*obj%a2==0) then
          write(obj%stdo,*) 'modspec_getdefparm: error bad coeficients'
          write(obj%stdo,*) 'modspec_getdefparm: a1=',obj%a1,' b2=',obj%b2
          write(obj%stdo,*) 'modspec_getdefparm: b1=',obj%b1,' a2=',obj%a2
          status = -1
          return
        endif
        !defaults
        obj%hx = 7
        obj%hy = 8
        if(obj%a1/=0) then  !implicit rule for line is x, trace is y
          obj%xyz_order = 'ZYX'
        endif
        if(obj%hdra/=0 .and. obj%hdrb/=0) then !explicit x-y information
          if(obj%hdrb==HDR_MIDPOINT_XGRID) obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_MIDPOINT_XLOC)  obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_SOURCE_XGRID)   obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_RECEIVER_XGRID) obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_SOURCE_XLOC)    obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_RECEIVER_XLOC)  obj%xyz_order='ZXY'
          if(obj%hdrb==HDR_MIDPOINT_YGRID) obj%xyz_order='ZYX'
          if(obj%hdrb==HDR_MIDPOINT_YLOC)  obj%xyz_order='ZYX'
          if(obj%hdrb==HDR_SOURCE_YGRID)   obj%xyz_order='ZYX'
          if(obj%hdrb==HDR_RECEIVER_YGRID) obj%xyz_order='ZYX'
          if(obj%hdrb==HDR_SOURCE_YLOC)    obj%xyz_order='ZYX'
          if(obj%hdrb==HDR_RECEIVER_YLOC)  obj%xyz_order='ZYX'
          if(obj%xyz_order=='ZXY') then
            obj%hx=obj%hdrb
            obj%hy=obj%hdra
            if(obj%b1==0) then  !b axis is modspec y but cps x
              obj%xy_reversed= .true.
            endif
          else
            obj%hx=obj%hdra
            obj%hy=obj%hdrb
            if(obj%b1/=0) then  !b axis is modspec x but cps y
              obj%xy_reversed= .true.
            endif
          endif
        endif
      !  line = a1*ix + a2*iy + a3   (slow coordinate)
      !  trace= b1*ix + b2*iy + b3   (fast coordinate)
      !  (2 of the 6 coef should be zero)
      !  if a1=0 and b2=0 (trace,line) ==> CPS (7,8) or (xgloc,ygloc)
      !  line(iy=1)  = a2 + a3 = ygloc(1) = ygo
      !  trace(ix=1) = b1 + b3 = xgloc(1) = xgo
      !  a2 = dygdef
      !  b1 = dxgdef
        hasc = modspec_get_coef_full(obj,a,b, xo, yo, dx, dy, xgo,ygo,&
             hx,hy,xyz_order)
      endif
      call cardset_delete(cobj)

      if (obj%units== 'f') obj%units= 'F'
      if (obj%units== 'm') obj%units= 'M'
      if(obj%nlay <1) then
        write(obj%stdo,*) 'modspec_getdefparm: error nlay=',obj%nlay
      endif
      return
      end function modspec_getdefparm_i


! modspec_rddefmod: convert all units and grids to default units and grid
      integer function modspec_rddefmod(obj) result(status)
      type(modspec_struct),intent(inout) :: obj
      integer          :: nlay
      character(len=4) :: units
      integer          :: stdo
      integer          :: nx
      integer          :: ny
      real             :: dx
      real             :: dy
      double precision :: xorg
      double precision :: yorg
      real             :: angle
      integer          :: lun,grdunit
      integer          :: i_err
      real,pointer     :: gridin(:)
      real,pointer     :: gridout(:)
      integer          :: ngridmax

!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      nullify(gridin)
      nullify(gridout)
      status = -1
      if(obj%file ==' ') return
      if(obj%nlay < 1 ) return
      stdo=obj%stdo
      nx  = obj%nx
      ny  = obj%ny
      dx  = obj%dx
      dy  = obj%dy
      xorg = obj%xorg
      yorg = obj%yorg
      nlay = obj%nlay
      units= obj%units
      angle= obj%angle

      call getlun(grdunit,i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod: getlun failied'
        return
      endif
      call getlun(lun,i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod: getlun failied'
        return
      endif
!
!  Allocate space for target grids
      i_err =  modspec_allocate(obj)
      allocate(gridin(nx),stat=i_err) !irrelevant
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod: gridout allocation error',nx,ny
        return
      endif
      ngridmax = max(1000000,nx*ny + 2*nx)
      allocate(gridout(ngridmax),stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod: gridout allocation error',ngridmax
        return
      endif

      call gridlib_model_from_grid(stdo,lun,grdunit,obj%file,   &
                   units,nx,ny,nlay,xorg,yorg,dx,dy,angle,&
                   obj%v,obj%z,obj%g,&
                   obj%vclipmin,&
                   obj%vclipmax,&
                   gridin,gridout,ngridmax,&
                   obj%znon,      &
                   status)

      if(associated(gridin)) deallocate(gridin)
      if(associated(gridout)) deallocate(gridout)
      return
      end function modspec_rddefmod

      !input in world coordinate units
      !regrid but keep angle,units, and number of layers fixed
      !will change number of points the origin, and the bin size
      !headers and xyz order are unchanged
      integer function modspec_rddefmod_reset(obj,nx,ox,dx,&
      ny,oy,dy, nz,oz,dz) result(status)
      type(modspec_struct),intent(inout) :: obj
      integer, intent(in)          :: nx   !number of x grid points
      integer, intent(in)          :: ny   !number of y grid points
      integer, intent(in)          :: nz   !number of z grid points
      double precision, intent(in) :: ox   !the x grid origin
      double precision, intent(in) :: oy   !the y grid origin
      real, intent(in)             :: oz   !the z grid origin
      real, intent(in)             :: dx   !the x grid bin size
      real, intent(in)             :: dy   !the y grid bin size
      real, intent(in)             :: dz   !the z grid bin size

      integer          :: nlay
      character(len=4) :: units
      integer          :: stdo
      real             :: angle
      integer          :: lun,grdunit
      integer          :: i_err
      real,pointer     :: gridin(:)
      real,pointer     :: gridout(:)
      integer          :: ngridmax
      real             :: a(3),b(3)

!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      nullify(gridin)
      nullify(gridout)
      status = -1
      if(obj%file ==' ') return
      if(obj%nlay < 1 ) return
      stdo=obj%stdo
      nlay = obj%nlay
      units= obj%units
      angle= obj%angle
      a = 0.0
      b = 0.0
      if(obj%has_coef) then
        !reset the coeficients
        i_err = modspec_transform_abs(obj,nx,ny,ox,oy,dx,dy,a,b)
        if(i_err /=0) then
           write(stdo,*) 'modspec_rddefmod_reset: ab transform error'
          return
        endif
      endif
      obj%a1 = a(1)
      obj%b1 = b(1)
      obj%a2 = a(2)
      obj%b2 = b(2)
      obj%a3 = a(3)
      obj%b3 = b(3)

      call getlun(grdunit,i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod_reset: getlun failied'
        return
      endif
      call getlun(lun,i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod_reset: getlun failied'
        return
      endif
!
!  Allocate space for target grids
      !override the default settings in the file
      obj%nx = nx
      obj%ny = ny
      obj%nz = nz
      obj%dx = dx
      obj%dy = dy
      obj%dz = dz
      obj%xorg = ox
      obj%yorg = oy  !RSD bug fixed 3-31-05
      obj%zorg = oz

      i_err =  modspec_allocate(obj)
      allocate(gridin(nx),stat=i_err) !irrelevant
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod_reset: gridout allocate error',nx,ny
        return
      endif
      ngridmax = max(1000000,nx*ny + 2*nx)
      allocate(gridout(ngridmax),stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modspec_rddefmod_reset: gridout allocate error',ngridmax
        return
      endif


      !
      !the coordinates should be world coordinates
      call gridlib_model_from_grid(stdo,lun,grdunit,obj%file,   &
                   units,nx,ny,nlay,ox,oy,dx,dy,angle,&
                   obj%v,obj%z,obj%g,&
                   obj%vclipmin,&
                   obj%vclipmax,&
                   gridin,gridout,ngridmax,&
                   obj%znon,      &
                   status)


      if(associated(gridin)) deallocate(gridin)
      if(associated(gridout)) deallocate(gridout)
      return
      end function modspec_rddefmod_reset

      integer function modspec_print_to_cards(obj,cards) result(cnt)
      type(modspec_struct),intent(in) :: obj
      character(len=*),intent(out)  :: cards(:)
      cnt = 0
      if(size(cards)<10) return
      cnt = 1
      write (cards(cnt), *) 'modspec: HAS_COEF  = ', obj%has_coef
      if(cnt==size(cards)) return
      if(obj%has_coef) then
        cnt = cnt + 1
        write (cards(cnt), *) 'modspec_print: CPS_HDRA  = ', obj%hdra
        cnt = cnt + 1
        write (cards(cnt), *) 'modspec_print: CPS_HDRB  = ', obj%hdrb
        cnt = cnt + 1
        write (cards(cnt), *) 'modspec: a1= ',obj%a1,' a2=',obj%a2,&
        ' a3=',obj%a3
        cnt = cnt + 1
        write (cards(cnt), *) 'modspec: b1= ',obj%b1,' b2=',obj%b2,&
        ' b3=',obj%b3
      endif
      if(cnt==size(cards)) return
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: DOMAIN  = ', obj%domain
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: UNITS  = ', obj%units
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: XORG    = ', obj%xorg,&
      '  YORG    = ', obj%yorg
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: DX      = ', obj%dx,&
      '  DY      = ', obj%dy
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: NX      = ', obj%nx,&
      '  NY      = ', obj%ny
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: NLAY    = ', obj%nlay
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: ANGLE   = ', obj%angle
      cnt = cnt + 1
      write (cards(cnt), *) 'modspec: xy_reversed = ',obj%xy_reversed

      return
      end function modspec_print_to_cards

      subroutine modspec_print_obj(obj,stdo)
      type(modspec_struct),intent(in) :: obj
      integer , intent(in) :: stdo
      write (stdo, *) 'modspec_print: HAS_COEF  = ', obj%has_coef
      if(obj%has_coef) then
        write (stdo, *) 'modspec_print: CPS_HDRA  = ', obj%hdra
        write (stdo, *) 'modspec_print: CPS_HDRB  = ', obj%hdrb
        write (stdo, *) 'modspec_print: a1= ',obj%a1,' a2=',obj%a2,&
        ' a3=',obj%a3
        write (stdo, *) 'modspec_print: b1= ',obj%b1,' b2=',obj%b2,&
        ' b3=',obj%b3
      endif
      write (stdo, *) 'modspec_print: DOMAIN  = ', obj%domain
      write (stdo, *) 'modspec_print: UNITS  = ', obj%units
      call  modspec_print(stdo,obj%units, obj%nx, obj%ny, obj%nlay, &
       obj%xorg, obj%yorg, obj%dx, obj%dy, obj%angle, &
       obj%v, obj%z, obj%g, obj%vclipmin, obj%vclipmax, obj%znon)
      return
      end subroutine modspec_print_obj

      subroutine modspec_print(stdo,units, nx, ny, nlay, xorg, yorg, dx, &
        dy, angle, v, z, g, vclipmin, vclipmax, znon)
      integer , intent(in) :: stdo
      integer , intent(in) :: nx
      integer , intent(in) :: ny
      integer , intent(in) :: nlay
      real    , intent(in) :: znon
      character(len=*)    :: units
      real , intent(in) :: dx
      real , intent(in) :: dy
      real , intent(in) :: angle
      double precision, intent(in) :: xorg
      double precision, intent(in) :: yorg
      real , intent(in) :: v(nlay,nx,ny)
      real , intent(in) :: z(nlay,nx,ny)
      real , intent(in) :: g(nlay,nx,ny)
      real , intent(in) :: vclipmin(nlay)
      real , intent(in) :: vclipmax(nlay)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, ix, iy
      integer :: izero, icount
      real    :: dmin, dmax, vmin, vmax, gmin, gmax
      real    :: big
!-----------------------------------------------


      izero=0
      write(6,*)' '
      write(6,*)' TOTALLY ZERO THICKNESS LAYERS ARE BETWEEN'
      write(6,*)'       HORIZON    and    HORIZON'
      do i=2,nlay
        icount=0
        do ix=1,nx
        do iy=1,ny
           if(z(i,ix,iy) == z(i-1,ix,iy)) icount=icount+1
        end do
        end do
        if(icount == nx*ny) write(6,'(9x,i5,13x,i5)')  i-1,i
      end do


!---Print out important model information
      write (stdo, *) 'modspec_print: '
      write (stdo, *) 'modspec_print: '
      write (stdo, *) 'modspec_print: FINAL MODEL PARAMETERS '
      write (stdo, *) 'modspec_print: '
      write (stdo, *) 'modspec_print: XORG    = ', xorg, '  YORG    = ', yorg
      write (stdo, *) 'modspec_print: DX      = ', dx, '  DY      = ', dy
      write (stdo, *) 'modspec_print: NX      = ', nx, '  NY      = ', ny
      write (stdo, *) 'modspec_print: NLAY    = ', nlay
      write (stdo, *) 'modspec_print: ANGLE   = ', angle
      write (stdo, *) 'modspec_print: ZNON    = ', znon
      write (stdo, *) 'modspec_print: '

      write (stdo, *) 'LAYER  ZMIN    ZMAX    VMIN    VMAX    GMIN &
     &   GMAX   VCLIPMIN VCLIPMAX'

      big=999999999.
      do i = 1, nlay
        dmin = big
        dmax = -big
        vmin = big
        vmax = -big
        gmin = big
        gmax = -big
        do ix=1,nx
        do iy=1,ny

          if(z(i,ix,iy) /= znon) then
            if(z(i,ix,iy) < dmin) dmin=z(i,ix,iy)
            if(z(i,ix,iy) > dmax) dmax=z(i,ix,iy)
          endif
          if(v(i,ix,iy) /= znon) then
            if(v(i,ix,iy) < vmin) vmin=v(i,ix,iy)
            if(v(i,ix,iy) > vmax) vmax=v(i,ix,iy)
          endif
          if(g(i,ix,iy) /= znon) then
            if(g(i,ix,iy) < gmin) gmin=g(i,ix,iy)
            if(g(i,ix,iy) > gmax) gmax=g(i,ix,iy)
          endif

        enddo
        enddo

        write (stdo, '(I5,4F8.0,2F8.4,2F10.0)') i, dmin, dmax, &
        vmin, vmax, gmin, gmax, vclipmin(i), vclipmax(i)
      end do

      return
      end subroutine modspec_print

      integer function modspec_getv(obj, ipsdm,&
        dmax, depth, velocity, layer, gx, gy, znil) result(status)
      type(modspec_struct),intent(inout) :: obj
      integer, intent(in)   :: ipsdm
      real, intent(in)      :: dmax
      real, intent(in)      :: depth
      real, intent(inout)   :: velocity
      integer,intent(inout) :: layer
      integer, intent(in)   :: gx
      integer, intent(in)   :: gy
      real,optional,intent(in) :: znil

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer          :: stdo
      integer          :: nlay
      integer          :: nlaymax  !1st dimension of z,v,g
      integer          :: nx
      integer          :: ny
      real, pointer    :: v (:,:,:)
      integer          :: ix ,iy
      logical          :: has_coef
      real             :: a(3)
      real             :: b(3)
      real             :: xo,yo
      real             :: dx,dy
      real             :: xgo,ygo
      real             :: znon
      integer          :: domain
!
      nlay = obj%nlay   !should be first dimension of z,v,g
      nlaymax = size(obj%v,1)
      stdo = obj%stdo
      v=>obj%v
      nx = obj%nx  !modspec nx
      ny = obj%ny  !modspec ny
      znon = obj%znon
      if(present(znil) ) then
        znon = znil
      endif

      velocity = znon
      ! get cps perspective values
      has_coef = modspec_get_coef(obj,a,b, xo, yo, dx, dy, xgo,ygo)
      ix = gx
      iy = gy
      if(obj%xy_reversed) then
        ix = gy
        iy = gx
      endif
      ix= max(1,min(ix,nx))
      iy= max(1,min(iy,ny))
      !convert from cps min-min origin to modspec origin
     !if(dx < 0) then
     !  if(obj%xy_reversed) then
     !    iy = ny - iy + 1
     !  else
     !    ix = nx - ix + 1
     !  endif
     !endif
     !if(dy < 0) then
     !  if(obj%xy_reversed) then
     !    ix = nx - ix + 1
     !  else
     !    iy = ny - iy + 1
     !  endif
     !endif
      if(nlay > nlaymax) then
       layer = 0
       status = -1
       write(stdo,*) 'modspec_getv: nlay > nlaymax'
       return
      endif
      if(nx /= size(v,2) .or. ny /= size(v,3)) then
       layer = 0
       status = -1
       write(stdo,*) 'modspec_getv: nx=',nx,' ny=',ny
       write(stdo,*) 'modspec_getv: size(v,2)=',&
       size(v,2),' size(v,3)=',size(v,3)
       return
      endif

      if(obj%domain=='DEPTH') then
       !status =  modspec_getv_classic(ipsdm, dmax, depth, velocity, layer,&
       !  ix, iy,nx, ny, obj%nlay, obj%v, obj%z, obj%g, obj%vclipmin,&
       !  obj%vclipmax, znon)
        domain = MODSPEC_DEPTH
        status =  modspec_getv_torz(domain, depth, velocity, layer, ix, iy,&
        nx, ny, obj%nlay, obj%v, obj%z, obj%g, znon)
      else
        domain = MODSPEC_TIME
        status =  modspec_getv_torz(domain, depth, velocity, layer, ix, iy,&
        nx, ny, obj%nlay, obj%v, obj%z, obj%g, znon)
      endif

      return
      end function modspec_getv

      !z,v,g are dimensioned (nlay,nx,ny)
      integer function modspec_getv_classic(ipsdm,&
        dmax, depth, velocity, layer, ix, iy,&
        nx,ny, nlay,v,z,g, vclipmin, vclipmax, znon) result(status)
      integer,intent(in)    :: ipsdm
      real   ,intent(in)    :: dmax            !defunct
      real   ,intent(in)    :: depth
      real   ,intent(inout) :: velocity
      integer,intent(inout) :: layer
      integer,intent(in)    :: ix
      integer,intent(in)    :: iy
      integer,intent(in)    :: nx
      integer,intent(in)    :: ny
      integer,intent(in)    :: nlay
      real   ,intent(in)    :: v(nlay,nx,ny)
      real   ,intent(in)    :: z(nlay,nx,ny)
      real   ,intent(in)    :: g(nlay,nx,ny)
      real   ,intent(in)    :: vclipmin(nlay)  !defunct
      real   ,intent(in)    :: vclipmax(nlay)  !defunct
      real   ,intent(in)    :: znon

      velocity = znon
      call gridlib_compute_velocity(ipsdm,dmax,depth,velocity,layer,  &
           ix,iy,nx,ny,nlay,nlay,v,z,     &
           g,vclipmin,vclipmax,znon)

      status = 0
      return
      end function modspec_getv_classic

      !if domain=1=MODSPEC_DEPTH,  depth = distance and z is a distance(
      !if domain=2=MODSPEC_TIME,   depth = time and z should be a time
      integer function modspec_getv_torz(domain,&
        depth, velocity, layer, ix, iy,&
        nx,ny, nlay,v,z,g,znon) result(status)
      integer,intent(in)    :: domain
      real   ,intent(in)    :: depth
      real   ,intent(inout) :: velocity
      integer,intent(inout) :: layer
      integer,intent(in)    :: ix
      integer,intent(in)    :: iy
      integer,intent(in)    :: nx
      integer,intent(in)    :: ny
      integer,intent(in)    :: nlay
      real   ,intent(in)    :: v(nlay,nx,ny)
      real   ,intent(in)    :: z(nlay,nx,ny)
      real   ,intent(in)    :: g(nlay,nx,ny)
      real   ,intent(in)    :: znon

      integer               :: nlaymax
      real                  :: vclipmin(nlay),vclipmax(nlay)
      real                  :: dmax
      velocity = znon
      if(domain /= MODSPEC_DEPTH .and. domain /= MODSPEC_TIME) then
        status = -1
        return
      endif
      nlaymax = nlay
      vclipmin = 0.0
      vclipmax = 0.0

      call gridlib_compute_velocity_torz(domain,dmax,depth,velocity,&
           layer,ix,iy,nx,ny,nlay,      &
           nlaymax,v,z,g,vclipmin,   &
           vclipmax,znon)

     !if(depth <= z(1,ix,iy).and. z(1,ix,iy) /= znon) then
     !    velocity = v(1,ix,iy)
     !    if(g(1,ix,iy) == znon) velocity=znon
     !    layer = 1
     !elseif(depth >= z(nlay,ix,iy) .and. z(nlay,ix,iy) /= znon) then
     !    graduse = g(nlay,ix,iy)
     !    deltaz  = depth-z(nlay,ix,iy)
     !    if(domain == 1) then        !depth domain modspec
     !       velocity = v(nlay,ix,iy)+graduse*deltaz
     !    elseif(domain == 2) then    !time domain modspec
     !       deltaz   = deltaz/2000.  !convert to 1-way time in sec
     !       velocity = v(nlay,ix,iy)*exp(graduse*deltaz)
     !    endif
     !    if(graduse == znon .or. v(nlay,ix,iy) == znon) velocity=znon
     !    layer=nlay
     !else
     !    do i=1,nlay-1
     !       layer=i
     !       if(depth >= z(i,ix,iy) .and. depth <= z(i+1,ix,iy).and. &
     !          z(i,ix,iy) /= znon .and. z(i+1,ix,iy) /= znon) then
     !          graduse = g(i,ix,iy)
     !          deltaz  = depth-z(i,ix,iy)
     !          if(domain == 1) then        !depth domain modspec
     !             velocity = v(i,ix,iy) + graduse*deltaz
     !          elseif(domain == 2) then    !time domain modspec
     !             deltaz   = deltaz/2000.  !convert to 1-way time in sec
     !             velocity = v(i,ix,iy)*exp(graduse*deltaz)
     !          endif
     !          if(graduse == znon .or. v(i,ix,iy) == znon) velocity=znon
     !          exit
     !       endif
     !    enddo
     !endif
      status = 0

      return
      end function modspec_getv_torz

      subroutine modspec_fixmodel(stdo,ivirtual,&
       nx, ny, nlay, nlaynew, nlaymax, &
       zdeepest, vtemp, ztemp, ktemp,&
       vclipmintemp, vclipmaxtemp, v, z, k, vclipmin, vclipmax)

      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: stdo
      integer, intent(in) :: NLAY
      integer, intent(in) :: IVIRTUAL (nlay)
      integer, intent(in) :: NX
      integer, intent(in) :: NY
      integer, intent(out) :: NLAYNEW
      integer, intent(in) :: NLAYMAX
      real, intent(in) :: ZDEEPEST
      real,  intent(in) :: VTEMP (nlay,nx,ny)
      real,  intent(in) :: ZTEMP (nlay,nx,ny)
      real,  intent(in) :: KTEMP (nlay,nx,ny)
      real,  intent(in) :: VCLIPMINTEMP (nlay)
      real,  intent(in) :: VCLIPMAXTEMP (nlay)
      real,  intent(out) :: V (nlaymax,nx,ny)
      real,  intent(out) :: Z (nlaymax,nx,ny)
      real,  intent(out) :: K (nlaymax,nx,ny)
      real,  intent(out) :: VCLIPMIN (nlaymax)
      real,  intent(out) :: VCLIPMAX (nlaymax)

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: errRetVal
!-----------------------------------------------
!
      call gridlib_fix_model(stdo,ivirtual,                       &
           nx,ny,nlay,nlaynew,nlaymax,zdeepest,   &
           vtemp,ztemp,ktemp,vclipmintemp,        &
           vclipmaxtemp,v,z,k,vclipmin,vclipmax,  &
           errRetVal)
      if(errRetVal /=0) then
        if(stat_verbose) then
          write(stdo,*) 'modspec_fixmodel: error in gridlib_fix_model'
        endif
      endif


      return
      end subroutine modspec_fixmodel

      subroutine modspec_checkclip(stdo, nx, ny, nlay, zdeepest, &
        v, z, k, vclipmin, vclipmax, nlayclip,znon)
      implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: stdo
      integer , intent(in) :: nx
      integer , intent(in) :: ny
      integer , intent(in) :: nlay
      integer , intent(out) :: nlayclip
      real , intent(in) :: zdeepest
      real , intent(in) :: v(nlay,nx,ny)
      real , intent(in) :: z(nlay,nx,ny)
      real , intent(in) :: k(nlay,nx,ny)
      real , intent(in) :: vclipmin(nlay)
      real , intent(in) :: vclipmax(nlay)
      real , intent(in) :: znon
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!
      call gridlib_check_clip_velocity(stdo,nx,ny,nlay,zdeepest,  &
           v,z,k,vclipmin,vclipmax,     &
           nlayclip,znon)

      return
      end subroutine modspec_checkclip

!
      subroutine modspec_warpgrid(stdo, iflag, znon, gridx, gridy, &
        grid1, nx1, ny1, dx1, dy1, xorg1, yorg1, angle1, units1,&
        grid2, nx2, ny2, dx2, dy2, xorg2, yorg2, angle2, units2)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer,intent(in) :: iflag
      integer, intent(in):: stdo
      real, intent(in)   :: znon
      integer, intent(in):: nx1
      integer, intent(in):: ny1
      integer, intent(in):: nx2
      integer, intent(in):: ny2
      real, intent(in)   :: gridx (nx2,ny2)
      real, intent(in)   :: gridy (nx2,ny2)
      real, intent(inout):: grid1 (nx1,ny1)
      real, intent(out)  :: grid2 (nx2,ny2)
      real, intent(inout):: dx1
      real, intent(inout):: dy1
      double precision, intent(inout) :: xorg1
      double precision, intent(inout) :: yorg1
      real, intent(in)   :: angle1
      character (len = 1):: units1
      real, intent(in)   :: angle2
      character (len = 1):: units2
      real,intent(in) :: dx2
      real,intent(in) :: dy2
      double precision, intent(in) :: xorg2
      double precision, intent(in) :: yorg2

      call gridlib_convert_warp_grid(stdo,iflag,znon,gridx,gridy,  &
           grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1,       &
           grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2)

      return
      end subroutine modspec_warpgrid


      subroutine modspec_abort (stdo)
      integer,intent(in) :: stdo
!-----------------------------------------------
      write (stdo, *) ' modspec_abort:'
      return
      end subroutine modspec_abort

      subroutine modspec_set_verbose(obj,verbose)
      type(modspec_struct),intent(inout) :: obj
      logical,intent(in)  :: verbose
      obj%modspec_verbose = verbose
      return
      end subroutine modspec_set_verbose

      subroutine modspec_to_gocad_vs(obj, ofile, name, i_err)
      type(modspec_struct),pointer :: obj
      integer,intent(out)  :: i_err
      character(len=*),intent(in)  :: ofile
      character(len=*),intent(in)  :: name
      integer      :: ilay
      integer      :: wlay
      integer      :: ix,iy
      integer      :: ipsdm
      integer      :: domain
      integer      :: cnt
      real         :: zval
      real         :: zmax
      real         :: pval
      real         :: a(4),b(4)
      real         :: xo,yo,dx,dy,xgo,ygo,x,y
      integer      :: hx,hy
      integer      :: stdo
      integer      :: ufi,nwr
      logical      :: hasc
      character(len=4096):: ascrep
      character(len=106) :: card
      character(len=4)   :: xyz_order
      integer            :: temp

      ascrep = ' '
      card   = ' '
      ipsdm  = MODSPEC_PRESTACK
      domain = MODSPEC_DEPTH
      cnt    = 1
      hasc = modspec_get_coef_full(obj,a,b, xo, yo, dx, dy, xgo,ygo,&
             hx,hy,xyz_order)

      stdo = -1
      ufi  = -1
      if(ofile==' ' .or. ofile=='NONE') then
        stdo=obj%stdo
      else
        ufi = cio_fopen(ofile,'w')
        if(ufi==CIO_ERROR) then
          print *,'modspec_to_gocad_vs: error '
          return
        endif
      endif
      card = 'GOCAD VSET 1'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = 'HEADER {'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card =  'name:'//trim(name)
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = '}'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = 'PROPERTIES velocity gradient'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      if(stdo > 0) then
        write(obj%stdo,*) trim(ascrep)
      else
        temp = len_trim(ascrep)
        nwr   = cio_fwrite(ascrep,1,temp,ufi)
      endif

      zmax = obj%zorg + (obj%nz-1)* obj%dz + 100
      do ilay = 1,obj%nlay
        if(ilay > 1) then
          card = ' '//char(10)//'SUBVSET '//char(10)
          temp = len_trim(card)
          if(ufi>0) nwr = cio_fwrite(card,1,temp,ufi)
          if(stdo > 0) write(obj%stdo,*) trim(card)
        endif
        do iy = 1,obj%ny
          y = ygo + (iy-1)*dy
          do ix = 1,obj%nx
             x = xgo + (ix-1)*dx
             zval = obj%z(ilay,ix,iy)
             i_err = modspec_getv(obj, ipsdm,&
             zmax, zval, pval, wlay, ix, iy)
             if(i_err ==0) then
               write(card,'("PVRTX ",i7,1x,f12.3,1x,f12.3,1x,f12.3,1x,&
              &f12.3,1x, f12.3)') cnt, x, y, zval,&
               obj%v(ilay,ix,iy), obj%g(ilay,ix,iy)
               if(ufi>0) card = trim(card)//char(10)
               temp = len_trim(card)
               if(ufi>0) nwr = cio_fwrite(card,1,temp,ufi)
               if(stdo > 0) write(obj%stdo,*) trim(card)
               cnt = cnt + 1
             endif
          enddo
        enddo
      enddo
      card =  'END'
      if(ufi>0) card = trim(card)//char(10)
      temp = len_trim(card)
      if(ufi>0) nwr = cio_fwrite(card,1,temp,ufi)
      if(stdo > 0) write(obj%stdo,*) trim(card)
      if(ufi>0) i_err = cio_fclose(ufi)
      return
      end subroutine modspec_to_gocad_vs

      integer function modspec_layer_to_gocad(obj, ofile, name, &
      xdec,ydec, layer) result(status)
      type(modspec_struct),pointer :: obj
      character(len=*),intent(in)  :: ofile
      character(len=*),intent(in)  :: name
      integer,intent(in)   :: layer
      integer,intent(in)   :: xdec
      integer,intent(in)   :: ydec

      integer      :: i_err
      integer      :: ilay
      integer      :: wlay
      integer      :: ix,iy
      integer      :: ipsdm
      integer      :: domain
      integer      :: cnt
      real         :: zval
      real         :: zmax
      real         :: pval
      real         :: a(4),b(4)
      real         :: xo,yo,dx,dy,xgo,ygo,x,y
      integer      :: hx,hy
      integer      :: stdo
      integer      :: ufi,nwr
      logical      :: hasc
      character(len=4096):: ascrep
      character(len=106) :: card
      character(len=4)   :: xyz_order
      integer            :: temp

      status = -1
      ascrep = ' '
      card   = ' '
      ipsdm  = MODSPEC_PRESTACK
      domain = MODSPEC_DEPTH
      i_err  = 0
  
      if( layer <1 .or. layer >obj%nlay) then
        return
      endif
      if( xdec <1 .or. xdec >obj%nx) then
        return
      endif
      if( ydec <1 .or. ydec >obj%ny) then
        return
      endif

      hasc = modspec_get_coef_full(obj,a,b, xo, yo, dx, dy, xgo,ygo,&
             hx,hy,xyz_order)

      stdo = -1
      ufi  = -1
      if(ofile==' ' .or. ofile=='NONE') then
        stdo=obj%stdo
      else
        ufi = cio_fopen(ofile,'w')
        if(ufi==CIO_ERROR) then
          print *,'modspec_layer_to_gocad: error '
          return
        endif
      endif
      card = 'GOCAD VSET 1'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = 'HEADER {'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card =  'name:'//trim(name)
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = '}'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      card = 'PROPERTIES velocity gradient'
      ascrep=trim(ascrep)//' '//trim(card)//char(10)
      if(stdo > 0) then
        write(obj%stdo,*) trim(ascrep)
      else
        temp = len_trim(ascrep)
        nwr   = cio_fwrite(ascrep,1,temp,ufi)
      endif

      zmax = obj%zorg + (obj%nz-1)* obj%dz + 100
      ilay = layer
      cnt    = 1
      do iy = 1,obj%ny,ydec
          y = ygo + (iy-1)*dy
          do ix = 1,obj%nx,xdec
             x = xgo + (ix-1)*dx
             zval = obj%z(ilay,ix,iy)
             i_err = modspec_getv(obj, ipsdm,&
             zmax, zval, pval, wlay, ix, iy)
             if(i_err ==0) then
               write(card,'("PVRTX ",i7,1x,f12.3,1x,f12.3,1x,f12.3,1x,&
              &f12.3,1x, f12.3)') cnt, x, y, zval,&
               obj%v(ilay,ix,iy), obj%g(ilay,ix,iy)
               if(ufi>0) card = trim(card)//char(10)
               temp = len_trim(card)
               if(ufi>0) nwr = cio_fwrite(card,1,temp,ufi)
               if(stdo > 0) write(obj%stdo,*) trim(card)
               cnt = cnt + 1
             endif
          enddo
      enddo

      card =  'END'
      if(ufi>0) card = trim(card)//char(10)
      temp = len_trim(card)
      if(ufi>0) nwr = cio_fwrite(card,1,temp,ufi)
      if(stdo > 0) write(obj%stdo,*) trim(card)
      if(ufi>0) i_err = cio_fclose(ufi)
      status = 0
      return
      end function modspec_layer_to_gocad

      real function modspec_get_znon(obj) result(znon)
      type(modspec_struct),intent(in) :: obj
      znon = obj%znon
      return
      end function modspec_get_znon

      subroutine modspec_set_znon(obj,znon)
      type(modspec_struct),intent(inout) :: obj
      real,intent(in)  :: znon
      obj%znon = znon
      return
      end subroutine modspec_set_znon


      subroutine modspec_tdconv (obj,domain)
      type(modspec_struct),intent(inout) :: obj
      integer             ,intent(in)    :: domain
      real                               :: temp(obj%nlay,obj%nx,obj%ny)
      integer                            :: ix,iy,lay
      real                               :: dz,dt,fact

      if (domain == modspec_get_domain(obj)) return
      if (domain /= MODSPEC_DEPTH .and. domain /= MODSPEC_TIME) return

      temp = 0.0

      do lay = 1,obj%nlay-1
         if (domain == MODSPEC_TIME) then
            write(6,'(a,i3)') ' Converting layer # ',lay,' from depth to time'
         else
            write(6,'(a,i3)') ' Converting layer # ',lay,' from time to depth'
         end if
         do ix = 1,obj%nx
         do iy = 1,obj%ny
            if (domain == MODSPEC_TIME) then  !d2t  temp()=dt
               dz = obj%z(lay+1,ix,iy) - obj%z(lay,ix,iy)
               if (obj%g(lay,ix,iy) == 0) then
                  temp(lay,ix,iy) = dz / obj%v(lay,ix,iy)
               else
                  temp(lay,ix,iy) = alog(1.0 + obj%g(lay,ix,iy) * dz &
                                      / obj%v(lay,ix,iy)) / obj%g(lay,ix,iy)
               endif
            else                              !t2d  temp()=dz
               dt = (obj%z(lay+1,ix,iy) - obj%z(lay,ix,iy)) / 2000.
               if (obj%g(lay,ix,iy) == 0) then
                  temp(lay,ix,iy) = dt*obj%v(lay,ix,iy)
               else
                  temp(lay,ix,iy) = obj%v(lay,ix,iy) * &
                           (exp(obj%g(lay,ix,iy)*dt) - 1.0) / obj%g(lay,ix,iy)
               endif
            endif
         enddo
         enddo
      enddo

      obj%z = 0.0

      fact = 1.0                                      !t2d
      if (domain == MODSPEC_TIME) fact = 2000.0       !d2t
      do lay = 2,obj%nlay
         do ix = 1,obj%nx
         do iy = 1,obj%ny
            obj%z(lay,ix,iy) = obj%z(lay-1,ix,iy) + temp(lay-1,ix,iy) * fact
         enddo
         enddo
      enddo

      if (domain == MODSPEC_DEPTH) then
           obj%domain = 'DEPTH'
      else
           obj%domain = 'TIME'
      end if
      return
      end subroutine modspec_tdconv


      integer function modspec_grid2world(obj, trace, line,xw,yw) &
      result(status)
      type(modspec_struct),intent(in) :: obj
      real,intent(in)               :: trace,line
      double precision,intent(out)  :: xw,yw
      real     :: rad
      real     :: xgl,ygl
      integer  :: ix,iy
      xw = 0.0
      yw = 0.0
      rad = obj%angle/180.*3.141592654
      if(.not. obj%has_coef) then
        status = -1
        return
      endif
      !
      !  modspec local coordinate
      !   xloc(i) = (i-1)*dxdef
      !   yloc(j) = (j-1)*dydef
      !  modspec world coordinate
      !   xwc(i,j) = xloc(i)*cos(theta) - yloc(j)*sin(theta) + xorgdef
      !   ywc(i,j) = xloc(i)*sin(theta) + yloc(j)*cos(theta) + yorgdef
      !  grid local coordinate
      !   xgloc(i) = xgo + (i-1)*dxgdef
      !   ygloc(j) = ygo + (j-1)*dygdef
      !  grid local to modspec local
      !   xloc(i)  = (xgloc(i) - xgo)*dxdef/dxgdef + xl0
      !   yloc(j)  = (ygloc(j) - ygo)*dydef/dygdef + yl0
      !  where (xl0,yl0) is the point (xg0,yg0) in the modspec local c.s.
      !(ix,iy)=(1,1) ==> (xorg,yorg)
      ! line = obj%a1*ix + obj%a2*iy + obj%a3
      ! trace= obj%b1*ix + obj%b2*iy + obj%b3
      ! convert line trace into world coordinate for the
      ! given input coord. system
       if(obj%a1==0) then
        if(obj%a2==0 .or. obj%b1==0) then
          status = -1
          return
        endif
        iy = (line - obj%a3)/obj%a2
        ix = (trace- obj%b3)/obj%b1
       else
        if(obj%a1==0 .or. obj%b2==0) then
          status = -1
          return
        endif
        ix = (line - obj%a3)/obj%a1
        iy = (trace- obj%b3)/obj%b2
       endif
       ! xgl, ygl = physical distance aligned with local coordinates
       xgl = (ix-1)*obj%dx
       ygl = (iy-1)*obj%dy
       xw  = obj%xorg + xgl*cos(rad) - ygl*sin(rad)
       yw  = obj%yorg + xgl*sin(rad) + ygl*cos(rad)

    !  a1 = a1*dxo/dxi
    !  b1 = b1*dxo/dxi
    !  a2 = a2*dyo/dyi
    !  b2 = b2*dyo/dyi
      status = 0
      return
      end function modspec_grid2world
      


      ! scale a,b values from obj to the new coord system
      integer function modspec_transform_abs(obj,nx,ny,xorg,yorg,dx,dy,&
      a,b) result(status)
      type(modspec_struct),intent(in) :: obj
      integer, intent(in)          :: nx   !number of x grid points
      integer, intent(in)          :: ny   !number of y grid points
      double precision, intent(in) :: xorg !the x grid origin
      double precision, intent(in) :: yorg !the y grid origin
      real, intent(in)             :: dx   !the x grid bin size
      real, intent(in)             :: dy   !the y grid bin size
      real, intent(out)            :: a(:) !the a coefcients
      real, intent(out)            :: b(:) !the b coefcients
      double precision :: w_delx,w_dely   ! world coord origin shift
      real             :: l_delx,l_dely   ! local coord origin shift
      real             :: n_binx,n_biny   ! shif in local x,uy bins
      real             :: n_l,n_t
      real             :: rad

      rad = obj%angle/180.*3.141592654
      a = 0.0
      b = 0.0
      status = -1
      if(obj%dx*obj%dy==0.0) return
      status = 0
      !reset the coeficients
      if(obj%has_coef) then
        !first scale the bin sizes
        a(1) = obj%a1*(dx/obj%dx)
        b(1) = obj%b1*(dx/obj%dx)
        a(2) = obj%a2*(dy/obj%dy)
        b(2) = obj%b2*(dy/obj%dy)

        !now compute the origin shift
        w_delx = xorg - obj%xorg  !origin x shift in world coords
        w_dely = yorg - obj%yorg  !origin y shift in world coords
        l_delx = w_delx*cos(rad) + w_dely*sin(rad)   !local x shift
        l_dely = -w_delx*sin(rad) + w_dely*cos(rad)  !local y shift
        n_binx = l_delx/obj%dx  !origin x shift in old dx bins
        n_biny = l_dely/obj%dy  !origin y shift in old dy bins
        !now calculate the line and trace number of the new origin
        !set new a3,b3 so line, trace numbers are preserved
        if(obj%a1==0) then
           ! line in y direction, traces in x
           n_t  = (obj%b1 + obj%b3) + n_binx * obj%b1
           b(3) = n_t - b(1)
           n_l  = (obj%a2 + obj%a3) + n_biny * obj%a2
           a(3) = n_l - a(2)

        else
           ! line in x direction, traces in y
           n_t  = (obj%b2 + obj%b3) + n_biny * obj%b2
           b(3) = n_t - b(2)
           n_l  = (obj%a1 + obj%a3) + n_binx * obj%a1
           a(3) = n_l - a(1)
        endif
      endif
      return
      end function modspec_transform_abs

      !parallel copy of modspec object
      !root cpu reads from fname to create obj on root
      !obj data is broadcast to non-root cpus so obj can be created there
      subroutine modspec_pcopy(fname, obj, stdo)
      character(len=*),intent(in) :: fname
      type(modspec_struct),pointer :: obj
      integer,intent(in)   :: stdo
      integer              :: cpu

      cpu  = pcpsx_i_pel()
      if(cpu == 0) then
        call modspec_create(obj,stdo,fname,'GRID')
        if(.not.associated(obj)) then
          print *,'modspec_pcopy: error 1'
          return
        endif
      endif
      call modspec_pcopy_obj( obj, stdo)

      return
      end subroutine modspec_pcopy

      subroutine modspec_pcopy_obj( obj, stdo)
      use ppio_module
      type(modspec_struct),pointer :: obj
      integer,intent(in)   :: stdo
      integer              :: root
      integer              :: nlay
      integer              :: nx
      integer              :: ny
      integer              :: nz           !modspec extension
      double precision     :: xorg         !x-origin in world coord
      double precision     :: yorg         !y-origin in world coord
      real                 :: zorg         !modspec extension
      real                 :: dx           !physical dx in rotated sytem
      real                 :: dy           !physical dy in rotated sytem
      real                 :: dz           !modspec extension
      real                 :: angle        !angle between wc and local coords
      character(len=4)     :: units
      integer              :: ilog
      integer              :: cpu

      root = 0
      cpu  = pcpsx_i_pel()
      if(cpu == root) then
        if(.not.associated(obj)) then
          print *,'modspec_pcopy_obj: error 1'
          return
        endif
        nlay = obj%nlay
        units = obj%units
        angle = obj%angle
        nx = obj%nx
        ny = obj%ny
        nz = obj%nz
        dx = obj%dx
        dy = obj%dy
        dz = obj%dz
        xorg = obj%xorg
        yorg = obj%yorg
        zorg = obj%zorg

      endif
      !broadcast critical parameters first
      call pcpsx_broadcast ( root, nx  )
      call pcpsx_broadcast ( root, xorg  )
      call pcpsx_broadcast ( root, dx  )
      call pcpsx_broadcast ( root, ny  )
      call pcpsx_broadcast ( root, yorg  )
      call pcpsx_broadcast ( root, dy  )
      call pcpsx_broadcast ( root, nz  )
      call pcpsx_broadcast ( root, zorg  )
      call pcpsx_broadcast ( root, dz  )
      call pcpsx_broadcast ( root, nlay)
      call pcpsx_broadcast ( root, angle)
      call pcpsx_broadcast ( root, units)
      ! now set key parameters and allocate arrays on non-root
      if(pcpsx_i_pel() /= root) then
        call modspec_create_new(obj,stdo,nlay,angle,units,&
        nx,xorg,dx,ny,yorg,dy,nz,zorg,dz)
        if(.not.associated(obj) ) then
          print *,'modspec_pcopy_obj:',cpu,' modspec_create failed'
          return
        endif
      endif
      !send remainder of modspec parameters to non root nodes
      call pcpsx_broadcast ( root, obj%file)
      call pcpsx_broadcast ( root, obj%nlay_file)
      call pcpsx_broadcast ( root, obj%a1)
      call pcpsx_broadcast ( root, obj%a2)
      call pcpsx_broadcast ( root, obj%a3)
      call pcpsx_broadcast ( root, obj%b1)
      call pcpsx_broadcast ( root, obj%b2)
      call pcpsx_broadcast ( root, obj%b3)
      call pcpsx_broadcast ( root, obj%hdra)
      call pcpsx_broadcast ( root, obj%hdrb)
      ilog=0
      if(obj%has_coef) ilog=1
      call pcpsx_broadcast ( root, ilog)
      obj%has_coef=.true.
      if(ilog==0) obj%has_coef=.false.
      ilog=0
      if(obj%xy_reversed) ilog=1
      call pcpsx_broadcast ( root, ilog)
      obj%xy_reversed=.true.
      if(ilog==0) obj%xy_reversed=.false.
      call pcpsx_broadcast ( root, obj%xyz_order)
      call pcpsx_broadcast ( root, obj%hx)
      call pcpsx_broadcast ( root, obj%hy)
      call pcpsx_broadcast ( root, obj%hz)
      call pcpsx_broadcast ( root, nlay, obj%dominance)
      call pcpsx_broadcast ( root, obj%ny, obj%z)
      call pcpsx_broadcast ( root, obj%ny, obj%v)
      call pcpsx_broadcast ( root, obj%ny, obj%g)
      call pcpsx_broadcast ( root,nlay, obj%vclipmin(1:nlay))
      call pcpsx_broadcast ( root,nlay, obj%vclipmax(1:nlay))

      return
      end subroutine modspec_pcopy_obj

      ! reset the modspec grid
      integer function modspec_regrid(obj,objo,nx,ox,dx,&
      ny,oy,dy, nz,oz,dz, angle) result(status)
      type(modspec_struct),pointer :: obj
      type(modspec_struct),pointer :: objo
      integer, intent(in)          :: nx   !number of x grid points
      integer, intent(in)          :: ny   !number of y grid points
      integer, intent(in)          :: nz   !number of z grid points
      double precision, intent(in) :: ox   !the x grid origin
      double precision, intent(in) :: oy   !the y grid origin
      real, intent(in)             :: oz   !the z grid origin
      real, intent(in)             :: dx   !the x grid bin size
      real, intent(in)             :: dy   !the y grid bin size
      real, intent(in)             :: dz   !the z grid bin size
      real, intent(in)             :: angle
      integer          :: nlay
      character(len=4) :: units
      integer          :: stdo
      integer          :: i_err
      real             :: a(3),b(3)
      integer          :: ilay
      integer          :: no_extrap
      integer          :: iflag

!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      status = -1

      if(obj%nlay < 1 ) then
        print *,'modspec_regrid: BAD NLAY',pcpsx_i_pel()
        return
      endif
      stdo = obj%stdo
      nlay = obj%nlay
      units= obj%units
      ! create a new modspec with the target grid size
      call modspec_create_new(objo,stdo,nlay,angle,units,&
      nx,ox,dx,ny,oy,dy,nz,oz,dz)
      if(.not.associated(objo)) then
        print *,'modspec_regrid: create call error'
        return
      endif

      objo%znon = obj%znon

      !create a new set of a,b coeficients that are consistent with the
      !target coordinates.
      a = 0.0
      b = 0.0
      if(obj%has_coef) then
        !set a,b values from obj and target
        i_err = modspec_transform_abs(obj,nx,ny,ox,oy,dx,dy,a,b)
        if(i_err /=0) then
           write(stdo,*) 'modspec_rddefmod_reset: ab transform error'
          return
        endif
        objo%has_coef = .true.
        objo%hdra = obj%hdra
        objo%hdrb = obj%hdrb
      endif

      objo%a1 = a(1)
      objo%b1 = b(1)
      objo%a2 = a(2)
      objo%b2 = b(2)
      objo%a3 = a(3)
      objo%b3 = b(3)
      objo%hx = obj%hx
      objo%hy = obj%hy
      objo%hz = obj%hz
      objo%hdra = obj%hdra
      objo%hdrb = obj%hdrb
      objo%xy_reversed = obj%xy_reversed
      objo%xyz_order = obj%xyz_order
      objo%domain = obj%domain
      objo%znon = obj%znon
      objo%file = obj%file
      objo%nlay_file= obj%nlay_file
      objo%dominance(1:nlay)= obj%dominance(1:nlay)
      objo%vclipmin(1:nlay) = obj%vclipmin(1:nlay)
      objo%vclipmax(1:nlay) = obj%vclipmax(1:nlay)

      no_extrap = 0
      iflag = 1
      do ilay=1,nlay
        call gridlib_convert_grid(stdo,iflag,obj%znon,          &
           obj%v(ilay,:,:),obj%nx,obj%ny,obj%dx,obj%dy,obj%xorg,obj%yorg,&
           obj%angle,obj%units(1:1), &
           objo%v(ilay,:,:),nx,ny,dx,dy,ox,oy,angle,units(1:1), &
           no_extrap)

        call gridlib_convert_grid(stdo,iflag,obj%znon,          &
           obj%g(ilay,:,:),obj%nx,obj%ny,obj%dx,obj%dy,obj%xorg,obj%yorg,&
           obj%angle,obj%units(1:1), &
           objo%g(ilay,:,:),nx,ny,dx,dy,ox,oy,angle,units(1:1), &
           no_extrap)

        call gridlib_convert_grid(stdo,iflag,obj%znon,          &
           obj%z(ilay,:,:),obj%nx,obj%ny,obj%dx,obj%dy,obj%xorg,obj%yorg,&
           obj%angle,obj%units(1:1), &
           objo%z(ilay,:,:),nx,ny,dx,dy,ox,oy,angle,units(1:1), &
           no_extrap)

      enddo

      status = 0
      return
      end function modspec_regrid

      !convert line & trace to integer x & y index into grid array
      ! will be clipped to (nx,ny)
      integer function modspec_lt2ixy(obj, trace, line,ix,iy) &
      result(status)
      type(modspec_struct),intent(in) :: obj
      real,intent(in)               :: trace,line
      integer,intent(out)  :: ix,iy
      ix=1
      iy=1
      if(.not. obj%has_coef) then
        status = -1
        return
      endif
      !
      ! convert line trace into index into the grid array
      !(ix,iy)=(1,1) ==> (xorg,yorg)
      ! line = obj%a1*ix + obj%a2*iy + obj%a3
      ! trace= obj%b1*ix + obj%b2*iy + obj%b3
       if(obj%a1==0) then
        if(obj%a2==0 .or. obj%b1==0) then
          status = -1
          return
        endif
        iy = (line - obj%a3)/obj%a2
        ix = (trace- obj%b3)/obj%b1
       else
        if(obj%a1==0 .or. obj%b2==0) then
          status = -1
          return
        endif
        ix = (line - obj%a3)/obj%a1
        iy = (trace- obj%b3)/obj%b2
       endif
      ix = min(ix,obj%nx)
      iy = min(iy,obj%ny)
      status = 0
      return
      end function modspec_lt2ixy

      end module modspec_module

