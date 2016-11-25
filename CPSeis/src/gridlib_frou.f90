!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- gridlib_frou.f90 ----------------------------!!
!!---------------------------- gridlib_frou.f90 ----------------------------!!
!!---------------------------- gridlib_frou.f90 ----------------------------!!

      ! other files are:  gridlib.f90 gridlib.h


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
!                        C P S   P R I M I T I V E  HELPER
!
! Name       : gridlib_frou
! Category   : velocity
! Written    : 2004-04-13   by: Bill Done
! Revised    : 2006-04-25   by: Brian Macy
! Maturity   : production
! Purpose    : Read, write, and manipulate modspec grids.
!-------------------------------------------------------------------------------
!</brief_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  7. 2006-04-25  Brian Macy Changed declaration of domain variable in
!                            getdefparms_ext.
!  6. 2006-01-24  Brian Macy Added subroutine getdefparms_ext to wrap
!                            module gridlib subroutine
!                            gridlib_read_def_model_parms.
!  5. 2004-09-28  Bill Done  Module gridlib subroutine gridlib_read_grid
!                            was rename to gridlib_read_abgrid, requiring
!                            a change in subroutine readgrid.
!  4. 2004-09-23  Bill Done  Change type of sounit to integer in parameter
!                            list for subroutine getk.
!  3. 2004-05-06  Bill Done  Eliminate subroutine readgriddata. Add cunits and
!                            punits parameters to some subroutines. Change
!                            len of character variable units in some places
!                            from 1 to *.
!  2. 2004-04-19  Bill Done  Correct order of type declarations.
!  1. 2004-04-13  Bill Done  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! None.
!
!--------------------------------------------------------------------------
!</portability_doc>


!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module gridlib_frou_module

      implicit none

      character(len=100),public,save :: gridlib_frou_ident = &
       '$Id: gridlib_frou.f90,v 1.7 2006/04/25 13:23:45 Macy prod sps $'


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module gridlib_frou_module

!********************************************************
!     subroutine to read in a grid array from disk.

      subroutine readgrid(sounit, iunit, filein, grid, ngridmax, nx, ny, &
                          xorg, yorg, dx, dy, angle, znon, resetznon,    &
                          description, attribute, cunits, punits, errRetVal)

      use gridlib_module, only: gridlib_read_abgrid

      implicit none

      integer,          intent(in)    :: sounit, iunit
      character(len=*), intent(in)    :: filein
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: grid(ngridmax)
      integer,          intent(inout) :: nx, ny
      double precision, intent(inout) :: xorg, yorg
      real,             intent(inout) :: dx, dy, angle
      real,             intent(inout) :: znon
      integer,          intent(in)    :: resetznon
      character(len=*), intent(inout) :: description, attribute
      character(len=*), intent(inout) :: cunits, punits
      integer,          intent(out)   :: errRetVal

      logical                         :: reset_znon

      reset_znon=.true.
      if (resetznon == 0) reset_znon = .false.
      call gridlib_read_abgrid(sounit, iunit, filein, grid, ngridmax, nx, ny, &
                               xorg, yorg, dx, dy, angle, znon, reset_znon,   &
                               description, attribute, cunits, punits,        &
                               errRetVal)


      end subroutine readgrid


!********************************************************

      subroutine DoGridHeadersMatch(nxin,nx,nyin,ny, &
                 dxin,dx,dyin,dy,                    &
                 xorgin,xorg,yorgin,yorg,            &
                 anglein,angle,                      &
                 unitsin,units,match)

      use gridlib_module, only: gridlib_verify_headers_match

      implicit none

      integer,          intent(in)    :: nxin, nx
      integer,          intent(in)    :: nyin, ny
      real,             intent(in)    :: dxin, dx
      real,             intent(in)    :: dyin, dy
      double precision, intent(in)    :: xorg, yorg, xorgin, yorgin
      real,             intent(in)    :: anglein, angle
      character(len=*), intent(in)    :: unitsin, units
      integer,          intent(inout) :: match

      call gridlib_verify_headers_match(nxin,nx,nyin,ny,          &
                                        dxin,dx,dyin,dy,          &
                                        xorgin,xorg,yorgin,yorg,  &
                                        anglein,angle,            &
                                        unitsin,units,match)

      end subroutine DoGridHeadersMatch


!********************************************************

      subroutine writegrid(filename, xorg, yorg, nx, ny, dx, dy, angle,      &
                           znon, nxdim, nydim, grid, description, attribute, &
                           cunits, punits, err, msg)

      use gridlib_module, only: gridlib_write_grid

      implicit none

      character(len=*), intent(in)    :: filename
      double precision, intent(in)    :: xorg, yorg
      integer,          intent(in)    :: nx, ny
      real,             intent(in)    :: dx, dy, angle, znon
      integer,          intent(in)    :: nxdim, nydim
      real,             intent(in)    :: grid(nxdim,nydim)
      character(len=*), intent(in)    :: description, attribute
      character(len=*), intent(in)    :: cunits, punits
      integer,          intent(out)   :: err
      character(len=*), intent(out)   :: msg

      call gridlib_write_grid(filename, xorg, yorg, nx, ny, dx, dy, angle, &
                              znon, nxdim, nydim, grid, description,       &
                              attribute, cunits, punits, err, msg)

      end subroutine writegrid


!********************************************************

      subroutine writegrid_fmt(iunit,fileout,grid,nxdim,nydim,  &
                               nx,ny,xorg,yorg,deltax,deltay,angle,znon,ier)

      use gridlib_module, only: gridlib_write_grid_fmt

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: fileout
      integer,            intent(in)    :: nxdim, nydim, nx, ny
      real,               intent(in)    :: grid(nxdim,nydim)
      double precision,   intent(in)    :: xorg, yorg
      real,               intent(in)    :: deltax, deltay, angle
      real,               intent(in)    :: znon
      integer,            intent(inout) :: ier

      call gridlib_write_grid_fmt(iunit,fileout,grid,nxdim,nydim,  &
                                  nx,ny,xorg,yorg,deltax,deltay,   &
                                  angle,znon,ier)

      end subroutine writegrid_fmt


!********************************************************

      subroutine getfmt(value,fmt0)

      use gridlib_module, only: gridlib_get_write_fmt

      implicit none

      real,              intent(in)    :: value
      character(len=*),  intent(inout) :: fmt0

      call gridlib_get_write_fmt(value,fmt0)

      end subroutine getfmt


!********************************************************

      subroutine get_min_max(grid, ngridmax, nx, ny, znon, &
                             gridmin, gridmax, errRetVal)

      use gridlib_module, only: gridlib_get_min_max

      implicit none

      integer,          intent(in)    :: ngridmax
      real,             intent(in)    :: grid(ngridmax)
      integer,          intent(in)    :: nx, ny
      real,             intent(in)    :: znon
      real,             intent(out)   :: gridmin, gridmax
      integer,          intent(out)   :: errRetVal

      call gridlib_get_min_max(grid, ngridmax, nx, ny, znon, &
                               gridmin, gridmax, errRetVal)

      return
      end subroutine get_min_max


!********************************************************
!
!     SUBROUTINE TO CHANGE UNITS OF A GRID ARRAY
!     FROM FEET TO METERS OR METERS TO FEET.
!        "unitsin"  is dimensioned "character*1" and is set to "F" or "M"
!        "unitsout" is dimensioned "character*1" and is set to "F" or "M"
!     The "IFLAG" parameter allows you to determine
!     whether only the grid parms (IFLAG=0) have unit changes
!     or whether the grid parms and the grid data have unit changes (IFLAG=1)
!     For example, if your data is in milliseconds, you would want to
!     set IFLAG=0.

      subroutine changeunits(                &
           iflag,gnull,unitsin,unitsout,     &
           grid,nx,ny,dx,dy,xorg,yorg,angle)

      use gridlib_module, only: gridlib_convert_units

      implicit none

      integer,          intent(in)    :: iflag
      real,             intent(in)    :: gnull
      character(len=*), intent(in)    :: unitsin, unitsout
      integer,          intent(in)    :: nx, ny
      real,             intent(inout) :: grid(nx,ny)
      real,             intent(inout) :: dx, dy
      double precision, intent(inout) :: xorg,yorg
      real,             intent(in)    :: angle

      call gridlib_convert_units(iflag,gnull,unitsin,unitsout,     &
                                 grid,nx,ny,dx,dy,xorg,yorg,angle)

      end subroutine changeunits


!********************************************************
!---PROGRAM NAME:         GRID2GRID
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        September 1995
!
!---DESCRIPTION:          This subroutine will convert an input grid array
!                         so that it has a set of desired grid parameters.
!                         Input grid parameters are:
!                            XORG1, YORG1, NX1, NY1, DX1, DY1, ANGLE1, UNITS1
!                         Desired grid parameters are:
!                            XORG2, YORG2, NX2, NY2, DX2, DY2, ANGLE2, UNITS2
!
!                         NOTE that XORG1, YORG1, XORG2, YORG2 are dimensioned
!                            "double precision"!!!
!                         UNIT1 and UNIT2 are dimensioned "character*1" and
!                            are set to be either "M" (meters) or "F" (feet).
!
!                         The "IFLAG" parameter allows you to determine
!                         whether only the grid parms (IFLAG=0) have unit
!                         changes or whether the grid parms and the grid data
!                         have unit  changes (IFLAG=1) For example, if your
!                         data is in  milliseconds, you would want to set
!                         IFLAG=0.
!
!                         Setting the "NO_EXTRAP" parameter to "1" sets all
!                         points outside the input grid to GNULL.
!

      subroutine grid2grid(sounit,iflag,gnull,        &
           grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,  &
           units1,                                    &
           grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,  &
           units2, no_extrap)

      use gridlib_module, only: gridlib_convert_grid

      implicit none

      integer,          intent(in)    :: sounit, iflag
      real,             intent(in)    :: gnull
      integer,          intent(in)    :: nx1, ny1
      real,             intent(inout) :: grid1(nx1,ny1)
      real,             intent(inout) :: dx1, dy1
      double precision ,intent(inout) :: xorg1, yorg1
      real,             intent(in)    :: angle1
      character(len=*), intent(in)    :: units1
      integer,          intent(in)    :: nx2, ny2
      real,             intent(inout) :: grid2(nx2,ny2)
      real,             intent(in)    :: dx2, dy2
      double precision, intent(in)    :: xorg2, yorg2
      real,             intent(in)    :: angle2
      character(len=*), intent(in)    :: units2
      integer,          intent(in)    :: no_extrap

      call gridlib_convert_grid(sounit, iflag,gnull,            &
               grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1, &
               grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2, &
               no_extrap)

      end subroutine grid2grid


!********************************************************
!---PROGRAM NAME:         GETDEFPARMS_EXT
!
!---AUTHOR:               Brian Macy
!
!---CREATION DATE:        Jan 2006
!
!---DESCRIPTION:          This subroutine will open a model specification
!                         file and read its default parameters, which will
!                         be stored as follows:
!
!                         * Set default parameters here
!                         * NLAYDEF=3
!                         * XORGDEF=422966.0
!                         * YORGDEF=5957525.6
!                         * DXDEF=112.5
!                         * DYDEF=100.0
!                         * NXDEF=70
!                         * NYDEF=146
!                         * ANGLEDEF=19.29
!                         * UNITSDEF='F'
!                         *
!
!                         The "extended" parameters will also be read,
!                         which are in free format.

      subroutine getdefparms_ext(iunit,sounit,filespec,nlaydef, &
                                 xorgdef,yorgdef,dxdef,dydef,   &
                                 nxdef,nydef,angledef,unitsdef, &
                                 nzdef,dzdef,znon,a1,a2,a3,     &
                                 b1,b2,b3,cps_hdra,cps_hdrb,    &
                                 domain,ierr)

      use gridlib_module, only: gridlib_read_def_model_parms, &
                                GRIDLIB_DOMAIN_LENGTH

      implicit none

      integer,          intent(in)  :: iunit
      integer,          intent(in)  :: sounit
      character(len=*), intent(in)  :: filespec
      integer,          intent(out) :: nlaydef
      double precision, intent(out) :: xorgdef,yorgdef
      real,             intent(out) :: dxdef, dydef
      integer,          intent(out) :: nxdef, nydef
      real,             intent(out) :: angledef
      character(len=1), intent(out) :: unitsdef
      integer,          intent(out) :: nzdef
      real,             intent(out) :: dzdef
      real,             intent(out) :: znon
      integer,          intent(out) :: a1,a2,a3
      integer,          intent(out) :: b1,b2,b3
      integer,          intent(out) :: cps_hdra,cps_hdrb
      character(len=GRIDLIB_DOMAIN_LENGTH), intent(inout) :: domain
      integer,          intent(out) :: ierr

      call gridlib_read_def_model_parms(iunit,sounit,filespec,nlaydef, &
                                        xorgdef,yorgdef,dxdef,dydef,   &
                                        nxdef,nydef,angledef,unitsdef, &
                                        nzdef,dzdef,znon,a1,a2,a3,     &
                                        b1,b2,b3,cps_hdra,cps_hdrb,    &
                                        domain,ierr)

      return

      end subroutine getdefparms_ext


!********************************************************
!---PROGRAM NAME:         GETDEFPARMS
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        OCT 1995
!
!---DESCRIPTION:          This subroutine will open a model specification
!                         file and read its default parameters.
!                         they will be stored as follows:
!
!
!                         * Set default parameters here
!                         * NLAYDEF=3
!                         * XORGDEF=422966.0
!                         * YORGDEF=5957525.6
!                         * DXDEF=112.5
!                         * DYDEF=100.0
!                         * NXDEF=70
!                         * NYDEF=146
!                         * ANGLEDEF=19.29
!                         * UNITSDEF='F'
!                         *

      subroutine getdefparms(iunit, filespec,              &
                     nlaydef,xorgdef,yorgdef,dxdef,dydef,  &
                     nxdef,nydef,angledef,unitsdef)

      use gridlib_module, only: gridlib_read_deflt_model_parms

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: filespec
      integer,            intent(inout) :: nlaydef
      double precision,   intent(inout) :: xorgdef,yorgdef
      real,               intent(inout) :: dxdef, dydef
      integer,            intent(inout) :: nxdef, nydef
      real,               intent(inout) :: angledef
      character(len=1),   intent(inout) :: unitsdef

      call gridlib_read_deflt_model_parms(iunit,filespec,nlaydef,      &
                                          xorgdef,yorgdef,dxdef,dydef, &
                                          nxdef,nydef,angledef,unitsdef)

      end subroutine getdefparms


!********************************************************
!---PROGRAM NAME:         GRID2MODEL
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        Jan 1993
!
!---DESCRIPTION:          This subroutine will read in grid files and create
!                         a layer type model.
!                         If velocity or depth is constant, then
!                         a constant can be supplied instead of a file.
!                         In addition, a layer can be defined as recessive
!                         or dominant.  If it is recessive and the depth
!                         lies above the layer above it, then
!                         the depth for the layer above it is
!                         used.  If it is dominant and the layer lies above
!                         the layer above it, then all layers that cut through
!                         the dominant layer will be reset to the dominant
!                         layer value.
!
!---LAST MOD. DATE:       August 1993 - Add capability for dominant/recessive
!             October  94 - Kay added array allocation for V and Z
!             December 94 - Kay added checks for 0-thickness layers
!             September 94 - Kay modified to run on workstation with
!                            a model specification file.

      subroutine grid2model(sounit,modunit,grdunit,filespec,units, &
                      nx,ny,nlay,xorg,yorg,dx,dy,angle,            &
                      v,z,grad,vclipmin,vclipmax,                  &
                      gridin,gridout,ngridmax,znon,errRetVal)

      use gridlib_module, only: gridlib_model_from_grid

      implicit none

      integer,          intent(in)    :: sounit, modunit, grdunit
      character(len=*), intent(in)    :: filespec
      character(len=1), intent(in)    :: units
      integer,          intent(in)    :: nx, ny, nlay
      double precision, intent(in)    :: xorg, yorg
      real,             intent(in)    :: dx, dy, angle
      real,             intent(inout) :: v(nlay,nx,ny)
      real,             intent(inout) :: z(nlay,nx,ny)
      real,             intent(inout) :: grad(nlay,nx,ny)
      real,             intent(inout) :: vclipmin(nlay), vclipmax(nlay)
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: gridin(ngridmax), gridout(nx*ny)
      real,             intent(in)    :: znon
      integer,          intent(out)   :: errRetVal

      call gridlib_model_from_grid(sounit,modunit,grdunit,filespec,units,  &
                                   nx,ny,nlay,xorg,yorg,dx,dy,angle,       &
                                   v,z,grad,vclipmin,vclipmax,             &
                                   gridin,gridout,ngridmax,znon,errRetVal)

      end subroutine grid2model


!********************************************************

      subroutine printmodelparms(sounit,            &
           units,nx,ny,nlay,xorg,yorg,dx,dy,angle,  &
           v,z,grad,vclipmin,vclipmax,znon)

      use gridlib_module, only: gridlib_print_model_parms

      implicit none

      ! the following was not typed in original code and was thus an integer
      ! but in other places is a character(len=1)
      integer,          intent(in)    :: sounit
      character(len=1), intent(in)    :: units
      integer,          intent(in)    :: nx, ny, nlay
      double precision, intent(in)    :: xorg,yorg
      real,             intent(in)    :: dx, dy, angle
      real,             intent(in)    :: v(nlay,nx,ny)
      real,             intent(in)    :: z(nlay,nx,ny)
      real,             intent(in)    :: grad(nlay,nx,ny)
      real,             intent(in)    :: vclipmin(nlay), vclipmax(nlay)
      real,             intent(in)    :: znon

      call gridlib_print_model_parms(sounit,units,nx,ny,nlay,xorg,yorg,  &
                                     dx,dy,angle,v,z,grad,vclipmin,      &
                                     vclipmax,znon)

      end subroutine printmodelparms


!********************************************************
!---PROGRAM NAME:         GETV
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        October 1995
!
!---DESCRIPTION:          This subroutine calculates the velocity at a given
!                         ix, iy, and depth location using data arrays created
!                         by reading a velocity model in MODSPEC format.
!                         INPUT:
!                          ipsdm     - Use "1" for Poststack DM velocity
!                                      Use "2" for Prestack DM velocity
!                         OUTPUT:
!                          velocity  - velocity at location ix,iy,depth
!                          layer     - layer number at location ix,iy,depth

      subroutine getv(ipsdm,dmax,depth,velocity,layer,ix,iy,  &
                 nx,ny,nlay,nlaymax,v,z,grad,vclipmin,vclipmax,znon)

      use gridlib_module, only: gridlib_compute_velocity

      implicit none

      integer, intent(in)    :: ipsdm
      real,    intent(in)    :: dmax, depth
      real,    intent(inout) :: velocity
      integer, intent(inout) :: layer
      integer, intent(in)    :: ix, iy
      integer, intent(in)    :: nx, ny, nlay, nlaymax
      real,    intent(in)    :: v(nlay,nx,ny)
      real,    intent(in)    :: z(nlay,nx,ny)
      real,    intent(in)    :: grad(nlay,nx,ny)
      real,    intent(in)    :: vclipmin(nlay), vclipmax(nlay)
      real,    intent(in)    :: znon

      call gridlib_compute_velocity(ipsdm,dmax,depth,velocity,layer,ix,iy,  &
                                    nx,ny,nlay,nlaymax,v,z,grad,vclipmin,   &
                                    vclipmax,znon)

      end subroutine getv


!********************************************************
!  PROGRAM:          FixModel
!
!  AUTHOR:           K. D. WYATT
!
!  CREATION DATE:    April 1996
!
!  DESCRIPTION:      This subroutine adds an artificial
!                    layer when a velocity gradient causes
!                    a velocity to exceed the clip velocity
!                    BEFORE the bottom of the layer

      subroutine FixModel(sounit,ivirtual,                              &
                      nx,ny,nlay,nlaynew,nlaymax,zdeepest,              &
                      vtemp, ztemp, ktemp, vclipmintemp, vclipmaxtemp,  &
                      v, z, k, vclipmin, vclipmax,errRetVal)

      use gridlib_module, only: gridlib_fix_model

      implicit none

      integer, intent(in)    :: sounit
      integer, intent(in)    :: nx, ny, nlay
      integer, intent(in)    :: ivirtual(nlay)
      integer, intent(inout) :: nlaynew
      integer, intent(in)    :: nlaymax
      real,    intent(in)    :: zdeepest
      real,    intent(in)    :: vtemp(nlay,nx,ny)
      real,    intent(in)    :: ztemp(nlay,nx,ny)
      real,    intent(in)    :: ktemp(nlay,nx,ny)
      real,    intent(in)    :: vclipmintemp(nlay)
      real,    intent(in)    :: vclipmaxtemp(nlay)
      real,    intent(inout) :: v(nlaymax,nx,ny)
      real,    intent(inout) :: z(nlaymax,nx,ny)
      real,    intent(inout) :: k(nlaymax,nx,ny)
      real,    intent(inout) :: vclipmin(nlaymax)
      real,    intent(inout) :: vclipmax(nlaymax)
      integer, intent(out)   :: errRetVal

      call gridlib_fix_model(sounit,ivirtual,                              &
                             nx,ny,nlay,nlaynew,nlaymax,zdeepest,          &
                             vtemp,ztemp,ktemp,vclipmintemp,vclipmaxtemp,  &
                             v,z,k,vclipmin,vclipmax,errRetVal)

      end subroutine FixModel


!********************************************************
!  PROGRAM:          CheckClip
!
!  AUTHOR:           K. D. WYATT
!
!  CREATION DATE:    April 1996
!
!  DESCRIPTION:      This subroutine checks to find
!                    layers when a velocity gradient causes
!                    a velocity to exceed the clip velocity
!                    BEFORE the bottom of the layer

      subroutine CheckClip(sounit,                  &
                      nx,ny,nlay,zdeepest,          &
                      v, z, k, vclipmin, vclipmax,  &
                      nlayclip,znon)

      use gridlib_module, only: gridlib_check_clip_velocity

      implicit none

      integer, intent(in)    :: sounit, nx, ny, nlay
      real,    intent(in)    :: zdeepest
      real,    intent(in)    :: v(nlay,nx,ny)
      real,    intent(in)    :: z(nlay,nx,ny)
      real,    intent(in)    :: k(nlay,nx,ny)
      real,    intent(in)    :: vclipmin(nlay)
      real,    intent(in)    :: vclipmax(nlay)
      integer, intent(inout) :: nlayclip
      real,    intent(in)    :: znon

      call gridlib_check_clip_velocity(sounit, nx, ny, nlay, zdeepest, &
                                       v, z, k, vclipmin, vclipmax,    &
                                       nlayclip, znon)

      end subroutine CheckClip


!********************************************************
!---PROGRAM NAME:         WARPGRID
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        September 1995
!
!---DESCRIPTION:          This subroutine will convert an input grid array
!                         so that it has a set of desired grid parameters.
!                         Input grid parameters are:
!                            XORG1, YORG1, NX1, NY1, DX1, DY1, ANGLE1, UNITS1
!                         Desired grid parameters are:
!                            XORG2, YORG2, NX2, NY2, DX2, DY2, ANGLE2, UNITS2
!
!                         NOTE that XORG1, YORG1, XORG2, YORG2 are dimensioned
!                            "double precision"!!!
!                         UNIT1 and UNIT2 are dimensioned "character*1" and
!                            are set to be either "M" (meters) or "F" (feet).
!
!                         The "IFLAG" parameter allows you to determine
!                         whether only the grid parms (IFLAG=0) have unit
!                         changes or whether the grid parms and the grid data
!                         have unit changes (IFLAG=1) For example, if your
!                         data is in milliseconds, you would want to set
!                         IFLAG=0.
!
!---LAST MOD. DATE:       6 December 1996 - changed "grid2grid" to
!                                           allow grid warping.

      subroutine warpgrid(sounit,                     &
           iflag,gnull,gridx,gridy,                   &
           grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,  &
           units1,                                    &
           grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,  &
           units2)

      use gridlib_module, only: gridlib_convert_warp_grid

      implicit none

      integer,          intent(in)    :: sounit, iflag
      real,             intent(in)    :: gnull
      integer,          intent(in)    :: nx1, ny1
      integer,          intent(in)    :: nx2, ny2
      real,             intent(in)    :: gridx(nx2,ny2)
      real,             intent(in)    :: gridy(nx2,ny2)
      real,             intent(inout) :: grid1(nx1,ny1)
      real,             intent(inout) :: dx1, dy1
      double precision, intent(inout) :: xorg1, yorg1
      real,             intent(in)    :: angle1
      character(len=1), intent(in)    :: units1
      real,             intent(inout) :: grid2(nx2,ny2)
      real,             intent(in)    :: dx2, dy2
      double precision, intent(in)    :: xorg2, yorg2
      real,             intent(in)    :: angle2
      character(len=1), intent(in)    :: units2

      call gridlib_convert_warp_grid(sounit,iflag,gnull,gridx,gridy,  &
               grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1,       &
               grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2)

      end subroutine warpgrid


!********************************************************
!---PROGRAM NAME:         WARPGRID2MODEL
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        Jan 1993
!
!---DESCRIPTION:          This subroutine will read in grid files and create
!                         a layer type model.
!                         If velocity or depth is constant, then
!                         a constant can be supplied instead of a file.
!                         In addition, a layer can be defined as recessive
!                         or dominant.  If it is recessive and the depth
!                         lies above the layer above it, then
!                         the depth for the layer above it is
!                         used.  If it is dominant and the layer lies above
!                         the layer above it, then all layers that cut through
!                         the dominant layer will be reset to the dominant
!                         layer value.
!
!---LAST MOD. DATE:       August 1993 - Add capability for dominant/recessive
!             October  94 - Kay added array allocation for V and Z
!             December 94 - Kay added checks for 0-thickness layers
!             September 94 - Kay modified to run on workstation with
!                            a model specification file.
!             December 96 - Add capability to warp grid.

      subroutine warpgrid2model(sounit,modunit,grdunit,filespec,units, &
                      nx,ny,nlay,xorg,yorg,dx,dy,angle,                &
                      v,z,grad,vclipmin,vclipmax,                      &
                      gridx,gridy,gridin,gridout,ngridmax,znon,errRetVal)

      use gridlib_module, only: gridlib_model_from_warp_grid

      implicit none

      integer,          intent(in)    :: sounit, modunit, grdunit
      character(len=*), intent(in)    :: filespec
      character(len=1), intent(in)    :: units
      integer,          intent(in)    :: nx, ny, nlay
      double precision, intent(in)    :: xorg, yorg
      real,             intent(in)    :: dx, dy, angle
      real,             intent(inout) :: v(nlay,nx,ny)
      real,             intent(inout) :: z(nlay,nx,ny)
      real,             intent(inout) :: grad(nlay,nx,ny)
      real,             intent(inout) :: vclipmin(nlay)
      real,             intent(inout) :: vclipmax(nlay)
      real,             intent(in)    :: gridx(nx,ny)
      real,             intent(in)    :: gridy(nx,ny)
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: gridin(ngridmax)
      real,             intent(inout) :: gridout(nx*ny)
      real,             intent(in)    :: znon
      integer,          intent(out)   :: errRetVal

      call gridlib_model_from_warp_grid(sounit,modunit,grdunit,filespec,  &
                                        units,nx,ny,nlay,xorg,yorg,dx,dy, &
                                        angle,v,z,grad,vclipmin,vclipmax, &
                                        gridx,gridy,gridin,gridout,       &
                                        ngridmax,znon,errRetVal)

      end subroutine warpgrid2model


!********************************************************
!     kdw
!     feb 97
!
!     this program calculates k for a given value of
!     dt, dz, v0.
!     If it iterates more than 500 times, then it
!     returns with an error code of -1
!        linear gradient equation:
!                 -----------------  0    ^
!                  V = Vo + K z           | dt
!                 -----------------  Z    v
!
!                  k*dt = ln ( 1 + k*dz/Vo )
!

      subroutine getk(sounit, k, dt, dz, v0, ier)

      use gridlib_module, only: gridlib_compute_gradient

      implicit none

      integer, intent(in)    :: sounit
      real,    intent(inout) :: k
      real,    intent(in)    :: dt, dz, v0
      integer, intent(inout) :: ier

      call gridlib_compute_gradient(sounit, k, dt, dz, v0, ier)

      end subroutine getk


!********************************************************

      subroutine ReadWell(iunit,                       &
                             filein,   ncfilein,       &
                             desc,      ncdesc,        &
                             source,    ncsource,      &
                             sourcefile,ncsourcefile,  &
                             xorg,yorg,                &
                             F_or_M,                   &
                             dinc,dmin,dmax,           &
                             npts,nptsmax,             &
                             velmin,velmax,            &
                             xvel,yvel,vel,ier)

      use gridlib_module, only: gridlib_read_well

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: filein
      integer,            intent(in)    :: ncfilein
      character(len=*),   intent(inout) :: desc
      integer,            intent(inout) :: ncdesc
      character(len=*),   intent(inout) :: source
      integer,            intent(inout) :: ncsource
      character(len=*),   intent(inout) :: sourcefile
      integer,            intent(inout) :: ncsourcefile
      double precision,   intent(inout) :: xorg, yorg
      character(len=1),   intent(inout) :: F_or_M
      real,               intent(inout) :: dinc, dmin, dmax
      integer,            intent(inout) :: npts
      integer,            intent(in)    :: nptsmax
      real,               intent(inout) :: velmin, velmax
      real,               intent(inout) :: xvel(nptsmax)
      real,               intent(inout) :: yvel(nptsmax)
      real,               intent(inout) :: vel(nptsmax)
      integer,            intent(inout) :: ier

      call gridlib_read_well(iunit,                    &
                             filein,ncfilein,          &
                             desc,ncdesc,              &
                             source,ncsource,          &
                             sourcefile,ncsourcefile,  &
                             xorg,yorg,                &
                             F_or_M,                   &
                             dinc,dmin,dmax,           &
                             npts,nptsmax,             &
                             velmin,velmax,            &
                             xvel,yvel,vel,ier)

       end subroutine ReadWell


!********************************************************

      subroutine WriteWell(iunit,                      &
                             fileout,   ncfileout,     &
                             desc,      ncdesc,        &
                             source,    ncsource,      &
                             sourcefile,ncsourcefile,  &
                             xorg,yorg,                &
                             F_or_M,                   &
                             dinc,dmin,dmax,           &
                             npts,nptsmax,             &
                             velmin,velmax,            &
                             xvel,yvel,vel,ier)

      use gridlib_module, only: gridlib_write_well

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: fileout
      integer,            intent(in)    :: ncfileout
      character(len=*),   intent(in)    :: desc
      integer,            intent(in)    :: ncdesc
      character(len=*),   intent(in)    :: source
      integer,            intent(in)    :: ncsource
      character(len=*),   intent(in)    :: sourcefile
      integer,            intent(in)    :: ncsourcefile
      double precision,   intent(in)    :: xorg,yorg
      character(len=1),   intent(in)    :: F_or_M
      real,               intent(in)    :: dinc, dmin, dmax
      integer,            intent(in)    :: npts, nptsmax
      real,               intent(in)    :: velmin, velmax
      real,               intent(in)    :: xvel(npts), yvel(npts), vel(npts)
      integer,            intent(inout) :: ier

      call gridlib_write_well(iunit,                    &
                              fileout,ncfileout,        &
                              desc,ncdesc,              &
                              source,ncsource,          &
                              sourcefile,ncsourcefile,  &
                              xorg,yorg,                &
                              F_or_M,                   &
                              dinc,dmin,dmax,           &
                              npts,nptsmax,             &
                              velmin,velmax,            &
                              xvel,yvel,vel,ier)

      end subroutine WriteWell


!********************************************************

      subroutine gridlib_set_verbose_c(verbose)

      use gridlib_module, only: gridlib_set_verbose

      implicit none

      integer,            intent(in)    :: verbose

      call gridlib_set_verbose(verbose)

      return
      end subroutine gridlib_set_verbose_c


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

