!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ gridlib.f90 --------------------------------!!
!!------------------------------ gridlib.f90 --------------------------------!!
!!------------------------------ gridlib.f90 --------------------------------!!

        ! other files are:  gridlib_frou.f90 gridlib.h

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
!                       C P S   P R I M I T I V E
!
! Name       : GRIDLIB
! Category   : velocity
! Written    : 2004-04-13   by: Bill Done
! Revised    : 2007-01-03   by: Brian Macy
! Maturity   : production
! Purpose    : Read, write, and manipulate modspec grids.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! --> Insert description information here.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
! --> Insert trace input requirements here.
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
! --> Insert globals that this process uses or changes:
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
! --> Insert header words used or changed by this process:
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE FOR NEW OBJECT INTERFACE
!
! The most recent interface added to the gridlib module uses a user defined
! type, gridlib_struct. This allows the numerous grid parameters to be hidden
! from the user, resulting in more flexibility when adding new features and
! parameters to the gridlib module.
!
! The gridlib object interface is given by the subroutines
!
!                                            i      i     i     b    o   o
!       call gridlib_obj_read_grid_header (sounit,iunit,filein,gobj,err,msg)
!                                            i      i     i        i       b
!       call gridlib_obj_read_grid        (sounit,iunit,filein,reset_znon,gobj,
!                                           o
!                                          err)
!                                            i      i     b   o   o
!       call gridlib_obj_read_bgrid       (sounit,filein,gobj,err,msg)
!                                             i     b   o   o
!       call gridlib_obj_write_bgrid      (fileout,gobj,err,msg)
!                                            b   o
!       call gridlib_obj_get_min_max      (gobj,err)
!                                                            opt
!                                            i     i     o    b
!       call gridlib_obj_verify_hdr_match (gobjin,gobj,match,eps)
!                                            i       i         i      b
!       call gridlib_obj_convert_units    (iflag,cunitsout,punitsout,gobj)
!                                            i      i       i       b     b
!       call gridlib_obj_convert_grid     (sounit,iflag,no_extrap,gobj1,gobj2)
!                                            i      i     b     b
!       call gridlib_obj_convert_warp_grid(sounit,iflag,gobj1,gobj2,
!                                            b      b
!                                          gobjx2,gobjy2)
!
!                      ++++++++++++++++++++++++++++
!
! integer               sounit = unit number for standard output.
! integer                iunit = unit number for grid data input during read.
! character(len=*)      filein = name of file containing grid to be read.
! character(len=*)     fileout = name of file to which a grid is to be written.
! type(gridlib_struct)    gobj = reference to the gridlib data structure.
! type(gridlib_struct)  gobjin = reference to the gridlib data structure to
!                                compare to gobj.
! integer                  err = error flag (returned).
!                                  = GRIDLIB_OK    = FIO_OK    => file io ok
!                                  = GRIDLIB_ERROR = FIO_ERROR => file io error
!                                  = GRIDLIB_BGRID_NOTFOUND => binary grid not
!                                                              found
!                                  = GRIDLIB_ALLOC_ERROR => memory allocation
!                                                           for grid occurred
! character(len=*)         msg = message for possible printing (returned).
! integer           reset_znon = flag indicating that znon value used in grid
!                                file should be changed to gobj%znon after
!                                reading. Reset occurs of reset_znon is "true".
! integer                match = return value of 1 if nx, ny, xorg, yorg, dx,
!                                dy, angle, cunits, and punits of gobjin and
!                                gobj are identical. 0 otherwise.
! real                     eps = Tolerance epsilon for comparing equality of
!                                floating point entities from grid headers.
!                                Default value is 0.01.
! integer                iflag = 0, convert grid parameters.
!                              = 1, convert grid parameters and grid data.
! integer            no_extrap = 1, set all points outside input grid to gnull.
! character(len=*)   cunitsout = desired output coordinate units.
! character(len=*)   punitsout = desired output property units.
! type(gridlib_struct)   gobj1 = reference to the input gridlib data structure
!                                (which may have units converted).
! type(gridlib_struct)   gobj2 = reference to the output gridlib data structure
!                                after conversion from the grid1 space to the
!                                grid2 space.
! type(gridlib_struct)  gobjx2 = defines the x coordinate of the (x,y)
!                                coordinate pairs of the nx2-by-ny2 matrix
!                                that defines the warped grid. It is the
!                                gobjx2%grid and gobjy2%grid arrays that
!                                are used to make the output grid gobj2%grid
!                                irregular or warped.
! type(gridlib_struct)  gobjy2 = defines the y coordinate of the (x,y)
!                                coordinate pairs of the nx2-by-ny2 matrix
!                                that defines the warped grid. It is the
!                                gobjx2%grid and gobjy2%grid arrays that
!                                are used to make the output grid gobj2%grid
!                                irregular or warped.
!
!  GRIDLIB_OBJ_READ_GRID_HEADER: Open the grid file specified in filein
!                                and read its header. Will handle the new
!                                binary and old ascii file types. Can be
!                                accessed by interface GRIDLIB_READ_GRID_HEADER.
!  GRIDLIB_OBJ_READ_GRID:        Open the grid file specified in filein.
!                                If the file is detected as a binary grid file,
!                                subroutine GRIDLIB_OBJ_READ_BGRID is used to
!                                read it. If it is ascii, then it is read
!                                using subroutine GRIDLIB_READ_AGRID.
!  GRIDLIB_OBJ_READ_BGRID:       Open the binary grid file specified in
!                                filein and read its header, allocate
!                                memory for the grid, and ready the grid.
!  GRIDLIB_OBJ_WRITE_BGRID:      Open the grid file specified in fileout
!                                and write the grid in gobj in binary format.
!  GRIDLIB_OBJ_GET_MIN_MAX:      Scan the 2D grid in gobj and determine the
!                                minimum and maximum grid values.
!  GRIDLIB_OBJ_VERIFY_HDR_MATCH: Compare the nx, ny, xorg, yorg, dx, dy, angle,
!                                cunits, and punits of gobjin and gobj. If
!                                equal (to within an epsilon "eps" on the
!                                floating point types), return 1.
!  GRIDLIB_OBJ_CONVERT_UNITS:    Convert units of grid.
!  GRIDLIB_OBJ_CONVERT_GRID:     Convert a grid from the coordinate space
!                                defined by grid 1 (in gobj1) to the space
!                                defined by the header of grid 2 (in gobj2).
!                                The converted grid is returned in gobj2.
!  GRIDLIB_OBJ_CONVERT_WARP_GRID: Convert a grid from the coordinate space
!                                 defined by grid 1 (in gobj1) to the space
!                                 defined by the header of grid 2 (in gobj2).
!                                 The converted grid is returned in gobj2.
!                                 The grid is warped by the x and y coordinate
!                                 grids defined in gobjx2 and gobjy2.
!
!
!
!                      ++++++++++++++++++++++++++++
!
!                          CALLING SEQUENCE FOR OLD INTERFACE
!
! The older interface for gridlib does not use the gridlib_struct to pass
! parameters to the interface subroutines. This results in long parameter
! lists for many of the subroutines. The following gives the calling sequences
! for the old interface.
!
!                                        i      i     i       o     b  b
!       call gridlib_read_abgrid_header(sounit,iunit,filein,ngridmax,nx,ny,
!                                        b    b   b  b    b    b        b
!                                       xorg,yorg,dx,dy,angle,znon,description,
!                                          b       b      b        b
!                                      attribute,cunits,punits,errRetVal)
!                                         i      o    o   o  o  o  o    o
!       call gridlib_read_bgrid_header(filename,xorg,yorg,nx,ny,dx,dy,angle,
!                                       o        o         o        o
!                                      znon,description,attribute,cunits,
!                                        o     o   o
!                                      punits,err,msg)
!                                        i      i     i    b  b   b    b
!       call gridlib_read_agrid_header(sounit,iunit,filein,nx,ny,xorg,yorg,
!                                      b  b    b    b       o
!                                      dx,dy,angle,znon,errRetVal)
!                                        i      i     i     b      i     b  b
!       call gridlib_read_abgrid      (sounit,iunit,filein,grid,ngridmax,nx,ny,
!                                       b    b   b  b    b    b       i
!                                      xorg,yorg,dx,dy,angle,znon,reset_znon,
!                                           b          b       b      b
!                                      description,attribute,cunits,punits,
!                                          o
!                                      errRetVal)
!                                        i      i     i     b      i     b  b
!       call gridlib_read_agrid       (sounit,iunit,filein,grid,ngridmax,nx,ny,
!                                       b    b   b  b    b    b       o
!                                      xorg,yorg,dx,dy,angle,znon,errRetVal)
!                                          i   i   i   i   i   i   i   i
!       call gridlib_verify_headers_match(nxin,nx,nyin,ny,dxin,dx,dyin,dy,
!                                           i     i     i     i      i
!                                         xorgin,xorg,yorgin,yorg,anglein,
!                                           i      i      i     b
!                                         angle,unitsin,units,match)
!                                        i      i   i  i   b
!       call gridlib_read_agrid_data  (sounit,iunit,nx,ny,grid)
!                                        i      i     i     i     i
!       call gridlib_write_agrid      (iunit,fileout,grid,nxdim,nydim,
!                                      i  i   i    i     i      i
!                                      nx,ny,xorg,yorg,deltax,deltay,
!                                        i    i    b
!                                      angle,znon,ier)
!                                        i      i     i     i     i
!       call gridlib_write_agrid_fmt  (iunit,fileout,grid,nxdim,nydim,
!                                      i  i   i    i     i      i
!                                      nx,ny,xorg,yorg,deltax,deltay,
!                                        i    i    b
!                                      angle,znon,ier)
!                                        i    b
!       call gridlib_get_write_fmt    (value,fmt0)
!                                         i      i    i   i  i  i  i    i
!       call gridlib_write_bgrid      (filename,xorg,yorg,nx,ny,dx,dy,angle,
!                                        i    i     i    i        i
!                                      znon,nxdim,nydim,grid,description,
!                                          i       i      i     o   o
!                                      attribute,cunits,punits,err,msg)
!                                         i      o    o   o  o  o  o    o
!       call gridlib_read_bgrid       (filename,xorg,yorg,nx,ny,dx,dy,angle,
!                                        o     i      b        o
!                                      znon,ngridmax,grid,description,
!                                          o       o      o     o  o
!                                      attribute,cunits,punits,err,msg)
!                                       i      i     i  i   i      o
!       call gridlib_get_min_max      (grid,ngridmax,nx,ny,znon,gridmin,
!                                         o        o
!                                      gridmax,errRetVal)
!                                        i     i      i       i      b
!       call gridlib_convert_units    (iflag,gnull,unitsin,unitsout,grid,
!                                      i  i  b  b   b    b     i
!                                      nx,ny,dx,dy,xorg,yorg,angle)
!                                        i      i     i     b    i   i
!       call gridlib_convert_grid     (sounit,iflag,gnull,grid1,nx1,ny1,
!                                       b   b    b     b     i      i
!                                      dx1,dy1,xorg1,yorg1,angle1,units1,
!                                        b    i   i   i   i    i     i
!                                      grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,
!                                        i      i        i
!                                      angle2,units2,no_extrap)
!                                           i     i       i        o
!       call gridlib_read_def_model_parms(iunit,sounit,filespec,nlaydef,
!                                            o       o      o     o
!                                         xorgdef,yorgdef,dxdef,dydef,
!                                            o    o      o        o
!                                         nxdef,nydef,angledef,unitsdef,
!                                           o     o    o   o  o  o
!                                         nzdef,dzdef,znon,a1,a2,a3,
!                                         o  o  o     o        o
!                                         b1,b2,b3,cps_hdra,cps_hdrb,
!                                            o    o
!                                         domain,ierr)
!                                             i      i        b       b
!       call gridlib_read_deflt_model_parms(iunit,filespec,nlaydef,xorgdef,
!                                              b      b     b     b    b
!                                           yorgdef,dxdef,dydef,nxdef,nydef,
!                                              b        b
!                                           angledef,unitsdef)
!                                        i       i       i       i
!       call gridlib_model_from_grid  (sounit,modunit,grdunit,filespec,
!                                        i   i  i  i    i    i    i  i
!                                      units,nx,ny,nlay,xorg,yorg,dx,dy,
!                                        i   b b  b      b        b
!                                      angle,v,z,grad,vclipmin,vclipmax,
!                                           b        b         i
!                                      gridin_dep,gridout,ngridmax_dep,
!                                                       opt
!                                       i       o        l
!                                      znon,errRetVal,no_znons)
!                                        i      i   i  i   i    i    i
!       call gridlib_print_model_parms(sounit,units,nx,ny,nlay,xorg,yorg,
!                                      i  i    i   i i  i      i        i
!                                      dx,dy,angle,v,z,grad,vclipmin,vclipmax,
!                                       i
!                                      znon)
!                                            i    i  i   i   i i  i
!       call gridlib_print_parms_insertion(sounit,nx,ny,nlay,v,z,grad,
!                                              i       i
!                                          vclipmin,vclipmax)
!                                        i    i     i      b       b
!       call gridlib_compute_velocity (ipsdm,dmax,depth,velocity,layer,
!                                      i  i  i  i   i      i    i i
!                                      ix,iy,nx,ny,nlay,nlaymax,v,z,
!                                       i      i        i      i
!                                      grad,vclipmin,vclipmax,znon)
!                                        i       i     i  i   i      b
!       call gridlib_fix_model        (sounit,ivirtual,nx,ny,nlay,nlaynew,
!                                         i       i       i     i     i
!                                      nlaymax,zdeepest,vtemp,ztemp,ktemp,
!                                           i            i       b b b
!                                      vclipmintemp,vclipmaxtemp,v,z,k,
!                                         b        b         o
!                                      vclipmin,vclipmax,errRetVal)
!                                          i    i  i   i      i     i i i
!       call gridlib_check_clip_velocity(sounit,nx,ny,nlay,zdeepest,v,z,k,
!                                           i        i        b      i
!                                        vclipmin,vclipmax,nlayclip,znon)
!                                        i      i     i     i     i
!       call gridlib_convert_warp_grid(sounit,iflag,gnull,gridx,gridy,
!                       b    i   i   b   b    b     b     i      i
!                     grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1,
!                       b    i   i   i   i    i     i     i      i
!                     grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2)
!                                           i       i       i       i
!       call gridlib_model_from_warp_grid(sounit,modunit,grdunit,filespec,
!                                           i   i  i   i    i    i   i  i
!                                         units,nx,ny,nlay,xorg,yorg,dx,dy,
!                                           i   b b  b      b        b
!                                         angle,v,z,grad,vclipmin,vclipmax,
!                                           i     i       b         b
!                                         gridx,gridy,gridin_dep,gridout,
!                                                                       opt
!                                              i        i       o        l
!                                         ngridmax_dep,znon,errRetVal,no_znons)
!                                        i    b i  i  i   b
!       call gridlib_compute_gradient (sounit,k,dt,dz,v0,ier)
!                                         i
!       call gridlib_set_verbose      (verbose)
!                                            i     i     i      b       b
!       call gridlib_compute_velocity_torz(domain,dmax,depth,velocity,layer,
!                                          i  i  i  i   i      i    i i
!                                          ix,iy,nx,ny,nlay,nlaymax,v,z,
!                                           i      i        i      i
!                                          grad,vclipmin,vclipmax,znon)
!
!                      ++++++++++++++++++++++++++++
!
! integer               sounit = unit number for standard output.
! integer        grdunit,iunit = unit number for grid data input during read.
! integer              modunit = unit number for model data input during read.
! integer              grdunit = unit number for gradient data input during
!                                read.
! character(len=*)      filein = name of file containing grid to be read.
! character(len=*)    filename = name of file containing grid to be read or
!                                written.
! character(len=*)    filespec = name of file containing default grid parameters
!                                to be read.
! character(len=*)     fileout = name of file to which a grid is to be written.
! integer             ngridmax = maximum number of elements in grid.
! integer              nx,nxin = size of grid in x dimension.
! integer              ny,nyin = size of grid in y dimension.
! double precision xorg,xorgin = x origin of grid, world coordinates.
! double precision yorg,yorgin = y origin of grid, world coordinates.
! real          dx,dxin,deltax = distance between adjacent grid location in x
!                                direction, world coordinates.
! real          dy,dyin,deltay = distance between adjacent grid location in y
!                                direction, world coordinates.
! real           angle,anglein = orientation angle of grid, degrees.
! real                    znon = value indicating undefined point on grid.
! character(len=*) description = description of the grid.
! character(len=*)   attribute = attributes of the grid.
! character(len=*)       units = coordinate units.
! character(len=*)     unitsin = coordinate units.
! character(len=*)      cunits = coordinate units.
! character(len=*)      punits = property units.
! integer                  err = error flag (returned).
!                                  = GRIDLIB_OK    = FIO_OK    => file io ok
!                                  = GRIDLIB_ERROR = FIO_ERROR => file io error
!                                  = GRIDLIB_BGRID_NOTFOUND => binary grid not
!                                                              found
!                                  = GRIDLIB_ALLOC_ERROR => memory allocation
!                                                           for grid occurred
! integer            errRetVal = error flag (returned), same as err.
! logical             no_znons = do not permit znons in model grids
! integer                  ier = error flag (returned), 0=no error, -1=error.
! character(len=*)         msg = message for possible printing (returned).
! integer           reset_znon = flag indicating that znon value used in grid
!                                file should be changed to gobj%znon after
!                                reading. Reset occurs of reset_znon is "true".
! integer                match = return value of 1 if nx = nxin, ny = nyin,
!                                xorg = xorgin, yorg = yorgin, dx = dxin,
!                                dy = dyin, angle = anglein, and units = unitsin
! real                     eps = Tolerance epsilon for comparing equality of
!                                floating point entities from grid headers.
!                                Default value is 0.01.
! integer                iflag = 0, convert grid parameters.
!                              = 1, convert grid parameters and grid data.
! integer            no_extrap = 1, set all points outside input grid to gnull.
! character(len=*)   cunitsout = desired output coordinate units.
! character(len=*)   punitsout = desired output property units.
! real                    grid = input grid array.
! real                 gridmin = minimum value on the grid, excluding znon
!                                values.
! real                 gridmax = maximum value on the grid, excluding znon
!                                values.
! integer         nlay,nlaydef = number of layers in model.
! double precision     xorgdef = x origin of model, world coordinates.
! double precision     yorgdef = y origin of model, world coordinates.
! real                   dxdef = distance between adjacent grid location in x
!                                direction for model, world coordinates.
! real                   dydef = distance between adjacent grid location in y
!                                direction for model, world coordinates.
! integer                nxdef = size of grid in x dimension for model.
! integer                nydef = size of grid in y dimension for model.
! real                angledef = orientation angle of model, degrees.
! character(len=*)    unitsdef = coordinate units.
! integer                nzdef = default size of grid in z dimension for model.
! real                   dzdef = distance between adjacent grid location in z
!                                direction for model, world coordinates.
! integer                   a1 = coefficient for mapping grid to world coord.
! integer                   a2 = coefficient for mapping grid to world coord.
! integer                   a3 = coefficient for mapping grid to world coord.
! integer                   b1 = coefficient for mapping grid to world coord.
! integer                   b2 = coefficient for mapping grid to world coord.
! integer                   b3 = coefficient for mapping grid to world coord.
! integer             cps_hdra = CPS header word to match with a coefficients.
! integer             cps_hdrb = CPS header word to match with b coefficients.
! character(len=*)      domain = label describing depth or time domain.
! real                       v = velocity model for all layers.
! real                       z = depth model for all layers.
! real                 k, grad = gradient model for all layers.
! real                vclipmin = minimum clip velocity for the layers.
! real                vclipmax = maximum clip velocity for the layers.
! real                 gridout = output grid array.
! integer         ngridmax_dep = maximum number of elements in grid
!                                (deprecated).
! real                grid_dep = input grid array (deprecated).
! integer                ipsdm = 1 for Poststack DM velocity
!                                2 for Prestack DM velocity
! integer                   ix = x location at which velocity is to be computed.
! integer                   iy = y location at which velocity is to be computed.
! real                    dmax = maximum depth.
! real                   depth = depth at which velocity is to be computed.
! integer              nlaymax = maximum number of layers.
! real                velocity = computed velocity at location (ix,iy,depth).
! integer                layer = layer number at location (ix,iy,depth).
! integer             ivirtual = array indicating layers to add (if 
!                                ivirtual(j) > 0).
! integer              nlaynew = new number of layers in model.
! real                zdeepest = specifies deepest extent of model. if
!                                zdeepest = 0.0, reset zmax (current maximum
!                                depth of model) to 1.6*zmax. Error occurs
!                                if zdeepest is not 0.0 but is less than zmax.
! real     vtemp, ztemp, ktemp = input arrays of velocity, depth, and gradient
!                                before inserting virtual layers.
! real vclipmintemp, vclipmaxtemp = input arrays of clipping velocities
!                                   before inserting virtual layers.
! type(gridlib_struct)   gobj2 = reference to the output gridlib data structure
!                                after conversion from the grid1 space to the
!                                grid2 space.
! integer             nlayclip = number of layers in which velocity exceed
!                                clip value due to gradient.
! integer              nx1,nx2 = size of grid in x dimension.
! integer              ny1,ny2 = size of grid in y dimension.
! real                 dx1,dx2 = distance between adjacent grid location in x
!                                direction, world coordinates.
! real                 dy1,dy2 = distance between adjacent grid location in y
!                                direction, world coordinates.
! double precision xorg1,xorg2 = x origin of grid, world coordinates.
! double precision yorg1,yorg2 = y origin of grid, world coordinates.
! real           angle1,angle2 = orientation angle of grid, degrees.
! real                   gnull = value indicating undefined point on grid.
! real                   gridx = defines the x coordinate of the (x,y)
!                                coordinate pairs of the nx2-by-ny2 matrix
!                                that defines the warped grid. It is the
!                                gridx and gridy arrays that are used to make 
!                                the output grid grid2 irregular or warped.
! real                   gridy = defines the y coordinate of the (x,y)
!                                coordinate pairs of the nx2-by-ny2 matrix
!                                that defines the warped grid. It is the
!                                gridx and gridy arrays that are used to make 
!                                the output grid grid2 irregular or warped.
! real                   grid1 = input grid array, which may have its units
!                                converted.
! real                   grid2 = output grid array containing grid1 after
!                                converting to the space defined for grid2.
! real              gridin_dep = input grid array. Use is deprecated.
! real                       k = gradient value returned by iteration. Error
!                                occurs if number of iterations exceeds 500,
!                                returning ier = -1.
! real                      dt = time increment. 
! real                      dz = depth increment. 
! real                      v0 = initial velocity.
! integer              verbose = verbosity flag. If verbose > 0, set the
!                                module variable gridlib_verbose to "verbose",
!                                indicating greater levels of printout. A
!                                value of verbose = 0 gives the most terse
!                                printout.
! integer               domain = 1 for Depth domain velocity,
!                                2 for Time domain velocity.
!
!                      ++++++++++++++++++++++++++++
!
! The following is the call information for the two well related subroutines:
!
!                                        i      i        i      b     b
!       call gridlib_read_well        (iunit,fileout,ncfileout,desc,ncdesc,
!                                        b       b         b           b
!                                      source,ncsource,sourcefile,ncsourcefile,
!                                       b    b     b     b    b    b    b
!                                      xorg,yorg,F_or_M,dinc,dmin,dmax,npts,
!                                         i      b      b     b    b    b   b
!                                      nptsmax,velmin,velmax,xvel,yvel,vel,ier)
!                                        i      i        i      i     i
!       call gridlib_write_well       (iunit,fileout,ncfileout,desc,ncdesc,
!                                        i       i         i           i
!                                      source,ncsource,sourcefile,ncsourcefile,
!                                       i    i     i     i    i    i    i
!                                      xorg,yorg,F_or_M,dinc,dmin,dmax,npts,
!                                         i      i      i     i    i    i   b
!                                      nptsmax,velmin,velmax,xvel,yvel,vel,ier)
!
! integer                iunit = output unit for well information.
! character(len=256)   fileout = output file name.
! integer            ncfileout = number of characters in output file name.
! character(len=256)      desc = description of well.
! integer               ncdesc = number of characters in description.
! character(len=256)    source = ?
! integer             ncsource = number of characters in source.
! character(len=256)sourcefile = source file
! integer         ncsourcefile = number of characters in source file.
! double precision        xorg = x origin of model, world coordinates.
! double precision        yorg = y origin of model, world coordinates.
! character(len=1)      F_or_M = indicates units: feet = 'f' or 'F',
!                                meters = 'm' or 'M'.
! real                    dinc = depth increment.
! real                    dmin = top depth.
! real                    dmax = bottom depth.
! integer                 npts = number of velocity points to write.
! integer              nptsmax = maximum array size for xvel, yvel, and
!                                vel arrays. Not used in write subroutine.
! real                  velmin = minimum velocity.
! real                  velmax = maximum velocity.
! real                    xvel = x coordinate of velocity.
! real                    yvel = y coordinate of velocity.
! real                     vel = velocity at (xvel,yvel).
! integer                  ier = error flag = 0 for no error, -1 otherwise.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Wherever possible, use the object interface subroutines. These are denoted
! by the prefix "gridlib_obj_".
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 15. 2007-01-03  Brian Macy Add optional argument to not permit znons in
!                            the model built by gridlib_model_from_grid and
!                            gridlib_model_from_warp_grid.
!014. 2006-06-27  D. Glover  Added NULLIFY statements for Intel compiler.
! 13. 2006-02-09  Brian Macy Fixed bug in handling units conversion for
!                            gradient grids.  Eliminate hard-coded units
!                            parameter in binary grid header.
! 12. 2006-01-31  Brian Macy Changed declaration of domain variable in
!                            gridlib_read_def_model_parms.
! 11. 2006-01-24  Brian Macy Added subroutine gridlib_read_def_model_parms and
!                            data structure gridlib_modspecheader_struct.  Also
!                            added private subroutines gridlib_decode,
!                            gridlib_upcase, and gridlib_keywrd to facilitate
!                            parsing of modspec header.
! 10. 2006-01-10  B. Menger  Removed Unused Variables.
!  9. 2005-09-12  Bill Done  Change format statement in subroutine
!                            gridlib_print_parms_insertion to prevent field
!                            overflow and assure white space between fields.
!  8. 2005-01-31  Bill Done  Remove velocity clipping code from subroutines
!                            gridlib_model_from_grid, gridlib_compute_velocity,
!                            gridlib_model_from_warp_grid.
!  7. 2004-09-30  Bill Done  Declare interfaces gridlib_read_grid_header,
!                            gridlib_read_grid, and gridlib_write_grid public.
!  6. 2004-09-28  Bill Done  The pgi compiler seemed to be sensitive to having
!                            an interface and module procedure with the same
!                            name. So gridlib_read_grid_header() was renamed
!                            gridlib_read_abgrid_header() and subroutine
!                            gridlib_read_grid was rename gridlib_read_abgrid.
!                            gridlib_read_well() and gridlib_write_well()
!                            had len=256 replaced with len=* for several input
!                            character parameters.
!  5. 2004-09-23  Bill Done  Add interface subroutines for using gridlib_struct
!                            user defined type. Add calling_doc documentation
!                            for the new gridlib_struct interface routines.
!  4. 2004-06-15  Bill Done  Modify subroutines gridlib_model_from_grid and
!                            gridlib_model_from_warp_grid to first read the
!                            input grid header to determine the input grid
!                            size, then allocate a local array to receive
!                            the input grid, then read the grid.
!  3. 2004-05-06  Bill Done  Make subroutines gridlib_read_grid_data and
!                            gridlib_read_agrid_data private. Add description,
!                            attributes, cunits, and punits to parameter list
!                            of several subroutines. Remove ngridmax parameter
!                            from subroutine gridlib_read_agrid_header. Make
!                            dimension of character units variable len=*
!                            where possible. Add subroutine
!                            gridlib_print_parms_insertion.
!  2. 2004-04-19  Bill Done  Add subroutine gridlib_compute_velocity_torz to
!                            compute velocity from time or depth modspecs.
!                            Add subroutine gridlib_read_grid_header to just
!                            read the header info for the grid. Correct order
!                            of type declarations.
!  1. 2004-04-13  Bill Done  Initial version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! --> Insert description of algorithms used.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! --> Insert any useful programming notes here.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module gridlib_module

      use fio_module
      use dio_module
      use pjar_module
      use named_constants_module

      implicit none

      private

      public :: gridlib_obj_read_grid_header
      public :: gridlib_obj_read_grid
      public :: gridlib_obj_read_bgrid
      public :: gridlib_obj_write_bgrid
      public :: gridlib_obj_get_min_max
      public :: gridlib_obj_verify_hdr_match
      public :: gridlib_obj_convert_units
      public :: gridlib_obj_convert_grid
      public :: gridlib_obj_convert_warp_grid

      public :: gridlib_read_grid_header           ! interface
      public :: gridlib_read_grid                  ! interface
      public :: gridlib_write_grid                 ! interface

      public :: gridlib_read_abgrid_header
      public :: gridlib_read_bgrid_header
      public :: gridlib_read_agrid_header
      public :: gridlib_read_abgrid
      public :: gridlib_read_agrid
      public :: gridlib_read_bgrid
      public :: gridlib_verify_headers_match
      public :: gridlib_write_agrid
      public :: gridlib_write_grid_fmt
      public :: gridlib_get_write_fmt
      public :: gridlib_write_bgrid
      public :: gridlib_get_min_max
      public :: gridlib_convert_units
      public :: gridlib_convert_grid
      public :: gridlib_read_def_model_parms
      public :: gridlib_read_deflt_model_parms
      public :: gridlib_model_from_grid
      public :: gridlib_print_model_parms
      public :: gridlib_print_parms_insertion
      public :: gridlib_compute_velocity
      public :: gridlib_fix_model
      public :: gridlib_check_clip_velocity
      public :: gridlib_convert_warp_grid
      public :: gridlib_model_from_warp_grid
      public :: gridlib_compute_gradient
      public :: gridlib_read_well
      public :: gridlib_write_well
      public :: gridlib_set_verbose
      public :: gridlib_compute_velocity_torz

      private :: gridlib_read_grid_data      ! interface
      private :: gridlib_read_agrid_data
      private :: gridlib_decode
      private :: gridlib_upcase
      private :: gridlib_keywrd

      character(len=100),save,public :: gridlib_ident =                        &
      '$Id: gridlib.f90,v 1.15 2007/01/03 14:01:40 Macy prod sps $'
      

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter, public   :: GRIDLIB_ATTRIB_LENGTH  = 80
      integer, parameter, public   :: GRIDLIB_UNITS_LENGTH   = 10
      integer, parameter, public   :: GRIDLIB_VERSION_LENGTH = 20
      integer, parameter, public   :: GRIDLIB_ENCODE_LENGTH  = 256
      integer, parameter, public   :: GRIDLIB_DOMAIN_LENGTH  = 5

      type,public :: gridlib_struct

         character(len=GRIDLIB_VERSION_LENGTH) :: version
         character(len=filename_length)        :: data_file
         integer                               :: ngridmax
         integer                               :: nx
         integer                               :: ny
         double precision                      :: xorg
         double precision                      :: yorg
         real                                  :: dx
         real                                  :: dy
         real                                  :: angle
         real                                  :: znon
         real                                  :: gridmin
         real                                  :: gridmax
         real, pointer                         :: grid(:,:)
         character(len=GRIDLIB_ENCODE_LENGTH)  :: encoding
         character(len=GRIDLIB_ATTRIB_LENGTH)  :: storage_order
         character(len=GRIDLIB_ATTRIB_LENGTH)  :: description
         character(len=GRIDLIB_ATTRIB_LENGTH)  :: attribute
         character(len=GRIDLIB_UNITS_LENGTH)   :: cunits
         character(len=GRIDLIB_UNITS_LENGTH)   :: punits

      end type gridlib_struct

      type,public :: gridlib_modspecheader_struct

         double precision                      :: xorg
         double precision                      :: yorg
         integer                               :: nx
         integer                               :: ny
         integer                               :: nzdef
         integer                               :: nlay
         real                                  :: dx
         real                                  :: dy
         real                                  :: dzdef
         real                                  :: angle
         real                                  :: znon
         integer                               :: a1
         integer                               :: a2
         integer                               :: a3
         integer                               :: b1
         integer                               :: b2
         integer                               :: b3
         integer                               :: cps_hdra
         integer                               :: cps_hdrb
         character(len=1)                      :: units
         character(len=GRIDLIB_DOMAIN_LENGTH)  :: domain

      end type gridlib_modspecheader_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface gridlib_read_grid_header
         module procedure gridlib_read_abgrid_header
         module procedure gridlib_read_agrid_header
         module procedure gridlib_read_bgrid_header
         module procedure gridlib_obj_read_grid_header
      end interface

      interface gridlib_read_grid
         module procedure gridlib_read_abgrid
         module procedure gridlib_read_agrid
         module procedure gridlib_obj_read_grid
      end interface

      interface gridlib_read_grid_data
         module procedure gridlib_read_agrid_data
      end interface

      interface gridlib_write_grid               ! write grid
         module procedure gridlib_write_agrid      ! ascii  format
         module procedure gridlib_write_bgrid      ! binary format
         module procedure gridlib_obj_write_bgrid  ! binary format from object
      end interface
      

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                      :: gridlib_verbose = 0
      ! Assume that FIO_OK=DIO_OK, FIO_ERROR=DIO_ERROR
      integer, parameter, public   :: GRIDLIB_OK       = FIO_OK
      integer, parameter, public   :: GRIDLIB_ERROR    = FIO_ERROR
      integer, parameter, public   :: GRIDLIB_BGRID_NOTFOUND = -99
      integer, parameter, public   :: GRIDLIB_ALLOC_ERROR    = -100
      character(len=20), parameter :: defsecname  = 'modspec_grid'
      character(len=20), parameter :: deffiletype = 'modspec_grid'

      contains


!********************************************************
!     subroutine to read in a grid header info from disk using object.

      subroutine gridlib_obj_read_grid_header(sounit, iunit, filein, gobj, &
                                              err, msg)

      implicit none

      integer,              intent(in)    :: sounit, iunit
      character(len=*),     intent(in)    :: filein
      type(gridlib_struct), intent(inout) :: gobj
      integer,              intent(out)   :: err
      character(len=*),     intent(out)   :: msg

      ! Local variables
      type(fio_struct),  pointer     :: fio
      type(dio_struct),  pointer     :: dio
      type(pjar_struct), pointer     :: pjar
      real                           :: grid_znon
      integer                        :: ngrid, ngriddata

      character(len=20)              :: filetype
      integer                        :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Open file and read headers
      call pjar_create(pjar)
      call dio_open_read(dio, filein, err, msg)
      if (err /= DIO_OK) then
         call pjar_delete(pjar)
         return
      end if
      call fio_create(fio, dio)
      call fio_read_header_sections(fio, pjar, err, msg, filetype)
      if (err /= FIO_OK .or. filetype /= deffiletype) then

         write(sounit,*) 'gridlib_obj_read_grid_header: attempt ascii read'

         err = GRIDLIB_BGRID_NOTFOUND

         ! attempt to read ascii header
         grid_znon = gobj%znon
         call gridlib_read_agrid_header(sounit, iunit, filein, gobj%nx, &
                                        gobj%ny, gobj%xorg, gobj%yorg,  &
                                        gobj%dx, gobj%dy, gobj%angle,   &
                                        grid_znon, err)

         if (err/= 0) then
            write(msg,*) 'gridlib_obj_read_grid_header: error in &
                         &gridlib_read_agrid_header'

            err = GRIDLIB_ERROR   ! convert error code

            ! Clean up
            call pjar_delete(pjar)
            call dio_close(dio)
            call fio_delete(fio)
            return
         else
            err = GRIDLIB_OK      ! convert error code
            gobj%znon = grid_znon
         end if

         write(sounit,*) 'gridlib_obj_read_grid_header: successfully read &
                         &ascii header'
         gobj%description = ''
         gobj%attribute   = ''
         gobj%cunits   = ''
         gobj%punits   = ''

         ! Clean up
         call pjar_delete(pjar)
         call dio_close(dio)
         call fio_delete(fio)
         return
      end if

      if (filetype /= deffiletype) then
         call pjar_delete(pjar)
         call dio_close(dio)
         call fio_delete(fio)
         write(msg,*) 'gridlib_obj_read_grid_header: Filetype should be ', &
                      deffiletype, ', not ', filetype
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      call pjar_choose_section(pjar, defsecname)

      ! Get parameters from header
      call pjar_get(pjar, 'version',       gobj%version,       CNIL)
      if (gobj%version /= '1.0') then
         write(msg,*) 'gridlib_obj_read_grid_header: Expected version=1.0, &
                      &but version = ', gobj%version
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'xorg',          gobj%xorg,          dble(0.0))
      call pjar_get(pjar, 'yorg',          gobj%yorg,          dble(0.0))
      call pjar_get(pjar, 'nx',            gobj%nx,            1)
      call pjar_get(pjar, 'ny',            gobj%ny,            1)
      call pjar_get(pjar, 'dx',            gobj%dx,            1.0)
      call pjar_get(pjar, 'dy',            gobj%dy,            1.0)
      call pjar_get(pjar, 'angle',         gobj%angle,         0.0)
      call pjar_get(pjar, 'znon',          gobj%znon,          FNIL)
      call pjar_get(pjar, 'description',   gobj%description,   CNIL)
      call pjar_get(pjar, 'attribute',     gobj%attribute,     CNIL)
      call pjar_get(pjar, 'cunits',        gobj%cunits,        CNIL)
      call pjar_get(pjar, 'punits',        gobj%punits,        CNIL)
      call pjar_get(pjar, 'data_file',     gobj%data_file,     'NONE')
      if (gobj%data_file /= 'NONE') then
         write(msg,*) 'gridlib_obj_read_grid_header: Cannot handle &
                      &data_file != NONE'
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'storage_order', gobj%storage_order, 'XY')
      call pjar_get(pjar, 'encoding', gobj%encoding)
      if (gobj%encoding /= FIO_BINARY) then
         write(msg,*) 'gridlib_obj_read_grid_header: Encoding must be &
                      &FIO_BINARY, not ', gobj%encoding
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'maxpicks', ngrid)
      if (ngrid /= gobj%nx*gobj%ny) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_obj_read_grid_header: Number &
                                      &of data points (', ngrid, &
                                      ') must = nx*ny (', gobj%nx*gobj%ny, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Read binary data
      column_grid = pjar_find(pjar, 'fields', 'grid')
      call fio_read_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if
      call fio_before_read_binary(fio, ngriddata)
      if (ngriddata /= ngrid) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_obj_read_grid_header: &
                                      &Inconsistency in ngrid between &
                                      &header (', ngrid,') and data (', &
                                      ngriddata, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Check storage order
      if (gobj%storage_order /= 'XY') then
         write(msg,'(A,A,A)') 'gridlib_obj_read_grid_header: Cannot handle &
                              &storage order = ', gobj%storage_order, &
                              ', must be = XY'
         err = GRIDLIB_ERROR
         return
      end if

      ! Clean up
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return

      end subroutine gridlib_obj_read_grid_header


!********************************************************
!     subroutine to read in a grid array from disk, using gridlib struct.

      subroutine gridlib_obj_read_grid(sounit, iunit, filein, reset_znon, &
                                       gobj, err)

      implicit none

      integer,              intent(in)    :: sounit, iunit
      character(len=*),     intent(in)    :: filein
      type(gridlib_struct), intent(inout) :: gobj
      logical,              intent(in)    :: reset_znon
      integer,              intent(out)   :: err

!---Local declarations
      integer          :: j, k, alloc_err
      real             :: grid_znon, znonin
      real             :: gridlcl(gobj%ngridmax)
      character(len=256) :: msg

!---Set default return error status
      err = 0

!---Save input znon to local variable
      znonin = gobj%znon

!---Read the grid file
      call gridlib_obj_read_bgrid(sounit, filein, gobj, err, msg)
      if (err == GRIDLIB_OK) then

         ! successfully read binary grid file

         ! save znon value from input binary grid
         grid_znon = gobj%znon

      else if (err == GRIDLIB_BGRID_NOTFOUND) then

         ! File wasn't a binary grid, try to read an ascii grid

         ! use input znon
         grid_znon = znonin

         ! attempt to read ascii grid
         call gridlib_read_agrid(sounit, iunit, filein, gridlcl,         &
                                 gobj%ngridmax, gobj%nx, gobj%ny,        &
                                 gobj%xorg, gobj%yorg, gobj%dx, gobj%dy, &
                                 gobj%angle, grid_znon, err)
         if (err /= GRIDLIB_OK) then
            write(sounit,*) 'gridlib_obj_read_grid: error in gridlib_read_agrid'
            return
         end if

!--------got the grid ok, transfer to grid in object
         allocate(gobj%grid(gobj%nx, gobj%ny), stat=alloc_err)
         if (alloc_err /= 0) then
            ! grid allocation failed
            write(sounit,*) 'Allocation for grid after reading ascii grid &
                            &failed'
            err = GRIDLIB_ALLOC_ERROR
            return
         end if
         do k = 1, gobj%ny
            gobj%grid(:,k) = gridlcl((k-1)*gobj%nx+1 : k*gobj%nx)
         end do

         ! set other attributes to empty strings
         gobj%description = ''
         gobj%attribute   = ''
         gobj%cunits   = ''
         gobj%punits   = ''
      else if (err /= GRIDLIB_OK) then

         ! error reading grid file
         write(sounit,*) msg
         return
      end if

!---Reset znon in grid if appropriate
      if (reset_znon) then
         do j = 1, gobj%nx
            do k = 1, gobj%ny
               if (gobj%grid(j,k) == grid_znon) then
                  gobj%grid(j,k) = znonin
               end if
            end do
         end do
         gobj%znon = znonin 
      else
         gobj%znon = grid_znon
      end if

!---Print out important grid information
      if (gridlib_verbose > 0) then
         call gridlib_obj_get_min_max(gobj, err)
         write(sounit,*)' '
         write(sounit,*)'*** READ FILE *** '
         write(sounit,*)trim(filein)
         write(sounit,*)' XORG    = ',gobj%xorg, '  YORG    = ',gobj%yorg
         write(sounit,*)' DX      = ',gobj%dx,   '  DY      = ',gobj%dy
         write(sounit,*)' NX      = ',gobj%nx,   '  NY      = ',gobj%ny
         write(sounit,*)' ANGLE   = ',gobj%angle,'  ZNON    = ',gobj%znon
         write(sounit,*)' GRIDMIN = ',gobj%gridmin,'  GRIDMAX = ',gobj%gridmax
         write(sounit,*)' '
      end if

      return

      end subroutine gridlib_obj_read_grid


!********************************************************
!     gridlib_read_bgrid - read a binary grid from a file

      subroutine gridlib_obj_read_bgrid(sounit, filein, gobj, err, msg)

      implicit none

      ! Subroutine arguments
      integer,              intent(in)    :: sounit
      character(len=*),     intent(in)    :: filein
      type(gridlib_struct), intent(inout) :: gobj
      integer,              intent(out)   :: err
      character(len=*),     intent(out)   :: msg

      ! local variables
      type(fio_struct),  pointer    :: fio
      type(dio_struct),  pointer    :: dio
      type(pjar_struct), pointer    :: pjar
      real, pointer                 :: gridlcl(:)
      integer                       :: ngrid, ngriddata
      integer                       :: alloc_err
      integer                       ::            k  
      character(len=20)             :: filetype
      integer                       :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Open file and read headers
      call pjar_create(pjar)
      call dio_open_read(dio, filein, err, msg)
      if (err /= DIO_OK) then
         return
      end if
      call fio_create(fio, dio)
      call fio_read_header_sections(fio, pjar, err, msg, filetype)
      if (err /= FIO_OK) then
         write(msg,*) 'gridlib_obj_read_bgrid: Error in &
                      &fio_read_header_sections', err
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      if (filetype /= deffiletype) then
         call pjar_delete(pjar)
         call dio_close(dio)
         call fio_delete(fio)
         write(msg,*) 'gridlib_obj_read_bgrid: Filetype should be ', &
                      deffiletype, ', not ', filetype
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      call pjar_choose_section(pjar, defsecname)

      ! Get parameters from header
      call pjar_get(pjar, 'version',       gobj%version,       CNIL)
      if (gobj%version /= '1.0') then
         write(msg,*) 'gridlib_obj_read_bgrid: Expected version=1.0, but &
                      &version = ', gobj%version
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'xorg',          gobj%xorg,          dble(0.0))
      call pjar_get(pjar, 'yorg',          gobj%yorg,          dble(0.0))
      call pjar_get(pjar, 'nx',            gobj%nx,            1)
      call pjar_get(pjar, 'ny',            gobj%ny,            1)
      call pjar_get(pjar, 'dx',            gobj%dx,            1.0)
      call pjar_get(pjar, 'dy',            gobj%dy,            1.0)
      call pjar_get(pjar, 'angle',         gobj%angle,         0.0)
      call pjar_get(pjar, 'znon',          gobj%znon,          FNIL)
      call pjar_get(pjar, 'description',   gobj%description,   CNIL)
      call pjar_get(pjar, 'attribute',     gobj%attribute,     CNIL)
      call pjar_get(pjar, 'cunits',        gobj%cunits,        CNIL)
      call pjar_get(pjar, 'punits',        gobj%punits,        CNIL)
      call pjar_get(pjar, 'data_file',     gobj%data_file,     'NONE')
      if (gobj%data_file /= 'NONE') then
         write(msg,*) 'gridlib_obj_read_bgrid: Cannot handle data_file != NONE'
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'storage_order', gobj%storage_order, 'XY')
      call pjar_get(pjar, 'encoding', gobj%encoding)
      if (gobj%encoding /= FIO_BINARY) then
         write(msg,*) 'gridlib_obj_read_bgrid: Encoding must be FIO_BINARY, &
                      &not ', gobj%encoding
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'maxpicks', ngrid)
      if (ngrid /= gobj%nx*gobj%ny) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_obj_read_bgrid: Number of data &
                                      &points (', ngrid, ') must = nx*ny (', &
                                      gobj%nx*gobj%ny, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! allocate memory for local 1D grid data
      allocate(gridlcl(gobj%nx * gobj%ny), stat=alloc_err)
      if (alloc_err /= 0) then
         ! grid allocation failed
         write(sounit,*) 'gridlib_obj_read_bgrid: Allocation for 1D local &
                         &grid failed'
         err = GRIDLIB_ALLOC_ERROR
         return
      end if

      ! Read binary data into 1D local grid object
      column_grid = pjar_find(pjar, 'fields', 'grid')
      call fio_read_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if
      call fio_before_read_binary(fio, ngriddata)
      if (ngriddata /= ngrid) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_obj_read_bgrid: Inconsistency &
                                      &in ngrid between header (', ngrid,    &
                                      ') and data (', ngriddata, ')'
         err = GRIDLIB_ERROR
         return
      end if
      call fio_read_binary      (fio, column_grid, gridlcl)
      call fio_after_read_binary(fio, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! allocate memory for object grid data
      allocate(gobj%grid(gobj%nx, gobj%ny), stat=alloc_err)
      if (alloc_err /= 0) then
         ! grid allocation failed
         write(sounit,*) 'gridlib_obj_read_bgrid: Allocation for grid failed'
         err = GRIDLIB_ALLOC_ERROR
         return
      end if

      ! transfer grid data from local 1D grid to 2D grid
      do k = 1, gobj%ny
         gobj%grid(:,k) = gridlcl((k-1)*gobj%nx+1 : k*gobj%nx)
      end do

      ! free 1D local memory
      deallocate(gridlcl)

      ! Check storage order
      if (gobj%storage_order /= 'XY') then
         write(msg,'(A,A,A)') 'gridlib_obj_read_bgrid: Cannot handle storage &
                              &order = ', gobj%storage_order, ', must be = XY'
         err = GRIDLIB_ERROR
         return
      end if

      ! Clean up
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return

      end subroutine gridlib_obj_read_bgrid


!********************************************************
!     gridlib_obj_write_bgrid - write a binary grid to a file from gridlib
!                               struct

      subroutine gridlib_obj_write_bgrid(fileout, gobj, err, msg)

      implicit none

      ! Subroutine arguments
      character(len=*),     intent(in)    :: fileout
      type(gridlib_struct), intent(inout) :: gobj
      integer,              intent(out)   :: err
      character(len=*),     intent(out)   :: msg

      ! Internal variables
      type(fio_struct),  pointer    :: fio
      type(dio_struct),  pointer    :: dio
      type(pjar_struct), pointer    :: pjar
      integer                       :: ngdata
      real, allocatable             :: gdata(:)
      integer                       ::        iy  
      integer                       :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Copy grid data to 1-d array with x as fastest dimension
      ngdata = gobj%nx*gobj%ny
      allocate(gdata(ngdata), stat=err)
      if (err /= 0) then
         write(msg,*) 'gridlib_obj_write_bgrid: Error allocating gdata, &
                      &err = ', err
         err = GRIDLIB_ERROR
         return
      end if
      do iy = 1, gobj%ny
         gdata(1+(iy-1)*gobj%nx : iy*gobj%nx) = gobj%grid(:, iy)
      end do

      ! Define the header parameters
      call pjar_create(pjar)
      call pjar_choose_section(pjar, defsecname)

      call pjar_put(pjar, 'version',       '1.0')
      ! dio should always write LITTLE ENDIAN, according to swap_module;
      ! fio will provide function to determine endianness in the future
      call pjar_put(pjar, 'endian',        'LITTLE_ENDIAN')
      call pjar_put(pjar, 'xorg',          gobj%xorg)
      call pjar_put(pjar, 'yorg',          gobj%yorg)
      call pjar_put(pjar, 'nx',            gobj%nx)
      call pjar_put(pjar, 'ny',            gobj%ny)
      call pjar_put(pjar, 'dx',            gobj%dx)
      call pjar_put(pjar, 'dy',            gobj%dy)
      call pjar_put(pjar, 'angle',         gobj%angle)
      call pjar_put(pjar, 'znon',          gobj%znon)
      call pjar_put(pjar, 'description',   gobj%description)
      call pjar_put(pjar, 'attribute',     gobj%attribute)
      call pjar_put(pjar, 'data_file',     'NONE')
      call pjar_put(pjar, 'storage_order', 'XY')

      call pjar_put(pjar, 'encoding', FIO_BINARY)
      call pjar_put(pjar, 'fields',   (/'grid'/), 1)
      call pjar_put(pjar, 'vartypes', (/'F'/), 1)
      call pjar_put(pjar, 'cunits',   gobj%cunits)
      call pjar_put(pjar, 'punits',   gobj%punits)
      if (gobj%cunits(1:1) == 'F' .or. gobj%cunits(1:1) == 'f') then
        call pjar_put(pjar, 'units',    (/'feet'/), 1)
      else if (gobj%cunits(1:1) == 'M' .or. gobj%cunits(1:1) == 'm') then
        call pjar_put(pjar, 'units',    (/'meters'/), 1)
      else
        call pjar_put(pjar, 'units',    (/' '/), 1)
      end if
      call pjar_put(pjar, 'maxpicks', ngdata)

      column_grid = pjar_find(pjar, 'fields', 'grid')

      ! Open output file and write headers
      call dio_open_write(dio, fileout, err, msg)
      if (err /= DIO_OK) then
         return
      end if
      call fio_create(fio, dio)
      call fio_write_header_sections(fio, pjar, err, msg, deffiletype)
      if (err /= FIO_OK) then
         return
      end if
      call fio_write_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! Write binary data
      call fio_before_write_binary(fio, ngdata)
      call fio_write_binary       (fio, column_grid, gdata)
      call fio_after_write_binary (fio, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! Clean up
      deallocate(gdata)
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return
      end subroutine gridlib_obj_write_bgrid


!********************************************************
!     subroutine to find the min and max grid values, ignoring ZNONs

      subroutine gridlib_obj_get_min_max(gobj, err)

      implicit none

      type(gridlib_struct), intent(inout) :: gobj
      integer,              intent(out)   :: err

!---Local declarations
      real             :: big
      integer          :: iflag, j, k

!---Set default return error status
      err= 0

!---Find the min and max values, ignoring znons.
      big=999999999.
      iflag=0
      do j = 1, gobj%nx
         do k = 1, gobj%ny
            if (gobj%grid(j,k) /= gobj%znon) then
               if (iflag == 0) then
                  gobj%gridmin=big
                  gobj%gridmax=-big
                  iflag=1
               end if
               if (gobj%grid(j,k) < gobj%gridmin) gobj%gridmin=gobj%grid(j,k)
               if (gobj%grid(j,k) > gobj%gridmax) gobj%gridmax=gobj%grid(j,k)
            end if
         end do
      end do

      return

      end subroutine gridlib_obj_get_min_max


!********************************************************

      subroutine gridlib_obj_verify_hdr_match(gobjin, gobj, match, eps)

      implicit none

      type(gridlib_struct), intent(in)    :: gobjin
      type(gridlib_struct), intent(in)    :: gobj
      integer,              intent(out)   :: match
      real, optional,       intent(inout) :: eps

!---Local declarations
      double precision :: xorg_diff, yorg_diff
      real             :: dx_diff, dy_diff
      real             :: ang_diff

      if (.not. present(eps)) then
         eps = 0.01
      end if

      match=0   ! grid headers don't match
      dx_diff=abs(gobjin%dx - gobj%dx)
      dy_diff=abs(gobjin%dy - gobj%dy)
      xorg_diff=abs(gobjin%xorg - gobj%xorg)
      yorg_diff=abs(gobjin%yorg - gobj%yorg)
      ang_diff=abs(gobjin%angle - gobj%angle)

      if(gobjin%nx == gobj%nx .and. gobjin%ny == gobj%ny .and.  &
         dx_diff < eps   .and. dy_diff < eps   .and.            &
         xorg_diff < eps .and. yorg_diff < eps .and.            &
         ang_diff < eps  .and.                                  &
         gobjin%cunits == gobj%cunits .and.                     &
         gobjin%punits == gobj%punits) match=1

      return

      end subroutine gridlib_obj_verify_hdr_match


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

      subroutine gridlib_obj_convert_units(iflag,cunitsout,punitsout,gobj)

      implicit none

      integer,              intent(in)    :: iflag
      character(len=*),     intent(in)    :: cunitsout, punitsout
      type(gridlib_struct), intent(inout) :: gobj

!---Local declarations
      integer          :: iflagin_c, iflagout_c
      integer          :: iflagin_p, iflagout_p
      integer          :: ix, iy
      real             :: scale_c, scale_p

      ! see if coordinate units require change
      iflagin_c = 1
      if(gobj%cunits == 'm' .or. gobj%cunits == 'M') iflagin_c = 2
      iflagout_c = 1
      if(cunitsout == 'm' .or. cunitsout == 'M') iflagout_c = 2

      ! see if property units require change
      iflagin_p = 1
      if(gobj%punits == 'm' .or. gobj%punits == 'M') iflagin_p = 2
      iflagout_p = 1
      if(punitsout == 'm' .or. punitsout == 'M') iflagout_p = 2

      ! can return if both coordinate and property units unchanged
      if(iflagin_c == iflagout_c .and. iflagin_p == iflagout_p) return

      ! there was a scale change in coordinates and/or property units
      if(iflagin_c == 1 .and. iflagout_c == 2) then
         scale_c = 0.3048
      else  ! only iflagin_c == 2 and iflagout_c == 1 case remains
         scale_c = 1.0/0.3048
      end if
      if(iflagin_p == 1 .and. iflagout_p == 2) then
         scale_p = 0.3048
      else  ! only iflagin_p == 2 and iflagout_p == 1 case remains
         scale_p = 1.0/0.3048
      end if

      if(iflag /= 0) then
         ! flag indicates property units should be changed
         do ix = 1, gobj%nx
            do iy = 1, gobj%ny
               if(gobj%grid(ix,iy) /= gobj%znon) then
                  gobj%grid(ix,iy) = gobj%grid(ix,iy)*scale_p
               end if
            end do
         end do
      end if

      ! change coordinate units
      gobj%xorg = gobj%xorg*scale_c
      gobj%yorg = gobj%yorg*scale_c
      gobj%dx = gobj%dx*scale_c
      gobj%dy = gobj%dy*scale_c

      return

      end subroutine gridlib_obj_convert_units


!********************************************************
!---PROGRAM NAME:         gridlib_obj_convert_grid
!
!---DESCRIPTION:          This subroutine will convert an input grid array
!                         so that it has a set of desired grid parameters.
!                         Input grid parameters are: gobj1
!                         Desired grid parameters are: gobj2, which represents
!                             the grid structure in the overall modspec space
!                             to which the grid defined by gobj1 is to be 
!                             mapped. The data for the mapped grid is returned
!                             in gobj2.
!
!                         This is the object version of gridlib_convert_grid.
!
!                         The "IFLAG" parameter allows you to determine
!                         whether only the grid parms (IFLAG=0) have unit
!                         changes or whether the grid parms and the grid data
!                         have unit  changes (IFLAG=1) For example, if your
!                         data is in  milliseconds, you would want to set
!                         IFLAG=0.
!
!                         Setting the "NO_EXTRAP" parameter to "1" sets all
!                         points outside the input grid to GNULL, defined as
!                         the znon value associated with gobj2.
!

      subroutine gridlib_obj_convert_grid(sounit, iflag, no_extrap,  &
                                          gobj1, gobj2)

      implicit none

      integer,              intent(in)    :: sounit, iflag
      integer,              intent(in)    :: no_extrap
      type(gridlib_struct), intent(inout) :: gobj1
      type(gridlib_struct), intent(inout) :: gobj2

!---Local declarations
      real             :: gnull
      real, parameter  :: e1=0.01
      double precision :: xf,yf
      double precision :: dxo, dyo, dxg, dyg
      real             :: rad1, rad2
      integer          :: ix1, iy1, ix2, iy2
      real             :: xp, yp, xx, yy, xndx, yndx
      real             :: g1, g2, g11, g21, g12, g22
      real             :: s, u, w

!---set gnull to the znon of grid object gobj2
      gnull = gobj2%znon

!---change units in grid object 1 to those of grid object 2 if necessary
      call gridlib_obj_convert_units(iflag,gobj2%cunits,gobj2%punits,gobj1)

!---check if new grid is subarea of old grid
      if (abs(gobj2%dx-gobj1%dx) < e1 .and. abs(gobj2%dy-gobj1%dy) < e1 .and.  &
          abs(gobj2%angle-gobj1%angle) < e1) then
         rad1 = gobj1%angle/180.*3.141592654
         dxo = gobj2%xorg-gobj1%xorg
         dyo = gobj2%yorg-gobj1%yorg
         dxg =  dxo*cos(rad1) + dyo*sin(rad1)
         dyg = -dxo*sin(rad1) + dyo*cos(rad1)
         ix1 = nint(dxg/gobj1%dx+1.0)
         iy1 = nint(dyg/gobj1%dy+1.0)
         if (ix1 > 0 .and. iy1 > 0 .and.  &
             abs((ix1-1)*gobj1%dx-dxg) < e1 .and.  &
             abs((iy1-1)*gobj1%dy-dyg) < e1 .and.  &
             (ix1+gobj2%nx-1) <= gobj1%nx   .and.  &
             (iy1+gobj2%ny-1) <= gobj1%ny) then
            do ix2=1,gobj2%nx
            do iy2=1,gobj2%ny
               gobj2%grid(ix2,iy2)=gobj1%grid(ix1+ix2-1,iy1+iy2-1)
            end do
            end do
            goto 999
         end if
      end if

!---resample grid
      rad1 = gobj1%angle/180.*3.141592654
      rad2 = gobj2%angle/180.*3.141592654
      do ix2=1,gobj2%nx
      do iy2=1,gobj2%ny
         xp = (ix2-1)*gobj2%dx
         yp = (iy2-1)*gobj2%dy
         xf = xp*cos(rad2) - yp*sin(rad2) + gobj2%xorg
         yf = xp*sin(rad2) + yp*cos(rad2) + gobj2%yorg

         xx=xf-gobj1%xorg
         yy=yf-gobj1%yorg

         xndx =  xx*cos(rad1) + yy*sin(rad1)
         yndx = -xx*sin(rad1) + yy*cos(rad1)
         xndx=xndx/gobj1%dx+1.0
         yndx=yndx/gobj1%dy+1.0
         ix1=xndx
         iy1=yndx

!         -----------------------------
!        |       |              |       |
!        | case1 |    case 7    | case4 |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        |       |              |       |
!        | case3 |    case 9    | case6 |
!        |       |              |       |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        | case2 |    case 8    | case5 |
!        |       |              |       |
!         ------------------------------
         if (no_extrap == 1 .and. (xndx < 1 .or. xndx > gobj1%nx .or.  &
                                   yndx < 1 .or. yndx > gobj1%ny)) then
            gobj2%grid(ix2,iy2)=gnull
         else if(ix1 < 1 ) then
            if(iy1 < 1)then
! case 1
               gobj2%grid(ix2,iy2)=gobj1%grid(1,1)
            else if (iy1 >= gobj1%ny) then
! case 2
               gobj2%grid(ix2,iy2)=gobj1%grid(1,gobj1%ny)
            else
! case 3
               g1 = gobj1%grid(1,iy1)
               g2 = gobj1%grid(1,iy1+1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear y-interp
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               elseif((yndx-iy1) > 0.5) then           !nearest neighbor
                  gobj2%grid(ix2,iy2)=g2
               else
                  gobj2%grid(ix2,iy2)=g1
               endif
            end if
         else if(ix1 >= gobj1%nx) then
            if(iy1 < 1)then
! case 4
               gobj2%grid(ix2,iy2)=gobj1%grid(gobj1%nx,1)
            else if (iy1 >= gobj1%ny) then
! case 5
               gobj2%grid(ix2,iy2)=gobj1%grid(gobj1%nx,gobj1%ny)
            else
! case 6
               g1 = gobj1%grid(gobj1%nx,iy1)
               g2 = gobj1%grid(gobj1%nx,iy1+1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear y-interp
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               elseif((yndx-iy1) > 0.5) then           !nearest neighbor
                  gobj2%grid(ix2,iy2)=g2
               else
                  gobj2%grid(ix2,iy2)=g1
               endif
            end if

         else if(iy1 < 1 .and.  &
                (ix1 >= 1 .and. ix1 < gobj1%nx) ) then
! case 7
               g1 = gobj1%grid(ix1,1)
               g2 = gobj1%grid(ix1+1,1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear x-interp
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
               elseif((xndx-ix1) > 0.5) then           !nearest neighbor
                  gobj2%grid(ix2,iy2)=g2
               else
                  gobj2%grid(ix2,iy2)=g1
               endif
         else if(iy1 >= gobj1%ny .and.  &
                (ix1 >= 1 .and. ix1 < gobj1%nx) ) then
! case 8
               g1 = gobj1%grid(ix1,gobj1%ny)
               g2 = gobj1%grid(ix1+1,gobj1%ny)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear x-interp
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
               elseif((xndx-ix1) > 0.5) then           !nearest neighbor
                  gobj2%grid(ix2,iy2)=g2
               else
                  gobj2%grid(ix2,iy2)=g1
               endif
! case 9
!     interpolate along y's first
         else if(ix1 >= 1 .and. ix1 < gobj1%nx .and.  &
                 iy1 >= 1 .and. iy1 < gobj1%ny ) then
               gobj2%grid(ix2,iy2)=gnull
               g11 = gobj1%grid(ix1  ,iy1  )
               g21 = gobj1%grid(ix1+1,iy1  )
               g12 = gobj1%grid(ix1  ,iy1+1)
               g22 = gobj1%grid(ix1+1,iy1+1)
               if(g11 /= gnull .and. g21 /= gnull .and. &    !bi-linear interp
                  g12 /= gnull .and. g22 /= gnull) then
                  g1 = g11+(g21-g11)*(xndx-ix1)
                  g2 = g12+(g22-g12)*(xndx-ix1)
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               else if ((xndx-ix1) > 0.5 .and. (yndx-iy1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1+1,iy1+1)
                  if (g22 == gnull) then
                     gobj2%grid(ix2,iy2)=gnull
                  elseif(g22 /= gnull .and. g21 /= gnull .and.  &
                     g12 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     gobj2%grid(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g22 /= gnull .and. g12 /= gnull .and.  &
                     g11 /= gnull .and. (xndx-ix1) <= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     gobj2%grid(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g22 /= gnull .and. g21 /= gnull .and.  &
                     g11 /= gnull .and. (xndx-ix1) >= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     gobj2%grid(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g22 /= gnull .and. g11 /= gnull .and.  &
                     (xndx-ix1) >= (yndx-iy1)) then
                     !modified tri-linear interp
                     w=ix1+1-xndx
                     gobj2%grid(ix2,iy2)=w*(g11-g22)+g22
                  elseif(g22 /= gnull .and. g11 /= gnull .and.  &
                     (xndx-ix1) < (yndx-iy1)) then
                     !modified tri-linear interp
                     u=iy1+1-yndx
                     gobj2%grid(ix2,iy2)=u*(g11-g22)+g22
                  elseif(g22 /= gnull .and. g12 /= gnull) then
                     !linear x-interp
                     gobj2%grid(ix2,iy2)=(g22-g12)*(xndx-ix1)+g12
                  elseif(g22 /= gnull .and. g21 /= gnull) then
                     !linear y-interp
                     gobj2%grid(ix2,iy2)=(g22-g21)*(yndx-iy1)+g21
                  else
                     !nearest neighbor
                     gobj2%grid(ix2,iy2)=g22
                  end if
               else if ((xndx-ix1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1+1,iy1)
                  if (g21 == gnull) then
                     gobj2%grid(ix2,iy2)=gnull
                  elseif(g21 /= gnull .and. g11 /= gnull .and.  &
                     g22 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     gobj2%grid(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g21 /= gnull .and. g11 /= gnull .and.  &
                     g12 /= gnull .and. (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     gobj2%grid(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     gobj2%grid(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     u=ix1+1-xndx
                     gobj2%grid(ix2,iy2)=u*(g12-g21)+g21
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     (xndx-ix1) < (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     w=yndx-iy1
                     gobj2%grid(ix2,iy2)=w*(g12-g21)+g21
                  elseif(g21 /= gnull .and. g11 /= gnull) then
                     !linear x-interp
                     gobj2%grid(ix2,iy2)=(g21-g11)*(xndx-ix1)+g11
                  elseif(g21 /= gnull .and. g22 /= gnull) then
                     !linear y-interp
                     gobj2%grid(ix2,iy2)=(g22-g21)*(yndx-iy1)+g21
                  else
                     !nearest neighbor
                     gobj2%grid(ix2,iy2)=g21
                  end if
               else if ((yndx-iy1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1,  iy1+1)
                  if (g12 == gnull) then
                     gobj2%grid(ix2,iy2)=gnull
                  elseif(g12 /= gnull .and. g11 /= gnull .and.  &
                     g22 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     gobj2%grid(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     gobj2%grid(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g12 /= gnull .and. g11 /= gnull .and.  &
                     g21 /= gnull .and. (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     gobj2%grid(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     u=xndx-ix1
                     gobj2%grid(ix2,iy2)=u*(g21-g12)+g12
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     (xndx-ix1) > (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     w=iy1+1-yndx
                     gobj2%grid(ix2,iy2)=w*(g21-g12)+g12
                  elseif(g12 /= gnull .and. g22 /= gnull) then
                     !linear x-interp
                     gobj2%grid(ix2,iy2)=(g22-g12)*(xndx-ix1)+g12
                  elseif(g12 /= gnull .and. g11 /= gnull) then
                     !linear y-interp
                     gobj2%grid(ix2,iy2)=(g12-g11)*(yndx-iy1)+g11
                  else
                     !nearest neighbor
                     gobj2%grid(ix2,iy2)=g12
                  end if
               else
                  ! (xndx,yndx) closest to (ix1,  iy1)
                  if (g11 == gnull) then
                     gobj2%grid(ix2,iy2)=gnull
                  elseif(g11 /= gnull .and. g21 /= gnull .and.  &
                     g12 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     gobj2%grid(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g11 /= gnull .and. g21 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     gobj2%grid(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g11 /= gnull .and. g12 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) <= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     gobj2%grid(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g11 /= gnull .and. g22 /= gnull .and.  &
                     (xndx-ix1) <= (yndx-iy1)) then
                     !modified tri-linear interp
                     w=xndx-ix1
                     gobj2%grid(ix2,iy2)=w*(g22-g11)+g11
                  elseif(g11 /= gnull .and. g22 /= gnull .and.  &
                     (xndx-ix1) > (yndx-iy1)) then
                     !modified tri-linear interp
                     u=yndx-iy1
                     gobj2%grid(ix2,iy2)=u*(g22-g11)+g11
                  elseif(g11 /= gnull .and. g21 /= gnull) then
                     !linear x-interp
                     gobj2%grid(ix2,iy2)=(g21-g11)*(xndx-ix1)+g11
                  elseif(g11 /= gnull .and. g12 /= gnull) then
                     !linear y-interp
                     gobj2%grid(ix2,iy2)=(g12-g11)*(yndx-iy1)+g11
                  else
                     !nearest neighbor
                     gobj2%grid(ix2,iy2)=g11
                  end if
               end if
! case '10' - just in case we missed a case
         else
               gobj2%grid(ix2,iy2)=gnull
         end if

      end do
      end do

999   continue

      write(sounit,*)' '
      write(sounit,*)' gridlib_obj_convert_grid: xorg1, &
                     &yorg1=',gobj1%xorg,gobj1%yorg
      write(sounit,*)' gridlib_obj_convert_grid: xorg2, yorg2=',  &
                     gobj2%xorg,gobj2%yorg
      write(sounit,*)' '

      return

      end subroutine gridlib_obj_convert_grid


!********************************************************
!---PROGRAM NAME:         gridlib_obj_convert_warp_grid
!
!---DESCRIPTION:          This subroutine will convert an input grid array
!                         so that it has a set of desired grid parameters.
!                         Input grid parameters are: gobj1
!                         Desired grid parameters are: gobj2, which represents
!                             the grid structure in the overall modspec space
!                             to which the grid defined by gobj1 is to be 
!                             mapped. The data for the mapped grid is returned
!                             in gobj2.
!
!                         This is the object interface for converting a
!                         warped grid.
!
!                         The "IFLAG" parameter allows you to determine
!                         whether only the grid parms (IFLAG=0) have unit
!                         changes or whether the grid parms and the grid data
!                         have unit changes (IFLAG=1) For example, if your
!                         data is in milliseconds, you would want to set
!                         IFLAG=0.

      subroutine gridlib_obj_convert_warp_grid(sounit, iflag, gobj1, gobj2, &
                                               gobjx2, gobjy2)

      implicit none

      integer,              intent(in)    :: sounit, iflag
      type(gridlib_struct), intent(inout) :: gobj1
      type(gridlib_struct), intent(inout) :: gobj2
      type(gridlib_struct), intent(in)    :: gobjx2
      type(gridlib_struct), intent(in)    :: gobjy2

!---Local declarations
      real             :: gnull
      integer          :: ix1, iy1, ix2, iy2
      real             :: xp, yp, xf, yf, xx, yy, xndx, yndx
      real             :: rad1, rad2, g1, g2, g12, g21, g11, g22

!---set gnull to the znon of grid object gobj2
      gnull = gobj2%znon

!---change units in grid object 1 to those of grid object 2 if necessary
      call gridlib_obj_convert_units(iflag,gobj2%cunits,gobj2%punits,gobj1)

!---resample grid
      rad1 = gobj1%angle/180.*3.141592654
      rad2 = gobj2%angle/180.*3.141592654
      do ix2=1,gobj2%nx
      do iy2=1,gobj2%ny

!        xp = (ix2-1)*gobj2%dx
!        yp = (iy2-1)*gobj2%dy

         xp = gobjx2%grid(ix2,iy2)
         yp = gobjy2%grid(ix2,iy2)

         xf = xp*cos(rad2) - yp*sin(rad2) + gobj2%xorg
         yf = xp*sin(rad2) + yp*cos(rad2) + gobj2%yorg

         xx=xf-gobj1%xorg
         yy=yf-gobj1%yorg

         xndx =  xx*cos(rad1) + yy*sin(rad1)
         yndx = -xx*sin(rad1) + yy*cos(rad1)
         xndx=xndx/gobj1%dx+1.0
         yndx=yndx/gobj1%dy+1.0
         ix1=xndx
         iy1=yndx

!         -----------------------------
!        |       |              |       |
!        | case1 |    case 7    | case4 |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        |       |              |       |
!        | case3 |    case 9    | case6 |
!        |       |              |       |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        | case2 |    case 8    | case5 |
!        |       |              |       |
!         ------------------------------
         if(ix1 < 1 ) then
            if(iy1 < 1)then
! case 1
               gobj2%grid(ix2,iy2)=gobj1%grid(1,1)
            else if (iy1 >= gobj1%ny) then
! case 2
               gobj2%grid(ix2,iy2)=gobj1%grid(1,gobj1%ny)
            else
! case 3
               g1 = gobj1%grid(1,iy1)
               g2 = gobj1%grid(1,iy1+1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
            end if
         else if(ix1 >= gobj1%nx) then
            if(iy1 < 1)then
! case 4
               gobj2%grid(ix2,iy2)=gobj1%grid(gobj1%nx,1)
            else if (iy1 >= gobj1%ny) then
! case 5
               gobj2%grid(ix2,iy2)=gobj1%grid(gobj1%nx,gobj1%ny)
            else
! case 6
               g1 = gobj1%grid(gobj1%nx,iy1)
               g2 = gobj1%grid(gobj1%nx,iy1+1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
            end if

         else if(iy1 < 1 .and.   &
                (ix1 >= 1 .and. ix1 < gobj1%nx) ) then
! case 7
               g1 = gobj1%grid(ix1,1)
               g2 = gobj1%grid(ix1+1,1)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
         else if(iy1 >= gobj1%ny .and.  &
                (ix1 >= 1 .and. ix1 < gobj1%nx) ) then
! case 8
               g1 = gobj1%grid(ix1,gobj1%ny)
               g2 = gobj1%grid(ix1+1,gobj1%ny)
               gobj2%grid(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
! case 9
!     interpolate along y's first
         else if(ix1 >= 1 .and. ix1 < gobj1%nx .and.  &
                 iy1 >= 1 .and. iy1 < gobj1%ny ) then
               gobj2%grid(ix2,iy2)=gnull
               g11 = gobj1%grid(ix1  ,iy1  )
               g21 = gobj1%grid(ix1+1,iy1  )
               g12 = gobj1%grid(ix1  ,iy1+1)
               g22 = gobj1%grid(ix1+1,iy1+1)
               if(g11 /= gnull .and. g21 /= gnull .and.  &
                  g12 /= gnull .and. g22 /= gnull) then
                  g1 = g11+(g21-g11)*(xndx-ix1)
                  g2 = g12+(g22-g12)*(xndx-ix1)
                  gobj2%grid(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               end if
         end if

      end do
      end do

      write(sounit,*)' '
      write(sounit,*)' gridlib_obj_convert_warp_grid: xorg1, yorg1=', &
                     gobj1%xorg,gobj1%yorg
      write(sounit,*)' gridlib_obj_convert_warp_grid: xorg2, yorg2=', &
                     gobj2%xorg,gobj2%yorg
      write(sounit,*)' '

      return
      end subroutine gridlib_obj_convert_warp_grid


!********************************************************
!     subroutine to read in a grid header info from disk.

      subroutine gridlib_read_abgrid_header(sounit, iunit, filein, ngridmax,  &
                                            nx, ny, xorg, yorg, dx, dy, angle,&
                                            znon, description, attribute,     &
                                            cunits, punits, errRetVal)

      implicit none

      integer,          intent(in)    :: sounit, iunit
      character(len=*), intent(in)    :: filein
      integer,          intent(out)   :: ngridmax
      integer,          intent(inout) :: nx, ny
      double precision, intent(inout) :: xorg, yorg
      real,             intent(inout) :: dx, dy, angle
      real,             intent(inout) :: znon
      character(len=*), intent(inout) :: description, attribute
      character(len=*), intent(inout) :: cunits, punits
      integer,          intent(out)   :: errRetVal

!---Local declarations
      real               :: grid_znon
      character(len=256) :: msg

!---Set default return error status
      errRetVal = 0

!---Read the grid file
      call gridlib_read_bgrid_header(filein, xorg, yorg, nx, ny, dx, dy, &
                                     angle, grid_znon, description,      &
                                     attribute, cunits, punits, errRetVal, &
                                     msg)
      if (errRetVal == GRIDLIB_OK) then

         ! successfully read header from binary grid file

         ! set the output znon value
         znon = grid_znon

      else if (errRetVal == GRIDLIB_BGRID_NOTFOUND) then

         ! File wasn't a binary grid, try to read an ascii grid

         ! set the input znon value (a default value if znon not specified
         ! in ascii header)
         grid_znon = znon
         call gridlib_read_agrid_header(sounit, iunit, filein, nx, ny, &
                                        xorg, yorg, dx, dy, angle,     &
                                        grid_znon, errRetVal)
         if (errRetVal /= GRIDLIB_OK) then
            write(sounit,*) 'gridlib_read_abgrid_header: error in &
                            &gridlib_read_agrid_header'
            return
         end if

         znon = grid_znon
         description = ''
         attribute   = ''
         cunits   = ''
         punits   = ''

      else if (errRetVal /= GRIDLIB_OK) then

         ! error reading grid
         write(sounit,*) msg
         return
      end if

      ngridmax = nx * ny

      end subroutine gridlib_read_abgrid_header


!********************************************************
      ! gridlib_read_bgrid_header - read binary grid header infofrom a file
      !
      subroutine gridlib_read_bgrid_header(filename, xorg, yorg, nx, ny, &
                                    dx, dy, angle, znon, description,    &
                                    attribute, cunits, punits, err, msg)

      implicit none

      ! Subroutine arguments
      character(len=*), intent(in)    :: filename
      double precision, intent(out)   :: xorg, yorg
      integer,          intent(out)   :: nx,   ny
      real,             intent(out)   :: dx,   dy
      real,             intent(out)   :: angle
      real,             intent(out)   :: znon
      character(len=*), intent(out)   :: description
      character(len=*), intent(out)   :: attribute
      character(len=*), intent(out)   :: cunits, punits
      integer,          intent(out)   :: err
      character(len=*), intent(out)   :: msg

      ! Internal variables
      type(fio_struct),  pointer    :: fio
      type(dio_struct),  pointer    :: dio
      type(pjar_struct), pointer    :: pjar
      integer                       :: ngrid, ngriddata

      character(len=20)             :: filetype
      character(len=20)             :: version
      character(len=20)             :: storage_order
      character(len=256)            :: data_file
      character(len=256)            :: encoding
      integer                       :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Open file and read headers
      call pjar_create(pjar)
      call dio_open_read(dio, filename, err, msg)
      if (err /= DIO_OK) then
         return
      end if
      call fio_create(fio, dio)
      call fio_read_header_sections(fio, pjar, err, msg, filetype)
      if (err /= FIO_OK) then
         write(msg,*) 'gridlib_read_bgrid_header: Error in &
                      &fio_read_header_sections',err
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      if (filetype /= deffiletype) then
         call pjar_delete(pjar)
         call dio_close(dio)
         call fio_delete(fio)
         write(msg,*) 'gridlib_read_bgrid_header: Filetype should be ', &
                      deffiletype,', not ', filetype
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      call pjar_choose_section(pjar, defsecname)

      ! Get parameters from header
      call pjar_get(pjar, 'version',       version,       CNIL)
      if (version /= '1.0') then
         write(msg,*) 'gridlib_read_bgrid_header: Expected version=1.0, &
                      &but version = ', version
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'xorg',          xorg,          dble(0.0))
      call pjar_get(pjar, 'yorg',          yorg,          dble(0.0))
      call pjar_get(pjar, 'nx',            nx,            1)
      call pjar_get(pjar, 'ny',            ny,            1)
      call pjar_get(pjar, 'dx',            dx,            1.0)
      call pjar_get(pjar, 'dy',            dy,            1.0)
      call pjar_get(pjar, 'angle',         angle,         0.0)
      call pjar_get(pjar, 'znon',          znon,          FNIL)
      call pjar_get(pjar, 'description',   description,   CNIL)
      call pjar_get(pjar, 'attribute',     attribute,     CNIL)
      call pjar_get(pjar, 'cunits',        cunits,        CNIL)
      call pjar_get(pjar, 'punits',        punits,        CNIL)
      call pjar_get(pjar, 'data_file',     data_file,     'NONE')
      if (data_file /= 'NONE') then
         write(msg,*) 'gridlib_read_bgrid_header: Cannot handle data_file &
                      &/= NONE'
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'storage_order', storage_order, 'XY')
      call pjar_get(pjar, 'encoding', encoding)
      if (encoding /= FIO_BINARY) then
         write(msg,*) 'gridlib_read_bgrid_header: Encoding must be FIO_BINARY, &
                      &not ',encoding
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'maxpicks', ngrid)
      if (ngrid /= nx*ny) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_read_bgrid_header: Number of &
                                      &data points (', ngrid, &
                                      ') must = nx*ny (',nx*ny, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Read binary data
      column_grid = pjar_find(pjar, 'fields', 'grid')
      call fio_read_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if
      call fio_before_read_binary(fio, ngriddata)
      if (ngriddata /= ngrid) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_read_bgrid_header: &
                                      &Inconsistency in &ngrid between &
                                      &header (', ngrid,') and data (', &
                                      ngriddata, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Check storage order
      if (storage_order /= 'XY') then
         write(msg,'(A,A,A)') 'gridlib_read_bgrid_header: Cannot handle &
                              &storage order = ', storage_order, &
                              ', must be = XY'
         err = GRIDLIB_ERROR
         return
      end if

      ! Clean up
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return
      end subroutine gridlib_read_bgrid_header


!********************************************************
!     subroutine to read in header info from ascii grid on disk.

      subroutine gridlib_read_agrid_header(sounit,iunit,filein, &
                     nx,ny,xorg,yorg,dx,dy,angle,znon,errRetVal)

      implicit none

      integer,          intent(in)    :: sounit, iunit
      character(len=*), intent(in)    :: filein
      integer,          intent(inout) :: nx, ny
      double precision, intent(inout) :: xorg, yorg
      real,             intent(inout) :: dx, dy, angle
      real,             intent(inout) :: znon
      integer,          intent(out)   :: errRetVal

!---Local declarations
      integer           :: errstatus, rstat
      character(len=80) :: record
      real              :: anglein, znonin

!---Set default return error status
      errRetVal = 0

      anglein = -9999.0
      znonin  = 1.0

!---Open grid file
      open(unit=iunit,file=filein,status='old',iostat=errstatus)
      if (errstatus /= 0) then
         write(sounit,*)' '
         write(sounit,*)' *** ERROR *** Cannot read file:',filein
         write(sounit,*)' '
         errRetVal = 1
         return
      end if

!---Read grid parameters
      read(iunit,*) xorg,yorg
      read(iunit,*) dx,dy
      read(iunit,*) nx,ny
      read(iunit,'(A80)') record
      read(record,*,iostat=rstat) anglein,znonin
      if (rstat /= 0) then
         if (anglein == -9999.0) anglein = 0.0
         if (znonin  == 1.0)     znonin  = znon
      end if
      angle = anglein
      znon  = znonin

!---Close input file and exit
      close(unit=iunit)

      return

      end subroutine gridlib_read_agrid_header


!********************************************************
!     subroutine to read in a grid array (ascii or binary format) from disk.

      subroutine gridlib_read_abgrid(sounit, iunit, filein, grid, ngridmax,   &
                                     nx, ny, xorg, yorg, dx, dy, angle, znon, &
                                     reset_znon, description, attribute,      &
                                     cunits, punits, errRetVal)

      implicit none

      integer,          intent(in)    :: sounit, iunit
      character(len=*), intent(in)    :: filein
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: grid(ngridmax)
      integer,          intent(inout) :: nx, ny
      double precision, intent(inout) :: xorg, yorg
      real,             intent(inout) :: dx, dy, angle
      real,             intent(inout) :: znon
      logical,          intent(in)    :: reset_znon
      character(len=*), intent(inout) :: description, attribute
      character(len=*), intent(inout) :: cunits, punits
      integer,          intent(out)   :: errRetVal

!---Local declarations

      integer          :: ip
      real             :: grid_znon
      real             :: gridmin, gridmax
      character(len=256) :: msg

!---Set default return error status
      errRetVal = 0

!---Read the grid file
      call gridlib_read_bgrid(filein, xorg, yorg, nx, ny, dx, dy, angle, &
                              grid_znon, ngridmax, grid, description,    &
                              attribute, cunits, punits, errRetVal, msg)
      if (errRetVal == GRIDLIB_BGRID_NOTFOUND) then
         ! File wasn't a binary grid, try to read an ascii grid
         grid_znon = znon
         call gridlib_read_agrid(sounit, iunit, filein, grid, ngridmax,   &
                                 nx, ny, xorg, yorg, dx, dy, angle, &
                                 grid_znon, errRetVal)
         if (errRetVal /= GRIDLIB_OK) then
            write(sounit,*) 'gridlib_read_abgrid: error in gridlib_read_agrid'
            return
         end if
         description = ''
         attribute   = ''
         cunits   = ''
         punits   = ''
      else if (errRetVal /= GRIDLIB_OK) then
         write(sounit,*) msg
         return
      end if

!---Reset znon in grid if appropriate
      if (reset_znon) then
         do ip=1,nx*ny
            if (grid(ip) == grid_znon) then
               grid(ip) = znon
            end if
         end do
         grid_znon = znon
      else
         znon = grid_znon
      end if

!---Print out important grid information
      if (gridlib_verbose > 0) then
         call gridlib_get_min_max(grid, ngridmax, nx, ny, znon, &
                                     gridmin, gridmax, errRetVal)
         write(sounit,*)' '
         write(sounit,*)'*** READ FILE *** '
         write(sounit,*)trim(filein)
         write(sounit,*)' XORG    = ',xorg, '  YORG    = ',yorg
         write(sounit,*)' DX      = ',dx,   '  DY      = ',dy
         write(sounit,*)' NX      = ',nx,   '  NY      = ',ny
         write(sounit,*)' ANGLE   = ',angle,'  ZNON    = ',znon
         write(sounit,*)' GRIDMIN = ',gridmin,'  GRIDMAX = ',gridmax
         write(sounit,*)' '
      end if

      return

      end subroutine gridlib_read_abgrid


!********************************************************
!     subroutine to read in an ascii grid array from disk.

      subroutine gridlib_read_agrid(sounit,iunit,filein,grid,ngridmax, &
                     nx,ny,xorg,yorg,dx,dy,angle,znon,errRetVal)

      implicit none

      integer,          intent(in)    :: sounit, iunit
      character(len=*), intent(in)    :: filein
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: grid(ngridmax)
      integer,          intent(inout) :: nx, ny
      double precision, intent(inout) :: xorg, yorg
      real,             intent(inout) :: dx, dy, angle
      real,             intent(inout) :: znon
      integer,          intent(out)   :: errRetVal

!---Local declarations
      integer           :: errstatus, rstat
      character(len=80) :: record
      real              :: anglein, znonin

!---Set default return error status
      errRetVal = 0

      anglein = -9999.0
      znonin  = 1.0

!---Open grid file
      open(unit=iunit,file=filein,status='old',iostat=errstatus)
      if (errstatus /= 0) then
         write(sounit,*)' '
         write(sounit,*)' *** ERROR *** Cannot read file:',filein
         write(sounit,*)' '
         errRetVal = 1
         return
      end if

!---Read grid parameters
      read(iunit,*) xorg,yorg
      read(iunit,*) dx,dy
      read(iunit,*) nx,ny
      read(iunit,'(A80)') record
      read(record,*,iostat=rstat) anglein,znonin
      if (rstat /= 0) then
         if (anglein == -9999.0) anglein = 0.0
         if (znonin  == 1.0)     znonin  = znon
      end if
      angle = anglein
      znon  = znonin

!---Check to see that nx and ny are not too big
      if (nx*ny > ngridmax) then
         write(sounit,*)' '
         write(sounit,*)' *** ERROR *** In "GRIDLIB_READ_AGRID" ngridmax &
                        &too small'
         write(sounit,*)'               ngridmax=',ngridmax
         write(sounit,*)'               nx=',nx,' ny=',ny
         write(sounit,*)'               dx=',dx,' dy=',dy
         write(sounit,*)'               xorg=',xorg,' yorg=',yorg
         write(sounit,*)'               angle=',angle
         write(sounit,*)'               FILE=',filein
         write(sounit,*)' '
         errRetVal = 1
         return
      end if

!---Now read data
      call gridlib_read_agrid_data(sounit,iunit,nx,ny,grid)

!---Close input file and exit
      close(unit=iunit)

      return

      end subroutine gridlib_read_agrid


!********************************************************

      subroutine gridlib_verify_headers_match(nxin,nx,nyin,ny,         &
                                              dxin,dx,dyin,dy,         &
                                              xorgin,xorg,yorgin,yorg, &
                                              anglein,angle,           &
                                              unitsin,units,match)

      implicit none

      integer,          intent(in)    :: nxin, nx
      integer,          intent(in)    :: nyin, ny
      real,             intent(in)    :: dxin, dx
      real,             intent(in)    :: dyin, dy
      double precision, intent(in)    :: xorg, yorg, xorgin, yorgin
      real,             intent(in)    :: anglein, angle
      character(len=*), intent(in)    :: unitsin, units
      integer,          intent(inout) :: match

!---Local declarations
      double precision :: xorg_diff, yorg_diff
      real             :: dx_diff, dy_diff
      real             :: ang_diff
      real             :: e1

      match=0   ! grid headers don't match
      e1=0.01

      dx_diff=abs(dxin-dx)
      dy_diff=abs(dyin-dy)
      xorg_diff=abs(xorgin-xorg)
      yorg_diff=abs(yorgin-yorg)
      ang_diff=abs(anglein-angle)

      if(nxin == nx      .and. nyin == ny      .and.  &
         dx_diff < e1   .and. dy_diff < e1   .and.    &
         xorg_diff < e1 .and. yorg_diff < e1 .and.    &
         ang_diff < e1  .and.                         &
         unitsin == units) match=1

      return
      end subroutine gridlib_verify_headers_match


!********************************************************
!     NEW version by pav

      subroutine gridlib_read_agrid_data(sounit, iunit,nx,ny,grid)

      implicit none

      integer,   intent(in)    :: sounit, iunit, nx, ny
      real,      intent(inout) :: grid(nx,ny)

!---Local declarations
      integer    :: nvalues_line, nchar_value, nylines
      integer    :: ix, icount, istart, iend, iyl, i, iy
      character(len=80) :: cbuf

      nvalues_line=8
      nchar_value=10

      nylines=ny/nvalues_line
      if(nylines*nvalues_line < ny) nylines=nylines+1

      do ix=1,nx
         icount=0
         do iyl=1,nylines
            read(iunit,'(a80)',end=1005) cbuf
            do i=1,nvalues_line
               iy=ny-icount
               icount=icount+1
               istart=(i-1)*nchar_value+1
               iend=istart+nchar_value-1
               if(iy >= 1) read(cbuf(istart:iend),*,end=1005) grid(ix,iy)
            enddo
         enddo
      enddo
      goto 1006

1005  write(sounit,*) 'EOF in grid file at ix = ',ix,' iy = ',icount
1006  continue
      return
      end subroutine gridlib_read_agrid_data


!********************************************************

      subroutine gridlib_write_agrid(iunit,fileout,grid,nxdim,nydim, &
                                    nx,ny,xorg,yorg,deltax,deltay,   &
                                    angle,znon,ier)

      implicit none

      integer,          intent(in)    :: iunit
      character(len=*), intent(in)    :: fileout
      integer,          intent(in)    :: nxdim, nydim, nx, ny
      real,             intent(in)    :: grid(nxdim,nydim)
      double precision, intent(in)    :: xorg, yorg
      real,             intent(in)    :: deltax, deltay, angle, znon
      integer,          intent(inout) :: ier

!---Local declarations
      character(len=8) :: fmt
      integer          :: ix, iy, errstatus
      real             :: gmax

      ier=0

!---find grid max
      gmax=0.0
      do ix=1,nx
      do iy=1,ny
         if(abs(grid(ix,iy)) > gmax) gmax=abs(grid(ix,iy))
      end do
      end do

      fmt='(8f10.0)'
      if(gmax <  100000.0) fmt='(8f10.1)'
      if(gmax <   10000.0) fmt='(8f10.2)'
      if(gmax <    1000.0) fmt='(8f10.3)'
      if(gmax <     100.0) fmt='(8f10.4)'
      if(gmax <      10.0) fmt='(8f10.5)'
      if(gmax <       1.0) fmt='(8f10.6)'


!---Open grid file
      open(unit=iunit,file=fileout,status='unknown',iostat=errstatus)
      if (errstatus /= 0) then
         ier=-1
         return
      end if
      write(iunit,*,iostat=errstatus) xorg,yorg
      if (errstatus /= 0) then
         ier=-1
         return
      end if
      write(iunit,*,iostat=errstatus) deltax,deltay
      if (errstatus /= 0) then
         ier=-1
         return
      end if
      write(iunit,*,iostat=errstatus) nx,ny
      if (errstatus /= 0) then
         ier=-1
         return
      end if
      write(iunit,*,iostat=errstatus) angle,znon
      if (errstatus /= 0) then
         ier=-1
         return
      end if
      do ix=1,nx
         write(iunit,fmt,iostat=errstatus) (grid(ix,iy),iy=ny,1,-1)
         if (errstatus /= 0) then
            ier=-1
            return
         end if
      end do
      close(unit=iunit)
      return

      end subroutine gridlib_write_agrid


!********************************************************

      subroutine gridlib_write_grid_fmt(iunit,fileout,grid,nxdim,nydim,  &
                                        nx,ny,xorg,yorg,deltax,deltay,   &
                                        angle,znon,ier)

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: fileout
      integer,            intent(in)    :: nxdim, nydim, nx, ny
      real,               intent(in)    :: grid(nxdim,nydim)
      double precision,   intent(in)    :: xorg, yorg
      real,               intent(in)    :: deltax, deltay, angle
      real,               intent(in)    :: znon
      integer,            intent(inout) :: ier

!---Local declarations
      character (len=60) :: fmt1
      character (len=10) :: fmt0
      integer            :: ix, iy, iy0, nlines, iline, iost

      ier=0

!---Open grid file
      open(iunit,file=fileout,status='unknown',iostat=iost)
      if (iost/=0) then
        ier=-1
        return
      end if

      write(iunit,*) xorg,yorg
      write(iunit,*) deltax,deltay
      write(iunit,*) nx,ny
      write(iunit,*) angle,znon

      nlines = ny/8
      do ix=1,nx

        !following format of length 49
        fmt1='(f10.4,f10.4,f10.4,f10.4,f10.4,f10.4,f10.4,f10.4)'
        do iline=1,nlines  !whole lines
          iy0 = 0
          do iy=ny-(iline-1)*8,ny-iline*8+1,-1
            call gridlib_get_write_fmt(grid(ix,iy),fmt0)
            write(fmt1(6*iy0+2:6*iy0+6),'(A)')fmt0(1:5)
            iy0 = iy0+1
          end do

          write(iunit,fmt1) (grid(ix,iy),iy=ny-(iline-1)*8,ny-iline*8+1,-1)
        end do  !next iline

        if (ny-nlines*8>0) then
          iy0 = 0
          do iy=ny-nlines*8,1,-1
            call gridlib_get_write_fmt(grid(ix,iy),fmt0)
            write(fmt1(6*iy0+2:6*iy0+6),'(A)')fmt0(1:5)
            iy0 = iy0+1
          end do

          write(iunit,fmt1) (grid(ix,iy),iy=ny-nlines*8,1,-1)
        end if
      end do
      close(iunit)

      return
      end subroutine gridlib_write_grid_fmt


!********************************************************

      subroutine gridlib_get_write_fmt(value,fmt0)

      implicit none

      real,              intent(in)    :: value
      character(len=*),  intent(inout) :: fmt0

!---Local declarations
      real              :: absvalue

      if (value>=0.0) then
        absvalue = value
      else
        absvalue = -value
      end if

      if (absvalue>100000.0) then
        fmt0(1:5) = 'e10.3'
      else if (absvalue>10000.0) then
        fmt0(1:5) = 'f10.0'
      else if (absvalue>1000.0) then
        fmt0(1:5) = 'f10.1'
      else if (absvalue>100.0) then
        fmt0(1:5) = 'f10.2'
      else if (absvalue>10.0) then
        fmt0(1:5) = 'f10.3'
      else if (absvalue>1.0) then
        fmt0(1:5) = 'f10.4'
      else
        fmt0(1:5) = 'f10.5'
      end if

      return
      end subroutine gridlib_get_write_fmt



      !
      ! gridlib_write_bgrid - write a binary grid to a file
      !
      subroutine gridlib_write_bgrid(filename, xorg, yorg, nx, ny, dx, dy,   &
                                     angle, znon, nxdim, nydim, grid,        &
                                     description, attribute, cunits, punits, &
                                     err, msg)

      implicit none

      ! Subroutine arguments
      character(len=*), intent(in)  :: filename
      double precision, intent(in)  :: xorg, yorg
      integer,          intent(in)  :: nx,   ny
      real,             intent(in)  :: dx,   dy
      real,             intent(in)  :: angle
      real,             intent(in)  :: znon
      integer,          intent(in)  :: nxdim, nydim
      real,             intent(in)  :: grid(nxdim, nydim)
      character(len=*), intent(in)  :: description
      character(len=*), intent(in)  :: attribute
      character(len=*), intent(in)  :: cunits, punits
      integer,          intent(out) :: err
      character(len=*), intent(out) :: msg

      ! Internal variables
      type(fio_struct),  pointer    :: fio
      type(dio_struct),  pointer    :: dio
      type(pjar_struct), pointer    :: pjar
      integer                       :: ngdata
      real, allocatable             :: gdata(:)
      integer                       ::    ix, iy 
      integer                       :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Copy grid data to 1-d array with x as fastest dimension
      ngdata = nx*ny
      allocate(gdata(ngdata), stat=err)
      if (err /= 0) then
         write(msg,*) 'gridlib_write_bgrid: Error allocating gdata, err = ', err
         err = GRIDLIB_ERROR
         return
      end if
      do iy=1,ny
         do ix=1,nx
            gdata(ix + (iy-1)*nx) = grid(ix, iy)
         end do
      end do

      ! Define the header parameters
      call pjar_create(pjar)
      call pjar_choose_section(pjar, defsecname)

      call pjar_put(pjar, 'version',       '1.0')
      ! dio should always write LITTLE ENDIAN, according to swap_module;
      ! fio will provide function to determine endianness in the future
      call pjar_put(pjar, 'endian',        'LITTLE_ENDIAN')
      call pjar_put(pjar, 'xorg',          xorg)
      call pjar_put(pjar, 'yorg',          yorg)
      call pjar_put(pjar, 'nx',            nx)
      call pjar_put(pjar, 'ny',            ny)
      call pjar_put(pjar, 'dx',            dx)
      call pjar_put(pjar, 'dy',            dy)
      call pjar_put(pjar, 'angle',         angle)
      call pjar_put(pjar, 'znon',          znon)
      call pjar_put(pjar, 'description',   description)
      call pjar_put(pjar, 'attribute',     attribute)
      call pjar_put(pjar, 'data_file',     'NONE')
      call pjar_put(pjar, 'storage_order', 'XY')

      call pjar_put(pjar, 'encoding', FIO_BINARY)
      call pjar_put(pjar, 'fields',   (/'grid'/), 1)
      call pjar_put(pjar, 'vartypes', (/'F'/), 1)
      call pjar_put(pjar, 'cunits',   cunits)
      call pjar_put(pjar, 'punits',   punits)
      if (cunits(1:1) == 'F' .or. cunits(1:1) == 'f') then
        call pjar_put(pjar, 'units',    (/'feet'/), 1)
      else if (cunits(1:1) == 'M' .or. cunits(1:1) == 'm') then
        call pjar_put(pjar, 'units',    (/'meters'/), 1)
      else
        call pjar_put(pjar, 'units',    (/' '/), 1)
      end if
      call pjar_put(pjar, 'maxpicks', ngdata)

      column_grid = pjar_find(pjar, 'fields', 'grid')

      ! Open output file and write headers
      call dio_open_write(dio, filename, err, msg)
      if (err /= DIO_OK) then
         return
      end if
      call fio_create(fio, dio)
      call fio_write_header_sections(fio, pjar, err, msg, deffiletype)
      if (err /= FIO_OK) then
         return
      end if
      call fio_write_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! Write binary data
      call fio_before_write_binary(fio, ngdata)
      call fio_write_binary       (fio, column_grid, gdata)
      call fio_after_write_binary (fio, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! Clean up
      deallocate(gdata)
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return
      end subroutine gridlib_write_bgrid


      !
      ! gridlib_read_bgrid - read a binary grid from a file
      !
      subroutine gridlib_read_bgrid(filename, xorg, yorg, nx, ny, dx, dy,   &
                                    angle, znon, ngridmax, grid,            &
                                    description, attribute, cunits, punits, &
                                    err, msg)

      implicit none

      ! Subroutine arguments
      character(len=*), intent(in)    :: filename
      double precision, intent(out)   :: xorg, yorg
      integer,          intent(out)   :: nx,   ny
      real,             intent(out)   :: dx,   dy
      real,             intent(out)   :: angle
      real,             intent(out)   :: znon
      integer,          intent(in)    :: ngridmax
      real,             intent(inout) :: grid(ngridmax)
      character(len=*), intent(out)   :: description
      character(len=*), intent(out)   :: attribute
      character(len=*), intent(out)   :: cunits, punits
      integer,          intent(out)   :: err
      character(len=*), intent(out)   :: msg

      ! Internal variables
      type(fio_struct),  pointer    :: fio
      type(dio_struct),  pointer    :: dio
      type(pjar_struct), pointer    :: pjar
      integer                       :: ngrid, ngriddata

      character(len=20)             :: filetype
      character(len=20)             :: version
      character(len=20)             :: storage_order
      character(len=256)            :: data_file
      character(len=256)            :: encoding
      integer                       :: column_grid

      nullify (fio) ! jpa
      nullify (dio) ! jpa
      nullify (pjar) ! jpa

      err = GRIDLIB_OK

      ! Open file and read headers
      call pjar_create(pjar)
      call dio_open_read(dio, filename, err, msg)
      if (err /= DIO_OK) then
         return
      end if
      call fio_create(fio, dio)
      call fio_read_header_sections(fio, pjar, err, msg, filetype)
      if (err /= FIO_OK) then
         write(msg,*) 'gridlib_read_bgrid: Error in fio_read_header_sections',&
                      err
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      if (filetype /= deffiletype) then
         call pjar_delete(pjar)
         call dio_close(dio)
         call fio_delete(fio)
         write(msg,*) 'gridlib_read_bgrid: Filetype should be ', deffiletype, &
                    ', not ', filetype
         err = GRIDLIB_BGRID_NOTFOUND
         return
      end if
      call pjar_choose_section(pjar, defsecname)

      ! Get parameters from header
      call pjar_get(pjar, 'version',       version,       CNIL)
      if (version /= '1.0') then
         write(msg,*) 'gridlib_read_bgrid: Expected version=1.0, but version &
                      &= ', version
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'xorg',          xorg,          dble(0.0))
      call pjar_get(pjar, 'yorg',          yorg,          dble(0.0))
      call pjar_get(pjar, 'nx',            nx,            1)
      call pjar_get(pjar, 'ny',            ny,            1)
      call pjar_get(pjar, 'dx',            dx,            1.0)
      call pjar_get(pjar, 'dy',            dy,            1.0)
      call pjar_get(pjar, 'angle',         angle,         0.0)
      call pjar_get(pjar, 'znon',          znon,          FNIL)
      call pjar_get(pjar, 'description',   description,   CNIL)
      call pjar_get(pjar, 'attribute',     attribute,     CNIL)
      call pjar_get(pjar, 'cunits',        cunits,        CNIL)
      call pjar_get(pjar, 'punits',        punits,        CNIL)
      call pjar_get(pjar, 'data_file',     data_file,     'NONE')
      if (data_file /= 'NONE') then
         write(msg,*) 'gridlib_read_bgrid: Cannot handle data_file != NONE'
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'storage_order', storage_order, 'XY')
      call pjar_get(pjar, 'encoding', encoding)
      if (encoding /= FIO_BINARY) then
         write(msg,*) 'gridlib_read_bgrid: Encoding must be FIO_BINARY, not ', &
                      encoding
         err = GRIDLIB_ERROR
         return
      end if
      call pjar_get(pjar, 'maxpicks', ngrid)
      if (ngrid /= nx*ny) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_read_bgrid: Number of data &
                                      &points (', ngrid, ') must = nx*ny (', &
                                      nx*ny, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Check to see that nx and ny are not bigger than ngridmax
      if (nx*ny > ngridmax) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_read_bgrid: ngridmax (',      &
                                      ngridmax, ') is smaller than nx*ny (', &
                                      nx*ny, ')'
         err = GRIDLIB_ERROR
         return
      end if

      ! Read binary data
      column_grid = pjar_find(pjar, 'fields', 'grid')
      call fio_read_data_section(fio, pjar, defsecname, err, msg)
      if (err /= FIO_OK) then
         return
      end if
      call fio_before_read_binary(fio, ngriddata)
      if (ngriddata /= ngrid) then
         write(msg,'(A,I10,A,I10,A)') 'gridlib_read_bgrid: Inconsistency in &
                                      &ngrid between header (', ngrid,    &
                                      ') and data (', ngriddata, ')'
         err = GRIDLIB_ERROR
         return
      end if
      call fio_read_binary      (fio, column_grid, grid)
      call fio_after_read_binary(fio, err, msg)
      if (err /= FIO_OK) then
         return
      end if

      ! Check storage order
      if (storage_order /= 'XY') then
         write(msg,'(A,A,A)') 'gridlib_read_bgrid: Cannot handle storage &
                              &order = ', storage_order, ', must be = XY'
         err = GRIDLIB_ERROR
         return
      end if

      ! Clean up
      call pjar_delete(pjar)
      call dio_close(dio)
      call fio_delete(fio)

      return
      end subroutine gridlib_read_bgrid



!********************************************************
!     subroutine to find the min and max grid values, ignoring ZNONs

      subroutine gridlib_get_min_max(grid, ngridmax, nx, ny, znon, &
                                     gridmin, gridmax, errRetVal)

      implicit none

      integer,          intent(in)    :: ngridmax
      real,             intent(in)    :: grid(ngridmax)
      integer,          intent(in)    :: nx, ny
      real,             intent(in)    :: znon
      real,             intent(out)   :: gridmin, gridmax
      integer,          intent(out)   :: errRetVal

!---Local declarations
      real             :: big
      integer          :: iflag, ip

!---Set default return error status
      errRetVal = 0

!---Find the min and max values, ignoring znons.
      big=999999999.
      iflag=0
      do ip=1,nx*ny
         if (grid(ip) /= znon) then
            if (iflag == 0) then
               gridmin=big
               gridmax=-big
               iflag=1
            end if
            if (grid(ip) < gridmin) gridmin=grid(ip)
            if (grid(ip) > gridmax) gridmax=grid(ip)
         end if
      end do

      return

      end subroutine gridlib_get_min_max


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

      subroutine gridlib_convert_units(iflag,gnull,unitsin,unitsout,    &
                                       grid,nx,ny,dx,dy,xorg,yorg,angle)

      implicit none

      integer,          intent(in)    :: iflag
      real,             intent(in)    :: gnull
      character(len=*), intent(in)    :: unitsin, unitsout
      integer,          intent(in)    :: nx, ny
      real,             intent(inout) :: grid(nx,ny)
      real,             intent(inout) :: dx, dy
      double precision, intent(inout) :: xorg,yorg
      real,             intent(in)    :: angle

!---Local declarations
      integer          :: iflagin, iflagout, ix, iy
      real             :: scale

      iflagin=1
      if(unitsin == 'm' .or. unitsin == 'M') iflagin =2
      iflagout=1
      if(unitsout == 'm' .or. unitsout == 'M') iflagout=2

      if(iflagin == iflagout) return
      if(iflagin == 1 .and. iflagout == 2) scale=    0.3048
      if(iflagin == 2 .and. iflagout == 1) scale=1.0/0.3048

      if(iflag /= 0) then
         do ix=1,nx
         do iy=1,ny
            if(grid(ix,iy) /= gnull) grid(ix,iy)=grid(ix,iy)*scale
         end do
         end do
      end if

      xorg=xorg*scale
      yorg=yorg*scale
      dx=dx*scale
      dy=dy*scale


      return
      end subroutine gridlib_convert_units


!********************************************************
!---PROGRAM NAME:         convert_grid
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

      subroutine gridlib_convert_grid(sounit,iflag,gnull,             &
                     grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1, &
                     grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2, &
                     no_extrap)

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

!---Local declarations
      real, parameter  :: e1=0.01
      double precision :: xf,yf
      double precision :: dxo, dyo, dxg, dyg
      real             :: rad1, rad2
      integer          :: ix1, iy1, ix2, iy2
      real             :: xp, yp, xx, yy, xndx, yndx
      real             :: g1, g2, g11, g21, g12, g22
      real             :: s, u, w

!---change units if necessary
      call gridlib_convert_units(iflag,gnull,units1,units2, &
               grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1)

!---check if new grid is subarea of old grid
      if (abs(dx2-dx1) < e1 .and. abs(dy2-dy1) < e1 .and.  &
          abs(angle2-angle1) < e1) then
         rad1 = angle1/180.*3.141592654
         dxo = xorg2-xorg1
         dyo = yorg2-yorg1
         dxg =  dxo*cos(rad1) + dyo*sin(rad1)
         dyg = -dxo*sin(rad1) + dyo*cos(rad1)
         ix1 = nint(dxg/dx1+1.0)
         iy1 = nint(dyg/dy1+1.0)
         if (ix1 > 0 .and. iy1 > 0 .and.  &
             abs((ix1-1)*dx1-dxg) < e1 .and.  &
             abs((iy1-1)*dy1-dyg) < e1 .and.  &
             (ix1+nx2-1) <= nx1 .and. (iy1+ny2-1) <= ny1) then
            do ix2=1,nx2
            do iy2=1,ny2
               grid2(ix2,iy2)=grid1(ix1+ix2-1,iy1+iy2-1)
            end do
            end do
            goto 999
         end if
      end if

!---resample grid
      rad1 = angle1/180.*3.141592654
      rad2 = angle2/180.*3.141592654
      do ix2=1,nx2
      do iy2=1,ny2
         xp = (ix2-1)*dx2
         yp = (iy2-1)*dy2
         xf = xp*cos(rad2) - yp*sin(rad2) + xorg2
         yf = xp*sin(rad2) + yp*cos(rad2) + yorg2

         xx=xf-xorg1
         yy=yf-yorg1

         xndx =  xx*cos(rad1) + yy*sin(rad1)
         yndx = -xx*sin(rad1) + yy*cos(rad1)
         xndx=xndx/dx1+1.0
         yndx=yndx/dy1+1.0
         ix1=xndx
         iy1=yndx

!         -----------------------------
!        |       |              |       |
!        | case1 |    case 7    | case4 |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        |       |              |       |
!        | case3 |    case 9    | case6 |
!        |       |              |       |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        | case2 |    case 8    | case5 |
!        |       |              |       |
!         ------------------------------
         if (no_extrap == 1 .and. (xndx < 1 .or. xndx > nx1 .or.  &
                                   yndx < 1 .or. yndx > ny1)) then
            grid2(ix2,iy2)=gnull
         else if(ix1 < 1 ) then
            if(iy1 < 1)then
! case 1
               grid2(ix2,iy2)=grid1(1,1)
            else if (iy1 >= ny1) then
! case 2
               grid2(ix2,iy2)=grid1(1,ny1)
            else
! case 3
               g1 = grid1(1,iy1)
               g2 = grid1(1,iy1+1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear y-interp
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               elseif((yndx-iy1) > 0.5) then           !nearest neighbor
                  grid2(ix2,iy2)=g2
               else
                  grid2(ix2,iy2)=g1
               endif
            end if
         else if(ix1 >= nx1) then
            if(iy1 < 1)then
! case 4
               grid2(ix2,iy2)=grid1(nx1,1)
            else if (iy1 >= ny1) then
! case 5
               grid2(ix2,iy2)=grid1(nx1,ny1)
            else
! case 6
               g1 = grid1(nx1,iy1)
               g2 = grid1(nx1,iy1+1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear y-interp
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               elseif((yndx-iy1) > 0.5) then           !nearest neighbor
                  grid2(ix2,iy2)=g2
               else
                  grid2(ix2,iy2)=g1
               endif
            end if

         else if(iy1 < 1 .and.  &
                (ix1 >= 1 .and. ix1 < nx1) ) then
! case 7
               g1 = grid1(ix1,1)
               g2 = grid1(ix1+1,1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear x-interp
                  grid2(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
               elseif((xndx-ix1) > 0.5) then           !nearest neighbor
                  grid2(ix2,iy2)=g2
               else
                  grid2(ix2,iy2)=g1
               endif
         else if(iy1 >= ny1 .and.  &
                (ix1 >= 1 .and. ix1 < nx1) ) then
! case 8
               g1 = grid1(ix1,ny1)
               g2 = grid1(ix1+1,ny1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull) then   !linear x-interp
                  grid2(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
               elseif((xndx-ix1) > 0.5) then           !nearest neighbor
                  grid2(ix2,iy2)=g2
               else
                  grid2(ix2,iy2)=g1
               endif
! case 9
!     interpolate along y's first
         else if(ix1 >= 1 .and. ix1 < nx1 .and.  &
                 iy1 >= 1 .and. iy1 < ny1 ) then
               grid2(ix2,iy2)=gnull
               g11 = grid1(ix1  ,iy1  )
               g21 = grid1(ix1+1,iy1  )
               g12 = grid1(ix1  ,iy1+1)
               g22 = grid1(ix1+1,iy1+1)
               if(g11 /= gnull .and. g21 /= gnull .and. &    !bi-linear interp
                  g12 /= gnull .and. g22 /= gnull) then
                  g1 = g11+(g21-g11)*(xndx-ix1)
                  g2 = g12+(g22-g12)*(xndx-ix1)
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               else if ((xndx-ix1) > 0.5 .and. (yndx-iy1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1+1,iy1+1)
                  if (g22 == gnull) then
                     grid2(ix2,iy2)=gnull
                  elseif(g22 /= gnull .and. g21 /= gnull .and.  &
                     g12 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     grid2(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g22 /= gnull .and. g12 /= gnull .and.  &
                     g11 /= gnull .and. (xndx-ix1) <= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     grid2(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g22 /= gnull .and. g21 /= gnull .and.  &
                     g11 /= gnull .and. (xndx-ix1) >= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     grid2(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g22 /= gnull .and. g11 /= gnull .and.  &
                     (xndx-ix1) >= (yndx-iy1)) then
                     !modified tri-linear interp
                     w=ix1+1-xndx
                     grid2(ix2,iy2)=w*(g11-g22)+g22
                  elseif(g22 /= gnull .and. g11 /= gnull .and.  &
                     (xndx-ix1) < (yndx-iy1)) then
                     !modified tri-linear interp
                     u=iy1+1-yndx
                     grid2(ix2,iy2)=u*(g11-g22)+g22
                  elseif(g22 /= gnull .and. g12 /= gnull) then
                     !linear x-interp
                     grid2(ix2,iy2)=(g22-g12)*(xndx-ix1)+g12
                  elseif(g22 /= gnull .and. g21 /= gnull) then
                     !linear y-interp
                     grid2(ix2,iy2)=(g22-g21)*(yndx-iy1)+g21
                  else
                     !nearest neighbor
                     grid2(ix2,iy2)=g22
                  end if
               else if ((xndx-ix1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1+1,iy1)
                  if (g21 == gnull) then
                     grid2(ix2,iy2)=gnull
                  elseif(g21 /= gnull .and. g11 /= gnull .and.  &
                     g22 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     grid2(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g21 /= gnull .and. g11 /= gnull .and.  &
                     g12 /= gnull .and. (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     grid2(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     grid2(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     u=ix1+1-xndx
                     grid2(ix2,iy2)=u*(g12-g21)+g21
                  elseif(g21 /= gnull .and. g12 /= gnull .and.  &
                     (xndx-ix1) < (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     w=yndx-iy1
                     grid2(ix2,iy2)=w*(g12-g21)+g21
                  elseif(g21 /= gnull .and. g11 /= gnull) then
                     !linear x-interp
                     grid2(ix2,iy2)=(g21-g11)*(xndx-ix1)+g11
                  elseif(g21 /= gnull .and. g22 /= gnull) then
                     !linear y-interp
                     grid2(ix2,iy2)=(g22-g21)*(yndx-iy1)+g21
                  else
                     !nearest neighbor
                     grid2(ix2,iy2)=g21
                  end if
               else if ((yndx-iy1) > 0.5) then
                  ! (xndx,yndx) closest to (ix1,  iy1+1)
                  if (g12 == gnull) then
                     grid2(ix2,iy2)=gnull
                  elseif(g12 /= gnull .and. g11 /= gnull .and.  &
                     g22 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     grid2(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=ix1+1-xndx
                     w=iy1+1-yndx
                     grid2(ix2,iy2)=s*(u*(g12-g22)+w*(g21-g22))+g22
                  elseif(g12 /= gnull .and. g11 /= gnull .and.  &
                     g21 /= gnull .and. (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     grid2(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     (xndx-ix1) <= (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     u=xndx-ix1
                     grid2(ix2,iy2)=u*(g21-g12)+g12
                  elseif(g12 /= gnull .and. g21 /= gnull .and.  &
                     (xndx-ix1) > (1.0-(yndx-iy1))) then
                     !modified tri-linear interp
                     w=iy1+1-yndx
                     grid2(ix2,iy2)=w*(g21-g12)+g12
                  elseif(g12 /= gnull .and. g22 /= gnull) then
                     !linear x-interp
                     grid2(ix2,iy2)=(g22-g12)*(xndx-ix1)+g12
                  elseif(g12 /= gnull .and. g11 /= gnull) then
                     !linear y-interp
                     grid2(ix2,iy2)=(g12-g11)*(yndx-iy1)+g11
                  else
                     !nearest neighbor
                     grid2(ix2,iy2)=g12
                  end if
               else
                  ! (xndx,yndx) closest to (ix1,  iy1)
                  if (g11 == gnull) then
                     grid2(ix2,iy2)=gnull
                  elseif(g11 /= gnull .and. g21 /= gnull .and.  &
                     g12 /= gnull) then
                     !tri-linear interp
                     s=1.0
                     u=xndx-ix1
                     w=yndx-iy1
                     grid2(ix2,iy2)=s*(u*(g21-g11)+w*(g12-g11))+g11
                  elseif(g11 /= gnull .and. g21 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) >= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=yndx-iy1
                     w=ix1+1-xndx
                     grid2(ix2,iy2)=s*(u*(g22-g21)+w*(g11-g21))+g21
                  elseif(g11 /= gnull .and. g12 /= gnull .and.  &
                     g22 /= gnull .and. (xndx-ix1) <= (yndx-iy1)) then
                     !tri-linear interp
                     s=1.0
                     u=iy1+1-yndx
                     w=xndx-ix1
                     grid2(ix2,iy2)=s*(u*(g11-g12)+w*(g22-g12))+g12
                  elseif(g11 /= gnull .and. g22 /= gnull .and.  &
                     (xndx-ix1) <= (yndx-iy1)) then
                     !modified tri-linear interp
                     w=xndx-ix1
                     grid2(ix2,iy2)=w*(g22-g11)+g11
                  elseif(g11 /= gnull .and. g22 /= gnull .and.  &
                     (xndx-ix1) > (yndx-iy1)) then
                     !modified tri-linear interp
                     u=yndx-iy1
                     grid2(ix2,iy2)=u*(g22-g11)+g11
                  elseif(g11 /= gnull .and. g21 /= gnull) then
                     !linear x-interp
                     grid2(ix2,iy2)=(g21-g11)*(xndx-ix1)+g11
                  elseif(g11 /= gnull .and. g12 /= gnull) then
                     !linear y-interp
                     grid2(ix2,iy2)=(g12-g11)*(yndx-iy1)+g11
                  else
                     !nearest neighbor
                     grid2(ix2,iy2)=g11
                  end if
               end if
! case '10' - just in case we missed a case
         else
               grid2(ix2,iy2)=gnull
         end if

      end do
      end do

999   continue

      write(sounit,*)' '
      write(sounit,*)' gridlib_convert_grid: xorg1, yorg1=',xorg1,yorg1
      write(sounit,*)' gridlib_convert_grid: xorg2, yorg2=',xorg2,yorg2
      write(sounit,*)' '

      return
      end subroutine gridlib_convert_grid


!********************************************************
!---PROGRAM NAME:         gridlib_read_def_model_parms
      subroutine gridlib_read_def_model_parms(iunit,sounit,filespec,nlaydef, &
                                              xorgdef,yorgdef,dxdef,dydef,   &
                                              nxdef,nydef,angledef,unitsdef, &
                                              nzdef,dzdef,znon,a1,a2,a3,     &
                                              b1,b2,b3,cps_hdra,cps_hdrb,    &
                                              domain,ierr)

      implicit none

      integer,          intent(in)  :: iunit
      integer,          intent(in)  :: sounit
      character(len=*), intent(in)  :: filespec
      integer,          intent(out) :: nlaydef
      double precision, intent(out) :: xorgdef,yorgdef
      real,             intent(out) :: dxdef, dydef
      integer,          intent(out) :: nxdef, nydef
      real,             intent(out) :: angledef
      character(len=*), intent(out) :: unitsdef
      integer,          intent(out) :: nzdef
      real,             intent(out) :: dzdef
      real,             intent(out) :: znon
      integer,          intent(out) :: a1,a2,a3
      integer,          intent(out) :: b1,b2,b3
      integer,          intent(out) :: cps_hdra,cps_hdrb
      character(len=GRIDLIB_DOMAIN_LENGTH), intent(inout) :: domain
      integer,          intent(out) :: ierr

      ! Local declarations
      integer :: nfatal, nwarn, ninform
      integer, parameter :: nbufchar = 50000
      character(len=nbufchar) :: workc
      type(gridlib_modspecheader_struct) :: h

      ierr = 0

      ! get "extended" grid parms
      nfatal  = 0
      nwarn   = 0
      ninform = 0
      call gridlib_decode(iunit, sounit, nfatal, nwarn, ninform, &
                          workc, nbufchar, filespec, h)
      if (nfatal /= 0) then
         ierr = 1
         return
      end if
      nzdef    = h%nzdef
      dzdef    = h%dzdef
      znon     = h%znon
      a1       = h%a1
      a2       = h%a2
      a3       = h%a3
      b1       = h%b1
      b2       = h%b2
      b3       = h%b3
      cps_hdra = h%cps_hdra
      cps_hdrb = h%cps_hdrb
      domain   = h%domain

      ! get default grid parms
      call gridlib_read_deflt_model_parms(iunit, filespec, h%nlay, &
                                          h%xorg, h%yorg, h%dx, h%dy, &
                                          h%nx, h%ny, h%angle, h%units)
      nlaydef  = h%nlay
      xorgdef  = h%xorg
      yorgdef  = h%yorg
      dxdef    = h%dx
      dydef    = h%dy
      nxdef    = h%nx
      nydef    = h%ny
      angledef = h%angle
      unitsdef = h%units

      return

      end subroutine gridlib_read_def_model_parms


!********************************************************
!---PROGRAM NAME:         gridlib_read_deflt_model_parms
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

      subroutine gridlib_read_deflt_model_parms(iunit,filespec,nlaydef,      &
                                                xorgdef,yorgdef,dxdef,dydef, &
                                                nxdef,nydef,angledef,unitsdef)

      implicit none

      integer,            intent(in)    :: iunit
      character(len=*),   intent(in)    :: filespec
      integer,            intent(inout) :: nlaydef
      double precision,   intent(inout) :: xorgdef,yorgdef
      real,               intent(inout) :: dxdef, dydef
      integer,            intent(inout) :: nxdef, nydef
      real,               intent(inout) :: angledef
      character(len=*),   intent(inout) :: unitsdef

!---Local declarations
      character(len=256) :: cbuf
      integer            :: i   

      open(unit=iunit,file=filespec,status='old')

      nlaydef=0
      xorgdef=0.0
      yorgdef=0.0
      dxdef=0.0
      dydef=0.0
      nxdef=0
      nydef=0
      angledef=0.0
      unitsdef=' '

100   continue
      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'NLAYDEF=')
      if(i == 0) goto 100
      if(i /= 0) read(cbuf(i+8:),*)nlaydef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'XORGDEF=')
      if(i /= 0) read(cbuf(i+8:),*)xorgdef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'YORGDEF=')
      if(i /= 0) read(cbuf(i+8:),*)yorgdef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'DXDEF=')
      if(i /= 0) read(cbuf(i+6:),*)dxdef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'DYDEF=')
      if(i /= 0) read(cbuf(i+6:),*)dydef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'NXDEF=')
      if(i /= 0) read(cbuf(i+6:),*)nxdef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'NYDEF=')
      if(i /= 0) read(cbuf(i+6:),*)nydef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'ANGLEDEF=')
      if(i /= 0) read(cbuf(i+9:),*)angledef

      read(iunit,'(a)',end=200) cbuf
      i=index(cbuf,'UNITSDEF=')
      if(i /= 0) read(cbuf(i+9:),*)unitsdef
      if(unitsdef == 'f') unitsdef='F'
      if(unitsdef == 'm') unitsdef='M'

200   continue
      close(unit=iunit)

      return
      end subroutine gridlib_read_deflt_model_parms


!********************************************************
!---PROGRAM NAME:         gridlib_model_from_grid
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

      subroutine gridlib_model_from_grid(sounit,modunit,grdunit,filespec,   &
                                         units,nx,ny,nlay,xorg,yorg,dx,dy,  &
                                         angle,v,z,grad,vclipmin,vclipmax,  &
                                         gridin_dep,gridout,ngridmax_dep,   &
                                         znon,errRetVal,no_znons_in)

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
      integer,          intent(in)    :: ngridmax_dep     ! _dep: deprecated
      real,             intent(inout) :: gridin_dep(ngridmax_dep) ! ditto
      real,             intent(inout) :: gridout(nx*ny)
      real,             intent(in)    :: znon
      integer,          intent(out)   :: errRetVal
      logical,optional, intent(in)    :: no_znons_in

!---Local declarations
      double precision   :: xorgin, yorgin
      character(len=256) :: filein
      character(len=80)  :: description, attribute
      character(len=256) :: cbuf
      character(len=1)   :: unitshang
      character(len=3)   :: unitsin
      character(len=10)  :: cunits, punits
      integer            :: no_extrap
      integer            :: i, idsource, iy, ix, ip, match, ii, k, j
      integer            :: idominance, icount, icounttemp
      integer            :: nlayin       , nxin, nyin 
      integer            :: ivsource, igsource, ihangsource
      real               :: dxin, dyin, anglein, percent
      real               :: depth, scale, velocity, gradient
      integer            :: stat_error
      logical            :: reset_znon, no_znons
      real               :: znontmp
      real, allocatable  :: gridin(:)
      integer            :: ngridmax, ngridmaxtmp

!---Set default return error status
      errRetVal = 0

      znontmp=znon
      reset_znon=.true.
      no_extrap=0
      if (present(no_znons_in)) then
        no_znons = no_znons_in
      else
        no_znons = .false.
      end if

      ! allocate memory for input grid array
      write(sounit,*)' Allocating memory for input grid'
      ngridmax = nx * ny
      allocate(gridin(ngridmax), stat=stat_error)
      if (stat_error /= 0) then
        errRetVal = 1
        write(sounit,*)' Error getting temp memory for input grid'
        return
      end if

      open(unit=modunit,file=filespec,status='old')

10    continue
      read(modunit,'(a)') cbuf
      if(cbuf(1:1) == '*') goto 10

      read(cbuf,*) nlayin
      if(nlayin < nlay) then
         write(sounit,*)' '
         write(sounit,*)' *** ERROR *** Desired NLAY is bigger'
         write(sounit,*)'               than NLAY in modspec file'
         write(sounit,*)' '
         errRetVal = 1
         return
      end if

!---get past first record
      read(modunit,'(a)') cbuf

!*** Now read each layer
      do i=1,nlay

         write(sounit,*)' '
         write(sounit,*)'Now building layer ',i
         write(sounit,*)' '

!        read(modunit,'(a)') cbuf
         read(modunit,*) unitsin
         if(unitsin(1:1) == 'f') unitsin(1:1)='F'
         if(unitsin(1:1) == 'm') unitsin(1:1)='M'
         if(unitsin(2:2) == 'f') unitsin(2:2)='F'
         if(unitsin(2:2) == 'm') unitsin(2:2)='M'
         if(unitsin(3:3) == 'f') unitsin(3:3)='F'
         if(unitsin(3:3) == 'm') unitsin(3:3)='M'

!---Do depth first
         read(modunit,*) idsource
         if(idsource == 0) then
            read(modunit,*) depth
!---adjust units if necessary
            scale=1.0
            if(unitsin(1:1) == 'F' .and. units == 'M') scale=.3048
            if(unitsin(1:1) == 'M' .and. units == 'F') scale=1./.3048
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=depth*scale
            end do
            end do
         else

            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input depth grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input depth grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &depth grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax, &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,   &
                     reset_znon,description,attribute,cunits,punits,      &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_verify_headers_match(nxin,nx,nyin,ny,         &
                                              dxin,dx,dyin,dy,         &
                                              xorgin,xorg,yorgin,yorg, &
                                              anglein,angle,           &
                                              unitsin(1:1),units,match)
            if(match == 1) then
               do ip=1,nx*ny
                  gridout(ip)=gridin(ip)
               end do
            else
               call gridlib_convert_grid(sounit,1,znon,                    &
                        gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                        unitsin(1:1),                                      &
                        gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                        units, no_extrap)
            end if
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            z(i,ix,iy)=gridout(ip)
         end do
         end do

         read(modunit,*) idominance
         if(i > 1) then
            if(idominance == 0) then
               icount=0
               do iy=1,ny
               do ix=1,nx
                  if (no_znons .and. z(i,ix,iy) == znon) then
                     errRetVal = 2
                     write(sounit,*)' ZNON in input depth grid!'
                     return
                  end if
!                 if(z(i,ix,iy) < z(i-1,ix,iy)) then
                  if(z(i,ix,iy) /= znon.and.z(i-1,ix,iy) /= znon.and.  &
                     z(i,ix,iy) < z(i-1,ix,iy)) then
                     z(i,ix,iy)=z(i-1,ix,iy)
                     icount=icount+1
                  end if
               end do
               end do
               if(icount > 0) then
                  percent=float(icount)/float(nx*ny)*100.
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is '
                  write(sounit,'(a,f5.1,a)')  &
                            '              ',percent,'% recessive.'
                  write(sounit,*)' '
               end if
               if(icount == nx*ny) then
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is entirely'
                  write(sounit,*)'              recessive.'
                  write(sounit,*)' '
               end if
            else
               icount=0
               do iy=1,ny
               do ix=1,nx
               icounttemp=0
               do ii=1,i-1
                  if (no_znons .and. z(i,ix,iy) == znon) then
                     errRetVal = 2
                     write(sounit,*)' ZNON in input depth grid!'
                     return
                  end if
!                 if(z(ii,ix,iy) > z(i,ix,iy)) then
                  if(z(i,ix,iy) /= znon.and.z(ii,ix,iy) /= znon.and.  &
                     z(ii,ix,iy) > z(i,ix,iy)) then
                     z(ii,ix,iy)=z(i,ix,iy)
                     icounttemp=1
                  end if
               end do
                     icount=icount+icounttemp
               end do
               end do
               if(icount > 0) then
                  percent=float(icount)/float(nx*ny)*100.
                  write(sounit,*)' '
                  write(sounit,'(a,f5.1,a)')  &
                            '              ',percent,'% dominant.'
                  write(sounit,*)' '
               end if
               if(icount == nx*ny) then
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is entirely'
                  write(sounit,*)'              dominant.'
                  write(sounit,*)' '
               end if
            end if
         end if


!---Do velocity next
         read(modunit,*) ivsource
         if(ivsource == 0) then
            read(modunit,*) velocity
!---adjust units if necessary
            scale=1.0
            if(unitsin(2:2) == 'F' .and. units == 'M') scale=.3048
            if(unitsin(2:2) == 'M' .and. units == 'F') scale=1./.3048
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=velocity*scale
            end do
            end do
         else

            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input velocity grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input velocity grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &velocity grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax,  &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,    &
                     reset_znon,description,attribute,cunits,punits,       &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_verify_headers_match(nxin,nx,nyin,ny,         &
                                              dxin,dx,dyin,dy,         &
                                              xorgin,xorg,yorgin,yorg, &
                                              anglein,angle,           &
                                              unitsin(2:2),units,match)
            if(match == 1) then
               do ip=1,nx*ny
                  gridout(ip)=gridin(ip)
               end do
            else
               call gridlib_convert_grid(sounit,1,znon,                    &
                        gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                        unitsin(2:2),                                      &
                        gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                        units, no_extrap)
            end if
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            v(i,ix,iy)=gridout(ip)
            if (no_znons .and. v(i,ix,iy) == znon) then
               errRetVal = 2
               write(sounit,*)' ZNON in input velocity grid!'
               return
            end if
         end do
         end do


!---Do gradient last
!---Note that gradients are dimensionless, so no unit conversion necessary
         read(modunit,*) igsource
         if(igsource == 0) then
            read(modunit,*) gradient
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=gradient
            end do
            end do
         else
            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input gradient grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input gradient grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &gradient grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax,  &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,    &
                     reset_znon,description,attribute,cunits,punits,       &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_verify_headers_match(nxin,nx,nyin,ny,         &
                                              dxin,dx,dyin,dy,         &
                                              xorgin,xorg,yorgin,yorg, &
                                              anglein,angle,           &
                                              unitsin(3:3),units,match)
            if(match == 1) then
               do ip=1,nx*ny
                  gridout(ip)=gridin(ip)
               end do
            else
               call gridlib_convert_grid(sounit,0,znon,                    &
                        gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                        unitsin(3:3),                                      &
                        gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                        units, no_extrap)
            end if
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            grad(i,ix,iy)=gridout(ip)
            if (no_znons .and. grad(i,ix,iy) == znon) then
               errRetVal = 2
               write(sounit,*)' ZNON in input gradient grid!'
               return
            end if
         end do
         end do

!--read in velocity clip values
         read(modunit,*) vclipmin(i)
         read(modunit,*) vclipmax(i)
!--change units in velocity clip values if necessary
         scale=1.0
         if(unitsin(2:2) == 'F' .and. units == 'M') scale=.3048
         if(unitsin(2:2) == 'M' .and. units == 'F') scale=1./.3048
         vclipmin(i)=vclipmin(i)*scale
         vclipmax(i)=vclipmax(i)*scale

!---ONLY if the next card does not start with an asterisk (*) then
!---read in a depth surface on which we can hang the V0 values.
!---this allows us to use something besides the current z
         read(modunit,'(a)',end=100) cbuf
         goto 200
100      continue
         if(i /= nlay) then
            write(sounit,*)' '
            write(sounit,*)' *** ERROR *** Hit EOF in MODSPEC file'
            write(sounit,*)' '
            errRetVal = 1
            return
         end if
         goto 1000

200      continue
         if(cbuf(1:1) /= '*') then

            ihangsource = 0
            read(cbuf,*) ihangsource
!---Check for valid value (elb)
            k = 0
            if(ihangsource  ==  0) then
               do j=1,256
                  if(cbuf(j:j) /= ' ') k=1 ! Indicates non blank
               end do
            else
               k = ihangsource
            endif
            if(k  /=  1) then
               write(sounit,*) 'Invalid hang source flag'
            endif
!---***************************
            read(modunit,*) unitshang

            if(ihangsource == 0) then
               read(modunit,*) depth
!---adjust units if necessary
               scale=1.0
               if(unitshang == 'F' .and. units == 'M') scale=.3048
               if(unitshang == 'M' .and. units == 'F') scale=1./.3048
               ip=0
               do iy=1,ny
               do ix=1,nx
                  ip=ip+1
                  gridout(ip)=depth*scale
               end do
               end do
            else

               ! get input grid file name
               read(modunit,*) filein

               ! read the input grid header
               call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                             ngridmaxtmp, nxin, nyin, xorgin,&
                                             yorgin, dxin, dyin, anglein,    &
                                             znontmp, description, attribute,&
                                             cunits, punits, stat_error)
               if (stat_error /= 0) then
                 errRetVal = 1
                 write(sounit,*)' Error reading input hanging source grid, &
                                &file ',filein
                 return
               end if

               ! reallocate input grid memory if this grid larger than
               ! previous input grid
               if (ngridmaxtmp > ngridmax) then

                 ngridmax = ngridmaxtmp

                 ! deallocate previous input grid memory
                 deallocate(gridin)

                 ! allocate memory for input grid array
                 write(sounit,*)' Reallocating memory for input hanging &
                                &source grid'
                 allocate(gridin(ngridmax), stat=stat_error)
                 if (stat_error /= 0) then
                   errRetVal = 1
                   write(sounit,*)' Error getting temp memory for input &
                                  &hanging source grid'
                   return
                 end if

               end if

               ! read the input grid
               call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax,  &
                        nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,    &
                        reset_znon,description,attribute,cunits,punits,       &
                        stat_error)
               if (stat_error /= 0) then
                 errRetVal = 1
                 return
               end if
               call gridlib_verify_headers_match(nxin,nx,nyin,ny,         &
                                                 dxin,dx,dyin,dy,         &
                                                 xorgin,xorg,yorgin,yorg, &
                                                 anglein,angle,           &
                                                 unitshang,units,match)
               if(match == 1) then
                  do ip=1,nx*ny
                     gridout(ip)=gridin(ip)
                  end do
               else
                  call gridlib_convert_grid(sounit,1,znon,                    &
                           gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                           unitshang,                                         &
                           gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                           units, no_extrap)
               end if
            end if

!---now re-calculate the v0 value so that it is adjusted to
!--- "hang" off the current depth surface
            ip=0
            do iy=1,ny
            do ix=1,nx
            ip=ip+1
               if (no_znons .and. gridout(ip) == znon) then
                  errRetVal = 2
                  write(sounit,*)' ZNON in input hanging source grid!'
                  return
               end if
               v(i,ix,iy)= v(i,ix,iy)+  &
                          (z(i,ix,iy)-gridout(ip))*grad(i,ix,iy)
! eliminate clipping of velocity in hanging situation (bill done)
!              if(v(i,ix,iy) < vclipmin(i)) v(i,ix,iy)=vclipmin(i)
!              if(v(i,ix,iy) > vclipmax(i)) v(i,ix,iy)=vclipmax(i)
               if(v(i,ix,iy) == znon.or.z(i,ix,iy) == znon.or.  &
                  grad(i,ix,iy) == znon.or.gridout(ip) == znon)  &
                  v(i,ix,iy)=znon
            end do
            end do

!---now get past the "***" record separating layers
            if(i /= nlay) read(modunit,'(a)') cbuf
         end if


      end do



1000  continue
      close(unit=modunit)

      return
      end subroutine gridlib_model_from_grid


!********************************************************

      subroutine gridlib_print_model_parms(sounit,units,nx,ny,nlay,    &
                                           xorg,yorg,dx,dy,angle,v,z,  &
                                           grad,vclipmin,vclipmax,znon)

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

!---Local declarations
      integer          :: izero, i, icount, ix, iy
      real             :: big, dmin, dmax, vmin, vmax, gmin, gmax

!---First check to see if we have any layers that are
!   completely zero thickness.

      izero=0
      write(sounit,*)' '
      write(sounit,*)' TOTALLY ZERO THICKNESS LAYERS ARE BETWEEN'
      write(sounit,*)'       HORIZON    and    HORIZON'
      do i=2,nlay
      icount=0
      do ix=1,nx
      do iy=1,ny
         if(z(i,ix,iy) == z(i-1,ix,iy)) icount=icount+1
      end do
      end do
      if(icount == nx*ny)  &
          write(sounit,'(9x,i5,13x,i5)')  i-1,i
      end do

!---Print out important model information
      write(sounit,*)' '
      write(sounit,*)' '
      write(sounit,*)'    ***   FINAL MODEL PARAMETERS ***'
      write(sounit,*)' '
      write(sounit,*)' XORG    = ',xorg,'  YORG    = ',yorg
      write(sounit,*)' DX      = ',dx,  '  DY      = ',dy
      write(sounit,*)' NX      = ',nx,  '  NY      = ',ny
      write(sounit,*)' ANGLE   = ',angle
      write(sounit,*)' '

      write(sounit,*)  &
       'LAYER  ZMIN    ZMAX    VMIN    VMAX    GMIN    GMAX   ',  &
       'VCLIPMIN VCLIPMAX'

      big=999999999.
      do i=1,nlay
         dmin=big
         dmax=-big
         vmin=big
         vmax=-big
         gmin=big
         gmax=-big
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
            if(grad(i,ix,iy) /= znon) then
               if(grad(i,ix,iy) < gmin) gmin=grad(i,ix,iy)
               if(grad(i,ix,iy) > gmax) gmax=grad(i,ix,iy)
            endif
         enddo
         enddo

         write(sounit,'(i5,4f8.0,2f8.4,2f10.0)')   &
         i,dmin,dmax,vmin,vmax,gmin,gmax,vclipmin(i),vclipmax(i)
      enddo

      return
      end subroutine gridlib_print_model_parms


!********************************************************

      subroutine gridlib_print_parms_insertion(sounit, nx, ny, nlay, &
                                               v, z, grad, vclipmin, &
                                               vclipmax)

      implicit none

      integer,          intent(in)    :: sounit
      integer,          intent(in)    :: nx, ny, nlay
      real,             intent(in)    :: v(nlay,nx,ny)
      real,             intent(in)    :: z(nlay,nx,ny)
      real,             intent(in)    :: grad(nlay,nx,ny)
      real,             intent(in)    :: vclipmin(nlay), vclipmax(nlay)

      ! local variables
      integer :: i, ix, iy
      real    :: deltaz, dmin, dmax, vmin, vmax, gmin, gmax

      ! Print out important model information
      write(sounit,*)' '
      write(sounit,*)' ***  MODEL AFTER INSERTION ***'
      write(sounit,*)' '
      write(sounit,*)'NLAY NX NY=',NLAY,NX,NY
      write(sounit,*)' '

      write(sounit,*)'LAYER  ZMIN    ZMAX   DELTAZ   VMIN    VMAX   &
                     &GMIN  GMAX VCLIPMIN VCLIPMAX'

      do i = 1, nlay
         deltaz = 0.0
         dmin = z(i,1,1)
         dmax = z(i,1,1)
         vmin = v(i,1,1)
         vmax = v(i,1,1)
         gmin = grad(i,1,1)
         gmax = grad(i,1,1)
         do ix = 1, nx
             do iy = 1, ny
                 if (z(i,ix,iy) < dmin) dmin = z(i,ix,iy)
                 if (z(i,ix,iy) > dmax) dmax = z(i,ix,iy)
                 if (v(i,ix,iy) < vmin) vmin = v(i,ix,iy)
                 if (v(i,ix,iy) > vmax) vmax = v(i,ix,iy)
                 if (grad(i,ix,iy) < gmin) gmin = grad(i,ix,iy)
                 if (grad(i,ix,iy) > gmax) gmax = grad(i,ix,iy)
                 if (i.ne.nlay) deltaz = deltaz + z(i+1,ix,iy) - z(i,ix,iy)
             end do
         end do
         deltaz = deltaz/(nx*ny)

         write(sounit,'(i5,5(1x,f8.0),2(1x,f10.2),2(1x,f8.0))') &
             i,dmin,dmax,deltaz,vmin,vmax,gmin,gmax, &
             vclipmin(i),vclipmax(i)
      end do

      write(sounit,*)' '

      end subroutine gridlib_print_parms_insertion


!********************************************************
!---PROGRAM NAME:         gridlib_compute_velocity
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

      subroutine gridlib_compute_velocity(ipsdm,dmax,depth,velocity,layer,  &
                                          ix,iy,nx,ny,nlay,nlaymax,v,z,     &
                                          grad,vclipmin,vclipmax,znon)

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

!---Local declarations
      real             :: graduse, deltaz, vmax
      integer          :: i

      graduse=0.0

      velocity=znon

      if(depth <= z(1,ix,iy).and.z(1,ix,iy) /= znon) then
         velocity=v(1,ix,iy)
         if(grad(1,ix,iy) == znon) velocity=znon
         layer=1
      elseif(depth >= z(nlay,ix,iy).and.z(nlay,ix,iy) /= znon) then
         graduse=grad(nlay,ix,iy)
         if(ipsdm == 2) then
            deltaz = dmax-z(nlay,ix,iy)
            vmax=v(nlay,ix,iy)+  &
                  deltaz*graduse
            if(graduse < 0.0 .and.   &
               vmax < vclipmin(nlay) .and.  &
               deltaz  >  0.0) then
               graduse = (vclipmin(nlay) - v(nlay,ix,iy))/deltaz
            elseif(graduse > 0.0 .and.   &
               vmax > vclipmax(nlay) .and.  &
               deltaz  >  0.0) then
               graduse = (vclipmax(nlay) - v(nlay,ix,iy))/deltaz
            endif
         end if
         velocity=v(nlay,ix,iy)+  &
                  (depth-z(nlay,ix,iy))*graduse
         if(grad(nlay,ix,iy) == znon.or.  &
            v(nlay,ix,iy) == znon) velocity=znon
         layer=nlay
      else
         do i=1,nlay-1
            layer=i
            if(depth >= z(i,ix,iy) .and.   &
               depth <= z(i+1,ix,iy).and.  &
               z(i,ix,iy) /= znon.and.z(i+1,ix,iy) /= znon) then
               graduse = grad(i,ix,iy)
               if(ipsdm == 2) then
                  deltaz = z(i+1,ix,iy)-z(i,ix,iy)
                  vmax=v(i,ix,iy)+  &
                        deltaz*graduse
                  if(graduse < 0.0 .and.   &
                     vmax < vclipmin(i) .and.  &
                     deltaz  >  0.0) then
                     graduse = (vclipmin(i) - v(i,ix,iy))/deltaz
                  elseif(graduse > 0.0 .and.   &
                     vmax > vclipmax(i) .and.  &
                     deltaz  >  0.0) then
                     graduse = (vclipmax(i) - v(i,ix,iy))/deltaz
                  endif
               endif
               velocity=v(i,ix,iy)+  &
                        (depth-z(i,ix,iy))*graduse


               if(grad(i,ix,iy) == znon.or.v(i,ix,iy) == znon)   &
               velocity=znon

               goto 888
            endif
         enddo
888      continue
      endif

! eliminate clipping of velocity in hanging situation (bill done)
!     if(velocity /= znon) then
!        if(graduse /= 0.0) then
!           if(velocity < vclipmin(layer)) velocity=vclipmin(layer)
!           if(velocity > vclipmax(layer)) velocity=vclipmax(layer)
!        endif
!     endif

      return
      end subroutine gridlib_compute_velocity


!********************************************************
!  PROGRAM:          gridlib_fix_model
!
!  AUTHOR:           K. D. WYATT
!
!  CREATION DATE:    April 1996
!
!  DESCRIPTION:      This subroutine adds an artificial
!                    layer when a velocity gradient causes
!                    a velocity to exceed the clip velocity
!                    BEFORE the bottom of the layer

      subroutine gridlib_fix_model(sounit,ivirtual,                       &
                                   nx,ny,nlay,nlaynew,nlaymax,zdeepest,   &
                                   vtemp,ztemp,ktemp,vclipmintemp,        &
                                   vclipmaxtemp,v,z,k,vclipmin,vclipmax,  &
                                   errRetVal)

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

!---Local declarations
      integer          :: ix, iy, i
      real             :: zmax, ztop, zbot, vtop, vclip, zclip

!---Set default return error status
      errRetVal = 0

!---first find out what "zdeepest" should really be
      zmax=0.
      do iy=1,ny
      do ix=1,nx
         if(ztemp(nlay,ix,iy) > zmax) zmax=ztemp(nlay,ix,iy)
      end do
      end do
      write(sounit,*)'Maximum depth in GRID model  =',zmax

      if(zdeepest == 0.0) then
         zmax=zmax*1.6
      else
         if(zdeepest < zmax) then
            write(sounit,*)' '
            write(sounit,*)' *** ERROR *** The maximum depth for last layer'
            write(sounit,*)'               that you requested is less than'
            write(sounit,*)'               the deepest value in the model.'
            write(sounit,*)'                 You requested:    ',zdeepest
            write(sounit,*)'                 Max. model depth: ',zmax
            write(sounit,*)' '
            errRetVal = 1
            return
         end if
         zmax = zdeepest
      end if
      write(sounit,*)'Maximum depth in full model=',zmax


      nlaynew=0
      do i=1,nlay
!---add top of this layer to our list
         nlaynew=nlaynew+1
         do ix=1,nx
         do iy=1,ny
            z(nlaynew,ix,iy)=ztemp(i,ix,iy)
            v(nlaynew,ix,iy)=vtemp(i,ix,iy)
            k(nlaynew,ix,iy)=ktemp(i,ix,iy)
         end do
         end do
         vclipmin(nlaynew)=vclipmintemp(i)
         vclipmax(nlaynew)=vclipmaxtemp(i)
!---only do layers that
         if(ivirtual(i) > 0) then
!---insert layer
            nlaynew=nlaynew+1
            vclipmin(nlaynew)=vclipmintemp(i)
            vclipmax(nlaynew)=vclipmaxtemp(i)

            do ix=1,nx
            do iy=1,ny
               ztop=ztemp(i,ix,iy)
               if(i == nlay) then
                  zbot=zmax
               else
                  zbot=ztemp(i+1,ix,iy)
               end if

               if(ktemp(i,ix,iy) == 0.0) then
                  v(nlaynew,ix,iy)=vtemp(i,ix,iy)
                  z(nlaynew,ix,iy)=zbot
                  k(nlaynew,ix,iy)=ktemp(i,ix,iy)
               else
                  vtop = vtemp(i,ix,iy)
                  if(ktemp(i,ix,iy) > 0.0) vclip=vclipmaxtemp(i)
                  if(ktemp(i,ix,iy) < 0.0) vclip=vclipmintemp(i)
                  zclip = ztop + (vclip-vtop)/ktemp(i,ix,iy)

                  v(nlaynew,ix,iy)=vclip
                  z(nlaynew,ix,iy)=min(zclip,zbot)
                  k(nlaynew,ix,iy)=ktemp(i,ix,iy)

               end if

            end do
            end do

         end if

      end do

      if (nlaynew /= nlaymax) then
          write(sounit,*)' *** ERROR *** in gridlib_fix_model'
          write(sounit,*)'    Final number of layers (nlaynew) = ',nlaynew
          write(sounit,*)'    does NOT match specified number (nlaymax) = ', &
                         nlaymax
          errRetVal = 1
          return
      end if

      end subroutine gridlib_fix_model


!********************************************************
!  PROGRAM:          gridlib_check_clip_velocity
!
!  AUTHOR:           K. D. WYATT
!
!  CREATION DATE:    April 1996
!
!  DESCRIPTION:      This subroutine checks to find
!                    layers when a velocity gradient causes
!                    a velocity to exceed the clip velocity
!                    BEFORE the bottom of the layer

      subroutine gridlib_check_clip_velocity(sounit,nx,ny,nlay,zdeepest,  &
                                             v,z,k,vclipmin,vclipmax,     &
                                             nlayclip,znon)

      implicit none

      integer, intent(in)    :: sounit
      integer, intent(in)    :: nx, ny, nlay
      real,    intent(in)    :: zdeepest
      real,    intent(in)    :: v(nlay,nx,ny)
      real,    intent(in)    :: z(nlay,nx,ny)
      real,    intent(in)    :: k(nlay,nx,ny)
      real,    intent(in)    :: vclipmin(nlay)
      real,    intent(in)    :: vclipmax(nlay)
      integer, intent(inout) :: nlayclip
      real,    intent(in)    :: znon

!---Local declarations
      integer          :: i, ix, iy, iflag
      real             :: ztop, zbot, graduse, deltaz
      real             :: vtop, vbot, avgdeltaz, zok
      real             :: avgdeltazok

      write(sounit,*)' '
      write(sounit,*)' *** NOTE*** The following layers have'
      write(sounit,*)'             gradients which cause its'
      write(sounit,*)'             velocities to clip BEFORE'
      write(sounit,*)'             they hit the bottom of the'
      write(sounit,*)'             layer:'

      write(sounit,*)'LAYER    AVG. THICKNESS  AVG. THICKNESS BEFORE CLIP'

      nlayclip=0
      do i=1,nlay
         avgdeltaz=0.0
         avgdeltazok=0.0
!---search for layers whose gradient causes the velocities
!---to clip BEFORE they hit the bottom of the layer.
         iflag=0

         do ix=1,nx
         do iy=1,ny

            if(z(i,ix,iy) /= znon.and.v(i,ix,iy) /= znon.and.  &
               z(i+1,ix,iy) /= znon.and.k(i,ix,iy) /= znon) then
            ztop=z(i,ix,iy)
            if(i == nlay) then
               zbot=zdeepest
            else
               zbot=z(i+1,ix,iy)
            endif

            graduse = k(i,ix,iy)
            deltaz = zbot - ztop
            vtop = v(i,ix,iy)
            vbot = vtop + graduse*deltaz
            avgdeltaz = avgdeltaz + deltaz
            zok = zbot

            if( vbot < vclipmin(i) .and.   &
                graduse < 0.0 .and.  &
                deltaz  >  0.0) then
                iflag=iflag+1
                zok = (vclipmin(i)-vtop)/graduse + ztop
            elseif (vbot > vclipmax(i) .and.  &
                     graduse > 0.0 .and.  &
                     deltaz  >  0.0) then
                iflag=iflag+1
                zok = (vclipmax(i)-vtop)/graduse + ztop
            endif

            avgdeltazok = avgdeltazok + (zok-ztop)

            endif

         enddo
         enddo

         if(iflag > 0)then
            avgdeltaz = avgdeltaz/(nx*ny)
            avgdeltazok = avgdeltazok/(nx*ny)
            nlayclip=nlayclip+1
            write(sounit,'(i5,6x,f8.0,10x,f8.0)')   &
                 i, avgdeltaz, avgdeltazok
         endif
      enddo

      write(sounit,*)' '
      write(sounit,*)' TOTAL NUMBER OF SUSPECT LAYERS=',nlayclip
      write(sounit,*)' '

      return
      end subroutine gridlib_check_clip_velocity


!********************************************************
!---PROGRAM NAME:         gridlib_convert_warp_grid
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
!---LAST MOD. DATE:       6 December 1996 - changed "convert_grid" to
!                                           allow grid warping.

      subroutine gridlib_convert_warp_grid(sounit,iflag,gnull,gridx,gridy,  &
                     grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1,units1,       &
                     grid2,nx2,ny2,dx2,dy2,xorg2,yorg2,angle2,units2)

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

!---Local declarations
      integer          :: ix1, iy1, ix2, iy2
      real             :: xp, yp, xf, yf, xx, yy, xndx, yndx
      real             :: rad1, rad2, g1, g2, g12, g21, g11, g22

!---change units if necessary
      call gridlib_convert_units(iflag,gnull,units1,units2,  &
               grid1,nx1,ny1,dx1,dy1,xorg1,yorg1,angle1)


!---resample grid
      rad1 = angle1/180.*3.141592654
      rad2 = angle2/180.*3.141592654
      do ix2=1,nx2
      do iy2=1,ny2

!        xp = (ix2-1)*dx2
!        yp = (iy2-1)*dy2

         xp = gridx(ix2,iy2)
         yp = gridy(ix2,iy2)

         xf = xp*cos(rad2) - yp*sin(rad2) + xorg2
         yf = xp*sin(rad2) + yp*cos(rad2) + yorg2

         xx=xf-xorg1
         yy=yf-yorg1

         xndx =  xx*cos(rad1) + yy*sin(rad1)
         yndx = -xx*sin(rad1) + yy*cos(rad1)
         xndx=xndx/dx1+1.0
         yndx=yndx/dy1+1.0
         ix1=xndx
         iy1=yndx

!         -----------------------------
!        |       |              |       |
!        | case1 |    case 7    | case4 |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        |       |              |       |
!        | case3 |    case 9    | case6 |
!        |       |              |       |
!        |       |              |       |
!         ------------------------------
!        |       |              |       |
!        | case2 |    case 8    | case5 |
!        |       |              |       |
!         ------------------------------
         if(ix1 < 1 ) then
            if(iy1 < 1)then
! case 1
               grid2(ix2,iy2)=grid1(1,1)
            else if (iy1 >= ny1) then
! case 2
               grid2(ix2,iy2)=grid1(1,ny1)
            else
! case 3
               g1 = grid1(1,iy1)
               g2 = grid1(1,iy1+1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
            end if
         else if(ix1 >= nx1) then
            if(iy1 < 1)then
! case 4
               grid2(ix2,iy2)=grid1(nx1,1)
            else if (iy1 >= ny1) then
! case 5
               grid2(ix2,iy2)=grid1(nx1,ny1)
            else
! case 6
               g1 = grid1(nx1,iy1)
               g2 = grid1(nx1,iy1+1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
            end if

         else if(iy1 < 1 .and.   &
                (ix1 >= 1 .and. ix1 < nx1) ) then
! case 7
               g1 = grid1(ix1,1)
               g2 = grid1(ix1+1,1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  grid2(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
         else if(iy1 >= ny1 .and.  &
                (ix1 >= 1 .and. ix1 < nx1) ) then
! case 8
               g1 = grid1(ix1,ny1)
               g2 = grid1(ix1+1,ny1)
               grid2(ix2,iy2)=gnull
               if(g1 /= gnull .and. g2 /= gnull)   &
                  grid2(ix2,iy2)=g1+(g2-g1)*(xndx-ix1)
! case 9
!     interpolate along y's first
         else if(ix1 >= 1 .and. ix1 < nx1 .and.  &
                 iy1 >= 1 .and. iy1 < ny1 ) then
               grid2(ix2,iy2)=gnull
               g11 = grid1(ix1  ,iy1  )
               g21 = grid1(ix1+1,iy1  )
               g12 = grid1(ix1  ,iy1+1)
               g22 = grid1(ix1+1,iy1+1)
               if(g11 /= gnull .and. g21 /= gnull .and.  &
                  g12 /= gnull .and. g22 /= gnull) then
                  g1 = g11+(g21-g11)*(xndx-ix1)
                  g2 = g12+(g22-g12)*(xndx-ix1)
                  grid2(ix2,iy2)=g1+(g2-g1)*(yndx-iy1)
               end if
         end if

      end do
      end do

      write(sounit,*)' '
      write(sounit,*)' convert_grid: xorg1, yorg1=',xorg1,yorg1
      write(sounit,*)' convert_grid: xorg2, yorg2=',xorg2,yorg2
      write(sounit,*)' '

      return
      end subroutine gridlib_convert_warp_grid


!********************************************************
!---PROGRAM NAME:         gridlib_model_from_warp_grid
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

      subroutine gridlib_model_from_warp_grid(sounit,modunit,grdunit,   &
                     filespec,units,nx,ny,nlay,xorg,yorg,dx,dy,angle,   &
                     v,z,grad,vclipmin,vclipmax,gridx,gridy,gridin_dep, &
                     gridout,ngridmax_dep,znon,errRetVal,no_znons_in)

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
      integer,          intent(in)    :: ngridmax_dep     ! _dep: deprecated
      real,             intent(inout) :: gridin_dep(ngridmax_dep) ! ditto
      real,             intent(inout) :: gridout(nx*ny)
      real,             intent(in)    :: znon
      integer,          intent(out)   :: errRetVal
      logical,optional, intent(in)    :: no_znons_in

!---Local declarations
      character(len=256) :: filein
      character(len=256) :: cbuf
      character(len=3)   :: unitsin
      character(len=1)   :: unitshang
      character(len=80)  :: description, attribute
      character(len=10)  :: cunits, punits
      double precision   :: xorgin, yorgin
      integer            :: i, ix, iy, ip       , idsource 
      integer            :: icount, idominance, icounttemp
      integer            :: ivsource, igsource, ihangsource
      real               :: depth, scale, percent, velocity, gradient
      real               :: dxin, dyin, anglein
      integer            :: nxin, nyin, ii, nlayin
      integer            :: stat_error
      logical            :: reset_znon, no_znons
      real               :: znontmp
      real, allocatable  :: gridin(:)
      integer            :: ngridmax, ngridmaxtmp

!---Set default return error status
      errRetVal = 0

      znontmp=znon
      reset_znon=.true.
      if (present(no_znons_in)) then
        no_znons = no_znons_in
      else
        no_znons = .false.
      end if
      
      ! allocate memory for input grid array
      write(sounit,*)' Allocating memory for input grid'
      ngridmax = nx * ny
      allocate(gridin(ngridmax), stat=stat_error)
      if (stat_error /= 0) then
        errRetVal = 1
        write(sounit,*)' Error getting temp memory for input grid'
        return
      end if

      open(unit=modunit,file=filespec,status='old')

10    continue
      read(modunit,'(a)') cbuf
      if(cbuf(1:1) == '*') goto 10

      read(cbuf,*) nlayin
      if(nlayin < nlay) then
         write(sounit,*)' '
         write(sounit,*)' *** ERROR *** Desired NLAY is bigger'
         write(sounit,*)'               than NLAY in modspec file'
         write(sounit,*)' '
         errRetVal = 1
         return
      end if

!---get past first record
      read(modunit,'(a)') cbuf

!*** Now read each layer
      do i=1,nlay

         write(sounit,*)' '
         write(sounit,*)'Now building layer ',i
         write(sounit,*)' '

!        read(modunit,'(a)') cbuf
         read(modunit,*) unitsin
         if(unitsin(1:1) == 'f') unitsin(1:1)='F'
         if(unitsin(1:1) == 'm') unitsin(1:1)='M'
         if(unitsin(2:2) == 'f') unitsin(2:2)='F'
         if(unitsin(2:2) == 'm') unitsin(2:2)='M'
         if(unitsin(3:3) == 'f') unitsin(3:3)='F'
         if(unitsin(3:3) == 'm') unitsin(3:3)='M'

!---Do depth first
         read(modunit,*) idsource
         if(idsource == 0) then
            read(modunit,*) depth
!---adjust units if necessary
            scale=1.0
            if(unitsin(1:1) == 'F' .and. units == 'M') scale=.3048
            if(unitsin(1:1) == 'M' .and. units == 'F') scale=1./.3048
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=depth*scale
            end do
            end do
         else

            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input depth grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input depth grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &depth grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax, &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,   &
                     reset_znon,description,attribute,cunits,punits,      &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_convert_warp_grid(sounit,1,znon,gridx,gridy,   &
                     gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                     unitsin(1:1),                                      &
                     gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                     units)
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            z(i,ix,iy)=gridout(ip)
         end do
         end do

         read(modunit,*) idominance
         if(i > 1) then
            if(idominance == 0) then
               icount=0
               do iy=1,ny
               do ix=1,nx
                  if (no_znons .and. z(i,ix,iy) == znon) then
                     errRetVal = 2
                     write(sounit,*)' ZNON in input depth grid!'
                     return
                  end if
!                 if(z(i,ix,iy) < z(i-1,ix,iy)) then
                  if(z(i,ix,iy) /= znon.and.z(i-1,ix,iy) /= znon.and.  &
                     z(i,ix,iy) < z(i-1,ix,iy)) then
                     z(i,ix,iy)=z(i-1,ix,iy)
                     icount=icount+1
                  end if
               end do
               end do
               if(icount > 0) then
                  percent=float(icount)/float(nx*ny)*100.
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is '
                  write(sounit,'(a,f5.1,a)')  &
                            '              ',percent,'% recessive.'
                  write(sounit,*)' '
               end if
               if(icount == nx*ny) then
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is entirely'
                  write(sounit,*)'              recessive.'
                  write(sounit,*)' '
               end if
            else
               icount=0
               do iy=1,ny
               do ix=1,nx
               icounttemp=0
               do ii=1,i-1
                  if (no_znons .and. z(i,ix,iy) == znon) then
                     errRetVal = 2
                     write(sounit,*)' ZNON in input depth grid!'
                     return
                  end if
!                 if(z(ii,ix,iy) > z(i,ix,iy)) then
                  if(z(i,ix,iy) /= znon.and.z(ii,ix,iy) /= znon.and.  &
                     z(ii,ix,iy) > z(i,ix,iy)) then
                     z(ii,ix,iy)=z(i,ix,iy)
                     icounttemp=1
                  end if
               end do
                     icount=icount+icounttemp
               end do
               end do
               if(icount > 0) then
                  percent=float(icount)/float(nx*ny)*100.
                  write(sounit,*)' '
                  write(sounit,'(a,f5.1,a)')  &
                            '              ',percent,'% dominant.'
                  write(sounit,*)' '
               end if
               if(icount == nx*ny) then
                  write(sounit,*)' '
                  write(sounit,*)' *** NOTE *** This layer is entirely'
                  write(sounit,*)'              dominant.'
                  write(sounit,*)' '
               end if
            end if
         end if


!---Do velocity next
         read(modunit,*) ivsource
         if(ivsource == 0) then
            read(modunit,*) velocity
!---adjust units if necessary
            scale=1.0
            if(unitsin(2:2) == 'F' .and. units == 'M') scale=.3048
            if(unitsin(2:2) == 'M' .and. units == 'F') scale=1./.3048
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=velocity*scale
            end do
            end do
         else

            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input velocity grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input velocity grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &velocity grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax, &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,   &
                     reset_znon,description,attribute,cunits,punits,      &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_convert_warp_grid(sounit,1,znon,gridx,gridy,   &
                     gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                     unitsin(2:2),                                      &
                     gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                     units)
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            v(i,ix,iy)=gridout(ip)
            if (no_znons .and. v(i,ix,iy) == znon) then
               errRetVal = 2
               write(sounit,*)' ZNON in input velocity grid!'
               return
            end if
         end do
         end do


!---Do gradient last
!---Note that gradients are dimensionless, so no unit conversion necessary
         read(modunit,*) igsource
         if(igsource == 0) then
            read(modunit,*) gradient
            ip=0
            do iy=1,ny
            do ix=1,nx
               ip=ip+1
               gridout(ip)=gradient
            end do
            end do
         else

            ! get input grid file name
            read(modunit,*) filein

            ! read the input grid header
            call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                          ngridmaxtmp, nxin, nyin, xorgin,&
                                          yorgin, dxin, dyin, anglein,    &
                                          znontmp, description, attribute,&
                                          cunits, punits, stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              write(sounit,*)' Error reading input gradient grid, file ',filein
              return
            end if

            ! reallocate input grid memory if this grid larger than
            ! previous input grid
            if (ngridmaxtmp > ngridmax) then

              ngridmax = ngridmaxtmp

              ! deallocate previous input grid memory
              deallocate(gridin)

              ! allocate memory for input grid array
              write(sounit,*)' Reallocating memory for input gradient grid'
              allocate(gridin(ngridmax), stat=stat_error)
              if (stat_error /= 0) then
                errRetVal = 1
                write(sounit,*)' Error getting temp memory for input &
                               &gradient grid'
                return
              end if

            end if

            ! read the input grid
            call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax, &
                     nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,   &
                     reset_znon,description,attribute,cunits,punits,      &
                     stat_error)
            if (stat_error /= 0) then
              errRetVal = 1
              return
            end if
            call gridlib_convert_warp_grid(sounit,0,znon,gridx,gridy,   &
                     gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                     unitsin(3:3),                                      &
                     gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                     units)
         end if

         ip=0
         do iy=1,ny
         do ix=1,nx
            ip=ip+1
            grad(i,ix,iy)=gridout(ip)
            if (no_znons .and. grad(i,ix,iy) == znon) then
               errRetVal = 2
               write(sounit,*)' ZNON in input gradient grid!'
               return
            end if
         end do
         end do

!--read in velocity clip values
         read(modunit,*) vclipmin(i)
         read(modunit,*) vclipmax(i)
!--change units in velocity clip values if necessary
         scale=1.0
         if(unitsin(2:2) == 'F' .and. units == 'M') scale=.3048
         if(unitsin(2:2) == 'M' .and. units == 'F') scale=1./.3048
         vclipmin(i)=vclipmin(i)*scale
         vclipmax(i)=vclipmax(i)*scale

!---ONLY if the next card does not start with an asterisk (*) then
!---read in a depth surface on which we can hang the V0 values.
!---this allows us to use something besides the current z
         read(modunit,'(a)',end=100) cbuf
         goto 200
100      continue
         if(i /= nlay) then
            write(sounit,*)' '
            write(sounit,*)' *** ERROR *** Hit EOF in MODSPEC file'
            write(sounit,*)' '
            errRetVal = 1
            return
         end if
         goto 1000

200      continue
         if(cbuf(1:1) /= '*') then

            read(cbuf,*) ihangsource
            read(modunit,*) unitshang

            if(ihangsource == 0) then
               read(modunit,*) depth
!---adjust units if necessary
               scale=1.0
               if(unitshang == 'F' .and. units == 'M') scale=.3048
               if(unitshang == 'M' .and. units == 'F') scale=1./.3048
               ip=0
               do iy=1,ny
               do ix=1,nx
                  ip=ip+1
                  gridout(ip)=depth*scale
               end do
               end do
            else

               ! get input grid file name
               read(modunit,*) filein

               ! read the input grid header
               call gridlib_read_grid_header(sounit, grdunit, filein,        &
                                             ngridmaxtmp, nxin, nyin, xorgin,&
                                             yorgin, dxin, dyin, anglein,    &
                                             znontmp, description, attribute,&
                                             cunits, punits, stat_error)
               if (stat_error /= 0) then
                 errRetVal = 1
                 write(sounit,*)' Error reading input hanging source grid, &
                                &file ',filein
                 return
               end if

               ! reallocate input grid memory if this grid larger than
               ! previous input grid
               if (ngridmaxtmp > ngridmax) then

                 ngridmax = ngridmaxtmp

                 ! deallocate previous input grid memory
                 deallocate(gridin)

                 ! allocate memory for input grid array
                 write(sounit,*)' Reallocating memory for input &
                                &hanging source grid'
                 allocate(gridin(ngridmax), stat=stat_error)
                 if (stat_error /= 0) then
                   errRetVal = 1
                   write(sounit,*)' Error getting temp memory for input &
                                  &hanging source grid'
                   return
                 end if

               end if

                ! read the input grid
                call gridlib_read_grid(sounit,grdunit,filein,gridin,ngridmax, &
                         nxin,nyin,xorgin,yorgin,dxin,dyin,anglein,znontmp,   &
                         reset_znon,description,attribute,cunits,punits,      &
                         stat_error)
               if (stat_error /= 0) then
                 errRetVal = 1
                 return
               end if
               call gridlib_convert_warp_grid(sounit,1,znon,gridx,gridy,   &
                        gridin,nxin,nyin,dxin,dyin,xorgin,yorgin,anglein,  &
                        unitshang,                                         &
                        gridout,nx,ny,dx,dy,xorg,yorg,angle,               &
                        units)
            end if

!---now re-calculate the v0 value so that it is adjusted to
!--- "hang" off the current depth surface
            ip=0
            do iy=1,ny
            do ix=1,nx
            ip=ip+1
               if (no_znons .and. gridout(ip) == znon) then
                  errRetVal = 2
                  write(sounit,*)' ZNON in input hanging source grid!'
                  return
               end if
               v(i,ix,iy)= v(i,ix,iy)+  &
                          (z(i,ix,iy)-gridout(ip))*grad(i,ix,iy)
! eliminate clipping of velocity in hanging situation (bill done)
!              if(v(i,ix,iy) < vclipmin(i)) v(i,ix,iy)=vclipmin(i)
!              if(v(i,ix,iy) > vclipmax(i)) v(i,ix,iy)=vclipmax(i)
               if(v(i,ix,iy) == znon.or.z(i,ix,iy) == znon.or.  &
                  grad(i,ix,iy) == znon.or.gridout(ip) == znon)  &
                  v(i,ix,iy)=znon
            end do
            end do

!---now get past the "***" record separating layers
            if(i /= nlay) read(modunit,'(a)') cbuf
         end if


      end do



1000  continue
      close(unit=modunit)


      return
      end subroutine gridlib_model_from_warp_grid


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

      subroutine gridlib_compute_gradient(sounit, k, dt, dz, v0, ier)

      implicit none

      integer, intent(in)    :: sounit
      real,    intent(inout) :: k
      real,    intent(in)    :: dt, dz, v0
      integer, intent(inout) :: ier

!---Local declarations
      real             :: epsilon, vtest, ktest1, ktest2
      real             :: dt1, dt2, dtnew
      integer          :: ntimes         

      ier=-1
      epsilon=1.e-6

      vtest=dz/dt
      if (vtest == v0) then
         k=0.
         return
      else if(vtest < v0) then
         ktest1=-v0/dz+epsilon
         ktest2=-1e-6
      else if(vtest > v0) then
         ktest1=1e-6
         ktest2=10.
      end if


      dt1=log(1.0+ktest1*dz/v0)/ktest1
      dt2=log(1.0+ktest2*dz/v0)/ktest2
      ntimes=0
100   continue
      ntimes=ntimes+1
      if(ntimes > 500) return
      k=ktest1+(dt-dt1)/(dt2-dt1)*(ktest2-ktest1)
      dtnew=log(1.0+k*dz/v0)/k
!     write(sounit,'(i5,7f10.6)')ntimes,ktest1,k,ktest2,dt1,dtnew,dt2,dt
      if( abs(dtnew-dt) < epsilon) goto 9999
      if( dt >= dtnew .and. dt <= dt1) then
         dt2=dtnew
         ktest2=k
      else if (dt >= dt2 .and. dt <= dtnew) then
         dt1=dtnew
         ktest1=k
      else
         return
      end if
      goto 100

9999  continue
      ier=0
      return
      end subroutine gridlib_compute_gradient


!********************************************************

      subroutine gridlib_read_well(iunit,                    &
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

!---Local declarations
      character(len=256) :: cbuf
      integer            :: ii    , i 

      ier=0
      open(unit=iunit,file=filein(1:ncfilein),  &
           status='old',err=9000)
      read(iunit,'(a)') desc
      ncdesc = len(trim(desc))

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),'(a)')source
      ncsource = len(trim(source))

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),'(a)')sourcefile
      ncsourcefile = len(trim(sourcefile))

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)xorg

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)yorg

      read(iunit,'(a)') cbuf
      F_or_M='F'
      if(index(cbuf,'FEET') == 0) F_or_M='M'


      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)dinc

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)dmin

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)dmax

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)velmin

      read(iunit,'(a)') cbuf
      ii=index(cbuf,'=')
      read(cbuf(ii+1:),*)velmax



      npts=0
100   continue
         i=npts+1
         if(i > nptsmax) goto 9000
         read(iunit,'(3f12.6)',end=200) xvel(i),yvel(i),vel(i)
         npts=npts+1
         goto 100

200   continue

      close(unit=iunit)


      return


9000  continue
      ier=-1
      return

      end subroutine gridlib_read_well


!********************************************************

      subroutine gridlib_write_well(iunit,                    &
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

!---Local declarations
      integer            :: i

      ier=0
      open(unit=iunit,file=fileout(1:ncfileout),  &
           status='unknown',err=9000)
      write(iunit,'(a)') desc(1:ncdesc)
      write(iunit,'(a,a)') 'SOURCE=',source(1:ncsource)
      write(iunit,'(a,a)')'SOURCE FILE=',sourcefile(1:ncsourcefile)
      write(iunit,*)' X LOCATION OF WELL = ',xorg
      write(iunit,*)' Y LOCATION OF WELL = ',yorg
      if(F_or_M == 'F' .or. F_or_M == 'f') then
         write(iunit,*)' UNITS=FEET'
         write(iunit,*)' DEPTH INCREMENT = ',dinc
         write(iunit,*)' TOP DEPTH = ',dmin
         write(iunit,*)' BOTTOM DEPTH = ',dmax
         write(iunit,*)' MINIMUM = ',velmin
         write(iunit,*)' MAXIMUM = ',velmax
      else
         write(iunit,*)' UNITS=METERS'
         write(iunit,*)' DEPTH INCREMENT = ',dinc
         write(iunit,*)' TOP DEPTH = ',dmin
         write(iunit,*)' BOTTOM DEPTH = ',dmax
         write(iunit,*)' MINIMUM = ',velmin
         write(iunit,*)' MAXIMUM = ',velmax
      end if


      do i=1,npts
         write(iunit,'(3f12.6)') xvel(i),yvel(i),vel(i)
      end do

      close(unit=iunit)


      return


9000  continue
      ier=-1
      return

      end subroutine gridlib_write_well


!********************************************************

      subroutine gridlib_set_verbose(verbose)

      implicit none

      integer,            intent(in)    :: verbose

      if (verbose > 0) then
         gridlib_verbose = verbose
      else
         gridlib_verbose = 0
      end if

      return
      end subroutine gridlib_set_verbose


!********************************************************
!---PROGRAM NAME:         gridlib_compute_velocity_torz
!
!---AUTHOR:               Kay Wyatt
!
!---CREATION DATE:        October 1995
!
!---DESCRIPTION:          This subroutine calculates the velocity at a given
!                         ix, iy, and depth location using data arrays created
!                         by reading a velocity model in MODSPEC format.
!                         INPUT:
!                          domain    - Use "1" for Depth domain velocity
!                                      Use "2" for Time domain velocity
!                         OUTPUT:
!                          velocity  - velocity at location ix,iy,depth
!                          layer     - layer number at location ix,iy,depth

      subroutine gridlib_compute_velocity_torz(domain,dmax,depth,velocity,  &
                                               layer,ix,iy,nx,ny,nlay,      &
                                               nlaymax,v,z,grad,vclipmin,   &
                                               vclipmax,znon)

      implicit none

      integer, intent(in)    :: domain
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

!---Local declarations
      real             :: graduse, deltaz   
      integer          :: i

      graduse=0.0

      velocity=znon

      if(depth <= z(1,ix,iy).and.z(1,ix,iy) /= znon) then
         velocity=v(1,ix,iy)
         if(grad(1,ix,iy) == znon) velocity=znon
         layer=1
      elseif(depth >= z(nlay,ix,iy).and.z(nlay,ix,iy) /= znon) then
         graduse=grad(nlay,ix,iy)
         deltaz=depth-z(nlay,ix,iy)
         if(domain == 1) then      !depth domain modspec
            velocity=v(nlay,ix,iy)+graduse*deltaz
         elseif(domain == 2) then  !time domain modspec
            deltaz=deltaz/2000.
            velocity=v(nlay,ix,iy)*exp(graduse*deltaz)
         endif
         if(graduse == znon.or.v(nlay,ix,iy) == znon) velocity=znon
         layer=nlay
      else
         do i=1,nlay-1
            layer=i
            if(depth >= z(i,ix,iy) .and.   &
               depth <= z(i+1,ix,iy).and.  &
               z(i,ix,iy) /= znon.and.z(i+1,ix,iy) /= znon) then
               graduse=grad(i,ix,iy)
               deltaz=depth-z(i,ix,iy)
               if(domain == 1) then      !depth domain modspec
                  velocity=v(i,ix,iy)+graduse*deltaz
               elseif(domain == 2) then  !time domain modspec
                  deltaz=deltaz/2000.
                  velocity=v(i,ix,iy)*exp(graduse*deltaz)
               endif
               if(graduse == znon.or.v(i,ix,iy) == znon) velocity=znon
               exit
            endif
         enddo
      endif

      return

      end subroutine gridlib_compute_velocity_torz


!********************************************************
!     gridlib_decode - decode parameters from input file

      subroutine gridlib_decode(luin, sounit, nfatal, nwarn, ninform, &
                                buffer, nbufchar, filespec, h)

      implicit none

      integer,          intent(in)    :: luin
      integer,          intent(in)    :: sounit
      integer,          intent(inout) :: nfatal, nwarn, ninform
      character(len=*), intent(inout) :: buffer
      integer,          intent(in)    :: nbufchar
      character(len=*), intent(in)    :: filespec
      type(gridlib_modspecheader_struct), intent(inout) :: h

      ! Local variables
      character(len=120) :: record
      character(len=16)  :: field
      integer  :: bufflen
      integer  :: ibgn, iend, iblank
      integer  :: i, j, ierr

      ! Empty the stdin file to get the global parameters.  Concatenate all
      ! records into a single read buffer, stripping trailing blanks on the
      ! fly.

      open(unit=luin,file=filespec,status='old',iostat=ierr)
      if (ierr /= 0) then
        write(sounit,*)
        write(sounit,*) 'gridlib_decode: Received error ', ierr, &
                        ' while trying to open file ', filespec
        write(sounit,*)
        nfatal = nfatal + 1
        return
      end if

      j=0
    1 read(luin,'(A)',end=2) record
      !write(sounit,'(A)') record
      i=len(record)
      do while( (record(i:i) == ' ') .and. (i > 1))
        i=i-1
      end do
      if (i <= 1) go to 1
      buffer(j+1:j+i)=record(1:i)
      j=j+i
      if (j > nbufchar) then
         write(sounit,*) 'gridlib_decode: character buffer not big enough'
         write(sounit,*) 'gridlib_decode: ', j, nbufchar
         nfatal = nfatal + 1
         return
      end if
      IF (buffer(j:j) /= ',') then
        j=j+1
        buffer(j:j)=','
      end if
      go to 1
    2 continue
      bufflen = j

      ! Convert the global parms to all upper case.  Note that this can only
      ! be done after filenames (or other case-sensitive parameters) have been
      ! recovered, since converting them to upper case would destroy them.

      call gridlib_upcase(buffer(1:bufflen), bufflen)

      call gridlib_keywrd(buffer(1:j), 'DOMAIN', ibgn, iend, iblank)
      if (iend > 0) then
        h%domain = buffer(ibgn:iend)
      else
        h%domain='DEPTH'
        write(sounit,'(A)') ' W -- domain keyword is missing, DOMAIN=DEPTH &
                            &is assumed'
        nwarn=nwarn+1
      end if

      call gridlib_keywrd(buffer(1:j), 'ZNON', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,*)h%znon
      else
        h%znon=-999
        write(sounit,'(A)') ' W -- ZNON keyword is missing, ZNON=-999 is &
                            &assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'A1', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%a1
      else
        h%a1=1
        write(sounit,'(A)') ' W -- A1 keyword is missing, A1=1 is assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'A2', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%a2
      else
        h%a2=0
        write(sounit,'(A)') ' W -- A2 keyword is missing, A2=0 is assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'A3', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%a3
      else
        h%a3=0
        write(sounit,'(A)') ' W -- A3 keyword is missing, A3=0 is assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'B1', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%b1
      else
        h%b1=0
        write(sounit,'(A)') ' W -- B1 keyword is missing, B1=0 is assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'B2', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%b2
      else
        h%b2=1
        write(sounit,'(A)') ' W -- B2 keyword is missing, B2=1 is assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'B3', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%b3
      else
        h%b3=0
        write(sounit,'(A)') ' W -- B3 keyword is missing, B3=0 is assumed'
        nwarn=nwarn+1
      end if

      call gridlib_keywrd(buffer(1:j), 'DZDEF', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,*)h%dzdef
      else
        h%dzdef=h%znon
        write(sounit,'(A)') ' W -- DZDEF keyword is missing, DZDEF=ZNON is used'
        nwarn=nwarn+1
      end if

      call gridlib_keywrd(buffer(1:j), 'NZDEF', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%nzdef
      else
        h%nzdef=nint(h%znon)
        write(sounit,'(A)') ' W -- NZDEF keyword is missing, NZDEF=ZNON is used'
        nwarn=nwarn+1
      end if

      call gridlib_keywrd(buffer(1:j), 'CPS_HDRA', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%cps_hdra
      else
        h%cps_hdra=8
        write(sounit,'(A)') ' W -- CPS_HDRA keyword is missing, CPS_HDRA=8 is &
                            &assumed'
        nwarn=nwarn+1
      end if
      call gridlib_keywrd(buffer(1:j), 'CPS_HDRB', ibgn, iend, iblank)
      if (iend > 0) then
        field=buffer(ibgn:iend)
        read(field,'(BN,I8)')h%cps_hdrb
      else
        h%cps_hdrb=7
        write(sounit,'(A)') ' W -- CPS_HDRB keyword is missing, CPS_HDRB=7 is &
                            &assumed'
        nwarn=nwarn+1
      end if

      write(sounit,'(I4,'' FATAL ERRORS'',I4,'' WARNINGS'', I4, &
        &'' INFORMATIONAL MESSAGES'')')nfatal, nwarn, ninform

      close(unit=luin)

      return

      end subroutine gridlib_decode


!********************************************************
!     gridlib_upcase - convert character string to uppercase

      subroutine gridlib_upcase(string, length)

      implicit none

      character(len=*), intent(inout) :: string
      integer,          intent(in)    :: length

      ! Local variables
      character(len=256), save :: map
      logical, save :: init = .true.
      integer       :: i, j

      ! Load the map if this is the initial call
      if (init) then
         do i = 1, 256
           map(i:i) = char(i-1)
         end do
         j = 65
         do i = 98, 123
            map(i:i) = char(j)
            j = j + 1
         end do
      end if

      ! Translate the character string
      do i = 1, length
         j = ichar(string(i:i)) + 1
         string(i:i) = map(j:j)
      end do

      return

      end subroutine gridlib_upcase


!********************************************************
!     gridlib_keywrd - parse keyword=value from card

      subroutine gridlib_keywrd(card, key, ibgn, iend, iblank)

      !      THIS ROUTINE SEARCHES FOR A KEYWORD ON A CARD AND RETURNS
      ! THE BEGINNING AND ENDING POSITION OF THE STRING VALUE.  THE
      ! KEYWORD MUST BE FOLLOWED BY AN EQUAL SIGN OR IT WILL NOT BE
      ! RECOGNIZED AS SUCH.
      !
      !      IF THE KEYWORD IS FOUND ON THE CARD THE STRING VALUE IS
      ! DELIMITED BY EITHER A COMMA OR THE LAST NON-BLANK CHARACTER
      ! ON THE CARD. IF THE KEYWORD IS NOT FOUND ON THE CARD, THEN iend
      ! IS RETURNED WITH A VALUE OF ZERO.
      !
      !      BLANKS MAY BE INSERTED BEFORE AND AFTER THE EQUAL SIGN WHICH
      ! FOLLOWS THE KEYWORD OR THEY MAY BE EMBEDDED IN THE STRING VALUE
      ! RETURNED.  THEY ARE NOT ALLOWED AS PART OF THE ACTUAL KEYWORD,
      ! AND LEADING AND TRAILING BLANKS ARE STRIPPED FROM THE STRING
      ! WHICH IS RETURNED.

      implicit none

      character(len=*), intent(in)  :: card, key
      integer,          intent(out) :: ibgn, iend, iblank

      ! Local variables
      integer :: i, j, itmp, ikey, ilen, icrd, istart

      ibgn   = 1
      iend   = 0
      iblank = 0

      ! STRIP OFF LEADING BLANKS FROM key
      itmp = 0
      do i=1,len(key)
         if (key(i:i) /= ' ') then
            ikey = i
            !print *,"Starting loc of key: ", ikey
            exit
         end if
         itmp = i + 1
      end do
      if (itmp > len(key)) then
         ! VALUE OF KEYWORD IS ALL BLANKS (NOT VALID)
         iend = 0
         return
      end if

      ! REMOVE TRAILING BLANKS FROM key
      do i=len(key),ikey,-1
         if (key(i:i) /= ' ') then
            ilen = i-ikey+1
            !print *,"length of key: ", ilen
            exit
         end if
      end do

      ! SEARCH FOR key ON card
   40 if (ibgn >= len(card)) return
      icrd = len(card)
      istart = index(card(ibgn:icrd),key(ikey:ikey+ilen-1))
      if (istart == 0) return
      ibgn = ibgn + istart - 1

      ! SEARCH FOR EQUAL SIGN FOLLOWING KEYWORD
      ibgn = ibgn+ilen
      do j=ibgn,len(card)-1
         if (card(j:j) == '=') then
            ibgn = j+1
            exit
         else if (card(j:j) /= ' ') then
            goto 40
         end if
      end do

      ! STRIP OFF LEADING BLANKS
      itmp = 0
      do i=ibgn,len(card)
         if (card(i:i) /= ' ') then
            ibgn = i
            exit
         end if
         itmp = i+1
      end do
      if (itmp > len(card)) then
         ! VALUE OF KEYWORD IS ALL BLANKS (NOT VALID)
         iend = 0
         iblank = 1
         return
      end if

      ! TRUNCATE KEYWORD BEFORE FIRST COMMA
      iend = len(card)
      j = index(card(ibgn:iend),',')
      if (j /= 0) then
         iend = ibgn+j-2
      end if
      ! REMOVE TRAILING BLANKS
      do i=iend,ibgn,-1
         if (card(i:i) /= ' ') then
            iend = i
            exit
         end if
      end do

      if (iend < ibgn) then
         ! VALUE OF KEYWORD IS ALL BLANKS (NOT VALID)
         iend = 0
         iblank = 1
      end if

      return

      end subroutine gridlib_keywrd


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module gridlib_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!


