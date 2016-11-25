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
! C P S   P R I M I T I V E
! Name       : MODGRID
! Category   : velocity
! Written    : 2001-02-22   by: RSDay
! Revised    : 2008-01-29   by: RSDay
! Maturity   : beta
! Purpose    : manipulate velocity models
! Portability: No known problems
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MODGRID
!  The modgrid object defines regular gridded data.  It carries a general
!  grid description for grids from 1D to 4D. It has information to
!  register the grid to seis data, and to an arbitrary XYZ frame of reference.
!
! A modgrid object can be created via either the modgrid_create function
! or by one of the overloaded modgrid_rd functions. File types that can be
! read to create a modgrid object are:
!   RMOD HGRID
!   RMOD LAYER (description only)
!   RMOD G3DL
!   GOCAD VOXET, GSURF, SGRID
!   CPS VELOCITY - ascii velocity function file
!   TRCIO files that were created by modgrid_wr
!   MODSPEC
!   MSGRID - a modspec binary 2D grid
!   SEGY - for segy traces that are on a regular x,y cmp grid
! Other methods exist to manipulate the modgrid parameters, and to store
! the grid to disk as a special(has a modgrid section header) trcio file.
!  SITI file (/apps/spspromax/2003.12/prod/linux/lib/libvfio.a)
!
! Objectives:
!   Support regular gridded models with arbitrary dimensionality
!   Register the grid with seismic data sets - Optional
!   Describe orientation-location of grid in XYZ space - Optional
!
!  Assumptions:
!  1. The grid description is not rotated relative to a seismic
!     data set. In particular hdwd(i) is consistent with n_grid(i),o_grid(i),
!     and d_grid(i). For instance with CPS headers (7,8) or (17,18), etc.
!     Traces may be spaced differently, but there should not be a rotation
!     between the model and the seismic data.
!  2. The grid bin spacing is regular
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
!     VARIABLE   DESCRIPTION
!     obj    ... an object of type(modgrid_struct)
!     fname  ... file containing velocity data
!     stdo   ... A fortran unit number for messages to output
!     name   ... a mneumonmic name for the grid object (len=64)
!     pname  ... name of the stored property
!                (e.g. velocity, VZIN,VTIN, density)
!     punits ... units of the stored property(e.g. m/s)(len=16)
!     rank   ... integer rank(e.g. a cube has rank = 3)
!     dfile  ... file containing velocity binary data
!                can be same as fname
!     wtype  ... word type of binary data.
!     ng(:)  ... grid sizes
!                (e.g. for cube ng(1:3)=(101,21,12))
!     og(:)  ... grid origins
!     dg(:)  ... grid bin widths
!     hdwd(:)... array 1:rank of CPS header word locations
!     axis   ... index of axis to get or set. 1=fastest, etc.
!     labels ... axis labels for grid axis in 3D space
!     oxyz   ... grid origin in 3D XYZ space
!     axis1  ... (X,Y,Z) vector for grid axis 1 (length and orientation)
!     axis2  ... (X,Y,Z) vector for grid axis 2
!     axis3  ... (X,Y,Z) vector for grid axis 3
!     u,v,w  ... triplet that indexes an entry into the data array.
!                w is ignored for rank 2 data arrays, etc.
!                index is for the grid in the create call
!     npts   ... maximum number of points to reference
!     stride ... stride through the data( > 0)
!     i_err  ... 0 = OK, -1 = ERROR
!                data will be null if an error occurs
!     slice  ... A slice index relative to the grid specified
!                in the create call.
!     sslice ... first slice index relative to the grid specified
!                in the create call. We can store a sub-slab starting
!                with index sslice.
!     nslice ... number of slices from the data. Ignored for rank<3
!
!                                     o   i     i    o     o     o
!     integer function modgrid_rddesc(obj,fname,stdo,dfile,wtype,rftype,
!      opt   opt
!      xhdr, yhdr)
!        - read the grid description, but not the data
!     type(modgrid_struct),pointer    :: obj
!     character(len=*),intent(in)     :: fname
!     integer,intent(in)              :: stdo
!     character(len=*),intent(out)    :: dfile
!     character(len=*),intent(out)    :: wtype
!     character(len=*),intent(out)    :: rftype
!     integer,optional,intent(in)     :: xhdr
!     integer,optional,intent(in)     :: yhdr
!
!                                 i     i    o    o    o     o
!     integer function modgrid_rd(fname,stdo,rank,name,pname,punits, &
!      o     o  o  o
!      hdwd, ng,og,dg)
!        - read the grid description and the binary data for the grid
!     character(len=*),intent(in)     :: fname
!     integer,intent(in)              :: stdo
!     integer,intent(out)             :: rank
!     character(len=*),intent(out)    :: name
!     character(len=*),intent(out)    :: pname
!     character(len=*),intent(out)    :: punits
!     integer,intent(out)             :: hdwd(:)
!     integer,intent(out)             :: ng(:)
!     real,intent(out)                :: og(:)
!     real,intent(out)                :: dg(:)
!
!                                 o   i     i    i      i
!     integer function modgrid_rd(obj,fname,stdo,sslice,nslice)
!        - read the grid description and the binary data for the grid
!     type(modgrid_struct),pointer    :: obj
!     character(len=*),intent(in)     :: fname
!     integer,intent(in)              :: stdo
!     integer,intent(in)              :: sslice
!     integer,intent(in)              :: nslice
!
!                                      b   i    i      i
!     integer function modgrid_rd_data(obj,stdo,sslice,nslice)
!     type(modgrid_struct),intent(inout) :: obj
!        - read some binary data from a grid file
!          (obj will contain the name of the data file after a call to rddesc)
!
!                                 i   i     i    opt
!     integer function modgrid_wr(obj,fname,stdo,oorder)
!     integer function modgrid_wr_gocad(obj,fname,stdo,oorder)
!        - Save the modgrid object to a trcio(or gocad voxet) file
!          oorder controls orientation of the output data. oorder
!          must be a permutation of the letters X, Y, and Z. It defaults
!          to the input order. oorder='ZXY' will cause the output
!          data to store the Z axis as the fastest dimension and Y
!          as the slowest. Saves only what is in memory.
!     type(modgrid_struct),intent(in) :: obj
!     character(len=*),intent(in)     :: fname
!     integer,intent(in)              :: stdo
!     character(len=*),optional,intent(in) :: oorder
!
!                          o    i     i     i      i
!     call modgrid_create (obj, rank, name, pname, punits, &
!       i   i   i   i
!       ng, og, dg, stdo)
!             - create an instance of a basic modgrid object
!     type(modgrid_struct),pointer  :: obj       ! arguments
!     character(len=*),intent(in)   :: name
!     character(len=*),intent(in)   :: pname
!     character(len=*),intent(in)   :: punits
!     integer,intent(in)            :: rank
!     integer,intent(in)            :: stdo
!     integer,intent(in)            :: ng(:)
!     real,intent(in)               :: og(:)
!     real,intent(in)               :: dg(:)
!
!                         b
!     call modgrid_delete(obj)
!             - frees resources tied to the modgrid object
!     type(modgrid_struct),pointer  :: obj       ! arguments
!
!                        i   i
!     call modgrid_print(obj,stdo)
!             - prints state of obj to fortran unit stdo
!
!                                 i     i    opt
!     function modgrid_ftype_read(fname,stdo,fsize) result(ftype)
!             - returns a string for the file type.
!               UNKNOWN if file type is not recognized.
!               fsize is the file size in bytes(no extents)
!
!     logical function modgrid_trace_ordered(obj)
!             - returns true if it detects that the object will
!               store its data in trace storage order.
!     logical function modgrid_time_ordered(obj)
!             - returns true if it detects that the object will
!               store its data in time slice storage order.
!
!                                i    o    o    o
!     i_err = modgrid_data_stats(obj, dmin,dmax,davg,gmax)
!             - find min, max, and average of the data in memory
!             - gmax(*) max gradient estimate for axis i<=3
!     i_err = modgrid_data_stats(fname, dmin,dmax,davg,gmax)
!             - find min, max, and average of the data in file fname
!               (where fname may point to a seperate data file)
!
!                            i     i    b
!     i_err = modgrid_paint_by_obj(obj,maxmem, stdo, ovors,ivors, &
!       hx_out, hy_out,            &
!       n1_out, o1_out, d1_out,    &
!       n2_out, o2_out, d2_out,    &
!       i       i       i       o       o        b
!       n3_out, o3_out, d3_out, ocube,  out_xyz, vtyp_out)
!       - paint ocube with values from input object obj
!         maxmem = maximum number of words to use for input cube storage
!         *** set maxmem large enough to hold at least 2 planes ***
!         out_xyz = some permutation of 'XYZ'. Identifies the 1,2,3 axis
!                   as X, Y, and Z
!         vtyp_out= specify the desired output type. Is ignored unless
!                   the input model is of type CPSVEL. Returns the input
!                   type if it is initialized as ' '.
!         hx_out  = CPS header consistent with the 'X' axis
!         hy_out  = CPS header consistent with the 'Y' axis
!         ovors   = V or S indicating output domain is Velocity or Slowness
!         ivors   = V or S indicating interpolation domain
!         ocube(n1_out,n2_out,n3_out) ... storage for output data
!         n1_out, etc = output grid description
!     i_err = modgrid_paint_by_file(file,maxmem, stdo, ovors,...)
!       - same as paint_by_obj but obj is replaced by an input file name
!
!     call modgrid_set_hdwd(obj,hdwd)
!                           i   o
!     call modgrid_get_hdwd(obj,hdwd)
!             - store or retreive header indices to register grid to seismic
!
!                               b   i     i    i  i  i
!     call modgrid_set_griddesc(obj,axis, hdwd,ng,og,dg)
!             - get or set the grid description for a grid axis
!                               i   o     o    o  o  o
!     call modgrid_get_griddesc(obj,axis, hdwd,ng,og,dg)
!          - or -
!     call modgrid_get_griddesc(obj,name,pname,punits,rank,&
!      hdwd,ng,og,dg)
!             - same as above but hdwd(:),ng(:),og(:),dg(:)
!
!     call modgrid_get_name_rank(obj,name,pname,punits,rank)
!
!                          i   i       i     i     i     i
!     call modgrid_set_xyz(obj,labels, oxyz, axis1,axis2,axis3)
!     call modgrid_get_xyz(obj,labels, oxyz, axis1,axis2,axis3)
!             - set/get grid location-orientation in XYZ space
!     type(modgrid_struct),intent(inout) :: obj       ! arguments
!     character(len=*),intent(in)        :: labels(:)
!     real,intent(in)                 :: oxyz(:)
!     real,intent(in)                 :: axis1(:)
!     real,intent(in)                 :: axis2(:)
!     real,intent(in)                 :: axis3(:)
!
!     integer function modgrid_put_data(obj,data,npts,stride,u,v,w)
!             - store from data to internal storage
!               start at grid index (u,v,w)
!
!                                       b   i  i  i      i      opt
!     integer function modgrid_put_data(obj,n1,n2,sslice,nslice,data)
!             - hand data to the modgrid object for storage.
!     type(modgrid_struct),intent(inout) :: obj       ! arguments
!     integer,intent(in)                 :: n1,n2,sslice,nslice
!     real,optional,intent(in)           :: data(*)
!               allocates memory and copys data.
!               storage from fast to slow is n1:n2:nslice.
!               a real array with n1*n2*nslice elements
!     n1     ... fast storage dimension(same as ng(1)?)
!     n2     ... storage dimension(same as ng(2)?)
!
!                              b   i    i  i  i      i
!     i_err = modgrid_rep_data(obj,data,n1,n2,sslice,nslice)
!             - replace data that the modgrid object is storing.
!               copys data to the array allocated by the put call.
!               storage from fast to slow is n1:n2:nslice.
!
!                              b   i    i    i      i i i
!     i_err = modgrid_rep_data(obj,data,npts,stride,u,v,w)
!             - replace data that the modgrid object is storing.
!     real,intent(in)   :: data(:)... a real array with npts elements
!     npts   ... number of points to copy from data to internal store.
!
!     Overloaded modgrid_get_data:
!
!                              i   o
!     i_err = modgrid_get_data(obj,data)
!             - return pointer to the entire data array
!     real,pointer                       :: data(:)
!
!
!                              i   o     i
!     i_err = modgrid_get_data(obj,data, slice)
!             - return a pointer to a slice of the data array
!             - returns pointer to the entire array for rank < 3
!     real,pointer                       :: data(:)
!
!                              i   o    b     i       i i i
!     i_err = modgrid_get_data(obj,data,npts, stride, u,v,w)
!             - return a copy of a vector out of the data array
!               grabs npts values starting at (u,v,w) using stride
!     real                              :: data(:)
!
!     nel = modgrid_size(obj)
!     nel = modgrid_size8(obj)
!             - nel is the size of the grid in words
!                        b   o
!     call modgrid_recip(obj,i_err)
!             - take reciprocal of the data in memory(velocity to slowness)
!     call modgrid_add(obj, add)
!             - add a constant to the data in memory
!     call modgrid_scale(obj,scale)
!             - multiply the data in memory by a scalar
!     i_err =  modgrid_interpolate(obj,mdata, data,&
!       n_out,o_out,d_out)
!             - interpolate the data in memory and put the output into data.
!               n_out,o_out,d_out describe the output grid.
!     integer,intent(in)                 :: mdata  size of data
!     real,intent(out)                   :: data(*)
!     integer,intent(in)                 :: n_out(*) !at least 3
!     real,intent(in)                    :: o_out(*)
!     real,intent(in)                    :: d_out(*)
!
!     i_err = modgrid_get_trslice(obj, mwrds, datao,&
!     slice, dim1, dim2)&
!            - fill datao with transpose of the stored data slice
!
!                                        i   o     i     i
!     integer function modgrid_copy_desc(obj,oobj, full, oorder) result(status)
!            - create a copy of the input grid object.
!              data pointers are not included in the copy
!     type(modgrid_struct),intent(in) :: obj  input object
!     type(modgrid_struct),pointer    :: oobj output object copied from input
!     integer,intent(in)              :: full
!     character(len=*),intent(in)     :: oorder
!     full = 1 grid description copy is based on the full input grid
!     full = 0 grid description copy is based on only the data that is loaded
!            in memory, which can be a subset of the full grid.
!     oorder  controls the axis ordering of the copy. For instance oorder='XYZ'
!     implies X is the fast storage order and Z is the slowest. The axis labels
!     determine which are the X, Y, and Z axis. oorder must be a permutation
!     of the letters X, Y, and Z.
!
!      C accessible helper function in modgrid_frou.f90
!      subroutine modgrid_file_info(ifname,iftype,iascii,&
!      rank,ng,og,dg,hd,&
!      label1,label2,label3,&
!      unit1,unit2,unit3,&
!      xhscan, yhscan,ixyz)
!            - return a character string with modgrid file global information
!              and explicit axis information in arguments
!       integer,intent(in)     :: ifname(*)
!       integer,intent(inout)  :: iftype(*)
!       integer,intent(inout)  :: iascii(*)
!       integer,intent(inout)  :: rank,ng(*)
!       real,intent(out)       :: og(*),dg(*)
!       integer,intent(out)    :: label1(*)
!       integer,intent(out)    :: label2(*)
!       integer,intent(out)    :: label3(*)
!       integer,intent(in)     :: xhdr
!       integer,intent(in)     :: yhdr
!-------------------------------------------------------------------------------
!</calling_doc>d.


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 86. 2008-01-29  RSDay        Added optional arg to modgrid_open_binslice.
!                              Fixes warning from trcio_update_header_private
!                              Split off new modules modgmem and model.
!                              Added region functions, and more convenience 
!                              functions. Some support for Gocad GSURF and
!                              SGRID objects.
! 85. 2007-10-23  RSDay        Multi-attribute output. modgrid_saveas becomes
!                              obsolete. voxet region information.
!                              Property name added to output file names.
! 84. 2007-09-20  RSDay        Fix feature in modgrid_rddesc_cvf.
! 83. 2007-09-13  RSDay        More changes to support multi-attribute models
! 82. 2007-08-28  RSDay        Add support for multi attribute voxet models.
!                              Modified modgrid_rddesc_gocad. Added
!                              modgrid_model_rddesc, modgrid_rddesc_model.
! 81. 2007-07-26  RSDay        Fix problem with TRCIO recognition 
! 80. 2007-07-24  RSDay        Add recognition of modspec 2D binary grid, 
!                              modgrid_rddesc_msgrid, modspec_rd_msgrid.
!                              Functions to get and set nil value.
! 79. 2006-11-07  RSDay        more robust modgrid_rddesc_gocad function.
! 78. 2006-10-31  RSDay        Fix a modspec problem related to last REV update
! 77. 2006-10-24  RSDay        Set endian flag for modspec grids. Set grid
!                              transform from modspec input. Modify
!                              modgrid_paint functions to handle input object
!                              with an inverted axis.
! 76. 2006-10-17  Hanson       Modify prints.
! 75. 2006-09-21  Hanson       Change interpolate calls.
! 74. 2006-09-14  RSDay        Better parsing of Voxet headers
! 73. 2006-08-24  D. Glover    Added NULLIFY statements for Intel compiler.
! 72. 2006-08-17  RSDay        Use grid transform to set header words for TRCIO
!                              output files
! 71. 2006-05-09  RSDay        Changes to modgrid_get_pointsr, 
!                              modgrid_paint_simple, modgrid_paint_by_obj
! 70. 2006-03-21  RSDay        Fixed modgrid_rd_modspec coord. problem,
!                              and modgrid_regrid. Add modgrid_regrid_modspec.
!                              Fixed modgrid_is_in_mema.
! 69. 2006-03-02  RSDay        Enhanced output functions. modgrid_open_binfile
!                              modgrid_close_binfile , modgrid_wr_binslice.
!                              Check data ownership before deletes.
! 68. 2006-02-23  RSDay        Fix unit grid shift for SITI models.
!                              Fix modgrid_regrid to preserve grid transform
!                              from an input model.
! 67. 2006-02-16  RSDay        Improved parseing of voxet header. Handle 
!                              8bit,and 16bit voxet data. Modified 
!                              modgrid_set_dskdata,modgrid_dskdata.
! 66. 2006-01-26  RSDay        Fix bug in scans of trcio and segy files that
!                              reported false axis size for 3rd dimension.
!                              Preserve axis units information when available.
!                              Added function modgrid_tovxeta. 
! 65  2006-01-17  RSDay        Preserve and use grid transform information.
!                              from trcio, modspec, and SITI files. Changes
!                              to modgrid_regrid,modgrid_to_ascii,modgrid_print,
!                              modgrid_rddesc_trcio, modgrid_rddesc_siti,
!                              modgrid_to_siti. Fix problems with models that
!                              have over 2 billion grid points.
! 64  2006-01-10  RSDay        Removed debug print in paint_simple
! 63  2006-01-09  RSDay        Fix problem related to modspec models with
!                              inverted x or y axis.
! 62  2005-11-22  RSDay        Fixed segy recognition problem.
!                              Changed modgrid_regrid to support byte swap data.
! 61  2005-10-25  RSDay        Added subdomain decomposition functions.
! 60  2005-10-13  RSDay        Fixed problem with input of SITI hdr file.
! 59  2005-10-06  RSDay        Increased supported file name size
! 58  2005-09-20  RSDay        Modified Gocad Voxet ouput.
! 57  2005-08-30  RSDay        Correct modgrid_regrid for 32 bit problem. Can
!                              now transpose files > 1 GWords.
! 56  2005-08-18  RSDay        Delete unused variables. Changes for SITI files.
! 55  2005-07-28  RSDay        Recognize SITI HDR files.
!                              Add axis units to object
! 54  2005-05-31  RSDay        More robust segy scans.
! 53  2005-04-12  RSDay        Fixed modgrid_regrid for modspec input files
! 52  2005-01-31  RSDay        Modified modgrid_regrid, modgrid_paint_by_file
!                              logic for modspec files. Add
! 51  2004-10-26  RSDay        Modified modgrid_rd_modspec
! 50  2004-09-16  RSDay        More robust SU recognition.
! 49  2004-08-31  RSDay        Added recognition of SU files.
! 48  2004-05-25  RSDay        Added utility function for getting and setting
!                              object, property and units names.Added
!                              modgrid_wr_header,  modgrid_model_consistent,
!                              modgrid_model_comensurate
! 47  2004-03-30  RSDay        Fix memory problem in modgrid_print.
! 46  2004-03-19  RSDay        Overloaded modgrid_model_add_property. Added
!                              modgrid_create_from_data. modgrid_get pname
!                              changed to a subroutine.
! 45  2004-03-16  RSDay        nullify unassigned pointers in
!                              modgrid_put_data_iclip. Removed modgrid_file_info
! 44  2004-03-02  RSDay        Fixed error in modgrid_regrid for trcio output
!                              file. More economical scan of large CPSVEL files
! 43  2004-02-19  RSDay        paint_by functions will use at least enough
!                              memory to hold 2 planes of data. MOD_RNIL
!                              parameter introduced. modgrid_clip_scale and
!                              modgrid_data_stats, modgrid_recip will ignore
!                              MOD_RNIL data values. modgrid_recip will also
!                              ignore zero values.
! 42. 2004-02-18  RSDay        Corrected modgrid_rd_cvf setting of logical flag
!                              that was causing vel file slowness,velocity mixup
! 41. 2004-02-16  RSDay        Optional error flag added to modgrid_recip.
!                              More robust checks for models with zero values.
! 40. 2004-02-12  RSDay        Better recognition of SEGY. Raw trcio
!                              data will default to VZIN type and post warning.
! 39. 2004-01-06  RSDay        All allocate calls do a status check
! 38. 2003-12-22  RSDay        Fix bug caught by portland compiler.
! 37. 2003-12-12  RSDay        Added modelstructure and model functions to
!                              support multi component models. Added
!                              modgrid_data_zbnd function. Added optional
!                              clip and scale arguments to modgrid_regrid.
!                              Added optional pname arg to modgrid_create_const
! 36. 2003-10-21  RSDay        Fixed extent size for double precision in
!                              modgrid_regrid. Output will be 1 chunk.
!                              modgrid_
! 35. 2003-10-16  RSDay        Fixed double precision arithmetic in rddesc
! 34. 2003-10-15  RSDay        Fixed segy reads for file size problems.
! 33. 2003-10-08  RSDay        Recognize new self defining CPS VEL files.
!                              Fixed problem with maxpicks allocation for
!                              CPSVEL files.
! 32. 2003-10-06  RSDay        Use 2gig + versions of cio_fseek amd cio_ftell.
!                              Fixed for files greater than 2 gig in size.
! 31. 2003-08-15  RSDay        Grid coordinates passed to modspec_getv are
!                              in CPS grid coordinates. Modspec module now
!                              handles the translation.
! 30. 2003-08-07  RSDay        Use new modspec function modspec_get_coef_full.
! 29. 2003-07-31  RSDay        Fix unitialized variable in modgrid_trcio_scan.
!                              Default scanning headers=7&8 rather than 17&18.
! 28. 2003-07-24  RSDay        Better recognition od modspec format.
! 27. 2003-07-17  RSDay        Revisions to modgrid_paint.. and modgrid_regrid
!                              for improved memory usage.
!                              Added is_inverted flag to track data state.
!                              Fixed non-zero z-origin and other problems
!                              related to modspec support. More versions of the
!                              modgrid_rddesc function. Improved the
!                              modgrid_rep_points,modgrid_get_points functions.
!                              Eliminated explicit CPS header indices. Added
!                              vtyp_out argument to paint and regrid functions
! 26. 2003-06-30  RSDay        Fixed modgrid_wr_trcio_trace to properly set
!                              headers for transposed x-y axis.
!                              modgrid_rd_modspec & modgrid_rddesc_modspec can
!                              handle modspec files with inverted axes for the
!                              grid units. Added modgrid_trcio_scan.
! 25. 2003-06-18  RSDay        Fixed memory problem in modgrid_regrid. Use
!                              more robust version of cio_ftell. Added
!                              modgrid_xangle.
! 24. 2003-05-27  RSDay        More intelligence in modgrid_paint when input is
!                              a modspec file. Added modgrid_set_modspec_zgrid
!                              modgrid_create_names, mogrid_regrid. Fixed bug
!                              in modgrid_paint_by_obj.
! 23. 2003-05-09  RSDay        Added modgrid_paint_by_file and
!                              modgrid_paint_by_obj
! 22. 2003-04-22  RSDay        Added ability to input SEGY files. Modfied
!                              modgrid_rddesc,modgrid_ftype,modgrid_set_dskdata,
!                              modgrid_file_info. Changed internal structures.
!                              Added modgrid_rddesc_segy, modgrid_rd_segy.
! 21. 2002-12-02  RSDay        Recognize and read MODSPEC velocity format which
!                              is a heritage Phillips layered velocity model.
!                              modgrid_rddesc_modspec, modgrid_rd_modspec added.
!                              optional argument nz and dz added to the function
!                              modgrid_saveas
! 20. 2002-08-05  RSDay        Use mth_module for binning calculations
! 19. 2002-06-25  Goodger      If grid file name in the hgrid file does not
!                              have a full path, prepend the path from the
!                              hgrid file.
! 18. 2002-06-12  RSDay        Check for open&write errors in modgrid_saveas.
! 17. 2002-04-18  RSDay        Fixed 80 byte clip in modgrid_rddesc_gocad
!                              Set tmin,tmax for trcio output.
! 16. 2002-02-14  RSDay        80 byte clip fixed in modgrid_to_voxet .
!                              Window clipping features added to reads.
!                              Added modgrid_clip_limits. More overloaded
!                              versions of modgrid_put_data, modgrid_rd_data.
!                              modgrid_put_data reuses memory if possible.
! 15. 2001-12-28  Stoeckley    Change argument name UNITS to DUNITS in call
!                               to velio_scan_alloc.
! 14. 2001-11-20  RSDay        Fixed bug in modgrid_data_stats for large models.
! 13. 2001-11-15  RSDay        Fix to avoid extents on transpose of large files.
! 12. 2001-11-06  RSDay        Corrected grid origin problem for hgrid files.
! 11. 2001-10-29  RSDay        Added modgrid_create_const,modgrid_array_size.
!                              Added modgrid_read, modgrid_permute,
!                              modgrid_set_dskdata, modgrid_get_data_mem,
!                              modgrid_is_in_mem
!                              Overloaded modgrid_xyz_order.
!                              Modified modgrid_data_stats.
!                              Use swap module for big endian output.
! 10. 2001-10-04  RSDay        Added new type, prop_cvfstore_struct, to support
!                              cps velocity function files. (file type=CPSVEL)
!                              Added modgrid_cvf_delete,modgrid_rddesc_cvf,
!                              modgrid_rd_cvf. Fixed modgrid_data_stats.
!                              Fixed modgrid_header_to_string.
!  9. 2001-09-04  RSDay        Enhanced routine modgrid_copy_desc and
!                              renamed modgrid_transpose_file to
!                              modgrid_saveas.
!                              Added optional argument to modgrid_rddesc.
!                              Added modgrid_get_zslices, modgrid_get_xyz,
!                              modgrid_all_in_mem, modgrid_xyz_order,
!                              modgrid_build_map, modgrid_map_address,
!                              modgrid_map_toseq, modgrid_wr_trcio_trace,
!                              modgrid_wr_trcio_trace,modgrid_wr_trcio_header,
!                              modgrid_write_buffer.
!  8. 2001-08-07  RSDay        Fixed problems in modgrid_wr & modgrid_wr_gocad
!                              when extracting subsets from larger volumes.
!                              Argument addition and fixes to modgrid_copy_desc
!                              so it can handle sub-cube extraction from a cube
!                              Gradient estimate added to modgrid_data_stats.
!                              modgrid_print modified. modgrid_file_info added.
!                              Fixed modgrid_rddesc_gocad so grid origin is set
!                              properly when reading in a voxet.
!  7. 2001-07-19  RSDay        Parsing of gocad header files case insensitive
!  6. 2001-07-03  RSDay        Fixed parsing problems with gocad files
!  5. 2001-06-26  RSDay        Add functions modgrid_rd_data, modgrid_datafile,
!                              modgrid_ftype, modgrid_transpose_file,
!                              modgrid_data_stats(overloaded),modgrid_size
!                              modgrid_trace_ordered, modgrid_time_ordered,
!                              modgrid_string_to_header,
!                              modgrid_header_to_string
!  4. 2001-05-30  RSDay        Corrected modgrid_rd_trcio. Was checking
!                              wrong variable for an error condition on
!                              open attempts.
!  3. 2001-04-25  RSDay        Added functions modgrid_recip,modgrid_scale,
!                              modgrid_add, modgrid_data_avg
!  2. 2001-03-29  RSDay        Support for gocad output format.
!                              (modgrid_wr_gocad, modgrid_to_voxet)
!                              Fixed problem with modgrid_get_pointsr?
!                              Added modgrid_get_xsect.
!                              modgrid_find_ijk debugged.
!  1. 2001-02-22  RSDay        Initial version
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
!
!-----------------------------------------------------------------------------
! Information Categories
!    Global       - Parameters that pertain to any layered or gridded model
!                   e.g. rank, name, type
!    ModelLimits  - Defines the maximum size of the model window
!    GridModel    - Defines a regular gridded model
!    LayerModel   - Defines a model composed of connected regions
!
!-----------------------------------------------------------------------------
!    Global
! rank      Dimensionality of the model(1-4)
!           1-3 are static physical models. rank 4 is will be
!           an animation of a 3D model or a 4D data set
! modname   Model name
! modtype   Model type(e.g. GRID,LAYER,ZGRID,3DSHOTS)
!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
!    ModelLimits
!  hdwrd(i)  Header to use for registering model window to seismic.
!            -1 implies no matching header(e.g. time-depth axis)
!  label(i)  Ascii label for axis i.
!  modmin(i) Model window minimum for axis i.
!  modmax(i) Model window maximum for axis i.
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!    ExternRepresentation
!  datafile  name of an optional binary data file(SAME,NONE are legitimate)
!  databits  number of bits in the data words (1,8,16,32,64)
!  datapos   byte offset to data in datafile
!  endian    endian behavior of the data (0:little, 1:big, -1:unknown)

      module modgrid_module
      use modgmem_module
      use cardset_module
      use path_module
      use string_module
      use wrdc_module
      use named_constants_module
      use interpolate_module
      use mth_module
      use grid_module
      use modspec_module
      use pcpsx_module
      use trcio_module
      use cio_module
      use cpsio_module
      use velio_module
      use velutil_module
      use swap_module
      use segy_module
      implicit none
       private

! modgrid methods - The API to the outside world
       public  :: modgrid_create
       public  :: modgrid_create_const
       public  :: modgrid_create_from_data
       public  :: modgrid_initialize     !set default grid values
       public  :: modgrid_delete

       ! region methods
       public  :: modgrid_create_rgndata
       public  :: modgrid_delete_rgndata
       public  :: modgrid_set_rgndata
       public  :: modgrid_get_rgndata
       public  :: modgrid_rgndata_str
       !
       public  :: modgrid_modgrids_get

       !
       public  :: modgrid_file_size
       public  :: modgrid_ftype          !return file format, e.g. TRCIO
       public  :: modgrid_ftype_read     !return file format, e.g. TRCIO
       public  :: modgrid_memdata        !get memdata info

       public  :: modgrid_dskdata        !get disk info
       public  :: modgrid_set_dskdata    !set disk paramters
       public  :: modgrid_datafile       !dskdata data file name
       public  :: modgrid_set_datafile
       public  :: modgrid_headfile       !dskdata header file name
       public  :: modgrid_set_headfile
       public  :: modgrid_pointfile       !dskdata header file name
       public  :: modgrid_set_pointfile
       public  :: modgrid_set_endian
       public  :: modgrid_endian
       public  :: modgrid_palign

       public  :: modgrid_create_names   !get data file name
       public  :: modgrid_copy_desc      !make copy of the grid description
       public  :: modgrid_get_pname      !property name
       public  :: modgrid_pname          !property name
       public  :: modgrid_set_pname      !property name
       public  :: modgrid_get_punits     !get property units name
       public  :: modgrid_set_punits     !set property units name
       public  :: modgrid_set_aunit      !set axis units
       public  :: modgrid_get_aunit      !set axis units
       public  :: modgrid_get_name       !get object name
       public  :: modgrid_set_name       !set object name
       public  :: modgrid_set_rnil       !set real nil value
       public  :: modgrid_get_rnil       !get real nil value
       public  :: modgrid_wr
       public  :: modgrid_wr_gocad
       public  :: modgrid_wr_header
       public  :: modgrid_wr_trcio_trace
       public  :: modgrid_wr_cvf
       public  :: modgrid_rd             !read grid description and data
       public  :: modgrid_read           !read grid description and data
       public  :: modgrid_rddesc_verbose !read the grid description(grid info)
       public  :: modgrid_rddesc_modgrids
       public  :: modgrid_rddesc         !read the grid description(grid info)
       public  :: modgrid_rd_data        !read the data only
       public  :: modgrid_paint_by_obj   !paint one grid onto another
       public  :: modgrid_paint_by_file  !paint one grid onto another
       public  :: modgrid_regrid         !paint grid and save to file
       public  :: modgrid_regrid_prop    !paint grid and save to file
       public  :: modgrid_get_zslices    !read and interpolate a z-slice
       public  :: modgrid_saveas         !reorder and save data file
       public  :: modgrid_trace_ordered  !true if data is trace ordered
       public  :: modgrid_time_ordered   !true id data stored as t or z slices
       public  :: modgrid_clip_limits    !translate clip window to grid nodes
       public  :: modgrid_clip_limit     !translate clip window to grid nodes
       public  :: modgrid_print          !ascii report of grid parameters
       public  :: modgrid_data_stats     !scan data for min,max,lav
       public  :: modgrid_data_zbnd
       public  :: modgrid_data_stats_mem     !scan data for min,max,lav
       public  :: modgrid_data_stats_dsk     !scan data for min,max,lav
       public  :: modgrid_data_stats_prop
       public  :: modgrid_to_ascii       !binary to ascii string
       public  :: modgrid_to_cards       !binary to ascii cards
       public  :: modgrid_ascii_to_cards !ascii string with LFs to card images
       public  :: modgrid_to_voxet
       public  :: modgrid_voxet_body
       public  :: modgrid_voxet_property
       public  :: modgrid_from_ascii     !ascii string to binary
       public  :: modgrid_set_xyz        !set grid location-orientation in XYZ
       public  :: modgrid_get_xyz        !get grid location-orientation in XYZ
       public  :: modgrid_xangle         !get x axis rotation angle
       public  :: modgrid_set_hdwd       !set headers,register grid to seismic
       public  :: modgrid_string_to_header
       public  :: modgrid_get_name_rank  !get name and rank
       public  :: modgrid_get_hdwd
       public  :: modgrid_set_modspec
       public  :: modgrid_get_modspec
       public  :: modgrid_set_modspec_zgrid !set grid description for z axis
       public  :: modgrid_set_griddesc   !set grid description for 1 axis
       public  :: modgrid_get_griddesc   !get grid description for 1 axis
       public  :: modgrid_put_data       !store grid data for the grid object
       public  :: modgrid_use_data       !store grid data for the grid object
       public  :: modgrid_get_data       !get access to the data in memory
       public  :: modgrid_get_xsect      !get a slice from memdata
       public  :: modgrid_interpolate    !3D interpolation
       public  :: modgrid_recip          !take reciprocal of the data
       public  :: modgrid_scale          !multiply data by scale
       public  :: modgrid_add            !add a constant to the data
       public  :: modgrid_data_avg       !slice averages of data
       public  :: modgrid_size           !model grid size in word units
       private :: modgrid_size8          !model grid size in word units
       public  :: modgrid_has_data       !tests for data in memory
       public  :: modgrid_all_in_mem     !tests if entire model is in memory
       public  :: modgrid_is_in_mem      !tests if needed data is in memory
       public  :: modgrid_xyz_order      !maps XYZ labels to axis
       public  :: modgrid_build_xyz_map  !maps XYZ srting
       public  :: modgrid_get_buffer     !get access to ascii file header
       public  :: modgrid_toepsdel       !vx,vz,eps to eps,delta
       public  :: modgrid_tovxeta        !vz,eps,del to vx,vz,eta 

       public  :: modgrid_partition
       public  :: modgrid_bricks
       public  :: modgrid_brick_info
       public  :: modgrid_brick_gtos
       public  :: modgrid_set_grid
       public  :: modgrid_get_grid

       public  :: modgrid_wr_trcio_header
       public  :: modgrid_open_binfile
       public  :: modgrid_close_binfile
       public  :: modgrid_wr_binslice
       public  :: modgrid_regrid_modspec
       public  :: modgrid_segy_scan

       character(len=100),save,public :: modgrid_ident = &
       "$Id: modgrid.f90,v 1.4 2008/02/15 19:49:48 mengewm Exp $"

       character(len=16)    :: begtag    !beginning tag of disk header file
       character(len=16)    :: endtag    !ending tag of disk header file

! Data storage object for the external storage of the grid data.
      type,public :: prop_dskstore_struct
        double precision  :: fsize
        character(len=8)  :: ftype      !file format
        character(len=160):: headfile   !can be same as datafile
        character(len=160):: datafile   !property file, blank is legit
        character(len=160):: pointfile  !sgrid points file
        character(len=8)  :: datafrmt   !i.e. IEEE,IBM,SEGY,ASCII
        character(len=8)  :: dataname   !i.e. REAL,SHORT,CHAR
        integer           :: databits   !element bit size
        integer           :: datapos    !offset to data in datafile
        integer           :: endian     !0,1
        real              :: datascale  !for gocad linear function scaling
        real              :: dataadd    !for gocad linear function scaling
      end type prop_dskstore_struct

      type,public :: region_struct
        character(len=160):: flagfile
        integer           :: flag_off
        integer           :: flag_esize
        integer           :: flag_bit_len
        integer           :: rcnt
        integer           :: rbits(40)
        character(len=32) :: rnames(40)
      end type region_struct

      type,public :: prop_cvfstore_struct
        integer           :: nvfun      !set if ftype=CPSVEL
        character(len=8)  :: v_type
        integer           :: hx
        integer           :: hy
        integer           :: nx_bins
        integer           :: ny_bins
        integer           :: maxpicks
        real              :: nmosign
        real              :: nmoexp
        real,pointer      :: x_coords(:)   !nvfun
        real,pointer      :: y_coords(:)   !nvfun
        real,pointer      :: x_bins(:)     !nx_bins
        real,pointer      :: y_bins(:)     !nx_bins
        real,pointer      :: vel(:,:,:)    !maxpick,nx_bin,ny_bins
      end type prop_cvfstore_struct

! Data storage object for the memory storage of the grid data.
!      type,public :: prop_memstore_struct
!       private
!       character(len=64) :: pname        !attribute name(e.g. velocity)
!       character(len=16) :: punits       !attribute units(e.g. m/s)
!       integer           :: bits         !8,16,32
!       integer           :: axis         !slices normal to axis in memory
!       integer           :: org1         !start index of dim1
!       integer           :: org2         !start index of dim2
!       integer           :: org3         !start index of dim3
!       integer           :: dim1         !1st dimension of slice
!       integer           :: dim2         !2nd dimension of slice
!       integer           :: dim3         !number of slices in memory
!       real,pointer      :: data(:)      !buffer holding slices
!       double precision,pointer    :: headers(:)   !optional buffer
!       logical           :: is_inverted  !data state flag
!       logical           :: owns_data    !data state flag
!      end type prop_memstore_struct


! A generic description of a grid object
       type,public :: modgrid_struct
        private
        integer              :: stdo
        integer              :: rank      !dimensionality of the grid
        character(len=64)    :: name      !name of the grid
        character(len=64)    :: pname     !name of the grid attribute
        character(len=16)    :: punits    !attribute units(e.g. m/s)
        character(len=8 )    :: palign    !property alighnment for SGRID
        integer              :: hdwd(4)   !headers to register axis to seismic
        integer              :: n_grid(4) !number of points for grid axis
        real                 :: o_grid(4) !origins for grid axis
        real                 :: d_grid(4) !bin widths for grid axis
        character(len=4)     :: a_unit(4) !axis unit(M,Ft,S,U...)
        character(len=32)    :: label(3)  !axis labels
        real                 :: oxyz(3)   !grid origin in XYZ space
        real                 :: axis1(3)  !axis 1 in XYZ space
        real                 :: axis2(3)  !axis 2 in XYZ space
        real                 :: axis3(3)  !axis 3 in XYZ space
        character(len=8)     :: keys(14)  !decode keywords for grid parameters
        character(len=8)     :: fmts(14)  !parameter types(A-array, S-Scalar)
        integer              :: inil
        real                 :: rnil
        type(prop_memstore_struct),pointer  :: memdata
        type(prop_dskstore_struct),pointer  :: dskdata
        type(prop_cvfstore_struct),pointer  :: cvfdata
        type(modspec_struct),pointer        :: mspdata
        type(grid_struct)    :: gobj      ! grid transform.
        character(len=4000)  :: cbuff
       end type modgrid_struct

! multi attribute models
       type,public :: modgrids_struct
        type(modgrid_struct),pointer         :: mobj
       end type modgrids_struct

!
       interface modgrid_is_in_mem
        module procedure modgrid_is_in_memr
        module procedure modgrid_is_in_memi
        module procedure modgrid_is_in_mema
        module procedure modgrid_is_in_memi3
       end interface
       interface modgrid_ftype
        module procedure modgrid_ftype_read
        module procedure modgrid_ftype_check
       end interface
       interface modgrid_data_stats
        module procedure modgrid_data_stats_mem
        module procedure modgrid_data_stats_dsk
       end interface
       interface modgrid_get_griddesc
        module procedure modgrid_get_griddesc_one
        module procedure modgrid_get_griddesc_all
       end interface
       interface modgrid_put_data
         module procedure modgrid_put_datar
         module procedure modgrid_put_data_rclip
         module procedure modgrid_put_data_iclip
       end interface
       interface modgrid_rep_data
         module procedure modgrid_rep_datar
         module procedure modgrid_rep_pointsr
       end interface
       interface modgrid_get_data
         module procedure modgrid_get_datar
         module procedure modgrid_get_slicer
         module procedure modgrid_get_pointsr
         module procedure modgrid_get_data_mem
         module procedure modgrid_get_data_memz
       end interface
       interface modgrid_rd
         module procedure modgrid_rd77
         module procedure modgrid_rdoo
       end interface
       interface modgrid_rd_data
         module procedure modgrid_rd_data_iclip
         module procedure modgrid_rd_data_rclips
         module procedure modgrid_rd_data_iclips
       end interface
       interface modgrid_xyz_order
         module procedure modgrid_xyz_order_s
         module procedure modgrid_xyz_order_o
       end interface

      real, parameter     :: MOD_RNIL = -99999.0
      integer, parameter  :: MOD_INIL = -999999

      contains


! Functional Support for grid model objects
!!----------------------------- create -------------------------------------!!
! Do not allocate buffer at this time, as it may be huge
! Create a grid model object
      subroutine modgrid_create (obj, rank, name, pname, punits, &
        ng, og, dg, stdo)
      implicit none
      type(modgrid_struct),pointer  :: obj       ! arguments
      character(len=*),intent(in)   :: name
      character(len=*),intent(in)   :: pname
      character(len=*),intent(in)   :: punits
      integer,intent(in)            :: rank
      integer,intent(in)            :: stdo
      integer,intent(in)            :: ng(:)
      real,intent(in)               :: og(:)
      real,intent(in)               :: dg(:)
      integer    :: n, i_err       !local variables
      allocate (obj, stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_create: object allocation tanked'
        nullify(obj)
        return
      endif
! Set defaults

      nullify (obj%memdata) ! jpa
      nullify (obj%dskdata) ! jpa
      nullify (obj%cvfdata) ! jpa
      nullify (obj%mspdata) ! jpa

      call modgrid_initialize(obj)

      obj%stdo  = stdo
      n = max(1,rank)
      n = min(4,rank)
      obj%rank  = rank
      obj%name  = name
      obj%pname = pname
      obj%punits= punits
      obj%palign= ' '
      obj%n_grid(1:n) = ng(1:n)
      obj%o_grid(1:n) = og(1:n)
      obj%d_grid(1:n) = dg(1:n)
! set standard axis1 to Z direction (time-depth axis)
      obj%axis1(1)  = 0.0
      obj%axis1(2)  = 0.0
      obj%axis1(3)  = max(abs(obj%d_grid(1)),&
                          abs((obj%n_grid(1)-1)*obj%d_grid(1)))
! axis2 to X direction
      obj%axis2(1)  = max(abs(obj%d_grid(2)),&
                          abs((obj%n_grid(2)-1)*obj%d_grid(2)))
      obj%axis2(2)  = 0.0
      obj%axis2(3)  = 0.0
! axis3 to Y direction
      obj%axis3(1)  = 0.0
      obj%axis3(2)  = max(abs(obj%d_grid(3)),&
                          abs((obj%n_grid(3)-1)*obj%d_grid(3)))
      obj%axis3(3)  = 0.0
      nullify(obj%memdata)
      nullify(obj%dskdata)
      nullify(obj%cvfdata)
      nullify(obj%mspdata)
      return
      end subroutine modgrid_create

      integer function modgrid_create_const(obj,hd,stdo,cval,o_pname)&
      result(status)
      type(modgrid_struct),pointer  :: obj       ! arguments
      integer,intent(in) :: hd(:)
      integer,intent(in) :: stdo
      real,intent(in)    :: cval
      character(len=*),optional,intent(in)  :: o_pname
      character(len=64)  :: name
      character(len=64)  :: pname
      character(len=16)  :: punits
      integer     :: rank,i_err
      integer     :: ng(3)
      real        :: og(3)
      real        :: dg(3)
      real        :: data(2)
      status = 0
      rank = 3
      name = 'CONSTANT'
      pname= 'VZIN'
      if(present(o_pname)) then
        pname = o_pname
      endif
      punits='UNKNOWN'
      ng=1
      og=0.0
      dg=1.0

      call modgrid_create (obj, rank, name, pname, punits, &
      ng, og, dg, stdo)
      if(.not.associated(obj)) then
        status = -1
        return
      endif
      call modgrid_set_hdwd(obj,hd)
      obj%label(1)= modgrid_header_to_string(hd(1))
      obj%label(2)= modgrid_header_to_string(hd(2))
      obj%label(3)= modgrid_header_to_string(hd(3))
      data = cval
      i_err = modgrid_put_datar(obj,1,1,1,1,data)
      return
      end function modgrid_create_const

      integer function modgrid_create_from_data(obj,hdi,ngi,ogi,dgi,&
       stdo,data,o_pname) result(status)
      type(modgrid_struct),pointer  :: obj       ! arguments
      integer,intent(in) :: hdi(:)
      integer,intent(in) :: ngi(:)
      real   ,intent(in) :: ogi(:)
      real   ,intent(in) :: dgi(:)
      integer,intent(in) :: stdo
      real,intent(in)    :: data(:)
      character(len=*),optional,intent(in)  :: o_pname
      character(len=64)  :: name
      character(len=64)  :: pname
      character(len=16)  :: punits
      integer     :: rank,i_err
      integer     :: i
      integer     :: hd(3)
      integer     :: ng(3)
      real        :: og(3)
      real        :: dg(3)
      status = 0
      rank = 3
      name = 'COPY'
      pname= 'VZIN'
      if(present(o_pname)) then
        pname = o_pname
      endif
      punits='UNKNOWN'
      ng=1
      og=0.0
      dg=1.0
      do i = 1, min(3,size(ngi))
        hd(i) = hdi(i)
        ng(i) = ngi(i)
        og(i) = ogi(i)
        dg(i) = dgi(i)
      enddo

      call modgrid_create (obj, rank, name, pname, punits, &
      ng, og, dg, stdo)
      if(.not.associated(obj)) then
        status = -1
        return
      endif
      call modgrid_set_hdwd(obj,hd)
      obj%label(1)= modgrid_header_to_string(hd(1))
      obj%label(2)= modgrid_header_to_string(hd(2))
      obj%label(3)= modgrid_header_to_string(hd(3))
      i_err = modgrid_put_datar(obj,ng(1),ng(2),1,ng(3),data)
      return
      end function modgrid_create_from_data

! Set default behavior
      subroutine modgrid_initialize(obj)
      type(modgrid_struct),intent(inout)  :: obj       ! arguments
      obj%rank   = 3
      obj%name   = 'Generic 3D Model'
      obj%pname  = 'VZIN'
      obj%punits = 'UNKNOWN'
      obj%palign = ' '
      obj%a_unit = 'U'  !unknown
      obj%label(1) = 'DEPTH'
      obj%label(2) = 'XGRID'
      obj%label(3) = 'YGRID'
      obj%hdwd   = -1
      obj%hdwd(1) = modgrid_string_to_header(obj%label(1))
      obj%hdwd(2) = modgrid_string_to_header(obj%label(2))
      obj%hdwd(3) = modgrid_string_to_header(obj%label(3))
      obj%n_grid(1) = 1
      obj%n_grid(2) = 1
      obj%n_grid(3) = 1
      obj%n_grid(4) = 1
      obj%o_grid = 0.0
      obj%d_grid = 1.0
      obj%oxyz   = 0.0
! default axis1 to Z direction (time-depth axis)
      obj%axis1(1)  = 0.0
      obj%axis1(2)  = 0.0
      obj%axis1(3)  = max(1.0, (obj%n_grid(1)-1)*obj%d_grid(1))
! default axis2 to X direction
      obj%axis2(1)  = max(1.0, (obj%n_grid(2)-1)*obj%d_grid(2))
      obj%axis2(2)  = 0.0
      obj%axis2(3)  = 0.0
! default axis3 to Y direction
      obj%axis3(1)  = 0.0
      obj%axis3(2)  = max(1.0, (obj%n_grid(3)-1)*obj%d_grid(3))
      obj%axis3(3)  = 0.0
      obj%cbuff     = ' '
! keywords
      obj%keys(1) = 'RANK'
      obj%keys(2) = 'NAME'
      obj%keys(3) = 'PNAME'
      obj%keys(4) = 'PUNITS'
      obj%keys(5) = 'LABEL'
      obj%keys(6) = 'HDWD'
      obj%keys(7) = 'N_GRID'
      obj%keys(8) = 'O_GRID'
      obj%keys(9) = 'D_GRID'
      obj%keys(10)= 'OXYZ'
      obj%keys(11)= 'AXIS1'
      obj%keys(12)= 'AXIS2'
      obj%keys(13)= 'AXIS3'
      obj%keys(14)= 'GUNIT'
      obj%fmts(1) = 'S'   ! scalar(S) or array(A)
      obj%fmts(2) = 'S'
      obj%fmts(3) = 'S'
      obj%fmts(4) = 'S'
      obj%fmts(5) = 'A'
      obj%fmts(6) = 'A'
      obj%fmts(7) = 'A'
      obj%fmts(8) = 'A'
      obj%fmts(9) = 'A'
      obj%fmts(10)= 'A'
      obj%fmts(11)= 'A'
      obj%fmts(12)= 'A'
      obj%fmts(13)= 'A'
      obj%fmts(14)= 'A'
      obj%inil    = MOD_INIL
      obj%rnil    = MOD_RNIL
      begtag = '#<MODGRID>'
      endtag = '#</MODGRID>'

      !provide a default grid transform
      !defines a mapping form survey to grid coordinates
      call grid_initialize(obj%gobj)
      return
      end subroutine modgrid_initialize

! Destroy a grid model object
      subroutine modgrid_delete(obj)
      type(modgrid_struct),pointer :: obj       ! arguments
      integer i_err
      if (.not. associated(obj)) return
      if (associated(obj%memdata)) then
     !  if(associated(obj%memdata%data)) deallocate(obj%memdata%data)
        i_err = modgmem_delete(obj%memdata)
       !if(associated(obj%memdata%data) .and. obj%memdata%owns_data)&
       ! deallocate(obj%memdata%data)
       !if(associated(obj%memdata%headers)) deallocate(obj%memdata%headers)
       !deallocate (obj%memdata, stat=i_err)
      endif
      if (associated(obj%mspdata)) then
        call modspec_delete(obj%mspdata)
        nullify(obj%mspdata)
      endif
      call modgrid_cvf_delete(obj%cvfdata)
      if (associated(obj%dskdata)) then
        deallocate (obj%dskdata, stat=i_err)
      endif
      deallocate (obj, stat=i_err)
      nullify(obj)
      return
      end subroutine modgrid_delete

      subroutine modgrid_cvf_delete(obj)
      type(prop_cvfstore_struct),pointer :: obj       ! arguments
      integer i_err
      if(associated(obj)) then
        if(associated(obj%x_coords)) deallocate(obj%x_coords)
        if(associated(obj%y_coords)) deallocate(obj%y_coords)
        if(associated(obj%x_bins)) deallocate(obj%x_bins)
        if(associated(obj%y_bins)) deallocate(obj%y_bins)
        if(associated(obj%vel)) deallocate(obj%vel)
        obj%nvfun=0
        deallocate (obj, stat=i_err)
        nullify(obj)
      endif
      return
      end subroutine modgrid_cvf_delete


      subroutine modgrid_print(obj,stdo)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(in)              :: stdo
      integer       :: i_err
      character(len=120) :: card
      if(stdo < 0) return

      xxif_pcpsx_i_pel : if ( pcpsx_i_pel() .eq. 0 ) then
      write(card,*) '#modgrid_print: name=',trim(obj%name)
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: rank=',obj%rank
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: pname=',trim(obj%pname),&
      &' punits=',trim(obj%punits),' palign=',obj%palign
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: n_grid=(',obj%n_grid(1),', ',&
       &obj%n_grid(2),', ',obj%n_grid(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: o_grid=(',obj%o_grid(1),', ',&
       &obj%o_grid(2),', ',obj%o_grid(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: d_grid=(',obj%d_grid(1),', ',&
       &obj%d_grid(2),', ',obj%d_grid(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: hdwd=(',obj%hdwd(1),',',&
       &obj%hdwd(2),',',obj%hdwd(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: label=(',trim(obj%label(1)),','
      write(stdo,'(a)') trim(card)
      write(card,*) '#                       ',trim(obj%label(2)),','
      write(stdo,'(a)') trim(card)
      write(card,*) '#                       ',trim(obj%label(3)),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: oxyz=(',obj%oxyz(1),', ',&
       &obj%oxyz(2),', ',obj%oxyz(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: axis1=(',obj%axis1(1),', ',&
       &obj%axis1(2),', ',obj%axis1(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: axis2=(',obj%axis2(1),', ',&
       &obj%axis2(2),', ',obj%axis2(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: axis3=(',obj%axis3(1),', ',&
       &obj%axis3(2),', ',obj%axis3(3),')'
      write(stdo,'(a)') trim(card)
      write(card,*) '#modgrid_print: gunit=(',obj%a_unit(1),', ',&
       &obj%a_unit(2),', ',obj%a_unit(3),')'
      write(stdo,'(a)') trim(card)
      if(modgrid_trace_ordered(obj)) then
        write(card,*) '#modgrid_print: file is TRACE ordered'
        write(stdo,'(a)') trim(card)
      else
        write(card,*) '#modgrid_print: file is NOT trace ordered?'
        write(stdo,'(a)') trim(card)
        if(modgrid_time_ordered(obj)) then
          write(card,*) '#modgrid_print: file is TIME ordered'
          write(stdo,'(a)') trim(card)
        endif
      endif

      write(stdo,*) '#modgrid_print: xorigin=',grid_get_xorigin(obj%gobj),&
       ' yorigin=', grid_get_yorigin(obj%gobj)
      write(stdo,*) '#modgrid_print: xwidth=', grid_get_xgrid_width(obj%gobj),&
       ' ywidth=', grid_get_ygrid_width (obj%gobj)
      write(stdo,*) '#modgrid_print: rotation angle=', &
       grid_get_rotation_angle (obj%gobj)
      write(stdo,*) '#modgrid_print: handedness=', &
       grid_get_handedness (obj%gobj)

      if(associated(obj%dskdata)) then
        write(card,*) '#modgrid_print: ftype   =',trim(obj%dskdata%ftype)
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: fsize   =',obj%dskdata%fsize
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: datafrmt=',trim(obj%dskdata%datafrmt)
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: dataname=',trim(obj%dskdata%dataname)
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: headfile=',trim(obj%dskdata%headfile)
        write(stdo,'(a)') trim(card)
        if(obj%dskdata%pointfile .ne. ' ') then
         write(card,*) '#modgrid_print: pointfile=',trim(obj%dskdata%pointfile)
         write(stdo,'(a)') trim(card)
        endif
        write(card,*) '#modgrid_print: datafile=',trim(obj%dskdata%datafile)
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: databits=',obj%dskdata%databits
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: endian=',obj%dskdata%endian
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: datapos=',obj%dskdata%datapos
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: datascale=',obj%dskdata%datascale
        write(stdo,'(a)') trim(card)
        write(card,*) '#modgrid_print: dataadd=',obj%dskdata%dataadd
        write(stdo,'(a)') trim(card)
      endif
      i_err = modgmem_print(obj%memdata,obj%stdo, obj%rnil)

      if(associated(obj%mspdata))  then
         call modspec_print_obj(obj%mspdata,stdo)
      endif
      write(stdo,*) ' '
      end if xxif_pcpsx_i_pel 
      return
      end subroutine modgrid_print

      integer function modgrid_data_stats_mem(obj, dmin,dmax,davg,gmax,dbar)&
      result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,intent(out)                :: dmin(:),dmax(:)
      real,intent(out)                :: davg(:)
      real,intent(out)                :: gmax(*)
      real,intent(out)                :: dbar
      gmax(1:3) = 0.0
      status = modgmem_data_stats(obj%memdata,obj%rnil,dmin,dmax,davg,dbar)
      return
      end function modgrid_data_stats_mem

      integer function modgrid_data_stats_prop(fname,stdo, dmin,dmax,davg,&
       gmax, dbar, pn, xhdr, yhdr) result(status)
      character(len=*),intent(in)     :: fname       ! arguments
      real,intent(out)                :: dmin(:),dmax(:)
      real,intent(out)                :: davg(:)
      real,intent(out)                :: gmax(*)
      real,intent(out)                :: dbar
     !real,intent(out)                :: dmin,dmax,davg
      integer,intent(in)              :: stdo
      integer,intent(inout)           :: pn
      integer,optional,intent(in)     :: xhdr
      integer,optional,intent(in)     :: yhdr
      type(modgrid_struct),pointer    :: obj
      type(modgrids_struct),pointer   :: objs(:)
      type(region_struct),pointer     :: rgndata
      integer      :: i,i_err
      integer      :: lxhdr
      integer      :: lyhdr
      integer      :: i1,i2
      integer      :: sslice,nslice
      character(len=128) :: dfile
      character(len=8)   :: wtype, vel_out
      integer      :: rank
      integer      :: pcnt
      character(len=32)  :: pnames(16)
      character    :: name*64
      character    :: pname*64
      character    :: punits*16
      character    :: ftype*8
      integer      :: hdwd(4),ng(4)
      real         :: og(4),dg(4),gm(3)
      integer(kind=8) :: npts8

      nullify (obj)   ! jpa
      nullify (objs)
      nullify (rgndata)

      status = -1
      lxhdr = 7 !headers to scan with if a segy or trc file
      lyhdr = 8
      if ( present(xhdr) ) then
        lxhdr = xhdr
      endif
      if ( present(yhdr) ) then
        lyhdr = yhdr
      endif
      vel_out = ' '
      i_err =  modgrid_rddesc_modgrids(objs,fname,stdo,dfile,wtype,&
       ftype,lxhdr,lyhdr,vel_out,pcnt,pnames,rgndata)
      if(i_err < 0 .or. .not. associated(objs)) then
        if(stdo > 0) write(stdo,*) &
         'modgrid_data_stats_prop: error reading description ',trim(fname)
        return
      endif
      if(ftype=='UNKNOWN') then
        if(stdo>0) write(stdo,*) &
         'modgrid_data_stats: UNKNOWN file type,',trim(fname)
        return
      endif
      pcnt = size(objs)
      pn = min(pcnt,max(1,pn))   !clip to valid property range
      obj => objs(pn)%mobj

      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      do i = 1,rank
        call modgrid_get_griddesc(obj,i, hdwd(i),ng(i),og(i),dg(i))
      enddo
      if(rank >=3) then
        nslice = 32000000/(ng(1)*ng(2))
      else
        nslice = 1
      endif
      npts8 = ng(1)*ng(2)
      npts8 = npts8*ng(3)
      sslice = 1
      gm(1:3) = 0.0
      gmax(1:3) = 0.0
      sslice = 1
      !read in big chunks of input data

      do while (sslice <= ng(3))
        nslice = min(nslice,ng(3)-sslice+1)
        if(sslice > ng(3)) exit

        i_err = modgrid_rd_data_iclip(obj,stdo,sslice,nslice)
        if(i_err /= 0) then
           if(stdo>0) write(stdo,*) '#modgrid_data_stats: sslice=',sslice,&
           ' nslice=',nslice,' ERROR READING'
           sslice = sslice + nslice
           return
          !cycle
        endif
        i1 = sslice !i
        i2 = i1 + nslice - 1
        i_err = modgmem_data_stats(obj%memdata,obj%rnil,&
                dmin(i1:i2),dmax(i1:i2),davg(i1:i2),dbar)
        if(i_err<0) then
           if(stdo>0) write(stdo,*) '#modgrid_data_stats_prop: error??&
           & sslice=',sslice
        else
          gmax(1) = max(gm(1),gmax(1))
          gmax(2) = max(gm(2),gmax(2))
          gmax(3) = max(gm(3),gmax(3))
        endif
        sslice = sslice + nslice
      enddo
      dbar = 0.0
      do i=1,ng(3)
        dbar = dbar + davg(i)
      enddo
      dbar = dbar/ng(3)
      if ( pcpsx_i_pel() .eq. 0 ) &
      write(stdo,*) '#modgrid_data_stats_prop: data min=',&
      minval(dmin(1:ng(3))),' data max=',maxval(dmax(1:ng(3))),&
      ' population=',npts8
      i_err =  modgrid_modgrids_delete(objs)
      status = ng(3)
      return
      end function modgrid_data_stats_prop

      integer function modgrid_data_stats_dsk(fname,stdo, dmin,dmax,davg,&
       gmax, dbar,xhdr, yhdr) result(status)
      character(len=*),intent(in)     :: fname       ! arguments
      real,intent(out)                :: dmin(:),dmax(:)
      real,intent(out)                :: davg(:)
      real,intent(out)                :: gmax(*)
      real,intent(out)                :: dbar
     !real,intent(out)                :: dmin,dmax,davg
      integer,intent(in)              :: stdo
      integer,optional,intent(in)     :: xhdr
      integer,optional,intent(in)     :: yhdr
      integer      :: lxhdr
      integer      :: lyhdr
      integer      :: lpn

      status = -1
      lxhdr  = 7 !headers to scan with if a segy or trc file
      lyhdr  = 8
      lpn    = 1
      if ( present(xhdr) ) then
       lxhdr = xhdr
      endif
      if ( present(yhdr) ) then
       lyhdr = yhdr
      endif

      status =  modgrid_data_stats_prop(fname,stdo, dmin,dmax,davg,&
       gmax, dbar, lpn, lxhdr, lyhdr)

      return
      end function modgrid_data_stats_dsk

      integer function modgrid_data_avg(obj, axis, avg) result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,intent(out)                :: avg(*)
      integer,intent(in)              :: axis
      print *,'modgrid_data_avg: obsolete - call modgmem_data_avg instead'
      avg(1) = 0
      status = -1
      return
      end function modgrid_data_avg

! Encode the modgrid binary info into a set of card images
      integer function modgrid_to_cards(obj, cards)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)    :: cards(:)
      character(len=96)           :: card       !local variables
      character(len=80)           :: sval
      integer                     :: i,cnt

      cnt=0
      modgrid_to_cards=cnt
      card = ' NAME='//char(39)//trim(obj%name)//char(39)
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      write(sval,'(I2)')  obj%rank
      card = ' RANK='//trim(sval)//' PNAME='//char(39)//trim(obj%pname)&
      &//char(39)//' PUNITS='//trim(obj%punits)
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card

      card = ' N_GRID=('
      do i = 1,obj%rank
        write(sval,'(I6)') obj%n_grid(i)
        if(i<obj%rank) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' O_GRID=('
      do i = 1,obj%rank
        write(sval,'(F12.4)') obj%o_grid(i)
        if(i<obj%rank) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' D_GRID=('
      do i = 1,obj%rank
        if(obj%d_grid(i) < 0.0001) then
          write(sval,'(F12.6)') obj%d_grid(i)
        else
          write(sval,'(F12.4)') obj%d_grid(i)
        endif
        if(i<obj%rank) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' GUNIT=('
      do i = 1,obj%rank
        if(i<obj%rank) then
          card = trim(card)//trim(obj%a_unit(i))//' , '
        else
          card = trim(card)//trim(obj%a_unit(i))//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' HDWD=('
      do i = 1,obj%rank
        write(sval,'(I4)') obj%hdwd(i)
        if(i<obj%rank) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card=' LABEL=('//trim(obj%label(1))//','//trim(obj%label(2))//&
            &','//trim(obj%label(3))//')'
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' OXYZ=('
      do i = 1,3
        write(sval,'(F12.4)') obj%oxyz(i)
        if(i<3) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS1=('
      do i = 1,3
        write(sval,'(F12.4)') obj%axis1(i)
        if(i<3) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS2=('
      do i = 1,3
        write(sval,'(F12.4)') obj%axis2(i)
        if(i<3) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS3=('
      do i = 1,3
        write(sval,'(F12.4)') obj%axis3(i)
        if(i<3) then
          card = trim(card)//trim(sval)//' , '
        else
          card = trim(card)//trim(sval)//')'
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' NO_DATA_VALUE='
      write(sval,'(F12.4)') obj%rnil
      card = trim(card)//trim(sval)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      write(card,*) ' XORIGIN=',grid_get_xorigin(obj%gobj),' YORIGIN=',&
         grid_get_yorigin(obj%gobj)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      write(card,*) ' XWIDTH=', grid_get_xgrid_width(obj%gobj),&
       ' YWIDTH=',grid_get_ygrid_width (obj%gobj)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      write(card,*) ' ANGLE=', grid_get_rotation_angle (obj%gobj),&
      ' HANDEDNESS=', grid_get_handedness(obj%gobj)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      
      if(associated(obj%dskdata)) then
        write(card,*) 'ETYPE=',trim(obj%dskdata%datafrmt),&
         '  ESIZE=', obj%dskdata%databits/8,&
         '  ENAME=',trim(obj%dskdata%dataname)
        cnt = cnt+1
        cards(min(cnt,size(cards)))=card
        write(card,*) 'DATA_OFFSET=',obj%dskdata%datapos,' ENDIAN=',&
         obj%dskdata%endian
        cnt = cnt+1
        cards(min(cnt,size(cards)))=card
        if(modgrid_pointfile(obj) .ne. ' ') then
          card=' POINTS_FILE='//trim(modgrid_pointfile(obj))
          cnt = cnt+1
          cards(min(cnt,size(cards)))=card
        endif
      endif
      if(obj%palign .ne.' ') then
        card = ' PROP_ALIGNMENT='//trim(obj%palign)
        cnt = cnt+1
        cards(min(cnt,size(cards)))=card
      endif
      if(associated(obj%mspdata)) then
        cnt = cnt + modspec_print_to_cards(obj%mspdata,cards(cnt+1:))
      endif
      modgrid_to_cards=cnt
      if(cnt>size(cards)) then
        write(obj%stdo,*) 'modgrid_to_cards: too many output cards'
      endif

      return
      end function modgrid_to_cards

      integer function modgrid_to_voxet(obj, ascrep,dfile) result(cnt)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)    :: ascrep
      character(len=*),intent(in)     :: dfile

      cnt=0
      ascrep=' '
      cnt =  modgrid_voxet_body(obj, ascrep)

      cnt = cnt +  modgrid_voxet_property(obj,ascrep,1,dfile)
      ascrep=trim(ascrep)//'END'//char(10)
      return
      end function modgrid_to_voxet

      integer function modgrid_voxet_body(obj, ascrep) result(cnt)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)    :: ascrep
      character(len=128)   :: cards(40)
      integer       :: i
      character(len=128) :: card      !local variables
      character(len=64)  :: sval,type
      real          :: x1,x2
      cnt=0
      card = 'GOCAD VOXET 1.0 '
      type =  modgrid_ftype_check(obj)
      if(string_upper_compare(type,'GSURF')) then
        card = 'GOCAD GSURF 1.0 '
      endif
      if(string_upper_compare(type,'SGRID')) then
        card = 'GOCAD SGRID 1.0 '
      endif
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card = ' HEADER {'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card = ' *name:'//trim(obj%name)
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card =  ' *axis:on'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card =  ' *real_value:true'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card =  ' *grid:off }'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card = ' AXIS_O  '
      if(string_upper_compare(type,'GSURF')) then
        card = ' ORIGIN  '
      endif
      do i = 1,3
        write(sval,'(F12.4)') obj%oxyz(i)
        if(i<3) then
          card = trim(card)//'  '//trim(sval)
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_U  '
      do i = 1,3
        write(sval,'(F12.4)') obj%axis1(i)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_V  '
      do i = 1,3
        write(sval,'(F12.4)') obj%axis2(i)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_W  '
      do i = 1,3
        write(sval,'(F12.4)') obj%axis3(i)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_N  '
      do i = 1,3
        write(sval,'(I6)') obj%n_grid(i)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_MIN  0.0 0.0 0.0'
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card = ' AXIS_MAX  1.0 1.0 1.0'
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' AXIS_LABEL_MIN  '
      do i = 1,3
        x1 = obj%o_grid(i)
        x2 = x1 + (obj%n_grid(i)-1)*obj%d_grid(i)
        if(x2< x1) goto 90
        write(sval,'(F12.4)') min(x1,x2)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card = ' AXIS_LABEL_MAX '
      do i = 1,3
        x1 = obj%o_grid(i)
        x2 = x1 + (obj%n_grid(i)-1)*obj%d_grid(i)
        write(sval,'(F12.4)') max(x1,x2)
        if(i<3) then
          card = trim(card)//trim(sval)//'   '
        else
          card = trim(card)//trim(sval)
        endif
      enddo
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
 90   continue

      if(modgrid_pointfile(obj) .ne. ' ') then
        card=' POINTS_FILE='//trim(modgrid_pointfile(obj))
        cnt = cnt+1
        cards(min(cnt,size(cards)))=card
      endif

      card=' AXIS_NAME  '//trim(obj%label(1))//' '//trim(obj%label(2))//&
            &' '//trim(obj%label(3))
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card=' AXIS_UNIT  '//trim(obj%a_unit(1))//' '//trim(obj%a_unit(2))//&
            &' '//trim(obj%a_unit(3))
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      ascrep=' '
      do i = 1,cnt
        ascrep=trim(ascrep)//' '//trim(cards(i))//char(10)
      enddo
      return
      end function modgrid_voxet_body

      integer function modgrid_voxet_property(obj,ascrep,pn,dfile) result(cnt)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(inout)  :: ascrep
      character(len=*),intent(in)     :: dfile
      integer,intent(in) :: pn
      character(len=128) :: cards(20)
      character(len=128) :: card      !local variables
      character(len=2)   :: swrk      !local variables
      integer       :: i
      cnt = 0
      write(swrk,'(I2)') max(0,min(pn,99))
      card= ' PROPERTY  '//swrk//' '//trim(obj%pname)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card= ' PROP_FILE  '//swrk//' '//trim(dfile)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card = ' PROP_ESIZE '//swrk//'  4'
      if(associated(obj%dskdata)) &
        write(card,'(" PROP_ESIZE ",A2," ",I1)') swrk,obj%dskdata%databits/8
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card
      card = ' PROP_ETYPE '//swrk//' IEEE'
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      write(card,'(" PROP_NO_DATA_VALUE ",A2," ",F12.4)') swrk,obj%rnil
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      card = ' PROP_UNIT  '//swrk//' '//trim(obj%punits)
      cnt = cnt+1
      cards(min(cnt,size(cards)))=card

      do i = 1,cnt
        ascrep=trim(ascrep)//' '//trim(cards(i))//char(10)
      enddo
      ascrep=trim(ascrep)//char(10)
      return
      end function modgrid_voxet_property


! Convert binary object to a single string
      subroutine modgrid_to_ascii(obj,ascrep,dfile,ostyle)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)  :: ascrep
      character(len=*),intent(in)     :: dfile
      integer,intent(in)              :: ostyle ! 0:modgrid, 1:gocad, , 2:siti
     !integer,intent(in)              :: gocad
      character(len=88)   :: cards(40)
      integer  :: i,lc
      ascrep=' '
      if(ostyle==0) then
        ascrep=trim(begtag)//char(10)
        lc = len_trim(ascrep)
        do i = 1,modgrid_to_cards(obj,cards)
         !ascrep=trim(ascrep)//'# '//trim(cards(i))//char(10)
          ascrep(lc+1:)='# '//trim(cards(i))//char(10)
          lc = len_trim(ascrep)
        enddo
       !ascrep=trim(ascrep)//trim(endtag)//char(10)
        ascrep(lc+1:)='# '//trim(endtag)//char(10)
      endif
      if(ostyle==1) then
        i = modgrid_to_voxet(obj,ascrep,dfile)
      endif
      if(ostyle==2) then
        i = modgrid_to_siti(obj,ascrep,dfile)
      endif
      return
      end subroutine modgrid_to_ascii

! Seperate string with LFs to card images
      integer function modgrid_ascii_to_cards(ascrep, cards, msg)
      character(len=*),intent(in)               :: ascrep
      character(len=*),intent(out)              :: cards(:)
      character(len=*),intent(out)              :: msg
      integer    :: i1,i2,i3,i4,cnt
      cnt= 0
      msg= ' '
      modgrid_ascii_to_cards=0
      i1 = 1
      i2 = index(ascrep(i1:),char(10))
      if(i2<=0) then
        msg='modgrid_ascii_to_cards: no line feed in input string'
        return
      endif
      i3 = i1
      i4 = i2
      cnt = 0
! Transfer ascii string to card images
      do while (i2 > 0)

        cnt = cnt+1
        if(cnt> size(cards)) then
          msg='modgrid_ascii_to_cards: too many output cards'
          return
        endif
        cards(cnt) = ' '
        cards(cnt) = ascrep(i3:i4-1)
        if(cards(cnt)(1:1)=='#') cards(cnt)(1:1)=' '
        i3 = i4 + 1
        i2 = index(ascrep(i3:),char(10))
        i4 = i3 + i2 -1
        if(index(cards(cnt),trim(begtag)) > 0) cnt = cnt-1
        if(index(cards(cnt),trim(endtag)) > 0) cnt = cnt-1
        modgrid_ascii_to_cards=cnt

      enddo
      return
      end function modgrid_ascii_to_cards

! Get information that is encoded in ascrep
      integer function modgrid_from_ascii(obj, ascrep)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      character(len=*),intent(in)               :: ascrep

      integer                      :: nel,cnt
      character(len=80)            :: card,msg
      character(len=80)            :: cards(20)

      type(cardset_struct),pointer :: cobj

      nullify (cobj) ! jpa

      modgrid_from_ascii = -1
      call modgrid_initialize(obj)
      cnt = modgrid_ascii_to_cards(ascrep,cards,msg)
      if(msg /= ' ') then
        write(obj%stdo,*) 'modgrid_from_ascii: ERROR?'
        write(obj%stdo,*) trim(msg)
      endif
      call cardset_create(cobj)
      card = ' '
      call cardset_put_cards(cobj,cards(1:cnt),cnt)
!     nkeys = cardset_num_keywords(cobj)
!     write(6,*) 'nkeys=',nkeys
!     write(6,*) 'ncards=',cardset_num_cards(cobj)
!     do nkey=1,nkeys
!       keywd = cardset_get_keyword(cobj,nkey)
!       write(6,*) 'keywd=',keywd
!     enddo
! Get modgrid parameter values from the cardset
      call cardset_get_scalar(cobj,'NAME', obj%name, msg)
      call cardset_get_scalar(cobj,'RANK', obj%rank, msg)
      call cardset_get_scalar(cobj,'PNAME', obj%pname, msg)
      call cardset_get_scalar(cobj,'PUNITS', obj%punits, msg)
      call cardset_get_array(cobj,'HDWD'  , obj%hdwd, nel, msg)
      call cardset_get_array(cobj,'LABEL' , obj%label, nel, msg)
      call cardset_get_array(cobj,'GUNIT' , obj%a_unit, nel, msg)
      call cardset_get_array(cobj,'N_GRID', obj%n_grid, nel, msg)
      call cardset_get_array(cobj,'O_GRID', obj%o_grid, nel, msg)
      call cardset_get_array(cobj,'D_GRID', obj%d_grid, nel, msg)
      call cardset_get_array(cobj,'OXYZ'  , obj%oxyz, nel, msg)
      call cardset_get_array(cobj,'AXIS1' , obj%axis1, nel, msg)
      call cardset_get_array(cobj,'AXIS2' , obj%axis2, nel, msg)
      call cardset_get_array(cobj,'AXIS3' , obj%axis3, nel, msg)
      call cardset_delete(cobj)
      modgrid_from_ascii = 0
      return
      end function modgrid_from_ascii

      subroutine modgrid_set_hdwd(obj,hdwd)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)                 :: hdwd(:)
      integer    :: nel
      nel = min(obj%rank,size(hdwd))
      obj%hdwd(1:nel) = hdwd(1:nel)
      return
      end subroutine modgrid_set_hdwd

      subroutine modgrid_get_hdwd(obj,hdwd)
      type(modgrid_struct),intent(in)    :: obj       ! arguments
      integer,intent(inout)              :: hdwd(:)
      integer    :: nel
      nel = min(obj%rank,size(hdwd))
      hdwd(1:nel) = obj%hdwd(1:nel)
      return
      end subroutine modgrid_get_hdwd

      subroutine modgrid_set_xyz(obj,labels, oxyz, axis1,axis2,axis3)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      character(len=*),intent(in)        :: labels(:)
      real,intent(in)                 :: oxyz(:)
      real,intent(in)                 :: axis1(:)
      real,intent(in)                 :: axis2(:)
      real,intent(in)                 :: axis3(:)
      integer    :: nel
      nel = min(3,size(labels))
      obj%label(1:nel) = labels(1:nel)
      nel = min(3,size(oxyz))
      obj%oxyz(1:nel) = oxyz(1:nel)
      nel = min(3,size(axis1))
      obj%axis1(1:nel) = axis1(1:nel)
      nel = min(3,size(axis2))
      obj%axis2(1:nel) = axis2(1:nel)
      nel = min(3,size(axis3))
      obj%axis3(1:nel) = axis3(1:nel)
      return
      end subroutine modgrid_set_xyz

      subroutine modgrid_get_xyz(obj,labels, oxyz, axis1,axis2,axis3)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)     :: labels(:)
      real,intent(out)                 :: oxyz(:)
      real,intent(out)                 :: axis1(:)
      real,intent(out)                 :: axis2(:)
      real,intent(out)                 :: axis3(:)
      integer    :: nel
      nel = min(3,size(labels))
      labels(1:nel) = obj%label(1:nel)
      nel = min(3,size(oxyz))
      oxyz(1:nel) = obj%oxyz(1:nel)
      nel = min(3,size(axis1))
      axis1(1:nel) = obj%axis1(1:nel)
      nel = min(3,size(axis2))
      axis2(1:nel) = obj%axis2(1:nel)
      nel = min(3,size(axis3))
      axis3(1:nel) = obj%axis3(1:nel)

      return
      end subroutine modgrid_get_xyz

      real function modgrid_xangle(obj) result(xangle)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real       :: xaxis(3),xlen
      integer    :: i_err, ixlab,iylab,izlab
      i_err = modgrid_xyz_order(obj%label,ixlab,iylab,izlab)
      xangle=0.0
      if(i_err/=0) then
        write(obj%stdo) 'modgrid_xangle: can not identify x axis?'
        return
      endif
      if(ixlab==1) xaxis(1:3) = obj%axis1(1:3)
      if(ixlab==2) xaxis(1:3) = obj%axis2(1:3)
      if(ixlab==3) xaxis(1:3) = obj%axis3(1:3)
      if(xaxis(3) /= 0 ) then
        write(obj%stdo) 'modgrid_xangle: non-zero x axis z component?'
        return
      endif
      xlen = xaxis(1)*xaxis(1)+xaxis(2)*xaxis(2)+xaxis(3)*xaxis(3)
      if(xlen==0) then
        write(obj%stdo) 'modgrid_xangle: zero-length x axis?'
        return
      endif
      if(xlen>0) then
        xlen=sqrt(xlen)
        xangle = acos(xaxis(1)/xlen)
        xangle = (180*xangle)/pi !convert from radians to angle
      endif
      return
      end function modgrid_xangle

      subroutine modgrid_get_name_rank(obj,name,pname,punits,rank)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(out)             :: rank
      character(len=*),intent(out)    :: name
      character(len=*),intent(out)    :: pname
      character(len=*),intent(out)    :: punits
      rank = obj%rank
      name = obj%name
      pname= obj%pname
      punits= obj%punits
      return
      end subroutine modgrid_get_name_rank

      subroutine modgrid_get_griddesc_all(obj,name,pname,punits,rank,&
       hdwd,ng,og,dg)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(out)             :: rank
      character(len=*),intent(out)    :: name
      character(len=*),intent(out)    :: pname
      character(len=*),intent(out)    :: punits
      integer,intent(out)             :: hdwd(:),ng(:)
      real,intent(out)                :: og(:),dg(:)
      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      punits= obj%punits
      call modgrid_get_griddesc(obj,1,hdwd(1),ng(1),og(1),dg(1))
      if(rank < 2) return
      call modgrid_get_griddesc(obj,2,hdwd(2),ng(2),og(2),dg(2))
      if(rank < 3 .or. size(ng) < 3) return
      call modgrid_get_griddesc(obj,3,hdwd(3),ng(3),og(3),dg(3))
      if(rank < 4 .or. size(ng) < 4) return
      call modgrid_get_griddesc(obj,4,hdwd(4),ng(4),og(4),dg(4))
      return
      end subroutine modgrid_get_griddesc_all

      subroutine modgrid_get_griddesc_one(obj,axis, hdwd,ng,og,dg)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(in)              :: axis
      integer,intent(inout)           :: hdwd
      integer,intent(inout)           :: ng
      real,intent(inout)              :: og
      real,intent(inout)              :: dg
      if(axis<1 .or. axis > obj%rank) return
      hdwd = obj%hdwd(axis)
      ng   = max(1,obj%n_grid(axis))
      og   = obj%o_grid(axis)
      dg   = obj%d_grid(axis)
      return
      end subroutine modgrid_get_griddesc_one


      logical function modgrid_has_data(obj)  result(has_data)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      has_data = .false.
      if(associated(obj%memdata) ) has_data = .true.
      return
      end function modgrid_has_data

      integer(kind=8) function modgrid_size8(obj)  result(nel)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer :: i
      nel = 1
      do i = 1,obj%rank
        nel = nel*obj%n_grid(i)
      enddo
      return
      end function modgrid_size8

      integer function modgrid_size(obj)  result(nel)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer :: i
      nel = 1   !this can overflow
      do i = 1,obj%rank
        nel = nel*obj%n_grid(i)
      enddo
      return
      end function modgrid_size


      logical function modgrid_all_in_mem(obj)  result(all_in)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      all_in = .false.
      if(modgrid_has_data(obj) ) then
        if(modgrid_size8(obj) > modgmem_size(obj%memdata) ) then
          return
        endif
      else
        all_in = .false.
        return
      endif
      all_in = .true.
      return
      end function modgrid_all_in_mem

! zs,ze ... start and end coordinate of slices in memory.
      logical function modgrid_is_in_memr(obj,zs,ze)  result(is_in)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,intent(in)                 :: zs,ze
      integer   :: dim3,org3
      real      :: zs_mem,ze_mem,zmin,zmax
      real      :: z1,z2
      is_in = .false.
      ! what are the model extrema
      zmin = obj%o_grid(3)
      zmax = obj%o_grid(3) + (obj%n_grid(3)-1)*obj%d_grid(3)
      ! sort min to max
      z1 = min(zs,ze)
      z2 = max(zs,ze)
      ! clip to model extrema
      z1 = max(z1,zmin)
      z2 = min(z2,zmax)
      if(modgrid_all_in_mem(obj)) then
        is_in = .true.
        return
      endif
      if(modgrid_has_data(obj) ) then
        dim3 = modgmem_get_dim(obj%memdata,3) 
        org3 = modgmem_get_org(obj%memdata,3) 
        zs_mem = obj%o_grid(3) + (org3-1)*obj%d_grid(3)
        ze_mem = zs_mem + (dim3-1)*obj%d_grid(3)
        if(z1 >= zs_mem .and. z2<= ze_mem) then
          is_in = .true.
          return
        endif
      endif
      return
      end function modgrid_is_in_memr

! cs,ce ... start and end coordinates to test if in memory.
! ax    ... coordinates are for axis ax
      logical function modgrid_is_in_mema(obj,cs,ce, ax)  result(is_in)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,intent(in)                 :: cs,ce
      integer,intent(in)              :: ax
      integer   :: dim
      integer   :: org
      real      :: cs_mem,ce_mem,cmin,cmax
      real      :: c1,c2
      is_in = .false.
      ! what are the model extrema
      cmin = obj%o_grid(ax)
      cmax = obj%o_grid(ax) + (obj%n_grid(ax)-1)*obj%d_grid(ax)
      if(cmin > cmax) then
        c1 = cmin
        cmin = cmax
        cmax = c1    !fixed march 15, 2006
      endif
      ! sort min to max
      c1 = min(cs,ce)
      c2 = max(cs,ce)
      ! clip to model extrema
      c1 = max(c1,cmin)
      c2 = min(c2,cmax)
      if(modgrid_all_in_mem(obj)) then
        is_in = .true.
        return
      endif
      if(ax==1) then
        dim = modgmem_get_dim(obj%memdata,1) 
        org = modgmem_get_org(obj%memdata,1)  
      endif
      if(ax==2) then
        dim = modgmem_get_dim(obj%memdata,2) 
        org = modgmem_get_org(obj%memdata,2)  
      endif
      if(ax==3) then
        dim = modgmem_get_dim(obj%memdata,3) 
        org = modgmem_get_org(obj%memdata,3)  
      endif
      if(modgrid_has_data(obj) ) then
          cs_mem = obj%o_grid(ax) + (org-1)*obj%d_grid(ax)
          ce_mem = cs_mem + (dim-1)*obj%d_grid(ax)
          if(cs_mem > ce_mem) then
            cmax = cs_mem
            cs_mem = ce_mem
            ce_mem = cmax
          endif
          if(c1 >= cs_mem .and. c2<= ce_mem) then
            is_in = .true.
            return
          endif
      endif
      return
      end function modgrid_is_in_mema

      logical function modgrid_is_in_memi(obj,izs,ize)  result(is_in)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(in)              :: izs,ize
      real      :: zs_mem,ze_mem
      integer   :: iz1,iz2
      is_in = .false.
      if(modgrid_all_in_mem(obj)) then
        is_in = .true.
        return
      endif
      iz1 = min(izs,ize)
      iz2 = max(izs,ize)
      if(modgrid_has_data(obj) ) then
        zs_mem = obj%o_grid(3) + (iz1-1)*obj%d_grid(3)
        ze_mem = obj%o_grid(3) + (iz2-1)*obj%d_grid(3)
        is_in = modgrid_is_in_mem(obj,zs_mem,ze_mem)
      endif
      return
      end function modgrid_is_in_memi

      logical function modgrid_is_in_memi3(obj,ics,ice)  result(is_in)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(in)              :: ics(:),ice(:)
      real      :: cs_mem,ce_mem
      integer   :: ic1,ic2
      integer   :: i
      is_in = .false.
      if(modgrid_all_in_mem(obj)) then
        is_in = .true.
        return
      endif
      if(.not. modgrid_has_data(obj) ) then
        return
      endif
      is_in = .true.
      do i = 1,3
        ic1 = min(ics(i),ice(i))
        ic2 = max(ics(i),ice(i))
        if(modgrid_has_data(obj) ) then
          cs_mem = obj%o_grid(i) + (ic1-1)*obj%d_grid(i)
          ce_mem = obj%o_grid(i) + (ic2-1)*obj%d_grid(i)
          is_in = modgrid_is_in_mema(obj,cs_mem,ce_mem,i)
          if(.not. is_in) return
        endif
      enddo
      return
      end function modgrid_is_in_memi3

      subroutine modgrid_set_griddesc(obj,axis, hdwd,ng,og,dg)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)              :: axis
      integer,intent(in)              :: hdwd
      integer,intent(in)              :: ng
      real,intent(in)                 :: og
      real,intent(in)                 :: dg
      if(axis<1 .or. axis > obj%rank) return
      obj%hdwd(axis)   = hdwd
      obj%n_grid(axis) = max(1,ng)
      obj%o_grid(axis) = og
      obj%d_grid(axis) = dg
      return
      end subroutine modgrid_set_griddesc


! store some data grid data
! New data to store, do not save old data
      integer function modgrid_put_datar(obj,n1,n2,sslice,nslice,data)&
       result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)                 :: n1,n2
      integer,intent(in)                 :: sslice,nslice
      real,optional,intent(in)           :: data(*)
      integer :: ss(3)
      integer :: ns(3)
      status = -1
      ss(1) = 1
      ss(2) = 1
      ss(3) = sslice
      ns(1) = n1
      ns(2) = n2
      ns(3) = nslice
      if(present(data)) then
        status = modgrid_put_data_iclip(obj,ss,ns,data)
      else
        status = modgrid_put_data_iclip(obj,ss,ns)
      endif
      return
      end function modgrid_put_datar

! store some data grid data
! New data to store, do not save old data
      integer function modgrid_put_data_rclip(obj,axs,axe,&
       data) result(status)
      type(modgrid_struct),intent(inout) :: obj
      real,intent(in)                 :: axs(:)
      real,intent(in)                 :: axe(:)
      real,optional,intent(in)        :: data(*)
      integer              :: ns(3)
      integer              :: ss(3),se(3)
      status =  modgrid_clip_limits(obj,axs,axe,ss,se)
      if(status <0) return
      ns(1:3) = se(1:3) - ss(1:3) + 1
      if(present(data)) then
       status = modgrid_put_data_iclip(obj,ss,ns,data)
      else
       status = modgrid_put_data_iclip(obj,ss,ns)
      endif
      return
      end function  modgrid_put_data_rclip

      integer function modgrid_put_data_iclip(obj,ss,ns,&
       data) result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      real,optional,intent(in)           :: data(*)
      integer                :: ierr,nel
      integer                :: i
      type(prop_memstore_struct),pointer  :: memdata
      real,pointer           :: mdata(:)
      character(len=80) :: msg
      status = -1
      nullify(memdata)
      do i = 1,3
        if(ss(i) < 1 .or. ss(i) > obj%n_grid(i)) then
          write(obj%stdo,*) 'modgrid_put_data_clip: ss out of range',&
          ss(i)
          return
        endif
        if(ns(i) < 1 .or. ss(i)+ns(i)-1 > obj%n_grid(i)) then
          write(obj%stdo,*) 'modgrid_put_data_clip: ns out of range',&
          ns(i)
          return
        endif
      enddo
      if(associated(obj%memdata)) then
        memdata => obj%memdata
      else
        ierr = modgmem_create(memdata,obj%pname,obj%punits,ns,ss,.false.,msg)
        if(ierr/=0) then
          write(obj%stdo,*) 'modgrid_put_data_clip: modgmem error'
          write(obj%stdo,*) trim(msg)
          return
        endif
        obj%memdata => memdata
      endif
      ierr = modgmem_initialize(memdata,obj%pname,obj%punits,3,ns,ss,&
            .true.,.false.)

      ierr = modgrid_alloc(obj,ns(1)*ns(2)*ns(3))
      if(ierr<=0) then
        write(obj%stdo,*) 'modgrid_put_data_iclip: failed to allocate data'
        write(obj%stdo,*) 'modgrid_put_data_iclip: ns = ',ns
        return
      endif
      mdata => modgmem_data(memdata)
      if(.not.associated(mdata))   write(obj%stdo,*) 'put_data_iclip: null mdata?'
      nel= ns(1)*ns(2)*ns(3)
      if(present(data)) then
        mdata => modgmem_data(memdata)
        if(associated(mdata)) mdata(1:nel) = data(1:nel)
      else
        ierr =modgmem_set_const(memdata,obj%rnil, 1, 0.0)
      endif
      status = 0

      return
      end function modgrid_put_data_iclip

! use the data buffer supplied by the user
      integer function modgrid_use_data(obj,n1,n2,sslice,nslice,data)&
       result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)                 :: n1,n2,sslice,nslice
      real,target                        :: data(n1*n2*nslice)
      status = modgmem_use_data(obj%memdata,obj%pname,obj%punits,&
       n1,n2,sslice,nslice, data)
 
      return
      end function modgrid_use_data

      integer function modgrid_rep_datar(obj,data,n1,n2,sslice,nslice) &
       result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      real,intent(in)        ::data(*)
      integer,intent(in)     :: n1,n2,sslice,nslice
      status = modgmem_rep_datar(obj%memdata,data,n1,n2,sslice,nslice) 
      return
      end function modgrid_rep_datar

      integer function modgrid_rep_pointsr(obj,data,npts,stride,u,v,w) &
       result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      real,intent(in)                    :: data(*)
      integer,intent(inout)              :: npts
      integer,intent(in)                 :: stride
      integer,intent(in)                 :: u,v,w
      integer      :: axis
      status = -1
      axis=1
      if(stride == obj%n_grid(1)) axis=2
      if(stride == obj%n_grid(1)*obj%n_grid(2) ) axis=3
      status = modgmem_rep_pointsr(obj%memdata,data,npts,axis, stride,u,v,w)
      return
      end function modgrid_rep_pointsr

      integer function modgrid_get_pointsr(obj,data,npts,stride,u,v,w) &
       result(status)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      real,intent(out)                   :: data(:)
      integer,intent(inout)              :: npts
      integer,intent(in)                 :: stride
      integer,intent(in)                 :: u,v,w
      integer                            :: axis
      status = -1
      axis=1
      if(stride == obj%n_grid(1)) axis=2
      if(stride == obj%n_grid(1)*obj%n_grid(2) ) axis=3
      status = modgmem_get_pointsr(obj%memdata,data,npts,axis, stride,u,v,w)
      return
      end function modgrid_get_pointsr

! Return pointer to the data
      integer function modgrid_get_datar(obj,data) result(status)
      type(modgrid_struct),intent(in)    :: obj       ! arguments
      real,pointer                       ::data(:)
      status = -1
      nullify(data)
      if(.not. associated(obj%memdata)) then
        write(obj%stdo,*) 'get_datar:DBG0 null memdata ?'
        return
      endif
      data => modgmem_data(obj%memdata)
      if(.not. associated(data)) then
        write(obj%stdo,*) 'get_datar:DBG0 null data ?'
        return
      endif
      status = 0
      return
      end function modgrid_get_datar
!
      integer function modgrid_get_data_mem(obj,data,hd,ng,og,dg)&
      result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,pointer                    :: data(:)
      integer,intent(inout)           :: hd(:)
      integer,intent(inout)           :: ng(:)
      real,intent(inout)              :: og(:)
      real,intent(inout)              :: dg(:)
      integer           :: org(3)

      status = 0
      hd   = obj%hdwd(1:3)
      ng   = 0.0       !if zero implies no data in memory
      og   = obj%o_grid(1:3)
      dg   = obj%d_grid(1:3)
      nullify(data)
      status = modgmem_get_data_mem(obj%memdata,data,ng,org)
      if(status>0) then
        og(1) = obj%o_grid(1) + (org(1)-1)*obj%d_grid(1)
        og(2) = obj%o_grid(2) + (org(2)-1)*obj%d_grid(2)
        og(3) = obj%o_grid(3) + (org(3)-1)*obj%d_grid(3)
      endif

      return
      end function modgrid_get_data_mem

      integer function modgrid_get_data_memz(obj,data,izs,ize)&
      result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,pointer                    :: data(:)
      integer,intent(inout)           :: izs,ize
      nullify(data)
      status  = modgmem_get_data_mem3(obj%memdata,data,izs,ize)
      return
      end function modgrid_get_data_memz

! Return pointer to a slice of the data
      integer function modgrid_get_slicer(obj,data, slice) result(status)
      type(modgrid_struct),intent(in)    :: obj       ! arguments
      real,pointer                       :: data(:)
      integer,intent(in)                 :: slice
      status = -1
      nullify(data)
      status =  modgmem_get_slicer(obj%memdata,data, slice)
      return
      end function modgrid_get_slicer

! Return pointer to a slice of the data along axis
      integer function modgrid_get_xsect(obj,axis, gval,mdata,data,n1,n2)
      type(modgrid_struct),intent(in)    :: obj       ! arguments
      integer,intent(in)                 :: mdata
      real,intent(out)                   :: data(*)
      real,intent(in)                    :: gval
      integer,intent(in)                 :: axis
      integer,intent(out)                :: n1,n2
      integer      :: slice
      integer      :: dim1,dim2,dim3
      real         :: wt
      integer      :: n_out(3)
      real         :: o_out(3)
      real         :: d_out(3)
      modgrid_get_xsect = -1
      if(axis<1 .or. axis > obj%rank) return
      dim1 = modgmem_get_dim(obj%memdata,1)
      dim2 = modgmem_get_dim(obj%memdata,2)
      dim3 = modgmem_get_dim(obj%memdata,3)
      if(obj%n_grid(1) /= dim1) then
        write(obj%stdo,*) 'modgrid_get_xsect: incomplete memdata ',1
        return
      endif
      if(obj%n_grid(2) /= dim2) then
        write(obj%stdo,*) 'modgrid_get_xsect: incomplete memdata ',2
        return
      endif
      call modgrid_find_ijk(obj,axis, gval,slice,wt)
      if(axis==1) then    !time-depth slice
        n1 = dim2
        n2 = obj%n_grid(3)
        n_out(1) = 1
        n_out(2) = n1
        n_out(3) = obj%n_grid(3)
        o_out(1) = obj%o_grid(axis) + (slice-1)*obj%d_grid(axis)
        o_out(2) = obj%o_grid(2)
        o_out(3) = obj%o_grid(3)
        d_out(1) = obj%d_grid(1)
        d_out(2) = obj%d_grid(2)
        d_out(3) = obj%d_grid(3)
      endif
      if(axis==2) then    !crossline
        n1 = dim1
        n2 = obj%n_grid(3)
        n_out(1) = n1
        n_out(2) = 1
        n_out(3) = n2
        o_out(1) = obj%o_grid(axis)
        o_out(2) = obj%o_grid(axis) + (slice-1)*obj%d_grid(axis)
        o_out(3) = obj%o_grid(3)
        d_out(1) = obj%d_grid(axis)
        d_out(2) = obj%d_grid(2)
        d_out(3) = obj%d_grid(3)
      endif
      if(axis==3) then    !inline
        n1 = dim1
        n2 = dim2
        n_out(1) = n1
        n_out(2) = n2
        n_out(3) = 1
        o_out(1) = obj%o_grid(axis)
        o_out(2) = obj%o_grid(2)
        o_out(3) = obj%o_grid(axis) + (slice-1)*obj%d_grid(axis)
        d_out(1) = obj%d_grid(1)
        d_out(2) = obj%d_grid(2)
        d_out(3) = obj%d_grid(3)
      endif
      modgrid_get_xsect = modgrid_interpolate(obj,mdata,data,&
        n_out,o_out,d_out)
      return
      end function modgrid_get_xsect

      subroutine modgrid_recip(obj,i_err)
      type(modgrid_struct),intent(inout)  :: obj   ! arguments
      integer,intent(out),optional        :: i_err
      integer   i
      integer   stat
      integer   zcnt
      integer   ncnt
      real,pointer  :: data(:)
      if(present(i_err)) then
        i_err = -1
      endif
      if(.not.associated(obj%memdata)) return
      data => modgmem_data(obj%memdata)
      if(.not.associated(data )) return
      if(present(i_err)) then
        i_err = 0
      endif
      if(modgmem_has_zero_values(obj%memdata, stat)) then
        if(stat < 0) then
          if(present(i_err)) then
            write(obj%stdo,*) 'modgrid_recip: zero values?'
            i_err = -1
            return
          endif
        endif
      endif
      zcnt=0
      ncnt=0
      i =  modgmem_invert(obj%memdata, obj%rnil, zcnt, ncnt )
      if(present(i_err)) then
        i_err = i
      endif
      if(zcnt>0 .or. ncnt>0) then
        write(obj%stdo,*) 'modgrid_recip: WARNING zero_values=',zcnt,&
        ' nil values=',ncnt
      endif
      return
      end subroutine modgrid_recip

      subroutine modgrid_scale(obj,scale)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      real,intent(in)                    :: scale
      integer   ierr
      ierr =  modgmem_muladd(obj%memdata, obj%rnil, scale, 0.0) 
      return
      end subroutine modgrid_scale

      subroutine modgrid_add(obj, add)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      real,intent(in)                    :: add
      integer   ierr
      ierr =  modgmem_muladd(obj%memdata, obj%rnil, 1.0, add) 
      return
      end subroutine modgrid_add

      integer function modgrid_math(fname,stdo, scale, add)&
      result(status)
      character(len=*),intent(in)     :: fname       ! arguments
      real,intent(in)                :: scale,add
      integer,intent(in)              :: stdo
      type(modgrid_struct),pointer :: obj
      integer      :: i,i_err
      integer      :: sslice,nslice
      character(len=8)   :: wtype
      character(len=128) :: dfile
      integer      :: rank
      character    :: name*64
      character    :: pname*64
      character    :: punits*16
      character    :: ftype*8
      integer      :: hdwd(4),ng(4)
      real         :: og(4),dg(4)

      nullify (obj) ! jpa

      status = -1

      i_err = modgrid_rddesc(obj,fname,stdo,dfile,wtype,ftype)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_math: error reading globals ',trim(fname)
        return
      endif
      if(ftype=='UNKNOWN') then
        write(stdo,*) 'modgrid_math: UNKNOWN file type,',trim(fname)
        return
      endif
      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      do i = 1,rank
        call modgrid_get_griddesc(obj,i, hdwd(i),ng(i),og(i),dg(i))
      enddo
      if(rank >=3) then
        nslice = 32000000/(ng(1)*ng(2))
      else
        nslice = 1
      endif
      sslice = 1
      do i = 1,ng(3)   !scan slices
        nslice = min(nslice,ng(3)-sslice+1)
        if(sslice > ng(3)) exit
        i_err = modgrid_rd_data_iclip(obj,stdo,sslice,nslice)
        if(i_err /= 0) then
           write(stdo,*) '#modgrid_math: sslice=',sslice,&
           ' nslice=',nslice,' ERROR READING'
           cycle
        endif
        i_err =  modgmem_muladd(obj%memdata, obj%rnil, scale, add) 
        ! i_err = modgrid_wr_data(obj,....)  ! write back in place
        if(i_err==0) then
           write(stdo,*) '#modgrid_math: error??'
        endif
        sslice = sslice + nslice
      enddo
      call modgrid_delete(obj)
      status = 0
      return
      end function modgrid_math

      integer function modgrid_interpolate(obj,mdata, data,&
        n_out,o_out,d_out) result(status)
      type(modgrid_struct),intent(in)    :: obj       ! arguments
      integer,intent(in)                 :: mdata
      real,intent(out)                   :: data(*)
      integer,intent(in)                 :: n_out(*) !at least 3
      real,intent(in)                    :: o_out(*)
      real,intent(in)                    :: d_out(*)

      integer      :: n_inp(3)
      real         :: o_inp(3)
      real         :: d_inp(3)
      real,pointer :: indata(:)
      integer      :: org1,org2,org3
      status = -1
      if(.not. associated(obj%memdata)) return
      n_inp(1) = modgmem_get_dim(obj%memdata,1)
      n_inp(2) = modgmem_get_dim(obj%memdata,2)
      n_inp(3) = modgmem_get_dim(obj%memdata,3)
      org1 = modgmem_get_org(obj%memdata,1)
      org2 = modgmem_get_org(obj%memdata,2)
      org3 = modgmem_get_org(obj%memdata,3)
      o_inp(1) = obj%o_grid(1) + (org1-1)*obj%d_grid(1)
      o_inp(2) = obj%o_grid(2) + (org2-1)*obj%d_grid(2)
      o_inp(3) = obj%o_grid(3) + (org3-1)*obj%d_grid(3)
      d_inp(1) = obj%d_grid(1)
      d_inp(2) = obj%d_grid(2)
      d_inp(3) = obj%d_grid(3)
      if(mdata < n_out(1)*n_out(2)*n_out(3)) then
        status = -2
        return
      endif
     !print *,' n_inp=',n_inp(1:3)
     !print *,' o_inp=',o_inp(1:3)
     !print *,' d_inp=',d_inp(1:3)
     !print *,' n_out=',n_out(1:3)
     !print *,' o_out=',o_out(1:3)
     !print *,' d_out=',d_out(1:3)
      if(.not.associated(obj%memdata)) then
        print *,'modgrid_interpolate: DBG no memdata?'
        return
      endif
      indata => modgmem_data(obj%memdata) 
      if(.not.associated(indata)) then
        print *,'modgrid_interpolate: DBG no indata?'
        return
      endif
      call interpolate_3d_to_3d( &
         n_inp(1),o_inp(1),d_inp(1), &
         n_inp(2),o_inp(2),d_inp(2), &
         n_inp(3),o_inp(3),d_inp(3), indata, &
         n_out(1),o_out(1),d_out(1), &
         n_out(2),o_out(2),d_out(2), &
         n_out(3),o_out(3),d_out(3), data)
      status = 0
      return
      end function modgrid_interpolate


      integer function modgrid_wr_gocad(obj,fname,stdo,oorder) result(status)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),optional,intent(in) :: oorder
      character                  :: msg*88

      character(len=128) :: dfile
      type(modgrid_struct),pointer:: oobj
      type(trcio_struct),pointer  :: trcio
      real,pointer :: dpntr(:)
      integer      :: hdwd,hdwd2,hdwd3,n1,n2,n3
      integer      :: n1o,n2o,n12o,  stride
      integer      :: j,k,i
      integer      :: kk,j1,j2
      real         :: o1,d1,o2,d2,o3,d3
      integer      :: i_err
      integer      :: gocad
      integer      :: ixlabel,iylabel,izlabel
      character    :: in_xyz*4
      character    :: out_xyz*4
      integer      :: itoo(3)
      integer      :: otoi(3)
      integer      :: itrip(3)
      integer      :: otrip(3)
      integer      :: axis
      gocad = 1
      status = -1
      nullify(dpntr)
      nullify(oobj)
      nullify(trcio)
! get grid parameters
      call modgrid_get_griddesc(obj,1, hdwd,n1,o1,d1)
      call modgrid_get_griddesc(obj,2, hdwd2,n2,o2,d2)
      call modgrid_get_griddesc(obj,3, hdwd3,n3,o3,d3)
      i_err = modgrid_xyz_order(obj%label,ixlabel,iylabel,izlabel)
      if(i_err < 0) then
        write(obj%stdo,*) 'modgrid_wr_gocad: ERROR - bad input order'
        write(obj%stdo,*) 'modgrid_wr_gocad: ',obj%label(1)
        write(obj%stdo,*) 'modgrid_wr_gocad: ',obj%label(2)
        write(obj%stdo,*) 'modgrid_wr_gocad: ',obj%label(3)
        return
      endif
      in_xyz=' '
      in_xyz(ixlabel:ixlabel)='X'
      in_xyz(iylabel:iylabel)='Y'
      in_xyz(izlabel:izlabel)='Z'
      out_xyz=in_xyz
      if(present(oorder)) then
        out_xyz = oorder
      endif
      call string_to_upper(out_xyz)
!
! create the output description
! clips the axis description in oobj to what is in memory
      i_err = modgrid_copy_desc(obj,oobj, 0, out_xyz)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_wr_gocad: error creating output description'
        return
      endif
      i_err = modgrid_build_map(obj%label,oobj%label,itoo)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_wr_gocad: ERROR creating map'
        return
      endif
      i_err = modgrid_build_map(oobj%label,obj%label,otoi)
!
      dfile = fname
      i = index(dfile,'.vo')
      if( i==0) then
        dfile = trim(dfile)//'.vodat'
      else
        dfile = dfile(1:i)//'vodat'
      endif

! output the gocad file header
      call modgrid_wr_header(oobj,fname,dfile,1,stdo,i_err)
      if(i_err < 0 ) then
        msg = 'modgrid_wr_gocad: error writing header '//trim(fname)
        goto 99
      endif
!
! output the grid values to the file
      trcio => modgrid_open_binfile(oobj,dfile,'w+',stdo,'VOXET')
      if(.not. associated(trcio) ) then
         msg = 'modgrid_wr_gocad: error opening data file'
         goto 99
      endif

! allocate a buffer to hold an output slice
      allocate(dpntr(oobj%n_grid(1)*oobj%n_grid(2)),stat=i_err)
      if(i_err/=0) then
        msg = 'modgrid_wr_gocad: allocate error'
        goto 99
      endif
      call modgrid_print(oobj,stdo)

      n1o  = oobj%n_grid(1)
      n2o  = oobj%n_grid(2)
      n12o = oobj%n_grid(1) * oobj%n_grid(2)
      stride= 1
      if(itoo(1)==2) stride= n1
      if(itoo(1)==3) stride= n1*n2
      axis=1
      if(stride==n1) axis=2
      if(stride==n1*n2) axis=3
      do k =1,oobj%n_grid(3)
        otrip(3) = k
        do j =1,n2o
          otrip(2) = j
          otrip(1) = 1
          itrip(1) = otrip(otoi(1))
          itrip(2) = otrip(otoi(2))
          itrip(3) = otrip(otoi(3))
          kk = itrip(3)-1 + modgmem_get_org(obj%memdata,3)
          j1 = (j-1)*n1o+1
          j2 = j1 + n1o-1
          !get data from the input object
         !i_err = modgrid_get_data(obj,dpntr(j1:j2),n1o, stride, &
         !        itrip(1),itrip(2),kk)
          i_err = modgmem_get_pointsr(obj%memdata,dpntr(j1:j2),n1o,axis,stride,&
                  itrip(1),itrip(2),kk)
 
          if(i_err .ne.0) then
            write(msg ,'("modgrid_wr_gocad: get_data error kk=",i4)') kk
            goto 99
          endif
        enddo
        !write out this slice of data
        i_err = modgrid_wr_binslice(oobj,trcio,k,stdo,'VOXET',&
         n1o,n2o,dpntr)
        if(i_err .ne.0) then
          write(msg ,'("modgrid_wr_gocad: wr_binslice error k=",i4)') k
          goto 99
        endif
      enddo

      status = 0
 99   continue
      if(associated(dpntr)) deallocate(dpntr, stat=i_err)
      if(associated(oobj)) call modgrid_delete(oobj)
      call modgrid_close_binfile(trcio)
      if(status< 0) write(stdo,*) trim(msg)
      return
      end function modgrid_wr_gocad


      integer function modgrid_wr(obj,fname,stdo,oorder) result(status)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),optional,intent(in) :: oorder
      character                  :: msg*88


      type(trcio_struct),pointer :: file
      type(modgrid_struct),pointer:: oobj
      real,pointer :: dpntr(:)
      integer      :: hdwd,hdwd2,hdwd3,n1,n2,n3
      integer      :: j,k
      integer      :: npts,stride
      integer      :: kk
      real         :: o1,d1,o2,d2,o3,d3

      integer      :: ixlabel,iylabel,izlabel
      integer      :: oxlabel,oylabel,ozlabel
      character    :: in_xyz*4
      character    :: out_xyz*4
      integer      :: itoo(3)
      integer      :: otoi(3)
      integer      :: itrip(3)
      integer      :: otrip(3)
      integer      :: traceno
      integer      :: i_err
      integer      :: axis

      nullify (oobj) ! jpa

      status = -1
! get grid parameters
      call modgrid_get_griddesc(obj,1, hdwd,n1,o1,d1)
      call modgrid_get_griddesc(obj,2, hdwd2,n2,o2,d2)
      call modgrid_get_griddesc(obj,3, hdwd3,n3,o3,d3)
      i_err = modgrid_xyz_order(obj%label,ixlabel,iylabel,izlabel)
      if(i_err < 0) then
        write(obj%stdo,*) 'modgrid_wr: ERROR - bad input order'
        write(obj%stdo,*) 'modgrid_wr: ',obj%label(1)
        write(obj%stdo,*) 'modgrid_wr: ',obj%label(2)
        write(obj%stdo,*) 'modgrid_wr: ',obj%label(3)
        return
      endif
      in_xyz=' '
      in_xyz(ixlabel:ixlabel)='X'
      in_xyz(iylabel:iylabel)='Y'
      in_xyz(izlabel:izlabel)='Z'
      out_xyz=in_xyz
      if(present(oorder)) then
        out_xyz = oorder
      endif
      call string_to_upper(out_xyz)
!
! create the output description
      i_err = modgrid_copy_desc(obj,oobj,0,out_xyz)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_wr: error creating output description'
        return
      endif
      i_err = modgrid_xyz_order(oobj%label,oxlabel,oylabel,ozlabel)
      i_err =  modgrid_build_map(obj%label,oobj%label,itoo)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_wr: error creating map'
        return
      endif
      i_err =  modgrid_build_map(oobj%label,obj%label,otoi)
!
! open output file and fill in header
      nullify(file)
      file => modgrid_wr_trcio_header(oobj,fname,stdo)
      if(.not.associated(file) ) return
!
! output the grid values to the file
      allocate(dpntr(max(n1,n2,n3)),stat=i_err)
      if(i_err/=0) then
        write(stdo,*) 'modgrid_wr: allocate error'
        return
      endif
      npts=oobj%n_grid(1)
      stride= 1
      if(itoo(1)==2) stride= n1
      if(itoo(1)==3) stride= n1*n2
      axis=1
      if(stride==n1) axis=2
      if(stride==n1*n2) axis=3
      traceno = 0
      do k =1,oobj%n_grid(3)
        otrip(3) = k
        do j =1,oobj%n_grid(2)
          otrip(2) = j
          otrip(1) = 1
          itrip(1) = otrip(otoi(1))
          itrip(2) = otrip(otoi(2))
          itrip(3) = otrip(otoi(3))
         !kk = itrip(3)-1 + obj%memdata%org3
          kk = itrip(3)-1 + modgmem_get_org(obj%memdata,3)
         !i_err = modgrid_get_data(obj,dpntr,npts, stride, &
         !        itrip(1),itrip(2),kk)
          i_err = modgmem_get_pointsr(obj%memdata,dpntr, &
                  npts, axis, stride, itrip(1),itrip(2),kk)

          traceno = traceno + 1
          i_err = modgrid_wr_trcio_trace(oobj,file,traceno,dpntr)
          if(i_err /= 0) then
           msg = 'modgrid_wr: error writing data'
           goto 99
          endif
        enddo
      enddo
      deallocate(dpntr, stat=i_err)

      if(associated(oobj)) call modgrid_delete(oobj)
      i_err = trcio_close(file)
      status = 0
      return
 99   continue
      write(stdo,*) trim(msg)
      if(associated(oobj)) call modgrid_delete(oobj)
      i_err = trcio_close(file)
      return
      end function modgrid_wr

      function modgrid_wr_trcio_header(oobj,fname,stdo) result(file)
      type(modgrid_struct),intent(in) :: oobj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo

      type(trcio_struct),pointer      :: file
      character(len=32)          :: sname
      character(len=80)          :: cards(40)
      character(len=96)    :: msg
      integer      :: i_err
      integer      :: ncards
!
! open output file
      nullify(file)
      if(fname==' ') return
      file => modgrid_open_binfile(oobj,fname,'w+',stdo,'TRCIO')
      if(.not. associated(file) ) then
        msg='modgrid_wr_trcio_header: error opening '//trim(fname)
        goto 99
      endif

      i_err = trcio_set_num_values(file,oobj%n_grid(1))
      i_err = trcio_set_dt(file,oobj%d_grid(1))
      i_err = trcio_set_nbits(file,32)
      i_err = trcio_set_nbits_hd(file,32)
      i_err = trcio_set_tmin(file,oobj%o_grid(1))
      i_err = trcio_set_tmax(file,&
              oobj%o_grid(1) + (oobj%n_grid(1)-1)*oobj%d_grid(1))
      i_err = trcio_set_xorigin(file, grid_get_xorigin(oobj%gobj))
      i_err = trcio_set_yorigin(file, grid_get_yorigin(oobj%gobj))
      i_err = trcio_set_dx11(file, grid_get_dx11(oobj%gobj))
      i_err = trcio_set_dx12(file, grid_get_dx12(oobj%gobj))
      i_err = trcio_set_dx21(file, grid_get_dx21(oobj%gobj))
      i_err = trcio_set_dx22(file, grid_get_dx22(oobj%gobj))

      file%nwih       = 64
! output the file header
      i_err   = trcio_writeheader(file)
      if(i_err < 0) then
        msg ='modgrid_wr_trcio_header: failed to write file header'
        goto 99
      endif
! generate the card deck for the modgrid section
      ncards = modgrid_to_cards(oobj, cards)
      sname = 'modgrid'
      i_err   = trcio_write_section(file,sname,ncards, cards)
      if(i_err < 0) then
        msg='modgrid_wr_trcio_header: error writing section '//trim(sname)
        goto 99
      endif
      return
 99   continue
      write(stdo,*) trim(msg)
      i_err = trcio_close(file)
      nullify(file)
      return
      end function modgrid_wr_trcio_header

      integer function modgrid_wr_trcio_trace(oobj,file,traceno,trace) &
      result(status)
      type(modgrid_struct),intent(in) :: oobj
      type(trcio_struct),pointer      :: file
      integer,intent(in)              :: traceno
      real,intent(in)                 :: trace(:)

      integer,save :: old_traceno=0
      double precision :: hd(64)
      real             :: c2,c3
      real             :: tx,ty

      integer      :: j,k,i
      integer      :: npts
      real         :: lav
      integer      :: oxlab,oylab,ozlab
      integer      :: i_err
      status = -1

      i_err = modgrid_xyz_order(oobj%label,oxlab,oylab,ozlab)
      if(i_err .ne.0) then
         print *,"modgrid_wr_trcio_trace: error, can not figure out axis ordering?"
         return
      endif
      if(ozlab.ne.1) then
         print *,"modgrid_wr_trcio_trace: error, z-axis is not the fast axis?"
         return
      endif
      hd = 0.0
      k = 1 + (traceno-1)/(oobj%n_grid(2))    !line number
      j = (traceno - (k-1)*(oobj%n_grid(2)))  !trace within line
      c2 = oobj%o_grid(2) + (j-1)*oobj%d_grid(2)
      c3 = oobj%o_grid(3) + (k-1)*oobj%d_grid(3)
      if(oxlab==2) then
        tx=c2
        ty=c3
      else
        tx=c3
        ty=c2
      endif
      hd(HDR_SEQUENCE) = (k-1)*oobj%n_grid(2) + j
      hd(HDR_TOP_MUTE) = 1

      if(oobj%hdwd(oxlab)== HDR_MIDPOINT_XLOC) then
         !input grid is in terms of survey coordinates 17,18
         !use grid transform to set the bins
         hd(HDR_MIDPOINT_XLOC) = tx
         hd(HDR_MIDPOINT_YLOC) = ty
         hd(HDR_MIDPOINT_XGRID)= grid_get_xbin_number (oobj%gobj, tx , ty )
         hd(HDR_MIDPOINT_YGRID)= grid_get_ybin_number (oobj%gobj, tx , ty )
      else
         !input grid is in terms of grid coordinates 7,8 (assumption)
         !use grid transform to set the survey coordinates
         hd(HDR_MIDPOINT_XGRID)= tx
         hd(HDR_MIDPOINT_YGRID)= ty
         hd(HDR_MIDPOINT_XLOC) = grid_get_xbin_center (oobj%gobj, tx, ty)
         hd(HDR_MIDPOINT_YLOC) = grid_get_ybin_center (oobj%gobj, tx, ty)
      endif


    ! if(oxlab==2) then
    !   hd(HDR_MIDPOINT_XGRID)= oobj%o_grid(oxlab) + (j-1)*oobj%d_grid(oxlab)
    !   hd(HDR_MIDPOINT_YGRID)= oobj%o_grid(oylab) + (k-1)*oobj%d_grid(oylab)
    ! else
    !   hd(HDR_MIDPOINT_XGRID)= oobj%o_grid(oxlab) + (k-1)*oobj%d_grid(oxlab)
    !   hd(HDR_MIDPOINT_YGRID)= oobj%o_grid(oylab) + (j-1)*oobj%d_grid(oylab)
    ! endif
    ! hd(HDR_MIDPOINT_XLOC) = hd(HDR_MIDPOINT_XGRID)
    ! hd(HDR_MIDPOINT_YLOC) = hd(HDR_MIDPOINT_YGRID)
      lav=0.0
      npts  = oobj%n_grid(1)
      do i=1,npts
        lav = max(lav,abs(trace(i)))
      enddo
      hd(HDR_LAV) = lav
      hd(64) = npts
      if(old_traceno /= traceno - 1) then
        i_err = trcio_seek_trace(file,traceno)
      endif
      i_err = trcio_write_trace(file,hd,trace)
      if(i_err /= TRCIO_OK) then
        status = -1
        return
      endif
      old_traceno = traceno
      status = 0
      return
      end function modgrid_wr_trcio_trace

      integer function modgrid_wr_trcio_traces(oobj,ngi,icube, &
        ilabels,olabels,obuf,file,clips,clipe,mems,meme) result(status)
      integer,intent(in) :: ngi(:)  !dimension of sub cube in memory
      real,intent(in)    :: icube(ngi(1),ngi(2),ngi(3)) !3D sub cube in memory
      type(trcio_struct),pointer :: file
      character(len=*),intent(in) :: ilabels(3)
      character(len=*),intent(in) :: olabels(3)
      type(modgrid_struct),intent(in) :: oobj
      real,intent(out)   :: obuf(:)
      integer,intent(in) :: clips(*)
      integer,intent(in) :: clipe(*)
      integer,intent(in) :: mems(*)   !start index loaded in memory
      integer,intent(in) :: meme(*)
!
      integer   :: ocd(3)    !dimensions of output cube
      integer   :: count
      integer   :: opt
      integer   :: i, i1_out,i2_out,i3_out
      integer   :: nel,i_err,seqno
      integer   :: otoi(3)
      integer   :: itoo(3)
      integer   :: itrip(3)  !input data triplet
      integer   :: otrip(3)  !output data triplet
      integer   :: oclips(3)
      integer   :: oclipe(3)
      integer   :: omems(3)
      integer   :: omeme(3)
      integer   :: spt(3),ept(3)
      integer   :: oxlabel,oylabel,ozlabel
      logical   :: noskip
      noskip=.true.
      status = -1
      i_err = modgrid_build_map(ilabels,olabels,otoi)
      i_err = modgrid_build_map(olabels,ilabels,itoo)
      i_err = modgrid_xyz_order(olabels,oxlabel,oylabel,ozlabel)
      if(i_err == -1) then
        write(oobj%stdo,*) 'modgrid_wr_trcio_traces: ERROR!'
        return
      endif
      call modgrid_map_address(clips,otoi,oclips)
      call modgrid_map_address(clipe,otoi,oclipe)
      call modgrid_map_address(mems,otoi,omems)
      call modgrid_map_address(meme,otoi,omeme)
      do i = 1,3
        if(omems(i) > oclipe(i)) return  !no intersection of clip and memory
        if(omeme(i) < oclips(i)) return  !no intersection of clip and memory
        if(omems(i) >= oclips(i) .and. &
           omeme(i) <= oclipe(i) )  then !memory entirely within clip
          spt(i) = omems(i)
          ept(i) = omeme(i)
        else                             !some overlap
          spt(i) = max(omems(i),oclips(i))
          ept(i) = min(omeme(i),oclipe(i))
        endif
        ocd(i) = oclipe(i) - oclips(i) + 1
      enddo

      count = 0
      do i3_out = omems(3), omeme(3)
        if(i3_out < oclips(3) .or. i3_out > oclipe(3)) cycle
        otrip(3) = i3_out-omems(3)+1
        do i2_out = omems(2), omeme(2)
          if(i2_out < oclips(2) .or. i2_out > oclipe(2)) cycle
          otrip(2) = i2_out-omems(2)+1
          opt = 1
          do i1_out = omems(1),omeme(1)
            otrip(1) = opt
            itrip(1) = otrip(itoo(1)) !map in (i,j,k) to out (l,m,n)
            itrip(2) = otrip(itoo(2))
            itrip(3) = otrip(itoo(3))
            obuf(opt) = icube(itrip(1),itrip(2),itrip(3))
            opt = opt+1
          enddo
          nel = ept(1) - spt(1) + 1
          if(nel > 0) then
            seqno = (i3_out-oclips(3))*ocd(2) + &
                (i2_out - oclips(2))
            i   = spt(1) - omems(1) + 1
            count = count + nel
           !if(noskip) then
            i_err = modgrid_wr_trcio_trace(oobj,file,seqno+1,obuf(i:i+nel-1))
            if(i_err < 0) then
              return
            endif
           !endif
          endif
        enddo
      enddo
      status = 0
      write(oobj%stdo,*) 'modgrid_wr_trcio_traces: count=',count
      return
      end function modgrid_wr_trcio_traces

      integer function modgrid_copy_desc(obj,oobj, full, oorder) result(status)
      type(modgrid_struct),intent(in) :: obj
      type(modgrid_struct),pointer    :: oobj
      integer,intent(in)              :: full !copy full desc or data in mem
      character(len=*),intent(in)     :: oorder
      integer   :: ng(4),hd(4)
      real      :: og(4),dg(4)
      real      :: oxyz(3)
      real      :: iaxis(3,3)
      integer   :: ax_s,ax_e
      character :: in_xyz*4
      character :: out_xyz*4
      character :: label(3)*32
      integer   :: itoo(4),oclip
      integer   :: xlabel,ylabel,zlabel
      integer   :: i,i_err
      status = 0
      if(associated(oobj)) call modgrid_delete(oobj)
      nullify(oobj)
      iaxis(1:3,1)= obj%axis1(1:3)
      iaxis(1:3,2)= obj%axis2(1:3)
      iaxis(1:3,3)= obj%axis3(1:3)
      oxyz(1:3)  = obj%oxyz(1:3)
      label(1:3) = obj%label(1:3)
      hd(1:4) = obj%hdwd(1:4)
      ng(1:4) = obj%n_grid(1:4)
      og(1:4) = obj%o_grid(1:4)
      dg(1:4) = obj%d_grid(1:4)
      itoo(1) = 1
      itoo(2) = 2
      itoo(3) = 3
      itoo(4) = 4
      i_err = modgrid_xyz_order(obj%label,xlabel,ylabel,zlabel)
      if(i_err /=0) then
        status = -1
        return
      endif
      in_xyz(xlabel:xlabel) = 'X'
      in_xyz(ylabel:ylabel) = 'Y'
      in_xyz(zlabel:zlabel) = 'Z'
      out_xyz = oorder
      i_err = modgrid_build_xyz_map(in_xyz,out_xyz,itoo)
      if(i_err /=0) then
        status = -1
        return
      endif
      do i = 1,4
          ng(i) = obj%n_grid(itoo(i))
          og(i) = obj%o_grid(itoo(i))
          dg(i) = obj%d_grid(itoo(i))
          hd(i) = obj%hdwd(itoo(i))
          if(i<4) label(i) = obj%label(itoo(i))
          if(itoo(i)==3) oclip=i
      enddo
      call modgrid_create(oobj, obj%rank, obj%name, obj%pname,&
           obj%punits, ng, og, dg, obj%stdo)
      call modgrid_set_hdwd(oobj,hd)
      call modgrid_set_xyz(oobj,label, oxyz,&
           iaxis(1:3,itoo(1)),iaxis(1:3,itoo(2)), iaxis(1:3,itoo(3)))
      if(associated(obj%memdata) .and. full==0) then
        ax_s = modgmem_get_org(obj%memdata,3)
        ax_e = ax_s + modgmem_get_dim(obj%memdata,3) - 1
        call modgrid_clip_axis(oobj,oclip, ax_s,ax_e)
      endif
      !preserve transform information
      oobj%gobj = obj%gobj
      return
      end function modgrid_copy_desc

      integer function modgrid_copy(obj,oobj, axis, full) result(status)
      type(modgrid_struct),intent(in) :: obj
      type(modgrid_struct),pointer    :: oobj
      integer,intent(in)              :: axis
      integer,intent(in)              :: full !copy full desc or data in mem
      character :: in_xyz*4
      character :: out_xyz*4
      integer   :: xlabel,ylabel,zlabel
      integer   :: i_err
      status = 0
      if(associated(oobj)) call modgrid_delete(oobj)
      nullify(oobj)

      i_err = modgrid_xyz_order(obj%label,xlabel,ylabel,zlabel)
      if(i_err /=0) then
        status = -1
        return
      endif
      in_xyz= ' '
      in_xyz(xlabel:xlabel) = 'X'
      in_xyz(ylabel:ylabel) = 'Y'
      in_xyz(zlabel:zlabel) = 'Z'
      out_xyz = in_xyz

      if(axis==1) then !t-z axis becomes slowest or fastest storage
        ! axis 1-2-3 reordered to 2-3-1 for trace ordered cube(zlabel=1)
        ! axis 1-2-3 reordered to 3-1-2 for time ordered cube(zlabel=3)
        if(zlabel == 1) then
          out_xyz(1:1) = in_xyz(2:2) !'X'
          out_xyz(2:2) = in_xyz(3:3) !'Y'
          out_xyz(3:3) = 'Z'
        else
          out_xyz(1:1) = 'Z'
          out_xyz(2:2) = in_xyz(1:1) !'X'
          out_xyz(3:3) = in_xyz(2:2) !'Y'
        endif
        status = modgrid_copy_desc(obj,oobj,full,out_xyz)
        return
      endif
      if(axis==2) then !interchange order of axis 2 and 3
        ! axis 1-2-3 reordered to 1-3-2
        out_xyz(1:1) = in_xyz(1:1)
        out_xyz(2:2) = in_xyz(3:3)
        out_xyz(3:3) = in_xyz(2:2)
        status = modgrid_copy_desc(obj,oobj,full,out_xyz)
        return
      endif
      if(axis==3) then !new description has same order as current
        ! axis 1-2-3 reordered to 1-2-3
        out_xyz = in_xyz
        status = modgrid_copy_desc(obj,oobj,full,out_xyz)
        return
      endif
      status = -1
      return
      end function modgrid_copy

! gc(i)  ... grid coordinate on axis i
! ijk(i) ... index of slice on axis i with coord just < gc(i)
!            a zero value indicates an error
! wts(i) ... weight  for nodes ijk(i) and (1-wts(i)) for ijk(i)+1
      subroutine modgrid_find_ijks(obj,nc, gc,ijk,wts)
      type(modgrid_struct),intent(in) :: obj
      integer,intent(in)              :: nc
      real,intent(in)                 :: gc(*)
      integer,intent(out)             :: ijk(*)
      real,intent(out)                :: wts(*)
      integer    :: i
      do i = 1,min(nc,obj%rank)
        call modgrid_find_ijk(obj,i, gc(i),ijk(i),wts(i))
      enddo
      return
      end subroutine modgrid_find_ijks

      subroutine modgrid_find_ijk(obj,dim, gc,ijk,wt)
      type(modgrid_struct),intent(in) :: obj
      integer,intent(in)              :: dim
      real,intent(in)                 :: gc
      integer,intent(out)             :: ijk
      real,intent(out)                :: wt
      real      :: rslice
      real      :: val,valmax,valmin
      ijk = 0
      wt  = 1.0
      if(dim< 1 .or. dim>obj%rank) return
      val = gc
      valmin = obj%o_grid(dim)
      valmax = obj%o_grid(dim) + (obj%n_grid(dim)-1)*obj%d_grid(dim)
      if(val < valmin) val = valmin
      if(val > valmax) val = valmax
      rslice =  1. + (val - obj%o_grid(dim))/obj%d_grid(dim)
      rslice = max(1.,rslice)
      if(rslice>obj%n_grid(dim)) rslice = obj%n_grid(dim)
      ijk = rslice
      wt  = 1.0 - (rslice - float(ijk))
      wt  = max(0.00,wt)
      wt  = min(1.00,wt)
      return
      end subroutine modgrid_find_ijk

      ! assumes that one of u,v,w is aligned with the Z axis
      ! and that the other two axes have no component along Z
      ! udirvec ... unit direction vectors for du,dv,dw
      !             udirvec(xyz comp, uvw axis)
      !             du = udirvec(1:3,1)
      !             dv = udirvec(1:3,2)
      !             dw = udirvec(1:3,3)
      integer function modgrid_xyz_to_uvw(udirvec, oxyz, zlab,&
       ntodo, X,Y,Z,U,V,W) result(status)
      real,intent(in)                 :: udirvec(3,3)
      real,intent(in)                 :: oxyz(:)
      real,intent(in)                 :: X(:),Y(:),Z(:)
      real,intent(out)                :: u(:),v(:),w(:)
      integer,intent(in)              :: ntodo
      integer,intent(in)              :: zlab
      real      :: a11,a22,a12,a21
      real      :: dz
      real      :: idet
      real      :: delx,dely,delz
      integer   :: i
      status = -1
      u = 0
      v = 0
      w = 0
      if(zlab == 3) then ! z axis is slow axis
        a11 = udirvec(1,1)
        a12 = udirvec(1,2)
        a21 = udirvec(2,1)
        a22 = udirvec(2,2)
        idet = 1.0/(a11*a22 - a12*a21)
        dz = udirvec(3,3)
        status = 0
      endif
      if(zlab == 1) then ! z axis is fast axis
        a11 = udirvec(1,2)
        a12 = udirvec(1,3)
        a21 = udirvec(2,2)
        a22 = udirvec(2,3)
        dz  = udirvec(3,1)
        idet = 1.0/(a11*a22 - a12*a21)
        status = 0
      endif
      if(status < 0) return
      do i = 1,ntodo
        delx= X(i) - oxyz(1)
        dely= Y(i) - oxyz(2)
        delz= Z(i) - oxyz(3)
        u(i) = 1.0 + (a22*delx - a12*dely)*idet
        v(i) = 1.0 + (a11*dely - a21*delx)*idet
        w(i) = 1.0 + delz / dz
      enddo

      return
      end function modgrid_xyz_to_uvw

      integer function modgrid_rd77(fname,stdo,rank,name,pname,punits, &
       hdwd, ng,og,dg) result(status)
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      integer,intent(out)             :: rank
      character(len=*),intent(out)    :: name
      character(len=*),intent(out)    :: pname
      character(len=*),intent(out)    :: punits
      integer,intent(out)             :: hdwd(:)
      integer,intent(out)             :: ng(:)
      real,intent(out)                :: og(:)
      real,intent(out)                :: dg(:)
      type(modgrid_struct),pointer    :: obj
      integer    :: i_err

      nullify (obj) ! jpa

      status = -1
      i_err = modgrid_rd(obj,fname,stdo,1,10000)
      if(i_err<0) return
      call modgrid_get_griddesc(obj,name,pname,punits,rank,&
       hdwd,ng,og,dg)
      call modgrid_delete(obj)
      status = 0
      return
      end function modgrid_rd77

! read from file and create obj

      integer function modgrid_rdoo(obj,fname,stdo,sslice,nslice)&
      result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      integer,intent(in)              :: sslice
      integer,intent(in)              :: nslice
      character    :: ftype*8
      character(len=8)   :: wtype
      character(len=128) :: dfile
      integer      :: i_err,ss,ns
      status = -1
      nullify(obj)
! open input file and get grid description
! allocates obj
      i_err = modgrid_rddesc(obj,fname,stdo,dfile,wtype,ftype)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_rd: error reading globals ',trim(fname)
        return
      endif
      ss = sslice
      ns = nslice
!     if(present(sslice)) then
!       ss = sslice
!     endif
!     if(present(nslice)) then
!       ns = nslice
!     endif
      status = modgrid_rd_data_iclip(obj,stdo,ss,ns)
      return
      end function modgrid_rdoo

!  Read data only. Call after file global info has been scanned
!  Allocate and use internal buffer to hold the data
      integer function modgrid_rd_data_iclip(obj,stdo,sslice,nslice)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      integer,intent(in)              :: stdo
      integer,intent(in)              :: sslice
      integer,intent(in)              :: nslice
      character    :: ftype*8
      character(len=128) :: dfile
      integer      :: ns(3),ss(3)
      status = -1
      ns(1:3)= obj%n_grid(1:3)
      ss = 1
      ss(3) = min(max(1,sslice),obj%n_grid(3))
      ns(3) = min(max(1,nslice),obj%n_grid(3)-ss(3)+1)
      status = modgrid_rd_data_iclips(obj,ss,ns)
      ftype = modgrid_ftype(obj)
      dfile = modgrid_datafile(obj)
      return
      end function modgrid_rd_data_iclip

!  Read data only. Call after file global info has been scanned
!  Allocate and use internal buffer to hold the data
      integer function modgrid_rd_data_rclips(obj,axs,axe)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      real,intent(in)                 :: axs(:)
      real,intent(in)                 :: axe(:)
      integer              :: ns(3)
      integer              :: ss(3),se(3)
      status = modgrid_clip_limits(obj,axs,axe,ss,se)
      if(status <0) return
      ns(1:3) = se(1:3) - ss(1:3) + 1
      status = modgrid_rd_data_iclips(obj,ss,ns)
      return
      end function modgrid_rd_data_rclips

      integer function modgrid_rd_data_iclips(obj,ss,ns)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      integer,intent(in)              :: ns(:)
      integer,intent(in)              :: ss(:)
      character    :: ftype*8
      character    :: vtypeo*8
      character(len=128) :: dfile
      integer      :: og(3),ng(3)
      integer      :: i
      real         :: angle
      status = -1
      angle  = 0.0
      ng(1:3) = ns(1:3)
      og(1:3) = ss(1:3)
      do i = 1,3
        og(i) = max(1,og(i))
        og(i) = min(obj%n_grid(i),og(i))
        ng(i) = max(1,ng(i))
        ng(i) = min(obj%n_grid(i)-og(i)+1,ng(i))
      enddo
      ftype = modgrid_ftype(obj)
      dfile = modgrid_datafile(obj)
     !call modgrid_print(obj,obj%stdo)
      if(ftype=='TRCIO') then
        status = modgrid_rd_trcio(obj,dfile,obj%stdo,og,ng)
      endif
      if(ftype=='HGRID' .or. ftype=='LAYER' .or. ftype=='G3DL') then
        status = modgrid_rd_rmod(obj,dfile,obj%stdo,og,ng)
      endif
      if(ftype=='MSGRID') then
        status = modgrid_rd_rmod(obj,dfile,obj%stdo,og,ng)
      endif
      if(ftype=='VOXET' .or. ftype=='GSURF' .or. ftype=='SGRID') then
        status = modgrid_rd_gocad(obj,dfile,obj%stdo,og,ng)
      endif
      if(ftype=='MODSPEC') then
        !modspec is held in memory at this point
        status = modgrid_rd_modspec(obj,dfile,obj%stdo,og,ng)
        if(status==0) then
          angle = modspec_get_angle(obj%mspdata)
         !print *,' modgrid:DBG modspec angle=',angle
        endif
      endif
      if(ftype=='SEGY') then
        status = modgrid_rd_segy(obj,dfile,obj%stdo,og,ng)
      endif
      if(ftype=='SU') then
        status = modgrid_rd_su(obj,dfile,obj%stdo,og,ng)
        print *,'modgrid_rd_data_iclips: su status=',status
      endif
      if(ftype=='CPSVEL') then
        vtypeo = 'VZIN'              !force to depth and interval vel
        vtypeo = obj%cvfdata%v_type  !no conversion
        status = modgrid_rd_cvf(obj,dfile,obj%stdo,og,ng,vtypeo)
      endif 
      if(ftype=='SITI') then
        !does not convert type, does a raw read
        status = modgrid_rd_rmod(obj,dfile,obj%stdo,og,ng,.true.)
      endif
      return
      end function modgrid_rd_data_iclips

! read in velocity functions and interpolate the functions
! onto a regular grid defined by the internal modgrid obj parameters.
      integer function modgrid_rd_cvf&
      (obj,fname,stdo,ss,ns,vtypeo) result(status)
      type(modgrid_struct),intent(inout) :: obj    !arguments
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      integer,intent(in)              :: ss(:)
      integer,intent(in)              :: ns(:)
      character(len=*),intent(in)     :: vtypeo*8

      type(velio_struct),pointer      :: velio_obj !local variables
      type(prop_cvfstore_struct),pointer:: cvf_obj
      integer     :: i_err
      integer     :: hx,hy
      integer     :: ix,iy,it,i
      integer     :: opt
      character   :: msg*96
      character   :: v_type*8,v_name*8
      integer     :: nx_bins,ny_bins
      real        :: xtmp,ytmp
      integer     :: n_inp,m_inp
      integer     :: n(4)
      real        :: o1,o2,o3
      integer     :: og(3),ng(3)
      integer,pointer :: nnlx(:),nnrx(:)  !near neighbors in x
      real,pointer    :: wtlx(:),wtrx(:)  !neighbor weights in x
      integer,pointer :: nnly(:),nnry(:)  !near neighbors in y
      real,pointer    :: wtly(:),wtry(:)  !neighbor weights in y
      real,pointer    :: t_inp(:),v_inp(:)
      real,pointer    :: t_tmp(:),v_tmp(:)
      real,pointer    :: dpntr(:)
      real,pointer    :: vel(:,:,:)

      nullify (velio_obj) ! jpa

      ! initializations
      status = -1
      cvf_obj => obj%cvfdata
      nullify(t_inp)
      nullify(v_inp)
      nullify(t_tmp)
      nullify(v_tmp)
      nullify(dpntr)
      nullify(nnlx)
      nullify(nnrx)
      nullify(nnly)
      nullify(nnry)
      nullify(wtlx)
      nullify(wtrx)
      nullify(wtly)
      nullify(wtry)
      nullify(vel)
      if(.not. associated(cvf_obj) ) then
        msg = 'modgrid_rd_cvf: nil cvf object'
        go to 99
      endif
      m_inp = max(2,cvf_obj%maxpicks)
!
! QC the choice of slice data to generate
      og = ss
      ng = ns
      do i = 1,3
        og(i) = min(max(1,og(i)),obj%n_grid(i))
        ng(i) = min(max(1,ng(i)),obj%n_grid(i)-og(i)+1)
      enddo
      n(1) = obj%n_grid(1)
      n(2) = obj%n_grid(2)
      n(3) = obj%n_grid(3)
!
! - open the velocity file
      call velio_open_read (obj = velio_obj,&
        filename = fname,    &
        nfun     = cvf_obj%nvfun,&
        err      = i_err,    &
        msg      = msg,      &
        nhx      = hx,  &
        nhy      = hy)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rd_cvf: open_read err=',i_err
        write(stdo,*) 'modgrid_rd_cvf: open_read msg=',trim(msg)
        return
      endif

      nx_bins=cvf_obj%nx_bins
      ny_bins=cvf_obj%ny_bins
      msg = 'modgrid_rd_cvf: allocation error'
      allocate(nnlx(nx_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(nnrx(nx_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(nnly(ny_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(nnry(ny_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(wtlx(nx_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(wtrx(nx_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(wtly(ny_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      allocate(wtry(ny_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(t_inp(m_inp+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(v_inp(m_inp+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(t_tmp(m_inp), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(v_tmp(m_inp), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      ! allocate array to hold velocity function resampled to n(1) points
      allocate(cvf_obj%vel(ng(1),nx_bins,ny_bins), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      vel => cvf_obj%vel
!
! read in all the cps velocity functions
! - read the functions into t_inp, v_inp
      o1 = obj%o_grid(1) + (og(1)-1)*obj%d_grid(1)
      do iy = 1,ny_bins
        do ix = 1,nx_bins
          ! - read in the next function
          call velio_read_velfun (obj     = velio_obj,    &
             xcoord  = xtmp,       &
             ycoord  = ytmp,       &
             npicks  = n_inp,      &
             tpicks  = t_inp,      &
             vpicks  = v_inp,      &
             err     = i_err,      &
             msg     = msg,        &
             velname = v_name,     &
             veltype = v_type)
          if(i_err /= 0) then
            write(stdo,*) 'modgrid_rd_cvf: read_velfun err=',i_err
            write(stdo,*) 'modgrid_rd_cvf: read_velfun msg=',trim(msg)
            goto 99
          else
            ! - convert from the input type to the output type
            if(v_type /= vtypeo )  then
              call velutil_convert (veltype    = v_type,    &
              npicks     = n_inp,     &
              X          = t_inp,     &
              V          = v_inp,     &
              veltypeout = vtypeo,    &
              xout       = t_tmp,     &
              vout       = v_tmp,     &
              ierr       = i_err)
              if ( i_err /= 0 ) then
                msg='modgrid_rd_cvf: velutil_convert error '
                goto 99
              end if
            else
              t_tmp(1:n_inp) = t_inp(1:n_inp)
              v_tmp(1:n_inp) = v_inp(1:n_inp)
            endif
            v_tmp(n_inp:) = v_tmp(n_inp)  !fill to end
            v_tmp(:) = 1.0/v_tmp(:)       !invert to interp slowness
            call interpolate_i_to_r (nx_inp = n_inp,     &
              rx_inp = t_tmp,         &
              ry_inp = v_tmp,         &
              nx_out = ng(1),         &
              x0_out = o1,            &
              dx_out = obj%d_grid(1), &
              ry_out = cvf_obj%vel (:, ix, iy))
          end if
        enddo
      enddo
!
! - Determine interpolation weights for output locations
     !o2 = obj%o_grid(2) + (og(2)-1)*obj%d_grid(2)
      o2 = mth_bin_center(obj%o_grid(2), obj%d_grid(2) , og(2))
      call interpolate_find_index_g (nx_inp = nx_bins,&
        rx_inp   = cvf_obj%x_bins, &
        nx_out   = ng(2),          &
        x0_out   = o2,             &
        dx_out   = obj%d_grid(2),  &
        ix_inp_1 = nnlx,           &
        ix_inp_2 = nnrx,           &
        fx_inp_1 = wtlx,           &
        fx_inp_2 = wtrx)
     !o3 = obj%o_grid(3) + (og(3)-1)*obj%d_grid(3)
      o3 = mth_bin_center(obj%o_grid(3), obj%d_grid(3) , og(3))
      call interpolate_find_index_g (nx_inp = ny_bins,&
        rx_inp   = cvf_obj%y_bins, &
        nx_out   = ng(3),          &
        x0_out   = o3,             &
        dx_out   = obj%d_grid(3),  &
        ix_inp_1 = nnly,           &
        ix_inp_2 = nnry,           &
        fx_inp_1 = wtly,           &
        fx_inp_2 = wtry)
!
! init data object and allocate buffer to hold the data
      i_err =  modgrid_put_data_iclip(obj,og,ng)
      if(i_err /= 0) then
        msg = 'modgrid_rd_cvf: allocation error(dpntr)'
        goto 99
      endif
! get pointer to the data buffer
      i_err =  modgrid_get_data(obj,dpntr)
      if(i_err /= 0) then
        msg = 'modgrid_rd_cvf: failed to get dpntr'
        goto 99
      endif
! construct velocity on regular output grid
! results placed in object data buffer
      call  modgmem_set_inverted(obj%memdata,.true.)
      opt = 0
      do iy = 1,ng(3)
        do ix = 1,ng(2)
          do it = 1,ng(1)
            opt = opt+1
            dpntr(opt) = &
                  wtlx(ix) * wtly(iy) * vel(it, nnlx(ix) , nnly(iy)) &
                + wtrx(ix) * wtly(iy) * vel(it, nnrx(ix) , nnly(iy)) &
                + wtlx(ix) * wtry(iy) * vel(it, nnlx(ix) , nnry(iy)) &
                + wtrx(ix) * wtry(iy) * vel(it, nnrx(ix) , nnry(iy))
          enddo   !it
        enddo   !ix
      enddo   !iy
!
! return data to non inverted form after interpolation
      call modgrid_recip(obj,i_err)
      if(i_err < 0) then
        write(stdo,*) 'modgrid_rd_cvf: modgrid_recip failed'
      endif

      status = 0
 99   continue
      if (associated(velio_obj)) call velio_close (velio_obj)
      if (associated(t_inp)) deallocate (t_inp)
      if (associated(v_inp)) deallocate (v_inp)
      if (associated(t_tmp)) deallocate (t_tmp)
      if (associated(v_tmp)) deallocate (v_tmp)
      if(associated(nnlx)) deallocate(nnlx)
      if(associated(nnrx)) deallocate(nnrx)
      if(associated(nnly)) deallocate(nnly)
      if(associated(nnry)) deallocate(nnry)
      if(associated(wtlx)) deallocate(wtlx)
      if(associated(wtrx)) deallocate(wtrx)
      if(associated(wtly)) deallocate(wtly)
      if(associated(wtry)) deallocate(wtry)
      if (associated(cvf_obj%vel)) deallocate (cvf_obj%vel)
      nullify(cvf_obj%vel)
      if(status /=0) then
        write(stdo,*) 'modgrid_rd_cvf: msg=',trim(msg)
      endif
      return
      end function modgrid_rd_cvf

! reads trcio data. Saves data but not the headers
      integer function modgrid_rd_trcio(obj,fname,stdo,ss,ns)&
       result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      integer,intent(in)              :: ss(:)
      integer,intent(in)              :: ns(:)
      character                  :: msg*88
      character                  :: mode*4
      type(trcio_struct),pointer :: file
      real,pointer :: dpntr(:)

      integer      :: tnum,tnum_old
      integer      :: m1,m2
      integer      :: j,k
      integer      :: n(4)
      integer      :: og(3),ng(3)
      real,pointer :: tr(:)
      double precision hd(64)
      integer      :: i1,i2
      integer      :: i_err
      status = -1
      nullify(tr)
      mode = 'r'
      file   => trcio_open(fname,mode)
     !print *,'modgrid_rd_trcio:DBG file ftyp=',file%ftype
      if( .not. associated(file) ) then
        write(stdo,*) 'modgrid_rd_trcio: failed to open ',trim(fname)
        return
      endif
      n(1) = obj%n_grid(1)
      n(2) = obj%n_grid(2)
      n(3) = obj%n_grid(3)
      og(1:3) = ss(1:3)   !starting grid point
      ng(1:3) = ns(1:3)   !number of grid points
      do j = 1,3
        og(j) = min(max(1,og(j)),obj%n_grid(j))
        ng(j) = min(max(1,ng(j)),obj%n_grid(j)-og(j)+1)
      enddo
!
! allocate input buffer for reading grid values
      allocate(tr(n(1)), stat = i_err)
      if(i_err /= 0) then
        msg = 'modgrid_rd_trcio: allocation error(tr)'
        goto 99
      endif
!
! preallocate array to hold data
      i_err =  modgrid_put_data_iclip(obj,og,ng)
      if(i_err /= 0) then
        msg = 'modgrid_rd_trcio: allocation error(dpntr)'
        goto 99
      endif
!
! get pointer to the data buffer
      i_err =  modgrid_get_data(obj,dpntr)
! set the buffer values
      tnum_old = -1
      i1 = 1
      m1 = og(1)
      m2 = og(1)+ng(1)-1
      do k =og(3),og(3)+ng(3)-1  !loop over desired slices
      do j =og(2),og(2)+ng(2)-1  !loop over traces in a slice
        tnum = (k-1)*n(2) + j
        if(tnum /= tnum_old+1 ) then
          tnum_old = tnum
          i_err = trcio_read_trace(file,hd,tr,tnum)
        else
          i_err = trcio_read_trace(file,hd,tr)
        endif
        if(i_err /= TRCIO_OK) then
          msg = 'modgrid_rd_trcio: error reading data'
          goto 99
        endif
        i2=i1+ng(1)-1
        dpntr(i1:i2) = tr(m1:m2)
        i1=i2+1
      enddo
      enddo
      i_err = trcio_close(file)
      if(associated(tr)) deallocate(tr, stat=i_err)
      status = 0
      return
 99   continue
      write(stdo,*) trim(msg)
      i_err = trcio_close(file)
      if(associated(tr)) deallocate(tr, stat=i_err)
      return
      end function modgrid_rd_trcio
!
      integer function modgrid_rd_segy(obj,fname,stdo,ss,ns)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: fname
      integer,intent(in)                 :: stdo
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      ! a segy file is handled by trcio
      status = modgrid_rd_trcio(obj,fname,obj%stdo,ss,ns)
      return
      end function modgrid_rd_segy
!
      integer function modgrid_rd_rmod(obj,fname,stdo,ss,ns,native)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: fname
      integer,intent(in)                 :: stdo
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      logical,optional,intent(in)    :: native !true -> raw read with no swap

      integer    :: lun,i_err
      character  :: msg*80
      integer(kind=1),pointer :: i1tr(:)
      integer(kind=2),pointer :: i2tr(:)
      real,pointer:: dpntr(:),tr(:)
      integer    :: n(3),ng(3),og(3)
      integer    :: i,j,k,endian,nr
      integer    :: i1,i2
      integer    :: m1,m2
      integer    :: tnum,tnum_old
      integer    :: bsiz
      integer    :: wrdsz
      character(len=4)  :: mode
      character(len=8)  :: wname
      logical    :: native_read
      logical    :: signed
      integer    :: off

      native_read = .false.
      off=0
      tnum_old = -1
      status = -1
      mode='r'
      lun = cio_fopen(fname,mode) !'r')
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rd_rmod:(open error) ',trim(fname)
        return
      endif
!
! get and condition grid sizes.
      n(1) = obj%n_grid(1)!n(:) = sizes on disk
      n(2) = obj%n_grid(2)
      n(3) = obj%n_grid(3)
      og(1:3) = ss(1:3)   !starting grid point
      ng(1:3) = ns(1:3)   !number of grid points
      do i = 1,3
        og(i) = min(max(1,og(i)),obj%n_grid(i))
        ng(i) = min(max(1,ng(i)),obj%n_grid(i)-og(i)+1)
      enddo
!
! preallocate array to hold data
      i_err =  modgrid_put_data_iclip(obj,og,ng)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rd_rmod: allocation error?'
        write(stdo,*) 'modgrid_rd_rmod: ng= ',ng
        status = cio_fclose(lun)
        return
      endif
!
! get pointer to the data buffer
      i_err =  modgrid_get_data(obj,dpntr)
      if(.not.associated(dpntr) ) write(stdo,*) &
       'modgrid_rd_rmod:DBG0 null dpntr ?'
      if(i_err .ne.0) then
        write(stdo,*) 'modgrid_rd_rmod:DBG0 get data error ?'
      endif

      wrdsz = 4
      wname = 'REAL'
      signed = .true.
      if(wname(1:1)=='U') signed = .false.
      if(associated(obj%dskdata) ) then
         wrdsz = obj%dskdata%databits/8
         wname = obj%dskdata%dataname
         if(obj%dskdata%ftype=='MSGRID') off = obj%dskdata%datapos
      endif
      wrdsz = max(1,wrdsz)
!
! allocate input buffer for reading grid values
      allocate(tr(n(1)), stat = i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rd_rmod: allocation error(tr)'
        return
      endif
      if(index(wname,'SHORT') > 0)then
        allocate(i2tr(n(1)), stat = i_err)
        if(i_err /= 0) then
          write(stdo,*) 'modgrid_rd_rmod: allocation error(i2tr)'
          return
        endif
      endif
      if(index(wname,'CHAR') > 0)then
        allocate(i1tr(n(1)), stat = i_err)
        if(i_err /= 0) then
          write(stdo,*) 'modgrid_rd_rmod: allocation error(i1tr)'
          return
        endif
      endif
! set the buffer values
! hgrid data is stored as big endian
      if(present(native)) then !bypass swapping?
          native_read = native
      endif
      endian = swap_endian()
      i1 = 1
      m1 = og(1)
      m2 = og(1)+ng(1)-1
      bsiz=wrdsz*n(1)
      do k =og(3),og(3)+ng(3)-1  !loop over desired slices
      do j =og(2),og(2)+ng(2)-1  !loop over traces in a slice
        tnum = (k-1)*n(2) + j
        if(tnum /= tnum_old+1 ) then
         !off = (tnum-1)*bsiz
         !i_err = cio_fseek(lun,off,0)  RSD 10/05
          i_err = cio_fseek(lun,bsiz,tnum-1,off,0)
          if(i_err /=0) then
            msg = 'modgrid_rd_rmod: error cio_fseek'
            print *,'modgrid_rd_rmod: tnum=',tnum,',bsiz=',bsiz
            goto 99
          endif
          tnum_old = tnum
        endif
        if(wrdsz==4) then
          nr = cio_fread(tr,wrdsz,n(1),lun)
          if(endian ==0 .and. .not. native_read) then
            !file big endian, but mem is little
            call swap_bytes(tr(1:n(1)))
          endif
        else if(wrdsz==2) then
          nr = cio_fread(i2tr,wrdsz,n(1),lun)
          if(endian ==0 .and. .not. native_read) then
            !file big endian, but mem is little
            call swap_bytes(i2tr(1:n(1)))
          endif
          tr(1:n(1)) = i2tr(1:n(1))
          if(.not. signed) then
             do i = 1,n(1)
               if(tr(i) < 0) tr(i) = tr(i) + 65536.0
             enddo
          endif 
        endif
        if(wrdsz==1) then
          nr = cio_fread(i1tr,wrdsz,n(1),lun)
         !if(endian ==0 .and. .not. native_read) then
         !  !file big endian, but mem is little
         !  call swap_bytes(i2tr(1:n(1)))
         !endif
          tr(1:n(1)) = i1tr(1:n(1))
          if(.not. signed) then
             do i = 1,n(1)
               if(tr(i) < 0) tr(i) = tr(i) + 256.0
             enddo
          endif 
        endif
        if(nr <= 0) then
         msg = 'modgrid_rd_rmod: error reading data'
         goto 99
        endif
        i2=i1+ng(1)-1
        dpntr(i1:i2) = tr(m1:m2)
        i1=i2+1
      enddo
      enddo
      status = 0
      i_err = cio_fclose(lun)
      if(associated(tr) ) deallocate(tr, stat=i_err)
      if(associated(i2tr) ) deallocate(i2tr, stat=i_err)
      if(associated(i1tr) ) deallocate(i1tr, stat=i_err)
      return
 99   continue
      status = cio_fclose(lun)
      if(associated(tr) ) deallocate(tr, stat=i_err)
      return
      end function modgrid_rd_rmod
!
! modspec layered model is in memory.
! we need to create the grid slices on the fly.
! Data is returned in increasing x & y model order.
      integer function modgrid_rd_modspec(obj,fname,stdo,ss,ns)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: fname
      integer,intent(in)                 :: stdo
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      real,pointer:: dpntr(:),vz(:)
      integer          :: n(3),ng(3),og(3)
      integer          :: i,j,k
      integer          :: i1,i2
      integer          :: gx,gy
      integer          :: xlab,ylab,zlab
      integer          :: m1,m2
      integer          :: layer,ipsdm,i_err
      real             :: dmax,depth
      real             :: velocity
      double precision :: xorg
      double precision :: yorg
      integer          :: nx
      integer          :: ny
      integer          :: nz
      real             :: dx
      real             :: dy
      real             :: dz
      real             :: oz
      integer          :: hx,hy
      real             :: angle
      integer          :: nlay
      real             :: a(3),b(3),ox,oy,oxg,oyg,dxg,dyg
      real             :: odxg,odyg
      logical          :: has_coef
      character(len=4) :: units
      character(len=4) :: xyz
      integer          :: isx,isy
      real             :: line,trace
      double precision :: xs,ys

      status = -1
      xyz  = ' '
      if(.not. associated(obj%mspdata)) then
        write(stdo,*) 'modgrid_rd_modspec: null modspec object'
        return
      endif
    ! returned values are a cps viewpoint
      call modspec_getdesc(obj%mspdata,nlay,angle,units,&
      nx,xorg,dx,ny,yorg,dy,nz,oz,dz)
      has_coef = modspec_get_coef_full(obj%mspdata,a,b,&
           ox, oy, dxg, dyg, oxg,oyg,hx,hy,xyz)
!
! get and condition grid sizes.
! for modspec we assume 1=Z, 2=X|Y, 3=Y|X
      n(1) = obj%n_grid(1)
      n(2) = obj%n_grid(2)
      n(3) = obj%n_grid(3)
      og(1:3) = ss(1:3)   !starting grid point
      ng(1:3) = ns(1:3)   !number of grid points
      do i = 1,3
        og(i) = min(max(1,og(i)),obj%n_grid(i))
        ng(i) = min(max(1,ng(i)),obj%n_grid(i)-og(i)+1)
      enddo
!
! preallocate array to hold data
      i_err =  modgrid_put_data_iclip(obj,og,ng)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rd_modspec: allocation error?'
        return
      endif
      allocate(vz(n(1)),stat=i_err)
      if(i_err/=0) then
        write(obj%stdo,*) 'modgrid_rd_modspec: allocate failure'
        return
      endif
     !call modspec_get_order(obj%mspdata,xyz)
!
! get pointer to the data buffer
      i_err =  modgrid_get_data(obj,dpntr)
! set the buffer values
      xlab = index(xyz,'X')
      ylab = index(xyz,'Y')
      zlab = index(xyz,'Z')
      ipsdm = 2
      dmax = n(1)*obj%d_grid(1)
      i1 = 1
      isx= 1
      if(dxg<0) isx=-1
      isy= 1
      if(dyg<0) isy=-1
      odxg = 1.0/dxg
      odyg = 1.0/dyg
      m1 = og(1)
      m2 = og(1)+ng(1)-1
     !print *,'   rd_modspec DBG:og=', og,' ng=',ng
     !print *,'   rd_modspec DBG:ox,oxg', ox,oxg
     !print *,'   rd_modspec DBG:oy,oyg', oy,oyg
     !print *,'   rd_modspec DBG:isx isy=', isx,isy
      do k =og(3),og(3)+ng(3)-1  !loop over slow output axis
      do j =og(2),og(2)+ng(2)-1  !loop over faster output axis
        if(hx==7 .or. hx==8) then ! cps grid index to modpsec grid index
          if(xlab==2) then
            trace = obj%o_grid(2) + (j-1)*obj%d_grid(2)
            line = obj%o_grid(3) + (k-1)*obj%d_grid(3)
          else
            line = obj%o_grid(2) + (j-1)*obj%d_grid(2)
            trace = obj%o_grid(3) + (k-1)*obj%d_grid(3)
          endif
        else !  convert 17,18 to 7,8
          if(xlab==2) then
            xs = obj%o_grid(2) + (j-1)*obj%d_grid(2)
            ys = obj%o_grid(3) + (k-1)*obj%d_grid(3)
          else
            ys = obj%o_grid(2) + (j-1)*obj%d_grid(2)
            xs = obj%o_grid(3) + (k-1)*obj%d_grid(3)
          endif
          trace = grid_get_xgrid_coord(obj%gobj,xs,ys)
          line  = grid_get_ygrid_coord(obj%gobj,xs,ys)
        endif
        !convert line-trace to modspec grid index
        i_err = modspec_lt2ixy(obj%mspdata, trace, line,gx,gy)
        vz = 0.0
        do i = m1,m2
          depth =  obj%o_grid(zlab) + (i-1)*dz
          i_err = modspec_getv(obj%mspdata,ipsdm,&
                  dmax,depth,velocity,layer,gx,gy)
          if(i_err==0) then
            vz(i) = velocity
          else
            vz(i) = mspec_gnull
          endif
        enddo
        i2=i1+ng(1)-1
        dpntr(i1:i2) = vz(m1:m2)
        i1=i2+1
      enddo
      enddo

      deallocate(vz)

      status = 0
     !print *,'modgrid_rd_modspec: DBG status=',status
      return
      end function modgrid_rd_modspec

      integer function modgrid_rd_gocad(obj,fname,stdo,ss,ns)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: fname
      integer,intent(in)                 :: stdo
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      real  :: scale,add
      integer :: ierr
      ! voxet binary and rmod binary data can be read the same way
      status = modgrid_rd_rmod(obj,fname,stdo,ss,ns)
      if(obj%dskdata%ftype=='GSURF') then
        add = modgrid_dskdata_add(obj)
        scale = modgrid_dskdata_scale(obj)
        if(scale .ne.0.0) then
          ierr = modgmem_muladd(obj%memdata,obj%rnil, scale,add)
        endif
      endif
      return
      end function modgrid_rd_gocad

      integer function modgrid_saveas(fname,stdo,oname,&
        xgs,xge,ygs,yge,maxmem,gocad,oorder,nz,dz)&
      result(status)
      character(len=*),intent(in)     :: fname       ! arguments
      character(len=*),intent(inout)  :: oname
      integer,intent(in)              :: stdo
      integer,intent(in)              :: ygs,yge
      integer,intent(in)              :: xgs,xge
      integer,intent(in)              :: maxmem
      integer,intent(in)              :: gocad
      character(len=*),intent(in)     :: oorder       ! arguments
      integer,optional,intent(in) :: nz
      real,optional,intent(in)    :: dz

      print *, 'modgrid_saveas: this is obsolete, call model_regrid_prop &
      &instead'
      status = -1
      return
      end function modgrid_saveas
!
      ! apply a mask to n_grid,o_grid, oxyz, and axis
      subroutine modgrid_clip_axis(obj,ax, ax_s,ax_e)
      type(modgrid_struct),intent(inout)  :: obj
      integer,intent(in)   :: ax_s,ax_e
      integer,intent(in)   :: ax
      integer  :: n1,n2
      integer  :: new_n
      real     :: new_o
      real     :: scalea,scaleo
      real     :: axis(3)
      real     :: oxyz(3)
      integer  :: dec
      dec = 1

      n1 = max(ax_s, 1)
      n1 = min(ax_s,obj%n_grid(ax))
      n2 = max(ax_e,n1)
      n2 = min(ax_e,obj%n_grid(ax))
      dec= max(1,dec)
      dec= min(obj%n_grid(ax),dec)
      new_n = (n2 - n1 + 1)
      new_o = obj%o_grid(ax) + (n1 - 1)*obj%d_grid(ax)
      scalea = 1.0
      if(obj%n_grid(ax) > 1) &
        scalea = float(max(2,new_n)-1)/float(obj%n_grid(ax)-1)
      scaleo = 0.0
      if(obj%n_grid(ax) > 1) &
         scaleo = float(n1-1)/float(obj%n_grid(ax)-1)
      if(ax==1) then
        oxyz(1:3)  = obj%oxyz(1:3) + scaleo*obj%axis1(1:3)
        axis = scalea * obj%axis1
        obj%axis1(1:3) = axis(1:3)
      endif
      if(ax==2) then
        oxyz(1:3)  = obj%oxyz(1:3) + scaleo*obj%axis2(1:3)
        axis = scalea * obj%axis2
        obj%axis2(1:3) = axis(1:3)
      endif
      if(ax==3) then
        oxyz(1:3)  = obj%oxyz(1:3) + scaleo*obj%axis3(1:3)
        axis = scalea * obj%axis3
        obj%axis3(1:3) = axis(1:3)
      endif
      obj%oxyz(1:3) = oxyz(1:3)
      obj%n_grid(ax) = new_n
      obj%o_grid(ax) = new_o
      return
      end subroutine modgrid_clip_axis

      ! convert a real clip window to integer grid points
      integer function modgrid_clip_limits(obj,&
       ax_min,ax_max,clips,clipe) result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      real,intent(in   )    :: ax_min(:)
      real,intent(in   )    :: ax_max(:)
      integer,intent(inout) :: clips(:)
      integer,intent(inout) :: clipe(:)
      integer  :: i
      ! translate clip window to grid nodes
      do i = 1,3
        status = modgrid_clip_limit(obj,i,ax_min(i),ax_max(i),&
             clips(i),clipe(i))
        if(status < 0) return
      enddo
      return
      end function modgrid_clip_limits

      integer function modgrid_clip_limit(obj,&
       ax,ax_min,ax_max,clips,clipe) result(status)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      integer,intent(in)    :: ax
      real,intent(in   )    :: ax_min
      real,intent(in   )    :: ax_max
      integer,intent(inout) :: clips
      integer,intent(inout) :: clipe
      integer  :: i1
      status = -1
      clips = 1
      clipe = 1
      if(ax < 1 .or. ax > obj%rank) return
      status = 0
      ! translate clip window to grid nodes
     !i1 = 1 + nint(max(ax_min - obj%o_grid(ax),0.0)/obj%d_grid(ax))
      i1 = mth_bin_number(obj%o_grid(ax), obj%d_grid(ax) , ax_min )
      i1 = min(obj%n_grid(ax),i1)
      i1 = max(1,i1)
      clips = i1
     !i1 = 1 + nint(max(ax_max - obj%o_grid(ax),0.0)/obj%d_grid(ax))
      i1 = mth_bin_number(obj%o_grid(ax), obj%d_grid(ax) , ax_max )
      i1 = min(obj%n_grid(ax),i1)
      i1 = max(1,i1)
      clipe = i1
      return
      end function modgrid_clip_limit

!
! Transpose Operation: Time to Depth order, (T-X-Y ==> X-Y-T)
! Given a subcube in memory with axes T-X-Y , where axis Y starts at sslice
! i.e. icube(1:ngi(1),1:ngi(2),sslice:sslice+ngi(3)-1)
! write it to an output file in order X-Y-T
! i.e. obuf(sslice:sslice+ngi(3)-1, 1:ngi(2), 1:ngi(1))
! writes ngo(2)*ngo(3) elements at a time
! save only n2s:n2e,n3s:n3e from the input cube
! n3s <= sslice <=n3e
      subroutine modgrid_TXY2XYT(ngi,icube,sslice,obuf,ufi,&
        n2s,n2e,n3s,n3e)
      integer,intent(in) :: ngi(3)  !dimension of sub cube in memory
      integer,intent(in) :: sslice  !starting slice of macro cube in mem
      integer,intent(in) :: ufi
      real,intent(out)   :: obuf(:)
      real,intent(in)    :: icube(:) !3D sub cube in memory
      integer,intent(in) :: n2s,n2e
      integer,intent(in) :: n3s,n3e
!
      integer   :: ngo(3)
      integer   :: ipt,opt
      integer   :: i1_out,i2_out,i3_out
      integer   :: nel,nwr,i_err
      integer   :: bsiz,wblk
      integer   :: i2_abs
      ngo(3) = ngi(1)
      ngo(2) = ngi(3)
      ngo(1) = ngi(2)
      if(n3e>0) THEN
        ngo(2) = max(1,n3e - n3s + 1)
      else
        ngo(2) = max(1,ngi(3) - n3s + 1)
      endif
      if(n2e>0) THEN
        ngo(1) = max(1,n2e - n2s + 1)
      else
        ngo(1) = max(1,ngi(2) - n2s + 1)
      endif
      bsiz = 4*ngo(1)
      do i3_out = 1,ngo(3)      !T
        opt = 1
        do i2_out = 1,ngi(3)    !Y
          i2_abs = sslice + i2_out -1
          if(i2_abs< n3s .or. i2_abs>n3e)  cycle
          ipt = i3_out + (i2_out-1)*ngi(1)*ngi(2) + (n2s-1)*ngi(1)
          do i1_out = n2s,n2e   !X
              obuf(opt) = icube(ipt)
              opt = opt+1
              ipt = ipt+ngi(1)
          enddo
        enddo
       !off = (sslice - n3s)*ngo(1) + (i3_out-1)*ngo(1)*ngo(2)
        wblk = (sslice - n3s) + (i3_out-1)*ngo(2)
       !i_err = cio_fseek(ufi,4*off,0)   !off bytes from the origin
        i_err = cio_fseek(ufi,bsiz,wblk,0,0)
        nel = ngo(1)*ngo(2)  ! number of Y slices in memory
        nwr = cio_fwrite(obuf,1,4*nel,ufi)
      enddo
      return
      end subroutine modgrid_TXY2XYT
!
! Transpose Operation:
! transpose from Depth to Time order, (X-Y-T ==> T-X-Y) or vice versa
! Given a subcube in memory with dimensionality ngi(:)
! i.e. icube(1:ngi(1),1:ngi(2),1:ngi(3))
! ilabels(i) ... label for axis i of the input cube icube
! olabels(i) ... label for axis i of the output cube
!                ilabels and olabels determine how the output
!                is permutated relative to the input
! mems(i)    ... start index of axis i of the input cube that is in memory
!                mems(i) >= 1
! meme(i)    ... end index of axis i of the input cube that is in memory
!                meme(i) >= mems(i) , meme(i) <= size of axis i on disk
! clips(i)   ... start index of axis i to pass to output
! clipe(i)   ... end index of axis i to pass to output
!                output cube dimensions = clipe(i)-clpips(i)+1
!                (output dimensions are permutated)
      integer function modgrid_write_buffer(ngi,icube, &
        ilabels,olabels,obuf,ufi,clips,clipe,mems,meme,msg) result(status)
      integer,intent(in) :: ngi(:)  !dimension of sub cube in memory
      real,intent(in)    :: icube(ngi(1),ngi(2),ngi(3)) !3D sub cube in memory
      integer,intent(in) :: ufi
      character(len=*),intent(in) :: ilabels(3)
      character(len=*),intent(in) :: olabels(3)
      character(len=*),intent(inout) :: msg
      real,intent(out)   :: obuf(:)
      integer,intent(in) :: clips(*)
      integer,intent(in) :: clipe(*)
      integer,intent(in) :: mems(*)   !start index loaded in memory
      integer,intent(in) :: meme(*)
!
      integer   :: ocd(3)    !dimensions of output cube
      integer   :: count
      integer   :: opt
      integer   :: i, i1_out,i2_out,i3_out
      integer   :: nel,nwr,i_err,seqno
      integer   :: bsiz,wblk,wbyt
      integer   :: otoi(3)
      integer   :: itoo(3)
      integer   :: itrip(3)  !input data triplet
      integer   :: otrip(3)  !output data triplet
      integer   :: oclips(3)
      integer   :: oclipe(3)
      integer   :: omems(3)
      integer   :: omeme(3)
      integer   :: spt(3),ept(3)
      integer   :: oxlabel,oylabel,ozlabel
      integer   :: endian
      status = -1
      i_err = modgrid_build_map(ilabels,olabels,otoi)
      i_err = modgrid_build_map(olabels,ilabels,itoo)
      i_err = modgrid_xyz_order(olabels,oxlabel,oylabel,ozlabel)
      if(i_err == -1) then
        msg='modgrid_write_buffer: ERROR!'
        return
      endif
      endian = swap_endian()
      call modgrid_map_address(clips,otoi,oclips)
      call modgrid_map_address(clipe,otoi,oclipe)
      call modgrid_map_address(mems,otoi,omems)
      call modgrid_map_address(meme,otoi,omeme)
      do i = 1,3
        if(omems(i) > oclipe(i)) return  !no intersection of clip and memory
        if(omeme(i) < oclips(i)) return  !no intersection of clip and memory
        if(omems(i) >= oclips(i) .and. &
           omeme(i) <= oclipe(i) )  then !memory entirely within clip
          spt(i) = omems(i)
          ept(i) = omeme(i)
        else                             !some overlap
          spt(i) = max(omems(i),oclips(i))
          ept(i) = min(omeme(i),oclipe(i))
        endif
        ocd(i) = oclipe(i) - oclips(i) + 1
      enddo

      count = 0
      do i3_out = omems(3), omeme(3)
        if(i3_out < oclips(3) .or. i3_out > oclipe(3)) cycle
        otrip(3) = i3_out-omems(3)+1
        do i2_out = omems(2), omeme(2)
          if(i2_out < oclips(2) .or. i2_out > oclipe(2)) cycle
          otrip(2) = i2_out-omems(2)+1
          opt = 1
          do i1_out = omems(1),omeme(1)
            otrip(1) = opt
            itrip(1) = otrip(itoo(1)) !map in (i,j,k) to out (l,m,n)
            itrip(2) = otrip(itoo(2))
            itrip(3) = otrip(itoo(3))
            obuf(opt) = icube(itrip(1),itrip(2),itrip(3))
            opt = opt+1
          enddo
          nel = ept(1) - spt(1) + 1
          if(nel > 0) then
            bsiz = 4*ocd(1)
            wbyt = spt(1) - oclips(1)
            seqno = (i3_out-oclips(3))*ocd(2) + &
                (i2_out - oclips(2))
           !off = (spt(1) - oclips(1)) + seqno*ocd(1)
           !i_err = cio_fseek(ufi,4*off,0)   !off bytes from the origin
            wblk = seqno
            i_err = cio_fseek(ufi,bsiz,wblk,wbyt,0)
            i   = spt(1) - omems(1) + 1
            if(endian ==0) then !file big endian, but mem is little
              call swap_bytes(obuf(i:i+nel-1))
            endif
            nwr = cio_fwrite(obuf(i),1,4*nel,ufi)
            if(nwr < 0) then
              msg = 'modgrid_write_buffer: ERROR from cio_fwrite'
              return
            endif
            count = count + nwr
          endif
        enddo  !i2_out
      enddo  !i3_out
      status = 0
      return
      end function modgrid_write_buffer

! return 2D slice(s), that is for the range [zs,ze] of a 3D model
! return the slice defined on the grid (n1,o1,d1) (n2,o2,d2)
! Assumptions:
! 1) model has to have axis 3 labeled as DEPTH or TIME
! 2) model has been opened and the grid description is in memory
! 3) axis 1 and 2 on disk are in the same order as requested
!    the axis labels must map to CPS header words(i.e. XBASEMENT -> 17)
! buff   .... allocates and returns slices in buff
!             repeat calls will deallocate buff and reallocate
! maxmem .... maximum number of words to allow for model storage in memory
!             (internal buffer + buff returned to user, so in worst case
!               scenario, buff will only be allowed  half this amount)
! zs,ze  ...  start & end grid coordinate of the requested slices.
! h1      ... CPS header consistent with axis-1
! n1,o1,d1... grid description for fast axis of slice
! h2      ... CPS header consistent with axis-2
! n2,o2,d2... grid description for slow axis of slice
      integer function modgrid_get_zslices(obj,zs,ze,h1,n1,o1,d1,h2,n2,o2,d2,&
      buff,nz,oz,dz, maxmem) result(status)
      type(modgrid_struct),intent(inout)  :: obj
      integer,intent(in)  :: h1,h2,n1,n2
      real,intent(in)     :: zs,ze,o1,d1,o2,d2
      integer,intent(in)  :: maxmem
      real,pointer        :: buff(:,:,:)
      integer,intent(out) :: nz
      real,intent(out)    :: oz,dz

      integer        :: mbuff
      integer        :: hdwd(4),ng(4)
      real           :: og(4),dg(4)
      real           :: wt

      integer        :: i,i_err
      integer        :: slice1,nslice
      integer        :: zindex,izs,ize
      integer        :: xlabel,ylabel,zlabel
    ! integer        :: nel
      integer(kind=8) :: nel8
      integer        :: mslice

      integer        :: n_out(3)
      real           :: o_out(3),d_out(3)
      status = -1

      if(associated(buff)) deallocate(buff,stat=i_err)
      nullify(buff)
      nel8 = modgrid_size8(obj)
      i_err = modgrid_xyz_order(obj%label,xlabel,ylabel,zlabel)
      if(i_err < 0) return
      if(zlabel == -1 .or. zlabel /= 3) then
        write(obj%stdo,*) 'modgrid_get_zslices: z-axis missing or out of order'
        write(obj%stdo,*) 'modgrid_get_zslices: z-axis=',zlabel
        return
      endif
      do i = 1,obj%rank
        call modgrid_get_griddesc(obj,i, hdwd(i),ng(i),og(i),dg(i))
      enddo
      if(hdwd(1)  /= h1 .or. hdwd(2) /= h2) then
        write(obj%stdo,*) 'modgrid_get_zslices: input data is in wrong order'
        write(obj%stdo,*) 'modgrid_get_zslices: request - h1=',h1,' h2=',h2
        write(obj%stdo,*) 'modgrid_get_zslices: on disk - h1=',hdwd(1),&
        ' h2=',hdwd(2)
        return
      endif
     ! map zs,ze to a zaxis index
      call modgrid_find_ijk(obj,zlabel, zs,izs,wt)
      call modgrid_find_ijk(obj,zlabel, ze,ize,wt)
      ize = min(ize+1,obj%n_grid(3))
      nz = (ize-izs+1)
      oz = zs
      dz = (ze-zs)/(max(1,ize-izs))
     ! define the output grid
      n_out(1) = n1
      n_out(2) = n2
      n_out(3) = nz
      o_out(1) = o1
      o_out(2) = o2
      o_out(3) = oz
      d_out(1) = d1
      d_out(2) = d2
      d_out(3) = dz
      if(dz<=0) dz = dg(3)
      mbuff = n_out(1)*n_out(2)*n_out(3) !memory for output buffer
      if(mbuff > maxmem) then
        write(obj%stdo,*) 'modgrid_get_zslices: maxmem too small for request'
        write(obj%stdo,*) 'modgrid_get_zslices: nz = ',nz
        write(obj%stdo,*) 'modgrid_get_zslices: mbuff = ',mbuff
        return
      endif

      mslice = (maxmem-mbuff)/(ng(1)*ng(2))
      if(mslice < nz) then
        write(obj%stdo,*) 'modgrid_get_zslices:error,can store mslice= ',mslice
        write(obj%stdo,*) 'modgrid_get_zslices:error,need to store nz = ',nz
        return
      endif
      if(modgrid_has_data(obj)) then
        ! some data already loaded
        if(modgrid_all_in_mem(obj)) then
          ! requested slices already loaded in memory
          slice1 = 1
          nslice = ng(3)
        else
          zindex = modgmem_get_org(obj%memdata,3) +&
                   modgmem_get_dim(obj%memdata,3)
          if(modgmem_get_org(obj%memdata,3) <=izs .and. zindex>=ize) then
            ! requested slices already loaded in memory
            slice1 = modgmem_get_org(obj%memdata,3)
            nslice = modgmem_get_dim(obj%memdata,3)
          else
            if(izs < modgmem_get_org(obj%memdata,3)) slice1 = max(1,ize-mslice+1)
            if(izs >= modgmem_get_org(obj%memdata,3)) slice1 = izs
            nslice = max(mslice,nz)
            nslice = min(nslice,ng(3)-izs+1)
            i_err = modgrid_rd_data_iclip(obj,obj%stdo,slice1,nslice)
            if(i_err /= 0) return
          endif
        endif
      else
        ! no data loaded yet
        if(nel8 < maxmem - mbuff) then !read in entire model
          slice1 = 1
          nslice = ng(3)
          i_err = modgrid_rd_data_iclip(obj,obj%stdo,slice1,nslice)
          if(i_err /= 0) return
        else                  !load z-slices if not in memory
          slice1 = izs
          nslice = max(mslice,nz)
          nslice = min(nslice,ng(3)-izs+1)
          i_err = modgrid_rd_data_iclip(obj,obj%stdo,slice1,nslice)
          if(i_err /= 0) return
        endif
      endif
    ! allocate(buff(mbuff),stat=i_err)
      allocate(buff(ng(1),ng(2),ng(3)),stat=i_err)
      if(i_err /=0) then
        return
      endif
      i_err = modgrid_interpolate(obj,mbuff, buff,&
        n_out,o_out,d_out)
      if(i_err /= 0) return
      status = 0
      return
      end function modgrid_get_zslices

! allocate space for the data buffer
! deallocate buffers that are already allocated
! only if the size has changed!
      integer function modgrid_alloc(obj,nelements) result(status)
      type(modgrid_struct),intent(inout)  :: obj
      integer,intent(in)                  :: nelements

      status = modgmem_alloc(obj%memdata,nelements)
     !if(associated(obj%memdata)) then
     !  if(associated(obj%memdata%data)) then
     !    if(nelements < modgmem_size(obj%memdata)) then
     !      ierr=0
     !    else
     !      if(obj%memdata%owns_data) deallocate(obj%memdata%data)
     !      nullify(obj%memdata%data)
     !      allocate(obj%memdata%data(nelements),stat=ierr)
     !      if(ierr/=0) then
     !       print *,'modgrid_alloc: error- nelements=',nelements
     !      endif

     !    endif
     !  else
     !    allocate(obj%memdata%data(nelements),stat=ierr)
     !    if(ierr/=0) then
     !      print *,'modgrid_alloc: error- nelements=',nelements
     !    endif
     !  endif
     !else
     !  print *,'modgrid_alloc: memdata not allocated?'
     !  return
     !endif
     !if(ierr/=0) then
     !  write(obj%stdo,*) 'modgrid_alloc: failed to allocate data'
     !  return
     !endif
     !obj%memdata%data=0
     !obj%memdata%is_inverted=.false.
     !modgrid_alloc = nelements
      return
      end function modgrid_alloc

      integer function modgrid_dealloc(obj) result(status)
      type(modgrid_struct),intent(inout)  :: obj
      integer                :: ierr
      status = -1
      ierr = modgmem_delete(obj%memdata)
     !if(associated(obj%memdata)) then
     !  if(associated(obj%memdata%data)) then
     !    deallocate(obj%memdata%data, stat=ierr)
     !    if(ierr/=0) then
     !      write(obj%stdo,*) 'modgrid_dealloc: error 1'
     !      return
     !    endif
     !    nullify(obj%memdata%data)
     !  endif
     !  deallocate(obj%memdata, stat=ierr)
     !endif
      if(ierr/=0) then
        write(obj%stdo,*) 'modgrid_dealloc: error 2'
        return
      endif
      status = 0
      return
      end function modgrid_dealloc

      function modgrid_ftype_read(fname,stdo,fname_sz) result(ftype)
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      double precision,optional,intent(inout)  :: fname_sz !byte size of fname
      type(segy_bin_hdr)       :: binhdr
      type(segy_trc_hdr)       :: syh

      integer :: hit1,hit2,hit3,hit4, wt
      integer :: ntord,ndidrd
      integer :: lun
      integer :: status
      integer :: bsiz,wblk,wbyt
      integer :: i,ites
      character(len=8) :: ftype
      character(len=4) :: mode
      character(len=3600) :: buff,obuff
      double precision :: rsize
      double precision :: csize  !computed size
      integer    :: is_segy
      integer    :: bsize
      integer    :: h(100)
      integer    :: wrdsz
      integer    :: bytes_per_trace
      integer    :: ntrfil
      integer    :: endian
      logical    :: swap
      ftype = 'UNKNOWN'
      bsize = -1
      if(present(fname_sz)) fname_sz = bsize
    ! write(stdo,*) ' fname=',trim(fname)
      mode = 'r'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'modgrid_ftype: error - open failed???'
        write(stdo,*)' fname = ',fname
        return
      endif
      ntord  = 3600
      ndidrd = cio_fread      (buff, 1, ntord,  lun)
      if(ndidrd <= 0) then
        write(stdo,*) 'modgrid_ftype: read error ndidrd=',ndidrd
        status= cio_fclose(lun)
        return
      endif
      i     = cio_fseek(lun, 0, 2)    !go to end of file
      bsiz  = 1024
      status= cio_ftell(lun,bsiz,wblk,wbyt)     !get byte position
      rsize = bsiz
      rsize = rsize*wblk
      rsize = rsize + wbyt

      status= cio_fclose(lun)
      if(present(fname_sz)) then
        fname_sz = rsize
      endif

      is_segy=0
      if(ndidrd >= 3200) then
        if(buff(1:1)=='c' .or. buff(1:1)== 'C') then
          is_segy=1
          if(buff(1:1)  /='c' .and. buff(1:1)  /='C') is_segy=0
          if(buff(81:81) /='c' .and. buff(81:81) /='C') is_segy=0
          if(buff(161:161)/='c' .and. buff(161:161)/='C') is_segy=0
          if(is_segy==1) ftype='SEGY'
        else
          is_segy=1
          call wrdc_ebc_asc(buff(1:3200),obuff)
          if(obuff(1:1)  /='c' .and. obuff(1:1)  /='C') is_segy=0
          if(obuff(81:81) /='c' .and. obuff(81:81) /='C') is_segy=0
          if(obuff(161:161)/='c' .and. obuff(161:161)/='C') is_segy=0
          if(is_segy==1) ftype='SEGY'
        endif
        if(is_segy==1) then
          return
        else
          h(1:100) = transfer(buff(3201:3600),h(1:100))
          status = segy_buf2binh(h(1:100),binhdr)

          wrdsz = 4
          if(binhdr%format==3) wrdsz=2
          if(binhdr%format==5) wrdsz=1
          if(binhdr%hns > 0) then
            bytes_per_trace = 240 + wrdsz*binhdr%hns
            ntrfil = (rsize-3600)/bytes_per_trace
            csize = ntrfil
            csize = csize*bytes_per_trace
            csize = csize + 3600
           !print *,'modgrid_ftype:A csize=',csize,' rsize=',rsize,ntrfil
           !print *,'modgrid_ftype:A binhdr%hdt=',binhdr%hdt,binhdr%dto
            if(csize==rsize) is_segy=1
            if(is_segy==1) then
              ftype='SEGY'
              return
            endif
          endif
          swap = .false.
          endian = swap_endian()
          if(endian ==0) swap= .true. !file big endian, but mem is little
          h(1:60) = transfer(buff(1:240),h(1:60))
          call segy_unpack_segyhd(syh,h(1:60),swap)
          bytes_per_trace = 240 + 4*syh%ns
          ntrfil = (rsize)/bytes_per_trace
          csize = ntrfil
          csize = csize*bytes_per_trace
         !print *,'modgrid_ftype:B csize=',csize,' rsize=',rsize,ntrfil
         !print *,'modgrid_ftype:B syh%ns=',syh%ns
          if(csize==rsize) then
            ftype='SU'
            return
          endif
        endif
      endif

      do i = 1,ndidrd
        ites = ichar(buff(i:i))
        if(ites<10 .or. ites > 127) then
          ndidrd = i-1
          exit
        endif
      enddo

      call string_to_upper(buff(1:ndidrd))
      hit1=index(buff(1:240),'#<CPS_V1')
      if(hit1>0 ) then
        hit1=index(buff(1:240),'TYPE="TRCIO"')
        if(hit1>0 ) then
          ftype = 'TRCIO'
        endif
        hit1=index(buff(1:240),'TYPE=VEL')
        if(hit1>0 ) then
          ftype = 'CPSVEL'
        endif
        hit1=index(buff(1:240),'TYPE=MODSPEC_GRID')
        if(hit1>0 ) then
          ftype = 'MSGRID'
        endif
        return
      endif

      hit1=index(buff(1:240),'GOCAD')
      if(hit1>0 .and. hit1 < 80) then
        ftype = 'GOCAD'
        hit2=index(buff(hit1:240),'VOXET ')
        if( hit2 /= 0  .and. hit2 - hit1 < 60) ftype = 'VOXET'
        hit2=index(buff(hit1:240),'GSURF ')
        if( hit2 /= 0  .and. hit2 - hit1 < 60) ftype = 'GSURF'
        hit2=index(buff(hit1:240),'SGRID ')
        if( hit2 /= 0  .and. hit2 - hit1 < 60) ftype = 'SGRID'
        return
      endif

      hit1=index(buff(1:240),' SITI ')
      if(hit1>0 .or. index(fname,'HDR') == len_trim(fname)-2  ) then
        ftype = 'SITI'
        return
      endif

      ! see svfio.c in JavaSeis/util, www.javaseis.org
      hit1=index(buff(1:ndidrd),'SEISVERSION = ')
      if(hit1>0  ) then
        ftype = 'SEISSP'
        return
      endif

      hit1=index(buff(1:3600),'NLAYDEF=')
      if(hit1>0 ) then
        ftype = 'MODSPEC'
        hit1=index(buff(1:240),'MODSPEC_VERSION')
        if(hit1>0 ) ftype = 'MODSPEC2'
        return
      endif

      if(index(buff(1:ndidrd),'*HEADER') >0 .or.&
         index(buff(1:ndidrd),'XCOORDINATE')>0 ) then
        ftype = 'RMOD'
        hit2=index(buff(1:ndidrd),'TYPE')
        if(hit2 > 0) then
          hit3=index(buff(hit2:ndidrd),'=')
          if( hit3/= 0 .and. hit3-hit2 < 20) then
            hit4=index(buff(hit2+hit3-1:),'GRID')
            if( hit4 /= 0 .and. hit4-hit3 < 8 ) then
              ftype='HGRID'
              wt = index(buff(1:),'WORDTYPE')
              if( wt/= 0) then
                 hit1 = index(buff(wt:),'AGRID')
                  if(hit1>0 .and. (hit1>wt)) ftype = 'AGRID'
              endif
            endif
            hit4=index(buff(hit2+hit3-1:),'LAYER')
            if(hit4>0 .and. hit4-hit3 < 8)  ftype='LAYER'
            hit4=index(buff(hit2+hit3-1:),'G3DL')
            if(hit4>0 .and. hit4-hit3 < 8)  ftype='G3DL'
          endif
        endif
        return
      endif
      if(index(buff(1:80),'*GLOBALS: ') >0 ) then
        ftype = 'CPSVEL'
        return
      endif
      if(index(buff(1:80),'*GLOBAL ') >0 ) then
        ftype = 'TFILE'
        return
      endif

      return
      end function modgrid_ftype_read

      !! DSKDATA BEG
      ! defined after rddesc has been called
      function modgrid_ftype_check(obj) result(ftype)
      type(modgrid_struct),intent(in) :: obj
      character(len=8) :: ftype
      ftype='UNKNOWN'
      if(.not.associated(obj%dskdata) ) return
      ftype = obj%dskdata%ftype
      return
      end function modgrid_ftype_check

      function modgrid_datafile(obj) result(datafile)
      type(modgrid_struct),intent(in) :: obj
      character(len=128) ::  datafile
      datafile=' '
      if(.not.associated(obj%dskdata) ) return
      datafile = obj%dskdata%datafile
      return
      end function modgrid_datafile

      subroutine modgrid_set_datafile(obj,datafile)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in) ::  datafile
      if(associated(obj%dskdata) ) obj%dskdata%datafile = datafile 
      return
      end subroutine modgrid_set_datafile

      function modgrid_headfile(obj) result(headfile)
      type(modgrid_struct),intent(in) :: obj
      character(len=128) ::  headfile
      headfile=' '
      if(.not.associated(obj%dskdata) ) return
      headfile = obj%dskdata%headfile
      return
      end function modgrid_headfile

      subroutine modgrid_set_headfile(obj,headfile)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in) ::  headfile
      if(associated(obj%dskdata) ) obj%dskdata%headfile = headfile 
      return
      end subroutine modgrid_set_headfile

      function modgrid_pointfile(obj) result(pointfile)
      type(modgrid_struct),intent(in) :: obj
      character(len=128) ::  pointfile
      pointfile=' '
      if(.not.associated(obj%dskdata) ) return
      pointfile = obj%dskdata%pointfile
      return
      end function modgrid_pointfile

      function modgrid_palign(obj) result(palign)
      type(modgrid_struct),intent(in) :: obj
      character(len=8) ::  palign
      palign=' '
      palign = obj%palign
      return
      end function modgrid_palign

      subroutine modgrid_set_pointfile(obj,pointfile)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in) ::  pointfile
      if(associated(obj%dskdata) ) obj%dskdata%pointfile = pointfile 
      return
      end subroutine modgrid_set_pointfile

      subroutine modgrid_set_endian(obj,endian)
      type(modgrid_struct),intent(inout) :: obj
      integer,intent(in) ::  endian
      if(associated(obj%dskdata) ) obj%dskdata%endian = endian 
      return
      end subroutine modgrid_set_endian

      integer function modgrid_endian(obj) result(endian)
      type(modgrid_struct),intent(inout) :: obj
      endian = swap_endian()
      if(associated(obj%dskdata) ) endian = obj%dskdata%endian
      return
      end function modgrid_endian

      subroutine modgrid_set_datapos(obj,datapos)
      type(modgrid_struct),intent(inout) :: obj
      integer,intent(in) ::  datapos
      if(associated(obj%dskdata) ) obj%dskdata%datapos = datapos 
      return
      end subroutine modgrid_set_datapos

      integer function modgrid_datapos(obj) result(datapos)
      type(modgrid_struct),intent(inout) :: obj
      datapos = 0
      if(associated(obj%dskdata) ) datapos = obj%dskdata%datapos
      return
      end function modgrid_datapos

      real function modgrid_dskdata_scale(obj) result(datascale)
      type(modgrid_struct),intent(inout) :: obj
      datascale = 0
      if(associated(obj%dskdata) ) datascale= obj%dskdata%datascale 
      return
      end function modgrid_dskdata_scale

      subroutine modgrid_set_dskdata_scale(obj,scale)
      type(modgrid_struct),intent(inout) :: obj
      real,intent(in) ::  scale
      if(associated(obj%dskdata) ) obj%dskdata%datascale = scale 
      return
      end subroutine modgrid_set_dskdata_scale

      subroutine modgrid_set_dskdata_add(obj,add)
      type(modgrid_struct),intent(inout) :: obj
      real,intent(in) ::  add
      if(associated(obj%dskdata) ) obj%dskdata%dataadd = add 
      return
      end subroutine modgrid_set_dskdata_add

      real function modgrid_dskdata_add(obj) result(add)
      type(modgrid_struct),intent(inout) :: obj
      add = 0
      if(associated(obj%dskdata) ) add= obj%dskdata%dataadd 
      return
      end function modgrid_dskdata_add

      subroutine modgrid_memdata(obj,memdata)
      type(modgrid_struct),intent(in) :: obj
      type(prop_memstore_struct),pointer  :: memdata
      memdata =>obj%memdata
      return
      end subroutine modgrid_memdata

      subroutine modgrid_dskdata(obj,ftype,fsize,dfile,wtype,wrdsz,wname)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(out)    :: ftype
      double precision,intent(out)    :: fsize
      character(len=*),intent(out)    :: dfile
      character(len=*),intent(out)    :: wtype
      integer,optional,intent(out)             :: wrdsz
      character(len=*),optional,intent(out)    :: wname
      ftype='UNKNOWN'
      dfile=' '
      wtype=' '
      fsize=0
      if(present(wrdsz) ) wrdsz= 4
      if(.not.associated(obj%dskdata) ) return
      ftype = obj%dskdata%ftype
      fsize = obj%dskdata%fsize
      dfile = obj%dskdata%datafile
      wtype = obj%dskdata%datafrmt
      if(present(wrdsz) ) wrdsz= obj%dskdata%databits/8
      if(present(wname) ) wname= obj%dskdata%dataname
      return
      end subroutine modgrid_dskdata

      subroutine modgrid_set_dskdata(obj,ftype,fsize,dfile,wtype,wrdsz,wname,&
      datapos)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)    :: ftype
      double precision,intent(in)    :: fsize
      character(len=*),intent(in)    :: dfile
      character(len=*),intent(in)    :: wtype
      integer,optional,intent(in)    :: wrdsz
      character(len=*),optional,intent(in) :: wname
      integer,optional,intent(in)    :: datapos
      integer :: i_err
      if(.not.associated(obj%dskdata) ) then
        allocate(obj%dskdata, stat=i_err)
        if(i_err/=0) then
          write(obj%stdo,*) 'modgrid_set_dskdata: failed to allocate data'
          return
        endif
        obj%dskdata%headfile= ' '
        obj%dskdata%pointfile= ' '
        obj%dskdata%datapos = 0          !true for gocad & hgrid, siti
        obj%dskdata%dataadd = 0
        obj%dskdata%datascale = 0
        obj%dskdata%endian  = swap_endian()
        obj%dskdata%dataname= 'REAL'
        obj%dskdata%datafrmt= 'IEEE'     !IEEE,SEGY,ASCII
        obj%dskdata%databits= 32         !default to 4byte words
      endif
      obj%dskdata%ftype    = ftype
      obj%dskdata%fsize    = fsize
      obj%dskdata%datafile = dfile
      obj%dskdata%datafrmt = wtype      !IEEE,SEGY,ASCII
      if(present(wrdsz)) obj%dskdata%databits = 8*wrdsz

      obj%dskdata%dataname = 'REAL'    !default 
      if(present(wname)) obj%dskdata%dataname = wname
      if(present(datapos)) obj%dskdata%datapos = datapos
      if(ftype=='SEGY') then
        obj%dskdata%endian = 1     !big endian for segy
        obj%dskdata%datapos  = 3600
      endif
      if(ftype=='SU') then
        obj%dskdata%endian = 0     !little endian for su
        obj%dskdata%datapos  = 0
      endif
      if(ftype=='VOXET' .or. ftype=='HGRID' .or. ftype=='GSURF') then
        obj%dskdata%endian = 1       !big endian for gocad, hgrid
        obj%dskdata%datapos= 0
      endif
      if(ftype=='SITI') then
        obj%dskdata%endian = 0    !little endian for siti
        obj%dskdata%datapos= 0
      endif
      if(ftype=='MODSPEC') then
        obj%dskdata%endian = swap_endian()
      endif
      return
      end subroutine modgrid_set_dskdata
      !! DSKDATA END

      integer function modgrid_rddesc_verbose(obj,fname,stdo,&
       dfile, ftype, rank, &
       lab1, hd1, n1,o1,d1,&
       lab2, hd2, n2,o2,d2,&
       lab3, hd3, n3,o3,d3,&
       xyz_order, scan_xhdr,scan_yhdr,vel_type) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: ftype

      integer,intent(inout)           :: rank
      character(len=*),intent(inout)  :: lab1,lab2,lab3
      integer,intent(inout)           :: hd1,hd2,hd3
      integer,intent(inout)           :: n1,n2,n3
      real,intent(inout)              :: o1,o2,o3
      real,intent(inout)              :: d1,d2,d3
      character(len=*),intent(inout)  :: xyz_order
      integer,intent(in)              :: scan_xhdr !scanning header
      integer,intent(in)              :: scan_yhdr !scanning header
      character(len=*),intent(inout)  :: vel_type
         !CPSVEL looks at input value
         !other file type return a value

      integer          :: l_err
      integer          :: xlab,ylab,zlab
      character(len=32):: name
      character(len=32):: punits
      character(len=64):: pname
      character(len=8) :: wtype
      nullify(obj)
      status = -1
      rank = 0
      xyz_order = ' '
      n1 = 1
      n2 = 1
      n3 = 1
      d1 = 1.0
      d2 = 1.0
      d3 = 1.0
      status = modgrid_rddesc_base(obj,fname,stdo,dfile,wtype,&
      ftype,scan_xhdr,scan_yhdr,vel_type)
      if(status == 0 ) then
        call modgrid_get_name_rank(obj,name,pname,punits,rank)
        lab1 = obj%label(1)
        call modgrid_get_griddesc(obj,1,hd1,n1,o1,d1)
        if(rank > 1) then
            call modgrid_get_griddesc(obj,2,hd2,n2,o2,d2)
            lab2 = obj%label(2)
        endif
        if(rank > 2) then
            call modgrid_get_griddesc(obj,3,hd3,n3,o3,d3)
            lab3 = obj%label(3)
        endif
        l_err = modgrid_xyz_order(obj%label,xlab,ylab,zlab)
        if(l_err==0) then
            xyz_order(xlab:xlab)='X'
            xyz_order(ylab:ylab)='Y'
            xyz_order(zlab:zlab)='Z'
        else
          write(stdo,*) 'modgrid_rddesc_verbose: error, bad order'
          status = -1
        endif
      else
        print *,'modgrid_rddesc_verbose: error'
        !write(stdo,*) 'modgrid_rddesc_verbose: error'
      endif
      return
      end function modgrid_rddesc_verbose


      integer function modgrid_rddesc(obj,fname,stdo,dfile,wtype,&
       ftype,xhdr,yhdr) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      character(len=*),intent(inout)  :: ftype
      integer,optional,intent(in)     :: xhdr
      integer,optional,intent(in)     :: yhdr
      integer             :: lxhdr,lyhdr
      character(len=8)    :: vel_type

      lxhdr = HDR_MIDPOINT_XGRID
      lyhdr = HDR_MIDPOINT_YGRID
      if(present(xhdr)) then
        lxhdr = xhdr
      endif
      if(present(yhdr)) then
        lyhdr = yhdr
      endif
      vel_type = ' '         !no conversion
      status =  modgrid_rddesc_base(obj,fname,stdo,dfile,wtype,&
       ftype,lxhdr,lyhdr,vel_type)
      return
      end function modgrid_rddesc

! Read file, create obj but don't store velocity data here!
      integer function modgrid_rddesc_base(obj,fname,stdo,dfile,wtype,&
       ftype,xhdr,yhdr,vel_type) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      character(len=*),intent(inout)  :: ftype
      integer,intent(in)              :: xhdr      !for trace scans
      integer,intent(in)              :: yhdr      !for trace scans
      character(len=*),intent(inout)  :: vel_type  !for CPSVEL files
      type(modgrids_struct),pointer   :: objs(:)
      integer            :: lxhdr,lyhdr
      integer            :: pcnt
      character(len=32)  :: pnames(16)
      type(region_struct),pointer     :: rgndata

      nullify (objs)
      nullify(obj)
      nullify(rgndata)
      lxhdr = xhdr
      lyhdr = yhdr
      status =  modgrid_rddesc_modgrids(objs,fname,stdo,dfile,wtype,&
       ftype,lxhdr,lyhdr,vel_type,pcnt,pnames,rgndata)
      if(status==0) then
        obj => objs(1)%mobj
      endif
      return
      end function modgrid_rddesc_base

!! MODGRIDS_END
      !allocate a pointer to an array of modgrids structures
      integer function modgrid_modgrids_create(objs, cnt) result(status)
      type(modgrids_struct),pointer   :: objs(:)
      integer,intent(in)              :: cnt   !count
      integer :: k,i_err
      !create array of modgrids objects to hold the voxet components
      nullify (objs)
      status = -1
      if(cnt<=0) return    
      allocate(objs(cnt),stat=i_err)
      if(i_err /=0) then
         return
      endif
      do k=1,cnt
        nullify(objs(k)%mobj)
      enddo
      status = 0
      return
      end function modgrid_modgrids_create

      !deallocate an array of modgrids structures
      integer function modgrid_modgrids_delete(objs) result(status)
      type(modgrids_struct),pointer   :: objs(:)
      integer :: k
      integer :: i_err
      status = -1
      if(.not.associated(objs)) return
      do k = 1,size(objs)
        call modgrid_delete(objs(k)%mobj)
      enddo
      deallocate(objs,stat=i_err)
      if(i_err /=0) return 
      status = 0
      return
      end function modgrid_modgrids_delete

      function modgrid_modgrids_get(objs,nth) result(obj)
      type(modgrids_struct),pointer   :: objs(:)
      integer,intent(in) :: nth
      type(modgrid_struct),pointer   :: obj
      nullify(obj)
      if(.not. associated(objs)) return
      if(nth <= size(objs) .and. nth>= 1) obj=> objs(nth)%mobj
      return
      end function modgrid_modgrids_get

      integer function modgrid_rddesc_modgrids(objs,fname,stdo,dfile,wtype,&
       ftype,xhdr,yhdr,vel_type,pcnt,pnames,rgndata) result(status)
      type(modgrids_struct),pointer   :: objs(:)
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      character(len=*),intent(inout)  :: ftype
      integer,intent(in)              :: xhdr      !for trace scans
      integer,intent(in)              :: yhdr      !for trace scans
      character(len=*),intent(inout)  :: vel_type  !for CPSVEL files
      integer,intent(inout)           :: pcnt
      character(len=*),intent(inout)  :: pnames(:)
      type(region_struct),pointer     :: rgndata

      type(modgrid_struct),pointer    :: obj
      character(len=FILENAME_LENGTH) :: tempname
      integer :: k
      integer :: wrdsz
      integer :: lxhdr,lyhdr
      integer :: offset,endian
      double precision :: dfsize
      character(len=8) :: wname
      wrdsz  = 4
      status = -1
      nullify(obj)
      nullify(objs)
      nullify(rgndata)
      dfile = fname
      wname = 'REAL'
      wtype = 'IEEE'
      offset= 0
      endian= 0
      pcnt  = 0
      pnames= ' '
! determine file type, and the size of fname
      ftype = modgrid_ftype_read(fname,stdo,dfsize)
      lxhdr = xhdr
      lyhdr = yhdr
      if(ftype=='UNKNOWN') then
        return
      endif
      if(ftype == 'HGRID' .or. ftype=='LAYER' .or. ftype=='G3DL') then
        status = modgrid_rddesc_rmod(obj,fname,stdo,dfile,wtype)
!          Set the full path name in dfile if needed
        if(dfile(1:1).ne.'/'.and.dfile(1:1).ne.'~')then
          k=index(fname,'/',.true.)
          if(k.ne.0)then
            tempname=fname(1:k) // trim(dfile)
            dfile=' '
            dfile=trim(tempname)
          endif
        endif
        status = modgrid_modgrids_create(objs, 1)
        if(status==0) objs(1)%mobj =>obj
      endif
      if(ftype == 'VOXET' .or. ftype=='GSURF' .or. ftype=='SGRID') then
        !will read and store all property descriptions
        !dfile, wtype,... are for the first property
        status = modgrid_rddesc_gocad(objs,fname,stdo,dfile,wtype,wrdsz,wname,&
                 rgndata)
        if(status ==0) then
          obj => objs(1)%mobj
        else
          print *,'modgrid_rddesc_modgrids: gocad status=',status
        endif
      endif
      if(ftype == 'SITI') then
        status = modgrid_rddesc_siti(obj,fname,stdo,dfile,wtype)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif
      if(ftype == 'SEISSP') then
        status = -1 !modgrid_rddesc_siti(obj,fname,stdo,dfile,wtype)
      endif
      if(ftype == 'MODSPEC') then
        status = modgrid_rddesc_modspec(obj,fname,stdo,dfile,wtype)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif
      if(ftype == 'MSGRID') then
        status = modgrid_rddesc_msgrid(obj,fname,stdo,dfile,wtype,offset,endian)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif
      if(ftype == 'TRCIO') then
        status = modgrid_rddesc_trcio(obj,fname,stdo,dfile,wtype,wrdsz,&
        lxhdr,lyhdr,offset)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif
      if(ftype == 'SEGY') then
        status = modgrid_rddesc_segy(obj,fname,stdo,dfile,wtype,dfsize,&
        wrdsz,lxhdr,lyhdr)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif
      if(ftype == 'SU') then
        dfile = fname
        status = modgrid_rddesc_su(obj,fname,stdo,dfile,wtype,dfsize,&
        wrdsz,lxhdr,lyhdr)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      endif

      if(ftype == 'CPSVEL') then
        !vel_type= ' '    ==>   no conversion
        status = modgrid_rddesc_cvf(obj,fname,stdo,dfile,wtype,vel_type)
        if(status==0) then 
          status = modgrid_modgrids_create(objs, 1)
          if(status==0) objs(1)%mobj =>obj
        endif
      else
        if(associated(obj)) then
          if(obj%hdwd(1) == -2) vel_type = 'VTIN'
          if(obj%hdwd(1) == -3) vel_type = 'VZIN'
        endif
      endif

      if(dfile /= fname) then !get the data file size
        dfsize = modgrid_file_size(dfile)
        if(dfsize==-1) then
          if(dfile.ne.' ') then
           print *,'modgrid_rddesc_modgrids: data file open error-',trim(dfile)
          else
           print *,'modgrid_rddesc_modgrids: warning blank data file'
          endif
        endif
      endif
      if(status == 0 ) then
        if(associated(objs)) then
          do k = 1,size(objs)
            if(associated(objs(k)%mobj)) then
              pnames(k) = objs(k)%mobj%pname
              if(pnames(k) .ne.' ') pcnt = pcnt + 1
            else
              print *,'modgrid_rddesc_modgrids: null property? k=',k
              status = -1
            endif
          enddo
        endif
        !Already set if a VOXET, but no harm in resetting
        call modgrid_set_dskdata(obj,ftype,dfsize,dfile,wtype,wrdsz,&
        wname,offset)
        call modgrid_set_headfile(obj,fname)
        if(ftype=='MSGRID') then
          obj%dskdata%datapos  = offset+20
          obj%dskdata%endian  = endian
        endif
      endif
      return
      end function modgrid_rddesc_modgrids

      integer function modgrid_modgrids_to_voxhdr(objs, ascrep, nc,rgndata) &
        result(status)
      type(modgrids_struct),pointer   :: objs(:)
      character(len=*),intent(inout)  :: ascrep
      integer,intent(inout)       :: nc  !last non-blank character in ascrep
      type(region_struct),pointer     :: rgndata
      type(modgrid_struct),pointer    :: obj
      character(len=120)  :: dfile
      character(len=16)   :: ftype
      double precision    :: fsize
      character(len=8)    :: wtype
      integer             :: wrdsz
      character(len=8)    :: wname
      integer             :: pcnt,i,cnt,i_err
      character(len=3000) :: tstr
      status=0
      ascrep=' '
      nc = 0
      pcnt = size(objs)
      if(pcnt <=0) then
        status= -1
        return
      endif

      do i=1,pcnt
      
        obj => objs(i)%mobj
        if(i==1) then   !grid parameters shared by all properties
          cnt =  modgrid_voxet_body(obj, ascrep)
          nc = len_trim(ascrep)
          i_err = modgrid_rgndata_str(rgndata,ascrep,nc)
        endif
        ftype = 'UNKNOWN'
        dfile = ' '
        wtype = ' '
        wname = ' '
        fsize = 0
        wrdsz = 4
        call modgrid_dskdata(obj,ftype,fsize,dfile,wtype,wrdsz,wname)

        tstr = ' '
        cnt = cnt +  modgrid_voxet_property(obj,tstr,i,dfile)
        ascrep(nc+1:) = trim(tstr)
        nc = nc + len_trim(tstr)

      enddo
      tstr ='END'//char(10)
      ascrep(nc+1:) = trim(tstr)
      nc = nc + len_trim(tstr)
      if(nc .ne. len_trim(ascrep)) then
        print *,'modgrid_modgrids_to_voxhdr: error, the computed size=',nc
        print *,'modgrid_modgrids_to_voxhdr: error, the actual size=',&
        len_trim(ascrep)
        status = -1
      endif

      return
      end function modgrid_modgrids_to_voxhdr

!! MODGRIDS_END
      double precision function modgrid_file_size(dfile) result(dfsize)
      character(len=*),intent(in) :: dfile
      integer :: i_err
      integer :: lun
      integer :: bsiz,wblk,wbyt

      dfsize = -1.0
      if(dfile==' ') return
      lun = cio_fopen(dfile,'r')
      if(lun >0) then
          i_err  = cio_fseek(lun, 0, 2)   !go to end of file

          bsiz   = 1024
          i_err  = cio_ftell(lun,bsiz,wblk,wbyt)     !get byte position
          dfsize = bsiz
          dfsize = dfsize*wblk
          dfsize = dfsize + wbyt
          i_err  = cio_fclose(lun)
      else
          print *,'modgrid_file_size: open error ',trim(dfile)
          print *,'modgrid_file_size: error lun=',lun
          dfsize = -1.0
          return
      endif
      return
      end function modgrid_file_size
! Get information from a Conoco Velocity File
      integer function modgrid_rddesc_cvf&
      (obj,fname,stdo,dfile,wtype,vtypeo) result(status)

      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      character(len=*),intent(inout)  :: vtypeo
      ! vtypeo =  ' ' on input ==> no conversion
      ! vtypeo /= ' ' on input ==> do conversion

      type(velio_struct),pointer      :: velio_obj
      type(prop_cvfstore_struct),pointer :: cvf_obj
      integer      :: rank
      character(len=64) :: name
      character(len=64) :: pname
      character(len=16) :: punits
      character(len=96) :: msg
      character(len=8)  :: ftype

      integer      :: hdwd(4),n(4)
      real         :: o(4),d(4)
      real         :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character    :: coord(3)*32
      integer      :: i_err

      character     :: crd80*80
      integer       :: nfields     ! number of velocity function fields
      character (len = 80) :: dunits     ! units of velocity functions
      character (len = 80) :: attname    ! any attribute name given to the
                                         !  ordinate (e.g. ve
      character (len = 80) :: attunits   ! attribute units of the ordinate
                                         !(e.g. feet/sec).
      character (len = 80) :: tdunits    ! time/depth units of the abscissa
                                         !  (e.g. sec, feet,
      character (len = 80) :: encoding   ! encoding format of velocity file.
      character (len = 80) :: nilstring  ! string for nil values in the file
                                         !  (default '-nil-
      character (len = 80) :: fields(8)  ! list of velocity function fields
                                         !  to read or write
      character (len =  8) :: inp_name*8 ! input velocity function name
      integer       :: nvfun
      character     :: new_type*8
      character     :: old_type*8
      character     :: vtype*8
      real, pointer :: t_bins(:)
      real, pointer :: v_bins(:)
      real, pointer :: t_tmp(:)
      real, pointer :: v_tmp(:)
      real          :: x_tmp,y_tmp
      integer       :: j,nt_bins
      integer       :: ntoread !number of functions to scan and check
      real          :: tmin,tmax,tavg
      logical       :: bad_error

      bad_error = .false.
      status = -1
      msg = ' '
      dfile = fname
      wtype = 'ASCII'
      nullify(obj)
      nullify(velio_obj)
      nullify(cvf_obj)
      nullify (t_bins)
      nullify (v_bins)
      nullify (t_tmp)
      nullify (v_tmp)
      n = 1
      o = 0.0
      d = 1.0
! determine file type
      ftype = modgrid_ftype(fname,stdo)
      if(ftype(1:6) /= 'CPSVEL') then
        write(stdo,*) 'modgrid_rddesc: '//trim(fname)//' not a cpsvel file'
        write(stdo,*) 'modgrid_rddesc: '//trim(ftype)
        return
      endif
      allocate(cvf_obj,stat=i_err)
      nullify (cvf_obj%x_coords)
      nullify (cvf_obj%y_coords)
      nullify (cvf_obj%x_bins)
      nullify (cvf_obj%y_bins)
      nullify (cvf_obj%vel)
      cvf_obj%maxpicks=0
      !
      call velio_scan_alloc (filename  = fname,    &
        nfun      = cvf_obj%nvfun,      &
        err       = i_err,       &
        msg       = crd80,       &
        nhx       = cvf_obj%hx,  &
        nhy       = cvf_obj%hy,  &
        nmosign   = cvf_obj%nmosign,    &
        nmoexp    = cvf_obj%nmoexp,     &
        maxpicks  = cvf_obj%maxpicks,   &
        xcoords   = cvf_obj%x_coords,   &
        ycoords   = cvf_obj%y_coords,   &
        xbins     = cvf_obj%x_bins,     &
        ybins     = cvf_obj%y_bins,     &
        nxbins    = cvf_obj%nx_bins,    &
        nybins    = cvf_obj%ny_bins,    &
        dunits    = dunits,      &
        attname   = attname,     &
        attunits  = attunits,    &
        tdunits   = tdunits,     &
        encoding  = encoding,    &
        fields    = fields,      &
        nfields   = nfields,     &
        nilstring = nilstring)
      if ( i_err /=  0 ) then
        write(stdo,*) 'modgrid_rddesc_cvf: scan_alloc err=',i_err
        write(stdo,*) 'modgrid_rddesc_cvf: scan_alloc msg=',trim(crd80)
        call modgrid_cvf_delete(cvf_obj)
        return
      endif
      nvfun = cvf_obj%nvfun
      if (nvfun /= cvf_obj%nx_bins*cvf_obj%ny_bins .or. &
          nvfun < 1) then
        msg = 'modgrid_rddesc_cvf: nvfun /= nx_bins*ny_bins'
        write(stdo,*) 'modgrid_rddesc_cvf: irregular grid! nvfun=',&
        nvfun
        write(stdo,*) 'modgrid_rddesc_cvf: irregular grid! nx_bins=',&
        cvf_obj%nx_bins,' ny_bins=',cvf_obj%ny_bins
        call modgrid_cvf_delete(cvf_obj)
        goto 99
      end if
      !
      msg = 'modgrid_rddesc_cvf: error estimating grid array size'
      call modgrid_array_size (n(2),o(2),d(2),&
       cvf_obj%nx_bins,cvf_obj%x_bins,i_err,obj%stdo)
      if(i_err /=0) goto 99
      call modgrid_array_size (n(3),o(3),d(3),&
       cvf_obj%ny_bins,cvf_obj%y_bins,i_err,obj%stdo)
      if(i_err /=0) goto 99

      !
      ! - open the velocity file
      !
      call velio_open_read (obj      = velio_obj,&
        filename = fname,    &
        nfun     = nvfun,    &
        err      = i_err,    &
        msg      = msg,      &
        nhx      = hdwd(2),  &
        nhy      = hdwd(3))
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rddesc_cvf: open_read err=',i_err
        write(stdo,*) 'modgrid_rddesc_cvf: open_read msg=',trim(msg)
        return
      endif
      !
      msg = 'modgrid_rddesc_cvf: allocation error'

      allocate(t_bins(cvf_obj%maxpicks+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(v_bins(cvf_obj%maxpicks+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(t_tmp(cvf_obj%maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(v_tmp(cvf_obj%maxpicks), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      !   read the functions into t_inp, v_inp
      tavg = 0.0
      old_type = ' '
      vtype = vtypeo
      ! Read through the input functions.
      !check that they are all the same input type
      !convert each function to the output type if vtypeo /= ' '
      !find the global tmin and tmax, and average last time
      ntoread = nvfun
      ! do not read in all of huge files unless necessary
      if(vtypeo==' ') ntoread = min(1000,nvfun)
      do j = 1,ntoread
        call velio_read_velfun (obj     = velio_obj,    &
           xcoord  = x_tmp,        &
           ycoord  = y_tmp,        &
           npicks  = nt_bins,      &
           tpicks  = t_bins,       &
           vpicks  = v_bins,       &
           err     = i_err,        &
           msg     = crd80,        &
           velname = inp_name,     &
           veltype = new_type)
        if(i_err /= 0) then
          write(stdo,*) 'modgrid_rddesc_cvf: read_velfun err=',i_err
          write(stdo,*) 'modgrid_rddesc_cvf: read_velfun msg=',trim(crd80)
          bad_error = .true.
          exit
        else
          if(old_type==' ') old_type = new_type
          if(old_type /= new_type) then
            write(stdo,*) 'modgrid_rddesc_cvf: error - old_type=',old_type
            write(stdo,*) 'modgrid_rddesc_cvf: error - new_type=',new_type
            bad_error = .true.
            exit
          endif
          if(vtype==' ') then
            vtypeo=new_type
            vtype=new_type
          endif
          ! - convert from the input type to the output type
          if(new_type /= vtype )  then
            call velutil_convert (veltype    = new_type,    &
              npicks     = nt_bins,   &
              X          = t_bins,    &
              V          = v_bins,    &
              veltypeout = vtype,     &
              xout       = t_tmp,     &
              vout       = v_tmp,     &
              ierr       = i_err)
            if ( i_err /= 0 ) then
              msg='modgrid_rddesc_cvf: velutil_convert error '
              goto 99
            end if
          else
            t_tmp(1:nt_bins) = t_bins(1:nt_bins)
            v_tmp(1:nt_bins) = v_bins(1:nt_bins)
          endif
          if(j==1) then
            tmin = t_tmp(1)
            tmax = t_tmp(nt_bins)
          endif
          tmin = min(t_tmp(1),tmin)
          tmax = max(t_tmp(nt_bins),tmax)
          tavg = tavg + t_tmp(nt_bins)
        endif
      enddo
      cvf_obj%v_type = old_type
      if(bad_error) then
        msg = 'modgrid_rddesc_cvf: error scanning v-functions'
        goto 99
      endif
      !reset tmax if tmax is anomalous
      tavg = tavg/nvfun
     !if(tavg < 0.9*tmax) tmax = tavg   !eliminate 09-19-07 RSD
      n(1) = cvf_obj%maxpicks
      o(1) = tmin
      d(1) = (tmax - tmin)/max(1,(cvf_obj%maxpicks-1))
!
! set default orientation & size to align with XYZ axis
      axis1=0.0
      axis2=0.0
      axis3=0.0
      axis1(3)= tmax - tmin
      axis2(1)= (n(2)-1)*d(2)
      axis3(2)= (n(3)-1)*d(3)
      oxyz(1) = o(2)  !cvf_obj%x_bins(1)
      oxyz(2) = o(3)  !cvf_obj%y_bins(1)
      oxyz(3) = tmin
    !
      rank = 3
      name = 'CPSVEL_ESTIMATES'
      pname= vtype !cvf_obj%v_type
      punits=attunits
      call modgrid_create (obj, rank, name, pname, punits, &
        n, o, d, stdo)
      obj%hdwd(1) = -1
      if(vtype(1:2)=='VZ') hdwd(1)= -3
      if(vtype(1:2)=='VT') hdwd(1)= -2
      obj%hdwd(2) = hdwd(2)
      obj%hdwd(3) = hdwd(3)
      coord(1)= modgrid_header_to_string(hdwd(1))
      coord(2)= modgrid_header_to_string(hdwd(2))
      coord(3)= modgrid_header_to_string(hdwd(3))
      call modgrid_set_xyz(obj,coord, oxyz, axis1,axis2,axis3)
      obj%cvfdata => cvf_obj
      status = 0
 99   continue
      if (associated(velio_obj)) call velio_close (velio_obj)
      if (associated(t_bins)) deallocate (t_bins)
      if (associated(v_bins)) deallocate (v_bins)
      if (associated(t_bins)) deallocate (t_tmp)
      if (associated(v_bins)) deallocate (v_tmp)
      if(status /= 0) write(stdo,*) trim(msg)
      return
      end function modgrid_rddesc_cvf

      subroutine modgrid_array_size (nx, x0, dx, n, x, i_err,stdo)
      integer, intent (out)  :: nx  ! - Arguments
      real,    intent (out)  :: x0
      real,    intent (out)  :: dx
      integer, intent (in )  :: n
      integer, intent (out)  :: i_err
      integer, intent (in )  :: stdo
      real,    intent (in )  :: x (n)

      integer :: i      ! - Local variables

      real    :: x_min, x_max, x_eps
      !
      i_err = 0
      x_min = minval (x)
      x_max = maxval (x)
      !
      nx = 1
      x0 = x_min
      dx = 1.
      !
      if (x_min == x_max) then
        !
        x_eps = 1.e-6
        !
        if (n <= 0) nx = 0
        !
       else if (n > 1) then    ! if (x_min == x_max) then
        x_eps = (x_max - x_min) / 10000.0
        dx = x_max - x_min
        dx = sign(1., dx) * max(abs(dx), x_eps)
        do i = 2 , n
          if (abs(x(i)-x(i-1)) > x_eps) then
              dx = min (a1 = dx,    &
                        a2 = abs (x (i) - x (i - 1)))
          end if
        end do    ! do i = 2 , n
      ! nx = nint((x_max - x_min) / dx) + 1
        nx = mth_bin_number(x_min, dx, x_max)
      end if    ! if (x_min == x_max) then
      !
      ! - make sure the array is regular
      if ((mod (n, nx) /= 0)    &
        .or. (abs ((nx - 1) * dx + x0 - x_max) > x_eps)) then
        !
        write (stdo,                                        &
         "(/, 'modgrid_array_size:  grid is not regular', " &
         // " /, ' n    =', i10, ' x_min=', g14.7, ' x_max=', g14.7, "  &
         // " /, ' nx   =', i10, ' x0   =', g14.7, ' xl   =', g14.7, "  &
         // " ' dx=', g14.7)")                              &
         n, x_min, x_max, nx, x0, (nx - 1) * dx + x0, dx
        !
        i_err = -1
      end if
      return
      end subroutine modgrid_array_size

      integer function modgrid_rddesc_segy(obj,&
       fname,stdo,dfile,wtype,dfsize,wrdsz,xhdr,yhdr) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      double precision,intent(in)     :: dfsize
      integer,intent(out)             :: wrdsz
      integer,intent(in)              :: xhdr
      integer,intent(in)              :: yhdr

      integer    :: i_err
      integer    :: ntord
      integer    :: ndidrd
      integer    :: ibuff(900)
      integer    :: bytes_per_trace
      integer    :: ntraces
      integer    :: nscan
      real       :: rtraces
      character(len=4) :: mode
      character(len=8) :: ftype

      type(segy_bin_hdr)    :: binhdr

      integer    :: lun
      integer    :: hfast,hslow
      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3)
      real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character  :: label(3)*32
      character  :: name*64,pname*32,punits*16

      status = -1
      nullify(obj)
      dfile= ' '
      wtype=' '
      pname='segy_data'
      wrdsz = 4
      n = 1
      o = 0.0
      d = 1.0
      hdwd = -1
      label= ' '
      oxyz = 0.0
      axis1= 0.0
      axis2= 0.0
      axis3= 0.0
      name = 'segy_file'
      punits= 'UNKNOWN'
      if(dfsize <=0) then
         write(stdo,*) 'modgrid_rddesc_segy: error in segy dfsize=',dfsize
         write(stdo,*) 'modgrid_rddesc_segy: error fname=',fname
         return
      endif
      ftype= 'SEGY'
      dfile= fname
      wtype= 'IBM'
      mode = 'r'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_segy: open error:',trim(fname)
        return
      endif
      ! read in 3600 bytes of file header info
      ntord = 900
      ndidrd= cio_fread(ibuff,4,ntord,lun)
      if(ndidrd < ntord) then
        write(stdo,*) 'modgrid_rddesc_segy: ndidrd=',ndidrd,' < ntord=',ntord
        i_err = cio_fclose(lun)
        return
      endif
      i_err = cio_fclose(lun)
      i_err = segy_buf2binh(ibuff(801:900),binhdr)
      if(i_err <0) then
        write(stdo,*) 'modgrid_rddesc_segy: error in segy_buf2binh'
        return
      endif

      wrdsz = 4
      if(binhdr%format==3) wrdsz=2
      if(binhdr%format==5) wrdsz=1
      bytes_per_trace = 240 + wrdsz*binhdr%hns
      rtraces = (dfsize-3600)/bytes_per_trace
      ntraces = nint(rtraces)
      if(binhdr%mfeet==1) label='meters'
      if(binhdr%mfeet==2) label='feet'
      label(1) = 'DEPTH'

      ! save the segy ndpt and dt
      n(1) = binhdr%hns
      n(1) = max(1,n(1))
      o(1) = 0.0
      d(1) = binhdr%hdt*.000001
      if(d(1) < 0) d(1) = 1.0
      nscan = min(10000,ntraces)
      status =  modgrid_rddesc_segy_scan(stdo,dfile,dfsize,&
       ntraces, nscan,bytes_per_trace, 64,xhdr,yhdr,hfast,hslow,&
       n(1),d(1),n,o,d )
      if(status <0) then
        write(stdo,*) 'modgrid_rddesc_segy: scan error'
        return
      endif
      axis1(1) = 0.0
      axis1(2) = 0.0
      axis1(3) = (n(1)-1)*d(1)
      axis2(1) = (n(2)-1)*d(2)
      axis2(2) = 0.0
      axis2(3) = 0.0
      axis3(1) = 0.0
      axis3(2) = (n(3)-1)*d(3)
      axis3(3) = 0.0
      n(2)    = min(n(2),ntraces)
      oxyz(1) = o(2)
      oxyz(2) = o(3)
      oxyz(3) = o(1)

      call modgrid_create (obj, 3, name, pname, punits, &
        n, o, d, stdo)
      hdwd(2) = hfast
      hdwd(3) = hslow
      label(2) = modgrid_header_to_string(hfast)
      label(3) = modgrid_header_to_string(hslow)
      call modgrid_set_hdwd(obj,hdwd)
      call modgrid_set_xyz(obj,label, oxyz, axis1,axis2,axis3)
! save the ascii version of the 3200 byte ebcdic header
      obj%cbuff(1:3200) = transfer(ibuff(1:800),obj%cbuff(1:3200))
      if(obj%cbuff(1:1) /= 'c' .and. obj%cbuff(1:1)/='C') &
        call wrdc_ebc_asc(obj%cbuff(1:3200))
      status = 0
      return
      end function modgrid_rddesc_segy


! note: the grid may descibe a larger data set than actually
!       exists within the file contents.
      integer function modgrid_rddesc_segy_scan(stdo,dfile,dfsize,&
       ntrfil, nscan,bypertr, nhdwd,xhdr,yhdr,hfast,hslow,&
       ndpt,dt,nbin,obin,dbin ) result(status)
      integer,intent(in)    :: stdo
      character(len=*),intent(in) :: dfile
      double precision,intent(in) :: dfsize
      integer,intent(in)    :: ntrfil
      integer,intent(in)    :: nscan
      integer,intent(in)    :: bypertr
      integer,intent(in)    :: nhdwd
      integer,intent(in)    :: xhdr
      integer,intent(in)    :: yhdr
      integer,intent(inout) :: hfast
      integer,intent(inout) :: hslow
      integer,intent(in)    :: ndpt
      real   ,intent(in)    :: dt
      integer,intent(inout) :: nbin(:)
      real,intent(inout)    :: obin(:)
      real,intent(inout)    :: dbin(:)
      logical               :: swap
      integer               :: endian

      integer      :: lun
      integer      :: i_err
      integer      :: itr,i
      integer      :: nwrds
      integer      :: ndidrd

      real         :: nil
      integer      :: tnum
      integer      :: traces_per_line
      integer      :: xcnt   ! x group size
      integer      :: ycnt   ! y group size
      integer,pointer  :: ibuff(:)
      double precision :: tstrt
      integer          :: count
      integer          :: wblk,wbyt
      double precision :: diff
      double precision :: hmin(100)
      double precision :: hmax(100)
      double precision :: hold(100)
      double precision :: hbin(100)
      double precision :: cpshd(nhdwd)
      type (segy_trc_hdr)   :: syh






      endian = swap_endian()
      status= -1
      nil   = 999.2
      tstrt = 0.0
      nwrds = 1 + bypertr/4
      hfast = xhdr
      hslow = yhdr
      allocate (ibuff(nwrds), stat=i_err)
      if(i_err /=0) then
        write(stdo,*) 'modgrid_rddesc_segy_scan: allocate error:',trim(dfile)
        return
      endif

      traces_per_line = 0
      xcnt = 0
      ycnt = 0
      lun = cio_fopen(dfile,'r')
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_segy_scan: open error:',trim(dfile)
        return
      endif

      wbyt  = 3600
      i_err = cio_fseek(lun, wbyt, 0)    !go to 1st trace
      count = 0
      tnum  = 0
      swap = .false.
      if(endian ==0) swap= .true. !file big endian, but mem is little
      do itr=1,min(nscan,ntrfil)
        tnum = tnum+1
        if(tnum == nscan/2 .and. nscan < ntrfil) then
          tnum = ntrfil - nscan/2 !skip traces and read to file end
         !i_err  = cio_fseek(lun, wbyt + (tnum-1)*bypertr, 0)
          wblk = (tnum-1)
          i_err  = cio_fseek(lun,bypertr,wblk,wbyt,0)
        endif
        ndidrd = cio_fread      (ibuff, 1, bypertr,  lun)
        if(ndidrd <=0) exit
        call segy_unpack_segyhd(syh,ibuff(1:60),swap)
        call segy_segyhd_to_cpshd(cpshd,syh,tstrt)
!       nummap = 2
!       nbyte(1:2) = 4
!       wtype(1:2) = 0 ! integer
!       sbyte(1) = xhdr
!       sbyte(2) = yhdr
!       tocps(1) = 7
!       tocps(2) = 8
!       if(nummap > 0 ) then
!         do i =1,obj%nummap
!           iwdtype(i)=0
!           if(obj%wtype(i)(1:1)=='F') iwdtype(i)=1
!         enddo
!         call ttrin_map_segy_to_cps(ibuff(1:60),nummap,nbyte,&
!         sbyte, tocps, wtype, cpshd)
!       endif

        if(count==0) then
          hmin(1:nhdwd) = cpshd(1:nhdwd)
          hmax(1:nhdwd) = cpshd(1:nhdwd)
          hold(1:nhdwd) = cpshd(1:nhdwd)
          hbin(1:nhdwd) = nil
          count = 1
          traces_per_line = 1
          xcnt  = 1
          ycnt  = 1
        else
          do i=1,nhdwd
            hmin(i) = min(cpshd(i),hmin(i))
            hmax(i) = max(cpshd(i),hmax(i))
          enddo
          count = count + 1
        endif
        do i=1,nhdwd
          if(cpshd(i) /= hold(i) ) then
            diff = abs(cpshd(i) - hold(i) )
           !if( abs(diff-hbin(i))>0.1  .and. hbin(i)/= nil .and.&
           ! (i==xhdr .or. i==yhdr)) then
           !  print *,'i=',i,'cpshd(i)=',cpshd(i),' hold(i)=',hold(i)
           !  print *,'hbin(i)=',hbin(i),' diff=',diff
           !endif
            hbin(i) = min(hbin(i), diff)
            if(hbin(i) <= 0.002) hbin(i)=nil
          else
            if(i== xhdr) xcnt = xcnt + 1
            if(i== yhdr) ycnt = ycnt + 1
          endif
          if(count==2) then
            if(hold(xhdr)==cpshd(xhdr) .and. &
               hold(yhdr)/=cpshd(yhdr)) then
              hfast = yhdr
              hslow = xhdr
            endif
          endif
          hold(i) = cpshd(i)
        enddo
      enddo
      ! save the segy ndpt and dt
      nbin(1) = ndpt
      obin(1) = 0.0
      dbin(1) = dt
      if(dbin(1) < 0) dbin(1) = 1.0

      dbin(2) = hbin(hfast)
      obin(2) = hmin(hfast)
      nbin(2) = 1 + (hmax(hfast) - obin(2) + 0.01*dbin(2))/dbin(2);
      if(nbin(2)<1) nbin(2) = 1;

      nbin(3) = 1 + (ntrfil-1)/nbin(2);
      obin(3) = hmin(hslow)
      dbin(3) = hbin(hslow)

      if(nbin(3)*nbin(2) /= ntrfil) then
        nbin(2) = ntrfil
        nbin(3) = 1
      endif
      i_err = cio_fclose(lun)
      deallocate(ibuff)
      status = 0
      return
      end function modgrid_rddesc_segy_scan

      integer function modgrid_rddesc_trcio(obj,fname,stdo,dfile,wtype,wrdsz,&
       xhdr,yhdr,offset) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      integer,intent(in)              :: xhdr
      integer,intent(in)              :: yhdr
      character(len=*),intent(out)    :: dfile
      integer,intent(out)             :: wrdsz
      character(len=*),intent(out)    :: wtype
      integer,intent(out)             :: offset

      character(len=32)               :: sname
      character(len=8)                :: ftype
      character(len=64)               :: name,pname
      character(len=16)               :: punits
      character(len=32)               :: labels(3)
      character(len=4)                :: mode

      type(trcio_struct),pointer :: file
      character(len=64),pointer  :: sectionlist(:)
      integer      :: rank
      integer      :: j,nel
      integer      :: hdwd(4),n(4)
      real         :: o(4),d(4)
      real         :: oxyz(3),axis1(3),axis2(3),axis3(3)
      integer      :: i_err,nsect
      integer      :: ntraces
      integer      :: tcount

      integer      :: hslow,hfast
      integer      :: lun
      integer      :: start_pos(2)
      !double precision   :: dfsize

      status = -1
      nullify(obj)
      dfile = fname
      wtype = 'IEEE'
      punits='XXX'
      pname ='UNKNOWN'
      name  ='NONE'
      rank  = 3
! determine fie type
      ftype = modgrid_ftype(fname,stdo)
      if(ftype(1:5) /= 'TRCIO') then
        write(stdo,*) 'modgrid_rddesc: '//trim(fname)//' not a trcio file'
        write(stdo,*) 'modgrid_rddesc: '//trim(ftype)
        return
      endif
! open input file
      mode = 'r'
      file   => trcio_open(fname,mode )
      if(.not. associated(file) ) then
        write(stdo,*) 'modgrid_rddesc: failed to open ',trim(fname)
        return
      endif
      ntraces = modgrid_get_trace_count(file)
      tcount= ntraces
      wtype = trcio_get_wtype(file) !%wtype

      lun = trcio_get_lun(file) !%lun
! determine number of sections
      nsect = cpsio_number_sections(lun)
      if(nsect < 1) then
        write(stdo,*) 'modgrid_rddesc: '//trim(fname)//' no sections found'
        i_err = trcio_close(file)
        return
      endif
      allocate(sectionlist(nsect), stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_rddesc: sectionlist allocation error'
        return
      endif
      if(cpsio_get_sectionlist(lun,sectionlist) /= 0) then
        write(stdo,*) 'modgrid_rddesc: failed to get sectionlist'
        deallocate(sectionlist)
        i_err = trcio_close(file)
        return
      endif



! get the modgrid parameters from the section header
      n = 1
      o = 0.0
      d = 1.0
      hdwd = -1
      sname = 'MODGRID'
      do  j = 1,nsect
        if(trim(sectionlist(j)) == sname) exit
      end do
      if( j <= nsect ) then   ! we found a modgrid generated trcio file
        i_err = cpsio_get_keyword(lun, &
                'rank',rank,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'name',name,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'pname',pname,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'punits',punits,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'label',labels,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'hdwd',hdwd,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'n_grid',n,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'o_grid',o,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'd_grid',d,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'oxyz',oxyz,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'axis1',axis1,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'axis2',axis2,nel,section_name='MODGRID')
        i_err = cpsio_get_keyword(lun, &
                'axis3',axis3,nel,section_name='MODGRID')
      else              ! No modgrid section, scan the file
        deallocate(sectionlist)
        write(stdo,*) 'modgrid_rddesc: did not find modgrid section'
        i_err = modgrid_rddesc_trcio_scan(stdo,file,&
        10000, xhdr,yhdr,hfast,hslow, n,o,d, tcount)
        if(i_err /= 0) then
          i_err = trcio_close(file)
          return
        endif
        if(tcount /= ntraces) ntraces=tcount
        axis1(1) = 0.0
        axis1(2) = 0.0
        axis1(3) = (n(1)-1)*d(1)
        axis2(1) = (n(2)-1)*d(2)
        axis2(2) = 0.0
        axis2(3) = 0.0
        axis3(1) = 0.0
        axis3(2) = (n(3)-1)*d(3)
        axis3(3) = 0.0
        n(2)    = min(n(2),ntraces)
        oxyz(1) = o(2)
        oxyz(2) = o(3)
        oxyz(3) = o(1)
        hdwd(2) = hfast
        hdwd(3) = hslow
        labels(1) = 'DEPTH'
        hdwd(1) = modgrid_string_to_header(labels(1))
        labels(2) = modgrid_header_to_string(hfast)
        labels(3) = modgrid_header_to_string(hslow)
        if(pname=='UNKNOWN') then
          write(stdo,*) 'modgrid_rddesc_trcio: warning - defaulting&
          & velocity type to VZIN'
          pname = 'VZIN'
        endif
      endif
      status = 0

!
! create the modgrid object
      call modgrid_create (obj, rank, name, pname, punits, &
        n, o, d, stdo)
      call modgrid_set_hdwd(obj,hdwd)
      call modgrid_set_xyz(obj,labels, oxyz, axis1,axis2,axis3)

!
! preserve grid transform information that is in the input file
      call grid_set_xorigin  (obj%gobj, trcio_get_xorigin(file))
      call grid_set_yorigin  (obj%gobj, trcio_get_yorigin(file))
      call grid_set_dx11     (obj%gobj, trcio_get_dx11(file))
      call grid_set_dx22     (obj%gobj, trcio_get_dx22(file))
      call grid_set_dx21     (obj%gobj, trcio_get_dx21(file))
      call grid_set_dx12     (obj%gobj, trcio_get_dx12(file))

      !dfsize = modgrid_file_size(dfile)
      wrdsz = trcio_get_nbits(file)/8
      start_pos = trcio_get_data_start_pos(file)
      offset = start_pos(2)  !assuming we start in the 1st extent

      i_err = trcio_close(file)
      return
      end function modgrid_rddesc_trcio

      integer function modgrid_rddesc_msgrid(obj,fname,stdo,dfile,wtype,offset,endian) &
        result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      integer,intent(out)             :: offset
      integer,intent(out)             :: endian

      type(cardset_struct),pointer :: cobj
      character  :: card*80,cards(100)*80
      integer    :: lun,nr,nc,i_err

      integer    :: n(3)
      real       :: o(3),d(3)
      real       :: angle
      character(len=4)  :: cxy
      character(len=12) :: cendian
 !    real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
 !    character  :: coord(3)*32, msg*80,mode*4
      character  :: msg*80
      character  :: name*64,pname*32,punits*16
      integer    :: nel
      integer    :: xindex,yindex,zindex
      integer    :: address(2)
      logical    :: found
      status = -1
      nullify(obj)
      nullify(cobj)

      offset=0
      endian=0
      dfile= fname
      wtype='IEEE'
      lun = cio_fopen(fname,"r")
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_msgrid:(open error) ',trim(fname)
        return
      endif
      nr=1
      nc=0
      card = ' '
      found = .false.
      do while (nr >= 0 .and. nc< size(cards))
        nr =   cio_fgetline (card, 80,  lun)
        if(.not. found) then
          if(index(card,'<HDR_MODSPEC_GRID>') > 0) found = .true.
          call cardset_create(cobj)
        else
          if(card== ' ') cycle
          if(card(1:1)=='#') card(1:1)=' '
          if(index(card,'</HDR_MODSPEC_GRID> *') > 0) exit
          if(index(card,'=') == 0) then
            cycle
          else
           nc = nc+1
           cards(nc) = card
          endif
        endif
      enddo
      i_err = cio_fclose(lun)
      if(nc > 0) then
        call cardset_put_cards(cobj,cards(1:nc),nc)
      else
        write(stdo,*) 'modgrid_rddesc_msgrid: no input cards?'
        return
      endif
      n    = 1
      o    = 0.0
      d    = 1.0
      cxy  = ' '
      cendian = ' '
      angle=0.0
     !coord= ' '
     !oxyz = 0.0
      xindex=1
      yindex=2
      zindex=3
      call cardset_get_scalar(cobj,'STORAGE_ORDER', cxy, msg)
      if(cxy(1:1)=='X') xindex=1
      if(cxy(2:2)=='X') xindex=2
      if(cxy(1:1)=='Y') yindex=1
      if(cxy(2:2)=='Y') yindex=2
      call cardset_get_scalar(cobj,'NX', n(xindex), msg)
      call cardset_get_scalar(cobj,'NY', n(yindex), msg)
      call cardset_get_scalar(cobj,'XORG', o(xindex), msg)
      call cardset_get_scalar(cobj,'YORG', o(yindex), msg)
      call cardset_get_scalar(cobj,'DX', d(xindex), msg)
      call cardset_get_scalar(cobj,'DY', d(yindex), msg)
      call cardset_get_scalar(cobj,'ANGLE', angle, msg)
      call cardset_get_scalar(cobj,'ENDIAN', cendian, msg)
      address=0
      call cardset_get_array(cobj,'ADDRESS' , address, nel, msg)
      if(nel>0) then
        offset = address(2)
      endif
      call cardset_delete(cobj)
      name = 'modspec_grid'
      pname= 'Z'
      punits= 'xxxx'
      call modgrid_create (obj, 3, name, pname, punits, &
        n, o, d, stdo)
      if(.not.associated(obj)) then
        write(stdo,*) 'modgrid_rddesc_msgrid: create error'
        return
      endif
      if(cendian .ne.' ') then
        if(cendian=='LITLLE_ENDIAN') endian=0
        if(cendian=='BIG_ENDIAN') endian=1
      endif
      status = 0
      return
      end function modgrid_rddesc_msgrid

      integer function modgrid_rddesc_modspec(obj,fname,stdo,dfile,wtype) &
        result(status)
      type(modgrid_struct),pointer    :: obj
      type(modspec_struct),pointer    :: mspobj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype

      character(len=8) :: ftype
      type(grid_struct):: gobj

      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3)
      real       :: alim(3)
      real       :: oxyz(3),axis(3,3)
      character(len=32)  :: coord(3),pname


      character(len=64)  :: name
      character(len=16)  :: punits
      character(len=4)   :: units
      integer    :: i

      double precision :: cpsxo
      double precision :: cpsyo
      double precision :: xorg, xorgs
      double precision :: yorg, yorgs
      double precision :: dangle,ddx,ddy
      integer          :: hx,hy,hz
      integer          :: nx
      integer          :: ny
      integer          :: nz
      real             :: dx
      real             :: dy
      real             :: dz
      real             :: oz
      real             :: ox,oy
      real             :: ogx,ogy
      real             :: angle
      integer          :: nlay
      real             :: rad
      real             :: zmin,zmax
      real             :: acoef(3)
      real             :: bcoef(3)
      logical          :: has_coef
      character(len=4) :: xyz
      integer          :: ix,iy,iz

      status = -1
      nullify(obj)
      nullify(mspobj)
      dfile = fname  !data file is same as header file
      wtype=' '
      ftype = modgrid_ftype(fname,stdo)
      if(ftype /= 'MODSPEC') return

!  reads file and creates default layered grid.
!  modspec is equivalent to an rmod g3dl file
      call modspec_create(mspobj,stdo,fname,'GRID')
      if(.not. associated(mspobj)) then
        write(stdo,*) 'modgrid_rddesc_modspec: failed to create modspec obj'
        return
      endif
      ! returned values are a cps viewpoint
      call modspec_getdesc(mspobj,nlay,angle,units,&
      nx,xorgs,dx,ny,yorgs,dy,nz,oz,dz)
      dangle = angle
      xorg = xorgs
      yorg = yorgs
      !convert negative modspec angles to positive CPS angle?
      if(dangle < 0.0) dangle = 360.0 - dangle 
      ddx = dx   !save physical bin width
      ddy = dy   !save physical bin width
      has_coef = modspec_get_coef_full(mspobj,acoef,bcoef,ox,oy,dx,dy,ogx,ogy,&
                 hx,hy,xyz)
      ! preserve the transform information. May not be compatible with CPS
      
      if(has_coef) then ! coefficients are defined
       !pass actual dx,dy and origin
        xorg = ox  !ogx
        yorg = oy  !ogy
       !dx   = abs(dx)
       !dy   = abs(dy)
       !compute physical bin size for 1 grid bin. Adjust for dx|dy > 1
       if(dx .ne. 0) ddx = ddx/abs(dx)
       if(dy .ne. 0) ddy = ddy/abs(dy)
      else
        write(stdo,*) 'modgrid_rddesc_modspec: get_coef problem has_coef=',&
        has_coef
      endif


      ! load data from modspec file
      call modspec_get_hdrs(mspobj,hx,hy,hz) !x,y and z
      iz = 1  !always true for modspec?
      hdwd(iz)= hz
      if(xyz(2:2)=='X')  ix = 2
      if(xyz(2:2)=='Y')  iy = 2
      if(xyz(3:3)=='X')  ix = 3
      if(xyz(3:3)=='Y')  iy = 3
      hdwd(ix)= hx
      hdwd(iy)= hy
      call modspec_get_zlimits(mspobj,zmin,zmax)
      if(zmax /= zmin) then
         if(zmax-zmin < dz) dz =  (zmax-zmin)/max(1,nlay-1)
      endif



      n    = 1
      o    = 0.0
      d    = 1.0
      coord= ' '
      oxyz = 0.0
      n(iz) = nlay
      if(nz > nlay) n(iz) = nz
      o(iz) = 0.0
      d(iz) = dz
      coord(1) = modgrid_header_to_string(hdwd(1))   !depth
      coord(2) = modgrid_header_to_string(hdwd(2))
      coord(3) = modgrid_header_to_string(hdwd(3))
      name = 'ModspecModel'
      pname= 'VZIN'
      punits= units

      n(ix) = nx
      n(iy) = ny
      if(hdwd(ix)==7 .or. hdwd(ix)==8) then
        o(ix) = xorg
        o(iy) = yorg
        d(ix) = dx
        d(iy) = dy
      else
        o(ix) = xorgs
        o(iy) = yorgs
        d(ix) = dx*ddx
        d(iy) = dy*ddy
      endif

      call modgrid_create (obj, 3, name, pname, punits, &
        n, o, d, stdo)
      if(.not.associated(obj)) then
        write(stdo,*) 'modgrid_rddesc_modspec: create error'
        return
      endif

      rad = dangle/180.* pi
      cpsxo = xorgs - ogx*ddx*cos(rad) + ogy*ddy*sin(rad)
      cpsyo = yorgs - ogx*ddx*sin(rad) - ogy*ddy*cos(rad)
      call grid_set_transform(gobj,cpsxo,cpsyo, dangle, ddx, ddy, 1)
      ! preserve the transform information
      obj%gobj = gobj

      ! save pointer to the mospec object
      obj%mspdata=>mspobj
      call modgrid_set_rnil(obj,modspec_get_znon(mspobj))


      ! determine and set header mapping
      hdwd= -1
      do i = 1,3
        hdwd(i) = modgrid_string_to_header(coord(i))
      enddo
      call modgrid_set_hdwd(obj,hdwd)

      !
      ! set default orientation & size to align with XYZ axis
      axis =0.0

      alim(1)=(n(1)-1)* d(1)
      if(alim(1)==0) alim(1) = d(1)
      alim(2)=(n(2)-1)* d(2)
      if(alim(2)==0) alim(2) = d(2)
      alim(3)=(n(3)-1)* d(3)
      if(alim(3)==0) alim(3) = d(3)
      axis(iz,3) = (nz-1)*dz
      if(has_coef) then
        if(ix==2) then
          axis(ix,1) = alim(2)
          axis(iy,2) = alim(3)
          if(modspec_reversed(obj%mspdata)) then
            axis(ix,1) = alim(3)
            axis(iy,2) = alim(2)
          endif
        else
          axis(iy,2) = alim(2)
          axis(ix,1) = alim(3)
          if(modspec_reversed(obj%mspdata)) then
            axis(iy,2) = alim(3)
            axis(ix,1) = alim(2)
          endif
        endif
      else
        rad = angle/180.* pi
        axis(ix,1) = alim(2)*cos(rad)
        axis(ix,2) = alim(2)*sin(rad)
        axis(iy,1) = -alim(3)*sin(rad)
        axis(iy,2) = alim(3)*cos(rad)
        if(modspec_reversed(obj%mspdata)) then
          axis(ix,1) = alim(3)*cos(rad)
          axis(ix,2) = alim(3)*sin(rad)
          axis(iy,1) = -alim(2)*sin(rad)
          axis(iy,2) = alim(2)*cos(rad)
        endif
      endif
      oxyz(1)  = o(ix)
      oxyz(2)  = o(iy)
      if(modspec_reversed(obj%mspdata)) then
        oxyz(1)  = o(iy)
        oxyz(2)  = o(ix)
      endif
      oxyz(3)  = oz
      call modgrid_set_xyz(obj,coord, oxyz, axis(1,:),axis(2,:),axis(3,:))
      ! set the axis units
      obj%a_unit = units

      status = 0
      return
      end function modgrid_rddesc_modspec

      integer function modgrid_rddesc_rmod(obj,fname,stdo,dfile,wtype) &
        result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype

      character(len=8) :: ftype
      character  :: card*80,cards(100)*80
      integer    :: lun,nr,nc,i_err
      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3),alim(3)
      real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character  :: coord(3)*32, msg*80,mode*4
      character  :: name*64,pname*32,punits*16
      integer    :: i
      logical    :: found
      type(cardset_struct),pointer :: cobj
      status = -1
      nullify(obj)
      nullify(cobj)
      dfile= ' '
      wtype=' '
      ftype = modgrid_ftype(fname,stdo)
      if(ftype /= 'HGRID' .and. ftype /= 'LAYER') return
      mode = 'r'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_rmod:(open error) ',trim(fname)
        return
      endif
      nr=1
      nc=0
      card = ' '
      found = .false.
      do while (nr >= 0 .and. nc< size(cards))
        nr =   cio_fgetline (card, 80,  lun)
        if(.not. found) then
          if(index(card,'*HEADER') > 0) found = .true.
          call cardset_create(cobj)
        else
          if(card(1:1)== '#') cycle
          if(card== ' ') cycle
          if(index(card,' *') > 0) exit
          if(index(card,'=') == 0) then
            cycle
          else
           nc = nc+1
           cards(nc) = card
          endif
        endif
      enddo
      i_err = cio_fclose(lun)
      if(nc > 0) then
        call cardset_put_cards(cobj,cards(1:nc),nc)
      else
        write(stdo,*) 'modgrid_rddesc_rmod: no input cards?'
        return
      endif
! Get modgrid parameter values from the cardset
      n    = 1
      o    = 0.0
      d    = 1.0
      coord= ' '
      oxyz = 0.0
      dfile= ' '
      call cardset_get_scalar(cobj,'NX', n(2), msg)
      call cardset_get_scalar(cobj,'NY', n(3), msg)
      call cardset_get_scalar(cobj,'NZ', n(1), msg)
      call cardset_get_scalar(cobj,'XMIN', o(2), msg)
      call cardset_get_scalar(cobj,'YMIN', o(3), msg)
      call cardset_get_scalar(cobj,'ZMIN', o(1), msg)
      call cardset_get_scalar(cobj,'XINC', d(2), msg)
      call cardset_get_scalar(cobj,'YINC', d(3), msg)
      call cardset_get_scalar(cobj,'ZINC', d(1), msg)
      call cardset_get_scalar(cobj,'XCOORDINATE', coord(2), msg)
      call cardset_get_scalar(cobj,'YCOORDINATE', coord(3), msg)
      call cardset_get_scalar(cobj,'ZCOORDINATE', coord(1), msg)
      call cardset_get_scalar(cobj,'XMODMIN', oxyz(1), msg)
      call cardset_get_scalar(cobj,'YMODMIN', oxyz(2), msg)
      call cardset_get_scalar(cobj,'ZMODMIN', oxyz(3), msg)
      call cardset_get_scalar(cobj,'XMODMAX', alim(1), msg)
      call cardset_get_scalar(cobj,'YMODMAX', alim(2), msg)
      call cardset_get_scalar(cobj,'ZMODMAX', alim(3), msg)
      call cardset_get_scalar(cobj,'FILE', dfile, msg)
      call cardset_get_scalar(cobj,'WORDTYPE', wtype, msg)
      call cardset_delete(cobj)
      name = 'rmod_model'
      pname= 'VZIN'
      punits= 'xxxx'
      call modgrid_create (obj, 3, name, pname, punits, &
        n, o, d, stdo)
      if(.not.associated(obj)) then
        write(stdo,*) 'modgrid_rddesc_rmod: create error'
        return
      endif
! determine and set header mapping
      hdwd= -1
      do i = 1,3
        hdwd(i) = modgrid_string_to_header(coord(i))
      enddo
      call modgrid_set_hdwd(obj,hdwd)
!
! set default orientation & size to align with XYZ axis
      axis1=0.0
      axis2=0.0
      axis3=0.0
      axis1(3) = max(1,(n(1)-1))* d(1) !(alim(3) - oxyz(3))
      axis2(1) = max(1,(n(2)-1))* d(2) !(alim(1) - oxyz(1))
      axis3(2) = max(1,(n(3)-1))* d(3) !(alim(2) - oxyz(2))
      oxyz(1)  = o(2)  !RSD 10-30
      oxyz(2)  = o(3)
      oxyz(3)  = o(1)
      call modgrid_set_xyz(obj,coord, oxyz, axis1,axis2,axis3)
      status = 0
      return
      end function modgrid_rddesc_rmod
!
      logical function modgrid_trace_ordered(obj)&
      result(truth)
      type(modgrid_struct),intent(in)    :: obj
      truth = .false.
      if(string_upper_compare(obj%label(1),'DEPTH')) truth = .true.
      if(string_upper_compare(obj%label(1),'TIME')) truth = .true.
      if(string_upper_compare(obj%label(1),'METER')) truth = .true.
      if(string_upper_compare(obj%label(1),'FEET')) truth = .true.
      if(string_upper_compare(obj%label(1),'SEC')) truth = .true.
      return
      end function modgrid_trace_ordered
!
      logical function modgrid_time_ordered(obj)&
      result(truth)
      type(modgrid_struct),intent(in)    :: obj
      integer   :: rank
      rank = obj%rank
      truth = .false.
      if(string_upper_compare(obj%label(rank),'DEPTH')) truth = .true.
      if(string_upper_compare(obj%label(rank),'TIME')) truth = .true.
      if(string_upper_compare(obj%label(rank),'METER')) truth = .true.
      if(string_upper_compare(obj%label(rank),'FEET')) truth = .true.
      if(string_upper_compare(obj%label(rank),'SEC')) truth = .true.
      return
      end function modgrid_time_ordered
!
      integer function modgrid_xyz_order_o(obj,X,Y,Z) result(status)
      type(modgrid_struct),intent(in)    :: obj
      integer,intent(out)          :: X,Y,Z
      status =  modgrid_xyz_order_s(obj%label,X,Y,Z)
      return
      end function modgrid_xyz_order_o

      integer function modgrid_xyz_order_s(labels,X,Y,Z) result(status)
      character(len=*),intent(in)  :: labels(*)
      integer,intent(out)          :: X,Y,Z
      integer x_is_set
      integer y_is_set
      x_is_set=0
      y_is_set=0
      Z = 1
      X = 2
      Y = 3
      status = 0
      if(string_upper_compare(labels(1)(1:1),'Z')) Z=1
      if(string_upper_compare(labels(1)(1:1),'D')) Z=1
      if(string_upper_compare(labels(1)(1:1),'T')) Z=1
      if(string_upper_compare(labels(1)(1:1),'M')) Z=1
      if(string_upper_compare(labels(1)(1:1),'F')) Z=1
      if(string_upper_compare(labels(1)(1:1),'S')) Z=1
      if(string_upper_compare(labels(2)(1:1),'Z')) Z=2
      if(string_upper_compare(labels(2)(1:1),'D')) Z=2
      if(string_upper_compare(labels(2)(1:1),'T')) Z=2
      if(string_upper_compare(labels(2)(1:1),'M')) Z=2
      if(string_upper_compare(labels(2)(1:1),'F')) Z=2
      if(string_upper_compare(labels(2)(1:1),'S')) Z=2
      if(string_upper_compare(labels(3)(1:1),'Z')) Z=3
      if(string_upper_compare(labels(3)(1:1),'D')) Z=3
      if(string_upper_compare(labels(3)(1:1),'T')) Z=3
      if(string_upper_compare(labels(3)(1:1),'M')) Z=3
      if(string_upper_compare(labels(3)(1:1),'F')) Z=3
      if(string_upper_compare(labels(3)(1:1),'S')) Z=3

      if(string_upper_compare(labels(1)(1:1),'X')) then
         X=1
         x_is_set=1
      endif
      if(string_upper_compare(labels(2)(1:1),'X')) then
         X=2
         x_is_set=1
      endif
      if(string_upper_compare(labels(3)(1:1),'X'))then
         X=3
         x_is_set=1
      endif

      if(string_upper_compare(labels(1)(1:1),'Y')) then
        Y=1
        y_is_set=1
      endif
      if(string_upper_compare(labels(2)(1:1),'Y')) then
        Y=2
        y_is_set=1
      endif
      if(string_upper_compare(labels(3)(1:1),'Y')) then
        Y=3
        y_is_set=1
      endif
      if(string_upper_compare(labels(1),'IL')) then
        Y=1
        y_is_set=1
      endif
      if(string_upper_compare(labels(2),'IL')) then
        Y=2
        y_is_set=1
      endif
      if(string_upper_compare(labels(3),'IL')) then
        Y=3
        y_is_set=1
      endif

      if(string_upper_compare(labels(2)(1:1),'O')) then
        if(y_is_set==1) X=2
        if(x_is_set==1) Y=2
      endif
      if(string_upper_compare(labels(3)(1:1),'O')) then
        if(y_is_set==1) X=3
        if(x_is_set==1) Y=3
      endif

      if( X==Y .or. X==Z .or. Y==Z) status = -1
      if( X + Y + Z /= 6) status = -1
      return
      end function modgrid_xyz_order_s
!
      integer function modgrid_string_to_header(str)&
      result(ihdr)
      character(len=*),intent(in)    :: str
      ihdr = -1
      ! see named_constants
      if(string_upper_compare(str,'CROSSLINE')) ihdr= HDR_MIDPOINT_XGRID
      if(string_upper_compare(str,'INLINE')) ihdr= HDR_MIDPOINT_YGRID
      if(string_upper_compare(str,'XL')) ihdr= HDR_MIDPOINT_XGRID
      if(string_upper_compare(str,'IL')) ihdr= HDR_MIDPOINT_YGRID
      if(string_upper_compare(str,'XANNOTATION')) ihdr= HDR_MIDPOINT_SHOTPOINT
      if(string_upper_compare(str,'YANNOTATION')) ihdr= HDR_MIDPOINT_LINE
      if(string_upper_compare(str,'XBASEMENT')) ihdr= HDR_MIDPOINT_XLOC
      if(string_upper_compare(str,'YBASEMENT')) ihdr= HDR_MIDPOINT_YLOC
      if(string_upper_compare(str,'OFFSET')) ihdr= HDR_OFFSET
      if(string_upper_compare(str,'XSRC')) ihdr= HDR_SOURCE_XLOC
      if(string_upper_compare(str,'YSRC')) ihdr= HDR_SOURCE_YLOC
      if(string_upper_compare(str,'XRCV')) ihdr= HDR_RECEIVER_XLOC
      if(string_upper_compare(str,'YRCV')) ihdr= HDR_RECEIVER_YLOC
      if(string_upper_compare(str,'XGRID')) ihdr= HDR_MIDPOINT_XGRID
      if(string_upper_compare(str,'YGRID')) ihdr= HDR_MIDPOINT_YGRID
      if(string_upper_compare(str,'TIME')) ihdr= -2
      if(string_upper_compare(str,'DEPTH')) ihdr= -3
      if(string_upper_compare(str,'T')) ihdr= -2
      if(string_upper_compare(str,'Z')) ihdr= -3
      if(string_upper_compare(str(1:3),'SEC')) ihdr= -2
      if(string_upper_compare(str(1:5),'METER')) ihdr= -3
      if(string_upper_compare(str(1:2),'KM')) ihdr= -3
      if(string_upper_compare(str,'FEET')) ihdr= -3

      if(string_upper_compare(str,'S_SHOTPOINT')) ihdr=HDR_SOURCE_SHOTPOINT
      if(string_upper_compare(str,'R_SHOTPOINT')) ihdr=HDR_RECEIVER_SHOTPOINT
      if(string_upper_compare(str,'S_LINE')) ihdr=HDR_SOURCE_LINE
      if(string_upper_compare(str,'R_LINE')) ihdr=HDR_RECEIVER_LINE
      if(string_upper_compare(str,'S_XGRID')) ihdr=HDR_SOURCE_XGRID
      if(string_upper_compare(str,'R_XGRID')) ihdr=HDR_RECEIVER_XGRID
      if(string_upper_compare(str,'S_YGRID')) ihdr=HDR_SOURCE_YGRID
      if(string_upper_compare(str,'R_YGRID')) ihdr=HDR_RECEIVER_YGRID
      if(string_upper_compare(str,'ORIG_GROUP')) ihdr=HDR_ORIGINAL_GROUP
      if(string_upper_compare(str,'ORIG_CHANNEL')) ihdr=HDR_ORIGINAL_CHANNEL
      if(string_upper_compare(str,'X')) ihdr=HDR_MIDPOINT_XLOC
      if(string_upper_compare(str,'Y')) ihdr=HDR_MIDPOINT_YLOC
      return
      end function modgrid_string_to_header
!
      function modgrid_header_to_string(ihdr) result(str)
      integer,intent(in)  :: ihdr
      character(len=16)  :: str
      str = 'UNKNOWN'
      if(ihdr==HDR_MIDPOINT_SHOTPOINT) str='XANNOTATION'
      if(ihdr==HDR_MIDPOINT_LINE) str='YANNOTATION'
      if(ihdr==HDR_MIDPOINT_XLOC) str='XBASEMENT'
      if(ihdr==HDR_MIDPOINT_YLOC) str='YBASEMENT'
      if(ihdr==HDR_OFFSET)  str='OFFSET'
      if(ihdr==HDR_SOURCE_XLOC) str='XSRC'
      if(ihdr==HDR_SOURCE_YLOC) str='YSRC'
      if(ihdr==HDR_RECEIVER_XLOC) str='YRCV'
      if(ihdr==HDR_RECEIVER_YLOC) str='XRCV'
      if(ihdr==HDR_MIDPOINT_XGRID)  str='XGRID'
      if(ihdr==HDR_MIDPOINT_YGRID)  str='YGRID'
      if(ihdr==HDR_SOURCE_SHOTPOINT)  str='S_SHOTPOINT'
      if(ihdr==HDR_RECEIVER_SHOTPOINT)  str='R_SHOTPOINT'
      if(ihdr==HDR_SOURCE_LINE)  str='S_LINE'
      if(ihdr==HDR_RECEIVER_LINE)  str='R_LINE'
      if(ihdr==HDR_SOURCE_XGRID)  str='S_XGRID'
      if(ihdr==HDR_RECEIVER_XGRID)  str='R_XGRID'
      if(ihdr==HDR_SOURCE_YGRID)  str='S_YGRID'
      if(ihdr==HDR_RECEIVER_YGRID)  str='R_YGRID'
      if(ihdr==HDR_ORIGINAL_GROUP)  str='ORIG_GROUP'
      if(ihdr==HDR_ORIGINAL_CHANNEL)  str='ORIG_CHANNEL'
      if(ihdr==-1) str='UNKNOWN'
      if(ihdr==-2) str='TIME'
      if(ihdr==-3) str='DEPTH'
      return
      end function modgrid_header_to_string

! ilabels ... labels for the axis of a data cube
! olabels ... labels for the axis of a transposed data cube
! map    ... the index map olabels(i) = ilabels(map(i))
! build inverse map by interchanging ilabels and olabels
      integer function modgrid_build_map(ilabels,olabels,map) result(status)
      character(len=*),intent(in)  :: ilabels(*)
      character(len=*),intent(in)  :: olabels(*)
      integer,intent(out)         :: map(:)
      integer  :: i,j
      status = 0
      map(1:3) = -1
      do i = 1,3
        do j = 1,3
          if(ilabels(i)(1:1) == olabels(j)(1:1)) map(j) = i
        enddo
      enddo
      if(map(1)+map(2)+map(3) /= 6) status = -1
      if(map(1)*map(2)*map(3) /= 6) status = -1
      return
      end function modgrid_build_map

! Same as modgrid_build_map but uses strings rather than character arrays
      integer function modgrid_build_xyz_map(in_xyz,out_xyz,map) result(status)
      character(len=*),intent(in)  :: in_xyz,out_xyz
      integer,intent(out)          :: map(:)
      integer     :: i,j
      status = 0
      map(1:3) = -1
      do i = 1,3
        do j = 1,3
          if(string_upper_compare(in_xyz(i:i),out_xyz(j:j))) map(j) = i
        enddo
      enddo
      if(map(1)+map(2)+map(3) /= 6) status = -1
      if(map(1)*map(2)*map(3) /= 6) status = -1
      return
      end function modgrid_build_xyz_map

! convert a triplet address in a cube to a transformed cube triplet
! using the function map. The transformation is for an interchange
! of axis.
! otrip(i) = itrip(map(i))  , i = 1:3
      subroutine modgrid_map_address(itrip,map,otrip)
      integer,intent(in)  :: itrip(*)
      integer,intent(out) :: otrip(*)
      integer,intent(in)  :: map(*)
      otrip(1) = itrip(map(1))
      otrip(2) = itrip(map(2))
      otrip(3) = itrip(map(3))
      return
      end subroutine modgrid_map_address

! Map a triplet cube address to a sequential storage location
! itrip... the input triplet
! cd   ... cube dimensions
      integer function modgrid_map_toseq(itrip,cd) result(seq)
      integer,intent(in)  :: itrip(*)
      integer,intent(in)  :: cd(*)
      seq = itrip(1) + (itrip(2)-1)*cd(1) + (itrip(3)-1)*cd(1)*cd(2)
      return
      end function modgrid_map_toseq


      integer function modgrid_rddesc_gocad(objs,fname,stdo,dfile,wtype,&
       wrdsz,wname, rgndata)  result(status)
      type(modgrids_struct),pointer   :: objs(:)
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      integer,intent(inout)           :: wrdsz
      character(len=*),intent(inout)  :: wname
      type(region_struct),pointer     :: rgndata
      

      type(modgrid_struct),pointer    :: obj
      character(len=8)    :: ftype
      character(len=2048) :: card
      character(len=120)  :: cards(1000)
      integer    :: lun,nr,nc,i_err
      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3),asize
      real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character(len=32)  :: coord(3),punits
      character(len=32)  :: apunits(16)
      character(len=64)  :: name
      character(len=32)  :: pname,apname(16)
      character(len=160) :: adfile(16)
      character(len=120) :: flagfile    !gocad flags file name
      character(len=120) :: pointfile   !gocad sgrid points file name
      character(len=8)   :: palign(16)  !CELLS or POINTS
      character(len=8)   :: align       !CELLS or POINTS
      character(len=32)  :: rname(40)   !local array for region names
      integer            :: rbit(40)    !region bit settings
      integer            :: rcnt        !region count
      integer            :: flag_esize, flag_bit_len, flag_off
      integer            :: awrdsz(16)
      character(len=12)  :: aunits(3)
      character(len=12)  :: fmt
      integer            :: bu,bv,bw
      character(len=12)  :: afmt(16),awtype(16),awname(16)
      integer            :: abu(16),abv(16),abw(16)
      real               :: ascale(16),aweight(16),scale,weight
      real               :: arndv(16),rndv
      character(len=12)  :: cuvw
      integer            :: i,j,k,l,hit1,nkeys
      integer            :: isgn=0
      integer            :: pn,pcnt
      character(len=24)  :: keys(29)
      character(len=4)   :: mode
      character(len=24)  :: tmp1

      double precision   :: dfsize
      character(len=160) :: tokens(6)
      character(len=8)   :: nilstring
      integer    :: ntokens
      logical    :: found
      real               :: axis_o(3)
      real               :: axis_u(3)
      real               :: axis_v(3)
      real               :: axis_w(3)
      real               :: axis_min(3)
      real               :: axis_max(3)
      real               :: axis_del(3)
      integer            :: ixlab,iylab,izlab
      real               :: s(3)
      character(len=120) :: pdir,dir

      status = -1
      nullify(objs)
      nullify(obj)
      nullify(rgndata)

      dfile= ' '
      flagfile=' '
      pointfile=' '
      palign = ' '
      align = ' '
      apname = ' '
      apunits= ' '
      adfile = ' '
      awname = 'REAL'
      awtype = 'IEEE'
      wname = 'REAL'
      wtype = 'IEEE'
      afmt   = ' '
      awrdsz = 4
      arndv  = MOD_RNIL
      rndv   = MOD_RNIL
      ascale = 0.0
      aweight= 0.0
      abu = 1
      abv = 1
      abw = 1
      pcnt= 0
      flag_esize = 1
      flag_bit_len= 6
      flag_off = 0
      ftype = modgrid_ftype(fname,stdo)
      if(ftype /= 'VOXET' .and. ftype /= 'GSURF' &
        .and. ftype /= 'SGRID') then
       write(stdo,*) 'modgrid_rddesc_gocad: not a gocad grid ',trim(fname)
       return
      endif
      mode = 'r'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_gocad:(open error) ',trim(fname)
        return
      endif
      nr=1
      nc=0
      card = ' '
      found = .false.
      pdir = path_get_dir(fname)   !get fname path 
      if(pdir=='NONE') pdir = ' '

      do while (nr >= 0 .and. nc< size(cards))
        nr =   cio_fgetline (card, 2048,  lun)
        if(.not. found) then
          if(card(1:1)== '#') cycle
          call string_to_upper(card)
          if(index(card,'GOCAD ') > 0) found = .true.
        else
          if(card(1:1)== '#') cycle
          if(card== ' ') cycle
          if(index(card,'END') > 0 .and. &
             index(card,'END_')==0 ) exit
          nc = nc+1
          cards(nc) = card
        endif
      enddo
      i_err = cio_fclose(lun)
      if(nc <= 0) then
        write(stdo,*) 'modgrid_rddesc_gocad: no input cards?'
        return
      endif
      if(nc > size(cards)) then
        write(stdo,*) 'modgrid_rddesc_gocad: > input cards?'
        return
      endif
      keys    = ' '
      keys(1) = 'NAME:'
      keys(2) = 'AXIS_O'      ! -2384.000 89.000000 29.000000
      keys(3) = 'AXIS_U'      !0.000000 0.000000 5369.000000
      keys(4) = 'AXIS_V'      !593.000000 0.000000 0.000000
      keys(5) = 'AXIS_W'      !0.000000 174.0 0.000000
      keys(6) = 'AXIS_N '     !201 101 8
      keys(7) = 'AXIS_MIN'    ! 0  0  0
      keys(8) = 'AXIS_MAX'    ! 1  1  1
      keys(9) = 'AXIS_NAME'   ! "TIME" "XXXX" "YYYY"
      keys(10) = 'PROPERTY '  ! 1 blank separates from PROPERTY_
      keys(11) = 'PROP_FILE'  ! 1 eb3d-0.grid
      keys(12) = 'PROP_ESIZE' ! 1 4
      keys(13) = 'PROP_ETYPE' ! 1 IEEE
      keys(14) = 'name:' !
      keys(15) = 'PROP_UNIT'  ! 'M/S'
      keys(16) = 'AXIS_UNIT'  ! 'M,Ft'
      keys(17) = 'PROP_STORAGE_TYPE'  !  UShort, Char
      keys(18) = 'PROP_SIGNED'        !0,1 for false,true
      keys(19) = 'PROP_FORMAT'        !RAY, SEGY, BRICK
      keys(20) = 'FLAGS_FILE'
      keys(21) = 'FLAGS_ESIZE'        ! (bit_len/8) + 1
      keys(22) = 'FLAGS_BIT_LENGTH'
      keys(23) = 'FLAGS_OFFSET'
      keys(24) = 'REGION '
      keys(25) = 'ORIGIN'      ! -2384.000 89.000000 29.000000
      keys(26) = 'PROPERTY_SUBCLASS'      ! -2384.000 89.000000 29.000000
      keys(27) = 'PROP_NO_DATA_VALUE'     ! pn, rval
      keys(28) = 'POINTS_FILE'
      keys(29) = 'PROP_ALIGNMENT'
      nkeys=29
      n = 1
      o = 0
      d = 1
      hdwd = -1
      coord = ' '
      oxyz = 0.0
      axis1= 0.0
      axis2= 0.0
      axis3= 0.0

      axis_o = 0.0
      axis_u= 0.0
      axis_v= 0.0
      axis_w= 0.0
      axis_min = 0.0
      axis_max = 1.0
      
      punits= 'xxxx'
      aunits= 'U'
      rname = ' '
      rbit  = -1
      rcnt  = 0
      pn = 1    !pn = property number
      do i = 1,nc
        card = cards(i)
        call string_to_upper(card)
        do j = 1,nkeys
          !check for match near front of card
          hit1 = index(card,trim(keys(j)))
          if(hit1 > 0 .and. hit1<12) then
            l = len_trim(keys(j))
            if(j==1 .and. i<12) read(cards(i)(hit1+l:),*) name
            if(j==2) read(cards(i)(hit1+l:),*) axis_o
            if(j==3) then  !AXIS_U and not AXIS_UNIT
              if(cards(i)(hit1+l:hit1+l)==' ') &
              read(cards(i)(hit1+l:),*) axis_u
            endif
            if(j==4) read(cards(i)(hit1+l:),*) axis_v
            if(j==5) read(cards(i)(hit1+l:),*) axis_w
            if(j==6) then !AXIS_N and not AXIS_NAME
              if(cards(i)(hit1+l:hit1+l)==' ') then
               if(ftype=='VOXET' .or. ftype=='SGRID') then
                 read(cards(i)(hit1+l:),*) n(1),n(2),n(3)
               endif
               if(ftype=='GSURF') then
                 read(cards(i)(hit1+l:),*) n(1),n(2)
                 n(3) = 1
               endif
              endif
            endif
            if(j==7) read(cards(i)(hit1+l:),*) axis_min
            if(j==8) read(cards(i)(hit1+l:),*) axis_max
            if(j==9) then
              read(cards(i)(hit1+l:),*) coord(1),coord(2),coord(3)
              do k = 1,3
                hdwd(k) = modgrid_string_to_header(coord(k))
              enddo
            endif
            if(j==10) then !ignore PROPERTY_CLASS
              if(cards(i)(hit1+l:hit1+l)==' ') then
                read(cards(i)(hit1+l:),*) pn, pname
                if(pn<size(apname)) apname(pn) = pname
              endif
            endif
            if(j==11) then
             !read (cards(i)(hit1+l:),*) pn, dfile
              nilstring='9999'
              ntokens=0
              tokens = ' '
              call string_get_tokens(cards(i), tokens, ntokens, nilstring)
              read(tokens(2),*) pn
              pcnt = max(pcnt,pn)
              dfile = tokens(3)
              if(pn<size(adfile)) then
                dir = path_get_dir(dfile)
                if(dir=='NONE') dir = ' '
                adfile(pn) = dfile
                if(dir==' ' .and. pdir.ne.' ') then
                  adfile(pn) = trim(pdir)//dfile
                endif
              endif
            endif
            if(j==12) then ! PROP_ESIZE
              read(cards(i)(hit1+l:),*) pn, wrdsz
              if(pn<size(awrdsz)) awrdsz(pn) = wrdsz
            endif
            ! PROP_ETYPE , GSURF has PROP_TYPE
            if(j==13) then
              read(cards(i)(hit1+l:),*) pn, wtype
              if(pn<size(awtype)) awtype(pn) = wtype
            endif
            if(j==14 .and. i<12) read(cards(i)(hit1+l:),*) name
            if(j==15) then
              nilstring='9999'
              ntokens=0
              tokens = ' '
              call string_get_tokens(cards(i), tokens, ntokens, nilstring)
              read(tokens(2),*) pn
              punits = tokens(3)
              if(pn<size(apunits)) apunits(pn) = punits
            endif
            if(j==16) then
             read(cards(i)(hit1+l:),*) aunits(1),aunits(2),aunits(3)
            endif
            if(j==17) then
             read(cards(i)(hit1+l:),*) pn,wname  !UShort, Char
             call string_to_upper(wname)
             if(pn<size(awname)) awname(pn) = wname
            endif
            if(j==18) then
             read(cards(i)(hit1+l:),*) pn,isgn   !signed flag
            endif
            if(j==19) then
             read(cards(i)(hit1+l:),*) pn,fmt    !PROP_FORMAT, RAW,SEGY,BRICK
             if(pn<size(afmt)) afmt(pn) = fmt
             if(fmt=='BRICK') then
               read(cards(i)(hit1+l:),*) pn,fmt,cuvw   !PROP_FORMAT
               do k=1,len_trim(cuvw)
                if(cuvw(k:k)=='_') CUVW(K:K) = ' '
               enddo
               read(cuvw,*) bu,bv,bw
               abu(pn) = bu
               abv(pn) = bv
               abw(pn) = bw
               print *,'modgrid_rddesc_gocad: DBG Brick format, brick u,v,w=',&
               bu,bv,bw
             endif
            endif
            if(j==20) then
              nilstring='9999'
              ntokens=0
              tokens = ' '
              call string_get_tokens(cards(i), tokens, ntokens, nilstring)
              flagfile = tokens(2)
              dir = path_get_dir(flagfile)
              if(dir=='NONE') dir = ' '
              if(dir==' ' .and. pdir.ne.' ') then
                flagfile = trim(pdir)//flagfile
              endif
            endif
            if(j==21) then
             read(cards(i)(hit1+l:),*) flag_esize
            endif
            if(j==22) then
             read(cards(i)(hit1+l:),*) flag_bit_len
            endif
            if(j==23) then
             read(cards(i)(hit1+l:),*) flag_off
            endif
            if(j==24) then
             if(cards(i)(hit1+l:hit1+l) == ' ') then
               rcnt = rcnt + 1
               rcnt = min(rcnt,size(rbit))
               read(cards(i)(hit1+l:),*) rname(rcnt),rbit(rcnt)
             endif
            endif
            if(j==25) then
              if(cards(i)(hit1+l:hit1+l)==' ') then
               read(cards(i)(hit1+l:),*) axis_o
              endif
            endif
            if(j==26) then
              if(index(cards(i),'LINEARFUNCTION') > 0) then
                read(cards(i)(hit1+l:),*) pn, tmp1,fmt,scale,weight
                if(tmp1=='LINEARFUNCTION') then
                  if(pn<size(ascale)) then
                    ascale(pn) = scale
                    aweight(pn) = weight
                  endif
                endif
              endif
            endif
            if(j==27) then
              read(cards(i)(hit1+l:),*) pn, rndv
              if(pn<size(arndv)) then
                  arndv(pn) = rndv
              endif
            endif
            if(j==28) then
              nilstring='9999'
              ntokens=0
              tokens = ' '
              call string_get_tokens(cards(i), tokens, ntokens, nilstring)
              pointfile = tokens(2)
              dir = path_get_dir(pointfile)
              if(dir=='NONE') dir = ' '
              if(dir==' ' .and. pdir.ne.' ') then
                pointfile = trim(pdir)//pointfile
              endif
            endif
            if(j==29) then
              nilstring='9999'
              ntokens=0
              tokens = ' '
              call string_get_tokens(cards(i), tokens, ntokens, nilstring)
              if(ntokens==3) then
                read(tokens(2),*) pn
                if(pn<size(palign)) palign(pn) = tokens(3)
              endif
              if(ntokens==2) then
                if(pn<size(palign)) align = tokens(2)
              endif
            endif
          endif
        enddo
      enddo  !loop over cards

      wrdsz = max(1,wrdsz)
      wrdsz = min(8,wrdsz)

      !try to determine axis orientation from the labels
      ixlab = 0
      iylab = 0
      izlab = 0
      i_err = modgrid_xyz_order(coord,ixlab,iylab,izlab)
      if(i_err == 0) then
      endif

      !convert to axis_min=0, axis_max=1 viewpoint
      oxyz = axis_o + axis_min(1)*axis_u + axis_min(2)*axis_v + &
       axis_min(3)*axis_w
      n(1) = max(n(1),1)
      n(2) = max(n(2),1)
      n(3) = max(n(3),1)
      if(ftype=='SGRID') then
      endif
      axis_del = (axis_max(1)-axis_min(1))*axis_u
      axis1 = axis_del
      asize = sqrt(axis_del(1)**2 + axis_del(2)**2 + axis_del(3)**2)
      d(1)  = asize/max(1,n(1)-1)
      if(d(1)==0.0) then
        d(1) = 1.0
        i = max(n(1)-1,1)
        if(ixlab==1) axis1 = (/i*d(1), 0.0, 0.0/) 
        if(iylab==1) axis1 = (/0.0, i*d(1), 0.0/) 
        if(izlab==1) axis1 = (/0.0, 0.0, i*d(1)/) 
      endif

      axis_del = (axis_max(2)-axis_min(2))*axis_v
      axis2 = axis_del
      asize = sqrt(axis_del(1)**2 + axis_del(2)**2 + axis_del(3)**2)
      d(2)  = asize/max(1,n(2)-1)
      if(d(2)==0.0) then
        d(2) = 1.0
        i = max(n(2)-1,1)
        if(ixlab==2) axis2 = (/i*d(2), 0.0, 0.0/) 
        if(iylab==2) axis2 = (/0.0, i*d(2), 0.0/) 
        if(izlab==2) axis2 = (/0.0, 0.0, i*d(2)/) 
      endif
      axis_del = (axis_max(3)-axis_min(3))*axis_w
      axis3 = axis_del
      asize = sqrt(axis_del(1)**2 + axis_del(2)**2 + axis_del(3)**2)
      d(3)  = asize/max(1,n(3)-1)
      if(d(3)==0.0) then
        d(3) = 1.0
        i = max(n(3)-1,1)
        if(ixlab==3) axis3 = (/i*d(3), 0.0, 0.0/) 
        if(iylab==3) axis3 = (/0.0, i*d(3), 0.0/) 
        if(izlab==3) axis3 = (/0.0, 0.0, i*d(3)/) 
      endif

      if(axis1(1)< 0.0) d(1) = -d(1)
      if(axis1(2)< 0.0) d(2) = -d(2)
      if(axis1(3)< 0.0) d(3) = -d(3)

      s(1:3) = 1.0
      o = 0.0
      if(axis_u(1)==0 .and. axis_u(2)==0) then
        o(1) = oxyz(3)
        izlab = 1
        if(axis_u(3)< 0.0) s(1) = -1.0
      endif
      if(axis_u(2)==0 .and. axis_u(3)==0) then
        o(1) = oxyz(1)
        ixlab = 1
        if(axis_u(1)< 0.0) s(1) = -1.0
      endif
      if(axis_u(1)==0 .and. axis_u(3)==0) then
        o(1) = oxyz(2)
        iylab = 1
        if(axis_u(2)< 0.0) s(1) = -1.0
      endif

      if(axis_v(1)==0 .and. axis_v(2)==0) then
        o(2) = oxyz(3)
        izlab = 2
        if(axis_v(3)< 0.0) s(2) = -1.0
      endif
      if(axis_v(2)==0 .and. axis_v(3)==0) then
        o(2) = oxyz(1)
        ixlab = 2
        if(axis_v(1)< 0.0) s(2) = -1.0
      endif
      if(axis_v(1)==0 .and. axis_v(3)==0) then
        o(2) = oxyz(2)
        iylab = 2
        if(axis_v(2)< 0.0) s(2) = -1.0
      endif

      if(axis_w(1)==0 .and. axis_w(2)==0) then
        o(3) = oxyz(3)
        izlab = 3
        if(axis_w(3)< 0.0) s(3) = -1.0
      endif
      if(axis_w(2)==0 .and. axis_w(3)==0) then
        o(3) = oxyz(1)
        ixlab = 3
        if(axis_w(1)< 0.0) s(3) = -1.0
      endif
      if(axis_w(1)==0 .and. axis_w(3)==0) then
        o(3) = oxyz(2)
        iylab = 3
        if(axis_w(2)< 0.0) s(3) = -1.0
      endif
      d = s*d  !put proper sign on bin size

      !create array of modgrids objects to hold the voxet components
      i_err = modgrid_modgrids_create(objs, pn)
      if(i_err /=0) then
         status = -1
         return
      endif
      
      do pn = 1,max(1,pcnt) 
        dfsize = modgrid_file_size(adfile(pn))
        call modgrid_create (objs(pn)%mobj, 3, name, apname(pn), apunits(pn), &
        n, o, d, stdo)
        if(.not.associated(objs(pn)%mobj)) then
          print *,'modgrid_rddesc_gocad NULL obj, pn=',pn
          return
        endif
        call modgrid_set_hdwd(objs(pn)%mobj,hdwd)
        call modgrid_set_xyz(objs(pn)%mobj,coord, oxyz, axis1,axis2,axis3)
        objs(pn)%mobj%a_unit(1:3) = aunits(1:3)
        call modgrid_set_dskdata(objs(pn)%mobj,ftype,dfsize,adfile(pn),&
         awtype(pn),awrdsz(pn),awname(pn))
        call modgrid_set_headfile(objs(pn)%mobj,fname)
        if(pointfile .ne. ' ') then
          call modgrid_set_pointfile(objs(pn)%mobj,pointfile )
        endif
        objs(pn)%mobj%palign = align
        if(palign(pn) .ne. ' ') objs(pn)%mobj%palign = palign(pn)
        call modgrid_set_dskdata_scale(objs(pn)%mobj,ascale(pn))
        call modgrid_set_dskdata_add(objs(pn)%mobj,aweight(pn))
        call modgrid_set_rnil(objs(pn)%mobj,arndv(pn))
      enddo

      if(flagfile .ne. ' ' .and. rcnt > 0) then
        call modgrid_create_rgndata(rgndata,flagfile,&
         flag_off,flag_esize,flag_bit_len, rcnt, rname, rbit)
      endif

      dfile = adfile(1)
      wtype = awtype(1)
      wname = awname(1)
      wrdsz = awrdsz(1)
      status = 0

      return
      end function modgrid_rddesc_gocad
  !
  ! - read a velocity file into a grid
  ! - allow interpolation of the X & Y dimensions
  ! - allocate and return the result in ocube
  !
      integer function modgrid_read (maxmem,      &
        stdo, vel_file,   &
        vel_parm,        &
        hx_out, hy_out,            &
        nx_out, x0_out, dx_out,    &
        ny_out, y0_out, dy_out,    &
        nt_out, t0_out, dt_out,    &
        ocube,                     &
        out_xyz                    &
        ) result(status)
    !
    ! - Arguments
    !
      integer, intent (in )               :: maxmem   ! memory limit
      character (len = *), intent (in   ) :: vel_file ! input file name
      character (len = *), intent (in   ) :: vel_parm ! parameter type:
                                                    ! VELOCITY, SLOWNESS
    !
      real,pointer            :: ocube(:,:,:)
      integer, intent (in   ) :: hx_out    !  x location
      integer, intent (in   ) :: hy_out    !  y location
      integer, intent (in   ) :: stdo
      integer, intent (in   ) :: nt_out
      integer, intent (in   ) :: nx_out
      integer, intent (in   ) :: ny_out
      real,    intent (in   ) :: dt_out
      real,    intent (in   ) :: dx_out
      real,    intent (in   ) :: dy_out
      real,    intent (in   ) :: t0_out
      real,    intent (in   ) :: x0_out
      real,    intent (in   ) :: y0_out
      character(len=*),intent(in) :: out_xyz ! grid order
    !
    ! - Local variables
    !
      integer       :: hx_inp, hy_inp
      integer       :: ix_inp_1(nx_out)    ! temporary array
      integer       :: ix_inp_2(nx_out)    ! temporary array
      integer       :: iy_inp_1(ny_out)    ! temporary array
      integer       :: iy_inp_2(ny_out)    ! temporary array
      integer       :: nx_inp              ! number of X coordinates in the
                                           !  x_inp array.
      integer       :: ny_inp              ! number of Y coordinates in the
                                           !  y_inp array.
      real          :: fx_inp_1(nx_out)    ! temporary array
      real          :: fx_inp_2(nx_out)    ! temporary array
      real          :: fy_inp_1(ny_out)    ! temporary array
      real          :: fy_inp_2(ny_out)    ! temporary array
      real, pointer :: x_inp(:)            ! pointer to X coordinates of
                                           !  rectangular array
      real, pointer :: y_inp(:)            ! pointer to Y coordinates of
                                           !  rectangular array

      type(modgrid_struct),pointer    :: obj
      real,pointer  :: icube(:)
      character     :: ftype*8
      character(len=128) :: dfile
      character     :: wtype*8
      character     :: name*64,pname*64,punits*16
      integer       :: npts,ig
      integer       :: ng(3),hd(3),rank,dim
      integer       :: ngo(3)
      real          :: og(3),dg(3)
      integer       :: xlab,ylab,zlab
      character     :: in_xyz*4

      integer       :: otrip(3)
      integer       :: itoo(3)
      logical       :: tf
      integer       :: i_err,ss,ns,n
      status = -1
      nullify (obj )
      nullify (x_inp )
      nullify (y_inp )
      nullify (icube )
      nullify (ocube)
      !get parameters of the input cube
      i_err = modgrid_rddesc(obj,vel_file,stdo,dfile,wtype,ftype)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_read: failed to read input grid'
        return
      endif
      tf = .false.
      if(ftype == 'TRCIO' .or. ftype=='HGRID' .or. ftype=='GSURF' .or. &
         ftype=='VOXET'   .or. ftype=='CPSVEL' .or. &
         ftype=='MODSPEC' .or. ftype=='SEGY' .or. ftype=='SU') then
         tf = .true.
      endif
      if(.not.tf) then
        write(stdo,*) 'modgrid_read: error-unsupported file type!'
        status = -1
        goto 1999
      end if

      ss = 1
      ns = obj%n_grid(3)
      npts  = modgrid_size(obj)
      n = nt_out*nx_out*ny_out
      if(npts  + n > maxmem) then
        write(stdo,*) 'modgrid_read: maxmem < in + out model size'
        write(stdo,*) 'modgrid_read: maxmem =',maxmem
        write(stdo,*) 'modgrid_read: input model_size =',npts
        write(stdo,*) 'modgrid_read: output model_size=',n
        status = -1
        goto 1999
      endif

      !read in the entire input model
      i_err = modgrid_rd_data_iclip(obj,stdo,1,ns)
      if( i_err < 0) then
        write(stdo,*) 'modgrid_read: error-rd_data!'
        status = -1
        goto 1999
      endif

      call modgrid_print(obj,stdo)
      call modgrid_get_name_rank(obj,name,pname ,punits,rank)
      i_err = modgrid_xyz_order(obj,xlab,ylab,zlab)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_read: error-bad input cube ordering!'
        write(stdo,*) 'modgrid_read: ',xlab,ylab,zlab
        goto 1999
      endif
      in_xyz = ' '
      in_xyz(xlab:xlab)='X'
      in_xyz(ylab:ylab)='Y'
      in_xyz(zlab:zlab)='Z'
      if(in_xyz(1:3) /= out_xyz(1:3) ) then
        write(stdo,*) 'modgrid_read: error-bad input cube ordering!'
        write(stdo,*) 'modgrid_read: ',xlab,ylab,zlab
        goto 1999
      endif
      ng=1
      call modgrid_get_name_rank(obj,name,pname ,punits,rank)
      do dim=1,rank
        call modgrid_get_griddesc(obj,dim,hd(dim),ng(dim),og(dim),dg(dim))
      enddo
      nx_inp = ng(xlab)
      ny_inp = ng(ylab)
      hx_inp = hd(xlab)
      hy_inp = hd(ylab)
      allocate(x_inp(ng(xlab)),stat=i_err)
      if(i_err/=0) then
        write(stdo,*) 'modgrid_read: failed to allocate data ', ng(xlab)
        return
      endif
      do ig = 1,ng(xlab)
        x_inp(ig) = og(xlab) + (ig-1)*dg(xlab)
      enddo
      allocate(y_inp(ng(ylab)),stat=i_err)
      if(i_err/=0) then
        write(stdo,*) 'modgrid_read: failed to allocate data ', ng(ylab)
        return
      endif
      do ig = 1,ng(ylab)
        y_inp(ig) = og(ylab) + (ig-1)*dg(ylab)
      enddo

      i_err =  modgrid_build_xyz_map(in_xyz,out_xyz,itoo)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_read: no map from ',in_xyz,' to ',out_xyz
        goto 1999
      endif
      !
      ! save output x-y-z dimensions and save in output order in ngo()
      otrip(xlab) = nx_out  !save dimension of X output axis
      otrip(ylab) = ny_out  !save dimension of Y output axis
      otrip(zlab) = nt_out  !save dimension of T output axis
      ngo(1) = otrip(itoo(1))  !output dimension 1, ie fast
      ngo(2) = otrip(itoo(2))  !output dimension 2
      ngo(3) = otrip(itoo(3))  !output dimension 3, ie slow
      !
      ! allocate memory for the output cube
      allocate(ocube(ngo(1),ngo(2),ngo(3)),stat=i_err)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_read: ocube allocate error'
        status = -1
        goto 1999
      endif

      !
      if (hx_inp == 0 ) hx_inp = hx_out
      if (hy_inp == 0 ) hy_inp = hy_out
! make sure one of hx_inp or hy_inp is hx_out
      if (hx_inp /= hx_out .and. hy_inp /= hx_out) then
        write(stdo,*)  'modgrid_read: Error-The header words do not &
                       &match'
        goto 1999
      end if
      !
      ! - for each output x point determine the input x interpolation
      !   index and coefficient
      !
      call interpolate_find_index_g (nx_inp   = nx_inp,      &
          rx_inp   = x_inp,       &
          nx_out   = nx_out,      &
          x0_out   = x0_out,      &
          dx_out   = dx_out,      &
          ix_inp_1 = ix_inp_1,    &
          ix_inp_2 = ix_inp_2,    &
          fx_inp_1 = fx_inp_1,    &
          fx_inp_2 = fx_inp_2)
      !
      ! - for each output y point determine the input y interpolation
      !   index and coefficient
      !
      call interpolate_find_index_g (nx_inp   = ny_inp,      &
          rx_inp   = y_inp,       &
          nx_out   = ny_out,      &
          x0_out   = y0_out,      &
          dx_out   = dy_out,      &
          ix_inp_1 = iy_inp_1,    &
          ix_inp_2 = iy_inp_2,    &
          fx_inp_1 = fy_inp_1,    &
          fx_inp_2 = fy_inp_2)
      !
      ! - convert from velocity to slowness before interpolating
      !
      i_err =  modgrid_get_data(obj,icube)
      if ( string_upper_compare ( vel_parm(1:1), 'S' ) )then
        !
        if(icube(1) > 1) then
          call modgrid_recip(obj,i_err)
          if(i_err < 0) then
            write(stdo,*) 'modgrid_read: modgrid_recip failed'
            status = -1
            return
          endif
        endif
      end if
      !
      ! - interpolate and permute the input cube
      i_err = modgrid_permute(ng(1),ng(2),ng(3),icube,&
          ngo(1),ngo(2),ngo(3),ocube,&
          in_xyz,out_xyz,&
          fx_inp_1,fx_inp_2,ix_inp_1,ix_inp_2,&
          fy_inp_1,fy_inp_2,iy_inp_1,iy_inp_2)
      status = i_err
      if(i_err /= 0) then
        write(stdo,*)  'modgrid_read: Error-permute failed'
        goto 1999
      endif

      if(ocube(1,1,1) < 1.0) then
        ocube = 1.0/ocube
      endif
     !
     ! - come to here to deallocate memory and return
     !
 1999 continue
     !
     ! - deallocate memory
     !
      call modgrid_delete(obj)
      if (associated(x_inp )) deallocate (x_inp )
      if (associated(y_inp )) deallocate (y_inp )
      if(status /= 0) then
        write(stdo,*) "Error in modgrid_read."
      endif
      return
      end function modgrid_read

! Create an output cube that is a permuted version
! of the input with possible interpolation in the X and Y directions
! n1,n2,n3    ... input cube dimensions
! vi(n1,n2,n3)... input cube
! m1,m2,m3    ... output cube dimensions
! vo(m1,m2,m3)... output cube
! in_xyz   ... string giving X-Y-Z order of input cube axis
!              i.e. in_xyz(1:3)='ZXY' means storage is vi(Z,X,Y)
! out_xyz  ... string giving desired order of output cube axis
!              i.e. in_xyz(1:3)='XYZ' means storage is vo(X,Y,Z)
      integer function modgrid_permute(n1,n2,n3,vi,&
            m1,m2,m3,vo,&
            in_xyz,out_xyz,&
            fx_inp_1,fx_inp_2,ix_inp_1,ix_inp_2,&
            fy_inp_1,fy_inp_2,iy_inp_1,iy_inp_2) result(status)
      integer,intent(in) :: n1,n2,n3
      real,intent(in)    :: vi(n1,n2,n3)
      integer,intent(in) :: m1,m2,m3
      real,intent(inout) :: vo(m1,m2,m3)
      character(len=*),intent(in) :: in_xyz ! grid order
      character(len=*),intent(in) :: out_xyz ! grid order
      real,intent(in)    :: fx_inp_1(:),fx_inp_2(:)
      integer,intent(in) :: ix_inp_1(:),ix_inp_2(:)
      real,intent(in)    :: fy_inp_1(:),fy_inp_2(:)
      integer,intent(in) :: iy_inp_1(:),iy_inp_2(:)

      integer     :: i1,i2,i3
      integer     :: ix,iy,iz
      integer     :: nx1,nx2
      integer     :: ny1,ny2
      integer     :: no(3),i_err
      integer     :: itrip(3)
      integer     :: otrip(3)
      integer     :: itoo(3),otoi(3)
      integer     :: ixlab,iylab,izlab
      real        :: v11,v21,v12,v22
      integer     :: ip123(3)

      i_err =  modgrid_build_xyz_map(in_xyz,out_xyz,itoo)
      if(i_err /= 0) then
        status = -1
        return
      endif
      i_err =  modgrid_build_xyz_map(out_xyz,in_xyz,otoi)

      if(in_xyz(1:1)=='X') ixlab=1
      if(in_xyz(1:1)=='Y') iylab=1
      if(in_xyz(1:1)=='Z' .or. in_xyz(1:1)=='T') izlab=1
      if(in_xyz(2:2)=='X') ixlab=2
      if(in_xyz(2:2)=='Y') iylab=2
      if(in_xyz(2:2)=='Z' .or. in_xyz(2:2)=='T') izlab=2
      if(in_xyz(3:3)=='X') ixlab=3
      if(in_xyz(3:3)=='Y') iylab=3
      if(in_xyz(3:3)=='Z' .or. in_xyz(3:3)=='T') izlab=3

      otrip(1) = m1 !output cube dimensions
      otrip(2) = m2
      otrip(3) = m3
      !reorder for input
      no(1) = otrip(otoi(1))
      no(2) = otrip(otoi(2))
      no(3) = otrip(otoi(3))

      ! cycle over output points, but do it in the order
      ! that is optimal for the input data.
      do i3 = 1 , no(3)   !output index for axis 3 of input
        !
        itrip(3) = i3
        do i2 = 1 , no(2)   !output index for axis 2 of input
          !
          itrip(2) = i2
          do i1 = 1 , no(1)   !output index for axis 1 of input
            !
            itrip(1) = i1
            ix = itrip(ixlab)
            iy = itrip(iylab)
            iz = itrip(izlab)
            nx1= ix_inp_1(ix)  !output ix mapped to an input x location nx1
            nx2= ix_inp_2(ix)
            ny1= iy_inp_1(iy)  !output iy mapped to an input y location ny1
            ny2= iy_inp_2(iy)

            ip123(izlab) = iz
            ip123(ixlab) = nx1
            ip123(iylab) = ny1
            v11 = vi(ip123(1),ip123(2),ip123(3))
            ip123(ixlab) = nx2
            ip123(iylab) = ny1
            v21 = vi(ip123(1),ip123(2),ip123(3))
            ip123(ixlab) = nx1
            ip123(iylab) = ny2
            v12 = vi(ip123(1),ip123(2),ip123(3))
            ip123(ixlab) = nx2
            ip123(iylab) = ny2
            v22 = vi(ip123(1),ip123(2),ip123(3))
            otrip(1) = itrip(itoo(1))
            otrip(2) = itrip(itoo(2))
            otrip(3) = itrip(itoo(3))
            vo(otrip(1), otrip(2), otrip(3)) =          &
                fx_inp_1(ix)*fy_inp_1(iy)*v11 &
              + fx_inp_2(ix)*fy_inp_1(iy)*v21 &
              + fx_inp_1(ix)*fy_inp_2(iy)*v12 &
              + fx_inp_2(ix)*fy_inp_2(iy)*v22
          end do    ! do i1
          !
        end do    ! do i2
        !
      end do    ! do i3
      status = 0
      return
      end function modgrid_permute

      integer function modgrid_paint_by_file(file,maxmem, stdo, &
        ovors, ivors  ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  out_xyz, vtyp_out ) result(status)
      character(len=*),intent(in)        :: file   !input model file name
      integer,intent (in   )             :: maxmem !memory limit
      integer,intent (in )               :: stdo   !standard output
      character (len = *), intent (in   ):: ovors  !slowness/velocity flag
      character (len = *), intent (in   ):: ivors  !interp domain

      integer, intent (in   ) :: hx_out            !x header
      integer, intent (in   ) :: hy_out            !y header
      integer, intent (in   ) :: n1_out
      integer, intent (in   ) :: n2_out
      integer, intent (in   ) :: n3_out
      real,    intent (in   ) :: d1_out
      real,    intent (in   ) :: d2_out
      real,    intent (in   ) :: d3_out
      real,    intent (in   ) :: o1_out
      real,    intent (in   ) :: o2_out
      real,    intent (in   ) :: o3_out
      real,    intent(inout)  :: ocube(n1_out,n2_out,n3_out)
      character(len=*),intent(inout) :: out_xyz ! grid order
      character(len=*),intent(inout) :: vtyp_out ! output type (CPSVEL only)

      character(len=8)  :: wtype
      character(len=12) :: ftype
      character(len=128) :: dfile
      integer           :: scanx, scany
      double precision  :: dfsize
      integer      :: ngo(3)   !number of grid points
      double precision:: ogo(3) !the  grid origin
      real         :: dgo(3)   !the grid bin size
      real         :: oz
      integer      :: ix,iy,iz
      type(modgrid_struct),pointer :: obj
      type(modspec_struct),pointer :: mspdata

      status = -1
      nullify(obj)
      nullify(mspdata)

      ftype = modgrid_ftype(file,stdo,dfsize)
      if(ftype== 'MODSPEC') then
        ix = index(out_xyz,'X')
        iy = index(out_xyz,'Y')
        iz = index(out_xyz,'Z')
        ngo(1) = n1_out
        ogo(1) = o1_out
        dgo(1) = d1_out
        ngo(2) = n2_out
        ogo(2) = o2_out
        dgo(2) = d2_out
        ngo(3) = n3_out
        ogo(3) = o3_out
        dgo(3) = d3_out

        if(ix+iy+iz /=6) then
          print *,'modgrid_paint_by_file DBG: order error'
          return
        endif
        oz = ogo(iz)
        call modspec_create(mspdata,stdo,file,'GRID',&
        ngo(ix),ogo(ix),dgo(ix),&
        ngo(iy),ogo(iy),dgo(iy),&
        ngo(iz),oz,dgo(iz))
        if(.not.associated(mspdata)) then
          print *,'modgrid_paint_by_file: error calling modspec_create'
          return
        endif
      endif

      scanx = HDR_MIDPOINT_XGRID
      scany = HDR_MIDPOINT_YGRID
      status =  modgrid_rddesc_base (obj, file, stdo,dfile,wtype,ftype,&
      scanx, scany,vtyp_out)
      if(status < 0) then
          write(stdo,*) 'modgrid_paint_by_file: error in modgrid_rddesc call'
          return
      endif
      if(associated(mspdata)) then
        ix = modgrid_set_modspec(obj,mspdata,stdo)
       !call modgrid_print(obj,stdo)
      endif

      status = modgrid_paint_by_obj(obj,maxmem, stdo,&
               ovors , ivors ,            &
               hx_out, hy_out,            &
               n1_out, o1_out, d1_out,    &
               n2_out, o2_out, d2_out,    &
               n3_out, o3_out, d3_out,    &
               ocube,  out_xyz, vtyp_out )
      call modgrid_delete(obj)
      return
      end function modgrid_paint_by_file

      integer function modgrid_paint_by_obj(obj,maxmem, stdo,&
        ovors , ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  out_xyz, vtyp_out ) result(status)

     ! arguments
      type(modgrid_struct),intent(inout) :: obj    !input object - painter
      integer,intent (in )               :: maxmem !memory limit
      integer,intent (in )               :: stdo   !standard output
      character (len = *), intent (in   ):: ovors  !slowness/velocity flag
      character (len = *), intent (in   ):: ivors  !slowness/velocity flag

      integer, intent (in   ) :: hx_out            !x header
      integer, intent (in   ) :: hy_out            !y header
      integer, intent (in   ) :: n1_out
      integer, intent (in   ) :: n2_out
      integer, intent (in   ) :: n3_out
      real,    intent (in   ) :: d1_out
      real,    intent (in   ) :: d2_out
      real,    intent (in   ) :: d3_out
      real,    intent (in   ) :: o1_out
      real,    intent (in   ) :: o2_out
      real,    intent (in   ) :: o3_out
      real,    intent(inout)  :: ocube(n1_out,n2_out,n3_out)
      character(len=*),intent(inout) :: out_xyz ! X,Y,Z grid order
      character(len=*),intent(inout) :: vtyp_out ! output type (CPSVEL only)
     ! local variables
      character(len=64)  :: name*64
      character(len=64)  :: pname*64
      character(len=16)  :: punits*16
      character(len=4)   :: in_xyz
      integer            :: hdi(3),hdo(3)
      integer            :: ngi(3),ngo(3),nz
      real               :: ogi(3),ogo(3),oz
      real               :: dgi(3),dgo(3),dz,ze,z1,z2
      integer            :: ixlab,iylab,izlab
      integer            :: oxlab,oylab,ozlab

      ! interpolation coeficients for axis 1,2,3 in input order
      real,pointer       :: f1_inp_1(:) !n1_out)
      real,pointer       :: f1_inp_2(:) !n1_out)
      integer,pointer    :: i1_inp_1(:) !n1_out)
      integer,pointer    :: i1_inp_2(:) !n1_out)
      real,pointer       :: f2_inp_1(:) !n2_out)
      real,pointer       :: f2_inp_2(:) !n2_out)
      integer,pointer    :: i2_inp_1(:) !n2_out)
      integer,pointer    :: i2_inp_2(:) !n2_out)
      real,pointer       :: f3_inp_1(:) !n3_out)
      real,pointer       :: f3_inp_2(:) !n3_out)
      integer,pointer    :: i3_inp_1(:) !n3_out)
      integer,pointer    :: i3_inp_2(:) !n3_out)
      integer            :: otrip(3),itrip(3)
      real, pointer :: c_inp(:)       ! pointer to input coordinates
      real,pointer  :: c4(:,:)
      real          :: ct(2,2)
      real          :: fi(2,2)
      real,pointer  :: icube(:)
      real     :: cube_val
      integer  :: itoo(3)
      integer  :: otoi(3)
      integer  :: i_err
      integer  :: rank
      integer  :: nplanes              ! max input planes in memory
      integer  :: dim
      integer  :: nget
      integer  :: do_read
      integer  :: ig,i1,i2,i3
      integer  :: clips(3)   !min input grid points needed
      integer  :: clipe(3)   !max input grid points needed
      integer  :: slcnt(3)
      integer  :: memclip
      integer  :: sin(3),ein(3),nin(3)
      integer  :: dir        !data in normal or inverted order
      integer  :: c1,c2
      integer  :: u1,u2
      integer  :: v1,v2
      integer  :: w1,w2
      integer(kind=8)  :: nel8
      real   :: s1,s2,s3

      nullify (icube) ! jpa

! input grid = the painter, output grid = the paintee
      ! determine the input grid X-Y-Z ordering
      in_xyz = ' '
      i_err = modgrid_xyz_order(obj,ixlab,iylab,izlab)
      if(i_err/=0) then
        write(stdo,*) 'modgrid_paint: bad in_xyz=',in_xyz
        return
      endif
      in_xyz(ixlab:ixlab)='X'
      in_xyz(iylab:iylab)='Y'
      in_xyz(izlab:izlab)='Z'

      !default to input order if no preference expressed
      if(out_xyz==' ') out_xyz=in_xyz
      oxlab = index(out_xyz,'X')
      oylab = index(out_xyz,'Y')
      ozlab = index(out_xyz,'Z')
      if(ozlab<=0) ozlab = index(out_xyz,'T')

      ! save output variables in arrays
      ngo(1)=n1_out
      ngo(2)=n2_out
      ngo(3)=n3_out
      dgo(1)=d1_out
      dgo(2)=d2_out
      dgo(3)=d3_out
      ogo(1)=o1_out
      ogo(2)=o2_out
      ogo(3)=o3_out
      hdo(oxlab)=hx_out
      hdo(oylab)=hy_out
      hdo(ozlab)=hdi(izlab) !assume depth axis is the same
      if(oxlab+oylab+ozlab /= 6) then
        write(stdo,*) 'modgrid_paint: bad out_xyz=',out_xyz
        return
      endif

      nel8 = modgrid_size8(obj)
      if(nel8 == 1) then
          sin=1
          nin=1
          i_err = modgrid_rd_data_iclips(obj,sin,nin)
          i_err =  modgrid_get_data(obj,icube)
          ocube = icube(1)
          status = 0
          return
      endif

      ! reset z parameters of modspec input objects to the output
      if (associated(obj%mspdata)) then
        nz = 1
        oz = 0
        call modgrid_get_modspec_zgrid(obj,&
             nz,oz,dz)
        ze = oz + (nz-1)*dz
        z1 = ogo(ozlab)
        z2 = ogo(ozlab) + (ngo(ozlab)-1)*dgo(ozlab)
        if(dz /= dgo(ozlab) .or. oz>z1 .or. ze < z2)  then
          call modgrid_set_modspec_zgrid(obj,&
               ngo(ozlab),ogo(ozlab),dgo(ozlab))
          !force a read by freeing data
          i_err =  modgrid_dealloc(obj)
        endif
      endif

      ! Next get axis information about the input object
      status = -1
      ngi=1
      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      do dim=1,rank
        call modgrid_get_griddesc(obj,dim,hdi(dim),ngi(dim),ogi(dim),dgi(dim))
      enddo

      if(in_xyz(1:1)=='X') s1 = sign(1.0,dgi(1)*obj%axis1(1)) 
      if(in_xyz(1:1)=='Y') s1 = sign(1.0,dgi(1)*obj%axis1(2)) 
      if(in_xyz(1:1)=='Z') s1 = sign(1.0,dgi(1)*obj%axis1(3)) 
      if(in_xyz(2:2)=='X') s2 = sign(1.0,dgi(2)*obj%axis2(1)) 
      if(in_xyz(2:2)=='Y') s2 = sign(1.0,dgi(2)*obj%axis2(2)) 
      if(in_xyz(2:2)=='Z') s2 = sign(1.0,dgi(2)*obj%axis2(3)) 
      if(in_xyz(3:3)=='X') s3 = sign(1.0,dgi(3)*obj%axis3(1)) 
      if(in_xyz(3:3)=='Y') s3 = sign(1.0,dgi(3)*obj%axis3(2)) 
      if(in_xyz(3:3)=='Z') s3 = sign(1.0,dgi(3)*obj%axis3(3)) 

      if(s1<0 .or. s2< 0 .or. s3< 0 ) then
        write(stdo,*) 'modgrid_paint_by_obj: warning negative sign?'
        write(stdo,*) 'modgrid_paint_by_obj: ngi=',ngi
        write(stdo,*) 'modgrid_paint_by_obj: ogi=',ogi
        write(stdo,*) 'modgrid_paint_by_obj: dgi=',dgi
        write(stdo,*) 'modgrid_paint_by_obj: s1,s2,s3=',s1,s2,s3
      endif


      if(in_xyz(1:3) == out_xyz(1:3) ) then
        status = modgrid_paint_simple(obj,maxmem, stdo,&
        ovors , ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  out_xyz, vtyp_out )
        return
      endif

      !determine how the axis are to be permuted
      i_err =  modgrid_build_xyz_map(in_xyz,out_xyz,itoo)
      i_err =  modgrid_build_xyz_map(out_xyz,in_xyz,otoi)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_paint: no map from ',in_xyz,' to ',out_xyz
        goto 1999
      endif
     !print *,'modgrid_paint:  in_xyz=',in_xyz,' out_xyz=',out_xyz
     !print *,'paint_by_obj DBG:  itoo(1:3)=', itoo(1:3)
     !print *,'paint_by_obj DBG:  otoi(1:3)=', otoi(1:3)
     !print *,' ngo   ogo  dgo '
     !print *,'paint_by_obj DBG o1=', ngo(1),ogo(1),dgo(1)
     !print *,'paint_by_obj DBG o2=', ngo(2),ogo(2),dgo(2)
     !print *,'paint_by_obj DBG o3=', ngo(3),ogo(3),dgo(3)
     !print *,'paint_by_obj DBG maxmem=', maxmem
     !print *,' ngi   ogi  dgi '
     !print *, ngi(1),ogi(1),dgi(1)
     !print *, ngi(2),ogi(2),dgi(2)
     !print *, ngi(3),ogi(3),dgi(3)
     !print *,' ngo   ogo  dgo '
     !print *,'paint_by DBG', ngo(otoi(1)),ogo(otoi(1)),dgo(otoi(1))
     !print *,'paint_by DBG', ngo(otoi(2)),ogo(otoi(2)),dgo(otoi(2))
     !print *,'paint_by DBG', ngo(otoi(3)),ogo(otoi(3)),dgo(otoi(3))

      if (hdi(ixlab) == -1 ) hdi(ixlab) = hx_out
      if (hdi(iylab) == -1 ) hdi(iylab) = hy_out
      ! make sure one of hx_inp or hy_inp is hdo(oxlab)
      if (hdi(ixlab) /= hx_out .and. hdi(iylab) /= hy_out) then
        write(stdo,*)  'modgrid_paint: Error-The header words do not &
        &match'
        print *,'hdi(ixlab)=',hdi(ixlab),'hdo(oxlab)=',hdo(oxlab)
        print *,'hdi(iylab)=',hdi(iylab),'hdo(oylab)=',hdo(oylab)
        print *,'ixlab=',ixlab,' iylab=',iylab
        goto 1999
      end if

      !set the values of the axis-1, axis-2, axis-3 input coordinate arrays

      !
      ! - for each output point determine the input interpolation
      !   index and coefficient. Match the correct input & output axis.
      !

      ! compute interpolation factors for axis 1,2,3
      allocate(f1_inp_1(ngo(otoi(1))), stat=i_err)
      allocate(f1_inp_2(ngo(otoi(1))), stat=i_err)
      allocate(i1_inp_1(ngo(otoi(1))), stat=i_err)
      allocate(i1_inp_2(ngo(otoi(1))), stat=i_err)
      allocate(f2_inp_1(ngo(otoi(2))), stat=i_err)
      allocate(f2_inp_2(ngo(otoi(2))), stat=i_err)
      allocate(i2_inp_1(ngo(otoi(2))), stat=i_err)
      allocate(i2_inp_2(ngo(otoi(2))), stat=i_err)
      allocate(f3_inp_1(ngo(otoi(3))), stat=i_err)
      allocate(f3_inp_2(ngo(otoi(3))), stat=i_err)
      allocate(i3_inp_1(ngo(otoi(3))), stat=i_err)
      allocate(i3_inp_2(ngo(otoi(3))), stat=i_err)

      allocate(c_inp(max(ngi(1),ngi(2),ngi(3))), stat=i_err)
      if(i_err /= 0) goto 1999

      ! compute interpolation factors for input axis-2
      do ig = 1,ngi(1)
        c_inp(ig) = ogi(1) + s1*(ig-1)*dgi(1)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(1),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(otoi(1)),  &
          x0_out   = ogo(otoi(1)),  &
          dx_out   = dgo(otoi(1)),  &
          ix_inp_1 = i1_inp_1,    &
          ix_inp_2 = i1_inp_2,    &
          fx_inp_1 = f1_inp_1,    &
          fx_inp_2 = f1_inp_2)
      c1 = minval(i1_inp_1(1:ngo(otoi(1))))
      c2 = minval(i1_inp_2(1:ngo(otoi(1))))
      clips(1) = min(c1,c2)
      c1 = maxval(i1_inp_1(1:ngo(otoi(1))))
      c2 = maxval(i1_inp_2(1:ngo(otoi(1))))
      clipe(1) = max(c1,c2)
      nel8 = modgrid_size8(obj)
      if(maxmem >= nel8)  then
        clips(1) = 1
        clipe(1) = ngi(1)  ! read all of input fast storage axis
      endif

      ! compute interpolation factors for input axis-2
      do ig = 1,ngi(2)
        c_inp(ig) = ogi(2) + s2*(ig-1)*dgi(2)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(2),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(otoi(2)),  &
          x0_out   = ogo(otoi(2)),  &
          dx_out   = dgo(otoi(2)),  &
          ix_inp_1 = i2_inp_1,    &
          ix_inp_2 = i2_inp_2,    &
          fx_inp_1 = f2_inp_1,    &
          fx_inp_2 = f2_inp_2)
      c1 = minval(i2_inp_1(1:ngo(otoi(2))))
      c2 = minval(i2_inp_2(1:ngo(otoi(2))))
      clips(2) = min(c1,c2)
      c1 = maxval(i2_inp_1(1:ngo(otoi(2))))
      c2 = maxval(i2_inp_2(1:ngo(otoi(2))))
      clipe(2) = max(c1,c2)
      if(maxmem >= nel8 .or. ngo(3)==1)  then
        clips(2) = 1
        clipe(2) = ngi(2)  ! read all of input 2nd fast storage axis
      endif

      ! compute interpolation factors for input axis-3
      do ig = 1,ngi(3)
        c_inp(ig) = ogi(3) + s3*(ig-1)*dgi(3)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(3),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(otoi(3)),  &
          x0_out   = ogo(otoi(3)),  &
          dx_out   = dgo(otoi(3)),  &
          ix_inp_1 = i3_inp_1,    &
          ix_inp_2 = i3_inp_2,    &
          fx_inp_1 = f3_inp_1,    &
          fx_inp_2 = f3_inp_2)
      c1 = minval(i3_inp_1(1:ngo(otoi(3))))
      c2 = minval(i3_inp_2(1:ngo(otoi(3))))
      clips(3) = min(c1,c2)
      c1 = maxval(i3_inp_1(1:ngo(otoi(3))))
      c2 = maxval(i3_inp_2(1:ngo(otoi(3))))
      clipe(3) = max(c1,c2)
      if (associated(c_inp )) deallocate (c_inp )

      slcnt(1:3) = clipe(1:3) - clips(1:3) + 1
      memclip = slcnt(1)*slcnt(2)*slcnt(3)
      if(memclip > maxmem) then
        print *,'modgrid_paint_by_obj: all input data can not be loaded&
        & at once'
      endif
      dir = 1
      if(ngo(otoi(3)) > 1) then
        if(i3_inp_1(1) > i3_inp_1(ngo(otoi(3))) ) dir = -1
      endif
      !print *,'DBGX: data traversal dir=',dir

     !nplanes = maxmem/(ngi(1)*ngi(2))
      nplanes = maxmem/(slcnt(1)*slcnt(2))
      if(nplanes < 2 .and. ngi(3) > 1) then
        write(stdo,*) 'modgrid_paint_by_obj:maxmem=',maxmem,' too small'
        !maxmem = slcnt(1)*slcnt(2)*2
        nplanes=2
        write(stdo,*) 'modgrid_paint_by_obj:reset maxmem=',slcnt(1)*slcnt(2)*2
      endif

      allocate(c4(ngi(1),4), stat=i_err)
      if(i_err /= 0) goto 1999
      !
      !

      ! read the data in input order and build the output cube
      ! cycle through output locations
      ! but in the storage order of the input cube
      !   print *,'DBG: clips=',clips(1:3)
      !   print *,'DBG: clipe=',clipe(1:3)
      do i3 = 1,ngo(otoi(3))   !corresponds to slow input axis
        itrip(3)=i3
        w1 = i3_inp_1(i3)
        w2 = i3_inp_2(i3)
        if(w2 < w1) then    !we assume w2 >= w1
          write(stdo,*) 'modgrid_paint: error w2=',w2,' < w1=',w1
          status = -1
        endif
        sin = clips
        ein = clipe
        nin = slcnt
        sin(3) = w1
        ein(3) = w2
        nin(3) = w2 - w1 + 1
        do_read = 0
        if(.not. modgrid_is_in_memi3(obj,clips,clipe)) then
          do_read = 1
        endif
       !if(.not. modgrid_is_in_memi(obj,w1,w2) ) then
       !  do_read = 1
       !endif
        if(do_read ==1) then !read if data not in memory
          if(dir > 0) then
            sin(3) = max(w1,1)
            sin(3) = min(w1,ngi(3))
            nin(3) = min(nplanes,ngi(3)- sin(3) + 1)
          else
            sin(3) = max(1,w2 - nplanes + 1)  !is clipped by read
            nin(3) = min(nplanes,ngi(3)- sin(3) + 1)
           !nin(3) = nplanes
          endif
          ein(1:3) = sin(1:3) + nin(1:3) - 1
          i_err = modgrid_rd_data_iclips(obj,sin,nin)

          if(i_err /=0 ) then
            write(stdo,*) 'modgrid_paint: error on read w1=',w1,' w2=',w2
            write(stdo,*) 'modgrid_paint: error nplanes=',nplanes
            status = -1
            goto 1999
          endif
        endif
        ! force interpolation in the slowness domain
       !if(icube(1) > 1) then! - convert from velocity to slowness
        i_err =  modgrid_get_data(obj,icube)
        if(ivors(1:1) == 'V') then! - convert from velocity to slowness
          if(modgmem_inverted(obj%memdata)) then
            call modgrid_recip(obj,i_err)
            if(i_err /=0) then
              write(stdo,*) 'modgrid_paint: modgrid_recip failed'
              return
            endif
          endif
        else
          if(.not. modgmem_inverted(obj%memdata)) then
            call modgrid_recip(obj,i_err)
            if(i_err /=0) then
              write(stdo,*) 'modgrid_paint: modgrid_recip failed'
              return
            endif
          endif
        endif

        do i2 = 1,ngo(otoi(2))
          itrip(2)=i2
          v1 = i2_inp_1(i2)
          v2 = i2_inp_2(i2)
          if(v1<1 .or. v1 > obj%n_grid(2)) then
            write(stdo,*) 'modgrid_paint: error v1=',v1,' v2=',v2
            write(stdo,*) 'modgrid_paint: error i2=',i2
            goto 1999
          endif
          if(v2<1 .or. v2 > obj%n_grid(2)) then
            write(stdo,*) 'modgrid_paint: error v1=',v1,' v2=',v2
            write(stdo,*) 'modgrid_paint: error i2=',i2
            goto 1999
          endif
          !get the 4 neighboring vectors
         !nget = ngi(1)
          nget = nin(1)
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),1), &
                  npts=nget, axis=1, stride=1, u=sin(1),v=v1,w=w1)
          if(i_err /= 0 .or. nget /=nin(1)) goto 1999
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),2), &
                  npts=nget, axis=1, stride=1, u=sin(1),v=v2,w=w1)
          if(i_err /= 0 .or. nget /=nin(1)) goto 1999
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),3), &
                  npts=nget, axis=1, stride=1, u=sin(1),v=v1,w=w2)
          if(i_err /= 0 .or. nget /=nin(1)) goto 1999
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),4), &
                  npts=nget, axis=1, stride=1, u=sin(1),v=v2,w=w2)
          if(i_err /= 0 .or. nget /=nin(1)) goto 1999
          fi(1,1) = f2_inp_1(i2)*f3_inp_1(i3)
          fi(2,1) = f2_inp_2(i2)*f3_inp_1(i3)
          fi(1,2) = f2_inp_1(i2)*f3_inp_2(i3)
          fi(2,2) = f2_inp_2(i2)*f3_inp_2(i3)
          do i1=1,ngo(otoi(1)) !output pts but corresponds to fast input axis
             ! do axis-1 interpolation first
             u1 = i1_inp_1(i1)
             u2 = i1_inp_2(i1)
             ct(1,1) = f1_inp_1(i1)*c4(u1,1) + &
                       f1_inp_2(i1)*c4(u2,1)
             ct(2,1) = f1_inp_1(i1)*c4(u1,2) + &
                       f1_inp_2(i1)*c4(u2,2)
             ct(1,2) = f1_inp_1(i1)*c4(u1,3) + &
                       f1_inp_2(i1)*c4(u2,3)
             ct(2,2) = f1_inp_1(i1)*c4(u1,4) + &
                       f1_inp_2(i1)*c4(u2,4)
             ! now interpolate axis 2-3
             !cube_val = f2_inp_1(i2)*f3_inp_1(i3)*ct(1,1) +&
             !           f2_inp_2(i2)*f3_inp_1(i3)*ct(2,1) +&
             !           f2_inp_1(i2)*f3_inp_2(i3)*ct(1,2) +&
             !           f2_inp_2(i2)*f3_inp_2(i3)*ct(2,2)
              cube_val = fi(1,1)*ct(1,1) +&
                         fi(2,1)*ct(2,1) +&
                         fi(1,2)*ct(1,2) +&
                         fi(2,2)*ct(2,2)
             itrip(1)=i1
             otrip(1) = itrip(itoo(1))
             otrip(2) = itrip(itoo(2))
             otrip(3) = itrip(itoo(3))
            !call modgrid_map_address(itrip,itoo,otrip)
             ocube(otrip(1),otrip(2),otrip(3)) = cube_val
          enddo  !input axis-1 loop
        enddo  !input axis-2 loop
      enddo  !input axis-3 loop

      !return output in the correct state
      if(ovors(1:1) =='V' .or. ovors==' ' ) then
       !if(obj%memdata%is_inverted) &

        if(modgmem_inverted(obj%memdata)) &
          call modgrid_invert_cube(n1_out,n2_out,n3_out,ocube)
      else
       !if(.not. obj%memdata%is_inverted) &

        if(.not. modgmem_inverted(obj%memdata)) &
          call modgrid_invert_cube(n1_out,n2_out,n3_out,ocube)
      endif
      status=0
 1999 continue
     !
     ! - deallocate memory
     !
      deallocate(f1_inp_1, stat=i_err)
      deallocate(f1_inp_2, stat=i_err)
      deallocate(i1_inp_1, stat=i_err)
      deallocate(i1_inp_2, stat=i_err)
      deallocate(f2_inp_1, stat=i_err)
      deallocate(f2_inp_2, stat=i_err)
      deallocate(i2_inp_1, stat=i_err)
      deallocate(i2_inp_2, stat=i_err)
      deallocate(f3_inp_1, stat=i_err)
      deallocate(f3_inp_2, stat=i_err)
      deallocate(i3_inp_1, stat=i_err)
      deallocate(i3_inp_2, stat=i_err)
      if (associated(c4 )) deallocate (c4 )
      if(status /= 0) then
        write(stdo,*) "modgrid_paint: Error"
      endif
      return
      end function modgrid_paint_by_obj


! Paint the output cube assuming the input cube has the same ordering
! of axis. No axis permutaion is involved.
! The Input cube is denoted I and the output cube is denoted O
! The subset of I needed to construct O is I'. If I' does not fit in memory we
! load as many slow axis slices as possible and do disk buffering.
! maxmem is the maximum amount of memory to use
      integer function modgrid_paint_simple(obj,maxmem, stdo,&
        ovors , ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  out_xyz, vtyp_out ) result(status)

     ! arguments
      type(modgrid_struct),intent(inout) :: obj    !input object - painter
      integer,intent (in )               :: maxmem !memory limit
      integer,intent (in )               :: stdo   !standard output
      character (len = *), intent (in   ):: ovors  !slowness/velocity flag
      character (len = *), intent (in   ):: ivors  !slowness/velocity flag

      integer, intent (in   ) :: hx_out    !  x header
      integer, intent (in   ) :: hy_out    !  y header
      integer, intent (in   ) :: n1_out
      integer, intent (in   ) :: n2_out
      integer, intent (in   ) :: n3_out
      real,    intent (in   ) :: d1_out
      real,    intent (in   ) :: d2_out
      real,    intent (in   ) :: d3_out
      real,    intent (in   ) :: o1_out
      real,    intent (in   ) :: o2_out
      real,    intent (in   ) :: o3_out
      real,intent(inout)      :: ocube(n1_out,n2_out,n3_out)
      character(len=*),intent(inout) :: out_xyz ! grid order
      character(len=*),intent(inout) :: vtyp_out ! output type (CPSVEL only)
     ! local variables
      character(len=64)  :: name*64
      character(len=64)  :: pname*64
      character(len=16)  :: punits*16
      character(len=4)   :: in_xyz
      integer            :: hdi(3),hdo(3)
      integer            :: ngi(3),ngo(3)
      real               :: ogi(3),ogo(3)
      real               :: dgi(3),dgo(3)
      integer            :: ixlab,iylab,izlab
      ! interpolation coeficients for axis 1,2,3 in input order
      real               :: f1_inp_1(n1_out)
      real               :: f1_inp_2(n1_out)
      integer            :: i1_inp_1(n1_out)
      integer            :: i1_inp_2(n1_out)
      real               :: f2_inp_1(n2_out)
      real               :: f2_inp_2(n2_out)
      integer            :: i2_inp_1(n2_out)
      integer            :: i2_inp_2(n2_out)
      real               :: f3_inp_1(n3_out)
      real               :: f3_inp_2(n3_out)
      integer            :: i3_inp_1(n3_out)
      integer            :: i3_inp_2(n3_out)
      real, pointer :: c_inp(:)       ! pointer to input coordinates
      real, pointer :: c4(:,:)        ! to store 4 neighbor vector
      real          :: ct(2,2)
      real          :: fi(2,2)
      real,pointer  :: icube(:)
      integer  :: i_err
      integer  :: rank
      integer  :: nplanes              ! max input planes in memory
      integer  :: dim
      integer  :: nget
      integer  :: do_read
      integer  :: ig,i1,i2,i3
      integer  :: clips(3)   !min input grid points needed
      integer  :: clipe(3)   !max input grid points needed
      integer  :: slcnt(3)
      integer  :: memclip
      integer  :: sin(3),ein(3),nin(3)
      integer  :: dir        !data in normal or inverted order
      integer  :: c1,c2
      integer  :: u1,u2
      integer  :: v1,v2
      integer  :: w1,w2,ws,we
     !integer  :: ix,iy
      real     :: s1,s2,s3
      integer(kind=8)  :: nel8  !number of elements in the model

      nullify (icube) ! jpa

! First get information about the input object
! input = the painter.. output grid = the paintee
      status = -1
      ngi=1
      in_xyz = ' '
      i_err = modgrid_xyz_order(obj,ixlab,iylab,izlab)
      if(i_err/=0) then
        write(stdo,*) 'modgrid_paint_simple: bad in_xyz=',in_xyz
        return
      endif
      in_xyz(ixlab:ixlab)='X'
      in_xyz(iylab:iylab)='Y'
      in_xyz(izlab:izlab)='Z'
      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      do dim=1,rank
        call modgrid_get_griddesc(obj,dim,hdi(dim),ngi(dim),ogi(dim),dgi(dim))
      enddo

      nel8 = modgrid_size8(obj)
      if(nel8 == 1) then
          sin=1
          nin=1
          i_err = modgrid_rd_data_iclips(obj,sin,nin)
          i_err =  modgrid_get_data(obj,icube)
          ocube = icube(1)
          status = 0
          return
      endif

      ngo(1)=n1_out
      ngo(2)=n2_out
      ngo(3)=n3_out
      dgo(1)=d1_out
      dgo(2)=d2_out
      dgo(3)=d3_out
      ogo(1)=o1_out
      ogo(2)=o2_out
      ogo(3)=o3_out
      hdo(ixlab)=hx_out
      hdo(iylab)=hy_out
      hdo(izlab)=hdi(izlab) !assume depth axis is the same
      if(out_xyz==' ') out_xyz=in_xyz
      if(in_xyz(1:3) /= out_xyz(1:3) ) then
        write(stdo,*) 'modgrid_paint_simple: in_xyz=',in_xyz,' != out_xyz'
        return
      endif
     !print *,'paint_simple: DBG ngo=',ngo
     !print *,'paint_simple: DBG ogo=',ogo
     !print *,'paint_simple: DBG dgo=',dgo
     !print *,'paint_simple: DBG ngi=',ngi
     !print *,'paint_simple: DBG ogi=',ogi
     !print *,'paint_simple: DBG dgi=',dgi

      if (hdi(ixlab) == -1 ) hdi(ixlab) = hx_out
      if (hdi(iylab) == -1 ) hdi(iylab) = hy_out
      ! make sure the in and out headers match
      if (hdi(ixlab) /= hdo(ixlab) .and. hdi(iylab) /= hdo(ixlab)) then
        write(stdo,*)  'modgrid_paint_simple: Error-The header words &
        &do not match'
        print *,'hdi(ixlab)=',hdi(ixlab),'hdo(ixlab)=',hdo(ixlab)
        print *,'hdi(iylab)=',hdi(iylab),'hdo(iylab)=',hdo(iylab)
        print *,'ixlab=',ixlab,' iylab=',iylab
        goto 1999
      end if

      !set the values of the axis-1, axis-2, axis-3 input coordinate arrays

      !
      ! - for each output point determine the input interpolation
      !   index and coefficient.
      !
      ! compute interpolation factors for output axis-1
      allocate(c_inp(max(ngi(1),ngi(2),ngi(3))), stat=i_err)
      if(i_err /= 0) goto 1999

      if(in_xyz(1:1)=='X') s1 = sign(1.0,dgi(1)*obj%axis1(1)) 
      if(in_xyz(1:1)=='Y') s1 = sign(1.0,dgi(1)*obj%axis1(2)) 
      if(in_xyz(1:1)=='Z') s1 = sign(1.0,dgi(1)*obj%axis1(3)) 
      if(in_xyz(2:2)=='X') s2 = sign(1.0,dgi(2)*obj%axis2(1)) 
      if(in_xyz(2:2)=='Y') s2 = sign(1.0,dgi(2)*obj%axis2(2)) 
      if(in_xyz(2:2)=='Z') s2 = sign(1.0,dgi(2)*obj%axis2(3)) 
      if(in_xyz(3:3)=='X') s3 = sign(1.0,dgi(3)*obj%axis3(1)) 
      if(in_xyz(3:3)=='Y') s3 = sign(1.0,dgi(3)*obj%axis3(2)) 
      if(in_xyz(3:3)=='Z') s3 = sign(1.0,dgi(3)*obj%axis3(3)) 

      if(s1<0 .or. s2< 0 .or. s3< 0 ) then
        write(stdo,*) 'modgrid_paint_simple: warning negative sign?'
        write(stdo,*) 'modgrid_paint_simple: ngi=',ngi
        write(stdo,*) 'modgrid_paint_simple: ogi=',ogi
        write(stdo,*) 'modgrid_paint_simple: dgi=',dgi
        write(stdo,*) 'modgrid_paint_simple: s1,s2,s3=',s1,s2,s3
      endif

      do ig = 1,ngi(1)
        c_inp(ig) = ogi(1) + s1*(ig-1)*dgi(1)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(1),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(1),  &
          x0_out   = ogo(1),  &
          dx_out   = dgo(1),  &
          ix_inp_1 = i1_inp_1,    &
          ix_inp_2 = i1_inp_2,    &
          fx_inp_1 = f1_inp_1,    &
          fx_inp_2 = f1_inp_2)
      c1 = minval(i1_inp_1(1:ngo(1)))
      c2 = minval(i1_inp_2(1:ngo(1)))
      clips(1) = min(c1,c2)
      c1 = maxval(i1_inp_1(1:ngo(1)))
      c2 = maxval(i1_inp_2(1:ngo(1)))
      clipe(1) = max(c1,c2)
      if(maxmem >= nel8)  then
        clips(1) = 1
        clipe(1) = ngi(1)  ! read all of input fast storage axis
      endif

      ! compute interpolation factors for output axis-2
      do ig = 1,ngi(2)
        c_inp(ig) = ogi(2) + s2*(ig-1)*dgi(2)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(2),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(2),  &
          x0_out   = ogo(2),  &
          dx_out   = dgo(2),  &
          ix_inp_1 = i2_inp_1,    &
          ix_inp_2 = i2_inp_2,    &
          fx_inp_1 = f2_inp_1,    &
          fx_inp_2 = f2_inp_2)
      c1 = minval(i2_inp_1(1:ngo(2)))
      c2 = minval(i2_inp_2(1:ngo(2)))
      clips(2) = min(c1,c2)
      c1 = maxval(i2_inp_1(1:ngo(2)))
      c2 = maxval(i2_inp_2(1:ngo(2)))
      clipe(2) = max(c1,c2)
      if(maxmem >= nel8 .or. ngo(3)==1)  then
        clips(2) = 1
        clipe(2) = ngi(2)  ! read all of input 2nd fast storage axis
      endif
     !print *,'paint_simple: DBG c1=',c1,' c2=',c2
     !print *,'paint_simple: DBG clips(2)=',clips(2),' clipe(2)=',clipe(2)

      ! compute interpolation factors for output axis-3
      do ig = 1,ngi(3)
        c_inp(ig) = ogi(3) + s3*(ig-1)*dgi(3)
      enddo
      call interpolate_find_index_g (nx_inp   = ngi(3),      &
          rx_inp   = c_inp,       &
          nx_out   = ngo(3),  &
          x0_out   = ogo(3),  &
          dx_out   = dgo(3),  &
          ix_inp_1 = i3_inp_1,    &
          ix_inp_2 = i3_inp_2,    &
          fx_inp_1 = f3_inp_1,    &
          fx_inp_2 = f3_inp_2)
      c1 = minval(i3_inp_1(1:ngo(3)))
      c2 = minval(i3_inp_2(1:ngo(3)))
      clips(3) = min(c1,c2)
      c1 = maxval(i3_inp_1(1:ngo(3)))
      c2 = maxval(i3_inp_2(1:ngo(3)))
      clipe(3) = max(c1,c2)
      if (associated(c_inp )) deallocate (c_inp )


      slcnt(1:3) = clipe(1:3) - clips(1:3) + 1
      memclip = slcnt(1)*slcnt(2)*slcnt(3)
      if(memclip > maxmem) then
        print *,'modgrid_paint_simple DBG: all input data&
        & can not be loaded at once'
      endif
      dir = 1
      if(ngo(3) > 1) then
        if(i3_inp_1(1) > i3_inp_1(ngo(3)) ) dir = -1
      endif

      nplanes = maxmem/(slcnt(1)*slcnt(2))
      if(nplanes < 2 .and. ngi(3) > 1) then
        write(stdo,*) 'modgrid_paint_simple: maxmem =',maxmem,' too small'
        write(stdo,*) 'modgrid_paint_simple: setting nplanes=2 &
        &and hoping for best'
        nplanes=2
      endif
      ! Do not read more than needed if only outputting a few slices
      if(ngo(3) <= 2 .and. nplanes>3) nplanes = 3

      ! allocate enough for entire axis even if not used
      allocate(c4(ngi(1),4), stat=i_err)
      if(i_err /= 0) goto 1999
      !
      ! read the data in input order and build the output cube
      !paint the output cube with values from the input cube
      !cycle through output locations
      do i3 = 1,ngo(3)
        w1 = i3_inp_1(i3)
        w2 = i3_inp_2(i3)
        ws = min(w1,w2)
        we = max(w1,w2)
        if(w2 < w1) then    !we assume w2 >= w1
          write(stdo,*) 'modgrid_paint_simple: error w2=',w2,' < w1=',w1
          goto 1999
        endif
        if(w1<1 .or. w1 > obj%n_grid(3)) then
          write(stdo,*) 'modgrid_paint_simple: error w1=',w1,' w2=',w2
          write(stdo,*) 'modgrid_paint_simple: error i3=',i3
          goto 1999
        endif
        if(w2<1 .or. w2 > obj%n_grid(3)) then
          write(stdo,*) 'modgrid_paint_simple: error w1=',w1,' w2=',w2
          write(stdo,*) 'modgrid_paint_simple: error i3=',i3
          goto 1999
        endif
! min needed w1:w2 x clips(2):clipe(2) x clips(1):clipe(1)
! max needed clips(3):clipe(3) x clips(2):clipe(2) x clips(1):clipe(1)
! max possible  nplanes x clips(2):clipe(2) x clips(1):clipe(1)
        sin = clips
        ein = clipe
        nin = slcnt
        sin(3) = min(w1,w2)
        ein(3) = max(w1,w2)
        nin(3) = abs(w2 - w1) + 1  !use this ?

        do_read = 0
        if(.not. modgrid_is_in_memi3(obj,sin,ein)) then
          do_read = 1
        endif
        if(do_read ==1) then !read if data not in memory
          if(dir > 0) then
            sin(3) = max(ws,1)  !DBG
            sin(3) = min(ws,ngi(3))
            nin(3) = min(nplanes,ngi(3)- sin(3) + 1)
          else
            sin(3) = max(1,w2 - nplanes + 1)  !is clipped by read
            nin(3) = min(nplanes,ngi(3)- sin(3) + 1)
           !nin(3) = nplanes
          endif
          ein(1:3) = sin(1:3) + nin(1:3) - 1
          i_err = modgrid_rd_data_iclips(obj,sin,nin)
          if(i_err /=0 ) then
            write(stdo,*) 'modgrid_paint_simple: error on read w1=',&
            w1,' w2=',w2,' dir=',dir
            write(stdo,*) 'modgrid_paint_simple: error nplanes=',nplanes
            write(stdo,*) 'modgrid_paint_simple:DBG sin=',sin
            write(stdo,*) 'modgrid_paint_simple:DBG nin=',nin
            write(stdo,*) 'modgrid_paint_simple:DBG ein=',ein
            status = -1
            goto 1999
          endif
        endif
        i_err =  modgrid_get_data(obj,icube)
        if(.not. associated(icube)) then
            write(stdo,*) 'modgrid_paint_simple: DBG0 null icube '
        endif
        ! force interpolation in the slowness domain
        if(ivors(1:1) == 'V') then! - convert from velocity to slowness
          if(modgmem_inverted(obj%memdata)) then
            call modgrid_recip(obj,i_err)
            if(i_err /=0) then
              write(stdo,*) 'modgrid_paint_simple: modgrid_recip failed',i_err
              return
            endif
          endif
        else
          if(.not. modgmem_inverted(obj%memdata)) then
            call modgrid_recip(obj,i_err)
            if(i_err /=0) then
              write(stdo,*) 'modgrid_paint_simple: modgrid_recip failed',i_err
              return
            endif
          endif
        endif

        do i2 = 1,ngo(2)
          v1 = i2_inp_1(i2)
          v2 = i2_inp_2(i2)
          if(v1<1 .or. v1 > obj%n_grid(2)) then
            write(stdo,*) 'modgrid_paint_simple: error v1=',v1,' v2=',v2
            write(stdo,*) 'modgrid_paint_simple: error i2=',i2
            goto 1999
          endif
          if(v2<1 .or. v2 > obj%n_grid(2)) then
            write(stdo,*) 'modgrid_paint_simple: error v1=',v1,' v2=',v2
            write(stdo,*) 'modgrid_paint_simple: error i2=',i2
            goto 1999
          endif

          nget = nin(1)
          !get 4 neighboring vectors surrounding the output location
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),1),&
                  npts=nget, axis=1, stride=1, u=sin(1),v=v1,w=ws)
          if(i_err /= 0 .or. nget /=nin(1)) then
            print *,'modgrid_paint_simple:ERROR 1:',v1,ws
            goto 1999
          endif
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),2),&
                  npts=nget, axis=1, stride=1, u=sin(1),v=v2,w=ws)
          if(i_err /= 0 .or. nget /=nin(1)) then
            print *,'modgrid_paint_simple:ERROR 2:',v1,ws
            goto 1999
          endif
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),3),&
                  npts=nget, axis=1, stride=1, u=sin(1),v=v1,w=we)
          if(i_err /= 0 .or. nget /=nin(1)) then
            print *,'modgrid_paint_simple:ERROR 3:',v1,we
            goto 1999
          endif
          i_err = modgmem_get_pointsr(obj%memdata,c4(sin(1):ein(1),4),&
                  npts=nget, axis=1, stride=1, u=sin(1),v=v2,w=we)
          if(i_err /= 0 .or. nget /=nin(1)) then
            print *,'modgrid_paint_simple:ERROR 4:',v1,v2,we
            goto 1999
          endif
          fi(1,1) = f2_inp_1(i2)*f3_inp_1(i3)
          fi(2,1) = f2_inp_2(i2)*f3_inp_1(i3)
          fi(1,2) = f2_inp_1(i2)*f3_inp_2(i3)
          fi(2,2) = f2_inp_2(i2)*f3_inp_2(i3)
          do i1=1,ngo(1)
             ! do axis-1 interpolation first
             u1 = i1_inp_1(i1)
             u2 = i1_inp_2(i1)
             ct(1,1) = f1_inp_1(i1)*c4(u1,1) + &
                       f1_inp_2(i1)*c4(u2,1)
             ct(2,1) = f1_inp_1(i1)*c4(u1,2) + &
                       f1_inp_2(i1)*c4(u2,2)
             ct(1,2) = f1_inp_1(i1)*c4(u1,3) + &
                       f1_inp_2(i1)*c4(u2,3)
             ct(2,2) = f1_inp_1(i1)*c4(u1,4) + &
                       f1_inp_2(i1)*c4(u2,4)
             ! now interpolate axis 2-3
             !ocube(i1,i2,i3) = f2_inp_1(i2)*f3_inp_1(i3)*ct(1,1) +&
             !                  f2_inp_2(i2)*f3_inp_1(i3)*ct(2,1) +&
             !                  f2_inp_1(i2)*f3_inp_2(i3)*ct(1,2) +&
             !                  f2_inp_2(i2)*f3_inp_2(i3)*ct(2,2)
              ocube(i1,i2,i3) = fi(1,1)*ct(1,1) +&
                                fi(2,1)*ct(2,1) +&
                                fi(1,2)*ct(1,2) +&
                                fi(2,2)*ct(2,2)
          enddo  !output axis-1 loop
        enddo  !output axis-2 loop
      enddo  !output axis-3 loop

      !return output in the correct state
      if(ovors(1:1) =='V' .or. ovors==' ' ) then
       !if(obj%memdata%is_inverted) &

        if(modgmem_inverted(obj%memdata)) &
          call modgrid_invert_cube(n1_out,n2_out,n3_out,ocube)
      else
       !if(.not. obj%memdata%is_inverted) &

        if(.not. modgmem_inverted(obj%memdata)) &
          call modgrid_invert_cube(n1_out,n2_out,n3_out,ocube)
      endif
      status=0
 1999 continue
     !
     ! - deallocate memory
     !
      if (associated(c4 )) deallocate (c4 )
      if(status /= 0) then
        write(stdo,*) 'modgrid_paint_simple: Error'
        write(stdo,*) 'modgrid_paint_simple: w1=',w1,' w2=',w2
        write(stdo,*) 'modgrid_paint_simple: v1=',v1,' v2=',v2
        write(stdo,*) 'modgrid_paint_simple: i1=',i1,' i2=',i2,' i3=',i3
        write(stdo,*) 'modgrid_paint_simple: ngo=',ngo(1:3)
        write(stdo,*) 'modgrid_paint_simple: ogo=',ogo(1:3)
        write(stdo,*) 'modgrid_paint_simple: nget=',nget
        write(stdo,*) 'modgrid_paint_simple: i_err=',i_err
      endif
      return
      end function modgrid_paint_simple

      subroutine modgrid_invert_cube(n1,n2,n3,ocube)
      integer,intent(in)  :: n1,n2,n3
      real,intent(inout)  :: ocube(n1,n2,n3)
      integer  :: i1,i2,i3
          do i3 = 1,n3
            do i2 = 1,n2
              do i1 = 1,n1
                if(ocube(i1,i2,i3)/=0.0 .and. ocube(i1,i2,i3)/=MOD_RNIL) &
                 ocube(i1,i2,i3) = 1.0/ocube(i1,i2,i3)
              enddo
            enddo
          enddo
      return
      end subroutine modgrid_invert_cube

     ! the modspec z grid is a desired target and can be
     ! reset at any time
      subroutine modgrid_set_modspec_zgrid(obj,nz,oz,dz)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(in)    :: nz
      real,intent(in)       :: oz
      real,intent(in)       :: dz
      integer     :: ixlab,iylab,izlab
      integer     :: i_err
      i_err = modgrid_xyz_order(obj,ixlab,iylab,izlab)
      !izlab should be 1 for a modspec object
      if(associated(obj%mspdata)) then
        call modspec_set_z_desc(obj%mspdata,nz,oz,dz)
        obj%n_grid(izlab) = nz
        obj%o_grid(izlab) = oz
        obj%d_grid(izlab) = dz
      endif
      return
      end subroutine modgrid_set_modspec_zgrid

      subroutine modgrid_get_modspec_zgrid(obj,nz,oz,dz)
      type(modgrid_struct),intent(inout) :: obj       ! arguments
      integer,intent(inout)    :: nz
      real,intent(inout)       :: oz
      real,intent(inout)       :: dz
      integer     :: ixlab,iylab,izlab
      integer     :: i_err
      i_err = modgrid_xyz_order(obj,ixlab,iylab,izlab)
      !izlab should be 1 for a modspec object
      if(associated(obj%mspdata)) then
        call modspec_get_z_desc(obj%mspdata,nz,oz,dz)
        nz = obj%n_grid(izlab)
        oz = obj%o_grid(izlab)
        dz = obj%d_grid(izlab)
      endif
      return
      end subroutine modgrid_get_modspec_zgrid


      subroutine modgrid_create_names(oname, ohname,obname, otype, pname)
      character(len=*),intent(in)    :: oname    !target output file name
      character(len=*),intent(out)   :: ohname   !name of header file
      character(len=*),intent(out)   :: obname   !name of binary file
      integer,intent(in)             :: otype    !0-TRCIO, 1-VOXET, 2-SITI
      character(len=*),optional,intent(in)    :: pname   !a property name
      integer    :: idot
      character(len=32) :: pstr

      pstr = ' '
      if(present(pname)) pstr=pname
      ohname = oname
      obname = oname
      idot  = index(oname,'.')
      if(otype==0) then
        if(idot > 1) then  !replace extensions
          ohname = oname(1:idot)//'trc'
        else               !add extensions
          ohname = trim(oname)//'.trc'
        endif
        obname = ohname
      endif
      if(otype==1) then     !VOXET
        ! create the name of the header and binary file
        idot  = index(oname,'.')
        if(idot > 1) then  !replace extensions
          ohname = oname(1:idot)//'vo'
          obname = oname(1:idot)//'vodat'
          if(pstr .ne.' ') then
            obname = oname(1:idot-1)//trim(pstr)//'.vodat'
          endif
        else               !add extensions
          ohname = trim(oname)//'.vo'
          obname = trim(oname)//'.vodat'
          if(pstr .ne.' ') then
            obname = oname(1:idot)//trim(pstr)//'.vodat'
          endif
        endif
      endif
      if(otype==3) then     !GSURF
        ! create the name of the header and binary file
        idot  = index(oname,'.')
        if(idot > 1) then  !replace extensions
          ohname = oname(1:idot)//'grs'
          obname = oname(1:idot)//'grsdat'
          if(pstr .ne.' ') then
            obname = oname(1:idot-1)//trim(pstr)//'.grsdat'
          endif
        else               !add extensions
          ohname = trim(oname)//'.grs'
          obname = trim(oname)//'.grsdat'
          if(pstr .ne.' ') then
            obname = oname(1:idot)//trim(pstr)//'.grsdat'
          endif
        endif
      endif
      if(otype==2) then
        ! create the name of the header and binary file
        idot  = index(oname,'.')
        if(idot > 1) then  !replace extensions
          ohname = oname(1:idot)//'HDR'
          obname = oname(1:idot)//'TRC'
        else               !add extensions
          ohname = trim(oname)//'.HDR'
          obname = trim(oname)//'.TRC'
        endif
      endif
      return
      end subroutine modgrid_create_names
!
      integer function modgrid_regrid(iname, oname, o_type,o_xyz,&
      ovors, ivors, ngo, ogo, dgo ,stdo, maxmem,vtyp_out,&
      xhdr, yhdr,o_endian,&
      scale, clip_min,clip_max,gobj) result(status)
      implicit none
      !input arguments
      character(len=*),intent(in):: iname
      character(len=*),intent(in):: oname
      character(len=*),intent(in):: o_type
      character(len=*),intent(inout):: o_xyz
      character(len=*),intent(inout):: vtyp_out
      character(len=*),intent(in   ):: ovors  !slowness/velocity flag
      character(len=*),intent(in   ):: ivors  !slowness/velocity flag
      integer,intent(in):: ngo(*)
      real,   intent(in):: ogo(*)
      real,   intent(in):: dgo(*)
      integer,intent(in):: stdo
      integer,intent(in):: maxmem
      integer,optional,intent(in):: xhdr
      integer,optional,intent(in):: yhdr
      real,optional,intent(in):: scale
      real,optional,intent(in):: clip_min    !clip on out values after scale
      real,optional,intent(in):: clip_max    !clip on out values after scale
      integer,optional,intent(in):: o_endian !output endian for floats
      type(grid_struct),optional,intent(in) :: gobj
      integer:: pn

      pn=1
      status =  modgrid_regrid_prop(iname, oname, o_type,o_xyz,&
      ovors, ivors, ngo, ogo, dgo ,stdo, maxmem,pn, vtyp_out,&
      xhdr, yhdr,o_endian,&
      scale, clip_min,clip_max,gobj) 
      return
      end function modgrid_regrid


      ! read in one or more model properties, regrid, and
      ! save to a new output file.
      ! Supported output styles: VOXET | TRCIO | SITI | GSURF
      integer function modgrid_regrid_prop(iname, oname, o_type,o_xyz,&
      ovors, ivors, ngo, ogo, dgo ,stdo, maxmem,pn, vtyp_out,&
      xhdr, yhdr,o_endian,&
      scale, clip_min,clip_max,gobj) result(status)

      implicit none
      !input arguments
      character(len=*),intent(in):: iname
      character(len=*),intent(in):: oname
      character(len=*),intent(in):: o_type
      character(len=*),intent(inout):: o_xyz
      character(len=*),intent(inout):: vtyp_out
      character(len=*),intent(in   ):: ovors  !slowness/velocity flag
      character(len=*),intent(in   ):: ivors  !slowness/velocity flag
      integer,intent(in):: ngo(*)
      real,   intent(in):: ogo(*)
      real,   intent(in):: dgo(*)
      integer,intent(in):: stdo
      integer,intent(in):: maxmem
      integer,intent(inout):: pn
      integer,optional,intent(in):: xhdr
      integer,optional,intent(in):: yhdr
      real,optional,intent(in):: scale
      real,optional,intent(in):: clip_min    !clip on out values after scale
      real,optional,intent(in):: clip_max    !clip on out values after scale
      integer,optional,intent(in):: o_endian !output endian for floats
      type(grid_struct),optional,intent(in) :: gobj

      ! local variables
      type(modgrid_struct),pointer :: obj    !a component of input model
      type(modgrid_struct),pointer :: oobj   !a component of output model
      type(modgrids_struct),pointer:: objs(:)
      type(modgrids_struct),pointer:: oobjs(:)
      type(region_struct),pointer  :: rgndata
      type(trcio_struct),pointer   :: trcio
      character(len=8)  :: wtype
      character(len=12) :: ftype
      character(len=128) :: dfile
      character(len=64) :: name
      character(len=64) :: pname
      character(len=16) :: punits
      character(len=128) :: ohname,obname

      character(len=4)  :: i_xyz
      character(len=2048) :: ascrep
      character(len=32) ::   ilabels(3)
      character(len=32) ::   olabels(3)

      integer      :: rank
      integer      :: i_err
      integer      :: hdi(4),hdo(4)
      integer      :: ngi(4)
      real         :: ogi(4),dgi(4)
      integer      :: sslice
      integer      :: ng_slice
      real         :: og_slice, dg_slice
      integer      :: ufi,ufib
      integer      :: gocad
      integer      :: i,j
      integer      :: m1,i1,i2,i3,j1,j2,j3
      integer      :: ixlab,iylab,izlab
      integer      :: oxlab,oylab,ozlab
      integer(kind=8) :: ipts8,opts8
      integer      :: ipts,opts
      integer      :: traceno
      real,pointer :: obuff(:)
      integer      :: endian  !0 for little endian, 1 for big-endian
      integer      :: nwrds,nc
      integer      :: bsiz, wblk
      double precision :: fsize
      integer      :: ngig
      integer      :: ext(2)
      integer      :: start_pos(2) !trcio data start position
      integer      :: offset       !where data starts in an output file
      integer      :: nwr, per_cent
      real         :: orig_i(3), orig_o(3)
      real         :: axis_i(3,3) !(axis index, coord index)
      real         :: axis_o(3,3) !(axis index, coord index)
      integer      :: itoo(3)
      integer      :: scanx, scany
      real         :: data_scale
      real         :: data_clip_min, def_clip_min
      real         :: data_clip_max, def_clip_max
      logical      :: l_doscale
      integer      :: ss(3),ng(3)
      integer      :: oendian = 1
      integer      :: pcnt,props,prope,iprop
      integer      :: do_all_prop
      character(len=32) :: pnames(16)
      character(len=12) :: oftype

      double precision ::  dmem,memi,memo
      character(len=4)  :: units
      integer       :: temp

      status= -1

      gocad = 1
      ufi = -1
      ufib= -1
      nullify(rgndata)
      nullify(objs)
      nullify(oobjs)
      nullify(obj)
      nullify(oobj)
      nullify(trcio)
      nullify(obuff)

      if(iname == oname) then
        write(stdo,*) 'modgrid_regrid_prop: ERROR,output=input,',trim(iname)
        return
      endif
      if(oname== ' ') then
        write(stdo,*) 'modgrid_regrid_prop: ERROR, blank output'
        return
      endif


      data_scale = 1.0
      def_clip_min = MOD_RNIL
      def_clip_max = MOD_RNIL
      data_clip_min = def_clip_min
      data_clip_max = def_clip_max
      if(present(scale) ) data_scale = scale
      if(present(clip_min) ) data_clip_min = clip_min
      if(present(clip_max) ) data_clip_max = clip_max
      if(present(o_endian) ) oendian = o_endian
      l_doscale=.false.
      if(data_scale /=1.0 .or. data_clip_min /= def_clip_min .or.&
         data_clip_max /= def_clip_max) l_doscale=.true.

     ! Get the input data parameters from file iname
     ! The model object will have information on all input properties.
      scanx = HDR_MIDPOINT_XGRID
      scany = HDR_MIDPOINT_YGRID
      if(present(xhdr) ) scanx = xhdr
      if(present(yhdr) ) scany = yhdr
      i_err =  modgrid_rddesc_modgrids(objs,iname,stdo,dfile,wtype,&
       ftype,scanx,scany,vtyp_out,pcnt,pnames,rgndata)
      if(i_err /=0 .or. ftype=='UNKNOWN') then
        status= -1
        write(stdo,*) 'modgrid_regrid_prop: error in modgrid_model_rddesc?,'
        write(stdo,*) 'modgrid_regrid_prop: file=',trim(iname)
        return
      endif

      if(o_type(1:1)=='V') gocad=1   !VOXET output
      if(o_type(1:2)=='GS') gocad=1  !GSURF output is like 2D to VOXET
      if(o_type(1:1)=='T') gocad=0   !TRCIO output
      if(o_type(1:1)=='C') gocad=0   !CPSVEL
      if(o_type(1:1)=='H') gocad=1   !reset HGRID to VOXET
      if(o_type(1:1)=='S') gocad=2   !SITI output
      oftype='VOXET'
      if(gocad==0) oftype='TRCIO'
      if(gocad==2) oftype='SITI'
      if(string_upper_compare(ftype,'GSURF') .and. gocad==1) then
        oftype='GSURF'
      endif
     
      !
      !pcnt = size(objs)

      do_all_prop = 0
      props = pn
      prope = pn
      !only allow multi-prop output for VOXET or GSURF output
      if(gocad==1) then
        if(pn>pcnt .or. pn < 0 ) then
          do_all_prop=1
          props = 1
          prope = pcnt
        endif
        i_err = modgrid_modgrids_create(oobjs, prope-props+1)
        if(i_err .ne. 0) then
          status= -1
          write(stdo,*) 'modgrid_regrid_prop: error wcreating oobjs'
          return
        endif
      else
        props = max(1,min(pn,pcnt))
        prope = max(1,min(pn,pcnt))
        i_err = modgrid_modgrids_create(oobjs, 1)
        if(i_err .ne. 0) then
          status= -1
          write(stdo,*) 'modgrid_regrid_prop: error wcreating oobjs'
          return
        endif
      endif

      !loop over the targeted properties of the model.
      do iprop = props, prope !start of property loop, iprop

        !Get input object for property iprop.
        obj => objs(iprop)%mobj

        !Check to see if we are dealing with a layered modspec model on input
        !We treat modspec models as follows.
        ! 1. build new modspec on the target output grid
        ! 2. interploate mspec_out to output grid points
        !Do not interpolate from input 'grid' to output 'grid'!
        !When there is a modspec input we create a modspec output
        !consistent with the output grid and then we use the
        !regridded modspec to reset the input obj
        if(ftype== 'MODSPEC') then

          call modgrid_regrid_modspec(obj,iname,o_xyz, ngo,ogo,dgo)

        endif
        
        !get detailed information about the input property grid 
        call modgrid_get_name_rank(obj,name,pname,punits,rank)
        do i=1,rank
          call modgrid_get_griddesc(obj,i,hdi(i),ngi(i),ogi(i),dgi(i))
        enddo
        call modgrid_get_xyz(obj,ilabels, orig_i, &
             axis_i(1,:),axis_i(2,:),axis_i(3,:))
        i_err = modgrid_xyz_order(obj, ixlab,iylab,izlab)
        if(i_err /=0) then
          write(stdo,*) 'modgrid_regrid_prop: bad input order'
          goto 99
        endif
        i_xyz=' '
        i_xyz(ixlab:ixlab)='X'
        i_xyz(iylab:iylab)='Y'
        i_xyz(izlab:izlab)='Z'

        !create output object description
        ozlab = index(o_xyz,'Z')
        oxlab = index(o_xyz,'X')
        oylab = index(o_xyz,'Y')
        if(oylab < 1) oylab = index(o_xyz,'IL')
        if(oxlab<1 .or. oylab<1 .or. ozlab<1) then
          write(stdo,*) 'modgrid_regrid_prop:2 bad output order,',o_xyz
          goto 99
        endif
        if(gocad==0 .and. ozlab /= 1) then
          write(stdo,*) 'modgrid_regrid_prop: trcio output has to&
          & be in trace order'
          write(stdo,*) 'modgrid_regrid_prop: trcio output order=',o_xyz
          goto 99
         endif

        !find the mapping, itoo,  from input to output grid ordering
        i_err =  modgrid_build_xyz_map(i_xyz,o_xyz,itoo)

        !Create the ouput object
        name = trim(name)//' regrid'
        call modgrid_create (oobj, rank, name, pname, punits, &
            ngo(1:3), ogo(1:3), dgo(1:3), stdo)
        !Set further details for the output object
        olabels(1)= ilabels(itoo(1))
        olabels(2)= ilabels(itoo(2))
        olabels(3)= ilabels(itoo(3))
        orig_o(1) = ogo(oxlab)
        orig_o(2) = ogo(oylab)
        orig_o(3) = ogo(ozlab)
 
        axis_o = 0.0
        axis_o(oxlab,1) = (ngo(oxlab)-1)*dgo(oxlab)
        if(axis_o(oxlab,1) == 0)  axis_o(oxlab,1) = dgo(oxlab)
        axis_o(oylab,2) = (ngo(oylab)-1)*dgo(oylab)
        if(axis_o(oylab,2) == 0)  axis_o(oylab,2) = dgo(oylab)
        axis_o(ozlab,3) = (ngo(ozlab)-1)*dgo(ozlab)
        if(axis_o(ozlab,3) == 0)  axis_o(ozlab,3) = dgo(ozlab)
        call modgrid_set_xyz(oobj,olabels, orig_o,&
           axis_o(1,:),axis_o(2,:),axis_o(3,:))
        hdo(1) = hdi(itoo(1))
        hdo(2) = hdi(itoo(2))
        hdo(3) = hdi(itoo(3))
        call modgrid_set_hdwd(oobj,hdo)
        !Preserve the input object grid transform information(if any)
        if(present(gobj) ) then !use any passed arguments
          oobj%gobj = gobj
        else
          oobj%gobj = obj%gobj
        endif
        do i=1,rank
          call modgrid_get_aunit(obj,i,units)
          call modgrid_set_aunit(oobj,i,units)
        enddo

        !generate output file names
        if(o_type(1:1)=='V' .or. string_upper_compare(o_type,'GSURF') ) then
          if(string_upper_compare(o_type,'GSURF')) then
            call modgrid_create_names(oname, ohname,obname, 3,pname)
          else
            call modgrid_create_names(oname, ohname,obname, gocad,pname)
          endif
        else
          call modgrid_create_names(oname, ohname,obname, gocad)
        endif
        endian = swap_endian()
        if(gocad == 0 .and. o_type(1:1)=='T') then
          !write trcio header before traces are written
          write(stdo,*) 'modgrid_regrid_prop: writing trcio header'
          trcio => modgrid_wr_trcio_header(oobj,obname,stdo)
          if(.not.associated(trcio)) then
            write(stdo,*) 'modgrid_regrid_prop: error - null trcio'
            goto 99
          endif
          start_pos = trcio_get_data_start_pos(trcio)
          offset = start_pos(2)  !assuming we start in the 1st extent
        endif

        !set constraints on buffers for input and output
        offset = 0
        start_pos = 0
        opts8= ngo(1)*ngo(2)
        opts8= opts8*ngo(3)
        ipts8= ngi(1)*ngi(2)
        ipts8= ipts8*ngi(3)
        memo = ngo(1)*ngo(2)
        memo = memo*ngo(3)
        memi = ngi(1)*ngi(2)
        memi = memi*ngi(3)
        dmem = memi+memo
        if(dmem < maxmem) then
          ipts = ipts8
          opts = opts8
        else
          if(opts8 > maxmem/2) then
            opts8 = maxmem/2
          endif
          if(ipts8 > maxmem/2) then
            ipts8 = maxmem/2
          endif
          opts = opts8
          ipts = ipts8
        endif
        if(iprop==props)then
          allocate(obuff(opts),stat=i_err)
          if(i_err .ne. 0) then
            write(stdo,*) 'modgrid_regrid_prop: obuff allocate error ',opts
            goto 99
          endif
          obuff = 0.0
        endif
        !Constrain the output slab size
        if(rank >=3) then
          ng_slice = opts/(ngo(1)*ngo(2))
          if(ng_slice>ngo(3)) ng_slice = ngo(3)
        else
          ng_slice = 1
        endif

        if(gocad/=0) then   !force non-trcio files to be a single extent
          fsize = 4*ngo(1)
          fsize = fsize*ngo(2)
          fsize = fsize*ngo(3)
          fsize = fsize + 8192
          ngig  = fsize/250000000 + 1
          ext(1)= ngig
          ext(2)= 0
          i_err = cio_set_file_ext_size(ext)
          ufib  = cio_fopen(obname,'w+')
          if(ufib==CIO_ERROR) then
            write(stdo,*) 'modgrid_regrid_prop: error opening obname=',&
            trim(obname)
            goto 99
          endif
        endif

        !Now interpolate the input data and cycle over output slabs
        sslice = 1 !first output slice
        per_cent = 0
        do while (sslice <= ngo(3) )  !cycle over output slabs

          !set output slab coordinates
          ng_slice  = min(ng_slice,ngo(3)-sslice+1)
          og_slice  = ogo(3) + (sslice-1)*dgo(3)
          dg_slice  = dgo(3)
          nwrds = ngo(1)*ngo(2)*ng_slice
          obuff(1:nwrds) = 0.0

          !generate the output slab ngo(1) x ngo(2)x ng_slice
          !output has same x-y headers as input
          i_err = modgrid_paint_by_obj(obj,ipts,stdo,&
              ovors,ivors, &
              hdi(ixlab),hdi(iylab),&
              ngo(1),ogo(1),dgo(1),&
              ngo(2),ogo(2),dgo(2),&
              ng_slice,og_slice,dg_slice,&
              obuff, o_xyz, vtyp_out)
          if(i_err < 0) then
            write(stdo,*) '#modgrid_regrid_prop: paint error !'
            write(stdo,*) '#modgrid_regrid_prop: iprop=',iprop
            write(stdo,*) '#modgrid_regrid_prop: pname=',trim(pname)
            write(stdo,*) '#modgrid_regrid_prop: sslice=',sslice
            write(stdo,*) '#modgrid_regrid_prop: ng_slice=',ng_slice
            write(stdo,*) '#modgrid_regrid_prop: og_slice=',og_slice
            write(stdo,*) '#modgrid_regrid_prop: ngo=',ngo(1:3)
            write(stdo,*) '#modgrid_regrid_prop: ogo=',ogo(1:3)
            write(stdo,*) '#modgrid_regrid_prop: dgo=',dgo(1:3)
            write(stdo,*) '#modgrid_regrid_prop: ipts=',ipts
            write(stdo,*) '#modgrid_regrid_prop: opts=',opts
            goto 99
          endif

          ! cycle through output grid points in disk order
          ! which may be reverse order in memory. Output a block of
          ! data that is ngo(1)*ngo(2)*ng_slice words.
          i1 = 1
          i2 = ng_slice
          i3 = 1
          j1 = 1
          j2 = ngo(2)
          j3 = 1
          bsiz = 4*ngo(1)           !for trcio output, make a block = 1 trace
          wblk = (sslice-1)*ngo(2)
          traceno=(sslice-1)*ngo(2)
          if(l_doscale) then
              call modgrid_clip_scale(obuff,nwrds,data_clip_min,data_clip_max,&
              data_scale)
          endif
          if(gocad==0) then   !TRCIO or CPSVEL output
           if(o_type=='CPSVEL') then !output a CPSVEL ascii file
             !hand oobj the data buffer. May be used by multiple oobj.
             i_err = modgrid_use_data(oobj,ngo(1),ngo(2),1,ng_slice,obuff)
             if(i_err /=0) then
               write(stdo,*) '#modgrid_regrid_prop: use_data error'
               goto 99
             endif
             ss(1:3) = 1
             ng(1:3) = ngo(1:3)
             i_err = modgrid_wr_cvf (oobj,oname,stdo,ss,ng,vtyp_out)
             if(i_err /=0) then
               write(stdo,*) '#modgrid_regrid_prop: wr_cvf error'
               goto 99
             endif
           else               !output a modified TRCIO file
             do i= i1,i2,i3
             do j= j1,j2,j3
               traceno = traceno + 1
               m1 = (i-1)*ngo(2)*ngo(1) + (j-1)*ngo(1) + 1
               i_err = modgrid_wr_trcio_trace(oobj,trcio,traceno,&
                obuff(m1:m1+ngo(1)-1))
               if(i_err /=0) then
                 write(stdo,*) '#modgrid_regrid_prop: trcio write error traceno='&
                 ,traceno
                 goto 99
               endif
             enddo !trace within slice
             enddo !slice
           endif
          else    !VOXET or SITI binary output
           !! we normally save voxet output as big-endian,SITI as native
           if(oendian /= endian) then
             call swap_bytes(obuff(1:nwrds))
           endif
           ! seek to correct start point
           i_err = cio_fseek(ufib,bsiz,wblk,0,0)
           do i= i1,i2,i3 !slow output dimension
           do j= j1,j2,j3
                m1 = (i-1)*ngo(2)*ngo(1) + (j-1)*ngo(1) + 1
                nwr = cio_fwrite(obuff(m1),1,bsiz,ufib)
                if(nwr < 0) then
                  write(stdo,*) 'modgrid_regrid_prop: cio write error'
                  i_err = -1
                  goto 99
                endif
               !off= off + bsiz  RSD 10/05
                wblk = wblk + 1
           enddo !trace within slice
           enddo !slice
          endif

         sslice = sslice + ng_slice
         per_cent = 100.0*(sslice-1)/(ngo(3)-1+1)
         write(stdo,*) '#modgrid_regrid_prop:',trim(pname),' % done=',per_cent

        enddo  !end of slab output loop

        !if binary data write is OK set the dskdata infomation in oobj
        if(i_err /=0) then
          write(stdo,*) '#modgrid_regrid_prop: error'
          goto 99
        else
          fsize = 4*ngo(1)
          fsize = fsize*ngo(2)
          fsize = fsize*ngo(3)
          print *,'regrid_prop DBG: obname=',trim(obname)
          call modgrid_set_dskdata(oobj,oftype,fsize,obname,&
           'IEEE',4,'REAL',offset)
          call modgrid_set_headfile(oobj,ohname)
          oobjs(iprop-props+1)%mobj => oobj
          if(ufib>0) then
            i_err = cio_fclose(ufib)
            if(i_err==0) ufib= 0
          endif
        endif

      enddo  !end of property loop, iprop

      print *,'modgrid_regrid_prop DBG: end of property loop'
      if(gocad==2 .and. o_type(1:1)=='S') then
        call modgrid_to_ascii(oobj,ascrep,obname,gocad)
      endif
      if(gocad==1 .and. o_type(1:1)=='V') then
        !write voxet header after all binary files have been written
        i_err = modgrid_modgrids_to_voxhdr(oobjs, ascrep, nc,rgndata)
        if(i_err < 0)then
          write(stdo,*) 'modgrid_regrid_prop: voxet header error ascrep(1:nc)=',&
          ascrep(1:nc)
          write(stdo,*) 'modgrid_regrid_prop: will try to write it anyway'
        endif
      endif
      if(gocad .ne. 0) then
        ! write the voxet or siti header
        ufi = cio_fopen(ohname,'w')
        if(ufi <= 0) then
          write(stdo,*) 'modgrid_regrid_prop: error on open:',trim(ohname)
          goto 99
        endif
        temp = len_trim(ascrep)
        nwr = cio_fwrite(ascrep,1,temp,ufi)
        if(nwr <0) then
          write(stdo,*) 'modgrid_regrid_prop: header write error nwr=',nwr
          goto 99
        endif
        i_err = cio_fclose(ufi)
      endif


      status = 0
 99   continue
      if(associated(obuff) .and. o_type(1:1)/='C') deallocate(obuff)
      if(gocad == 0) then
        if(associated(trcio)) then
        !print *,'CLOSING trcio file'
         i_err = trcio_close(trcio)
        endif
      else
        if(ufib>0) i_err = cio_fclose(ufib)
      endif
      i_err =  modgrid_modgrids_delete(objs)
     !call modgrid_delete(obj)
     !call modgrid_delete(oobj)

      return
      end function modgrid_regrid_prop

      subroutine modgrid_wr_header(oobj,hfile,dfile,ostyle,stdo,i_err)
      type(modgrid_struct),pointer :: oobj
      character(len=*),intent(in)  :: hfile
      character(len=*),intent(in)  :: dfile
      integer,intent(in )          :: ostyle ! 0:modgrid, 1:gocad, , 2:siti
      integer,intent(in )          :: stdo
      integer,intent(out)          :: i_err
      character(len=2048)  :: ascrep
      integer   :: ufi
      integer   :: nwr , temp
      i_err = -1
      !
      ! write the voxet or vset header
      ufi = cio_fopen(hfile,'w')
      if(ufi <= 0) then
        write(stdo,*) 'modgrid_wr_header: error on open:',trim(hfile)
        goto 99
      endif

      call modgrid_to_ascii(oobj,ascrep,dfile,ostyle)
      temp = len_trim(ascrep)
      nwr = cio_fwrite(ascrep,1,temp,ufi)
      if(nwr <0) then
        write(stdo,*) 'modgrid_wr_header: write error nwr=',nwr
        goto 99
      endif
      if(ufi>0) i_err = cio_fclose(ufi)
      i_err = 0
      return
 99   continue
      if(ufi>0) i_err = cio_fclose(ufi)
      return
      end subroutine modgrid_wr_header

    ! integer function modgrid_bracket(obj,&
    !  ax,ax_min,ax_max,clips,clipe) result(status)
    ! type(modgrid_struct),intent(in) :: obj       ! arguments
    ! integer,intent(in)    :: ax
    ! real,intent(in   )    :: ax_min
    ! real,intent(in   )    :: ax_max
    ! integer,intent(inout) :: clips
    ! integer,intent(inout) :: clipe
    ! integer  :: i1
    ! status = -1
    ! clips = 1
    ! clipe = 1
    ! if(ax < 1 .or. ax > obj%rank) return
    ! status = 0
    ! ! translate clip window to grid nodes
    ! i1 = 1 + max(ax_min - obj%o_grid(ax),0.0)/obj%d_grid(ax)
    ! i1 = min(obj%n_grid(ax),i1)
    ! clips = i1
    ! i1 = 1 + max(ax_max - obj%o_grid(ax),0.0)/obj%d_grid(ax)
    ! i1 = i1+1
    ! i1 = min(obj%n_grid(ax),i1)
    ! clipe = i1
    ! fint = (ax_min - (obj%o_grid(ax) + (clips-1)*obj%d_grid(ax))
    ! fint = max(0.0,fint/obj%d_grid(ax))
    ! return
    ! end function modgrid_bracket

! note: the grid may descibe a larger data set than actually
!       exists within the file contents.
      integer function modgrid_rddesc_trcio_scan(stdo,file,&
        nscan, xhdr,yhdr,hfast,hslow,&
       nbin,obin,dbin, tcount ) result(status)
      integer,intent(in)    :: stdo
      type(trcio_struct),pointer :: file
      integer,intent(in)    :: nscan
      integer,intent(in)    :: xhdr
      integer,intent(in)    :: yhdr
      integer,intent(inout) :: hfast
      integer,intent(inout) :: hslow
      integer,intent(inout) :: nbin(:)
      real,intent(inout)    :: obin(:)
      real,intent(inout)    :: dbin(:)
      integer,intent(inout) :: tcount

      integer      :: i_err
      integer      :: itr,i
      integer      :: ndpt
      integer      :: nhdwd
      integer      :: ntrfil
      real         :: nil
      integer      :: tnum,tnum_old
      real,pointer :: tr(:)
      integer          :: count
      double precision :: diff
      double precision :: hmin(100)
      double precision :: hmax(100)
      double precision :: hold(100)
      double precision :: hbin(100)
      double precision :: cpshd(64)
      character(len=80)  :: msg
     !integer          :: gcount,gcount_old
     !integer          :: ngrp

      status= -1
      nil   = 999.2
      hfast = xhdr
      hslow = yhdr
      ndpt    = trcio_get_num_values(file)
      allocate (tr(ndpt), stat=i_err)
      if(i_err /=0) then
        write(stdo,*) 'modgrid_rddesc_trcio_scan: allocate error:'
        return
      endif


      ntrfil = modgrid_get_trace_count(file)
     !print *,'trcio_scan: ntrfil=',ntrfil
      nhdwd  = file%nwih
     !print *,'trcio_scan: nhdwd=',nhdwd
     !print *,'trcio_scan: ndpt=',ndpt
     !print *,'trcio_scan: xhdr=',xhdr
     !print *,'trcio_scan: yhdr=',yhdr
      count = 0
     !gcount = 0
     !gcount_old = 0
     !ngrp = 0
      tnum  = 0
      tnum_old = -1
      do itr=1,min(nscan,ntrfil)
        tnum = tnum+1
        if(tnum == nscan/2 .and. nscan < ntrfil) then
          tnum = ntrfil - nscan/2 !skip traces and read to file end
        endif
        if(tnum /= tnum_old+1 ) then
          tnum_old = tnum
          i_err = trcio_read_trace(file,cpshd,tr,tnum)
        else
          i_err = trcio_read_trace(file,cpshd,tr)
        endif
        if(i_err /= TRCIO_OK) then
          msg = 'modgrid _trcio_scan: error reading data'
          write(msg,'("modgrid _trcio_scan: error reading record ",i6)') tnum
          if(itr > 1 ) then
           status = 0
           ntrfil = tnum-1
           tcount = ntrfil
          endif
          goto 99
        endif

        if(count==0) then
          hmin(1:nhdwd) = cpshd(1:nhdwd)
          hmax(1:nhdwd) = cpshd(1:nhdwd)
          hold(1:nhdwd) = cpshd(1:nhdwd)
          hbin(1:nhdwd) = nil
          count = 1
         !gcount = 1
         !gcount_old = 1
        else
          do i=1,nhdwd
            hmin(i) = min(cpshd(i),hmin(i))
            hmax(i) = max(cpshd(i),hmax(i))
          enddo
          count = count + 1
        endif
        !determine which is fast and which is slow header
        if(count==2) then
            if(hold(xhdr)==cpshd(xhdr) .and. &
               hold(yhdr)/=cpshd(yhdr)) then
              hfast = yhdr
              hslow = xhdr
            endif
        endif
        !keep track of groups
       !if(count>=2 .and. count < nscan/2) then
       !    if(hold(hslow)==cpshd(hslow) ) then
       !      gcount = gcount + 1
       !    else
       !      ngrp = ngrp + 1
       !      if(ngrp > 1) then
       !        print *,'DBG: gcount=',gcount,' gcount_old=',gcount_old 
       !        if(gcount /= gcount_old) then
       !          print *,'DBG: irregular groups gcount ne. gcount_old'
       !          print *,'DBG: irregular groups cpshd(hslow)=',cpshd(hslow)
       !        endif
       !      endif
       !      gcount_old = gcount
       !      gcount = 1
       !    endif
       !endif
        do i=1,nhdwd
          if(cpshd(i) /= hold(i) ) then
            diff = abs(cpshd(i) - hold(i) )
            if(hbin(i)==nil) then
              hbin(i) = diff
            else
              hbin(i) = min(hbin(i), diff)
            endif
            if(hbin(i) <= 0.002) hbin(i)=nil
          endif
          hold(i) = cpshd(i)
        enddo
      enddo
      status = 0
 99   continue
      ! save the ndpt and dt
      nbin(1) = trcio_get_num_values(file)
      obin(1) = trcio_get_tmin(file)
      dbin(1) = trcio_get_dt(file)
      if(dbin(1) < 0) dbin(1) = 1.0


      dbin(2) = hbin(hfast)
      obin(2) = hmin(hfast)
      nbin(2) = 1 + (hmax(hfast) - obin(2) + 0.01*dbin(2))/dbin(2);
      if(nbin(2)<1) nbin(2) = 1;

      nbin(3) = 1 + (ntrfil-1)/nbin(2);
      obin(3) = hmin(hslow)
      dbin(3) = hbin(hslow)

     !print *,'trcio_scan: count=',count
     !print *,'trcio_scan: n2=',nbin(2)
     !print *,'trcio_scan: o2=',obin(2)
     !print *,'trcio_scan: d2=',dbin(2)
     !print *,'trcio_scan: n3=',nbin(3)
     !print *,'trcio_scan: o3=',obin(3)
     !print *,'trcio_scan: d3=',dbin(3)
      deallocate(tr)
      if(status /=0) then
        write(stdo,*) trim(msg)
      endif
      return
      end function modgrid_rddesc_trcio_scan

      logical function modgrid_has_zero_values(obj, i_err) result(status)
      type(modgrid_struct),intent(inout)  :: obj       ! arguments
      integer,intent(inout)  :: i_err       ! arguments
      status = modgmem_has_zero_values(obj%memdata, i_err)
      return
      end function modgrid_has_zero_values

     integer function modgrid_get_trace_count(trcio_obj) result(ncount)
     type ( trcio_struct ),   pointer :: trcio_obj        ! trcio structure
     integer :: ccount
     integer :: recl
     integer :: i,stat,lun
     integer :: bsiz

     integer :: wblki,wbyti
     integer :: wblk,wbyt
     integer :: start_pos(2)
     double precision :: rsize

      start_pos = trcio_get_data_start_pos(trcio_obj)
      recl   = trcio_get_recl(trcio_obj)

      lun = trcio_get_lun(trcio_obj)
      bsiz  = 1024
      stat  = cio_ftell(lun,bsiz,wblki,wbyti)   !save byte position
      i     = cio_fseek(lun, 0, 2)              !go to end of file
      stat  = cio_ftell(lun,bsiz,wblk,wbyt)     !get byte position
      rsize = bsiz
      rsize = rsize*wblk
      rsize = rsize + wbyt

      rsize = rsize - start_pos(2)
      rsize = rsize/ trcio_get_recl(trcio_obj)
      ccount = rsize
      !reposition to original spot
      stat  =   cio_fseek(lun, bsiz ,wblki, wbyti, 0)
      ncount=ccount
     return
     end function modgrid_get_trace_count

      subroutine modgrid_get_pname(obj,pname)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(out)    :: pname
      pname = obj%pname
      return
      end subroutine modgrid_get_pname

      function modgrid_pname(obj) result(pname)
      type(modgrid_struct),intent(in) :: obj
      character(len=32)  :: pname
      pname = obj%pname
      return
      end function modgrid_pname

      subroutine modgrid_set_pname(obj,pname)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: pname
      obj%pname = pname
      if(associated(obj%memdata)) then
       !obj%memdata%pname = pname
        call modgmem_set_pname(obj%memdata,pname)
      endif
      return
      end subroutine modgrid_set_pname

      subroutine modgrid_get_punits(obj,punits)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(out)    :: punits
      punits = obj%punits
      return
      end subroutine modgrid_get_punits

      subroutine modgrid_set_punits(obj,punits)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: punits
      obj%punits = punits
      if(associated(obj%memdata)) then
       !obj%memdata%punits = punits
        call modgmem_set_punits(obj%memdata,punits)
      endif
      return
      end subroutine modgrid_set_punits

      subroutine modgrid_get_aunit(obj,axis,aunit)
      type(modgrid_struct),intent(in) :: obj
      integer,intent(in)              :: axis
      character(len=*),intent(inout)  :: aunit
      if(axis< 1 .or. axis > obj%rank) return
      aunit = obj%a_unit(axis)
      return
      end subroutine modgrid_get_aunit

      subroutine modgrid_set_aunit(obj,axis,aunit)
      type(modgrid_struct),intent(inout) :: obj
      integer,intent(in)                 :: axis
      character(len=*),intent(in)        :: aunit
      if(axis< 1 .or. axis > obj%rank) return
      obj%a_unit(axis) = aunit
      return
      end subroutine modgrid_set_aunit

      subroutine modgrid_get_name(obj,name)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(out)    :: name
      name = obj%name
      return
      end subroutine modgrid_get_name

      subroutine modgrid_set_name(obj,name)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: name
      obj%name = name
      return
      end subroutine modgrid_set_name

      !robust 2D average
      subroutine modgrid_avg2d(data,n1,n2, avg)
      integer,intent(in)  :: n1,n2
      real,intent(in)     :: data(n1,n2)
      real,intent(out)    :: avg
      integer :: i2
      real    :: avgx
       avg = 0.0
       do i2=1,n2
         avgx = sum(data(1:n1,i2))/n1
         avg = avg+avgx
       enddo
       avg = avg/n2
      return
      end subroutine modgrid_avg2d

      subroutine modgrid_avgvec(data,n1,n2, avg)
      integer,intent(in)  :: n1,n2
      real,intent(in)     :: data(n1,n2)
      real,intent(out)    :: avg(:)
      integer :: i1,i2

       avg(1:n1) = 0.0
       do i2=1,n2
         do i1=1,n1
           avg(i1) = avg(i1) + data(i1,i2)
         enddo
       enddo
       avg = avg/n2
      return
      end subroutine modgrid_avgvec

      !scales before clipping
      subroutine modgrid_clip_scale(data,n1,clip_min,clip_max,scale)
      integer,intent(in)  :: n1
      real,intent(inout)  :: data(n1)
      real,intent(in)     :: scale
      real,intent(in)     :: clip_min
      real,intent(in)     :: clip_max
      integer :: i1
      real    :: cmin, cmax
      cmin = clip_min
      cmax = clip_max
      if(cmin /= MOD_RNIL .and. cmax /= MOD_RNIL) then
        cmin = min(clip_min,clip_max)
        cmax = max(clip_min,clip_max)
        do i1=1,n1
          if(data(i1) /= MOD_RNIL) &
           data(i1) = min(cmax,max(cmin, scale* data(i1)))
        enddo
      else
        if(cmin /= MOD_RNIL) then
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = max(cmin,scale* data(i1))
          enddo
        else if(cmax /= MOD_RNIL) then
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = min(cmax,scale* data(i1))
          enddo
        else
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = scale* data(i1)
          enddo
        endif
      endif
      return
      end subroutine modgrid_clip_scale

    !find grid point zbnd for which D(z<zbnd) <= dbnd
      integer function modgrid_data_zbnd(fname,stdo,dbnd,zbnd,zval)&
      result(status)
      character(len=*),intent(in)     :: fname       ! arguments
      integer,intent(in)              :: stdo
      real,intent(in)                 :: dbnd
      integer,intent(out)             :: zbnd
      real,intent(out)                :: zval

      type(modgrid_struct),pointer :: obj
      integer      :: i,i_err

      integer      :: sslice,nslice
      character(len=120) :: dfile
      character(len=8)   :: wtype
      character(len=64)  :: name
      character(len=64)  :: pname
      character(len=16)  :: punits
      character(len=8)   :: ftype
      character(len=4)   :: xyz
      integer      :: rank
      integer      :: hdwd(4),ng(4)
      real         :: og(4),dg(4)
      integer      :: ix,iy,iz
      integer      :: nz,izs

      integer      :: n1,n2,n3

      real,pointer :: data(:)
      real,pointer :: zvec(:)

      nullify (obj) ! jpa
      nullify (data) ! jpa

      status = -1
      zbnd = 1
      i_err = modgrid_rddesc(obj,fname,stdo,dfile,wtype,ftype)
      if(i_err < 0) then
        if(stdo > 0) write(stdo,*) &
         'modgrid_data_zbnd: error reading globals ',trim(fname)
        return
      endif
      if(ftype=='UNKNOWN') then
        if(stdo>0) write(stdo,*) &
         'modgrid_data_zbnd: UNKNOWN file type,',trim(fname)
        return
      endif
      call modgrid_get_name_rank(obj,name,pname,punits,rank)
      do i = 1,rank
        call modgrid_get_griddesc(obj,i, hdwd(i),ng(i),og(i),dg(i))
      enddo
      if(rank >=3) then
        nslice = 32000000/(ng(1)*ng(2))
      else
        nslice = 1
      endif
      i_err = modgrid_xyz_order(obj,ix,iy,iz)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_data_zbnd: - failed to determine xyz order'
        call modgrid_delete(obj)
        status = -1
        return
      endif
      xyz=' '
      xyz(ix:ix)='X'
      xyz(iy:iy)='Y'
      xyz(iz:iz)='Z'

      if(iz==1) then
        nz = ng(1)
      endif
      if(iz==2) then
        nz = ng(2)
      endif
      if(iz==3) then
        nz = ng(3)
      endif
      allocate(zvec(nz),stat=i_err)
      if ( i_err .ne. 0 ) then
        if(stdo > 0) write(stdo,*) &
         'modgrid_data_zbnd: allocate error '
         return
      endif
      zbnd = nz

      sslice = 1
      !read in big chunks of input data
      do while (sslice <= ng(3))
        nslice = min(nslice,ng(3)-sslice+1)
        if(sslice > ng(3)) exit
        i_err = modgrid_rd_data_iclip(obj,stdo,sslice,nslice)
        if(i_err /= 0) then
           if(stdo>0) write(stdo,*) '#modgrid_data_zbnd: sslice=',sslice,&
           ' nslice=',nslice,' ERROR READING'
           sslice = sslice + nslice
           return
          !cycle
        endif
       !print *,'modgrid_data_zbnd: slices ', sslice,' to ',sslice+nslice-1
       !print *,' xyz=',xyz,' iz=',iz,' ix=',ix,' iy=',iy
        sslice = sslice + nslice
        if(.not. associated(obj%memdata)) return
        data => modgmem_data(obj%memdata)
       !if(.not. associated(obj%memdata%data)) return
        if(.not. associated(data)) return
       !data => obj%memdata%data
        if(iz==1) then
       !  nz = obj%memdata%dim1
          nz = modgmem_get_dim(obj%memdata,1)
       !  nxy= obj%memdata%dim2 * obj%memdata%dim3
       !  izs = obj%memdata%org1
          izs = modgmem_get_org(obj%memdata,1)
       !  ize = izs + nz - 1
       !  zstride=1
       !  xy_jmp=nz
        endif
        if(iz==2) then
          nz = modgmem_get_dim(obj%memdata,2)
       !  nz = obj%memdata%dim2
       !  nxy= obj%memdata%dim1 * obj%memdata%dim3
       !  izs = obj%memdata%org2
          izs = modgmem_get_org(obj%memdata,2)
       !  ize = izs + nz - 1
       !  zstride=obj%memdata%dim1
          print *,'modgrid_data_zbnd: error iz=2'
          goto 99
        endif
        if(iz==3) then
          nz = modgmem_get_dim(obj%memdata,3)
       !  nz = obj%memdata%dim3
       !  nxy= obj%memdata%dim1 * obj%memdata%dim2
       !  izs = obj%memdata%org3
          izs = modgmem_get_org(obj%memdata,3)
       !  ize = izs + nz - 1
       !  zstride=obj%memdata%dim1*obj%memdata%dim2
       !  xy_jmp=1
        endif

      ! n1 = obj%memdata%dim1
      ! n2 = obj%memdata%dim2
      ! n3 = obj%memdata%dim3
        n1 = modgmem_get_dim(obj%memdata,1)
        n2 = modgmem_get_dim(obj%memdata,2)
        n3 = modgmem_get_dim(obj%memdata,3)
        i_err = modgrid_data_zscan(n1,n2,n3,data,izs, xyz,dbnd, zbnd)

      enddo  !model chunk
      zval = og(iz) + (zbnd-1)*dg(iz)
     !print *,'modgrid_data_zbnd: dbnd =',dbnd,' zbnd=',zbnd,' zval=',zval
      status = 0
 99   call modgrid_delete(obj)
      deallocate(zvec)
      return
      end function modgrid_data_zbnd

      integer function modgrid_data_zscan(n1,n2,n3,data,izs, xyz,dbnd, zbnd) &
      result(status)
      real,intent(in)        :: data(*)    !cube to scan
      integer,intent(in)     :: n1
      integer,intent(in)     :: n2
      integer,intent(in)     :: n3
      integer,intent(in)     :: izs        !z grid origin of data
      real,intent(in)        :: dbnd
      integer,intent(inout)  :: zbnd
      character(len=*),intent(in) :: xyz        !permutation of "XYZ"
      integer      ::  nxy
      integer      ::  zstride
      integer      ::  x_inc,y_inc,xy_inc
      integer      ::  i2,i,ize
      integer      ::  nx,ny,nz
      integer      ::  ix,iy
      real         ::  zvec(3000)
      status = -1
      if(xyz(1:1)=='Z') then
        nxy    = n2*n3
        zstride= 1
        xy_inc = n1
        nz     = n1
        if(xyz(2:2)=='X') then
          nx = n2
          ny = n3
          x_inc  = n1
          y_inc  = n1*n2
        else
          nx = n3
          ny = n2
          y_inc  = n1
          x_inc  = n1*n2
        endif
      endif
      if(xyz(2:2)=='Z') then
        nxy    = n1*n3
        zstride= n1
        xy_inc = 1
        nz     = n2
        if(xyz(1:1)=='X') then
          nx = n1
          ny = n3
          x_inc  = 1
          y_inc  = n1*n2
        else
          nx = n3
          ny = n1
          y_inc  = 1
          x_inc  = n1*n2
        endif
      endif
      if(xyz(3:3)=='Z') then
        nxy    = n1*n2
        zstride= nxy
        xy_inc = 1
        nz     = n3
        if(xyz(1:1)=='X') then
          nx = n1
          ny = n2
          x_inc  = 1
          y_inc  = n1
        else
          nx = n2
          ny = n1
          y_inc  = 1
          x_inc  = n1
        endif
      endif
      !izs is the grid index of what is in memory
      ize = izs + nz-1
     !print *,'DBG: n1=',n1,' n2=',n2,' n3=',n3,' izs=',izs,' nz=',nz
     !print *,'DBG: nxy=',nxy,' nx-',nx,' ny=',ny
     !print *,'DBG: x_inc=',x_inc,' y_inc=',y_inc
     !print *,'DBG: xy_inc=',xy_inc,' zstride=',zstride
      do iy = 1,ny             !loop over x,y positions
        do ix = 1,nx             !loop over x,y positions
          i2 = 1 + (ix-1)*x_inc + (iy-1)*y_inc

     !do ixy = 1,nxy             !loop over x,y positions
     !    i2 = (ixy-1)*xy_inc + 1
          do i = 1, nz            !get zvec for x,y
             zvec(i) = data(i2)
             i2 = i2 + zstride
          enddo
          do i = izs,ize          !assumes z increases with iz
            if(zvec(i-izs+1) > dbnd) then
              if(zbnd > i) then
                zbnd=i
                goto 10
              endif
            endif
          enddo
 10   continue
      enddo !end x loop
      enddo !end y loop
      status = 0
      return
      end function modgrid_data_zscan

! write out the grid as CPS velocity functions
! write from ss(i) to ss+ns(i)-1 for 1=1:3
      integer function modgrid_wr_cvf&
      (obj,fname,stdo,ss,ns,vtypeo) result(status)
      type(modgrid_struct),intent(inout) :: obj    !arguments
      character(len=*),intent(in)     :: fname     !output file name
      integer,intent(in)              :: stdo
      integer,intent(in)              :: ss(:)
      integer,intent(in)              :: ns(:)
      character(len=*),intent(in)     :: vtypeo

      type(velio_struct),pointer      :: velio_obj !local variables
      integer     :: i_err
      integer     :: hdwd(4),hx,hy
      integer     :: ix,iy,i

      character   :: msg*96

      integer     :: nx_bins,ny_bins
      real        :: xtmp,ytmp
      integer     :: n_inp,m_inp
      integer     :: n(4)
      real        :: o(4),d(4)
      integer     :: u,v,w,stride
      integer     :: og(3),ng(3)
      integer     :: nfun
      integer     :: ixlab,iylab,izlab
      real,pointer    :: t_inp(:),v_inp(:)
      character(len=8)  :: v_type

      real,pointer    :: dpntr(:)

      nullify (velio_obj) ! jpa

      ! initializations
      status = -1
      nullify(t_inp)
      nullify(v_inp)
      nullify(dpntr)
      v_type = vtypeo
!
! QC the choice of slice data to generate
      if(.not. modgrid_all_in_mem(obj) ) then
        write(stdo,*) 'modgrid_wr_cvf: error, entire model is not in memory'
        return
      endif
      og = ss
      ng = ns
      do i = 1,3
        og(i) = min(max(1,og(i)),obj%n_grid(i))
        ng(i) = min(max(1,ng(i)),obj%n_grid(i)-og(i)+1)
      enddo
      do i = 1,obj%rank
        call modgrid_get_griddesc(obj,i, hdwd(i),n(i),o(i),d(i))
      enddo
      i_err = modgrid_xyz_order(obj,ixlab,iylab,izlab)
      if(i_err /=0) then
        write(stdo,*) 'modgrid_wr_cvf: modgrid_xyz_order error'
        call modgrid_print(obj,stdo)
        return
      endif
      hx = obj%hdwd(ixlab)
      hy = obj%hdwd(iylab)
      m_inp = max(2,ng(izlab))
      nfun = ng(ixlab)*ng(iylab)
!
! - open the velocity file
      call velio_open_write (obj = velio_obj,&
        filename = fname,    &
        nfun     = nfun,&
        err      = i_err,    &
        msg      = msg,      &
        nhx      = hx,  &
        nhy      = hy)
      if(i_err /= 0) then
        write(stdo,*) 'modgrid_wr_cvf: velio_open_write err=',i_err
        write(stdo,*) 'modgrid_wr_cvf: open_write msg=',trim(msg)
        return
      endif

      nx_bins=ng(ixlab)
      ny_bins=ng(iylab)
      !
      allocate(t_inp(m_inp+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !
      allocate(v_inp(m_inp+1), stat=i_err)
      if ( i_err .ne. 0 ) goto 99
      !

! get pointer to the data buffer
      i_err =  modgrid_get_data(obj,dpntr)
      if(i_err /= 0) then
        msg = 'modgrid_wr_cvf: failed to get dpntr'
        goto 99
      endif
!
! write all the x,y locations to cps velocity functions
! - read the functions into t_inp, v_inp

      n_inp = n(izlab)
      if(izlab==1) then
        stride=1
        u=1
      endif
      if(izlab==3) then
        stride=n(ixlab)*n(iylab)
        u=ix + iy
      endif
      do i = 1,ng(izlab)
        t_inp(i)  = o(izlab) + (i-1)*d(izlab)
      enddo
      do iy = og(iylab), og(iylab) + ng(iylab)-1
        ytmp = o(iylab) + (iy-1)*d(iylab)
        do ix = og(ixlab), og(ixlab) + ng(ixlab)-1
          xtmp = o(ixlab) + (ix-1)*d(ixlab)

          if(izlab==1) then
            if(ixlab==2 .and. iylab==3) then
              u=1
              v = ix
              w = iy
            endif
            if(ixlab==3 .and. iylab==2) then
              u=1
              v = iy
              w = ix
            endif
          else
            if(ixlab==1 .and. iylab==2) then
              u = ix
              v = iy
              w = 1
            endif
            if(ixlab==2 .and. iylab==1) then
              u = iy
              v = ix
              w = 1
            endif
          endif
         !do i = 1,ng(izlab)
         !   v_inp(i)  = data(i + (ixy-1)*n(izlab) )
         !enddo
          i_err = modgmem_get_pointsr(obj%memdata,v_inp,&
                  ng(izlab),izlab,stride, u,v,w)
          call velio_write_velfun (obj     = velio_obj,    &
             xcoord  = xtmp,       &
             ycoord  = ytmp,       &
             npicks  = n_inp,      &
             tpicks  = t_inp,      &
             vpicks  = v_inp,      &
             err     = i_err,      &
             msg     = msg, &
             veltype = v_type)
          if(i_err /= 0) then
            write(stdo,*) 'modgrid_wr_cvf: write_velfun err=',i_err
            write(stdo,*) 'modgrid_wr_cvf: write_velfun msg=',trim(msg)
            goto 99
          end if
        enddo
      enddo

      status = 0
 99   continue
      if (associated(velio_obj)) call velio_close (velio_obj)
      if (associated(t_inp)) deallocate (t_inp)
      if (associated(v_inp)) deallocate (v_inp)
      if(status /=0) then
        write(stdo,*) 'modgrid_wr_cvf: msg=',trim(msg)
      endif
      return
      end function modgrid_wr_cvf

      subroutine modgrid_get_buffer(obj,cbuff)
      type(modgrid_struct),intent(in) :: obj    !arguments
      character(len=*)                :: cbuff
      cbuff = obj%cbuff
      return
      end subroutine modgrid_get_buffer
      !
     ! Given vx,eta,vz calculate Thomsen epsilon and delta
     ! Note: in the isotropic limit vx = vz , eta = epsilon = delta = 0
     subroutine modgrid_toepsdel(n,vx,eta,vz,epsilon,delta)
     integer,intent(in)  :: n
     real,intent(in)     :: vx(n)      !vx=vz for isotropy
     real,intent(in)     :: eta(n)     !eta=0 for isotropy
     real,intent(in)     :: vz(n)      !vx=vz for isotropy
     real,intent(out)    :: epsilon(n)
     real,intent(out)    :: delta(n)
     real        :: vxovz
     integer     :: i
     epsilon(1:n) = 0.0
     delta(1:n)   = 0.0
     do i = 1,n
        if(vz(i) /= 0.0) then
          vxovz = vx(i)/vz(i)
         !eps(i)= (vx(i)*vx(i)-vz(i)*vz(i))/(2*vz(i)*vz(i))
          epsilon(i)= (vxovz-1.0)*(vxovz+1.0)*0.5
        endif
       !delta(i) = (vx(i) - (1+eta(i))*vz(i))/(1 + (eta(i)+eta(i))*vz(i))
        delta(i) = (epsilon(i) - eta(i))/(1.0 + 2.0*eta(i))
     enddo
     return
     end subroutine modgrid_toepsdel

     ! Given vz,epsilon,delta calculate eta,and vx
     ! Note: in the isotropic limit vx = vz , eta = epsilon = delta = 0
     subroutine modgrid_tovxeta(n,vx,eta,vz,epsilon,delta)
     integer,intent(in)  :: n
     real,intent(out)     :: vx(n)      !vx=vz for isotropy
     real,intent(out)     :: eta(n)     !eta=0 for isotropy
     real,intent(in)     :: vz(n)      !vx=vz for isotropy
     real,intent(in)    :: epsilon(n)
     real,intent(in)    :: delta(n)
     integer     :: i
     vx(1:n) = 0.0
     eta(1:n)= 0.0
     do i = 1,n
        eta(i) = (epsilon(i)-delta(i))/(1.0+2*delta(i))
        vx(i)  = vz(i)*sqrt(1+2.0*epsilon(i))
     enddo
     return
     end subroutine modgrid_tovxeta

     subroutine modgrid_sinthetasq(cr2, delta, epsilon, sinthetasq)
     real,intent(in) ::  cr2        ! (K*V/w)^2
     real,intent(in) ::  delta      !thomsen delta
     real,intent(in) ::  epsilon    !thomsen epsilon
     real,intent(out)::  sinthetasq
     !
     ! local variables
     real     :: temp,temp1

     temp = 1.0-2.0*cr2*delta

     if (temp==0.0) then
       sinthetasq = 0.0
       print *,'temp = 0.0, warning'
       return
     endif

     ! temp1 = temp - 4.0*cr2*cr2*(2.0*(epsilon-delta)+delta*delta)  a bug?
     temp1 = temp*temp - 4.0*cr2*cr2*(2.0*(epsilon-delta)+delta*delta)

     if (temp1<0.0) then
       sinthetasq = cr2/temp
       return
     endif

     sinthetasq = 2.0*cr2/(temp + sqrt(temp1))
     return
     end subroutine modgrid_sinthetasq

     ! compute the effect of anisotropy on the slowness
     ! Weak VTI anisotropy limit
     ! in the isotropic limit epsilon = delta = 0 ==> slotho=slothi
     subroutine modgrid_aniso_sloth(cr2,delta, epsilon, slothi, slotho)
     real,intent(in)     :: cr2     !(Kt/(s*w))**2 , Kt = transverse wavenumber
     real,intent(in)     :: epsilon
     real,intent(in)     :: delta
     real,intent(in)     :: slothi   !isotropic slowness
     real,intent(out)    :: slotho   !anisotropic slowness
     real     :: sinthetasq
     !compute an effective angle and then compute an effective slowness
     call modgrid_sinthetasq(cr2, delta, epsilon, sinthetasq)
     slotho = slothi/(1.0 + delta*sinthetasq*(1.0-sinthetasq) + &
              epsilon*sinthetasq*sinthetasq)
     return
     end subroutine modgrid_aniso_sloth

      integer function modgrid_rddesc_su(obj,fname,stdo,dfile,wtype,dfsize,&
        wrdsz,xhdr,yhdr) result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      double precision,intent(in)     :: dfsize
      integer,intent(out)             :: wrdsz
      integer,intent(in)              :: xhdr
      integer,intent(in)              :: yhdr

      integer    :: i_err
      integer    :: ntord
      integer    :: ndidrd
      integer    :: ibuff(900)
      integer    :: bytes_per_trace
      integer    :: ntraces
      integer    :: nscan
      real       :: rtraces
      character(len=4) :: mode
      character(len=8) :: ftype

      type(segy_trc_hdr)  :: syh
     !type(segy_trc_hdr)  :: syh_tmp
      integer    :: endian
      logical    :: swap

      integer    :: lun
      integer    :: hfast,hslow
      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3)
      real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character  :: label(3)*32
      character  :: name*64,pname*32,punits*16

      status = -1
      nullify(obj)
      dfile= ' '
      wtype=' '
      pname='su_data'
      wrdsz = 4
      n = 1
      o = 0.0
      d = 1.0
      hdwd = -1
      label= ' '
      oxyz = 0.0
      axis1= 0.0
      axis2= 0.0
      axis3= 0.0
      name = 'su_file'
      punits= 'UNKNOWN'
      if(dfsize <=0) then
         write(stdo,*) 'modgrid_rddesc_su: error in su dfsize=',dfsize
         write(stdo,*) 'modgrid_rddesc_su: error fname=',fname
         return
      endif
      ftype= 'SU'
      dfile= fname
      wtype= 'IBM'
      mode = 'r'
      lun = cio_fopen(fname,mode)
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_segy: open error:',trim(fname)
        return
      endif
      ! read in 240 bytes of file header info
      ntord = 60
      ndidrd= cio_fread(ibuff,4,ntord,lun)
      if(ndidrd < ntord) then
        write(stdo,*) 'modgrid_rddesc_su: ndidrd=',ndidrd,' < ntord=',ntord
        i_err = cio_fclose(lun)
        return
      endif
      i_err = cio_fclose(lun)

      swap = .false.
      endian = swap_endian()
      if(endian ==0) swap= .true. !file big endian, but mem is little
      call segy_unpack_segyhd(syh,ibuff(1:60),swap)
      if(syh%dt <= 0) then
        print *,'modgrid_rddesc_su: swap=',swap
        print *,'modgrid_rddesc_su: DBG1 dt,ns=',syh%dt,syh%ns
        call segy_unpack_segyhd(syh,ibuff(1:60),.not.swap)
        print *,'modgrid_rddesc_su: swap=',swap
        print *,'modgrid_rddesc_su: DBG2 dt, ns=',syh%dt,syh%ns
        if(syh%dt <= 0) then
          return
        endif
      endif

      bytes_per_trace = 240 + 4*syh%ns

      wrdsz = 4
      bytes_per_trace = 240 + wrdsz*syh%ns
      rtraces = (dfsize)/bytes_per_trace
      ntraces = nint(rtraces)
      label(1) = 'DEPTH'

      ! save the segy ndpt and dt
      n(1) = syh%ns
      n(1) = max(1,n(1))
      o(1) = 0.0
      d(1) = syh%dt*.000001
      
      if(d(1) < 0) d(1) = 1.0
      nscan = min(10000,ntraces)
      status =  modgrid_rddesc_su_scan(stdo,dfile,dfsize,&
       ntraces, nscan,bytes_per_trace, 64,xhdr,yhdr,hfast,hslow,&
       n(1),d(1),n,o,d )
      if(status <0) then
        write(stdo,*) 'modgrid_rddesc_segy: scan error'
        return
      endif
      axis1(1) = 0.0
      axis1(2) = 0.0
      axis1(3) = (n(1)-1)*d(1)
      axis2(1) = (n(2)-1)*d(2)
      axis2(2) = 0.0
      axis2(3) = 0.0
      axis3(1) = 0.0
      axis3(2) = (n(2)-1)*d(2)
      axis3(3) = 0.0
      n(2)    = min(n(2),ntraces)
      oxyz(1) = o(2)
      oxyz(2) = o(3)
      oxyz(3) = o(1)

      call modgrid_create (obj, 3, name, pname, punits, &
        n, o, d, stdo)
      hdwd(2) = hfast
      hdwd(3) = hslow
      label(2) = modgrid_header_to_string(hfast)
      label(3) = modgrid_header_to_string(hslow)
      call modgrid_set_hdwd(obj,hdwd)
      call modgrid_set_xyz(obj,label, oxyz, axis1,axis2,axis3)
      status = 0
      return
      end function modgrid_rddesc_su
!
      integer function modgrid_rddesc_su_scan(stdo,dfile,dfsize,&
       ntrfil, nscan,bypertr, nhdwd,xhdr,yhdr,hfast,hslow,&
       ndpt,dt,nbin,obin,dbin ) result(status)
      integer,intent(in)    :: stdo
      character(len=*),intent(in) :: dfile
      double precision,intent(in) :: dfsize
      integer,intent(in)    :: ntrfil
      integer,intent(in)    :: nscan
      integer,intent(in)    :: bypertr
      integer,intent(in)    :: nhdwd
      integer,intent(in)    :: xhdr
      integer,intent(in)    :: yhdr
      integer,intent(inout) :: hfast
      integer,intent(inout) :: hslow
      integer,intent(in)    :: ndpt
      real   ,intent(in)    :: dt
      integer,intent(inout) :: nbin(:)
      real,intent(inout)    :: obin(:)
      real,intent(inout)    :: dbin(:)
      logical               :: swap
      integer               :: endian

      integer      :: lun
      integer      :: i_err
      integer      :: itr,i
      integer      :: nwrds
      integer      :: ndidrd

      real         :: nil
      integer      :: tnum
      integer,pointer  :: ibuff(:)
      double precision :: tstrt
      integer          :: count
      integer          :: wblk,wbyt
      double precision :: diff
      double precision :: hmin(100)
      double precision :: hmax(100)
      double precision :: hold(100)
      double precision :: hbin(100)
      double precision :: cpshd(nhdwd)
      type (segy_trc_hdr)   :: syh






      endian = swap_endian()
      status= -1
      nil   = 999.2
      tstrt = 0.0
      nwrds = 1 + bypertr/4
      hfast = xhdr
      hslow = yhdr
      allocate (ibuff(nwrds), stat=i_err)
      if(i_err /=0) then
        write(stdo,*) 'modgrid_rddesc_su_scan: allocate error:',trim(dfile)
        return
      endif

      lun = cio_fopen(dfile,'r')
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_su_scan: open error:',trim(dfile)
        return
      endif

      wbyt  = 0    !su files are missing the 3600 byte header
      i_err = cio_fseek(lun, wbyt, 0)    !go to 1st trace
      count = 0
      tnum  = 0
      swap = .false.
      if(endian ==0) swap= .true. !file big endian, but mem is little
      do itr=1,min(nscan,ntrfil)
        tnum = tnum+1
        if(tnum == nscan/2 .and. nscan < ntrfil) then
          tnum = ntrfil - nscan/2 !skip traces and read to file end
         !i_err  = cio_fseek(lun, wbyt + (tnum-1)*bypertr, 0)
          wblk = (tnum-1)
          i_err  = cio_fseek(lun,bypertr,wblk,wbyt,0)
        endif
        ndidrd = cio_fread      (ibuff, 1, bypertr,  lun)
        if(ndidrd <=0) exit
        call segy_unpack_segyhd(syh,ibuff(1:60),swap)
        call segy_segyhd_to_cpshd(cpshd,syh,tstrt)

        if(count==0) then
          hmin(1:nhdwd) = cpshd(1:nhdwd)
          hmax(1:nhdwd) = cpshd(1:nhdwd)
          hold(1:nhdwd) = cpshd(1:nhdwd)
          hbin(1:nhdwd) = nil
          count = 1
        else
          do i=1,nhdwd
            hmin(i) = min(cpshd(i),hmin(i))
            hmax(i) = max(cpshd(i),hmax(i))
          enddo
          count = count + 1
        endif
        do i=1,nhdwd
          if(cpshd(i) /= hold(i) ) then
            diff = abs(cpshd(i) - hold(i) )
            hbin(i) = min(hbin(i), diff)
            if(hbin(i) <= 0.002) hbin(i)=nil
          endif
          if(count==2) then
            if(hold(xhdr)==cpshd(xhdr) .and. &
               hold(yhdr)/=cpshd(yhdr)) then
              hfast = yhdr
              hslow = xhdr
            endif
          endif
          hold(i) = cpshd(i)
        enddo
      enddo
      ! save the segy ndpt and dt
      nbin(1) = ndpt
      obin(1) = 0.0
      dbin(1) = dt
      if(dbin(1) < 0) dbin(1) = 1.0

      dbin(2) = hbin(hfast)
      obin(2) = hmin(hfast)
      nbin(2) = 1 + (hmax(hfast) - obin(2) + 0.01*dbin(2))/dbin(2);
      if(nbin(2)<1) nbin(2) = 1;

      nbin(3) = 1 + (ntrfil-1)/nbin(2);
      obin(3) = hmin(hslow)
      dbin(3) = hbin(hslow)

      i_err = cio_fclose(lun)
      deallocate(ibuff)
      status = 0
      return
      end function modgrid_rddesc_su_scan

      integer function modgrid_rd_su(obj,fname,stdo,ss,ns)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: fname
      integer,intent(in)                 :: stdo
      integer,intent(in)                 :: ss(:)
      integer,intent(in)                 :: ns(:)
      ! a su file is handled by trcio
      status = -1
      status = modgrid_rd_trcio(obj,fname,obj%stdo,ss,ns)
      return
      end function modgrid_rd_su

      !Use a modspec template to set the modgrid coordinates
      !set the modgrid's modspec pointer
      integer function modgrid_set_modspec(obj,mspdata,stdo) &
        result(status)
      type(modgrid_struct),intent(inout)    :: obj
      type(modspec_struct),pointer    :: mspdata
      integer,intent(in)              :: stdo



      integer    :: n(3),hdwd(3)
      real       :: o(3),d(3)
      real       :: alim(3)
      real       :: oxyz(3),axis(3,3)
      character(len=32)  :: coord(3),pname


      character(len=64)  :: name
      character(len=16)  :: punits
      character(len=4)   :: units
      integer    :: i

      double precision :: cpsxo
      double precision :: cpsyo
      double precision :: xorg, xorgs
      double precision :: yorg, yorgs
      double precision :: dangle,ddx,ddy
      integer          :: hx,hy,hz
      integer          :: nx
      integer          :: ny
      integer          :: nz
      real             :: dx
      real             :: dy
      real             :: dz
      real             :: oz
      real             :: ox,oy
      real             :: ogx,ogy
      real             :: angle
      integer          :: nlay
      real             :: rad
      real             :: zmin,zmax
      real             :: acoef(3)
      real             :: bcoef(3)
      logical          :: has_coef
      character(len=4) :: xyz
      integer          :: ix,iy,iz

      status = -1

!  modspec is similar to an rmod g3dl file
!  Get the world coordinate description
!  returned values are a cps viewpoint (may transpose modspec x & y )
      call modspec_getdesc(mspdata,nlay,angle,units,&
      nx,xorgs,dx,ny,yorgs,dy,nz,oz,dz)
      dangle = angle
      xorg = xorgs
      yorg = yorgs
      !convert negative modspec angles to positive CPS angle?
      if(dangle < 0.0) dangle = 360.0 - dangle 
      ddx = dx   !save physical bin width
      ddy = dy   !save physical bin width

      !
      ! See if line trace coordinates are defined for the modspec
      ! use min line min trace for origin and line-trace bin sizes
      has_coef = modspec_get_coef_full(mspdata,acoef,bcoef,ox,oy,dx,dy,&
      ogx,ogy, hx,hy,xyz)
      if(has_coef) then ! a,b coefficients are defined
        xorg = ox
        yorg = oy
       !removed abs and use  ox,oy  rather than ogx,ogy 12-03-05
       if(dx .ne. 0) ddx = ddx/abs(dx)
       if(dy .ne. 0) ddy = ddy/abs(dy)
      else
        write(stdo,*) 'modgrid_set_modspec: get_coef problem status=',status
      endif

      rad = dangle/180.* pi
      cpsxo = xorgs - ogx*ddx*cos(rad) + ogy*ddy*sin(rad)
      cpsyo = yorgs - ogx*ddx*sin(rad) - ogy*ddy*cos(rad)
     !call grid_set_transform(gobj,cpsxo,cpsyo, dangle, ddx, ddy, 1)
      ! preserve the transform information
     !obj%gobj = gobj

      !
      !     get other modspec settings
      call modspec_get_hdrs(mspdata,hx,hy,hz) !x,y and z
      iz = 1  !always true for modspec?
      hdwd(iz)= hz
      if(xyz(2:2)=='X')  ix = 2
      if(xyz(2:2)=='Y')  iy = 2
      if(xyz(3:3)=='X')  ix = 3
      if(xyz(3:3)=='Y')  iy = 3
      hdwd(ix)= hx
      hdwd(iy)= hy
      call modspec_get_zlimits(mspdata,zmin,zmax)
      if(zmax /= zmin) then
         if(zmax-zmin < dz) dz =  (zmax-zmin)/max(1,nlay-1)
      endif

!
! Set the modgrid settings consistent with the modspec template
      n    = 1
      o    = 0.0
      d    = 1.0
      coord= ' '
      oxyz = 0.0
      n(iz) = nlay
      if(nz > nlay) n(iz) = nz
      o(iz) = 0.0
      d(iz) = dz
      coord(1) = modgrid_header_to_string(hdwd(1))   !depth
      coord(2) = modgrid_header_to_string(hdwd(2))
      coord(3) = modgrid_header_to_string(hdwd(3))
      name = 'ModspecModel'
      pname= 'VZIN'
      punits= units

      n(ix) = nx
      n(iy) = ny
      if(hdwd(ix)==7) then
        o(ix) = xorg
        o(iy) = yorg
        d(ix) = dx
        d(iy) = dy
      else
        o(ix) = xorgs
        o(iy) = yorgs
        d(ix) = dx*ddx
        d(iy) = dy*ddy
      endif

        ! delete old mspdata and replace with new
        call modspec_delete(obj%mspdata)
      ! save pointer to the mospec object
      obj%mspdata=>mspdata

      ! determine and set header mapping
      hdwd= -1
      do i = 1,3
        hdwd(i) = modgrid_string_to_header(coord(i))
      enddo
      call modgrid_set_hdwd(obj,hdwd)

      !
      ! set default orientation & size to align with XYZ axis
      axis =0.0

      alim(1)=(n(1)-1)* d(1)
      if(alim(1)==0) alim(1) = d(1)
      alim(2)=(n(2)-1)* d(2)
      if(alim(2)==0) alim(2) = d(2)
      alim(3)=(n(3)-1)* d(3)
      if(alim(3)==0) alim(3) = d(3)

      axis(iz,3) = (nz-1)*dz
      if(has_coef) then
        if(ix==2) then
          axis(ix,1) = alim(2)
          axis(iy,2) = alim(3)
        else
          axis(iy,2) = alim(2)
          axis(ix,1) = alim(3)
        endif
      else
        rad = angle/180.* pi
        axis(ix,1) = alim(2)*cos(rad)
        axis(ix,2) = alim(2)*sin(rad)
        axis(iy,1) = -alim(3)*sin(rad)
        axis(iy,2) = alim(3)*cos(rad)
      endif
      oxyz(1)  = xorg
      oxyz(2)  = yorg
      oxyz(3)  = oz
      call modgrid_set_griddesc(obj,1, hdwd(1),n(1),o(1),d(1))
      call modgrid_set_griddesc(obj,2, hdwd(2),n(2),o(2),d(2))
      call modgrid_set_griddesc(obj,3, hdwd(3),n(3),o(3),d(3))
      call modgrid_set_xyz(obj,coord, oxyz, axis(1,:),axis(2,:),axis(3,:))

      status = 0

      return
      end function modgrid_set_modspec

      integer function modgrid_get_modspec(obj,mspdata) &
        result(status)
      type(modgrid_struct),intent(in) :: obj
      type(modspec_struct),pointer    :: mspdata
      status = -1
      status = 0
      mspdata => obj%mspdata
      return
      end function modgrid_get_modspec

! Scan the SITI HDR file
! grid size, origin, increment, label, unit information
      integer function modgrid_rddesc_siti(obj,fname,stdo,dfile,wtype) &
      result(status)
      type(modgrid_struct),pointer    :: obj
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile
      character(len=*),intent(inout)  :: wtype
      integer    :: lun,nr,nc,i_err
      integer    :: n(3),hdwd(3)
      real       :: o(3)               !grid origins
      real       :: d(3)               !grid deltas
      double precision       :: po(3)  !physical origins
      double precision       :: pd(3)  !physical deltas
      double precision       :: dangle = 0.0
      real       :: oxyz(3),axis1(3),axis2(3),axis3(3)
      character(len=120) :: card,cards(100)
      character(len=8)   :: ftype
      character(len=16)  :: coord(3)   !axis label
      character(len=8)   :: aunit(3)   !axis unit

      character(len=64)  :: name
      character(len=32)  :: pname
      character(len=16)  :: punits
      integer    :: i,j
      integer    :: xlab,ylab,zlab
      integer    :: rank
      character  :: mode*4

      logical    :: found
      type(grid_struct)  :: gobj
      status = -1
      nullify(obj)
      dfile= ' '
      wtype=' '

      ftype = modgrid_ftype(fname,stdo)
      if(ftype /= 'SITI') then
       write(stdo,*) 'modgrid_rddesc_siti: not a SITI ',trim(fname)
       return
      endif
      i = index(fname,'HDR')
      if(i>0) dfile = fname(1:i-1)//'TRC'

      mode = 'r'
      lun = cio_fopen(fname,'r')
      if(lun <= 0) then
        write(stdo,*) 'modgrid_rddesc_siti:(open error) ',trim(fname)
        return
      endif
      nr=1
      nc=0
      card = ' '
      found = .false.

      do while (nr >= 0 .and. nc< size(cards))
        nr =   cio_fgetline (card,  120,  lun)
        if(.not. found) then !search for 1st non comment , non blank card
          card = adjustl(card)
          if(card(1:1)== '#') cycle
          if(card== ' ') cycle
          nc = nc+1
          call string_to_upper(card)
          cards(nc) = card
          found = .true.
        else
          card = adjustl(card)
          if(card(1:1)== '#') cycle
          if(card== ' ') cycle
          if(index(card,'END') > 0) exit
          nc = nc+1
          cards(nc) = card
        endif
      enddo
      i_err = cio_fclose(lun)
      if(nc <= 0) then
        write(stdo,*) 'modgrid_rddesc_siti: no input cards?'
        return
      endif
      if(nc > 100) then
        write(stdo,*) 'modgrid_rddesc_siti: >100 input cards?'
        return
      endif

      !initialize variables in case rank < 3
      n = 1
      hdwd = -1
      coord = ' '
      oxyz = 0.0
      axis1= 0.0
      axis2= 0.0
      axis3= 0.0
      punits= 'xxxx'
      xlab = -1
      ylab = -1
      zlab = -1
      aunit= '  '
      po = 0.0
      pd = 1.0
      o  = 0.0
      d  = 1.0
      do i = 1,nc
        card = cards(i)
        call string_to_upper(card)
        if(i==1) read(cards(i),*) rank
        j = i-1
        if(i>1 .and.j<4) read(cards(i),*) n(j),o(j),d(j),po(j),pd(j),&
        coord(j), aunit(j)
        o(j) = o(j) - 1.0    !note SITI origin counts from 1 , CPS counts from 0
        if(coord(j)(1:1)=='X') then
          xlab=j
          hdwd(j) = 7
        endif
        if(coord(j)(1:1)=='I') then
          ylab=j
          hdwd(j) = 8
        endif
        if(coord(j)(1:1)=='Z') then
           zlab=j
          hdwd(j) = -3
        endif
        if(coord(j)(1:1)=='T') then
          zlab=j
          hdwd(j) = -2
        endif
      enddo
      n(1) = max(n(1),1)
      n(2) = max(n(2),1)
      n(3) = max(n(3),1)


      name = 'SITI'
      pname= 'XXXX'
      punits= '  '
      if(xlab>0) then
       oxyz(1) = o(xlab)
      else
       xlab = 6 - zlab - ylab
       hdwd(xlab) = 7
      endif
      if(ylab>0) then
       oxyz(2) = o(ylab)
      else
       ylab = 6 - zlab - xlab
       hdwd(ylab) = 8
      endif
      if(zlab>0) then
       oxyz(3) = po(zlab)
      else
       zlab = 6 - xlab - xlab
      endif

      o(zlab) = po(zlab)  !use physical z size
      d(zlab) = pd(zlab)  !use physical z size
      if(xlab>0) then
        if(xlab==1) then
          axis1(1) = (n(1)-1)*d(1)
        endif
        if(xlab==2) then
          axis2(1) = (n(2)-1)*d(2)
        endif
        if(xlab==3) then
          axis3(1) = (n(3)-1)*d(3)
        endif
      endif
      if(ylab>0) then
        if(ylab==1) then
          axis1(2) = (n(1)-1)*d(1)
        endif
        if(ylab==2) then
          axis2(2) = (n(2)-1)*d(2)
        endif
        if(ylab==3) then
          axis3(2) = (n(3)-1)*d(3)
        endif
      endif
      if(zlab>0) then
        if(zlab==1) then
          axis1(3) = (n(1)-1)*d(1)
        endif
        if(zlab==2) then
          axis2(3) = (n(2)-1)*d(2)
        endif
        if(zlab==3) then
          axis3(3) = (n(3)-1)*d(3)
        endif
      endif

      if(rank < 3) then
        aunit(3) = aunit(2)
        coord(3) = 'IL'
      endif
      call modgrid_create (obj, rank, name, pname, punits, &
        n, o, d, stdo)
      call modgrid_set_hdwd(obj,hdwd)
      call modgrid_set_aunit(obj,1,aunit(1))
      call modgrid_set_aunit(obj,2,aunit(2))
      call modgrid_set_aunit(obj,3,aunit(3))
      call modgrid_set_xyz(obj,coord, oxyz, axis1,axis2,axis3)

      ! define grid transform based upon HDR information
      ! pd is assumed to be the physical bin width of 1 grid
      call grid_set_transform(gobj,po(xlab),po(ylab),&
       dangle, pd(xlab),pd(ylab), 1)
      obj%gobj = gobj
      status = 0
      return
      end function modgrid_rddesc_siti

      !generate siti bDR string from a modgrid object
      integer function modgrid_to_siti(obj, ascrep,dfile)
      type(modgrid_struct),intent(in) :: obj       ! arguments
      character(len=*),intent(out)    :: ascrep
      character(len=*),intent(in)     :: dfile
      character(len=120)   :: cards(40)
      integer       :: i
      integer       :: cnt
      real          :: o(3)            !grid origins
      real          :: d(3)            !grid deltas
      double precision    :: po(3)     !physical origins
      double precision    :: pd(3)     !physical bin width
      character(len=4)    :: slab(3)   !Siti labels
      character(len=120)  :: card      !local variables
      character(len=64)   :: sval
      character(len=4)    :: aunit     !Siti units
      integer       :: xlab,ylab,zlab
      integer       :: i_err
      cnt=0
      modgrid_to_siti=cnt

      i_err = modgrid_xyz_order(obj,xlab,ylab,zlab)
      if(i_err /=0) then
        write(obj%stdo,*) 'modgrid_to_siti: modgrid_xyz_order error'
        call modgrid_print(obj,obj%stdo)
        return
      endif


      !see if iwe are in physical distances or grid units
      !we assume headers 7-8, or 17-18 are in use
      !siti needs both grid and physical settings defined

      pd(xlab) = grid_get_xgrid_width (obj%gobj)   !physical inline bin size
      pd(ylab) = grid_get_ygrid_width (obj%gobj)   !physical x-line bin size
      pd(zlab) = obj%d_grid(zlab)                  !depth bin size
      po(zlab) = 0.0
      if(obj%hdwd(xlab)==7) then    !we are in grid units
        o(xlab)  = obj%o_grid(xlab)
        o(ylab)  = obj%o_grid(ylab)
       !po(xlab) = grid_get_xsurvey_coord(obj%gobj,o(xlab),o(ylab))
       !po(ylab) = grid_get_ysurvey_coord(obj%gobj,o(xlab),o(ylab))
        po(xlab) = pd(xlab)*o(xlab)
        po(ylab) = pd(ylab)*o(ylab)
        d(xlab)  = obj%d_grid(xlab)
        d(ylab)  = obj%d_grid(ylab)
      else                          !we are in survey units
        po(xlab) = obj%o_grid(xlab)
        po(ylab) = obj%o_grid(ylab)
        o(xlab)  = grid_get_xgrid_coord(obj%gobj,po(xlab),po(ylab))
        o(ylab)  = grid_get_ygrid_coord(obj%gobj,po(xlab),po(ylab))
        d(xlab)  = obj%d_grid(xlab)/pd(xlab)
        d(ylab)  = obj%d_grid(ylab)/pd(ylab)
      endif
      o(zlab)  = 1
      d(zlab)  = 1
      slab(xlab) = 'XL'
      slab(ylab) = 'IL'
      slab(zlab) = 'Z'
      card = '# This is a SITI Header File.'//char(10)//'# Version 1.0.'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      card = '# Created by ConocoPhillips modgrid process'
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card
      write(card,'("       3         6         1       ",I6)') &
        obj%n_grid(3) !depth size
      cnt = cnt + 1
      cards(min(cnt,size(cards)))=card

      !  N   Grid_origin  Grid_delta  Phys_origin  Phys_bin_width Label Unit
      !  Note: SITI origin counts from 1 whereas CPS starts at 0
      do i = 1,obj%rank
        card = ' '
        write(sval,'(" ",I6)') obj%n_grid(i)     !data size
        card = trim(card)//'  '//trim(sval)
        write(sval,'(" ",I6)') nint(o(i))        !logical origin
        card = trim(card)//'  '//trim(sval)
        write(sval,'(" ",I6)') nint(d(i))        !logical delta
        card = trim(card)//'  '//trim(sval)
        write(sval,'(" ",F12.3)') po(i)          !physical origin
        card = trim(card)//'  '//trim(sval)
        write(sval,'(" ",F12.3)') pd(i)          !physical bin width
        card = trim(card)//'  '//trim(sval)
        ! SITI XL -> trace position in a line
        ! SITI IL -> line number
        card = trim(card)//'  '//slab(i)   !obj%label(i)
        aunit = 'Ft'
        if(obj%a_unit(i)(1:1)=='F') aunit = 'Ft'
        if(obj%a_unit(i)(1:1)=='M') aunit = 'M'
        card = trim(card)//'  '//aunit
        cnt = cnt+1
        cards(min(cnt,size(cards)))=card
      enddo

      modgrid_to_siti=cnt
      ascrep=' '
      do i = 1,cnt
        ascrep=trim(ascrep)//' '//trim(cards(i))//char(10)
      enddo
      ascrep=trim(ascrep)//char(10)
      return
      end function modgrid_to_siti

!     integer function modgrid_copy_obj(obj, objo, stdo) result(status)
!     type(modgrid_struct),intent(in)  :: obj
!     type(modgrid_struct),pointer     :: objo
!     integer,intent(in)               :: stdo
!     integer      :: i_err
!     status = -1
!     !allocate structure, initialize and set basic variables
!     call modgrid_create (objo, obj%rank, obj%name, obj%pname, obj%punits, &
!       obj%n_grid, obj%o_grid, obj%d_grid, stdo)
!     if(.not.associated(objo)) then
!       print *,'modgrid_copy_obj: failed create call'
!       return
!     endif
!     call modgrid_set_hdwd(objo,obj%hdwd)
!     objo%label(1)= modgrid_header_to_string(obj%hdwd(1))
!     objo%label(2)= modgrid_header_to_string(obj%hdwd(2))
!     objo%label(3)= modgrid_header_to_string(obj%hdwd(3))
!     objo%a_unit = obj%a_unit
!     i_err = modgrid_put_datar(objo,obj%n_grid(1),obj%n_grid(2),&
!     1,obj%n_grid(3),&
!      obj%memdata%data)
!     if(i_err /=0) return
!     status = 0
!     return
!     end function modgrid_copy_obj


      !copy object on root to a target cpu
      !Call this function after creating obj on the root cpu
      integer function modgrid_pcopy(obj, to, stdo) result(status)
      type(modgrid_struct),pointer     :: obj
      integer,intent(in)               :: stdo
      integer,intent(in)               :: to
      integer  :: cpu
      character(len=64)  :: name
      character(len=64)  :: pname
      character(len=16)  :: punits
      integer            :: rank
      integer            :: ng(4),ss(3)
      real               :: og(4)
      real               :: dg(4)
      integer            :: hd(4)
      integer            :: i_err
      real,pointer       :: data(:)

      status = -1
      ss = 1
      cpu = pcpsx_i_pel()
      if(cpu==0) then
         rank = obj%rank
         name = obj%name
         pname = obj%pname
         punits = obj%punits
         ng = obj%n_grid
         og = obj%o_grid
         dg = obj%d_grid
         hd = obj%hdwd
      endif
      !if(cpu== 0) then
      if(pcpsx_n_pel() > 1) then
      ! call pcpsx_send(root,to,name)

        call pcpsx_broadcast(0,rank)
        call pcpsx_broadcast(0,name)
        call pcpsx_broadcast(0,pname)
        call pcpsx_broadcast(0,punits)
        call pcpsx_broadcast(0,rank,ng)
        call pcpsx_broadcast(0,rank,og)
        call pcpsx_broadcast(0,rank,dg)
        call pcpsx_broadcast(0,rank,hd)
      endif
      if(cpu == to) then
      ! pcpsx_recieve(root,0,name)
      endif
      !allocate structure, initialize and set basic variables
      if(cpu /= 0) then
        call modgrid_create (obj, rank, name, pname, punits, &
        ng, og, dg, stdo)
        if(.not.associated(obj)) then
          print *,'modgrid_pcopy: failed create call on cpu=',cpu
          return
        endif
        call modgrid_set_hdwd(obj,hd)
        obj%label(1)= modgrid_header_to_string(hd(1))
        obj%label(2)= modgrid_header_to_string(hd(2))
        obj%label(3)= modgrid_header_to_string(hd(3))
       ! obj%a_unit = obj%a_unit
        !allocate data buffer
        i_err =  modgrid_put_data_iclip(obj,ss,ng)

      endif
      if(associated(obj%memdata)) then
       !call pcpsx_broadcast(0,ng(1)*ng(2)*ng(3),obj%memdata%data)
        data => modgmem_data(obj%memdata)
        call pcpsx_broadcast(0,ng(1)*ng(2)*ng(3),data)
      endif
      if(associated(obj%mspdata)) then
        call modspec_pcopy_obj(obj%mspdata,stdo)
      endif

      status = 0
      return
      end function modgrid_pcopy

!BRICK_START

      !break an a super-domain into overlapping sub-domains
      ! Given:
      !        xsz  ... max allowed sub domain size in x
      !        ysz  ... max allowed sub domain size in y
      !        xov  ... x overlap size of sub-domains
      !        yov  ... y overlap size of sub-domains
      !        sup_nx  ... size of the x super-domain in bins
      !        sup_ny  ... size of the y super-domain in bins
      !        sup_ox  ... origin of the x super-domain
      !        sup_oy  ... origin of the y super-domain
      !        sup_dx  ... bin size of the x super-domain
      !        sup_dy  ... bin size of the y super-domain
      ! Compute:
      !        nxb  ... number of sub-domain blocks in x direction
      !        nyb  ... number of sub-domain blocks in y direction
      !        cpt  ... store x,y corner points of the sub-domains (4D)
      integer function modgrid_partition(nxb, nyb,cpt,&
        sup_nx,sup_ox,sup_dx,&
        sup_ny,sup_oy,sup_dy,&
        xsz,ysz,xov,yov, stdo) result(status)
      integer,intent(inout)         :: nxb
      integer,intent(inout)         :: nyb
      integer,intent(in)            :: sup_nx
      integer,intent(in)            :: sup_ny
      real,intent(in)               :: sup_ox
      real,intent(in)               :: sup_oy
      real,intent(in)               :: sup_dx
      real,intent(in)               :: sup_dy
      real,pointer :: cpt(:,:,:,:) !x_sub-block,y_sub-block,point,x-y
      real,intent(in)               :: xov !x overlap size
      real,intent(in)               :: yov !y overlap size
      real,intent(in)               :: xsz !max sub domain size in x
      real,intent(in)               :: ysz !max sub domain size in y
      integer,intent(in)            :: stdo

      integer   :: i_err
   !  integer   :: x1,x2,y1,y2
      real      :: rx1,rx2,ry1,ry2
      integer   :: ixov !x overlap size
      integer   :: iyov !y overlap size
      integer   :: ixsz !max sub domain size in x
      integer   :: iysz !max sub domain size in y
      integer   :: i,j
      integer,pointer :: sdnx(:), sdox(:)
      integer,pointer :: sdny(:), sdoy(:)
      real,pointer    :: rsdox(:)
      real,pointer    :: rsdoy(:)


      status = -1
      if(xsz < xov) then
        write(stdo,*) 'modgrid_partition: error xsz < xov',xsz,xov
        return
      endif
      if(ysz < yov) then
        write(stdo,*) 'modgrid_partition: error ysz < yov',ysz,yov
        return
      endif
      if(sup_dx ==0 .or. sup_dy==0) then
        write(stdo,*) 'modgrid_partition: error sup_dx|sup_dy= 0',sup_dx,sup_dy
        return
      endif
      if(sup_nx <1 .or. sup_ny<1) then
        write(stdo,*) 'modgrid_partition: error sup_nx|sup_ny< 1',sup_nx,sup_ny
        return
      endif

      ! integerize the bin arithmetic
      ixov =  max(0.0,xov) /sup_dx
      iyov =  max(0.0,yov) /sup_dy
      ixsz =  max(0.0,xsz) /sup_dx
      iysz =  max(0.0,ysz) /sup_dy
      ixsz = max(1,ixsz)
      iysz = max(1,iysz)
     !print *,'sz',ixsz,iysz
     !print *,'ov',ixov,iyov
     !print *,'nsup',sup_nx,sup_ny


      !determine number of bricks in x & y direction
      nxb = modgrid_bricks(sup_nx, ixsz,ixov)
      nyb = modgrid_bricks(sup_ny, iysz,iyov)

      !determine the brick integerized coordinates
      !breaks sup_nx into nxb overlapping sub_domains
      i_err = modgrid_brick_info(nxb, sup_nx, ixsz,&
              ixov,sdnx,sdox)
      !breaks sup_ny into nyb overlapping sub_domains
      i_err = modgrid_brick_info(nyb, sup_ny, iysz,&
              iyov,sdny,sdoy)

      ! convert sub_domain integer coord back to real
      i_err = modgrid_brick_gtos(nxb, sup_ox,sup_dx,sdox, rsdox)
      i_err = modgrid_brick_gtos(nyb, sup_oy,sup_dy,sdoy, rsdoy)

     !do i = 1,nxb
     !   print *,'partition: x', i,' nx=',sdnx(i),' gox=',sdox(i),' sox=',&
     !   rsdox(i)
     !enddo
     !do i = 1,nyb
     !   print *,'partition: y', i,' ny=',sdny(i),' goy=',sdoy(i),' soy=',&
     !   rsdoy(i)
     !enddo

      ! Compute corner points of sub-domain cells

      allocate(cpt(nxb,nyb,4,2), stat=i_err)
      if(i_err /= 0) then
         print *,'modgrid_partition: allocation error '
         return
      endif
      do j=1,nyb

        ry1 = rsdoy(j)
        ry2 = ry1 + (sdny(j)-1)*sup_dy
        do i=1,nxb
          rx1 = rsdox(i)
          rx2 = rx1 + (sdnx(i)-1)*sup_dx

          cpt(i,j,1,1) = rx1
          cpt(i,j,1,2) = ry1
          cpt(i,j,2,1) = rx2
          cpt(i,j,2,2) = ry1
          cpt(i,j,3,1) = rx2
          cpt(i,j,3,2) = ry2
          cpt(i,j,4,1) = rx1
          cpt(i,j,4,2) = ry2

        enddo
      enddo
      if(associated(rsdox)) deallocate(rsdox)
      if(associated(rsdoy)) deallocate(rsdoy)
      if(associated( sdnx)) deallocate(sdnx)
      if(associated( sdny)) deallocate(sdny)
      if(associated( sdox)) deallocate(sdox)
      if(associated( sdoy)) deallocate(sdoy)

      status = 0
      return
      end function modgrid_partition

      !compute the number of bricks
      !Given:   ng ...  size of domain to sub divide (length in bins)
      !         isz...  max allowed sub-domain size  (length in bins)
      !         iovrlp. overlap between sub-domains  (length in bins)
      integer function modgrid_bricks(ng, isz, iovrlp) result(nbricks)
      integer,intent(in)    :: ng     !size of domain to sub-divide
      integer,intent(inout) :: isz    !max allowed sub-domain size (bins)
      integer,intent(inout) :: iovrlp !overlap between subdomains

      integer     :: x1,x2   !domain grid boundary limits
      nbricks = 0
      isz = max(1,isz)
      isz = min(ng,isz)
      iovrlp = max(0,iovrlp)
      iovrlp = min(isz-1,iovrlp)
      x1 = 1
      x2 = 0
     !if(iovrlp==0) then
     !  nbricks = 1 + (ng - 1)/isz
     !else
        do while(x2 < ng )
          nbricks =  nbricks + 1
          x2 = x1  + isz - 1
          x2 = min(x2, ng)
          if(x2>= ng) exit
          x1 = x2 - iovrlp + 1
        enddo
     !endif
      nbricks = max(1,nbricks)

      return
      end function modgrid_bricks

      !compute sub-domain origin, and size in bin units
      !assumes size and overlap have same increment size. no fractional steps
      integer function modgrid_brick_info(nbricks,ng, isz, iovrlp, sdn, sdo) &
      result(status)
      integer,intent(in)    :: nbricks !
      integer,intent(in)    :: ng      !size of domain to sub-divide
      integer,intent(in)    :: isz     !max allowed sub-domain size (bins)
      integer,intent(in)    :: iovrlp  !overlap between subdomains
      integer,pointer       :: sdn(:)  !sub-domain size array
      integer,pointer       :: sdo(:)  !sub-domain origin array

      integer  :: x1,x2
      integer  :: i, i_err
      status = -1
      if(nbricks<1) then
        print *,'modgrid_brick_info: error, no bricks?'
        return
      endif
      if(nbricks>1000) then
        print *,'modgrid_brick_info: error, > 1000 bricks?'
        return
      endif
      allocate(sdo(nbricks),stat=i_err)
      if(i_err /= 0) then
        print *,'modgrid_brick_info: error, sdo allocation failure'
        return
      endif
      allocate(sdn(nbricks),stat=i_err)
      if(i_err /= 0) then
        print *,'modgrid_brick_info: error, sdn allocation failure'
        return
      endif

      x1 = 1
      do i = 1,nbricks
         x2 = x1  + isz - 1
         x2 = min(x2, ng)
         sdo(i) = x1
         sdn(i) = x2 - x1 + 1 !sdn=1 --> 1 bin wide with x1=x2
         x1 = x2 - iovrlp + 1
      enddo
      status = 0
      return
      end function modgrid_brick_info

      !compute sub-domain origin, and size
      integer function modgrid_brick_gtos(nbricks,&
        sup_o, sup_d, sdo, rsdo) result(status)
      integer,intent(in)    :: nbricks !number of sub-domains in super-domain
      real,intent(in)       :: sup_o   !super-domain origin
      real,intent(in)       :: sup_d   !super-domain sample size
      integer,intent(in)    :: sdo(:)  !sub-domain integer origin array
      real,pointer          :: rsdo(:) !sub-domain real origin array

      integer  :: j, i_err

      status = -1
      if(nbricks<1) then
        print *,'modgrid_brick_gtos: error, no bricks?'
        return
      endif
      if(nbricks>1000) then
        print *,'modgrid_brick_gtos: error, > 1000 bricks?'
        return
      endif

      allocate(rsdo(nbricks),stat=i_err)
      if(i_err /= 0) then
        print *,'modgrid_brick_gtos: error, rsdo allocation failure'
        return
      endif

      !compute real coordinates
      !bin counting starts at bin 1(i.e. bin sdo(1)=1 is at the origin sup_o)
      do j=1,nbricks
        rsdo(j) = sup_o + (sdo(j)-1)*sup_d
      enddo

      status = 0
      return
      end function modgrid_brick_gtos
!BRICK_END
      ! read data into buffer
      ! assume same sampling as obj and same x-y-z ordering
      integer function modgrid_rd_data_cube(obj,ngo,ogo,dgo,buff)&
      result(status)
      type(modgrid_struct),intent(inout) :: obj

      integer,intent(in)              :: ngo(:)
      real,intent(in)                 :: ogo(:)
      real,intent(in)                 :: dgo(:)
      real,intent(out)                :: buff(:)
      integer      :: ss
      integer      :: sg(3)
      integer      :: ng(3)

      character(len=8) :: ftype
      integer      :: i_err
      integer      :: i, j, k
      real         :: xval
      real,pointer :: data(:)
      integer      :: m1,n1

      nullify (data) ! jpa

      status = -1

      ftype = modgrid_ftype(obj)
      if(ftype=='UNKNOWN') then
        write(obj%stdo) 'modgrid_rd_data_cube: UNKNOWN type'
        return
      endif
      if(ngo(1)*ngo(2)*ngo(3) > size(buff)) then
        write(obj%stdo) 'modgrid_rd_data_cube: input buffer is too small'
        write(obj%stdo) 'modgrid_rd_data_cube: buff size=',size(buff)
        write(obj%stdo) 'modgrid_rd_data_cube: need size=',ngo(1)*ngo(2)*ngo(3)
        return
      endif
     !call modgrid_print(obj,obj%stdo)

      
      ng(1:3) = obj%n_grid(1:3)
      sg = 1
      m1 = 1
      !load requested slices one at a time
      do i = 1,ngo(3)
        !coordinate of ith slice
        xval = ogo(3) + (i-1)*dgo(3)
        !nearest neighbor index of ith slice
        ss = 1 + nint((xval - obj%o_grid(3))/obj%d_grid(3))
        !clip index to legitimate range
        ss = max(ss,1)
        ss = min(ss,obj%n_grid(3))
        ! form address triplets to read data
        sg(3) = ss
        ng(3) = 1
        !read in the ith slice
        status =  modgrid_rd_data_iclips(obj,sg,ng)
        if(status /=0 ) then
          write(obj%stdo,*) 'modgrid_rd_data_cube: error reading'
          return
        endif
        !get access to the data pointer
        i_err =  modgrid_get_datar(obj,data)
        if(i_err /=0) then
          write(obj%stdo,*) 'modgrid_rd_data_cube: data point error '
          return
        endif

        do j = 1,ngo(2)
           xval = ogo(2) + (k-1)*dgo(2)
           ss = 1 + nint((xval - obj%o_grid(2))/obj%d_grid(2))
           ss = max(ss,1)
           ss = min(ss,obj%n_grid(2))
           n1 = (ss-1)*obj%n_grid(1)
           
           do k = 1,ngo(1)
              xval = ogo(1) + (k-1)*dgo(1)
              ss = 1 + nint((xval - obj%o_grid(1))/obj%d_grid(1))
              ss = max(ss,1)
              ss = min(ss,obj%n_grid(1))
              n1 = n1+ss
              buff(m1:m1) = data(n1:n1)
              m1 = m1+1
           enddo
        enddo


      enddo
      return
      end function modgrid_rd_data_cube

      subroutine modgrid_set_grid(obj,gobj)
      type(modgrid_struct),intent(inout) :: obj
      type(grid_struct),intent(in) :: gobj
         obj%gobj = gobj
      return
      end subroutine modgrid_set_grid

      subroutine modgrid_get_grid(obj,gobj)
      type(modgrid_struct),intent(in) :: obj
      type(grid_struct),intent(inout) :: gobj
         gobj = obj%gobj
      return
      end subroutine modgrid_get_grid

      function modgrid_open_binfile(obj,dfile,mode,stdo,otype) result(trcio)
      type(modgrid_struct),intent(in) :: obj
      character(len=*),intent(in)     :: dfile
      character(len=*),intent(in)     :: mode
      character(len=*),intent(in),optional     :: otype
      integer,intent(in)              :: stdo
      type(trcio_struct),pointer      :: trcio  !returned

      integer(kind=8) :: i8
      integer         :: ext(2)
      integer         :: i_err

      nullify(trcio)
      if(dfile==' ') return
      !estimate a size big enough for a trcio file
      !it will be trimmed on close if it is smaller
      i8 = 4*(64+obj%n_grid(1))
      i8 = i8*obj%n_grid(2)
      i8 = i8*obj%n_grid(3)
      i8 = i8 + 8192
      ext(1) = i8/256000000 + 1
      ext(2) = 0
      i_err = cio_set_file_ext_size(ext)
      if(i_err .ne. 0) then
        print *,'modgrid_open_binfile: error from cio_set_file_ext_size'
        return
      endif
 
      trcio   => trcio_open(dfile,mode )
      if(.not. associated(trcio) ) then
          write(stdo,*) 'modgrid_open_binfile: error opening data file ',&
          trim(dfile)
          return
      endif
      if(present(otype)) i_err= trcio_set_ftype(trcio,otype) 
      return
      end function modgrid_open_binfile

      subroutine modgrid_close_binfile(file)
      type(trcio_struct),pointer     :: file  !returned
      integer       :: i_err
      i_err = 0
      if(associated(file)) i_err = trcio_close(file)
      if(i_err .ne.0) then
         print *,'modgrid_close_binfile: error closing data file'
      endif
      nullify(file)
      return
      end subroutine modgrid_close_binfile

      ! write a slice of data from the object to the proper position in
      ! a binary data file. Write data that is internally held by obj.
      ! slice    ... from 1 to n=oobj%n_grid(3). output location
      ! trcio    ... trcio file handle

      integer function modgrid_wr_binslice_i(obj,trcio,slice,stdo,otype) &
      result(status)
      type(modgrid_struct),intent(in) :: obj
      type(trcio_struct),pointer      :: trcio   !input
      integer,intent(in)              :: slice
      integer,intent(in)              :: stdo
      character(len=*)                :: otype

      character(len=88)  :: msg

      real,pointer :: dpntr(:)
      integer      :: hdwd,hdwd2,hdwd3,n1,n2,n3
      real         :: o1,d1,o2,d2,o3,d3
      integer      :: i_err
      status = -1
      msg    = ' '

! get grid parameters
      call modgrid_get_griddesc(obj,1, hdwd,n1,o1,d1)
      call modgrid_get_griddesc(obj,2, hdwd2,n2,o2,d2)
      call modgrid_get_griddesc(obj,3, hdwd3,n3,o3,d3)
!   
      if(.not. associated(obj%memdata)) then
        msg = 'modgrd_wr_binslice_i: no data to output'
        goto 99
      endif

      !get a pointer to the data to output
      i_err = modgrid_get_slicer(obj,dpntr, slice)
      if(i_err < 0) then
        write(msg,&
        '("modgrid_wr_binslice_i: get_slicer failed for slice=",i6)') &
        slice
        goto 99
      endif
      if(.not. associated(dpntr)) then
        write(msg,&
        '("modgrid_wr_binslice_i: dpntr is null slice=",i6)') slice
        goto 99
      endif

      i_err = modgrid_wr_binslice(obj,trcio,slice,stdo,otype,&
       n1,n2, dpntr)
      if(i_err < 0) then
        write(msg,&
        '("modgrid_wr_binslice_i: get_slicer failed for slice=",i6)') &
        slice
        goto 99
      endif


      status = 0
 99   continue
      if(status .ne.0) write(stdo,*) trim(msg)
      return
      end function modgrid_wr_binslice_i

      ! write a slice of data from the object to the proper position in
      ! a binary data file.
      ! slice    ... from 1 to n=oobj%n_grid(3)
      ! trcio    ... trcio file handle
      ! data     ... data to output as the ith slice 
      !          (Note: we assume data is same n1,n2 grid size as obj)
      integer function modgrid_wr_binslice(obj,trcio,slice,stdo,otype,&
       n1i,n2i, data) result(status)
      type(trcio_struct),pointer      :: trcio   !input
      type(modgrid_struct),intent(in) :: obj
      integer,intent(in)              :: slice
      integer,intent(in)              :: stdo
      character(len=*),intent(in)     :: otype
      integer,intent(in)              :: n1i,n2i
      real,intent(in)                 :: data(n1i,n2i)

      integer            :: lun
      character(len=88)  :: msg

      integer      :: hdwd1,hdwd2,hdwd3,n1,n2,n3
      real         :: o1,d1,o2,d2,o3,d3
      integer      :: npts,i,j,k
      integer      :: i_err
      integer      :: nwr
      integer      :: endian
      integer      :: oendian
      integer      :: bsiz,wblk,wbyt
      real,pointer :: wdpntr(:)
      status = -1
      msg    = ' '
      nullify(wdpntr)

      oendian = 1 !normally output as big endian
      if(otype=='SITI') oendian=0
! get grid parameters
      call modgrid_get_griddesc(obj,1, hdwd1,n1,o1,d1)
      call modgrid_get_griddesc(obj,2, hdwd2,n2,o2,d2)
      call modgrid_get_griddesc(obj,3, hdwd3,n3,o3,d3)
      if(n1i.ne.n1  .or. n2i.ne.n2) then
        msg = 'modgrid_wr_binslice: error, passed data is inconsistent'
        print *,'DBG: n1=',n1,' n1i=',n1i
        print *,'DBG: n2=',n2,' n2i=',n2i
        goto 99
      endif
!   
      lun = trcio_get_lun(trcio) !%lun
      if(lun<=0)  then
        write(msg,&
        '("modgrd_wr_binslice: lun=",i3," error in file handle")') lun
        goto 99
      endif
      if(slice<1 .or. slice> obj%n_grid(3)) then
        write(msg,&
        '("modgrd_wr_binslice: slice=",i6," out of valid range. error")')&
        slice
        goto 99
      endif


      if(otype=='VOXET' .or. otype=='SITI' .or. otype=='GSURF') then
        npts  = n1*n2
        allocate(wdpntr(npts),stat=i_err)
        if(i_err/=0) then
          write(stdo,*) 'modgrid_wr_binslice: allocate error npts=',npts
          goto 99
        endif
        k = 1
        do j = 1,n2
          do i = 1,n1
            wdpntr(k:k) = data(i,j)
            k = k+1
          enddo
        enddo
        !swap bytes if we are on a little endian machine
        endian = swap_endian()
       !if(endian ==0) then !file big endian, but mem is little
        if(oendian /= endian) then
          call swap_bytes(wdpntr(1:npts))
        endif

        bsiz = 4*npts
        wbyt = 0
        wblk = slice-1
        i_err = cio_fseek(lun,bsiz,wblk,wbyt,0)
        if(i_err < 0) then
          write(msg,'("modgrd_wr_binslice: seek error for slice=",I6)') &
          slice
          goto 99
        endif
        nwr   = cio_fwrite(wdpntr,1,bsiz,lun)
        if(nwr.ne.bsiz) then
          write(msg,'("modgrd_wr_binslice: write error for slice=",I6)') &
          slice
          goto 99
        endif
       !status= cio_ftell(lun,bsiz,wblk,wbyt)     !get byte position
       !print *,'wr_binslice: from ftell wblk=',wblk,' wbyt=',wbyt,' bsiz=',bsiz
      endif

      if(otype=='TRCIO') then
         i_err = modgrid_wr_trcio_slice(obj,trcio,slice,n1,n2,data)
         if(i_err < 0) then
           write(msg,'("modgrd_wr_binslice:  error writing traces, slice=",&
          &I6)')  slice
           goto 99
         endif
      endif

      status = 0
 99   continue
      if(associated(wdpntr)) deallocate(wdpntr, stat=i_err)
      if(status .ne.0) write(stdo,*) trim(msg)
      return
      end function modgrid_wr_binslice

      !output a slice of data held by oobj
      integer function modgrid_wr_trcio_slice(oobj,trcio,slice,n1,n2, data) &
      result(status)
      type(modgrid_struct),intent(in) :: oobj
      type(trcio_struct),pointer      :: trcio
      integer,intent(in)              :: slice
      integer,intent(in)              :: n1,n2
      real,intent(in)                 :: data(n1,n2)
      integer  traceno
      integer  i_err
      integer  i
      character(len=88) :: msg

      status = -1
      msg = ' '

      if( (n1.ne.oobj%n_grid(1)) .or. (n2.ne.oobj%n_grid(2))) then
        msg = 'modgrid_wr_trcio_slice: error, passed data is inconsistent'
        goto 99
      endif 
      if(slice < 1 .or. slice > oobj%n_grid(3)) then
        msg = 'modgrid_wr_trcio_slice: error, slice out of range'
        goto 99
      endif

      traceno = (slice-1)*oobj%n_grid(2) + 1
      do i = 1,min(oobj%n_grid(2),n2)
        i_err = modgrid_wr_trcio_trace(oobj,trcio,traceno,data(1:n1,i))
        if(i_err .ne.0) then
          write(msg,'("modgrid_wr_trcio_slice: error, writing traceno=",i6)')&
          traceno
          goto 99
        endif
        traceno = traceno + 1
      enddo
      status = 0
 99   continue
      if(status.ne.0) print *,trim(msg)
      return
      end function modgrid_wr_trcio_slice

      subroutine modgrid_regrid_modspec(obj,iname,o_xyz,  ngo,ogo,dgo)
      type(modgrid_struct),intent(inout) :: obj
      character(len=*),intent(in)   :: iname
      character(len=*),intent(inout):: o_xyz
      integer,intent(in)            :: ngo(*)
      real,   intent(in)            :: ogo(*)
      real,   intent(in)            :: dgo(*)

      type(modspec_struct),pointer :: mspec_in   !input modspec
      type(modspec_struct),pointer :: mspec_out  !output modspec
      integer           :: ix,iy,iz
      double precision  :: ox,oy,xw,yw
      real              :: oz
      real              :: wdx,wdy
      integer           :: nxdef,nydef,nzdef
      double precision  :: oxdef,oydef
      real              :: dxdef,dydef,dzdef,ozdef
      real              :: angle
      integer           :: nlay
      character(len=4)  :: units

      integer    :: i_err
      integer      :: ixlab,iylab,izlab
      integer      :: i
      integer      :: hdi(4)
      integer      :: ngi(4)
      real         :: ogi(4),dgi(4)

      !Check to see if we are dealing with a layered modspec model on input
      if( modgrid_ftype(obj) .ne. 'MODSPEC') return

      nullify(mspec_in)
      nullify(mspec_out)
      do i=1,obj%rank
        call modgrid_get_griddesc(obj,i,hdi(i),ngi(i),ogi(i),dgi(i))
      enddo
      i_err = modgrid_xyz_order(obj, ixlab,iylab,izlab)
      if(i_err /=0) then
        write(obj%stdo,*) 'modgrid_regrid: bad input order'
        return
      endif
      !do special interpolation for modspec models.
      ! 1. build new modspec on the output grid
      ! 2. interploate mspec_out to output grid points
      !do not interpolate from input grid to output grid
      !Create an output modspec that is based upon the output grid

      !get input modspec structure
      i_err = modgrid_get_modspec(obj,mspec_in)
      if(i_err /=0) then
         print *,'modgrid_regrid_modspec: error from get_modspec'
         return
      endif
      ! get input modspec world coordinates
      ! returned values are a cps viewpoint (may transpose x & y )
      call modspec_getdesc(mspec_in,nlay,angle,units,&
       nxdef,oxdef,dxdef,nydef,oydef,dydef,nzdef,ozdef,dzdef)
       ix = index(o_xyz,'X')
       iy = index(o_xyz,'Y')
       iz = index(o_xyz,'Z')
       ox = ogo(ix)
       oy = ogo(iy)
       oz = ogo(iz)
       xw = ox
       yw = oy
       wdx = dxdef
       wdy = dydef
       if(modspec_has_coef(mspec_in)) then
         if(hdi(ix)==7) then
          !assume the inp & out coords are both line trace description
          !compute world coordinate output origin from line,trace coordinate
         !print *,'modgrid_regrid_modspec: ox=',ogo(ix),' oy=',ogo(iy)
         !print *,'modgrid_regrid_modspec: xw=',xw,' yw=',yw
          i_err = modspec_grid2world(mspec_in,ogo(ix),ogo(iy),xw,yw)
          wdx = dxdef *  dgo(ix)/dgi(ixlab)
          wdy = dydef *  dgo(iy)/dgi(iylab)
         !print *,'modgrid_regrid_modspec: wdx=',wdx,' wdy=',wdy
         !print *,'modgrid_regrid_modspec: xw=',xw,' yw=',yw
         !print *,'modgrid_regrid_modspec: dgo=',dgo(1:3),' dgi=',dgi(1:3)
        !else
        ! print *, 'modgrid_regrid_modspec: physical coordinates'
        ! print *,'modgrid_regrid_modspec: wdx=',wdx,' wdy=',wdy
        ! print *,'modgrid_regrid_modspec: xw=',xw,' yw=',yw
         endif
       endif

       !create a new modspec on the same x-y grid as the output
       call modspec_create(mspec_out,obj%stdo,iname,'GRID',&
        ngo(ix),xw,wdx,&
        ngo(iy),yw,wdy,&
        ngo(iz),ogo(iz),dgo(iz))
       if(.not.associated(mspec_out)) then
          print *,'modgrid_regrid_modspec: error calling modspec_create'
          return
       endif
        
      !When there is a modspec input we create a modspec output
      !consistent with the output grid and then we use the
      !output modspec to reset the input obj
      if(associated(mspec_out)) then
        i_err = modgrid_set_modspec(obj,mspec_out,obj%stdo)
       !call modgrid_print(obj,stdo)
      endif


      return
      end subroutine modgrid_regrid_modspec

! note: the grid may descibe a larger data set than actually
!       exists within the file contents.
      integer function modgrid_segy_scan(stdo,ntoscan,ndidscan, dfile, &
      ntrfil, ndpt, dt, bypertr, nhdwd, xhdr,yhdr,hfast,hslow,&
      nbin,obin,dbin,&
      ngroup,first_tig,ntig,&
      xb,yb,xe,ye, do_print) result(status)
      character(len=*),intent(in) :: dfile
      integer,intent(in)    :: stdo
      integer,intent(in)    :: ntrfil   !traces in the file dfile
      integer,intent(in)    :: ndpt     !number of trace samples
      real   ,intent(in)    :: dt       !trace sample increment
      integer,intent(in)    :: bypertr  !number of bytes per hd-tr
      integer,intent(in)    :: nhdwd    !number of cps hd words
      integer,intent(in)    :: ntoscan  !number of traces to scan
      integer,intent(out)   :: ndidscan !number of traces actually scanned
      integer,intent(in)    :: xhdr
      integer,intent(in)    :: yhdr
      integer,intent(inout) :: hfast
      integer,intent(inout) :: hslow
      integer,intent(inout) :: nbin(:)
      real,intent(inout)    :: obin(:)
      real,intent(inout)    :: dbin(:)
      integer,intent(out)   :: ngroup  !number of groups scanned
                                       !group count defined by xhdr and yhdr
      integer,pointer  :: first_tig(:)
      integer,pointer  :: ntig(:)
      real,pointer     :: xb(:),xe(:)
      real,pointer     :: yb(:),ye(:)

      logical,intent(in),optional    :: do_print

      logical          :: quick_scan = .false.
      logical          :: doprint = .false.
      logical          :: swap
      integer          :: endian

      integer          :: lun
      integer          :: i_err
      integer          :: itr,i
      integer          :: nwrds
      integer          :: ndidrd

      real             :: rnil
      integer          :: inil
      integer          :: count       !total number of traces scanned
      integer          :: tnum        !the current trace to read
      
      integer,pointer  :: ibuff(:)
      double precision :: tstrt
      integer          :: wblk
      integer          :: cgrp        !computed number of groups
      integer          :: skip_bytes
      double precision :: hmin(100)
      double precision :: hmax(100)
      double precision :: hold(100)
      double precision :: hbin(100)
      double precision :: hdif(100)
      double precision :: cpshd(nhdwd)
      double precision :: tol     !tolerance for group membership
      double precision :: delx, dely
      integer          :: nwrk = 10000
      integer          :: grpbeg  !start trace for current group
      integer          :: grpend  !last trace for current group
      integer,pointer  :: traces_in_group(:)
      integer,pointer  :: first_trace_in_group(:)
      real,pointer     :: xbeg(:),xend(:)
      real,pointer     :: ybeg(:),yend(:)
      type (segy_trc_hdr)   :: syh
      character(len=120) :: msg

      endian = swap_endian()
      status= -1
      rnil  = 999.2
      inil  = MOD_INIL
      tstrt = 0.0
      nwrds = 1 + bypertr/4
      lun   = -1

      hfast = xhdr
      hslow = yhdr
      count = 0
      ndidscan = 0
      tnum  = 0
      ngroup= 0
      grpbeg= inil
      grpend= inil
      hdif  = rnil
      hmin  = rnil
      hmax  = rnil
      hbin  = rnil
      nullify(first_tig)
      nullify(ntig)
      nullify(xb)
      nullify(yb)
      nullify(xe)
      nullify(ye)
      nullify(ibuff)
      nullify(first_trace_in_group)
      nullify(traces_in_group)
      nullify(xbeg)
      nullify(ybeg)
      nullify(xend)
      nullify(yend)
      msg = 'modgrid_segy_scan: error allocating work buffers'
      allocate(first_trace_in_group(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      allocate(traces_in_group(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      allocate(xbeg(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      allocate(xend(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      allocate(ybeg(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      allocate(yend(nwrk), stat=i_err)
      if(i_err /= 0) goto 99
      msg = ' '
      first_trace_in_group(1) = inil
      traces_in_group(1) = inil
      xbeg  = rnil
      xend  = rnil
      ybeg  = rnil
      yend  = rnil
      tol   = 0.001
      msg   = ' '
      if(present(do_print)) doprint=do_print
      
      allocate (ibuff(nwrds), stat=i_err)
      if(i_err /=0) then
        msg = 'modgrid_segy_scan: allocate error:'
        goto 99
      endif

      lun = cio_fopen(dfile,'r')
      if(lun <= 0) then
        msg = 'modgrid_segy_scan: open error:'//trim(dfile)
        goto 99
      endif

      skip_bytes  = 3600
      i_err = cio_fseek(lun, skip_bytes, 0)    !go to 1st trace
      swap = .false.
      if(endian ==0) swap= .true. !file big endian, but mem is little

     !maxrd = min(ntoscan,ntrfil)
      do itr=1,min(ntoscan,ntrfil)

        tnum = tnum+1
        if(quick_scan .and. count < ntrfil) then
          if(ngroup > 2 .and. ntoscan < ntrfil) then
           !tnum = ntrfil - ntoscan/2 !skip traces and read to file end
            tnum = ntrfil - (cgrp/2)*traces_in_group(1) + 1
            wblk = tnum
         !  i_err  = cio_fseek(lun,bypertr,wblk,skip_bytes,0)
          endif
        endif

        ! get next trace and header
        ndidrd = cio_fread(ibuff, 1, bypertr,  lun)
        if(ndidrd /= 0) then
          if(ndidrd== CIO_EOF) then
            write(msg,*) 'segy_scan: hit EOF'
            !end of file is the end of a group
            if(ngroup>0) then
              first_trace_in_group(ngroup) = grpbeg
              traces_in_group(ngroup) = grpend - grpbeg + 1
            endif
            exit
          else if (ndidrd==CIO_ERROR) then
            write(msg,'(a,i3)') 'modgrid_segy_scan: read error:',i_err
            goto 99
          endif
        endif
        call segy_unpack_segyhd(syh,ibuff(1:60),swap)
        call segy_segyhd_to_cpshd(cpshd,syh,tstrt)

        count = count + 1
        ndidscan = count
        if(count==1) then
          ! first trace, do initialization
          hmin(1:nhdwd) = cpshd(1:nhdwd)
          hmax(1:nhdwd) = cpshd(1:nhdwd)
          hold(1:nhdwd) = cpshd(1:nhdwd)
          hbin(1:nhdwd) = rnil
          ngroup= 1
          grpbeg= tnum
          grpend= tnum
          first_trace_in_group(ngroup) = grpbeg
          traces_in_group(ngroup) = grpend - grpbeg + 1
          xbeg(ngroup) = cpshd(xhdr)
          ybeg(ngroup) = cpshd(yhdr)
          xend(ngroup) = cpshd(xhdr)
          yend(ngroup) = cpshd(yhdr)
        endif

        if(count==2) then   !determine fast & slow headers for groups
           if(hold(xhdr)==cpshd(xhdr) .and. &
              hold(yhdr)/=cpshd(yhdr)) then
              hfast = yhdr
              hslow = xhdr
           endif
           if(hold(yhdr)==cpshd(yhdr) .and. &
              hold(xhdr)/=cpshd(xhdr)) then
              hfast = xhdr
              hslow = yhdr
           endif
           if(hold(yhdr)/=cpshd(yhdr) .and. &
              hold(xhdr)/=cpshd(xhdr)) then  !stick with default
              msg ='modgrid_segy_scan: can not determine fast&
                  & or slow header by 1st two traces'
              print *,trim(msg)
           endif
        endif  !end of trace 2

        ! update min max header ranges, bin sizes, 
        ! and difference between current and last trace headers
        if(count>=2) then   !determine fast & slow headers for groups

          do i=1,nhdwd
           hmin(i) = min(cpshd(i),hmin(i))
           hmax(i) = max(cpshd(i),hmax(i))
           hdif(i) = abs(cpshd(i) - hold(i) )
           delx = hdif(xhdr)
           dely = hdif(yhdr)

           if(hdif(i) >= tol) then
             if(hbin(i) /= rnil) then
               hbin(i) = min(hbin(i), hdif(i))
             else
               hbin(i) = hdif(i)
             endif
           endif
          enddo !end of header loop

          !check for new group
         !if(hdif(hslow) < tol .and. cpshd(i) .ne. 0.0 ) then
          if(hdif(hslow) < tol ) then
               !increment current group size. reset group end
               grpend=tnum
               traces_in_group(ngroup) = grpend - grpbeg + 1
               xend(ngroup) = cpshd(xhdr)
               yend(ngroup) = cpshd(yhdr)
          else
               !new group detected
               !complete the old group information
               traces_in_group(ngroup) = grpend - grpbeg + 1
               if(ngroup==1) cgrp = ntrfil / traces_in_group(ngroup)
              !!can we scan another full group?
              !if(ntoscan-ngroup*traces_in_group(1)<traces_in_group(1)) then
              !endif
                
               if(mod(ntrfil , traces_in_group(ngroup)) .ne.0) then
                 !file must be irregular
                 print *,'modgrid_segy_scan: irregular grid?'
               endif
               xend(ngroup) = hold(xhdr)
               yend(ngroup) = hold(yhdr)
               ! start a new group
               ngroup = ngroup+1
               if(ngroup > size(first_trace_in_group) ) then
                 !we have run out of space
                 msg ='modgrid_segy_scan: ran out of space&
                  & for group storage'
                 exit
               endif
               grpbeg=tnum
               grpend=tnum
               first_trace_in_group(ngroup) = grpbeg
               xbeg(ngroup) = cpshd(xhdr)
               ybeg(ngroup) = cpshd(yhdr)
               xend(ngroup) = cpshd(xhdr)
               yend(ngroup) = cpshd(yhdr)
          endif

        endif  !count >=2

        hold(1:nhdwd) = cpshd(1:nhdwd)

      enddo  !end of trace loop

      ! save the segy ndpt and dt
      nbin(1) = ndpt
      obin(1) = 0.0
      dbin(1) = dt
      if(dbin(1) < 0) dbin(1) = 1.0

      dbin(2) = hbin(hfast)
      obin(2) = hmin(hfast)
      nbin(2) = 1 + (hmax(hfast) - obin(2) + 0.01*dbin(2))/dbin(2);
      if(nbin(2)<1) nbin(2) = 1;

      nbin(3) = ngroup  !1 + (ntrfil-1)/nbin(2);
      obin(3) = hmin(hslow)
      dbin(3) = hbin(hslow)

     !if(nbin(3)*nbin(2) /= ntrfil) then
     !  nbin(2) = ntrfil
     !  nbin(3) = 1
     !endif

      if(doprint) then
        print *,'segy_scan: count=',count,' ngroup=',ngroup
       !print *,'segy_scan: grpbeg=',grpbeg,' grpend=',grpend
        print *,'segy_scan: xhdr =',xhdr,'  yhdr =',yhdr
        print *,'segy_scan: hfast=',hfast,' hslow=',hslow
        print *,'segy_scan: Fast hmin=',hmin(hfast),&
              ' hmax=',hmax(hfast)
        print *,'segy_scan: Slow hmin=',hmin(hslow),&
              ' hmax=',hmax(hslow)
        print *,'  Group    first    count     xbeg  ybeg  xend  yend'
        do i = 1,ngroup
           print *,i,first_trace_in_group(i),&
            traces_in_group(i),xbeg(i),ybeg(i),xend(i),yend(i)
        enddo
        print *,'  i      hmin       hmax      hbin xbeg  ybeg  xend  yend'
        do i = 1,nhdwd
           print *,i, hmin(i),hmax(i),hbin(i)
        enddo
     endif

      status = 0
 99   continue

      if(status < 0) print *,msg
      if(associated(ibuff)) deallocate(ibuff, stat=i_err)
      if(ngroup> 0 .and. status ==0) then
        status = -1
        msg = 'modgrid_segy_scan: error allocating return buffers'
        allocate(first_tig(ngroup), stat=i_err)
        first_tig(1:ngroup) = first_trace_in_group(1:ngroup)
        if(i_err /= 0) goto 99
        allocate(ntig(ngroup), stat=i_err)
        ntig(1:ngroup) = traces_in_group(1:ngroup)
        if(i_err /= 0) goto 99
        allocate(xb(ngroup), stat=i_err)
        if(i_err /= 0) goto 99
        xb(1:ngroup) = xbeg(1:ngroup)
        allocate(xe(ngroup), stat=i_err)
        if(i_err /= 0) goto 99
        xe(1:ngroup) = xend(1:ngroup)
        allocate(yb(ngroup), stat=i_err)
        if(i_err /= 0) goto 99
        yb(1:ngroup) = ybeg(1:ngroup)
        allocate(ye(ngroup), stat=i_err)
        if(i_err /= 0) goto 99
        ye(1:ngroup) = yend(1:ngroup)
        status = 0
      endif
      if(associated(first_trace_in_group)) deallocate(first_trace_in_group)
      if(associated(traces_in_group)) deallocate(traces_in_group)
      if(associated(xbeg)) deallocate(xbeg)
      if(associated(ybeg)) deallocate(ybeg)
      if(associated(xend)) deallocate(xend)
      if(associated(yend)) deallocate(yend)
      i_err = cio_fclose(lun)
      return
      end function modgrid_segy_scan

!NILL METHODS BEG
      subroutine modgrid_set_rnil(obj,rnil)
      type(modgrid_struct),intent(inout) :: obj
      real,intent(in)  :: rnil
      obj%rnil = rnil
      return
      end subroutine modgrid_set_rnil
 
      real function modgrid_get_rnil(obj) result(rnil)
      type(modgrid_struct),intent(in) :: obj
      rnil = obj%rnil
      return
      end function modgrid_get_rnil
!NILL METHODS END

!REGION METHODS BEG

      subroutine modgrid_create_rgndata(rgndata,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      type(region_struct),pointer  :: rgndata
      character(len=*),intent(in)    :: flagfile
      integer,intent(in)    :: flag_off
      integer,intent(in)    :: flag_esize
      integer,intent(in)    :: flag_bit_len
      integer,intent(in)    :: rcnt
      integer,intent(in)    :: rbits(:)
      character(len=*),intent(in)    :: rnames(:)
      integer :: i_err
      nullify(rgndata)
      allocate(rgndata, stat=i_err)
      if(i_err/=0) then
        print *,'modgrid_create_rgndata: allocate error'
        return
      endif

      rgndata%flagfile = ' '
      rgndata%rcnt = 0
      rgndata%flag_esize = 6
      rgndata%rbits = -1
      rgndata%rnames = ' '
      rgndata%flagfile = flagfile
      rgndata%flag_esize = flag_esize
      rgndata%flag_off   = flag_off
      rgndata%flag_bit_len = flag_bit_len
      rgndata%rcnt   = rcnt
      rgndata%rbits  = rbits
      rgndata%rnames = rnames
      return
      end subroutine modgrid_create_rgndata

      subroutine modgrid_delete_rgndata(rgndata)
      type(region_struct),pointer  :: rgndata
      if(associated(rgndata) ) then
        deallocate(rgndata)
        nullify(rgndata)
      endif
      return
      end subroutine modgrid_delete_rgndata

      subroutine modgrid_set_rgndata(rgndata,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      type(region_struct),pointer  :: rgndata
      character(len=*),intent(in)    :: flagfile
      integer,intent(in)    :: flag_off
      integer,intent(in)    :: flag_esize
      integer,intent(in)    :: flag_bit_len
      integer,intent(in)    :: rcnt
      integer,intent(in)    :: rbits(:)
      character(len=*),intent(in)    :: rnames(:)
      if(.not.associated(rgndata) ) then
        call modgrid_create_rgndata(rgndata,flagfile,&
         flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
          return
      endif
      rgndata%flagfile = ' '
      rgndata%rcnt = 0
      rgndata%flag_esize = 6
      rgndata%rbits = -1
      rgndata%rnames = ' '
      rgndata%flagfile = flagfile
      rgndata%flag_esize = flag_esize
      rgndata%flag_off   = flag_off
      rgndata%flag_bit_len = flag_bit_len
      rgndata%rcnt   = rcnt
      rgndata%rbits  = rbits
      rgndata%rnames = rnames
      return
      end subroutine modgrid_set_rgndata

      subroutine modgrid_get_rgndata(rgndata,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      type(region_struct),pointer  :: rgndata
      character(len=*),intent(inout)    :: flagfile
      integer,intent(inout)    :: flag_off
      integer,intent(inout)    :: flag_esize
      integer,intent(inout)    :: flag_bit_len
      integer,intent(inout)    :: rcnt
      integer,intent(inout)    :: rbits(:)
      character(len=*),intent(inout)    :: rnames(:)

      if(.not.associated(rgndata)) return
      flagfile = rgndata%flagfile
      rcnt = rgndata%rcnt
      flag_esize = rgndata%flag_esize
      rbits = rgndata%rbits
      rnames = rgndata%rnames
      flag_off = rgndata%flag_off
      flag_bit_len = rgndata%flag_bit_len
      return
      end subroutine modgrid_get_rgndata

      integer function modgrid_rgndata_str(rgndata,ascrep,nc) result(status)
      type(region_struct),pointer    :: rgndata
      character(len=*),intent(inout) :: ascrep
      integer,intent(inout) :: nc    !position of last character
      character(len=128)    :: card
      integer       :: i
      status = 0
      if(associated(rgndata) ) then
        card = '# FLAGS_FILE='//trim(rgndata%flagfile)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_ESIZE=',rgndata%flag_esize
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_BIT_LENGTH=',rgndata%flag_bit_len
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_OFFSET=',rgndata%flag_off
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        do i = 1,rgndata%rcnt
          write(card,*) '# REGION '//trim(rgndata%rnames(i)),&
          rgndata%rbits(i)
          card = trim(card)//char(10)
          ascrep(nc+1:) = trim(card)
          nc = nc + len_trim(card)
        enddo
        if(nc .ne. len_trim(ascrep)) then
          print *,'modgrid_rgndata_str: error, nc=',nc,' len=',&
          len_trim(ascrep)
          status = -1
        endif 
      endif
      return
      end function modgrid_rgndata_str
!REGION METHODS END

      end module modgrid_module
