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
!                        C P S   P R I M I T I V E        
!
! Name       : trcio (TRaCe Input Output)
! Category   : io
! Written    : 1999-10-07   by: Bill Menger
! Revised    : 2009-02-05   by: Bill Menger
! Maturity   : beta
! Purpose    : Read and write trace files (JSEIS,CPS,TFIL,QTROT,SEGY,SU,LBO)
! Portability: For JavaSeis (JSEIS) this will invoke a JVM.
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
! A set of routines that allow read/write of Seismic Trace data files in the 
! following formats: CPS, JSEIS, TRCIO, LBO, TFIL, QTROT, SEGY, and SU.  
! CPS files (trcio and lbo formats) follow the CPSIO RULES.  A header is 
! followed by a HISTORY section and a DATA section. 
! In addition to read/write, there are header writing functions (to put a cpsio
! header in a TRCIO file), header printing functions (to print to stdout the
! trcio header), and other utils such as seek and tell, rewind, read-history,
! write-history, etc.
!-------------------------------------------------------------------------------
!</descript_doc>
!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!Traces are always converted TO native floating point IEEE format on read, and
!FROM native floating point IEEE format on write. (actual format on file can
! be specified in the file header which will affect the conversion if needed,
! but the routines for read/write always expect to put/get native ieee floats.
! (in other words, you should declare trace(:) to be REAL type.)
!-------------------------------------------------------------------------------
!</trace_io_doc>
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!Headers are always converted TO double precision on READ (native IEEE fmt) and
!are converted FROM double precision on WRITE.
!(in other words, you should declare header(:) to be DOUBLE PRECISION type.)
! An exception is the QTROT (deprecated) format.
!-------------------------------------------------------------------------------
!</header_word_doc>
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
!     * = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
! PUBLIC CONSTANTS:
!   TRCIO_OK    = all is well with the operation
!   TRCIO_ERROR = error on operation
!   TRCIO_EOF   = end of file
! DATA STRUCTURES:
!
!  type(trcio_struct),pointer :: file ! use this to access/pass file information
!  integer                    :: status  ! TRCIO_OK, TRCIO_ERROR, or TRCIO_EOF
!  
!                              i
!  status = trcio_writeheader(file)
!         Purpose: to write the header to a freshly opened trcio file before
!                  you've written traces to the file. (you should modify the
!                  file%nwih, file%num_values, file%nbits, file%nbits_hd as
!                  a minimum before writing the header.  This routine will
!                  calculate the file%recl for you based on the above and will
!                  assume IEEE floating point with native ENDIAN.  It will also
!                  assume 64 bit header, 64 word header, 32 bit trace vals, and
!                  0 trace length! It will calculate data_start_pos 
!
!          IF file%ftype = 'QTROT' It will write a "qtrot" header (temporary)
!
!                             i
!  status = trcio_readheader(file)
!         Purpose: To read the file's header information and load the file 
!                  structure with that information (if possible).
!                  Used in trcio_open.
!                            i
!  call   trcio_headerdump (file)
!         Purpose: To print file header to unit 6.
!
!                       i        i     i(opt) i(opt) i(opt) i(opt) i(opt)
!  file   => trcio_open(filename,mode,scratch, nwih, ndpt,  nbits, nbitshd,&
!            i(opt)  i(opt)     i(opt)i(opt)   i*  i*  i*  i*
!            history,compressed,srate,strt_val,snr,cmp,wdr,mtpc,
!               i(opt)     i(opt)
!            calledFrom, lbo_version)
!
!         Purpose: Open trace file
!                 RETURNS POINTER TO STRUCTURE, ALLOCATES THE STRUCTURE FOR YOU.
!         character(len=*) :: filename,mode
!         mode = [r w a r+ w+ a+ rn an](read/write/append, see cio doc.)
!         logical scratch (true = delete after close)
!         integer :: nwih,ndpt,nbits,nbitshd
!         character(len=8) :: history !! ALL BRIEF CURRENT (NONE)
!          nwih=number values in header, 
!          ndpt=number of values in trace
!          nbits=number of bits/value in trace
!          nbitshd=number of bits/value in header
!         IF you specify all of these 4 optional values, the header is pre-
!         written with them and you do not need to call trcio_writeheader(file)
!         before writing traces.  ALL 4 parms must be present or no action is
!         taken.
!         ************** OPTIONAL ARGUMENTS ADDED FOR COMPRESSED FILES ******
!         If writing a compressed trace file, you use the arguments following
!         "history".
!         logical   :: compressed            ! .true. or .false.
!         real      :: srate,strt_val        ! default start val=0.0
!         integer   :: snr,cmp,mtpc          ! either snr (signal/noise) or
!                                            ! cmp (compression ratio) should
!                                            ! be given, the other can be also
!                                            ! present but must be 0
!                                            ! mtpc = max traces per cluster
!         If reading a compressed file, you don't need to pass the optional
!         arguments indicating that it is compressed.  trcio will detect.
!         char(len=*) :: calledFrom   if == 'promaxW' then it calls 
!                                     trcio_write_history_cards(file,'NONE')
!                                     otherwise, does nothing.
!         integer   :: lbo_version           ! default = -1 (not lbo)
!                                            ! set == 2 for new LBO version
!                                            ! old LBO version was == 1 (read only
!                                            ! please!!!
!
!                        i     i(opt)
!  status = trcio_close(file,remove)
!         Purpose: Close the trace file.  Update header with number of traces,
!                  De-allocate file structure.
!         if remove == true, the file is deleted upon close.
!
!                             i   o  o   i*    i*    i*  (*=opt) 
!  status = trcio_read_trace(file,hd,tr,tnum, celv, ntrc)
!         Purpose: Read a trace from file into hd and tr, optionally from trace
!                  number tnum.
!                  If tnum is missing, it will read the next sequential trace.
!         type(trcio_struct),pointer :: file
!         double precision, dimension(64) :: hd
!         real, dimension (:)             :: tr
!         integer      ,optional          :: tnum
!            OR
!         real,optional                   :: celv = common element value
!         (if celv is present, traces are read by group according to which 
!          group has header 3 == celv.  tnum will be ignored.!)
!         integer,optional                :: ntrc ! number of traces to read.
!         real, dimension (:,:)           :: tr
!         double precision, dimension(:,:):: hd
!        
!                              i   i  i  i*    i*  (*=opt)
!  status = trcio_write_trace(file,hd,tr,tnum,ntrc)
!         Purpose: Write a trace (or an array of traces) to a file using the
!                  values in the hd and tr (1 or 2-d) arrays, optionally the
!                  position in the file is determined by tnum for random writes.
!                  If tnum is missing, it will write the next sequential trace.
!                  The file is not cleared if you write beyond the EOF.  You
!                  will simply allocate that file space.  In other words, if you
!                  have a file with 5 traces in it, open it for r+ (read /write)
!                  and then write trace 100, you will write the trace 100 to 
!                  position 100 but will have garbage (uncleared file space) in
!                  traces 6-99.
!                  The "ntrc" optional argument is ignored if writing with a
!                  1-dimensional hd and tr array set.  It is automatically set
!                  to the array size if a 2-dimensional array set is used for
!                  hd and tr, but it can be optionally overridden.
!         type(trcio_struct),pointer      :: file
!         double precision, dimension(:)  :: hd
!         real, dimension (:)             :: tr
!            OR
!         double precision, dimension(:,:):: hd
!         real, dimension (:,:)           :: tr
!         integer,optional                :: tnum ! trace number within the file
!                                                   to position the write operation.
!         integer,optional                :: ntrc ! number of traces to write.
!
!                                    i
!  status = trcio_update_num_traces(file)
!         Purpose: Update the file header with proper num of traces. (this is
!                  done for you at trace file close if it is a trcio file.)
!                  This only works on trcio files.
!
!                             i    i  
!  status = trcio_seek_trace(file,trace_number)
!         Purpose: To position the file at beginning of the specified
!                  record number (trace number).
! 
!    o                              i 
!  trace_number = trcio_tell_trace(file)
!         Purpose: To tell you which trace number the file is positioned on.
!
!                                        i
!  num_traces = trcio_get_number_traces(file)
!         Purpose: To tell the number of traces in the file.
!
!                                 i    i
!  call trcio_write_history_cards(file,history)
!         Purpose: To write history using the "history" flag.
!                  history = [ALL MODEL NONE or CURRENT]
!
! trcio_struct,pointer  file     pointer to the history file
! character             history  The history option.
!
!                                 i    o
!  call trcio_read_history_cards (file,status)
!         Purpose: To read history from a file.
!                  It is good practice to follow this sequence :
!                  open | write header | write history | write traces | close
!                  (the header will be updated upon file close.)
! 
! trcio_struct,pointer   file    pointer to the history file
! integer                status  status return, 0=OK
!
!                          i    i
!  status = trcio_set_ipn(file,ipn)
!         Purpose: To set the ipn of a process writing traces, before calling
!                  trcio_write_history. The default is ipn=999 (job)
!
!                             i     i
!  status = trcio_set_stdout(file,stdout)
!         Purpose: to set the standard output unit number for error messages.
!                  The default is unit 6.
!
!  trmaxg = trcio_get_trmaxg(file)
!         Purpose: To return the maximum LAV of all traces (so far if still
!                  writing the file).
!                  This will return 0D0 if the file header does not contain
!                  this value.
!    double precision :: trmaxg
!                                             i
!  num_global_cards = trcio_num_global_cards(file)
!         Purpose: To return the number of data cards in the file header that
!                  contain job-globals from the job when the file was created.
!         (This is needed so you can allocate an array of string variables
!          -- cards -- that will contain the globals, one per card, in case
!          you want to use these globals.)
!         type(trcio_struct) :: file
!         integer            :: num_global_cards
!         *** use this to allocate "num_global_cards" size of "global_cards"
!         character(len=80),dimension(:),allocatable :: global_cards
!                                i         i             o
!  status = trcio_read_globals(file,num_global_cards,global_cards)
!         Purpose: to return "num_global_cards" card images, each containing
!         one of the job-globals from the job that created the trcio file.
!         type(trcio_struct) :: file
!         integer            :: num_global_cards
!         character(len=80),dimension(:) :: global_cards(num_global_cards)
!         OUTPUT will be 1 global on each global_card string, in the form:
!         SCALAR_GLOBAL=VALUE
!         ARRAY_GLOBAL=(VAL1,VAL2...,VALn)
!         You then can use these to update or modify a job's globals.
!        
!                                  i          i             i 
!   status = trcio_write_globals(file,num_global_cards,global_cards)
!     Purpose: To allow one to query the parameter cache for all of its 
!              global cards (done separately) and transfer those cards to 
!              the header of a trcio file.  This must be done prior to
!              writing the traces or history, and after initializing the
!              trcio file header.
!     Sequence:
!             write_header
!             write_globals (optional)
!             write_history (optional)
!             write_traces
!             
!     type(trcio_struct),pointer               :: file
!     integer,intent(in)                       :: num_global_cards
!     character(len=*),dimension(:),intent(in) :: global_cards(num_global_cards)
!     integer                                  :: status
!
!                                   i  
!    num_errors = trcio_num_errors(file)
!     Purpose: To return the number of bad samples found so far in the file.
!     type(trcio_struct),pointer               :: file
!     integer                                  :: num_errors
!
!                                   i  
!    num_traces_with_errors = trcio_num_traces_with_errors(file)
!     Purpose: To return the number of bad traces found so far in the file.
!     type(trcio_struct),pointer               :: file
!     integer                                  :: num_traces_with_errors
!
! The next function changes the extent size of the next file to open.  It calls
! cio to do this.
!                              I
! status=trcio_set_ext_size(extsize)
!
! extsize = integer value of # millions of bytes in the extent.
! (example: ierr=trcio_set_ext_size(myfp,10) 
! would set the extent size to 10,000,000 bytes.
! Minimum extsize=1 (1000000 bytes).
! status returns the returned value from cio_set_file_ext. (0=ok)
!
! The following "set" functions allow you to "set" an element of the trcio
! structure, although some should not be used.  Example, don't set the "lun"
! or you could lose the connection to your file. !! 
! Both arguments are intent(in) and the result is an integer == trcio_ok or
! trcio_error
! The type of the "file" arg is type(trcio_struct), and the second arg is
! always the same as that found in the above-documented structure element
! of the same name.
!
!  function trcio_set_ipn(file,ipn) result(status)
!  function trcio_set_stdout(file,stdout) result (status)
!  function trcio_set_lun(file,lun) result(status)
!  function trcio_set_filename(file,filename) result(status)
!  function trcio_set_io_mode(file,io_mode) result(status)
!  function trcio_set_permission(file,permission) result(status)
!  function trcio_set_retention(file,retention) result(status)
!  function trcio_set_ftype(file,ftype) result (status)
!  function trcio_set_endian(file,endian) result (status)
!  function trcio_set_wtype(file,wtype) result (status)
!  function trcio_set_nbits(file,nbits) result (status)
!  function trcio_set_nbits_hd(file,nbits_hd) result (status)
!  function trcio_set_tmin(file,tmin) result (status)
!  function trcio_set_tmax(file,tmax) result (status)
!  function trcio_set_dt(file,dt) result (status)
!  function trcio_set_nhd1(file,nhd1) result (status)
!  function trcio_set_nhd2(file,nhd2) result (status)
!  function trcio_set_nhd3(file,nhd3) result (status)
!  function trcio_set_vwidth1(file,vwidth1) result (status)
!  function trcio_set_vwidth2(file,vwidth2) result (status)
!  function trcio_set_vwidth3(file,vwidth3) result (status)
!  function trcio_set_num_values(file,num_values) result (status)
!  function trcio_set_vbin1(file,vbin1) result (status)
!  function trcio_set_vbin2(file,vbin2) result (status)
!  function trcio_set_vbin3(file,vbin3) result (status)
!  function trcio_set_vmin1(file,vmin1) result (status)
!  function trcio_set_vmin2(file,vmin2) result (status)
!  function trcio_set_vmin3(file,vmin3) result (status)
!  function trcio_set_vmax1(file,vmax1) result (status)
!  function trcio_set_vmax2(file,vmax2) result (status)
!  function trcio_set_vmax3(file,vmax3) result (status)
!  function trcio_set_xorigin(file,xorigin) result (status)
!  function trcio_set_yorigin(file,yorigin) result (status)
!  function trcio_set_dx11(file,dx11) result (status)
!  function trcio_set_dx12(file,dx12) result (status)
!  function trcio_set_dx21(file,dx21) result (status)
!  function trcio_set_dx22(file,dx22) result (status)
!  function trcio_set_num_traces(file,num_traces) result (status)
!  function trcio_set_recl(file,recl) result (status)
!  function trcio_set_trmaxg(file,trmaxg) result (status)
!  function trcio_set_hist_start_pos(file,hist_start_pos) result (status)
!  function trcio_set_hist_end_pos(file,hist_end_pos) result (status)
!  function trcio_set_data_start_pos(file,data_start_pos) result (status)
!  function trcio_set_data_end_pos(file,data_end_pos) result (status)
!
!  The following "get" functions always return the structure element of the
!  same name, in its native data type, or they will return clearly bad data if
!  the file structure is not associated.
!
!  function trcio_get_number_traces(file) result (number_traces)
!  function trcio_get_trmaxg(file) result (trmaxg)
!  function trcio_get_lun(file) result(lun)
!  function trcio_get_filename(file) result(filename)
!  function trcio_get_io_mode(file) result(io_mode)
!  function trcio_get_permission(file) result(permission)
!  function trcio_get_retention(file) result(retention)
!  function trcio_get_ftype(file) result (ftype)
!  function trcio_get_endian(file) result (endian)
!  function trcio_get_wtype(file) result (wtype)
!  function trcio_get_nbits(file) result (nbits)
!  function trcio_get_nbits_hd(file) result (nbits_hd)
!  function trcio_get_tmin(file) result (tmin)
!  function trcio_get_tmax(file) result (tmax)
!  function trcio_get_dt(file) result (dt)
!  function trcio_get_nhd1(file) result (nhd1)
!  function trcio_get_nhd2(file) result (nhd2)
!  function trcio_get_nhd3(file) result (nhd3)
!  function trcio_get_vwidth1(file) result (vwidth1)
!  function trcio_get_vwidth2(file) result (vwidth2)
!  function trcio_get_vwidth3(file) result (vwidth3)
!  function trcio_get_num_values(file) result (num_values)
!  function trcio_get_vbin1(file) result (vbin1)
!  function trcio_get_vbin2(file) result (vbin2) 
!  function trcio_get_vbin3(file) result (vbin3) 
!  function trcio_get_vmin1(file) result (vmin1)
!  function trcio_get_vmin2(file) result (vmin2) 
!  function trcio_get_vmin3(file) result (vmin3) 
!  function trcio_get_vmax1(file) result (vmax1)
!  function trcio_get_vmax2(file) result (vmax2) 
!  function trcio_get_vmax3(file) result (vmax3) 
!  function trcio_get_xorigin(file) result (xorigin)
!  function trcio_get_yorigin(file) result (yorigin)
!  function trcio_get_dx11(file) result (dx11)
!  function trcio_get_dx12(file) result (dx12)
!  function trcio_get_dx21(file) result (dx21)
!  function trcio_get_dx22(file) result (dx22)
!  function trcio_get_num_traces(file) result (num_traces)
!  function trcio_get_recl(file) result (recl)
!  function trcio_get_hist_start_pos(file) result (hist_start_pos)
!  function trcio_get_hist_end_pos(file) result (hist_end_pos)
!  function trcio_get_data_start_pos(file) result (data_start_pos)
!  function trcio_get_data_end_pos(file) result (data_end_pos)
!
!
!-------------------------------------------------------------------------------
!</calling_doc>
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!-------------------------------------------------------------------------------
!</advice_doc>
!
!TRCIO is for TRACE data!  It expects a header and trace to be passed in or out.
!
!TRCIO supports:
!            Native IEEE format or BYTE-SWAPPED IEEE format
!
!            IBM format
!
!            LBO format (Localized Bit Optimization)
!                        Each trace is divided into non-overlapping windows
!                         that are (typically) 20 samples in length.  Samples
!                         within each window are stored as integers that
!                         are scaled such that they fully utilize the chosen
!                         bit precision (6 - 20).  A floating point scale
!                         factor is also stored, allowing the trace segments to
!                         be recombined during the read step to yield the
!                         original trace amplitudes, except for the roundoff as
!                         per the selected precision.
!
!            LBO2        Same as LBO but handles FNILs. (null values)
!
!            INT format  16 or 8 bit words, expects to find a scale factor
!                         in the HDR_LAV word of the header which it will 
!                         either WRITE to on output, or READ from on input.
!                         The scale is the original trace LAV divided into
!                         an nbits-controlled factor, as follows:
!                           scale = (2**(nbits-1) - 1)/MAXVAL(ABS(trace))
!                        If INT format in 8-bit segy, will look in segy
!                         header trwf (169-170) first for scaling info.  If
!                         there is nothing in trwf, it will get info from LAV
!                         header as discribed above.
!
!            CMPI format 16 or 8 bit words, same as INT except that the trace
!                         is compressed BEFORE SCALING on write, and is
!                         unscaled and then decompressed after reading.
!                         Default compression is:
!                           trace = sign(1.0,trace)*trace**(1/4)
!
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
! 92. 2009-02-05  Bill Menger  Corrected the ability to write SU files for
!                              compatibility with the seismic unix system.
!                              Added ability to read Javaseis files.
! 91. 2009-01-29  Bill Menger  Modified trcio_get_number_traces to return 0 if
!                              queried when writing a file but before writing
!                              any data.  Previously this rare question would
!                              return a number based on the file size divided by
!                              the trace record length, truncated.
!                              Modified trcio_tell_trace for same condition.
!                              Added ability to randomly write traces in 2D arrays.
!                              Modified the trace_write call to make sure that
!                              the num_traces is updated in the file structure.
!                              similar to the ability with 1D arrays. (optional args)
! 90. 2008-08-07  Bill Menger  Changed trcio_open for the append modes so that
!                              files are always opened with cpsio and not just
!                              with cio.  This keeps the cpsio structure full
!                              for subsequent queries to the file header.
! 89. 2008-08-05  Bill Menger  Modified trcio_open to make the code more simple,
!                              extracted code to create a new function called
!                              trcio_readheader, and modified trcio_close to 
!                              not be able to reference a null pointer when an
!                              error in calling is made.
! 88. 2007-11-27  Bill Menger  Added a setter function for extent size of the
!                              next file that is opened with TRCIO or CIO.
! 87. 2007-02-15  Bill Menger  Changed to only warn if duplicate job names found
!                              between this job and a previous one, or on any 
!                              other error that might occur while putting hist-
!                              ory records into the memory structure 'card'.
! 86. 2007-02-13  Bill Menger  Modified documentation to reflect arguments added
!                              but not documented for "calledFrom" and "lbo_version"
!                              on trcio_open call.
! 85. 2007-01-30  Bill Menger  Changed order of tests for file types so that test
!                              for SEGY is prior to test for SU.
! 84. 2006-01-25  Bill Menger  Removed txfr_module use statement.
! 83. 2006-08-29  D. Glover    Added NULLIFY statements for Intel compiler.
! 82. 2006-07-18  Bill Menger  Added SEGY 4-byte INTEGER reader capability.
! 81. 2006-06-06  SMCook       Fixed problem in trcio_open call causing mixup
!                               of header information vs. actual trace type.
! 80. 2006-05-25  SMCook/Menger  Removed cmprio support.
!                              Added support for LBO "version 2" -- supports
!                               datasets that intentionally use FNILs (e.g. to
!                               mark salt bodies).
!                              Removed requirement for trcio to deal with the
!                               lbo bit mask array -- this is now handled by
!                               lbo_crou.c.
! 79. 2005-10-10  Bill Menger  Modified to error-out quickly on write errors in
!                              the header-writing routines.
! 78. 2005-06-14  Bill Menger  Modified printout for trace errors.
! 77. 2005-05-10  Bill Menger  Fixed long-standing bug in write_trace that
!                              now allows bad write status to be passed up to
!                              the calling function.
! 76. 2005-04-19  Goodger      Add calledFrom argument to create routine so
!                              it will write history tags if called from
!                              promax.
! 75. 2005-02-22  Bill Menger  In trcio_write_trace_1d I changed the code to
!                              NOT do a seek if not needed AND to update the
!                              file structure for data_end_pos after each
!                              trace write operation.  The info is not added
!                              to the file header until the file is closed.
! 74. 2004-12-15  SMCook       Fixed data corruption for case when job trace
!                               length doesn't match trace length of LBO input.
! 73. 2004-09-16  RSDay        Fix problem with SU header translation.
!                              More robust SU recognition.
! 72. 2004-08-31  RSDay        Added SU file rcognition and read capability.
!                 Menger       Took out "kind(1)" from segy_read_hd
! 71. 2004-08-23  SMCook       Added error status logic for lbo_compress_trace,
!                              and documentation for user that LBO does not
!                              handle FNILs.
! 70. 2004-06-29  SMCook       Added call to trcio_recl immediately after LBO
!                              header read in trcio_open.  Without this, the
!                              variable file%lbo_recl doesn't get set properly
!                              for the read case.  TRIN fortuitously worked
!                              anyway because it was doing a header dump that
!                              called trcio_recl at an opportune stage (a stage
!                              at which ftype == LBO is true).  The bug showed
!                              up while running CBYT because CBYT has no reason
!                              to make the header dump call.
! 69. 2004-04-22  Bill Menger  Replace bad sample zero'er to return FNILs.
! 68. 2004-04-20  Bill Menger  Fixed bad trace indicator.  was off by 1 trc.
! 67. 2004-04-06  SMCook       Added LBO trace compression format.
! 66. 2004-02-06  R. Selzler   Improved segy recognition of format variations.
! 65. 2004-02-05  R. Selzler   Checksum printout shows every bad trace.
!                                Bill Menger's code to recover bad files.
! 64. 2004-01-27  R. Selzler   Added support for GeoEnergy compression
!                              using wavelet dynamic range (WAVE_DR).
! 63. 2004-01-08  R. Selzler   Replaced txfr and transfer with cmem.
! 62. 2003-07-31  Bill Menger  Added code to calculate num_traces if missing.
! 61. 2002-10-07  R.S.Day      trcio_read_trace_1d returns TRCIO_ERROR if the
!                              hd_ptr is less than 64 for a SEGY file.
! 60. 2002-09-20  K. Goodger   Add process number as argument to routine
!                              trcio_write_history_cards.
! 59. 2002-08-12  Ed Schmauch  Removed optional arguments extsize and alloc_disk
!                              from trcio_open; calling code must use
!                              cio_set_file_ext_size and
!                              cio_set_file_space_commit.
! 58. 2002-04-18  Ed Schmauch  Delete truncating file size in trcio_close.
!                              Added a type cmprio_common_with_trcio_struct
!                              to trcio_struct and removed elements from
!                              trcio_struct that are in
!                              cmprio_common_with_trcio_struct.
! 57. 2002-02-04  Ed Schmauch  When opening a compressed file for read,
!                              populate trcio_struct as possible from
!                              compressed file ascii header.
! 56. 2002-01-21  Ed Schmauch  Modified trcio_read_trace_1d to always set
!                              the checksum header, 32, to 0.0.  This is
!                              necessary because a checksum may have been
!                              set in header 32, then in another job the file
!                              read by an older software version that didn't
!                              have checksum.  The output of this second job
!                              will be a file without CHECKSUM set in the
!                              ASCII header but with illegal floating numbers
!                              in header 32.  Changed error flags in
!                              trcio_read_trace_1d back to TRCIO_EOF as needed.
!                              Added Checksum and included cmprio operations
!                              so that cmprio does not need to be called from
!                              outside of trcio.  Checked for SEGY operations.
! 55. 2001-11-06  Ed Schmauch  Removed all Conoco extensions to segy format.
!                              Removed use of user headers for segy headers.
! 54. 2001-10-24  Ed Schmauch  Eliminated writting ebcdic.dat from
!                              trcio_read_segy_headers.
!                              Changed trcio_read_history_cards to use
!                              cpsio_number_cards instead of
!                              cpsio_number_records.
! 53. 2001-10-16  Ed Schmauch  Made more changes to compile on Intel compiler.
! 52. 2001-10-16  Ed Schmauch  Fix bug in trcio_headerdump that kept code
!                              from compiling on Intel compiler.
! 51. 2001-08-27  K. Goodger   Fix documentation on read_history_cards and 
!                              write_history_cards.
! 50. 2001-08-03  K. Goodger   Replace read_history with read_history_cards.
!                              Replace write_history with write_history_cards.
!                              Replace history module with manhist module.
! 49. 2001-06-05  Ed Schmauch  If ampl_max is used in trcio_write_trace,
!                              clip trmaxg to ampl_max.  If (tr_alloc) in
!                              trcio_read_trace, copy tr_ptr to tr after
!                              unscaling and uncompressing.
! 48. 2001-05-25  K. Goodger   In history_read, insure we do not index outside
!                              of buff array.  Allow numcards to be the total
!                              number of cards in the entire history.
! 47. 2001-05-15  Ed Schmauch  Fixed amplitude recovery for Conoco 8-bit segy
!                              that I had broken when I added Landmark 8-bit
!                              segy.
! 46. 2001-05-14  K. Goodger   Set numcards to maxcards if it is > maxcards.
!                              Increase maxcards from 300 to 400.
! 45. 2001-05-03  Ed Schmauch  Added support for reading Landmark 8-bit segy.
!                              Landmark 8-bit segy is scaled with segyhd%trwf.
! 44. 2001-04-30  Ed Schmauch  Added ampl_max.
! 43. 2001-04-23  Ed Schmauch  Changed byte-output back to signed format.
! 42. 2001-03-23  Bill Menger  Changed byte-output to unsigned format.
! 41. 2001-03-15  Bill Menger  Added variable length string args for set fntns.
!                              Set trcio vile version = 1.1
! 40. 2001-03-06  Bill Menger  Added qtrot trmaxg, Accessor functions.
! 39. 2001-02-28  Bill Menger  Modified the header creation and update funct.
!                              Removed the "table" section from the file.
! 38. 2001-02-21  Bill Menger  Added history optional arg to open call, moved
!                              the "write_history" to inside trace write just
!                              before first trace is written
!                              * made public two functions: 
!                                trcio_determine_ftype - returns file type.
!                                trcio_write_section   - write header section.
! 37. 2001-02-07  R.S.Day      Fixed bug in card count in 1st history record.
!                              Problem caused by revision 36.
! 36. 2001-01-31  R.S.Day      Altered trcio_write_history so that the project
!                              history of the first history record reflects
!                              what is in the output file.
! 35. 2001-01-11  Bill Menger  Modified read-history, dumpheader.
! 34. 2000-12-14  Bill Menger  Added new function to test for file type more
!                              cleanly.
! 33. 2000-12-12  Bill Menger  Took out calls to cio_extsize() and replaced with
!                              a call to cio_get_file_extsize(file%lun).
!                              Fixed trace_write call and documentation so that
!                              traces aren't harmed when writing to disk.
! 32. 2000-11-27  Bill Menger  Added code to help open-appendmode of QTROT files
! 31. 2000-11-13  Bill Menger  Modified to use fortran interface instead of "c".
! 30. 2000-11-09  R.S.Day      Added optional custom header mapping of SEGY
! 29. 2000-10-26  Bill Menger  Bug fix on trcio_open
! 28. 2000-10-24  Bill Menger  Changed trcio_open to correctly handle QTROT in
!                              append mode.
! 27. 2000-10-17  Bill Menger  * Documentation (internal) change.
!                              * Modified so that files are able to be 
!                                closed as cpsio files and left as cio only when
!                                opening the file in append mode.  This frees up
!                                resources.
! 26. 2000-10-03  Bill Menger  Removed print statements on file open.
! 25. 2000-09-26  Bill Menger  Added functions: trcio_num_global_cards,
!                              trcio_write_globals, trcio_read_globals,
!                              added "clean_zero" function to trcio_read,
!                              added trcio_num_errors, trcio_num_traces-
!                              _with_errors functions, added printout to
!                              close upon having found errors in data.
!                              Also modified line 1252 because of f90 internal
!                              compiler bug. (now it is line 1254!)
! 24. 2000-09-15  R.S.Day      Corrected a logic bug in trcio_read_history.
!                              buff was not allocated large enough.
! 23. 2000-08-31  R.S.Day      Increased length of job_name to 16 bytes.
! 22. 2000-08-23  Bill Menger  *Modified so that opening in W+ mode positioned
!                               the file correctly upon writing first trace,
!                               and did not write bad qtrot/segy data on first
!                               trace.(bug fix)
! 21. 2000-08-17  Bill Menger  *Added trmaxg support.  
!                              *Added history support.
! 20. 2000-07-11  Bill Menger  Removed prints from TFIL support, and caused it
!                              to fail if it finds a "keyword" that is really
!                              from the QTROT file header.  otherwise it thinks
!                              it has a bad TFIL not a valid QTROT file.
! 19. 2000-07-07  Bill Menger  Added QTROT file support.
! 18. 2000-05-11  Bill Menger  Modified bf_flsz call to extsize call.
! 17. 2000-04-25  Bill Menger  Added remove function to trcio_close.
! 16. 2000-04-19  Bill Menger  Took out 2 print statements left in after the
!                              last surgery. oops.:<
! 15. 2000-04-19  Bill Menger  Found some "bugs" that work on Solaris & Absoft 
!                              but not on portland group.  Modified a pointer
!                              to the sectionlist to an allocatable array, and
!                              included a deallocate (to fix memory leak).
! 14. 2000-04-12  Bill Menger  Updated for 8 byte file positions.
! 13. 2000-03-08  Bill Menger  Took out the "smart" ibm float detector that 
!                              wasn't so smart!.
! 12. 2000-02-04  Bill Menger  Modified permission and added optional args to
!                              trcio_open
! 11. 2000-02-01  Bill Menger  Added functions to read HISTORY and TABLE headers
!                              and to put default dt,num_values into header if
!                              values are missing when file is opened.
! 10. 2000-01-27  Bill Menger  Minor mods to word types on tfils, added 
!                              code to insert sequence number if missing. 
!  9. 2000-01-21  Bill Menger  Added INT and CMPI word types, all necessary
!                              code to support them.
!  8. 2000-01-18  Bill Menger  Ensure hdr words for top/bottom mute are in
!                              range, calculate LAV on input and output.
!  7. 2000-01-11  Bill Menger  Added code to auto-detect ibm/ieee word types, 
!                              autodetection of endian in segy bin header is
!                              better, able to write segy traces in ibm flt on
!                              linux added.
!  6. 2000-01-07  Bill Menger  Modified trcio_update_num_traces to only work on
!                              ftype = 'TRCIO'
!                              Added more code to the update_header function.
!  5. 1999-12-27  Bill Menger  added scratch open feature.
!                              Modified for TROT documentation
!  4. 1999-12-09  Bill Menger  Added ident string and segy IEEE as well as IBM
!  3. 1999-12-08  Bill Menger  Added segy read/write (4-byte trace fmt only)
!  2. 1999-11-12  Bill Menger  Modified to use newer wrdc primitives.
!  1. 1999-10-07  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
! This process may require 4-byte (32-bit) word sizes.
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
! No special requirements.
!-------------------------------------------------------------------------------
!</compile_doc>
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!-------------------------------------------------------------------------------
!</algorithm_doc>
!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module trcio_module
  use cpsio_module
  use swap_module
  use wrdc_module
  use cio_module
  use named_constants_module
  use segy_module
  use sizeof_module
  use clean_module
  use manhist_module
  !use cmprio_module     ! (not used) This wrapped wavelet compressed data clusters
  use lbo_module         ! Localized bit optimization format helper
  use cmem_module
  use ll_module          ! Creates long-long word types for offsets in large files
  use string_module
  use jsf_wrapper_module ! wraps the javaseis file format system

  implicit none

  private

  public:: trcio_writeheader,trcio_write_header
  public:: trcio_headerdump,trcio_print_header

  public:: trcio_struct
  public:: trcio_open
  public:: trcio_close
  public:: trcio_read_trace
  public:: trcio_write_trace
  public:: trcio_update_num_traces
  public:: trcio_update_header
  public:: trcio_readheader
  public:: trcio_seek_trace
  public:: trcio_tell_trace
  public:: trcio_read_history_cards,trcio_write_history_cards

  public:: trcio_get_number_traces
  public:: trcio_num_global_cards,trcio_write_globals, trcio_read_globals

  public:: trcio_num_errors
  public:: trcio_num_traces_with_errors

  public:: trcio_set_ipn
  public:: trcio_set_stdout
  public:: trcio_set_map

  public:: trcio_write_section
  public:: trcio_determine_ftype

  public:: trcio_get_lun
  public:: trcio_get_filename
  public:: trcio_get_io_mode
  public:: trcio_get_retention
  public:: trcio_get_permission

  private:: trcio_set_lun
  private:: trcio_set_filename
  private:: trcio_set_io_mode
  public:: trcio_set_retention
  private:: trcio_set_permission
  public:: trcio_set_ext_size

  public:: trcio_set_ftype
  public:: trcio_get_ftype

  public:: trcio_set_endian
  public:: trcio_get_endian

  public:: trcio_set_wtype
  public:: trcio_get_wtype

  public:: trcio_set_nbits
  public:: trcio_get_nbits

  public:: trcio_set_nbits_hd
  public:: trcio_get_nbits_hd

  public:: trcio_set_tmin
  public:: trcio_get_tmin

  public:: trcio_set_tmax
  public:: trcio_get_tmax

  public:: trcio_set_dt
  public:: trcio_get_dt

  public:: trcio_set_nhd1
  public:: trcio_get_nhd1

  public:: trcio_set_nhd2
  public:: trcio_get_nhd2

  public:: trcio_set_nhd3
  public:: trcio_get_nhd3

  public:: trcio_set_vwidth1
  public:: trcio_get_vwidth1

  public:: trcio_set_vwidth2
  public:: trcio_get_vwidth2

  public:: trcio_set_vwidth3
  public:: trcio_get_vwidth3

  public:: trcio_set_num_values
  public:: trcio_get_num_values

  public:: trcio_set_vbin1
  public:: trcio_get_vbin1

  public:: trcio_set_vbin2
  public:: trcio_get_vbin2

  public:: trcio_set_vbin3
  public:: trcio_get_vbin3

  public:: trcio_set_vmin1
  public:: trcio_get_vmin1

  public:: trcio_set_vmin2
  public:: trcio_get_vmin2

  public:: trcio_set_vmin3
  public:: trcio_get_vmin3

  public:: trcio_set_vmax1
  public:: trcio_get_vmax1

  public:: trcio_set_vmax2
  public:: trcio_get_vmax2

  public:: trcio_set_vmax3
  public:: trcio_get_vmax3

  public:: trcio_set_xorigin
  public:: trcio_get_xorigin

  public:: trcio_set_yorigin
  public:: trcio_get_yorigin

  public:: trcio_set_dx11
  public:: trcio_get_dx11

  public:: trcio_set_dx12
  public:: trcio_get_dx12

  public:: trcio_set_dx21
  public:: trcio_get_dx21

  public:: trcio_set_dx22
  public:: trcio_get_dx22

  public:: trcio_set_num_traces
  public:: trcio_get_num_traces

  public:: trcio_set_recl
  public:: trcio_get_recl

  public:: trcio_set_trmaxg
  public:: trcio_get_trmaxg

  public:: trcio_set_hist_start_pos
  public:: trcio_get_hist_start_pos

  public:: trcio_set_hist_end_pos
  public:: trcio_get_hist_end_pos

  public:: trcio_set_data_start_pos
  public:: trcio_get_data_start_pos

  public:: trcio_set_data_end_pos
  public:: trcio_get_data_end_pos

  public:: trcio_set_globals

  public:: TRCIO_OK, TRCIO_ERROR, TRCIO_EOF

  character(len=100),public,save :: trcio_ident = &
  '$Id: trcio.f90,v 1.90 2008/08/07 14:45:25 mengewm Exp $'

  integer,parameter     :: TRCIO_OK    = CPSIO_OK
  integer,parameter     :: TRCIO_ERROR = CPSIO_ERROR
  integer,parameter     :: TRCIO_EOF   = CPSIO_EOF
 
  integer,parameter     :: LBO_SAMPSPERPACK = 20   ! do not change this value

  !
  ! -- added cmprio_common_with_trcio_struct here because I am removing
  ! -- cmprio module from the system. wmm 2006/05/18
  public :: cmprio_common_with_trcio_struct 

  type   :: cmprio_common_with_trcio_struct
    !sequence
    character(len=8)                         :: history ! ALL BRIEF CURRENT NONE
    integer                                  :: stdout  ! standard output unit.
    integer                                  :: ipn     ! process number
    double precision                         :: xorigin
    double precision                         :: yorigin
    double precision                         :: dx11
    double precision                         :: dx12
    double precision                         :: dx21
    double precision                         :: dx22
    integer                                  :: num_global_cards
    character(len=80),pointer,dimension(:)   :: global_cards
  end type cmprio_common_with_trcio_struct

! !M! in first 3 columns denotes field that was moved to
! cmprio_common_with_trcio_struct.
! ehs
!
  type :: trcio_struct 
    type (cmprio_common_with_trcio_struct) :: common
    integer             :: lun            ! unit number of this file as trcio sees it.
    integer             :: alt_lun        ! unit number for JSEIS subroutines
    integer(kind=8)     :: last_jseis_pos ! long trace number for javaseis file pointer
    character(len=160)  :: filename
    character(len=2)    :: io_mode
    integer             :: retention      ! 9999
    character(len=16)   :: permission     ! 'rwxrwxrwx'
    character(len=5)    :: ftype   ! TRCIO, LBO, LBO2, SEGY, VOXET, SU, QTROT, or JSEIS
    integer             :: endian         ! 0      ! 0(intel,dec) or 1(sun)
    character(len=4)    :: wtype          ! IEEE or IBM or IBM2 or BYTE or SBYT
    integer             :: nbits          ! 32     ! 1 - 64 bits/sample
    integer             :: nbits_hd       ! 64     ! 32, 64 bits/header word.
    real                :: tmin           ! 0.0    ! mintime of trace in seconds
    real                :: tmax           ! 0.0    ! maxtime of trace in seconds
    real                :: dt             ! 0.0    ! sample rate in seconds.
    integer             :: num_dimensions ! Added this for JSEIS data type.
    integer             :: nhd1           ! 8      ! primary bin coordinate
    integer             :: nhd2           ! 7      ! secondary bin coordinate
    integer             :: nhd3           ! 6      ! tertiary bin coordinate
    real                :: vwidth1        ! 1.0    ! width of bin (nhd1 )
    real                :: vwidth2        ! 1.0    ! " for nhd2.
    real                :: vwidth3        ! 1.0    ! " for nhd3.
    integer             :: nwih           ! 64     ! number words in header
    integer             :: num_values     ! 0      ! nint((tmax-tmin)/dt+1) or
                                          !          0 if(tmax<tmin
    real                :: vbin1          ! 1.0    ! center of bin (nhd1 units)
    real                :: vbin2          ! 1.0    ! center of bin (nhd2 units)
    real                :: vbin3          ! 1.0    ! center of bin (nhd3 units)
    real                :: vmin1          ! vbin1 - 0.5 * vwidth1
    real                :: vmin2          ! vbin1 - 0.5 * vwidth2
    real                :: vmin3          ! vbin1 - 0.5 * vwidth3
    real                :: vmax1          ! vbin1 + 0.5 * vwidth1
    real                :: vmax2          ! vbin2 + 0.5 * vwidth2
    real                :: vmax3          ! vbin3 + 0.5 * vwidth3
!M! double precision    :: xorigin        ! 
!M! double precision    :: yorigin        ! 
!M! double precision    :: dx11           ! 
!M! double precision    :: dx12           ! 
!M! double precision    :: dx21           ! 
!M! double precision    :: dx22           ! 
    integer             :: recl                    ! bytes per record (hdr+tr)
    integer             :: num_traces              ! # of traces on the file.
    integer             :: data_start_pos(2) ! where is the starting trace?
    integer             :: data_end_pos(2)   ! last pos(2) of last trace.
    integer             :: hist_start_pos(2) ! where is the starting history?
    integer             :: hist_end_pos(2)   ! end of last history.
    logical             :: first_trace_written
    logical             :: first_trace_read
!M! character(len=8)    :: history ! ALL BRIEF NONE CURRENT
    logical             :: history_written
    logical             :: header_written
!M! integer             :: stdout
!M! integer             :: ipn
    double precision    :: trmaxg
    integer             :: num_errors
    integer             :: num_traces_with_errors
    integer             :: prt_err_cnt

    character(len=8)    :: mod_segy
    integer             :: nummap    !number of segy headers to special map
    integer             :: sbyte(64) !byte location of segy header,from 1
    integer             :: bytes(64) !number of bytes in segy header
    integer             :: cps_hdr(64) !map to this cps header word
    character(len=8)    :: mtype(64) !word type of the mapped segy header

    logical             :: use_ampl_max
    real                :: ampl_max
    character(len=8)    :: checksum !'CHECKSUM'|'CRC'|'NONE'
    integer             :: last_trace_read

    integer                              :: lbo_version
    integer,pointer                      :: lbo_ibuf(:)
    integer                              :: lbo_recl   ! excludes header bytes
  end type trcio_struct

  interface trcio_write_header
    module procedure trcio_writeheader
  end interface

  interface trcio_print_header
    module procedure trcio_headerdump
  end interface

  interface trcio_add_hdr_elem
    module procedure trcio_add_hdel_i
    module procedure trcio_add_hdel_r
    module procedure trcio_add_hdel_d
    module procedure trcio_add_hdel_c
    module procedure trcio_add_hdel_i2
  end interface

  interface trcio_update_hdr_elem
    module procedure trcio_upd_hdel_i
    module procedure trcio_upd_hdel_r
    module procedure trcio_upd_hdel_d
    module procedure trcio_upd_hdel_c
    module procedure trcio_upd_hdel_i2
  end interface

  interface trcio_update_header
    module procedure trcio_update_header_private
  end interface

  interface trcio_update_num_traces
    module procedure trcio_update_header_private
  end interface

  interface trcio_write_trace
    module procedure trcio_write_trace_1d
    module procedure trcio_write_trace_2d
  end interface

  interface trcio_read_trace
    module procedure trcio_read_trace_1d
    module procedure trcio_read_trace_2d
  end interface

  interface
    function segy_is_file_segy_c(unit) result (status)
      integer, intent(in) :: unit
      integer             :: status
    end function segy_is_file_segy_c
  end interface

  contains

  function trcio_set_ext_size(ext_size) result (status)
    integer, intent(in)   ::  ext_size
    integer               ::  status
    !-------local
    integer, dimension(2) ::  size
    integer               ::  local_ext_size

    !--- trcio uses cio which uses pfio which sets (in pfio.h) a constant
    !--- DEFAULT_EXTENT_SIZE = 256000000 ... we will need to use this to
    !--- determine how to pass in the numbers to set the file ext_size.
 
    local_ext_size=max(ext_size,1)

    size(1)  = local_ext_size/256 ! number of DEFAULTs to set for high 4 bytes
    size(2)  = 1000000*mod(local_ext_size,256) ! number of BYTES for low 4 bytes

    !- Calculate new extent size based on this block size
    status = cio_set_file_ext_size(size)
    return
  end function trcio_set_ext_size

  function trcio_num_errors(file) result (num_errors)
    type(trcio_struct),pointer :: file
    integer                    :: num_errors
    if(associated(file) ) then
      num_errors = file%num_errors
    else
      num_errors = 0
    endif
  end function trcio_num_errors

  function trcio_num_traces_with_errors(file) result (num_traces_with_errors)
    type(trcio_struct),pointer :: file
    integer                    :: num_traces_with_errors
    if(associated(file) ) then
      num_traces_with_errors = file%num_traces_with_errors
    else
      num_traces_with_errors = 0
    endif
  end function trcio_num_traces_with_errors

  function trcio_create () result (file)
    type(trcio_struct),pointer :: file
    integer                        :: status
    allocate(file,stat=status)
    if(.not. associated(file) ) then
      nullify (file)
      return
    endif
    file%filename       = ' '
    file%retention      = 9999
    file%permission     = 'rw-r--r--'
    file%ftype          = 'TRCIO'
    file%endian         = swap_endian()
    file%wtype          = 'IEEE'
    file%nbits          = 32
    file%nbits_hd       = 64
    file%nwih           = 64
    file%num_values     = 0
    file%tmin           = 0.0
    file%tmax           = 0.0
    file%dt             = 0.0
    !--------- binning information
    file%nhd1           = 0
    file%nhd2           = 0
    file%nhd3           = 0
    file%vwidth1        = 0.0
    file%vwidth2        = 0.0
    file%vwidth3        = 0.0
    file%vbin1          = 0.0
    file%vbin2          = 0.0
    file%vbin3          = 0.0
    file%vmin1          = file%vbin1 - 0.5 * file%vwidth1
    file%vmin2          = file%vbin1 - 0.5 * file%vwidth2
    file%vmin3          = file%vbin1 - 0.5 * file%vwidth3
    file%vmax1          = file%vbin1 + 0.5 * file%vwidth1
    file%vmax2          = file%vbin2 + 0.5 * file%vwidth2
    file%vmax3          = file%vbin3 + 0.5 * file%vwidth3
    !---------- grid and origin information
    file%common%xorigin = 0.0
    file%common%yorigin = 0.0
    file%common%dx11    = 1.0 
    file%common%dx12    = 0.0
    file%common%dx21    = 0.0
    file%common%dx22    = 1.0
    file%recl           =trcio_recl(file)
    file%num_traces     =0
    file%data_start_pos = 0
    file%data_end_pos = 0
    file%hist_start_pos = 0
    file%hist_end_pos = 0
    file%first_trace_written = .false.
    file%first_trace_read    = .false.
    file%history_written     = .false.
    file%header_written      = .false.
    file%common%history      = '    NONE'
    file%common%stdout       = 6
    file%common%ipn          = 999
    file%trmaxg              = 0d0
    file%num_errors                = 0
    file%num_traces_with_errors    = 0
    file%prt_err_cnt               = 0
    !---------- custom header mapping for segy data
    file%mod_segy='NO'
    file%nummap = 0 !number of segy headers to special map
    file%sbyte  = 0 !starting byte location of segy header
    file%bytes  = 0 !number of bytes in segy header
    file%cps_hdr= 0 !map to this cps header word
    file%mtype(1:64)  = 'I'

    file%use_ampl_max = .false.
    file%checksum = '    NONE'
    file%last_trace_read = 0

    file%lbo_version = -1   ! must determine explicitly -- never assume
    nullify(file%lbo_ibuf)

  end function trcio_create

  subroutine trcio_delete(file)
  type(trcio_struct),pointer :: file

    if(.not. associated(file) ) go to 999

    if(associated(file%lbo_ibuf)) deallocate(file%lbo_ibuf)
    nullify(file%lbo_ibuf)
    if(associated(file) ) deallocate(file)

999 continue
    nullify(file)
  end subroutine trcio_delete

  subroutine trcio_set_map(file,mod_segy,nummap,sbyte,bytes,mtype,cps_hdr)
    !------ args ----
    type(trcio_struct),pointer                  :: file
    character(len=*),intent(in)                 :: mod_segy
    integer,intent(in)                          :: nummap
    integer,intent(in),dimension(:)             :: sbyte
    integer,intent(in),dimension(:)             :: bytes
    character(len=*),intent(in),dimension(:)    :: mtype
    integer,intent(in),dimension(:)             :: cps_hdr
    !------------
  
    file%mod_segy = mod_segy
    file%nummap = nummap
    file%sbyte(1:nummap) = sbyte(1:nummap)
    file%bytes(1:nummap) = bytes(1:nummap)
    file%mtype(1:nummap) = mtype(1:nummap)
    file%cps_hdr(1:nummap) = cps_hdr(1:nummap)
    
  end subroutine trcio_set_map

  function trcio_open(filename,io_mode,scratch, &
                      nwih,ndpt,nbits,nbitshd,history,&
                      compressed,&
                      srate,strt_val,snr,cmp,wdr,mtpc,calledFrom,lbo_version) &
           result (file)
    !-------- INPUT ARGUMENTS -----------------------
    character(len=*),intent(in)          :: filename
    character(len=*),intent(in)          :: io_mode
    !---------OPTIONAL INPUT ARGUMENTS --------------
    logical         ,intent(in),optional :: scratch
    integer         ,intent(in),optional :: wdr      ! dynamic range ratio
    integer         ,intent(in),optional :: nwih !num words in headers 
    integer         ,intent(in),optional :: ndpt!nsmp.number of samples/trc.
    integer         ,intent(in),optional :: nbits
    integer         ,intent(in),optional :: nbitshd
    character(len=*),intent(in),optional :: history  ! default=ALL
    logical         ,intent(in),optional :: compressed ! default = .no.
    real            ,intent(in),optional :: srate !(sample rate,
                                                  ! units understood by
                                                  ! the user.)
    real            ,intent(in),optional :: strt_val ! start val. of trc
    !                                            (tstart for time, init depth
    !                                             val for depth, ...)
    integer         ,intent(in),optional :: snr      ! signal/noise
    integer         ,intent(in),optional :: cmp      ! sizeout/sizein
    integer         ,intent(in),optional :: mtpc     ! max trc/cluster
    character(len=*),intent(in),optional :: calledFrom
    integer         ,intent(in),optional :: lbo_version
    !---------- we will need to put optional arguments here for JSEIS type
    !---------- 



    !----------- OUTPUT ARG -----------------------
    type(trcio_struct),pointer           :: file


    !--- local variables
    logical                        :: optional_parms
    integer                        :: status
    character(len=8)                         :: local_io_mode
    !--- private copies of optional arguments ---
    logical                                  :: private_scratch
    integer                                  :: private_nwih
    integer                                  :: private_wdr
    !character(len=8)                         :: private_history
    character(len=8)                         :: private_calledFrom
    integer                                  :: private_lbo_version
    logical                                  :: private_compressed
    real                                     :: private_srate
    real                                     :: private_strt_val
    integer                                  :: private_snr
    integer                                  :: private_cmp
    integer                                  :: private_mtpc
    !-----

    !-----------

    optional_parms = present(nwih).and.present(ndpt).and.present(nbits).and. &
                     present(nbitshd)


    if(present(calledFrom))then
      private_calledFrom=calledFrom
    else
      private_calledFrom=' '
    endif

    if(io_mode == 'w' ) then
      local_io_mode = 'w'
    else
      local_io_mode = io_mode
    endif

    if (present(scratch) ) then
      private_scratch = scratch
    else
      private_scratch = .false.
    endif

    if(present(lbo_version)) then
      private_lbo_version = lbo_version
    else
      private_lbo_version = -1
    endif

    file => trcio_create()

    if(.not.associated(file) ) then
      call trcio_print(file,'trcio_open: ERROR-> Could not create object.')
      go to 999
    endif

    !-- We now have a memory structure "file" that is allocated via trcio_create.

    !-- if only reading the file, we should load info from the structure and exit.

    !-- otherwise, move on.


    file%filename    = trim(filename)
    file%lun         = -1                   ! this forces errors if files not actually opened.
    file%alt_lun     = -1                   ! if a javaseis file JSEIS is open, this will be set.
    file%io_mode     = trim(local_io_mode)
    file%data_start_pos = 0
    file%data_end_pos   = 0
    file%num_traces     = 0
    file%hist_start_pos = 0
    file%hist_end_pos   = 0
    file%dt             = 0.0   ! SMCook - previously was left uninitialized
    file%tmin           = 0.0
    private_wdr =   50
    if(present(nwih))    file%nwih         = nwih
    if(present(ndpt))    file%num_values   = ndpt
    if(present(nbits))   file%nbits        = nbits
    if(present(nbitshd)) file%nbits_hd     = nbitshd
    if(present(strt_val))file%tmin         = strt_val
    if(present(srate))   then 
      file%dt           = srate
    endif
    if(file%dt /= 0.0 ) then
      private_srate    = file%dt
    else
      private_srate    = .004 ! default
    endif
    private_strt_val = file%tmin
    private_snr =   50
    private_cmp =    0
    private_mtpc= 2048
    private_nwih = file%nwih
    if(present(snr))   private_snr = snr
    if(present(cmp))   private_cmp = cmp
    if(present(wdr))   private_wdr = wdr
    if(present(mtpc)) private_mtpc = mtpc
    if(present(history)) file%common%history &
      = trim(history(:min(8,len(history))))
    if(present(compressed)) then
      private_compressed = compressed
    else
      if(present(snr) .or. present(cmp) .or. present(wdr) ) then
        private_compressed = .true.
      else
        private_compressed = .false.
      endif
    endif

    file%lbo_version = private_lbo_version
    if(private_lbo_version == 1) then
      file%ftype = 'LBO'
      file%wtype = 'LBO'
    elseif(private_lbo_version == 2) then
      file%ftype = 'LBO2'
      file%wtype = 'LBO2'
    endif

    select case (io_mode)
      case ('w','wn','w+')
        select case (io_mode)
        case ('w+')
          !-- file exists, read the header.
          file%ftype = trcio_determine_ftype(trim(filename))
          select case (file%ftype)
            case ('JSEIS')
              local_io_mode = 'rw' ! JSEIS only recognizes r and rw
              file%alt_lun = jsf_wrapper_getlun(filename,local_io_mode)
              if(file%alt_lun < 1 ) go to 999
            case default
          end select
          if( .NOT. private_compressed) then 
            status = cpsio_open(filename,trim(io_mode),file%lun,private_scratch)
            if(status < 0 ) go to 999
          endif
          status = trcio_readheader(file)
          if(status < 0 ) go to 999

        case default !--- this is a new file, we write the header!
          select case (file%ftype)
            case ('JSEIS')
print*,'trcio:1382: Create a javaseis file mode=',file%io_mode
!TODO -- add dimension calls here to set axes, etc.
!TODO -- Do I want to destroy the js dataset if they say "w" and it exists?
              local_io_mode = 'rw' ! JSEIS only recognizes r and rw
              file%alt_lun = jsf_wrapper_getlun(filename,local_io_mode)
print*,'trcio:1387: unit number = ',file%alt_lun,' io-mode=',local_io_mode,' Forcing failure.'
              if(file%alt_lun < 1 ) go to 999
status = TRCIO_ERROR
            case default
          end select
          if( .NOT. private_compressed) then 
            status = cpsio_open(filename,trim(io_mode),file%lun,private_scratch)
            if(status < 0 ) go to 999
          endif
          !status            = trcio_writeheader(file)
          !if(status /= TRCIO_OK) then
          !  call trcio_print(file,'trcio_open: ERROR-> Could not write header.')
          !  go to 999
          !endif
          if(private_calledFrom.eq.'promaxW')then
            call trcio_write_history_cards(file,'NONE')
          endif
        end select 

        if( private_compressed) then
          if((file%nbits .ne. 32 ) .or. (file%nbits_hd .ne. 64) ) then
            call &
              trcio_print(file,&
                'trcio_open: ERROR-> Wrong parms for a compressed file.')
            status = trcio_error ! wrong parameters (in case we add support back in)
          else
            status = trcio_error ! removed cmprio support 5/18/2006 wmm

!           file%lun=cmprio_open(file%common,&
!                    filename=file%filename,mode=file%io_mode,&
!                    strt_val=private_strt_val,&
!                    srate=private_srate,nwih=private_nwih,&
!                    nsmp=file%num_values, &
!                    snr=private_snr,cmp=private_cmp,&
!                    wdr=private_wdr,&
!                    mtpc=private_mtpc,&
!                    history=private_history, &
!                    scratch=private_scratch)
!           file%ftype='CMPR'
!           status = min(0,file%lun)

          endif
        endif

        if(status < 0 ) go to 999
        !-------------------------------------------------------------------------------
        !- wmm added the below line because file type is unknown if writing a file,
        !-     and this writes trcio header to segy, qtrot, tfil even when it shouldn't.
        !-     By rewinding, we don't confuse QTROT/SEGY files, which write the header
        !-     using different calls. (QTROT uses a trcio call, but SEGY uses the segy
        !-     module.
        !-------------------------------------------------------------------------------
        call cio_frewind(file%lun)
        !-------------------------------------------------------------------------------

        return     !--- OPENed for WRITE ... OK

      case ('a','an','a+')
        file%ftype = trcio_determine_ftype(trim(filename))
        select case (file%ftype)
            case ('JSEIS')
              local_io_mode = 'rw' ! JSEIS only recognizes r and rw
              file%alt_lun = jsf_wrapper_getlun(filename,local_io_mode)
              if(file%alt_lun < 1 ) go to 999
            case default
        end select
        if(file%ftype == 'LBO')  file%lbo_version = 1
        if(file%ftype == 'LBO2') file%lbo_version = 2

        if(file%ftype == 'UNK') then ! User has opened an unknown file type.  Rather than trash it, 
                                      ! close the file gracefully and let user know.
          if(io_mode == 'an') then    ! ... unless user used the 'an' specification.
            local_io_mode = 'w'       ! act like this is a new file opened for write.
          else
            go to 999 
          endif
        endif
        ! -- open the file with cpsio to load headers 
        status = cpsio_open(filename,trim(local_io_mode),file%lun,private_scratch)
        if(status /= cpsio_ok .or. file%lun <= 0 ) go to 999
        if(trcio_readheader(file) /= TRCIO_OK) go to 999

        !--- seek to end
        if(cio_fseek(file%lun,0,2) < 0 ) go to 999

        return       !----OPENed for APPEND ... OK
        

      case ('r','r+')           ! OPEN for READ any variation.

        file%ftype = trcio_determine_ftype(trim(filename))
        select case(file%ftype)
          case('UNK')
            go to 999
          case('LBO')
            file%lbo_version = 1
          case('LBO2')
            file%lbo_version = 2
          case('JSEIS')
            local_io_mode = 'r'
            status = cpsio_open(filename,trim(local_io_mode),file%lun,private_scratch)
            file%alt_lun = jsf_wrapper_getlun(filename,trim(local_io_mode))
            if(status /= cpsio_ok .or. file%lun <= 0 .or. file%alt_lun < 1) go to 999
          case default
        end select
        status = cpsio_open(filename,trim(local_io_mode),file%lun,private_scratch)
        if(status /= cpsio_ok .or. file%lun <= 0 ) go to 999
        if(trcio_readheader(file) /= TRCIO_OK) go to 999
        return   ! --> OPENed for READ ... OK

      case default
        call trcio_print(file,'trcio_open: ERROR-> bad I/O mode specified ('//io_mode//')')
        go to 999
    end select

999 continue ! error continuance
      call trcio_print(file,'trcio_open: ERROR-> Could not open file.')
      status = trcio_close(file)
      status = trcio_error

  end function trcio_open

  subroutine trcio_headerdump(file)
    type(trcio_struct),pointer :: file
    !-------------- locala variables 
    integer                    :: u,i
    type(segy_ebcdic_hdr)      :: ascii

    if(file%ftype == 'CMPR') then
      !call cmprio_dump_header(file%lun) removed cmprio support wmm 2006/05/18
      return
    endif

    u = file%common%stdout
    write(u,*)'Filename               : ', trim(file%filename)
    write(u,*)'Logical Unit Number    : ', file%lun
    write(u,*)'io_mode                : ', trim(file%io_mode)
    write(u,*)'file_type              : ', trim(file%ftype)
    write(u,*)'num_traces             : ', file%num_traces
    write(u,*)'tmin                   : ', file%tmin
    write(u,*)'tmax                   : ', file%tmax
    write(u,*)'dt                     : ', file%dt
    write(u,*)'ipn                    : ', file%common%ipn
    select case(file%ftype)
      case('JSEIS')
        write(u,*)'Alternate Unit Number  : ', file%alt_lun
        write(u,*)'num_dimensions         : ', file%num_dimensions
      case default
        !write(u,*)'retention              : ', file%retention
        write(u,*)'permission             : ', trim(file%permission)
        write(u,*)'endian                 : ', file%endian
        write(u,*)'word_type              : ', trim(file%wtype)
        write(u,*)'nbits_sample           : ', file%nbits
        write(u,*)'nbits_header_word      : ', file%nbits_hd
        !write(u,*)'nhd1                   : ', file%nhd1
        !write(u,*)'nhd2                   : ', file%nhd2
        !write(u,*)'nhd3                   : ', file%nhd3
        !write(u,*)'vwidth1                : ', file%vwidth1
        !write(u,*)'vwidth2                : ', file%vwidth2
        !write(u,*)'vwidth3                : ', file%vwidth3
        write(u,*)'nwih                   : ', file%nwih
        write(u,*)'num_values             : ', file%num_values
        !write(u,*)'vbin1                  : ', file%vbin1
        !write(u,*)'vbin2                  : ', file%vbin2
        !write(u,*)'vbin3                  : ', file%vbin3
        !write(u,*)'vmin1                  : ', file%vmin1
        !write(u,*)'vmin2                  : ', file%vmin2
        !write(u,*)'vmin3                  : ', file%vmin3
        !write(u,*)'vmax1                  : ', file%vmax1
        !write(u,*)'vmax2                  : ', file%vmax2
        !write(u,*)'vmax3                  : ', file%vmax3
        write(u,*)'xorigin                : ', file%common%xorigin
        write(u,*)'yorigin                : ', file%common%yorigin
        write(u,*)'dx11                   : ', file%common%dx11    
        write(u,*)'dx12                   : ', file%common%dx12    
        write(u,*)'dx21                   : ', file%common%dx21    
        write(u,*)'dx22                   : ', file%common%dx22    
        file%recl = trcio_recl(file)
        write(u,*)'record_length          : ', file%recl
        write(u,*)'data_start_pos         : ', file%data_start_pos
        write(u,*)'data_end_pos           : ', file%data_end_pos
        write(u,*)'hist_start_pos         : ', file%hist_start_pos
        write(u,*)'hist_end_pos           : ', file%hist_end_pos
        !write(u,*)'first_trace_written    : ', file%first_trace_written
        !write(u,*)'first_trace_read       : ', file%first_trace_read
        !write(u,*)'history_written        : ', file%history_written
        write(u,*)'history                : ', trim(file%common%history)
        !write(u,*)'stdout                 : ', file%common%stdout
        write(u,*)'trmaxg(max LAV of file): ', file%trmaxg
        write(u,*)'Checksum_type          : ', trim(file%checksum)
        write(u,*)'Last Trace Read        : ', file%last_trace_read
    end select

    if(file%ftype == 'SEGY') then
      call cio_frewind(file%lun)
      if(segy_read_ebchdr(file%lun,ascii)  >= 0 ) then
        do i = 1, 40
          if(ascii%h(i)(80:80) == char(10)) ascii%h(i)(80:80) = " "
        end do
        do i = 1,40
          write(u,*)(trim(ascii%h(i)))
        end do
      else
        write(u,*)'SEGY Unable to read EBCDIC header.'
      endif
    endif

  end subroutine trcio_headerdump

  ! recl calculation modified March 2004 to handle LBO format
  function trcio_recl(file) result (recl)
    type(trcio_struct),pointer :: file
    integer                        :: recl,scrap

    if(file%ftype == 'QTROT' ) then
      file%recl = file%nwih+file%num_values
      if(modulo(file%recl,2) /= 0 ) file%recl = file%recl+1
      recl = file%recl*4

    elseif(file%ftype(1:3) == 'LBO') then

      recl = 0
      call lbo_get_recl_excluding_header( &
        file%lbo_version,file%num_values,LBO_SAMPSPERPACK,file%nbits,recl)

      file%lbo_recl = recl                       ! lbo_recl excludes header
      recl = recl + (file%nwih*file%nbits_hd)/8  !     recl includes header

    else
      scrap = mod(file%nwih*file%nbits_hd + &
                  file%num_values*file%nbits,8)      ! leftover bits.
      if (scrap > 0 ) scrap = 1                      ! save entire byte.
      recl = (file%nwih*file%nbits_hd+file%num_values*file%nbits)/8 + scrap

    endif

  end function trcio_recl

  function trcio_writeheader(file) result (status)
    type(trcio_struct),pointer :: file
    integer                        :: status
    ! LOCAL vars.
    character(len=81)              :: s
    integer                        :: u,ls

    status = TRCIO_OK ! start with good status.
    if(file%header_written) return
    file%header_written=.true.
    select case(file%ftype)
      case('JSEIS')
print*,'trcio:1638: JavaSeis File: Will not write the header.'
        return
      case('CMPR')
        return
      case('QTROT')
        status = trcio_write_qtrot_header(file)
        return
      case('SEGY')
        !---- what if we wanted to write a segy file ... would we do it here???
        file%data_start_pos = (/0,3600/)
        return
      case('TFILE')
        file%data_start_pos = (/0,1024/)
        return
      case default
    end select
    !---- end if
    u = file%lun  ! unit to write to.
    ls = len(s)   ! length of string to put
    status = cio_fflush(file%lun)
    if(status /= cio_ok) goto 999
    call cio_frewind(file%lun)
    ! write the header from structure "file" into file "file%lun"
    !--- Prolog
    if(file%ftype == 'LBO') then
      write(s,'(a)')'#<CPS_v1 type="LBO" version="1.1" encoding=binary />'
    elseif(file%ftype == 'LBO2') then
      write(s,'(a)')'#<CPS_v1 type="LBO2" version="1.1" encoding=binary />'
    else
      write(s,'(a)')'#<CPS_v1 type="TRCIO" version="1.1" encoding=binary />'
    endif
    if(cio_fputline(trim(s),len_trim(s),u) < 0 ) goto 999

    !--- History
    if(trcio_add_hdr_elem(u,'#<hdr_history>','(A)') < 0 ) goto 999
    if(trcio_add_hdr_elem(u,'# SKIP_BNK = ','(A13,A)','.false.') < 0 ) &
    goto 999
    if(trcio_add_hdr_elem&
    (u,'# hist_start_pos = (','(A20,2(i11,a1))',file%hist_start_pos) &
    < 0 ) goto 999
    if(trcio_add_hdr_elem&
    (u,'# hist_end_pos = (','(A18,2(i11,a1))',file%hist_end_pos) &
    < 0 ) goto 999
    if(trcio_add_hdr_elem(u,'#</history>','(A)') < 0 ) goto 999
   
    !--- Seismic
    !--- Figure out what is missing and put in the missing values.
    !--- If insufficient information is available, I will put in:
    !---     *dt = .004,
    !---     *num_values = 2,
    !--- in order to calculate the other values.
    if(file%dt > 0.0 ) then
      if    (file%tmax >  file%tmin .and. file%num_values >  0) then
        !----------------- everything should be ok --------------
      elseif(file%tmax >  file%tmin .and. file%num_values <= 0) then
        file%num_values = 1 + (file%tmax-file%tmin)/file%dt
      elseif(file%tmax <= file%tmin .and. file%num_values >  0) then
        file%tmax = file%tmin+file%dt*(file%num_values-1)
      elseif(file%tmax <= file%tmin .and. file%num_values <= 0) then
        file%num_values = 2
        file%tmax = file%tmin+file%dt*(file%num_values-1)
      endif 
    else
      if    (file%tmax >  file%tmin .and. file%num_values >  0) then
      !--- calculate dt
        file%dt = (file%num_values-1)/(file%tmax - file%tmin)
      elseif(file%tmax <= file%tmin .and. file%num_values >  0) then
        file%dt   = .004
        file%tmax = file%tmin+file%dt*(file%num_values-1)
      elseif(file%tmax >  file%tmin .and. file%num_values <= 0) then
        file%dt   = .004
        file%num_values = 1 + (file%tmax-file%tmin)/file%dt
      elseif(file%tmax <= file%tmin .and. file%num_values <= 0) then
        file%dt   = .004
        file%num_values = 2
        file%tmax = file%tmin+file%dt*(file%num_values-1)
      endif
    endif
    file%recl = trcio_recl(file)

    if(trcio_add_hdr_elem(u,'#<hdr_seismic>','(A)') < 0 ) goto 999
    if(trcio_add_hdr_elem(u,'# lun = ','(A8,i4)',file%lun) < 0 ) goto 999
    if(trcio_add_hdr_elem(u,'# io_mode = ','(A12,A)',file%io_mode) < 0 ) &
    goto 999
!    if(trcio_add_hdr_elem(u,'# retention = ','(A14,i7)',file%retention) < 0) &
!    goto 999
!    if(trcio_add_hdr_elem(u,'# permission = ','(A15,A)',file%permission) < 0) &
!    goto 999
    if(trcio_add_hdr_elem(u,'# ftype = ','(A10,A9)',file%ftype) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# endian = ','(A11,i1)',file%endian) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# wtype = ','(A10,A5)',file%wtype) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nbits = ','(A10,i7)',file%nbits) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nbits_hd = ','(A13,i7)',file%nbits_hd) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# tmin = ','(A9,f14.7)',file%tmin) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# tmax = ','(A9,f14.7)',file%tmax) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# dt = ','(a7,f14.7)',file%dt) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nhd1 = ','(A9,i2)',file%nhd1) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nhd2 = ','(A9,i2)',file%nhd2) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nhd3 = ','(A9,i2)',file%nhd3) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vwidth1 = ','(A12,f14.7)',file%vwidth1) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vwidth2 = ','(A12,f14.7)',file%vwidth2) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vwidth3 = ','(A12,f14.7)',file%vwidth3) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# nwih = ','(A9,i3)',file%nwih) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# num_values = ','(A15,i11)',file%num_values)<0) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vbin1 = ','(A10,f14.7)',file%vbin1) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vbin2 = ','(A10,f14.7)',file%vbin2) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vbin3 = ','(A10,f14.7)',file%vbin3) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmin1 = ','(A10,f14.7)',file%vmin1) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmin2 = ','(A10,f14.7)',file%vmin2) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmin3 = ','(A10,f14.7)',file%vmin3) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmax1 = ','(A10,f14.7)',file%vmax1) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmax2 = ','(A10,f14.7)',file%vmax2) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# vmax3 = ','(A10,f14.7)',file%vmax3) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# xorigin = ','(A12,g16.8)',file%common%xorigin) &
      < 0 ) &
        goto 999
    if(trcio_add_hdr_elem(u,'# yorigin = ','(A12,g16.8)',file%common%yorigin) &
      < 0 ) &
        goto 999
    if(trcio_add_hdr_elem(u,'# dx11 = ','(A9,g16.8)',file%common%dx11) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# dx12 = ','(A9,g16.8)',file%common%dx12) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# dx21 = ','(A9,g16.8)',file%common%dx21) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# dx22 = ','(A9,g16.8)',file%common%dx22) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# num_traces = ','(A15,i11)',file%num_traces)<0) &
    goto 999
    if(trcio_add_hdr_elem(u,'# record_length = ','(A18,i11)',file%recl) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem&
    (u,'# data_start_pos = (','(A20,2(i11,a1))',file%data_start_pos) &
    < 0 ) goto 999
    if(trcio_add_hdr_elem&
    (u,'# data_end_pos = (','(A18,2(i11,a1))',file%data_end_pos) &
    < 0 ) goto 999
    if(trcio_add_hdr_elem(u,'# trmaxg = ','(A11,E24.14e3)',file%trmaxg) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'# checksum = ','(A13,A)',file%checksum) < 0 ) &
    goto 999
    if(trcio_add_hdr_elem(u,'#</seismic>','(A)') < 0 ) &
    goto 999

    status = cio_fflush(file%lun)
    if(status /= cio_ok) goto 999
    status = TRCIO_OK
    return
    
999 status = TRCIO_ERROR

  end function trcio_writeheader

  integer function trcio_close(file,remove) result (status)
    type(trcio_struct),pointer :: file
    logical,intent(in),optional :: remove
    !--- local vars.
    logical                     :: local_remove

    if(present(remove)) then
      local_remove = remove
    else
      local_remove = .false.
    endif 
    ! not even memory allocated for this file... bad situation.
    if(.not. associated(file)) go to 999
    ! associated but not opened by cio/cpsio
    if(file%lun <= 0 ) go to 998

    !--- report errors found while reading ---
    if(file%num_errors > 0 ) then
      write(file%common%stdout,'(A,I11,A)') &

      'Notice (trcio): Read (and flagged) ',file%num_traces_with_errors,&
      ' bad traces from file '//trim(file%filename)//'.'

      write(file%common%stdout,'(A,I11,A)') &
      'Notice (trcio): Changed ',file%num_errors, &
      ' bad samples to FNIL from file '//trim(file%filename)//'.'

    endif
    select case (file%io_mode)
      case ('r','r+')
      case default
        !-- update structure as needed.
        !-- update the open file as needed.
        status = trcio_update_header(file)
    end select
    
    if(file%ftype == 'QTROT' .and. file%io_mode /= 'r' ) &
    status = trcio_writeheader(file)

    !-- close the file
    if (file%ftype == 'CMPR') then
      !status = cmprio_close(file%lun) removed cmprio support wmm 2006/05/18
      !--- re-open the file in read-only mode so we can remove it if desired
      !--- below.
      status = cpsio_open(trim(file%filename),'r',file%lun)
    endif

    !--- truncate file to exact size ---
    !   call cio_flsz(trim(file%filename),file_size)
    !   status = cio_truncate(trim(file%filename),file_size) 
    !--- now you may close the file.

    status = cpsio_close(file%lun,local_remove)
    if(file%ftype == 'JSEIS') then
      status = jsf_wrapper_close(file%alt_lun)
    endif
    
    !-- remove memory block for file struct.
998 continue
    call trcio_delete(file)
    return
999 nullify(file)
    return

  end function trcio_close

  integer function trcio_read_trace_1d(file,hd,tr,tnum) result (status)
    type(trcio_struct),pointer                        :: file
    double precision ,intent(out),dimension(:),target :: hd
    real             ,intent(out),dimension(:),target :: tr
    integer, optional,intent(in)                      :: tnum
    ! pointers
    real,dimension(:),pointer                         :: tr_ptr
    integer,dimension(:),pointer                      :: itr_ptr
    double precision, dimension(:),pointer            :: hd_ptr
    integer,dimension(:),pointer                      :: hd_tr_int
    ! local
    !----------- optional word sizes for the trace/header on disk.-------
    real             ,dimension(:),allocatable        :: hdr ! r*4 header
    integer          ,dimension(:),allocatable        :: hdi ! i*4 header
    double precision ,dimension(:),allocatable        :: tr_dblptr ! r*8 trace
    !------------ i is scratch integer for loops, twsiz = trace word size,
    !------------ hwsiz is header word size
    integer                                           :: i,twsiz,hwsiz,mystat
    integer                                           :: tlen,hlen
    logical                                           :: swap,ibmswap
    logical                                           :: swapsu
    logical                                           :: tr_alloc,hd_alloc
    logical                                           :: hd_tr_int_alloc
    integer                                           :: recl_bytes,recl_word4
    integer,dimension(2)                              :: checksum
    !--- added for segy
    type(segy_trc_hdr)                                :: syh
    real                                              :: tscle
    integer                                           :: nbadval
    integer                                           :: iwdtype(64)
    integer                                           :: trace_number
    logical                                           :: header_bad,trace_bad
    integer                                 :: lbo_samps_per_pack!,lbo_version

    
    status = TRCIO_OK
    select case(file%ftype)
      case ('CMPR')
        status = trcio_error
        ! --- removed cmprio support 5/18/2006 wmm
        !if(present(tnum) ) then
        !  trnum                = tnum
        !else
        !  trnum = file%last_trace_read + 1
        !endif
        !status = cmprio_read_hdtrc (file%lun,size(hd),size(tr),hd,tr,trnum) 
        !status = status - 1 ! cmprio returns 1 for 1 trace successfully read.
        !file%last_trace_read = trnum
        return
      case ('JSEIS')
        if(present(tnum)) then
          file%last_jseis_pos = tnum
          file%last_trace_read = tnum
        else
          file%last_jseis_pos  = file%last_jseis_pos + 1
          file%last_trace_read = file%last_jseis_pos
        endif
        if(trcio_seek_trace(file,file%last_trace_read) /= 0 ) then
          status        = TRCIO_EOF
        else
          tlen = jsf_wrapper_gettrace      (file%alt_lun, tr,   size(tr))
          hlen = jsf_wrapper_getheaders    (file%alt_lun, hd,   size(hd))
          if(hlen >= hdr_lav) hd(hdr_lav) = maxval(abs( tr(:tlen) ))
          if(hlen >= hdr_top_mute) hd(hdr_top_mute)= max(1D0,hd(hdr_top_mute))
          if(hlen >= hdr_bottom_mute) hd(hdr_bottom_mute)= min(hd(hdr_bottom_mute),1D0*tlen)
          if(hlen >= hdr_sequence) then
            if(hd(hdr_sequence) <= 0D0) then
              ! Don't have a good logic for this...hd(hdr_sequence) = file%last_jseis_pos
            endif
          endif
        endif
        if(tlen < 0 .or. hlen < 0 ) then
           status       = TRCIO_ERROR
        endif
        return
      case default
        !------------ find my place if the user wanted random i/o
        if(present(tnum)) then
          status = trcio_seek_trace(file,tnum)
          if(status /= 0 ) go to 8000
        endif
        file%last_trace_read = trcio_tell_trace(file)
        trace_number = file%last_trace_read
        !-- indicate good header and trace checksums.
        header_bad = .false.
        trace_bad  = .false.
    end select

    if(.not.file%first_trace_read) then
      file%first_trace_read=.true.
      if(cio_fflush(file%lun) /= cio_ok) return
      status = trcio_seek_trace(file,1)
      if(status /= 0 ) return
    endif

    !------------ Start code here.
    !------------ Do I need to swap bytes? compare file with machine.
    swap = swap_endian() /= file%endian 
    !------------ Figure out length based on size of passed array.
    tlen = min(size(tr),file%num_values)
    hlen = min(size(hd),file%nwih)
    select case (file%ftype)
      case default
        if (file%num_values <= size(tr) ) then 
          tr_ptr => tr
          tr_alloc = .false.
          !--- zero the part of the trace for which I have no data.
          if(file%num_values < size(tr) ) tr_ptr(file%num_values+1:) = 0.0
        else
          allocate(tr_ptr(file%num_values))
          tr_alloc = .true.
        endif
        !--- always do this allocate!
        allocate(itr_ptr(file%num_values)) ! needed for conversion from integer
        if (file%nwih <= size(hd) ) then 
          hd_ptr => hd
          hd_alloc = .false.
        else
          allocate(hd_ptr(file%nwih))
          hd_alloc = .true.
        endif
    
        !------------ Determine the size of the words on disk for header.
        hwsiz = file%nbits_hd/8
        !------------ Determine the size of the words on disk for trace
        twsiz = file%nbits/8
    
        hd_tr_int_alloc = .false.
        if(file%checksum == 'CHECKSUM') then
          recl_bytes = hwsiz*file%nwih + twsiz*file%num_values
          recl_word4 = recl_bytes/4
          if(mod(recl_bytes,4) /= 0 ) then
            call trcio_print(file,&
            'trcio_read_trace: Checksum file has non-mod-4 byte count per record.')
            status = TRCIO_ERROR
            go to 8000
          endif
          allocate(hd_tr_int(recl_word4))
          hd_tr_int_alloc = .true.
          status = cio_fread(hd_tr_int(1),4,recl_word4,file%lun)
          if(status /= recl_word4) then
            status = TRCIO_EOF
            go to 8000
          endif
          !--- put checksums into the checksum words
          checksum(1) = hd_tr_int(63) !-- header checksum
          checksum(2) = hd_tr_int(64) !-- trace  checksom
          !--- zero the header words where checksums were stored.
          hd_tr_int(63) = 0
          hd_tr_int(64) = 0
    
          !--- calculate new checksums, put into the header words.
          call cio_checksum(hd_tr_int,file%nwih*2,hd_tr_int(63))
          call cio_checksum(hd_tr_int(file%nwih*2+1:),file%num_values,hd_tr_int(64))
    
          !--- compare checksums with those read from the file.
          if(checksum(1) /= hd_tr_int(63) ) header_bad = .true.
          if(checksum(2) /= hd_tr_int(64) ) trace_bad  = .true.
          if    (header_bad        .or.       trace_bad ) then
            file%prt_err_cnt = file%prt_err_cnt + 1
            file%num_traces_with_errors = file%num_traces_with_errors + 1
            file%num_errors = file%num_errors + 1
          endif
          if    (      header_bad .and.       trace_bad ) then
            write(file%common%stdout,'(A6,I11,A4,A,a)')&
              'TRACE ',trace_number,' on ',trim(file%filename),&
              ' has bad header&trace checksums.' 
          elseif(      header_bad .and. .not. trace_bad ) then
            write(file%common%stdout,'(A6,I11,A4,A,a)')&
              'TRACE ',trace_number,' on ',trim(file%filename),&
              ' has bad header       checksum.' 
          elseif(.not. header_bad .and.       trace_bad ) then
            write(file%common%stdout,'(A6,I11,A4,A,a)')&
              'TRACE ',trace_number,' on ',trim(file%filename),&
              ' has bad        trace checksum.' 
          endif
    
          !-- remove checksums from header.
          hd_tr_int(63:64) = 0
        endif
    
        !------------ Read the header.
        select case (hwsiz)
          case(8)
            if(file%checksum == 'CHECKSUM') then
              call cmem_cpy(hd_ptr, hd_tr_int(:file%nwih*2),  &
                  sizeof(hd_tr_int(1)) * file%nwih*2)
              status = file%nwih
              !---if(header_bad) hd(:hlen) = 0d0
            else
              status = cio_fread(hd_ptr(1),hwsiz,file%nwih,file%lun)
            endif
            !--- don't swap before now, because we need the 8-byte swap for hdrs.
            if (swap) call swap_bytes(hd_ptr(1:file%nwih))
    
            ! See comment with revision 57.
            !
            if (file%nwih >= 32) then
               hd_ptr(32) = 0.0
            endif
          case(4)
              if( file%ftype /= 'SEGY' .and.  file%ftype /= 'SU' ) then
                allocate (hdr(file%nwih))
                status = cio_fread(hdr(1),hwsiz,file%nwih,file%lun)
                if(swap) call swap_bytes(hdr(1:file%nwih))
                hd_ptr(:hlen) = hdr(:hlen)
                deallocate(hdr)
    
                ! See comment with revision 57.
                !
                if (file%nwih >= 32) then
                   hd_ptr(32) = 0.0
                endif
              else
                ! is segy file
                if (hd_alloc) then 
                  deallocate(hd_ptr)
                  allocate(hd_ptr(64))
                endif
                if(size(hd_ptr) < 64) then !RSD 2002-09-20
                 ! supplied buffer must be at least 64 words long
                 !write(file%common%stdout,*) &
                 !'trcio_read_trace: error, hd_ptr is < 64'
                  status = TRCIO_ERROR
                  go to 8000
                endif
    
                allocate (hdi(file%nwih))
                status = cio_fread(hdi(1),hwsiz,file%nwih,file%lun)
                if(file%ftype=='SU') then
                  swapsu = .false.
                  if(swap_endian() ==0) swapsu= .true.
                  call segy_unpack_segyhd(syh,hdi,swapsu)
                else
                  call segy_unpack_segyhd(syh,hdi,swap)
                endif
                call segy_segyhd_to_cpshd(hd_ptr(:64),syh,1d0*file%tmin)
    
                !--- Do additional custom mapping of segy to cps headers
                !--- (RSD, 00/11/09)
                if(file%nummap > 0 .and. file%mod_segy(1:1)=='Y') then
                  do i =1,file%nummap
                    iwdtype(i)=0
                    if(file%mtype(i)(1:1)=='F') iwdtype(i)=1
                  enddo
                  call segy_map_segy_to_cps(hdi,file%nummap,file%bytes,&
                    file%sbyte, file%cps_hdr, iwdtype, hd_ptr)
                endif
                deallocate(hdi)
    
              endif
          case default
            status = -1
        end select
        if (hd_alloc) hd(:hlen) = hd_ptr(:hlen)
        if(status /= file%nwih ) then
          status = TRCIO_EOF
          go to 8000
        endif
        !------------ Read the trace.
    
        if(file%ftype(1:3) == 'LBO') then
          file%recl = trcio_get_recl(file)   ! this ensures lbo_recl is known
          if(.not. associated(file%lbo_ibuf)) then
            allocate(file%lbo_ibuf(file%lbo_recl/4))
          endif
          file%lbo_ibuf = 0
          status = cio_fread(file%lbo_ibuf(1),4,file%lbo_recl/4,file%lun)
          if(status /= file%lbo_recl/4) then
            status = TRCIO_ERROR
            go to 8000
          endif
          call lbo_uncompress_trace(                             &
                 file%lbo_ibuf,tr_ptr(:file%num_values),         &
                 file%lbo_version,file%nbits,lbo_samps_per_pack,status)
        else
          select case (twsiz)
            case(4)
              if(file%checksum == 'CHECKSUM') then
                call cmem_cpy(tr_ptr, hd_tr_int(file%nwih*2+1:), &
                    file%num_values * sizeof(tr_ptr(1)))
                status = file%num_values
                !---if(trace_bad) tr(:tlen) = 0.0
              else
                status = cio_fread(tr_ptr(1),twsiz,file%num_values,file%lun)
              endif
              if(file%ftype == 'SEGY' .and. file%wtype == 'IBM' ) then
                !-- segy
                !-- Since ibm is always written with big-endian, we must set swap to
                !-- TRUE if we are little-endian(endian=0) or FALSE if we are big-
                !-- endian (endian = 1)
    
                if(file%endian == 0 ) then
                 ibmswap = .true.  ! because we are needing to swap bytes beforehand
                else
                 ibmswap = .false. ! because ibm is always big endian,nothing to do
                endif
    
                call wrdc_ibm_to_float(tr_ptr(:file%num_values),file%num_values, &
                                       ibmswap)
              elseif(file%ftype == 'SEGY' .and. file%wtype == 'INT4' ) then
                !-- read into integer array, swap bytes, and transfer to real array.
                !-- . not supported yet.
                itr_ptr = transfer(tr_ptr,itr_ptr)
                if(swap) call swap_bytes(itr_ptr(1:file%num_values))
                tr_ptr = itr_ptr
              else
                if(file%ftype=='SU') then
                  if(.not. swap) call swap_bytes(tr_ptr(1:file%num_values))
                else
                  if(swap) call swap_bytes(tr_ptr(1:file%num_values))
                endif
              endif
            case(1)
              status = cio_fread(tr_ptr(1),twsiz,file%num_values,file%lun)
              !-- 1 byte tr into 4 byte tr values. (unpack is a c routine.)
              ! -- here add conversion from unsigned byte to signed byte data.
              ! -- Commented out, data should be unsigned on disk.   ehs 23apr01
              ! call wrdc_usb_to_sb(file%num_values,tr_ptr,tr_ptr)
              mystat = wrdc_unpack(tr_ptr(1),file%num_values,twsiz,4)
            case(2)
              status = cio_fread(tr_ptr(1),twsiz,file%num_values,file%lun)
              mystat = wrdc_unpack(tr_ptr(1),file%num_values,twsiz,4,swap)
            case(8)
              allocate (tr_dblptr(file%num_values))
              status = cio_fread(tr_dblptr(1),twsiz,file%num_values,file%lun)
              if(swap) call swap_bytes(tr_dblptr(1:file%num_values))
              tr_ptr(:tlen) = tr_dblptr(:tlen)
              deallocate (tr_dblptr)
            case default
              status = -1
          end select
        endif
    
    !   Moved to after unscaling and uncompressing.
    !   ehs --- 04jun01
    !   if (tr_alloc) tr(:tlen) = tr_ptr(:tlen)
    
        if(status /= file%num_values ) then
          status = TRCIO_EOF
          go to 8000
        endif
    
        status = TRCIO_OK
    end select

8000 continue
    !--- if scaled and compressed, then unscale and decompress.

    ! If 8-bit segy, 1st check segy header trwf (169-170) for scaling
    ! information.  segy_segyhd_to_cpshd moves the scaling info from
    ! trwf to cps header 54.  If it is not there, try cps header 25.
    ! If this is 0.0, wrdc_unscale will correctly do nothing.
    ! This will work for Landmark and Conoco generated segy.  Landmark
    ! uses trwf, Conoco uses lav.  If Landmark has no scaling info
    ! in trwf, cps header 25 will have been set to 0.0 in segy_segyhd_to_cpshd
    ! so wrdc_unscale will do nothing.  Conoco puts no scaling info in
    ! trwf, so this code will get it from 25.  Maybe non-Conoco 16-bit
    ! segy should look in trwf for scaling info, but I'm not implimenting
    ! that since I do not have a file to test.
    ! ehs -- 15may01
    !
    if ( (file%ftype == 'SEGY' ) .and. (file%nbits == 8 ) ) then
      if (hlen >= HDR_SCRATCH_30) then
        if((hd_ptr(HDR_SCRATCH_30) /= 1.0)  ) then
          tscle = 1.0 / hd_ptr(HDR_SCRATCH_30)   ! Landmark 8-bit segy
        endif
      endif
    elseif(hlen >= HDR_LAV ) then
      tscle = hd_ptr(hdr_lav)
    else
      tscle = 1.0
    endif
    if(file%wtype == 'CMPI' ) then
      call wrdc_unscale(tr_ptr(:tlen),tscle)
      call wrdc_uncompress(tr_ptr(:tlen))
    elseif (file%wtype(:3) == 'INT') then
      call wrdc_unscale(tr_ptr(:tlen),tscle)
    endif
    ! Moved from above, ehs 04jun01
    if (tr_alloc) tr(:tlen) = tr_ptr(:tlen)

    !------------ Add information to the trace header------------------
    if(hlen >= hdr_sequence) then
      if(hd(hdr_sequence) <= 0.0 ) then
        hd(hdr_sequence) = trcio_tell_trace(file)
      endif
    endif
    if(hlen >= hdr_top_mute) &
      hd(hdr_top_mute)    = max(1D0,hd(hdr_top_mute) )
    if(hlen >= hdr_bottom_mute) &
      hd(hdr_bottom_mute) = min(hd(hdr_bottom_mute),1D0*tlen)
    !-------------------------------------------------------------------
    !          CLEAN bad trace floating point values here.  This will
    !          create FNIL values wherever a NaN or +/- INF IEEE value 
    !          is found.
    !-------------------------------------------------------------------
    nbadval = clean_fnil(tr(:tlen))
    file%num_errors = file%num_errors + nbadval
    if(nbadval == 1 ) then
      write(file%common%stdout,'(A6,I11,A4,A,a)')&
        'TRACE ',trace_number,' on ',trim(file%filename),&
        ': one bad sample found.' 
      if(header_bad .or. trace_bad) then
        !--- don't increment "num_traces_with_errors" 
        !--- because we already counted this trace !!!
        file%num_errors = file%num_errors - 1 ! 1 already added above
      else ! checksums were ok BUT nbadval is > 0 !!!
        file%num_traces_with_errors = file%num_traces_with_errors + 1
        file%prt_err_cnt = file%prt_err_cnt + 1
      endif
    elseif(nbadval > 1 ) then
      write(file%common%stdout,'(A6,I11,A4,A,A2,I6,a)')&
        'TRACE ',trace_number,' on ',trim(file%filename),&
        ': ',nbadval,' bad samples found.' 
      if(header_bad .or. trace_bad) then
        !--- don't increment "num_traces_with_errors" 
        !--- because we already counted this trace !!!
        file%num_errors = file%num_errors - 1 ! 1 already added above
      else ! checksums were ok BUT nbadval is > 0 !!!
        file%num_traces_with_errors = file%num_traces_with_errors + 1
        file%prt_err_cnt = file%prt_err_cnt + 1
      endif
    endif

    if(hlen >= HDR_LAV )  then
      hd(hdr_lav) = 0d0
      if(header_bad) hd(hdr_lav) = hd(hdr_lav) -1d0
      if(trace_bad) hd(hdr_lav) = hd(hdr_lav) - 2d0
      if(.not.header_bad .and. .not.trace_bad) &
        hd(hdr_lav) = maxval(abs( tr(:tlen) ))
    endif
    !-----------------------------------------------------------------
    if(tr_alloc) then
      deallocate(tr_ptr)
    endif
    !-- always do this deallocate
    deallocate(itr_ptr)
    if(hd_alloc) deallocate(hd_ptr)
    if(hd_tr_int_alloc) deallocate( hd_tr_int)

  end function trcio_read_trace_1d

  integer function trcio_read_trace_2d(file,hd,tr,tnum,celv,ntrc)result (status)
    type(trcio_struct),pointer                          :: file
    real             ,intent(out),dimension(:,:),target :: tr
    double precision ,intent(out),dimension(:,:),target :: hd
    integer, optional,intent(in)                        :: tnum
    real,intent(in),optional                            :: celv !common el.val.
    integer,intent(in),optional                         :: ntrc
    ! local
    integer                                           :: i
    integer                                           :: tnum_private
    integer                                           :: ntrc_private


    if(present(ntrc) ) then
      ntrc_private = min(ntrc,size(tr(1,:)))
    else
      ntrc_private = size(tr(1,:))
    endif

    if(present(tnum) ) then
      tnum_private = tnum
    else
      tnum_private = file%last_trace_read + 1 
    endif

    !--- if celv is present, traces are read by the common element value group,
    !--- and the tnum is ignored.

    if(file%ftype == 'CMPR') then
      status = trcio_error ! removed cmprio support 2006/05/18 wmm
!     if(present(celv)) then
!       status = cmprio_read_hdtrc (unit=file%lun,ntrc=ntrc_private,&
!                nhdw=size(hd(:,1)),nsmp=size(tr(:,1)),hd=hd,tr=tr,&
!                cmnelval=celv) 
!     else
!       status = cmprio_read_hdtrc (unit=file%lun,ntrc=ntrc_private,&
!                nhdw=size(hd(:,1)),nsmp=size(tr(:,1)),hd=hd,tr=tr,&
!                first_seq_trc=tnum_private) 
!     endif
!     if(status > 0 ) then 
!       status = trcio_ok
!     else
!       status = trcio_error
!     endif

    else
 
      do i = 1, ntrc
        status = trcio_read_trace(file,hd(:,i),tr(:,i),tnum_private)
        if (status < 0 ) exit
        tnum_private = tnum_private + 1
      end do
    
    endif

    file%last_trace_read = ntrc + tnum_private - 1

  end function trcio_read_trace_2d

  integer function trcio_write_trace_1d(file,hd,tr,tnum,ntrc,update) &
  result (status) 
    type(trcio_struct),pointer                        :: file
    real             ,intent(in ),dimension(:)        :: tr
    double precision ,intent(in ),dimension(:)        :: hd
    integer,optional, intent(in)                      :: tnum
    integer,optional, intent(in)                      :: ntrc ! This is ignored for write 1d!!!
    integer,optional, intent(in)                      :: update
    ! LOCAL vars.
    double precision, dimension(:),pointer             :: hd_ptr
    real            , dimension(:),pointer             :: tr_ptr
    integer         , dimension(:),pointer             :: hd_tr_int
    !----------- optional word sizes for the trace/header on disk.-------
    double precision ,dimension(:),allocatable         :: trd ! r*8 trace
    !------------ i is scratch integer for loops, twsiz = trace word size,
    !------------ hwsiz is header word size
    integer                                            :: i,twsiz,hwsiz,mystat
    integer                                            :: tlen,hlen
    logical                                            :: swap
    logical                                            :: tr_alloc,hd_alloc
    logical                                            :: hd_tr_int_alloc
    logical                                            :: update_private
    !--- added for segy
    type(segy_trc_hdr)                                 :: syh
    integer                                            :: hdi(60)
    integer                                            :: iwdtype(64)
    real                                               :: tscle
    double precision                                   :: trmax
    character(len=80)                                  :: stmp
    integer                                            :: recl_bytes,recl_word4
    integer,dimension(2)                               :: checksum
    integer,dimension(2)                               :: whereami
    type(ll)                                           :: curpos,tnumpos
    !--------------- copy input data to scratch space --------------
    !--(tr_i, hd_i are internal copies of the arguments passed in.
    real             ,dimension(:),target              :: tr_i(size(tr))
    double precision ,dimension(:),target              :: hd_i(size(hd))

    !------------ Start code here.

    !-- the trcio_write_trace_2d function will set update=0 until it writes the last
    !-- trace in its group.  This saves some overhead doing seeks and tells until they
    !-- are needed.  If update is not present then the update to num_traces in the
    !-- file structure is done automatically, since someone called to write 1 trace 
    !-- and the transaction needs to update the file structure.
    update_private = .false.
    if(present(update) ) then
      !-- we will update the number of traces in the file structure.
      if(update == 1 ) update_private = .true.
    else
      update_private = .true.
    endif

    if(file%ftype == 'CMPR') then
      !--- Require user to call with an array of traces (other write routine.)
      status = trcio_error
    endif

    !-------- copy the data ---------

    tr_i(:) = tr(:)
    hd_i(:) = hd(:) 
    !------------ has the header been written?
    if(.not.file%header_written) then
      status = trcio_writeheader(file) 
      if(status /= trcio_ok ) return
    endif
    !------------ Is this the first trace written?
    if(.not.file%first_trace_written) then
      file%first_trace_written=.true.
      if((file%ftype == 'TRCIO' .or. file%ftype(1:3) == 'LBO') &
          .and. file%nwih >= 32 .and. &
         file%nbits == 32 .and. file%nbits_hd == 64 ) file%checksum='CHECKSUM'
      status = trcio_error
      if(cio_fflush(file%lun) /= cio_ok) return
      !--- is the file a cpsio-style file? (trcio)
      select case (file%ftype)
      case ('TRCIO','LBO','LBO2')
        ! we know when history ends.
        if(cio_fseek(file%lun,file%hist_end_pos,0) /= cio_ok) return
        !--- search through the remaining lines in the ascii header
        do 
          if(cio_fgetline(stmp,80,file%lun) < 0 ) exit !we are in binary area.
          if(trim(stmp) == '#<DTA_seismic>' )  then
            !--- we are in seismic area ---
            call cio_fbackspace(file%lun) ! prepare to overwrite marker.
            exit
          endif
        end do
        !--- now we should be positioned at the first byte of an area where
        !--- we can do some i/o with traces.
      case default
        !--- seek to data_start_pos
        if(cio_fseek(file%lun,file%data_start_pos,0) /= cio_ok) return
      end select
      !---- start the seismic data section
      if(cio_fflush(file%lun) /= cio_ok) return
      select case (file%ftype)
      case ('TRCIO','LBO','LBO2')
        write(stmp,'(a)')'#<DTA_seismic>'
        if(cio_fputline(trim(stmp),unit=file%lun) < 0 ) return
        if(cio_fflush(file%lun) /= cio_ok) return
      case default
      end select
      if(cio_ftell(file%lun,file%data_start_pos) /= cio_ok) return
      if(trcio_update_header(file) /= trcio_ok) return
      select case (file%ftype)
        case ('JSEIS')
          if(.not. present(tnum) ) then
            file%last_jseis_pos = 0
            status = jsf_wrapper_settracenumber(file%alt_lun,file%last_jseis_pos)
          endif
        case default
          if(.not. present(tnum) ) then
            status = trcio_seek_trace(file,1)
            if(status /= 0 ) return
          endif
      end select
    endif

    !------------ Do I need to swap bytes? compare file with machine.
    swap = swap_endian() /= file%endian 

    !------------ If swap is true, the file header says to write these
    !------------ byte_swapped from this machine's native mode. ok.
    !------------ Find my place if I am writing random i/o
    select case (file%ftype)
      case ('JSEIS')
      if(present(tnum)) then
      else
      endif
        if(present(tnum) ) then
            file%last_jseis_pos = tnum
            status = jsf_wrapper_settracenumber(file%alt_lun,file%last_jseis_pos)
        else
            file%last_jseis_pos = file%last_jseis_pos + 1
            status = jsf_wrapper_settracenumber(file%alt_lun,file%last_jseis_pos)
        endif
      case default
        if(present(tnum) ) then
          !--- calculate where the file should be to write this trace tnum.
          tnumpos = file%data_start_pos
          tnumpos = tnumpos + file%recl*(tnum-1)
          !--- now get the current position of the file
          status =cio_ftell(file%lun,curpos)
          !--- return if error
          if(status /= 0 ) return
          !--- now compare the position needed with current.  If diff, seek
          if(curpos /= tnumpos) then
            status = trcio_seek_trace(file,tnum)
            !--- return if error
            if(status /= 0 ) return
          endif
        endif
    end select

    tlen = min(file%num_values,size(tr_i))
    hlen = min(file%nwih,size(hd_i))
    if (file%num_values <= size(tr_i) ) then 
      tr_ptr => tr_i
      tr_alloc = .false.
    else
      allocate(tr_ptr(file%num_values))
      tr_ptr(:tlen)=tr_i(:tlen)
      tr_ptr(tlen+1:)=0
      tr_alloc = .true.
    endif
    if (file%nwih <= size(hd_i) ) then 
      hd_ptr => hd_i
      hd_alloc = .false.
    else
      allocate(hd_ptr(file%nwih))
      hd_ptr(:hlen)=hd_i(:hlen)
      hd_ptr(hlen+1:)=0
      hd_alloc = .true.
    endif
   
    !------------ Add information to the trace header------------------
    if(hlen >= hdr_sequence ) then
      if(hd_ptr(hdr_sequence) <= 0.0 ) then
        hd_ptr(hdr_sequence) = trcio_tell_trace(file)
      endif
    endif
    if(hlen >= hdr_top_mute) &
      hd_ptr(hdr_top_mute)    = max(1D0,hd_i(hdr_top_mute) )
    if(hlen >= hdr_bottom_mute) &
      hd_ptr(hdr_bottom_mute) = min(hd_i(hdr_bottom_mute),1D0*tlen)
    trmax = maxval(abs( tr_i(:tlen) ))
    if(hlen >= HDR_LAV ) hd_ptr(hdr_lav) = trmax
    file%trmaxg = max(file%trmaxg,trmax)
    !-------------------------------------------------------------------

    !--- based on output word size and word type, we may need to compress
    !--- and scale the trace
    !------------ Set trace  word size
    twsiz = file%nbits/8
    !------------ Set header word size
    hwsiz = file%nbits_hd/8

    if (file%ftype(1:3) == 'LBO') then

      !file%wtype = 'LBO'

      if(.not. associated(file%lbo_ibuf)) then
        file%recl = trcio_get_recl(file)       ! this ensures lbo_recl is known
        allocate(file%lbo_ibuf(file%lbo_recl/4))
      endif

      file%lbo_ibuf = 0
      status = 0
      !tr_ptr(1)=FNIL ; tr_ptr(2)=FNIL ; tr_ptr(3)=FNIL  ! tests FNIL abort
      call lbo_compress_trace(                                  &
             file%lbo_version,tr_ptr(:tlen),file%lbo_ibuf,      &
             file%nbits,LBO_SAMPSPERPACK,status)
      if(status /= 0) then
        if(status == LBO_FNIL_VIOLATION) then
          call trcio_print(file,&
            'trcio_write_trace: lbo_compress_trace found FNIL data')
          call trcio_print(file,&
            'trcio_write_trace: do not use LBO format if your data has FNILs')
        else
          call string_ii2cc(status,stmp)
          call trcio_print(file,&
            'trcio_write_trace: lbo_compress_trace status = ' // trim(stmp))
        endif
        hd_tr_int_alloc = .false.
        status = trcio_error
        go to 8000
      endif

    else if(file%wtype == 'CMPI') then
      call wrdc_compress(tr_ptr(:tlen))

      if (file%use_ampl_max) then
        call wrdc_scale(tr_ptr(:tlen),tscle,file%nbits,file%ampl_max)
        file%trmaxg = min(file%trmaxg, dble(file%ampl_max))
      else
        call wrdc_scale(tr_ptr(:tlen),tscle,file%nbits)
      endif

      if(hlen >= HDR_LAV ) hd_ptr(hdr_lav) = tscle

    elseif (file%wtype(:3) == 'INT') then

      if (file%use_ampl_max) then
        call wrdc_scale(tr_ptr(:tlen),tscle,file%nbits,file%ampl_max)
        file%trmaxg = min(file%trmaxg, dble(file%ampl_max))
      else
        call wrdc_scale(tr_ptr(:tlen),tscle,file%nbits)
      endif

      if(hlen >= HDR_LAV ) hd_ptr(hdr_lav) = tscle
    endif

    hd_tr_int_alloc = .false.
    if(file%checksum == 'CHECKSUM') then
      !---------- hwsiz*hlen+twsiz*tlen = number of bytes in hd,tr together.
      recl_bytes = hwsiz*file%nwih+twsiz*file%num_values
      !--- we will be storing this in 4 byte integers, so calc amount of space.
      recl_word4 = recl_bytes/4
      if(mod(recl_bytes,4) /= 0 ) then
        call trcio_print(file,&
        'trcio_write_trace: Checksum file has non-mod-4 byte count per record.')
        status = trcio_error
        go to 8000
      endif
      !--- allocate space for header and trace in integer array.
      allocate(hd_tr_int(recl_word4))
      hd_tr_int_alloc = .true.
      !---------- Calculate a checksum to be put into header # 32 (if it exists)
      !---------- First zero out hd_ptr(32) (thanks for the suggestion Ed !)
      !---------- Then calc checksum for trace and header hd_ptr(1:hlen) and
      !---------- tr_ptr(1:tlen)
      !---------- then put into hd_ptr(32)
      hd_ptr(32) = 0d0
    endif

    select case(file%ftype)
      case ('JSEIS')
        ! write hd_ptr from 1 to file%nwih
        status = jsf_wrapper_putheaders(file%alt_lun,hd_ptr,file%nwih)
        status = TRCIO_OK
        ! write tr_ptr from 1 to file%num_values
        status = jsf_wrapper_puttrace(file%alt_lun,tr_ptr,file%num_values)
        status = TRCIO_OK
        ! data is written, now clean up and exit.
        go to 8000
    end select


    select case(hwsiz)
      case(8) 
        if(swap) call swap_bytes(hd_ptr(1:file%nwih)) 
        !--- put header into output array ----
        if(file%checksum == 'CHECKSUM') then
          call cmem_cpy(hd_tr_int(1:file%nwih*2),hd_ptr,  &
              sizeof(hd_ptr(1)) * size(hd_ptr))
          status = file%nwih
        else
          status = cio_fwrite(hd_ptr(1),hwsiz,file%nwih,file%lun)
        endif
      case(4)
        select case (file%ftype)
        case ('SEGY')
          ! is a segy file
          call segy_cpshd_to_segyhd(hd_ptr,syh,1d0*file%tmin, &
                                    1d0*file%dt,file%num_values)
          call segy_pack_segyhd(syh,hdi,swap)

          !--- Do additional custom mapping of cps to segy headers
          !--- (RSD, 00/11/09)
          if(file%nummap > 0 .and. file%mod_segy(1:1)=='Y') then
            do i =1,file%nummap
              iwdtype(i)=0
              if(file%mtype(i)(1:1)=='F') iwdtype(i)=1
            enddo
            call segy_map_cps_to_segy(hdi,file%nummap,file%bytes,&
              file%sbyte, file%cps_hdr, iwdtype, hd_i)
          endif
          !--- put header into output array ----
          status = cio_fwrite(hdi(1),hwsiz,file%nwih,file%lun)
        case ('SU')
          ! is an SU file
          call segy_cpshd_to_segyhd(hd_ptr,syh,1d0*file%tmin, &
                                    1d0*file%dt,file%num_values)
          call segy_pack_segyhd(syh,hdi,swap)
          !--- put header into output array ----
          status = cio_fwrite(hdi(1),hwsiz,file%nwih,file%lun)
        case default
          mystat = wrdc_pack(hd_ptr(1),file%nwih,8,hwsiz,swap)
          !--- put header into output array ----
          status = cio_fwrite(hd_ptr(1),hwsiz,file%nwih,file%lun)
        end select
      case default
        status = -1
    end select

    if(status /= file%nwih ) then
      status = TRCIO_ERROR
      go to 8000
    endif

    if(file%ftype(1:3) == 'LBO') then
      status = cio_fwrite(file%lbo_ibuf(1),4,file%lbo_recl/4,file%lun)
      if(status /= file%lbo_recl/4) then
        status = TRCIO_ERROR
        go to 8000
      endif
    else
      select case(twsiz)
        case(4)
          if((file%ftype=='SEGY'.or.file%ftype=='SU') .and. file%wtype == 'IBM' ) then
            call wrdc_float_to_ibm(tr_ptr(:file%num_values),file%num_values)
          elseif((file%ftype=='SEGY'.or.file%ftype=='SU') .and. file%wtype == 'INT4' ) then
!           call transfer function to convert to integer
!           if(swap) call swap_bytes(integer vector)
            status = -1
          else
            if(swap) call swap_bytes(tr_ptr(1:file%num_values))
          endif            
          if(file%checksum == 'CHECKSUM') then
            !--- now load the trace into the output array
            call cmem_cpy(hd_tr_int(file%nwih*2+1:), tr_ptr, &
                file%num_values * sizeof(tr_ptr(1)))
            status = file%num_values
          else
            status = cio_fwrite(tr_ptr(1),twsiz,file%num_values,file%lun)
          endif
        case(8)
          allocate(trd(file%num_values) )
          trd(:file%num_values) = tr_ptr(:file%num_values)
          if(swap) then
            call swap_bytes(trd(1:file%num_values))
          endif
          !--- now load the trace into the output array
          status = cio_fwrite(trd(1),twsiz,file%num_values,file%lun)
          deallocate(trd)
        case(2)
          mystat = wrdc_pack(tr_ptr(1),file%num_values,4,twsiz,swap)
          !--- now load the trace into the output array
          status = cio_fwrite(tr_ptr(1),twsiz,file%num_values,file%lun)
        case(1)
          mystat = wrdc_pack(tr_ptr(1),file%num_values,4,twsiz)
          ! -- here add conversion from signed byte to unsigned byte data.
          ! -- Commented out, use signed.   ehs 23apr01
          ! call wrdc_sb_to_usb(file%num_values,tr_ptr,tr_ptr)
          !--- now load the trace into the output array
          status = cio_fwrite(tr_ptr(1),twsiz,file%num_values,file%lun)
        case default
          status = -1
      end select

      if(status /= file%num_values ) then
        status = TRCIO_ERROR
        go to 8000
      endif

    endif

    if(file%checksum == 'CHECKSUM') then
      !--- zero the checksum header words (header # 32)
      hd_tr_int(63) = 0
      hd_tr_int(64) = 0
      !--- calculate new checksums, put into checksum array.
      call cio_checksum(hd_tr_int,file%nwih*2,checksum(1)) 
      call cio_checksum(hd_tr_int(file%nwih*2+1:),file%num_values,checksum(2)) 
      !--- put checksums into header
      hd_tr_int(63)=checksum(1)
      hd_tr_int(64)=checksum(2)
      !--- write the data to disk.
      status = cio_fwrite(hd_tr_int(1),4,recl_word4,file%lun)
      if(status /= recl_word4 ) then
        status = TRCIO_ERROR
        go to 8000
      endif
    endif
    status = TRCIO_OK

8000 continue
    if(update_private) then
      select case (file%ftype)
      case ('JSEIS')
      case default
        !--- update the file structure with position of data written
        !-- stash current file position
        status = cio_ftell(file%lun,whereami)
        !-- go to end of file
        status = cio_fseek(file%lun,0,2) ! end of file
        !-- write that position as the end of data.
        status = cio_ftell(file%lun,file%data_end_pos) ! set end of data point here.
        !-- go back to current position
        status = cio_fseek(file%lun,whereami,0)
      end select
      file%num_traces = trcio_get_number_traces(file)
    endif

    if(tr_alloc)         deallocate (tr_ptr)
    if(hd_alloc)         deallocate (hd_ptr)
    if(hd_tr_int_alloc)  deallocate (hd_tr_int)
  end function trcio_write_trace_1d

  integer function trcio_write_trace_2d(file,hd,tr,tnum,ntrc) result (status) 
    type(trcio_struct),pointer                        :: file
    double precision ,intent(in ),dimension(:,:)      :: hd
    real             ,intent(in ),dimension(:,:)      :: tr
    integer,intent(in),optional                       :: tnum
    integer,intent(in),optional                       :: ntrc
    !------------ LOCAL vars.
    integer                                           :: i  ! for loop.
    !integer                                           :: nbytes_written
    integer                                           :: ntrc_private
    integer                                           :: tnum_private
    integer                                           :: update
    !------------ Start code here.

    status = trcio_ok
    update = 0

    !-- wmm 01/26/09 added tnum so that one could position the file correctly
    !--              when writing a 2-d array of traces.
    if(present(ntrc) ) then
      ntrc_private = ntrc
    else
      ntrc_private = size(tr(1,:))
    endif

    if(present(tnum) ) then
      tnum_private = tnum
    else
      tnum_private = trcio_tell_trace(file)
    endif

    if(file%ftype /= 'CMPR') then
      update = 0
      do i = 1, ntrc_private
        if(i == ntrc_private) update = 1
        status = trcio_write_trace_1d(file,hd(:,i),tr(:,i),tnum=tnum_private,ntrc=1,update=update)
        tnum_private = tnum_private + 1
        if(status /= trcio_ok ) exit
      end do

    else
      ! wmm 2006/05/18 removed cmprio support
      !---------- only compressed trace files are written here. ------------
      !nbytes_written = cmprio_write_hdtrc(unit=file%lun,ntrc=ntrc_private,&
      ! nhdw=file%nwih,nsmp=file%num_values,hd=hd,tr=tr,common_hdr_num=3)
      !if ( nbytes_written <= 0 ) status = trcio_error
      status = trcio_error
    
    endif

  end function trcio_write_trace_2d


  function trcio_get_number_traces(file) result (number_traces)
    type(trcio_struct),pointer         :: file
    integer                                :: number_traces
    integer                                :: status
    integer,dimension(2)                   :: whereami,data_bytes
    integer                                :: leftover_bytes,extsize

    number_traces = 0
    !--- wmm added 1/26/09 in case someone asks on a new file prior to writing data.
    if(file%io_mode == "w " .and. .not. file%first_trace_written ) return

    status = cio_fflush(file%lun)
    select case (file%ftype)
    case ('QTROT','SEGY','SU')
      number_traces = file%num_traces
    case ('TRCIO','LBO','LBO1','LBO2')
      !- store where I currently am in the file.
      status = cio_ftell(file%lun,whereami)
  
      ! go to end of file or end of data.
      if(file%data_end_pos(1) /= file%data_start_pos(1) .or. &
         file%data_end_pos(2) /= file%data_start_pos(2)       ) then
        !--- retrieve end position.
        status = cio_fseek(file%lun,file%data_end_pos,0)
      else
        !--- need to set the end position to end of file.
        status = cio_fseek(file%lun,0,2) ! end of file
        status = cio_ftell(file%lun,file%data_end_pos)
      endif
  
      !- data_bytes(1) = number of extents full of traces
      !- data_bytes(2) = difference in byte offsets from start to finish between
      !-                 extents (in data_bytes(1)).
      !- Example:
      !- assume 9 bytes/extent, 5 bytes/trace, data_start_pos = extent 1, 
      !- byte 4 (byte 0 is first byte within the extent).  Assume 2 traces.
      !- the end position will be extent 2 byte 5 (just before byte 5).
      !- therefore file%data_end_pos (2,5) - file%data_start_pos (1,4) ==
      !- data_bytes == (1,1).
      !- for 3 traces, end pos = (3,0) so data_bytes=(2,-1)
  
      data_bytes = file%data_end_pos - file%data_start_pos
  
      !- formula to figure out number of traces == below:
      !- number_traces   = data_bytes(1) * (extent_size/trc_len)
      !- remainder_bytes = data_bytes(2) + (data_bytes(1)*(extent_size%trc_len))
      !- number_traces   = number_traces+data_bytes(2)/tlen
 
      extsize        = cio_get_file_ext_size(file%lun) 
      number_traces  = data_bytes(1) * (extsize/file%recl)
      leftover_bytes = data_bytes(2) + &
                      (data_bytes(1)*(mod(extsize,file%recl)))
      number_traces  = number_traces + leftover_bytes/file%recl
      !- Don't add one to number of traces since we are at the start of the
      !- UNWRITTEN next trace.!!!
  
      !- Reset end-pos based on even number of traces.
      status = cio_fseek(file%lun,file%data_start_pos,file%recl,number_traces+1)
      status = cio_ftell(file%lun,file%data_end_pos)
  
      !- Go back to my original position in the file.
      status = cio_fseek(file%lun,whereami,0)
    case default
    end select

  end function trcio_get_number_traces

  function trcio_add_hdel_i2(lun,srchstr,fmtstr,value) result(status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    integer,intent(in),dimension(:) :: value(2)
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    !--------------------------------------------------
    write(s,fmtstr)srchstr,value(1),',',value(2),')'
    status = min(0,cio_fputline(s,len(s),lun))
  end function trcio_add_hdel_i2

  function trcio_add_hdel_i(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    integer,intent(in)          :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    !--------------------------------------------------
    write(s,fmtstr)srchstr,value
    status = min(0,cio_fputline(s,len(s),lun))
  end function trcio_add_hdel_i

  function trcio_add_hdel_r(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    real   ,intent(in)          :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    !--------------------------------------------------
    write(s,fmtstr)srchstr,value
    status = min(0,cio_fputline(s,len(s),lun))
  end function trcio_add_hdel_r

  function trcio_add_hdel_d(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    double precision,intent(in) :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    !--------------------------------------------------
    write(s,fmtstr)srchstr,value
    status = min(0,cio_fputline(s,len(s),lun))
  end function trcio_add_hdel_d

  function trcio_add_hdel_c(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    character(len=*),intent(in),optional :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    !--------------------------------------------------
    if(present(value)) then
      write(s,fmtstr)srchstr,value
    else
      write(s,fmtstr)srchstr
    endif
    status = min(0,cio_fputline(s,len(s),lun))
    !-- len_trim can cause problems here.
    !-- wmm 12/5/2001 status = min(0,cio_fputline(s,len_trim(s),lun))
  end function trcio_add_hdel_c

  function trcio_upd_hdel_i2(lun,srchstr,fmtstr,value) result(status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    integer,intent(in),dimension(:) :: value(2)
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    integer                     :: out_length
    !--------------------------------------------------
    status = trcio_ok

    call cio_frewind(lun) 
    do 
      status = cio_fgetline(s,80,lun)
      if(status < cio_ok ) goto 999
      if(status < 0 ) exit
      if(index(s,srchstr) > 0 ) then
        out_length = status
        call cio_fbackspace(lun)
        write(s,fmtstr)srchstr,value(1),',',value(2),')'
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        status = cio_fputline(s,out_length,lun)
        if(status < cio_ok ) goto 999
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        exit
      endif
    end do 
    return
999 status = trcio_error
 
  end function trcio_upd_hdel_i2

  function trcio_upd_hdel_i(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    integer,intent(in)          :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    integer                     :: out_length
    !--------------------------------------------------
    status = trcio_ok

    call cio_frewind(lun) 
    do 
      status = cio_fgetline(s,80,lun)
      if(status < cio_ok ) goto 999
      if(status < 0 ) exit
      if(index(s,srchstr) > 0 ) then
        out_length = status
        call cio_fbackspace(lun)
        write(s,fmtstr)srchstr,value
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        status = cio_fputline(s,out_length,lun)
        if(status < cio_ok ) goto 999
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        exit
      endif
    end do 
    return
999 status = TRCIO_ERROR

  end function trcio_upd_hdel_i

  function trcio_upd_hdel_r(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    real   ,intent(in)          :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    integer                     :: out_length
    !--------------------------------------------------
    status = trcio_ok

    call cio_frewind(lun) 
    do 
      status = cio_fgetline(s,80,lun)
      if(status < cio_ok ) goto 999
      if(status < 0 ) exit
      if(index(s,srchstr) > 0 ) then
        out_length = status
        call cio_fbackspace(lun)
        write(s,fmtstr)srchstr,value
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        status = cio_fputline(s,out_length,lun)
        if(status < cio_ok ) goto 999
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        exit
      endif
    end do 
    return
999 status = TRCIO_ERROR

  end function trcio_upd_hdel_r

  function trcio_upd_hdel_d(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    double precision,intent(in) :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    integer                     :: out_length
    !--------------------------------------------------
    status = trcio_ok

    call cio_frewind(lun) 
    do 
      status = cio_fgetline(s,80,lun)
      if(status < cio_ok ) goto 999
      if(status < 0 ) exit
      if(index(s,srchstr) > 0 ) then
        out_length = status
        call cio_fbackspace(lun)
        write(s,fmtstr)srchstr,value
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        status = cio_fputline(s,out_length,lun)
        if(status < cio_ok ) goto 999
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        exit
      endif
    end do 
    return
999 status = TRCIO_ERROR

  end function trcio_upd_hdel_d

  function trcio_upd_hdel_c(lun,srchstr,fmtstr,value) result (status)
    integer,intent(in)          :: lun
    character(len=*),intent(in) :: srchstr
    character(len=*),intent(in) :: fmtstr
    character(len=*),intent(in) :: value
    !----
    integer                     :: status
    !----
    character(len=80)           :: s
    integer                     :: out_length
    !--------------------------------------------------
    status = trcio_ok

    call cio_frewind(lun) 
    do 
      status = cio_fgetline(s,80,lun)
      if(status < 0 ) goto 999
      if(status < 0 ) exit
      if(index(s,srchstr) > 0 ) then
        out_length = status
        call cio_fbackspace(lun)
        write(s,fmtstr)srchstr,value
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        status = cio_fputline(s,out_length,lun)
        if(status < 0 ) goto 999
        status = cio_fflush(lun)
        if(status /= cio_ok ) goto 999
        exit
      endif
    end do 
    return
999 status = TRCIO_ERROR

  end function trcio_upd_hdel_c

  function trcio_update_header_private(file) result (status)
    type(trcio_struct),pointer         :: file
    integer                                :: status

    status = cio_fflush(file%lun)
    if(status /= cio_ok ) goto 999
    status = trcio_writeheader(file)
    if(status /= trcio_ok ) goto 999

    select case (file%ftype)

      case ('TRCIO','LBO','LBO1','LBO2')
      !-------- update ftype
      if(trcio_update_hdr_elem(file%lun,'# ftype = ','(a10,a9)',file%ftype) < 0)&
        goto 999
  
      !-------- update endian
      if(trcio_update_hdr_elem(file%lun,'# endian = ','(a11,i1)',file%endian)< 0)&
        goto 999
  
      !-------- wtype
      if(trcio_update_hdr_elem(file%lun,'# wtype = ','(a10,a5)',file%wtype) < 0)&
        goto 999
  
      !-------- nbits
      if(trcio_update_hdr_elem(file%lun,'# nbits = ','(a10,i7)',file%nbits) < 0)&
        goto 999
  
      !-------- nbits_hd
      if( &
      trcio_update_hdr_elem(file%lun,'# nbits_hd = ','(a13,i7)',file%nbits_hd)&
      < 0) goto 999
  
      !-------- tmin
      if( &
      trcio_update_hdr_elem(file%lun,'# tmin = ','(a9,f14.7)',file%tmin)&
      < 0) goto 999
  
      !-------- tmax
      if( &
      trcio_update_hdr_elem(file%lun,'# tmax = ','(a9,f14.7)',file%tmax)&
      < 0) goto 999
  
      !-------- dt
      if( &
      trcio_update_hdr_elem(file%lun,'# dt = ','(a7,f14.7)',file%dt)&
      < 0) goto 999
  
      !-------- nhd1
      if( &
      trcio_update_hdr_elem(file%lun,'# nhd1 = ','(a9,i2)',file%nhd1)&
      < 0) goto 999
  
      !-------- nhd2
      if( &
      trcio_update_hdr_elem(file%lun,'# nhd2 = ','(a9,i2)',file%nhd2)&
      < 0) goto 999
  
      !-------- nhd3
      if( &
      trcio_update_hdr_elem(file%lun,'# nhd3 = ','(a9,i2)',file%nhd3)&
      < 0) goto 999
  
      !-------- vwidth1
      if( &
      trcio_update_hdr_elem(file%lun,'# vwidth1 = ','(a12,f14.7)',file%vwidth1)&
      < 0) goto 999
  
      !-------- vwidth2
      if( &
      trcio_update_hdr_elem(file%lun,'# vwidth2 = ','(a12,f14.7)',file%vwidth2)&
      < 0) goto 999
  
      !-------- vwidth3
      if( &
      trcio_update_hdr_elem(file%lun,'# vwidth3 = ','(a12,f14.7)',file%vwidth3)&
      < 0) goto 999
  
      !-------- nwih
      if(trcio_update_hdr_elem(file%lun,'# nwih = ','(a9,i3)',file%nwih) < 0)&
        goto 999
  
      !-------- num_values
      if(&
      trcio_update_hdr_elem &
      (file%lun,'# num_values = ','(a15,i11)',file%num_values) < 0) goto 999
  
      !-------- vbin1
      if( &
      trcio_update_hdr_elem(file%lun,'# vbin1 = ','(a10,f14.7)',file%vbin1)&
      < 0) goto 999
  
      !-------- vbin2
      if( &
      trcio_update_hdr_elem(file%lun,'# vbin2 = ','(a10,f14.7)',file%vbin2)&
      < 0) goto 999
  
      !-------- vbin3
      if( &
      trcio_update_hdr_elem(file%lun,'# vbin3 = ','(a10,f14.7)',file%vbin3)&
      < 0) goto 999
  
      !-------- vmin1
      if( &
      trcio_update_hdr_elem(file%lun,'# vmin1 = ','(a10,f14.7)',file%vmin1)&
      < 0) goto 999
  
      !-------- vmin2
      if( &
      trcio_update_hdr_elem(file%lun,'# vmin2 = ','(a10,f14.7)',file%vmin2)&
      < 0) goto 999
  
      !-------- vmin3
      if( &
      trcio_update_hdr_elem(file%lun,'# vmin3 = ','(a10,f14.7)',file%vmin3)&
      < 0) goto 999
  
      !-------- vmax1
      if( &
      trcio_update_hdr_elem(file%lun,'# vmax1 = ','(a10,f14.7)',file%vmax1)&
      < 0) goto 999
  
      !-------- vmax2
      if( &
      trcio_update_hdr_elem(file%lun,'# vmax2 = ','(a10,f14.7)',file%vmax2)&
      < 0) goto 999
  
      !-------- vmax3
      if( &
      trcio_update_hdr_elem(file%lun,'# vmax3 = ','(a10,f14.7)',file%vmax3)&
      < 0) goto 999
  
      !--------  xorigin
      if( &
      trcio_update_hdr_elem( &
        file%lun,'# xorigin = ','(a12,g16.8)',file%common%xorigin) < 0) &
          goto 999
  
      !--------  yorigin
      if( &
      trcio_update_hdr_elem( &
        file%lun,'# yorigin = ','(a12,g16.8)',file%common%yorigin) < 0) &
          goto 999
  
      !--------  dx11
      if(trcio_update_hdr_elem( &
        file%lun,'# dx11 = ','(a9,g16.8)',file%common%dx11) < 0)&
          goto 999
  
      !--------  dx12
      if(trcio_update_hdr_elem( &
        file%lun,'# dx12 = ','(a9,g16.8)',file%common%dx12) < 0)&
          goto 999
  
      !--------  dx21
      if(trcio_update_hdr_elem( &
        file%lun,'# dx21 = ','(a9,g16.8)',file%common%dx21) < 0)&
          goto 999
  
      !--------  dx22
      if(trcio_update_hdr_elem( &
        file%lun,'# dx22 = ','(a9,g16.8)',file%common%dx22) < 0)&
          goto 999
  
      !--------  num_traces
      file%num_traces = trcio_get_number_traces(file)
      if(trcio_update_hdr_elem &
      (file%lun,'# num_traces = ','(a15,i11)',file%num_traces) < 0) goto 999
  
      !--------  recl
      if(trcio_update_hdr_elem &
      (file%lun,'# record_length = ','(a18,i11)',file%recl) < 0) goto 999
  
      !--------  trmaxg
      if(trcio_update_hdr_elem &
      (file%lun,'# trmaxg = ','(a11,E24.14E3)',file%trmaxg) < 0) goto 999
  
      !--- update hist start pos
      if(trcio_update_hdr_elem &
      (file%lun,'# hist_start_pos = (','(A20,2(i11,a1))',file%hist_start_pos)&
      < 0 ) goto 999
  
      !--- update hist end pos
      if(trcio_update_hdr_elem &
      (file%lun,'# hist_end_pos = (','(A18,2(i11,a1))',file%hist_end_pos)&
      < 0 ) goto 999
  
      !--- update data_start_pos
      if(trcio_update_hdr_elem &
      (file%lun,'# data_start_pos = (','(A20,2(i11,a1))',file%data_start_pos)&
      < 0 ) goto 999
  
      !--- update data_end_pos
      if(trcio_update_hdr_elem &
      (file%lun,'# data_end_pos = (','(A18,2(i11,a1))',file%data_end_pos)&
      < 0 ) goto 999
  
      !--- update checksum type
      if(trcio_update_hdr_elem &
      (file%lun,'# checksum = ','(A13,A8)',file%checksum) < 0 ) &
      goto 999
    case default
    end select
    return
999 status = TRCIO_ERROR
    call trcio_print(file, &
     'trcio_update_header_private: Errors occurred while updating the header.')

  end function trcio_update_header_private

  function trcio_seek_trace(file,trace_number) result(status)
    type(trcio_struct),pointer         :: file
    integer, intent(in)                    :: trace_number
    integer                                :: status
    integer(kind=8)                        :: trace_number_8
    select case(file%ftype)
      case ('JSEIS')
        trace_number_8 = trace_number
        status   = jsf_wrapper_settracenumber(file%alt_lun, trace_number_8  ) - 1
        file%last_trace_read = trace_number
      case default
        status = cio_fseek(file%lun,file%data_start_pos,file%recl,trace_number)
        file%last_trace_read = cio_ftell(file%lun,file%data_start_pos, file%recl)-1
    end select
  end function trcio_seek_trace

  function trcio_tell_trace(file) result(trace_number)
    type(trcio_struct),pointer         :: file
    integer                                :: trace_number
    integer(kind=8)                        :: trace_number_8
    integer                                :: status
    select case(file%ftype)
      case('JSEIS')
        trace_number_8 = jsf_wrapper_gettracenumber(file%alt_lun) - 1
        trace_number = trace_number_8
      case default
        if(file%io_mode /= "w " .or. file%first_trace_written) then
          trace_number = cio_ftell(file%lun,file%data_start_pos, file%recl)
        else
          trace_number = 1
        endif
    end select

  end function trcio_tell_trace

  function trcio_read_segy_headers(file) result(status)
    type(trcio_struct),pointer           :: file
    integer                              :: status
    type(segy_ebcdic_hdr)                :: ascii
    type(segy_bin_hdr)                   :: s
    !--- LOCAL vars. ---
    integer                              :: endian
    !---
    status = segy_read_ebchdr(file%lun,ascii)

! Don't write ebcdic header to ascii file.  ehs 19oct01
!   if ( status == 0 ) then ! save the ebcdic header in ascii format in a file.
!     unit   = cio_fopen('ebcdic.dat','w+')
!     if (unit   > 0 ) then
!       do i = 1, 40
!         status = cio_fputline(ascii%h(i),81,unit  )
!       end do
!       status = cio_fclose(unit)
!       !call trcio_print(file,&
!       !'trcio_read_segy_headers: &
!       ! & Wrote the ebcdic header to "ebcdic.dat" in ASCII format.')
!     end if
!   endif

    status = segy_read_binhdr(file%lun,s,endian=endian) 
    if (status == 0 ) then
      ! map segybh to file object
      file%ftype      = 'SEGY'
      file%nwih       = 60
      file%num_values = s%hns
      file%dt         = s%hdt*(1.e-6)
!
! No more conoco segy format.
! ehs 02nov01
!
!     file%tmin       = s%tstart
      file%tmax       = file%tmin + (file%num_values-1)*file%dt
      file%nbits      = 32 
      file%nbits_hd   = 32
      status = 0
      file%endian     = endian
      file%data_start_pos = (/0,3600/)
      file%data_end_pos   = (/0,3600/)
      file%common%xorigin = 0.0
      file%common%yorigin = 0.0
      file%common%dx11    = 1.0
      file%common%dx12    = 0.0
      file%common%dx21    = 0.0
      file%common%dx22    = 1.0
      file%nbits      = 32 
      file%wtype      = 'IBM'
      select case (int(s%format))
        case(1); file%wtype = 'IBM'
        case(2); file%wtype = 'INT4'
        case(3); file%wtype = 'INT2'
          file%nbits = 16
        case(4); file%wtype = 'FIXG'
        case(5); file%wtype = 'INT1'   ! Landmark 8-bit segy
          file%nbits =  8
        case default
      end select
      file%recl = (file%nbits_hd*file%nwih + file%nbits*file%num_values)/8
    endif

    status          = cio_fseek(file%lun,0,2)
    file%num_traces = trcio_tell_trace(file) - 1
    status          = cio_fseek(file%lun,file%data_start_pos,0)
      
  end function trcio_read_segy_headers

  function trcio_read_tfil_header(file) result (status)
    use cardset_module
    implicit none
    type(trcio_struct),pointer           :: file
    integer                              :: status
    !--- local variables ---
    real :: x
    type(cardset_struct),pointer         :: cardset
    integer                              :: nkeys,iostatus,i,j,k,kk
    character(len=89 )                   :: card,tmp
    integer                              :: nerrs
    type :: tfil_struct
      integer                          :: ntrfil
      integer                          :: nbycll
      integer                          :: ntrcll
      integer                          :: grecsiz
      integer                          :: ntb
      integer                          :: numhc
      integer                          :: nbydp
      integer                          :: nbyhd
      integer                          :: hdtyp
      integer                          :: wdtyp
      integer                          :: nhdwd
      integer                          :: ndptr
      double precision                 :: srval
      double precision                 :: tstrt
      double precision                 :: xorg
      double precision                 :: yorg
      double precision                 :: dx0(4)
      double precision                 :: dn0(4)
      double precision                 :: trmaxg
      integer                          :: dunits
      integer                          :: lun
      character(len=16)                :: ftyp
      character(len=132)               :: path
      character(len=16)                :: srun
    end type
    type(tfil_struct)                  :: t

    nullify (cardset) ! jpa

    nerrs = 0 ! number of errors.
    card=' ' 
    call cardset_create(cardset)
    call cardset_set_name(cardset,'tfil_header')
    call cardset_put_card(cardset,card)

    call cio_frewind(file%lun)
    status = 0
    do
      status = cio_fgetline(card,len(card),file%lun)
      if(status < 0 ) exit
      j = index(card,'dx0= ')
      if(j > 0 ) then
        k = 0; kk = 0
        k = index(card(j+5:),'=')
        if(k > 0 ) then
          kk = index(card(j+5:k),',',.true.) + j + 3
        else
          kk = index(card(j+5:),',',.true.) + j + 3  
        endif
        tmp = card(:j-1)//'dx0=('//card(j+5:kk)//')'
        call cardset_add_card(cardset,tmp)
        card = card(kk+1:)
      endif

      j = index(card,'dn0= ')
      if(j > 0 ) then
        k = 0; kk = 0
        k = index(card(j+5:),'=')
        if(k > 0 ) then
          kk = index(card(j+5:k),',',.true.) + j + 3
        else
          kk = index(card(j+5:),',',.true.) + j + 3  
        endif
        tmp = card(:j-1)//'dn0=('//card(j+5:kk)//')'
        call cardset_add_card(cardset,tmp)
        card = card(kk+1:)
      endif

      i = 1
      cardloop: do 
        j = index(card(i:),',') + i
        if(j <= i ) exit cardloop
        call cardset_add_card(cardset,card(i:j-2))
        i = j+1
      end do cardloop
      if(i < len(card))  then
        if (len_trim(card(i:)) > 0 ) then
          call cardset_add_card(cardset,card(i:))
        endif
      endif
      if( index(card,'srun') >= 1 ) exit
    end do

    nkeys = cardset_num_keywords(cardset)
    if(nkeys <= 0 ) then
      call cardset_delete(cardset)
      status = -1
      return
    endif

    t%ntrfil   = 0
    call cardset_get_scalar(cardset,'ntrfil',t%ntrfil,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%nbycll   = 0
    call cardset_get_scalar(cardset,'nbycll',t%nbycll,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%ntrcll = 0
    call cardset_get_scalar(cardset,'ntrcll',t%ntrcll,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%grecsiz  = 0
    call cardset_get_scalar(cardset,'grecsiz',t%grecsiz,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%ntb = 0
    call cardset_get_scalar(cardset,'ntb',t%ntb,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%numhc = 0
    call cardset_get_scalar(cardset,'numhc',t%numhc,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%nbydp = 4
    call cardset_get_scalar(cardset,'nbydp',t%nbydp,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%nbyhd    = 4
    call cardset_get_scalar(cardset,'nbyhd',t%nbyhd,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%hdtyp    = 0
    call cardset_get_scalar(cardset,'hdtyp',t%hdtyp,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%wdtyp    = 1
    call cardset_get_scalar(cardset,'wdtyp',t%wdtyp,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%nhdwd    = 64
    call cardset_get_scalar(cardset,'nhdwd',t%nhdwd,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%ndptr    = 0
    call cardset_get_scalar(cardset,'ndptr',t%ndptr,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%srval    = 0.0
    call cardset_get_scalar(cardset,'srval',t%srval,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%tstrt    = 0.0
    call cardset_get_scalar(cardset,'tstrt',t%tstrt,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%xorg = 0.0
    call cardset_get_scalar(cardset,'xorg',t%xorg,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%yorg = 0.0
    call cardset_get_scalar(cardset,'yorg',t%yorg,card)
      if(card /= ' ') nerrs = nerrs + 1
    call cardset_get_element(cardset,'dx0',1,t%dx0(1),card)
     if(card /= ' ') nerrs = nerrs + 1
    call cardset_get_element(cardset,'dx0',2,t%dx0(2),card)
     if(card /= ' ') nerrs = nerrs + 1
    call cardset_get_element(cardset,'dx0',3,t%dx0(3),card)
     if(card /= ' ') nerrs = nerrs + 1
    call cardset_get_element(cardset,'dx0',4,t%dx0(4),card)
     if(card /= ' ') nerrs = nerrs + 1
    t%trmaxg = 0
    call cardset_get_scalar(cardset,'trmaxg',t%trmaxg,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%dunits = 0
    call cardset_get_scalar(cardset,'dunits',t%dunits,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%lun = 0
    call cardset_get_scalar(cardset,'lun',t%lun,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%ftyp = ' '
    call cardset_get_scalar(cardset,'ftyp',t%ftyp,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%path = ' '
    call cardset_get_scalar(cardset,'path',t%path,card)
      if(card /= ' ') nerrs = nerrs + 1
    t%srun =  ' '
    call cardset_get_scalar(cardset,'srun',t%srun,card)
      if(card /= ' ') nerrs = nerrs + 1
      file%ftype = 'TFILE'
      file%num_traces = t%ntrfil
      file%recl       = t%nbycll
      file%nwih       = t%nhdwd
      file%num_values = t%ndptr
      file%data_start_pos(1) = 0
      file%data_start_pos(2) = t%grecsiz
      !---- commented this out in case someone wants to pipe data to CPS.
      !status= cio_fseek(file%lun,file%data_start_pos,file%recl,file%num_traces)
      !status= cio_ftell(file%lun,file%data_end_pos)
      !---- instead, just compute it...
      !extsize = cio_get_file_ext_size(file%lun)
      !numtrc_in_first_ext = (extsize-file%data_start_pos(2))/file%recl
      !numtrc_per_ext = extsize/file%recl
      !file%data_end_pos(1) = 0
      !if(numtrc_in_first_ext <= file%num_traces) then
      !  file%data_end_pos(2)=file%data_start_pos(2)+file%recl*file%num_traces
      !else
      !    file%data_end_pos(1) = 0
      !    remaining_traces = file%num_traces - numtrc_in_first_ext
      !  do
      !    file%data_end_pos(1) = file%data_end_pos(1) + 1
      !    remaining_traces = remaining_traces - numtrc_per_ext
      !    if(remaining_traces <= 0) exit
      !  end do
      !  file%data_end_pos(2) = file%recl*remaining_traces
      !endif
      file%dt         = t%srval
      file%tmin       = t%tstrt
      file%nbits      = t%nbydp*8
      file%nbits_hd   = t%nbyhd*8
      file%tmax       = t%tstrt+(t%ndptr-1)*t%srval
      file%common%xorigin = t%xorg
      file%common%yorigin = t%yorg
      file%common%dx11    = t%dx0(1)
      file%common%dx12    = t%dx0(2)
      file%common%dx21    = t%dx0(3)
      file%common%dx22    = t%dx0(4)
      file%trmaxg     = t%trmaxg

      select case (t%wdtyp) 
        case(1); file%wtype = 'IEEE'
        case(2); file%wtype = 'IBM'
        case(5); file%wtype = 'INT'
        case(15); file%wtype = 'BYTE'
        case(16); file%wtype = 'INT'
        case(3); file%wtype = 'VMS'
        case default; file%wtype = 'IEEE'
      end select
      status = 0
      select case (file%wtype)
        case('VMS')
          status = -1
        case('BYTE') 
          if(file%nbits /= 8 ) then
            status = -1
          endif
        case default
      end select
      file%endian = 1
      if(file%wtype(1:3) /= 'IBM' ) then
        !--- position file at end of header, read first header word of first
        !--- trace.  This will indicate the "endian" of the writing machine.
         iostatus = cio_fseek(file%lun,file%data_start_pos,0)
         if(iostatus /= 0 ) status = -1
         iostatus = cio_fread(x,4,1,file%lun)
         if(iostatus /= 1 ) status = -1
         if(abs(x) < abs(fnil) ) then 
           file%endian = 1 - swap_endian()
         else
           file%endian = swap_endian()
         endif
      else
      endif 
      iostatus = cio_fseek(file%lun,file%data_start_pos,0)
      if(iostatus /= 0 ) status = -1
      if(nerrs > 4 ) status = -1
    call cardset_delete(cardset)

  end function trcio_read_tfil_header

  function trcio_read_qtrot_header(file) result (status)
    type(trcio_struct),pointer           :: file
    integer                              :: status
    !--- local variables ---
    !------------- start of header------------
    integer,dimension(:)                 :: number_traces(2)          !(  1:  8)
    character(len=8)                     :: packing_flag              !(  9: 16)
    character(len=8)                     :: history_flag              !( 17: 24)
    integer,dimension(:)                 :: nwih(2)                   !( 25: 32)
    integer,dimension(:)                 :: ndpt(2)                   !( 33: 40)
    real                                 :: dt,tstrt,xo,yo,dx(4),dn(4)!( 41: 88)
    character(len=8)                     :: file_type                 !(137:144)
    integer,dimension(:)                 :: buffer_size(2)            !(145:152)
    integer,dimension(:)                 :: five_twelve(2)            !(153:160)
    character(len=8)                     :: version                   !(161:168)
    double precision                     :: trmaxg                    !(169:176)
    !------------- end of header -------------

    call cio_frewind(file%lun)
    status = trcio_error
    if(cio_fread(number_traces,8,1,file%lun)/= 1) return ! 1-8
    if(cio_fread(packing_flag ,8,1,file%lun)/= 1) return ! 9-16
    if(cio_fread(history_flag ,8,1,file%lun)/= 1) return ! 17-24
    if(cio_fread(nwih         ,8,1,file%lun)/= 1) return ! 25-32
    if(cio_fread(ndpt         ,8,1,file%lun)/= 1) return ! 33-40
    if(cio_fread(dt           ,4,1,file%lun)/= 1) return ! 41-44
    if(cio_fread(tstrt        ,4,1,file%lun)/= 1) return ! 45-48
    if(cio_fread(xo           ,4,1,file%lun)/= 1) return ! 49-52
    if(cio_fread(yo           ,4,1,file%lun)/= 1) return ! 53-56
    if(cio_fread(dx           ,4,4,file%lun)/= 4) return ! 57-72
    if(cio_fread(dn           ,4,4,file%lun)/= 4) return ! 73-88
    if(cio_fseek(file%lun,136,0) /= cio_ok) return
    if(cio_fread(file_type    ,8,1,file%lun)/= 1) return ! 137-144
    if(cio_fread(buffer_size  ,8,1,file%lun)/= 1) return ! 145-152
    if(cio_fread(five_twelve  ,8,1,file%lun)/= 1) return ! 153-160
    if(cio_fread(version      ,8,1,file%lun)/= 1) return ! 161-168
    if(cio_fread(trmaxg       ,8,1,file%lun)/= 1) return ! 169-176
    if(swap_endian() == 0 ) then
      call swap_bytes(number_traces)
      call swap_bytes(nwih)        
      call swap_bytes(ndpt)          
      call swap_bytes(dt)
      call swap_bytes(tstrt)
      call swap_bytes(xo)
      call swap_bytes(yo)
      call swap_bytes(dx)
      call swap_bytes(dn)
      call swap_bytes(buffer_size)
      call swap_bytes(five_twelve)
      call swap_bytes(trmaxg)
    endif
    if(packing_flag(1:3) /= 'YES') return
    if(file_type(1:6) /= 'STROT1') return
    if(five_twelve(2) /= 512)      return

    if(cio_fseek(file%lun,4096,0) /= cio_ok) return

    file%ftype      = 'QTROT'
    file%num_traces = number_traces(2)
    file%recl       = nwih(2)+ndpt(2)
    if(modulo(file%recl,2) /= 0 ) file%recl = file%recl+1
    file%recl       = file%recl*4
    file%nwih       = nwih(2)
    file%num_values = ndpt(2)
    file%data_start_pos(1) = 0
    file%data_start_pos(2) = 4096
    file%dt         = dt
    file%tmin       = tstrt
    file%nbits      = 32
    file%nbits_hd   = 32
    file%tmax       = tstrt+(ndpt(2)-1)*dt
    file%common%xorigin = xo
    file%common%yorigin = yo
    file%common%dx11    = dx(1)
    file%common%dx12    = dx(2)
    file%common%dx21    = dx(3)
    file%common%dx22    = dx(4)
    file%wtype      = 'IEEE'
    file%endian     = 1 ! always written in SUN format
    if(version == 'CPSQTROT') then
      file%trmaxg = trmaxg
    endif

    status          = trcio_ok

  end function trcio_read_qtrot_header

  function trcio_write_qtrot_header(file) result (status)
    type(trcio_struct),pointer           :: file
    integer                              :: status
    !--- local variables ---
    !------------- start of header------------
    integer,dimension(:)                 :: number_traces(2)          !(  1:  8)
    character(len=8)                     :: packing_flag              !(  9: 16)
    character(len=8)                     :: history_flag              !( 17: 24)
    integer,dimension(:)                 :: nwih(2)                   !( 25: 32)
    integer,dimension(:)                 :: ndpt(2)                   !( 33: 40)
    real                                 :: dt,tstrt,xo,yo,dx(4),dn(4)!( 41: 88)
    character(len=8)                     :: file_type                 !(137:144)
    integer,dimension(:)                 :: buffer_size(2)            !(145:152)
    integer,dimension(:)                 :: five_twelve(2)            !(153:160)
    character(len=8)                     :: version                   !(161:168)
    double precision                     :: trmaxg                    !(169:176)
    !------------- end of header -------------

    call cio_frewind(file%lun)
    status = trcio_error
    if(modulo(file%num_values+file%nwih , 2) /= 0 ) then
      ! *** error because buffer_size will be incorrect
      write(file%common%stdout,'(a)')&
         'trcio_write_qtrot_header: buffer_size is incorrect.'
      return
    endif
      
    file_type        = 'STROT1'
    five_twelve      = (/0,512/)
    number_traces    = (/0,file%num_traces/)
    nwih             = (/0,file%nwih/)
    ndpt             = (/0,file%num_values/)
    dt               = file%dt
    tstrt            = file%tmin
    xo               = file%common%xorigin
    yo               = file%common%yorigin
    dx               = (/file%common%dx11,file%common%dx12, &
                         file%common%dx21,file%common%dx22/)
    dn               = 0
    packing_flag     = 'YES'
    buffer_size      = (/0,((nwih(2)+1)/2 +(ndpt(2)+1)/2) /)  ! in 8 byte words
    trmaxg           = file%trmaxg
    version          = 'CPSQTROT'
    if(swap_endian() == 0 ) then
      call swap_bytes(number_traces)
      call swap_bytes(nwih)        
      call swap_bytes(ndpt)          
      call swap_bytes(dt)
      call swap_bytes(tstrt)
      call swap_bytes(xo)
      call swap_bytes(yo)
      call swap_bytes(dx)
      call swap_bytes(dn)
      call swap_bytes(buffer_size)
      call swap_bytes(five_twelve)
      call swap_bytes(trmaxg)
    endif
    
    if(cio_fwrite(number_traces,8,1,file%lun)/= 1) return ! 1-8
    if(cio_fwrite(packing_flag ,8,1,file%lun)/= 1) return ! 9-16
    if(cio_fwrite(history_flag ,8,1,file%lun)/= 1) return ! 17-24
    if(cio_fwrite(nwih         ,8,1,file%lun)/= 1) return ! 25-32
    if(cio_fwrite(ndpt         ,8,1,file%lun)/= 1) return ! 33-40
    if(cio_fwrite(dt           ,4,1,file%lun)/= 1) return ! 41-44
    if(cio_fwrite(tstrt        ,4,1,file%lun)/= 1) return ! 45-48
    if(cio_fwrite(xo           ,4,1,file%lun)/= 1) return ! 49-52
    if(cio_fwrite(yo           ,4,1,file%lun)/= 1) return ! 53-56
    if(cio_fwrite(dx           ,4,4,file%lun)/= 4) return ! 57-72
    if(cio_fwrite(dn           ,4,4,file%lun)/= 4) return ! 73-88
    if(cio_fseek(file%lun,136,0) /= cio_ok) return
    if(cio_fwrite(file_type    ,8,1,file%lun)/= 1) return ! 137-144
    if(cio_fwrite(buffer_size  ,8,1,file%lun)/= 1) return ! 145-152
    if(cio_fwrite(five_twelve  ,8,1,file%lun)/= 1) return ! 153-160
    if(cio_fwrite(version      ,8,1,file%lun)/= 1) return ! 161-168
    if(cio_fwrite(trmaxg       ,8,1,file%lun)/= 1) return ! 169-176

    file%data_start_pos = (/0,4096/)
    if(cio_fseek(file%lun,4096,0) /= cio_ok) return
    status = trcio_ok

  end function trcio_write_qtrot_header

  subroutine trcio_print(file,msg)
    type(trcio_struct),pointer              :: file
    character(len=*),intent(in)             :: msg
    if(.not. associated(file)) then 
      write(6,*) trim(msg)
    else
      write(file%common%stdout,*) trim(msg)
    endif
  end subroutine trcio_print

  function trcio_num_global_cards(file) result (num_global_cards)
    type(trcio_struct),pointer               :: file
    integer                                  :: num_global_cards
    !---------- local variables ...
    character(len=256)                       :: err
    character(len=10)                        :: secnam='jobglobals'

    num_global_cards = -1
  !
    write(err,*)'"trcio_struct" file object not associated.'
    if(.not. associated(file) ) go to 999
  
    !write(err,*)'job-global cards not available. File type not "trcio".'
    !if(.not. cpsio_test(file%lun)  ) go to 999

    if(file%ftype == 'CMPR') then
      ! wmm 2006/05/18 removed cmprio support
      !num_global_cards = cmprio_num_global_cards(file%lun) 2006/05/18 wmm 
      return
    endif

    if(file%ftype /= 'TRCIO' .and. file%ftype(1:3) /= 'LBO') go to 999

    num_global_cards = cpsio_number_keywords(file%lun,secnam)
    write(err,*)'No job-global keywords found in file. (',num_global_cards,')'
    if(num_global_cards <= 0 ) go to 999
    return

999 continue
    !write(file%common%stdout,*)'Error (trcio_num_global_cards): ',trim(err)
  end function trcio_num_global_cards
  
  function trcio_read_globals(file,num_global_cards,global_cards) &
  result (status)
    type(trcio_struct),pointer               :: file
    integer,intent(in)                       :: num_global_cards
    character(len=*),dimension(:),intent(out):: global_cards(num_global_cards)
    integer                                  :: status
    !---------- local variables ...
    character(len=256)                       :: s
    character(len=512)                       :: s2
    integer                                  :: i,j,k,num_kw,num_card,nel
    character(len=64),dimension(:),allocatable :: kwlist
    character(len=10)                        :: secnam='jobglobals'
    character(len=80),dimension(:),allocatable :: val

    if(.not. associated(file) ) then
      status = trcio_error
      return
    endif

    if(file%ftype == 'CMPR') then
      !status = cmprio_read_globals(file%lun, num_global_cards,global_cards)
      ! wmm 2006/05/18 removed cmprio support
      status = trcio_error
      return
    endif

    status = trcio_ok

    num_kw = trcio_num_global_cards(file)
    !write(err,*)'No global keywords available to read.'
    if( num_kw <= 0 ) return

    global_cards(:)(:) = ' '

    allocate(kwlist(num_kw))
    k = min(len(global_cards(1)),len(s))
    status = cpsio_get_keywordlist(file%lun,kwlist,secnam)
    num_card = 0
    do i = 1, num_kw
      nel = cpsio_get_keyword_nelem(file%lun,kwlist(i),secnam)
      if(nel <= 0 ) exit
      allocate(val(nel))
      if(nel > 1 ) then
        status = cpsio_get_keyword(file%lun,kwlist(i),val,nel,secnam)
        s = trim(kwlist(i))//'=('
        do j = 1, nel-1
          ! s2 is used to eliminate compiler warning on ifc.
          s2=trim(s)//trim(val(j))//','
          s = s2(1:256)
        end do
        s2=trim(s)//trim(val(nel))//')'
        s = s2(1:256)
      else
        status = cpsio_get_keyword(file%lun,kwlist(i),val(1),secnam)
        s = trim(kwlist(i))//'='//trim(val(1))
      endif
      deallocate(val)
      num_card = num_card + 1
      global_cards(num_card)(1:k) = trim(s(1:k))
    end do
    deallocate(kwlist)
    status = trcio_ok
    return

  end function trcio_read_globals

  function trcio_write_globals(file,num_global_cards,global_cards) &
  result(status)
    type(trcio_struct),pointer               :: file
    integer,intent(in)                       :: num_global_cards
    character(len=*),dimension(:),intent(in) :: global_cards(num_global_cards)
    integer                                  :: status
    !---------- local variables ...

    character(len=80)                        :: s
    integer                                  :: i
    character(len=256)                       :: err

    status = trcio_ok
  !
    write(err,*)'"trcio_struct" file object not associated.'
    if(.not. associated(file) ) go to 999
  
    select case (file%ftype)
      case ('TRCIO','LBO1','LBO2','LBO')
        write(err,*)'Problem flushing buffers.'
        if(cio_fflush(file%lun) /= cio_ok) go to 999
    
        write(err,*)'Problem seeking to EOF.'
        if(cio_fseek(file%lun,0,2) /= cio_ok) go to 999 ! seek to eof.
    
        write(s,'(a)')'#<HDR_jobglobals>'
        write(err,*)'"cio_fputline" error while writing globals header.'
        if(cio_fputline(trim(s),unit=file%lun) < 0 ) go to 999
    
        write(err,*)'"cio_fputline" error while writing job globals.'
        do i = 1, num_global_cards
          if(cio_fputline('# '//trim(global_cards(i)),unit=file%lun)< 0) go to 999
        end do
    
        write(s,'(a)')'#</jobglobals>'
        write(err,*)'"cio_fputline" error while writing globals trailer.'
        if(cio_fputline(trim(s),unit=file%lun) < 0 ) go to 999
    
        write(err,*)'Problem flushing buffers.'
        if(cio_fflush(file%lun) /= cio_ok) go to 999

      case default
        write(err,*)'No job-globals available.  File type not "trcio".', file%ftype
    end select
    return

999 status = trcio_error
    !write(file%common%stdout,'(A)')'Error (trcio_write_globals): '//trim(err) 
  end function trcio_write_globals

  subroutine trcio_write_history_cards(file,history,process_number_arg)
    type(trcio_struct),pointer              :: file
    character(len=*),intent(in)             :: history
    integer, optional                       :: process_number_arg
    !---------- local variables ...

    character(len=80) :: card,err

    integer :: process_number,status
  
    if(.not. associated(file) ) return

    select case (file%ftype)
      case ('TRCIO','LBO1','LBO2','LBO')
        if(present(process_number_arg))then
          process_number=process_number_arg
        else
          process_number=0
        endif
      
        write(err,*)'error flushing buffers.'
        status = cio_fflush(file%lun)
        if(status /= 0 ) go to 9999
        !--- File should be perfectly positioned so that history is going to be
        !--- written here.  "here" is just after the ascii CPS header section. 
        !--- We will begin at EOF.
        write(err,*)'error seeking to eof.'
        status = cio_fseek(file%lun,0,2) ! seek to EOF
        if(status /= 0 ) go to 9999
    
        write(card,'(a)')'#<DTA_history>'
        write(err,*)'cio_fputline error while writing history header.'
        if(cio_fputline(trim(card),len_trim(card),file%lun) < 0 ) go to 9999
    
        write(err,*)'error flushing buffers.'
        status = cio_fflush(file%lun)
        if(status /= cio_ok) go to 9999
        write(err,*)'error determining file position.'
        status = cio_ftell(file%lun,file%hist_start_pos)
        if(status /= cio_ok) go to 9999
    
        if(history.ne.'NONE')then
          call manhist_cio(file%lun,history,process_number)
        endif
    
        write(err,*)'error flushing buffers.'
        status = cio_fflush(file%lun)
        if(status /= cio_ok) go to 9999
    
        write(err,*)'error determining file position.'
        status = cio_ftell(file%lun,file%hist_end_pos)
        if(status /= cio_ok) go to 9999
    
        !---- end the history section
        write(card,'(a)')'#</history>'
        if(cio_fputline(trim(card),len_trim(card),file%lun) < 0 ) go to 9999
    
        write(err,*)'error flushing buffers.'
        status = cio_fflush(file%lun)
        if(status /= cio_ok) go to 9999
    
        file%history_written = .true.
        write(err,*)'error updating file header.'
        status = trcio_update_header(file)
        if (status /= trcio_ok) go to 9999
    
        if(status /= 0 ) go to 9999

      case default
    end select

    return
    
    9999  continue
          write(file%common%stdout,'(A)')'TRCIO_WRITE_HISTORY_CARDS: ERROR: '&
                                   //trim(err) 
    
  end subroutine trcio_write_history_cards


  subroutine trcio_read_history_cards(file,status)
    type(trcio_struct),pointer              :: file
    integer,intent(out)                     :: status
    
    character(len=80) :: card

    integer :: i,tot_cards_in_all_histories,num_errs

    status=0 
    num_errs=0
    if(.not. associated(file) ) go to 8000
 
    select case (file%ftype)
      case ('CMPR')
        status=trcio_error ! wmm 2006/05/18 removed cmprio support
        !call cmprio_read_history_cards(file%lun, status)
      case ('TRCIO','LBO','LBO1','LBO2')
        if(file%hist_start_pos(1) == file%hist_end_pos(1)  .and. &
           file%hist_start_pos(2) == file%hist_end_pos(2)  )then
             go to 8000
        endif
        tot_cards_in_all_histories = cpsio_number_cards(file%lun,'HISTORY')
        if(tot_cards_in_all_histories .le. 0)go to 8000
        if(cpsio_position_file(file%lun,1,'HISTORY') /= cpsio_ok)then
          write(file%common%stdout) &
              'TRCIO-->cannot position file to history section.'
          go to 8000
        endif
        do i = 1,tot_cards_in_all_histories+1
          status = cpsio_read(file%lun,card)
          if(status /= cpsio_ok)then
             write(file%common%stdout,*) &
              'TRCIO-->failed while reading history card # ',i
             go to 8000
          endif
          call manhist_putcard(card,status)
          if(status.ne.0)then
            write(file%common%stdout,*) &
              'TRCIO--> Error putting History card ',i,'[',card,'].'
            num_errs = num_errs + 1
            status=0 ! reset status, I don't care about this.
          endif
        enddo
      case default
    end select 

 8000 continue
  if(num_errs > 0 ) write(file%common%stdout,*) &
    'TRCIO--> There were ',num_errs, &
    ' errors while putting history cards into object' 

  end subroutine trcio_read_history_cards    


  function trcio_set_ipn(file,ipn) result(status)
    type(trcio_struct),pointer     :: file
    integer,intent(in)             :: ipn
    !----- result -----
    integer                        :: status
    if(.not. associated(file) ) then
      status = -1
      return
    else
      status = 0
    endif
    file%common%ipn = ipn
  end function trcio_set_ipn

  function trcio_set_stdout(file,stdout) result (status)
    type(trcio_struct),pointer     :: file
    integer,intent(in)             :: stdout
    !----- result -----
    integer                        :: status
    if(.not. associated(file) ) then
      status = -1
      return
    else
      status = 0
    endif
    file%common%stdout = stdout
  end function trcio_set_stdout

  function trcio_get_trmaxg(file) result (trmaxg)
    type(trcio_struct),pointer     :: file
    !----- result -----
    double precision               :: trmaxg
    if(.not. associated(file) ) then
      trmaxg = 0d0
      return
    endif
    trmaxg = file%trmaxg
  end function trcio_get_trmaxg

  function trcio_determine_ftype(filename) result (ftype)
    character(len=*),intent(in)    :: filename
    character(len=5)               :: ftype

    integer                        :: status
    integer                        :: unit
    character(len=8)               :: prolog
    character(len=6)               :: file_type
    character(len=80)              :: card
    type(trcio_struct),pointer     :: file

    integer                  :: ibuf(60)
    integer                  :: bsiz,wblk,wbyt
    double precision         :: rsize
    double precision         :: csize  !computed size
    type(segy_trc_hdr)       :: syh
    integer    :: bytes_per_trace
    integer    :: ntrfil
    integer    :: endian,i
    logical    :: swap
    !--- added for JSEIS
    integer    :: alt_lun
    
    ftype = 'UNK'

    ! -- look for JSEIS files first
    alt_lun = jsf_wrapper_getlun(filename,'r')
    if(alt_lun > 0 ) then
      ftype = 'JSEIS'
      return
    endif
    
    ! -- now look for cpsio files
    unit = cio_fopen(trim(filename),'r')
    if(unit < 0 ) then
      status = cio_error
      return
    endif

    !------- test for a cpsio file type first ---------
    status = cio_fread(card,80,1,unit) - 1
    prolog=card(1:8) 
    if(prolog == '#<CPS_v1') then
      if(index(card,'LBO2') > 0 .or. index(card,'lbo2') > 0 ) then
        ftype = 'LBO2'
      elseif(index(card,'LBO') > 0 .or. index(card,'lbo') > 0 ) then
        ftype = 'LBO'
      elseif(index(card,'TRCIO') > 0 .or. index(card,'trcio') > 0 ) then
        ftype = 'TRCIO'
      elseif(index(card,'CMPR') > 0 .or. index(card,'cmpr') > 0 ) then
        ftype = 'CMPR'
      endif
      status = cio_fclose(unit)
      return
    endif

    !------- test for segy file next ---------------
    call cio_frewind(unit)
    if(0 == segy_is_file_segy_c(unit)) then
      ftype = 'SEGY'
      status = cio_fclose(unit)
      return
    endif


    !------- test for su file next ---------------
    call cio_frewind(unit)
    status = cio_fread(ibuf,240,1,unit) - 1
    swap = .false.
    endian = swap_endian()
    if(endian ==0) swap= .true. !file big endian, but mem is little
    call segy_unpack_segyhd(syh,ibuf(1:60),swap)
    bytes_per_trace = 240 + 4*syh%ns

    i     = cio_fseek(unit, 0, 2)    !go to end of file
    bsiz  = 1024
    status= cio_ftell(unit,bsiz,wblk,wbyt)     !get byte position
    rsize = bsiz
    rsize = rsize*wblk
    rsize = rsize + wbyt
    ntrfil = (rsize)/bytes_per_trace
    csize =  ntrfil*bytes_per_trace
    if(csize==rsize) then
      ftype='SU'
      status = cio_fclose(unit)
      return
    endif

    !------- test for qtrot file next ---------------
    status = cio_fseek(unit,136,0)
    status = cio_fread(file_type,6,1,unit ) - 1
    if(file_type == 'STROT1') then
      ftype = 'QTROT'
      status = cio_fclose(unit)
      return
    endif

    !----- test for TFIL last ---------------------
    call cio_frewind(unit)
    file => trcio_create()
    if(associated(file) )  then
      file%filename = trim(filename)
      file%lun      = unit
  
      status = trcio_read_tfil_header(file)
      call trcio_delete(file)
  
      if(status == 0 ) then
        ftype = 'TFIL'
        status = cio_fclose(unit)
        return
      endif
    endif

    !---------- unknown --------------------------
    ftype = 'UNK'
    status = cio_fclose(unit)
    
  end function trcio_determine_ftype

  function trcio_read_cpsio_header(file) result (status)
    type(trcio_struct),pointer                 :: file
    integer                                    :: status

    integer                                    :: i
    character(len=64),dimension(:),allocatable :: sectionlist
    integer                                    :: nelm
    integer                                    :: data_start_pos(2)
    integer                                    :: file_size(2)
    integer                                    :: old_extent_length
    integer                                    :: extent_length
    integer                                    :: num_ext,num_byt
    type(ll)                                   :: LL_file_size
    type(ll)                                   :: LL_data_start_pos
    type(ll)                                   :: LL_bytes_for_traces

        allocate(sectionlist(cpsio_number_sections(file%lun)))
        if(cpsio_get_sectionlist(file%lun,sectionlist) /= 0) then
          call &
          trcio_print(file,&
          'trcio_read_cpsio_header: &
          &ERROR-> Problem reading file header section list.')
          status = trcio_error
          deallocate(sectionlist)

          return !--- FATAL ERROR ... 
                 !--- COULD NOT READ THE SECTION LIST FROM CPSIO FILE.

        endif
      
        !------ PROCESS THE HISTORY HEADER SECTION OF THE TRCIO FILE
        file%hist_start_pos = 0
        file%hist_end_pos   = 0
        do  i = 1,cpsio_number_sections(file%lun) 
          if(trim(sectionlist(i)) == 'HISTORY') exit
        end do
        if( i <= cpsio_number_sections(file%lun) ) then
          !--------- the HISTORY header section exists.
          status = cpsio_get_keyword(file%lun, &
                'hist_end_pos',file%hist_end_pos,nelm,section_name='HISTORY')
          status = cpsio_get_keyword(file%lun, &
                   'hist_start_pos',file%hist_start_pos,nelm,&
                   section_name='HISTORY')
          status = cpsio_get_keyword(file%lun, &
                'hist_end_byte',file%hist_end_pos(2),section_name='HISTORY')
          status = cpsio_get_keyword(file%lun, &
              'hist_start_byte',file%hist_start_pos(2),section_name='HISTORY')
        endif
        if(file%hist_start_pos(1) /= file%hist_end_pos(1) .or. &
           file%hist_start_pos(2) /= file%hist_end_pos(2) )    &
           file%history_written=.true.
  
        !------ PROCESS THE SEISMIC HEADER SECTION OF THE TRCIO FILE
        do  i = 1,cpsio_number_sections(file%lun) 
          if(trim(sectionlist(i)) == 'SEISMIC') exit
        end do
        if( i <= cpsio_number_sections(file%lun) ) then 
          !----------- the SEISMIC section exists.
          status = cpsio_get_keyword(file%lun, &
                   'retention',file%retention,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'ftype',file%ftype,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'endian',file%endian,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'wtype',file%wtype,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nbits',file%nbits,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nbits_hd',file%nbits_hd,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'tmin',file%tmin,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'tmax',file%tmax,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'dt',file%dt,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nhd1',file%nhd1,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nhd2',file%nhd2,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nhd3',file%nhd3,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vwidth1',file%vwidth1,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vwidth2',file%vwidth2,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vwidth3',file%vwidth3,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'nwih',file%nwih,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'num_values',file%num_values,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vbin1',file%vbin1,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vbin2',file%vbin2,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vbin3',file%vbin3,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmin1',file%vmin1,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmin2',file%vmin2,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmin3',file%vmin3,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmax1',file%vmax1,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmax2',file%vmax2,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'vmax3',file%vmax3,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'xorigin',file%common%xorigin,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'yorigin',file%common%yorigin,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'dx11',file%common%dx11,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'dx12',file%common%dx12,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'dx21',file%common%dx21,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'dx22',file%common%dx22,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'record_length',file%recl,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'trmaxg',file%trmaxg,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'checksum',file%checksum,section_name='SEISMIC')
          file%num_traces = 0
          status = cpsio_get_keyword(file%lun, &
                   'num_traces',file%num_traces,section_name='SEISMIC')
          file%data_end_pos = 0
          status = cpsio_get_keyword(file%lun, &
               'data_end_byte',file%data_end_pos(2),section_name='SEISMIC')
          file%data_start_pos = 0
          status = cpsio_get_keyword(file%lun, &
              'data_start_byte',file%data_start_pos(2),section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
               'data_end_pos',file%data_end_pos,nelm,section_name='SEISMIC')
          status = cpsio_get_keyword(file%lun, &
                   'data_start_pos',file%data_start_pos, &
                   nelm,section_name='SEISMIC')
          status = cpsio_position_file(file%lun,1,'SEISMIC')
          ! === the ...pos arrays have the extent in (1) and byte offset in(2)
          status = cio_ftell(file%lun,data_start_pos)
          if(data_start_pos(1) /= file%data_start_pos(1) .or. &
             data_start_pos(2) /= file%data_start_pos(2)  ) then
            file%data_start_pos = data_start_pos
          endif
          file%data_end_pos(1) = &
           max(file%data_end_pos(1),file%data_start_pos(1))
          file%data_end_pos(2) = &
           max(file%data_end_pos(2),file%data_start_pos(2))
          if(file%data_end_pos(1) /= file%data_start_pos(1) .or. &
             file%data_end_pos(2) /= file%data_start_pos(2) )    &
             file%first_trace_written=.true.
          if(file%tmax == 0.0 ) then
            file%tmax = file%tmin + (file%num_values-1)*file%dt
          endif

        endif
        deallocate(sectionlist)
        !------ 7/28/03 wmm need to correct for numtraces error if file not
        !------         written out correctly (because of job abort...)
        if(file%num_traces == 0 ) then
          !--- save the long-long maximum integer value being used currently
          old_extent_length=ll_get_maxintval()
          !figure out file extent size
          extent_length=cio_get_file_ext_size(file%lun)
          !--- tell long-long module that maximum integer is the file's extlng.
          status=ll_set_maxintval(extent_length)
          !--- get the file size (two word result: 1) num extent 2) bytes)
          call cio_flsz(file%lun,file_size)
          LL_file_size = file_size
          LL_data_start_pos = file%data_start_pos
          !calculate number of records by subtracting data_start_pos
          LL_bytes_for_traces=LL_file_size-LL_data_start_pos
          num_ext=ll_get_hi(LL_bytes_for_traces)
          num_byt=ll_get_lo(LL_bytes_for_traces)
          ! calculate number of traces total
          ! and insert num-records into file%num_traces
          file%num_traces  = num_ext*(extent_length/file%recl) + &
              (num_ext*(extent_length-(extent_length/file%recl)*file%recl)&
              + num_byt) / file%recl
          !--- in case system was using the extent length, put it back.
          status=ll_set_maxintval(old_extent_length)
        endif

        status = 0

  end function trcio_read_cpsio_header


  integer function trcio_write_section(file,section,ncards,cards) result(status)
    type(trcio_struct),intent(in) :: file
    character(len=*),intent(in)   :: section
    integer,intent(in)            :: ncards
    character(len=*),intent(in)   :: cards(:)
    ! LOCAL vars.
    character(len=81)              :: s
    integer                        :: u,ls,i
    status = TRCIO_OK ! start with good status.
    if(ncards==0) return
    u = file%lun  ! unit to write to.
    if(u<0) return
    ls = len(s)   ! length of string to put
    status = cio_fflush(file%lun)
    status = cio_fseek(file%lun,0,2)
    write(s,'(a)') '#<HDR_'//trim(section)//'>'
    if(cio_fputline(trim(s),len_trim(s),u) < 0 ) goto 999
    do i = 1,min(ncards,size(cards))
      if(cards(i)(1:1) /= '#') then
        s='#'//cards(i)
      else
        s=cards(i)
      endif
      if(cio_fputline(trim(s),len_trim(s),u) < 0 ) goto 999
    enddo
    write(s,'(a)') '#</'//trim(section)//'>'
    if(cio_fputline(trim(s),len_trim(s),u) < 0 ) goto 999
    status = cio_fflush(file%lun)
    if(status /= cio_ok) goto 999
    return
999 status = TRCIO_ERROR
  end function trcio_write_section

  !------------ ACCESS FUNCTIONS INTO/OUT OF the TRCIO_STRUCT ------------

  function trcio_set_lun(file,lun) result(status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: lun
    !---- result
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated(file) ) return
    file%lun = lun
    status = trcio_ok
  end function trcio_set_lun

  function trcio_get_lun(file) result(lun)
    type(trcio_struct),pointer :: file
    !---- result
    integer                    :: lun
    !---- code
    lun = 0
    if(.not. associated(file) ) return
    lun = file%lun
  end function trcio_get_lun

  function trcio_set_filename(file,filename) result(status)
    type(trcio_struct),pointer    :: file
    character(len=*),intent(in) :: filename
    !---- result
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated(file) ) return
    file%filename = filename(:min(160,len(filename)))
    status = trcio_ok
  end function trcio_set_filename

  function trcio_get_filename(file) result(filename)
    type(trcio_struct),pointer :: file
    !---- result
    character(len=160)         :: filename
    !---- code
    filename = '----'
    if(.not. associated(file) ) return
    filename = file%filename
  end function trcio_get_filename

  function trcio_set_io_mode(file,io_mode) result(status)
    type(trcio_struct),pointer :: file
    character(len=*),intent(in):: io_mode
    !---- result
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated(file) ) return
    file%io_mode = io_mode(:min(2,len(io_mode)))
    status = trcio_ok
  end function trcio_set_io_mode

  function trcio_get_io_mode(file) result(io_mode)
    type(trcio_struct),pointer :: file
    !---- result
    character(len=2)           :: io_mode
    !---- code
    io_mode = '--'
    if(.not. associated(file) ) return
    io_mode = file%io_mode
  end function trcio_get_io_mode

  function trcio_set_permission(file,permission) result(status)
    type(trcio_struct),pointer  :: file
    character(len=*),intent(in):: permission
    !---- result
    integer                     :: status
    !---- code
    status = trcio_error
    if(.not. associated(file) ) return
    file%permission = permission(:min(16,len(permission)))
    status = trcio_ok
  end function trcio_set_permission

  function trcio_get_permission(file) result(permission)
    type(trcio_struct),pointer :: file
    !---- result
    character(len=16)          :: permission
    !---- code
    permission = '---------'
    if(.not. associated(file) ) return
    permission = file%permission
  end function trcio_get_permission

  function trcio_set_retention(file,retention) result(status)
    type(trcio_struct),pointer :: file 
    integer,intent(in)         :: retention
    !---- result
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated(file) ) return
    file%retention = retention
    status = trcio_ok
  end function trcio_set_retention

  function trcio_get_retention(file) result(retention)
    type(trcio_struct),pointer :: file 
    !---- result
    integer                    :: retention
    !---- code
    retention = 0 
    if(.not. associated(file) ) return
    retention = file%retention
  end function trcio_get_retention

  function trcio_set_ftype(file,ftype) result (status)
    type(trcio_struct),pointer :: file
    character(len=*),intent(in):: ftype
                              ! TRCIO, LBO, LBO2, SEGY, VOXET, SU, QTROT
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%ftype = ftype(:min(5,len(ftype)))
    if(file%ftype == 'LBO') file%wtype = 'LBO'
    if(file%ftype == 'LBO2') file%wtype = 'LBO2'
    status = trcio_ok
  end function trcio_set_ftype

  function trcio_get_ftype(file) result (ftype)
    type(trcio_struct),pointer :: file
    !---- result 
    character(len=5)           :: ftype
                              ! TRCIO, LBO, LBO2, SEGY, VOXET, SU, or QTROT
    ftype = 'UNK'
    if(.not. associated (file) ) return
    ftype = file%ftype
  end function trcio_get_ftype

  function trcio_set_endian(file,endian) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: endian ! 0      ! 0(intel,dec) or 1(sun)
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%endian = endian
    status = trcio_ok
  end function trcio_set_endian

  function trcio_get_endian(file) result (endian)
    type(trcio_struct),pointer :: file
    !---- result 
    integer             :: endian         ! 0      ! 0(intel,dec) or 1(sun)
    !---- code
    endian = -1
    if(.not. associated (file) ) return
    endian = file%endian
  end function trcio_get_endian

  function trcio_set_wtype(file,wtype) result (status)
    type(trcio_struct),pointer :: file
    character(len=*),intent(in):: wtype ! IEEE or IBM or IBM2 or BYTE or SBYT
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%wtype = wtype(:min(4,len(wtype)))
    status = trcio_ok
  end function trcio_set_wtype

  function trcio_get_wtype(file) result (wtype)
    type(trcio_struct),pointer :: file
    !---- result 
    character(len=4)           :: wtype ! IEEE or IBM or IBM2 or BYTE or SBYT
    !---- code
    wtype = 'UNON'
    if(.not. associated (file) ) return
    wtype = file%wtype
  end function trcio_get_wtype

  function trcio_set_nbits(file,nbits) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: nbits          ! 32     ! 1 - 64 bits/sample
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%nbits = nbits
    status = trcio_ok
  end function trcio_set_nbits

  function trcio_get_nbits(file) result (nbits)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: nbits          ! 32     ! 1 - 64 bits/sample
    !---- code
    nbits = -1 
    if(.not. associated (file) ) return
    nbits = file%nbits
  end function trcio_get_nbits


  function trcio_set_nbits_hd(file,nbits_hd) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: nbits_hd ! 64     ! 32, 64 bits/header word.
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%nbits_hd = nbits_hd
    status = trcio_ok
  end function trcio_set_nbits_hd

  function trcio_get_nbits_hd(file) result (nbits_hd)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: nbits_hd ! 64     ! 32, 64 bits/header word.
    !---- code
    nbits_hd = -1
    if(.not. associated (file) ) return
    nbits_hd = file%nbits_hd
  end function trcio_get_nbits_hd

  function trcio_set_tmin(file,tmin) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: tmin ! 0.0    ! mintime of trace in seconds
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%tmin = tmin
    status = trcio_ok
  end function trcio_set_tmin

  function trcio_get_tmin(file) result (tmin)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: tmin ! 0.0    ! mintime of trace in seconds
    !---- code
    tmin = -999.
    if(.not. associated (file) ) return
    tmin = file%tmin 
  end function trcio_get_tmin

  function trcio_set_tmax(file,tmax) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: tmax ! 0.0    ! maxtime of trace in seconds
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%tmax = tmax
    status = trcio_ok
  end function trcio_set_tmax

  function trcio_get_tmax(file) result (tmax)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: tmax ! 0.0    ! maxtime of trace in seconds
    !---- code
    tmax = -999.
    if(.not. associated (file) ) return
    tmax = file%tmax
  end function trcio_get_tmax


  function trcio_set_dt(file,dt) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: dt ! 0.0    ! sample rate in seconds.
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%dt = dt
    status = trcio_ok
  end function trcio_set_dt

  function trcio_get_dt(file) result (dt)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: dt ! 0.0    ! sample rate in seconds.
    !---- code
    dt = -1.
    if(.not. associated (file) ) return
    dt = file%dt
  end function trcio_get_dt

  function trcio_set_nhd1(file,nhd1) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: nhd1 ! 8      ! primary bin coordinate
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%nhd1 = nhd1
    status = trcio_ok
  end function trcio_set_nhd1

  function trcio_get_nhd1(file) result (nhd1)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: nhd1 ! 8      ! primary bin coordinate
    !---- code
    nhd1 = -1
    if(.not. associated (file) ) return
    nhd1 = file%nhd1
  end function trcio_get_nhd1

  function trcio_set_nhd2(file,nhd2) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: nhd2 ! 8      ! primary bin coordinate
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%nhd2 = nhd2
    status = trcio_ok
  end function trcio_set_nhd2

  function trcio_get_nhd2(file) result (nhd2)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: nhd2 ! 8      ! primary bin coordinate
    !---- code
    nhd2 = -1
    if(.not. associated (file) ) return
    nhd2 = file%nhd2
  end function trcio_get_nhd2

  function trcio_set_nhd3(file,nhd3) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: nhd3 ! 8      ! primary bin coordinate
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%nhd3 = nhd3
    status = trcio_ok
  end function trcio_set_nhd3

  function trcio_get_nhd3(file) result (nhd3)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: nhd3 ! 8      ! primary bin coordinate
    !---- code
    nhd3 = -1
    if(.not. associated (file) ) return
    nhd3 = file%nhd3
  end function trcio_get_nhd3

  function trcio_set_vwidth1(file,vwidth1) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vwidth1 ! 1.0    ! width of bin (nhd1 )
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vwidth1 = vwidth1
    status = trcio_ok
  end function trcio_set_vwidth1

  function trcio_get_vwidth1(file) result (vwidth1)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vwidth1 ! 1.0    ! width of bin (nhd1 )
    !---- code
    vwidth1 = -1.0
    if(.not. associated (file) ) return
    vwidth1 = file%vwidth1
  end function trcio_get_vwidth1


  function trcio_set_vwidth2(file,vwidth2) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vwidth2 ! 1.0    ! width of bin (nhd1 )
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vwidth2 = vwidth2
    status = trcio_ok
  end function trcio_set_vwidth2

  function trcio_get_vwidth2(file) result (vwidth2)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vwidth2 ! 1.0    ! width of bin (nhd1 )
    !---- code
    vwidth2 = -1.0
    if(.not. associated (file) ) return
    vwidth2 = file%vwidth2
  end function trcio_get_vwidth2



  function trcio_set_vwidth3(file,vwidth3) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vwidth3 ! 1.0    ! width of bin (nhd1 )
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vwidth3 = vwidth3
    status = trcio_ok
  end function trcio_set_vwidth3

  function trcio_get_vwidth3(file) result (vwidth3)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vwidth3 ! 1.0    ! width of bin (nhd1 )
    !---- code
    vwidth3 = -1.0
    if(.not. associated (file) ) return
    vwidth3 = file%vwidth3
  end function trcio_get_vwidth3



  function trcio_set_num_values(file,num_values) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: num_values
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%num_values = num_values
    status = trcio_ok
  end function trcio_set_num_values

  function trcio_get_num_values(file) result (num_values)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: num_values
    !---- code
    num_values = -1
    if(.not. associated (file) ) return
    num_values = file%num_values
  end function trcio_get_num_values

  function trcio_set_vbin1(file,vbin1) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vbin1
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vbin1 = vbin1
    status = trcio_ok
  end function trcio_set_vbin1

  function trcio_get_vbin1(file) result (vbin1)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vbin1
    !---- code
    vbin1 = -1.0
    if(.not. associated (file) ) return
    vbin1 = file%vbin1
  end function trcio_get_vbin1

  function trcio_set_vbin2(file,vbin2) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vbin2
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vbin2 = vbin2 
    status = trcio_ok
  end function trcio_set_vbin2

  function trcio_get_vbin2(file) result (vbin2) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vbin2
    !---- code
    vbin2 = -1.0
    if(.not. associated (file) ) return
    vbin2 = file%vbin2
  end function trcio_get_vbin2

  function trcio_set_vbin3(file,vbin3) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vbin3
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vbin3 = vbin3 
    status = trcio_ok
  end function trcio_set_vbin3

  function trcio_get_vbin3(file) result (vbin3) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vbin3
    !---- code
    vbin3 = -1.0
    if(.not. associated (file) ) return
    vbin3 = file%vbin3
  end function trcio_get_vbin3

  function trcio_set_vmin1(file,vmin1) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vmin1
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmin1 = vmin1
    status = trcio_ok
  end function trcio_set_vmin1

  function trcio_get_vmin1(file) result (vmin1)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vmin1
    !---- code
    vmin1 = -1.0
    if(.not. associated (file) ) return
    vmin1 = file%vmin1
  end function trcio_get_vmin1

  function trcio_set_vmin2(file,vmin2) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vmin2
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmin2 = vmin2 
    status = trcio_ok
  end function trcio_set_vmin2

  function trcio_get_vmin2(file) result (vmin2) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vmin2
    !---- code
    vmin2 = -1.0
    if(.not. associated (file) ) return
    vmin2 = file%vmin2
  end function trcio_get_vmin2

  function trcio_set_vmin3(file,vmin3) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vmin3
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmin3 = vmin3 
    status = trcio_ok
  end function trcio_set_vmin3

  function trcio_get_vmin3(file) result (vmin3) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vmin3
    !---- code
    vmin3 = -1.0
    if(.not. associated (file) ) return
    vmin3 = file%vmin3
  end function trcio_get_vmin3

  function trcio_set_vmax1(file,vmax1) result (status)
    type(trcio_struct),pointer :: file
    real,intent(in)            :: vmax1
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmax1 = vmax1
    status = trcio_ok
  end function trcio_set_vmax1

  function trcio_get_vmax1(file) result (vmax1)
    type(trcio_struct),pointer :: file
    !---- result 
    real                       :: vmax1
    !---- code
    vmax1 = -1.0
    if(.not. associated (file) ) return
    vmax1 = file%vmax1
  end function trcio_get_vmax1

  function trcio_set_vmax2(file,vmax2) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vmax2
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmax2 = vmax2 
    status = trcio_ok
  end function trcio_set_vmax2

  function trcio_get_vmax2(file) result (vmax2) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vmax2
    !---- code
    vmax2 = -1.0
    if(.not. associated (file) ) return
    vmax2 = file%vmax2
  end function trcio_get_vmax2

  function trcio_set_vmax3(file,vmax3) result (status)
    type(trcio_struct),pointer :: file 
    real,intent(in)            :: vmax3
    !---- result  
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%vmax3 = vmax3 
    status = trcio_ok
  end function trcio_set_vmax3

  function trcio_get_vmax3(file) result (vmax3) 
    type(trcio_struct),pointer :: file 
    !---- result  
    real                       :: vmax3
    !---- code
    vmax3 = -1.0
    if(.not. associated (file) ) return
    vmax3 = file%vmax3
  end function trcio_get_vmax3

  function trcio_set_xorigin(file,xorigin) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: xorigin
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%xorigin = xorigin
    status = trcio_ok
  end function trcio_set_xorigin

  function trcio_get_xorigin(file) result (xorigin)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: xorigin
    !---- code
    xorigin = -999.
    if(.not. associated (file) ) return
    xorigin = file%common%xorigin
  end function trcio_get_xorigin


  function trcio_set_yorigin(file,yorigin) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: yorigin
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%yorigin = yorigin
    status = trcio_ok
  end function trcio_set_yorigin

  function trcio_get_yorigin(file) result (yorigin)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: yorigin
    !---- code
    yorigin = -999.
    if(.not. associated (file) ) return
    yorigin = file%common%yorigin
  end function trcio_get_yorigin

  function trcio_set_dx11(file,dx11) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: dx11
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%dx11 = dx11
    status = trcio_ok
  end function trcio_set_dx11

  function trcio_get_dx11(file) result (dx11)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: dx11
    !---- code
    dx11 = -999.
    if(.not. associated (file) ) return
    dx11 = file%common%dx11
  end function trcio_get_dx11

  function trcio_set_dx12(file,dx12) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: dx12
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%dx12 = dx12
    status = trcio_ok
  end function trcio_set_dx12

  function trcio_get_dx12(file) result (dx12)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: dx12
    !---- code
    dx12 = -999.
    if(.not. associated (file) ) return
    dx12 = file%common%dx12
  end function trcio_get_dx12

  function trcio_set_dx21(file,dx21) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: dx21
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%dx21 = dx21
    status = trcio_ok
  end function trcio_set_dx21

  function trcio_get_dx21(file) result (dx21)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: dx21
    !---- code
    dx21 = -999.
    if(.not. associated (file) ) return
    dx21 = file%common%dx21
  end function trcio_get_dx21


  function trcio_set_dx22(file,dx22) result (status)
    type(trcio_struct),pointer :: file
    double precision,intent(in):: dx22
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%common%dx22 = dx22
    status = trcio_ok
  end function trcio_set_dx22

  function trcio_get_dx22(file) result (dx22)
    type(trcio_struct),pointer :: file
    !---- result 
    double precision           :: dx22
    !---- code
    dx22 = -999.
    if(.not. associated (file) ) return
    dx22 = file%common%dx22
  end function trcio_get_dx22

  function trcio_set_num_traces(file,num_traces) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: num_traces
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%num_traces = num_traces
    status = trcio_ok
  end function trcio_set_num_traces

  function trcio_get_num_traces(file) result (num_traces)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: num_traces
    !---- code
    num_traces = trcio_get_number_traces(file)
  end function trcio_get_num_traces

  function trcio_set_recl(file,recl) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: recl
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%recl = recl
    status = trcio_ok
  end function trcio_set_recl

  function trcio_get_recl(file) result (recl)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: recl
    !---- code
    recl = 0
    if(.not. associated (file) ) return
    recl = file%recl
  end function trcio_get_recl

  function trcio_set_trmaxg(file,trmaxg) result (status)
    type(trcio_struct),pointer :: file
    double precision           :: trmaxg
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%trmaxg = trmaxg
    status = trcio_ok
  end function trcio_set_trmaxg

!  function trcio_get_trmaxg(file) result (trmaxg)
!    type(trcio_struct),pointer :: file
!    !---- result 
!    double precision           :: trmaxg
!    !---- code
!    trmaxg = 0.0
!    if(.not. associated (file) ) return
!    trmaxg = file%trmaxg
!  end function trcio_get_trmaxg


  function trcio_set_hist_start_pos(file,hist_start_pos) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: hist_start_pos(2)
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%hist_start_pos = hist_start_pos
    status = trcio_ok
  end function trcio_set_hist_start_pos

  function trcio_get_hist_start_pos(file) result (hist_start_pos)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: hist_start_pos(2)
    !---- code
    hist_start_pos = (/0,0/)
    if(.not. associated (file) ) return
    hist_start_pos = file%hist_start_pos
  end function trcio_get_hist_start_pos

  function trcio_set_hist_end_pos(file,hist_end_pos) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: hist_end_pos(2)
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%hist_end_pos = hist_end_pos
    status = trcio_ok
  end function trcio_set_hist_end_pos

  function trcio_get_hist_end_pos(file) result (hist_end_pos)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: hist_end_pos(2)
    !---- code
    hist_end_pos = (/0,0/)
    if(.not. associated (file) ) return
    hist_end_pos = file%hist_end_pos
  end function trcio_get_hist_end_pos


  function trcio_set_data_start_pos(file,data_start_pos) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: data_start_pos(2)
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%data_start_pos = data_start_pos
    status = trcio_ok
  end function trcio_set_data_start_pos

  function trcio_get_data_start_pos(file) result (data_start_pos)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: data_start_pos(2)
    !---- code
    data_start_pos = (/0,0/)
    if(.not. associated (file) ) return
    data_start_pos = file%data_start_pos
  end function trcio_get_data_start_pos

  function trcio_set_data_end_pos(file,data_end_pos) result (status)
    type(trcio_struct),pointer :: file
    integer,intent(in)         :: data_end_pos(2)
    !---- result 
    integer                    :: status
    !---- code
    status = trcio_error
    if(.not. associated (file) ) return
    file%data_end_pos = data_end_pos
    status = trcio_ok
  end function trcio_set_data_end_pos

  function trcio_get_data_end_pos(file) result (data_end_pos)
    type(trcio_struct),pointer :: file
    !---- result 
    integer                    :: data_end_pos(2)
    !---- code
    data_end_pos = (/0,0/)
    if(.not. associated (file) ) return
    data_end_pos = file%data_end_pos
  end function trcio_get_data_end_pos


  !-----------------------------------------------------------------------------
  ! trcio_set_globals
  ! Purpose: To set the element globals in the cmprio structure
  !    Date: 2/13/00 Bill Menger
  ! Moved from cmprio_set_globals --- ehs 05apr02
  ! Only used by cmprio files, but moved to trcio so it can be accessed
  ! by trot.
  !-----------------------------------------------------------------------------
  subroutine trcio_set_globals(file,globals)
    type(trcio_struct),pointer                :: file
    character(len=80),dimension(:),intent(in) :: globals
    !---------------------------
    integer             :: num,status
    character(len = 80) :: err
    status = 0
    if(.not. associated (file) ) return
    num  = size(globals)
    file%common%num_global_cards = num
    if(associated(file%common%global_cards)) then
       deallocate(file%common%global_cards,stat=status)
    endif
    err='deallocate global_cards'
    if(status /= 0 )  go to 999
    
    allocate(file%common%global_cards(num),stat=status)
    err='allocate global_cards'
    if(status /= 0 )  go to 999

    file%common%global_cards(:num) = globals(:num)

    return
999 continue
    write(file%common%stdout,*) 'trcio_set_globals -> ', err

  end subroutine trcio_set_globals

  function trcio_read_su_header(file) result(status)
    type(trcio_struct),pointer           :: file
    integer                              :: status
    !--- LOCAL vars. ---
    integer                              :: endian
    !---
    integer                  :: ibuf(60)
    type(segy_trc_hdr)       :: syh
    integer    :: bytes_per_trace
    logical    :: swap

    call cio_frewind(file%lun)
    status = cio_fread(ibuf,240,1,file%lun) - 1
    if (status == 0 ) then
      swap = .false.
      endian = swap_endian()
      if(endian ==0) swap= .true. !file big endian, but mem is little
      call segy_unpack_segyhd(syh,ibuf(1:60),swap)
      bytes_per_trace = 240 + 4*syh%ns
      ! map syh to file object
      file%ftype      = 'SU'
      file%nwih       = 60
      file%num_values = syh%ns
      file%dt         = syh%dt*(1.e-6)
!     file%tmin       = 0.0
      file%tmax       = file%tmin + (file%num_values-1)*file%dt
      file%nbits      = 32 
      file%nbits_hd   = 32
      status = 0
      file%endian     = 0 !endian
      file%data_start_pos = (/0,0/)
      file%data_end_pos   = (/0,0/)
      file%common%xorigin = 0.0
      file%common%yorigin = 0.0
      file%common%dx11    = 1.0
      file%common%dx12    = 0.0
      file%common%dx21    = 0.0
      file%common%dx22    = 1.0
      file%nbits      = 32 
      file%wtype      = 'IEEE'
      file%recl = (file%nbits_hd*file%nwih + file%nbits*file%num_values)/8
     !ntrfil = (rsize)/bytes_per_trace
     !csize  =  ntrfil*bytes_per_trace
    endif

    status          = cio_fseek(file%lun,0,2)
    file%num_traces = trcio_tell_trace(file) - 1
    status          = cio_fseek(file%lun,file%data_start_pos,0)
   !print *,'trcio_read_su_header: endian=',endian,' swap=',swap,&
   !  'file endian=',file%endian,'status=',status,file%num_traces
      
  end function trcio_read_su_header

  function trcio_readheader(file) result (status)
    type(trcio_struct), pointer       :: file
    integer                           :: status

      status = TRCIO_OK
      select case(file%ftype) 
  
        case('TRCIO') !--- Standard CPS file type. (Requires lots of processing)
          status = trcio_read_cpsio_header(file)
          if(status /= 0 ) then
            call trcio_print(file, &
            'trcio_readheader: ERROR-> Problem reading TRCIO headers')
          endif
  
        case('LBO', 'LBO2') !--- Modified TRCIO format.
          status = trcio_read_cpsio_header(file)
          if(status /= 0 ) then
            call trcio_print(file, &
            'trcio_readheader: ERROR-> Problem reading LBO headers')
          endif
          file%recl = trcio_recl(file)
  
        case('SEGY')
          status = trcio_read_segy_headers(file)
          if(status /= 0 ) then
            call trcio_print(file, &
            'trcio_readheader: ERROR-> Problem reading SEGY headers')
          endif
  
        case('QTROT')
          status = trcio_read_qtrot_header(file)
          if(status /= 0 )  then
            call trcio_print(file, &
            'trcio_readheader: ERROR-> Problem reading QTROT header.')
          endif
  
        case('SU')
          status = trcio_read_su_header(file)
          if(status /= 0 )  then
            call trcio_print(file, &
            'trcio_readheader: ERROR-> Problem reading SU header.')
          endif
  
        case('TFIL')
          status = trcio_read_tfil_header(file)
          if(status /= 0 ) then
            call trcio_print(file,&
            'trcio_readheader: ERROR-> Problem reading TFIL header.')
          endif
  
        case('CMPR')
          status = trcio_error ! 2006-05-18 removed cmprio support wmm
         !status  = cpsio_close(file%lun,keep_open=.false.)
         !file%lun= cmprio_open(file%common,file%filename,file%io_mode)
         !status  = min(0,file%lun)
         !if(status /= 0 ) then
         !  call trcio_delete(file)
         !else
         !  ! Populate trcio_struct as possible from compressed file ascii header.
         !  ! ehs 29jan02
         !  !
         !  file%tmin       = cmprio_get_strt_val(file%lun)
         !  file%dt         = cmprio_get_srate   (file%lun)
         !  file%num_values = cmprio_get_nsmp    (file%lun)
         !  file%num_traces = cmprio_get_ntrc    (file%lun)
         ! 
         !  file%tmax = file%tmin + float(file%num_values - 1) * file%dt
         ! 
         !endif
         ! 
         !return             !--- Compressed file exits here.

        case('JSEIS') 
          if(jsf_wrapper_open(file%alt_lun) == 1 ) then
            file%num_traces     = jsf_wrapper_gettracecount(file%alt_lun)
            file%num_dimensions = jsf_wrapper_getnumdimensions(file%alt_lun)
            file%num_values     = jsf_wrapper_getsamplecount(file%alt_lun)
            file%dt             = jsf_wrapper_getsamplerate(file%alt_lun)
            file%tmin           = jsf_wrapper_getstarttimeinsecs(file%alt_lun)
            file%tmax           = file%tmin + (file%num_values-1)*file%dt
            file%nwih           = jsf_wrapper_getheadercount(file%alt_lun)
          else
            status = TRCIO_ERROR
          endif
        case default
         ! no header to read! Pass out the status = ok flag.
      end select

    end function trcio_readheader

end module trcio_module
